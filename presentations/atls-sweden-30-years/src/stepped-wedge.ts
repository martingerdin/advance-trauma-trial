import { trialDesign, type TrialDesignData, type TrialDesignSegment } from "./figure-data";

export const MONTH_WIDTH = 22;
/** Gutter for the rotated "Cluster" y-axis title. */
export const LABEL_LEFT = 52;
/** Gutter for batch numbers and the "Batch" title. */
export const LABEL_RIGHT = 52;
/** Room above the top cluster so "Batch" sits clear of the bars. */
export const TOP_PAD = 24;
export const AXIS_HEIGHT = 52;
export const AXIS_TICK_LEN = 4;
/** Distance from the plot bottom to the tick-number baseline (alphabetic). */
export const AXIS_TICK_LABEL_OFFSET = 20;
export const AXIS_TITLE_OFFSET = 42;
export const ROW_HEIGHT = 12;
export const ROW_GAP = 3;

export type DesignFocusStage = "site" | "batch" | "full";

export interface WedgeViewBox {
  x: number;
  y: number;
  w: number;
  h: number;
}

function svgEl(tag: string, attrs: Record<string, string | number> = {}): SVGElement {
  const el = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const [key, value] of Object.entries(attrs)) {
    el.setAttribute(key, String(value));
  }
  return el;
}

export function monthX(month: number, pad = 0): number {
  return LABEL_LEFT + month * MONTH_WIDTH + pad * MONTH_WIDTH;
}

/** Horizontal center of a study month on the chart, as % of total SVG width. */
export function monthCenterPercent(month: number, totalMonths: number): number {
  const totalWidth = LABEL_LEFT + totalMonths * MONTH_WIDTH + LABEL_RIGHT;
  return (monthX(month) / totalWidth) * 100;
}

/** Cluster 1 at the bottom, matching the R ggplot figure. */
export function clusterY(cluster: number, clusters: number): number {
  return TOP_PAD + (clusters - cluster) * (ROW_HEIGHT + ROW_GAP);
}

function segmentFill(segment: TrialDesignSegment, data: TrialDesignData): string {
  if (segment.layer === "background") {
    return data.colors["Main stepped-wedge patient inclusion period"] ?? "#999999";
  }
  return data.colors[segment.phase] ?? "#999999";
}

export function viewBoxString(box: WedgeViewBox): string {
  return `${box.x} ${box.y} ${box.w} ${box.h}`;
}

export function lerpViewBox(from: WedgeViewBox, to: WedgeViewBox, t: number): WedgeViewBox {
  const e = 1 - Math.pow(1 - t, 3);
  return {
    x: from.x + (to.x - from.x) * e,
    y: from.y + (to.y - from.y) * e,
    w: from.w + (to.w - from.w) * e,
    h: from.h + (to.h - from.h) * e,
  };
}

/** Months visible on the x-axis for a given camera stage. */
export function stageMonthSpan(data: TrialDesignData, stage: DesignFocusStage): number {
  if (stage === "full") return data.xMax;
  return Math.max(data.parameters.totalMonths || 13, 13);
}

/** Camera framing for each reveal stage — tight crop (CSS/fit handles centering & scale). */
export function focusViewBox(data: TrialDesignData, stage: DesignFocusStage): WedgeViewBox {
  const { clusters, clustersPerBatch } = data.parameters;
  const months = stageMonthSpan(data, stage);
  const width = LABEL_LEFT + months * MONTH_WIDTH + LABEL_RIGHT;
  const fullHeight = TOP_PAD + clusters * (ROW_HEIGHT + ROW_GAP) + AXIS_HEIGHT;

  if (stage === "full") {
    return { x: 0, y: 0, w: width, h: fullHeight };
  }

  if (stage === "site") {
    const barY = clusterY(1, clusters);
    const axisBottom = TOP_PAD + clusters * (ROW_HEIGHT + ROW_GAP) + AXIS_HEIGHT;
    // Tight crop — phase callouts live in HTML above the SVG, not inside the viewBox.
    const padY = 8;
    return { x: 0, y: barY - padY, w: width, h: axisBottom - barY + padY * 2 };
  }

  const topCluster = clustersPerBatch;
  const topY = clusterY(topCluster, clusters);
  const contentH = clustersPerBatch * (ROW_HEIGHT + ROW_GAP) + AXIS_HEIGHT;
  const padY = 14;
  return { x: 0, y: topY - padY, w: width, h: contentH + padY * 2 };
}

/**
 * Size the SVG to the largest rectangle that fits in `container` while
 * preserving the current viewBox aspect.
 *
 * `preferWidth` (one-site): fill container width first so a short crop
 * stays a wide readable timeline instead of a tiny centered blob.
 * `reserveBottom` / `reserveTop`: space taken by siblings inside the container.
 */
export function fitSvgInContainer(
  svg: SVGSVGElement,
  box: WedgeViewBox,
  container: HTMLElement,
  options: { preferWidth?: boolean; reserveTop?: number; reserveBottom?: number } = {}
): void {
  const style = getComputedStyle(container);
  const padX = parseFloat(style.paddingLeft) + parseFloat(style.paddingRight);
  const padY = parseFloat(style.paddingTop) + parseFloat(style.paddingBottom);
  const reserve = Math.max(options.reserveTop ?? 0, 0) + Math.max(options.reserveBottom ?? 0, 0);
  const cw = Math.max(container.clientWidth - padX, 1);
  const ch = Math.max(container.clientHeight - padY - reserve, 1);
  if (!Number.isFinite(box.w) || !Number.isFinite(box.h) || box.w <= 0 || box.h <= 0) return;

  let scale: number;
  if (options.preferWidth) {
    scale = cw / box.w;
    // Flex wells can report a tiny height before layout settles — only clamp
    // when there is a real vertical budget.
    if (ch > 120 && box.h * scale > ch) scale = ch / box.h;
  } else {
    scale = Math.min(cw / box.w, ch / box.h);
  }

  svg.style.width = `${Math.max(box.w * scale, 1)}px`;
  svg.style.height = `${Math.max(box.h * scale, 1)}px`;
  svg.style.flex = "0 0 auto";
  svg.setAttribute("preserveAspectRatio", "xMidYMid meet");
}

/** Show only x-axis ticks/title for the months in frame; hide out-of-range chrome. */
export function syncAxisToStage(
  svg: SVGSVGElement,
  data: TrialDesignData,
  stage: DesignFocusStage
): void {
  const months = stageMonthSpan(data, stage);
  const plotCenterX = LABEL_LEFT + (months * MONTH_WIDTH) / 2;

  svg.querySelectorAll<SVGElement>(".wedge-axis-tick").forEach((tick) => {
    const month = Number(tick.dataset.month);
    const on = Number.isFinite(month) && month <= months + 0.01;
    tick.setAttribute("opacity", on ? "1" : "0");
  });

  const xTitle = svg.querySelector<SVGTextElement>(".wedge-axis-xtitle");
  if (xTitle) {
    xTitle.setAttribute("x", String(plotCenterX));
    // Keep the title just under the visible cluster block for cropped stages.
    if (stage === "full") {
      const fullHeight = TOP_PAD + data.parameters.clusters * (ROW_HEIGHT + ROW_GAP) + AXIS_HEIGHT;
      xTitle.setAttribute("y", String(fullHeight - 4));
    } else if (stage === "site") {
      const barY = clusterY(1, data.parameters.clusters);
      xTitle.setAttribute("y", String(barY + ROW_HEIGHT + AXIS_TITLE_OFFSET));
    } else {
      const topY = clusterY(data.parameters.clustersPerBatch, data.parameters.clusters);
      const blockH = data.parameters.clustersPerBatch * (ROW_HEIGHT + ROW_GAP);
      xTitle.setAttribute("y", String(topY + blockH + AXIS_TITLE_OFFSET));
    }
  }

  const yLabel = svg.querySelector<SVGTextElement>(".wedge-axis-ylabel");
  const batchTitle = svg.querySelector<SVGTextElement>(".wedge-axis-batch-title");
  if (yLabel) yLabel.setAttribute("opacity", stage === "full" ? "1" : "0");
  if (batchTitle) batchTitle.setAttribute("opacity", stage === "full" ? "1" : "0");
}

/** Fixed HTML legend — not affected by SVG viewBox zoom. */
export function createWedgeLegend(data: TrialDesignData): HTMLElement {
  const el = document.createElement("div");
  el.className = "wedge-legend";
  el.setAttribute("aria-hidden", "true");
  for (const phase of data.legend) {
    const item = document.createElement("span");
    item.className = "wedge-legend__item";
    const swatch = document.createElement("span");
    swatch.className = "wedge-legend__swatch";
    swatch.style.background = data.colors[phase] ?? "#999999";
    const label = document.createElement("span");
    label.className = "wedge-legend__label";
    label.textContent = phase === "Transition" ? "Training" : phase;
    item.append(swatch, label);
    el.appendChild(item);
  }
  return el;
}

/** Set how far along the study-month axis the bars are revealed (clip wipe). */
export function setRevealMonth(svg: SVGSVGElement, month: number): void {
  const rect = svg.querySelector<SVGRectElement>(".wedge-reveal-rect");
  if (!rect) return;
  const width = Math.max(monthX(Math.max(month, 0)), LABEL_LEFT);
  rect.setAttribute("width", String(width));
  svg.dataset.revealMonth = String(month);
}

export function createSteppedWedgeSvg(data: TrialDesignData = trialDesign): SVGSVGElement {
  const { clusters, batches, clustersPerBatch, currentMonth } = data.parameters;
  const { xPadding, barHalfHeight, overlayHalfHeight } = data.geometry;
  const plotWidth = data.xMax * MONTH_WIDTH;
  const plotHeight = clusters * (ROW_HEIGHT + ROW_GAP);
  const totalWidth = LABEL_LEFT + plotWidth + LABEL_RIGHT;
  const totalHeight = TOP_PAD + plotHeight + AXIS_HEIGHT;
  const initial = focusViewBox(data, "site");
  const clipId = `wedge-reveal-${Math.random().toString(36).slice(2, 9)}`;

  const svg = svgEl("svg", {
    viewBox: viewBoxString(initial),
    class: "stepped-wedge-chart",
    role: "img",
    preserveAspectRatio: "xMidYMid meet",
  }) as SVGSVGElement;
  svg.dataset.stage = "site";
  svg.dataset.revealMonth = "0";
  svg.setAttribute(
    "aria-label",
    `Batched stepped-wedge design with ${clusters} hospital clusters, ${batches} batches, and ${data.parameters.sequences} sequences`
  );

  const defs = svgEl("defs");
  const clipPath = svgEl("clipPath", { id: clipId });
  // Tall enough to cover any camera crop; wipe reveals left→right by study month.
  clipPath.appendChild(
    svgEl("rect", {
      class: "wedge-reveal-rect",
      x: 0,
      y: -200,
      width: 0,
      height: totalHeight + 400,
    })
  );
  defs.appendChild(clipPath);
  svg.appendChild(defs);

  const rows = svgEl("g", {
    class: "wedge-rows",
    "clip-path": `url(#${clipId})`,
  });
  const segmentsByCluster = new Map<number, TrialDesignSegment[]>();
  for (const segment of data.segments) {
    const list = segmentsByCluster.get(segment.cluster) ?? [];
    list.push(segment);
    segmentsByCluster.set(segment.cluster, list);
  }

  for (let cluster = 1; cluster <= clusters; cluster++) {
    const y = clusterY(cluster, clusters);
    const batch = Math.ceil(cluster / clustersPerBatch);
    const row = svgEl("g", {
      class: "wedge-row",
      "data-cluster": String(cluster),
      "data-batch": String(batch),
      opacity: 0,
    });
    const positioned = svgEl("g", {
      transform: `translate(0, ${y})`,
    });

    const clusterSegments = segmentsByCluster.get(cluster) ?? [];
    const ordered = [
      ...clusterSegments.filter((s) => s.layer !== "overlay"),
      ...clusterSegments.filter((s) => s.layer === "overlay"),
    ];

    for (const segment of ordered) {
      const isOverlay = segment.layer === "overlay";
      const pad = isOverlay || segment.layer === "main" ? xPadding : 0;
      const half = isOverlay ? overlayHalfHeight : barHalfHeight;
      const unit = ROW_HEIGHT + ROW_GAP;
      const height = half * 2 * unit;
      const yOff = (ROW_HEIGHT - height) / 2;
      const x = monthX(segment.start, pad);
      const width = Math.max((segment.end - segment.start - pad * 2) * MONTH_WIDTH, 1);
      const phaseSlug = segment.phase.toLowerCase().replace(/\s+/g, "-");
      positioned.appendChild(
        svgEl("rect", {
          class: `wedge-segment wedge-${phaseSlug}`,
          "data-phase": phaseSlug,
          "data-start": String(segment.start),
          x,
          y: yOff,
          width,
          height,
          rx: 1,
          fill: segmentFill(segment, data),
          stroke: isOverlay ? "#082830" : "none",
          "stroke-width": isOverlay ? 0.4 : 0,
          opacity: isOverlay ? 0.9 : 0.92,
        })
      );
    }

    row.appendChild(positioned);
    rows.appendChild(row);
  }
  svg.appendChild(rows);

  const axis = svgEl("g", { class: "wedge-axis" });
  const plotBottom = TOP_PAD + plotHeight;
  const yLabelX = 22;
  const yLabelY = TOP_PAD + plotHeight / 2;
  const yLabel = svgEl("text", {
    x: yLabelX,
    y: yLabelY,
    class: "axis-label wedge-axis-ylabel",
    "text-anchor": "middle",
    transform: `rotate(-90 ${yLabelX} ${yLabelY})`,
    opacity: 0,
  });
  yLabel.textContent = data.labels.y;
  axis.appendChild(yLabel);

  const tickStep = data.xMax > 24 ? 4 : data.geometry.xBreakStep;
  for (let month = 0; month <= data.xMax; month += tickStep) {
    const x = monthX(month, 0);
    const tick = svgEl("g", {
      class: "wedge-axis-tick",
      "data-month": String(month),
      opacity: month <= stageMonthSpan(data, "site") ? 1 : 0,
    });
    tick.appendChild(
      svgEl("line", {
        x1: x,
        x2: x,
        y1: plotBottom,
        y2: plotBottom + AXIS_TICK_LEN,
        stroke: "#4a5c64",
        "stroke-width": 0.75,
      })
    );
    const tickLabel = svgEl("text", {
      x,
      y: plotBottom + AXIS_TICK_LABEL_OFFSET,
      class: "axis-label wedge-axis-tick-label",
      "text-anchor": "middle",
    });
    tickLabel.textContent = String(month);
    tick.appendChild(tickLabel);
    axis.appendChild(tick);
  }

  const siteMonths = stageMonthSpan(data, "site");
  const xTitle = svgEl("text", {
    x: LABEL_LEFT + (siteMonths * MONTH_WIDTH) / 2,
    y: clusterY(1, clusters) + ROW_HEIGHT + AXIS_TITLE_OFFSET,
    class: "axis-label wedge-axis-xtitle",
    "text-anchor": "middle",
  });
  xTitle.textContent = data.labels.x;
  axis.appendChild(xTitle);

  for (let batch = 1; batch <= batches; batch++) {
    const midCluster = (batch - 0.5) * clustersPerBatch;
    const label = svgEl("text", {
      x: totalWidth - LABEL_RIGHT / 2,
      y: clusterY(midCluster, clusters) + ROW_HEIGHT / 2 + 3,
      class: "axis-label wedge-batch-label",
      "data-batch": String(batch),
      "text-anchor": "middle",
      opacity: 0,
    });
    label.textContent = String(batch);
    axis.appendChild(label);
  }

  const batchTitle = svgEl("text", {
    x: totalWidth - LABEL_RIGHT / 2,
    y: TOP_PAD - 8,
    class: "axis-label wedge-axis-batch-title",
    "text-anchor": "middle",
    opacity: 0,
  });
  batchTitle.textContent = data.labels.batch;
  axis.appendChild(batchTitle);

  if (currentMonth != null) {
    const x = monthX(currentMonth, 0);
    axis.appendChild(
      svgEl("line", {
        x1: x,
        x2: x,
        y1: TOP_PAD,
        y2: TOP_PAD + plotHeight,
        stroke: "#c0392b",
        "stroke-width": 1,
        "stroke-dasharray": "3 2",
      })
    );
  }

  svg.appendChild(axis);
  syncAxisToStage(svg, data, "site");
  return svg;
}
