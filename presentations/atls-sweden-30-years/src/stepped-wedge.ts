import { trialDesign, type TrialDesignData, type TrialDesignSegment } from "./figure-data";

const ROW_HEIGHT = 10;
const ROW_GAP = 2;
const MONTH_WIDTH = 8;
const LABEL_LEFT = 36;
const LABEL_RIGHT = 36;
const LEGEND_HEIGHT = 22;
const AXIS_HEIGHT = 28;

function svgEl(tag: string, attrs: Record<string, string | number> = {}): SVGElement {
  const el = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const [key, value] of Object.entries(attrs)) {
    el.setAttribute(key, String(value));
  }
  return el;
}

function monthX(month: number, pad: number): number {
  return LABEL_LEFT + month * MONTH_WIDTH + pad * MONTH_WIDTH;
}

function clusterY(cluster: number, clusters: number): number {
  return LEGEND_HEIGHT + (clusters - cluster) * (ROW_HEIGHT + ROW_GAP);
}

function segmentFill(segment: TrialDesignSegment, data: TrialDesignData): string {
  if (segment.layer === "background") {
    return data.colors["Main stepped-wedge patient inclusion period"] ?? "#999999";
  }
  return data.colors[segment.phase] ?? "#999999";
}

export function createSteppedWedgeSvg(data: TrialDesignData = trialDesign): SVGSVGElement {
  const { clusters, batches, clustersPerBatch, currentMonth } = data.parameters;
  const { xPadding, barHalfHeight, overlayHalfHeight } = data.geometry;
  const plotWidth = data.xMax * MONTH_WIDTH;
  const plotHeight = clusters * (ROW_HEIGHT + ROW_GAP);
  const totalWidth = LABEL_LEFT + plotWidth + LABEL_RIGHT;
  const totalHeight = LEGEND_HEIGHT + plotHeight + AXIS_HEIGHT;

  const svg = svgEl("svg", {
    viewBox: `0 0 ${totalWidth} ${totalHeight}`,
    class: "stepped-wedge-chart",
    role: "img",
  }) as SVGSVGElement;
  svg.setAttribute(
    "aria-label",
    `Batched stepped-wedge design with ${clusters} hospital clusters, ${batches} batches, and ${data.parameters.sequences} sequences`
  );

  const legend = svgEl("g", { class: "wedge-legend" });
  let legendX = LABEL_LEFT;
  for (const phase of data.legend) {
    const swatch = svgEl("rect", {
      x: legendX,
      y: 2,
      width: 12,
      height: 8,
      rx: 1,
      fill: data.colors[phase] ?? "#999999",
    });
    const label = svgEl("text", {
      x: legendX + 16,
      y: 10,
      class: "legend-label",
    });
    label.textContent = phase;
    legend.append(swatch, label);
    legendX += Math.min(phase.length * 5.2 + 28, 160);
  }
  svg.appendChild(legend);

  const rows = svgEl("g", { class: "wedge-rows" });
  const segmentsByCluster = new Map<number, TrialDesignSegment[]>();
  for (const segment of data.segments) {
    const list = segmentsByCluster.get(segment.cluster) ?? [];
    list.push(segment);
    segmentsByCluster.set(segment.cluster, list);
  }

  for (let cluster = 1; cluster <= clusters; cluster++) {
    const y = clusterY(cluster, clusters);
    const row = svgEl("g", {
      class: "wedge-row",
      "data-batch": String(Math.ceil(cluster / clustersPerBatch)),
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
      const rect = svgEl("rect", {
        class: `wedge-segment wedge-${segment.phase.toLowerCase().replace(/\s+/g, "-")}`,
        x,
        y: yOff,
        width,
        height,
        rx: 1,
        fill: segmentFill(segment, data),
        stroke: isOverlay ? "#082830" : "none",
        "stroke-width": isOverlay ? 0.4 : 0,
        opacity: isOverlay ? 0.9 : 0.92,
      });
      row.appendChild(rect);
    }

    rows.appendChild(row);
  }
  svg.appendChild(rows);

  const axis = svgEl("g", { class: "wedge-axis" });
  const yLabel = svgEl("text", {
    x: 0,
    y: LEGEND_HEIGHT + plotHeight / 2,
    class: "axis-label",
    transform: `rotate(-90 8 ${LEGEND_HEIGHT + plotHeight / 2})`,
  });
  yLabel.textContent = data.labels.y;
  axis.appendChild(yLabel);

  const tickStep = data.xMax > 24 ? 4 : data.geometry.xBreakStep;
  for (let month = 0; month <= data.xMax; month += tickStep) {
    const x = monthX(month, 0);
    const tick = svgEl("line", {
      x1: x,
      x2: x,
      y1: LEGEND_HEIGHT + plotHeight,
      y2: LEGEND_HEIGHT + plotHeight + 3,
      stroke: "#4a5c64",
      "stroke-width": 0.75,
    });
    const tickLabel = svgEl("text", {
      x,
      y: LEGEND_HEIGHT + plotHeight + 14,
      class: "axis-label",
      "text-anchor": "middle",
    });
    tickLabel.textContent = String(month);
    axis.append(tick, tickLabel);
  }

  const xTitle = svgEl("text", {
    x: LABEL_LEFT + plotWidth / 2,
    y: totalHeight - 2,
    class: "axis-label",
    "text-anchor": "middle",
  });
  xTitle.textContent = data.labels.x;
  axis.appendChild(xTitle);

  for (let batch = 1; batch <= batches; batch++) {
    const midCluster = (batch - 0.5) * clustersPerBatch;
    const label = svgEl("text", {
      x: totalWidth - 4,
      y: clusterY(midCluster, clusters) + ROW_HEIGHT / 2 + 3,
      class: "axis-label",
      "text-anchor": "end",
    });
    label.textContent = String(batch);
    axis.appendChild(label);
  }

  const batchTitle = svgEl("text", {
    x: totalWidth - 4,
    y: LEGEND_HEIGHT - 4,
    class: "axis-label",
    "text-anchor": "end",
  });
  batchTitle.textContent = data.labels.batch;
  axis.appendChild(batchTitle);

  if (currentMonth != null) {
    const x = monthX(currentMonth, 0);
    axis.appendChild(
      svgEl("line", {
        x1: x,
        x2: x,
        y1: LEGEND_HEIGHT,
        y2: LEGEND_HEIGHT + plotHeight,
        stroke: "#c0392b",
        "stroke-width": 1,
        "stroke-dasharray": "3 2",
      })
    );
  }

  svg.appendChild(axis);
  return svg;
}
