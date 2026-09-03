import { trialDesign, type TrialDesignData, type TrialDesignSegment } from "./figure-data";

const MONTH_WIDTH = 8;
const LABEL_LEFT = 36;
const LABEL_RIGHT = 36;
const LEGEND_HEIGHT = 22;
const AXIS_HEIGHT = 28;

export interface SteppedWedgeOptions {
  /** Clusters to draw. Defaults to all clusters in the design. */
  visibleClusters?: number[];
}

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

function rowMetrics(visibleCount: number): { rowHeight: number; rowGap: number } {
  if (visibleCount <= 1) return { rowHeight: 40, rowGap: 6 };
  if (visibleCount <= 5) return { rowHeight: 20, rowGap: 4 };
  return { rowHeight: 10, rowGap: 2 };
}

/** Visible clusters laid out bottom→top in ascending cluster order. */
function clusterY(
  cluster: number,
  visible: number[],
  rowHeight: number,
  rowGap: number
): number {
  const index = visible.indexOf(cluster);
  const fromBottom = index;
  return LEGEND_HEIGHT + (visible.length - 1 - fromBottom) * (rowHeight + rowGap);
}

function segmentFill(segment: TrialDesignSegment, data: TrialDesignData): string {
  if (segment.layer === "background") {
    return data.colors["Main stepped-wedge patient inclusion period"] ?? "#999999";
  }
  return data.colors[segment.phase] ?? "#999999";
}

export function createSteppedWedgeSvg(
  data: TrialDesignData = trialDesign,
  options: SteppedWedgeOptions = {}
): SVGSVGElement {
  const { clustersPerBatch, currentMonth, clusters: totalClusters } = data.parameters;
  const { xPadding, barHalfHeight, overlayHalfHeight } = data.geometry;
  const visible =
    options.visibleClusters?.length
      ? [...options.visibleClusters].sort((a, b) => a - b)
      : Array.from({ length: totalClusters }, (_, i) => i + 1);

  const { rowHeight, rowGap } = rowMetrics(visible.length);
  const plotWidth = data.xMax * MONTH_WIDTH;
  const plotHeight = Math.max(visible.length, 1) * (rowHeight + rowGap);
  const totalWidth = LABEL_LEFT + plotWidth + LABEL_RIGHT;
  const totalHeight = LEGEND_HEIGHT + plotHeight + AXIS_HEIGHT;

  const svg = svgEl("svg", {
    viewBox: `0 0 ${totalWidth} ${totalHeight}`,
    class: "stepped-wedge-chart",
    role: "img",
  }) as SVGSVGElement;
  svg.dataset.visibleCount = String(visible.length);
  svg.setAttribute(
    "aria-label",
    `Batched stepped-wedge design showing ${visible.length} of ${totalClusters} hospital clusters over months 0–${data.xMax}`
  );

  const legend = svgEl("g", { class: "wedge-legend" });
  let legendX = LABEL_LEFT;
  for (const phase of data.legend) {
    legend.append(
      svgEl("rect", {
        x: legendX,
        y: 2,
        width: 12,
        height: 8,
        rx: 1,
        fill: data.colors[phase] ?? "#999999",
      })
    );
    const label = svgEl("text", {
      x: legendX + 16,
      y: 10,
      class: "legend-label",
    });
    label.textContent = phase;
    legend.appendChild(label);
    legendX += 22 + phase.length * 5.2;
  }
  svg.appendChild(legend);

  const rows = svgEl("g", { class: "wedge-rows" });
  const segmentsByCluster = new Map<number, TrialDesignSegment[]>();
  for (const segment of data.segments) {
    if (!visible.includes(segment.cluster)) continue;
    const list = segmentsByCluster.get(segment.cluster) ?? [];
    list.push(segment);
    segmentsByCluster.set(segment.cluster, list);
  }

  for (const cluster of visible) {
    const y = clusterY(cluster, visible, rowHeight, rowGap);
    const row = svgEl("g", {
      class: "wedge-row",
      "data-cluster": String(cluster),
      "data-batch": String(Math.ceil(cluster / clustersPerBatch)),
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
      const unit = rowHeight + rowGap;
      const height = half * 2 * unit;
      const yOff = (rowHeight - height) / 2;
      const x = monthX(segment.start, pad);
      const width = Math.max((segment.end - segment.start - pad * 2) * MONTH_WIDTH, 1);
      positioned.appendChild(
        svgEl("rect", {
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
        })
      );
    }

    row.appendChild(positioned);
    rows.appendChild(row);
  }
  svg.appendChild(rows);

  const axis = svgEl("g", { class: "wedge-axis" });
  const yLabel = svgEl("text", {
    x: 8,
    y: LEGEND_HEIGHT + plotHeight / 2,
    class: "axis-label",
    transform: `rotate(-90 8 ${LEGEND_HEIGHT + plotHeight / 2})`,
  });
  yLabel.textContent = data.labels.y;
  axis.appendChild(yLabel);

  const tickStep = data.xMax > 24 ? 4 : data.geometry.xBreakStep;
  for (let month = 0; month <= data.xMax; month += tickStep) {
    const x = monthX(month, 0);
    axis.appendChild(
      svgEl("line", {
        x1: x,
        x2: x,
        y1: LEGEND_HEIGHT + plotHeight,
        y2: LEGEND_HEIGHT + plotHeight + 3,
        stroke: "#4a5c64",
        "stroke-width": 0.75,
      })
    );
    const tickLabel = svgEl("text", {
      x,
      y: LEGEND_HEIGHT + plotHeight + 14,
      class: "axis-label",
      "text-anchor": "middle",
    });
    tickLabel.textContent = String(month);
    axis.appendChild(tickLabel);
  }

  const xTitle = svgEl("text", {
    x: LABEL_LEFT + plotWidth / 2,
    y: totalHeight - 2,
    class: "axis-label",
    "text-anchor": "middle",
  });
  xTitle.textContent = data.labels.x;
  axis.appendChild(xTitle);

  const visibleBatches = [
    ...new Set(visible.map((c) => Math.ceil(c / clustersPerBatch))),
  ].sort((a, b) => a - b);

  for (const batch of visibleBatches) {
    const batchClusters = visible.filter((c) => Math.ceil(c / clustersPerBatch) === batch);
    const mid =
      batchClusters.reduce((sum, c) => sum + c, 0) / Math.max(batchClusters.length, 1);
    // Place label at the average cluster position among the visible set.
    const closest = batchClusters.reduce((best, c) =>
      Math.abs(c - mid) < Math.abs(best - mid) ? c : best
    );
    const label = svgEl("text", {
      x: totalWidth - 4,
      y: clusterY(closest, visible, rowHeight, rowGap) + rowHeight / 2 + 3,
      class: "axis-label",
      "text-anchor": "end",
    });
    label.textContent = String(batch);
    axis.appendChild(label);
  }

  if (visibleBatches.length > 0) {
    const batchTitle = svgEl("text", {
      x: totalWidth - 4,
      y: LEGEND_HEIGHT - 4,
      class: "axis-label",
      "text-anchor": "end",
    });
    batchTitle.textContent = data.labels.batch;
    axis.appendChild(batchTitle);
  }

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
