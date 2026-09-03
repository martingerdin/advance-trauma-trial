const BATCHES = 6;
const CLUSTERS = 30;
const ROW_HEIGHT = 10;
const ROW_GAP = 2;
const TRAINING_WIDTH = 14;
const PERIOD_WIDTH = 48;

export function createSteppedWedgeSvg(): SVGSVGElement {
  const totalHeight = CLUSTERS * (ROW_HEIGHT + ROW_GAP) + 40;
  const totalWidth = TRAINING_WIDTH + BATCHES * PERIOD_WIDTH + 60;

  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("viewBox", `0 0 ${totalWidth} ${totalHeight}`);
  svg.setAttribute("class", "stepped-wedge-chart");
  svg.setAttribute("role", "img");
  svg.setAttribute(
    "aria-label",
    "Batched stepped-wedge design showing 30 hospital clusters crossing from standard care to ATLS intervention over 6 batches"
  );

  const defs = document.createElementNS("http://www.w3.org/2000/svg", "defs");
  defs.innerHTML = `
    <linearGradient id="control-grad" x1="0" y1="0" x2="1" y2="0">
      <stop offset="0%" stop-color="#f6851f"/>
      <stop offset="100%" stop-color="#e07510"/>
    </linearGradient>
    <linearGradient id="training-grad" x1="0" y1="0" x2="1" y2="0">
      <stop offset="0%" stop-color="#1a9dbb"/>
      <stop offset="100%" stop-color="#1589a4"/>
    </linearGradient>
    <linearGradient id="intervention-grad" x1="0" y1="0" x2="1" y2="0">
      <stop offset="0%" stop-color="#7b5ea7"/>
      <stop offset="100%" stop-color="#6a4f96"/>
    </linearGradient>
  `;
  svg.appendChild(defs);

  const legend = document.createElementNS("http://www.w3.org/2000/svg", "g");
  legend.setAttribute("class", "wedge-legend");
  legend.innerHTML = `
    <rect x="0" y="0" width="14" height="8" rx="2" fill="url(#control-grad)"/>
    <text x="18" y="7" class="legend-label">Standard care</text>
    <rect x="110" y="0" width="14" height="8" rx="2" fill="url(#training-grad)"/>
    <text x="128" y="7" class="legend-label">Training</text>
    <rect x="200" y="0" width="14" height="8" rx="2" fill="url(#intervention-grad)"/>
    <text x="218" y="7" class="legend-label">Intervention</text>
  `;
  svg.appendChild(legend);

  const rows = document.createElementNS("http://www.w3.org/2000/svg", "g");
  rows.setAttribute("class", "wedge-rows");
  rows.setAttribute("transform", "translate(0, 24)");

  for (let cluster = 0; cluster < CLUSTERS; cluster++) {
    const batch = Math.floor((cluster * BATCHES) / CLUSTERS);
    const y = cluster * (ROW_HEIGHT + ROW_GAP);
    const row = document.createElementNS("http://www.w3.org/2000/svg", "g");
    row.setAttribute("class", "wedge-row");
    row.setAttribute("data-batch", String(batch));
    row.setAttribute("transform", `translate(0, ${y})`);

    let x = 0;

    for (let period = 0; period < BATCHES; period++) {
      const isTraining = period === batch;
      const isIntervention = period > batch;

      if (isTraining) {
        const training = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        training.setAttribute("class", "wedge-segment wedge-training");
        training.setAttribute("x", String(x));
        training.setAttribute("y", "0");
        training.setAttribute("width", String(TRAINING_WIDTH));
        training.setAttribute("height", String(ROW_HEIGHT));
        training.setAttribute("rx", "2");
        training.setAttribute("fill", "url(#training-grad)");
        row.appendChild(training);
        x += TRAINING_WIDTH;
      }

      const segment = document.createElementNS("http://www.w3.org/2000/svg", "rect");
      segment.setAttribute(
        "class",
        `wedge-segment ${isIntervention ? "wedge-intervention" : "wedge-control"}`
      );
      segment.setAttribute("x", String(x));
      segment.setAttribute("y", "0");
      segment.setAttribute("width", String(PERIOD_WIDTH));
      segment.setAttribute("height", String(ROW_HEIGHT));
      segment.setAttribute("rx", "2");
      segment.setAttribute(
        "fill",
        isIntervention ? "url(#intervention-grad)" : "url(#control-grad)"
      );
      row.appendChild(segment);
      x += PERIOD_WIDTH;
    }

    rows.appendChild(row);
  }

  svg.appendChild(rows);

  const axis = document.createElementNS("http://www.w3.org/2000/svg", "g");
  axis.setAttribute("class", "wedge-axis");
  axis.setAttribute("transform", `translate(0, ${CLUSTERS * (ROW_HEIGHT + ROW_GAP) + 28})`);
  axis.innerHTML = `
    <text x="0" y="0" class="axis-label">Cluster</text>
    <text x="${totalWidth - 40}" y="0" class="axis-label" text-anchor="end">Time →</text>
  `;
  svg.appendChild(axis);

  return svg;
}
