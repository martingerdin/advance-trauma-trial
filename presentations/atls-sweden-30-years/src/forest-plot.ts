import { metaAnalysis, type MetaAnalysisData } from "./figure-data";

const ROW_HEIGHT = 18;
const LABEL_WIDTH = 186;
const N_WIDTH = 52;
const FOREST_WIDTH = 260;
const EFFECT_WIDTH = 96;
const PAD_LEFT = 4;
const PAD_TOP = 22;
const AXIS_HEIGHT = 28;

function svgEl(tag: string, attrs: Record<string, string | number> = {}): SVGElement {
  const el = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const [key, value] of Object.entries(attrs)) {
    el.setAttribute(key, String(value));
  }
  return el;
}

function logToX(logRr: number, xlim: [number, number], forestX: number): number {
  const t = (logRr - xlim[0]) / (xlim[1] - xlim[0]);
  return forestX + t * FOREST_WIDTH;
}

function formatRr(value: number): string {
  return value.toFixed(2);
}

export function createForestPlotSvg(data: MetaAnalysisData = metaAnalysis): SVGSVGElement {
  const totalWidth = PAD_LEFT + LABEL_WIDTH + N_WIDTH + FOREST_WIDTH + EFFECT_WIDTH + 8;
  const totalHeight = PAD_TOP + (data.studies.length + 1) * ROW_HEIGHT + AXIS_HEIGHT + 8;
  const forestX = PAD_LEFT + LABEL_WIDTH + N_WIDTH;
  const [xMin, xMax] = data.logScaleXlim;
  const maxWeight = Math.max(...data.studies.map((s) => s.weightPercent));
  const nullX = logToX(0, data.logScaleXlim, forestX);

  const svg = svgEl("svg", {
    viewBox: `0 0 ${totalWidth} ${totalHeight}`,
    class: "forest-plot-chart",
    role: "img",
  }) as SVGSVGElement;
  svg.setAttribute(
    "aria-label",
    `Forest plot of ATLS effect on mortality. Pooled risk ratio ${data.pooled.rrFormatted}, 95% CI ${data.pooled.ciFormatted}`
  );

  const headerStudy = svgEl("text", { x: PAD_LEFT, y: 14, class: "forest-header" });
  headerStudy.textContent = data.labels.study;
  const headerN = svgEl("text", {
    x: PAD_LEFT + LABEL_WIDTH + N_WIDTH - 4,
    y: 14,
    class: "forest-header",
    "text-anchor": "end",
  });
  headerN.textContent = "n";
  const headerEffect = svgEl("text", {
    x: forestX + FOREST_WIDTH + 8,
    y: 14,
    class: "forest-header",
  });
  headerEffect.textContent = `${data.labels.effect} (95% CI)`;
  svg.append(headerStudy, headerN, headerEffect);

  svg.appendChild(
    svgEl("line", {
      x1: nullX,
      x2: nullX,
      y1: PAD_TOP - 4,
      y2: PAD_TOP + data.studies.length * ROW_HEIGHT + ROW_HEIGHT,
      stroke: "#8aa0a8",
      "stroke-width": 0.8,
      "stroke-dasharray": "2 2",
    })
  );

  data.studies.forEach((study, i) => {
    const y = PAD_TOP + i * ROW_HEIGHT + ROW_HEIGHT / 2;
    const row = svgEl("g", { class: "forest-row" });

    const name = svgEl("text", { x: PAD_LEFT, y: y + 4, class: "forest-study" });
    name.textContent = study.study;
    const n = svgEl("text", {
      x: PAD_LEFT + LABEL_WIDTH + N_WIDTH - 4,
      y: y + 4,
      class: "forest-n",
      "text-anchor": "end",
    });
    n.textContent = study.sampleSize.toLocaleString("en-US");

    const x = logToX(study.logRr, data.logScaleXlim, forestX);
    const xLo = Math.max(logToX(Math.log(study.ciLower), data.logScaleXlim, forestX), forestX);
    const xHi = Math.min(
      logToX(Math.log(study.ciUpper), data.logScaleXlim, forestX),
      forestX + FOREST_WIDTH
    );
    const size = 3 + (study.weightPercent / maxWeight) * 8;

    row.append(
      name,
      n,
      svgEl("line", {
        class: "forest-whisker",
        x1: xLo,
        x2: xHi,
        y1: y,
        y2: y,
        stroke: study.color,
        "stroke-width": 1.4,
      }),
      svgEl("rect", {
        class: "forest-square",
        x: x - size / 2,
        y: y - size / 2,
        width: size,
        height: size,
        fill: study.color,
      })
    );

    const effect = svgEl("text", {
      x: forestX + FOREST_WIDTH + 8,
      y: y + 4,
      class: "forest-effect",
    });
    effect.textContent = `${formatRr(study.rr)} (${formatRr(study.ciLower)}–${formatRr(study.ciUpper)})`;
    row.appendChild(effect);
    svg.appendChild(row);
  });

  const pooledY = PAD_TOP + data.studies.length * ROW_HEIGHT + ROW_HEIGHT / 2;
  const pooledRow = svgEl("g", { class: "forest-row forest-row--pooled" });
  const pooledName = svgEl("text", {
    x: PAD_LEFT,
    y: pooledY + 4,
    class: "forest-study forest-study--pooled",
  });
  pooledName.textContent = data.pooled.label;

  const px = logToX(data.pooled.logRr, data.logScaleXlim, forestX);
  const pLo = Math.max(logToX(Math.log(data.pooled.ciLower), data.logScaleXlim, forestX), forestX);
  const pHi = Math.min(
    logToX(Math.log(data.pooled.ciUpper), data.logScaleXlim, forestX),
    forestX + FOREST_WIDTH
  );
  const diamondH = 7;
  pooledRow.append(
    pooledName,
    svgEl("polygon", {
      class: "forest-diamond",
      points: `${pLo},${pooledY} ${px},${pooledY - diamondH} ${pHi},${pooledY} ${px},${pooledY + diamondH}`,
      fill: data.pooled.color,
      stroke: "#12788f",
      "stroke-width": 0.4,
    })
  );
  const pooledEffect = svgEl("text", {
    x: forestX + FOREST_WIDTH + 8,
    y: pooledY + 4,
    class: "forest-effect forest-effect--pooled",
  });
  pooledEffect.textContent = `${data.pooled.rrFormatted} (${data.pooled.ciFormatted.replace("; ", "–")})`;
  pooledRow.appendChild(pooledEffect);
  svg.appendChild(pooledRow);

  const axisY = PAD_TOP + (data.studies.length + 1) * ROW_HEIGHT + 8;
  svg.appendChild(
    svgEl("line", {
      x1: forestX,
      x2: forestX + FOREST_WIDTH,
      y1: axisY,
      y2: axisY,
      stroke: "#4a5c64",
      "stroke-width": 0.8,
    })
  );

  for (const tick of [0.25, 0.5, 1, 2]) {
    const logTick = Math.log(tick);
    if (logTick < xMin || logTick > xMax) continue;
    const x = logToX(logTick, data.logScaleXlim, forestX);
    svg.appendChild(
      svgEl("line", {
        x1: x,
        x2: x,
        y1: axisY,
        y2: axisY + 4,
        stroke: "#4a5c64",
        "stroke-width": 0.8,
      })
    );
    const label = svgEl("text", {
      x,
      y: axisY + 14,
      class: "axis-label",
      "text-anchor": "middle",
    });
    label.textContent = String(tick);
    svg.appendChild(label);
  }

  const leftLab = svgEl("text", { x: forestX, y: axisY + 26, class: "axis-label" });
  leftLab.textContent = data.labels.left;
  const rightLab = svgEl("text", {
    x: forestX + FOREST_WIDTH,
    y: axisY + 26,
    class: "axis-label",
    "text-anchor": "end",
  });
  rightLab.textContent = data.labels.right;
  svg.append(leftLab, rightLab);

  return svg;
}
