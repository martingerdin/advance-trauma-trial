import { trialDesign, type TrialDesignData } from "./figure-data";

export interface SequencePhase {
  phase: string;
  months: number;
  start: number;
  end: number;
}

export interface ImplementationSequence {
  sequence: number;
  phases: SequencePhase[];
  totalMonths: number;
}

const PHASE_ORDER = ["Standard care", "Transition", "Intervention"] as const;

/** Lighter fills for month cells — match the CONSORT shell figure. */
const STRIP_COLORS: Record<string, string> = {
  "Standard care": "#FCCE94",
  Transition: "#92D9E6",
  Intervention: "#BFA6D6",
};

const BOX_FILL = "#FCCE94";
const LINE = "#6b7a80";

export function implementationSequences(
  data: TrialDesignData = trialDesign,
  batch = 1
): ImplementationSequence[] {
  const segments = data.segments.filter((s) => s.batch === batch && s.layer === "main");
  const ids = [...new Set(segments.map((s) => s.sequence))].sort((a, b) => a - b);

  return ids.map((sequence) => {
    const ofSeq = segments.filter((s) => s.sequence === sequence);
    const phases: SequencePhase[] = PHASE_ORDER.map((phase) => {
      const seg = ofSeq.find((s) => s.phase === phase);
      if (!seg) {
        throw new Error(`Missing ${phase} for sequence ${sequence} in batch ${batch}`);
      }
      return {
        phase,
        months: seg.end - seg.start,
        start: seg.start,
        end: seg.end,
      };
    });
    const last = phases[phases.length - 1];
    return {
      sequence,
      phases,
      totalMonths: last.end,
    };
  });
}

function phaseClass(phase: string): string {
  return phase.toLowerCase().replace(/\s+/g, "-");
}

function monthPhases(seq: ImplementationSequence): string[] {
  const months: string[] = [];
  for (const phase of seq.phases) {
    for (let i = 0; i < phase.months; i++) months.push(phase.phase);
  }
  return months;
}

function svgEl(tag: string, attrs: Record<string, string | number> = {}): SVGElement {
  const el = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const [key, value] of Object.entries(attrs)) {
    el.setAttribute(key, String(value));
  }
  return el;
}

function flowBox(text: string, kind: "primary" | "side" = "primary"): HTMLElement {
  const box = document.createElement("div");
  box.className = `consort-box consort-box--${kind}`;
  if (kind === "primary") box.style.background = BOX_FILL;
  box.textContent = text;
  return box;
}

function exclusionBox(): HTMLElement {
  const box = document.createElement("aside");
  box.className = "consort-box consort-box--side";
  const title = document.createElement("p");
  title.className = "consort-box__title";
  title.textContent = "Excluded before randomisation:";
  const list = document.createElement("ul");
  list.className = "consort-box__list";
  for (const item of [
    "Not meeting inclusion criteria",
    "Declined to participate",
    "Other reasons",
  ]) {
    const li = document.createElement("li");
    li.textContent = item;
    list.appendChild(li);
  }
  box.append(title, list);
  return box;
}

function sequenceColumn(seq: ImplementationSequence, totalMonths: number): HTMLElement {
  const col = document.createElement("div");
  col.className = "consort-sequence";
  col.dataset.sequence = String(seq.sequence);

  const title = document.createElement("p");
  title.className = "consort-sequence__title";
  title.textContent = `Sequence ${seq.sequence}`;
  col.appendChild(title);

  const strip = document.createElement("div");
  strip.className = "consort-strip";
  strip.style.gridTemplateColumns = `repeat(${totalMonths}, minmax(0, 1fr))`;
  strip.setAttribute(
    "aria-label",
    `Sequence ${seq.sequence}: ${seq.phases[0].months} months standard care, training, ${seq.phases[2].months} months intervention`
  );

  for (const phase of monthPhases(seq)) {
    const cell = document.createElement("span");
    cell.className = `consort-cell consort-cell--${phaseClass(phase)}`;
    cell.style.background = STRIP_COLORS[phase] ?? "#ccc";
    cell.title = phase;
    strip.appendChild(cell);
  }

  col.appendChild(strip);
  return col;
}

/** Mid stem with side branch + arrow into the exclusion box (SVG for continuous joins). */
function createMidConnectors(): SVGSVGElement {
  const w = 200;
  const h = 160;
  const cx = 20;
  const midY = h / 2;
  const branchEnd = 78;
  const svg = svgEl("svg", {
    class: "consort-mid-svg",
    viewBox: `0 0 ${w} ${h}`,
    preserveAspectRatio: "none",
    "aria-hidden": "true",
  }) as SVGSVGElement;

  svg.appendChild(
    svgEl("line", {
      x1: cx,
      y1: 0,
      x2: cx,
      y2: h,
      stroke: LINE,
      "stroke-width": 2,
    })
  );
  svg.appendChild(
    svgEl("line", {
      x1: cx,
      y1: midY,
      x2: branchEnd,
      y2: midY,
      stroke: LINE,
      "stroke-width": 2,
    })
  );
  // Arrowhead flush with branch tip
  svg.appendChild(
    svgEl("polygon", {
      points: `${branchEnd - 1},${midY - 6} ${branchEnd + 10},${midY} ${branchEnd - 1},${midY + 6}`,
      fill: LINE,
    })
  );
  return svg;
}

/** Fan from randomised box into sequence columns (SVG for continuous T-junctions). */
function createFanSvg(nSeq: number): SVGSVGElement {
  const w = 1000;
  const stemH = 28;
  const dropH = 36;
  const h = stemH + dropH;
  const colW = w / nSeq;
  const railY = stemH;
  const arrowH = 10;
  const arrowHalf = 6;
  const tipY = h - 1;

  const svg = svgEl("svg", {
    class: "consort-fan-svg",
    viewBox: `0 0 ${w} ${h}`,
    preserveAspectRatio: "none",
    "aria-hidden": "true",
  }) as SVGSVGElement;

  // Stem from randomised (centre) down to rail
  svg.appendChild(
    svgEl("line", {
      x1: w / 2,
      y1: 0,
      x2: w / 2,
      y2: railY,
      stroke: LINE,
      "stroke-width": 2,
    })
  );

  // Horizontal rail across column centres
  const x0 = colW / 2;
  const x1 = w - colW / 2;
  svg.appendChild(
    svgEl("line", {
      x1: x0,
      y1: railY,
      x2: x1,
      y2: railY,
      stroke: LINE,
      "stroke-width": 2,
    })
  );

  for (let i = 0; i < nSeq; i++) {
    const x = colW * (i + 0.5);
    // Drop from rail to just above arrow tip
    svg.appendChild(
      svgEl("line", {
        x1: x,
        y1: railY,
        x2: x,
        y2: tipY - arrowH + 2,
        stroke: LINE,
        "stroke-width": 2,
      })
    );
    // Arrowhead overlapping the shaft end
    svg.appendChild(
      svgEl("polygon", {
        points: `${x - arrowHalf},${tipY - arrowH} ${x + arrowHalf},${tipY - arrowH} ${x},${tipY}`,
        fill: LINE,
      })
    );
  }

  return svg;
}

/**
 * CONSORT-style cluster flow: assessed → excluded side branch → randomised →
 * five implementation-sequence strips (month cells).
 */
export function createSequencesChart(data: TrialDesignData = trialDesign): HTMLElement {
  const sequences = implementationSequences(data);
  const totalMonths = sequences[0]?.totalMonths ?? data.parameters.totalMonths;
  const nSeq = sequences.length;

  const chart = document.createElement("div");
  chart.className = "consort-flow";
  chart.style.setProperty("--consort-seqs", String(nSeq));
  chart.setAttribute(
    "aria-label",
    "Cluster flow: hospitals are assessed for eligibility, then randomised to one of five implementation sequences"
  );

  const assessed = flowBox("Clusters assessed for eligibility");
  assessed.classList.add("consort-flow__assessed");
  chart.appendChild(assessed);

  const mid = document.createElement("div");
  mid.className = "consort-flow__mid";

  const connectors = document.createElement("div");
  connectors.className = "consort-flow__mid-connectors";
  connectors.setAttribute("aria-hidden", "true");
  connectors.appendChild(createMidConnectors());
  mid.appendChild(connectors);

  const exclWrap = document.createElement("div");
  exclWrap.className = "consort-flow__excl-wrap";
  exclWrap.appendChild(exclusionBox());
  mid.appendChild(exclWrap);
  chart.appendChild(mid);

  const randomised = flowBox("Clusters randomised");
  randomised.classList.add("consort-flow__randomised");
  chart.appendChild(randomised);

  const sequencesBlock = document.createElement("div");
  sequencesBlock.className = "consort-flow__sequences";

  const fan = document.createElement("div");
  fan.className = "consort-flow__fan";
  fan.setAttribute("aria-hidden", "true");
  fan.appendChild(createFanSvg(nSeq));
  sequencesBlock.appendChild(fan);

  const cols = document.createElement("div");
  cols.className = "consort-sequences";
  for (const seq of sequences) {
    cols.appendChild(sequenceColumn(seq, totalMonths));
  }
  sequencesBlock.appendChild(cols);
  chart.appendChild(sequencesBlock);

  return chart;
}

export function createSequencesLegend(): HTMLElement {
  const el = document.createElement("div");
  el.className = "wedge-legend sequences-legend";
  el.setAttribute("aria-hidden", "true");
  const labels: Array<[string, string]> = [
    ["Standard care", STRIP_COLORS["Standard care"]],
    ["Training", STRIP_COLORS.Transition],
    ["Intervention", STRIP_COLORS.Intervention],
  ];
  for (const [text, color] of labels) {
    const item = document.createElement("span");
    item.className = "wedge-legend__item";
    const swatch = document.createElement("span");
    swatch.className = "wedge-legend__swatch";
    swatch.style.background = color;
    const label = document.createElement("span");
    label.className = "wedge-legend__label";
    label.textContent = text;
    item.append(swatch, label);
    el.appendChild(item);
  }
  return el;
}
