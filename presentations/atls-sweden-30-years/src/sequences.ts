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

  const drop = document.createElement("div");
  drop.className = "consort-sequence__drop";
  drop.setAttribute("aria-hidden", "true");
  col.appendChild(drop);

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

/**
 * CONSORT-style cluster flow: assessed → excluded side branch → randomised →
 * five implementation-sequence strips (month cells).
 */
export function createSequencesChart(data: TrialDesignData = trialDesign): HTMLElement {
  const sequences = implementationSequences(data);
  const totalMonths = sequences[0]?.totalMonths ?? data.parameters.totalMonths;

  const chart = document.createElement("div");
  chart.className = "consort-flow";
  chart.setAttribute(
    "aria-label",
    "Cluster flow: hospitals are assessed for eligibility, then randomised to one of five implementation sequences"
  );

  const assessed = flowBox("Clusters assessed for eligibility");
  assessed.classList.add("consort-flow__assessed");
  chart.appendChild(assessed);

  const mid = document.createElement("div");
  mid.className = "consort-flow__mid";

  const stem = document.createElement("div");
  stem.className = "consort-flow__stem";
  stem.setAttribute("aria-hidden", "true");
  const stemLine = document.createElement("div");
  stemLine.className = "consort-flow__stem-line";
  const stemBranch = document.createElement("div");
  stemBranch.className = "consort-flow__stem-branch";
  stem.append(stemLine, stemBranch);
  mid.appendChild(stem);

  const exclusion = exclusionBox();
  mid.appendChild(exclusion);
  chart.appendChild(mid);

  const randomised = flowBox("Clusters randomised");
  randomised.classList.add("consort-flow__randomised");
  chart.appendChild(randomised);

  const sequencesBlock = document.createElement("div");
  sequencesBlock.className = "consort-flow__sequences";

  const bus = document.createElement("div");
  bus.className = "consort-flow__bus";
  bus.setAttribute("aria-hidden", "true");
  sequencesBlock.appendChild(bus);

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
