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

function monthsLabel(n: number): string {
  return n === 1 ? "1 month" : `${n} months`;
}

function phaseClass(phase: string): string {
  return phase.toLowerCase().replace(/\s+/g, "-");
}

function barLabel(phase: SequencePhase): string {
  if (phase.phase === "Transition") return "ATLS®";
  return monthsLabel(phase.months);
}

export function createSequencesChart(data: TrialDesignData = trialDesign): HTMLElement {
  const sequences = implementationSequences(data);
  const totalMonths = sequences[0]?.totalMonths ?? data.parameters.totalMonths;
  const chart = document.createElement("div");
  chart.className = "sequences-chart";
  chart.setAttribute(
    "aria-label",
    `Five implementation sequences. Sequence 1 has ${sequences[0]?.phases[0]?.months} months of standard care, training, then ${sequences[0]?.phases[2]?.months} months of intervention. Each later sequence adds one month of standard care and one fewer month of intervention.`
  );

  const axis = document.createElement("div");
  axis.className = "sequences-axis";
  axis.setAttribute("aria-hidden", "true");
  const axisSpacer = document.createElement("span");
  axisSpacer.className = "sequences-axis__label";
  axis.appendChild(axisSpacer);

  const ticks = document.createElement("div");
  ticks.className = "sequences-axis__ticks";
  const tickStep = 2;
  for (let month = 0; month <= totalMonths; month += tickStep) {
    const tick = document.createElement("span");
    tick.className = "sequences-axis__tick";
    tick.style.left = `${(month / totalMonths) * 100}%`;
    tick.textContent = String(month);
    ticks.appendChild(tick);
  }
  if (totalMonths % tickStep !== 0) {
    const endTick = document.createElement("span");
    endTick.className = "sequences-axis__tick sequences-axis__tick--end";
    endTick.style.left = "100%";
    endTick.textContent = String(totalMonths);
    ticks.appendChild(endTick);
  }
  axis.appendChild(ticks);
  chart.appendChild(axis);

  const rows = document.createElement("div");
  rows.className = "sequences-rows";

  for (const seq of sequences) {
    const row = document.createElement("div");
    row.className = "sequence-row";
    row.dataset.sequence = String(seq.sequence);

    const label = document.createElement("p");
    label.className = "sequence-row__label";
    label.textContent = `Sequence ${seq.sequence}`;
    row.appendChild(label);

    const track = document.createElement("div");
    track.className = "sequence-row__track";
    track.style.gridTemplateColumns = `repeat(${totalMonths}, minmax(0, 1fr))`;

    for (const phase of seq.phases) {
      const bar = document.createElement("div");
      bar.className = `sequence-bar sequence-bar--${phaseClass(phase.phase)}`;
      bar.style.gridColumn = `${phase.start + 1} / ${phase.end + 1}`;
      bar.style.background = data.colors[phase.phase] ?? "#999999";
      bar.title = `${phase.phase}: ${monthsLabel(phase.months)}`;
      const text = document.createElement("span");
      text.className = "sequence-bar__text";
      text.textContent = barLabel(phase);
      bar.appendChild(text);
      track.appendChild(bar);
    }

    row.appendChild(track);
    rows.appendChild(row);
  }

  chart.appendChild(rows);

  const axisTitle = document.createElement("p");
  axisTitle.className = "sequences-axis-title";
  axisTitle.textContent = "Study month";
  chart.appendChild(axisTitle);

  return chart;
}

export function createSequencesLegend(data: TrialDesignData = trialDesign): HTMLElement {
  const el = document.createElement("div");
  el.className = "wedge-legend sequences-legend";
  el.setAttribute("aria-hidden", "true");
  for (const phase of data.legend) {
    const item = document.createElement("span");
    item.className = "wedge-legend__item";
    const swatch = document.createElement("span");
    swatch.className = "wedge-legend__swatch";
    swatch.style.background = data.colors[phase] ?? "#999999";
    const label = document.createElement("span");
    label.className = "wedge-legend__label";
    label.textContent = phase === "Transition" ? "Training (1 month)" : phase;
    item.append(swatch, label);
    el.appendChild(item);
  }
  return el;
}
