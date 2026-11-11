import { selectDesignBatch, trialDesign } from "./figure-data";
import { implementationSequences } from "./sequences";
import { createWedgeLegend } from "./stepped-wedge";

export function renderBatchTimeline(startDate: string): string {
  const [year, month, day] = startDate.split("-").map(Number);
  const data = selectDesignBatch(trialDesign, 3);
  const sequences = implementationSequences(data).reverse();
  const totalMonths = sequences[0].totalMonths;
  const dateLabel = (offset: number) => {
    const date = new Date(Date.UTC(year, month - 1 + offset, day));
    return `${date.toLocaleDateString("en-GB", { day: "numeric", month: "short", timeZone: "UTC" })}<br>${date.getUTCFullYear()}`;
  };
  return `
    <div class="batch-timeline" data-animate aria-label="Five hospital clusters, each with 13 months of data collection, ordered from cluster 5 to cluster 1">
      <div class="batch-timeline__legend">${createWedgeLegend(data).outerHTML}</div>
      <div class="batch-timeline__axis">
        <span></span>
        <div class="batch-timeline__dates">
          ${[0, 4, 8, totalMonths].map(offset => `<span style="left: ${offset / totalMonths * 100}%">${dateLabel(offset)}</span>`).join("")}
        </div>
      </div>
      ${sequences.map(sequence => `
        <div class="batch-timeline__row">
          <span class="batch-timeline__label">Cluster ${sequence.sequence}</span>
          <div class="batch-timeline__track">
            ${sequence.phases.map(phase => `<span class="batch-timeline__phase" style="left: ${(phase.start + data.geometry.xPadding) / totalMonths * 100}%; width: ${(phase.months - 2 * data.geometry.xPadding) / totalMonths * 100}%; background: ${data.colors[phase.phase]}; opacity: 0.92" title="${phase.phase === "Transition" ? "Training" : phase.phase}: ${phase.months} months"></span>`).join("")}
          </div>
        </div>`).join("")}
    </div>`;
}
