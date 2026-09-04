import { animate } from "motion";
import { metaAnalysis, type MetaAnalysisData } from "./figure-data";
import { poolRandomEffects, sameStudySet, type PooledEstimate } from "./pool-meta";

const ROW_HEIGHT = 18;
const LABEL_WIDTH = 186;
const N_WIDTH = 52;
const FOREST_WIDTH = 260;
const EFFECT_WIDTH = 96;
const PAD_LEFT = 4;
const PAD_TOP = 22;
const AXIS_HEIGHT = 28;
const DIAMOND_H = 7;

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

function diamondPoints(
  logRr: number,
  ciLower: number,
  ciUpper: number,
  y: number,
  xlim: [number, number],
  forestX: number
): string {
  const px = logToX(logRr, xlim, forestX);
  const pLo = Math.max(logToX(Math.log(ciLower), xlim, forestX), forestX);
  const pHi = Math.min(logToX(Math.log(ciUpper), xlim, forestX), forestX + FOREST_WIDTH);
  return `${pLo},${y} ${px},${y - DIAMOND_H} ${pHi},${y} ${px},${y + DIAMOND_H}`;
}

function footerText(pooled: PooledEstimate, measure: string): string {
  return (
    `Random-effects ${measure} ${pooled.rrFormatted} ` +
    `(95% CI ${pooled.ciFormatted.replace("; ", "–")}); ` +
    `I² ${pooled.i2Rounded.toFixed(2)}; ` +
    `${pooled.numberOfStudies} ${pooled.numberOfStudies === 1 ? "study" : "studies"}`
  );
}

/** Compact filters for live presentation use. */
const DESIGN_FILTERS: { label: string; match: (design: string) => boolean }[] = [
  {
    label: "RCT",
    match: (design) => /randomi[sz]ed|cluster randomised|cluster randomized|\bRCT\b/i.test(design),
  },
  {
    label: "Prospective",
    match: (design) => /prospective/i.test(design) || design === "Cohort study",
  },
  {
    label: "Retrospective",
    match: (design) => /retrospective/i.test(design),
  },
  {
    label: "Registry",
    match: (design) => /registry/i.test(design),
  },
];

/** Preferred chip order for training-programme filters. */
const PROGRAMME_ORDER = ["ATLS", "PTC", "RTTDC", "JATEC", "CTCT"];

function programmeFilters(
  studies: MetaAnalysisData["studies"]
): { label: string; keys: string[] }[] {
  const byProgramme = new Map<string, string[]>();
  for (const study of studies) {
    const programme = study.programme?.trim() || "Other";
    const keys = byProgramme.get(programme) ?? [];
    keys.push(study.citationKey);
    byProgramme.set(programme, keys);
  }
  const ordered = [
    ...PROGRAMME_ORDER.filter((name) => byProgramme.has(name)),
    ...[...byProgramme.keys()].filter((name) => !PROGRAMME_ORDER.includes(name)).sort(),
  ];
  return ordered.map((label) => ({ label, keys: byProgramme.get(label)! }));
}

export interface ForestPlotController {
  element: HTMLElement;
  setIncluded(keys: Iterable<string>): void;
  includeAll(): void;
  getIncluded(): Set<string>;
  /** Reveal studies oldest→newest, updating the pooled estimate after each. */
  playChronologicalReveal(options?: { stepMs?: number }): Promise<void>;
  abortReveal(): void;
}

/**
 * Interactive forest plot: click a study row to include/exclude it.
 * The pooled diamond and footer update from an inverse-variance
 * DerSimonian–Laird random-effects model. When every study is included,
 * the R-exported REML pooled estimate is shown so the full set matches
 * the protocol figure.
 */
export function createForestPlot(data: MetaAnalysisData = metaAnalysis): ForestPlotController {
  const allKeys = data.studies.map((s) => s.citationKey);
  // Start empty so the entrance reveal can build the pool chronologically.
  const included = new Set<string>();
  let revealToken = 0;
  let revealing = false;
  const maxWeight = Math.max(...data.studies.map((s) => s.weightPercent));
  const forestX = PAD_LEFT + LABEL_WIDTH + N_WIDTH;
  const [xMin, xMax] = data.logScaleXlim;
  const totalWidth = PAD_LEFT + LABEL_WIDTH + N_WIDTH + FOREST_WIDTH + EFFECT_WIDTH + 8;
  const totalHeight = PAD_TOP + (data.studies.length + 1) * ROW_HEIGHT + AXIS_HEIGHT + 8;
  const pooledY = PAD_TOP + data.studies.length * ROW_HEIGHT + ROW_HEIGHT / 2;
  const nullX = logToX(0, data.logScaleXlim, forestX);

  const panel = document.createElement("div");
  panel.className = "forest-panel";

  const controls = document.createElement("div");
  controls.className = "forest-controls";
  controls.setAttribute("data-animate", "");

  const allBtn = document.createElement("button");
  allBtn.type = "button";
  allBtn.className = "forest-chip";
  allBtn.textContent = "All studies";
  allBtn.dataset.filter = "all";
  allBtn.addEventListener("click", (e) => {
    e.stopPropagation();
    abortReveal();
    includeAll();
  });
  controls.appendChild(allBtn);

  const timelineBtn = document.createElement("button");
  timelineBtn.type = "button";
  timelineBtn.className = "forest-chip";
  timelineBtn.dataset.filter = "timeline";
  timelineBtn.textContent = "Play timeline";
  timelineBtn.title = "Add studies in chronological order and watch the pooled estimate change";
  timelineBtn.addEventListener("click", (e) => {
    e.stopPropagation();
    void playChronologicalReveal();
  });
  controls.appendChild(timelineBtn);

  const designGroup = document.createElement("div");
  designGroup.className = "forest-chip-group";
  designGroup.setAttribute("role", "group");
  designGroup.setAttribute("aria-label", "Filter by study design");
  for (const filter of DESIGN_FILTERS) {
    const keys = data.studies.filter((s) => filter.match(s.design)).map((s) => s.citationKey);
    if (keys.length === 0) continue;
    const btn = document.createElement("button");
    btn.type = "button";
    btn.className = "forest-chip";
    btn.dataset.filter = filter.label;
    btn.dataset.filterKind = "design";
    btn.textContent = filter.label;
    btn.title = `Include only ${filter.label.toLowerCase()} studies`;
    btn.addEventListener("click", (e) => {
      e.stopPropagation();
      abortReveal();
      setIncluded(keys);
    });
    designGroup.appendChild(btn);
  }
  if (designGroup.childElementCount > 0) controls.appendChild(designGroup);

  const programmeGroup = document.createElement("div");
  programmeGroup.className = "forest-chip-group";
  programmeGroup.setAttribute("role", "group");
  programmeGroup.setAttribute("aria-label", "Filter by training programme");
  const programmes = programmeFilters(data.studies);
  for (const filter of programmes) {
    const btn = document.createElement("button");
    btn.type = "button";
    btn.className = "forest-chip";
    btn.dataset.filter = filter.label;
    btn.dataset.filterKind = "programme";
    btn.textContent = filter.label;
    btn.title = `Include only ${filter.label} studies`;
    btn.addEventListener("click", (e) => {
      e.stopPropagation();
      abortReveal();
      setIncluded(filter.keys);
    });
    programmeGroup.appendChild(btn);
  }
  if (programmeGroup.childElementCount > 0) controls.appendChild(programmeGroup);

  const hint = document.createElement("p");
  hint.className = "forest-hint";
  hint.textContent = "Click a study to include or exclude it";
  controls.appendChild(hint);
  panel.appendChild(controls);

  const svg = svgEl("svg", {
    viewBox: `0 0 ${totalWidth} ${totalHeight}`,
    class: "forest-plot-chart",
    role: "img",
  }) as SVGSVGElement;

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

  const rowEls = new Map<string, SVGGElement>();

  data.studies.forEach((study, i) => {
    const y = PAD_TOP + i * ROW_HEIGHT + ROW_HEIGHT / 2;
    const row = svgEl("g", {
      class: "forest-row forest-row--interactive",
      "data-citation-key": study.citationKey,
      tabindex: 0,
      role: "button",
      "aria-pressed": "true",
    }) as SVGGElement;

    const hit = svgEl("rect", {
      class: "forest-row-hit",
      x: 0,
      y: y - ROW_HEIGHT / 2,
      width: totalWidth,
      height: ROW_HEIGHT,
      fill: "transparent",
    });

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

    const effect = svgEl("text", {
      x: forestX + FOREST_WIDTH + 8,
      y: y + 4,
      class: "forest-effect",
    });
    effect.textContent = `${formatRr(study.rr)} (${formatRr(study.ciLower)}–${formatRr(study.ciUpper)})`;

    row.append(
      hit,
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
      }),
      effect
    );

    const toggle = (e: Event) => {
      e.stopPropagation();
      e.preventDefault();
      abortReveal();
      if (included.has(study.citationKey)) included.delete(study.citationKey);
      else included.add(study.citationKey);
      refresh();
    };
    row.addEventListener("click", toggle);
    row.addEventListener("keydown", (e) => {
      if (e.key === "Enter" || e.key === " ") toggle(e);
    });

    rowEls.set(study.citationKey, row);
    svg.appendChild(row);
  });

  const pooledRow = svgEl("g", { class: "forest-row forest-row--pooled" });
  const pooledName = svgEl("text", {
    x: PAD_LEFT,
    y: pooledY + 4,
    class: "forest-study forest-study--pooled",
  });
  pooledName.textContent = data.pooled.label;
  const diamond = svgEl("polygon", {
    class: "forest-diamond",
    points: diamondPoints(
      data.pooled.logRr,
      data.pooled.ciLower,
      data.pooled.ciUpper,
      pooledY,
      data.logScaleXlim,
      forestX
    ),
    fill: data.pooled.color,
    stroke: "#12788f",
    "stroke-width": 0.4,
  }) as SVGPolygonElement;
  const pooledEffect = svgEl("text", {
    x: forestX + FOREST_WIDTH + 8,
    y: pooledY + 4,
    class: "forest-effect forest-effect--pooled",
  }) as SVGTextElement;
  pooledEffect.textContent = `${data.pooled.rrFormatted} (${data.pooled.ciFormatted.replace("; ", "–")})`;
  const emptyNote = svgEl("text", {
    x: forestX + FOREST_WIDTH / 2,
    y: pooledY + 4,
    class: "forest-empty",
    "text-anchor": "middle",
  }) as SVGTextElement;
  emptyNote.textContent = "Select at least one study";
  emptyNote.style.display = "none";
  pooledRow.append(pooledName, diamond, pooledEffect, emptyNote);
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

  const chartWrap = document.createElement("div");
  chartWrap.className = "forest-container";
  chartWrap.setAttribute("data-animate", "");
  chartWrap.appendChild(svg);
  panel.appendChild(chartWrap);

  const footer = document.createElement("p");
  footer.className = "slide-footer forest-footer";
  footer.setAttribute("data-animate", "");
  panel.appendChild(footer);

  function currentPooled(): PooledEstimate | null {
    if (sameStudySet(included, allKeys)) {
      return {
        logRr: data.pooled.logRr,
        seLogRr: data.pooled.seLogRr,
        rr: data.pooled.rr,
        ciLower: data.pooled.ciLower,
        ciUpper: data.pooled.ciUpper,
        rrFormatted: data.pooled.rrFormatted,
        ciFormatted: data.pooled.ciFormatted,
        i2: data.pooled.i2,
        i2Rounded: data.pooled.i2Rounded,
        tau2: data.pooled.tau2,
        numberOfStudies: data.pooled.numberOfStudies,
        q: 0,
      };
    }
    const selected = data.studies.filter((s) => included.has(s.citationKey));
    return poolRandomEffects(selected);
  }

  function syncChips(): void {
    const chips = [...controls.querySelectorAll<HTMLButtonElement>(".forest-chip")];
    for (const chip of chips) {
      chip.classList.remove("is-active");
    }
    if (revealing) {
      timelineBtn.classList.add("is-active");
      return;
    }
    if (sameStudySet(included, allKeys)) {
      allBtn.classList.add("is-active");
      return;
    }
    for (const filter of DESIGN_FILTERS) {
      const keys = data.studies.filter((s) => filter.match(s.design)).map((s) => s.citationKey);
      if (keys.length > 0 && sameStudySet(included, keys)) {
        const chip = chips.find(
          (c) => c.dataset.filterKind === "design" && c.dataset.filter === filter.label
        );
        chip?.classList.add("is-active");
        return;
      }
    }
    for (const filter of programmes) {
      if (sameStudySet(included, filter.keys)) {
        const chip = chips.find(
          (c) => c.dataset.filterKind === "programme" && c.dataset.filter === filter.label
        );
        chip?.classList.add("is-active");
        return;
      }
    }
  }

  function refresh(): void {
    for (const study of data.studies) {
      const row = rowEls.get(study.citationKey);
      if (!row) continue;
      const on = included.has(study.citationKey);
      row.classList.toggle("is-excluded", !on);
      row.setAttribute("aria-pressed", on ? "true" : "false");
    }

    const pooled = currentPooled();
    if (!pooled) {
      diamond.style.display = "none";
      pooledEffect.style.display = "none";
      emptyNote.style.display = "";
      emptyNote.textContent = revealing
        ? "Adding studies chronologically…"
        : "Select at least one study";
      footer.textContent = revealing
        ? "Building the pooled estimate as studies appear (oldest → newest)"
        : "No studies selected — click a study to include it";
      svg.setAttribute("aria-label", "Forest plot with no studies selected");
    } else {
      diamond.style.display = "";
      pooledEffect.style.display = "";
      emptyNote.style.display = "none";
      diamond.setAttribute(
        "points",
        diamondPoints(
          pooled.logRr,
          pooled.ciLower,
          pooled.ciUpper,
          pooledY,
          data.logScaleXlim,
          forestX
        )
      );
      diamond.classList.remove("is-updating");
      // Force reflow so the pulse can replay on each update.
      void diamond.getBoundingClientRect();
      diamond.classList.add("is-updating");
      pooledEffect.textContent = `${pooled.rrFormatted} (${pooled.ciFormatted.replace("; ", "–")})`;
      footer.textContent = footerText(pooled, data.measure);
      svg.setAttribute(
        "aria-label",
        `Forest plot of trauma life support training effect on mortality. Pooled odds ratio ${pooled.rrFormatted}, 95% CI ${pooled.ciFormatted}, ${pooled.numberOfStudies} studies`
      );
    }
    syncChips();
  }

  function abortReveal(): void {
    revealToken += 1;
    revealing = false;
    panel.classList.remove("is-revealing");
    timelineBtn.disabled = false;
  }

  function delay(ms: number): Promise<void> {
    return new Promise((resolve) => {
      window.setTimeout(resolve, ms);
    });
  }

  async function playChronologicalReveal(options: { stepMs?: number } = {}): Promise<void> {
    const stepMs = options.stepMs ?? 420;
    if (window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
      includeAll();
      return;
    }

    const token = ++revealToken;
    revealing = true;
    panel.classList.add("is-revealing");
    timelineBtn.disabled = true;
    included.clear();
    refresh();
    await delay(280);
    if (token !== revealToken) return;

    // Studies are already ordered by year in the R export.
    for (const study of data.studies) {
      if (token !== revealToken) return;
      included.add(study.citationKey);
      refresh();

      const row = rowEls.get(study.citationKey);
      if (row) {
        row.style.opacity = "0";
        await animate(
          row,
          { opacity: [0, 1] } as Record<string, unknown>,
          { duration: 0.32, ease: "easeOut" }
        );
        row.style.opacity = "";
      }

      await delay(stepMs);
      if (token !== revealToken) return;
    }

    if (token !== revealToken) return;
    revealing = false;
    panel.classList.remove("is-revealing");
    timelineBtn.disabled = false;
    // Final paint uses the R-exported REML pooled estimate.
    refresh();
  }

  function setIncluded(keys: Iterable<string>): void {
    abortReveal();
    included.clear();
    for (const key of keys) included.add(key);
    refresh();
  }

  function includeAll(): void {
    setIncluded(allKeys);
  }

  refresh();

  return {
    element: panel,
    setIncluded,
    includeAll,
    getIncluded: () => new Set(included),
    playChronologicalReveal,
    abortReveal,
  };
}
