import { animate, type AnimationPlaybackControls } from "motion";
import type { TrialDesignData } from "./figure-data";
import {
  createSteppedWedgeSvg,
  createWedgeLegend,
  fitSvgInContainer,
  focusViewBox,
  layoutClusterPhases,
  lerpViewBox,
  restoreClusterPhases,
  setRevealMonth,
  siteSchematicSpans,
  syncAxisToStage,
  viewBoxString,
  type DesignFocusStage,
  type WedgeViewBox,
} from "./stepped-wedge";

export type DesignRevealStage = DesignFocusStage;

export interface DesignRevealControls {
  toggle: () => void;
  pause: () => void;
  resume: () => void;
  goToStage: (stage: DesignRevealStage) => void;
  isPaused: () => boolean;
  isComplete: () => boolean;
  cancel: () => void;
}

const STAGES: Array<{
  id: DesignRevealStage;
  label: string;
  caption: string;
  holdMs: number;
}> = [
  {
    id: "site",
    label: "One cluster",
    caption: "One hospital — three phases of care",
    // Phase pauses are manual (Space / play); no timed hold before the next stage.
    holdMs: 0,
  },
  {
    id: "batch",
    label: "First batch",
    caption: "One batch — five hospitals, randomised transition months",
    // Pause after this stage; Right Arrow / Full trial continues.
    holdMs: 0,
  },
  {
    id: "full",
    label: "Full trial",
    caption: "Six batches — 30 hospitals over 48 months",
    holdMs: 0,
  },
];

const SITE_PHASE_IDS = ["standard-care", "transition", "intervention"] as const;

type SitePhaseId = (typeof SITE_PHASE_IDS)[number];

const SITE_PHASE_COPY: Record<SitePhaseId, { caption: string; duration: number }> = {
  "standard-care": { caption: "Standard care", duration: 4 },
  transition: { caption: "ATLS® course", duration: 2 },
  intervention: { caption: "After training", duration: 4 },
};

function reduceMotion(): boolean {
  return window.matchMedia("(prefers-reduced-motion: reduce)").matches;
}

function setCaption(el: HTMLElement | null, text: string): void {
  if (el) el.textContent = text;
}

function setStageButtons(root: HTMLElement, active: DesignRevealStage | null, complete: boolean): void {
  const activeIndex = STAGES.findIndex((s) => s.id === active);
  root.querySelectorAll<HTMLButtonElement>("[data-stage]").forEach((btn) => {
    const id = btn.dataset.stage as DesignRevealStage;
    const index = STAGES.findIndex((s) => s.id === id);
    btn.disabled = false;
    btn.classList.toggle("is-active", id === active);
    btn.classList.toggle("is-done", complete || (activeIndex >= 0 && index < activeIndex));
    // The is-active class alone conveys state to sighted users only.
    btn.setAttribute("aria-pressed", id === active ? "true" : "false");
  });
}

function setPlayPauseButton(btn: HTMLButtonElement | null, paused: boolean, complete: boolean): void {
  if (!btn) return;
  btn.disabled = false;
  btn.setAttribute("aria-pressed", paused ? "true" : "false");
  btn.dataset.state = complete ? "done" : paused ? "paused" : "playing";
  btn.title = complete
    ? "Replay from start (Space)"
    : paused
      ? "Resume (Space)"
      : "Pause (Space)";
  btn.setAttribute("aria-label", btn.title);
  btn.innerHTML = complete
    ? `<svg width="14" height="14" viewBox="0 0 14 14" aria-hidden="true"><path fill="currentColor" d="M3 2.5v9l9-4.5L3 2.5z"/></svg>`
    : paused
      ? `<svg width="14" height="14" viewBox="0 0 14 14" aria-hidden="true"><path fill="currentColor" d="M3 2.5v9l9-4.5L3 2.5z"/></svg>`
      : `<svg width="14" height="14" viewBox="0 0 14 14" aria-hidden="true"><path fill="currentColor" d="M3 2h3v10H3V2zm5 0h3v10H8V2z"/></svg>`;
}

async function wait(ms: number, shouldContinue: () => Promise<void>): Promise<void> {
  const step = 40;
  let elapsed = 0;
  while (elapsed < ms) {
    await shouldContinue();
    await new Promise((r) => setTimeout(r, step));
    elapsed += step;
  }
}

function parseViewBox(svg: SVGSVGElement): WedgeViewBox {
  const parts = (svg.getAttribute("viewBox") ?? "0 0 0 0").split(/\s+/).map(Number);
  return { x: parts[0] || 0, y: parts[1] || 0, w: parts[2] || 0, h: parts[3] || 0 };
}

export function startDesignReveal(
  slideEl: HTMLElement,
  data: TrialDesignData
): DesignRevealControls {
  const captionEl = slideEl.querySelector<HTMLElement>(".wedge-caption");
  const playPauseBtn = slideEl.querySelector<HTMLButtonElement>(".wedge-play-pause");
  const stageBar = slideEl.querySelector<HTMLElement>(".wedge-stages") ?? slideEl;
  const mount = slideEl.querySelector<HTMLElement>("[data-wedge-mount]");
  const legendMount = slideEl.querySelector<HTMLElement>(".wedge-legend-mount");
  const phaseCallouts = slideEl.querySelector<HTMLElement>(".wedge-phase-callouts");
  const chartStack = slideEl.querySelector<HTMLElement>(".wedge-chart-stack");
  const bullets = Array.from(slideEl.querySelectorAll<HTMLElement>(".design-bullets li"));

  let paused = false;
  let complete = false;
  let cancelled = false;
  let activeStage: DesignRevealStage | null = null;
  /** Overrides the stage caption while stepping through one-site phases. */
  let sitePhaseCaption: string | null = null;
  /** Callouts unlock only when that phase's wipe starts (after play), not at the prior pause. */
  const unlockedSitePhases = new Set<SitePhaseId>();
  let runToken = 0;
  const running: AnimationPlaybackControls[] = [];
  const listenerAbort = new AbortController();
  const { signal } = listenerAbort;

  if (!mount) {
    return {
      toggle() {},
      pause() {},
      resume() {},
      goToStage() {},
      isPaused: () => false,
      isComplete: () => true,
      cancel() {},
    };
  }

  if (legendMount) {
    legendMount.replaceChildren(createWedgeLegend(data));
  }

  const svg = createSteppedWedgeSvg(data);
  // Keep phase callouts as the first child; chart follows below them.
  mount.querySelectorAll("svg").forEach((node) => node.remove());
  mount.appendChild(svg);

  const shortEnd = Math.max(data.parameters.totalMonths, 13);
  const fullEnd = data.xMax;
  const schematic = siteSchematicSpans(data);
  const sitePhaseScrubs = SITE_PHASE_IDS.map((id) => ({
    id,
    endMonth: schematic[id].end,
    duration: SITE_PHASE_COPY[id].duration,
    caption: SITE_PHASE_COPY[id].caption,
  }));

  const applySiteSchematic = () => {
    layoutClusterPhases(svg, data, 1, schematic);
  };

  const restoreRealClusterOne = () => {
    restoreClusterPhases(svg, data, 1);
  };

  const rows = () => Array.from(svg.querySelectorAll<SVGGElement>(".wedge-row"));
  const batchLabels = () => Array.from(svg.querySelectorAll<SVGTextElement>(".wedge-batch-label"));

  /** Keep the phase grid the same width as the fitted SVG so columns line up with bars. */
  const layoutPhaseCallouts = () => {
    if (!phaseCallouts) return;
    const svgWidth = svg.getBoundingClientRect().width;
    if (svgWidth <= 0) return;
    phaseCallouts.style.width = `${svgWidth}px`;
  };

  const calloutReserve = () => {
    if (!phaseCallouts || phaseCallouts.hidden) return 0;
    return phaseCallouts.getBoundingClientRect().height + 8;
  };

  const setCamera = (box: WedgeViewBox, stage?: DesignRevealStage) => {
    svg.setAttribute("viewBox", viewBoxString(box));
    const resolved = stage ?? ((svg.dataset.stage as DesignRevealStage) || undefined);
    if (stage) {
      svg.dataset.stage = stage;
      syncAxisToStage(svg, data, stage);
      chartStack?.setAttribute("data-stage", stage);
      mount.dataset.stage = stage;
    }
    fitSvgInContainer(svg, box, mount, {
      preferWidth: true,
      reserveTop: resolved === "site" ? calloutReserve() : 0,
    });
    layoutPhaseCallouts();
  };

  const phaseIsActive = (phaseId: SitePhaseId, month: number): boolean => {
    const span = schematic[phaseId];
    const eps = 0.15;
    if (phaseId === "intervention") return month >= span.start - eps;
    return month >= span.start - eps && month < span.end + eps;
  };

  const syncPhaseCallouts = (month: number, forceAll = false) => {
    if (!phaseCallouts) return;
    const showOverlay = activeStage === "site" && !complete;
    if (!showOverlay) {
      phaseCallouts.querySelectorAll<HTMLElement>(".wedge-phase").forEach((el) => {
        el.classList.remove("is-revealed", "is-active");
        el.setAttribute("aria-hidden", "true");
      });
      return;
    }

    for (const phaseId of SITE_PHASE_IDS) {
      const el = phaseCallouts.querySelector<HTMLElement>(`.wedge-phase[data-phase="${phaseId}"]`);
      if (!el) continue;
      const revealed = forceAll || unlockedSitePhases.has(phaseId);
      const active = forceAll || (revealed && phaseIsActive(phaseId, month));
      el.classList.toggle("is-revealed", revealed);
      el.classList.toggle("is-active", active);
      el.setAttribute("aria-hidden", revealed ? "false" : "true");
    }
  };

  const setReveal = (month: number, forceAllPhases = false) => {
    setRevealMonth(svg, month);
    syncPhaseCallouts(month, forceAllPhases);
  };

  setReveal(0);
  layoutPhaseCallouts();

  const syncUi = () => {
    const stageCaption = STAGES.find((s) => s.id === activeStage)?.caption ?? "";
    setCaption(captionEl, sitePhaseCaption ?? stageCaption);
    setStageButtons(stageBar, activeStage, complete);
    setPlayPauseButton(playPauseBtn, paused, complete);
    const showPhases = activeStage === "site" && !complete;
    if (phaseCallouts) {
      phaseCallouts.hidden = !showPhases;
      phaseCallouts.setAttribute("aria-hidden", showPhases ? "false" : "true");
      if (showPhases) void phaseCallouts.offsetHeight; // force layout before measuring reserve
    }
    if (legendMount) {
      legendMount.hidden = showPhases;
      legendMount.setAttribute("aria-hidden", showPhases ? "true" : "false");
    }
    // Re-fit after callouts show/hide so reserveTop stays accurate.
    setCamera(parseViewBox(svg), activeStage ?? undefined);
    bullets.forEach((li, i) => {
      const stageIndex = STAGES.findIndex((s) => s.id === activeStage);
      li.classList.toggle("is-emphasis", !complete && stageIndex === i);
      li.classList.toggle("is-muted", !complete && stageIndex >= 0 && i > stageIndex);
    });
  };

  const stopRunning = () => {
    running.forEach((a) => {
      try {
        a.stop();
      } catch {
        /* ignore */
      }
    });
    running.length = 0;
  };

  const pauseAnimations = () => {
    running.forEach((a) => {
      try {
        a.pause();
      } catch {
        /* ignore */
      }
    });
  };

  const resumeAnimations = () => {
    running.forEach((a) => {
      try {
        a.play();
      } catch {
        /* ignore */
      }
    });
  };

  const gate = async (token: number) => {
    while (paused && !cancelled && token === runToken) {
      await new Promise((r) => setTimeout(r, 40));
    }
    if (cancelled || token !== runToken) throw new Error("cancelled");
  };

  const track = (controls: AnimationPlaybackControls, token: number) => {
    if (token !== runToken) {
      try {
        controls.stop();
      } catch {
        /* ignore */
      }
      return Promise.resolve();
    }
    running.push(controls);
    if (paused) {
      try {
        controls.pause();
      } catch {
        /* ignore */
      }
    }
    return controls.finished.finally(() => {
      const idx = running.indexOf(controls);
      if (idx >= 0) running.splice(idx, 1);
    });
  };

  // Start cropped to one site so the first paint is not the full 0–48 axis.
  applySiteSchematic();
  setCamera(focusViewBox(data, "site"), "site");
  // Re-fit after layout — first paint often has a 0×0 container.
  requestAnimationFrame(() => {
    if (!cancelled) setCamera(parseViewBox(svg), (svg.dataset.stage as DesignRevealStage) || "site");
  });

  const resizeObserver = new ResizeObserver(() => {
    setCamera(parseViewBox(svg), (svg.dataset.stage as DesignRevealStage) || undefined);
    layoutPhaseCallouts();
  });
  resizeObserver.observe(mount);
  signal.addEventListener("abort", () => resizeObserver.disconnect());

  const morphCamera = async (toStage: DesignRevealStage, token: number, duration = 1.2) => {
    const from = parseViewBox(svg);
    const to = focusViewBox(data, toStage);
    syncAxisToStage(svg, data, toStage);
    svg.dataset.stage = toStage;
    if (reduceMotion() || duration <= 0) {
      setCamera(to, toStage);
      return;
    }
    await gate(token);
    await track(
      animate(0, 1, {
        duration,
        ease: [0.22, 1, 0.36, 1],
        onUpdate: (t) => setCamera(lerpViewBox(from, to, t)),
      }),
      token
    );
    setCamera(to, toStage);
  };

  const setRowVisibility = (clusterIds: number[], visible: boolean) => {
    const idSet = new Set(clusterIds);
    rows().forEach((row) => {
      if (!idSet.has(Number(row.dataset.cluster))) return;
      row.style.opacity = visible ? "1" : "0";
      row.setAttribute("opacity", visible ? "1" : "0");
    });
  };

  const hideAllRows = () => {
    rows().forEach((row) => {
      row.style.opacity = "0";
      row.setAttribute("opacity", "0");
    });
    batchLabels().forEach((label) => label.setAttribute("opacity", "0"));
  };

  const showBatchLabels = (batches: number[]) => {
    const set = new Set(batches);
    batchLabels().forEach((label) => {
      label.setAttribute("opacity", set.has(Number(label.dataset.batch)) ? "1" : "0");
    });
  };

  /** Continuous left→right wipe through study months. */
  const scrubTimeline = async (
    fromMonth: number,
    toMonth: number,
    token: number,
    duration: number
  ) => {
    if (reduceMotion() || duration <= 0) {
      setReveal(toMonth, toMonth >= shortEnd - 0.05);
      return;
    }
    await gate(token);
    await track(
      animate(fromMonth, toMonth, {
        duration,
        ease: "linear",
        onUpdate: (month) => setReveal(month),
      }),
      token
    );
    setReveal(toMonth, toMonth >= shortEnd - 0.05);
  };

  /** Wipe the plot clear before changing camera framing. */
  const clearPlot = async (token: number, animateIn: boolean) => {
    const current = Number(svg.dataset.revealMonth ?? 0);
    if (current <= 0.05) {
      setReveal(0);
      return;
    }
    if (!animateIn || reduceMotion()) {
      setReveal(0);
      return;
    }
    const duration = Math.min(1.8, 0.55 + (current / fullEnd) * 1.1);
    await scrubTimeline(current, 0, token, duration);
  };

  const animateSite = async (token: number, animateIn: boolean) => {
    activeStage = "site";
    complete = false;
    sitePhaseCaption = null;
    unlockedSitePhases.clear();
    applySiteSchematic();
    syncUi();

    await clearPlot(token, animateIn);
    await morphCamera("site", token, animateIn ? 0.9 : 0);
    hideAllRows();
    setRowVisibility([1], true);
    showBatchLabels([1]);

    if (!animateIn || reduceMotion()) {
      unlockedSitePhases.clear();
      for (const phaseId of SITE_PHASE_IDS) unlockedSitePhases.add(phaseId);
      setReveal(shortEnd, true);
      sitePhaseCaption = null;
      return;
    }

    let fromMonth = 0;
    for (let i = 0; i < sitePhaseScrubs.length; i++) {
      const phase = sitePhaseScrubs[i];
      sitePhaseCaption = phase.caption;
      // Unlock the illustration only as this phase's wipe begins (after play).
      unlockedSitePhases.add(phase.id);
      syncUi();
      setReveal(fromMonth);
      await scrubTimeline(fromMonth, phase.endMonth, token, phase.duration);
      fromMonth = phase.endMonth;

      // Pause after every phase — including the last — so Space / play advances.
      paused = true;
      syncUi();
      await gate(token);
    }

    sitePhaseCaption = null;
  };

  const animateBatch = async (token: number, animateIn: boolean) => {
    activeStage = "batch";
    complete = false;
    sitePhaseCaption = null;
    syncUi();

    const batchClusters = Array.from({ length: data.parameters.clustersPerBatch }, (_, i) => i + 1);

    await clearPlot(token, animateIn);
    restoreRealClusterOne();
    await morphCamera("batch", token, animateIn ? 1.1 : 0);
    setRowVisibility(batchClusters, true);
    showBatchLabels([1]);
    // Hide clusters outside this batch while the shared timeline plays.
    rows().forEach((row) => {
      if (!batchClusters.includes(Number(row.dataset.cluster))) {
        row.style.opacity = "0";
        row.setAttribute("opacity", "0");
      }
    });

    if (!animateIn || reduceMotion()) {
      setReveal(shortEnd);
      return;
    }
    await scrubTimeline(0, shortEnd, token, 5);
  };

  const animateFull = async (token: number, animateIn: boolean) => {
    activeStage = "full";
    complete = false;
    sitePhaseCaption = null;
    syncUi();

    const allClusters = rows().map((r) => Number(r.dataset.cluster));

    await clearPlot(token, animateIn);
    restoreRealClusterOne();
    await morphCamera("full", token, animateIn ? 1.4 : 0);
    setRowVisibility(allClusters, true);
    showBatchLabels(Array.from({ length: data.parameters.batches }, (_, i) => i + 1));

    if (!animateIn || reduceMotion()) {
      setReveal(fullEnd);
      return;
    }
    await scrubTimeline(0, fullEnd, token, 7);
  };

  const animateStage = async (stage: DesignRevealStage, token: number, animateIn: boolean) => {
    if (stage === "site") await animateSite(token, animateIn);
    else if (stage === "batch") await animateBatch(token, animateIn);
    else await animateFull(token, animateIn);
  };

  const finish = () => {
    complete = true;
    paused = false;
    bullets.forEach((li) => li.classList.remove("is-emphasis", "is-muted"));
    syncPhaseCallouts(Number(svg.dataset.revealMonth ?? 0));
    syncUi();
  };

  const runFrom = async (startStage: DesignRevealStage, autoContinue: boolean) => {
    const token = ++runToken;
    stopRunning();
    paused = false;
    complete = false;
    cancelled = false;

    const startIndex = STAGES.findIndex((s) => s.id === startStage);
    const sequence = STAGES.slice(startIndex);

    try {
      if (reduceMotion()) {
        await animateStage("full", token, false);
        finish();
        return;
      }

      for (let i = 0; i < sequence.length; i++) {
        const stage = sequence[i];
        await gate(token);
        await animateStage(stage.id, token, true);

        const isLast = i === sequence.length - 1;
        // Pause after First batch (and after any stage when started via a
        // stage button) so Right Arrow / Full trial must continue the reveal.
        if (!isLast && (stage.id === "batch" || !autoContinue)) {
          paused = true;
          syncUi();
          await gate(token);
          autoContinue = true;
        } else if (!isLast && stage.holdMs > 0) {
          await wait(stage.holdMs, () => gate(token));
        }
      }

      if (token === runToken) finish();
    } catch {
      /* cancelled / superseded */
    }
  };

  const controls: DesignRevealControls = {
    toggle() {
      if (complete) {
        void runFrom("site", true);
        return;
      }
      if (paused) controls.resume();
      else controls.pause();
    },
    pause() {
      if (complete || paused) return;
      paused = true;
      pauseAnimations();
      syncUi();
    },
    resume() {
      if (complete || !paused) return;
      paused = false;
      resumeAnimations();
      syncUi();
    },
    goToStage(stage: DesignRevealStage) {
      void runFrom(stage, false);
    },
    isPaused: () => paused,
    isComplete: () => complete,
    cancel() {
      cancelled = true;
      runToken += 1;
      paused = false;
      stopRunning();
      listenerAbort.abort();
    },
  };

  stageBar.querySelectorAll<HTMLButtonElement>("[data-stage]").forEach((btn) => {
    btn.addEventListener(
      "click",
      () => {
        const stage = btn.dataset.stage as DesignRevealStage;
        if (stage) controls.goToStage(stage);
      },
      { signal }
    );
  });

  playPauseBtn?.addEventListener("click", () => controls.toggle(), { signal });

  syncUi();
  void runFrom("site", true);

  return controls;
}

export function designRevealStageMeta() {
  return STAGES.map(({ id, label }) => ({ id, label }));
}
