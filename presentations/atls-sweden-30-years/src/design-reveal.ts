import { animate, stagger, type AnimationPlaybackControls } from "motion";
import type { TrialDesignData } from "./figure-data";
import { createSteppedWedgeSvg } from "./stepped-wedge";

export type DesignRevealStage = "site" | "batch" | "full";

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
    label: "One site",
    caption: "One hospital — standard care, then transition, then intervention",
    holdMs: 2200,
  },
  {
    id: "batch",
    label: "First batch",
    caption: "First batch — five hospitals in staggered sequences",
    holdMs: 2200,
  },
  {
    id: "full",
    label: "Full trial",
    caption: "Full trial — six batches, 30 hospitals",
    holdMs: 0,
  },
];

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

function visibleClustersForStage(stage: DesignRevealStage, data: TrialDesignData): number[] {
  const { clusters, clustersPerBatch } = data.parameters;
  if (stage === "site") return [1];
  if (stage === "batch") return Array.from({ length: clustersPerBatch }, (_, i) => i + 1);
  return Array.from({ length: clusters }, (_, i) => i + 1);
}

async function wait(ms: number, shouldContinue: () => Promise<void>): Promise<void> {
  const step = 50;
  let elapsed = 0;
  while (elapsed < ms) {
    await shouldContinue();
    await new Promise((r) => setTimeout(r, step));
    elapsed += step;
  }
}

export function startDesignReveal(
  slideEl: HTMLElement,
  data: TrialDesignData
): DesignRevealControls {
  const captionEl = slideEl.querySelector<HTMLElement>(".wedge-caption");
  const playPauseBtn = slideEl.querySelector<HTMLButtonElement>(".wedge-play-pause");
  const stageBar = slideEl.querySelector<HTMLElement>(".wedge-stages") ?? slideEl;
  const mount = slideEl.querySelector<HTMLElement>("#wedge-mount");
  const bullets = Array.from(slideEl.querySelectorAll<HTMLElement>(".design-bullets li"));

  let paused = false;
  let complete = false;
  let cancelled = false;
  let activeStage: DesignRevealStage | null = null;
  let runToken = 0;
  const running: AnimationPlaybackControls[] = [];
  const listenerAbort = new AbortController();
  const { signal } = listenerAbort;

  const syncUi = () => {
    setCaption(captionEl, STAGES.find((s) => s.id === activeStage)?.caption ?? "");
    setStageButtons(stageBar, activeStage, complete);
    setPlayPauseButton(playPauseBtn, paused, complete);
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
      await new Promise((r) => setTimeout(r, 50));
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

  const mountChart = (stage: DesignRevealStage) => {
    if (!mount) return [] as SVGGElement[];
    const clusters = visibleClustersForStage(stage, data);
    const svg = createSteppedWedgeSvg(data, { visibleClusters: clusters });
    mount.replaceChildren(svg);
    return Array.from(svg.querySelectorAll<SVGGElement>(".wedge-row"));
  };

  const animateStage = async (stage: DesignRevealStage, token: number, animateIn: boolean) => {
    activeStage = stage;
    complete = false;
    syncUi();

    const rows = mountChart(stage);
    const isSiteIntro = stage === "site";

    if (!animateIn || reduceMotion()) {
      rows.forEach((row) => {
        row.style.opacity = "1";
        row.querySelectorAll<SVGRectElement>(".wedge-segment").forEach((seg) => {
          seg.style.transform = "scaleX(1)";
        });
      });
      return;
    }

    rows.forEach((row) => {
      row.style.opacity = "0";
      row.querySelectorAll<SVGRectElement>(".wedge-segment").forEach((seg) => {
        seg.style.transformOrigin = "left center";
        seg.style.transform = "scaleX(0)";
      });
    });

    await gate(token);
    await track(
      animate(
        rows,
        { opacity: [0, 1], transform: ["translateX(-16px)", "translateX(0)"] } as Record<string, unknown>,
        { duration: 0.4, delay: stagger(0.05), ease: "easeOut" }
      ),
      token
    );

    const segments = rows.flatMap((row) => Array.from(row.querySelectorAll<SVGRectElement>(".wedge-segment")));
    await gate(token);
    if (segments.length) {
      await track(
        animate(
          segments,
          { transform: ["scaleX(0)", "scaleX(1)"] } as Record<string, unknown>,
          {
            duration: isSiteIntro ? 0.55 : 0.4,
            delay: isSiteIntro ? stagger(0.45) : stagger(0.02),
            ease: [0.22, 1, 0.36, 1],
          }
        ),
        token
      );
    }
  };

  const finish = () => {
    complete = true;
    paused = false;
    bullets.forEach((li) => li.classList.remove("is-emphasis", "is-muted"));
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
        if (!autoContinue && !isLast) {
          // Manual stage pick: show this stage, then pause for the presenter.
          paused = true;
          syncUi();
          await gate(token);
          // After resume, continue remaining stages automatically.
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
