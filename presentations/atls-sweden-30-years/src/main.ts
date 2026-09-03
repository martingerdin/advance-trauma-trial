import { animate, stagger } from "motion";
import { slides, type Slide } from "./slides";
import { createSteppedWedgeSvg, setRevealMonth, focusViewBox, viewBoxString, syncAxisToStage } from "./stepped-wedge";
import { createForestPlotSvg } from "./forest-plot";
import { designRevealStageMeta, startDesignReveal, type DesignRevealControls } from "./design-reveal";
import { metaAnalysis, trialDesign, trialDesignStaircase } from "./figure-data";
import "./style.css";

let currentIndex = 0;
let isAnimating = false;
let designReveal: DesignRevealControls | null = null;

const slidesEl = document.getElementById("slides")!;
const counterEl = document.getElementById("slide-counter")!;
const progressBar = document.getElementById("progress-bar")!;
const prevBtn = document.getElementById("prev")!;
const nextBtn = document.getElementById("next")!;

function evidenceCardClass(tag?: string): string {
  switch (tag) {
    case "Manual claim":
      return " evidence-card--manual";
    case "Further studies":
      return " evidence-card--further";
    case "Systematic review":
      return " evidence-card--review";
    default:
      return "";
  }
}

function renderSlide(slide: Slide): HTMLElement {
  const el = document.createElement("article");
  el.className = `slide slide--${slide.layout}`;
  el.id = `slide-${slide.id}`;
  el.dataset.layout = slide.layout;

  switch (slide.layout) {
    case "title":
      el.innerHTML = `
        <div class="slide-inner slide-inner--center">
          ${slide.eyebrow ? `<p class="eyebrow" data-animate>${slide.eyebrow}</p>` : ""}
          <h1 class="display" data-animate>${slide.title}</h1>
          ${slide.subtitle ? `<p class="subtitle" data-animate>${slide.subtitle}</p>` : ""}
          ${slide.footer ? `<p class="meta" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "section":
      el.innerHTML = `
        <div class="slide-inner slide-inner--center">
          <h2 class="section-title" data-animate>${slide.title}</h2>
          ${slide.subtitle ? `<p class="section-subtitle" data-animate>${slide.subtitle}</p>` : ""}
        </div>
      `;
      break;

    case "presenter":
      el.innerHTML = `
        <div class="slide-inner slide-inner--presenter">
          ${slide.eyebrow ? `<p class="eyebrow" data-animate>${slide.eyebrow}</p>` : ""}
          <h1 class="presenter-name" data-animate>${slide.title}</h1>
          ${slide.subtitle ? `<p class="presenter-degrees" data-animate>${slide.subtitle}</p>` : ""}
          <div class="presenter-grid">
            ${
              slide.bullets?.length
                ? `<section class="presenter-block" data-animate>
                    <h2 class="presenter-heading">Positions</h2>
                    <ul class="presenter-list">
                      ${slide.bullets.map((b) => `<li>${b}</li>`).join("")}
                    </ul>
                  </section>`
                : ""
            }
            ${
              slide.affiliations?.length
                ? `<section class="presenter-block" data-animate>
                    <h2 class="presenter-heading">Affiliations</h2>
                    <ul class="presenter-list">
                      ${slide.affiliations.map((a) => `<li>${a}</li>`).join("")}
                    </ul>
                  </section>`
                : ""
            }
          </div>
          ${
            slide.body
              ? `<section class="presenter-disclosure" data-animate>
                  <h2 class="presenter-heading">Conflicts of interest</h2>
                  <p class="presenter-disclosure__text">${slide.body}</p>
                </section>`
              : ""
          }
        </div>
      `;
      break;

    case "closing":
      el.innerHTML = `
        <div class="slide-inner slide-inner--center">
          <h2 class="display" data-animate>${slide.title}</h2>
          ${slide.subtitle ? `<p class="closing-link" data-animate><a href="https://www.advancetrauma.info">${slide.subtitle}</a></p>` : ""}
          ${slide.footer ? `<p class="meta" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "visual":
      el.innerHTML = `
        <div class="slide-inner slide-inner--visual">
          ${slide.title ? `<h2 class="visual-title" data-animate>${slide.title}</h2>` : ""}
          <figure class="visual-figure" data-animate>
            <img src="${slide.image}" alt="${slide.imageAlt ?? ""}" loading="eager" />
          </figure>
        </div>
      `;
      break;

    case "stats":
      el.innerHTML = `
        <div class="slide-inner">
          <h2 data-animate>${slide.title}</h2>
          <div class="stats-layout ${slide.image ? "stats-layout--with-image" : ""}">
            <div class="stats-grid" data-animate-group>
              ${(slide.stats ?? [])
                .map(
                  (s) => `
                <div class="stat-card" data-animate>
                  <span class="stat-value">${s.value}</span>
                  <span class="stat-label">${s.label}${
                    s.source
                      ? `<sup class="cite-ref" aria-label="Reference ${s.source}">${s.source}</sup>`
                      : ""
                  }</span>
                </div>`
                )
                .join("")}
            </div>
            ${
              slide.image
                ? `<figure class="slide-figure" data-animate><img src="${slide.image}" alt="${slide.imageAlt ?? ""}" /></figure>`
                : ""
            }
          </div>
          ${
            slide.references?.length
              ? `<ol class="slide-references" data-animate>
                  ${slide.references
                    .map((ref) => `<li value="${ref.id}"><span class="ref-marker">${ref.id}.</span> ${ref.text}</li>`)
                    .join("")}
                </ol>`
              : ""
          }
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "quote":
      el.innerHTML = `
        <div class="slide-inner slide-inner--quote">
          <h2 data-animate>${slide.title}</h2>
          <blockquote class="quote-block" data-animate>
            <p class="quote-text">${slide.body}</p>
            ${slide.cite ? `<cite>${slide.cite}</cite>` : ""}
          </blockquote>
        </div>
      `;
      break;

    case "bullets":
      el.innerHTML = `
        <div class="slide-inner">
          <h2 data-animate>${slide.title}</h2>
          <ul class="bullet-list" data-animate-group>
            ${(slide.bullets ?? []).map((b) => `<li data-animate>${b}</li>`).join("")}
          </ul>
          ${slide.cite ? `<p class="cite-line" data-animate>${slide.cite}</p>` : ""}
        </div>
      `;
      break;

    case "evidence":
      el.innerHTML = `
        <div class="slide-inner slide-inner--evidence">
          <h2 data-animate>${slide.title}</h2>
          ${slide.body ? `<p class="evidence-intro" data-animate>${slide.body}</p>` : ""}
          <div class="evidence-grid${(slide.evidence?.length ?? 0) > 4 ? " evidence-grid--compact" : ""}" data-animate-group>
            ${(slide.evidence ?? [])
              .map(
                (item) => `
              <article class="evidence-card${evidenceCardClass(item.tag)}" data-animate>
                <div class="evidence-card__header">
                  <span class="evidence-card__id" aria-hidden="true">${item.id}</span>
                  ${item.tag ? `<span class="evidence-card__tag">${item.tag}</span>` : ""}
                </div>
                <p class="evidence-card__claim">${item.claim}</p>
                <p class="evidence-card__source">${item.source}</p>
              </article>`
              )
              .join("")}
          </div>
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "references":
      el.innerHTML = `
        <div class="slide-inner slide-inner--references">
          <h2 data-animate>${slide.title}</h2>
          ${slide.body ? `<p class="references-intro" data-animate>${slide.body}</p>` : ""}
          ${
            slide.references?.length
              ? `<ol class="slide-references slide-references--standalone" data-animate>
                  ${slide.references
                    .map((ref) => `<li value="${ref.id}"><span class="ref-marker">${ref.id}.</span> ${ref.text}</li>`)
                    .join("")}
                </ol>`
              : ""
          }
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "aim":
      el.innerHTML = `
        <div class="slide-inner slide-inner--aim">
          <h2 data-animate>${slide.title}</h2>
          <p class="aim-statement" data-animate>${slide.body}</p>
        </div>
      `;
      break;

    case "two-col":
      el.innerHTML = `
        <div class="slide-inner">
          <h2 data-animate>${slide.title}</h2>
          <div class="two-col">
            <div class="two-col__content">
              ${
                slide.stats
                  ? `<div class="inline-stats" data-animate-group>
                      ${slide.stats
                        .map(
                          (s) => `
                        <div class="inline-stat" data-animate>
                          <span class="stat-value">${s.value}</span>
                          <span class="stat-label">${s.label}</span>
                        </div>`
                        )
                        .join("")}
                    </div>`
                  : ""
              }
              ${
                slide.bullets
                  ? `<ul class="bullet-list" data-animate-group>
                      ${slide.bullets.map((b) => `<li data-animate>${b}</li>`).join("")}
                    </ul>`
                  : ""
              }
            </div>
            ${
              slide.image
                ? `<figure class="slide-figure" data-animate><img src="${slide.image}" alt="${slide.imageAlt ?? ""}" /></figure>`
                : ""
            }
          </div>
        </div>
      `;
      break;

    case "milestones":
      el.innerHTML = `
        <div class="slide-inner slide-inner--milestones">
          <header class="milestones-header" data-animate>
            <h2>${slide.title}</h2>
            ${slide.subtitle ? `<p class="milestones-subtitle">${slide.subtitle}</p>` : ""}
          </header>
          <div class="milestones-grid" data-animate-group>
            ${(slide.milestones ?? [])
              .map(
                (m) => `
              <article class="milestone-card" data-animate>
                ${
                  m.image
                    ? `<figure class="milestone-card__media">
                        <img src="${m.image}" alt="${m.imageAlt ?? ""}" />
                      </figure>`
                    : ""
                }
                <div class="milestone-card__body">
                  <p class="milestone-card__year">${m.year}${
                    m.cite
                      ? `<sup class="cite-ref" aria-label="Reference ${m.cite}">${m.cite}</sup>`
                      : ""
                  }</p>
                  <p class="milestone-card__label">${m.label}</p>
                </div>
              </article>`
              )
              .join("")}
          </div>
        </div>
      `;
      break;

    case "design":
      el.innerHTML = `
        <div class="slide-inner">
          <h2 data-animate>${slide.title}</h2>
          <div class="design-layout">
            <ul class="bullet-list design-bullets" data-animate-group>
              ${(slide.bullets ?? []).map((b) => `<li data-animate>${b}</li>`).join("")}
            </ul>
            <div class="wedge-container" data-animate id="wedge-mount"></div>
          </div>
        </div>
      `;
      break;

    case "design-animation": {
      const stages = designRevealStageMeta();
      el.innerHTML = `
        <div class="slide-inner slide-inner--design-animation">
          <h2 data-animate>${slide.title}</h2>
          <div class="wedge-panel wedge-panel--solo" data-animate>
            <div class="wedge-toolbar">
              <div class="wedge-stages" role="list" aria-label="Design reveal stages">
                ${stages
                  .map(
                    (s) =>
                      `<button type="button" class="wedge-stage" data-stage="${s.id}" role="listitem">${s.label}</button>`
                  )
                  .join("")}
              </div>
              <button type="button" class="wedge-play-pause" aria-label="Pause animation" title="Pause (Space)"></button>
            </div>
            <p class="wedge-caption" aria-live="polite"></p>
            <div class="wedge-legend-mount"></div>
            <div class="wedge-phase-callouts" hidden>
              <article class="wedge-phase" data-phase="standard-care">
                <figure class="wedge-phase__figure">
                  <img src="./patient-review-before-illustration.png" alt="Standard care in the emergency department" />
                </figure>
                <p class="wedge-phase__title">Standard care</p>
                <p class="wedge-phase__when">Months 0–4</p>
              </article>
              <article class="wedge-phase" data-phase="transition">
                <figure class="wedge-phase__figure">
                  <img src="./training-illustration.png" alt="ATLS training course" />
                </figure>
                <p class="wedge-phase__title">Transition</p>
                <p class="wedge-phase__when">Month 4–5 · ATLS® course</p>
              </article>
              <article class="wedge-phase" data-phase="intervention">
                <figure class="wedge-phase__figure">
                  <img src="./patient-review-after-illustration.png" alt="Care after ATLS training" />
                </figure>
                <p class="wedge-phase__title">Intervention</p>
                <p class="wedge-phase__when">Months 5–13</p>
              </article>
            </div>
            <div class="wedge-container wedge-container--animation" id="wedge-mount"></div>
          </div>
        </div>
      `;
      break;
    }

    case "forest": {
      const pooled = metaAnalysis.pooled;
      el.innerHTML = `
        <div class="slide-inner">
          <h2 data-animate>${slide.title}</h2>
          <div class="forest-container" data-animate id="forest-mount"></div>
          <p class="slide-footer" data-animate>
            Random-effects ${metaAnalysis.measure} ${pooled.rrFormatted}
            (95% CI ${pooled.ciFormatted.replace("; ", "–")});
            I² ${(pooled.i2Rounded * 100).toFixed(0)}%;
            ${pooled.numberOfStudies} observational studies
          </p>
        </div>
      `;
      break;
    }

    case "implications":
      el.innerHTML = `
        <div class="slide-inner">
          <h2 data-animate>${slide.title}</h2>
          <div class="implications-grid" data-animate-group>
            <div class="implication-card implication-card--positive" data-animate>
              <figure><img src="./patient-review-after-illustration.png" alt="Positive outcome — ATLS improves care" /></figure>
              <p>If ATLS<sup>®</sup> <strong>improves</strong> patient outcomes, it should be further promoted.</p>
            </div>
            <div class="implication-card implication-card--negative" data-animate>
              <figure><img src="./training-illustration.png" alt="Training needs to evolve" /></figure>
              <p>If ATLS<sup>®</sup> <strong>does not improve</strong> patient outcomes, trauma life support training needs to change.</p>
            </div>
          </div>
        </div>
      `;
      break;

    case "team":
      el.innerHTML = `
        <div class="slide-inner slide-inner--team">
          <header class="team-header" data-animate>
            <h2>${slide.title}</h2>
            ${slide.subtitle ? `<p class="team-subtitle">${slide.subtitle}</p>` : ""}
          </header>
          <div class="team-grid" data-animate-group>
            ${(slide.teamGroups ?? [])
              .map(
                (group) => `
              <article class="team-card" data-animate>
                <h3 class="team-card__label">${group.label}</h3>
                <p class="team-card__location">${group.location}</p>
                <ul class="team-card__members">
                  ${group.members
                    .map(
                      (m) => `
                    <li>
                      <span class="team-member__name">${m.name}</span>
                      <span class="team-member__role">${m.role}</span>
                    </li>`
                    )
                    .join("")}
                </ul>
              </article>`
              )
              .join("")}
          </div>
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "funding":
      el.innerHTML = `
        <div class="slide-inner slide-inner--funding">
          <h2 data-animate>${slide.title}</h2>
          ${slide.body ? `<p class="funding-intro" data-animate>${slide.body}</p>` : ""}
          <div class="funding-grid" data-animate-group>
            ${(slide.funders ?? [])
              .map(
                (f) => `
              <article class="funding-card" data-animate>
                <p class="funding-card__name">${f.name}</p>
                <p class="funding-card__detail">${f.detail}</p>
              </article>`
              )
              .join("")}
          </div>
          ${slide.footer ? `<p class="funding-note" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;
  }

  return el;
}

function mountSlides(): void {
  slidesEl.innerHTML = "";
  slides.forEach((slide, i) => {
    const el = renderSlide(slide);
    el.classList.toggle("is-active", i === currentIndex);
    el.setAttribute("aria-hidden", i === currentIndex ? "false" : "true");
    slidesEl.appendChild(el);

    if (slide.layout === "design" || slide.layout === "design-animation") {
      const mount = el.querySelector("#wedge-mount");
      if (mount) {
        if (slide.layout === "design") {
          const data = slide.designVariant === "staircase" ? trialDesignStaircase : trialDesign;
          mount.appendChild(createSteppedWedgeSvg(data));
        }
        // design-animation chart is mounted by startDesignReveal.
      }
    }
    if (slide.layout === "forest") {
      const mount = el.querySelector("#forest-mount");
      if (mount) mount.appendChild(createForestPlotSvg());
    }
  });
}

function updateUI(): void {
  counterEl.textContent = `${currentIndex + 1} / ${slides.length}`;
  progressBar.style.width = `${((currentIndex + 1) / slides.length) * 100}%`;
  document.title = `${slides[currentIndex].title ?? "ADVANCE TRAUMA"} — ATLS Sweden 30 Years`;
  history.replaceState(null, "", `#${slides[currentIndex].id}`);
}

function animateSlideIn(slideEl: HTMLElement): void {
  const targets = Array.from(slideEl.querySelectorAll<HTMLElement>("[data-animate]"));
  if (targets.length === 0) return;

  animate(
    targets,
    { opacity: [0, 1], transform: ["translateY(24px)", "translateY(0)"] } as Record<string, unknown>,
    { duration: 0.55, delay: stagger(0.08), ease: [0.22, 1, 0.36, 1] }
  );

  if (slideEl.dataset.layout === "design" || slideEl.dataset.layout === "design-animation") {
    const slide = slides[currentIndex];
    designReveal?.cancel();
    designReveal = null;

    if (slide?.layout === "design-animation") {
      designReveal = startDesignReveal(slideEl, trialDesign);
    } else if (slide?.layout === "design") {
      const data = slide.designVariant === "staircase" ? trialDesignStaircase : trialDesign;
      const rows = Array.from(slideEl.querySelectorAll<SVGGElement>(".wedge-row"));
      const svg = slideEl.querySelector<SVGSVGElement>(".stepped-wedge-chart");
      if (svg) {
        svg.setAttribute("viewBox", viewBoxString(focusViewBox(data, "full")));
        syncAxisToStage(svg, data, "full");
        setRevealMonth(svg, data.xMax);
      }
      rows.forEach((row) => {
        row.style.opacity = "1";
        row.setAttribute("opacity", "1");
      });
      slideEl.querySelectorAll<SVGTextElement>(".wedge-batch-label").forEach((label) => {
        label.setAttribute("opacity", "1");
      });
      animate(
        rows,
        { opacity: [0, 1], transform: ["translateX(-20px)", "translateX(0)"] } as Record<string, unknown>,
        { duration: 0.4, delay: stagger(0.02, { startDelay: 0.3 }), ease: "easeOut" }
      );

      const segments = Array.from(slideEl.querySelectorAll<SVGRectElement>(".wedge-segment"));
      segments.forEach((seg) => {
        seg.style.transformOrigin = "left center";
      });
      animate(
        segments,
        { transform: ["scaleX(0)", "scaleX(1)"] } as Record<string, unknown>,
        {
          duration: 0.35,
          delay: stagger(0.008, { startDelay: 0.4 }),
          ease: [0.22, 1, 0.36, 1],
        }
      );
    }
  } else {
    designReveal?.cancel();
    designReveal = null;
  }

  if (slideEl.dataset.layout === "forest") {
    const rows = Array.from(slideEl.querySelectorAll<HTMLElement>(".forest-row"));
    animate(
      rows,
      { opacity: [0, 1], transform: ["translateX(-12px)", "translateX(0)"] } as Record<string, unknown>,
      { duration: 0.35, delay: stagger(0.04, { startDelay: 0.25 }), ease: "easeOut" }
    );
  }

  const img = slideEl.querySelector<HTMLElement>(".slide-figure img, .visual-figure img");
  if (img) {
    animate(
      img,
      { transform: ["scale(0.92)", "scale(1)"], opacity: [0, 1] } as Record<string, unknown>,
      { duration: 0.7, delay: 0.15, ease: "easeOut" }
    );
  }
}

function goTo(index: number): void {
  if (isAnimating || index < 0 || index >= slides.length || index === currentIndex) return;
  isAnimating = true;
  designReveal?.cancel();
  designReveal = null;

  const current = slidesEl.children[currentIndex] as HTMLElement;
  const next = slidesEl.children[index] as HTMLElement;

  animate(
    current,
    { opacity: [1, 0], transform: ["scale(1)", "scale(0.98)"] } as Record<string, unknown>,
    { duration: 0.25, ease: "easeIn" }
  ).then(() => {
    current.classList.remove("is-active");
    current.setAttribute("aria-hidden", "true");
    next.classList.add("is-active");
    next.setAttribute("aria-hidden", "false");
    currentIndex = index;
    updateUI();
    animateSlideIn(next);
    animate(
      next,
      { opacity: [0, 1], transform: ["scale(1.02)", "scale(1)"] } as Record<string, unknown>,
      { duration: 0.35, ease: [0.22, 1, 0.36, 1] }
    ).then(() => {
      isAnimating = false;
    });
  });
}

function next(): void {
  goTo(currentIndex + 1);
}

function prev(): void {
  goTo(currentIndex - 1);
}

function initFromHash(): void {
  const hash = location.hash.slice(1);
  if (hash) {
    const idx = slides.findIndex((s) => s.id === hash);
    if (idx >= 0) currentIndex = idx;
  }
}

function setupKeyboard(): void {
  document.addEventListener("keydown", (e) => {
    const onStagedDesign = slides[currentIndex]?.layout === "design-animation";

    if (e.key === " " && onStagedDesign && designReveal) {
      e.preventDefault();
      designReveal.toggle();
      return;
    }

    if (e.key === "ArrowRight" || e.key === " " || e.key === "PageDown") {
      e.preventDefault();
      next();
    } else if (e.key === "ArrowLeft" || e.key === "PageUp") {
      e.preventDefault();
      prev();
    } else if (e.key === "Home") {
      e.preventDefault();
      goTo(0);
    } else if (e.key === "End") {
      e.preventDefault();
      goTo(slides.length - 1);
    } else if (e.key === "f" || e.key === "F") {
      if (!document.fullscreenElement) {
        document.documentElement.requestFullscreen();
      } else {
        document.exitFullscreen();
      }
    }
  });
}

function setupTouch(): void {
  let startX = 0;
  let startY = 0;

  slidesEl.addEventListener(
    "touchstart",
    (e) => {
      startX = e.touches[0].clientX;
      startY = e.touches[0].clientY;
    },
    { passive: true }
  );

  slidesEl.addEventListener(
    "touchend",
    (e) => {
      const dx = e.changedTouches[0].clientX - startX;
      const dy = e.changedTouches[0].clientY - startY;
      if (Math.abs(dx) > Math.abs(dy) && Math.abs(dx) > 50) {
        if (dx < 0) next();
        else prev();
      }
    },
    { passive: true }
  );
}

prevBtn.addEventListener("click", prev);
nextBtn.addEventListener("click", next);

initFromHash();
mountSlides();
updateUI();

const activeSlide = slidesEl.querySelector(".is-active") as HTMLElement;
animateSlideIn(activeSlide);

setupKeyboard();
setupTouch();

window.addEventListener("hashchange", () => {
  const hash = location.hash.slice(1);
  const idx = slides.findIndex((s) => s.id === hash);
  if (idx >= 0 && idx !== currentIndex) goTo(idx);
});

const progressBarEl = document.getElementById("progress-bar");
if (progressBarEl) {
  progressBarEl.style.transformOrigin = "left center";
}
