import { animate, stagger } from "motion";
import { slides, type Slide } from "./slides";
import {
  createSteppedWedgeSvg,
  createWedgeLegend,
  setRevealMonth,
  focusViewBox,
  viewBoxString,
  syncAxisToStage,
} from "./stepped-wedge";
import { createForestPlot, type ForestPlotController } from "./forest-plot";
import { designRevealStageMeta, startDesignReveal, type DesignRevealControls } from "./design-reveal";
import { createSequencesChart, createSequencesLegend } from "./sequences";
import { trialDesign, trialDesignStaircase } from "./figure-data";
import { buildSitesLegendHtml, mountSitesMap, type SitesMapController } from "./sites-map";
import { participatingSites } from "./data/sites";
// Fonts are bundled rather than fetched from Google so the deck renders
// identically on a venue network that blocks or throttles external requests.
import "@fontsource-variable/eb-garamond";
import "@fontsource-variable/roboto";
import "@fontsource-variable/quicksand";
import "./style.css";

let currentIndex = 0;
let isAnimating = false;
let overviewOpen = false;
const forestControllers = new WeakMap<HTMLElement, ForestPlotController>();
const sitesMapControllers = new WeakMap<HTMLElement, SitesMapController>();
let designReveal: DesignRevealControls | null = null;

const slidesEl = document.getElementById("slides")!;
const counterEl = document.getElementById("slide-counter")!;
const progressBar = document.getElementById("progress-bar")!;
const prevBtn = document.getElementById("prev")!;
const nextBtn = document.getElementById("next")!;
const overviewEl = document.getElementById("overview")!;
const overviewFilmstrip = document.getElementById("overview-filmstrip")!;
const overviewToggle = document.getElementById("overview-toggle")!;
const overviewClose = document.getElementById("overview-close")!;
const overviewBackdrop = document.getElementById("overview-backdrop")!;
const overviewPanel = document.getElementById("overview-panel");

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

    case "provocation":
      el.innerHTML = `
        <div class="slide-inner slide-inner--provocation">
          <p class="provocation-statement" data-animate>${slide.body}</p>
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
          <figure class="closing-qr" data-animate>
            <img
              src="./advancetrauma-qr.svg"
              alt="QR code linking to advancetrauma.info"
              width="256"
              height="256"
            />
          </figure>
          ${slide.subtitle ? `<p class="closing-link" data-animate><a href="https://advancetrauma.info">${slide.subtitle}</a></p>` : ""}
          ${slide.footer ? `<p class="meta" data-animate>${slide.footer}</p>` : ""}
          ${slide.body ? `<p class="closing-trademark" data-animate>${slide.body}</p>` : ""}
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
                <div class="panel stat-card" data-animate>
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

    case "bullets": {
      const items = slide.bullets ?? [];
      const reviewRows = items.length > 0 && items.every((b) => b.includes(" — "));
      el.innerHTML = `
        <div class="slide-inner slide-inner--bullets">
          <h2 data-animate>${slide.title}</h2>
          <ul class="${reviewRows ? "paper-list" : "bullet-list"}" data-animate-group>
            ${items
              .map((b, i) => {
                if (!reviewRows) return `<li data-animate>${b}</li>`;
                const [author, ...rest] = b.split(" — ");
                return `<li class="paper-list__item" data-animate>
                  <span class="paper-list__n" aria-hidden="true">${i + 1}</span>
                  <div class="paper-list__body">
                    <p class="paper-list__primary">${author}</p>
                    <p class="paper-list__secondary">${rest.join(" — ")}</p>
                  </div>
                </li>`;
              })
              .join("")}
          </ul>
          ${slide.cite ? `<p class="cite-line" data-animate>${slide.cite}</p>` : ""}
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;
    }

    case "evidence":
      el.innerHTML = `
        <div class="slide-inner slide-inner--evidence">
          <h2 data-animate>${slide.title}</h2>
          <ul class="paper-list" data-animate-group>
            ${(slide.evidence ?? [])
              .map(
                (item) => `
              <li class="paper-list__item" data-animate>
                <span class="paper-list__n" aria-hidden="true">${item.id}</span>
                <div class="paper-list__body">
                  ${item.tag ? `<span class="panel-tag">${item.tag}</span>` : ""}
                  <p class="paper-list__primary">${item.claim}</p>
                  <p class="paper-list__secondary">${item.source}</p>
                </div>
              </li>`
              )
              .join("")}
          </ul>
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

    /* Cards here carry the follow-up method for the primary outcome. Outcomes
       themselves are listed by the outcome-list layout, never carded. */
    case "outcomes":
      el.innerHTML = `
        <div class="slide-inner slide-inner--outcomes">
          <h2 data-animate>${slide.title}</h2>
          ${slide.body ? `<p class="lead" data-animate>${slide.body}</p>` : ""}
          <div class="outcomes-grid" data-animate-group>
            ${(slide.outcomes ?? [])
              .map(
                (item) => `
              <article class="panel outcome-card" data-animate>
                ${item.tag ? `<span class="panel-tag">${item.tag}</span>` : ""}
                <p class="outcome-card__title">${item.title}</p>
                ${item.detail ? `<p class="outcome-card__detail">${item.detail}</p>` : ""}
              </article>`
              )
              .join("")}
          </div>
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "outcome-list":
      el.innerHTML = `
        <div class="slide-inner slide-inner--outcome-list">
          <h2 data-animate>${slide.title}</h2>
          <ul class="outcome-list" data-animate-group>
            ${(slide.outcomes ?? [])
              .map(
                (item) => `
              <li class="outcome-list__row" data-animate>
                ${item.tag ? `<span class="outcome-list__category">${item.tag}</span>` : ""}
                <span class="outcome-list__title">${item.title}</span>
                ${item.detail ? `<span class="outcome-list__detail">${item.detail}</span>` : ""}
              </li>`
              )
              .join("")}
          </ul>
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "arms":
      el.innerHTML = `
        <div class="slide-inner slide-inner--arms">
          <h2 data-animate>${slide.title}</h2>
          <div class="arms-grid" data-animate-group>
            ${(slide.arms ?? [])
              .map(
                (arm) => `
              <article class="panel arm-card arm-card--${arm.variant}" data-animate>
                <span class="panel-tag">${arm.tag}</span>
                <h3 class="arm-card__title">${arm.title}</h3>
                <figure class="arm-card__figure">
                  <img src="${arm.image}" alt="${arm.imageAlt}" />
                </figure>
                <p class="arm-card__text">${arm.body}</p>
              </article>`
              )
              .join("")}
          </div>
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "columns":
      el.innerHTML = `
        <div class="slide-inner slide-inner--columns">
          <h2 data-animate>${slide.title}</h2>
          <div class="columns-grid" data-animate-group>
            ${(slide.columns ?? [])
              .map(
                (col) => `
              <article class="panel column-card" data-animate>
                <h3 class="column-card__heading">${col.heading}</h3>
                <ul class="bullet-list">
                  ${col.bullets.map((b) => `<li>${b}</li>`).join("")}
                </ul>
              </article>`
              )
              .join("")}
          </div>
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
        </div>
      `;
      break;

    case "milestones":
      el.innerHTML = `
        <div class="slide-inner slide-inner--milestones">
          <h2 data-animate>${slide.title}</h2>
          <div class="milestones-grid" data-animate-group>
            ${(slide.milestones ?? [])
              .map(
                (m) => `
              <article class="panel milestone-card" data-animate>
                ${
                  m.image
                    ? `<figure class="milestone-card__media">
                        <img src="${m.image}" alt="${m.imageAlt ?? ""}" />
                      </figure>`
                    : ""
                }
                <div class="milestone-card__body">
                  <p class="milestone-card__label">${m.label}${
                    m.cite
                      ? `<sup class="cite-ref" aria-label="Reference ${m.cite}">${m.cite}</sup>`
                      : ""
                  }</p>
                  <p class="milestone-card__year">${m.year}</p>
                </div>
              </article>`
              )
              .join("")}
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
        </div>
      `;
      break;

    case "design":
      el.innerHTML = `
        <div class="slide-inner slide-inner--design">
          <h2 data-animate>${slide.title}</h2>
          <div class="design-layout">
            <div class="design-copy" data-animate-group>
              ${slide.body ? `<p class="lead" data-animate>${slide.body}</p>` : ""}
              ${
                slide.stats?.length
                  ? `<div class="design-metrics" data-animate>
                      ${slide.stats
                        .map(
                          (s) => `
                        <div class="design-metric">
                          <span class="stat-value">${s.value}</span>
                          <span class="stat-label">${s.label}</span>
                        </div>`
                        )
                        .join("")}
                    </div>`
                  : ""
              }
              ${
                slide.bullets?.length
                  ? `<ul class="bullet-list design-bullets" data-animate-group>
                      ${slide.bullets.map((b) => `<li data-animate>${b}</li>`).join("")}
                    </ul>`
                  : ""
              }
            </div>
            <div class="design-figure" data-animate>
              <div class="wedge-legend-mount"></div>
              <div class="wedge-container" data-wedge-mount></div>
            </div>
          </div>
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
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
            <div class="wedge-chart-stack">
              <div class="wedge-container wedge-container--animation" data-wedge-mount>
                <div class="wedge-phase-callouts" hidden aria-hidden="true">
                  <article class="wedge-phase" data-phase="standard-care" aria-hidden="true">
                    <figure class="wedge-phase__figure">
                      <img src="./patient-review-before-illustration.png" alt="" />
                    </figure>
                    <div class="wedge-phase__copy">
                      <p class="wedge-phase__title">Standard care</p>
                    </div>
                  </article>
                  <article class="wedge-phase" data-phase="transition" aria-hidden="true">
                    <figure class="wedge-phase__figure">
                      <img src="./training-illustration.png" alt="" />
                    </figure>
                    <div class="wedge-phase__copy">
                      <p class="wedge-phase__title">Training</p>
                    </div>
                  </article>
                  <article class="wedge-phase" data-phase="intervention" aria-hidden="true">
                    <figure class="wedge-phase__figure">
                      <img src="./patient-review-after-illustration.png" alt="" />
                    </figure>
                    <div class="wedge-phase__copy">
                      <p class="wedge-phase__title">Intervention</p>
                    </div>
                  </article>
                </div>
              </div>
            </div>
          </div>
        </div>
      `;
      break;
    }

    case "sequences":
      el.innerHTML = `
        <div class="slide-inner slide-inner--sequences">
          <h2 data-animate>${slide.title}</h2>
          <div class="sequences-panel">
            <div class="sequences-mount" id="sequences-mount"></div>
            <div class="sequences-legend-mount" data-animate></div>
          </div>
        </div>
      `;
      break;

    case "forest":
      el.innerHTML = `
        <div class="slide-inner slide-inner--forest">
          <h2 data-animate>${slide.title}</h2>
          ${slide.source ? `<p class="slide-source" data-animate>${slide.source}</p>` : ""}
          <div id="forest-mount"></div>
        </div>
      `;
      break;

    case "implications":
      el.innerHTML = `
        <div class="slide-inner">
          <h2 data-animate>${slide.title}</h2>
          <div class="implications-grid" data-animate-group>
            <div class="panel implication-card implication-card--positive" data-animate>
              <figure><img src="./patient-review-after-illustration.png" alt="Positive outcome — ATLS improves care" /></figure>
              <p>If ATLS<sup>®</sup> <strong>improves</strong> patient outcomes, it should be further promoted.</p>
            </div>
            <div class="panel implication-card implication-card--negative" data-animate>
              <figure><img src="./training-illustration.png" alt="Training needs to evolve" /></figure>
              <p>If ATLS<sup>®</sup> <strong>does not improve</strong> patient outcomes, trauma life support training needs to change.</p>
            </div>
          </div>
        </div>
      `;
      break;

    case "status-map":
      el.innerHTML = `
        <div class="slide-inner slide-inner--status-map">
          <h2 data-animate>${slide.title}</h2>
          <div class="status-map-layout">
            <div class="stats-grid" data-animate-group>
              ${(slide.stats ?? [])
                .map(
                  (s) => `
                <div class="panel stat-card" data-animate>
                  <span class="stat-value">${s.value}</span>
                  <span class="stat-label">${s.label}</span>
                </div>`
                )
                .join("")}
            </div>
            <figure class="status-map-figure" data-animate>
              <div
                class="sites-map"
                data-map
                role="region"
                aria-label="Map of the ${participatingSites.length} hospitals participating in the trial"
              ></div>
              <figcaption class="sites-legend">
                ${buildSitesLegendHtml()}
              </figcaption>
            </figure>
          </div>
          ${slide.footer ? `<p class="slide-footer" data-animate>${slide.footer}</p>` : ""}
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
              <article class="panel team-card" data-animate>
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
              <article class="panel funding-card" data-animate>
                <p class="funding-card__name">${f.name}</p>
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
      const mount = el.querySelector("[data-wedge-mount]");
      if (mount) {
        if (slide.layout === "design") {
          const data = slide.designVariant === "staircase" ? trialDesignStaircase : trialDesign;
          mount.appendChild(createSteppedWedgeSvg(data));
          const legendMount = el.querySelector(".wedge-legend-mount");
          if (legendMount) legendMount.appendChild(createWedgeLegend(data));
        }
        // design-animation chart is mounted by startDesignReveal.
      }
    }
    if (slide.layout === "sequences") {
      const mount = el.querySelector("#sequences-mount");
      const legendMount = el.querySelector(".sequences-legend-mount");
      if (mount) mount.appendChild(createSequencesChart(trialDesign));
      if (legendMount) legendMount.appendChild(createSequencesLegend());
    }
    if (slide.layout === "forest") {
      const mount = el.querySelector("#forest-mount");
      if (mount) {
        const forest = createForestPlot();
        mount.appendChild(forest.element);
        forestControllers.set(el, forest);
      }
    }
    // Keyed off the DOM, not the layout name, so any slide carrying a map works.
    const mapMount = el.querySelector<HTMLElement>("[data-map]");
    if (mapMount) {
      void mountSitesMap(mapMount).then((controller) => {
        sitesMapControllers.set(el, controller);
        // Covers a deep link landing on the map slide before Leaflet resolves.
        if (el.classList.contains("is-active")) controller.refresh();
      });
    }
  });
}

function slideThumbLabel(slide: Slide): string {
  if (slide.layout === "provocation" && slide.body) {
    return slide.body.length > 48 ? `${slide.body.slice(0, 45)}…` : slide.body;
  }
  return slide.title ?? slide.subtitle ?? slide.id;
}

function thumbPreviewClass(slide: Slide): string {
  if (
    slide.layout === "title" ||
    slide.layout === "closing" ||
    slide.layout === "section" ||
    slide.layout === "aim" ||
    slide.layout === "provocation"
  ) {
    return `overview-thumb__preview--${slide.layout}`;
  }
  if (slide.image) return "overview-thumb__preview--image";
  return "";
}

function thumbPreviewInner(slide: Slide): string {
  const label = slideThumbLabel(slide);
  const chips =
    slide.layout === "stats" ||
    slide.layout === "evidence" ||
    slide.layout === "milestones" ||
    slide.layout === "columns" ||
    slide.layout === "arms" ||
    slide.layout === "outcome-list" ||
    slide.layout === "status-map"
      ? `<div class="overview-thumb__chips" aria-hidden="true">
          <span class="overview-thumb__chip"></span>
          <span class="overview-thumb__chip overview-thumb__chip--accent"></span>
          <span class="overview-thumb__chip overview-thumb__chip--purple"></span>
        </div>`
      : slide.layout === "design" || slide.layout === "design-animation" || slide.layout === "forest"
        ? `<div class="overview-thumb__chips" aria-hidden="true">
            <span class="overview-thumb__chip" style="max-width:100%"></span>
          </div>`
        : "";

  return `${chips}<span class="overview-thumb__mini-title">${label}</span>`;
}

function mountOverview(): void {
  overviewFilmstrip.innerHTML = "";
  slides.forEach((slide, i) => {
    const btn = document.createElement("button");
    btn.type = "button";
    btn.className = "overview-thumb";
    btn.dataset.index = String(i);
    btn.setAttribute("role", "listitem");
    btn.setAttribute("aria-label", `Go to slide ${i + 1}: ${slideThumbLabel(slide)}`);
    if (i === currentIndex) btn.classList.add("is-current");

    const previewClass = thumbPreviewClass(slide);
    const imageStyle = slide.image ? ` style="background-image:url('${slide.image}')"` : "";

    btn.innerHTML = `
      <div class="overview-thumb__preview ${previewClass}"${imageStyle}>
        <div class="overview-thumb__preview-inner">
          ${thumbPreviewInner(slide)}
        </div>
      </div>
      <div class="overview-thumb__meta">
        <span class="overview-thumb__num">${i + 1}</span>
        <span class="overview-thumb__label">${slideThumbLabel(slide)}</span>
      </div>
    `;

    btn.addEventListener("click", () => {
      const target = i;
      closeOverview();
      if (target !== currentIndex) goTo(target);
    });

    overviewFilmstrip.appendChild(btn);
  });
}

function updateOverviewActive(): void {
  const thumbs = overviewFilmstrip.querySelectorAll<HTMLButtonElement>(".overview-thumb");
  thumbs.forEach((thumb, i) => {
    thumb.classList.toggle("is-current", i === currentIndex);
  });
  const current = thumbs[currentIndex];
  if (current && overviewOpen) {
    current.scrollIntoView({ behavior: "smooth", inline: "center", block: "nearest" });
  }
}

function openOverview(): void {
  if (overviewOpen) return;
  overviewOpen = true;
  overviewEl.hidden = false;
  overviewEl.setAttribute("aria-hidden", "false");
  overviewToggle.setAttribute("aria-expanded", "true");
  overviewToggle.setAttribute("aria-label", "Close slide overview");
  document.body.classList.add("overview-open");
  // Force reflow so the open transition runs.
  void overviewEl.offsetWidth;
  overviewEl.classList.add("is-open");
  updateOverviewActive();
  overviewClose.focus();
}

function closeOverview(): void {
  if (!overviewOpen) return;
  overviewOpen = false;
  overviewEl.classList.remove("is-open");
  overviewToggle.setAttribute("aria-expanded", "false");
  overviewToggle.setAttribute("aria-label", "Open slide overview");
  document.body.classList.remove("overview-open");

  const finish = (): void => {
    if (overviewOpen) return;
    overviewEl.hidden = true;
    overviewEl.setAttribute("aria-hidden", "true");
  };

  overviewPanel?.addEventListener("transitionend", finish, { once: true });
  window.setTimeout(finish, 350);
  overviewToggle.focus();
}

function toggleOverview(): void {
  if (overviewOpen) closeOverview();
  else openOverview();
}

function updateUI(): void {
  counterEl.textContent = `${currentIndex + 1} / ${slides.length}`;
  progressBar.style.width = `${((currentIndex + 1) / slides.length) * 100}%`;
  document.title = `${slides[currentIndex].title ?? "ADVANCE TRAUMA"} — ATLS Sweden 30 Years`;
  history.replaceState(null, "", `#${slides[currentIndex].id}`);
  updateOverviewActive();
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

  if (slideEl.dataset.layout === "sequences") {
    const flowParts = Array.from(
      slideEl.querySelectorAll<HTMLElement>(
        ".consort-flow__assessed, .consort-flow__mid, .consort-flow__randomised, .consort-flow__sequences"
      )
    );
    animate(
      flowParts,
      { opacity: [0, 1], transform: ["translateY(18px)", "translateY(0)"] } as Record<string, unknown>,
      { duration: 0.45, delay: stagger(0.14, { startDelay: 0.15 }), ease: [0.22, 1, 0.36, 1] }
    );
    const cols = Array.from(slideEl.querySelectorAll<HTMLElement>(".consort-sequence"));
    animate(
      cols,
      { opacity: [0, 1], transform: ["translateY(12px)", "translateY(0)"] } as Record<string, unknown>,
      { duration: 0.4, delay: stagger(0.08, { startDelay: 0.55 }), ease: [0.22, 1, 0.36, 1] }
    );
    const cells = Array.from(slideEl.querySelectorAll<HTMLElement>(".consort-cell"));
    cells.forEach((cell) => {
      cell.style.transformOrigin = "left center";
    });
    animate(
      cells,
      { transform: ["scaleX(0)", "scaleX(1)"] } as Record<string, unknown>,
      {
        duration: 0.35,
        delay: stagger(0.012, { startDelay: 0.7 }),
        ease: [0.22, 1, 0.36, 1],
      }
    );
  }

  if (slideEl.dataset.layout === "forest") {
    // Idle empty chart; presenter starts with Play timeline.
    forestControllers.get(slideEl)?.resetIdle();
  }

  // Leaflet must remeasure once the slide is on screen, or tiles render grey.
  // The WeakMap returns undefined for every slide without a map.
  sitesMapControllers.get(slideEl)?.refresh();

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
  forestControllers.get(current)?.abortReveal();

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
    if (e.key === "Escape" && overviewOpen) {
      e.preventDefault();
      closeOverview();
      return;
    }

    if ((e.key === "o" || e.key === "O") && !e.metaKey && !e.ctrlKey && !e.altKey) {
      e.preventDefault();
      toggleOverview();
      return;
    }

    if (overviewOpen) {
      if (e.key === "ArrowRight" || e.key === "PageDown") {
        e.preventDefault();
        const nextThumb = Math.min(currentIndex + 1, slides.length - 1);
        if (nextThumb !== currentIndex) goTo(nextThumb);
        else updateOverviewActive();
        return;
      }
      if (e.key === "ArrowLeft" || e.key === "PageUp") {
        e.preventDefault();
        const prevThumb = Math.max(currentIndex - 1, 0);
        if (prevThumb !== currentIndex) goTo(prevThumb);
        else updateOverviewActive();
        return;
      }
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        closeOverview();
        return;
      }
      if (e.key === "Home") {
        e.preventDefault();
        goTo(0);
        return;
      }
      if (e.key === "End") {
        e.preventDefault();
        goTo(slides.length - 1);
        return;
      }
      return;
    }

    const onStagedDesign = slides[currentIndex]?.layout === "design-animation";

    // Space drives the animation only while it is still running. Once it has
    // finished, Space advances the deck as it does everywhere else — otherwise
    // the natural "I'm done here" keypress replays the whole sequence.
    if (e.key === " " && onStagedDesign && designReveal && !designReveal.isComplete()) {
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
overviewToggle.addEventListener("click", toggleOverview);
counterEl.addEventListener("click", toggleOverview);
overviewClose.addEventListener("click", closeOverview);
overviewBackdrop.addEventListener("click", closeOverview);

initFromHash();
mountSlides();
mountOverview();
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
