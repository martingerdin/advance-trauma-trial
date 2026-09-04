# ADVANCE TRAUMA — ATLS Sweden 30 Years

Web slide deck for the Swedish ATLS chapter 30-year anniversary presentation (Region 15 + US guests). Aimed at a **25–30 minute** delivery (28 slides).

## Development

```bash
pnpm install
pnpm dev
```

Open [http://localhost:5173](http://localhost:5173). Use arrow keys, space, or swipe to navigate. Press **O** (or the grid / counter controls) for a thumbnail overview to jump to any slide. Press **F** for fullscreen.

## Build for website

```bash
pnpm build
```

Static files are output to `dist/`. Deploy the entire `dist/` folder to your web host (e.g. `advancetrauma.info/presentations/atls-sweden-30-years/`).

Image assets in `public/` are copied to `dist/` during build:

- `crash-illustration.png`
- `training-illustration.png`
- `patient-review-before-illustration.png`
- `patient-review-after-illustration.png`
- `needs-to-change-illustration.png`
- `qr-advancetrauma.png`

## Features

- **Motion** animations — staggered entrance, forest plot, and stepped-wedge reveal
- **Deep linking** — each slide has a URL hash (e.g. `#design`)
- **Slide overview** — thumbnail filmstrip to jump to any slide (`O`, grid button, or counter)
- **Responsive** — works on projectors, laptops, and tablets
- **Accessible** — keyboard navigation, ARIA labels, reduced-motion support
- **Brand-aligned** — colours and typography match advancetrauma.info

## Slide overview

| # | ID | Content |
|---|-----|---------|
| 1 | `title` | Title slide |
| 2 | `presenter` | Speaker introduction — Martin Gerdin Wärnberg |
| 3–4 | `section-problem`, `trauma-stats` | Trauma burden |
| 5–7 | `section-atls`, `atls-purpose`, `atls-spread` | ATLS purpose and reach |
| 8–9 | `atls-providers`, `atls-provider-evidence` | Clinician impact claims + conclusions |
| 10–11 | `atls-outcomes-claim`, `atls-patient-impact-sources` | Patient-outcome claims + conclusions |
| 12–13 | `atls-outcomes-reviews`, `atls-forest` | Systematic reviews and updated meta-analysis |
| 14–16 | `section-trial`, `aim`, `previous-work` | Trial aim and prior work |
| 17–18 | `design`, `design-animation` | Stepped-wedge design |
| 19–20 | `intervention`, `eligibility` | Trial arms and who is included |
| 21–23 | `primary-outcome`, `secondary-outcomes`, `sample-size` | Outcomes and power |
| 24 | `current-status` | Trial progress and participating hospitals map |
| 25–28 | `implications`, `team`, `funding`, `closing` | Take-homes and close |

Trimmed for timing: redundant provider “Further studies” cards, scoping-review list, sequence randomisation chart, and nested staircase walkthrough.

## Speaker briefing

`qa-and-panel-briefing.md` — anticipated critical questions after the talk and in the future-of-trauma-care panel, with suggested replies (Sweden · Region 15 · US audience).

## Source

Content adapted from `presentation.pptx` (trial meeting deck), tailored for an international ATLS audience celebrating 30 years of the Swedish chapter.

## Figure data

The forest plot and stepped-wedge chart read JSON exported from the trial R functions into `src/data/`:

- `meta-analysis.json` — `conduct_meta_analysis(plot = FALSE, export.path = ...)`
- `trial-design.json` — `create_trial_design_flowchart(..., staircase.months = 0, export.path = ...)`
- `trial-design-staircase.json` — nested staircase variant of the same function

Typed accessors are in `src/figure-data.ts`. Regenerate after changing systematic-review data or trial design parameters:

```bash
Rscript export-figure-data.R
```

### Interactive forest plot

On the `#atls-forest` slide you can:

- Click **Play timeline** (with play/pause icon) to reveal studies oldest → newest; the slide starts empty with a pulsing Play button
- Click **Pause timeline** while playing to stop the reveal
- Click a study row to include or exclude it
- Use the chips to show all studies, filter by study design, or filter by training programme (ATLS, PTC, RTTDC, JATEC, CTCT)

Study-level counts come from Nakhid et al. 2026 (17 observational studies in the mortality meta-analysis, including non-ATLS programmes), plus Lule et al. 2026 (cluster RCT of RTTDC; doi:10.2196/82591). When every study is included, the plot shows the R-exported REML pooled odds ratio (same pipeline as the protocol export). Subsets are re-pooled in the browser with inverse-variance DerSimonian–Laird random effects (`src/pool-meta.ts`).
