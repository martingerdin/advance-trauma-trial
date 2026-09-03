# ADVANCE TRAUMA — ATLS Sweden 30 Years

Web slide deck for the Swedish ATLS chapter 30-year anniversary presentation (Region 15 + US guests).

## Development

```bash
pnpm install
pnpm dev
```

Open [http://localhost:5173](http://localhost:5173). Use arrow keys, space, or swipe to navigate. Press **F** for fullscreen.

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

## Features

- **Motion** animations — staggered entrance, forest plot and stepped-wedge reveal
- **Deep linking** — each slide has a URL hash (e.g. `#design`)
- **Responsive** — works on projectors, laptops, and tablets
- **Accessible** — keyboard navigation, ARIA labels, reduced-motion support
- **Brand-aligned** — colours and typography match advancetrauma.info

## Slide overview

| # | ID | Content |
|---|-----|---------|
| 1 | `title` | Title slide |
| 2–3 | `section-problem`, `trauma-stats` | Trauma burden |
| 4–12 | ATLS slides | Purpose, evidence, forest plot, critique |
| — | `atls-impact-sources` | Historical sources for manual Impact claims |
| — | `atls-forest` | Updated systematic review forest plot |
| 12–21 | Trial slides | Design, nested staircase, outcomes, status |
| 21–22 | `implications`, `closing` | Take-home messages |

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
