# ADVANCE TRAUMA — ATLS Sweden 30 Years

Web slide deck for the Swedish ATLS chapter 30-year anniversary presentation (Region 15 + US guests).

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

## Features

- **Motion** animations — staggered entrance, forest plot, sequence randomisation, and stepped-wedge reveal
- **Deep linking** — each slide has a URL hash (e.g. `#design`)
- **Slide overview** — thumbnail filmstrip to jump to any slide (`O`, grid button, or counter)
- **Responsive** — works on projectors, laptops, and tablets
- **Accessible** — keyboard navigation, ARIA labels, reduced-motion support
- **Brand-aligned** — colours and typography match advancetrauma.info

## Slide overview

| # | ID | Content |
|---|-----|---------|
| 1 | `title` | Title slide |
| 2 | `presenter` | Speaker introduction — Martin Gerdin Wärnberg (positions, affiliations, conflicts of interest) |
| 3–4 | `section-problem`, `trauma-stats` | Trauma burden |
| — | ATLS slides | Purpose, evidence, critique |
| — | `atls-patient-impact-sources` | Historical sources for manual Impact claims |
| — | `atls-outcomes-reviews` | Systematic reviews on patient outcomes |
| — | `atls-outcomes-scoping` | Scoping reviews on patient outcomes |
| — | `atls-forest` | Updated systematic review forest plot |
| — | Trial slides | Design, sequences, nested staircase, outcomes, status |
| — | `implications` | Take-home messages |
| — | `team` | International collaboration overview |
| — | `funding` | Current funders and funding gap |
| — | `closing` | Thank you |

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

- Watch studies appear oldest → newest on enter, with the pooled estimate updating after each
- Click **Play timeline** to replay that chronological reveal
- Click a study row to include or exclude it
- Use the chips to show all studies, filter by study design, or filter by training programme (ATLS, PTC, RTTDC, JATEC, CTCT)

Study-level counts come from Nakhid et al. 2026 (17 observational studies in the mortality meta-analysis, including non-ATLS programmes). When every study is included, the plot shows the R-exported REML pooled odds ratio (same pipeline as the protocol export). Subsets are re-pooled in the browser with inverse-variance DerSimonian–Laird random effects (`src/pool-meta.ts`).
