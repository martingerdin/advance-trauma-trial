# ADVANCE TRAUMA — ATLS Sweden 30 Years

Web slide deck for the Swedish ATLS chapter 30-year anniversary presentation (Region 15 + US guests). Aimed at a **25–30 minute** delivery (29 slides).

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
- `milestones/` — the four `previous-work` portraits
- `advancetrauma-qr.svg` — closing-slide QR code

Regenerate the QR code if the URL ever changes:

```bash
npx qrcode -t svg -e M -d 082830 -l ffffff -o public/advancetrauma-qr.svg "https://advancetrauma.info"
```

## Features

- **Motion** animations — staggered entrance, forest plot, and stepped-wedge reveal
- **Deep linking** — each slide has a URL hash (e.g. `#design`)
- **Slide overview** — thumbnail filmstrip to jump to any slide (`O`, grid button, or counter)
- **Responsive** — works on projectors, laptops, and tablets
- **Offline-safe** — fonts are bundled, so a blocked venue network cannot change the typography
- **Accessible** — keyboard navigation, ARIA labels, reduced-motion support
- **Brand-aligned** — colours and typography match advancetrauma.info

## Design system

Three typefaces, three jobs — enforced by the tokens at the top of `src/style.css`:

| Family | Owns |
|---|---|
| Quicksand | the `ADVANCE TRAUMA` wordmark, and nothing else |
| EB Garamond | slide titles, section and closing displays, the speaker's name, and the verbatim ATLS® manual quotations |
| Roboto | every lead, label, figure, card heading, list and body string |

Four emphasis treatments, each using a different kind of signal so they can never be
confused: the **aim** takes a tinted, centred slide of its own; the **lead** (study design,
primary outcome) takes the accent rule in the left gutter; **quotations** take a bordered
card with a quote glyph; **footnotes** take a hairline above muted text.

Sizes come from the `--fs-*` scale and gaps from `--space-*`; per-layout widths are one of
`--measure-narrow`, `--measure-standard` or `--measure-wide`. SVG text (forest plot,
stepped wedge) keeps its own user-space px values and is deliberately outside the scale.

Cards mean *how an outcome is ascertained* (primary-outcome slide); outcomes themselves are
listed, never carded.

## Slide overview

| # | ID | Content |
|---|-----|---------|
| 1–2 | `title`, `presenter` | Title and speaker, including research-support disclosure |
| 3–4 | `section-problem`, `trauma-stats` | Trauma burden |
| 5–7 | `section-atls`, `atls-purpose`, `atls-spread` | ATLS purpose and reach |
| 8–9 | `atls-providers`, `atls-provider-evidence` | Impact on clinicians — the manual's claim, then the studies |
| 10–11 | `atls-outcomes-claim`, `atls-patient-impact-sources` | Impact on patients — the manual's claim, then the studies |
| 12–13 | `atls-outcomes-reviews`, `atls-forest` | Five systematic reviews and the updated forest plot |
| 14–15 | `hook`, `section-trial` | The evidence gap, named — then the trial |
| 16–17 | `aim`, `previous-work` | Trial aim and prior work |
| 18–19 | `design`, `design-animation` | Stepped-wedge design, defined then demonstrated |
| 20–21 | `eligibility`, `intervention` | Who is included, then what changes at the transition |
| 22–24 | `primary-outcome`, `secondary-outcomes`, `sample-size` | Outcomes and power |
| 25 | `current-status` | Status and the India site map on one slide |
| 26–29 | `implications`, `team`, `funding`, `closing` | Take-homes and close |

The claim → evidence pairs are named in parallel (*Impact on clinicians* → *Evidence on
clinicians*, *Impact on patients* → *Evidence on patients*) so the audience can see that the
clinician column is solid while the patient column is thin.

Trimmed for timing: redundant “Further studies” cards on the clinician evidence, the
scoping-review list, the sequence randomisation chart, the nested staircase walkthrough, and
the systematic-review milestone (which duplicated two full slides eleven slides earlier).

## Speaker briefing

`qa-and-panel-briefing.md` — anticipated critical questions after the talk and in the future-of-trauma-care panel, with suggested replies (Sweden · Region 15 · US audience).

Two figures in the briefing need reconciling with the trial database before the talk: it says
“~1,200 included” where the deck now says ~2,000, and it gives trial completion as December
2028 where the protocol says October 2029. Whatever the true numbers are, the slide and the
spoken answer must agree.

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

- Click **Play timeline** (with play/pause icon) to reveal studies oldest → newest; the slide starts empty with a pulsing Play button and the reveal takes about 20 seconds
- Click **Pause timeline** while playing to stop the reveal
- Click a study row to include or exclude it
- Use the chips to show all studies, filter by study design, or filter by training programme (ATLS, PTC, RTTDC, JATEC, CTCT)

Because the deck's pooled estimate (18 studies) differs from the published review's (17), the
slide carries a source line naming Lule et al. 2026 as the added cluster randomised trial —
of RTTDC, not ATLS®. Selecting a single study prints that study's estimate rather than
describing it as a random-effects pooled result.

Study-level counts come from Nakhid et al. 2026 (17 observational studies in the mortality meta-analysis, including non-ATLS programmes), plus Lule et al. 2026 (cluster RCT of RTTDC; doi:10.2196/82591). When every study is included, the plot shows the R-exported REML pooled odds ratio (same pipeline as the protocol export). Subsets are re-pooled in the browser with inverse-variance DerSimonian–Laird random effects (`src/pool-meta.ts`).
