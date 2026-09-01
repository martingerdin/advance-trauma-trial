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

Image assets are copied from the project root during build. Ensure these files remain alongside the built `index.html`:

- `advance-trauma-visual-abstract.png`
- `crash-illustration.png`
- `training-illustration.png`
- `patient-review-before-illustration.png`
- `patient-review-after-illustration.png`

## Features

- **Motion** animations — staggered entrance, stepped-wedge chart reveal
- **Deep linking** — each slide has a URL hash (e.g. `#design`)
- **Responsive** — works on projectors, laptops, and tablets
- **Accessible** — keyboard navigation, ARIA labels, reduced-motion support
- **Brand-aligned** — colours and typography match advancetrauma.info

## Slide overview

| # | ID | Content |
|---|-----|---------|
| 1 | `title` | Title slide |
| 2 | `visual-abstract` | Full visual abstract |
| 3–4 | `section-problem`, `trauma-stats` | Trauma burden |
| 5–12 | ATLS slides | Purpose, evidence, critique |
| 13–21 | Trial slides | Design, outcomes, status |
| 22–23 | `implications`, `closing` | Take-home messages |

## Source

Content adapted from `presentation.pptx` (trial meeting deck), tailored for an international ATLS audience celebrating 30 years of the Swedish chapter.
