# ADVANCE TRAUMA — Batch 3 Site Initiation

Web slide deck for the **batch 3 site initiation** meeting — prepared to get the **batch 3 principal investigators (PIs)** started at their hospitals. Aimed at a **maximum 10-minute** delivery (15 slides, planned speaking time 9:55).

Built on the same framework and brand as the ATLS Sweden 30 Years deck (`presentations/atls-sweden-30-years/`).

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

Static files are output to `dist/`. Deploy the entire `dist/` folder to your web host (e.g. `advancetrauma.info/presentations/batch-3-initiation/`).

### PDF handout

Print to PDF from the browser (or headless Chrome) and you get one slide per page.
Choose landscape and a 16:9 paper size to match the deck:

```bash
google-chrome --headless --print-to-pdf=deck.pdf \
  --no-pdf-header-footer --print-to-pdf-no-header http://localhost:5173
```

Image assets in `public/` are copied to `dist/` during build. The animated design uses the before-care, training and after-care illustrations; the intervention slide uses the before-care and training illustrations. The closing slide uses `advancetrauma-qr.svg`.

Regenerate the QR code if the URL ever changes:

```bash
npx qrcode -t svg -e M -d 082830 -l ffffff -o public/advancetrauma-qr.svg "https://advancetrauma.info"
```

## Features

- **Motion** animations — staggered entrance and stepped-wedge reveal
- **Deep linking** — each slide has a URL hash (e.g. `#design`)
- **Slide overview** — thumbnail filmstrip to jump to any slide (`O`, grid button, or counter)
- **Responsive** — works on projectors, laptops, and tablets
- **Offline-safe** — fonts are bundled, so a blocked venue network cannot change the typography
- **Typography** — slide text is at least 16 pt (21.34 CSS px); SVG labels are checked at their rendered scale
- **Accessible** — keyboard navigation, ARIA labels, reduced-motion support
- **Brand-aligned** — colours and typography match advancetrauma.info

## Slide overview

| # | ID | Content |
|---|-----|---------|
| 1 | `title` | Trial question and batch 3 initiation |
| 2 | `background` | Injury burden, ATLS adoption and the evidence gap, with references |
| 3 | `aim` | Verbatim trial aim from the protocol |
| 4 | `design` | Animated reveal: one hospital → five hospitals → full trial |
| 5 | `batch-timeline` | Anticipated 16 November 2026 start; five clusters in the same order and phase colours as the animation |
| 6 | `intervention` | Standard care versus ATLS training, with illustrations |
| 7 | `training-selection` | Resident diagram: training within one unit, with a checkmark |
| 8 | `training-distributed` | Resident diagram: spreading trainees across units, with a cross |
| 9 | `training-selection-emphasis` | Concentrated selection revisited; average of 10 physicians trained per hospital |
| 10 | `eligibility` | Patient inclusion and exclusion criteria, including unit working days |
| 11 | `sample-size` | At least 4,320 trial participants; monthly and total site recruitment |
| 12 | `data-collection` | Primary outcome; main versus nested data collection |
| 13 | `consent` | Opt-out, opt-in and waiver provisions |
| 14 | `safety` | Safety events and the reporting process |
| 15 | `closing` | Questions and trial website |

## Timing for ≤ 10 minutes

The planned delivery is 9:55: title 0:15, background 0:40, aim 0:20, animated design 1:00, batch timeline 0:30, intervention 0:45, trainee selection 1:10 across three slides, eligibility 1:00, sample size 0:30, outcomes 1:15, consent 1:00, safety 1:15, closing 0:15. Keep transitions brief; a full Q&A needs separate time.

## Visual conventions

- No labels or pills above slide titles.
- Persistent branding centred above the navigation shows ADVANCE TRAUMA · advancetrauma.info in trial blue, hidden on the first and last slides. The brandmark uses Quicksand. Branding remains stationary during slide transitions and is repeated per slide when printing.
- Equivalent content uses identical formatting: plain columns with the same heading and body styles, without coloured borders, fills or shadows.
- Colour in the timeline identifies study phases; coloured resident figures identify selected course participants.
- Existing clinical illustrations explain care and training; they are retained rather than treated as generic mood images.
- The resident diagrams reproduce onboarding slides 11–13: four residents in one unit versus three spread across three units. These are not equal-number comparisons.
- `src/initiation.css` contains the batch-specific styling, separate from the inherited presentation framework.

## Source

Content based on the ATLS Sweden 30 Years deck (`presentations/atls-sweden-30-years/`) and the trial's canonical documents:

- Protocol overview: `meetings/trial-meeting-march-2026/protocol.pptx`
- Batch timeline, nested outcomes and trainee selection: `meetings/trial-meeting-march-2026/batch-3-onboarding.pptx`
- Exact eligibility, intervention, outcomes, consent and safety procedures: `protocol/protocol.qmd` and `shared-assets/`
- Trial design parameters: `src/data/trial-design.json` (exported from the trial R functions)

### Figure data

The stepped-wedge chart reads JSON exported from the trial R functions into `src/data/`:

- `trial-design.json` — `create_trial_design_flowchart(..., staircase.months = 0, export.path = ...)`

Typed accessors are in `src/figure-data.ts`. Regenerate after changing trial design parameters:

```bash
Rscript export-figure-data.R
```

## Notes

**Confirm before the talk:** batch start date, training slots and notification arrangements, participating units, and the site safety reporting contact/route. The animated timeline uses relative months and schematic hospital numbers, not named site assignments. The batch timeline assumes an anticipated start of 16 November 2026, giving 13 months through 16 December 2027. Sample size is at least 4,320 patients (30 sites × 12 patients × 12 non-transition months): 144 per site for analysis, or about 156 recruited if the same rate continues through the transition month. Calendar dates remain provisional.

On `#design`, Space pauses/resumes the reveal, which pauses after each care phase and after the five-hospital view. Use the stage controls to jump between views; Right Arrow continues a paused reveal before advancing to the next slide. With reduced motion, the full design is shown without animation.

`presentation.pptx` is a separate reference deck, not an export of this web presentation; it is not updated by `pnpm build`.
