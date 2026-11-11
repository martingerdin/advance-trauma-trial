# ADVANCE TRAUMA — standard trial presentation

A reusable, standalone browser presentation of the trial rationale and methods. It describes the planned trial, not a recruitment, approval or funding-status update. Slide content and route membership are defined in [`src/slides.ts`](src/slides.ts); presenter guidance is in [`speaker-notes.md`](speaker-notes.md).

## Why a standard deck?

- [`../batch-3-initiation/src/slides.ts`](../batch-3-initiation/src/slides.ts) is operational: site training selection, patient inclusion, consent and safety reporting. Its batch-specific timeline is not appropriate for a reusable overview. The standard deck retains practical clarity while adding setting/cluster eligibility, allocation, assessment timing, analysis, safeguards and governance.
- [`../atls-sweden-30-years/src/slides.ts`](../atls-sweden-30-years/src/slides.ts) is audience-specific: ATLS history, evidence, implications, team and a status snapshot. The standard deck restores three original ACS manual quote slides verbatim from the Sweden deck, retains the review summary and inherited interactive forest plot, then concentrates on the trial rather than the anniversary narrative or time-sensitive recruitment/funding assertions. Both original decks remain unchanged.
- Canonical protocol definitions take precedence over presentation shorthand: eligibility starts at **15 years**, the primary outcome is **in-hospital** mortality during the initial admission within 30 days, secondary mortality is **all-cause**, and safety-event relatedness is assessed by **trial management**.

This is a **SPIRIT-informed sequence**, not a complete SPIRIT checklist or a claim of full SPIRIT compliance. The presentation does not replace the approved protocol, statistical analysis plan, committee charters or site procedures.

## Run independently

From `presentations/standard-trial/` (this directory), run:

```sh
pnpm install
pnpm build
pnpm dev
```

Use the local URL printed by Vite. `pnpm build` runs TypeScript checking and creates the browser build in `dist/`; `pnpm dev` starts the development server. The package declares its pnpm version in `package.json`. No parent presentation build is required. An optional `pnpm preview` serves the built output locally.

**No PPTX export exists.** These are Vite/browser workflows, not PowerPoint generation commands. The Markdown speaker notes are a separate reference, not an integrated presenter-notes mode.

## Routes and navigation

Append these paths/query strings to the served presentation URL:

| Route | Selection | Suggested speaking time, excluding questions |
| --- | --- | --- |
| `/` — `view` omitted | Core: 25 slides, excluding `detail: true` slides | 20–25 minutes |
| `/?view=brief` | Brief: 13 selected slides, including `atls-forest` | 9–12 minutes |
| `/?view=full` | Full: all 29 slides, including four detail slides | 25–30 minutes |

An unrecognised `view` value also selects core. Change the query string and load that URL to change routes. The brief route retains source order: `title`, `background`, `atls-forest`, `aim`, `design`, `setting`, `eligibility`, `intervention`, `primary-outcome`, `secondary-outcomes`, `sample-size`, `oversight`, `closing`.

The hash names a slide by its exact ID, for example `/#allocation`, `/?view=brief#primary-outcome`, or `/?view=full#nested-methods`. A hash only navigates to a slide included in the selected route; it does not enable an omitted detail slide. An unknown/excluded hash on initial load leaves the deck at its first slide. Navigation updates the hash using `replaceState`, so browser Back is not a per-slide history.

Use the on-screen arrows, Right/Left or Page Down/Page Up; Home/End select the first/last slide. `O` opens/closes the slide overview; Escape closes it. `F` requests/toggles browser fullscreen. Space normally advances, but on the staged design slide it toggles the design animation while that animation is incomplete. Rehearse the design reveal before presenting.

## SPIRIT-informed sequence and canonical source map

All canonical paths below are relative to the repository root `advance-trauma-trial/`. Follow the named sections and their included assets rather than treating a previous deck as the authority. `†` denotes a full-route-only detail slide; other slides are core, with brief membership listed above.

| Slide IDs, in presentation order | Purpose | Canonical sources / sections |
| --- | --- | --- |
| `title`, `background` | Identification and rationale | `shared-assets/study-identifiers.qmd`; `protocol/protocol.qmd` → Background and rationale; references displayed on `background` |
| `atls-purpose` | “Purpose and content”: original manual quote | ACS, *Advanced Trauma Life Support® (ATLS®) Student Course Manual*, 11th ed., 2025; source deck `presentations/atls-sweden-30-years/src/slides.ts` → `atls-purpose` |
| `clinician-evidence` | “Impact on clinicians”: original manual quote | Same ACS manual; Sweden source ID `atls-providers`; protocol → Background and rationale for educational versus patient-outcome evidence |
| `patient-evidence` | “Impact on patients”: original manual quote | Same ACS manual; Sweden source ID `atls-outcomes-claim`; protocol → Background and rationale, Updated systematic review for causal-evidence caveats |
| `systematic-reviews`, `atls-forest` | Review summary and updated forest plot | References displayed on these slides; Sweden source IDs `atls-outcomes-reviews`, `atls-forest`; inherited `presentations/standard-trial/src/data/meta-analysis.json`; protocol → Background and rationale, Updated systematic review (scope differs; see below) |
| `aim` | Trial objective | Protocol → Trial aim |
| `design`, `setting`, `cluster-eligibility` † | Design, setting, hospital/unit criteria | `protocol/protocol.qmd` → Overall trial design, Design justification, Cluster selection; `shared-assets/eligibility-criteria-cluster.qmd` |
| `eligibility` | Exact patient population | `shared-assets/eligibility-criteria-patient-participant.qmd`; protocol → Patient participant selection |
| `intervention`, `training` † | Intervention, comparator and delivery | `shared-assets/intervention-and-control-treatment.qmd`; protocol → Concomitant use of other medications or treatments |
| `allocation`, `timeline` | Randomisation, concealment/masking and participant timeline | Protocol → Randomisation, Blinding, Overall trial design; `protocol/functions/global_variables.R` for numeric design parameters |
| `primary-outcome`, `secondary-outcomes`, `schedule`, `nested-methods` † | Endpoints, assessment times and nested sampling | `shared-assets/outcomes.qmd`; protocol → Procedures, Overall trial design; `shared-assets/sample-size-calculations.qmd` → Nested staircase design and secondary outcomes |
| `sample-size`, `analysis`, `analysis-detail` † | Power assumptions, estimands/models and robustness | Protocol → Statistics; `shared-assets/sample-size-calculations.qmd`; `protocol/functions/global_variables.R` |
| `data`, `consent` | Collection, quality, confidentiality and consent | Protocol → Screening, Withdrawal criteria, Procedures, Monitoring, Procedure for obtaining consent, Data protection, Collection/handling/archiving of data |
| `safety`, `oversight` | Harms, monitoring, governance, funding and dissemination | Protocol → Handling of adverse and safety events, Trial organisation, Funding, Notification of trial completion/reporting/publication; `shared-assets/interim-analysis.qmd`, `shared-assets/study-identifiers.qmd` |
| `interpretation`, `closing` | Uncertainty, relevance and discussion | Protocol → Statistics, Trial aim; interpretation is presenter synthesis, not trial results |

## Evidence and forest-plot provenance

The five added background slides are core/full; only `atls-forest` is also in brief. `atls-purpose` (“Purpose and content”), `clinician-evidence` (“Impact on clinicians”) and `patient-evidence` (“Impact on patients”) reproduce the original Sweden quote bodies verbatim, attributed to the **American College of Surgeons, ATLS Student Course Manual, 11th ed., 2025**. These are **quoted manual claims, not ADVANCE TRAUMA findings** or new study summaries. The purpose quote no longer contains the earlier reach/count bullets; the clinician/patient quotes do not display the earlier small-study bullet summaries. Contextual studies in the speaker notes are explicitly off-slide.

Educational knowledge/performance gains do not by themselves establish a causal mortality benefit. The patient quote's causal-sounding wording and “equivalent patient survival” should be presented as the manual's claims, not as randomised causal evidence or a formal equivalence finding. Educational performance, observational patient associations and pooled estimates answer different questions. The inherited forest data are in [`src/data/meta-analysis.json`](src/data/meta-analysis.json), carried over from the Sweden source deck and its `src/data/meta-analysis.json`.

- The default **all-18-study** view uses the exported random-effects REML estimate: **OR 0.58 (95% CI 0.46–0.72), I² 75%**. This is a multiple-programme update, not 18 ATLS trials.
- The original published **Nakhid et al. (2026)** review summarised **17 observational studies**, **OR 0.60 (95% CI 0.48–0.75)**. The additional **Lule et al. (2026)** study is a **cluster randomised trial of RTTDC, not ATLS**.
- Filters and individual study selection **re-pool subsets live** in the browser; those are not published subset estimates. The full set uses the exported REML result; multi-study subsets use the browser's DerSimonian–Laird implementation (`src/forest-plot.ts`, `src/pool-meta.ts`). Do not quote a filtered diamond as the Nakhid published result or as definitive causal evidence for ATLS.
- `protocol/protocol.qmd` → Updated systematic review describes a separate ATLS-only, quality-rating-aware analysis. Its study set/estimate must not be conflated with this inherited multiple-programme forest. Reconcile scope explicitly when citing either analysis.

## Validation and PDF

Completed validation passed **`pnpm build`** and **`node visual-review/check.mjs`**. The recorded browser review covers all **29 slides at 1280×720 and 1920×1080**, all three route counts (25/13/29), overview opening/closing, and keyboard navigation. [`visual-review/results.json`](visual-review/results.json) records no overflow, branding collisions, broken images, browser runtime errors or print overflow. The generated [`standard-trial.pdf`](standard-trial.pdf) is the **25-page core deck**; PDF output is not PPTX export.

To repeat the browser check after building, run `node visual-review/check.mjs` from this directory. It serves `dist/`, launches headless **Microsoft Edge at a hard-coded macOS application path**, uses Node's global `WebSocket`, and writes review screenshots/results plus the core PDF. It is an Edge/macOS-specific check, not a portable cross-browser test or proof of scientific/ethical correctness. These documentation updates do not rerun or regenerate those artifacts.

## Before each use

- Check content against the applicable approved protocol/amendments and analysis plan. The source files contain planned procedures; they do **not** establish that approvals have been obtained or that procedures have been completed.
- **Verify the legal sponsor, sponsor responsibilities, funder roles and applicable regulatory/ethics/site approvals.** Do not infer legal sponsorship from a coordinating institution, investigator affiliation, insurance responsibility or funder name.
- The protocol Funding section names **Swedish Research Council (2023-03128)** and **Laerdal Foundation (2023-0297)**. Do not infer additional funding or funder influence from the other decks. Any additional attribution needs an authoritative source and confirmation of its role. Do not present historical funding-shortfall language as current status.
- Keep dates, batch starts, recruitment totals and completion/status claims out of this reusable narrative unless separately verified and explicitly dated. The sample size and design numbers are targets/assumptions, not achieved recruitment or results.
- Preserve the distinction between 13 inclusion months and 12 non-transition analysis months: 4,320 is the planned analysis denominator, not all inclusions at the same monthly rate. Available-data analysis does not guarantee complete follow-up.
- The protocol includes `shared-assets/sample-size-calculations.qmd`, which describes simple random sampling at shift level; the separate `shared-assets/sample-size-calculations-nested.qmd` uses “stratified random sampling by shift.” The slide deliberately says **randomised coordinator shifts**; reconcile the operational specification before giving a more specific description.
- Do not imply blinded assessment, a particular small-cluster correction, efficacy stopping boundaries, or a legal sponsor that the reviewed protocol does not specify. Do not turn a non-significant estimate into proof of no benefit.
- Rehearse the selected route, hash links, animated design and forest filters locally. The recorded validation above covers specific viewports and interactions, not every browser, interaction or subsequent content change.
