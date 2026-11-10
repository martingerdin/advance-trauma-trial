# SAP revision to-do list v0.7.0

Actionable changes to the Statistical Analysis Plan, derived from reviewer comments in:

- `comments/v0.7.0-2026-10-07/statistical-analysis-plan-v0.7.0-2026-10-07 KH 2026-10-21_AO_JK_JM.docx` (KH 2026-10-20; AO 2026-10-21; JM/JK 2026-10-26)

Discussion and decisions were triaged in `comments/v0.7.0-2026-10-07/comment-triage-2026-11-09.md` and the meeting deck `meeting-deck-2026-11-10.md` (meeting 2026-11-10).

## How to use this list

- Primary source file to edit: `statistical-analysis-plan.qmd`.
- Several passages live in shared includes under `../shared-assets/`. Edit the include only if explicitly agreed; prefer SAP-only text.
- Each item cites the originating comment(s) and reviewer. Items tagged **[edit]** are straightforward, essentially resolved edits. Items tagged **[issue]** need a GitHub issue (input or discussion) before editing. Items tagged **[verify]** may already be satisfied; confirm before closing. Items tagged **[done]** need no work.
- For every substantive edit, add an HTML comment immediately above the changed passage citing the to-do ID and comment IDs, e.g. `<!-- sap-revision-todo-v0.7.0 E1 (comments id=33 KH, id=46 JM): ... -->`.
- Keep SAP edits minimal and consistent in style and tone with the existing SAP text.
- Check corresponding text in `../protocol/protocol.qmd` before finalising substantive analysis changes. Default: keep the SAP aligned with the protocol. Add minimal items to `protocol-edits-todo.md` only for unavoidable conflicts.
- Mark an item complete only after the edits have been explicitly approved.
- After edits, re-render to confirm the document still builds.

---

## A. Straightforward edits (apply now) — [edit] — [#78](https://github.com/martingerdin/advance-trauma-trial/issues/78)

- [x] **A1. Define "unit" of physicians.** The trial-design text says "we train more than one unit of physicians in the same hospital" without defining "unit". Define it at first use (a group of physicians who staff the emergency department together). `statistical-analysis-plan.qmd` §Trial design (~line 143). _(Comments id=33 KH, id=46 JM)_ — applied with HTML comment.

- [x] **A2. Clarify "equal allocation across sequences".** The design text's "(equal allocation across sequences)" parenthetical confused reviewers (equal chance vs equal numbers). Removed the parenthetical; the sentence now states "one cluster allocated to each sequence of each batch" directly (equal numbers). `statistical-analysis-plan.qmd` §Trial design (~line 146). _(Comments id=34 JK, id=35 JK)_ — applied with HTML comment.

- [x] **A3. Define "key outcome analyses".** The reporting text says "We will present key outcome analyses in the main text … and all outcome analyses in the supplementary material" without defining "key". Define which analyses are "key" (e.g. primary outcome plus prespecified key secondary outcomes, consistent with the key descriptive summaries). `statistical-analysis-plan.qmd` §Presentation of analysis results (~line 837). _(Comment id=271 JM)_ — applied with HTML comment (key = primary + secondary outcomes at summary level; the all-table adds EQ-5D-5L domain and VAS analyses).

---

## B. GitHub issues (need input or discussion before editing) — [issue]

Created 2026-11-10: A → [#78](https://github.com/martingerdin/advance-trauma-trial/issues/78); B1–B18 → [#79](https://github.com/martingerdin/advance-trauma-trial/issues/79) – [#96](https://github.com/martingerdin/advance-trauma-trial/issues/96). All labelled `sap-revision`, unassigned.

### Power and sample size

- [ ] **B1. Add the assumed CV of cluster sizes to the power statement.** The power text says "allowed for substantial variation in cluster sizes" but gives no coefficient of variation. State the assumed CV (or reference where it is given). `../shared-assets/sample-size-calculations-main.qmd`. _(Comment id=55 KH)_ — needs the sample-size author's number. — [#79](https://github.com/martingerdin/advance-trauma-trial/issues/79)

- [ ] **B2. Reconcile the power statement with what the Shiny CRT Calculator supports.** The text says the calculation "incorporated the one month transition period" while also "allowed for substantial variation in cluster sizes"; KH notes the app cannot simultaneously allow varying cluster sizes and transition periods. Confirm what the app actually supported and correct the sentence. `../shared-assets/sample-size-calculations-main.qmd`. _(Comments id=56 KH, id=57 JK)_ — needs the sample-size author to confirm app capabilities. — [#80](https://github.com/martingerdin/advance-trauma-trial/issues/80)

### Clarifications needed from reviewers / teams

- [ ] **B3. ICD chapter scope for trauma eligibility.** The eligibility text defines trauma by ICD chapter XX; JM asks whether chapter XIX should also be included. Confirm the intended chapters with the clinical team. `statistical-analysis-plan.qmd` §Eligibility (~line 240 in v0.7.0). _(Comment id=102 JM)_ — clinical team input. — [#81](https://github.com/martingerdin/advance-trauma-trial/issues/81)

- [ ] **B4. Discharge-then-readmission handling.** KH asks what happens if a patient is discharged and then readmitted (does the stay count as one or two? are readmissions included?). Define for ED/hospital/ICU stay outcomes. `statistical-analysis-plan.qmd` §Length-of-stay sections. _(Comment id=133 KH)_ — clinical/data-team input. — [#82](https://github.com/martingerdin/advance-trauma-trial/issues/82)

- [ ] **B5. Clarify comment 155 ("get Res on these").** KH asked to "get Res on these" regarding the effect-measures table (ordinal EQ-5D-5L row). Clarify who/what "Res" refers to and what input is wanted. _(Comment id=155 KH)_ — KH input. — [#83](https://github.com/martingerdin/advance-trauma-trial/issues/83)

- [ ] **B6. Clarify comment 214 (correlations section).** JK: "I couldn't see this being considered anywhere, but I may have missed it." Ask what she expected in §9.3 (correlations). `statistical-analysis-plan.qmd` §Correlations. _(Comment id=214 JK)_ — JK input. — [#84](https://github.com/martingerdin/advance-trauma-trial/issues/84)

- [ ] **B7. Expected re-presentation frequency.** AO asks whether the same patient can contribute more than one ED presentation and how within-patient correlation is handled; JK suggests including only the first presentation (or treating repeats as independent, which is conservative). Confirm how common re-presentation is expected to be. `statistical-analysis-plan.qmd` §Analysis sets / outcomes. _(Comments id=258 AO, id=259 JK)_ — data-team input. — [#85](https://github.com/martingerdin/advance-trauma-trial/issues/85)

- [ ] **B8. Expected age distribution across subgroup groups.** KH suggests two age groups instead of four; JM worries about precision with four groups. Get the expected patient distribution across the current groups before deciding the categorisation. `statistical-analysis-plan.qmd` §Subgroup analyses. _(Comments id=260 KH, id=261 AO, id=262 JM)_ — data-team input. — [#86](https://github.com/martingerdin/advance-trauma-trial/issues/86)

### Decided in the meeting — open issues to track the work

- [ ] **B9. Missing-data strategy: replace the 10% rule.** Meeting suggestion: drop the fixed-threshold framing; pre-specify the primary analysis on all available observed outcome data; use multiple imputation as a sensitivity analysis under MAR; consider missingness separately per outcome. This changes `#sec-missing-data` and the MICE route. `statistical-analysis-plan.qmd` §Missing data. _(Comments id=217 AO, id=218 JK, id=223 AO; related id=221 JM, id=224 JM, id=225 JM, id=222 KH)_ — [#87](https://github.com/martingerdin/advance-trauma-trial/issues/87)

- [ ] **B10. QoL and disability: joint longitudinal model.** Meeting suggestion: one joint longitudinal model (time, time×treatment interaction, patient-level random effects), reporting the effect at each time point from the model, instead of the implied separate per-time-point analyses. `statistical-analysis-plan.qmd` §QoL / §Disability. _(Comments id=185 AO, id=186 JK)_ — [#88](https://github.com/martingerdin/advance-trauma-trial/issues/88)

- [ ] **B11. Small-sample correction for non-linear models.** Pre-specify the correction for GLMMs (or objective selection criteria), accept zero variance components, keep hospital-level randomisation (no unit-level random effect). `statistical-analysis-plan.qmd` §Analysis of the primary outcome / §Statistical principles. _(Comments id=166 AO, id=161 JK, id=162 JM, id=160 KH, id=206 AO, id=207 JK)_ — [#89](https://github.com/martingerdin/advance-trauma-trial/issues/89)

- [ ] **B12. Adjust the primary model for randomisation factors.** Include the anticipated cluster size (CCR covariate) as a fixed covariate in the primary model and make the cluster + cluster-by-period random effects explicit. `statistical-analysis-plan.qmd` §Analysis of the primary outcome. _(Comments id=219 KH, id=220 AO, id=163 KH, id=164 AO, id=165 JM)_ — [#90](https://github.com/martingerdin/advance-trauma-trial/issues/90)

- [ ] **B13. Interim analysis wording.** Adopt "routine monitoring reports" for per-batch reports and reserve "interim analysis" for the half-way review; no formal effect-based stopping rules; clarify the half-way report contents. `statistical-analysis-plan.qmd` §Interim analysis (+ shared include `../shared-assets/interim-analysis.qmd`). _(Comments id=63 JM, id=64 JK, id=65 KH, id=66 AO, id=67 JK)_ — [#91](https://github.com/martingerdin/advance-trauma-trial/issues/91)

- [ ] **B14. Sensitivity-analysis suite: protocol alignment.** The protocol mandates 9.5.2 (autoregressive), 9.5.3 (random cluster × intervention), 9.5.4 (spline), 9.5.5 (lag & weaning); only 9.5.1 (actual transition date) is an SAP addition. Keep the protocol-mandated analyses; decide on 9.5.1; add multiple imputation under MAR as a sensitivity analysis for missing outcome data. `statistical-analysis-plan.qmd` §Sensitivity analyses. _(Comments id=230 KH, id=233 KH, id=234 KH, id=235 AO, id=236 JK, id=240 KH, id=241 JM, id=245 KH, id=246 JK, id=250 KH, id=251 JK, id=252 JM)_ — [#92](https://github.com/martingerdin/advance-trauma-trial/issues/92)

- [ ] **B15. Subgroup analyses: age groups and region.** Use two clinically justified age groups (cut-off fixed before analysis); for region use zonal councils (Northern, Southern, Central, Eastern, Western, North Eastern) per the meeting deck. `statistical-analysis-plan.qmd` §Subgroup analyses. _(Comments id=260 KH, id=261 AO, id=262 JM, id=263 KH, id=264 AO)_ — [#93](https://github.com/martingerdin/advance-trauma-trial/issues/93)

- [ ] **B16. Repeated ED presentations: inclusion rule.** Include only the first presentation per patient and document the rule, unless the data check shows re-presentation is common (alternative: treat presentations as independent). `statistical-analysis-plan.qmd` §Analysis sets / outcomes. _(Comments id=258 AO, id=259 JK)_ — [#94](https://github.com/martingerdin/advance-trauma-trial/issues/94)

- [ ] **B17. Effect measures: OR/ARD vs RR; AME for adherence.** Meeting suggestion: keep OR and ARD for binary outcomes; report the average marginal effect (difference in expected percentage of checklist items completed) for the adherence proportion instead of the logit-scale difference. `statistical-analysis-plan.qmd` §Effect measures / §Adherence. _(Comments id=169 JK, id=155 KH, id=156 KH, id=157 AO)_ — [#95](https://github.com/martingerdin/advance-trauma-trial/issues/95)

- [ ] **B18. Length-of-stay estimand wording.** Confirm the estimand as observed duration until exit (interpreted alongside mortality and transfer frequencies); the LOS data are recorded as date & time (continuous durations) for all three outcomes. `statistical-analysis-plan.qmd` §Length-of-stay sections. _(Comments id=148 AO, id=195 AO, id=196 JK, id=123 KH, id=124 KH)_ — [#96](https://github.com/martingerdin/advance-trauma-trial/issues/96)

---

## C. Already resolved in source — verify only — [verify]

- [ ] **C1. Batches defined by entry into the study** (36 JM): text says "Clusters will be sequentially allocated to batches based on when they enter the study". ✓ confirm.
- [ ] **C2. Repeated-measurement wording** (37 JM, 38 JK): text says "We include individual patients cross-sectionally for the primary outcome, although we assess some secondary outcomes at multiple follow-up time points". ✓ confirm.
- [ ] **C3. Descriptive-table key/full version wording** (80 KH, 81 AO): text says "we present the key version in the main text and the full version in the supplementary material". ✓ confirm.
- [ ] **C4. Secondary-outcome model-sequence wording** (177 KH, 178 AO, 179 AO, 180 JM): text says "We will not repeat the model sequence for each secondary outcome. If the selected model does not converge … we will continue down the same sequence for that outcome." ✓ confirm.
- [ ] **C5. Outcome definitions and ICE handling** (123 KH, 124 KH, 126 KH, 130 JK, 131 KH, 132 AO, 134 KH, 135 AO, 136 KH, 137 AO, 142 JK, 145 KH, 146 AO): ED LOS routes, ICU admission classified by whether admission occurred first, LFU/missing handled under missing data, transfer/death handling. Largely covered by v0.6.0 items A2/C1/C2; verify each against the current text.
- [ ] **C6. Correlations/ICC derivation** (211 KH, 212 AO, 213 JK): text (B5/B6 of v0.6.0) says correlations derived from variance components, latent-scale for binary/ordinal. ✓ confirm. (Note: 214 JK is a separate clarification, see B6.)

---

## D. No work needed — [done]

- [x] 125 KH (self-retracted: "all clear on the next page — ignore me")
- [x] 147 KH ("not technically a strategy, but … keep as is")
- [x] 162 JM (agreement with JK's t(K−2) approach)
- [x] 222 KH (assigns cluster-aware imputation to JM; track under B9)
- [x] 246 JK (agrees to remove spline SA; superseded by protocol-alignment decision B14)
- [x] 251 JK (agrees to keep lag/weaning SA; covered by B14)
- [x] 252 JM (supports keeping lag/weaning SA; covered by B14)

---

## GitHub issue plan (draft)

Opening one issue per item in section B (B1–B18), titled `SAP: <short description>`, labelled `sap-revision`, body citing comment IDs and the input/decision needed. Items B5, B6, B7, B8 depend on reviewer/team input; B9–B18 track meeting decisions. Issue bodies will be drafted from the comments when created.

**Order of work after this plan is agreed:**
1. Create issues B1–B18 (labels: `sap-revision`).
2. Work through them one by one: for [edit]-ready issues apply the edit with an HTML comment citation; for input-needed issues chase the input.
3. Apply section A edits (straightforward, can be done alongside).