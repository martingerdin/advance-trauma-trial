# SAP revision to-do list v0.7.0

Actionable changes to the Statistical Analysis Plan, derived from reviewer comments in:

- `comments/v0.7.0-2026-10-07/statistical-analysis-plan-v0.7.0-2026-10-07 KH 2026-10-21_AO_JK_JM.docx` (KH 2026-10-20; AO 2026-10-21; JM/JK 2026-10-26)

Discussion and decisions were triaged in `comments/v0.7.0-2026-10-07/comment-triage-2026-11-09.md` and the meeting deck `meeting-deck-2026-11-10.md` (meeting 2026-11-10). Final decisions were taken at the SAP finalisation meeting on 2026-11-10 and are recorded in `../meetings/20261110/meeting-notes-2026-11-10.md` (diarized transcript: `../meetings/20261110/meeting-2026-11-10.named.txt`). Items B7–B18 below have been updated to reflect those decisions; B19–B20 track new decisions from the meeting.

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

Created 2026-11-10: A → [#78](https://github.com/martingerdin/advance-trauma-trial/issues/78); B1–B18 → [#79](https://github.com/martingerdin/advance-trauma-trial/issues/79) – [#96](https://github.com/martingerdin/advance-trauma-trial/issues/96). All labelled `sap-revision`, unassigned. Updated after the 2026-11-10 finalisation meeting: B7/B8 resolved (folded into B16/B15, issues #85/#86 closed); B9–B18 updated on GitHub to the final decisions; B19/B20 added and created as issues #98/#97.

### Power and sample size

- [ ] **B1. Add the assumed CV of cluster sizes to the power statement.** The power text says "allowed for substantial variation in cluster sizes" but gives no coefficient of variation. State the assumed CV (or reference where it is given). `../shared-assets/sample-size-calculations-main.qmd`. _(Comment id=55 KH)_ — needs the sample-size author's number. — [#79](https://github.com/martingerdin/advance-trauma-trial/issues/79)

- [ ] **B2. Reconcile the power statement with what the Shiny CRT Calculator supports.** The text says the calculation "incorporated the one month transition period" while also "allowed for substantial variation in cluster sizes"; KH notes the app cannot simultaneously allow varying cluster sizes and transition periods. Confirm what the app actually supported and correct the sentence. `../shared-assets/sample-size-calculations-main.qmd`. _(Comments id=56 KH, id=57 JK)_ — needs the sample-size author to confirm app capabilities. — [#80](https://github.com/martingerdin/advance-trauma-trial/issues/80)

**Verification note (2026-11-11) — applies to B1 and B2.** The saved power curves (`shared-assets/curvedata_*.csv`) were reproduced exactly with the Shiny CRT Calculator's custom-design-matrix engine (`vartheta_m_desmat`, trial's own 30×48 design matrix, ICC 0.02, CAC 0.9, `cv = 0`; RMS ≈ 1e-5 vs saved for both scenarios). Findings:
  1. The "Upload own design" path of the calculator **hard-codes `cv = 0`** and does not expose a CV field for uploaded design matrices (`ui.R` condition `choice2 != 'Upload matrix' & choice_corr != 'dtd'`; code comment: “Oct 2023 do not display ‘Varying cluster size’ option for user uploaded matrix as it is not an option”). So **no CV of cluster sizes was used in the calculation that produced the saved curves** — consistent with KH's comment 56 (app cannot combine varying cluster sizes with the custom design / transition periods).
  2. The curves match the calculator's **constant/exchangeable** within-cluster correlation (`type = 0`), **not** the “discrete time decay” structure the power text describes (`type = 1`; power at 12 patients/cluster/month ≈ 0.893 vs 0.947). This text-vs-curves discrepancy should be resolved when drafting the power statement.
  3. The only documented variation basis is the protocol's “cluster sizes are expected to vary between 12 and 20 patients per month” (protocol §Randomisation). Options for B1: cite that range (or a CV derived from it) rather than a recovered CV, or re-run on a built-in stepped-wedge tab where the CV field is available (needs a CV value / pilot data).

### Clarifications needed from reviewers / teams

- [ ] **B3. ICD chapter scope for trauma eligibility.** The eligibility text defines trauma by ICD chapter XX; JM asks whether chapter XIX should also be included. Confirm the intended chapters with the clinical team. `statistical-analysis-plan.qmd` §Eligibility (~line 240 in v0.7.0). _(Comment id=102 JM)_ — clinical team input. — [#81](https://github.com/martingerdin/advance-trauma-trial/issues/81)

- [ ] **B4. Discharge-then-readmission handling.** KH asks what happens if a patient is discharged and then readmitted (does the stay count as one or two? are readmissions included?). Define for ED/hospital/ICU stay outcomes. Related: repeated ED presentations are treated as independent (B16), and readmission handling interacts with the time-to-event LOS estimand (B18). `statistical-analysis-plan.qmd` §Length-of-stay sections. _(Comment id=133 KH)_ — clinical/data-team input. — [#82](https://github.com/martingerdin/advance-trauma-trial/issues/82)

- [ ] **B5. Clarify comment 155 ("get Res on these").** KH asked to "get Res on these" regarding the effect-measures table (ordinal EQ-5D-5L row). Clarify who/what "Res" refers to and what input is wanted. Flagged in the meeting for follow-up by Martin with KH; still open. _(Comment id=155 KH)_ — KH input. — [#83](https://github.com/martingerdin/advance-trauma-trial/issues/83)

- [ ] **B6. Clarify comment 214 (correlations section).** JK: "I couldn't see this being considered anywhere, but I may have missed it." Ask what she expected in §9.3 (correlations). `statistical-analysis-plan.qmd` §Correlations. _(Comment id=214 JK)_ — JK input. — [#84](https://github.com/martingerdin/advance-trauma-trial/issues/84)

- [x] **B7. Expected re-presentation frequency.** AO asks whether the same patient can contribute more than one ED presentation and how within-patient correlation is handled; JK suggests including only the first presentation (or treating repeats as independent, which is conservative). **Resolved at the SAP meeting (decision 10):** re-presentation is expected to be rare and presentations are treated as independent (see B16); the re-presentation-frequency data check is no longer a prerequisite for the decision, though the trial data can confirm the expected small number. `statistical-analysis-plan.qmd` §Analysis sets / outcomes. _(Comments id=258 AO, id=259 JK)_ — superseded by B16. — [#85](https://github.com/martingerdin/advance-trauma-trial/issues/85)

- [x] **B8. Expected age distribution across subgroup groups.** KH suggests two age groups instead of four; JM worries about precision with four groups. **Resolved at the SAP meeting (decision 9):** two conventional younger/older age groups are used (subject to protocol pre-specification, see B15), so the four-group distribution is no longer needed to decide the categorisation. `statistical-analysis-plan.qmd` §Subgroup analyses. _(Comments id=260 KH, id=261 AO, id=262 JM)_ — superseded by B15. — [#86](https://github.com/martingerdin/advance-trauma-trial/issues/86)

### Decided in the meeting — open issues to track the work

- [ ] **B9. Missing-data strategy (§9.4).** Decided at the SAP meeting (decision 2): present complete-case analysis as the default and use multiple imputation if primary-outcome missingness exceeds 5%, with the same threshold applied to covariate missingness in the adjusted analysis; use one imputation model and the same imputed datasets across all analyses, with at least 20 imputations chosen on the basis of the larger missingness requirement; describe MI under missing-data handling (not as a separate sensitivity analysis). This replaces the fixed 10% rule and the previous MICE route. `statistical-analysis-plan.qmd` §Missing data. _(Comments id=217 AO, id=218 JK, id=223 AO; related id=221 JM, id=224 JM, id=225 JM, id=222 KH)_ — [#87](https://github.com/martingerdin/advance-trauma-trial/issues/87)

**Open question (2026-11-11, email from Arpita Ghosh):** observations of QoL/disability unavailable because of *death* are currently treated like other missing values (or handled via the while-alive strategy). Whether "missing because dead" can be assumed MAR within the while-alive definition, and how the MI/available-case strategy should treat death-truncated outcomes, needs to be addressed when specifying §9.4 (see also B10).

- [ ] **B10. QoL and disability: joint longitudinal model.** Decided at the SAP meeting (decision 4): for each outcome, combine repeated measurements in one longitudinal model (time, time×treatment interaction, patient-level random effects) and report the treatment effect at each time point from the model, rather than differences between time points or separate per-time-point analyses. `statistical-analysis-plan.qmd` §QoL / §Disability. _(Comments id=185 AO, id=186 JK)_ — [#88](https://github.com/martingerdin/advance-trauma-trial/issues/88)

**Open question (2026-11-11, email from Arpita Ghosh) — needs team decision before drafting:** the joint longitudinal model treats QoL/disability observations unavailable because of *death* the same way as other missing observations, whereas the LoS analysis treats death as a competing event. Question: should the primary analysis for these outcomes explicitly account for death (e.g. beyond the current while-alive strategy + separate mortality analysis — e.g. joint modelling of the outcome and survival, or explicit handling of death-truncated outcomes)? Ask AN/JK/JM; overlaps with B9 (missing-data mechanism).

- [ ] **B11. Small-sample inference.** Decided at the SAP meeting (decision 6): defer fixing the small-sample correction for non-linear mixed models until after the forthcoming stepped-wedge simulation review of small-sample methods, and take the decision before the analysis (expected no earlier than late 2028, possibly 2029). Accept zero variance components; keep hospital-level randomisation (no unit-level random effect). `statistical-analysis-plan.qmd` §Analysis of the primary outcome / §Statistical principles. _(Comments id=166 AO, id=161 JK, id=162 JM, id=160 KH, id=206 AO, id=207 JK)_ — [#89](https://github.com/martingerdin/advance-trauma-trial/issues/89)

- [ ] **B12. Adjust the primary model for randomisation factors.** Decided at the SAP meeting (decision 5): include the anticipated cluster size recorded at randomisation (the CCR covariate) as a fixed covariate in the primary model — not the subsequently observed size — and make the cluster + cluster-by-period random effects explicit. `statistical-analysis-plan.qmd` §Analysis of the primary outcome. _(Comments id=219 KH, id=220 AO, id=163 KH, id=164 AO, id=165 JM)_ — [#90](https://github.com/martingerdin/advance-trauma-trial/issues/90)

- [ ] **B13. Interim analysis wording (§4.5).** Decided at the SAP meeting (decision 1): keep the term "interim analysis" exactly as used in the protocol, to avoid a protocol amendment. Distinguish the routine per-batch monitoring reports to the SDMC from the half-way review; the half-way review is a continue/stop decision based on feasibility (cluster inclusion, adherence to the randomisation schedule, missingness in key variables), with no treatment-arm comparisons and no outcome-based stopping rules. Refer to the SDMC charter for guidance (feasibility thresholds not yet set). `statistical-analysis-plan.qmd` §Interim analysis (+ shared include `../shared-assets/interim-analysis.qmd`). _(Comments id=63 JM, id=64 JK, id=65 KH, id=66 AO, id=67 JK)_ — [#91](https://github.com/martingerdin/advance-trauma-trial/issues/91)

- [ ] **B14. Sensitivity-analysis suite (§9.5): keep the full set.** Decided at the SAP meeting (decision 8): keep the full suite — 9.5.2 (autoregressive), 9.5.3 (random cluster × intervention), 9.5.4 (spline) and 9.5.5 (lag & weaning) are protocol-mandated; 9.5.1 (actual transition date) is the only SAP addition — and present them without emphasising them in the report. (Multiple imputation for missing outcomes is now specified under missing-data handling, see B9.) `statistical-analysis-plan.qmd` §Sensitivity analyses. _(Comments id=230 KH, id=233 KH, id=234 KH, id=235 AO, id=236 JK, id=240 KH, id=241 JM, id=245 KH, id=246 JK, id=250 KH, id=251 JK, id=252 JM)_ — [#92](https://github.com/martingerdin/advance-trauma-trial/issues/92)

- [ ] **B15. Subgroup analyses: age groups and region.** Decided at the SAP meeting (decision 9): use two conventional younger/older age groups (cut-off fixed before analysis) provided this is consistent with protocol pre-specification; use zonal councils (Northern, Southern, Central, Eastern, Western, North Eastern) instead of individual states for geographical region. `statistical-analysis-plan.qmd` §Subgroup analyses. _(Comments id=260 KH, id=261 AO, id=262 JM, id=263 KH, id=264 AO)_ — [#93](https://github.com/martingerdin/advance-trauma-trial/issues/93)

- [ ] **B16. Repeated ED presentations: inclusion rule.** Decided at the SAP meeting (decision 10): treat presentations as independent rather than restricting to the first admission per patient, and state that a small number of patients may present more than once; no within-patient correlation is modelled. (Re-presentation is expected to be rare; see B7.) `statistical-analysis-plan.qmd` §Analysis sets / outcomes. _(Comments id=258 AO, id=259 JK)_ — [#94](https://github.com/martingerdin/advance-trauma-trial/issues/94)

- [ ] **B17. Binary effect measures: OR and ARD (§8.4, §9.1).** Decided at the SAP meeting (decision 11): keep the odds ratio as the primary effect measure alongside the absolute risk difference (ARD), reporting the OR alone if the ARD model fails to converge; do not adopt marginal standardisation to also report a risk ratio alongside the OR and RD. (The adherence outcome is modelled as a count — see B19.) `statistical-analysis-plan.qmd` §Effect measures. _(Comments id=169 JK, id=155 KH; see B5 for the "get Res on these" clarification)_ — [#95](https://github.com/martingerdin/advance-trauma-trial/issues/95)

- [ ] **B18. Length-of-stay: time-to-event estimand (§9.2.5/6/8).** Decided at the SAP meeting (decision 3): analyse ED, hospital and ICU length of stay as time-to-event outcomes with death as a competing risk; confirm the estimand as observed duration until exit (interpreted alongside mortality and transfer frequencies). Clarify transfer handling for each outcome: mortality follow-up continues across hospitals, but transfer to another hospital interrupts ICU observation and is an intercurrent event. LOS is recorded as date & time (continuous durations) for all three outcomes. **Note:** this reverses the earlier decision in the SAP text (v0.6.0 A2/C2 replaced Fine–Gray competing-risks models with mixed-effects negative binomial count models — see the HTML comment above §ED length of stay); the meeting returns to a time-to-event approach with death as a competing risk. `statistical-analysis-plan.qmd` §Length-of-stay sections. _(Comments id=148 AO, id=195 AO, id=196 JK, id=123 KH, id=124 KH)_ — [#96](https://github.com/martingerdin/advance-trauma-trial/issues/96)

- [ ] **B19. Adherence outcome: count of completed checklist steps.** Decided at the SAP meeting (decision 12): model the number of completed ATLS checklist steps (out of 14) as a count outcome (e.g. Poisson with an offset for the total steps), rather than a beta-regression proportion on the logit scale or a dichotomised version. `statistical-analysis-plan.qmd` §Adherence. _(Comments id=156 KH, id=157 AO)_ — new issue [#98](https://github.com/martingerdin/advance-trauma-trial/issues/98).

- [ ] **B20. Intercurrent-events table restructure (§8.1).** Decided at the SAP meeting (decision 7): handle loss to follow-up and withdrawal under missing data (§9.4) rather than as intercurrent events (remove them from the intercurrent-events table); retain treatment-policy handling of cluster non-adherence, with exposure defined by the scheduled transition dates. `statistical-analysis-plan.qmd` §Intercurrent events. _(Comments id=130 JK, id=131 KH, id=132 AO, id=134 KH, id=135 AO, id=137 AO, id=142 JK, id=145 KH, id=146 AO)_ — new issue [#97](https://github.com/martingerdin/advance-trauma-trial/issues/97).

---

## C. Already resolved in source — verify only — [verify]

- [ ] **C1. Batches defined by entry into the study** (36 JM): text says "Clusters will be sequentially allocated to batches based on when they enter the study". ✓ confirm.
- [ ] **C2. Repeated-measurement wording** (37 JM, 38 JK): text says "We include individual patients cross-sectionally for the primary outcome, although we assess some secondary outcomes at multiple follow-up time points". ✓ confirm.
- [ ] **C3. Descriptive-table key/full version wording** (80 KH, 81 AO): text says "we present the key version in the main text and the full version in the supplementary material". ✓ confirm.
- [ ] **C4. Secondary-outcome model-sequence wording** (177 KH, 178 AO, 179 AO, 180 JM): text says "We will not repeat the model sequence for each secondary outcome. If the selected model does not converge … we will continue down the same sequence for that outcome." ✓ confirm.
- [ ] **C5. Outcome definitions and ICE handling** (123 KH, 124 KH, 126 KH, 130 JK, 131 KH, 132 AO, 134 KH, 135 AO, 136 KH, 137 AO, 142 JK, 145 KH, 146 AO): ED LOS routes, ICU admission classified by whether admission occurred first, LFU/missing handled under missing data, transfer/death handling. Largely covered by v0.6.0 items A2/C1/C2; verify each against the current text. (The structural restructure of §8.1 is tracked as B20.)
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

One issue per item in section B, titled `SAP: <short description>`, labelled `sap-revision`, body citing comment IDs and the input/decision needed.

Status after the SAP finalisation meeting (2026-11-10) and the issue update pass (2026-11-11):
- Issues A → #78 and B1–B18 → #79–#96 were created 2026-11-10.
- B7/#85 and B8/#86 were resolved by the meeting decisions (folded into B16/#94 and B15/#93); bodies updated and the issues closed on GitHub.
- B9–B18 issue bodies were updated directly on GitHub to the final decisions recorded in `../meetings/20261110/meeting-notes-2026-11-10.md` (pre-meeting "meeting suggestion" wording replaced). Titles updated where the decision changed the scope (##​87, #89, #91, #94, #95, #96).
- B19 → issue #98 (adherence as a count outcome) created; B20 → issue #97 (intercurrent-events table restructure) created, both labelled `sap-revision`.
- B1–B6 remain open clarifications awaiting reviewer/team input.

**Order of work from here:**
1. Work through the open issues one by one: for [edit]-ready issues apply the edit with an HTML comment citation; for input-needed issues chase the input.
2. Section A edits already applied.