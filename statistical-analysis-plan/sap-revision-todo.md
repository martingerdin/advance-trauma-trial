# SAP revision to-do list

Actionable changes to the Statistical Analysis Plan, derived from reviewer comments in:

- `comments/statistical-analysis-plan-v0.6.0-2026-03-04 KH   2026-04-15_AO.docx` (Karla Hemming review; Anna Olofsson + Martin Gerdin Wärnberg responses)
- `comments/Round Two Checklist V0.17  2026-03-23_AO_MGW 2026-04-15_AO.docx` (Round-two reporting checklist)

## How to use this list

- Primary source file to edit: `statistical-analysis-plan.qmd`.
- Several passages live in shared includes under `../shared-assets/` (e.g. `outcomes.qmd`, `sample-size-calculations-main.qmd`, `sample-size-calculations-nested.qmd`). Edit the include, not the rendered copy.
- Each item cites the originating comment(s) and reviewer. Items tagged **[decision]** need a human/statistician judgement call before editing — flag these rather than guessing. Items tagged **[verify]** may already be partly done; confirm the current text satisfies the comment before marking complete.
- For every substantive edit made for a to-do item, add an HTML comment immediately above the changed passage citing the to-do ID and originating comment IDs, e.g. `<!-- sap-revision-todo A1 (comments id=199 KH, id=200 AO): short rationale. -->`. Agent behaviour for this workflow is also in `.cursor/rules/sap-revision-todo.mdc`.
- Keep SAP edits **minimal** and **consistent in style and tone** with the existing SAP text (concise declarative sentences; no unnecessary rewriting or long justifications).
- Check corresponding text in `../protocol/protocol.qmd` before finalising substantive analysis changes. **Default: keep the SAP aligned with the protocol** (elaborate/operationalise; do not contradict). Do **not** edit `../shared-assets/` includes shared with the protocol unless explicitly agreed — prefer SAP-only text. Only if an approved SAP change unavoidably conflicts, add a **minimal** item to `protocol-edits-todo.md` — do not edit the protocol until the SAP revision is finished.
- Mark an item complete only after the edits have been explicitly approved.
- After edits, re-render to confirm the document still builds.

---

## A. Substantive statistical decisions (resolve first — may change downstream text)

- [x] **A1. Replace "RSPL" with the actual R estimation method.** The SAP states models are fitted using "residual pseudo-likelihood estimation based on linearisation with subject-specific expansion (RSPL)" (`statistical-analysis-plan.qmd` ~line 392, "Analysis of the primary outcome"). RSPL is a SAS `PROC GLIMMIX` method with no exact R equivalent. Describe the estimation method actually used in R (e.g. Laplace approximation or adaptive Gauss–Hermite quadrature via `glmmTMB`/`lme4::glmer`; or PQL via `MASS::glmmPQL`) and name the package(s). Confirm the chosen small-sample/Kenward–Roger correction is available for that package. _(Comments id=199 KH, id=200 AO)_ **[decision]**

- [x] **A2. Reconcile the length-of-stay model choice (Fine–Gray vs count model).** For length of ED stay, hospital stay and ICU stay the SAP previously specified a mixed-effects Fine–Gray competing-risks model. Anna (email) and reviewers stressed clarifying whether the estimand is time to a specific disposition (competing risks) vs length of stay as duration (simpler count/duration model), which also drives the effect measure in the outcomes table. Decision: negative binomial count models (hours for ED; days for hospital/ICU), with death/transfer truncating observed stay. _(Comments id=349, id=350 KH; id=351 AO; id=348 AO; Anna email)_ **[decision]**

- [x] **A3. Decide whether to keep "Models with correlated random cluster-by-intervention effects."** This exploratory sensitivity analysis (`statistical-analysis-plan.qmd` ~lines 478–508, section 5.10.1.4) was flagged as possibly unnecessarily complex. Decide to retain or remove; if retained, justify its added value briefly. _(Comments id=220 KH, id=221 AO)_ **[decision]**

- [x] **A4. Reconcile the three-level hierarchy (hospitals → teams/units → patients) with the models.** The "Analysis sets" section describes hospitals (clusters), clinical units within hospitals, and individual patients (`statistical-analysis-plan.qmd` ~line 253), but the models include only cluster and cluster-by-period random effects. Either add/justify a team/unit level in the random-effects structure or explicitly state and justify why the team level is not modelled. Also standardise terminology: the review refers to "teams", the SAP uses "clinical units"/"trained units" — pick one term and use it consistently here and in the methods/analysis sections. _(Comments id=102 KH, id=103 AO)_ **[decision]**

- [x] **A5. Reconcile the missing-data strategy with the complete-case + adjusted-analysis approach.** Anna (email) confirmed the meeting agreement: primary unadjusted analysis on all available primary-outcome data; adjusted analyses in the complete-case population defined by adjustment covariates; unadjusted analysis repeated in that same complete-case population for like-for-like comparison. Implemented as a fork on primary-outcome missingness (`#sec-treatment-of-missing-data`): **&lt;10%** → that available-case pipeline; **≥10%** → MICE (primary outcome, secondary outcomes as required, and adjustment covariates), with complete-case analyses also presented for comparison. Secondary outcomes are imputed only under the MICE route. Analysis sets text updated accordingly. _(Comment id=260 AO; Anna email)_ **[decision]**

- [x] **A6. Clarify the primary model's time parametrisation vs the "batch-specific secular trends" sensitivity analysis.** The primary model uses separate fixed period effects per batch (`β_bt`, `statistical-analysis-plan.qmd` ~lines 342, 375), yet the reviewer exchange describes the primary model as assuming a *common* secular trend, with batch-specific trends only in a sensitivity analysis (section 5.10.1.2, ~line 429). Resolve this apparent contradiction and make the primary-model time assumption and the sensitivity-analysis contrast internally consistent. _(Comments id=212 KH, id=213 AO)_ **[decision]**

---

## B. Analysis-model clarifications

- [x] **B1. Clarify the random-effects structure in the "shared period effects" model.** In the model-sequence bullet "Model with shared period effects" (`statistical-analysis-plan.qmd` ~line 354, `@eq-shared-period-model`), state explicitly which random effects are retained (cluster random intercept and cluster-by-period random effect) and clarify the batch indexing. _(Comments id=191 KH, id=192 AO)_

- [x] **B2. Add a sentence on how model assumptions/diagnostics will be checked.** In the primary-outcome analysis and secondary-outcomes sections, state that appropriate diagnostics will assess distributional assumptions, the proportional-odds assumption (for the cumulative logit / ordinal models), and model convergence. _(Comments id=24 MGW, id=25 AO; Round Two)_

- [x] **B3. Add reporting of standard errors and confidence intervals.** In the primary-outcome analysis, state that standard errors will be model-based, that small-sample adjustments (including degrees-of-freedom corrections, e.g. Kenward–Roger) will be applied where relevant, and that confidence intervals will be Wald-type. _(Comments id=21, id=22 AO; Round Two checklist items 27c/27d)_

- [x] **B4. Add the general estimation framework statement.** Briefly state the general estimation framework for the fitted mixed-effects models (see also A1). Implementation-specific numerical details need not be prespecified. _(Comment id=23 AO; Round Two 27b)_

- [x] **B5. Add reporting of correlation parameters (from the protocol).** State that time-adjusted within-cluster correlations will be reported with 95% CIs, alongside the correlations implied by the assumed correlation structures, all estimated variance components, and latent-scale correlations for binary outcomes. _(Comments id=26, id=27 AO; Round Two)_

- [x] **B6. Add how ICC and other correlation parameters will be estimated.** State that intra-cluster and related correlation parameters will be derived from the estimated variance components of the fitted models, and that for AR(1) within-cluster structures the correlation parameter is estimated directly from the AR(1) structure. _(Comments id=28, id=29 AO; Round Two)_

- [x] **B7. Add clustering to the missing-data / imputation strategy.** In the missing-data section, state that any multiple-imputation model will reflect the hierarchical data structure — clustering at hospital level plus design variables such as period and intervention exposure. _(Comments id=31 MGW, id=32 AO; Round Two)_ (Coordinate with A5.)

- [x] **B8. Confirm the primary-outcome model uses cluster-level / hospital-level clustering and gives cluster-specific estimates.** Ensure the primary and secondary analysis text states the mixed-effects models account for clustering at hospital level, use outcome-appropriate link functions/effect measures, and provide cluster-specific estimates of the intervention effect. _(Comment id=20 AO; Round Two 27a)_ **[verify]**

---

## C. Outcomes: definitions and handling of intercurrent events

- [x] **C1. Specify how death and transfer to another hospital are handled per outcome.** For outcomes where death or transfer may preclude measurement (e.g. ICU admission, length-of-stay outcomes), specify the intercurrent-event strategy (e.g. competing-event treatment, censoring, or exclusion) in the analysis text and in the outcomes summary table. Check this is applied consistently across all affected outcomes, not just length of ED stay. _(Comments id=55, id=56 KH; id=57, id=58 AO/MGW; Round Two id=11)_ **[verify]**

- [x] **C2. Clarify the definition of length of emergency department stay.** Define it as time to ED exit and enumerate the possible exit routes (ward admission, ICU admission, transfer, death, discharge home), making explicit how each is treated in the analysis. _(Comment id=348 AO)_ (Coordinate with A2.)

- [x] **C3. State that adherence to ATLS® is measured in both arms.** In the adherence outcome section (`statistical-analysis-plan.qmd` ~line 659) make clear the checklist is assessed in both standard-care and ATLS® periods/arms. _(Comment id=361 KH)_

- [x] **C4. Express adherence as a percentage and remove the duplicated paragraph.** Confirm adherence is defined as the proportion of completed checklist items (0–1), with completion of all 14 steps = 100% adherence, and present/interpret it as a percentage. NOTE: the adherence description paragraph is currently duplicated verbatim (`statistical-analysis-plan.qmd` ~lines 661–663) — delete the duplicate. _(Comments id=61 KH, id=62 AO, id=63 MGW)_

- [x] **C5. State that nested-design secondary outcomes are collected within the nested staircase design, and clarify period effects.** For quality of life (5.11.2), disability (5.11.3), and adherence (5.11.9), restate that these are collected within the nested staircase design, and clarify that period effects are defined on the global study timeline (not separately within each batch) and that not all clusters contribute data in every period. _(Comments id=288, id=289 KH; id=290, id=291 AO)_

- [x] **C6. Clarify the EQ-5D-5L outcome structure.** State clearly that the EQ-5D-5L yields two outcome types: five ordinal domain scores (each 1–5) analysed with the cumulative-logit model, and a separate VAS (0–100) analysed as continuous. Ensure the "five ordinal dimensions" wording is unambiguous about the 1–5 Likert scale. _(Comments id=295, id=296 KH/AO; id=303 KH)_ **[verify]**

- [x] **C7. Harmonise EQ-5D instrument naming (and confirm 5L not 3L).** Anna noted inconsistent naming across documents (e.g. EQ5D3L / EQ5D5L / EQ-5D-5L / EQ-5D5L). Standardise on one form (prefer official **EQ-5D-5L**) in the SAP analysis text, `@tbl-outcomes-summary`, and shared includes (`../shared-assets/outcomes.qmd`, sample-size nested text). Confirm the instrument is 5L throughout (CRF uses EQ-5D-5L; `protocol/variables.csv` still says EQ-5D-3L — add a minimal protocol-edits item only if needed). _(Anna email)_

---

## D. Design, randomisation and sample size

- [x] **D1. Soften the fixed "6-month overlap" commitment in the design description.** Reviewer felt the SAP should not tie itself down too tightly to the anticipated overlap between successive batches (`statistical-analysis-plan.qmd` ~line 93). Reword to indicate this is anticipated/approximate rather than fixed. _(Comment id=36 KH)_ **[decision]**

- [-] **D2. Add detail on the CCR random seed / random selection step.** Reviewer asked how the seed was chosen for covariate-constrained randomisation (`statistical-analysis-plan.qmd` ~lines 149–155). James (JM) to add how the final allocation was selected at random (seed / reproducibility). _(Comments id=43 KH, id=44 MGW — "James to add")_ **[decision]**

- [x] **D3. Add allocation ratio and repeated-measures statement to the trial-design description.** Ensure the "Design" section (`statistical-analysis-plan.qmd` ~lines 89–95) explicitly gives the definition of cluster, target number of clusters, number of intervention conditions, allocation ratio, whether clusters/participants are repeatedly measured, number of sequences, clusters per sequence, number of periods, and anticipated calendar duration. _(Comments id=3 MGW, id=4 AO; Round Two)_ **[verify]**

- [x] **D4. Consider referencing the protocol for full sample-size methodology.** In `../shared-assets/sample-size-calculations-main.qmd`, consider adding that the sample-size calculations were performed during the trial design phase (using the Shiny CRT Calculator) and that full methodological detail is in the protocol. _(Comments id=73 KH, id=74 AO, id=75 MGW — open discussion)_ **[decision]** — SAP-only Shiny CRT sentence; no protocol cross-ref

---

## E. Baseline and descriptive summaries

- [x] **E1. Base patient-characteristics summaries on all eligible patients across the whole trial.** Reviewer noted "baseline" should describe all patients meeting eligibility criteria for the entire trial period, not only pre-training periods. Update the "Patient characteristics" text (`statistical-analysis-plan.qmd` ~line 296) accordingly, keeping the by-group and overall summaries and the no-clustering-adjustment statement. _(Comments id=166 KH, id=167 AO)_ **[verify]**

- [x] **E2. Ensure both cluster-level and individual-level baseline characteristics are listed.** Confirm the SAP lists the baseline characteristics to be summarised at both cluster level (`@tbl-cluster-characteristics`) and individual level (`@tbl-patient-characteristics`). _(Comment id=10 MGW; Round Two)_ **[verify]**

---

## F. Subgroup analyses

- [x] **F1. Confirm subgroup analyses target the primary outcome only, via interaction terms.** The subgroup section (`statistical-analysis-plan.qmd` ~line 555) should state subgroups are analysed for the primary outcome only, list the prespecified subgroups, and specify that each is analysed by adding the subgroup variable and its interaction with intervention exposure to the primary model. _(Comments id=33 MGW, id=34 AO; id=368 KH; Round Two)_ **[verify]**

- [x] **F2. Clarify how subgroup variables are defined/coded, and whether cluster size is continuous or categorical.** State the coding of each subgroup (e.g. binary vs multi-category for clinical cohorts) and resolve whether cluster size enters as continuous or categorical (small/medium/large). Reduce the number of subgroups if considered too many. _(Comments id=243, id=246 KH)_ **[decision]** — keep six; cluster size &lt;12 / 12–20 / &gt;20; cohorts no residual; all states

---

## G. Statistical principles, hypotheses and reporting

- [x] **G1. Confirm the superiority framework is stated in the objectives/hypotheses.** Ensure the superiority (vs equivalence/non-inferiority) framework is explicitly specified and states which comparisons are presented on this basis. _(Comments id=1 MGW, id=2 AO; Round Two)_ **[verify]** (Design section already mentions superiority at ~line 91.)

- [x] **G2. Add a statement on the timing of analysis.** State that all outcomes will be analysed collectively once all data are in (single final analysis). Add to the statistical-principles/analysis section. _(Comment id=5 MGW; Round Two)_

- [x] **G3. Confirm the significance level / CI statement.** Ensure the two-sided 0.05 significance level and 95% CI reporting is present. _(Comments id=6, id=7 AO; Round Two)_ **[verify]** (Present at `statistical-analysis-plan.qmd` ~line 249.)

- [x] **G4. Confirm the revised statistical-hypotheses text (no null/alternative framing).** The reviewer preferred removing formal null/alternative hypotheses; the section was revised to the agreed effect-estimation wording. Confirm it reads as intended (`statistical-analysis-plan.qmd` ~line 227). _(Comments id=66 KH, id=67 AO, id=68 MGW)_ **[verify]**

---

## H. Effect measures and outcomes summary table

- [x] **H1. State effect measures for every outcome type, including hazard ratios.** Ensure the effect measure is specified for each outcome data type (OR and ARD for binary; mean difference for continuous; cumulative OR for ordinal; and (subdistribution) hazard ratio for time-to-event outcomes), and whether averaging is by cluster or individual. Add HR where relevant. _(Comments id=14, id=15, id=17 MGW/AO; Round Two)_ (Coordinate with A2.) — no HR (rate ratios for LOS); effect-measure sentence + cluster-specific estimates

- [x] **H2. Confirm/enrich the outcomes summary shell table.** Confirm `@tbl-outcomes-summary` (built from `tables/outcomes-summary.json`) captures outcome, design component, data type, analysis set, main analysis model, effect measure, potential intercurrent events, and strategy for handling them, and reconcile it against `comments/ALTS _ Summary of outcomes table.docx` / `comments/outcomes-summary.md`. Confirm a shell table showing how outcomes are summarised/compared by arm (or sequence/period) is present. Anna asked for a review of her first draft of this table (columns: outcome, data type, population, effect measure, potential intercurrent events, strategy). _(Comments id=18, id=19 MGW; Round Two; Anna email)_ **[verify]**

- [x] **H3. Confirm use of the stepped-wedge/cluster CONSORT flow diagram.** Ensure the SAP states the cluster/stepped-wedge CONSORT extension is used for the flow diagram (`@fig-consort-diagram`, `statistical-analysis-plan.qmd` ~line 261). _(Comment id=9 MGW; Round Two)_ **[verify]**

- [x] **H4. Confirm the target population is specified per analysis.** Ensure the population of clusters/individuals for whom the treatment effect is estimated is specified for each analysis, including survivor-only outcomes and nested-design outcomes (`statistical-analysis-plan.qmd` ~line 257). _(Comments id=12 MGW, id=13 AO; Round Two)_ **[verify]**

---

## I. Already resolved in shared includes — verify only

- [x] **I1. Nested-outcomes sampling described as stratified sampling by shift.** Already updated in `../shared-assets/sample-size-calculations-nested.qmd` (now "stratified random sampling by shift"). Confirm the stale wording ("simple random sampling on the shift level") in the unused `../shared-assets/sample-size-calculations.qmd` does not get included anywhere. _(Comments id=83, id=84, id=85, id=86)_ **[verify]** — SAP OK; protocol stale text noted as optional P4

- [x] **I2. Secondary-outcome / power framing.** `../shared-assets/sample-size-calculations-nested.qmd` already clarifies these are secondary outcomes whose power calculations only inform data-collection sample size. Confirm wording reads cleanly (there is a minor "are secondary outcomes are …" phrasing to tidy). _(Comments id=80, id=81, id=82)_ **[verify]** — SAP-only preface; preferred rephrase noted as comment in nested include

- [x] **I3. Region defined by official state.** Confirm the subgroup/region wording notes the number of states depends on the final set of participating clusters and cannot be prespecified. _(Comments id=238, id=239)_ **[verify]**

- [x] **I4. Staircase random-selection cross-reference.** Confirm the randomisation section cross-references the nested staircase sampling description so the reader is not left wondering where random selection for the staircase is defined. _(Comments id=45 KH, id=46 AO)_ **[verify]**

---

## J. Other (shell tables and process)

- [ ] **J1. Add summary shell table of all outcomes.** Add a second summary table of outcomes, similar to the first one, but including all secondary outcomes.

- [ ] **J2. Add summary shell table of the results of the analysis of the primary and secondary outcomes.**

- [ ] **J3. Add summary shell table of the results of the sensitivity, subgroup and fully adjusted analyses.**

- [ ] **J4. Add generative AI statement.** Add a statement describing the use of generative AI in creating and revising the SAP.
