# Comment review & triage — SAP v0.7.0

**Document reviewed:** `statistical-analysis-plan-v0.7.0-2026-10-07 KH 2026-10-21_AO_JK_JM.docx` (SAP v0.7.0, dated 2026-10-07)
**Reviewers:** Karla Hemming (KH), Anna Olofsson (AO), Jessica Kasza (JK), James Martin (JM)
**Comment counts:** KH 31 · JK 20 · JM 15 · AO 23 → **89 comments total**
**Comment dates:** KH 2026-10-20 · AO 2026-10-21 · JM 2026-10-26 · JK 2026-10-26
**Prepared for the meeting on:** 2026-11-10

**Purpose.** Group all 89 comments into discussion topics, flag the decisions that need to be taken in the meeting, and separate them from comments that can be drafted offline. Section references are to the SAP itself.

> Note on timing: AO's replies (21 Oct) predate most JM/JK comments (26 Oct). Some of AO's proposals were later superseded or extended by JK/JM and will need to be reconciled in the meeting (flagged below).

---

## 1. At a glance

| | Count |
|---|---|
| 🔴 P1 — feeds into a decision to be taken in the meeting | 49 |
| 🟡 P2 — wording/clarification edits, draftable offline | 33 |
| 🟢 P3 — agreement, answered, or discard | 7 |
| **Total** | **89** |

The 49 P1 comments collapse into ~11 decision topics (Section 2). Everything else is drafting work for the statisticians / AO (Section 3).

**Status labels used below**

| Label | Meaning |
|---|---|
| ⏳ **Decide** | Open question; needs an in-meeting decision (or an agreed owner + deadline) |
| ✍️ **Draft** | No statistical decision needed; wording/spec must be drafted (usually by AO / trial statistician) |
| ✅ **Proposal** | Concrete wording/proposal already provided in the thread; needs sign-off |
| 📌 **Noted** | Information/preference recorded; no action beyond incorporating into drafting |
| ❓ **Clarify** | Ambiguous; the original poster (or a third party) must clarify before drafting |
| 🗑️ **Discard** | Author themselves retracted |

*Tracked changes in the document are trivial (3 words inserted, 2 deletions by JM/JK) — all feedback is in the comments, nothing substantive was edited in track-changes.*

---

## 2. Decisions for the meeting (~11 topics)

### 🔴 D1 — Interim analysis wording (§4.5) — comments 63, 64, 65, 66, 67
- **Issue:** KH flagged the two highlights as contradictory (65). JK (64, 67) points out there are *no formal stopping rules based on the treatment effect*, so calling this an "interim analysis" is misleading — it is really trial monitoring. JM asks (63) what will differ in the half-way report vs the others, and whether a single report will contain an actual arms comparison.
- **Position already on the table:** AO (66) proposes calling the per-batch reports **"routine monitoring reports"** and reserving "interim analysis" for the half-way review; JK agrees.
- **Decision:** adopt the monitoring-vs-interim terminology; clarify the *content* of the half-way report (feasibility summaries only, or also a comparative analysis of outcomes?) and whether any formal stopping/decision rules apply.
- **Action:** AO (or JK) drafts revised §4.5 wording after the decision.

### 🔴 D2 — Missing-data strategy (§9.4) — comments 217, 218, 219, 220, 221, 222, 223, 224, 225
- **Issue:** AO (217) challenges the fixed **10% threshold**: bias depends on the missingness mechanism, not just the proportion. She proposes making the primary analysis "all available observed outcomes" with **multiple imputation pre-specified as a sensitivity analysis under MAR**, and considering missingness per outcome.
- **Positions:** JK (218) is fine with *always* pre-specifying MI (and optionally missingness DAGs). JM (221) asks about imputation of **covariates** for the adjusted analyses; (224) notes R packages exist for clustered imputation and suggests specifying auxiliary covariates; (225) suggests a **tipping-point analysis** for MNAR. AO (223) wants Rubin's rules, extent/pattern of missingness, and diagnostics specified. KH (222) assigns the cluster-aware imputation to JM.
- **Decision:** primary estimator (available-case vs always-MI); keep or drop the 10% rule; whether a tipping-point analysis is added; imputation specification (covariates, auxiliaries, diagnostics, software).
- **Action:** JM advises on software & model; AO drafts §9.4.

### 🔴 D3 — Length-of-stay outcomes — count vs continuous vs time-to-event (§9.2.5/6/8) — comments 195, 196 (+ 148, 123, 133)
- **Issue:** AO (195) asks how LOS is recorded (integer hours/days vs continuous). Negative binomial assumes counts — rounding/zeros need specifying; she suggests a mixed-effects **gamma with log link** as an alternative and reporting a **"ratio of expected lengths of stay"** rather than a "rate ratio". JK (196) offers the alternative of **time-to-event models with death as a competing risk**.
- **Related:** AO (148) wants the clinical interpretation of the truncated-by-death/transfer estimand clarified (short stays can reflect death/transfer, not good care) and the estimand confirmed as "observed duration until exit"; KH (123) asks whether transit as part of a transfer counts (this was partly answered — see comment 125), and (133) asks about discharge-then-readmission.
- **Data (confirmed 2026-11-09):** LOS is recorded as **date & time for all three outcomes** (ED, hospital, ICU) — i.e. continuous durations, not integer counts. This resolves AO's question in 195: negative binomial would require artificial discretisation; the realistic options are the **gamma/log-link** model (ratio of expected LOS) or JK's **time-to-event with competing risk**.
- **Decision:** gamma/log-link vs survival-with-competing-risk for the three LOS outcomes; effect-measure label (ratio of expected LOS vs rate ratio); estimand wording; readmission handling.
- **Action:** AO drafts after decision; clinical confirmations still needed on transit/readmission definitions.

### 🔴 D4 — QoL & disability: separate vs joint longitudinal analysis (§9.2.2/9.2.3) — comments 185, 186
- **Issue:** AO (185) asks whether QoL/disability are analysed separately at each time point (7 days, 30 days, 3 months) or jointly with a longitudinal model (within-patient correlation, time×treatment interaction).
- **Positions:** JK (186) prefers the **joint longitudinal model** (time, time×treatment, patient-level random effects). The current SAP text implies separate analyses.
- **Decision:** adopt longitudinal model (likely) or keep per-time-point; if joint, specify correlation structure and estimand per time point.
- **Action:** AO drafts §9.2.2/9.2.3 after decision.

### 🔴 D5 — Primary model: adjust for randomisation factors / cluster size (§9.1, §9.4) — comments 219, 220 (and 163/164/165)
- **Issue:** KH (219) reminds that the primary analysis *should usually adjust for randomisation factors* and asks whether it does (the randomisation is covariate-constrained on anticipated cluster size within batch). AO (220) confirms the model currently adjusts for time + cluster random effects but **not** the cluster-size variable; she notes model complexity. Related: JK's later comments on small-sample inference (D6) interact with model complexity here.
- **Also here:** KH (163) asked "random cluster by period?"; AO (164) confirmed it is in Equation 1; JM (165) asks that this be made explicit in the text (→ drafts).
- **Decision:** include the cluster-size randomisation variable (and/or batch) as a fixed covariate in the primary model, or argue explicitly why not.
- **Action:** decision in meeting; AO drafts text per 165.

### 🔴 D6 — Small-sample inference & model-fit criteria (§9 intro, §9.1) — comments 160, 161, 166, 206, 207
- **Issue:** AO (166) asks to **pre-specify the small-sample correction for non-linear mixed models** (or objective criteria for choosing), since the choice affects SEs/CIs/p-values and must not be data-driven. KH (160) wants to discuss the unit-level correlation question; JK (161) reports using a **t-distribution with K−2 d.f.** for few clusters (JM agrees, 162). On model fit, AO (206) wants criteria beyond convergence (singular fits, boundary variance components); JK (207) is relaxed about zero variance components and suggests simpler random-effects structures as sensitivity.
- **Decision:** wording that pre-specifies the inference strategy for GLMMs and defines acceptable fit; whether to adopt the t(K−2)/KR guidance implicitly.
- **Action:** JK advises on current methods; AO drafts.

### 🔴 D7 — Intercurrent-events table restructure (§8.1) — comments 130, 131, 132, 134, 135, 136, 137, 142, 145, 146, 147, 148
- **Issue:** Consensus emerging that the table mixes **true intercurrent events** with **missing data / loss to follow-up** and **outcome definitions**:
  - LFU is a missing-data issue, not an ICE (KH 131; AO 132) → move to §9.4, citing the missing-data section.
  - The "missing data" row (KH 134) is about non-ascertainable outcomes → same treatment (AO 135).
  - The ICU-admission row (KH 136) is an *outcome definition* (admitted only if ICU admission precedes death/transfer; §9.2.7), not an ICE strategy (AO 137) → simplify.
  - KH (145) and AO (146) want the section simplified — distinguish cluster-level deviations (delayed training, non-adherence) from patient-level ICEs; exposure is defined by the *scheduled* transition date so delivery deviations don't change treatment classification. KH (147) also notes "not technically a strategy" but is happy to keep the current wording.
  - JK (130, 142) wants consistent use of the already-defined **treatment-policy strategy** wording.
- **Decision:** scope & structure of Table 1 / §8.1 (which rows stay, which move to §9.4 or the outcome definitions); keep "treatment policy" wording; confirm the LOS estimand wording (148 → see D3).
- **Action:** AO drafts restructure; clinical confirmation needed on death/transfer/readmission definitions.

### 🔴 D8 — Sensitivity-analysis suite pruning (§9.5) — comments 230, 233, 234, 235, 236, 240, 241, 245, 246, 250, 251, 252
- **Issue:** KH (234) warns the SA list is too long (criticism received in a recent paper) and flags: **9.5.2 AR within-cluster → remove (233)**; **9.5.4 spline → remove (245)**; **9.5.3 random cluster×intervention → "perhaps keep, not sure" (240)**; **9.5.1 actual transition date → keep (230)**; **9.5.5 lag & weaning → keep (250)**. JK agrees with removing the spline (246) and with keeping lag/weaning (251); JM (252) calls lag/weaning the most interesting candidate. JM questions the justification for random cluster×intervention (241). AO (235) proposes separating "robustness of the primary estimand" from "exploratory" analyses to keep the SAP focused; JK (236) likes pre-specified SAs but notes interpretation issues.
- **Decision:** final SA list (recommend: keep 9.5.1, 9.5.3 with justification or drop, 9.5.5; drop 9.5.2, 9.5.4) and whether to split into "sensitivity" vs "exploratory".
- **Action:** AO updates §9.5 after decision.

### 🔴 D9 — Subgroup analyses: age groups & geographical region (§9.7) — comments 260, 261, 262, 263, 264
- **Issue:** KH (260) suggests **two age groups** instead of four; AO (261) agrees two groups would improve precision and that the cut-off should be clinically justified and fixed before looking at effects; JM (262) asks for the expected patient distribution across the current four groups (low-precision worry). For region: KH (263) — if region is the **primary subgroup**, the levels must be pre-specified; AO (264) notes the final set of states is unknown, so proposes broader pre-defined regions or classifying the state analysis as exploratory.
- **Decision:** age categorisation (2 vs 4 groups; cut-off); region approach (pre-specified broader categories vs exploratory).
- **Action:** need a quick look at expected age distribution (JM's question) before deciding; AO drafts.

### 🔴 D10 — Repeated ED presentations & patient-level correlation (models, §9) — comments 258, 259
- **Issue:** AO (258) asks whether the same patient can contribute more than one ED presentation and how within-patient correlation is handled (models only have cluster-level random effects). JK (259): "I'd generally only include the first presentation per patient"; treating repeats as independent would be conservative; asks whether many patients are likely to re-present.
- **Decision:** inclusion rule (likely "first presentation only") and whether within-patient (re)correlation needs modelling; check data for re-presentation frequency.
- **Action:** confirm with data/clinical team; AO drafts.

### 🔴 D11 — Effect measures: odds ratio vs risk ratio for binary outcomes (§8.4, §9.1) — comments 155, 156, 157, 169
- **Issue:** JK (169) notes ORs are fine, but she has been estimating **risk ratios (log-binomial with random effects)** and asks what is of interest. KH (155) asks to "get Res on these" (ordinal EQ-5D-5L cumulative OR row — clarify what "Res" refers to, likely methodological input on the effect-measures table), and (156) doesn't follow the adherence-proportion row; AO (157) explains the intent (hospital-level randomisation, no unit-level random effect) and flags the unit-correlation question (→ D6).
- **Decision:** keep ORs for binary primary/secondary outcomes or switch to RRs; confirm the ordinal (cumulative logit) and adherence (beta, logit-scale difference) rows — possibly with external/“Res” input.
- **Action:** JK advises on RR vs OR; clarify comment 155 with KH first.

---

## 3. Comments that can be drafted offline (P2 / P3)

### 🟡 P2 — wording & clarification edits (no in-meeting decision needed)

**§4.1–4.2 Trial design, definitions of units/batches (33, 34, 35, 36, 37, 38, 46)**
| ID | Who | Comment | Status |
|----|----|---------|--------|
| 33 | KH | "Can you explain what a unit is here" | ✍️ Define "unit" (ED physician staffing unit) at first use |
| 46 | JM | "I am not sure what a unit is" (same term later) | ✍️ Same fix as 33 — one definition, reuse it |
| 34 | JK | Does "equal allocation across sequences" mean equal *chance* per sequence? | ✍️ State the randomisation mechanism explicitly |
| 35 | JK | Or does it mean an equal *number* of clusters per sequence? | ✍️ Clarify both: uniform randomisation + expected balance (1 per sequence per batch) |
| 36 | JM | How are batches defined — cluster readiness or random? | ✍️ Describe batch formation (needs trial-team answer) |
| 37 | JM | "We measure clusters repeatedly" — are patients measured repeatedly? | ✍️ Clarify patient sampling across periods |
| 38 | JK | Suggest "Clusters provide measurements throughout trial periods" | ✅ Adopt clearer phrasing (proposal in comment) |

**§4.4 Sample size / power (55, 56, 57)**
| ID | Who | Comment | Status |
|----|----|---------|--------|
| 55 | KH | Add the assumed CV of cluster sizes | ✍️ Add assumed coefficient of variation |
| 56 | KH | "The app cannot simultaneously allow for varying cluster sizes and transition periods" — statement can't be right | ❓ Verify with whoever ran the power calc what the software supported; then rewrite |
| 57 | JK | Maybe the point is power is robust to smaller cluster sizes? | ✍️ Rewrite once 56 is resolved |

**§5.4 Descriptive summaries (80, 81)**
| ID | Who | Comment | Status |
|----|----|---------|--------|
| 80 | KH | "I'm sorry but I do not understand this" (key vs comprehensive table versions) | ✍️ Rewrite for clarity |
| 81 | AO | Proposed sentence: "selected key variables … main manuscript … complete table … supplementary material" | ✅ Adopt AO's wording |

**§6.1.2.1 Eligibility — ICD chapters (102)**
| ID | Who | Comment | Status |
|----|----|---------|--------|
| 102 | JM | "Should it include chapter XIX as well?" (trauma currently defined via ICD chapter XX) | ❓ Confirm with clinical team: XIX (injuries) and/or XX (external causes) — then draft |

**§7.1.2 Outcome definitions (123, 124, 126, 133)**
| ID | Who | Comment | Status |
|----|----|---------|--------|
| 123 | KH | Does ED LOS include time in transit as part of a transfer? | ✍️ Fold into LOS definition (with D3/148) |
| 124 | KH | "Comment applies to a few outcomes" (same transfer issue in several outcomes) | 📌 Noted — apply 123 fix across ED/hospital/ICU LOS |
| 126 | KH | "Do you have a plan to handle deaths here?" (ICU admission) | ✍️ Cross-reference §9.2.7 definition (admission only if ICU before death/transfer) |
| 133 | KH | What if a patient is discharged and then readmitted? | ❓ Clinical/data-team answer needed on readmission definition |

**§8.4 / §9.1 Effect measures (155, 156, 157)** — see D11; drafting follows the D11 decision.

**§9 intro / §9.1 Random-effects text (163, 164, 165)**
| ID | Who | Comment | Status |
|----|----|---------|--------|
| 163 | KH | "Random cluster by period?" | 📌 Answered by AO 164 |
| 164 | AO | Yes — random intercept + cluster-by-period effect (Equation 1), per-period variation within hospital | ✅ Add explicit statement |
| 165 | JM | "I would make this clear here then" | ✍️ Draft explicit text at §9 intro |

**§9.2 intro — Model rules for secondary outcomes (177, 178, 179, 180)**
| ID | Who | Comment | Status |
|----|----|---------|--------|
| 177 | KH | "Don't follow?" | ✍️ Clarify |
| 178 | AO | Intended meaning: start from primary model structure; simpler models only if non-convergence; "We will not repeat the model sequence" is misleading | ✍️ Rewrite |
| 179 | AO | Proposed replacement wording (start from primary; progressively simpler if non-convergence) | ✅ Adopt AO's wording |
| 180 | JM | AO's explanation makes sense but contradicts the text as written | ⏳ Confirm adoption of 179 (essentially agreed) |

**§9.3 Correlations / ICC (211, 212, 213, 214)**
| ID | Who | Comment | Status |
|----|----|---------|--------|
| 211 | KH | "Estimated via a linear mixed model?" | ✍️ Specify derivation |
| 212 | AO | Correlations derived from variance components of outcome-specific models; latent scale for binary/ordinal; NB & beta need clarification | ✍️ Draft + note methodological detail |
| 213 | JK | Good ICC estimates for binary outcomes can come from linear mixed models | 📌 Consider citing/using |
| 214 | JK | "I couldn't see this being considered anywhere, but I may have missed it" | ❓ Ask JK what element she expected (ambiguous anchor) |

**§9.5 SA suite (236)** — JK likes pre-specified SAs but notes interpretation issues → 📌 noted (see D8).

**§9.7 Subgroups (262)** — see D9.

**§9.10 Presentation of results (271)**
| ID | Who | Comment | Status |
|----|----|---------|--------|
| 271 | JM | What are "key outcome analyses" (Table 8 vs Table 9 split) | ✍️ Define the term explicitly |

### 🟢 P3 — agreements / answered / discard (no action beyond noting)

| ID | Who | Comment | Status |
|----|----|---------|--------|
| 125 | KH | Retracted ("all clear on the next page — ignore me") | 🗑️ Discard |
| 147 | KH | "Not technically a strategy, but makes sense — keep as is" | ✅ Keep |
| 162 | JM | Agrees with JK's t(K−2) approach | ✅ Agreement |
| 222 | KH | Assigns cluster-aware imputation to JM | ✅ Owner: JM (fold into D2) |
| 246 | JK | Agrees to remove spline SA | ✅ Agreement (D8) |
| 251 | JK | Agrees to keep lag/weaning SA | ✅ Agreement (D8) |
| 252 | JM | Calls lag/weaning the most important / potentially interesting SA | ✅ Supports keeping (D8) |

*Comments 66/67, 132, 135, 137, 146, 164, 178–180, 207, 218 are also effectively agreed proposals — but they belong to decision topics D1/D2/D4/D5/D6/D7 and should be confirmed as part of those decisions.*

---

## 4. Suggested meeting flow

**Prep before the meeting (assign now):**
- KH — clarify comment 155 ("get Res on these") and comment 214 expectation.
- Trial/data teams — readmission handling (133), ICD chapter scope (102), LOS recording (integer vs continuous, 195), re-presentation frequency (259), expected age distribution (262), what the sample-size software actually supports (56).
- JK — one-pager on RR vs OR and on small-sample corrections for GLMMs (169, 161, 166).

**Agenda (~2 h):**
1. Framing — interim analysis vs monitoring (D1) · 10 min
2. Estimands & outcomes — LOS modelling (D3), QoL longitudinal (D4), ICD/readmission clarifications · 20 min
3. Primary model — randomisation factors (D5), small-sample inference & fit criteria (D6) · 20 min
4. Missing data — strategy & imputation (D2) · 15 min
5. Intercurrent-events table restructure (D7) · 15 min
6. Sensitivity-analysis suite (D8) · 10 min
7. Subgroups (D9) & repeated presentations (D10) · 15 min
8. Effect measures — RR vs OR (D11) · 10 min
9. Remaining wording batch (P2) — confirm owners, no discussion · 5 min
10. Actions, owners, next-version timeline · 5 min

---

## 5. All comments by theme (reference index)

*Sections refer to the SAP; "status" is the pending action, not a judgment on the comment.*

| Topic (SAP §) | Comment IDs | Total |
|---|---|---|
| Trial design & definitions (§4.1–4.2) | 33, 34, 35, 36, 37, 38, 46 | 7 |
| Sample size & power (§4.4.1) | 55, 56, 57 | 3 |
| Interim analysis & monitoring (§4.5) | 63, 64, 65, 66, 67 | 5 |
| Descriptive summaries (§5.4) | 80, 81 | 2 |
| Eligibility — ICD chapters (§6.1.2.1) | 102 | 1 |
| Outcome definitions (§7.1.2) | 123, 124, 125, 126, 133 | 5 |
| Intercurrent events & estimands (§8.1) | 130, 131, 132, 134, 135, 136, 137, 142, 145, 146, 147, 148 | 12 |
| Effect measures (§8.4, §9.1) | 155, 156, 157, 169 | 4 |
| Random effects & small-sample inference (§9 intro/§9.1) | 160, 161, 162, 163, 164, 165, 166 | 7 |
| Secondary-outcome model rules (§9.2 intro) | 177, 178, 179, 180 | 4 |
| QoL & disability (§9.2.2/9.2.3) | 185, 186 | 2 |
| Length-of-stay modelling (§9.2.5/6/8) | 195, 196 | 2 |
| Model fit & convergence (§9) | 206, 207 | 2 |
| Correlations & ICC (§9.3) | 211, 212, 213, 214 | 4 |
| Missing data (§9.4) | 217, 218, 219, 220, 221, 222, 223, 224, 225 | 9 |
| Sensitivity analyses (§9.5) | 230, 233, 234, 235, 236, 240, 241, 245, 246, 250, 251, 252 | 12 |
| Repeated ED presentations | 258, 259 | 2 |
| Subgroup analyses (§9.7) | 260, 261, 262, 263, 264 | 5 |
| Presentation of results (§9.10) | 271 | 1 |
| **Total** | | **89** |

---

## 6. Open clarifications (need input before drafting)

| Comment | Who to ask | Question |
|---|---|---|
| 155 | KH | What does "get Res on these" mean (a person/group?), and what input is wanted on the ordinal effect-measures rows? |
| 214 | JK | What exactly did she expect to be considered in §9.3 that she "couldn't see"? |
| 56 | Power-calc author | What did the software actually support (varying cluster sizes vs transition periods)? |
| 102 | Clinical/data team | ICD chapter XIX and/or XX for trauma eligibility? |
| 133 | Clinical/data team | Discharge-then-readmission: counted as one stay or two? |
| 259 | Data team | Expected proportion of patients with >1 ED presentation? |
| 262 | Data team | Expected distribution across the 4 age groups (or availability of the sample-size assumptions)? |

*✔ Answered (2026-11-09): comment 195 — LOS is recorded as date & time for all three outcomes (continuous durations); see D3.*
