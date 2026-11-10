# Meeting notes — SAP v0.7.0 finalisation meeting (2026-11-10)

**Purpose:** Finalise the statistical analysis plan (SAP) — resolve the ~11 substantive decision topics raised in the reviewer comments.
**Attendees:** Martin Gerdin Wärnberg (chair), Karla Hemming (KH), Jessica Kasza (JK, "Jess"), James Martin (JM), Arpita Ghosh (guest, George Institute India). Anna Olofsson (AO) did not attend.
**Recording:** `2026-11-10 08.50.40 .../audio1595844097.m4a` (74 min; first 12:15 is silence, trimmed).
**Transcripts:** `transcripts/meeting-2026-11-10.transcript.txt` (label-based) · `transcripts/meeting-2026-11-10.named.txt` (speaker-named) · `transcripts/meeting-2026-11-10.diarized.json` (raw).
**Source documents:** `comment-triage-2026-11-09.md` (D1–D11 framework) · `meeting-deck-2026-11-10.qmd`.

---

## Trial status (Martin, ~04:58)

- Trial started **February 2025**. Batch 1 finished; **batch 2 ongoing**; **batch 3 planned to start next week** (delayed ~2 months because one cluster dropped out at a late stage).
- ~**2,500 patients** included so far — slightly more than anticipated.

---

## Decisions (mapped to the D1–D11 triage framework)

### D1 — Interim analysis wording (§4.5) — RESOLVED: keep "interim analysis" but clarify content
- Karla: "I don't think it matters too much what you call it just as long as it's clear whether or not you're going to be comparing the treatment arms." Jess agreed: would prefer not to call it "interim analysis" since it's not a formal outcome analysis, but "if it's clear, that's fine."
- Martin: changing the protocol wording would trigger a protocol amendment, which he wants to avoid.
- **Decision:** Keep the term "interim analysis" in the protocol/SAP (no amendment). Clarify in the SAP that per-batch SDMC reports are **routine monitoring**; the **half-way review** is where a continue/stop decision is made, based on **feasibility** (cluster inclusion, adherence to randomisation schedule, missingness) — **not** on estimated treatment effects, which would carry large uncertainty at that point. No formal stopping rules based on outcomes.
- Feasibility thresholds themselves are **not part of the SAP** — refer to the **DSMB/MTT charter** for those (James: "that's not part of the SAP, that's more the DSMB charter").

### D2 — Missing-data strategy (§9.4) — RESOLVED: 5% threshold, complete-case default, single imputation model
- Current text: impute if >10% missing in the primary outcome. Discussion: 10% was arbitrary.
- **Decision (Martin's summary, agreed):**
  - **Primary analysis default = complete-case analysis** (both Jess and Karla want complete-case always presented).
  - If **>5% missing** in the primary outcome → multiple imputation becomes the analysis. Same 5% concept applied to **missing covariate data** in the covariate-adjusted analysis.
  - **One single imputation model** (≥20 imputations, number based on the proportion of missingness) — all analyses (unadjusted + adjusted) run on the **same imputed datasets**; no separate per-outcome imputation (computationally much better with clustering; James's point).
  - MI is described under "how we handle missing data", **not** framed as a separate "sensitivity analysis" (the covariate-adjusted analysis is itself the sensitivity analysis).

### D3 — Length-of-stay outcomes (§9.2.5/6/8) — RESOLVED: time-to-event with death as competing risk
- Data are **date/time stamps** (continuous), not counts — so count/negative-binomial models are not appropriate.
- **Decision:** analyse all three LOS outcomes (ED, hospital, ICU) as **time-to-event with death as a competing risk** (Jess's suggestion; Karla agreed, "I see nodding heads").
- **Transfers:** for mortality outcomes, transfer to another hospital is followed up. For **ICU outcomes**, ICU data from other hospitals are **not** collected, so a transfer is an **intercurrent event** there (Martin) — though rare. Clarify in the SAP which outcomes follow up across hospitals (Karla: "that could have been a bit clearer").
- Note: whether "transfer" counts as an intercurrent event depends on the outcome (ED→ward is irrelevant to ED LOS; transfer to another hospital may truncate observation) — needs clear outcome-definition wording (Jess).

### D4 — QoL & disability (§9.2.2/9.2.3) — RESOLVED: joint longitudinal model
- **Decision:** analyse QoL and disability with **one joint longitudinal model** across the repeated time points (7 days, 30 days, 3 months), rather than separate analyses per time point (Jess's preference; Anna had suggested the same).
- Report **the treatment effect at each time point** (not differences between time points) — pre-specified (Karla's caveat: specify exactly which estimand will be reported, e.g. effect at 90 days vs 90 minus 0).
- Borrowing strength across time points helps if e.g. 90-day data are more missing than 30-day (Jess).

### D5 — Primary model: adjust for randomisation factors / cluster size (§9.1) — RESOLVED: include cluster size
- Karla's reminder: if a covariate was used in the (covariate-constrained) randomisation, you should adjust for it in the analysis.
- **Decision:** add **cluster size as defined at the randomisation time point** as a fixed covariate in the primary analysis model (Jess: "I just don't understand why you wouldn't just include it"; protects against informative cluster size). Martin was initially hesitant (cluster size is dynamic, "best guess" at randomisation) but agreed: "let's add that in."
- Same data as used for randomisation (as anticipated at that time point).

### D6 — Small-sample inference (§9 intro, §9.1) — RESOLVED: defer, don't pre-specify now
- SAP discussion had floated Kenward–Roger for linear models and a **t-distribution with K−2 d.f.** convention (Jess uses this for GLMMs; "I'd love to know I shouldn't be doing this so I can stop now").
- Karla: Jennifer Thompson evaluated simple d.f. corrections within GEE — none worked very well; a colleague (Derek) is doing a review of small-sample corrections **for stepped-wedge trials** (as was done for parallel designs).
- **Decision:** do **not** pre-specify a small-sample correction now; wait for the review and decide at analysis time. Analysis is expected **end of 2028 / early 2029**, so there is time. Martin: "we're not going to fix this, we're going to wait."

### D7 — Intercurrent-events table restructure (§8.1) — RESOLVED: LFU → missing data; keep treatment-policy wording
- **Loss to follow-up / withdrawal = missing data, not an intercurrent event** — remove from the ICE table, handle under the missing-data strategy (§9.4). (Karla & Jess agreed; "it doesn't have to go in here.")
- **Cluster-level non-adherence** (e.g. not delivering training on the scheduled transition date) stays, handled with the **treatment-policy strategy** wording; exposure is defined by the **scheduled** transition date, so delivery deviations don't change treatment classification.
- Keep the section simple; distinguish cluster-level deviations from patient-level intercurrent events; death handled as competing risk (see D3).

### D8 — Sensitivity-analysis suite (§9.5) — RESOLVED: keep, don't overplay
- Most sensitivity analyses were already in the protocol; only **exposure by actual/precision transition date** is genuinely new in this SAP.
- **Decision:** keep them all in the report ("it's hard to take them out"), but **don't overplay them** — present the protocol-specified set as sensitivity analyses without emphasising them.

### D9 — Subgroups: age groups & geographical region (§9.7) — RESOLVED: two age groups, county councils
- **Age:** reduce to **two age groups** using a conventional younger/older cutoff (if consistent with the protocol — Martin to check whether age groups were pre-specified in the protocol).
- **Region:** use **county councils (solo councils)** — the broader grouping — instead of individual states.

### D10 — Repeated ED presentations (§9 models) — RESOLVED: acknowledge, treat as independent
- Readmissions within the 13-month window are expected to be rare (no estimate yet; data available to check).
- **Decision:** add a clarity sentence — "a small number of patients may be admitted more than once; this is not specifically acknowledged in the analysis" — rather than restricting to first admission. (Karla: "you just need to be clear… we're not going to acknowledge it in our analysis.")

### D11 — Effect measures: OR vs RR for binary outcomes (§8.4/§9.1) — RESOLVED: keep OR (+ARD)
- Current text: report **odds ratio + absolute risk difference (ARD)**; report OR only if the ARD model does not converge.
- Karla's experience: log-binomial risk-ratio models **converge only ~30% of the time** in re-analyses (worst with clusters with zero events) — a strong argument against switching to RR.
- Karla offered the Andrew Copas / DMC suggestion (OR as primary inference + RR + RD reported alongside for interpretability, using marginal standard errors) — but **that approach relies on marginal standardisation, which isn't wanted here** → not adopted.
- **Decision:** keep **OR as the primary effect measure, with ARD** as currently specified. Jess is fine with OR.

### D12 (adjacent, adherence outcome §9.2.x) — RESOLVED: count outcome with offset, not beta regression
- Adherence = proportion of 14 ATLS checklist steps completed (per observed patient, nested-staircase substudy).
- Beta regression with random effects was seen as unfamiliar/risky ("I wouldn't know how to fit a beta regression model with random effects" — Karla).
- **Decision:** model adherence as a **count outcome (number of steps completed, with an offset for 14)** — e.g. Poisson-type model — rather than beta regression. (James's suggestion; Martin: "if we can use it as a count outcome, that works for me… let's do that then.") Avoid dichotomising. Martin to confirm details by email (see actions).

---

## Action items

| # | Action | Owner | Notes |
|---|--------|-------|-------|
| A1 | Draft revised SAP incorporating all decisions above + remaining wording comments; send revision for final sign-off | Martin (with Anna) | "I will work with this together with Anna… and send you a revision" |
| A2 | Clarify in §4.5: per-batch = routine monitoring; half-way review decision based on feasibility; refer to DSMB/MTT charter for thresholds; no arm comparisons at interim | Martin/Anna (draft) | D1 |
| A3 | Update missing-data section: complete-case default, 5% threshold, single imputation model (≥20), same imputed sets for all analyses | Martin/Anna (draft) | D2 |
| A4 | Rewrite LOS sections: time-to-event + competing risk; clarify transfer handling per outcome (ICU = intercurrent event; mortality = followed up) | Martin/Anna (draft); confirm transfer definitions | D3 |
| A5 | Add cluster size (randomisation-time value) as covariate to primary model | Martin/Anna (draft) | D5 |
| A6 | Adherence: switch to count-with-offset model; Martin to email the group the concrete model proposal | Martin | D12 |
| A7 | Martin to get back to Karla on her "get rest on these" comment (155) — clarify what she meant | Martin | Open clarification (triage §6) |
| A8 | Karla to send Martin the Andrew Copas OR+RR+RD suggestion (for reference; not adopted) | Karla | D11 context |
| A9 | Karla to send Martin the India fellowship details | Karla | Off-topic but promised at meeting start |
| A10 | Martin to check whether age groups were pre-specified in the protocol (2-group choice conditional on this) | Martin | D9 |
| A11 | Martin to check readmission frequency data (to quantify "rare") | Martin/data team | D10 |
| A12 | Keep small-sample correction flexible; revisit after Derek's stepped-wedge review, before analysis (end 2028/2029) | All (statisticians) | D6 |
| A13 | Martin to fill Anna in on the meeting | Martin | Anna absent |

---

## Notes / caveats

- **Speaker attribution** is based on diarization + self-introductions; a handful of brief interjections are marked "Unknown" in `meeting-2026-11-10.named.txt` (diarization labels restart per 12-min chunk).
- Names are occasionally garbled by the ASR in the raw transcript ("Peter"/"Al-Pita"/"Orpita"/"Avita"/"Evita" = Arpita; "Carla" = Karla; "Jess Case" = Jessica Kasza). These are corrected in `meeting-2026-11-10.named.txt` (see its header for the full fix list).
- One unresolved terminology point (from triage §6): comment 155 — what Karla meant by "get rest on these" — carried forward to A7.