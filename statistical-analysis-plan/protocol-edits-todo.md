# Protocol edits deferred from SAP revision

Minimal protocol changes needed to stay consistent with the revised Statistical Analysis Plan. **Do not edit the protocol while working through `sap-revision-todo.md`.** Collect items here and apply them in one small amendment pass after the SAP revision is finished.

## Principles

- **Prefer changing the SAP to stay within the protocol** over listing a protocol edit. The protocol should change only when unavoidable.
- Keep protocol edits to a **minimum**. Prefer SAP elaboration that stays within the protocol’s existing wording.
- Only list a protocol edit when the approved SAP text would otherwise **materially conflict** with `../protocol/protocol.qmd` and no protocol-compatible SAP wording is acceptable.
- Prefer the smallest possible wording change (one sentence / one phrase) over rewriting sections.
- Each item cites the SAP to-do ID that drove it and the protocol location.

---

## Required (unavoidable material conflict with approved SAP)

- [ ] **P1 (from A1).** Replace RSPL with the SAP estimation framework. Protocol § Statistics → Analysis models (`protocol.qmd` ~line 815) still says models are fitted using “residual pseudo-likelihood estimation based on linearization with subject-specific expansion (RSPL)”. The SAP uses maximum likelihood with Laplace approximation because RSPL is SAS-specific with no exact R equivalent — a protocol-compatible SAP wording is not available without retaining an inaccurate method name. *Minimal edit: replace the RSPL sentence only.*

- [ ] **P3 (from C7).** Harmonise EQ-5D naming to **EQ-5D-5L** (and correct 3L where present). `../shared-assets/outcomes.qmd` still uses `EQ5D5L`; `protocol/variables.csv` still says EQ-5D-3L while the CRF uses EQ-5D-5L. SAP analysis text, `@tbl-outcomes-summary`, and the SAP-only nested sample-size include already use EQ-5D-5L. *Minimal edits: replace instrument name strings only; do not rewrite outcome definitions.*

---

## Avoided by adjusting the SAP (no protocol edit)

- **P2 (from A3) — avoided.** Protocol’s “random cluster by intervention effects (with a non-zero covariance term)” is met in the SAP by a random intervention slope correlated with the cluster intercept $\alpha_{bk}$ only; $\gamma_{bkt}$ remains independent. The over-complex three-way correlated section was not restored.

---

## Not required (SAP elaborates; protocol already compatible)

- **A2 (length of stay).** Protocol already says continuous/count/prevalence outcomes use model-based approaches with appropriate links and distributions (`protocol.qmd` ~line 819). Negative binomial LOS in the SAP is an elaboration, not a conflict. No protocol edit.

---

## Outright errors in the shared includes (found during the SAP coherence review)

These are misspellings, grammar faults and one American spelling in files that render into both documents, so we cannot fix them during the SAP revision. They are not style preferences: each one is wrong as written. Strings are exact.

| # | File (`../shared-assets/`) | Current | Corrected |
|---|---|---|---|
| [ ] P5 | `intervention-and-control-treatment.qmd`:1 | `the training slot alloted to them` | `the training slot allotted to them` |
| [ ] P6 | `intervention-and-control-treatment.qmd`:3 | `We will train the number units of physicians needed` | `We will train the number of units of physicians needed` |
| [ ] P7 | `intervention-and-control-treatment.qmd`:3 | `which on average should be mean that we can train` | `which should mean that we can train` |
| [ ] P8 | `intervention-and-control-treatment.qmd`:5 | `The course includes intial treatment and resuscitation` | `The course includes initial treatment and resuscitation` |
| [ ] P9 | `intervention-and-control-treatment.qmd`:5 | `Leaning is based on practical scenario-driven skill stations, lectures and includes a final performance proficiency evaluation.` | `Learning is based on practical scenario-driven skill stations and lectures, and the course includes a final performance proficiency evaluation.` |
| [ ] P10 | `eligibility-criteria-patient-participant.qmd`:3 and :13 | `**Patients participants**` (twice) | `**Patient participants**` |
| [ ] P11 | `eligibility-criteria-cluster.qmd`:11 | `- admits or refers/transfers for admission at least 12 patients` | `- admit or refer/transfer for admission at least 12 patients` |
| [ ] P12 | `eligibility-criteria-cluster.qmd`:12 | `- no more than 25% of physicians providing initial trauma care trained in…` | `- have no more than 25% of physicians providing initial trauma care trained in…` |
| [ ] P13 | `eligibility-criteria-cluster.qmd`:20 | `other major interventions[^note:major-interventions] that affects trauma care` | `…that affect trauma care` |
| [ ] P14 | `eligibility-criteria-cluster.qmd`:24 | `not limited to implementing of a trauma team approach` | `not limited to implementing a trauma team approach` |
| [ ] P15 | `synopsis.qmd`:34 | `lack a legally authorized representative` | `lack a legally authorised representative` |
| [ ] P16 | `sample-size-calculations-main.qmd`:1 | `We assume that each cluster will contribute…, but allowed for` | `We assumed that each cluster would contribute…, and allowed for` |

Notes on the two judgement calls. **P9** is two faults in one string: `Leaning` for `Learning`, and a coordination fault, since learning does not "include" an evaluation — the course does. If the team wants the minimal fix, correct the spelling only and keep the coordination fault on this list. **P12** is here because the bullet has no verb and so does not complete its stem; the parallel criterion on line 8 of the same file does carry one (`have at most 25% of physicians …`).

The `EQ5D5L` spelling in `outcomes.qmd`:19 is the same defect and is already tracked as P3 above.

A longer list of **style** points in the same files — passive voice, nominalisations, `control` for `standard care`, `study sites` for `clusters`, hyphenation and dash characters — came out of the same review. They are reported in the pull request rather than recorded here, because none of them is an error and none is worth an amendment on its own.

---

## Parking lot (only if a later SAP decision forces it)

- [ ] **P4 (from I1) — upgraded from optional.** Protocol still includes `../shared-assets/sample-size-calculations.qmd`, which says “simple random sampling on the shift level”; the SAP nested include correctly says stratified random sampling by shift. That file is superseded: it duplicates `sample-size-calculations-main.qmd` word for word and survives only because `protocol/protocol.qmd`:806 still includes it. *Minimal edit when amending the protocol: replace that include with the two split includes (`sample-size-calculations-main.qmd` and `sample-size-calculations-nested.qmd`) and delete the superseded file.* Doing so also removes the `powere` typo, a second `EQ5D5L`, the outdated sampling phrase, and 18 lines of duplication in one move. Do **not** edit the superseded file in place: that would put a third variant into circulation.

_Add further minimal protocol edits here only when an approved SAP change creates a new material conflict that cannot be resolved by adjusting the SAP._

GitHub tracking after SAP revision: [#7](https://github.com/martingerdin/advance-trauma-trial/issues/7).
