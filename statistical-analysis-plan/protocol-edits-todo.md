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

- [ ] **P3 (from C7).** Harmonise EQ-5D naming to **EQ-5D-5L** (and correct 3L where present). Shared includes still use `EQ5D5L` (`../shared-assets/outcomes.qmd`, `../shared-assets/sample-size-calculations-nested.qmd`); `protocol/variables.csv` still says EQ-5D-3L while the CRF uses EQ-5D-5L. SAP analysis text and `@tbl-outcomes-summary` already use EQ-5D-5L. *Minimal edits: replace instrument name strings only; do not rewrite outcome definitions.*

---

## Avoided by adjusting the SAP (no protocol edit)

- **P2 (from A3) — avoided.** Protocol’s “random cluster by intervention effects (with a non-zero covariance term)” is met in the SAP by a random intervention slope correlated with the cluster intercept $\alpha_{bk}$ only; $\gamma_{bkt}$ remains independent. The over-complex three-way correlated section was not restored.

---

## Not required (SAP elaborates; protocol already compatible)

- **A2 (length of stay).** Protocol already says continuous/count/prevalence outcomes use model-based approaches with appropriate links and distributions (`protocol.qmd` ~line 819). Negative binomial LOS in the SAP is an elaboration, not a conflict. No protocol edit.

---

## Parking lot (only if a later SAP decision forces it)

- [ ] **P4 (from I1) — optional sync.** Protocol still includes `../shared-assets/sample-size-calculations.qmd`, which says “simple random sampling on the shift level”; the SAP nested include correctly says stratified random sampling by shift. *Minimal edit when amending the protocol: align that phrase (or switch the protocol include to the nested/main split used by the SAP).*

_Add further minimal protocol edits here only when an approved SAP change creates a new material conflict that cannot be resolved by adjusting the SAP._
