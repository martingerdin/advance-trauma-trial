# Protocol edits deferred from SAP revision

Minimal protocol changes needed to stay consistent with the revised Statistical Analysis Plan. **Do not edit the protocol while working through `sap-revision-todo.md`.** Collect items here and apply them in one small amendment pass after the SAP revision is finished.

## Principles

- Keep protocol edits to a **minimum**. Prefer SAP elaboration that stays within the protocol’s existing wording over changing the protocol.
- Only list a protocol edit when the approved SAP text would otherwise **materially conflict** with `../protocol/protocol.qmd`.
- Prefer the smallest possible wording change (one sentence / one phrase) over rewriting sections.
- Each item cites the SAP to-do ID that drove it and the protocol location.

---

## Required (material conflict with approved SAP)

- [ ] **P1 (from A1).** Replace RSPL with the SAP estimation framework. Protocol § Statistics → Analysis models (`protocol.qmd` ~line 815) still says models are fitted using “residual pseudo-likelihood estimation based on linearization with subject-specific expansion (RSPL)”. Align with the SAP: maximum likelihood with Laplace approximation; package/implementation may be chosen closer to analysis. *Minimal edit: replace the RSPL sentence only.*

- [ ] **P2 (from A3).** Soften or remove “with a non-zero covariance term” for random cluster-by-intervention effects. Protocol § Additional sensitivity analyses (`protocol.qmd` ~line 823) says models will include “random cluster by intervention effects (with a non-zero covariance term)”. The SAP retains an independent random cluster-by-intervention slope and dropped the correlated version. *Minimal edit: delete the parenthetical “(with a non-zero covariance term)” so the protocol matches the retained SAP sensitivity analysis.*

---

## Not required (SAP elaborates; protocol already compatible)

- **A2 (length of stay).** Protocol already says continuous/count/prevalence outcomes use model-based approaches with appropriate links and distributions (`protocol.qmd` ~line 819). Negative binomial LOS in the SAP is an elaboration, not a conflict. No protocol edit.

---

## Parking lot (only if a later SAP decision forces it)

_Add further minimal protocol edits here only when an approved SAP change creates a new material conflict._
