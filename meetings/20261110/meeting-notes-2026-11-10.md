# Meeting notes: SAP v0.7.0 finalisation meeting (2026-11-10)

Purpose: Finalise the ADVANCE TRAUMA statistical analysis plan (SAP).
Attendees: Martin Gerdin Wärnberg (chair), Karla Hemming, Jessica Kasza, James Martin, Arpita Ghosh. Anna Olofsson absent.

## Trial status

The trial started in February 2025. Batch 1 is complete and batch 2 is ongoing, with batch 3 planned for next week after a delay of about six months following a late cluster withdrawal. About 2,500 patients have been included, slightly above expectations.

## Decisions

1. Interim analysis (§4.5). Keep the term to avoid a protocol amendment, and distinguish routine per-batch monitoring from the half-way feasibility-based continue/stop review. There will be no treatment-arm comparisons or outcome-based stopping rules. Refer to the SDMC charter for guidance, as feasibility thresholds were not yet set.
2. Missing data (§9.4). Present complete-case analysis as the default and use MI if primary-outcome missingness exceeds 5%, with the same threshold for covariates in the adjusted analysis. Use one imputation model and the same datasets across analyses, with at least 20 imputations based on the larger missingness requirement. Describe MI under missing-data handling.
3. Length of stay (§9.2.5/6/8). Analyse ED, hospital and ICU LOS as time-to-event outcomes with death as a competing risk. Clarify transfer handling for each outcome, as mortality follow-up continues across hospitals but transfer to another hospital interrupts ICU observation and is an intercurrent event.
4. QoL and disability (§9.2.2/3). For each outcome, combine repeated measurements in one longitudinal model. Report treatment effects at each time point rather than differences between time points.
5. Primary model (§9.1). Add cluster size as a fixed covariate, using the anticipated size recorded for randomisation rather than subsequent observed size.
6. Small-sample inference (§9 intro/9.1). Defer the choice of correction until after the forthcoming stepped-wedge review and before analysis. Analysis is expected no earlier than late 2028, possibly in 2029.
7. Intercurrent events (§8.1). Handle loss to follow-up and withdrawal in the missing-data section. Retain treatment-policy handling of cluster non-adherence, with exposure defined by scheduled transition dates.
8. Sensitivity analyses (§9.5). Keep the full set without emphasising them in the report. The analysis using exposure defined by actual transition date was added in the SAP, while the others were specified in the protocol.
9. Subgroups (§9.7). Use two conventional younger/older age groups if consistent with protocol pre-specification. Use zonal councils instead of individual states for geographical region.
10. Repeated ED presentations (§9 models). State that a small number of patients may present more than once. Treat presentations as independent rather than restricting to first admission.
11. Binary effect measures (§8.4/9.1). Keep odds ratio as the primary effect measure alongside absolute risk difference (ARD), reporting OR alone if the ARD model fails to converge. Do not adopt marginal standardisation to report RR alongside OR and RD.
12. Adherence (§9.2.x). Model completed ATLS checklist steps as a count, rather than beta regression or dichotomisation.
