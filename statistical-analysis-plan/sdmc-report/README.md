# SDMC report — prepared data pipeline

> **For the data team:** the folder to email them is [`for-data-team/`](for-data-team/)
> (instructions + blank templates, also zipped as `for-data-team.zip`).

This folder holds the data pipeline for the **report to the Trial Steering and
Data Monitoring Committee (SDMC)** (rendered from
`../sdmc-report-template.qmd`). The report contains **no simulated data** and
**pulls nothing from REDCap**: every table is an empty shell whose cells are
completed by the data team in India from the summary files in [`prepared/`](prepared/),
and every figure shows blank counts that are completed with observed data.

The summary CSVs are the **contract** between the data team (who complete the
tables manually) and the report (which renders them). The row labels and
columns in each CSV are already in place; the team only fills in the values.

---

## How it works

1. **Complete (data team).** Open the CSVs in `prepared/` in Excel, fill in the
   values, and save (keep the CSV format, keep the columns in the same order).
   All row labels and column headers are already in place — only the value
   cells need to be filled.
2. **Render.** Re-render `../sdmc-report-template.qmd`; the report tables pick
   up the filled CSVs automatically.

Nothing else changes — the report always renders, whether the CSVs are blank
(empty shells), partially filled, or complete.

---

## Files

| File | Purpose |
|------|---------|
| `prepared/*.csv` | One summary CSV per report table. The rows and columns are the contract; fill the value cells. |
| `generate-prepared-templates.R` | Regenerates the blank CSVs (run it if the table layouts change — e.g., after SAP updates). |
| `figures/` | Figures generated when the report renders (CONSORT flowcharts, roll-out plot). |
| `../functions/sdmc_report_prepared_data.R` | Shared helpers: template writer, CSV reader, and value injector used by the report and scripts. |

---

## Filling in the CSVs

Each CSV has a `section` and `row_label` column; the remaining columns are the
table's strata. In Excel: open the CSV (UTF-8), type the numbers, save as CSV.

- **Counts columns** expect whole numbers, e.g. `12`.
- **Summary-statistic cells** expect text, e.g. `38 (24–52)` for a median
  (Q1–Q3) or `36 (30.0%)` for n (%).
- **Column order must not be changed** — the report injects values by column
  position. You may leave cells empty; empty cells stay blank in the report.
- The **Overall** column is always the last stratum column.
- Cells that are not applicable (e.g. a before/after split that does not apply
  to a cluster-level count) should be left blank, not filled with 0.

### Table-by-table guidance

| CSV | What goes in each row |
|-----|----------------------|
| `trial-progress.csv` | For the batch covered by the report: clusters included and dropping out; patients screened; patients included; patients who did not consent to out-of-hospital follow-up; patients lost to follow-up. Columns: before ATLS, after ATLS, overall. "Before/after ATLS" is defined by the patient's arrival date relative to the cluster's transition to ATLS®. |
| `trial-progress-by-batch.csv` | The same counts, one column per batch (only the covered batch will have data in the first report). |
| `randomisation-adherence.csv` | Number of clusters that transitioned on the scheduled date, within four weeks of it, or more than four weeks from it (plus reasons in the report text). |
| `training.csv` | Number of physicians trained in ATLS®; number of hospitals with their scheduled training completed by the data cut-off. |
| `protocol-deviations.csv` | Count of protocol deviations and of serious breaches (list details in the report text). |
| `safety-events.csv` | Number of patients with at least one event of each type: prolonged mechanical ventilation (>7 days); initiation of renal replacement therapy; prolonged (>2 days) or renewed use of vasopressors; other reported safety events; events assessed as probably related to the trial or intervention. |
| `outcome-completeness.csv` | For each outcome: the number of patients with the outcome observed and the number with it missing, before/after ATLS and overall. |
| `outcomes-summary.csv` | Pooled summary statistics of each outcome (all participants column only). The SDMC report must not show outcome values by intervention condition, so only the pooled column is completed. |
| `cluster-characteristics.csv` | Cluster characteristics (hospital beds, ICU beds, specialities, facilities, resuscitation provider, state, monthly trauma volume) by sequence and overall, for the included clusters. |
| `patient-characteristics.csv` | Key patient characteristics (age, sex, mechanism, Injury Severity Score, Glasgow Coma Scale, vital signs, surgery, transfusion, imaging) before/after ATLS and overall, for the included participants. |

Summary conventions follow the SAP: discrete variables as frequencies and
percentages; continuous and count variables as medians and interquartile
ranges (Q1–Q3); missing values reported per variable.

---

## For reviewers (how the report consumes the CSVs)

- The report builds each table with the same functions as the SAP shells
  (e.g. `create_interim_trial_progress_table()`), then overwrites the blank
  cells with the CSV values **positionally**: CSV row *i* fills the *i*-th
  fillable row (labels, levels, and missing rows) of the shell, and CSV column
  *j* fills the *j*-th stat column (`Overall` is always last).
- A CSV that is absent or has no values in any stratum column renders as the
  blank shell, so the report never fails on missing data.
- Figure counts (CONSORT flowcharts) can be passed through the `counts`
  argument of `create_cluster_consort_diagram()`,
  `create_patient_consort_diagram()`, and
  `create_nested_staircase_consort_diagram()`, with the same named structure
  as the `simulate_*_consort_counts()` functions that generate placeholders.