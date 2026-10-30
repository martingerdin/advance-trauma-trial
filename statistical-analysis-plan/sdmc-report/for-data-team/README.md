# How to complete the SDMC report summary tables

Thank you for completing the summary tables for the report to the Trial
Steering and Data Monitoring Committee (SDMC). This folder contains this
instruction sheet and the [`prepared/`](prepared/) folder with the 10 blank
tables to complete. Below are the steps to follow.

## Steps

1. Open the folder `prepared/` (next to this file). It contains 10 CSV files,
   one per table in the report (see the list below).
2. Open each CSV in Excel — double-click the file. If the text looks garbled,
   use **Data → From Text/CSV** and choose *UTF-8* encoding.
3. Fill in the **empty value cells** only. All row labels and column headers
   are already in place — do not change them.
4. When you are done, save the file as a CSV with the **same file name**
   (File → Save As → *CSV UTF-8*). Do not save as .xlsx.

## Rules

- **Do not** add or delete rows, and **do not** rename or reorder columns.
  The report reads the tables by row and column position.
- **Leave a cell blank** when it is not applicable (for example a
  before/after ATLS split for a cluster-level count). Do not type `0` for
  "not applicable" — `0` means zero.
- **Counts** are whole numbers, e.g. `12`.
- **Summary statistics** are written as text, e.g. `38 (24–52)` for a median
  with interquartile range, or `36 (30.0%)` for a count with percentage.
- **Avoid commas inside cells** (e.g. write `1234`, not `1,234`) so the CSV
  stays easy to read.
- The **Overall column is always the last column**.
- Cells you leave blank will appear blank in the report — that is fine.

## The 10 files and what each one needs

| File | What to enter |
|------|---------------|
| `trial-progress.csv` | For the covered batch: clusters included and dropping out; patients screened, included, not consenting to out-of-hospital follow-up, and lost to follow-up — before ATLS, after ATLS, and overall. |
| `trial-progress-by-batch.csv` | The same counts, one column per batch (only the covered batch will have data in the first report). |
| `randomisation-adherence.csv` | Clusters transitioning on the scheduled date, within four weeks, or more than four weeks from it. |
| `training.csv` | Physicians trained in ATLS®; hospitals with scheduled training completed by the data cut-off. |
| `protocol-deviations.csv` | Protocol deviations; serious breaches. |
| `safety-events.csv` | Patients with ≥1 event of each type: prolonged ventilation (>7 days); renal replacement therapy; prolonged (>2 days) or renewed vasopressors; other events; events probably related to the trial or intervention. |
| `outcome-completeness.csv` | For each outcome: n observed and n missing, before/after ATLS and overall. |
| `outcomes-summary.csv` | Pooled summary statistic per outcome (all participants column only). |
| `cluster-characteristics.csv` | Cluster characteristics by sequence and overall. |
| `patient-characteristics.csv` | Key patient characteristics before/after ATLS and overall. |

## Conventions (from the statistical analysis plan)

- Discrete variables: frequencies and percentages — `n (%)`.
- Continuous and count variables: medians and interquartile ranges — `median (Q1–Q3)`.
- Report missing values for each variable.
- "Before ATLS" / "After ATLS" is defined by the patient's arrival date
  relative to the cluster's transition to ATLS®.
- For `outcomes-summary.csv`, only the pooled (all participants) column is
  completed — outcome values are never shown by intervention condition in
  SDMC reports.

## After you finish

Send the completed `prepared/` folder back (the rest of this folder can stay
as it is). The report is rendered from these files — anything you enter
appears in the report, and blank cells stay blank.