#!/usr/bin/env Rscript
# =============================================================================
# Generate blank prepared-data templates for the SDMC report
#
# Writes the summary CSVs that the SDMC report reads (sdmc-report/prepared/).
# Run this whenever the table layouts change (or once, to create the files for
# the data team). It can be run from anywhere; it locates the trial repository
# from the location of this script.
#
#   Rscript sdmc-report/generate-prepared-templates.R
#
# Output: sdmc-report/prepared/*.csv, one file per report table, with all row
# labels and stratum columns in place and blank value cells.
# =============================================================================

script.dir <- dirname(sub("^--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))
if (is.na(script.dir) || !nzchar(script.dir)) {
    script.dir <- "."
}
project.root <- normalizePath(file.path(script.dir, ".."))
setwd(project.root)

noacsr::source_all_functions()

dir.create("sdmc-report/prepared", recursive = TRUE, showWarnings = FALSE)
sdmc_write_all_prepared_templates(dir = "sdmc-report/prepared")

cat("Blank prepared-data templates written to sdmc-report/prepared/:\n")
print(list.files("sdmc-report/prepared"))
