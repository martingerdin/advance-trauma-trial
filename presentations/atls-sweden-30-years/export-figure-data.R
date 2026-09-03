#!/usr/bin/env Rscript
#' Export meta-analysis and trial-design figure data as JSON for the
#' ATLS Sweden 30 years web presentation.

args <- commandArgs(trailingOnly = FALSE)
file.arg <- grep("^--file=", args, value = TRUE)
script.dir <- if (length(file.arg) == 1) {
    dirname(normalizePath(sub("^--file=", "", file.arg)))
} else {
    getwd()
}

repo.root <- normalizePath(file.path(script.dir, "..", ".."))
functions.dir <- file.path(repo.root, "functions")
out.dir <- file.path(script.dir, "src", "data")

source(file.path(functions.dir, "lighten_color.R"))
source(file.path(functions.dir, "colors.R"))
source(file.path(functions.dir, "systematic_review_data.R"))
source(file.path(functions.dir, "conduct_meta_analysis.R"))
source(file.path(functions.dir, "get_trial_design_data.R"))
source(file.path(functions.dir, "create_trial_design_flowchart.R"))
source(file.path(functions.dir, "global_variables.R"))

vars <- global_variables()
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

meta.analysis <- conduct_meta_analysis(
    plot = FALSE,
    export.path = file.path(out.dir, "meta-analysis.json")
)

trial.design.args <- list(
    clusters = vars$clusters,
    sequences = vars$sequences,
    batches = vars$batches,
    min.standard.care.months = vars$min.standard.care.months,
    min.intervention.months = vars$min.intervention.months,
    batches.overlap.months = vars$batches.overlap.months,
    transition.months = vars$transition.months,
    transition.overlap.months = vars$transition.overlap.months,
    start.month = 0,
    total.months = vars$total.months,
    current.month = NULL,
    return.figure = FALSE,
    save = FALSE,
    return.data = TRUE
)

invisible(do.call(create_trial_design_flowchart, c(trial.design.args, list(
    staircase.months = 0,
    export.path = file.path(out.dir, "trial-design.json")
))))

invisible(do.call(create_trial_design_flowchart, c(trial.design.args, list(
    staircase.months = vars$staircase.months,
    export.path = file.path(out.dir, "trial-design-staircase.json")
))))

message(
    "Exported figure data:\n",
    "  ", file.path(out.dir, "meta-analysis.json"), "\n",
    "  ", file.path(out.dir, "trial-design.json"), "\n",
    "  ", file.path(out.dir, "trial-design-staircase.json"), "\n",
    "Pooled RR ", meta.analysis$pooled.rr, " (95% CI ", meta.analysis$pooled.ci, "), I2 ", meta.analysis$I2
)
