#' Create a shell table of primary and secondary analysis results
#'
#' Builds a blank `gtsummary` results shell in the style of
#' `gtsummary::tbl_regression`, showing how intervention-effect estimates for
#' the primary and secondary outcomes will be reported. One stacked regression
#' row is created for each outcome–effect-measure combination listed in
#' `tables/outcomes-summary.json`. Estimate, confidence-interval, and p-value
#' cells are blanked for the analysis plan.
#'
#' @param data A data frame or NULL. Outcomes summary with columns
#'     `outcome` and `effect_measure`. If NULL, reads
#'     `tables/outcomes-summary.json` relative to the working directory.
#' @param path Character. Path to the outcomes-summary JSON used when `data`
#'     is NULL. Defaults to `"tables/outcomes-summary.json"`.
#' @return A `gtsummary` table (or LaTeX `kableExtra` longtable under
#'     `knitr::is_latex_output()`).
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_outcomes_analysis_results_table()
#' }
create_outcomes_analysis_results_table <- function(
    data = NULL,
    path = "tables/outcomes-summary.json") {
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.character(path) && length(path) == 1)

    if (is.null(data)) {
        data <- jsonlite::fromJSON(path)
    }

    required.columns <- c("outcome", "effect_measure")
    missing.columns <- setdiff(required.columns, names(data))
    assertthat::assert_that(
        length(missing.columns) == 0,
        msg = paste("Missing columns:", paste(missing.columns, collapse = ", "))
    )

    measure.labels <- c(
        "OR" = "Odds ratio",
        "ARD" = "Absolute risk difference",
        "COR" = "Cumulative odds ratio",
        "Rate ratio" = "Rate ratio",
        "mean difference" = "Mean difference",
        "Logit-scale difference" = "Logit-scale difference"
    )

    expand.specs <- function(outcome, effect.measure) {
        measures <- trimws(unlist(strsplit(effect.measure, ";", fixed = TRUE)))
        measures <- measures[nzchar(measures)]
        lapply(measures, function(measure) {
            label <- if (measure %in% names(measure.labels)) {
                unname(measure.labels[[measure]])
            } else {
                measure
            }
            list(
                outcome = outcome,
                measure = measure,
                label = paste0(outcome, " (", label, ")")
            )
        })
    }

    specs <- unlist(
        Map(expand.specs, data$outcome, data$effect_measure),
        recursive = FALSE
    )

    tables <- lapply(seq_along(specs), function(i) {
        create_outcomes_analysis_results_shell_row(
            row.label = specs[[i]]$label,
            measure = specs[[i]]$measure,
            seed = i
        ) |>
            gtsummary::modify_header(
                label ~ "**Outcome (effect measure)**",
                estimate ~ "**Estimate**",
                conf.low ~ "**95% CI**",
                p.value ~ "**p-value**"
            )
    })

    results.table <- gtsummary::tbl_stack(tables, quiet = TRUE) |>
        gtsummary::modify_footnote(everything() ~ NA_character_)

    if (isTRUE(knitr::is_latex_output())) {
        results.table <- results.table |>
            gtsummary::as_kable_extra(format = "latex", booktabs = TRUE, linesep = "") |>
            kableExtra::kable_styling(
                latex_options = c("repeat_header"),
                font_size = 8
            )
        ## Convert tabular to longtable for multi-page SAP output
        results.latex <- as.character(results.table)
        results.latex <- sub(
            "\\\\begin\\{tabular\\}",
            "\\\\begin{longtable}",
            results.latex
        )
        results.latex <- sub(
            "\\\\end\\{tabular\\}",
            "\\\\end{longtable}",
            results.latex
        )
        return(knitr::asis_output(results.latex))
    }

    results.table
}

#' Build one blanked `tbl_regression` row for the analysis-results shell
#'
#' Fits a tiny placeholder model only to obtain a `gtsummary::tbl_regression`
#' layout, then keeps the intervention row and blanks numeric result cells.
#'
#' @param row.label Character. Label shown in the characteristic column.
#' @param measure Character. Effect-measure code from the outcomes summary
#'     (e.g. `"OR"`, `"ARD"`, `"Rate ratio"`).
#' @param seed Integer. RNG seed for the placeholder data.
#' @return A one-row `gtsummary` table.
create_outcomes_analysis_results_shell_row <- function(row.label,
                                                       measure,
                                                       seed = 1L) {
    assertthat::assert_that(is.character(row.label) && length(row.label) == 1)
    assertthat::assert_that(is.character(measure) && length(measure) == 1)
    assertthat::assert_that(is.numeric(seed) && length(seed) == 1)

    set.seed(as.integer(seed))
    n <- 40L
    intervention <- stats::rbinom(n, 1L, 0.5)

    ## Placeholder models exist only to drive tbl_regression formatting
    if (identical(measure, "ARD") || identical(measure, "mean difference")) {
        outcome <- stats::rnorm(n, mean = 0.2 * intervention)
        model <- stats::lm(outcome ~ intervention)
        exponentiate <- FALSE
    } else if (identical(measure, "Rate ratio")) {
        outcome <- stats::rpois(n, lambda = exp(0.1 + 0.2 * intervention))
        model <- stats::glm(outcome ~ intervention, family = stats::poisson())
        exponentiate <- TRUE
    } else if (identical(measure, "Logit-scale difference")) {
        outcome <- stats::rbinom(n, 1L, 0.25)
        model <- stats::glm(outcome ~ intervention, family = stats::binomial())
        exponentiate <- FALSE
    } else {
        ## OR, COR, and any unrecognised ratio-style measure
        outcome <- stats::rbinom(n, 1L, 0.25)
        model <- stats::glm(outcome ~ intervention, family = stats::binomial())
        exponentiate <- TRUE
    }

    gtsummary::tbl_regression(
        model,
        exponentiate = exponentiate,
        label = list(intervention ~ row.label)
    ) |>
        gtsummary::modify_table_body(function(table.body) {
            keep <- table.body$variable == "intervention" &
                table.body$row_type == "label"
            table.body <- table.body[keep, , drop = FALSE]
            numeric.columns <- intersect(
                c(
                    "estimate", "std.error", "statistic",
                    "conf.low", "conf.high", "ci", "p.value"
                ),
                names(table.body)
            )
            for (column.name in numeric.columns) {
                if (is.numeric(table.body[[column.name]])) {
                    table.body[[column.name]] <- NA_real_
                } else {
                    table.body[[column.name]] <- NA_character_
                }
            }
            table.body
        })
}

#' Write a minimal Word preview of the analysis-results shell table
#'
#' @param output.file Character. Path for the Word document to create.
#' @param title Character. Title shown in the Word document.
#' @param cleanup.qmd Logical. If TRUE, delete the temporary `.qmd` after
#'     rendering.
#' @return Invisibly, the path to `output.file`.
#'
#' @examples
#' \dontrun{
#' create_outcomes_analysis_results_table_word_preview()
#' }
create_outcomes_analysis_results_table_word_preview <- function(
    output.file = "_test-outcomes-analysis-results-word.docx",
    title = "Analysis results — Word preview",
    cleanup.qmd = FALSE) {
    render_shell_table_word_preview(
        table.call = "create_outcomes_analysis_results_table()",
        output.file = output.file,
        title = title,
        table.label = "tbl-outcomes-analysis-results",
        table.caption = "Shell table of primary and secondary outcome analysis results",
        description = paste(
            "gtsummary::tbl_regression-style blank shell for intervention-effect",
            "estimates (estimate, 95% CI, and p-value) for primary and secondary",
            "outcomes."
        ),
        cleanup.qmd = cleanup.qmd
    )
}
