#' Create a shell table of primary and secondary analysis results
#'
#' Builds a blank `gtsummary` results shell in the style of
#' `gtsummary::tbl_regression`. One stacked regression row is created for each
#' outcome–effect-measure combination. Estimate, confidence-interval, and
#' p-value cells are blanked for the analysis plan. Hierarchy is shown with
#' columns for primacy (primary/secondary), design component, and timing,
#' rather than nested section headers that look identical in print.
#'
#' @param data A data frame or NULL. Outcomes summary with columns
#'     `outcome`, `design_component`, and `effect_measure`. If NULL, reads
#'     `tables/outcomes-summary.json` relative to the working directory.
#' @param path Character. Path to the outcomes-summary JSON used when `data`
#'     is NULL. Defaults to `"tables/outcomes-summary.json"`.
#' @param all Logical. If FALSE (the default), the table shows key
#'     outcome–effect-measure rows from the outcomes summary. If TRUE, nested
#'     quality-of-life and disability rows are expanded to EQ-5D-5L domains and
#'     VAS and WHODAS domain scores for supplementary reporting.
#' @return A `gtsummary` table (or LaTeX `kableExtra` longtable under
#'     `knitr::is_latex_output()`).
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_outcomes_analysis_results_table()
#' create_outcomes_analysis_results_table(all = TRUE)
#' }
create_outcomes_analysis_results_table <- function(
    data = NULL,
    path = "tables/outcomes-summary.json",
    all = FALSE) {
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.character(path) && length(path) == 1)
    assertthat::assert_that(is.logical(all) && length(all) == 1)

    if (is.null(data)) {
        data <- jsonlite::fromJSON(path)
    }

    required.columns <- c("outcome", "design_component", "effect_measure")
    missing.columns <- setdiff(required.columns, names(data))
    assertthat::assert_that(
        length(missing.columns) == 0,
        msg = paste("Missing columns:", paste(missing.columns, collapse = ", "))
    )

    specs <- build_outcomes_analysis_results_specs(data = data, all = all)
    spec.by.field <- stats::setNames(specs, vapply(specs, function(spec) spec$field, character(1)))

    tables <- lapply(seq_along(specs), function(i) {
        create_outcomes_analysis_results_shell_row(
            row.label = specs[[i]]$label,
            measure = specs[[i]]$measure,
            variable.name = specs[[i]]$field,
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

    results.table <- gtsummary::modify_table_body(
        results.table,
        function(table.body) {
            fields <- table.body$variable
            table.body$primacy <- vapply(fields, function(field) {
                spec <- spec.by.field[[field]]
                if (is.null(spec)) "" else spec$primacy
            }, character(1))
            table.body$design <- vapply(fields, function(field) {
                spec <- spec.by.field[[field]]
                if (is.null(spec)) "" else spec$design
            }, character(1))
            table.body$timing <- vapply(fields, function(field) {
                spec <- spec.by.field[[field]]
                if (is.null(spec)) "" else spec$timing
            }, character(1))

            ## Show grouping values only on the first row of each run
            blank.repeated <- function(values) {
                if (length(values) <= 1L) {
                    return(values)
                }
                same.as.previous <- c(
                    FALSE,
                    values[-1] == values[-length(values)] & nzchar(values[-1])
                )
                values[same.as.previous] <- ""
                values
            }
            table.body$primacy <- blank.repeated(table.body$primacy)
            table.body$design <- blank.repeated(table.body$design)
            table.body$timing <- blank.repeated(table.body$timing)

            group.cols <- c("primacy", "design", "timing")
            label.idx <- match("label", names(table.body))
            before <- setdiff(names(table.body)[seq_len(label.idx - 1L)], group.cols)
            after <- setdiff(
                names(table.body)[seq(label.idx, ncol(table.body))],
                group.cols
            )
            table.body[, c(before, group.cols, after), drop = FALSE]
        }
    )

    results.table <- results.table |>
        gtsummary::modify_column_unhide(columns = c(primacy, design, timing)) |>
        gtsummary::modify_table_styling(
            columns = c(primacy, design, timing),
            align = "left"
        ) |>
        gtsummary::modify_header(
            primacy ~ "**Primacy**",
            design ~ "**Design component**",
            timing ~ "**Timing**",
            label ~ "**Outcome (effect measure)**",
            estimate ~ "**Estimate**",
            conf.low ~ "**95% CI**",
            p.value ~ "**p-value**"
        )

    if (isTRUE(knitr::is_latex_output())) {
        results.table <- results.table |>
            gtsummary::as_kable_extra(format = "latex", booktabs = TRUE, linesep = "") |>
            kableExtra::kable_styling(
                latex_options = c("repeat_header"),
                font_size = 7
            )
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

#' Build analysis-results shell row specifications from the outcomes summary
#'
#' @param data Data frame of outcomes summary rows.
#' @param all Logical. Expand nested QoL/disability to domain-level rows.
#' @return A list of specs with `field`, `label`, `measure`, `primacy`,
#'     `design`, and `timing`.
build_outcomes_analysis_results_specs <- function(data, all = FALSE) {
    measure.labels <- c(
        "OR" = "Odds ratio",
        "ARD" = "Absolute risk difference",
        "COR" = "Cumulative odds ratio",
        "Rate ratio" = "Rate ratio",
        "mean difference" = "Mean difference",
        "Logit-scale difference" = "Logit-scale difference"
    )
    eq5d.domains <- c(
        "mobility",
        "self-care",
        "usual activities",
        "pain/discomfort",
        "anxiety/depression"
    )
    whodas.domains <- c(
        "cognition",
        "mobility",
        "self-care",
        "getting along",
        "life activities",
        "participation"
    )
    group.order <- c(
        "Primary|—|Within 30 days",
        "Secondary|Main stepped-wedge|During hospital stay",
        "Secondary|Main stepped-wedge|Within 24 hours",
        "Secondary|Main stepped-wedge|At 30 days",
        "Secondary|Main stepped-wedge|At three months",
        "Secondary|Nested staircase|During initial resuscitation",
        "Secondary|Nested staircase|Within seven days of discharge",
        "Secondary|Nested staircase|At 30 days",
        "Secondary|Nested staircase|At three months"
    )

    specs <- list()
    counter <- 0L

    add.spec <- function(label, measure, primacy, design, timing) {
        counter <<- counter + 1L
        measure.label <- if (measure %in% names(measure.labels)) {
            unname(measure.labels[[measure]])
        } else {
            measure
        }
        specs[[length(specs) + 1L]] <<- list(
            field = paste0("analysis_result_", counter),
            label = paste0(label, " (", measure.label, ")"),
            measure = measure,
            primacy = primacy,
            design = design,
            timing = timing
        )
    }

    for (i in seq_len(nrow(data))) {
        outcome <- data$outcome[[i]]
        design.component <- data$design_component[[i]]
        effect.measure <- data$effect_measure[[i]]
        grouping <- classify_outcomes_analysis_results_grouping(outcome, design.component)
        short.label <- shorten_outcomes_analysis_results_label(outcome)

        expand.nested <- isTRUE(all) && grepl("^(Quality of life|Disability)\\b", outcome)

        if (!expand.nested) {
            measures <- trimws(unlist(strsplit(effect.measure, ";", fixed = TRUE)))
            measures <- measures[nzchar(measures)]
            for (measure in measures) {
                add.spec(
                    short.label,
                    measure,
                    grouping$primacy,
                    grouping$design,
                    grouping$timing
                )
            }
            next
        }

        if (grepl("^Quality of life\\b", outcome)) {
            for (domain in eq5d.domains) {
                add.spec(
                    paste0("EQ-5D-5L ", domain),
                    "COR",
                    grouping$primacy,
                    grouping$design,
                    grouping$timing
                )
            }
            add.spec(
                "EQ-5D-5L VAS",
                "mean difference",
                grouping$primacy,
                grouping$design,
                grouping$timing
            )
        } else {
            for (domain in whodas.domains) {
                add.spec(
                    paste0("WHODAS 2.0 ", domain),
                    "COR",
                    grouping$primacy,
                    grouping$design,
                    grouping$timing
                )
            }
            add.spec(
                "WHODAS 2.0 summary score",
                "mean difference",
                grouping$primacy,
                grouping$design,
                grouping$timing
            )
        }
    }

    group.keys <- vapply(specs, function(spec) {
        paste(spec$primacy, spec$design, spec$timing, sep = "|")
    }, character(1))
    group.ranks <- match(group.keys, group.order)
    group.ranks[is.na(group.ranks)] <- length(group.order) + 1L
    specs[order(group.ranks, seq_along(specs))]
}

#' Classify an outcomes-summary row into primacy, design, and timing columns
classify_outcomes_analysis_results_grouping <- function(outcome, design) {
    if (grepl("^Primary outcome", outcome)) {
        return(list(
            primacy = "Primary",
            design = "Main stepped-wedge",
            timing = "Within 30 days"
        ))
    }

    design.label <- if (identical(design, "Main stepped-wedge")) {
        "Main stepped-wedge"
    } else if (identical(design, "Nested staircase")) {
        "Nested staircase"
    } else {
        design
    }

    timing <- if (grepl("Adherence|resuscitation", outcome)) {
        "During initial resuscitation"
    } else if (grepl("within 24 hours", outcome)) {
        "Within 24 hours"
    } else if (grepl("within seven days|within 7 days", outcome)) {
        "Within seven days of discharge"
    } else if (grepl("at 30 days|within 30 days", outcome) &&
        !grepl("three months|3 months", outcome)) {
        "At 30 days"
    } else if (grepl("three months|3 months", outcome)) {
        "At three months"
    } else {
        "During hospital stay"
    }

    list(
        primacy = "Secondary",
        design = design.label,
        timing = timing
    )
}

#' Shorten outcome labels when timing is conveyed by the Timing column
shorten_outcomes_analysis_results_label <- function(outcome) {
    label <- sub("^Primary outcome:\\s*", "", outcome)
    label <- sub("\\s+within 24 hours of arrival at the emergency department$", "", label)
    label <- sub("\\s+within 30 days of arrival at the emergency department$", "", label)
    label <- sub("\\s+within three months of arrival at the emergency department$", "", label)
    label <- sub("\\s+at 30 days after arrival at the emergency department$", "", label)
    label <- sub("\\s+at three months after arrival at the emergency department$", "", label)
    label <- sub("\\s+within seven days of discharge$", "", label)
    label <- sub("\\s+during initial patient resuscitation$", "", label)
    label
}

#' Build one blanked `tbl_regression` row for the analysis-results shell
#'
#' @param row.label Character. Label shown in the characteristic column.
#' @param measure Character. Effect-measure code from the outcomes summary.
#' @param variable.name Character. Unique variable name used for section headers.
#' @param seed Integer. RNG seed for the placeholder data.
#' @return A one-row `gtsummary` table.
create_outcomes_analysis_results_shell_row <- function(row.label,
                                                       measure,
                                                       variable.name = "intervention",
                                                       seed = 1L) {
    assertthat::assert_that(is.character(row.label) && length(row.label) == 1)
    assertthat::assert_that(is.character(measure) && length(measure) == 1)
    assertthat::assert_that(is.character(variable.name) && length(variable.name) == 1)
    assertthat::assert_that(is.numeric(seed) && length(seed) == 1)

    set.seed(as.integer(seed))
    n <- 40L
    intervention <- stats::rbinom(n, 1L, 0.5)

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
            table.body$variable <- variable.name
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
#' @param output.file Character or NULL. Path for the Word document.
#' @param title Character or NULL. Title shown in the Word document.
#' @param all Logical. If TRUE, preview the supplementary all-outcomes table.
#' @param cleanup.qmd Logical. If TRUE, delete the temporary `.qmd` after
#'     rendering.
#' @return Invisibly, the path to `output.file`.
#'
#' @examples
#' \dontrun{
#' create_outcomes_analysis_results_table_word_preview()
#' create_outcomes_analysis_results_table_word_preview(all = TRUE)
#' }
create_outcomes_analysis_results_table_word_preview <- function(
    output.file = NULL,
    title = NULL,
    all = FALSE,
    cleanup.qmd = FALSE) {
    assertthat::assert_that(is.null(output.file) || (is.character(output.file) && length(output.file) == 1))
    assertthat::assert_that(is.null(title) || (is.character(title) && length(title) == 1))
    assertthat::assert_that(is.logical(all) && length(all) == 1)
    assertthat::assert_that(is.logical(cleanup.qmd) && length(cleanup.qmd) == 1)

    if (is.null(output.file)) {
        output.file <- if (isTRUE(all)) {
            "_test-outcomes-analysis-results-all-word.docx"
        } else {
            "_test-outcomes-analysis-results-word.docx"
        }
    }
    if (is.null(title)) {
        title <- if (isTRUE(all)) {
            "All analysis results — Word preview"
        } else {
            "Key analysis results — Word preview"
        }
    }

    table.call <- if (isTRUE(all)) {
        "create_outcomes_analysis_results_table(all = TRUE)"
    } else {
        "create_outcomes_analysis_results_table()"
    }
    table.label <- if (isTRUE(all)) {
        "tbl-outcomes-analysis-results-all"
    } else {
        "tbl-outcomes-analysis-results"
    }
    table.caption <- if (isTRUE(all)) {
        "Shell table of all primary and secondary outcome analysis results"
    } else {
        "Shell table of key primary and secondary outcome analysis results"
    }

    render_shell_table_word_preview(
        table.call = table.call,
        output.file = output.file,
        title = title,
        table.label = table.label,
        table.caption = table.caption,
        description = paste(
            "gtsummary::tbl_regression-style blank shell for intervention-effect",
            "estimates, with section headers by design and follow-up time point."
        ),
        cleanup.qmd = cleanup.qmd
    )
}
