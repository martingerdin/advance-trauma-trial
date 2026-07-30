#' Create a shell table of sensitivity, adjusted, and subgroup analysis results
#'
#' Blank `gtsummary::tbl_regression`-style shell for primary-outcome sensitivity
#' analyses, the fully adjusted analysis, and subgroup analyses. Layout matches
#' the main analysis-results shells: section headers, effect measure folded into
#' the outcome label, and blank Estimate / 95% CI / p-value columns.
#'
#' @param label.width Numeric. Fraction of linewidth for the label column in
#'     LaTeX output. Defaults to `0.50`.
#' @return A `gtsummary` table (or LaTeX `kableExtra` longtable under
#'     `knitr::is_latex_output()`).
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_additional_analyses_results_table()
#' }
create_additional_analyses_results_table <- function(label.width = 0.50) {
    assertthat::assert_that(is.numeric(label.width) && length(label.width) == 1)

    specs <- build_additional_analyses_results_specs()

    tables <- lapply(seq_along(specs), function(i) {
        create_outcomes_analysis_results_shell_row(
            row.label = specs[[i]]$label,
            measure = specs[[i]]$measure,
            variable.name = specs[[i]]$field,
            seed = i
        ) |>
            gtsummary::modify_header(
                label ~ "**Analysis**",
                estimate ~ "**Estimate**",
                conf.low ~ "**95% CI**",
                p.value ~ "**p-value**"
            )
    })

    results.table <- gtsummary::tbl_stack(tables, quiet = TRUE) |>
        gtsummary::modify_footnote(everything() ~ NA_character_)

    section.requests <- lapply(specs, function(spec) {
        list(field = spec$field, section = spec$section)
    })
    results.table <- insert_table_section_headers(results.table, section.requests)
    section.labels <- get_table_section_labels(section.requests)

    results.table <- gtsummary::modify_table_body(
        results.table,
        function(table.body) {
            section.rows <- table.body$row_type == "section"
            blank.cols <- intersect(
                c(
                    "estimate", "std.error", "statistic",
                    "conf.low", "conf.high", "ci", "p.value"
                ),
                names(table.body)
            )
            for (column.name in blank.cols) {
                if (is.numeric(table.body[[column.name]])) {
                    table.body[[column.name]][section.rows] <- NA_real_
                } else {
                    table.body[[column.name]][section.rows] <- ""
                }
            }
            table.body
        }
    )

    results.table <- results.table |>
        gtsummary::modify_table_styling(columns = label, align = "left") |>
        gtsummary::modify_header(
            label ~ "**Analysis**",
            estimate ~ "**Estimate**",
            conf.low ~ "**95% CI**",
            p.value ~ "**p-value**"
        )

    if (length(section.labels) > 0L && !isTRUE(knitr::is_latex_output())) {
        results.table <- gtsummary::modify_table_styling(
            results.table,
            columns = label,
            rows = row_type == "section",
            text_format = "bold"
        )
    }

    if (isTRUE(knitr::is_latex_output())) {
        n.result.columns <- 3L
        n.columns <- 1L + n.result.columns
        result.width <- round((0.84 - label.width) / n.result.columns, 3)

        results.table <- results.table |>
            gtsummary::as_kable_extra(format = "latex", booktabs = TRUE, linesep = "") |>
            kableExtra::kable_styling(font_size = 8)

        column.preamble <- paste0(
            "\\setlength{\\tabcolsep}{3pt}\\begin{longtable}{",
            ">{\\raggedright\\arraybackslash}p{", label.width, "\\linewidth}",
            "*{", n.result.columns, "}{>{\\centering\\arraybackslash}p{",
            result.width, "\\linewidth}}",
            "}"
        )
        column.replacement <- gsub("\\\\", "\\\\\\\\", column.preamble)
        table.attributes <- attributes(results.table)
        results.latex <- sub(
            "\\\\begin\\{tabular\\}\\{[lcr]+\\}",
            column.replacement,
            as.character(results.table)
        )
        results.latex <- gsub("\\\\end\\{tabular\\}", "\\\\end{longtable}", results.latex)
        attributes(results.latex) <- table.attributes

        if (length(section.labels) > 0L) {
            results.latex <- format_section_rows_in_latex(
                kable.latex = results.latex,
                n.columns = n.columns,
                section.labels = section.labels
            )
        }

        caption <- knitr::opts_current$get("tbl.cap")
        label <- knitr::opts_current$get("label")
        results.latex <- convert_patient_table_to_longtable(
            kable.latex = results.latex,
            n.columns = n.columns,
            caption = caption,
            label = label
        )
        results.latex <- finalize_longtable_latex(results.latex)
        return(knitr::asis_output(results.latex))
    }

    results.table
}

#' Build row specifications for the additional-analyses results shell
#'
#' @return A list of specs with `field`, `label`, `measure`, and `section`.
build_additional_analyses_results_specs <- function() {
    measure.labels <- c(
        "OR" = "Odds ratio",
        "ARD" = "Absolute risk difference"
    )

    specs <- list()
    counter <- 0L

    add.spec <- function(section, label, measure) {
        counter <<- counter + 1L
        measure.label <- unname(measure.labels[[measure]])
        specs[[length(specs) + 1L]] <<- list(
            field = paste0("additional_analysis_", counter),
            label = paste0(label, ", ", measure.label),
            measure = measure,
            section = section
        )
    }

    add.pair <- function(section, label) {
        add.spec(section, label, "OR")
        add.spec(section, label, "ARD")
    }

    sensitivity <- "Sensitivity analyses"
    add.pair(sensitivity, "Autoregressive within-cluster correlation (AR(1))")
    add.pair(sensitivity, "Random cluster-by-intervention effects")
    add.pair(sensitivity, "Time modelled with a spline function")
    add.pair(sensitivity, "Lag and weaning effects (intervention)")
    add.pair(sensitivity, "Lag and weaning effects (periods since first exposure)")
    add.pair(sensitivity, "Actual date of transition for intervention exposure")

    adjusted <- "Fully adjusted analysis"
    add.pair(adjusted, "Fully adjusted analysis")

    subgroup <- "Subgroup analyses"
    add.pair(
        subgroup,
        "Geographical region (state-specific estimates; states depend on participating clusters)"
    )
    for (level in c(
        "Older adolescents (15-19 years)",
        "Young adults (20-24 years)",
        "Adults (25-59 years)",
        "Older adults (60 years and older)"
    )) {
        add.pair(subgroup, paste0("Age group: ", level))
    }
    for (level in c("Male", "Female")) {
        add.pair(subgroup, paste0("Sex: ", level))
    }
    for (level in c(
        "Blunt multisystem trauma",
        "Penetrating trauma",
        "Severe isolated traumatic brain injury"
    )) {
        add.pair(subgroup, paste0("Clinical cohort: ", level))
    }
    for (level in c("ISS >=16", "ISS <16")) {
        add.pair(subgroup, paste0("Major trauma: ", level))
    }
    for (level in c(
        "Small (<12 patients/month)",
        "Medium (12-20 patients/month)",
        "Large (>20 patients/month)"
    )) {
        add.pair(subgroup, paste0("Cluster size: ", level))
    }

    specs
}
