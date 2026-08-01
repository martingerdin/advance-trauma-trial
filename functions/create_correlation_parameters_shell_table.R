#' Create a shell table of within-cluster correlation parameters
#'
#' Blank `gtsummary::tbl_regression`-style shell for variance components and
#' time-adjusted within-cluster correlations (ICC, within- and between-period
#' correlations), with latent-scale correlations for binary outcomes and AR(1)
#' parameters from the autoregressive sensitivity analysis. Layout matches the
#' analysis-results shells: outcome section headers and blank Estimate / 95% CI
#' columns (no p-values).
#'
#' @param data A data frame or NULL. Outcomes summary with columns `outcome`
#'     and `data_type`. If NULL, reads `path`.
#' @param path Character. Path to the outcomes-summary JSON used when `data`
#'     is NULL. Defaults to `"tables/outcomes-summary.json"`.
#' @param all Logical. If FALSE (the default), the table shows the primary
#'     outcome only. If TRUE, one section is included for each outcome in the
#'     outcomes summary (without expanding EQ-5D-5L/WHODAS domains).
#' @param label.width Numeric. Fraction of linewidth for the parameter column
#'     in LaTeX output. Defaults to `0.50`.
#' @return A `gtsummary` table (or LaTeX `kableExtra` longtable under
#'     `knitr::is_latex_output()`).
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_correlation_parameters_shell_table()
#' create_correlation_parameters_shell_table(all = TRUE)
#' }
create_correlation_parameters_shell_table <- function(
    data = NULL,
    path = "tables/outcomes-summary.json",
    all = FALSE,
    label.width = 0.50) {
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.character(path) && length(path) == 1)
    assertthat::assert_that(is.logical(all) && length(all) == 1)
    assertthat::assert_that(is.numeric(label.width) && length(label.width) == 1)

    if (is.null(data)) {
        data <- jsonlite::fromJSON(path)
    }

    required.columns <- c("outcome", "data_type")
    missing.columns <- setdiff(required.columns, names(data))
    assertthat::assert_that(
        length(missing.columns) == 0,
        msg = paste("Missing columns:", paste(missing.columns, collapse = ", "))
    )

    specs <- build_correlation_parameters_shell_specs(data = data, all = all)

    tables <- lapply(seq_along(specs), function(i) {
        create_outcomes_analysis_results_shell_row(
            row.label = specs[[i]]$label,
            measure = "mean difference",
            variable.name = specs[[i]]$field,
            seed = i
        ) |>
            gtsummary::modify_header(
                label ~ "**Parameter**",
                estimate ~ "**Estimate**",
                conf.low ~ "**95% CI**",
                p.value ~ "**p-value**"
            )
    })

    results.table <- gtsummary::tbl_stack(tables, quiet = TRUE) |>
        gtsummary::modify_footnote(everything() ~ NA_character_) |>
        gtsummary::modify_column_hide(p.value)

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
            label ~ "**Parameter**",
            estimate ~ "**Estimate**",
            conf.low ~ "**95% CI**"
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
        n.result.columns <- 2L
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

#' Build row specifications for the correlation-parameters shell
#'
#' @param data Data frame of outcomes summary rows.
#' @param all Logical. If FALSE, keep only the primary outcome.
#' @return A list of specs with `field`, `label`, and `section`.
build_correlation_parameters_shell_specs <- function(data, all = FALSE) {
    if (!isTRUE(all)) {
        data <- data[grepl("^Primary outcome", data$outcome), , drop = FALSE]
    }

    specs <- list()
    counter <- 0L

    add.spec <- function(section, label) {
        counter <<- counter + 1L
        specs[[length(specs) + 1L]] <<- list(
            field = paste0("correlation_parameter_", counter),
            label = label,
            section = section
        )
    }

    for (i in seq_len(nrow(data))) {
        section <- format_outcomes_analysis_results_label(data$outcome[[i]])
        is.binary <- grepl("Dichotomous", data$data_type[[i]], ignore.case = TRUE)

        add.spec(section, "Cluster variance")
        add.spec(section, "Cluster-by-period variance")
        add.spec(section, "Intra-cluster correlation (ICC)")
        add.spec(section, "Within-period correlation")
        add.spec(section, "Between-period correlation")

        if (is.binary) {
            add.spec(section, "Latent-scale ICC")
            add.spec(section, "Latent-scale within-period correlation")
            add.spec(section, "Latent-scale between-period correlation")
        }

        add.spec(section, "AR(1) within-period correlation")
        add.spec(section, "AR(1) correlation decay parameter (rho)")
    }

    specs
}
