#' Build a shell patient characteristics table from a variable specification
#'
#' Shared implementation for patient baseline characteristic shell tables. Takes
#' a list of variable requests, resolves each against the REDCap data
#' dictionary where appropriate, and returns a `gtsummary` table stratified by
#' ATLS training period with blanked body cells.
#'
#' @param data A data frame. A REDCap trial-data dictionary (metadata).
#' @param requests A list of variable request lists, each with elements `field`,
#'     `label`, `source` ("dictionary" or "external"), and optionally `summary`
#'     and `levels`.
#' @param groups Character. Stratum labels for the table columns.
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
#' @param dropped.levels Character. Categorical levels to omit from the shell.
#' @param label.width Numeric. Fraction of `\linewidth` for the label column in
#'     PDF/LaTeX output.
#' @return A `gtsummary` or `kableExtra` table object.
build_patient_characteristics_shell_table <- function(data,
                                                      requests,
                                                      groups,
                                                      include.overall,
                                                      dropped.levels = c("Not sure", "Not known", "999. Not known"),
                                                      label.width = 0.34) {
    cell.placeholder <- ""
    dichotomous.value <- "Yes"
    continuous.statistics <- "{median} ({p25}, {p75})"
    missing.text <- "Missing"

    specifications <- lapply(requests, function(request) {
        if (identical(request$source, "dictionary")) {
            specification <- get_redcap_field_specification(data, request$field)
            if (is.null(specification)) {
                return(NULL)
            }
        } else {
            specification <- list(
                field_name = request$field,
                redcap_type = NA_character_,
                type = request$summary,
                levels = if (is.null(request$levels)) character(0) else request$levels
            )
        }

        if (!is.null(request$summary)) {
            specification$type <- request$summary
        }
        if (identical(specification$type, "dichotomous")) {
            specification$levels <- c(dichotomous.value, "No")
        }

        specification$levels <- specification$levels[!specification$levels %in% dropped.levels]
        specification$label <- request$label
        specification
    })
    specifications <- Filter(Negate(is.null), specifications)
    assertthat::assert_that(length(specifications) > 0, msg = "None of the requested characteristics were found.")

    n.rows <- length(groups)
    shell.data <- data.frame(group = factor(groups, levels = groups))
    for (specification in specifications) {
        if (specification$type == "continuous") {
            shell.data[[specification$field_name]] <- as.numeric(seq_len(n.rows))
        } else {
            values <- rep(specification$levels, length.out = n.rows)
            shell.data[[specification$field_name]] <- factor(values, levels = specification$levels)
        }
    }

    variable.names <- vapply(specifications, function(specification) specification$field_name, character(1))
    variable.labels <- stats::setNames(
        lapply(specifications, function(specification) specification$label),
        variable.names
    )
    variable.types <- stats::setNames(
        lapply(specifications, function(specification) specification$type),
        variable.names
    )
    dichotomous.names <- variable.names[vapply(specifications, function(specification) specification$type == "dichotomous", logical(1))]
    variable.values <- stats::setNames(
        rep(list(dichotomous.value), length(dichotomous.names)),
        dichotomous.names
    )

    patient.table <- gtsummary::tbl_summary(
        shell.data,
        by = "group",
        label = variable.labels,
        type = variable.types,
        value = variable.values,
        statistic = list(
            gtsummary::all_categorical() ~ "{n} ({p}%)",
            gtsummary::all_continuous() ~ continuous.statistics
        ),
        missing = "always",
        missing_text = missing.text
    )
    if (include.overall) {
        patient.table <- gtsummary::add_overall(patient.table, last = TRUE)
    }

    patient.table <- gtsummary::modify_table_body(
        patient.table,
        function(table.body) {
            statistic.columns <- grep("^stat_", names(table.body), value = TRUE)
            for (statistic.column in statistic.columns) {
                table.body[[statistic.column]][table.body$row_type %in% c("level", "missing")] <- cell.placeholder
                table.body[[statistic.column]][table.body$row_type == "label" &
                    table.body$var_type %in% c("continuous", "dichotomous")] <- cell.placeholder
            }
            table.body
        }
    )

    patient.table <- patient.table |>
        gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**{level}**") |>
        gtsummary::modify_spanning_header(
            gtsummary::all_stat_cols(stat_0 = FALSE) ~ "**ATLS training**"
        ) |>
        gtsummary::add_stat_label()

    if (isTRUE(knitr::is_latex_output())) {
        n.statistic.columns <- length(groups) + as.integer(include.overall)
        statistic.width <- round((0.84 - label.width) / n.statistic.columns, 3)

        patient.table <- patient.table |>
            gtsummary::as_kable_extra(format = "latex", booktabs = TRUE, linesep = "") |>
            kableExtra::kable_styling(latex_options = "HOLD_position", font_size = 8)

        column.preamble <- paste0(
            "\\setlength{\\tabcolsep}{3pt}\\begin{tabular}{",
            ">{\\raggedright\\arraybackslash}p{", label.width, "\\linewidth}",
            "*{", n.statistic.columns, "}{>{\\centering\\arraybackslash}p{", statistic.width, "\\linewidth}}",
            "}"
        )
        column.replacement <- gsub("\\\\", "\\\\\\\\", column.preamble)
        table.attributes <- attributes(patient.table)
        patient.table <- sub("\\\\begin\\{tabular\\}\\{[lcr]+\\}", column.replacement, patient.table)
        attributes(patient.table) <- table.attributes
    }

    return(patient.table)
}
