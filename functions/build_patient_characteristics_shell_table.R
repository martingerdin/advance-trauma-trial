#' Convert a LaTeX tabular kable to a longtable with repeated headers
#'
#' @param kable.latex Character. LaTeX produced by `kableExtra`.
#' @param n.columns Integer. Number of table columns.
#' @param caption Character. Table caption text.
#' @param label Character. Table label for cross-referencing.
#' @return Character vector of LaTeX.
convert_patient_table_to_longtable <- function(kable.latex,
                                               n.columns,
                                               caption = NULL,
                                               label = NULL) {
    kable.latex <- as.character(kable.latex)
    kable.latex <- gsub("\\\\begin\\{table\\}\\[[^]]*\\]", "", kable.latex)
    kable.latex <- gsub("\\\\begin\\{table\\}", "", kable.latex)
    kable.latex <- gsub("\\\\end\\{table\\}", "", kable.latex)

    if (!is.null(caption) && nzchar(caption)) {
        caption.line <- if (!is.null(label) && nzchar(label)) {
            paste0("\\caption{", caption, "\\label{", label, "}\\\\\n", "\\", "tabularnewline\n")
        } else {
            paste0("\\caption{", caption, "}\\\\\n", "\\", "tabularnewline\n")
        }
        kable.latex <- sub("\\toprule", paste0(caption.line, "\\toprule"), kable.latex, fixed = TRUE)
    }

    header.pattern <- "(\\\\toprule[\\s\\S]*?\\\\midrule)"
    header.match <- regexpr(header.pattern, kable.latex, perl = TRUE)
    if (header.match[1] == -1) {
        return(kable.latex)
    }
    header.start <- header.match
    header.end <- header.match + attr(header.match, "match.length") - 1
    header <- substr(kable.latex, header.start, header.end)
    before <- substr(kable.latex, 1, header.start - 1)
    after <- substr(kable.latex, header.end + 1, nchar(kable.latex))
    paste0(
        before,
        header,
        "\n\\endfirsthead\n",
        sprintf("\\multicolumn{%d}{@{\\extracolsep{\\fill}}l}{\\textit{(continued)}}\\\\\n", n.columns),
        header,
        "\n\\endhead\n\n\\endfoot\n\\bottomrule\n\\endlastfoot\n",
        after
    )
}

#' Prevent kableExtra `\centering` from affecting text after a longtable
#'
#' `kableExtra` emits `\centering\begingroup`; `\endgroup` restores the font
#' size group but leaves `\centering` active for the rest of the document.
finalize_longtable_latex <- function(kable.latex) {
    kable.latex <- as.character(kable.latex)
    kable.latex <- gsub("\\\\centering\\\\begingroup", "\\\\begingroup", kable.latex)
    kable.latex <- gsub("\\\\centering\\s*\\n\\\\begingroup", "\\\\begingroup", kable.latex, perl = TRUE)
    kable.latex <- sub(
        "\\\\endgroup\\{\\}\\s*$",
        paste0("\\\\endgroup{}", "\n", "\\\\par\\\\raggedright\n"),
        kable.latex,
        perl = TRUE
    )
    kable.latex
}

#' Insert section header rows into a gtsummary table body
#'
#' When requests include a `section` element, inserts a bold header row before
#' the first outcome in each section.
insert_table_section_headers <- function(tbl, requests) {
    has.sections <- any(vapply(requests, function(request) {
        !is.null(request$section) && nzchar(request$section)
    }, logical(1)))
    if (!has.sections) {
        return(tbl)
    }

    variable.sections <- stats::setNames(
        vapply(requests, function(request) {
            if (is.null(request$section)) "" else request$section
        }, character(1)),
        vapply(requests, function(request) request$field, character(1))
    )

    table.body <- tbl$table_body
    statistic.columns <- grep("^stat_", names(table.body), value = TRUE)
    sections.seen <- character(0)
    new.rows <- list()

    for (i in seq_len(nrow(table.body))) {
        row <- table.body[i, , drop = FALSE]
        if (identical(row$row_type, "label") && row$variable %in% names(variable.sections)) {
            section <- unname(variable.sections[[row$variable]])
            if (nzchar(section) && !section %in% sections.seen) {
                sections.seen <- c(sections.seen, section)
                header.row <- row
                header.row$variable <- paste0(".section_", length(sections.seen))
                header.row$var_type <- "section"
                header.row$row_type <- "section"
                header.row$var_label <- ""
                header.row$label <- section
                header.row$stat_label <- ""
                for (statistic.column in statistic.columns) {
                    header.row[[statistic.column]] <- ""
                }
                new.rows <- c(new.rows, list(header.row))
            }
        }
        new.rows <- c(new.rows, list(row))
    }

    tbl$table_body <- dplyr::bind_rows(new.rows)
    tbl
}

#' Extract ordered unique section labels from table requests
get_table_section_labels <- function(requests) {
    sections <- vapply(requests, function(request) {
        if (is.null(request$section)) "" else request$section
    }, character(1))
    sections <- sections[nzchar(sections)]
    sections[!duplicated(sections)]
}

#' Format section header rows in LaTeX table output
#'
#' gtsummary converts markdown `**bold**` in column headers to `\textbf{}`, but
#' custom section rows are emitted as plain text (or literal asterisks). This
#' helper rewrites those rows to spanning bold LaTeX headers.
format_section_rows_in_latex <- function(kable.latex, n.columns, section.labels) {
    kable.latex <- as.character(kable.latex)
    empty.columns <- paste0("(?:&\\s*){", n.columns - 1L, "}")
    for (section in section.labels) {
        escaped.section <- gsub("([\\()\\[\\]{}.*+?|^$\\\\])", "\\\\\\1", section, perl = TRUE)
        replacement <- paste0(
            "\\\\multicolumn{", n.columns, "}{@{\\\\extracolsep{\\\\fill}}l}{\\\\textbf{",
            section,
            "}}\\\\"
        )
        patterns <- c(
            paste0("<span[^>]*>\\*\\*", escaped.section, "\\*\\*,?\\s*</span>\\s*", empty.columns, "\\\\"),
            paste0("\\*\\*", escaped.section, "\\*\\*,\\s*", empty.columns, "\\\\"),
            paste0("\\\\textbf\\{", escaped.section, "\\},\\s*", empty.columns, "\\\\"),
            paste0(escaped.section, ",\\s*", empty.columns, "\\\\")
        )
        for (pattern in patterns) {
            kable.latex <- gsub(pattern, replacement, kable.latex, perl = TRUE)
        }
    }

    kable.latex
}

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
#' @param label.header Character. Header text for the first column.
#' @param longtable Logical. If TRUE and the output format is LaTeX, return a
#'     page-breaking `longtable` rather than a floating `table` environment.
#' @param dropped.levels Character. Categorical levels to omit from the shell.
#' @param label.width Numeric. Fraction of `\linewidth` for the label column in
#'     PDF/LaTeX output.
#' @return A `gtsummary` or `kableExtra` table object, or, when `longtable =
#'     TRUE` in LaTeX output, a `knitr_asis` object containing raw LaTeX.
build_patient_characteristics_shell_table <- function(data,
                                                      requests,
                                                      groups,
                                                      include.overall,
                                                      longtable = FALSE,
                                                      label.header = "**Characteristic**",
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
        gtsummary::modify_header(
            label ~ label.header,
            gtsummary::all_stat_cols() ~ "**{level}**"
        ) |>
        gtsummary::modify_spanning_header(
            gtsummary::all_stat_cols(stat_0 = FALSE) ~ "**ATLS training**"
        ) |>
        gtsummary::add_stat_label()

    patient.table <- insert_table_section_headers(patient.table, requests)
    section.labels <- get_table_section_labels(requests)

    if (length(section.labels) > 0L && !isTRUE(knitr::is_latex_output())) {
        patient.table <- gtsummary::modify_table_styling(
            patient.table,
            columns = label,
            rows = row_type == "section",
            text_format = "bold"
        )
    }

    if (isTRUE(knitr::is_latex_output())) {
        n.statistic.columns <- length(groups) + as.integer(include.overall)
        n.columns <- 1L + n.statistic.columns
        statistic.width <- round((0.84 - label.width) / n.statistic.columns, 3)
        table.environment <- if (isTRUE(longtable)) "longtable" else "tabular"

        patient.table <- patient.table |>
            gtsummary::as_kable_extra(format = "latex", booktabs = TRUE, linesep = "")

        if (isTRUE(longtable)) {
            patient.table <- kableExtra::kable_styling(patient.table, font_size = 8)
        } else {
            patient.table <- kableExtra::kable_styling(
                patient.table,
                latex_options = "HOLD_position",
                font_size = 8
            )
        }

        column.preamble <- paste0(
            "\\setlength{\\tabcolsep}{3pt}\\begin{", table.environment, "}{",
            ">{\\raggedright\\arraybackslash}p{", label.width, "\\linewidth}",
            "*{", n.statistic.columns, "}{>{\\centering\\arraybackslash}p{", statistic.width, "\\linewidth}}",
            "}"
        )
        column.replacement <- gsub("\\\\", "\\\\\\\\", column.preamble)
        table.attributes <- attributes(patient.table)
        patient.table <- sub("\\\\begin\\{tabular\\}\\{[lcr]+\\}", column.replacement, patient.table)
        if (isTRUE(longtable)) {
            patient.table <- gsub("\\\\end\\{tabular\\}", "\\\\end{longtable}", patient.table)
        }
        attributes(patient.table) <- table.attributes

        if (length(section.labels) > 0L) {
            patient.table <- format_section_rows_in_latex(
                kable.latex = patient.table,
                n.columns = n.columns,
                section.labels = section.labels
            )
        }

        if (isTRUE(longtable)) {
            caption <- knitr::opts_current$get("tbl.cap")
            label <- knitr::opts_current$get("label")
            patient.table <- convert_patient_table_to_longtable(
                kable.latex = patient.table,
                n.columns = n.columns,
                caption = caption,
                label = label
            )
            patient.table <- finalize_longtable_latex(patient.table)
            return(knitr::asis_output(patient.table))
        }
    }

    return(patient.table)
}
