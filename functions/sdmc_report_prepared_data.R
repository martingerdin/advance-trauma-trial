#' SDMC report prepared-data pipeline
#'
#' Helpers that let the SDMC report be fed from simple CSV summary files instead
#' of simulated shells. The CSV files in `sdmc-report/prepared/` are the
#' contract between the data team (who complete them manually with the required
#' summaries) and the report (which renders them into the tables). See
#' `sdmc-report/README.md` for the full pipeline description.
#'
#' The empty templates mirror, row for row, the shell tables produced by the
#' existing `create_*_table()` functions, so the layout stays identical to the
#' SAP. The data team completes the CSVs manually (typically in Excel, from the
#' trial data they hold). Column order in the CSVs must match the stat-column
#' order of the shell; when preparing a report, values are injected
#' positionally into the corresponding rows of the shell table.

#' Directory that holds the prepared summary CSVs
#'
#' @param path Character or NULL. Override directory. Defaults to
#'     `"sdmc-report/prepared"` relative to the working directory (the
#'     statistical-analysis-plan directory when documents render).
#' @return Character path.
sdmc_prepared_data_dir <- function(path = NULL) {
    if (is.null(path)) "sdmc-report/prepared" else path
}

#' Read a REDCap data dictionary CSV into the field-specification format
#'
#' Converts an exported REDCap data dictionary (as committed under
#' `../cluster-screening/` and `../case-record-form/`) into the
#' `field_name` / `field_type` / `select_choices_or_calculations` data frame
#' expected by [build_patient_characteristics_shell_table()] and
#' [create_cluster_characteristics_table()].
#'
#' @param path Character. Path to the dictionary CSV.
#' @return A data frame.
read_data_dictionary <- function(path) {
    dictionary <- utils::read.csv(path, check.names = FALSE)
    data.frame(
        field_name = dictionary[["Variable / Field Name"]],
        field_type = dictionary[["Field Type"]],
        select_choices_or_calculations = dictionary[["Choices, Calculations, OR Slider Labels"]]
    )
}

#' Pooled summaries of the primary and secondary outcomes
#'
#' Shell table with a single "All participants" stratum, as required by the SAP
#' for SDMC reports: outcome values are summarised pooled across intervention
#' conditions and periods, never by intervention condition. Rows are derived
#' from `tables/outcomes-summary.json`, the same source that drives the SAP's
#' outcome tables.
#'
#' @return A `gtsummary` or `knitr_asis` shell table depending on output format.
sdmc_pooled_outcomes_summary_table <- function() {
    outcomes <- jsonlite::fromJSON("tables/outcomes-summary.json")
    sections <- outcomes_shell_section_labels()
    requests <- lapply(seq_len(nrow(outcomes)), function(i) {
        outcome <- outcomes$outcome[[i]]
        label <- sub("^Primary outcome:\\s*", "", outcome)
        label <- sub(" of arrival at the emergency department", "", label, fixed = TRUE)
        label <- sub(" after arrival at the emergency department", "", label, fixed = TRUE)
        label <- sub(" during initial patient resuscitation", "", label, fixed = TRUE)
        data.type <- outcomes$data_type[[i]]
        summary <- if (grepl("Dichotomous", data.type)) {
            "dichotomous"
        } else {
            "continuous" # counts and continuous scores, median (Q1-Q3) per SAP
        }
        section <- if (grepl("In-hospital mortality", outcome)) {
            sections$primary
        } else if (identical(outcomes$design_component[[i]], "Nested staircase")) {
            "Secondary outcomes (nested staircase design)"
        } else {
            sections$main_stepped_wedge
        }
        list(
            field = paste0("outcome_", i),
            label = label,
            source = "external",
            summary = summary,
            section = section
        )
    })
    build_patient_characteristics_shell_table(
        data = NULL,
        requests = requests,
        groups = "All participants",
        include.overall = FALSE,
        longtable = TRUE,
        label.header = "**Outcome**",
        spanning.header = NULL,
        use.simulated.data = FALSE
    )
}

#' Constructor for the by-batch trial progress shell table
#'
#' The interim table functions stratify on intervention condition by default;
#' this closure builds the same layout stratified on batches, relabelling the
#' spanning header so the columns read "Batch 1 ... Batch N".
#'
#' @param batches Numeric. Number of batches.
#' @return A `gtsummary` object (or LaTeX string under LaTeX output).
sdmc_trial_progress_by_batch_table <- function(batches = global_variables()$batches) {
    tbl <- create_interim_trial_progress_table(
        groups = paste("Batch", seq_len(batches)),
        include.overall = TRUE
    )
    if (inherits(tbl, "gtsummary")) {
        tbl <- gtsummary::modify_spanning_header(
            tbl,
            gtsummary::all_stat_cols(stat_0 = FALSE) ~ "**Batch**"
        )
    }
    tbl
}

#' Constructor for the cluster characteristics shell table
#'
#' Uses the committed cluster-screening dictionary; cells are blank for
#' completion with observed data.
#'
#' @return A `gtsummary` or `knitr_asis` shell table.
sdmc_cluster_characteristics_table <- function() {
    create_cluster_characteristics_table(
        data = read_data_dictionary("../cluster-screening/data-dictionary.csv"),
        include.overall = TRUE,
        use.simulated.data = FALSE
    )
}

#' Constructor for the key patient characteristics shell table
#'
#' Uses the committed case-record-form dictionary; cells are blank for
#' completion with observed data.
#'
#' @return A `gtsummary` or `knitr_asis` shell table.
sdmc_patient_characteristics_table <- function() {
    create_patient_characteristics_table(
        data = read_data_dictionary("../case-record-form/data-dictionary.csv"),
        include.overall = TRUE,
        use.simulated.data = FALSE
    )
}

#' Build the gtsummary shell object even under LaTeX output
#'
#' The shell-building functions return a rendered LaTeX string when
#' `knitr::is_latex_output()` is TRUE, but the prepared-data values must be
#' injected into the underlying `gtsummary` object. Temporarily switch the
#' knitr output format to force the object path, then restore it.
#'
#' @param expr Expression that builds the shell table.
#' @return A `gtsummary` object.
sdmc_shell_object <- function(expr) {
    if (!isTRUE(knitr::is_latex_output())) {
        return(force(expr))
    }
    fmt <- knitr::opts_knit$get("out.format")
    knitr::opts_knit$set(out.format = "markdown")
    on.exit(knitr::opts_knit$set(out.format = fmt), add = TRUE)
    force(expr)
}

#' Write a blank prepared-data template for a table
#'
#' Extracts the row layout (sections and row labels) and the stat-column order
#' from the shell table built by `table.fun` and writes a CSV with one column
#' per stratum, all value cells blank, ready for the data team to fill.
#'
#' @param path Character. Output CSV path.
#' @param table.fun Function. Zero-argument function returning the shell table.
#' @param headers Character or NULL. Friendly column headers for the stratum
#'     columns, in the same order as the shell's stat columns. Defaults to the
#'     stat-column names.
#' @return Invisibly, `path`.
sdmc_write_prepared_template <- function(path, table.fun, headers = NULL) {
    tbl <- table.fun()
    assertthat::assert_that(inherits(tbl, "gtsummary"))
    tb <- tbl$table_body

    fillable <- tb$row_type %in% c("label", "level", "missing")
    section <- vapply(tb$row_type, function(type) {
        if (identical(type, "section")) NA_character_ else NA_character_
    }, character(1))
    ## Carry the last section header down to the fillable rows below it.
    for (i in seq_len(nrow(tb))) {
        if (identical(tb$row_type[[i]], "section")) {
            section[i] <- tb$label[[i]]
        } else if (i > 1) {
            section[i] <- section[i - 1]
        }
    }

    out <- data.frame(
        section = section[fillable],
        row_label = tb$label[fillable],
        stringsAsFactors = FALSE
    )
    stat.cols <- grep("^stat_[0-9]+$", names(tb), value = TRUE)
    if (is.null(headers)) {
        headers <- stat.cols
    }
    assertthat::assert_that(length(headers) == length(stat.cols))
    for (j in seq_along(stat.cols)) {
        out[[headers[[j]]]] <- ""
    }

    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    ## UTF-8 with BOM so Excel opens the templates correctly (readr strips the
    ## BOM when the report reads the completed files back).
    readr::write_excel_csv(out, path, na = "")
    invisible(path)
}

#' Write all blank prepared-data templates for the SDMC report
#'
#' @param dir Character. Output directory. Defaults to
#'     [sdmc_prepared_data_dir()].
#' @return Invisibly, `dir`.
sdmc_write_all_prepared_templates <- function(dir = sdmc_prepared_data_dir()) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    batches <- global_variables()$batches
    sequences <- global_variables()$sequences

    sdmc_write_prepared_template(
        file.path(dir, "trial-progress.csv"),
        create_interim_trial_progress_table,
        headers = c("Before ATLS", "After ATLS", "Overall")
    )
    sdmc_write_prepared_template(
        file.path(dir, "trial-progress-by-batch.csv"),
        sdmc_trial_progress_by_batch_table,
        headers = c(paste("Batch", seq_len(batches)), "Overall")
    )
    sdmc_write_prepared_template(
        file.path(dir, "randomisation-adherence.csv"),
        create_interim_randomisation_adherence_table,
        headers = c("Before ATLS", "After ATLS", "Overall")
    )
    sdmc_write_prepared_template(
        file.path(dir, "training.csv"),
        create_interim_training_table,
        headers = c("Before ATLS", "After ATLS", "Overall")
    )
    sdmc_write_prepared_template(
        file.path(dir, "protocol-deviations.csv"),
        create_interim_protocol_deviations_table,
        headers = c("Before ATLS", "After ATLS", "Overall")
    )
    sdmc_write_prepared_template(
        file.path(dir, "safety-events.csv"),
        create_interim_safety_events_table,
        headers = c("Before ATLS", "After ATLS", "Overall")
    )
    sdmc_write_prepared_template(
        file.path(dir, "outcome-completeness.csv"),
        create_interim_outcome_completeness_table,
        headers = c("Before ATLS", "After ATLS", "Overall")
    )
    sdmc_write_prepared_template(
        file.path(dir, "outcomes-summary.csv"),
        sdmc_pooled_outcomes_summary_table,
        headers = "All participants"
    )
    sdmc_write_prepared_template(
        file.path(dir, "cluster-characteristics.csv"),
        sdmc_cluster_characteristics_table,
        headers = c(paste("Sequence", seq_len(sequences)), "Overall")
    )
    sdmc_write_prepared_template(
        file.path(dir, "patient-characteristics.csv"),
        sdmc_patient_characteristics_table,
        headers = c("Before ATLS", "After ATLS", "Overall")
    )
    invisible(dir)
}

#' Inject prepared CSV values into a shell table
#'
#' Values are matched positionally: the CSV rows correspond one-to-one, in
#' order, to the fillable rows (labels, levels, and missing rows) of the shell
#' table. The first two CSV columns (`section` and `row_label`) describe the
#' rows; the remaining columns hold the stratum values in the same order as the
#' shell's stat columns. Extra CSV rows are ignored with a warning; missing
#' rows simply stay blank, so the CSVs can be filled gradually.
#'
#' @param shell A `gtsummary` shell table.
#' @param csv A data frame read from the prepared CSV (character columns).
#' @return The shell table with values injected.
sdmc_inject_prepared_values <- function(shell, csv) {
    gtsummary::modify_table_body(shell, function(tb) {
        fillable <- tb$row_type %in% c("label", "level", "missing")
        positions <- which(fillable)
        stat.cols <- grep("^stat_[0-9]+$", names(tb), value = TRUE)
        n.stat <- length(stat.cols)
        csv.stat.cols <- seq_len(max(0L, ncol(csv) - 2L))
        if (nrow(csv) > length(positions)) {
            warning(
                "Prepared CSV has ", nrow(csv), " rows but the shell table has ",
                length(positions), " fillable rows; extra rows are ignored."
            )
        }
        n.match <- min(nrow(csv), length(positions))
        if (n.match <= 0 || n.stat == 0) {
            return(tb)
        }
        for (j in seq_len(min(n.stat, length(csv.stat.cols)))) {
            value.col <- as.character(csv[[j + 2L]])
            tb[[stat.cols[[j]]]][positions[seq_len(n.match)]] <- value.col[seq_len(n.match)]
        }
        tb
    })
}

#' Render a prepared table in the SDMC report
#'
#' Builds the shell table with `table.fun`, injects the values from the
#' prepared CSV if it exists and has values, and returns the table ready for
#' printing. If the CSV is absent or entirely blank, the blank shell is
#' returned unchanged, so the report renders before data collection is
#' complete.
#'
#' @param csv.name Character. File name of the prepared CSV inside `dir`.
#' @param table.fun Function. Zero-argument function returning the shell table.
#' @param dir Character. Directory holding the prepared CSVs.
#' @return A `gtsummary` object (docx) or a formatted table (LaTeX).
sdmc_render_prepared_table <- function(csv.name, table.fun, dir = sdmc_prepared_data_dir()) {
    shell <- sdmc_shell_object(table.fun())
    path <- file.path(dir, csv.name)
    if (!file.exists(path)) {
        return(shell)
    }
    csv <- readr::read_csv(
        path,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE
    )
    ## readr parses empty cells as NA; normalise to "" so blank cells stay blank.
    csv[] <- lapply(csv, function(x) {
        x[is.na(x)] <- ""
        x
    })
    ## Report blank shells unchanged when no stratum values are filled yet.
    stratum.cols <- seq_len(max(0L, ncol(csv) - 2L))
    has.values <- any(vapply(stratum.cols, function(j) {
        any(nzchar(trimws(as.character(csv[[j + 2L]]))), na.rm = TRUE)
    }, logical(1)))
    if (!has.values) {
        return(shell)
    }
    shell <- sdmc_inject_prepared_values(shell, csv)
    if (isTRUE(knitr::is_latex_output())) {
        shell <- gtsummary::as_kable_extra(shell, format = "latex", booktabs = TRUE)
    }
    shell
}
