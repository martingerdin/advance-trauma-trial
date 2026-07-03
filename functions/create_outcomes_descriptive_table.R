#' Create a shell table of descriptive outcome summaries
#'
#' Builds a shell (template) table illustrating how primary and secondary
#' outcomes will be summarised before and after ATLS training is implemented in
#' a cluster. Dichotomous outcomes are reported as n (\%); continuous and
#' time-to-event outcomes are reported as median (Q1-Q3); EQ-5D-5L is summarised
#' using its index score and visual analogue scale. Derived outcomes
#' (mortality at specified time points, lengths of stay, adherence proportion,
#' and WHODAS summary scores) are specified directly rather than from single
#' REDCap fields. Missing values are shown for every outcome. The table is
#' laid out with `gtsummary`; body cells are blanked for the analysis plan.
#'
#' @param data A data frame or NULL. The REDCap trial-data dictionary
#'     (metadata). If NULL (the default) the dictionary is fetched from REDCap.
#' @param url.name Character. REDCap API URL environment variable name.
#' @param api.key.name Character. REDCap API token environment variable name.
#' @param groups Character. Stratum labels. Defaults to
#'     c("Before ATLS", "After ATLS").
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
#' @return A `gtsummary` or `kableExtra` table object for short output formats,
#'     or a `knitr_asis` object containing a page-breaking `longtable` for
#'     PDF/LaTeX output.
#'
#' @seealso [create_outcomes_descriptive_table_word_preview()] to render a
#'     minimal Word document for checking section headers and table layout.
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_outcomes_descriptive_table()
#'
#' ## Minimal Word preview (requires Quarto)
#' create_outcomes_descriptive_table_word_preview()
#' }
create_outcomes_descriptive_table <- function(data = NULL,
                                              url.name = "TGI_REDCAP_URL",
                                              api.key.name = "TGI_REDCAP_TRIAL_DATA_API_KEY",
                                              groups = c("Before ATLS", "After ATLS"),
                                              include.overall = TRUE) {
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.character(url.name) && length(url.name) == 1)
    assertthat::assert_that(is.character(api.key.name) && length(api.key.name) == 1)
    assertthat::assert_that(is.character(groups) && length(groups) >= 2)
    assertthat::assert_that(is.logical(include.overall) && length(include.overall) == 1)

    if (is.null(data)) {
        data <- get_redcap_data(
            url.name = url.name,
            api.key.name = api.key.name,
            content = "metadata"
        )
    }

    eq5d.timepoints <- c(
        "within seven days of discharge",
        "at 30 days",
        "at three months"
    )

    whodas.timepoints <- eq5d.timepoints

    requests <- list(
        ## Primary outcome
        list(field = "inhospital_mortality_30d",
             label = "In-hospital mortality within 30 days",
             source = "external", summary = "dichotomous",
             section = "Primary outcome"),

        ## Secondary outcomes (main stepped-wedge design)
        list(field = "all_cause_mortality_24h",
             label = "All-cause mortality within 24 hours",
             source = "external", summary = "dichotomous",
             section = "Secondary outcomes (main stepped-wedge design)"),
        list(field = "all_cause_mortality_30d",
             label = "All-cause mortality within 30 days",
             source = "external", summary = "dichotomous",
             section = "Secondary outcomes (main stepped-wedge design)"),
        list(field = "all_cause_mortality_90d",
             label = "All-cause mortality within three months",
             source = "external", summary = "dichotomous",
             section = "Secondary outcomes (main stepped-wedge design)"),
        list(field = "length_ed_stay",
             label = "Length of emergency department stay (days)",
             source = "external", summary = "continuous",
             section = "Secondary outcomes (main stepped-wedge design)"),
        list(field = "length_hospital_stay",
             label = "Length of hospital stay (days)",
             source = "external", summary = "continuous",
             section = "Secondary outcomes (main stepped-wedge design)"),
        list(field = "icu_admission",
             label = "Intensive care unit admission",
             source = "dictionary", summary = "dichotomous",
             section = "Secondary outcomes (main stepped-wedge design)"),
        list(field = "length_icu_stay",
             label = "Length of intensive care unit stay (days)",
             source = "external", summary = "continuous",
             section = "Secondary outcomes (main stepped-wedge design)"),
        list(field = "return_to_work_30d",
             label = "Return to work at 30 days",
             source = "external", summary = "dichotomous",
             section = "Secondary outcomes (main stepped-wedge design)"),
        list(field = "return_to_work_90d",
             label = "Return to work at three months",
             source = "external", summary = "dichotomous",
             section = "Secondary outcomes (main stepped-wedge design)"),

        ## Secondary outcomes (nested staircase design)
        list(field = "atls_adherence",
             label = "Adherence to ATLS principles (%)",
             source = "external", summary = "continuous",
             section = "Secondary outcomes (nested staircase design)")
    )

    for (timepoint in eq5d.timepoints) {
        timepoint.slug <- gsub("[^a-z0-9]+", "_", tolower(timepoint))
        requests <- c(requests, list(
            list(
                field = paste0("eq5d_index_", timepoint.slug),
                label = paste0("EQ-5D-5L index score ", timepoint),
                source = "external",
                summary = "continuous",
                section = "Secondary outcomes (nested staircase design)"
            ),
            list(
                field = paste0("eq5d_vas_", timepoint.slug),
                label = paste0("EQ-5D-5L visual analogue scale ", timepoint),
                source = "external",
                summary = "continuous",
                section = "Secondary outcomes (nested staircase design)"
            )
        ))
    }

    for (timepoint in whodas.timepoints) {
        timepoint.slug <- gsub("[^a-z0-9]+", "_", tolower(timepoint))
        requests <- c(requests, list(list(
            field = paste0("whodas_summary_", timepoint.slug),
            label = paste0("WHODAS 2.0 summary score ", timepoint),
            source = "external",
            summary = "continuous",
            section = "Secondary outcomes (nested staircase design)"
        )))
    }

    build_patient_characteristics_shell_table(
        data = data,
        requests = requests,
        groups = groups,
        include.overall = include.overall,
        longtable = TRUE,
        label.header = "**Outcome**"
    )
}

#' Write a minimal Word preview of the outcomes descriptive table
#'
#' Renders a single-table Quarto document to Word. Useful for checking section
#' headers, stratification columns, and non-LaTeX formatting without building
#' the full statistical analysis plan.
#'
#' @param output.file Character. Path for the Word document to create. A
#'     companion `.qmd` file is written alongside it unless `cleanup.qmd` is
#'     TRUE.
#' @param title Character. Title shown in the Word document.
#' @param cleanup.qmd Logical. If TRUE, delete the temporary `.qmd` after
#'     rendering.
#' @return Invisibly, the path to `output.file`.
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_outcomes_descriptive_table_word_preview()
#' create_outcomes_descriptive_table_word_preview("preview/outcomes-table.docx")
#' }
create_outcomes_descriptive_table_word_preview <- function(
    output.file = "_test-outcomes-word.docx",
    title = "Outcomes table — Word preview",
    cleanup.qmd = FALSE) {
    assertthat::assert_that(is.character(output.file) && length(output.file) == 1)
    assertthat::assert_that(is.character(title) && length(title) == 1)
    assertthat::assert_that(is.logical(cleanup.qmd) && length(cleanup.qmd) == 1)

    output.file <- normalizePath(output.file, winslash = "/", mustWork = FALSE)
    output.dir <- dirname(output.file)
    if (!dir.exists(output.dir)) {
        dir.create(output.dir, recursive = TRUE, showWarnings = FALSE)
    }

    qmd.file <- sub("\\.docx$", ".qmd", output.file, ignore.case = TRUE)
    if (!grepl("\\.qmd$", qmd.file, ignore.case = TRUE)) {
        qmd.file <- paste0(qmd.file, ".qmd")
    }

    qmd.content <- c(
        "---",
        paste0("title: \"", gsub("\"", "\\\\\"", title), "\""),
        "format:",
        "  docx: default",
        "execute:",
        "  echo: false",
        "  message: false",
        "  warning: false",
        "---",
        "",
        "```{r setup}",
        "noacsr::source_all_functions()",
        "```",
        "",
        "Minimal preview of the outcomes descriptive table for Word output",
        "(section headers and stratification).",
        "",
        "```{r}",
        "#| label: tbl-outcomes-descriptive",
        "#| tbl-cap: \"Descriptive summaries of outcomes\"",
        "create_outcomes_descriptive_table()",
        "```",
        ""
    )
    writeLines(qmd.content, qmd.file, useBytes = TRUE)

    old.wd <- getwd()
    on.exit(setwd(old.wd), add = TRUE)
    setwd(output.dir)

    quarto::quarto_render(
        input = basename(qmd.file),
        output_format = "docx",
        output_file = basename(output.file)
    )

    if (isTRUE(cleanup.qmd)) {
        unlink(basename(qmd.file))
    }

    invisible(output.file)
}
