#' Safety event rows for shell tables
#'
#' Protocol-aligned safety event definitions shared by the final-analysis
#' safety table and the interim SDMC report shell.
#'
#' @return A list of variable request lists for
#'     [build_patient_characteristics_shell_table()].
safety_events_shell_requests <- function() {
    list(
        list(
            field = "prolonged_ventilation",
            label = "Prolonged mechanical ventilation (>7 days)",
            source = "external",
            summary = "dichotomous"
        ),
        list(
            field = "renal_replacement",
            label = "Initiation of renal replacement therapy",
            source = "external",
            summary = "dichotomous"
        ),
        list(
            field = "prolonged_vasopressors",
            label = paste0(
                "Prolonged (>2 days) or renewed use of vasopressors ",
                "such as norepinephrine or vasopressin"
            ),
            source = "external",
            summary = "dichotomous"
        ),
        list(
            field = "other_safety",
            label = "Other reported safety events",
            source = "external",
            summary = "dichotomous"
        ),
        list(
            field = "probably_related",
            label = "Events assessed as probably related to the trial or intervention",
            source = "external",
            summary = "dichotomous"
        )
    )
}

#' Create a shell table of safety events for the final analysis
#'
#' Builds a shell table illustrating how protocol-defined safety events will
#' be summarised before and after ATLS training is implemented in a cluster.
#' Body cells are blanked for the analysis plan.
#'
#' @param groups Character. Stratum labels. Defaults to
#'     c("Before ATLS", "After ATLS").
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
#' @return A `gtsummary` table object, or a `knitr_asis` longtable for
#'     PDF/LaTeX output.
#'
#' @seealso [create_safety_events_table_word_preview()] to render a minimal Word
#'     document for checking table layout.
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_safety_events_table()
#' create_safety_events_table_word_preview()
#' }
create_safety_events_table <- function(
    groups = c("Before ATLS", "After ATLS"),
    include.overall = TRUE) {
    assertthat::assert_that(is.character(groups) && length(groups) >= 2)
    assertthat::assert_that(is.logical(include.overall) && length(include.overall) == 1)

    build_patient_characteristics_shell_table(
        data = data.frame(),
        requests = safety_events_shell_requests(),
        groups = groups,
        include.overall = include.overall,
        longtable = TRUE,
        label.header = "**Safety event**",
        label.width = 0.46,
        missing = "no"
    )
}

#' Write a minimal Word preview of the safety events table
#'
#' @param output.file Character. Path for the Word document to create.
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
#' create_safety_events_table_word_preview()
#' }
create_safety_events_table_word_preview <- function(
    output.file = "_test-safety-events-word.docx",
    title = "Safety events — Word preview",
    cleanup.qmd = FALSE) {
    render_shell_table_word_preview(
        table.call = "create_safety_events_table()",
        output.file = output.file,
        title = title,
        table.label = "tbl-safety-events",
        table.caption = "Safety events",
        description = paste(
            "Minimal preview of the final-analysis safety events table",
            "for Word output."
        ),
        cleanup.qmd = cleanup.qmd
    )
}
