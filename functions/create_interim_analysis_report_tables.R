#' Interim analysis report shell tables
#'
#' Shell builders for a future SDMC interim analysis report. These are **not**
#' currently included in the SAP supplement (the SAP carries only a slim
#' outline). Keep the same row labels and Before/After stratification when
#' wiring to prepared interim datasets later.

interim_analysis_report_groups <- function() {
    c("Before ATLS", "After ATLS")
}

#' Build a stratified interim count shell table
build_interim_count_shell_table <- function(requests,
                                            groups = interim_analysis_report_groups(),
                                            include.overall = TRUE,
                                            label.header = "**Metric**",
                                            label.width = 0.40) {
    build_patient_characteristics_shell_table(
        data = data.frame(),
        requests = requests,
        groups = groups,
        include.overall = include.overall,
        longtable = TRUE,
        label.header = label.header,
        missing = "no",
        spanning.header = "**Intervention condition**",
        label.width = label.width,
        force.stat.label = "n"
    )
}

#' Trial progress counts for the interim report
#'
#' @param groups Character. Intervention-condition labels.
#' @param include.overall Logical. Append an Overall column.
#' @return A shell table (or LaTeX longtable under `knitr::is_latex_output()`).
create_interim_trial_progress_table <- function(
    groups = interim_analysis_report_groups(),
    include.overall = TRUE) {
    requests <- list(
        list(
            field = "clusters_included",
            label = "Clusters included",
            source = "external",
            summary = "dichotomous",
            section = "Clusters"
        ),
        list(
            field = "clusters_dropped",
            label = "Clusters dropping out",
            source = "external",
            summary = "dichotomous",
            section = "Clusters"
        ),
        list(
            field = "participants_screened",
            label = "Potentially eligible participants screened",
            source = "external",
            summary = "dichotomous",
            section = "Participants"
        ),
        list(
            field = "participants_included",
            label = "Participants included",
            source = "external",
            summary = "dichotomous",
            section = "Participants"
        ),
        list(
            field = "participants_no_ooh_consent",
            label = "Participants who did not consent to out-of-hospital follow-up",
            source = "external",
            summary = "dichotomous",
            section = "Participants"
        ),
        list(
            field = "participants_ltfu",
            label = "Participants lost to follow-up",
            source = "external",
            summary = "dichotomous",
            section = "Participants"
        )
    )
    build_interim_count_shell_table(
        requests = requests,
        groups = groups,
        include.overall = include.overall,
        label.width = 0.40
    )
}

#' Randomisation-schedule adherence for the interim report
create_interim_randomisation_adherence_table <- function(
    groups = interim_analysis_report_groups(),
    include.overall = TRUE) {
    requests <- list(
        list(
            field = "clusters_on_schedule",
            label = "Clusters transitioning on scheduled date",
            source = "external",
            summary = "dichotomous",
            section = "Adherence"
        ),
        list(
            field = "clusters_within_4_weeks",
            label = "Clusters transitioning within four weeks of scheduled date",
            source = "external",
            summary = "dichotomous",
            section = "Adherence"
        ),
        list(
            field = "clusters_more_than_4_weeks",
            label = "Clusters transitioning more than four weeks from scheduled date",
            source = "external",
            summary = "dichotomous",
            section = "Adherence"
        )
    )
    build_interim_count_shell_table(
        requests = requests,
        groups = groups,
        include.overall = include.overall,
        label.width = 0.46
    )
}

#' Outcome-data completeness for the interim report (no outcome values)
#'
#' Row labels are taken from `tables/outcomes-summary.json`. Cells are blank
#' counts of observed and missing outcomes by intervention condition.
#'
#' @param path Character. Path to the outcomes-summary JSON.
#' @param key.only Logical. If TRUE (default), include main stepped-wedge
#'     outcomes and adherence only.
create_interim_outcome_completeness_table <- function(
    path = "tables/outcomes-summary.json",
    groups = interim_analysis_report_groups(),
    include.overall = TRUE,
    key.only = TRUE) {
    assertthat::assert_that(is.character(path) && length(path) == 1)
    assertthat::assert_that(is.logical(key.only) && length(key.only) == 1)

    outcomes <- jsonlite::fromJSON(path)
    if (isTRUE(key.only)) {
        outcomes <- outcomes[
            outcomes$design_component == "Main stepped-wedge" |
                grepl("Adherence", outcomes$outcome),
            ,
            drop = FALSE
        ]
    }

    requests <- list()
    for (i in seq_len(nrow(outcomes))) {
        label <- sub("^Primary outcome:\\s*", "", outcomes$outcome[[i]])
        label <- sub(" of arrival at the emergency department", "", label, fixed = TRUE)
        label <- sub(" after arrival at the emergency department", "", label, fixed = TRUE)
        label <- sub(" during initial patient resuscitation", "", label, fixed = TRUE)
        section <- if (grepl("In-hospital mortality", outcomes$outcome[[i]])) {
            "Primary outcome"
        } else if (identical(outcomes$design_component[[i]], "Nested staircase")) {
            "Secondary outcomes (nested staircase design)"
        } else {
            "Secondary outcomes (main stepped-wedge design)"
        }
        requests[[length(requests) + 1L]] <- list(
            field = paste0("outcome_observed_", i),
            label = paste0(label, ", n observed"),
            source = "external",
            summary = "dichotomous",
            section = section
        )
        requests[[length(requests) + 1L]] <- list(
            field = paste0("outcome_missing_", i),
            label = paste0(label, ", n missing"),
            source = "external",
            summary = "dichotomous",
            section = section
        )
    }

    build_interim_count_shell_table(
        requests = requests,
        groups = groups,
        include.overall = include.overall,
        label.header = "**Outcome completeness**",
        label.width = 0.42
    )
}

#' ATLS training delivery counts for the interim report
create_interim_training_table <- function(
    groups = interim_analysis_report_groups(),
    include.overall = TRUE) {
    requests <- list(
        list(
            field = "physicians_trained",
            label = "Physicians trained in ATLS®",
            source = "external",
            summary = "dichotomous",
            section = "Training"
        ),
        list(
            field = "hospitals_with_training_complete",
            label = "Hospitals with scheduled ATLS® training completed",
            source = "external",
            summary = "dichotomous",
            section = "Training"
        )
    )
    build_interim_count_shell_table(
        requests = requests,
        groups = groups,
        include.overall = include.overall,
        label.width = 0.46
    )
}

#' Protocol deviations for the interim report
create_interim_protocol_deviations_table <- function(
    groups = interim_analysis_report_groups(),
    include.overall = TRUE) {
    requests <- list(
        list(
            field = "deviations_total",
            label = "Protocol deviations",
            source = "external",
            summary = "dichotomous",
            section = "Protocol deviations"
        ),
        list(
            field = "serious_breaches",
            label = "Serious breaches",
            source = "external",
            summary = "dichotomous",
            section = "Protocol deviations"
        )
    )
    build_interim_count_shell_table(
        requests = requests,
        groups = groups,
        include.overall = include.overall,
        label.width = 0.40
    )
}

#' Safety events for the interim report
create_interim_safety_events_table <- function(
    groups = interim_analysis_report_groups(),
    include.overall = TRUE) {
    build_interim_count_shell_table(
        requests = safety_events_shell_requests(),
        groups = groups,
        include.overall = include.overall,
        label.header = "**Safety event**",
        label.width = 0.46
    )
}
