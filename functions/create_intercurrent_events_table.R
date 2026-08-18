#' Display label for an intercurrent event in the frequency shell
#'
#' Maps the short tokens in `tables/outcomes-summary.json` to the wording used
#' in the Intercurrent events section. Death is qualified by the outcome's
#' assessment window where that window is a follow-up time point.
#'
#' @param event Character. Token from `potential_intercurrent_events`.
#' @param timing Character. Timing label from
#'     [classify_outcomes_analysis_results_grouping()].
#' @return A single display label.
ice_event_display_label <- function(event, timing) {
    key <- tolower(trimws(event))
    if (identical(key, "death")) {
        if (identical(timing, "At 30 days")) {
            return("Death before 30 days")
        }
        if (identical(timing, "At three months")) {
            return("Death before three months")
        }
        if (identical(timing, "Within seven days of discharge")) {
            return("Death before assessment")
        }
        return("Death")
    }
    if (identical(key, "transfer")) {
        return("Transfer to another hospital")
    }
    if (identical(key, "withdrawal")) {
        return("Withdrawal of consent for non-routine data collection")
    }
    if (identical(key, "loss to follow-up")) {
        return("Loss to follow-up")
    }
    if (identical(key, "incomplete observation")) {
        return("Incomplete observation of resuscitation")
    }
    trimws(event)
}

#' Intercurrent event rows for the frequency shell table
#'
#' Section headers and outcome labels match
#' [create_outcomes_analysis_results_table()]. Events come from
#' `potential_intercurrent_events` in the outcomes summary.
#'
#' @param path Character. Path to `tables/outcomes-summary.json`.
#' @return A list of variable request lists for
#'     [build_patient_characteristics_shell_table()].
intercurrent_events_shell_requests <- function(path = "tables/outcomes-summary.json") {
    data <- jsonlite::fromJSON(path)
    required.columns <- c(
        "outcome",
        "design_component",
        "potential_intercurrent_events"
    )
    missing.columns <- setdiff(required.columns, names(data))
    assertthat::assert_that(
        length(missing.columns) == 0,
        msg = paste("Missing columns:", paste(missing.columns, collapse = ", "))
    )

    section.order <- outcomes_shell_section_order()
    requests <- list()

    for (i in seq_len(nrow(data))) {
        grouping <- classify_outcomes_analysis_results_grouping(
            data$outcome[[i]],
            data$design_component[[i]]
        )
        outcome.label <- format_outcomes_analysis_results_label(data$outcome[[i]])
        if (identical(grouping$design, "Nested staircase") &&
            !identical(grouping$timing, "During initial resuscitation")) {
            outcome.label <- sub(
                "\\s+(within seven days of discharge|at 30 days|at three months)$",
                "",
                outcome.label
            )
        }
        top.section <- outcomes_shell_section_for(
            outcome_type = grouping$outcome_type,
            design = grouping$design,
            timing = grouping$timing
        )
        events <- trimws(unlist(
            strsplit(data$potential_intercurrent_events[[i]], ";", fixed = TRUE)
        ))
        events <- events[nzchar(events)]
        event.order <- c(
            "death",
            "transfer",
            "withdrawal",
            "loss to follow-up",
            "incomplete observation"
        )
        events <- events[order(match(tolower(events), event.order))]
        for (event in events) {
            requests[[length(requests) + 1L]] <- list(
                field = paste0("ice_", length(requests) + 1L),
                label = ice_event_display_label(event, grouping$timing),
                source = "external",
                summary = "dichotomous",
                section = c(top.section, outcome.label)
            )
        }
    }

    ranks <- match(
        vapply(requests, function(request) request$section[[1]], character(1)),
        section.order
    )
    ranks[is.na(ranks)] <- length(section.order) + 1L
    requests[order(ranks, seq_along(requests))]
}

#' Create a shell table of intercurrent event frequencies by outcome
#'
#' Builds a shell table illustrating how intercurrent events will be summarised
#' for each outcome before and after ATLS training is implemented in a cluster.
#' Body cells are blanked for the analysis plan. Section headers and outcome
#' labels match [create_outcomes_analysis_results_table()].
#'
#' @param groups Character. Stratum labels. Defaults to
#'     c("Before ATLS", "After ATLS").
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
#' @param path Character. Path to `tables/outcomes-summary.json`.
#' @return A `gtsummary` table object, or a `knitr_asis` longtable for
#'     PDF/LaTeX output.
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_intercurrent_events_table()
#' }
create_intercurrent_events_table <- function(
    groups = c("Before ATLS", "After ATLS"),
    include.overall = TRUE,
    path = "tables/outcomes-summary.json") {
    assertthat::assert_that(is.character(groups) && length(groups) >= 2)
    assertthat::assert_that(is.logical(include.overall) && length(include.overall) == 1)
    assertthat::assert_that(is.character(path) && length(path) == 1)

    build_patient_characteristics_shell_table(
        data = data.frame(),
        requests = intercurrent_events_shell_requests(path = path),
        groups = groups,
        include.overall = include.overall,
        longtable = TRUE,
        label.header = "**Intercurrent event**",
        label.width = 0.46,
        missing = "no",
        indent.labels = TRUE
    )
}
