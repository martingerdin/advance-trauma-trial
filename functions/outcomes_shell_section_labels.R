#' Shared section headers for outcomes descriptive and analysis-results shells
#'
#' Keep wording identical across shell tables so layout stays consistent in the
#' statistical analysis plan.
#'
#' @return A named list of section header strings.
outcomes_shell_section_labels <- function() {
    list(
        primary = "Primary outcome",
        main_stepped_wedge = "Secondary outcomes (main stepped-wedge design)",
        nested_resuscitation = paste(
            "Secondary outcomes during initial resuscitation",
            "(nested staircase design)"
        ),
        nested_seven_days = paste(
            "Secondary outcomes within seven days of discharge",
            "(nested staircase design)"
        ),
        nested_30_days = paste(
            "Secondary outcomes at 30 days after arrival at the emergency department",
            "(nested staircase design)"
        ),
        nested_three_months = paste(
            "Secondary outcomes at three months after arrival at the emergency department",
            "(nested staircase design)"
        )
    )
}

#' Map outcome grouping to a shared outcomes-shell section header
#'
#' @param outcome_type Character. `"Primary"` or `"Secondary"`.
#' @param design Character. Design component label.
#' @param timing Character. Timing label used for nested staircase sections.
#' @return A single section header string.
outcomes_shell_section_for <- function(outcome_type, design, timing) {
    sections <- outcomes_shell_section_labels()
    if (identical(outcome_type, "Primary")) {
        return(sections$primary)
    }
    if (identical(design, "Main stepped-wedge")) {
        return(sections$main_stepped_wedge)
    }
    if (identical(timing, "During initial resuscitation")) {
        return(sections$nested_resuscitation)
    }
    if (identical(timing, "Within seven days of discharge")) {
        return(sections$nested_seven_days)
    }
    if (identical(timing, "At 30 days")) {
        return(sections$nested_30_days)
    }
    if (identical(timing, "At three months")) {
        return(sections$nested_three_months)
    }
    paste("Secondary outcomes", design)
}

#' Ordered section headers for outcomes shell tables
outcomes_shell_section_order <- function() {
    unname(unlist(outcomes_shell_section_labels(), use.names = FALSE))
}
