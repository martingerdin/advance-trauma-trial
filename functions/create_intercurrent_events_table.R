#' Intercurrent event rows for the frequency shell table
#'
#' Grouped by outcome (or outcome group). Each event is counted only where it
#' is an intercurrent event for that outcome, and only if it occurs before
#' assessment of that outcome.
#'
#' @return A list of variable request lists for
#'     [build_patient_characteristics_shell_table()].
intercurrent_events_shell_requests <- function() {
    main <- "Main stepped-wedge"
    nested <- "Nested staircase"
    withdrawal <- "Withdrawal of consent for non-routine data collection"
    ltfu <- "Loss to follow-up"

    ice_request <- function(field, label, section) {
        list(
            field = field,
            label = label,
            source = "external",
            summary = "dichotomous",
            section = section
        )
    }

    list(
        ice_request(
            "los_icu_death",
            "Death before exit or ICU admission",
            c(main, "Length of stay and ICU")
        ),
        ice_request(
            "los_icu_transfer",
            "Transfer to another hospital",
            c(main, "Length of stay and ICU")
        ),
        ice_request(
            "primary_withdrawal",
            withdrawal,
            c(main, "In-hospital mortality within 30 days")
        ),
        ice_request(
            "primary_ltfu",
            ltfu,
            c(main, "In-hospital mortality within 30 days")
        ),
        ice_request(
            "acm_24h_withdrawal",
            withdrawal,
            c(main, "All-cause mortality within 24 hours")
        ),
        ice_request(
            "acm_24h_ltfu",
            ltfu,
            c(main, "All-cause mortality within 24 hours")
        ),
        ice_request(
            "acm_30d_withdrawal",
            withdrawal,
            c(main, "All-cause mortality within 30 days")
        ),
        ice_request(
            "acm_30d_ltfu",
            ltfu,
            c(main, "All-cause mortality within 30 days")
        ),
        ice_request(
            "acm_90d_withdrawal",
            withdrawal,
            c(main, "All-cause mortality within three months")
        ),
        ice_request(
            "acm_90d_ltfu",
            ltfu,
            c(main, "All-cause mortality within three months")
        ),
        ice_request(
            "rtw_30d_death",
            "Death before 30 days",
            c(main, "Return to work at 30 days")
        ),
        ice_request(
            "rtw_30d_withdrawal",
            withdrawal,
            c(main, "Return to work at 30 days")
        ),
        ice_request(
            "rtw_30d_ltfu",
            ltfu,
            c(main, "Return to work at 30 days")
        ),
        ice_request(
            "rtw_90d_death",
            "Death before three months",
            c(main, "Return to work at three months")
        ),
        ice_request(
            "rtw_90d_withdrawal",
            withdrawal,
            c(main, "Return to work at three months")
        ),
        ice_request(
            "rtw_90d_ltfu",
            ltfu,
            c(main, "Return to work at three months")
        ),
        ice_request(
            "qol_dis_7d_death",
            "Death before assessment",
            c(nested, "Quality of life and disability within seven days of discharge")
        ),
        ice_request(
            "qol_dis_7d_withdrawal",
            withdrawal,
            c(nested, "Quality of life and disability within seven days of discharge")
        ),
        ice_request(
            "qol_dis_7d_ltfu",
            ltfu,
            c(nested, "Quality of life and disability within seven days of discharge")
        ),
        ice_request(
            "qol_dis_30d_death",
            "Death before 30 days",
            c(nested, "Quality of life and disability at 30 days")
        ),
        ice_request(
            "qol_dis_30d_withdrawal",
            withdrawal,
            c(nested, "Quality of life and disability at 30 days")
        ),
        ice_request(
            "qol_dis_30d_ltfu",
            ltfu,
            c(nested, "Quality of life and disability at 30 days")
        ),
        ice_request(
            "qol_dis_90d_death",
            "Death before three months",
            c(nested, "Quality of life and disability at three months")
        ),
        ice_request(
            "qol_dis_90d_withdrawal",
            withdrawal,
            c(nested, "Quality of life and disability at three months")
        ),
        ice_request(
            "qol_dis_90d_ltfu",
            ltfu,
            c(nested, "Quality of life and disability at three months")
        ),
        ice_request(
            "adherence_incomplete",
            "Incomplete observation of resuscitation",
            c(nested, "Adherence to ATLS principles")
        )
    )
}

#' Create a shell table of intercurrent event frequencies by outcome
#'
#' Builds a shell table illustrating how intercurrent events will be summarised
#' for each outcome before and after ATLS training is implemented in a cluster.
#' Body cells are blanked for the analysis plan.
#'
#' @param groups Character. Stratum labels. Defaults to
#'     c("Before ATLS", "After ATLS").
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
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
    include.overall = TRUE) {
    assertthat::assert_that(is.character(groups) && length(groups) >= 2)
    assertthat::assert_that(is.logical(include.overall) && length(include.overall) == 1)

    build_patient_characteristics_shell_table(
        data = data.frame(),
        requests = intercurrent_events_shell_requests(),
        groups = groups,
        include.overall = include.overall,
        longtable = TRUE,
        label.header = "**Intercurrent event**",
        label.width = 0.46,
        missing = "no"
    )
}
