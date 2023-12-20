create_patient_procedure_table <- function() {
    ## Load packages
    library(gt)

    ## Define borrowed functions
    assert_that <- assertthat::assert_that

    ## Define utility function to create follow up schedule
    create_follow_up_schedule <- function(screening = FALSE,
                                          follow.up.daily = FALSE,
                                          follow.up.24.hours = FALSE,
                                          follow.up.discharge = FALSE,
                                          follow.up.7.days.discharge = FALSE,
                                          follow.up.30.days = FALSE,
                                          follow.up.90.days = FALSE,
                                          return.only.labels = FALSE) {
        ## Check arguments
        assert_that(is.logical(screening))
        assert_that(is.logical(follow.up.daily))
        assert_that(is.logical(follow.up.24.hours))
        assert_that(is.logical(follow.up.discharge))
        assert_that(is.logical(follow.up.7.days.discharge))
        assert_that(is.logical(follow.up.30.days))
        assert_that(is.logical(follow.up.90.days))
        assert_that(is.logical(return.only.labels))

        ## Create schedule
        schedule <- c(
            screening = ifelse(screening, "√", ""),
            follow.up.daily = ifelse(follow.up.daily, "√", ""),
            follow.up.24.hours = ifelse(follow.up.24.hours, "√", ""),
            follow.up.discharge = ifelse(follow.up.discharge, "√", ""),
            follow.up.7.days.discharge = ifelse(follow.up.7.days.discharge, "√", ""),
            follow.up.30.days = ifelse(follow.up.30.days, "√", ""),
            follow.up.90.days = ifelse(follow.up.90.days, "√", "")
        )

        ## Define labels
        labels <- c(
            screening = "Screening",
            follow.up.daily = "Daily",
            follow.up.24.hours = "24 hours",
            follow.up.discharge = "Discharge",
            follow.up.7.days.discharge = "Within 7 days of discharge",
            follow.up.30.days = "30 days",
            follow.up.90.days = "90 days"
        )

        ## Stop if the names of the schedule and labels do not match
        if (!identical(names(schedule), names(labels))) {
            stop("The names of the schedule and labels do not match.")
        }

        ## Return schedule or labels
        if (return.only.labels) {
            return(labels)
        } else {
            return(schedule)
        }
    }

    ## Define follow up data
    procedure.list <- list(
        "Eligibility criteria" = create_follow_up_schedule(screening = TRUE),
        "Study information" = create_follow_up_schedule(screening = TRUE),
        "Informed consent for follow up" = create_follow_up_schedule(screening = TRUE),
        "Baseline data collection" = create_follow_up_schedule(screening = TRUE),
        "Injury data collection" = create_follow_up_schedule(
            screening = TRUE,
            follow.up.daily = TRUE
        ),
        "Assessment of safety events" = create_follow_up_schedule(
            follow.up.daily = TRUE
        ),
        "Mortality data collection" = create_follow_up_schedule(
            screening = TRUE,
            follow.up.daily = TRUE,
            follow.up.24.hours = TRUE,
            follow.up.discharge = TRUE,
            follow.up.30.days = TRUE,
            follow.up.90.days = TRUE
        ),
        "EQ-5D/WHODAS" = create_follow_up_schedule(
            follow.up.7.days.discharge = TRUE,
            follow.up.30.days = TRUE,
            follow.up.90.days = TRUE
        ),
        "Return to work" = create_follow_up_schedule(
            follow.up.30.days = TRUE,
            follow.up.90.days = TRUE
        ),
        "End of Trial" = create_follow_up_schedule(follow.up.90.days = TRUE)
    )

    procedure.data <- as.data.frame(do.call(rbind, procedure.list))
    procedure.data <- rbind(data.frame(Procedure = rownames(procedure.data), procedure.data))
    names(procedure.data) <- c("Procedure", create_follow_up_schedule(return.only.labels = TRUE))

    ## Create table
    procedure.table <- gt(procedure.data) |>
        tab_spanner(
            label = "Follow up",
            columns = c("Daily", "24 hours", "Discharge", "Within 7 days of discharge", "30 days", "90 days")
        ) |>
        tab_footnote(
            footnote = c(
                "Clinical research coordinators will inform patient participants about the study, including that they are free to withdraw their data from the study at any time, and approach them for informed consent for follow up either in person or telephonically."
            ),
            locations = cells_body(
                columns = "Procedure",
                rows = procedure.data$Procedure %in% c("Study information", "Informed consent for follow up")
            )
        ) |>
        tab_footnote(
            footnote = c(
                "Mortality data will be collected from the hospital records and from the patient participants or their caregivers by telephone."
            ),
            locations = cells_body(
                columns = "Procedure",
                rows = procedure.data$Procedure %in% "Mortality data collection"
            )
        )

    ## Return follow up table and notes
    return(procedure.table)
}
