create_patient_procedure_table <- function() {
    # Load packages
    library(gt)

    # Define borrowed functions
    assert_that <- assertthat::assert_that

    # Define utility function to create follow up schedule
    create_follow_up_schedule <- function(screening = FALSE,
                                          consenting = FALSE,
                                          initial.assessment = FALSE,
                                          in.hospital.care = FALSE,
                                          follow.up.7.days.discharge = FALSE,
                                          follow.up.30.days = FALSE,
                                          follow.up.90.days = FALSE,
                                          return.only.labels = FALSE) {
        # Check arguments
        assert_that(is.logical(screening))
        assert_that(is.logical(consenting))
        assert_that(is.logical(initial.assessment))
        assert_that(is.logical(in.hospital.care))
        assert_that(is.logical(follow.up.7.days.discharge))
        assert_that(is.logical(follow.up.30.days))
        assert_that(is.logical(follow.up.90.days))
        assert_that(is.logical(return.only.labels))

        # Create schedule
        schedule <- c(
            screening = ifelse(screening, "√", ""),
            consenting = ifelse(consenting, "√", ""),
            initial.assessment = ifelse(initial.assessment, "√", ""),
            in.hospital.care = ifelse(in.hospital.care, "√", ""),
            follow.up.7.days.discharge = ifelse(follow.up.7.days.discharge, "√", ""),
            follow.up.30.days = ifelse(follow.up.30.days, "√", ""),
            follow.up.90.days = ifelse(follow.up.90.days, "√", "")
        )

        # Define labels
        labels <- c(
            screening = "Screening",
            consenting = "Consenting",
            initial.assessment = "Initial assessment",
            in.hospital.care = "In-hospital care",
            follow.up.7.days.discharge = "Within 7 days of discharge",
            follow.up.30.days = "30 days",
            follow.up.90.days = "90 days"
        )

        # Stop if the names of the schedule and labels do not match
        if (!identical(names(schedule), names(labels))) {
            stop("The names of the schedule and labels do not match.")
        }

        # Return schedule or labels
        if (return.only.labels) {
            return(labels)
        } else {
            return(schedule)
        }
    }

    # Define follow up data
    procedure.list <- list(
        "Eligibility criteria" = create_follow_up_schedule(screening = TRUE),
        "Study information" = create_follow_up_schedule(consenting = TRUE),
        "Informed consent" = create_follow_up_schedule(consenting = TRUE),
        "Baseline data collection" = create_follow_up_schedule(initial.assessment = TRUE),
        "Prehospital data collection" = create_follow_up_schedule(initial.assessment = TRUE),
        "ATLS adherence" = create_follow_up_schedule(
            initial.assessment = TRUE
        ),
        "ED data collection" = create_follow_up_schedule(
            initial.assessment = TRUE
        ),
        "Hospital data collection" = create_follow_up_schedule(
            in.hospital.care = TRUE
        ),
        "Surgery data collection" = create_follow_up_schedule(
            in.hospital.care = TRUE
        ),
        "Imaging data collection" = create_follow_up_schedule(
            in.hospital.care = TRUE
        ),
        "Transfusion data collection" = create_follow_up_schedule(
            in.hospital.care = TRUE
        ),
        "Injury data collection" = create_follow_up_schedule(
            in.hospital.care = TRUE
        ),
        "Mortality data collection" = create_follow_up_schedule(
            in.hospital.care = TRUE,
            follow.up.7.days.discharge = TRUE,
            follow.up.30.days = TRUE,
            follow.up.90.days = TRUE
        ),
        "Assessment of safety events" = create_follow_up_schedule(
            in.hospital.care = TRUE
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
        "End of study" = create_follow_up_schedule(follow.up.90.days = TRUE)
    )

    procedure.data <- as.data.frame(do.call(rbind, procedure.list))
    procedure.data <- rbind(data.frame(Procedure = rownames(procedure.data), procedure.data))
    names(procedure.data) <- c("Procedure", create_follow_up_schedule(return.only.labels = TRUE))

    ## Create table
    procedure.table <- gt(procedure.data) |>
        tab_spanner(
            label = "Follow up",
            columns = c("Within 7 days of discharge", "30 days", "90 days")
        ) |>
        tab_footnote(
            footnote = c(
                "Clinical research coordinators will inform patient participants about the study, including that they are free to withdraw their data from the study at any time, and approach them for informed consent for collection of non-routinely recorded data in person or telephonically."
            ),
            locations = cells_body(
                columns = "Procedure",
                rows = procedure.data$Procedure %in% c("Study information", "Informed consent")
            )
        ) |>
        tab_footnote(
            footnote = c(
                "ATLS adherence will be assessed by observing the care provided to a random sample of patient participants."
            ),
            locations = cells_body(
                columns = "Procedure",
                rows = procedure.data$Procedure %in% c("ATLS adherence")
            )
        ) |>
        tab_footnote(
            footnote = c(
                "Emergency Department"
            ),
            locations = cells_body(
                columns = "Procedure",
                rows = procedure.data$Procedure %in% c("ED data collection")
            )
        ) |>
        tab_footnote(
            footnote = c(
                "Will be ascertained daily from when the patient participant arrive to hospital until they leave the hospital, are discharged or die."
            ),
            locations = cells_body(
                columns = "Procedure",
                rows = procedure.data$Procedure %in% c("Surgery data collection", "Imaging data collection", "Transfusion data collection", "Injury data collection", "Mortality data collection", "Assessment of safety events")
            )
        )

    # Return follow up table and notes
    return(procedure.table)
}
