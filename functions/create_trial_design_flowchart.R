#' Create trial design flowchart
#'
#' This function creates the trial design flowchart
#' @param clusters Numeric. Number of clusters in the trial. Must be a
#'     length 1 numeric value greater than 0. Default is 60.
#' @param sequences Numeric. Number of treatment sequences in the
#'     trial. Must be a length 1 numeric value greater than 0. Default
#'     is 5.
#' @param batches Numeric. Number of batches in the trial. Must be a
#'     length 1 numeric value greater than 0. Default is 6.
#' @param min.standard.care.months Numeric. Minimum number of months
#'     for the standard care phase. Must be a length 1 numeric value
#'     greater than or equal to 1. Default is 1.
#' @param min.intervention.months Numeric. Minimum number of months
#'     for the intervention phase. Must be a length 1 numeric value
#'     greater than or equal to 1. Default is 1.
#' @param batches.overlap.months Numeric. Number of months for overlap
#'     between batches. Must be a length 1 numeric value greater than
#'     or equal to 0. Default is 0.
#' @param transition.months Numeric. Number of months for the
#'     transition phase. Must be a length 1 numeric value greater than
#'     0. Default is 2.
#' @param transition.overlap.months Numeric. Number of months for
#'     overlap between the end of the standard care phase and the
#'     beginning of the transition phase. Must be a length 1 numeric
#'     value greater than or equal to 0. Default is 1.
#' @param start.month Numeric. The month to start the trial. Must be a
#'     length 1 numeric value greater than or equal to 0. Default is 0.
#' @param total.months Numeric. Total length of the batch in
#'     months. Must be a length 1 numeric value greater than
#'     0. Default is 8.
#' @param staircase.months Numeric. Number of months before and after
#'     the transition phase to include in the staircase design. Must be
#'     a length 1 numeric value greater than or equal to 0. Default is
#'     0.
#' @param return.figure Logical. If TRUE the function returns the
#'     figure. Defaults. to TRUE.
#' @param save Logical. If TRUE the trial design figure is saved to
#'     disk. Defaults to TRUE.
#' @param device Character. The device to save the figure to. Defaults to "pdf".
create_trial_design_flowchart <- function(clusters = 60,
                                          sequences = 5,
                                          batches = 6,
                                          min.standard.care.months = 1,
                                          min.intervention.months = 1,
                                          batches.overlap.months = 0,
                                          transition.months = 2,
                                          transition.overlap.months = 1,
                                          start.month = 0,
                                          total.months = 8,
                                          staircase.months = 0,
                                          return.figure = TRUE,
                                          save = TRUE,
                                          device = "pdf") {
    ## Load packages
    library(ggplot2)

    ## Check arguments
    assertthat::assert_that(is.numeric(clusters) && length(clusters) == 1 && clusters > 0)
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.numeric(batches) && length(batches) == 1 && batches > 0)
    assertthat::assert_that(is.numeric(batches.overlap.months) && length(batches.overlap.months) == 1 && batches.overlap.months >= 0)
    assertthat::assert_that(is.numeric(transition.months) && length(transition.months) == 1 && transition.months > 0)
    assertthat::assert_that(is.numeric(transition.overlap.months) && length(transition.overlap.months) == 1)
    assertthat::assert_that(is.numeric(start.month) && length(start.month) == 1 && start.month >= 0)
    assertthat::assert_that(is.numeric(staircase.months) && length(staircase.months) == 1 && staircase.months >= 0)

    ## Generate plot data
    plot.data <- get_trial_design_data(
        clusters = clusters,
        sequences = sequences,
        batches = batches,
        min.standard.care.months = min.standard.care.months,
        min.intervention.months = min.intervention.months,
        batches.overlap.months = batches.overlap.months,
        transition.months = transition.months,
        transition.overlap.months = transition.overlap.months,
        total.months = total.months,
        start.month = start.month,
        staircase.months = staircase.months
    )
    clusters.per.batch <- with(plot.data, clusters / batches)

    ## Create plot
    color.palette <- unname(colors())
    if (staircase.months > 0) {
        # Filter data to only show relevant phases in legend
        legend.data <- subset(plot.data, phase %in% c("Pre-transition staircase", "Transition", "Post-transition staircase"))

        # Create main plot with all data in gray except staircase periods
        trial.design.figure <- ggplot() +
            # Add gray segments for standard care and intervention
            geom_segment(
                data = subset(plot.data, phase %in% c("Standard care", "Intervention")),
                aes(y = cluster, yend = cluster, x = start, xend = end, color = "Main stepped-wedge patient inclusion period"),
                linewidth = 1
            ) +
            # Add colored segments for transition and staircase periods
            geom_segment(
                data = legend.data,
                aes(y = cluster, yend = cluster, x = start, xend = end, color = phase),
                linewidth = 1
            ) +
            scale_color_manual(
                values = c(
                    "Main stepped-wedge patient inclusion period" = "#999999",
                    "Pre-transition staircase" = color.palette[1],
                    "Transition" = color.palette[2],
                    "Post-transition staircase" = color.palette[3]
                ),
                breaks = c(
                    "Main stepped-wedge patient inclusion period",
                    "Pre-transition staircase",
                    "Transition",
                    "Post-transition staircase"
                )
            ) +
            scale_y_continuous(
                breaks = seq(1, 60),
                guide = guide_axis(n.dodge = 2),
                sec.axis = sec_axis(
                    trans = ~.,
                    breaks = seq(clusters.per.batch / 2,
                        by = clusters.per.batch,
                        length.out = batches
                    ),
                    labels = 1:batches,
                    name = "Batch"
                )
            ) +
            scale_x_continuous(breaks = seq(0, max(plot.data$end), 2)) +
            theme_bw() +
            theme(
                legend.position = "bottom",
                legend.box = "vertical",
                legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
                legend.spacing.y = unit(0.1, "cm")
            ) +
            guides(color = guide_legend(nrow = 2)) +
            labs(x = "Study month", y = "Cluster", color = "Phase")
    } else {
        trial.design.figure <- ggplot(plot.data, aes(y = cluster, yend = cluster, x = start, xend = end, color = phase)) +
            geom_segment(linewidth = 1) +
            scale_color_manual(
                values = c(
                    "Standard care" = color.palette[1],
                    "Transition" = color.palette[2],
                    "Intervention" = color.palette[3]
                ),
                breaks = c(
                    "Standard care",
                    "Transition",
                    "Intervention"
                )
            ) +
            scale_y_continuous(
                breaks = seq(1, 60),
                guide = guide_axis(n.dodge = 2),
                sec.axis = sec_axis(
                    trans = ~.,
                    breaks = seq(clusters.per.batch / 2,
                        by = clusters.per.batch,
                        length.out = batches
                    ),
                    labels = 1:batches,
                    name = "Batch"
                )
            ) +
            scale_x_continuous(breaks = seq(0, max(plot.data$end), 2)) +
            theme_bw() +
            theme(
                legend.position = "bottom",
                legend.box = "vertical",
                legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
                legend.spacing.y = unit(0.1, "cm")
            ) +
            guides(color = guide_legend(nrow = 2)) +
            labs(x = "Study month", y = "Cluster", color = "Phase")
    }

    ## Save figure
    if (save) {
        file.name <- paste0(
            "trial-design-figure-",
            clusters, "-clusters-",
            sequences, "-sequences-",
            batches, "-batches-",
            batches.overlap.months, "-batches-overlap-",
            min.standard.care.months, "-min-standard-care-",
            min.intervention.months, "-min-intervention-",
            transition.months, "-transition-months-",
            transition.overlap.months, "-transition-overlap.",
            staircase.months, "-staircase-months.",
            device
        )
        ggsave(file.name, trial.design.figure, width = 15, height = 9, units = "cm")
    }

    ## Return figure
    if (return.figure) {
        return(trial.design.figure)
    }

    ## Return file name
    if (!return.figure) {
        return(file.name)
    }
}
