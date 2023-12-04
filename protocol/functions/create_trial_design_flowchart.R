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
#' @param total.months Numeric. Total length of the batch in
#'     months. Must be a length 1 numeric value greater than
#'     0. Default is 8.
#' @param return.figure Logical. If TRUE the function returns the
#'     figure. Defaults. to TRUE.
#' @param save Logical. If TRUE the trial design figure is saved to
#'     disk. Defaults to TRUE.
create_trial_design_flowchart <- function(clusters = 60,
                                          sequences = 5,
                                          batches = 6,
                                          min.standard.care.months = 1,
                                          min.intervention.months = 1,
                                          batches.overlap.months = 0,
                                          transition.months = 2,
                                          transition.overlap.months = 1,
                                          total.months = 8,
                                          return.figure = TRUE,
                                          save = TRUE) {
    ## Load packages
    library(ggplot2)

    ## Check arguments
    assertthat::assert_that(is.numeric(clusters) && length(clusters) == 1 && clusters > 0)
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.numeric(batches) && length(batches) == 1 && batches > 0)
    assertthat::assert_that(is.numeric(batches.overlap.months) && length(batches.overlap.months) == 1 && batches.overlap.months >= 0)
    assertthat::assert_that(is.numeric(transition.months) && length(transition.months) == 1 && transition.months > 0)
    assertthat::assert_that(is.numeric(transition.overlap.months) && length(transition.overlap.months) == 1)

    ## Generate plot data
    plot.data <- get_trial_design_data(clusters = clusters,
                                       sequences = sequences,
                                       batches = batches,
                                       min.standard.care.months = min.standard.care.months,
                                       min.intervention.months = min.intervention.months, 
                                       batches.overlap.months = batches.overlap.months, 
                                       transition.months = transition.months,
                                       transition.overlap.months = transition.overlap.months,
                                       total.months = total.months)
    clusters.per.batch <- with(plot.data, clusters/batches)

    ## Create plot
    color.blind.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    trial.design.figure <- ggplot(plot.data, aes(y = cluster, yend = cluster, x = start, xend = end, color = phase)) +
        geom_segment(linewidth = 1) +
        scale_color_manual(values = color.blind.palette) +
        scale_y_continuous(breaks = seq(1, 60),
                           guide = guide_axis(n.dodge = 2),
                           sec.axis = sec_axis(trans = ~.,
                                               breaks = seq(clusters.per.batch/2,
                                                            by = clusters.per.batch,
                                                            length.out = batches),
                                               labels = 1:batches,
                                               name = "Batch")) +
        scale_x_continuous(breaks = seq(0, max(plot.data$end), 2)) +
        theme_bw() +
        labs(x = "Study month", y = "Cluster", color = "Phase")

    ## Save figure
    if (save) {
        file.name <- paste0("trial-design-figure-",
                            clusters, "-clusters-",
                            sequences, "-sequences-",
                            batches, "-batches-",
                            batches.overlap.months, "-batches-overlap-",
                            min.standard.care.months, "-min-standard-care-",
                            min.intervention.months, "-min-intervention-",
                            transition.months, "-transition-months-",
                            transition.overlap.months, "-transition-overlap.pdf")
        ggsave(file.name, trial.design.figure, width = 18, height = 9, units = "cm")
    }
    
    ## Return figure
    if (return.figure)
        return (trial.design.figure)

    ## Return file name
    if (!return.figure)
        return (file.name)
}
