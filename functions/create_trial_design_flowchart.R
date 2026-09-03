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
#' @param current.month Numeric or NULL. If provided, a vertical line
#'     will be drawn at this month to indicate trial progress. Must be
#'     a length 1 numeric value greater than or equal to 0. Default is
#'     NULL (no progress line).
#' @param return.figure Logical. If TRUE the function returns the
#'     figure. Defaults. to TRUE.
#' @param save Logical. If TRUE the trial design figure is saved to
#'     disk. Defaults to TRUE.
#' @param device Character. The device to save the figure to. Defaults to "pdf".
#' @param export.path Character or NULL. If provided, write the plot data as
#'     JSON to this path for use in web graphics. Defaults to NULL.
#' @param return.data Logical. If TRUE the function returns the JSON-ready plot
#'     data instead of the ggplot. Defaults to FALSE.
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
                                          current.month = NULL,
                                          return.figure = TRUE,
                                          save = TRUE,
                                          device = "pdf",
                                          export.path = NULL,
                                          return.data = FALSE) {
    ## Check arguments
    assertthat::assert_that(is.numeric(clusters) && length(clusters) == 1 && clusters > 0)
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.numeric(batches) && length(batches) == 1 && batches > 0)
    assertthat::assert_that(is.numeric(batches.overlap.months) && length(batches.overlap.months) == 1 && batches.overlap.months >= 0)
    assertthat::assert_that(is.numeric(transition.months) && length(transition.months) == 1 && transition.months > 0)
    assertthat::assert_that(is.numeric(transition.overlap.months) && length(transition.overlap.months) == 1)
    assertthat::assert_that(is.numeric(start.month) && length(start.month) == 1 && start.month >= 0)
    assertthat::assert_that(is.numeric(staircase.months) && length(staircase.months) == 1 && staircase.months >= 0)
    if (!is.null(current.month)) {
        assertthat::assert_that(is.numeric(current.month) && length(current.month) == 1 && current.month >= 0)
    }
    assertthat::assert_that(is.logical(return.figure) && length(return.figure) == 1)
    assertthat::assert_that(is.logical(save) && length(save) == 1)
    assertthat::assert_that(is.logical(return.data) && length(return.data) == 1)
    if (!is.null(export.path)) {
        assertthat::assert_that(is.character(export.path) && length(export.path) == 1)
    }

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
    color.palette <- unname(colors())
    staircase <- staircase.months > 0

    segments <- plot.data
    rownames(segments) <- NULL
    segments$layer <- if (staircase) {
        ifelse(segments$phase %in% c("Standard care", "Intervention"), "background", "overlay")
    } else {
        "main"
    }
    if (staircase) {
        fill.colors <- list(
            "Main stepped-wedge patient inclusion period" = "#999999",
            "Pre-transition staircase" = color.palette[1],
            "Transition" = color.palette[2],
            "Post-transition staircase" = color.palette[3]
        )
        legend <- c(
            "Main stepped-wedge patient inclusion period",
            "Pre-transition staircase",
            "Transition",
            "Post-transition staircase"
        )
    } else {
        fill.colors <- list(
            "Standard care" = color.palette[1],
            "Transition" = color.palette[2],
            "Intervention" = color.palette[3]
        )
        legend <- c("Standard care", "Transition", "Intervention")
    }
    payload <- list(
        parameters = list(
            clusters = clusters,
            sequences = sequences,
            batches = batches,
            minStandardCareMonths = min.standard.care.months,
            minInterventionMonths = min.intervention.months,
            batchesOverlapMonths = batches.overlap.months,
            transitionMonths = transition.months,
            transitionOverlapMonths = transition.overlap.months,
            startMonth = start.month,
            totalMonths = total.months,
            staircaseMonths = staircase.months,
            currentMonth = if (is.null(current.month)) NA_real_ else current.month,
            clustersPerBatch = clusters.per.batch
        ),
        segments = segments,
        colors = fill.colors,
        legend = legend,
        labels = list(
            x = "Study month",
            y = "Cluster",
            fill = "Phase",
            batch = "Batch"
        ),
        geometry = list(
            xPadding = 0.1,
            barHalfHeight = 0.3,
            overlayHalfHeight = 0.4,
            xBreakStep = 2,
            yMin = 0.5,
            yMax = clusters + 0.5
        ),
        xMax = max(plot.data$end)
    )
    if (!is.null(export.path)) {
        dir.create(dirname(export.path), recursive = TRUE, showWarnings = FALSE)
        jsonlite::write_json(
            payload,
            path = export.path,
            pretty = TRUE,
            auto_unbox = TRUE,
            na = "null",
            digits = 8,
            dataframe = "rows",
            rownames = FALSE
        )
    }

    need.figure <- save || (return.figure && !return.data)
    if (!need.figure) {
        if (return.data) {
            return(payload)
        }
        if (!is.null(export.path)) {
            return(export.path)
        }
        return(invisible(NULL))
    }

    ## Create plot
    library(ggplot2)
    if (staircase.months > 0) {
        # Filter data to only show relevant phases in legend
        legend.data <- subset(plot.data, phase %in% c("Pre-transition staircase", "Transition", "Post-transition staircase"))

        # Create main plot with all data in gray except staircase periods
        trial.design.figure <- ggplot() +
            # Add gray rectangles for standard care and intervention
            geom_rect(
                data = subset(plot.data, phase %in% c("Standard care", "Intervention")),
                aes(xmin = start, xmax = end, ymin = cluster - 0.3, ymax = cluster + 0.3, fill = "Main stepped-wedge patient inclusion period")
            ) +
            # Add colored rectangles for transition and staircase periods
            geom_rect(
                data = legend.data,
                aes(xmin = start + 0.1, xmax = end - 0.1, ymin = cluster - 0.4, ymax = cluster + 0.4, fill = phase),
                alpha = 0.8
            ) +
            # Add black border to rectangles
             geom_rect(
                data = legend.data,
                aes(xmin = start + 0.1, xmax = end - 0.1, ymin = cluster - 0.4, ymax = cluster + 0.4),
                fill = NA,
                color = "black",
                linewidth = 0.3
            ) +
            scale_fill_manual(
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
                breaks = seq(1, clusters),
                limits = c(0.5, clusters + 0.5),
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
            (if (!is.null(current.month)) geom_vline(xintercept = current.month, linetype = "dashed", color = "red", linewidth = 1) else geom_blank()) +
            theme_bw() +
            theme(
                legend.position = "bottom",
                legend.box = "vertical",
                legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
                legend.spacing.y = unit(0.1, "cm")
            ) +
            guides(fill = guide_legend(nrow = 2)) +
            labs(x = "Study month", y = "Cluster", fill = "Phase")
    } else {
        trial.design.figure <- ggplot(plot.data, aes(xmin = start + 0.1, xmax = end - 0.1, ymin = cluster - 0.3, ymax = cluster + 0.3, fill = phase)) +
            geom_rect(alpha = 0.8) + 
            geom_rect(fill = NA, color = "black", linewidth = 0.3) +
            scale_fill_manual(
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
                breaks = seq(1, clusters),
                limits = c(0.5, clusters + 0.5),
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
            (if (!is.null(current.month)) geom_vline(xintercept = current.month, linetype = "dashed", color = "red", linewidth = 1) else geom_blank()) +
            theme_bw() +
            theme(
                legend.position = "bottom",
                legend.box = "vertical",
                legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
                legend.spacing.y = unit(0.1, "cm")
            ) +
            guides(fill = guide_legend(nrow = 1)) +
            labs(x = "Study month", y = "Cluster", fill = "Phase")
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

    ## Return
    if (return.data) {
        return(payload)
    }
    if (return.figure) {
        return(trial.design.figure)
    }
    if (save) {
        return(file.name)
    }
    if (!is.null(export.path)) {
        return(export.path)
    }
    invisible(NULL)
}
