#' Create a shell flowchart of the actual trial roll-out
#'
#' Reads per-cluster phase dates from `tables/cluster-rollout.csv` and training
#' courses from `tables/cluster-training.csv` (one row per course, so a cluster
#' may appear more than once). Light gray bars show the planned patient-inclusion
#' window; coloured bars show the observed standard-care, transition, and
#' intervention periods (trial-design colours). Each training course is marked
#' with a diamond sitting on top of that cluster's bar (at the course midpoint).
#' Rows are spaced so markers do not spill into neighbouring bars. Placeholder
#' dates mirror the intended design schedule from a February 2025 origin.
#'
#' Roll-out columns: `cluster`, `batch`, `sequence`, `site_id` (e.g. `B1S1`),
#' `site_name` (optional), `planned_inclusion_start`, `planned_inclusion_end`,
#' `standard_care_start`, `standard_care_end`, `transition_start`,
#' `transition_end`, `intervention_start`, `intervention_end`.
#'
#' Training columns: `site_id`, `cluster`, `training_start`, `training_end`.
#' Date columns are ISO `YYYY-MM-DD`.
#'
#' @param path Character. Path to the roll-out CSV when `data` is NULL.
#' @param training.path Character. Path to the training CSV when `training` is
#'     NULL.
#' @param data A data frame or NULL. If NULL, read `path`.
#' @param training A data frame or NULL. If NULL, read `training.path`. May be
#'     empty (zero rows) if no courses are recorded yet.
#' @param return.figure Logical. If TRUE, return the ggplot object.
#' @param save Logical. If TRUE, save the figure to disk.
#' @param device Character. Device passed to `ggplot2::ggsave()`.
#' @return A ggplot if `return.figure` is TRUE; otherwise the saved file name
#'     when `save` is TRUE.
#'
#' @examples
#' noacsr::source_all_functions()
#' \dontrun{
#' create_trial_rollout_flowchart(save = FALSE)
#' }
create_trial_rollout_flowchart <- function(path = "tables/cluster-rollout.csv",
                                           training.path = "tables/cluster-training.csv",
                                           data = NULL,
                                           training = NULL,
                                           return.figure = TRUE,
                                           save = TRUE,
                                           device = "png") {
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.null(training) || is.data.frame(training))
    assertthat::assert_that(is.character(path) && length(path) == 1)
    assertthat::assert_that(is.character(training.path) && length(training.path) == 1)
    assertthat::assert_that(is.logical(return.figure) && length(return.figure) == 1)
    assertthat::assert_that(is.logical(save) && length(save) == 1)
    assertthat::assert_that(is.character(device) && length(device) == 1)

    if (is.null(data)) {
        data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
    if (is.null(training)) {
        training <- utils::read.csv(
            training.path,
            stringsAsFactors = FALSE,
            check.names = FALSE
        )
    }

    required.columns <- c(
        "cluster", "batch", "sequence", "site_id",
        "planned_inclusion_start", "planned_inclusion_end",
        "standard_care_start", "standard_care_end",
        "transition_start", "transition_end",
        "intervention_start", "intervention_end"
    )
    missing.columns <- setdiff(required.columns, names(data))
    assertthat::assert_that(
        length(missing.columns) == 0,
        msg = paste("Missing roll-out columns:", paste(missing.columns, collapse = ", "))
    )

    training.columns <- c("site_id", "cluster", "training_start", "training_end")
    missing.training <- setdiff(training.columns, names(training))
    assertthat::assert_that(
        length(missing.training) == 0,
        msg = paste(
            "Missing training columns:",
            paste(missing.training, collapse = ", ")
        )
    )

    date.columns <- c(
        "planned_inclusion_start", "planned_inclusion_end",
        "standard_care_start", "standard_care_end",
        "transition_start", "transition_end",
        "intervention_start", "intervention_end"
    )
    for (column in date.columns) {
        data[[column]] <- as.Date(data[[column]])
    }
    assertthat::assert_that(!anyNA(data[date.columns]))
    assertthat::assert_that(
        all(data$planned_inclusion_start <= data$planned_inclusion_end)
    )
    assertthat::assert_that(all(data$standard_care_start <= data$standard_care_end))
    assertthat::assert_that(all(data$transition_start <= data$transition_end))
    assertthat::assert_that(all(data$intervention_start <= data$intervention_end))

    if (nrow(training) > 0) {
        training$training_start <- as.Date(training$training_start)
        training$training_end <- as.Date(training$training_end)
        assertthat::assert_that(!anyNA(training[c("training_start", "training_end")]))
        assertthat::assert_that(all(training$training_start <= training$training_end))
        assertthat::assert_that(all(training$cluster %in% data$cluster))
    }

    color.palette <- unname(colors())

    planned.inclusion <- data.frame(
        cluster = data$cluster,
        start = data$planned_inclusion_start,
        end = data$planned_inclusion_end,
        phase = "Planned patient inclusion",
        stringsAsFactors = FALSE
    )

    phases <- rbind(
        data.frame(
            cluster = data$cluster,
            start = data$standard_care_start,
            end = data$standard_care_end,
            phase = "Standard care",
            stringsAsFactors = FALSE
        ),
        data.frame(
            cluster = data$cluster,
            start = data$transition_start,
            end = data$transition_end,
            phase = "Planned transition period",
            stringsAsFactors = FALSE
        ),
        data.frame(
            cluster = data$cluster,
            start = data$intervention_start,
            end = data$intervention_end,
            phase = "Intervention",
            stringsAsFactors = FALSE
        )
    )

    ## Space cluster rows farther apart so training diamonds can sit on top of
    ## each bar without spilling into the row above.
    row.spacing <- 1.4
    bar.half.height <- 0.28
    ## Taller than actual phase bars so planned start/end remains visible when
    ## the coloured periods fully cover the planned window horizontally.
    planned.half.height <- 0.48
    training.y.offset <- 0.52
    planned.inclusion$y <- planned.inclusion$cluster * row.spacing
    phases$y <- phases$cluster * row.spacing

    ## One diamond per training course, sitting on top of that cluster's bar.
    if (nrow(training) > 0) {
        training.marks <- data.frame(
            cluster = training$cluster,
            date = training$training_start +
                as.numeric(training$training_end - training$training_start) / 2,
            y = training$cluster * row.spacing + training.y.offset,
            marker = "Actual training",
            stringsAsFactors = FALSE
        )
    } else {
        training.marks <- data.frame(
            cluster = integer(),
            date = as.Date(character()),
            y = numeric(),
            marker = character(),
            stringsAsFactors = FALSE
        )
    }

    ## Match the trial-design flowchart gap (~0.1 study month). On a multi-year
    ## calendar axis that needs several days each side so the background shows
    ## between adjacent phase bars.
    phase.gap.days <- 3

    cluster.levels <- sort(unique(data$cluster))
    y.breaks <- cluster.levels * row.spacing
    batch.levels <- sort(unique(data$batch))
    batch.breaks <- vapply(
        batch.levels,
        function(batch) mean(data$cluster[data$batch == batch] * row.spacing),
        numeric(1)
    )

    library(ggplot2)
    trial.rollout.figure <- ggplot() +
        geom_rect(
            data = planned.inclusion,
            aes(
                xmin = start, xmax = end,
                ymin = y - planned.half.height, ymax = y + planned.half.height,
                fill = phase
            )
        ) +
        geom_rect(
            data = phases,
            aes(
                xmin = start + phase.gap.days, xmax = end - phase.gap.days,
                ymin = y - bar.half.height, ymax = y + bar.half.height,
                fill = phase
            ),
            alpha = 0.9
        ) +
        geom_rect(
            data = phases,
            aes(
                xmin = start + phase.gap.days, xmax = end - phase.gap.days,
                ymin = y - bar.half.height, ymax = y + bar.half.height
            ),
            fill = NA,
            color = "black",
            linewidth = 0.3
        )

    if (nrow(training.marks) > 0) {
        trial.rollout.figure <- trial.rollout.figure +
            geom_point(
                data = training.marks,
                aes(x = date, y = y, shape = marker),
                colour = "black",
                fill = "white",
                size = 1.6,
                stroke = 0.5
            )
    }

    trial.rollout.figure <- trial.rollout.figure +
        scale_fill_manual(
            values = c(
                "Planned patient inclusion" = "#d0d0d0",
                "Standard care" = color.palette[1],
                "Planned transition period" = color.palette[2],
                "Intervention" = color.palette[3]
            ),
            breaks = c(
                "Planned patient inclusion",
                "Standard care",
                "Planned transition period",
                "Intervention"
            )
        ) +
        scale_shape_manual(values = c("Actual training" = 23)) +
        scale_y_continuous(
            breaks = y.breaks,
            labels = cluster.levels,
            limits = c(min(y.breaks) - 0.55, max(y.breaks) + 0.65),
            guide = guide_axis(n.dodge = 2),
            sec.axis = sec_axis(
                trans = ~.,
                breaks = batch.breaks,
                labels = batch.levels,
                name = "Batch"
            )
        ) +
        scale_x_date(
            date_breaks = "6 months",
            date_labels = "%b %Y",
            expand = expansion(mult = c(0.01, 0.02))
        ) +
        theme_bw() +
        theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.box.just = "left",
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
            legend.spacing.x = unit(0.4, "cm"),
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        guides(
            fill = guide_legend(order = 1, nrow = 1, title.position = "top"),
            shape = guide_legend(
                order = 2,
                nrow = 1,
                title.position = "top",
                override.aes = list(
                    size = 2.4,
                    colour = "black",
                    fill = "white",
                    stroke = 0.5
                )
            )
        ) +
        labs(
            x = "Calendar date",
            y = "Cluster",
            fill = "Legend",
            shape = ""
        )

    if (save) {
        file.name <- paste0("trial-rollout-figure.", device)
        ggsave(
            file.name,
            trial.rollout.figure,
            width = 15,
            height = 14,
            units = "cm"
        )
    }

    if (return.figure) {
        return(trial.rollout.figure)
    }
    if (save) {
        return(file.name)
    }
    invisible(NULL)
}
