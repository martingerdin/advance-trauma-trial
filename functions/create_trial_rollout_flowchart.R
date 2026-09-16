#' Create a shell flowchart of the actual trial roll-out
#'
#' Reads per-cluster calendar dates from `tables/cluster-rollout.csv` (or a
#' supplied data frame) and draws a stepped-wedge-style figure with the same
#' phase colours as the trial-design flowchart (standard care, transition,
#' intervention). ATLS® training dates are overlaid as points. The CSV is the
#' operational source of truth: fill in real dates (and optional site names) as
#' clusters progress; placeholder dates mirror the intended design schedule
#' from a February 2025 origin.
#'
#' Expected columns: `cluster`, `batch`, `sequence`, `site_id` (e.g. `B1S1`),
#' `site_name` (optional), `standard_care_start`, `standard_care_end`,
#' `transition_start`, `transition_end`, `intervention_start`,
#' `intervention_end`, `training_start`, `training_end`.
#' Date columns are ISO `YYYY-MM-DD`.
#'
#' @param path Character. Path to the roll-out CSV when `data` is NULL.
#' @param data A data frame or NULL. If NULL, read `path`.
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
                                           data = NULL,
                                           return.figure = TRUE,
                                           save = TRUE,
                                           device = "png") {
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.character(path) && length(path) == 1)
    assertthat::assert_that(is.logical(return.figure) && length(return.figure) == 1)
    assertthat::assert_that(is.logical(save) && length(save) == 1)
    assertthat::assert_that(is.character(device) && length(device) == 1)

    if (is.null(data)) {
        data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    }

    required.columns <- c(
        "cluster", "batch", "sequence", "site_id",
        "standard_care_start", "standard_care_end",
        "transition_start", "transition_end",
        "intervention_start", "intervention_end",
        "training_start", "training_end"
    )
    missing.columns <- setdiff(required.columns, names(data))
    assertthat::assert_that(
        length(missing.columns) == 0,
        msg = paste("Missing columns:", paste(missing.columns, collapse = ", "))
    )

    date.columns <- c(
        "standard_care_start", "standard_care_end",
        "transition_start", "transition_end",
        "intervention_start", "intervention_end",
        "training_start", "training_end"
    )
    for (column in date.columns) {
        data[[column]] <- as.Date(data[[column]])
    }
    assertthat::assert_that(!anyNA(data[date.columns]))
    assertthat::assert_that(all(data$standard_care_start <= data$standard_care_end))
    assertthat::assert_that(all(data$transition_start <= data$transition_end))
    assertthat::assert_that(all(data$intervention_start <= data$intervention_end))
    assertthat::assert_that(all(data$training_start <= data$training_end))

    clusters.n <- nrow(data)
    batches.n <- length(unique(data$batch))
    clusters.per.batch <- clusters.n / batches.n
    color.palette <- unname(colors())

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
    ## One point per training day (inclusive), overlaid on the phase bars.
    training.points <- do.call(rbind, lapply(seq_len(nrow(data)), function(i) {
        days <- seq(data$training_start[i], data$training_end[i], by = "day")
        data.frame(
            cluster = data$cluster[i],
            date = days,
            marker = "Actual training days",
            stringsAsFactors = FALSE
        )
    }))

    library(ggplot2)
    trial.rollout.figure <- ggplot() +
        geom_rect(
            data = phases,
            aes(
                xmin = start + 0.1, xmax = end - 0.1,
                ymin = cluster - 0.3, ymax = cluster + 0.3,
                fill = phase
            ),
            alpha = 0.8
        ) +
        geom_rect(
            data = phases,
            aes(
                xmin = start + 0.1, xmax = end - 0.1,
                ymin = cluster - 0.3, ymax = cluster + 0.3
            ),
            fill = NA,
            color = "black",
            linewidth = 0.3
        ) +
        geom_point(
            data = training.points,
            aes(x = date, y = cluster, shape = marker),
            color = "black",
            fill = "white",
            size = 1.6,
            stroke = 0.4
        ) +
        scale_fill_manual(
            values = c(
                "Standard care" = color.palette[1],
                "Planned transition period" = color.palette[2],
                "Intervention" = color.palette[3]
            ),
            breaks = c("Standard care", "Planned transition period", "Intervention")
        ) +
        scale_shape_manual(values = c("Actual training days" = 21)) +
        scale_y_continuous(
            breaks = sort(unique(data$cluster)),
            limits = c(0.5, max(data$cluster) + 0.5),
            guide = guide_axis(n.dodge = 2),
            sec.axis = sec_axis(
                trans = ~.,
                breaks = seq(
                    clusters.per.batch / 2,
                    by = clusters.per.batch,
                    length.out = batches.n
                ),
                labels = sort(unique(data$batch)),
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
            fill = guide_legend(order = 1, nrow = 1, title.position = "left"),
            shape = guide_legend(order = 2, nrow = 1, title.position = "left")
        ) +
        labs(
            x = "Calendar date",
            y = "Cluster",
            fill = "Legend",
            shape = NULL
        )

    if (save) {
        file.name <- paste0("trial-rollout-figure.", device)
        ggsave(
            file.name,
            trial.rollout.figure,
            width = 15,
            height = 9,
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
