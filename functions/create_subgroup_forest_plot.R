#' Create a shell forest plot for primary-outcome subgroup analyses
#'
#' Builds a template forest plot showing the layout for subgroup-specific
#' intervention effects on in-hospital mortality within 30 days. Grouping
#' variable names appear as bold headers with indented level labels underneath
#' in a dedicated label column. By default estimates and confidence intervals
#' are filled with reproducible simulated values; set `use.simulated.data =
#' FALSE` for null-effect placeholders.
#'
#' @param page.width.mm Numeric. Width of the saved figure in millimetres.
#' @param row.height.mm Numeric. Vertical space allocated to each row.
#' @param text.size Numeric. Axis text size in points.
#' @param label.width Numeric. Fraction of total figure width for the label
#'     column.
#' @param return.figure Logical. If TRUE, return the combined patchwork object.
#' @param save Logical. If TRUE, save the figure to disk.
#' @param device Character. Device passed to `ggplot2::ggsave()`.
#' @param use.simulated.data Logical. If TRUE (default), draw simulated
#'     subgroup estimates. If FALSE, draw null-effect placeholders.
#' @param seed Integer. RNG seed for simulated estimates.
#' @return A patchwork object if `return.figure` is TRUE; otherwise the saved
#'     file name.
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_subgroup_forest_plot(save = FALSE)
#' }
create_subgroup_forest_plot <- function(page.width.mm = 174,
                                        row.height.mm = 5.5,
                                        text.size = 8,
                                        label.width = 0.42,
                                        return.figure = TRUE,
                                        save = TRUE,
                                        device = "png",
                                        use.simulated.data = TRUE,
                                        seed = shell_simulated_data_seed()) {
    library(ggplot2)
    library(patchwork)

    assertthat::assert_that(is.numeric(page.width.mm) && length(page.width.mm) == 1)
    assertthat::assert_that(is.numeric(row.height.mm) && length(row.height.mm) == 1)
    assertthat::assert_that(is.numeric(text.size) && length(text.size) == 1 && text.size >= 8)
    assertthat::assert_that(is.numeric(label.width) && length(label.width) == 1)
    assertthat::assert_that(label.width > 0.2 && label.width < 0.6)
    assertthat::assert_that(is.logical(return.figure) && length(return.figure) == 1)
    assertthat::assert_that(is.logical(save) && length(save) == 1)
    assertthat::assert_that(is.character(device) && length(device) == 1)
    assertthat::assert_that(is.logical(use.simulated.data) && length(use.simulated.data) == 1)
    assertthat::assert_that(is.numeric(seed) && length(seed) == 1)

    measures <- c("Odds ratio", "Absolute risk difference")
    measure.labels <- c(
        "Odds ratio" = "Odds ratio (95% CI)",
        "Absolute risk difference" = "Absolute risk difference (95% CI)"
    )
    layout <- build_subgroup_forest_plot_rows()
    y.limits <- range(layout$y)
    plot.width <- 1 - label.width
    panel.width <- plot.width / 2
    shared.margin <- margin(2, 8, 5.5, 5.5)

    build_measure_data <- function(measure) {
        measure.rows <- layout
        measure.rows$measure <- measure
        level.index <- which(measure.rows$row_type == "level")
        n.levels <- length(level.index)
        if (isTRUE(use.simulated.data) && n.levels > 0L) {
            set.seed(as.integer(seed) + if (measure == "Odds ratio") 0L else 100L)
            if (measure == "Odds ratio") {
                log.est <- stats::rnorm(n.levels, mean = -0.12, sd = 0.2)
                se <- abs(stats::rnorm(n.levels, mean = 0.14, sd = 0.04)) + 0.05
                estimate <- exp(log.est)
                ci.low <- exp(log.est - 1.96 * se)
                ci.high <- exp(log.est + 1.96 * se)
            } else {
                estimate <- stats::rnorm(n.levels, mean = -0.02, sd = 0.03)
                se <- abs(stats::rnorm(n.levels, mean = 0.018, sd = 0.005)) + 0.008
                ci.low <- estimate - 1.96 * se
                ci.high <- estimate + 1.96 * se
            }
            measure.rows$estimate <- NA_real_
            measure.rows$ci_low <- NA_real_
            measure.rows$ci_high <- NA_real_
            measure.rows$estimate[level.index] <- estimate
            measure.rows$ci_low[level.index] <- ci.low
            measure.rows$ci_high[level.index] <- ci.high
        } else {
            measure.rows$estimate <- ifelse(measure.rows$row_type == "level",
                ifelse(measure == "Odds ratio", 1, 0),
                NA_real_
            )
            measure.rows$ci_low <- ifelse(measure.rows$row_type == "level",
                ifelse(measure == "Odds ratio", 0.85, -0.05),
                NA_real_
            )
            measure.rows$ci_high <- ifelse(measure.rows$row_type == "level",
                ifelse(measure == "Odds ratio", 1.15, 0.05),
                NA_real_
            )
        }
        measure.rows
    }

    build_panel_title <- function(measure) {
        ggplot() +
            labs(title = unname(measure.labels[[measure]])) +
            theme_void() +
            theme(
                plot.title = element_text(
                    face = "bold",
                    size = text.size,
                    hjust = 0.5,
                    margin = margin(0, 0, 2, 0)
                ),
                plot.margin = margin(0, 8, 0, 5.5)
            )
    }

    build_measure_panel <- function(measure) {
        panel.data <- build_measure_data(measure)
        level.data <- panel.data[panel.data$row_type == "level", , drop = FALSE]
        xintercept <- if (measure == "Odds ratio") 1 else 0

        ggplot(panel.data, aes(y = y)) +
            geom_vline(
                xintercept = xintercept,
                color = "grey75",
                linewidth = 0.4
            ) +
            geom_errorbar(
                data = level.data,
                inherit.aes = FALSE,
                aes(x = estimate, y = y, xmin = ci_low, xmax = ci_high),
                orientation = "y",
                width = 0.22,
                color = "grey55",
                linewidth = 0.45
            ) +
            geom_point(
                data = level.data,
                inherit.aes = FALSE,
                aes(x = estimate, y = y),
                size = 1.8,
                color = "grey35"
            ) +
            scale_y_continuous(limits = y.limits, expand = expansion(mult = c(0.02, 0.02))) +
            scale_x_continuous(expand = expansion(mult = c(0.04, 0.08))) +
            labs(x = NULL, y = NULL) +
            theme_bw(base_size = text.size) +
            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x = element_text(size = text.size, color = "black"),
                plot.margin = shared.margin
            )
    }

    label.panel <- ggplot(layout, aes(y = y)) +
        geom_text(
            aes(x = 0, label = label, fontface = fontface),
            hjust = 0,
            size = text.size / .pt,
            color = "black"
        ) +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        scale_y_continuous(limits = y.limits, expand = expansion(mult = c(0.02, 0.02))) +
        labs(x = NULL, y = NULL) +
        theme_void() +
        theme(plot.margin = shared.margin)

    forest.plot <- (
        plot_spacer() + build_panel_title(measures[[1]]) + build_panel_title(measures[[2]])
    ) / (
        label.panel + build_measure_panel(measures[[1]]) + build_measure_panel(measures[[2]])
    ) +
        plot_layout(
            widths = c(label.width, panel.width, panel.width),
            heights = c(0.06, 1)
        )

    page.height.mm <- max(110, nrow(layout) * row.height.mm + 30)

    if (save) {
        file.name <- paste0("subgroup-forest-plot.", device)
        ggplot2::ggsave(
            filename = file.name,
            plot = forest.plot,
            width = page.width.mm,
            height = page.height.mm,
            units = "mm",
            limitsize = FALSE
        )
    }

    if (return.figure) {
        return(forest.plot)
    }
    file.name
}
