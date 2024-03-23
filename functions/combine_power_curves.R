combine_power_curves <- function(file.names, device = "pdf") {
    ## Define borrowed functions
    assert_that <- assertthat::assert_that

    ## Check arguments
    assert_that(is.character(file.names))

    ## Create power curves
    power.curves <- lapply(file.names, create_power_curve)
    legend <- cowplot::get_legend(power.curves[[1]])
    power.curves <- lapply(power.curves, function(x) x + theme(legend.position = "none"))

    ## Combine power curves
    combined.plots <- cowplot::plot_grid(
        plotlist = power.curves,
        ncol = 2,
        labels = "AUTO"
    )
    combined.plots.with.legend <- cowplot::plot_grid(
        combined.plots,
        legend,
        ncol = 2,
        rel_widths = c(2, 1)
    )

    ## Save combined power curves
    cowplot::save_plot(paste0("combined-power-curves.", device),
        combined.plots.with.legend,
        base_width = 18,
        base_height = 8,
        units = "cm",
        dpi = 300
    )
}
