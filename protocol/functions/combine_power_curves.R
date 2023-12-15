combine_power_curves <- function(file.names) {
    ## Define borrowed functions
    assert_that <- assertthat::assert_that

    ## Check arguments
    assert_that(is.character(file.names))
    file.names <- list.files(pattern = "curvedata")
    ## Create power curves
    power.curves <- lapply(file.names, create_power_curve)
    legend <- cowplot::get_legend(power.curves[[1]])
    power.curves <- lapply(power.curves, function(x) x + theme(legend.position = "none"))

    ## Combine power curves
    combined.plots <- cowplot::plot_grid(plotlist = power.curves, ncol = 2, labels = "AUTO")
    combined.plots.with.legend <- cowplot::plot_grid(combined.plots, legend, ncol = 3, rel_widths = c(1, 0.1))

    ## Save combined power curves
    cowplot::save_plot("combined-power-curves.pdf", combined.plots, base_width = 18, base_height = 10, units = "cm", dpi = 300)
}
