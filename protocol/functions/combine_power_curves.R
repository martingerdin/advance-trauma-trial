combine_power_curves <- function(file.names) {
    ## Define borrowed functions
    assert_that <- assertthat::assert_that

    ## Check arguments
    assert_that(is.character(file.names))

    ## Create power curves
    power.curves <- lapply(file.names, create_power_curve)

    ## Combine power curves
    combined.plots <- cowplot::plot_grid(plotlist = power.curves, ncol = 2, labels = "AUTO")

    ## Save combined power curves
    cowplot::save_plot("combined-power-curves.pdf", combined.plots, base_width = 18, base_height = 10, units = "cm", dpi = 300)
}
