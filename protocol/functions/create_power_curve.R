create_power_curve <- function(curve.data.file) {
    ## Load libraries
    library(ggplot2)

    ## Define borrowed functions
    assert_that <- assertthat::assert_that
    `%>%` <- magrittr::`%>%`

    ## Check arguments
    assert_that(file.exists(curve.data.file))

    ## Parse file name to get proportion in intervention group, proportion
    ## in control group, base ICC, lower ICC, upper ICC, base CAC, number
    ## of clusters, sequences, and batches
    parsed.curve.data.file <- stringr::str_split(curve.data.file, "_") %>%
        unlist() %>%
        stringr::str_replace_all(".csv", "") %>%
        stringr::str_split("-")

    ## Create curve metadata table by creating a data.frame from the parsed.curve.data.file list
    metadata.names <- lapply(parsed.curve.data.file, function(x) x[1])
    curve.metadata <- lapply(parsed.curve.data.file, function(x) x[2])
    names(curve.metadata) <- metadata.names

    ## Load data
    curve.data <- read.csv(curve.data.file, header = TRUE)

    ## Keep only relevant columns
    curve.data <- curve.data %>%
        dplyr::select(xtmp, dplyr::starts_with("power"))

    ## Transform curve data from wide to long format
    curve.data.long <- curve.data %>%
        tidyr::pivot_longer(cols = -xtmp, names_to = "Legend")

    curve.data.long$Legend <- factor(curve.data.long$Legend,
        levels = c("power_x", "power_x_cl", "power_x_cu", "power_x_l", "power_x_u"),
        labels = c(
            paste0("Base ICC ", curve.metadata$baseicc, "; Base CAC ", curve.metadata$basecac),
            paste0("Lower ICC ", curve.metadata$lowericc, "; Base CAC ", curve.metadata$basecac),
            paste0("Upper ICC ", curve.metadata$uppericc, "; Base CAC ", curve.metadata$basecac),
            paste0("Base ICC; Lower CAC ", round(as.numeric(curve.metadata$basecac) * 0.8, 2)),
            paste0("Base ICC; Upper CAC ", round(as.numeric(curve.metadata$basecac) * 1.2, 2))
        )
    )

    ## Create power curve
    power.curve <- ggplot(curve.data.long, aes(x = xtmp, y = value, colour = Legend, linetype = Legend)) +
        geom_line() +
        labs(
            x = "Cluster size (per period)",
            y = "Power"
        ) +
        theme_bw()
    return(power.curve)
}
