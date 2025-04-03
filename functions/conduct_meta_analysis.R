#' Conduct meta analysis
#'
#' Function to conduct the meta analysis for the TERN application
#' @param format The format of the forest plot to be created. Either "pdf" or "png". The default is "png".
conduct_meta_analysis <- function(format = "png") {
    # Check if format is valid
    if (format != "pdf" && format != "png") {
        stop("Invalid format. Please use 'pdf' or 'png'.")
    }

    library(meta)
    data.list <- systematic_review_data()
    data <- purrr::map_dfr(data.list, as.list)
    data$citation.key <- names(data.list)
    data$non.atls.r <- with(data, non.atls.died / non.atls.n)
    data$atls.r <- with(data, atls.died / atls.n)
    data$arr <- with(data, atls.r - non.atls.r)
    data$sample.size <- with(data, non.atls.n + atls.n)
    data <- data[order(data$year), ]
    pooled.studies.citation <- paste0("[", paste0("@", data$citation.key, collapse = ";"), "]")

    # Create outcome data for meta-analysis
    outcome <- metabin(
        event.c = non.atls.died, event.e = atls.died,
        n.c = non.atls.n, n.e = atls.n,
        studlab = study, data = data
    )

    # Run random effects meta-analysis
    result <- summary(outcome)

    # Create forest plot using ragg device instead of pdf
    file.name <- paste0("forest-plot.", format)
    if (format == "png") {
        ragg::agg_png(filename = file.name, width = 7, height = 2.8, units = "in", res = 300)
    } else {
        ragg::agg_pdf(filename = file.name, width = 7, height = 2.8)
    }

    # Create lighter versions by blending with white
    color.palette <- colors()
    forest.plot <- forest.meta(outcome,
        layout = "meta",
        common = FALSE,
        leftcols = c("studlab", "sample.size"),
        rightcols = c("effect", "ci"),
        leftlabs = c("Study", "Sample size"),
        rightlabs = c("RR", "95% CI"),
        text.random = "Pooled effect on mortality",
        label.left = "Favors ATLS®",
        label.right = "Favors comparison",
        fontsize = 9,
        spacing = 0.65,
        col.square = ifelse(outcome$TE < 0, color.palette["light.intervention"], color.palette["light.standard.care"]),
        col.diamond = color.palette["light.transition"]
    )
    dev.off()

    # Crop the plot
    knitr::plot_crop(file.name)

    # Get estimates
    pooled.rr <- forest.plot$effect.format[2]
    pooled.ci <- stringr::str_remove_all(forest.plot$ci.format[2], "[\\[\\]]")

    # Return estimates
    return(list(
        pooled.studies.citation = pooled.studies.citation,
        number.identified.studies = nrow(data),
        I2 = round(result$I2, 2),
        pooled.rr = pooled.rr,
        pooled.ci = pooled.ci,
        file.name = file.name
    ))
}
