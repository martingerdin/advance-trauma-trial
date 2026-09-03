#' Conduct meta analysis
#'
#' Function to conduct the meta analysis for the TERN application
#' @param format The format of the forest plot to be created. Either "pdf" or "png". The default is "png".
#' @param plot Logical. If TRUE, write a forest plot image. Defaults to TRUE.
#' @param export.path Character or NULL. If provided, write forest-plot data as
#'     JSON to this path for use in web graphics. Defaults to NULL.
conduct_meta_analysis <- function(format = "png", plot = TRUE, export.path = NULL) {
    # Check if format is valid
    if (plot && format != "pdf" && format != "png") {
        stop("Invalid format. Please use 'pdf' or 'png'.")
    }
    assertthat::assert_that(is.logical(plot) && length(plot) == 1)
    if (!is.null(export.path)) {
        assertthat::assert_that(is.character(export.path) && length(export.path) == 1)
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

    # Create outcome data for meta-analysis (odds ratio, matching Nakhid et al. 2026)
    outcome <- metabin(
        event.c = non.atls.died, event.e = atls.died,
        n.c = non.atls.n, n.e = atls.n,
        studlab = study, data = data,
        sm = "OR"
    )

    # Run random effects meta-analysis
    result <- summary(outcome)
    color.palette <- colors()
    favors.training.color <- unname(color.palette["light.intervention"])
    favors.comparison.color <- unname(color.palette["light.standard.care"])
    pooled.color <- unname(color.palette["light.transition"])

    pooled.rr.numeric <- as.numeric(exp(result$TE.random))
    pooled.ci.lower <- as.numeric(exp(result$lower.random))
    pooled.ci.upper <- as.numeric(exp(result$upper.random))
    format.rr <- function(x) formatC(x, format = "f", digits = 2)

    studies <- data.frame(
        citationKey = data$citation.key,
        study = as.character(outcome$studlab),
        year = data$year,
        design = data$design,
        programme = data$programme,
        eligibility = data$eligibility,
        outcome = data$outcome,
        sampleSize = data$sample.size,
        atlsN = data$atls.n,
        atlsDied = data$atls.died,
        atlsRate = data$atls.r,
        nonAtlsN = data$non.atls.n,
        nonAtlsDied = data$non.atls.died,
        nonAtlsRate = data$non.atls.r,
        arr = data$arr,
        logRr = as.numeric(outcome$TE),
        seLogRr = as.numeric(outcome$seTE),
        rr = as.numeric(exp(outcome$TE)),
        ciLower = as.numeric(exp(outcome$lower)),
        ciUpper = as.numeric(exp(outcome$upper)),
        weight = as.numeric(outcome$w.random),
        weightPercent = as.numeric(100 * outcome$w.random / sum(outcome$w.random)),
        favorsAtls = outcome$TE < 0,
        color = ifelse(outcome$TE < 0, favors.training.color, favors.comparison.color),
        stringsAsFactors = FALSE
    )
    rownames(studies) <- NULL

    # Create forest plot using ragg device instead of pdf
    file.name <- NULL
    if (plot) {
        file.name <- paste0("forest-plot.", format)
        if (format == "png") {
            ragg::agg_png(filename = file.name, width = 7, height = 2.8, units = "in", res = 300)
        } else {
            ragg::agg_pdf(filename = file.name, width = 7, height = 2.8)
        }

        forest.plot <- forest(outcome,
            layout = "meta",
            common = FALSE,
            leftcols = c("studlab", "sample.size"),
            rightcols = c("effect", "ci"),
            leftlabs = c("Study", "Sample size"),
            rightlabs = c("OR", "95% CI"),
            text.random = "Pooled effect on mortality",
            label.left = "Favors training",
            label.right = "Favors comparison",
            fontsize = 9,
            spacing = 0.65,
            col.square = ifelse(outcome$TE < 0, favors.training.color, favors.comparison.color),
            col.diamond = pooled.color
        )
        dev.off()

        # Crop the plot
        knitr::plot_crop(file.name)

        # Get estimates from the formatted forest plot columns
        pooled.rr <- forest.plot$effect.format[2]
        pooled.ci <- stringr::str_remove_all(forest.plot$ci.format[2], "[\\[\\]]")
    } else {
        pooled.rr <- format.rr(pooled.rr.numeric)
        pooled.ci <- paste(format.rr(pooled.ci.lower), format.rr(pooled.ci.upper), sep = "; ")
    }

    if (!is.null(export.path)) {
        payload <- list(
            measure = "OR",
            method = outcome$method,
            methodRandom = outcome$method.random,
            methodTau = outcome$method.tau,
            studies = studies,
            pooled = list(
                label = "Pooled effect on mortality",
                logRr = as.numeric(result$TE.random),
                seLogRr = as.numeric(result$seTE.random),
                rr = pooled.rr.numeric,
                ciLower = pooled.ci.lower,
                ciUpper = pooled.ci.upper,
                rrFormatted = pooled.rr,
                ciFormatted = pooled.ci,
                pValue = as.numeric(result$pval.random),
                i2 = as.numeric(result$I2),
                i2Rounded = round(as.numeric(result$I2), 2),
                tau2 = as.numeric(result$tau2),
                numberOfStudies = nrow(data),
                color = pooled.color
            ),
            labels = list(
                left = "Favors training",
                right = "Favors comparison",
                effect = "OR",
                sampleSize = "Sample size",
                study = "Study"
            ),
            logScaleXlim = c(-2, 2),
            pooledStudiesCitation = pooled.studies.citation
        )
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
