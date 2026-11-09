#' Conduct meta analysis
#'
#' Function to conduct the meta analysis for the TERN application.
#' Uses a random effects model with restricted maximum likelihood (REML)
#' estimation, matching the methods and results of the published systematic
#' review (Nakhid et al. 2026; doi:10.1186/s13049-026-01549-w).
#' @param format The format of the forest plot to be created. Either "pdf" or "png". The default is "png".
#' @param plot Logical. If TRUE, write a forest plot image. Defaults to TRUE.
#' @param export.path Character or NULL. If provided, write forest-plot data as
#'     JSON to this path for use in web graphics. Defaults to NULL.
#' @param programme Character or NULL. If provided, restrict the analysis to
#'     studies evaluating this trauma life support training programme
#'     (e.g. "ATLS"). Defaults to NULL, which includes all studies.
#' @param include.low.quality Logical. Include studies rated unacceptable by
#'     SIGN in the published review. Defaults to FALSE.
conduct_meta_analysis <- function(format = "png", plot = TRUE, export.path = NULL, programme = NULL, include.low.quality = FALSE) {
    # Check if format is valid
    if (plot && format != "pdf" && format != "png") {
        stop("Invalid format. Please use 'pdf' or 'png'.")
    }
    assertthat::assert_that(is.logical(plot) && length(plot) == 1)
    if (!is.null(export.path)) {
        assertthat::assert_that(is.character(export.path) && length(export.path) == 1)
    }

    assertthat::assert_that(is.logical(include.low.quality), length(include.low.quality) == 1, !is.na(include.low.quality))
    data.list <- systematic_review_data(include.low.quality = include.low.quality)
    if (!is.null(programme)) {
        assertthat::assert_that(is.character(programme) && length(programme) == 1)
        keep <- vapply(data.list, function(x) x$programme == programme, logical(1))
        data.list <- data.list[keep]
        if (length(data.list) == 0) {
            stop("No studies with programme '", programme, "'.")
        }
    }
    data <- purrr::map_dfr(data.list, as.list)
    data$citation.key <- names(data.list)
    data$non.atls.r <- with(data, non.atls.died / non.atls.n)
    data$atls.r <- with(data, atls.died / atls.n)
    data$arr <- with(data, atls.r - non.atls.r)
    data$sample.size <- with(data, non.atls.n + atls.n)
    data <- data[order(data$year), ]
    # These study identifiers differ from the keys in the shared bibliography.
    citation.aliases <- c(
        Noordin2011 = "noordin_evolution_2011",
        Petroze2015 = "Petroze2014",
        Kamau2024 = "kamau_impact_2024"
    )
    citation.keys <- data$citation.key
    mapped <- citation.keys %in% names(citation.aliases)
    citation.keys[mapped] <- unname(citation.aliases[citation.keys[mapped]])
    pooled.studies.citation <- paste0("[", paste0("@", citation.keys, collapse = "; "), "]")

    color.palette <- colors()
    favors.training.color <- unname(color.palette["light.intervention"])
    favors.comparison.color <- unname(color.palette["light.standard.care"])
    pooled.color <- unname(color.palette["light.transition"])
    data$plot.color <- ifelse(data$arr < 0, favors.training.color, favors.comparison.color)

    # Random effects meta-analysis of odds ratios, matching Nakhid et al. 2026
    library(metafor)
    outcome <- rma(
        measure = "OR",
        ai = atls.died, bi = atls.n - atls.died,
        ci = non.atls.died, di = non.atls.n - non.atls.died,
        data = data,
        method = "REML"
    )

    ## The pooled estimate and its confidence interval are obtained from
    ## the model summary (predict) on the log scale
    k <- outcome$k
    pred <- predict(outcome)
    pooled.rr.numeric <- as.numeric(exp(outcome$b))
    pooled.ci.lower <- as.numeric(exp(pred$ci.lb))
    pooled.ci.upper <- as.numeric(exp(pred$ci.ub))
    format.rr <- function(x) formatC(x, format = "f", digits = 2)

    studies <- data.frame(
        citationKey = data$citation.key,
        study = as.character(data$study),
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
        logRr = as.numeric(outcome$yi),
        seLogRr = sqrt(as.numeric(outcome$vi)),
        rr = as.numeric(exp(outcome$yi)),
        ciLower = as.numeric(exp(outcome$yi - 1.96 * sqrt(outcome$vi))),
        ciUpper = as.numeric(exp(outcome$yi + 1.96 * sqrt(outcome$vi))),
        weight = as.numeric(weights(outcome)),
        weightPercent = as.numeric(weights(outcome)),
        favorsAtls = outcome$yi < 0,
        color = ifelse(outcome$yi < 0, favors.training.color, favors.comparison.color),
        stringsAsFactors = FALSE
    )
    rownames(studies) <- NULL

    # Create forest plot
    file.name <- NULL
    if (plot) {
        file.name <- paste0("forest-plot.", format)
        if (format == "png") {
            ragg::agg_png(filename = file.name, width = 7, height = 5, units = "in", res = 300)
        } else {
            grDevices::pdf(file = file.name, width = 7, height = 5)
        }

        par(mar = c(6, 0, 1, 0))
        # Reserve separate columns for study labels, log-OR intervals and estimates.
        metafor::forest(outcome,
            slab = data$study,
            xlab = "",
            mlab = "Pooled effect on mortality",
            header = c("Study", "OR [95% CI]"),
            at = log(c(0.2, 0.5, 1, 2, 5)),
            atransf = exp,
            xlim = c(-5.5, 3.5),
            alim = log(c(0.1, 5)),
            textpos = c(-5.4, 3.4),
            rows = k:1,
            ylim = c(-3.5, k + 3),
            refline = 0,
            cex = 0.82,
            colout = "black",
            col = pooled.color,
            border = "black",
            pch = NA
        )
        # Draw squares separately so confidence intervals remain black.
        square.weights <- sqrt(weights(outcome))
        weight.range <- diff(range(square.weights))
        square.sizes <- if (weight.range <= sqrt(.Machine$double.eps)) {
            rep(1, k)
        } else {
            0.5 + (square.weights - min(square.weights)) / weight.range
        }
        points(outcome$yi, k:1, pch = 22, col = "black", bg = studies$color, cex = 0.82 * square.sizes)
        mtext(
            c(if (identical(programme, "ATLS")) "Favors ATLS\u00ae" else "Favors training", "Favors comparison"),
            side = 1, line = 2.3, at = log(c(0.3, 3)), cex = 0.7
        )
        mtext("Odds ratio", side = 1, line = 3.8, at = 0, cex = 0.82)
        # Add heterogeneity statistics below the pooled row, matching the
        # published systematic review (Nakhid et al. 2026)
        het.text <- paste0(
            "Heterogeneity: I\u00b2 = ", formatC(outcome$I2, format = "f", digits = 1), "%; ",
            "tau\u00b2 = ", formatC(outcome$tau2, format = "f", digits = 2), "; ",
            "Q = ", formatC(outcome$QE, format = "f", digits = 2), " (df = ", outcome$k - 1, ")"
        )
        text(-5.4, -2.3, pos = 4, cex = 0.7, het.text)
        dev.off()

        # Crop the plot
        knitr::plot_crop(file.name)

        pooled.rr <- format.rr(pooled.rr.numeric)
        pooled.ci <- paste(format.rr(pooled.ci.lower), format.rr(pooled.ci.upper), sep = "; ")
    } else {
        pooled.rr <- format.rr(pooled.rr.numeric)
        pooled.ci <- paste(format.rr(pooled.ci.lower), format.rr(pooled.ci.upper), sep = "; ")
    }

    if (!is.null(export.path)) {
        payload <- list(
            measure = "OR",
            method = "OR",
            methodRandom = "REML",
            methodTau = "REML",
            studies = studies,
            pooled = list(
                label = "Pooled effect on mortality",
                logRr = as.numeric(outcome$b),
                seLogRr = as.numeric(outcome$se),
                rr = pooled.rr.numeric,
                ciLower = pooled.ci.lower,
                ciUpper = pooled.ci.upper,
                rrFormatted = pooled.rr,
                ciFormatted = pooled.ci,
                pValue = as.numeric(outcome$pval),
                ## i2 exported as a proportion (0-1) for consistency with the
                ## subset pooling used in the web presentation
                i2 = as.numeric(outcome$I2) / 100,
                i2Rounded = round(as.numeric(outcome$I2) / 100, 2),
                tau2 = as.numeric(outcome$tau2),
                numberOfStudies = k,
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
        number.identified.studies = k,
        I2 = round(outcome$I2, 2),
        pooled.rr = pooled.rr,
        pooled.ci = pooled.ci,
        file.name = file.name
    ))
}