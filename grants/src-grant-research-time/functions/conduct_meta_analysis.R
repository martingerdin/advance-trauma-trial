#' Conduct meta analysis
#'
#' Function to conduct the meta analysis for the TERN application
conduct_meta_analysis <- function() {
    library(meta)
    data.list <- systematic_review_data()
    data <- purrr::map_dfr(data.list, as.list)
    data$citation.key <- names(data.list)
    data$non.atls.r <- with(data, non.atls.died/non.atls.n)
    data$atls.r <- with(data, atls.died/atls.n)
    data$arr <- with(data, atls.r - non.atls.r)
    data$sample.size <- with(data, non.atls.n + atls.n)
    data <- data[order(data$year), ]
    pooled.studies.citation <- paste0("[", paste0("@", data$citation.key, collapse = ";"), "]")

    ## Create outcome data for meta-analysis
    outcome <- metabin(event.c = non.atls.died, event.e = atls.died,
                       n.c = non.atls.n, n.e = atls.n,
                       studlab = study, data = data)

    ## Run random effects meta-analysis
    result <- summary(outcome)

    method.text <- "We performed a systematic literature search in the databases Medline, Embase, Cochrane, Web of Science, CINAHL and Google Scholar. We registered the review in PROSPERO, ID CRD42022373977. The last search was conducted 2022-11-29. We developed the search strategy in Medline (Ovid) in collaboration with librarians at the Karolinska Institutet University Library. For each search concept Medical Subject Headings (MeSH-terms) and free text terms were identified. The search was then translated, in part using Polyglot Search Translator, into the other databases. We limited the search to English language articles and searched all databases from inception. We used a random effects model to pool estimates across studies."

    wrapped.method.text <- paste0(c("Methods", strwrap(method.text, width = 93)), collapse = "\n")

    ## Create forest plot
    pdf(file = "forest-plot.pdf", width = 7, height = 3)
    forest.plot <- forest.meta(outcome,
                               layout = "meta",
                               common = FALSE,
                               leftcols = c("studlab", "sample.size"),
                               rightcols = c("effect", "ci"),
                               leftlabs = c("Study", "Sample size"),
                               rightlabs = c("RR", "95% CI"),
                               text.random = "Pooled estimate",
                               fontsize = 11,
                               spacing = 0.85)
    dev.off()

    ## Get estimates
    pooled.rr <- forest.plot$effect.format[2]
    pooled.ci <- stringr::str_remove_all(forest.plot$ci.format[2], "[\\[\\]]")

    ## Return estimates
    return (list(pooled.studies.citation = pooled.studies.citation,
                 number.identified.studies = nrow(data),
                 I2 = round(result$I2, 2),
                 pooled.rr = pooled.rr,
                 pooled.ci = pooled.ci))
}
