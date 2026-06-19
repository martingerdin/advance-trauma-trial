#' Create a shell table of cluster baseline characteristics
#'
#' Builds a shell (template) table illustrating how cluster baseline
#' characteristics will be summarised across the implementation sequences of the
#' stepped-wedge trial. The set of characteristics, their variable type and their
#' response options are pulled from the REDCap cluster-screening data dictionary
#' (metadata), so the table stays in sync with the data actually collected. The
#' table is laid out with `gtsummary`; the body cells are then replaced with
#' placeholders for the summary statistics, because this is an analysis plan
#' rather than a report and no data are summarised yet.
#'
#' @param data A data frame or NULL. The REDCap cluster-screening data
#'     dictionary (metadata), with the columns `field_name`, `field_type` and
#'     `select_choices_or_calculations`. If NULL (the default) the dictionary is
#'     fetched from REDCap with `get_redcap_data()` using `url.name` and
#'     `api.key.name`. Pass a data frame to supply the dictionary directly, for
#'     example when testing offline.
#' @param url.name Character. Name of the environment variable holding the
#'     REDCap API URL for the cluster-screening project. Used only when `data`
#'     is NULL. Defaults to "TGI_REDCAP_URL".
#' @param api.key.name Character. Name of the environment variable holding the
#'     REDCap API token for the cluster-screening project. Used only when `data`
#'     is NULL. Defaults to "TGI_REDCAP_CLUSTER_SCREENING_API_KEY".
#' @param sequences Numeric. Number of implementation sequences (columns) to
#'     display. Defaults to the trial-wide value from `global_variables()`.
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
#'     Defaults to TRUE.
#' @return A `gtsummary` table object (class `tbl_summary`).
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' ## Build the shell table by fetching the cluster-screening data dictionary
#' ## from REDCap (requires the API token in a project-level .env file)
#' cluster.table <- create_cluster_characteristics_table()
#' cluster.table
#' }
#'
#' ## Build the table offline by supplying the committed data-dictionary snapshot
#' ## instead of fetching from REDCap (paths are relative to the
#' ## statistical-analysis-plan directory)
#' dictionary <- read.csv("../cluster-screening/data-dictionary.csv", check.names = FALSE)
#' dictionary <- data.frame(
#'     field_name = dictionary[["Variable / Field Name"]],
#'     field_type = dictionary[["Field Type"]],
#'     select_choices_or_calculations = dictionary[["Choices, Calculations, OR Slider Labels"]]
#' )
#' create_cluster_characteristics_table(data = dictionary)
create_cluster_characteristics_table <- function(data = NULL,
                                                 url.name = "KI_REDCAP_URL",
                                                 api.key.name = "KI_REDCAP_CLUSTER_SCREENING_API_KEY",
                                                 sequences = global_variables()$sequences,
                                                 include.overall = TRUE) {
    ## Check arguments
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.character(url.name) && length(url.name) == 1)
    assertthat::assert_that(is.character(api.key.name) && length(api.key.name) == 1)
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.logical(include.overall) && length(include.overall) == 1)

    ## Fetch the cluster-screening data dictionary from REDCap unless one was
    ## supplied directly.
    if (is.null(data)) {
        data <- get_redcap_data(
            url.name = url.name,
            api.key.name = api.key.name,
            content = "metadata"
        )
    }

    ## Cluster baseline characteristics to summarise, given as REDCap field
    ## names mapped to the short labels used as table row headers. The field
    ## names are looked up in the data dictionary, so the response options
    ## (levels) and the variable type are taken directly from REDCap.
    characteristics <- c(
        volume = "Monthly trauma patient volume",
        hospital_beds = "Hospital beds",
        icu_beds = "Intensive care unit beds",
        trauma_beds = "Dedicated trauma beds",
        specialities = "Specialities available around the clock",
        facilities = "Facilities available around the clock",
        initial_resuscitation = "Initial resuscitation provider"
    )

    ## Placeholder summary statistics for the shell table
    categorical.placeholder <- "n (%)"
    continuous.placeholder <- "mean (SD); median (Q1-Q3)"

    ## Derive a specification (type and levels) for each characteristic from the
    ## data dictionary, using the generic REDCap field helper.
    specifications <- lapply(names(characteristics), function(field.name) {
        get_redcap_field_specification(data, field.name)
    })
    specifications <- Filter(Negate(is.null), specifications)
    assertthat::assert_that(length(specifications) > 0, msg = "None of the requested characteristics were found in the data dictionary.")

    ## Drop the uninformative "Not sure" response option from every characteristic
    specifications <- lapply(specifications, function(specification) {
        specification$levels <- specification$levels[specification$levels != "Not sure"]
        specification
    })

    ## Build a small placeholder data set so that gtsummary lays out every
    ## response option for every sequence. The values themselves are immaterial:
    ## the body cells are overwritten with placeholders below, and defining the
    ## categorical variables as factors guarantees that all levels are shown.
    sequence.levels <- paste("Sequence", seq_len(sequences))
    n.rows <- length(sequence.levels)
    shell.data <- data.frame(sequence = factor(sequence.levels, levels = sequence.levels))
    for (specification in specifications) {
        if (specification$type == "categorical") {
            values <- rep(specification$levels, length.out = n.rows)
            shell.data[[specification$field_name]] <- factor(values, levels = specification$levels)
        } else {
            shell.data[[specification$field_name]] <- as.numeric(seq_len(n.rows))
        }
    }

    ## Apply the human-readable labels and variable types
    variable.names <- vapply(specifications, function(specification) specification$field_name, character(1))
    variable.labels <- stats::setNames(as.list(unname(characteristics[variable.names])), variable.names)
    variable.types <- stats::setNames(
        as.list(vapply(specifications, function(specification) specification$type, character(1))),
        variable.names
    )

    ## Lay out the table with gtsummary, stratified by implementation sequence
    cluster.table <- gtsummary::tbl_summary(
        shell.data,
        by = "sequence",
        label = variable.labels,
        type = variable.types,
        statistic = list(
            gtsummary::all_categorical() ~ "{n} ({p}%)",
            gtsummary::all_continuous() ~ "{mean} ({sd})"
        ),
        missing = "no"
    )
    if (include.overall) {
        cluster.table <- gtsummary::add_overall(cluster.table, last = TRUE)
    }

    ## Replace the computed body cells with placeholders so the table reads as a
    ## shell: every statistic cell shows the format that will be reported rather
    ## than a value derived from the placeholder data.
    cluster.table <- gtsummary::modify_table_body(
        cluster.table,
        function(table.body) {
            statistic.columns <- grep("^stat_", names(table.body), value = TRUE)
            for (statistic.column in statistic.columns) {
                table.body[[statistic.column]][table.body$row_type == "level"] <- categorical.placeholder
                table.body[[statistic.column]][table.body$row_type == "label" & table.body$var_type == "continuous"] <- continuous.placeholder
            }
            table.body
        }
    )

    ## Tidy the column headers (plain sequence labels, no placeholder sample
    ## sizes), group the sequence columns under a spanning header, and document
    ## the shell with a caption.
    cluster.table <- cluster.table |>
        gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**{level}**") |>
        gtsummary::modify_spanning_header(
            gtsummary::all_stat_cols(stat_0 = FALSE) ~ "**Implementation sequence**"
        ) |>
        gtsummary::modify_caption(
            paste(
                "Shell table: cells show the summary statistics that will be reported",
                "(categorical characteristics as n (%) and continuous characteristics",
                "as mean (SD) and median (Q1-Q3)), not observed data. Characteristics",
                "and response options are taken from the REDCap cluster-screening data",
                "dictionary."
            )
        )

    return(cluster.table)
}
