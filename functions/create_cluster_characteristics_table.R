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
#' @param data A data frame. The REDCap cluster-screening data dictionary
#'     (metadata), as returned by `get_redcap_data(content = "metadata")`. Must
#'     contain the columns `field_name`, `field_type` and
#'     `select_choices_or_calculations`. No default.
#' @param sequences Numeric. Number of implementation sequences (columns) to
#'     display. Defaults to the trial-wide value from `global_variables()`.
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
#'     Defaults to TRUE.
#' @return A `gtsummary` table object (class `tbl_summary`).
create_cluster_characteristics_table <- function(data,
                                                 sequences = global_variables()$sequences,
                                                 include.overall = TRUE) {
    ## Check arguments
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.logical(include.overall) && length(include.overall) == 1)
    required.columns <- c("field_name", "field_type", "select_choices_or_calculations")
    assertthat::assert_that(
        all(required.columns %in% names(data)),
        msg = paste0(
            "`data` must be REDCap metadata containing the columns: ",
            paste(required.columns, collapse = ", "), "."
        )
    )

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

    ## Extract the response option labels from a REDCap choices string of the
    ## form "1, Label one | 2, Label two | ...".
    parse_choices <- function(choices) {
        options <- trimws(strsplit(choices, "|", fixed = TRUE)[[1]])
        labels <- vapply(options, function(option) trimws(sub("^[^,]*,", "", option)), character(1))
        unname(labels[nzchar(labels)])
    }

    ## Derive a specification (type and levels) for each characteristic from the
    ## data dictionary.
    specifications <- lapply(names(characteristics), function(field.name) {
        metadata <- data[data$field_name == field.name, , drop = FALSE]
        if (nrow(metadata) == 0) {
            warning("Field '", field.name, "' was not found in the REDCap data dictionary and was skipped.")
            return(NULL)
        }
        field.type <- metadata$field_type[1]
        if (field.type %in% c("radio", "dropdown", "checkbox")) {
            list(name = field.name, type = "categorical", levels = parse_choices(metadata$select_choices_or_calculations[1]))
        } else if (field.type == "yesno") {
            list(name = field.name, type = "categorical", levels = c("Yes", "No"))
        } else {
            ## Anything else (e.g. numeric text or calculated fields) is treated
            ## as a continuous variable.
            list(name = field.name, type = "continuous", levels = NULL)
        }
    })
    specifications <- Filter(Negate(is.null), specifications)
    assertthat::assert_that(length(specifications) > 0, msg = "None of the requested characteristics were found in the data dictionary.")

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
            shell.data[[specification$name]] <- factor(values, levels = specification$levels)
        } else {
            shell.data[[specification$name]] <- as.numeric(seq_len(n.rows))
        }
    }

    ## Apply the human-readable labels and variable types
    variable.names <- vapply(specifications, function(specification) specification$name, character(1))
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
