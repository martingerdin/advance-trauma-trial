#' Create a shell table of patient baseline characteristics
#'
#' Builds a shell (template) table illustrating how patient baseline
#' characteristics will be summarised before and after the ATLS training is
#' implemented in a cluster. Where possible the variable type and the response
#' options are pulled from the REDCap trial-data dictionary (metadata), so the
#' table stays in sync with the data actually collected. A few characteristics
#' are not single dictionary fields and are supplied directly: the Injury
#' Severity Score is derived from the recorded injury diagnoses, radiology is
#' derived from the imaging form, and the mechanism of injury is recorded as an
#' ICD-10 code and so is shown using the grouped categories it will be
#' collapsed into for reporting. The table is laid out with `gtsummary`; the
#' body cells are then blanked, because this is an analysis plan rather than a
#' report and no data are summarised yet, leaving the statistic label in each
#' row to document the format that will be reported.
#'
#' @param data A data frame or NULL. The REDCap trial-data dictionary
#'     (metadata), with the columns `field_name`, `field_type` and
#'     `select_choices_or_calculations`. If NULL (the default) the dictionary is
#'     fetched from REDCap with `get_redcap_data()` using `url.name` and
#'     `api.key.name`. Pass a data frame to supply the dictionary directly, for
#'     example when testing offline.
#' @param url.name Character. Name of the environment variable holding the
#'     REDCap API URL for the trial-data project. Used only when `data` is NULL.
#'     Defaults to "TGI_REDCAP_URL".
#' @param api.key.name Character. Name of the environment variable holding the
#'     REDCap API token for the trial-data project. Used only when `data` is
#'     NULL. Defaults to "TGI_REDCAP_TRIAL_DATA_API_KEY".
#' @param groups Character. Labels for the two strata, given before and after
#'     ATLS training is implemented in a cluster. Defaults to
#'     c("Before ATLS", "After ATLS").
#' @param mechanism.levels Character. The grouped mechanism-of-injury categories
#'     to show, because the underlying field is a full ICD-10 code list that is
#'     collapsed for reporting. Defaults to a provisional grouping that can be
#'     refined once the reporting categories are fixed.
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
#'     Defaults to TRUE.
#' @return A `gtsummary` table object (class `tbl_summary`), or, for PDF/LaTeX
#'     output, a `kableExtra` table sized to fit the text block.
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' ## Build the shell table by fetching the trial-data dictionary from REDCap
#' ## (requires the API token in a project-level .env file)
#' patient.table <- create_patient_characteristics_table()
#' patient.table
#' }
create_patient_characteristics_table <- function(data = NULL,
                                                 url.name = "TGI_REDCAP_URL",
                                                 api.key.name = "TGI_REDCAP_TRIAL_DATA_API_KEY",
                                                 groups = c("Before ATLS", "After ATLS"),
                                                 mechanism.levels = c(
                                                     "Road traffic injury",
                                                     "Fall",
                                                     "Assault",
                                                     "Other"
                                                 ),
                                                 include.overall = TRUE) {
    ## Check arguments
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.character(url.name) && length(url.name) == 1)
    assertthat::assert_that(is.character(api.key.name) && length(api.key.name) == 1)
    assertthat::assert_that(is.character(groups) && length(groups) >= 2)
    assertthat::assert_that(is.character(mechanism.levels) && length(mechanism.levels) >= 2)
    assertthat::assert_that(is.logical(include.overall) && length(include.overall) == 1)

    ## Fetch the trial-data dictionary from REDCap unless one was supplied
    ## directly.
    if (is.null(data)) {
        data <- get_redcap_data(
            url.name = url.name,
            api.key.name = api.key.name,
            content = "metadata"
        )
    }

    ## Categorical response options to drop as uninformative for a shell table
    ## (these record missing or unknown values rather than substantive groups).
    dropped.levels <- c("Not sure", "Not known")

    ## The requested patient characteristics, in display order. Each entry names
    ## the REDCap field (or a derived measure), the row label, and how it should
    ## be summarised. `source = "dictionary"` pulls the type and levels from the
    ## REDCap data dictionary; `source = "external"` is a measure that is not a
    ## single dictionary field (derived or grouped) and so is specified here.
    ## `summary` forces a summary type: "dichotomous" shows a single "Yes" row
    ## for yes/no measures, overriding the categorical type from the dictionary.
    requests <- list(
        list(field = "age", label = "Age (years)", source = "dictionary"),
        list(field = "sex", label = "Sex", source = "dictionary"),
        list(field = "mechanism_of_injury", label = "Mechanism of injury",
             source = "external", summary = "categorical", levels = mechanism.levels),
        list(field = "injury_severity_score", label = "Injury Severity Score",
             source = "external", summary = "continuous"),
        list(field = "glasgow_coma_scale", label = "Glasgow Coma Scale score", source = "dictionary"),
        list(field = "systolic_blood_pressure", label = "Systolic blood pressure (mmHg)", source = "dictionary"),
        list(field = "surgery_done", label = "Surgery", source = "dictionary", summary = "dichotomous"),
        list(field = "transfusion_done", label = "Transfusion", source = "dictionary", summary = "dichotomous"),
        list(field = "radiology", label = "Radiology", source = "external", summary = "dichotomous"),
        list(field = "icu_admission", label = "Intensive care unit admission", source = "dictionary", summary = "dichotomous")
    )

    ## Placeholder summary statistics for the shell table. The body cells are
    ## blanked; the statistic label added to each row documents the format that
    ## will be reported.
    cell.placeholder <- ""

    ## The "Yes" level reported for dichotomous (yes/no) characteristics
    dichotomous.value <- "Yes"

    ## Continuous variables are reported on two lines, as a mean (standard
    ## deviation) and a median (Q1-Q3), matching the patient-characteristics
    ## description in the analysis plan. gtsummary's "continuous2" type lays each
    ## statistic out on its own row.
    continuous.type <- "continuous2"
    continuous.statistics <- c("{mean} ({sd})", "{median} ({p25}, {p75})")

    ## Derive a specification (label, summary type and levels) for each
    ## requested characteristic. Dictionary fields use the generic REDCap field
    ## helper; external measures are specified directly. Dichotomous overrides
    ## collapse a yes/no field to a single reported "Yes" row.
    specifications <- lapply(requests, function(request) {
        if (identical(request$source, "dictionary")) {
            specification <- get_redcap_field_specification(data, request$field)
            if (is.null(specification)) {
                return(NULL)
            }
        } else {
            specification <- list(
                field_name = request$field,
                redcap_type = NA_character_,
                type = request$summary,
                levels = if (is.null(request$levels)) character(0) else request$levels
            )
        }

        ## Apply a forced summary type (e.g. dichotomous for yes/no measures)
        if (!is.null(request$summary)) {
            specification$type <- request$summary
        }
        if (identical(specification$type, "dichotomous")) {
            specification$levels <- c(dichotomous.value, "No")
        }

        ## Drop uninformative categorical levels and attach the row label
        specification$levels <- specification$levels[!specification$levels %in% dropped.levels]
        specification$label <- request$label
        specification
    })
    specifications <- Filter(Negate(is.null), specifications)
    assertthat::assert_that(length(specifications) > 0, msg = "None of the requested characteristics were found.")

    ## Build a small placeholder data set so that gtsummary lays out every
    ## response option for every group. The values themselves are immaterial:
    ## the body cells are blanked below, and defining the categorical variables
    ## as factors guarantees that all levels are shown.
    n.rows <- length(groups)
    shell.data <- data.frame(group = factor(groups, levels = groups))
    for (specification in specifications) {
        if (specification$type == "continuous") {
            shell.data[[specification$field_name]] <- as.numeric(seq_len(n.rows))
        } else {
            values <- rep(specification$levels, length.out = n.rows)
            shell.data[[specification$field_name]] <- factor(values, levels = specification$levels)
        }
    }

    ## Apply the human-readable labels, the variable types, and, for dichotomous
    ## variables, the level reported.
    variable.names <- vapply(specifications, function(specification) specification$field_name, character(1))
    variable.labels <- stats::setNames(
        lapply(specifications, function(specification) specification$label),
        variable.names
    )
    variable.types <- stats::setNames(
        lapply(specifications, function(specification) {
            if (specification$type == "continuous") continuous.type else specification$type
        }),
        variable.names
    )
    dichotomous.names <- variable.names[vapply(specifications, function(specification) specification$type == "dichotomous", logical(1))]
    variable.values <- stats::setNames(
        rep(list(dichotomous.value), length(dichotomous.names)),
        dichotomous.names
    )

    ## Lay out the table with gtsummary, stratified by ATLS period
    patient.table <- gtsummary::tbl_summary(
        shell.data,
        by = "group",
        label = variable.labels,
        type = variable.types,
        value = variable.values,
        statistic = list(
            gtsummary::all_categorical() ~ "{n} ({p}%)",
            gtsummary::all_continuous() ~ continuous.statistics
        ),
        missing = "no"
    )
    if (include.overall) {
        patient.table <- gtsummary::add_overall(patient.table, last = TRUE)
    }

    ## Blank the computed body cells so the table reads as a shell: the format
    ## that will be reported is documented by the statistic label in each row
    ## (added below) rather than by a value derived from the placeholder data.
    patient.table <- gtsummary::modify_table_body(
        patient.table,
        function(table.body) {
            statistic.columns <- grep("^stat_", names(table.body), value = TRUE)
            for (statistic.column in statistic.columns) {
                table.body[[statistic.column]][table.body$row_type == "level"] <- cell.placeholder
                table.body[[statistic.column]][table.body$row_type == "label" &
                    table.body$var_type %in% c("continuous", "continuous2", "dichotomous")] <- cell.placeholder
            }
            table.body
        }
    )

    ## Tidy the column headers (plain group labels, no placeholder sample sizes)
    ## and group the ATLS-period columns under a spanning header.
    patient.table <- patient.table |>
        gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**{level}**") |>
        gtsummary::modify_spanning_header(
            gtsummary::all_stat_cols(stat_0 = FALSE) ~ "**ATLS training**"
        )

    ## Add a label for the summary statistics shown in each row
    patient.table <- patient.table |>
        gtsummary::add_stat_label()

    ## For PDF/LaTeX output, keep a fixed font size (matching the other tables in
    ## the document) rather than scaling the whole table down. Every column is
    ## given a fixed width expressed as a fraction of the line width, so that long
    ## labels wrap across several lines, and the inter-column padding is reduced.
    ## Other output formats (HTML, Word) are returned as the gtsummary object
    ## unchanged.
    if (isTRUE(knitr::is_latex_output())) {
        n.statistic.columns <- length(groups) + as.integer(include.overall)
        label.width <- 0.34
        statistic.width <- round((0.84 - label.width) / n.statistic.columns, 3)

        patient.table <- patient.table |>
            gtsummary::as_kable_extra(format = "latex", booktabs = TRUE, linesep = "") |>
            kableExtra::kable_styling(latex_options = "HOLD_position", font_size = 8)

        ## Replace the tabular preamble with fixed-width paragraph columns (so the
        ## contents wrap rather than overflow) and reduce the inter-column
        ## padding. Written as a single rewrite because chained column widths
        ## expressed in \\linewidth confuse kableExtra::column_spec.
        column.preamble <- paste0(
            "\\setlength{\\tabcolsep}{3pt}\\begin{tabular}{",
            ">{\\raggedright\\arraybackslash}p{", label.width, "\\linewidth}",
            "*{", n.statistic.columns, "}{>{\\centering\\arraybackslash}p{", statistic.width, "\\linewidth}}",
            "}"
        )
        column.replacement <- gsub("\\\\", "\\\\\\\\", column.preamble)
        table.attributes <- attributes(patient.table)
        patient.table <- sub("\\\\begin\\{tabular\\}\\{[lcr]+\\}", column.replacement, patient.table)
        attributes(patient.table) <- table.attributes
    }

    return(patient.table)
}
