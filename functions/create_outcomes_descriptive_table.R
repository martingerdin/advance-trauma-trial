#' Create a shell table of descriptive outcome summaries
#'
#' Builds a shell (template) table illustrating how primary and secondary
#' outcomes will be summarised before and after ATLS training is implemented in
#' a cluster. Dichotomous outcomes are reported as n (\%); continuous outcomes
#' are reported as median (Q1-Q3); ordinal EQ-5D-5L and WHODAS domain scores are
#' reported as n (\%) by level. Derived outcomes (mortality at specified time
#' points, lengths of stay, adherence proportion, EQ-5D-5L index/VAS, and WHODAS
#' summary scores) are specified directly rather than from single REDCap fields.
#' Missing values are shown for every outcome. The table is laid out with
#' `gtsummary`; body cells are blanked for the analysis plan.
#'
#' @param data A data frame or NULL. The REDCap trial-data dictionary
#'     (metadata). If NULL (the default) the dictionary is fetched from REDCap.
#' @param url.name Character. REDCap API URL environment variable name.
#' @param api.key.name Character. REDCap API token environment variable name.
#' @param groups Character. Stratum labels. Defaults to
#'     c("Before ATLS", "After ATLS").
#' @param all Logical. If FALSE (the default), the table shows key outcome
#'     summaries for the main results (headline mortality, lengths of stay,
#'     return to work, adherence, EQ-5D-5L index, and WHODAS summary scores).
#'     If TRUE, the table also includes EQ-5D-5L domains and VAS and WHODAS
#'     domain scores at each follow-up time point for supplementary reporting.
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
#' @return A `gtsummary` or `kableExtra` table object for short output formats,
#'     or a `knitr_asis` object containing a page-breaking `longtable` for
#'     PDF/LaTeX output.
#'
#' @seealso [create_outcomes_descriptive_table_word_preview()] to render a
#'     minimal Word document for checking section headers and table layout.
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_outcomes_descriptive_table()
#' create_outcomes_descriptive_table(all = TRUE)
#'
#' ## Minimal Word preview (requires Quarto)
#' create_outcomes_descriptive_table_word_preview()
#' create_outcomes_descriptive_table_word_preview(all = TRUE)
#' }
create_outcomes_descriptive_table <- function(data = NULL,
                                              url.name = "TGI_REDCAP_URL",
                                              api.key.name = "TGI_REDCAP_TRIAL_DATA_API_KEY",
                                              groups = c("Before ATLS", "After ATLS"),
                                              all = FALSE,
                                              include.overall = TRUE) {
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.character(url.name) && length(url.name) == 1)
    assertthat::assert_that(is.character(api.key.name) && length(api.key.name) == 1)
    assertthat::assert_that(is.character(groups) && length(groups) >= 2)
    assertthat::assert_that(is.logical(all) && length(all) == 1)
    assertthat::assert_that(is.logical(include.overall) && length(include.overall) == 1)

    if (is.null(data)) {
        data <- get_redcap_data(
            url.name = url.name,
            api.key.name = api.key.name,
            content = "metadata"
        )
    }

    sections <- outcomes_shell_section_labels()
    timepoints <- list(
        list(
            label = "within seven days of discharge",
            slug = "within_seven_days_of_discharge",
            section = sections$nested_seven_days
        ),
        list(
            label = "at 30 days",
            slug = "at_30_days",
            section = sections$nested_30_days
        ),
        list(
            label = "at three months",
            slug = "at_three_months",
            section = sections$nested_three_months
        )
    )
    adherence.section <- sections$nested_resuscitation
    main.section <- sections$main_stepped_wedge
    ## WHODAS 2.0 difficulty response scale (same wording across domains)
    whodas.levels <- c(
        "1. None",
        "2. Mild",
        "3. Moderate",
        "4. Severe",
        "5. Extreme or cannot do"
    )
    eq5d.domains <- list(
        list(
            slug = "mobility",
            label = "mobility",
            levels = c(
                "1. I have no problems in walking about",
                "2. I have slight problems in walking about",
                "3. I have moderate problems in walking about",
                "4. I have severe problems in walking about",
                "5. I am unable to walk about"
            )
        ),
        list(
            slug = "self_care",
            label = "self-care",
            levels = c(
                "1. I have no problems washing or dressing myself",
                "2. I have slight problems washing or dressing myself",
                "3. I have moderate problems washing or dressing myself",
                "4. I have severe problems washing or dressing myself",
                "5. I am unable to wash or dress myself"
            )
        ),
        list(
            slug = "usual_activities",
            label = "usual activities",
            levels = c(
                "1. I have no problems doing my usual activities",
                "2. I have slight problems doing my usual activities",
                "3. I have moderate problems doing my usual activities",
                "4. I have severe problems doing my usual activities",
                "5. I am unable to do my usual activities"
            )
        ),
        list(
            slug = "pain_discomfort",
            label = "pain/discomfort",
            levels = c(
                "1. I have no pain or discomfort",
                "2. I have slight pain or discomfort",
                "3. I have moderate pain or discomfort",
                "4. I have severe pain or discomfort",
                "5. I have extreme pain or discomfort"
            )
        ),
        list(
            slug = "anxiety_depression",
            label = "anxiety/depression",
            levels = c(
                "1. I am not anxious or depressed",
                "2. I am slightly anxious or depressed",
                "3. I am moderately anxious or depressed",
                "4. I am severely anxious or depressed",
                "5. I am extremely anxious or depressed"
            )
        )
    )
    whodas.domains <- list(
        list(slug = "cognition", label = "cognition"),
        list(slug = "mobility", label = "mobility"),
        list(slug = "self_care", label = "self-care"),
        list(slug = "getting_along", label = "getting along"),
        list(slug = "life_activities", label = "life activities"),
        list(slug = "participation", label = "participation")
    )

    requests <- list(
        ## Primary outcome
        list(field = "inhospital_mortality_30d",
             label = "In-hospital mortality within 30 days",
             source = "external", summary = "dichotomous",
             section = sections$primary),

        ## Secondary outcomes (main stepped-wedge design)
        list(field = "all_cause_mortality_24h",
             label = "All-cause mortality within 24 hours",
             source = "external", summary = "dichotomous",
             section = main.section),
        list(field = "all_cause_mortality_30d",
             label = "All-cause mortality within 30 days",
             source = "external", summary = "dichotomous",
             section = main.section),
        list(field = "all_cause_mortality_90d",
             label = "All-cause mortality within three months",
             source = "external", summary = "dichotomous",
             section = main.section),
        list(field = "length_ed_stay",
             label = "Length of emergency department stay (hours)",
             source = "external", summary = "continuous",
             section = main.section),
        list(field = "length_hospital_stay",
             label = "Length of hospital stay (days)",
             source = "external", summary = "continuous",
             section = main.section),
        list(field = "icu_admission",
             label = "Intensive care unit admission",
             source = "dictionary", summary = "dichotomous",
             section = main.section),
        list(field = "length_icu_stay",
             label = "Length of intensive care unit stay (days)",
             source = "external", summary = "continuous",
             section = main.section),
        list(field = "return_to_work_30d",
             label = "Return to work at 30 days",
             source = "external", summary = "dichotomous",
             section = main.section),
        list(field = "return_to_work_90d",
             label = "Return to work at three months",
             source = "external", summary = "dichotomous",
             section = main.section),

        ## Nested staircase — adherence (not time-point follow-up)
        list(field = "atls_adherence",
             label = "Adherence to ATLS principles (%)",
             source = "external", summary = "continuous",
             section = adherence.section)
    )

    for (timepoint in timepoints) {
        ## Key summaries first within each follow-up section
        requests <- c(requests, list(
            list(
                field = paste0("eq5d_index_", timepoint$slug),
                label = "EQ-5D-5L index score",
                source = "external",
                summary = "continuous",
                section = timepoint$section
            ),
            list(
                field = paste0("whodas_summary_", timepoint$slug),
                label = "WHODAS 2.0 summary score",
                source = "external",
                summary = "continuous",
                section = timepoint$section
            )
        ))

        if (isTRUE(all)) {
            for (domain in eq5d.domains) {
                requests <- c(requests, list(list(
                    field = paste0("eq5d_", domain$slug, "_", timepoint$slug),
                    label = paste0("EQ-5D-5L ", domain$label),
                    source = "external",
                    summary = "categorical",
                    levels = domain$levels,
                    section = timepoint$section
                )))
            }
            requests <- c(requests, list(list(
                field = paste0("eq5d_vas_", timepoint$slug),
                label = "EQ-5D-5L VAS",
                source = "external",
                summary = "continuous",
                section = timepoint$section
            )))
            for (domain in whodas.domains) {
                requests <- c(requests, list(list(
                    field = paste0("whodas_", domain$slug, "_", timepoint$slug),
                    label = paste0("WHODAS 2.0 ", domain$label),
                    source = "external",
                    summary = "categorical",
                    levels = whodas.levels,
                    section = timepoint$section
                )))
            }
        }
    }

    build_patient_characteristics_shell_table(
        data = data,
        requests = requests,
        groups = groups,
        include.overall = include.overall,
        longtable = TRUE,
        label.header = "**Outcome**"
    )
}

#' Write a minimal Word preview of the outcomes descriptive table
#'
#' Renders a single-table Quarto document to Word. Useful for checking section
#' headers, stratification columns, and non-LaTeX formatting without building
#' the full statistical analysis plan.
#'
#' @param output.file Character or NULL. Path for the Word document to create.
#'     If NULL, defaults to `_test-outcomes-word.docx` for key outcomes and
#'     `_test-outcomes-all-word.docx` when `all = TRUE`.
#' @param title Character or NULL. Title shown in the Word document. If NULL, a
#'     default title is chosen based on `all`.
#' @param all Logical. If TRUE, preview the supplementary all-outcomes table;
#'     otherwise preview the key outcomes table.
#' @param cleanup.qmd Logical. If TRUE, delete the temporary `.qmd` after
#'     rendering.
#' @return Invisibly, the path to `output.file`.
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' create_outcomes_descriptive_table_word_preview()
#' create_outcomes_descriptive_table_word_preview(all = TRUE)
#' create_outcomes_descriptive_table_word_preview("preview/outcomes-table.docx")
#' }
create_outcomes_descriptive_table_word_preview <- function(
    output.file = NULL,
    title = NULL,
    all = FALSE,
    cleanup.qmd = FALSE) {
    assertthat::assert_that(is.null(output.file) || (is.character(output.file) && length(output.file) == 1))
    assertthat::assert_that(is.null(title) || (is.character(title) && length(title) == 1))
    assertthat::assert_that(is.logical(all) && length(all) == 1)
    assertthat::assert_that(is.logical(cleanup.qmd) && length(cleanup.qmd) == 1)

    if (is.null(output.file)) {
        output.file <- if (isTRUE(all)) {
            "_test-outcomes-all-word.docx"
        } else {
            "_test-outcomes-word.docx"
        }
    }
    if (is.null(title)) {
        title <- if (isTRUE(all)) {
            "All outcomes — Word preview"
        } else {
            "Key outcomes — Word preview"
        }
    }

    table.call <- if (isTRUE(all)) {
        "create_outcomes_descriptive_table(all = TRUE)"
    } else {
        "create_outcomes_descriptive_table()"
    }
    table.label <- if (isTRUE(all)) {
        "tbl-outcomes-descriptive-all"
    } else {
        "tbl-outcomes-descriptive"
    }
    table.caption <- if (isTRUE(all)) {
        "Descriptive summaries of all outcomes"
    } else {
        "Descriptive summaries of key outcomes"
    }
    description <- if (isTRUE(all)) {
        "Minimal preview of all outcomes for Word output (section headers and stratification)."
    } else {
        "Minimal preview of key outcomes for Word output (section headers and stratification)."
    }

    render_shell_table_word_preview(
        table.call = table.call,
        output.file = output.file,
        title = title,
        table.label = table.label,
        table.caption = table.caption,
        description = description,
        cleanup.qmd = cleanup.qmd
    )
}
