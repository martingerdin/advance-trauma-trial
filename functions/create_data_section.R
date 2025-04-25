#' Create data section
#'
#' This function downloads the data dictionary and formats it into a markdown section
#' describing all data collection instruments and their fields.
#'
#' @return A character string containing formatted markdown text describing the data
#'     collection instruments and fields. The text is organized by form, with each
#'     form as a level 3 heading followed by bullet points for each field. For
#'     fields with categorical options (checkbox, radio, dropdown), the options are
#'     listed as sub-bullets. Common abbreviations like WHODAS, EQ5D5L, ATLS and
#'     LAR are properly formatted.
#'
#' @details The function:
#' 1. Downloads the data dictionary
#' 2. Removes whitespace from all fields
#' 3. Filters out "specify other" fields and descriptive fields
#' 4. Groups fields by form
#' 5. For each form:
#'    - Creates a heading from the form name
#'    - Lists each field with its label and note
#'    - For categorical fields, lists all options
#' 6. Formats common abbreviations
create_data_section <- function() {
    data.dictionary <- download_data_dictionary()
    data.dictionary[] <- lapply(data.dictionary, trimws)
    data.dictionary <- data.dictionary[data.dictionary$field_label != "If other, please specify", ]
    data.dictionary <- data.dictionary[data.dictionary$field_type != "descriptive", ]
    variables.list <- split(data.dictionary, data.dictionary$form_name)
    variables.list <- variables.list[unique(data.dictionary$form_name)]
    data.section <- paste0(unlist(lapply(names(variables.list), function(form.name) {
        form.data <- variables.list[[form.name]]
        form.list <- split(form.data, form.data$field_label)
        form.list <- form.list[unique(form.data$field_label)]
        instrument.heading <- form.name |>
            stringr::str_remove("_v\\d+") |>
            stringr::str_replace_all("_", " ") |>
            stringr::str_to_sentence() |>
            paste0("### ", . = _)
        instrument.items <- unique(unlist(lapply(form.list, function(variable) {
            paste0(
                "- **", gsub("<.*?>", "", variable$field_label), "** ",
                variable$field_note, "\n",
                if (any(c("checkbox", "radio", "dropdown") %in% variable$field_type)) {
                    variable.levels <- unlist(strsplit(
                        variable$select_choices_or_calculations,
                        "|",
                        fixed = TRUE
                    ))
                    paste0(paste0(
                        "  ",
                        sapply(variable.levels, function(variable.level) {
                            trimws(stringr::str_replace(variable.level, ",", "."))
                        })
                    ), collapse = "\n")
                } else if (any("yesno" %in% variable$field_type)) {
                    paste0("  1. Yes\n  2. No")
                },
                "\n"
            )
        })))
        paste0(instrument.heading, "\n\n", paste0(instrument.items, collapse = "\n"))
    })), collapse = "\n")
    data.section <- stringr::str_replace_all(data.section, "whodas 20", "(WHODAS 2.0)")
    data.section <- stringr::str_replace_all(data.section, "eq5d5l", "(EQ5D5L)")
    data.section <- stringr::str_replace_all(data.section, stringr::coll("Atls"), "ATLS")
    data.section <- stringr::str_replace_all(data.section, stringr::coll("Lar"), "LAR")
    return(data.section)
}
