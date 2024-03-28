create_data_section <- function() {
    data.dictionary <- download_data_dictionary()
    data.dictionary[] <- lapply(data.dictionary, trimws)
    data.dictionary <- data.dictionary[data.dictionary$field_label != "If other, please specify", ]
    data.dictionary <- data.dictionary[data.dictionary$field_type != "descriptive", ]
    variables.list <- split(data.dictionary, data.dictionary$form_name)
    variables.list <- variables.list[unique(data.dictionary$form_name)]
    data.section <- unlist(lapply(names(variables.list), function(form_name) {
        form.data <- variables.list[[form_name]]
        form.list <- split(form.data, form.data$field_label)
        form.list <- form.list[unique(form.data$field_label)]
        paste0(
            "### ", stringr::str_to_sentence(stringr::str_replace_all(form_name, "_", " ")), "\n\n",
            paste0(unlist(lapply(form.list, function(variable) {
                paste0(
                    "- **", gsub("<.*?>", "", variable$field_label), "** ",
                    variable$Field.Note, "\n",
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
                    }
                )
            })), collapse = "\n")
        )
    }))
    data.section <- paste0(unique(data.section), collapse = "\n")
    data.section <- stringr::str_replace_all(data.section, "whodas 20", "(WHODAS 2.0)")
    data.section <- stringr::str_replace_all(data.section, "eq5d5l", "(EQ5D5L)")
    return(data.section)
}
