#' Create data section
#'
#' This function downloads the data dictionary and formats it into a markdown section
#' describing all data collection instruments and their fields.
#'
#' @return A character string containing formatted markdown text describing the data
#'     collection instruments and fields. The text is organized by form, with each
#'     form as a level 3 heading followed by bullet points for each field. For
#'     fields with categorical options (checkbox, radio, dropdown), the options are
#'     listed as a bulleted sub-list, with the numeric REDCap choice value and
#'     any redundant label numbering dropped from each option. Long lists of
#'     ICD-10 codes (such as the full ICD-10 code list) are summarized as code
#'     ranges by chapter letter with counts. Other fields with more than 20
#'     options show only the first and last five options, with an indicator that
#'     further options exist in between, so that very long lists do not bloat
#'     the document. Option definitions embedded in a field label (instructions
#'     to the coder) are dropped from the label. Common abbreviations like
#'     WHODAS, EQ5D5L, ATLS and LAR are properly formatted.
#'
#' @param exclude.forms Character vector. Instrument names (as rendered in the
#'     data section headings) to drop from the output. Used to exclude
#'     administrative forms, such as the scheduling and review forms, that do
#'     not describe research variables. Names are matched case-insensitively,
#'     allowing for version suffixes. Defaults to c("scheduling", "review").
#'
#' @details The function:
#' 1. Downloads the data dictionary
#' 2. Removes whitespace from all fields
#' 3. Filters out "specify other" fields and descriptive fields
#' 4. Groups fields by form
#' 5. Drops excluded forms (see `exclude.forms`)
#' 6. For each form:
#'    - Creates a heading from the form name
#'    - Lists each field with its label and note
#'    - For categorical fields, lists all options, unless there are more than
#'      20. Lists that are mostly ICD-10 codes are summarized as code ranges by
#'      chapter letter with counts; other long lists show the first and last
#'      five options with an indicator that further options exist in between
#' 7. Formats common abbreviations
create_data_section <- function(exclude.forms = c("scheduling", "review")) {
    data.dictionary <- download_data_dictionary()
    data.dictionary[] <- lapply(data.dictionary, trimws)
    data.dictionary <- data.dictionary[
        !grepl("if other|please specify|other specify", data.dictionary$field_label, ignore.case = TRUE),
    ]
    data.dictionary <- data.dictionary[data.dictionary$field_type != "descriptive", ]
    variables.list <- split(data.dictionary, data.dictionary$form_name)
    variables.list <- variables.list[unique(data.dictionary$form_name)]
    data.section <- paste0(unlist(lapply(names(variables.list), function(form.name) {
        form.data <- variables.list[[form.name]]
        form.list <- split(form.data, form.data$field_label)
        form.list <- form.list[unique(form.data$field_label)]
        instrument.name <- form.name |>
            stringr::str_remove("_v\\d+") |>
            stringr::str_replace_all("_", " ") |>
            stringr::str_to_sentence()
        if (any(vapply(exclude.forms, function(form) {
            startsWith(tolower(instrument.name), tolower(form))
        }, logical(1)))) {
            return(NULL)
        }
        instrument.heading <- paste0("### ", instrument.name)
        instrument.items <- unique(unlist(lapply(form.list, function(variable) {
            clean.option <- function(level) {
                # REDCap choices are "<value>, <label>". Drop the numeric value
                # so choice values (for example 1110 for ICD-10 codes or 999 for
                # "Not known") do not leak into the rendered list.
                parts <- strsplit(level, ",", fixed = TRUE)[[1]]
                if (length(parts) < 2) {
                    label <- trimws(level)
                } else {
                    value <- trimws(parts[1])
                    label <- if (grepl("^[0-9]+$", value)) {
                        trimws(paste(parts[-1], collapse = ","))
                    } else {
                        trimws(level)
                    }
                }
                # Options sometimes carry their own leading number (for example
                # "1. Very Fit" or "999. Not known"). Drop it so the displayed
                # options do not show double numbering.
                sub("^[0-9]+\\.\\s*", "", label)
            }
            format.option.list <- function(levels) {
                labels <- vapply(levels, clean.option, character(1), USE.NAMES = FALSE)
                paste0(paste0("  - ", labels), collapse = "\n")
            }
            is.icd.list <- function(labels) {
                # A list is treated as ICD-10 codes when most options start
                # with a code (letter followed by two digits).
                non.empty <- labels[nzchar(labels)]
                length(non.empty) > 0 &&
                    mean(grepl("^[A-Z][0-9]{2}", non.empty)) >= 0.8
            }
            format.icd.summary <- function(labels) {
                # Summarize an ICD-10 code list as code ranges by chapter
                # letter with counts, e.g. "S00-S99 (692), T00-T74 (301)".
                codes <- stringr::str_extract(labels, "^[A-Z][0-9]{2}")
                codes <- codes[!is.na(codes)]
                letters <- substr(codes, 1, 1)
                range.text <- vapply(sort(unique(letters)), function(letter) {
                    group.codes <- codes[letters == letter]
                    paste0(
                        min(group.codes), "\u2013", max(group.codes),
                        " (", format(length(group.codes), big.mark = ","), ")"
                    )
                }, character(1))
                paste0(
                    format(length(labels), big.mark = ","),
                    " ICD-10 codes: ",
                    paste(range.text, collapse = ", "),
                    ". The full list is available in the eCRF."
                )
            }
            first.option.match <- function(label, option.labels) {
                # Find the earliest position at which any option label occurs in
                # the field label. Some fields embed the option definitions in
                # the label (instructions to the coder), after the variable
                # name; this position separates the two.
                best <- NA_integer_
                for (option.label in option.labels) {
                    core <- sub("[^[:alnum:]_]+$", "", option.label)
                    if (!nzchar(core)) {
                        next
                    }
                    match.position <- regexpr(
                        paste0("\\b", stringr::str_escape(core), "\\b"),
                        label,
                        ignore.case = TRUE
                    )
                    if (match.position[1] > 0 && (is.na(best) || match.position[1] < best)) {
                        best <- match.position[1]
                    }
                }
                best
            }
            variable.levels <- NULL
            if (any(c("checkbox", "radio", "dropdown") %in% variable$field_type)) {
                variable.levels <- unlist(strsplit(
                    variable$select_choices_or_calculations,
                    "|",
                    fixed = TRUE
                ))
            }
            field.label <- sub(
                "^[0-9]+(\\.[0-9]+)*\\.?\\s+",
                "",
                trimws(gsub("<.*?>", "", variable$field_label))
            )
            if (!is.null(variable.levels)) {
                # Drop option definitions embedded in the label, keeping only
                # the text up to the first option name.
                option.labels <- vapply(variable.levels, clean.option, character(1), USE.NAMES = FALSE)
                cut <- first.option.match(field.label, option.labels)
                if (!is.na(cut) && cut > 1) {
                    field.label <- sub("[[:space:]]+$", "", substr(field.label, 1, cut - 1))
                }
            }
            paste0(
                "- **", field.label, "** ",
                variable$field_note, "\n",
                if (!is.null(variable.levels)) {
                    option.labels <- vapply(variable.levels, clean.option, character(1), USE.NAMES = FALSE)
                    if (length(variable.levels) > 20 && is.icd.list(option.labels)) {
                        paste0("  - ", format.icd.summary(option.labels))
                    } else if (length(variable.levels) > 20) {
                        # Very long non-ICD option lists would bloat the
                        # document. Show the first and last five options with
                        # an indicator in between.
                        n.levels <- length(variable.levels)
                        first.labels <- option.labels[1:5]
                        last.labels <- option.labels[(n.levels - 4):n.levels]
                        n.omitted <- n.levels - 10
                        omitted.label <- if (n.omitted == 1) {
                            "1 more option"
                        } else {
                            paste0(format(n.omitted, big.mark = ","), " more options")
                        }
                        paste0(
                            paste0(paste0("  - ", first.labels), collapse = "\n"),
                            "\n  … ", omitted.label, "\n",
                            paste0(paste0("  - ", last.labels), collapse = "\n")
                        )
                    } else {
                        format.option.list(variable.levels)
                    }
                } else if (any("yesno" %in% variable$field_type)) {
                    paste0("  - Yes\n  - No")
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
