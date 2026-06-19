#' Get a summary specification for a REDCap field
#'
#' Looks a field up in a REDCap data dictionary (metadata) and returns a small
#' specification describing how it should be summarised: whether it is
#' categorical or continuous, and, for categorical fields, its response options.
#' This wraps the generic dictionary-parsing logic so that table-building
#' functions can derive their rows and levels directly from REDCap.
#'
#' REDCap field types are mapped as follows: `radio`, `dropdown`, `checkbox` and
#' `yesno` are treated as categorical (with `yesno` given Yes/No levels), and all
#' other field types (e.g. numeric `text` or calculated fields) are treated as
#' continuous.
#'
#' @param data A data frame. A REDCap data dictionary (metadata), as returned by
#'     `get_redcap_data(content = "metadata")`. Must contain the columns
#'     `field_name`, `field_type` and `select_choices_or_calculations`. No
#'     default.
#' @param field.name Character. The name of the field to look up. No default.
#' @return A list with elements `field_name` (the field name), `redcap_type`
#'     (the raw REDCap field type), `type` (either "categorical" or
#'     "continuous") and `levels` (a character vector of response option labels,
#'     empty for continuous fields). Returns NULL, with a warning, if the field
#'     is not present in the dictionary.
get_redcap_field_specification <- function(data, field.name) {
    ## Check arguments
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.character(field.name) && length(field.name) == 1)
    required.columns <- c("field_name", "field_type", "select_choices_or_calculations")
    assertthat::assert_that(
        all(required.columns %in% names(data)),
        msg = paste0(
            "`data` must be a REDCap data dictionary containing the columns: ",
            paste(required.columns, collapse = ", "), "."
        )
    )

    ## Look the field up in the dictionary
    metadata <- data[data$field_name == field.name, , drop = FALSE]
    if (nrow(metadata) == 0) {
        warning("Field '", field.name, "' was not found in the REDCap data dictionary.")
        return(NULL)
    }
    redcap.type <- metadata$field_type[1]

    ## Map the REDCap field type to a summary type and derive the levels
    if (redcap.type %in% c("radio", "dropdown", "checkbox")) {
        type <- "categorical"
        levels <- parse_redcap_choices(metadata$select_choices_or_calculations[1])
    } else if (redcap.type == "yesno") {
        type <- "categorical"
        levels <- c("Yes", "No")
    } else {
        type <- "continuous"
        levels <- character(0)
    }

    return(list(
        field_name = field.name,
        redcap_type = redcap.type,
        type = type,
        levels = levels
    ))
}
