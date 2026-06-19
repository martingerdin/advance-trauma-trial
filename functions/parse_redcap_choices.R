#' Parse a REDCap choices string
#'
#' Parses a REDCap "select choices or calculations" string of the form
#' "1, Label one | 2, Label two | ..." (as used by radio, dropdown and checkbox
#' fields) into the response option labels, optionally keyed by their codes.
#'
#' @param choices Character. A single REDCap choices string. No default.
#' @param named Logical. If TRUE a named character vector is returned, with the
#'     option codes as names and the option labels as values. If FALSE (the
#'     default) an unnamed character vector of labels is returned.
#' @return A character vector of option labels (named by code if `named` is
#'     TRUE). Options with an empty label are dropped.
parse_redcap_choices <- function(choices, named = FALSE) {
    ## Check arguments
    assertthat::assert_that(is.character(choices) && length(choices) == 1)
    assertthat::assert_that(is.logical(named) && length(named) == 1)

    ## Split into individual "code, label" options
    options <- trimws(strsplit(choices, "|", fixed = TRUE)[[1]])
    options <- options[nzchar(options)]

    ## Separate each option into its code (before the first comma) and its label
    ## (everything after the first comma)
    codes <- trimws(sub(",.*$", "", options))
    labels <- trimws(sub("^[^,]*,", "", options))

    ## Drop options without a label
    keep <- nzchar(labels)
    codes <- codes[keep]
    labels <- labels[keep]

    if (named) {
        return(stats::setNames(labels, codes))
    }
    return(unname(labels))
}
