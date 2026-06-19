#' Get data from REDCap
#'
#' Exports data from a REDCap project using the REDCap API. The project URL and
#' API token are read from environment variables (typically defined in a
#' project-level `.env` file at the trial repository root) so that secrets are
#' never hard-coded. By default the function exports records, but any API
#' content type can be requested via `content`; for example
#' `content = "metadata"` exports the data dictionary.
#'
#' @param url.name Character. Name of the environment variable holding the
#'     REDCap API URL. No default.
#' @param api.key.name Character. Name of the environment variable holding the
#'     REDCap API token. No default.
#' @param content Character. The REDCap API content type to export, for example
#'     "record" (the default) or "metadata".
#' @param project.id Character or NULL. Optional human-readable identifier for
#'     the project, used only in messages. Defaults to NULL.
#' @param ... Additional name-value pairs passed as fields to the REDCap API,
#'     for example `rawOrLabel = "label"` or `forms = "screening_call"`.
#' @return A data frame.
get_redcap_data <- function(url.name,
                            api.key.name,
                            content = "record",
                            project.id = NULL,
                            ...) {
    ## Check arguments
    assertthat::assert_that(is.character(url.name) && length(url.name) == 1)
    assertthat::assert_that(is.character(api.key.name) && length(api.key.name) == 1)
    assertthat::assert_that(is.character(content) && length(content) == 1)
    assertthat::assert_that(is.null(project.id) || (is.character(project.id) && length(project.id) == 1))

    ## Read the URL and token from the environment, loading a .env file if they
    ## are not already set. The .env file lives at the trial repository root,
    ## which may be one or more levels above the working directory when
    ## documents are rendered.
    if (!nzchar(Sys.getenv(url.name)) || !nzchar(Sys.getenv(api.key.name))) {
        for (candidate in c(".env", "../.env", "../../.env")) {
            if (file.exists(candidate)) {
                readRenviron(candidate)
                break
            }
        }
    }
    url <- Sys.getenv(url.name)
    token <- Sys.getenv(api.key.name)
    if (!nzchar(url))
        stop("No REDCap URL found in the environment variable '", url.name, "'.")
    if (!nzchar(token))
        stop("No REDCap API token found in the environment variable '", api.key.name, "'.")

    ## Build and perform the API request
    fields <- c(
        list(
            token = token,
            content = content,
            format = "json",
            returnFormat = "json"
        ),
        list(...)
    )
    request <- httr2::request(url)
    request <- do.call(httr2::req_body_form, c(list(request), fields))
    response <- httr2::req_perform(request)
    result <- httr2::resp_body_json(response)

    ## Coerce the list of records into a data frame, tolerating records that
    ## populate different sets of fields.
    data <- do.call(dplyr::bind_rows, lapply(result, function(record) {
        as.data.frame(
            lapply(record, function(value) if (length(value) == 0) NA else unlist(value)),
            stringsAsFactors = FALSE,
            check.names = FALSE
        )
    }))
    return(as.data.frame(data))
}
