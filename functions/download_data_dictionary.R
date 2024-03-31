download_data_dictionary <- function() {
    library(dotenv)
    token <- Sys.getenv("REDCAP_API_TOKEN")
    url <- "https://redcap.georgeinstitute.org.in/api/"
    response <- httr2::request(url) |>
        httr2::req_body_form(
            "token" = token,
            content = "metadata",
            format = "json",
            returnFormat = "json"
        ) |>
        httr2::req_perform()
    result <- response |>
        httr2::resp_body_json()
    data.dictionary <- do.call(rbind, lapply(result, as.data.frame))
    return(data.dictionary)
}
