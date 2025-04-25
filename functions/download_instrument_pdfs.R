download_instrument_pdfs <- function(download.destination = getwd()) {
    library(dotenv)
    token <- Sys.getenv("REDCAP_API_TOKEN")
    url <- "https://redcap.georgeinstitute.org.in/api/"
    response <- httr2::request(url) |>
        httr2::req_body_form(
            token = token,
            binary = TRUE,
            content = "pdf",
            returnFormat = "json",
            compactDisplay = "true"
        ) |>
        httr2::req_perform()
    result <- response |>
        httr2::resp_body_raw()
    f <- file(file.path(download.destination, "all-instruments.pdf"), "wb")
    writeBin(as.vector(result), f)
    close(f)
}
