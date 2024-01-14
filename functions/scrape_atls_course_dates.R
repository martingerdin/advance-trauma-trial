scrape_atls_course_dates <- function() {
    library(rvest)

    url <- "https://www.atls.in/centre-rml.htm"
    page <- read_html(url)

    dates <- page %>% 
        html_nodes("td[bgcolor='#D21F01']") %>% 
        html_text()

                                        # specify the URL of the website
    url <- "https://www.atls.in/centre-rml.htm"
    page <- read_html(url)

    date_nodes <- page %>% html_nodes("td[bgcolor='#D21F01'] a")

    dates <- data.frame(year = integer(),
                        month = character(),
                        day = integer(),
                        stringsAsFactors = FALSE)

    for (node in date_nodes) {
        date_string <- node %>% html_text()
        href <- node %>% html_attr("href")
        year <- sub(".*/rml-provider-course-calendar-(\\d{4}).pdf", "\\1", href)
        month <- sub(".*/rml-provider-course-calendar-\\d{4}/rml-provider-course-(\\w{3}).pdf", "\\1", href)
        day <- as.numeric(date_string)
        dates <- rbind(dates, data.frame(year = year, month = month, day = day))
    }

    dates <- na.omit(dates)

    ## Extract year from file paths in year column
    dates$year <- gsub(".*-(\\d{4})/.*", "\\1", dates$year)

    ## Extract month from file paths in month column
    dates$month <- gsub(".*-([a-z]{3})\\.pdf", "\\1", dates$month)
}
