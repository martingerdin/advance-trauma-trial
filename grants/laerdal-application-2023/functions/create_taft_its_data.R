#' Create TAFT ITS data
#'
#' This function gets the Trauma Audit Filters Trial (TAFT) data and
#' outputs a dataset for generating Interrupted Time Series (ITS)
#' estimates. Has to be run on the nagratal server.
create_taft_its_data <- function() {
    library(dotenv)
    taft <- readr::read_csv(paste0(Sys.getenv("DATA_DIR"), "taft/current/dataset.csv"))
    taft$in.hospital.mortality <- taft$hd == 1
    taft$doi[taft$doi == "999" | taft$doi == "22"] <- NA
    taft$toi[taft$toi == "999"] <- NA
    taft$doar[taft$doar == "999"] <- NA
    taft$toar[taft$toar == "999"] <- NA
    taft$doi.toi <- with(taft, paste(doi, toi))
    taft$doi.toi[grep("NA", taft$doi.toi)] <- NA
    taft$doar.toar <- with(taft, paste(doar, toar))
    taft$doar.toar[grep("NA", taft$doar.toar)] <- NA
    taft$doar.toar <- as.POSIXct(taft$doar.toar)
    taft$delay <- as.numeric(with(taft, difftime(as.POSIXct(doar.toar), as.POSIXct(doi.toi), units = "hours")))
    taft <- taft[taft$delay < 48, ]
    taft <- taft[, c("centre", "delay", "retro", "doar.toar", "in.hospital.mortality")]
    taft <- taft[taft$centre != 4178, ]
    taft <- na.omit(taft)
    taft$month <- format(taft$doar.toar, "%Y-%m")
    its.data <- do.call(rbind, lapply(split(taft, taft$month), function(month.data) {
        data.frame(in.hospital.mortality = mean(month.data$in.hospital.mortality),
                   n.observations = nrow(month.data))
    }))
    its.data$month <- rownames(its.data)
    rownames(its.data) <- NULL
    saveRDS(its.data, "taft-its-data.Rds")
}
