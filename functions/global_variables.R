global_variables <- function() {
    ## Define functions
    `%m+%` <- lubridate::`%m+%`
    ymd <- lubridate::ymd
    month <- lubridate::month
    year <- lubridate::year

    ## Define global variables
    trial.start.date <- "2024-01-01"
    regulatory.approval.months <- 4
    ethical.approval.months <- 6
    follow.up.months <- 3
    clusters <- 30
    observations.per.month <- 12
    sequences <- 5
    batches <- 6
    batches.overlap.months <- 6
    min.standard.care.months <- 4
    min.intervention.months <- 4
    transition.months <- 1
    transition.overlap.months <- 0
    total.months <- min.standard.care.months +
        sequences * (transition.months - transition.overlap.months) +
        min.intervention.months
    patient.enrollment.months <- total.months
    final.patient.follow.up.date <- ymd(trial.start.date) %m+%
        months(ethical.approval.months) %m+%
        months(batches * total.months - (batches - 1) * batches.overlap.months) %m+%
        months(follow.up.months)
    wrap.up.months <- round(as.numeric(difftime(ymd("2028-12-31"), final.patient.follow.up.date)) / 30.5)
    final.patient.follow.up <- paste0(
        month(final.patient.follow.up.date, label = TRUE, abbr = FALSE),
        " ",
        year(final.patient.follow.up.date)
    )
    clusters.per.sequence <- clusters / batches / sequences
    sample.size <- clusters * observations.per.month * (total.months - transition.months)

    ## Return global variables
    global.variables <- list(
        trial.start.date = trial.start.date,
        regulatory.approval.months = regulatory.approval.months,
        ethical.approval.months = ethical.approval.months,
        follow.up.months = follow.up.months,
        clusters = clusters,
        observations.per.month = observations.per.month,
        sequences = sequences,
        batches = batches,
        batches.overlap.months = batches.overlap.months,
        min.standard.care.months = min.standard.care.months,
        min.intervention.months = min.intervention.months,
        transition.months = transition.months,
        transition.overlap.months = transition.overlap.months,
        total.months = total.months,
        patient.enrollment.months = patient.enrollment.months,
        final.patient.follow.up.date = final.patient.follow.up.date,
        wrap.up.months = wrap.up.months,
        final.patient.follow.up = final.patient.follow.up,
        clusters.per.sequence = clusters.per.sequence,
        sample.size = sample.size
    )
    return(global.variables)
}
