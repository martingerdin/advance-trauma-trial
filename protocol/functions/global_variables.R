global_variables <- function() {
    trial.start.date <- "2024-01-01"
    regulatory.approval.months <- 4
    ethical.approval.months <- 4
    follow.up.months <- 3
    hospitals <- 5
    observations.per.week <- 20
    hospitals <- 5L                 
    pre.training.phase.months <- 12L 
    training.phase.months <- 4L
    post.training.phase.months <- 24L
    total.months <- pre.training.phase.months + training.phase.months + post.training.phase.months
    patient.enrollment.months <- total.months
    sample.size <- hospitals * observations.per.week * total.months * 4
    final.patient.follow.up.date <- ymd(trial.start.date) %m+%
        months(ethical.approval.months) %m+%
        months(patient.enrollment.months) %m+%
        months(follow.up.months)
    wrap.up.months <- round(as.numeric(difftime(ymd("2027-12-31"), final.patient.follow.up.date))/30.5)
    final.patient.follow.up <- paste0(month(final.patient.follow.up.date, label = TRUE, abbr = FALSE),
                                      " ",
                                      year(final.patient.follow.up.date))
    global.variables <- list(trial.start.date = trial.start.date,
                             regulatory.approval.months = regulatory.approval.months,
                             ethical.approval.months = ethical.approval.months,
                             follow.up.months = follow.up.months,
                             hospitals = hospitals,
                             observations.per.week = observations.per.week,
                             pre.training.phase.months = pre.training.phase.months,
                             training.phase.months = training.phase.months,
                             post.training.phase.months = post.training.phase.months,
                             total.months = total.months,
                             patient.enrollment.months = patient.enrollment.months,
                             sample.size = sample.size,
                             final.patient.follow.up.date = final.patient.follow.up.date,
                             wrap.up.months = wrap.up.months,
                             final.patient.follow.up = final.patient.follow.up)
    return(global.variables)
}
