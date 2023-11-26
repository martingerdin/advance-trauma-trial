#' Create a trial time plan
#'
#' This function creates a time plan for a clinical trial, specifying
#' the start and end dates for each task involved.
#' 
#' @param trial.start.date The start date of the clinical trial in
#'     "YYYY-MM-DD" format. Defaults to "2024-01-01".
#' @param batches The number of batches. Defaults to 6.
#' @param regulatory.approval.months The number of months estimated for
#'     obtaining approval from the HMS committee. Defaults to 4.
#' @param ethical.approval.months The number of months estimated for
#'     obtaining ethical approval. Defaults to 6.
#' @param patient.enrollment.months The number of months estimated for
#'     enrolling patients. Defaults to 12.
#' @param follow.up.months The number of months estimated for patient
#'     follow-up. Defaults to 6.
#' @param batches.overlap.months The number of months to overlap
#'     between batches. Defaults to 6.
#' @param wrap.up.months The number of months estimated for wrapping
#'     up the clinical trial. Defaults to 4.
#' @param save Whether to save the output as a CSV file. Defaults to
#'     TRUE.
#' @export
create_trial_time_plan <- function(trial.start.date = "2024-01-01",
                                   batches = 6, 
                                   regulatory.approval.months = 4,
                                   ethical.approval.months = 6,
                                   patient.enrollment.months = 12,
                                   follow.up.months = 6,
                                   batches.overlap.months = 6,
                                   wrap.up.months = 4,
                                   save = TRUE) {
    library(ggplot2)
    library(lubridate)
    library(stringr)
    library(dplyr)
    
    obtain.regulatory.approval <- list(start = ymd(trial.start.date))
    obtain.regulatory.approval$end <- obtain.regulatory.approval$start %m+% months(regulatory.approval.months)

    ## 1st batch
    obtain.ethical.approval.batch.1 <- list(start = obtain.regulatory.approval$start)
    obtain.ethical.approval.batch.1$end <- obtain.ethical.approval.batch.1$start %m+% months(ethical.approval.months)

    enrollment.and.follow.up.batch.1 <- list(start = obtain.ethical.approval.batch.1$end)
    enrollment.and.follow.up.batch.1$end <- enrollment.and.follow.up.batch.1$start %m+% months(patient.enrollment.months + follow.up.months)

    ## Generate data for subsequent batches
    batch.list <-
        list(batch.1 = list(
                 enrollment.and.follow.up = enrollment.and.follow.up.batch.1,
                 obtain.ethical.approval = obtain.ethical.approval.batch.1
                 ))

    for (batch.id in 2:batches) {
        ## Create new list for patient enrollment and follow up
        previous.list <- batch.list[[paste0("batch.", batch.id - 1)]]
        enrollment.and.follow.up <- list(
            start = previous.list$enrollment.and.follow.up$end %m-%
                months(batches.overlap.months + follow.up.months))
        enrollment.and.follow.up$end <- enrollment.and.follow.up$start %m+%
            months(patient.enrollment.months + follow.up.months)

        ## And then for ethical approval
        obtain.ethical.approval <- list(start = enrollment.and.follow.up$start %m-%
                months(ethical.approval.months))
        obtain.ethical.approval$end <- obtain.ethical.approval$start %m+%
            months(ethical.approval.months)

        ## Combine these lists
        batch.data <- list(enrollment.and.follow.up = enrollment.and.follow.up,
                           obtain.ethical.approval = obtain.ethical.approval)
        batch.list[[paste0("batch.", batch.id)]] <- batch.data
    }

    ## Wrap up
    wrap.up.and.disseminate.findings <- list(start = batch.list[[batches]]$enrollment.and.follow.up$end)
    wrap.up.and.disseminate.findings$end <-  wrap.up.and.disseminate.findings$start %m+% months(wrap.up.months)
    
    ## Combine all task data
    task.data.list <- c(obtain.regulatory.approval = list(obtain.regulatory.approval),
                        unlist(batch.list, recursive = FALSE),
                        wrap.up.and.disseminate.findings = list(wrap.up.and.disseminate.findings))

    ## Create plot data
    plot.data <- do.call(rbind, lapply(task.data.list, function(list.element) data.frame(start = list.element$start, end = list.element$end)))
    plot.data$task <- str_to_sentence(gsub(".", " ", rownames(plot.data), fixed = TRUE))
    rownames(plot.data) <- NULL
    plot.data <- plot.data %>% 
        mutate(duration = as.numeric(end - start) + 1)
    plot.data <- plot.data[order(plot.data$start), ]
    plot.data$task <- factor(plot.data$task, levels = plot.data$task)
    plot.data.compressed <- plot.data
    plot.data.compressed$task <- gsub(" \\d+\\w{2} batch", "", plot.data.compressed$task)

    ## Create plot
    time.plan <- ggplot(plot.data, aes(x = start, xend = end, y = task, yend = task)) +
        geom_segment(linewidth = 2) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        ylab("Task") +
        xlab("Year") +
        theme_bw()

    ## Save plot to disk
    if (save)
        ggsave("time-plan.pdf", time.plan, width = 18, height = 7, units = "cm")
}
