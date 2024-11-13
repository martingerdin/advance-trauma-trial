#' Get trial design data
#'
#' This function returns the trial design data to use in the trial
#' design figure and design matrix for power calculations.
#'
#' @param clusters Numeric. Number of clusters in the trial. Must be a
#'     length 1 numeric value greater than 0. Default is 60.
#' @param sequences Numeric. Number of treatment sequences in the
#'     trial. Must be a length 1 numeric value greater than 0. Default
#'     is 5.
#' @param batches Numeric. Number of batches in the trial. Must be a
#'     length 1 numeric value greater than 0. Default is 6.
#' @param min.standard.care.months Numeric. Minimum number of months
#'     in standard care phase. Must be a length 1 numeric value
#'     greater than or equal to 0. Default is 1.
#' @param min.intervention.months Numeric. Minimum number of months in
#'     intervention phase. Must be a length 1 numeric value greater
#'     than or equal to 0. Default is 1.
#' @param batches.overlap.months Numeric. Number of months for overlap
#'     between batches. Must be a length 1 numeric value greater than
#'     or equal to 0. Default is 0.
#' @param transition.months Numeric. Number of months for the
#'     transition phase. Must be a length 1 numeric value greater than
#'     0. Default is 2.
#' @param transition.overlap.months Numeric. Number of months of
#'     overlap between transitions periods across clusters. Must be a
#'     length 1 numeric value. A negative overlap means that there
#'     will be a gap between transition periods. Default is 1.
#' @param start.month Numeric. The month to start the trial. Must be a
#'     length 1 numeric value greater than or equal to 0. Default is 0.
#' @param total.months Numeric. Total length of the batch in
#'     months. Must be a length 1 numeric value greater than
#'     0. Default is 8.
#' @param staircase.months Numeric. Number of months before and after
#'     the transition phase to include in the staircase design. Must be
#'     a length 1 numeric value greater than or equal to 0. Default is
#'     0.
#'
#' @return A data.frame containing the trial design data.
#'
#' @examples
#' get_trial_design_data()
#'
#' @import assertthat
#' @export
get_trial_design_data <- function(clusters = 60,
                                  sequences = 5,
                                  batches = 6,
                                  min.standard.care.months = 1,
                                  min.intervention.months = 1,
                                  batches.overlap.months = 0,
                                  transition.months = 2,
                                  transition.overlap.months = 1,
                                  start.month = 0,
                                  total.months = 8,
                                  staircase.months = 0) {
  ## Check arguments
  assertthat::assert_that(is.numeric(clusters) && length(clusters) == 1 && clusters > 0)
  assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
  assertthat::assert_that(is.numeric(batches) && length(batches) == 1 && batches > 0)
  assertthat::assert_that(is.numeric(batches.overlap.months) && length(batches.overlap.months) == 1 && batches.overlap.months >= 0)
  assertthat::assert_that(is.numeric(transition.months) && length(transition.months) == 1 && transition.months > 0)
  assertthat::assert_that(is.numeric(transition.overlap.months) && length(transition.overlap.months) == 1)
  assertthat::assert_that(is.numeric(start.month) && length(start.month) == 1 && start.month >= 0)
  assertthat::assert_that(is.numeric(total.months) && length(total.months) == 1 && total.months > 0)
  assertthat::assert_that(is.numeric(staircase.months) && length(staircase.months) == 1 && staircase.months >= 0)

  ## Create trial design data
  total.months <- min.standard.care.months + sequences * (transition.months - transition.overlap.months) + transition.overlap.months + min.intervention.months
  clusters.per.batch <- clusters / batches
  trial.design.data <- do.call(rbind, lapply(1:batches, function(batch) {
    start.cluster <- if (batch == 1) 1 else batch * clusters.per.batch - clusters.per.batch + 1
    batch.data <- data.frame(cluster = start.cluster:(start.cluster + clusters.per.batch - 1))
    batch.data$sequence <- rep(1:sequences, each = clusters.per.batch / sequences)
    batch.data$batch <- batch
    standard.care.start <- if (batch == 1) start.month else (batch - 1) * total.months - (batch - 1) * batches.overlap.months
    batch.data$standard.care.start <- standard.care.start
    batch.data$transition.start <- rep(
      seq(standard.care.start + min.standard.care.months,
        by = transition.months - transition.overlap.months,
        length.out = sequences
      ),
      each = clusters.per.batch / sequences
    )
    batch.data$standard.care.end <- batch.data$transition.start
    batch.data$transition.end <- batch.data$transition.start + transition.months
    batch.data$intervention.start <- batch.data$transition.end
    batch.data$intervention.end <- max(batch.data$transition.end) + min.intervention.months
    if (staircase.months > 0) {
      batch.data$pre.transition.staircase.start <- batch.data$standard.care.end - staircase.months
      batch.data$pre.transition.staircase.end <- batch.data$standard.care.end
      batch.data$post.transition.staircase.start <- batch.data$transition.end
      batch.data$post.transition.staircase.end <- batch.data$transition.end + staircase.months
    }
    if (staircase.months > 0) {
      reshape(batch.data,
        idvar = "cluster",
        timevar = "phase",
        times = c("Standard care", "Transition", "Intervention", "Pre-transition staircase", "Post-transition staircase"),
        varying = list(
          c(
            "standard.care.start",
            "transition.start",
            "intervention.start",
            "pre.transition.staircase.start",
            "post.transition.staircase.start"
          ),
          c(
            "standard.care.end",
            "transition.end",
            "intervention.end",
            "pre.transition.staircase.end",
            "post.transition.staircase.end"
          )
        ),
        v.names = c("start", "end"),
        direction = "long"
      )
    } else {
      reshape(batch.data,
        idvar = "cluster",
        timevar = "phase",
        times = c("Standard care", "Transition", "Intervention"),
        varying = list(
          c(
            "standard.care.start",
            "transition.start",
            "intervention.start"
          ),
          c(
            "standard.care.end",
            "transition.end",
            "intervention.end"
          )
        ),
        v.names = c("start", "end"),
        direction = "long"
      )
    }
  }))
  return(trial.design.data)
}
