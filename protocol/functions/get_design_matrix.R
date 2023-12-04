#' Returns the design matrix
#'
#' Function that returns the a design matrix for a batched stepped
#' wedge cluster randomised trial.
#' 
#' @param clusters Numeric. Number of clusters in the trial. Must be a
#'     length 1 numeric value greater than 0. Default is 60.
#' @param sequences Numeric. Number of treatment sequences in the
#'     trial. Must be a length 1 numeric value greater than 0. Default
#'     is 5.
#' @param batches Numeric. Number of batches in the trial. Must be a
#'     length 1 numeric value greater than 0. Default is 6.
#' @param min.standard.care.months Numeric. Minimum number of months
#'     for the standard care phase. Must be a length 1 numeric value
#'     greater than or equal to 1. Default is 1.
#' @param min.intervention.months Numeric. Minimum number of months
#'     for the intervention phase. Must be a length 1 numeric value
#'     greater than or equal to 1. Default is 1.
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
#' @param total.months Numeric. Total length of the batch in
#'     months. Must be a length 1 numeric value greater than
#'     0. Default is 8.
#' @param one.row.per.sequence Logical. If TRUE each row is a
#'     sequence, otherwise each row is a cluster. Defaults to FALSE.
#' @param save Logical. If TRUE the design matrix is saved to disk as
#'     a csv file. Defaults to TRUE.
#'
#' @return A data.frame containing the design matrix.
#'
#' @examples
#' get_design_matrix()
#'
#' @import assertthat
#' @export
get_design_matrix <- function(clusters = 60,
                              sequences = 5,
                              batches = 6,
                              min.standard.care.months = 1,
                              min.intervention.months = 1,
                              batches.overlap.months = 0,
                              transition.months = 2,
                              transition.overlap.months = 1,
                              total.months = 8,
                              one.row.per.sequence = FALSE,
                              save = TRUE) {
    ## Check arguments
    assertthat::assert_that(is.numeric(clusters) && length(clusters) == 1 && clusters > 0)
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.numeric(batches) && length(batches) == 1 && batches > 0)
    assertthat::assert_that(is.numeric(batches.overlap.months) && length(batches.overlap.months) == 1 && batches.overlap.months >= 0)
    assertthat::assert_that(is.numeric(transition.months) && length(transition.months) == 1 && transition.months > 0)
    assertthat::assert_that(is.numeric(transition.overlap.months) && length(transition.overlap.months) == 1)
    assertthat::assert_that(is.numeric(total.months) && length(total.months) == 1 && total.months > 0)
    
    ## Get trial design data
    trial.design.data <- get_trial_design_data(clusters = clusters,
                                               sequences = sequences,
                                               batches = batches,
                                               min.standard.care.months = min.standard.care.months,
                                               min.intervention.months = min.intervention.months, 
                                               batches.overlap.months = batches.overlap.months, 
                                               transition.months = transition.months,
                                               transition.overlap.months = transition.overlap.months,
                                               total.months = total.months)

    ## Modify trial.design.data to have index period 1
    trial.design.data$start <- trial.design.data$start + 1

    ## Use trial design data to identify number of cluster per
    ## sequence
    clusters.per.sequence <- length(clusters)/batches/sequences
    
    ## Iterate over clusters to create design matrix rows. There
    ## should be one cluster per row.
    design.matrix <- design.matrix.template <- matrix(ncol = max(trial.design.data$end), nrow = clusters)
    for (cluster.id in seq_len(clusters)) {
        long.cluster.data <- trial.design.data[trial.design.data$cluster == cluster.id, ]
        for (row.id in seq_len(nrow(long.cluster.data))) {
            row.data <- long.cluster.data[row.id, ]
            phase <- row.data$phase
            if (phase == "Standard care") {
                fill <- 0
            } else if (phase == "Intervention") {
                fill <- 1
            } else {
                fill <- NA
            }
            design.matrix[cluster.id, row.data$start:row.data$end] <- fill
        }
    }
    
    ## Keep only one row per sequence
    if (one.row.per.sequence) {
        rows.to.keep <- rep(c(TRUE, rep(FALSE, clusters.per.sequence - 1)), sequences*batches)
        design.matrix <- design.matrix[rows.to.keep, ]        
    }

    ## Save design matrix to disk
    file.name <- paste0("design-matrix-",
                        clusters, "-clusters-",
                        sequences, "-sequences-",
                        batches, "-batches-",
                        batches.overlap.months, "-batches-overlap-",
                        min.standard.care.months, "-min-standard-care-",
                        min.intervention.months, "-min-intervention-",
                        transition.months, "-transition-months-",
                        transition.overlap.months, "-transition-overlap.csv")

    write.table(design.matrix, file.name, sep = ",", row.names = FALSE, col.names = FALSE)
    
    ## Return design matrix
    return (design.matrix)
}
