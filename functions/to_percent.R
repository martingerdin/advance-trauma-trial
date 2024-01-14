to_percent <- function(proportion) {
    assertthat::assert_that(proportion >= 0 & proportion <= 1)
    round(proportion * 100)
}
