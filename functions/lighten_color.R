#' Lighten a color
#'
#' Function to lighten a hex color by blending it with white.
#' Created with assistance from the Cursor IDE.
#'
#' @param color Character string. A hex color code (e.g. "#663996")
#' @param white_amount Numeric between 0 and 1. The amount of white to blend in,
#'     where 0 returns the original color and 1 returns white. Defaults to 0.5.
#' @return Character string. The hex code of the lightened color.
#' @examples
#' lighten_color("#663996", 0.2) # Returns a slightly lighter purple
#' lighten_color("#f6851f", 0.8) # Returns a very light orange
lighten_color <- function(color, white_amount = 0.5) {
  # Check arguments
  assertthat::assert_that(is.character(color), length(color) == 1)
  assertthat::assert_that(grepl("^#[0-9A-Fa-f]{6}$", color),
    msg = "color must be a valid hex color code (e.g. '#663996')"
  )
  assertthat::assert_that(is.numeric(white_amount), length(white_amount) == 1)
  assertthat::assert_that(white_amount >= 0 && white_amount <= 1,
    msg = "white_amount must be between 0 and 1"
  )

  # Convert input hex color to RGB format
  rgb_col <- colorspace::hex2RGB(color)

  # Convert white (#FFFFFF) to RGB format
  white <- colorspace::hex2RGB("#FFFFFF")

  # Calculate weighted average of original color and white based on white_amount
  blended <- (1 - white_amount) * rgb_col@coords + white_amount * white@coords

  # Convert blended RGB values back to hex code and return
  hex_color <- colorspace::RGB(blended[1], blended[2], blended[3]) |>
    colorspace::hex()

  return(hex_color)
}
