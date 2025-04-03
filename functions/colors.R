colors <- function() {
  base.colors <- c(
    standard.care = "#f6851f",
    transition = "#1c9ebc",
    intervention = "#663996",
    yellow = "#f9b510",
    pink = "#b865a3",
    blue = "#8ecbe8"
  )
  light.colors <- sapply(base.colors, lighten_color, white_amount = 0.2)
  names(light.colors) <- paste0("light.", names(base.colors))
  return(c(base.colors, light.colors))
}
