number_to_text <- function(number) {
    if (number < 12)
        c("one", "two", "three", "four", "five", "six", "seven",
          "eight", "nine", "ten", "eleven")[number]
    else number
}
