release <- function(major = NULL, minor = NULL, patch = NULL) {
    # Define borrowed functions
    assert_that <- assertthat::assert_that
    `%>%` <- magrittr::`%>%`

    # Prompt for confirmation and set patch to TRUE and major and minor to FALSE if all are NULL
    if (is.null(major) && is.null(minor) && is.null(patch)) {
        message <- "No version increment specified. Increment patch version?"
        patch <- utils::menu(c("Yes", "No"), title = message, graphics = FALSE) == 1
        major <- FALSE
        minor <- FALSE
    } else {
        major <- ifelse(is.null(major), FALSE, major)
        minor <- ifelse(is.null(minor), FALSE, minor)
        patch <- ifelse(is.null(patch), FALSE, patch)
    }

    # Check arguments
    assert_that(major || minor || patch, msg = "At least one of major, minor or patch must be TRUE")

    assert_that(!(major && minor), msg = "major and minor cannot both be TRUE")
    assert_that(!(major && patch), msg = "major and patch cannot both be TRUE")
    assert_that(!(minor && patch), msg = "minor and patch cannot both be TRUE")

    # Read current version from _variables.yml
    description <- yaml::read_yaml("_variables.yml")
    version <- description$version %>%
        stringr::str_split("\\.") %>%
        unlist() %>%
        as.numeric()

    # Increment version
    if (major) {
        version[1] <- version[1] + 1
        version[2] <- 0
        version[3] <- 0
    } else if (minor) {
        version[2] <- version[2] + 1
        version[3] <- 0
    } else if (patch) {
        version[3] <- version[3] + 1
    }
    new.version.string <- paste0(version[1], ".", version[2], ".", version[3])

    # Update version
    description$version <- new.version.string

    # Update date
    description$date <- as.character(lubridate::today())

    # Write description
    yaml::write_yaml(description, "_variables.yml")

    # Compile protocol
    quarto::quarto_render("protocol.qmd", output_format = "all")

    # Commit changes
    base.git.path <- git2r::discover_repository(".") %>%
        stringr::str_remove("/.git")
    path <- file.path(base.git.path, "atls-vs-standard-care-trial")
    git2r::add(repo = ".", path = path)
    git2r::commit(repo = ".", message = paste0("Release protocol version ", new.version.string))
}
