release <- function(file.name, major = NULL, minor = NULL, patch = NULL, recompile.only = FALSE, commit = TRUE, tag = TRUE) {
    # Define borrowed functions
    assert_that <- assertthat::assert_that
    `%>%` <- magrittr::`%>%`

    # Check that file is a quarto file
    assert_that(stringr::str_detect(file.name, ".qmd$"), msg = paste0("File ", file.name, " is not a quarto file"))

    # Check that file exists
    assert_that(file.exists(file.name), msg = paste0("File ", file.name, " does not exist"))

    # Prompt for confirmation and set patch to TRUE and major and minor to FALSE if all are NULL
    if (is.null(major) && is.null(minor) && is.null(patch) && recompile.only == FALSE) {
        message <- "No version increment specified. Increment patch version?"
        patch <- utils::menu(c("Yes", "No"), title = message, graphics = FALSE) == 1
        major <- FALSE
        minor <- FALSE
    } else {
        major <- ifelse(is.null(major), FALSE, major)
        minor <- ifelse(is.null(minor), FALSE, minor)
        patch <- ifelse(is.null(patch), FALSE, patch)
    }

    # Check additional arguments
    assert_that(major || minor || patch || recompile.only, msg = "At least one of major, minor, patch or recompile.only must be TRUE")
    assert_that(!(major && minor), msg = "major and minor cannot both be TRUE")
    assert_that(!(major && patch), msg = "major and patch cannot both be TRUE")
    assert_that(!(minor && patch), msg = "minor and patch cannot both be TRUE")
    assert_that(!(recompile.only && (major || minor || patch)), msg = "recompile.only cannot be TRUE if major, minor or patch are TRUE")
    assert_that(is.logical(recompile.only) & length(recompile.only) == 1, msg = "recompile.only must be a logical")
    assert_that(is.logical(commit) & length(commit) == 1, msg = "commit must be a logical")

    # Read current version from _variables.yml
    description <- yaml::read_yaml("_variables.yml")
    version <- description$version %>%
        stringr::str_split("\\.") %>%
        unlist() %>%
        as.numeric()
    new.version.string <- paste0(version, collapse = ".")

    # Update version unless recompile.only is TRUE
    if (!recompile.only) {
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
        new.version.string <- paste0(version, collapse = ".")

        # Update version
        description$version <- new.version.string

        # Update date
        description$date <- as.character(lubridate::today())

        # Write description
        yaml::write_yaml(description, "_variables.yml")
    }

    # Compile file
    quarto::quarto_render(file.name, output_format = "all")

    # Use file.name to define a document.name that can be used in the commit message
    document.name <- stringr::str_replace_all(file.name, "-", " ") %>%
        stringr::str_remove(".qmd") %>%
        stringr::str_to_lower()

    # Commit changes and tag release
    if (commit) {
        base.git.path <- git2r::discover_repository(".") %>%
            stringr::str_remove("/.git")
        path <- file.path(base.git.path, "atls-vs-standard-care-trial")
        git2r::add(repo = ".", path = path)
        git2r::commit(repo = ".", message = paste0("Release ", document.name, " version ", new.version.string))

        if (tag) {
            git2r::tag(
                object = ".",
                name = stringr::str_to_sentence(paste0(document.name, " ", new.version.string)),
                message = paste0("Release ATLS vs standard care trial ", document.name, " version ", new.version.string)
            )
        }
    }
}
