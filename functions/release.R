release <- function(file.name, major = NULL, minor = NULL, patch = NULL, pre.release = NULL, recompile.only = FALSE, commit = TRUE, tag = TRUE) {
    # Define borrowed functions
    assert_that <- assertthat::assert_that

    # Check that file is a quarto file
    assert_that(stringr::str_detect(file.name, ".qmd$"), msg = paste0("File ", file.name, " is not a quarto file"))

    # Check that file exists
    assert_that(file.exists(file.name), msg = paste0("File ", file.name, " does not exist"))

    # Read current version from _variables.yml
    description <- yaml::read_yaml("_variables.yml")
    current.version <- description$version
    version <- current.version |>
        stringr::str_split("\\.|\\-") |>
        unlist() |>
        as.numeric()
    ## new.version.string <- paste0(version, collapse = ".")

    # Check if the current version is a pre-release
    current.pre.release <- length(version) > 3

    # If the current version is a pre-release, then the new version should either be the next version of the pre-release or the just the version without the pre-release indicator
    if (current.pre.release && is.null(pre.release) && !recompile.only) {
        message <- paste0("The current version is pre-release version ", current.version, ". Increment the pre-release version?")
        pre.release <- utils::menu(c("Yes", "No"), title = message, graphics = FALSE) == 1
    }

    # If the current version is not a pre-release and it is not indicated whether this new version should be, then check
    if (is.null(pre.release) && !recompile.only) {
        message <- paste0("You have not indicated whether this is a pre-release. Is this a pre-release?")
        pre.release <- utils::menu(c("Yes", "No"), title = message, graphics = FALSE) == 1
    }

    # Prompt for confirmation and set patch to TRUE and major and minor to FALSE if all are NULL
    if (is.null(major) &&
        is.null(minor) &&
        is.null(patch) &&
        ((is.null(pre.release) || pre.release) &&
            (is.null(current.pre.release) || !current.pre.release)) &&
        !recompile.only) {
        message <- "No version increment specified. Increment patch version?"
        patch <- utils::menu(c("Yes", "No"), title = message, graphics = FALSE) == 1
        major <- FALSE
        minor <- FALSE
    } else {
        major <- ifelse(is.null(major), FALSE, major)
        minor <- ifelse(is.null(minor), FALSE, minor)
        patch <- ifelse(is.null(patch), FALSE, patch)
        pre.release <- ifelse(is.null(pre.release), FALSE, pre.release)
    }

    # Check additional arguments
    assert_that(
        major ||
            minor ||
            patch ||
            pre.release ||
            current.pre.release ||
            recompile.only,
        msg = "At least one of major, minor, patch, pre.release or recompile.only must be TRUE"
    )
    assert_that(!(major && minor), msg = "major and minor cannot both be TRUE")
    assert_that(!(major && patch), msg = "major and patch cannot both be TRUE")
    assert_that(!(minor && patch), msg = "minor and patch cannot both be TRUE")
    assert_that(!(recompile.only && (major || minor || patch || pre.release)), msg = "recompile.only cannot be TRUE if any of major, minor, patch or pre.release is TRUE")
    assert_that(is.logical(recompile.only) & length(recompile.only) == 1, msg = "recompile.only must be a logical")
    assert_that(is.logical(commit) & length(commit) == 1, msg = "commit must be a logical")

    # If the next version is a pre-release but the current version is not, then major, minor or patch must be TRUE
    if (pre.release && !current.pre.release && !recompile.only) {
        assert_that(major || minor || patch, msg = "If the next version is a pre-release but the current version is not, then major, minor or patch must be TRUE")
    }

    # If the current version is a pre-release but the next version is not, then the new version is the current version without the pre-release indicator
    if (current.pre.release && !pre.release && !recompile.only) {
        new.version.string <- paste0(version[1:3], collapse = ".")
    }

    # Bump version if recompile.only is FALSE, and the current version is not a pre-release, or if the current version is a pre-release and pre.release is TRUE
    if (!recompile.only && !current.pre.release) {
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
        version.type <- "Version"
    }

    # Set version type if the new version is not a pre-release but the current version is a pre-release
    if (!recompile.only && !pre.release && current.pre.release) {
        version.type <- "Version"
    }

    # If the current version isn't a pre-release and the new version is a pre-release, then set the pre-release version to 1
    if (!recompile.only && !current.pre.release && pre.release) {
        new.version.string <- paste0(new.version.string, "-1")
        version.type <- "Pre-release version"
    }

    # If the current version is a pre-release and the new version is a pre-release, then increment the pre-release version
    if (!recompile.only && current.pre.release && pre.release) {
        version[4] <- version[4] + 1
        new.version.string <- paste0(paste0(version[1:3], collapse = "."), "-", version[4])
        version.type <- "Pre-release version"
    }

    # Ask for confirmation
    if (!recompile.only) {
        message <- paste0(
            "Release version ", new.version.string, "?"
        )
        release.new.version <- utils::menu(c("Yes", "Abort"), title = message, graphics = FALSE) == 1
        if (!release.new.version) {
            stop("Release aborted")
        }
    }

    if (recompile.only) {
        message <- paste0(
            "Recompile ", file.name, "?"
        )
        recompile.file <- utils::menu(c("Yes", "Abort"), title = message, graphics = FALSE) == 1
        if (!recompile.file) {
            stop("Recompile aborted")
        }
    }

    # Release new version
    if (!recompile.only) {
        # Update version
        description$version <- new.version.string

        # Update date
        description$date <- as.character(lubridate::today())

        # Update version type
        description$version_type <- version.type

        # Write description
        yaml::write_yaml(description, "_variables.yml")
    }

    # Compile file
    quarto::quarto_render(file.name, output_format = "all")

    # Use file.name to define a document.name that can be used in the commit message
    document.name <- file.name |>
        stringr::str_remove(paste0(".", tools::file_ext(file.name)))

    # Create a release name that includes the version number and date
    release.document.name <- paste0(document.name, "-v", new.version.string, "-", description$date)

    # Move compiled files to the release folder
    dir.create("releases", showWarnings = FALSE)
    new.version.dir.name <- paste0("v", new.version.string, "-", description$date)
    dir.create(file.path("releases", new.version.dir.name), showWarnings = FALSE)
    files.to.move <- fs::dir_ls(".",
        type = "file",
        glob = paste0(document.name, "*")
    ) |>
        stringr::str_subset("html|pdf|docx")
    for (file in files.to.move) {
        fs::file_copy(
            file,
            file.path("releases", new.version.dir.name, paste0(release.document.name, ".", tools::file_ext(file))),
            overwrite = TRUE
        )
    }

    # Commit changes and tag release
    release.string <- stringr::str_replace_all(document.name, "-", " ") |>
        stringr::str_to_lower()
    if (commit && !recompile.only) {
        version.indicator.string <- ifelse(pre.release, "pre-release version", "version")
        base.git.path <- git2r::discover_repository(".") |>
            stringr::str_remove("/.git")
        git2r::commit(
            repo = ".",
            all = TRUE,
            message = paste0("Release ", release.string, " ", version.indicator.string, " ", new.version.string)
        )

        #     ## if (tag) {
        #     ##    git2r::tag(
        #     ##        object = ".",
        #     ##        name = stringr::str_to_sentence(paste0(document.name, " ", new.version.string)),
        #     ##        message = paste0("Release ATLS vs standard care trial ", document.name, " version ", new.version.string)
        #     ##    )
        #     ## }
    }
}
