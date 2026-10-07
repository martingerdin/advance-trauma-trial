#' Read the PDF layout settings of a Quarto document
#'
#' Collects the settings that shape the PDF output, so that the Word output
#' can be styled to match: fonts, heading fonts and spacing, table of contents
#' entries, running header and footer, page margins, and the title page. The
#' settings are read from the project `_quarto.yml`, the document front
#' matter, and the LaTeX code included in the PDF header.
#'
#' Quarto loads all fonts except the main font with
#' `Scale=MatchLowercase`, so sans serif text is smaller in the PDF than its
#' nominal size. Font sizes are therefore scaled by the ratio of x-heights
#' when the fonts are installed.
#'
#' @param file.name Character. Path to the Quarto document.
#' @param variables List or NULL. Values used to resolve `{{< var >}}`
#'     shortcodes. If NULL, `_variables.yml` next to the document is used
#'     when it exists.
#' @return A list of layout settings for [create_word_reference_doc()].
#'     Lengths are in points.
#'
#' @examples
#' \dontrun{
#' settings <- read_pdf_layout_settings("statistical-analysis-plan.qmd")
#' }
read_pdf_layout_settings <- function(file.name, variables = NULL) {
    assertthat::assert_that(is.character(file.name) && length(file.name) == 1)
    assertthat::assert_that(file.exists(file.name), msg = paste0("File ", file.name, " does not exist"))
    assertthat::assert_that(is.null(variables) || is.list(variables))

    document.dir <- dirname(file.name)
    if (is.null(variables)) {
        variables.file <- file.path(document.dir, "_variables.yml")
        variables <- if (file.exists(variables.file)) yaml::read_yaml(variables.file) else list()
    }
    options <- read_quarto_format_options(file.name, "pdf")
    latex <- read_latex_header(options, document.dir)

    base.size <- latex_length_to_points(value_or(options$fontsize, "11pt"), 11)
    baseline.skip <- latex_baseline_skip(base.size) * as.numeric(value_or(options$linestretch, 1))
    fonts <- list(
        main = value_or(options$mainfont, last_latex_argument(latex, "setmainfont")),
        sans = value_or(options$sansfont, last_latex_argument(latex, "setsansfont")),
        mono = value_or(options$monofont, last_latex_argument(latex, "setmonofont"))
    )
    font.commands <- read_latex_font_commands(latex)
    colours <- read_latex_colours(latex)
    koma.fonts <- read_koma_fonts(latex)
    sections <- read_koma_section_settings(latex)
    ex <- font_x_height(fonts$main, base.size)

    # Turns a LaTeX font specification into a concrete font, size and weight
    resolve_font <- function(spec, default.size = base.size) {
        parsed <- parse_latex_font_spec(spec, base.size, font.commands, koma.fonts)
        font <- switch(value_or(parsed$family, "main"),
            main = fonts$main,
            sans = fonts$sans,
            mono = fonts$mono,
            parsed$family
        )
        list(
            font = font,
            size = value_or(parsed$size, default.size) * font_scale(font, fonts$main),
            bold = isTRUE(parsed$bold),
            italic = isTRUE(parsed$italic)
        )
    }

    heading.levels <- c("section", "subsection", "subsubsection", "paragraph", "subparagraph")
    headings <- lapply(heading.levels, function(level) {
        section <- sections[[level]]
        heading <- resolve_font(paste0("\\usekomafont{disposition}", koma.fonts[[level]]))
        heading$before <- abs(latex_length_to_points(section$beforeskip, base.size, ex))
        heading$after <- max(latex_length_to_points(section$afterskip, base.size, ex), 0)
        heading
    })
    toc <- lapply(heading.levels[1:4], function(level) {
        section <- sections[[level]]
        entry <- resolve_font(section$tocentryformat)
        entry$dots <- grepl("TOCLineLeaderFill", section$toclinefill, fixed = TRUE)
        entry$indent <- latex_length_to_points(section$tocindent, base.size, ex)
        entry$before <- latex_length_to_points(section$tocbeforeskip, base.size, ex)
        entry
    })

    list(
        paper = read_paper_size(options),
        margins = read_page_margins(options, latex, base.size),
        base.size = base.size,
        baseline.skip = baseline.skip,
        line.spacing = baseline.skip / font_line_height(fonts$main, base.size),
        paragraph.spacing = if (isTRUE(options$indent)) 0 else baseline.skip / 2,
        indent = isTRUE(options$indent),
        fonts = fonts,
        headings = headings,
        toc = toc,
        page.header = read_page_header_footer(latex, "head", resolve_font(paste(koma.fonts$pageheadfoot, koma.fonts$pagehead))),
        page.footer = read_page_header_footer(latex, "foot", resolve_font(paste(koma.fonts$pageheadfoot, koma.fonts$pagefoot))),
        footnote.size = latex_size("footnotesize", base.size),
        link.colour = value_or(resolve_latex_colour(value_or(options$linkcolor, "blue"), colours), "0000FF"),
        cover = read_title_page(options, latex, variables, base.size, fonts, font.commands, colours, koma.fonts),
        lang = options$lang
    )
}

#' Read the merged Quarto options for one output format
#'
#' Combines the project `_quarto.yml` and the document front matter, with
#' format options taking precedence over top-level options and the document
#' taking precedence over the project.
#'
#' @param file.name Character. Path to the Quarto document.
#' @param format Character. Format suffix, for example `"pdf"` or `"docx"`.
#'     Matches formats such as `titlepage-pdf`.
#' @return A list of options, with the matched format name in the attribute
#'     `format.name` and all declared formats in the attribute `formats`.
read_quarto_format_options <- function(file.name, format) {
    project.file <- file.path(dirname(file.name), "_quarto.yml")
    project <- if (file.exists(project.file)) yaml::read_yaml(project.file) else list()
    front.matter <- read_quarto_front_matter(file.name)

    formats <- unique(c(names(project$format), names(front.matter$format)))
    format.name <- formats[grepl(paste0("(^|-)", format, "$"), formats)][1]
    format_options <- function(x) {
        value <- if (is.na(format.name)) NULL else x$format[[format.name]]
        if (is.list(value)) value else list()
    }
    top_level <- function(x) x[setdiff(names(x), "format")]

    options <- utils::modifyList(top_level(project), format_options(project))
    options <- utils::modifyList(options, top_level(front.matter))
    options <- utils::modifyList(options, format_options(front.matter))
    attr(options, "format.name") <- format.name
    attr(options, "formats") <- formats
    options
}

read_quarto_front_matter <- function(file.name) {
    lines <- readLines(file.name, warn = FALSE, encoding = "UTF-8")
    delimiters <- which(trimws(lines) == "---")
    if (length(delimiters) < 2 || any(nzchar(trimws(lines[seq_len(delimiters[1] - 1)])))) {
        return(list())
    }
    front.matter <- yaml::yaml.load(paste(lines[(delimiters[1] + 1):(delimiters[2] - 1)], collapse = "\n"))
    if (is.list(front.matter)) front.matter else list()
}

read_latex_header <- function(options, document.dir) {
    read_include <- function(x) {
        if (is.null(x)) {
            return(character())
        }
        if (is.character(x)) {
            paths <- file.path(document.dir, x)
            paths <- paths[file.exists(paths)]
            return(vapply(paths, function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), character(1)))
        }
        if (!is.null(x$text) || !is.null(x$file)) {
            return(c(unlist(x$text), read_include(x$file)))
        }
        unlist(lapply(x, read_include))
    }
    latex <- paste(c(read_include(options[["include-in-header"]]), unlist(options[["header-includes"]])), collapse = "\n")
    gsub("(?m)(^|[^\\\\])%[^\n]*", "\\1", latex, perl = TRUE)
}

value_or <- function(...) {
    for (value in list(...)) {
        if (!is.null(value) && length(value) > 0 && !all(is.na(value))) {
            return(value)
        }
    }
    NULL
}

#' Find LaTeX commands and their arguments
#'
#' @param latex Character. LaTeX code.
#' @param command Character. Command name without the backslash.
#' @param n Integer. Number of mandatory arguments to read.
#' @return A list with one element per occurrence, each a list with the
#'     first optional argument (`optional`) and the mandatory arguments
#'     (`arguments`). An unbraced control sequence counts as an argument.
find_latex_commands <- function(latex, command, n = 1L) {
    starts <- gregexpr(paste0("\\\\", command, "(?![A-Za-z@])"), latex, perl = TRUE)[[1]]
    if (starts[1] == -1) {
        return(list())
    }
    chars <- strsplit(latex, "")[[1]]
    lapply(starts, function(start) {
        i <- start + nchar(command) + 1L
        optional <- NULL
        arguments <- character()
        while (length(arguments) < n && i <= length(chars)) {
            char <- chars[i]
            if (char %in% c(" ", "\n", "\t", "*")) {
                i <- i + 1L
            } else if (char %in% c("[", "{")) {
                end <- find_closing_delimiter(chars, i)
                content <- if (end - 1L > i) paste(chars[(i + 1L):(end - 1L)], collapse = "") else ""
                if (char == "[") {
                    if (is.null(optional)) optional <- content
                } else {
                    arguments <- c(arguments, content)
                }
                i <- end + 1L
            } else if (char == "\\") {
                control.sequence <- regmatches(
                    paste(chars[i:min(length(chars), i + 60L)], collapse = ""),
                    regexpr("^\\\\[A-Za-z@]+", paste(chars[i:min(length(chars), i + 60L)], collapse = ""))
                )
                if (length(control.sequence) == 0) break
                arguments <- c(arguments, control.sequence)
                i <- i + nchar(control.sequence)
            } else {
                break
            }
        }
        list(optional = optional, arguments = arguments)
    })
}

find_closing_delimiter <- function(chars, start) {
    opening <- chars[start]
    closing <- if (opening == "[") "]" else "}"
    depth <- 0L
    i <- start
    while (i <= length(chars)) {
        if (chars[i] == "\\") {
            i <- i + 2L
            next
        }
        if (chars[i] == opening) depth <- depth + 1L
        if (chars[i] == closing) depth <- depth - 1L
        if (depth == 0L) {
            return(i)
        }
        i <- i + 1L
    }
    length(chars)
}

last_latex_argument <- function(latex, command) {
    occurrences <- find_latex_commands(latex, command)
    if (length(occurrences) == 0 || length(occurrences[[length(occurrences)]]$arguments) == 0) {
        return(NULL)
    }
    trimws(occurrences[[length(occurrences)]]$arguments[1])
}

latex_to_text <- function(x) {
    x <- gsub("\\\\(href|textcolor|color|colorbox)\\s*\\{[^{}]*\\}", "", x)
    x <- gsub("\\\\fontsize\\s*\\{[^{}]*\\}\\s*\\{[^{}]*\\}", "", x)
    x <- gsub("\\\\[vh]space\\*?\\s*\\{[^{}]*\\}", "", x)
    x <- gsub("\\\\(&|%|_|#|\\$)", "\\1", x)
    x <- gsub("\\\\ ", " ", x)
    x <- gsub("\\\\[A-Za-z@]+\\*?", "", x)
    x <- gsub("[{}]", "", x)
    x <- gsub("~", " ", x, fixed = TRUE)
    trimws(gsub("\\s+", " ", x))
}

latex_size <- function(name, base.size) {
    sizes <- list(
        "10" = c(tiny = 5, scriptsize = 7, footnotesize = 8, small = 9, normalsize = 10, large = 12, Large = 14.4, LARGE = 17.28, huge = 20.74, Huge = 24.88),
        "11" = c(tiny = 6, scriptsize = 8, footnotesize = 9, small = 10, normalsize = 10.95, large = 12, Large = 14.4, LARGE = 17.28, huge = 20.74, Huge = 24.88),
        "12" = c(tiny = 6, scriptsize = 8, footnotesize = 10, small = 10.95, normalsize = 12, large = 14.4, Large = 17.28, LARGE = 20.74, huge = 24.88, Huge = 24.88)
    )
    table <- sizes[[as.character(min(max(round(base.size), 10), 12))]]
    if (!name %in% names(table)) {
        return(NULL)
    }
    unname(table[name] * base.size / table["normalsize"])
}

latex_baseline_skip <- function(base.size) {
    skips <- c("10" = 12, "11" = 13.6, "12" = 14.5)
    unname(skips[as.character(min(max(round(base.size), 10), 12))] * base.size / min(max(round(base.size), 10), 12))
}

#' Convert a LaTeX length to points
#'
#' Reads the first length in `x`, so glue such as `plus 1ex` is ignored.
#'
#' @param x Character or NULL. A LaTeX length, for example `"1.5ex plus .2ex"`.
#' @param font.size Numeric. Font size in points, used for `em`.
#' @param ex Numeric or NULL. Size of one `ex` in points.
#' @return Numeric length in points, or 0 if `x` is NULL or not a length.
latex_length_to_points <- function(x, font.size, ex = NULL) {
    if (is.null(x)) {
        return(0)
    }
    match <- regmatches(x, regexec("(-?[0-9]*\\.?[0-9]+)\\s*(pt|bp|mm|cm|in|em|ex|\\\\baselineskip)", x))[[1]]
    if (length(match) == 0) {
        return(0)
    }
    value <- as.numeric(match[2])
    unit <- switch(match[3],
        pt = 1,
        bp = 72.27 / 72,
        mm = 2.84528,
        cm = 28.4528,
        "in" = 72.27,
        em = font.size,
        ex = value_or(ex, 0.43 * font.size),
        latex_baseline_skip(font.size)
    )
    value * unit
}

parse_latex_font_spec <- function(spec, base.size, font.commands = list(), koma.fonts = list()) {
    spec <- value_or(spec, "")
    for (i in 1:3) {
        spec <- gsub_function("\\\\usekomafont\\s*\\{([^{}]*)\\}", spec, function(element) value_or(koma.fonts[[element]], ""))
    }
    tokens <- regmatches(spec, gregexpr("\\\\[A-Za-z]+(\\s*\\{[^{}]*\\}\\s*\\{[^{}]*\\})?", spec))[[1]]
    result <- list(family = NULL, size = NULL, bold = NULL, italic = NULL)
    for (token in tokens) {
        name <- sub("^\\\\([A-Za-z]+).*$", "\\1", token)
        if (name == "sffamily") {
            result$family <- "sans"
        } else if (name == "rmfamily") {
            result$family <- "main"
        } else if (name == "ttfamily") {
            result$family <- "mono"
        } else if (name == "bfseries") {
            result$bold <- TRUE
        } else if (name == "mdseries") {
            result$bold <- FALSE
        } else if (name %in% c("itshape", "slshape")) {
            result$italic <- TRUE
        } else if (name == "upshape") {
            result$italic <- FALSE
        } else if (name == "normalfont") {
            result$family <- "main"
            result$bold <- FALSE
            result$italic <- FALSE
        } else if (name == "fontsize") {
            result$size <- as.numeric(sub("^\\\\fontsize\\s*\\{([0-9.]+).*$", "\\1", token))
        } else if (!is.null(latex_size(name, base.size))) {
            result$size <- latex_size(name, base.size)
        } else if (name %in% names(font.commands)) {
            result$family <- font.commands[[name]]
        }
    }
    result
}

gsub_function <- function(pattern, x, replacement) {
    matches <- gregexpr(pattern, x, perl = TRUE)[[1]]
    if (matches[1] == -1) {
        return(x)
    }
    matched.text <- regmatches(x, list(matches))[[1]]
    captures <- regmatches(matched.text, regexec(pattern, matched.text, perl = TRUE))
    values <- vapply(captures, function(capture) replacement(capture[2]), character(1))
    regmatches(x, list(matches)) <- list(values)
    x
}

read_latex_font_commands <- function(latex) {
    occurrences <- find_latex_commands(latex, "newfontfamily", n = 2L)
    commands <- list()
    for (occurrence in occurrences) {
        if (length(occurrence$arguments) == 2) {
            commands[[sub("^\\\\", "", trimws(occurrence$arguments[1]))]] <- trimws(occurrence$arguments[2])
        }
    }
    commands
}

read_latex_macros <- function(latex) {
    macros <- list()
    for (command in c("newcommand", "renewcommand", "providecommand", "def")) {
        for (occurrence in find_latex_commands(latex, command, n = 2L)) {
            if (length(occurrence$arguments) == 2) {
                macros[[sub("^\\\\", "", trimws(occurrence$arguments[1]))]] <- occurrence$arguments[2]
            }
        }
    }
    macros
}

expand_latex_macros <- function(x, macros) {
    for (i in 1:5) {
        names.used <- regmatches(x, gregexpr("\\\\[A-Za-z@]+", x))[[1]]
        names.used <- intersect(sub("^\\\\", "", names.used), names(macros))
        if (length(names.used) == 0) break
        for (name in names.used) {
            x <- gsub(paste0("\\\\", name, "(?![A-Za-z@])"), gsub("\\\\", "\\\\\\\\", macros[[name]]), x, perl = TRUE)
        }
    }
    x
}

read_latex_colours <- function(latex) {
    colours <- c(
        white = "FFFFFF", black = "000000", red = "FF0000", green = "00FF00", blue = "0000FF",
        cyan = "00FFFF", magenta = "FF00FF", yellow = "FFFF00", gray = "808080", grey = "808080",
        darkgray = "404040", lightgray = "BFBFBF", brown = "BF8040", olive = "808000",
        orange = "FF8000", purple = "BF0040", teal = "008080", violet = "800080",
        Blue = "0000FF", Maroon = "AF3235"
    )
    for (occurrence in find_latex_commands(latex, "definecolor", n = 3L)) {
        if (length(occurrence$arguments) < 3) next
        model <- trimws(occurrence$arguments[2])
        value <- trimws(occurrence$arguments[3])
        hex <- switch(model,
            HTML = toupper(value),
            RGB = paste(sprintf("%02X", as.integer(strsplit(value, "\\s*,\\s*")[[1]])), collapse = ""),
            rgb = paste(sprintf("%02X", round(255 * as.numeric(strsplit(value, "\\s*,\\s*")[[1]]))), collapse = ""),
            NULL
        )
        if (!is.null(hex)) colours[trimws(occurrence$arguments[1])] <- hex
    }
    colours
}

resolve_latex_colour <- function(colour, colours) {
    if (is.null(colour) || !nzchar(colour)) {
        return(NULL)
    }
    colour <- sub("^#", "", trimws(colour))
    if (grepl("^[0-9A-Fa-f]{6}$", colour)) {
        return(toupper(colour))
    }
    if (colour %in% names(colours)) {
        return(unname(colours[colour]))
    }
    NULL
}

read_koma_fonts <- function(latex) {
    fonts <- list(
        disposition = "\\normalcolor\\sffamily\\bfseries",
        section = "\\Large",
        subsection = "\\large",
        subsubsection = "\\normalsize",
        paragraph = "\\normalsize",
        subparagraph = "\\normalsize",
        pageheadfoot = "\\normalcolor\\slshape",
        pagehead = "",
        pagefoot = ""
    )
    set.occurrences <- find_latex_commands(latex, "setkomafont", n = 2L)
    add.occurrences <- find_latex_commands(latex, "addtokomafont", n = 2L)
    set.positions <- gregexpr("\\\\setkomafont(?![A-Za-z@])", latex, perl = TRUE)[[1]]
    add.positions <- gregexpr("\\\\addtokomafont(?![A-Za-z@])", latex, perl = TRUE)[[1]]
    occurrences <- c(
        lapply(set.occurrences, function(x) c(x, list(add = FALSE))),
        lapply(add.occurrences, function(x) c(x, list(add = TRUE)))
    )
    positions <- numeric()
    if (set.positions[1] > 0) positions <- c(positions, set.positions)
    if (add.positions[1] > 0) positions <- c(positions, add.positions)
    for (occurrence in occurrences[order(positions)]) {
        if (length(occurrence$arguments) < 2) next
        element <- trimws(occurrence$arguments[1])
        fonts[[element]] <- if (occurrence$add) paste0(value_or(fonts[[element]], ""), occurrence$arguments[2]) else occurrence$arguments[2]
    }
    fonts
}

read_koma_section_settings <- function(latex) {
    sections <- list(
        section = list(beforeskip = "-3.5ex", afterskip = "2.3ex", tocentryformat = "\\usekomafont{disposition}", toclinefill = "\\hfill", tocindent = "0em", tocbeforeskip = "1em"),
        subsection = list(beforeskip = "-3.25ex", afterskip = "1.5ex", tocentryformat = "", toclinefill = "\\TOCLineLeaderFill", tocindent = "1.5em", tocbeforeskip = "0pt"),
        subsubsection = list(beforeskip = "-3.25ex", afterskip = "1.5ex", tocentryformat = "", toclinefill = "\\TOCLineLeaderFill", tocindent = "3.8em", tocbeforeskip = "0pt"),
        paragraph = list(beforeskip = "3.25ex", afterskip = "-1em", tocentryformat = "", toclinefill = "\\TOCLineLeaderFill", tocindent = "7em", tocbeforeskip = "0pt"),
        subparagraph = list(beforeskip = "3.25ex", afterskip = "-1em", tocentryformat = "", toclinefill = "\\TOCLineLeaderFill", tocindent = "10em", tocbeforeskip = "0pt")
    )
    occurrences <- c(
        find_latex_commands(latex, "RedeclareSectionCommand"),
        find_latex_commands(latex, "RedeclareSectionCommands")
    )
    for (occurrence in occurrences) {
        if (is.null(occurrence$optional) || length(occurrence$arguments) == 0) next
        settings <- parse_latex_keyval(occurrence$optional)
        for (level in intersect(trimws(strsplit(occurrence$arguments[1], ",")[[1]]), names(sections))) {
            sections[[level]] <- utils::modifyList(sections[[level]], settings)
        }
    }
    sections
}

parse_latex_keyval <- function(x) {
    chars <- strsplit(x, "")[[1]]
    depth <- cumsum((chars == "{") - (chars == "}"))
    splits <- which(chars == "," & depth == 0)
    pieces <- substring(x, c(1, splits + 1), c(splits - 1, nchar(x)))
    pieces <- trimws(pieces[grepl("=", pieces, fixed = TRUE)])
    values <- lapply(pieces, function(piece) sub("^\\{(.*)\\}$", "\\1", trimws(sub("^[^=]*=", "", piece))))
    stats::setNames(values, trimws(sub("=.*$", "", pieces)))
}

read_paper_size <- function(options) {
    paper <- tolower(value_or(options$papersize, "a4"))
    if (grepl("letter", paper)) {
        return(list(width = 612, height = 792))
    }
    list(width = 595.276, height = 841.89)
}

#' Read the page margins of the PDF
#'
#' Uses the geometry package settings when geometry is loaded, and the KOMA
#' typearea defaults otherwise. Header and footer distances follow the
#' positions measured in KOMA documents.
#'
#' @return A list with `top`, `bottom`, `left`, `right`, `header` and
#'     `footer` in points.
read_page_margins <- function(options, latex, base.size) {
    paper <- read_paper_size(options)
    geometry.options <- unlist(options$geometry)
    for (occurrence in find_latex_commands(latex, "geometry")) {
        geometry.options <- c(geometry.options, occurrence$arguments)
    }
    uses.geometry <- length(geometry.options) > 0 || grepl("\\\\usepackage(\\[[^]]*\\])?\\{geometry\\}", latex)
    package.options <- regmatches(latex, regexec("\\\\usepackage\\[([^]]*)\\]\\{geometry\\}", latex))[[1]]
    if (length(package.options) > 1) geometry.options <- c(geometry.options, package.options[2])

    if (uses.geometry) {
        settings <- parse_latex_keyval(paste(geometry.options, collapse = ","))
        length_of <- function(...) {
            value <- value_or(...)
            if (is.null(value)) NULL else latex_length_to_points(value, base.size)
        }
        margin <- settings$margin
        left <- length_of(settings$left, settings$lmargin, settings$inner, settings$hmargin, margin)
        right <- length_of(settings$right, settings$rmargin, settings$outer, settings$hmargin, margin)
        top <- length_of(settings$top, settings$tmargin, settings$vmargin, margin)
        bottom <- length_of(settings$bottom, settings$bmargin, settings$vmargin, margin)
        hscale <- as.numeric(value_or(settings$hscale, settings$scale, 0.7))
        vscale <- as.numeric(value_or(settings$vscale, settings$scale, 0.7))
        if (is.null(left) && is.null(right)) left <- right <- paper$width * (1 - hscale) / 2
        if (is.null(left)) left <- right
        if (is.null(right)) right <- left
        if (is.null(top) && is.null(bottom)) {
            top <- paper$height * (1 - vscale) * 2 / 5
            bottom <- paper$height * (1 - vscale) * 3 / 5
        }
        if (is.null(top)) top <- bottom * 2 / 3
        if (is.null(bottom)) bottom <- top * 3 / 2
    } else {
        div <- c("10" = 8, "11" = 10, "12" = 12)[as.character(min(max(round(base.size), 10), 12))]
        left <- right <- unname(1.5 * paper$width / div)
        top <- unname(paper$height / div)
        bottom <- unname(2 * paper$height / div)
    }
    list(
        top = top,
        bottom = bottom,
        left = left,
        right = right,
        header = max(top - 32, 14),
        footer = max(bottom - 47, 14)
    )
}

#' Read the running header or footer of the PDF
#'
#' @param latex Character. LaTeX header code.
#' @param part Character. `"head"` or `"foot"`.
#' @param font List. Resolved font of the header or footer.
#' @return A list with the `left`, `centre` and `right` content, each a
#'     character vector in which `"{PAGE}"` and `"{NUMPAGES}"` mark page
#'     fields, and the `font`.
read_page_header_footer <- function(latex, part, font) {
    content <- function(commands) {
        for (command in commands) {
            value <- last_latex_argument(latex, paste0(command, part))
            if (!is.null(value)) {
                value <- gsub("\\\\(thepage|pagemark)(?![A-Za-z@])", "{PAGE}", value, perl = TRUE)
                value <- gsub("\\\\pageref\\*?\\s*\\{LastPage\\}", "{NUMPAGES}", value)
                value <- gsub("\\{(PAGE|NUMPAGES)\\}", "\u0001\\1\u0001", value)
                value <- latex_to_text(value)
                value <- gsub("\u0001(PAGE|NUMPAGES)\u0001", "{\\1}", value)
                return(value)
            }
        }
        NULL
    }
    result <- list(
        left = content(c("lo", "i")),
        centre = content(c("co", "c")),
        right = content(c("ro", "o")),
        font = font
    )
    if (part == "foot" && is.null(result$left) && is.null(result$centre) && is.null(result$right)) {
        result$centre <- "{PAGE}"
    }
    result
}

#' Read the title page design
#'
#' Interprets the `titlepage-theme` elements of the Quarto titlepage
#' extension. Elements before `\vfill` are placed above the title, and
#' elements after `\vfill` at the bottom of the page.
#'
#' @return NULL if the PDF has no separate title page, otherwise a list with
#'     the page `colour`, the `title` and `subtitle` fonts, the `text.colour`,
#'     and the `top` and `bottom` paragraphs.
read_title_page <- function(options, latex, variables, base.size, fonts, font.commands, colours, koma.fonts) {
    format.name <- attr(options, "format.name")
    if (is.na(format.name) || !grepl("titlepage", format.name) || isFALSE(options$titlepage) || identical(options$titlepage, "none")) {
        return(NULL)
    }
    theme <- value_or(options[["titlepage-theme"]], list())
    page.colour <- resolve_latex_colour(theme[["page-color"]], colours)
    text.colour <- if (is.null(page.colour)) "000000" else "FFFFFF"

    text_style <- function(prefix, default.size) {
        spec <- paste0("\\", unlist(theme[[paste0(prefix, "-fontstyle")]]), collapse = "")
        parsed <- parse_latex_font_spec(spec, base.size, font.commands, koma.fonts)
        font <- value_or(
            theme[[paste0(prefix, "-fontfamily")]],
            switch(value_or(parsed$family, "main"), main = fonts$main, sans = fonts$sans, mono = fonts$mono, parsed$family)
        )
        list(
            font = font,
            size = as.numeric(value_or(theme[[paste0(prefix, "-fontsize")]], parsed$size, default.size)) * font_scale(font, fonts$main),
            line = if (is.null(theme[[paste0(prefix, "-spacing")]])) NULL else as.numeric(theme[[paste0(prefix, "-spacing")]]),
            bold = isTRUE(parsed$bold),
            italic = isTRUE(parsed$italic),
            colour = value_or(resolve_latex_colour(theme[[paste0(prefix, "-color")]], colours), text.colour)
        )
    }

    footer.text <- resolve_quarto_variables(options[["titlepage-footer"]], variables)
    header.text <- resolve_quarto_variables(options[["titlepage-header"]], variables)
    block_paragraph <- function(text, style) {
        if (is.null(text) || !nzchar(text)) {
            return(NULL)
        }
        c(list(text = text, url = NULL, before = 0, after = 0), style)
    }
    macros <- read_latex_macros(latex)
    groups <- list(top = list(), bottom = list())
    group <- "top"
    pending.space <- 0
    after.title <- FALSE

    add_paragraph <- function(paragraph) {
        if (is.null(paragraph) || (group == "top" && after.title)) {
            return(invisible())
        }
        paragraph$before <- paragraph$before + pending.space
        pending.space <<- 0
        groups[[group]] <<- c(groups[[group]], list(paragraph))
    }
    finish_group <- function() {
        count <- length(groups[[group]])
        if (pending.space > 0 && count > 0) {
            groups[[group]][[count]]$after <<- groups[[group]][[count]]$after + pending.space
        }
        pending.space <<- 0
    }

    elements <- unlist(theme$elements)
    if (is.null(elements)) {
        elements <- c("\\headerblock", "\\titleblock", "\\vfill", "\\footerblock")
    }
    for (element in trimws(elements)) {
        if (element == "\\vfill") {
            finish_group()
            group <- "bottom"
        } else if (element == "\\titleblock") {
            if (group == "top") finish_group()
            after.title <- TRUE
        } else if (element == "\\footerblock") {
            add_paragraph(block_paragraph(footer.text, text_style("footer", base.size)))
        } else if (element == "\\headerblock") {
            add_paragraph(block_paragraph(header.text, text_style("header", base.size)))
        } else if (grepl("^\\\\(author|affiliation|logo|date)block$", element)) {
            next
        } else {
            expanded <- expand_latex_macros(element, macros)
            for (piece in strsplit(expanded, "\\\\par(?![A-Za-z@])", perl = TRUE)[[1]]) {
                spaces <- regmatches(piece, gregexpr("\\\\vspace\\*?\\s*\\{[^{}]*\\}", piece))[[1]]
                text <- latex_to_text(piece)
                if (nzchar(text)) {
                    parsed <- parse_latex_font_spec(piece, base.size, font.commands, koma.fonts)
                    font <- switch(value_or(parsed$family, "main"), main = fonts$main, sans = fonts$sans, mono = fonts$mono, parsed$family)
                    colour <- regmatches(piece, regexec("\\\\textcolor\\s*\\{([^{}]*)\\}", piece))[[1]]
                    url <- regmatches(piece, regexec("\\\\href\\s*\\{([^{}]*)\\}", piece))[[1]]
                    add_paragraph(list(
                        text = text,
                        url = if (length(url) > 1) url[2] else NULL,
                        before = 0,
                        after = 0,
                        font = font,
                        size = value_or(parsed$size, base.size) * font_scale(font, fonts$main),
                        line = NULL,
                        bold = isTRUE(parsed$bold),
                        italic = isTRUE(parsed$italic),
                        colour = value_or(if (length(colour) > 1) resolve_latex_colour(colour[2], colours), text.colour)
                    ))
                } else if (grepl("\\\\(hrule|rule)(?![A-Za-z@])", piece, perl = TRUE) && !(group == "top" && after.title)) {
                    colour <- regmatches(piece, regexec("\\\\textcolor\\s*\\{([^{}]*)\\}", piece))[[1]]
                    count <- length(groups[[group]])
                    if (count > 0) {
                        groups[[group]][[count]]$border <- value_or(if (length(colour) > 1) resolve_latex_colour(colour[2], colours), text.colour)
                    }
                }
                for (space in spaces) {
                    pending.space <- pending.space + latex_length_to_points(sub("^\\\\vspace\\*?\\s*", "", space), base.size)
                }
            }
        }
    }
    finish_group()

    list(
        colour = page.colour,
        text.colour = text.colour,
        title = text_style("title", latex_size("Huge", base.size)),
        subtitle = text_style("subtitle", latex_size("LARGE", base.size)),
        top = groups$top,
        bottom = groups$bottom
    )
}

resolve_quarto_variables <- function(text, variables) {
    if (is.null(text)) {
        return(NULL)
    }
    text <- gsub_function("\\{\\{<\\s*var\\s+([^\\s>]+)\\s*>\\}\\}", text, function(name) {
        value <- variables
        for (key in strsplit(name, ".", fixed = TRUE)[[1]]) {
            value <- if (is.list(value)) value[[key]] else NULL
        }
        if (is.null(value)) "" else as.character(value)
    })
    text <- gsub("\\^([^^]*)\\^", "\\1", text)
    trimws(gsub("[*_]{1,2}([^*_]+)[*_]{1,2}", "\\1", text))
}

font_is_installed <- function(font) {
    if (is.null(font) || !requireNamespace("systemfonts", quietly = TRUE)) {
        return(FALSE)
    }
    tolower(font) %in% tolower(systemfonts::system_fonts()$family)
}

font_x_height <- function(font, size) {
    if (!font_is_installed(font)) {
        return(0.43 * size)
    }
    glyph <- systemfonts::glyph_info("x", family = font, size = 1000)
    glyph$bbox[[1]][["ymax"]] / 1000 * size
}

font_scale <- function(font, main.font) {
    if (is.null(font) || is.null(main.font) || identical(tolower(font), tolower(main.font)) ||
        !font_is_installed(font) || !font_is_installed(main.font)) {
        return(1)
    }
    font_x_height(main.font, 1) / font_x_height(font, 1)
}

font_line_height <- function(font, size) {
    if (!font_is_installed(font)) {
        return(1.2 * size)
    }
    systemfonts::font_info(family = font, size = size)$lineheight
}
