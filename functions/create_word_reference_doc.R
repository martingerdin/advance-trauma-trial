#' Create a Word template matching the PDF layout
#'
#' Builds the reference document that Quarto uses for Word output, so that the
#' Word document follows the PDF layout read by
#' [read_pdf_layout_settings()]: page size and margins, fonts, heading sizes
#' and spacing, table of contents styles, captions, hyperlinks, a booktabs
#' table style, a running header and footer, and the title page.
#'
#' The template starts from pandoc's default reference document, so the style
#' names that pandoc expects are kept. It is written next to the Quarto
#' document and should not be edited by hand, because a new release replaces
#' it.
#'
#' @param file.name Character. Path to the Quarto document.
#' @param settings List or NULL. Layout settings. If NULL, the settings are
#'     read from `file.name`.
#' @param output.file Character or NULL. Path of the template to write. If
#'     NULL, the template is named from the document.
#' @return The path to the template.
#'
#' @examples
#' \dontrun{
#' create_word_reference_doc("statistical-analysis-plan.qmd")
#' }
create_word_reference_doc <- function(file.name, settings = NULL, output.file = NULL) {
    assertthat::assert_that(is.character(file.name) && length(file.name) == 1)
    assertthat::assert_that(file.exists(file.name), msg = paste0("File ", file.name, " does not exist"))
    assertthat::assert_that(is.null(settings) || is.list(settings))
    assertthat::assert_that(is.null(output.file) || (is.character(output.file) && length(output.file) == 1))

    if (is.null(settings)) {
        settings <- read_pdf_layout_settings(file.name)
    }
    if (is.null(output.file)) {
        output.file <- file.path(dirname(file.name), paste0(tools::file_path_sans_ext(basename(file.name)), "-word-template.docx"))
    }

    temp.dir <- tempfile("word-template-")
    dir.create(temp.dir)
    on.exit(unlink(temp.dir, recursive = TRUE), add = TRUE)
    default.template <- file.path(temp.dir, "default.docx")
    status <- system2(
        "quarto",
        c("pandoc", "--print-default-data-file", "reference.docx"),
        stdout = default.template
    )
    if (!identical(status, 0L)) {
        stop("Could not read pandoc's default Word template")
    }
    utils::unzip(default.template, exdir = temp.dir)

    styles <- readLines(file.path(temp.dir, "word/styles.xml"), warn = FALSE, encoding = "UTF-8")
    styles <- paste(styles, collapse = "\n")
    styles <- sub("(?s)<w:docDefaults>.*?</w:docDefaults>", word_document_defaults(settings), styles, perl = TRUE)
    styles <- apply_word_style(styles, word_paragraph_style("Normal", spacing = word_spacing(after = settings$paragraph.spacing), based.on = NULL))
    styles <- apply_word_style(styles, word_paragraph_style("BodyText", spacing = word_spacing(before = settings$paragraph.spacing, after = settings$paragraph.spacing)))
    styles <- apply_word_style(styles, word_paragraph_style("Compact", spacing = word_spacing(before = 2, after = 2)))
    styles <- apply_word_style(styles, word_heading_style(styles, "Title", settings$cover$title, settings, spacing = word_spacing(before = 0, after = 4, line = settings$cover$title$line), centre = TRUE))
    styles <- apply_word_style(styles, word_heading_style(styles, "Subtitle", settings$cover$subtitle, settings, spacing = word_spacing(before = 4, after = 0, line = settings$cover$subtitle$line), centre = TRUE))
    styles <- apply_word_style(styles, word_paragraph_style("Author", spacing = word_spacing(before = 0, after = 0), run = word_run(settings$fonts$sans, settings$base.size, colour = settings$cover$text.colour)))
    styles <- apply_word_style(styles, word_paragraph_style("Date", spacing = word_spacing(before = 0, after = 0), run = word_run(settings$fonts$sans, settings$base.size, colour = settings$cover$text.colour)))

    for (level in seq_along(settings$headings)) {
        heading <- settings$headings[[level]]
        styles <- apply_word_style(
            styles,
            word_heading_style(styles, paste0("Heading", level), heading, settings, spacing = word_spacing(before = heading$before, after = heading$after))
        )
    }
    styles <- apply_word_style(styles, word_paragraph_style("TOCHeading", spacing = word_spacing(before = settings$headings[[1]]$before, after = settings$headings[[1]]$after), run = word_run(settings$fonts$sans, settings$headings[[2]]$size), name = "TOC Heading"))

    for (level in seq_along(settings$toc)) {
        entry <- settings$toc[[level]]
        styles <- apply_word_style(styles, word_toc_style(styles, level, entry))
    }

    caption.run <- word_run(settings$fonts$main, settings$base.size, italic = TRUE)
    styles <- apply_word_style(styles, word_paragraph_style("Caption", spacing = word_spacing(before = 4, after = 6), run = caption.run))
    styles <- apply_word_style(styles, word_paragraph_style("TableCaption", spacing = word_spacing(before = 8, after = 4), run = caption.run, keep.next = TRUE, name = "Table Caption"))
    styles <- apply_word_style(styles, word_paragraph_style("ImageCaption", spacing = word_spacing(before = 4, after = 8), run = caption.run, name = "Image Caption"))
    styles <- apply_word_style(styles, word_paragraph_style("FootnoteText", run = word_run(settings$fonts$main, settings$footnote.size)))
    styles <- apply_word_style(styles, word_paragraph_style("Bibliography", spacing = word_spacing(before = 0, after = 4), run = word_run(settings$fonts$main, settings$footnote.size)))
    styles <- apply_word_style(styles, word_character_style("Hyperlink", word_run(colour = settings$link.colour, underline = TRUE)))
    styles <- apply_word_style(styles, word_table_style(styles))
    writeLines(styles, file.path(temp.dir, "word/styles.xml"), useBytes = TRUE)

    header.footer <- write_word_header_footer(temp.dir, settings)
    document <- readLines(file.path(temp.dir, "word/document.xml"), warn = FALSE, encoding = "UTF-8")
    document <- paste(document, collapse = "\n")
    document <- sub("<w:sectPr\\b.*?</w:sectPr>", "", document)
    # Pandoc keeps the headers and footers of the reference document only when a
    # section break separates content from the final section. The first section
    # holds the title page and the second section is what pandoc copies.
    document <- sub(
        "</w:body>",
        paste0(
            word_cover_paragraphs(settings, "top"),
            word_cover_paragraphs(settings, "bottom"),
            "<w:p><w:pPr>", word_section_properties(settings, header.footer, first = !is.null(settings$cover)), "</w:pPr></w:p>",
            "<w:p><w:r><w:t></w:t></w:r></w:p>",
            word_section_properties(settings, header.footer, first = FALSE),
            "\n</w:body>"
        ),
        document
    )
    writeLines(document, file.path(temp.dir, "word/document.xml"), useBytes = TRUE)

    settings.xml <- readLines(file.path(temp.dir, "word/settings.xml"), warn = FALSE, encoding = "UTF-8")
    settings.xml <- paste(settings.xml, collapse = "\n")
    settings.xml <- sub("</w:settings>", "<w:compat><w:compatSetting w:name=\"compatibilityMode\" w:uri=\"http://schemas.microsoft.com/office/word\" w:val=\"15\" /></w:compat>\n</w:settings>", settings.xml)
    writeLines(settings.xml, file.path(temp.dir, "word/settings.xml"), useBytes = TRUE)

    zip_word_document(temp.dir, output.file)
    output.file
}

points_to_twips <- function(points) {
    as.integer(round(points * 20))
}

points_to_half_points <- function(points) {
    as.integer(round(points * 2))
}

word_xml <- function(text) {
    text <- gsub("&", "&amp;", text, fixed = TRUE)
    text <- gsub("<", "&lt;", text, fixed = TRUE)
    text <- gsub(">", "&gt;", text, fixed = TRUE)
    text
}

word_fonts <- function(font) {
    if (is.null(font)) {
        return("")
    }
    font <- word_xml(font)
    sprintf('<w:rFonts w:ascii="%s" w:hAnsi="%s" w:cs="%s" />', font, font, font)
}

word_run <- function(font = NULL, size = NULL, bold = FALSE, italic = FALSE, colour = NULL, underline = FALSE) {
    paste0(
        word_fonts(font),
        if (isTRUE(bold)) "<w:b /><w:bCs />" else "",
        if (isTRUE(italic)) "<w:i /><w:iCs />" else "",
        if (is.null(colour)) "" else sprintf('<w:color w:val="%s" />', colour),
        if (isTRUE(underline)) '<w:u w:val="single" />' else "",
        if (is.null(size)) "" else sprintf('<w:sz w:val="%d" /><w:szCs w:val="%d" />', points_to_half_points(size), points_to_half_points(size))
    )
}

word_spacing <- function(before = NULL, after = NULL, line = NULL) {
    attributes <- c(
        if (is.null(before)) NULL else sprintf('w:before="%d"', points_to_twips(before)),
        if (is.null(after)) NULL else sprintf('w:after="%d"', points_to_twips(after)),
        if (is.null(line)) NULL else sprintf('w:line="%d" w:lineRule="exact"', points_to_twips(line))
    )
    if (length(attributes) == 0) "" else paste0("<w:spacing ", paste(attributes, collapse = " "), " />")
}

word_document_defaults <- function(settings) {
    line <- if (settings$line.spacing == 1) "" else sprintf(' w:line="%d" w:lineRule="auto"', as.integer(round(240 * settings$line.spacing)))
    paste0(
        "<w:docDefaults><w:rPrDefault><w:rPr>",
        word_run(settings$fonts$main, settings$base.size),
        sprintf('</w:rPr></w:rPrDefault><w:pPrDefault><w:pPr><w:spacing w:after="0"%s /></w:pPr></w:pPrDefault></w:docDefaults>', line)
    )
}

#' Replace or append one style in a Word styles part
#'
#' @param styles Character. The styles part.
#' @param style Character. A complete `w:style` element.
#' @return The styles part with the style of the same id replaced, or appended
#'     if the template does not contain it.
apply_word_style <- function(styles, style) {
    id <- sub('.*w:styleId="([^"]+)".*', "\\1", style)
    pattern <- sprintf('(?s)<w:style\\b[^>]*w:styleId="%s"[^>]*>.*?</w:style>', id)
    if (grepl(pattern, styles, perl = TRUE)) {
        return(sub(pattern, style, styles, perl = TRUE))
    }
    sub("</w:styles>", paste0(style, "\n</w:styles>"), styles)
}

word_paragraph_style <- function(id, spacing = "", run = "", centre = FALSE, keep.next = FALSE, outline = NULL, based.on = "Normal", name = NULL) {
    if (is.null(name)) name <- gsub("([a-z])([A-Z])", "\\1 \\2", id)
    paragraph <- paste0(
        if (isTRUE(keep.next)) "<w:keepNext /><w:keepLines />" else "",
        spacing,
        if (isTRUE(centre)) '<w:jc w:val="center" />' else "",
        if (is.null(outline)) "" else sprintf('<w:outlineLvl w:val="%d" />', outline)
    )
    sprintf(
        '<w:style w:type="paragraph"%s w:styleId="%s"><w:name w:val="%s" />%s<w:qFormat /><w:pPr>%s</w:pPr><w:rPr>%s</w:rPr></w:style>',
        if (id == "Normal") ' w:default="1"' else "",
        id, name,
        if (is.null(based.on)) "" else sprintf('<w:basedOn w:val="%s" />', based.on),
        paragraph, run
    )
}

word_heading_style <- function(styles, id, heading, settings, spacing, centre = FALSE) {
    if (is.null(heading$font)) {
        return(sub(sprintf('(<w:style\\b[^>]*w:styleId="%s"[^>]*>.*?</w:style>)', id), "\\1", styles))
    }
    level <- suppressWarnings(as.integer(sub("Heading", "", id)))
    word_paragraph_style(
        id,
        spacing = spacing,
        run = word_run(heading$font, heading$size, bold = heading$bold, italic = heading$italic, colour = heading$colour),
        centre = centre,
        keep.next = TRUE,
        outline = if (is.na(level)) NULL else level - 1L,
        name = if (is.na(level)) id else paste("heading", level)
    )
}

word_toc_style <- function(styles, level, entry) {
    id <- paste0("TOC", level)
    paragraph <- paste0(
        word_spacing(before = entry$before, after = 2),
        sprintf('<w:ind w:left="%d" />', points_to_twips(entry$indent)),
        if (isTRUE(entry$dots)) '<w:tabs><w:tab w:val="right" w:leader="dot" w:pos="9350" /></w:tabs>' else ""
    )
    sprintf(
        '<w:style w:type="paragraph" w:styleId="%s"><w:name w:val="toc %d" /><w:basedOn w:val="Normal" /><w:uiPriority w:val="39" /><w:unhideWhenUsed /><w:qFormat /><w:pPr>%s</w:pPr><w:rPr>%s</w:rPr></w:style>',
        id, level, paragraph, word_run(entry$font, entry$size, bold = entry$bold, italic = entry$italic)
    )
}

word_character_style <- function(id, run) {
    sprintf(
        '<w:style w:type="character" w:styleId="%s"><w:name w:val="%s" /><w:rPr>%s</w:rPr></w:style>',
        id, id, run
    )
}

#' Booktabs table style
#'
#' A header row with a rule above and below it, a rule below the table, and
#' no vertical rules.
#'
#' @param styles Character. The styles part, used to keep the style that the
#'     template already contains when it cannot be parsed.
#' @return A `w:style` element.
word_table_style <- function(styles) {
    # Cell borders belong in tcBorders. tblBorders is only valid on the table,
    # and Word repairs a document that puts tblBorders inside tcPr.
    rule <- function(edges, cell = FALSE) {
        borders <- vapply(c("top", "left", "bottom", "right", "insideH", "insideV"), function(edge) {
            if (edge %in% edges) {
                return(sprintf('<w:%s w:val="single" w:sz="6" w:space="0" w:color="000000" />', edge))
            }
            sprintf('<w:%s w:val="nil" />', edge)
        }, character(1))
        tag <- if (cell) "tcBorders" else "tblBorders"
        paste0("<w:", tag, ">", paste(borders, collapse = ""), "</w:", tag, ">")
    }
    sprintf(
        '<w:style w:type="table" w:default="1" w:styleId="Table"><w:name w:val="Table" /><w:tblPr>%s<w:tblCellMar><w:top w:w="40" w:type="dxa" /><w:left w:w="60" w:type="dxa" /><w:bottom w:w="40" w:type="dxa" /><w:right w:w="60" w:type="dxa" /></w:tblCellMar></w:tblPr><w:tblStylePr w:type="firstRow"><w:tcPr>%s</w:tcPr></w:tblStylePr><w:tblStylePr w:type="lastRow"><w:tcPr>%s</w:tcPr></w:tblStylePr></w:style>',
        rule(c("insideH")),
        rule(c("top", "bottom"), cell = TRUE),
        rule("bottom", cell = TRUE)
    )
}

word_page_size <- function(settings, landscape = FALSE) {
    width <- points_to_twips(settings$paper$width)
    height <- points_to_twips(settings$paper$height)
    if (landscape) {
        sprintf('<w:pgSz w:w="%d" w:h="%d" w:orient="landscape" />', height, width)
    } else {
        sprintf('<w:pgSz w:w="%d" w:h="%d" />', width, height)
    }
}

word_page_margins <- function(settings, landscape = FALSE) {
    margins <- settings$margins
    if (landscape) {
        # The PDF resets the geometry to 2 cm on each edge for the landscape table.
        margins <- list(top = 56.9, bottom = 56.9, left = 56.9, right = 56.9, header = 28.3, footer = 28.3)
    }
    sprintf(
        '<w:pgMar w:top="%d" w:right="%d" w:bottom="%d" w:left="%d" w:header="%d" w:footer="%d" w:gutter="0" />',
        points_to_twips(margins$top), points_to_twips(margins$right), points_to_twips(margins$bottom),
        points_to_twips(margins$left), points_to_twips(margins$header), points_to_twips(margins$footer)
    )
}

word_section_properties <- function(settings, header.footer, first = FALSE, landscape = FALSE) {
    paste0(
        "<w:sectPr>",
        header.footer$references,
        if (isTRUE(first)) "<w:titlePg />" else "",
        word_page_size(settings, landscape),
        word_page_margins(settings, landscape),
        "</w:sectPr>"
    )
}

#' Write the header and footer parts of the template
#'
#' @return A list with the relationship `references` for a section and the
#'     next free relationship id.
write_word_header_footer <- function(temp.dir, settings) {
    relationships <- readLines(file.path(temp.dir, "word/_rels/document.xml.rels"), warn = FALSE, encoding = "UTF-8")
    relationships <- paste(relationships, collapse = "\n")
    content.types <- readLines(file.path(temp.dir, "[Content_Types].xml"), warn = FALSE, encoding = "UTF-8")
    content.types <- paste(content.types, collapse = "\n")

    ids <- as.integer(regmatches(relationships, gregexpr("(?<=Id=\"rId)\\d+", relationships, perl = TRUE))[[1]])
    next.id <- max(c(ids, 0)) + 1L
    references <- character()

    add_part <- function(kind, content, name) {
        part <- paste0("word/", name, ".xml")
        writeLines(word_header_footer_part(content, settings), file.path(temp.dir, part), useBytes = TRUE)
        id <- paste0("rId", next.id)
        next.id <<- next.id + 1L
        relationships <<- sub(
            "</Relationships>",
            sprintf('<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/%s" Target="%s.xml" /></Relationships>', id, kind, name),
            relationships
        )
        content.type <- if (kind == "header") "application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml" else "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"
        content.types <<- sub(
            "</Types>",
            sprintf('<Override PartName="/%s" ContentType="%s" /></Types>', part, content.type),
            content.types
        )
        references <<- c(references, sprintf('<w:%sReference w:type="default" r:id="%s" />', kind, id))
    }

    add_part("header", settings$page.header, "header1")
    add_part("footer", settings$page.footer, "footer1")
    if (!is.null(settings$cover) && !is.null(settings$cover$colour)) {
        add_part("header", list(xml = word_cover_header(settings)), "header2")
        references[length(references)] <- sub('w:type="default"', 'w:type="first"', references[length(references)])
    }
    if (!is.null(settings$cover) && length(settings$cover$bottom) > 0) {
        add_part("footer", list(xml = word_cover_footer(settings)), "footer2")
        references[length(references)] <- sub('w:type="default"', 'w:type="first"', references[length(references)])
    }

    writeLines(relationships, file.path(temp.dir, "word/_rels/document.xml.rels"), useBytes = TRUE)
    writeLines(content.types, file.path(temp.dir, "[Content_Types].xml"), useBytes = TRUE)
    list(references = paste(references, collapse = ""), next.id = next.id)
}

word_header_footer_part <- function(content, settings) {
    if (!is.null(content$xml)) {
        return(content$xml)
    }
    font <- content$font
    paragraph <- function(text, alignment) {
        if (is.null(text) || !nzchar(text)) {
            return("")
        }
        sprintf(
            '<w:p><w:pPr><w:pStyle w:val="Header" /><w:jc w:val="%s" /></w:pPr>%s</w:p>',
            alignment, word_runs_with_fields(text, font)
        )
    }
    if (is.character(content)) {
        return(content)
    }
    paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<w:hdr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
        paragraph(content$left, "left"),
        paragraph(content$centre, "center"),
        paragraph(content$right, "right"),
        "</w:hdr>"
    )
}

word_runs_with_fields <- function(text, font) {
    pieces <- strsplit(text, "\\{(PAGE|NUMPAGES)\\}")[[1]]
    fields <- regmatches(text, gregexpr("(?<=\\{)(PAGE|NUMPAGES)(?=\\})", text, perl = TRUE))[[1]]
    runs <- character()
    run <- function(body) paste0("<w:r><w:rPr>", word_run(font$font, font$size, bold = font$bold, italic = font$italic), "</w:rPr>", body, "</w:r>")
    for (i in seq_along(pieces)) {
        if (nzchar(pieces[i])) runs <- c(runs, run(paste0("<w:t xml:space=\"preserve\">", word_xml(pieces[i]), "</w:t>")))
        if (i <= length(fields)) {
            instruction <- if (fields[i] == "PAGE") " PAGE " else " NUMPAGES "
            runs <- c(
                runs,
                run('<w:fldChar w:fldCharType="begin" />'),
                run(paste0('<w:instrText xml:space="preserve">', instruction, "</w:instrText>")),
                run('<w:fldChar w:fldCharType="end" />')
            )
        }
    }
    paste(runs, collapse = "")
}

word_cover_header <- function(settings) {
    colour <- settings$cover$colour
    if (is.null(colour)) {
        return("")
    }
    shape <- sprintf(
        '<w:p><w:r><w:pict><v:rect xmlns:v="urn:schemas-microsoft-com:vml" style="position:absolute;margin-left:%.1fpt;margin-top:%.1fpt;width:%.1fpt;height:%.1fpt;z-index:-1" filled="true" stroked="false"><v:fill color="#%s" /></v:rect></w:pict></w:r></w:p>',
        -settings$margins$left, -(settings$margins$top - settings$margins$header), settings$paper$width, settings$paper$height, colour
    )
    paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<w:hdr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:v="urn:schemas-microsoft-com:vml">',
        shape,
        "</w:hdr>"
    )
}

word_cover_footer <- function(settings) {
    paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<w:ftr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
        word_cover_paragraphs(settings, "bottom"),
        "</w:ftr>"
    )
}

word_cover_paragraph <- function(item) {
    run <- paste0("<w:r><w:rPr>", word_run(item$font, item$size, bold = item$bold, italic = item$italic, colour = item$colour), "</w:rPr>")
    if (!is.null(item$url)) {
        run <- paste0(run, '<w:fldChar w:fldCharType="begin" /><w:instrText xml:space="preserve"> HYPERLINK "', word_xml(item$url), '" </w:instrText><w:fldChar w:fldCharType="separate" /></w:r><w:r><w:rPr>', word_run(item$font, item$size, colour = item$colour), '</w:rPr><w:t xml:space="preserve">', word_xml(item$text), '</w:t></w:r><w:r><w:fldChar w:fldCharType="end" /></w:r>')
    } else {
        run <- paste0(run, '<w:t xml:space="preserve">', word_xml(item$text), "</w:t></w:r>")
    }
    border <- if (is.null(item$border)) "" else sprintf('<w:pBdr><w:bottom w:val="single" w:sz="12" w:space="1" w:color="%s" /></w:pBdr>', item$border)
    sprintf(
        '<w:p><w:pPr><w:jc w:val="center" />%s%s</w:pPr>%s</w:p>',
        word_spacing(before = item$before, after = item$after), border, run
    )
}

word_cover_paragraphs <- function(settings, where = c("top", "bottom")) {
    cover <- settings$cover
    where <- match.arg(where)
    if (is.null(cover) || length(cover[[where]]) == 0) {
        return("")
    }
    paste(vapply(cover[[where]], word_cover_paragraph, character(1)), collapse = "")
}

#' Zip a directory into a Word document
#'
#' Writes `[Content_Types].xml` first, which the Office Open XML package
#' format requires.
zip_word_document <- function(temp.dir, output.file) {
    output.file <- normalizePath(output.file, winslash = "/", mustWork = FALSE)
    old.dir <- getwd()
    on.exit(setwd(old.dir), add = TRUE)
    setwd(temp.dir)
    if (file.exists(output.file)) unlink(output.file)
    files <- c("[Content_Types].xml", list.files(".", recursive = TRUE, all.files = TRUE))
    files <- unique(files[!grepl("^default.docx$", files)])
    status <- system2("zip", c("-q", "-X", output.file, files))
    if (!identical(status, 0L)) {
        stop("Could not write the Word template")
    }
    output.file
}
