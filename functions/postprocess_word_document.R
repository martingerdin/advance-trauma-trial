#' Finish the Word output of a Quarto document
#'
#' Adjusts the Word document that Quarto writes so that it follows the PDF
#' more closely. The title page is given the title page header, page numbering
#' starts after it, and paragraphs marked with the text
#' `word-landscape-start` and `word-landscape-end` become section breaks with
#' a landscape page. The table of contents is set to refresh when the document
#' is opened.
#'
#' The markers are written as raw OpenXML blocks in the Quarto document, for
#' example around a wide table. The marker is a paragraph style rather than
#' text, so that updating the fields of the document does not remove it:
#'
#' ```
#' ```{=openxml}
#' <w:p><w:pPr><w:pStyle w:val="word-landscape-start" /></w:pPr></w:p>
#' ```
#' ```
#'
#' @param file.name Character. Path to the Word document.
#' @param settings List or NULL. Layout settings from
#'     [read_pdf_layout_settings()]. If NULL, the document is left unchanged
#'     apart from refreshing the table of contents.
#' @return The path to the Word document.
#'
#' @examples
#' \dontrun{
#' postprocess_word_document("statistical-analysis-plan.docx", settings)
#' }
postprocess_word_document <- function(file.name, settings = NULL) {
    assertthat::assert_that(is.character(file.name) && length(file.name) == 1)
    assertthat::assert_that(file.exists(file.name), msg = paste0("File ", file.name, " does not exist"))
    assertthat::assert_that(is.null(settings) || is.list(settings))

    temp.dir <- tempfile("word-document-")
    dir.create(temp.dir)
    on.exit(unlink(temp.dir, recursive = TRUE), add = TRUE)
    utils::unzip(file.name, exdir = temp.dir)

    document.path <- file.path(temp.dir, "word/document.xml")
    document <- paste(readLines(document.path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    document <- convert_word_landscape_sections(document, settings, temp.dir)
    document <- split_word_title_page(document, settings)
    document <- keep_title_page_on_first_section(document)
    writeLines(document, document.path, useBytes = TRUE)
    set_word_update_fields(file.path(temp.dir, "word/settings.xml"))

    document <- fill_word_table_of_contents(document, settings)
    writeLines(document, document.path, useBytes = TRUE)

    zip_word_document(temp.dir, file.name)
    file.name
}

#' Write the table of contents entries
#'
#' Word fills the table of contents when the document is opened, but a
#' conversion to PDF does not. The entries are therefore written from the
#' headings, with page references that a conversion can resolve.
fill_word_table_of_contents <- function(document, settings) {
    field <- regexpr('TOC \\\\[^<]*&quot;1-(\\d)&quot;', document)
    if (field == -1 || is.null(settings)) {
        return(document)
    }
    depth <- as.integer(sub('.*&quot;1-(\\d)&quot;.*', "\\1", regmatches(document, field)))
    headings <- gregexpr(sprintf('<w:p\\b[^>]*>(?:(?!</w:p>).)*<w:pStyle w:val="Heading([1-%d])"(?:(?!</w:p>).)*</w:p>', depth), document, perl = TRUE)
    paragraphs <- regmatches(document, headings)[[1]]
    if (length(paragraphs) == 0) {
        return(document)
    }

    entries <- vapply(seq_along(paragraphs), function(i) {
        paragraph <- paragraphs[i]
        level <- as.integer(sub('.*<w:pStyle w:val="Heading(\\d)".*', "\\1", paragraph))
        text <- paste(unlist(regmatches(paragraph, gregexpr("<w:t[^>]*>[^<]*", paragraph))), collapse = "")
        text <- gsub("<w:t[^>]*>", "", text)
        text <- gsub("&amp;", "&", text, fixed = TRUE)
        bookmark <- paste0("contents", i)
        entry <- settings$toc[[level]]
        run <- function(body) paste0("<w:r><w:rPr>", word_run(entry$font, entry$size, bold = entry$bold, italic = entry$italic), "</w:rPr>", body, "</w:r>")
        page <- paste0(
            run('<w:fldChar w:fldCharType="begin" />'),
            run(paste0('<w:instrText xml:space="preserve"> PAGEREF ', bookmark, " </w:instrText>")),
            run('<w:fldChar w:fldCharType="end" />')
        )
        paste0(
            '<w:p><w:pPr><w:pStyle w:val="', paste0("TOC", level), '" /><w:tabs><w:tab w:val="right" w:leader="dot" w:pos="9350" /></w:tabs></w:pPr>',
            run(paste0('<w:t xml:space="preserve">', word_xml(text), "</w:t>")),
            if (isTRUE(entry$dots)) run('<w:tab />') else "",
            page,
            "</w:p>"
        )
    }, character(1))

    bookmark.ids <- as.integer(sub('.*w:id="(\\d+)".*', "\\1", unlist(regmatches(document, gregexpr("<w:bookmarkStart\\b[^>]*w:id=\"\\d+\"", document)))))
    next.bookmark.id <- max(c(bookmark.ids, 0)) + 1L
    paragraphs <- gregexpr(sprintf('<w:p\\b[^>]*>(?:(?!</w:p>).)*<w:pStyle w:val="Heading([1-%d])"(?:(?!</w:p>).)*</w:p>', depth), document, perl = TRUE)[[1]]
    for (i in rev(seq_along(paragraphs))) {
        paragraph <- substr(document, paragraphs[i], paragraphs[i] + attr(paragraphs, "match.length")[i] - 1)
        bookmark.id <- next.bookmark.id + i - 1L
        bookmarked <- sub("(<w:p\\b[^>]*>)", paste0("\\1<w:bookmarkStart w:id=\"", bookmark.id, "\" w:name=\"contents", i, "\" />"), paragraph)
        bookmarked <- sub("</w:p>$", paste0("<w:bookmarkEnd w:id=\"", bookmark.id, "\" /></w:p>"), bookmarked)
        document <- paste0(substr(document, 1, paragraphs[i] - 1), bookmarked, substr(document, paragraphs[i] + attr(paragraphs, "match.length")[i], nchar(document)))
    }

    field.paragraph <- regexpr('<w:p\\b[^>]*>(?:(?!</w:p>).)*<w:instrText[^>]*>\\s*TOC(?:(?!</w:p>).)*</w:p>', document, perl = TRUE)
    if (field.paragraph == -1) {
        return(document)
    }
    paste0(
        substr(document, 1, field.paragraph - 1),
        paste(entries, collapse = ""),
        substr(document, field.paragraph + attr(field.paragraph, "match.length"), nchar(document))
    )
}

#' Turn landscape markers into section breaks
#'
#' Each `word-landscape-start` paragraph ends the current section and each
#' `word-landscape-end` paragraph ends the landscape section. The new sections
#' keep the header and footer of the section they replace.
convert_word_landscape_sections <- function(document, settings, temp.dir) {
    marker <- '<w:p\\b[^>]*>(?:(?!</w:p>).)*<w:pStyle w:val="word-landscape-(start|end)"(?:(?!</w:p>).)*</w:p>'
    document <- gsub("</w:p>\\s*(<w:pPr\\b.*?</w:pPr>)", "\\1</w:p>", document, perl = TRUE)
    matches <- gregexpr(marker, document, perl = TRUE)[[1]]
    if (matches[1] == -1 || is.null(settings)) {
        return(gsub(marker, "", document, perl = TRUE))
    }

    body.section <- sub("(?s)^.*(<w:sectPr\\b.*</w:sectPr>).*$", "\\1", document, perl = TRUE)
    references <- word_section_references(body.section)
    if (!nzchar(references)) {
        references <- word_header_footer_references(settings, temp.dir)
    }

    # A section break describes the pages before it. The start marker therefore
    # ends the portrait section, and the end marker ends the landscape section.
    markers <- regmatches(document, gregexpr("(?<=w:val=\"word-landscape-)(start|end)", document, perl = TRUE))[[1]]
    sections <- vapply(markers, function(marker.name) {
        paste0("<w:p><w:pPr>", word_section_properties(settings, list(references = references), landscape = marker.name == "end"), "</w:pPr></w:p>")
    }, character(1))
    regmatches(document, list(matches)) <- list(sections)
    document
}

#' Header and footer references of a section, without duplicates
#'
#' Keeps the title page header as a first-page header and the running header
#' and footer as the default. Later sections then repeat the running header
#' and footer without the title page header.
word_section_references <- function(section) {
    references <- unlist(regmatches(section, gregexpr("<w:(header|footer)Reference\\b[^>]*/>", section)))
    seen <- character()
    kept <- character()
    for (reference in references) {
        type <- sub('.*w:type="([^"]+)".*', "\\1", reference)
        id <- sub('.*r:id="([^"]+)".*', "\\1", reference)
        key <- paste(type, id, sep = "-")
        if (key %in% seen) next
        seen <- c(seen, key)
        if (type != "default") next
        kept <- c(kept, reference)
    }
    first <- references[grepl('w:type="first"', references)]
    paste(c(first, kept), collapse = "")
}

#' Add a header and footer to a Word document that has none
#'
#' Used when the reference document could not be applied, so the rendered
#' document still gets the running header and footer of the PDF.
#'
#' @return Character. The header and footer references for a section.
word_header_footer_references <- function(settings, temp.dir) {
    relationships.path <- file.path(temp.dir, "word/_rels/document.xml.rels")
    relationships <- paste(readLines(relationships.path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    content.types.path <- file.path(temp.dir, "[Content_Types].xml")
    content.types <- paste(readLines(content.types.path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    ids <- as.integer(regmatches(relationships, gregexpr("(?<=Id=\"rId)\\d+", relationships, perl = TRUE))[[1]])
    next.id <- max(c(ids, 0)) + 1L
    references <- character()

    add_part <- function(kind, content, name) {
        writeLines(word_header_footer_part(content, settings), file.path(temp.dir, "word", paste0(name, ".xml")), useBytes = TRUE)
        id <- paste0("rId", next.id)
        next.id <<- next.id + 1L
        relationships <<- sub(
            "</Relationships>",
            sprintf('<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/%s" Target="%s.xml" /></Relationships>', id, kind, name),
            relationships
        )
        content.type <- if (kind == "header") "application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml" else "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"
        content.types <<- sub("</Types>", sprintf('<Override PartName="/word/%s.xml" ContentType="%s" /></Types>', name, content.type), content.types)
        references <<- c(references, sprintf('<w:%sReference w:type="default" r:id="%s" />', kind, id))
    }

    add_part("header", settings$page.header, "header1")
    add_part("footer", settings$page.footer, "footer1")
    writeLines(relationships, relationships.path, useBytes = TRUE)
    writeLines(content.types, content.types.path, useBytes = TRUE)
    paste(references, collapse = "")
}

#' Give the title page its own section
#'
#' Ends the section after the subtitle, so the title page keeps the portrait
#' page and the title page header of the template. Numbering of the following
#' pages starts at 1.
split_word_title_page <- function(document, settings) {
    if (is.null(settings$cover)) {
        return(document)
    }
    title <- regexpr("<w:p\\b[^>]*>\\s*<w:pPr>\\s*<w:pStyle w:val=\"Title\"", document, perl = TRUE)
    if (title > 0) {
        document <- paste0(substr(document, 1, title - 1), word_cover_paragraphs(settings, "top"), substr(document, title, nchar(document)))
    }
    subtitle <- regexpr('w:pStyle w:val="Subtitle"', document)
    if (subtitle == -1) {
        return(document)
    }
    paragraph.end <- regexpr("</w:p>", substr(document, subtitle, nchar(document)))
    if (paragraph.end == -1) {
        return(document)
    }
    insert.at <- subtitle + paragraph.end + nchar("</w:p>") - 1L
    references <- word_section_references(sub("(?s)^.*(<w:sectPr\\b.*</w:sectPr>).*$", "\\1", document, perl = TRUE))
    title.section <- paste0(
        "<w:p><w:pPr>",
        word_section_properties(settings, list(references = references), first = TRUE),
        "</w:pPr></w:p>"
    )
    document <- paste0(substr(document, 1, insert.at), title.section, substr(document, insert.at + 1, nchar(document)))
    # Number the pages after the cover from 1. The cover itself is not numbered.
    starts <- gregexpr("<w:sectPr\\b[^>]*>", document)[[1]]
    if (length(starts) < 2 || starts[1] < 0) {
        return(document)
    }
    second <- starts[2]
    tag.length <- attr(starts, "match.length")[2]
    tag <- substr(document, second, second + tag.length - 1)
    numbered <- sub("<w:sectPr\\b[^>]*>", "<w:sectPr><w:pgNumType w:start=\"1\" />", tag)
    paste0(substr(document, 1, second - 1), numbered, substr(document, second + tag.length, nchar(document)))
}

#' Keep the title page header and footer on the cover only
#'
#' Later sections keep their own running header and footer.
keep_title_page_on_first_section <- function(document) {
    first <- regexpr("<w:titlePg\\s*/>", document)
    if (first < 0) {
        return(document)
    }
    head <- substr(document, 1, first + attr(first, "match.length") - 1)
    tail <- gsub("<w:titlePg\\s*/>", "", substr(document, first + attr(first, "match.length"), nchar(document)))
    paste0(head, tail)
}

set_word_update_fields <- function(settings.path) {
    if (!file.exists(settings.path)) {
        return(invisible())
    }
    settings <- paste(readLines(settings.path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (!grepl("<w:updateFields\\b", settings)) {
        settings <- sub("</w:settings>", "<w:updateFields w:val=\"true\" /></w:settings>", settings)
        writeLines(settings, settings.path, useBytes = TRUE)
    }
    invisible()
}
