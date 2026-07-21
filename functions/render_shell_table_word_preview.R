#' Render a minimal Word preview of a shell table
#'
#' Writes a single-table Quarto document and renders it to Word. Used by the
#' `*_word_preview()` helpers for shell tables in the statistical analysis plan.
#'
#' @param table.call Character. R code evaluated in the table chunk.
#' @param output.file Character. Path for the Word document to create.
#' @param title Character. Title shown in the Word document.
#' @param table.label Character or NULL. Chunk label for cross-referencing.
#' @param table.caption Character or NULL. Table caption.
#' @param description Character or NULL. Brief prose shown above the table.
#' @param cleanup.qmd Logical. If TRUE, delete the temporary `.qmd` after
#'     rendering.
#' @return Invisibly, the path to `output.file`.
#'
#' @examples
#' \dontrun{
#' render_shell_table_word_preview(
#'     table.call = "create_cluster_characteristics_table()",
#'     output.file = "_test-cluster-characteristics-word.docx",
#'     title = "Cluster characteristics — Word preview",
#'     table.label = "tbl-cluster-characteristics",
#'     table.caption = "Cluster characteristics"
#' )
#' }
render_shell_table_word_preview <- function(table.call,
                                            output.file,
                                            title,
                                            table.label = NULL,
                                            table.caption = NULL,
                                            description = NULL,
                                            cleanup.qmd = FALSE) {
    assertthat::assert_that(is.character(table.call) && length(table.call) == 1)
    assertthat::assert_that(is.character(output.file) && length(output.file) == 1)
    assertthat::assert_that(is.character(title) && length(title) == 1)
    assertthat::assert_that(is.null(table.label) || (is.character(table.label) && length(table.label) == 1))
    assertthat::assert_that(is.null(table.caption) || (is.character(table.caption) && length(table.caption) == 1))
    assertthat::assert_that(is.null(description) || (is.character(description) && length(description) == 1))
    assertthat::assert_that(is.logical(cleanup.qmd) && length(cleanup.qmd) == 1)

    output.file <- normalizePath(output.file, winslash = "/", mustWork = FALSE)
    output.dir <- dirname(output.file)
    if (!dir.exists(output.dir)) {
        dir.create(output.dir, recursive = TRUE, showWarnings = FALSE)
    }

    qmd.file <- sub("\\.docx$", ".qmd", output.file, ignore.case = TRUE)
    if (!grepl("\\.qmd$", qmd.file, ignore.case = TRUE)) {
        qmd.file <- paste0(qmd.file, ".qmd")
    }

    chunk.header <- c("```{r}")
    if (!is.null(table.label)) {
        chunk.header <- c(chunk.header, paste0("#| label: ", table.label))
    }
    if (!is.null(table.caption)) {
        chunk.header <- c(chunk.header, paste0("#| tbl-cap: \"", gsub("\"", "\\\\\"", table.caption), "\""))
    }
    chunk.header <- c(chunk.header, table.call, "```")

    qmd.content <- c(
        "---",
        paste0("title: \"", gsub("\"", "\\\\\"", title), "\""),
        "format:",
        "  docx: default",
        "execute:",
        "  echo: false",
        "  message: false",
        "  warning: false",
        "---",
        "",
        "```{r setup}",
        "noacsr::source_all_functions()",
        "```",
        ""
    )
    if (!is.null(description)) {
        qmd.content <- c(qmd.content, description, "")
    }
    qmd.content <- c(qmd.content, chunk.header, "")

    writeLines(qmd.content, qmd.file, useBytes = TRUE)

    old.wd <- getwd()
    on.exit(setwd(old.wd), add = TRUE)
    setwd(output.dir)

    quarto::quarto_render(
        input = basename(qmd.file),
        output_format = "docx",
        output_file = basename(output.file)
    )

    if (isTRUE(cleanup.qmd)) {
        unlink(basename(qmd.file))
    }

    invisible(output.file)
}
