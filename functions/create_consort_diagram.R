#' Create CONSORT diagram for the stepped-wedge cluster randomised trial
#'
#' This function creates a CONSORT-style participant flow diagram following
#' the reporting extension for stepped-wedge cluster randomised trials
#' (Hemming et al. 2018, BMJ). The diagram is laid out as a grid with one
#' column per treatment sequence and one row per period, with cells shaded to
#' indicate whether the cluster is under the intervention or control condition
#' in that period. The text is a template (placeholder counts such as "n=") to
#' be completed once the trial is reported.
#'
#' The number of sequence columns is driven by `sequences`, so the figure
#' stays in sync with the design. Rather than shrinking text to fit fixed
#' boxes, the boxes are sized to the text: each box height is derived from the
#' number of wrapped lines its text needs at a fixed point size (`text.size`,
#' at least 8pt by default), so all text renders at that size. The overall
#' figure therefore grows taller as more periods are added.
#'
#' Cells are shaded by condition. Each sequence starts with a standard care
#' block, followed by a one-month (by default) transition, then the
#' intervention. The standard care block lengthens by one period per sequence:
#' sequence 1 has `min.standard.care.months` periods of standard care,
#' sequence 2 has one more, and so on, so the crossover steps across periods.
#'
#' @param sequences Numeric. Number of treatment sequences (columns) in the
#'     design. Must be a length 1 numeric value greater than 0. Default is 5.
#' @param periods Numeric. Number of periods (rows) in the design. Must be a
#'     length 1 numeric value greater than 0. Default is 13.
#' @param min.standard.care.months Numeric. Number of standard care periods
#'     for the first sequence. Each subsequent sequence has one more. Must be
#'     a length 1 numeric value greater than or equal to 0. Default is 4.
#' @param transition.months Numeric. Number of transition periods between
#'     standard care and intervention for each sequence. Must be a length 1
#'     numeric value greater than or equal to 0. Default is 1.
#' @param intervention.fill Character. Fill colour for cells under the
#'     intervention condition. Default is the project intervention colour.
#' @param control.fill Character. Fill colour for cells under the standard
#'     care (control) condition. Default is the project standard care colour.
#' @param transition.fill Character. Fill colour for cells in the transition
#'     period. Default is the project transition colour.
#' @param text.size Numeric. Fixed text size in points used for all text;
#'     boxes are grown so the text fits at this size. Must be a length 1
#'     numeric value greater than or equal to 8. Default is 8.
#' @param note Character. A figure note printed below the legend, used to keep
#'     the repeated cluster-size detail out of every box. Set to "" to omit.
#' @param return.figure Logical. If TRUE the function returns the ggplot
#'     object, otherwise it returns the path to the saved file. Default is TRUE.
#' @param save Logical. If TRUE the figure is saved to disk. Default is TRUE.
#' @param device Character. Device passed to ggplot2::ggsave. Default is "pdf".
#'
#' @return A ggplot object (if `return.figure`) or the saved file name.
#'
#' @examples
#' ## Load all project functions, then build and preview the diagram
#' noacsr::source_all_functions()
#' consort.figure <- create_consort_diagram(sequences = 5, periods = 13, save = FALSE)
#' print(consort.figure)
create_consort_diagram <- function(sequences = 5,
                                   periods = 13,
                                   min.standard.care.months = 4,
                                   transition.months = 1,
                                   intervention.fill = colors()["intervention"] |> unname(),
                                   control.fill = colors()["standard.care"] |> unname(),
                                   transition.fill = colors()["transition"] |> unname(),
                                   text.size = 8,
                                   note = paste(
                                       "Note: each \"n=\" should report the number of clusters,",
                                       "the average cluster size, and the variance of cluster sizes.",
                                       "For clusters that did not receive the intervention, give reasons."
                                   ),
                                   return.figure = TRUE,
                                   save = TRUE,
                                   device = "pdf") {
    ## Load packages
    library(ggplot2)
    library(ggfittext)

    ## Check arguments
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.numeric(periods) && length(periods) == 1 && periods > 0)
    assertthat::assert_that(is.numeric(min.standard.care.months) && length(min.standard.care.months) == 1 && min.standard.care.months >= 0)
    assertthat::assert_that(is.numeric(transition.months) && length(transition.months) == 1 && transition.months >= 0)
    assertthat::assert_that(is.character(intervention.fill) && length(intervention.fill) == 1)
    assertthat::assert_that(is.character(control.fill) && length(control.fill) == 1)
    assertthat::assert_that(is.character(transition.fill) && length(transition.fill) == 1)
    assertthat::assert_that(is.numeric(text.size) && length(text.size) == 1 && text.size >= 8)
    assertthat::assert_that(is.character(note) && length(note) == 1)

    n.seq <- sequences
    n.per <- periods

    ## Layout constants (arbitrary plotting units; the figure aspect ratio is
    ## preserved on save so that one x-unit equals one y-unit in centimetres).
    plot.left <- 1
    plot.right <- 99
    grid.left <- 7 # left margin reserved for the "Period k" labels

    ## Fixed mapping between plotting units and points. Box heights are derived
    ## from the text so that all text renders at exactly `text.size` points.
    cm.per.unit <- 0.30
    pt.per.unit <- cm.per.unit * 28.3465 # 1 cm = 28.3465 pt
    text.size.mm <- text.size / .pt # ggplot2 geom_text size (mm) for text.size pt

    ## Font metrics (in multiples of the font size), tuned to the rendered text
    ## so the boxes hug the text with only a little breathing room. The line
    ## height matches the 0.9 lineheight used by geom_fit_text below.
    char.width.units <- text.size * 0.50 / pt.per.unit
    line.height.units <- text.size * 0.90 / pt.per.unit
    pad.x <- 0.6
    pad.y <- 0.3

    ## Number of wrapped lines a label needs within a box of the given width
    n_lines <- function(label, box.width) {
        wrap.chars <- max(6, floor((box.width - 2 * pad.x) / char.width.units))
        blocks <- strsplit(label, "\n", fixed = TRUE)[[1]]
        sum(vapply(blocks, function(b) max(1L, length(strwrap(b, width = wrap.chars))), integer(1)))
    }
    ## Box height (units) needed to hold the label at the fixed text size
    box_height <- function(label, box.width) {
        n_lines(label, box.width) * line.height.units + 2 * pad.y
    }

    ## Column geometry, one column per sequence
    col.gap <- 1.2
    col.width <- ((plot.right - grid.left) - (n.seq - 1) * col.gap) / n.seq
    col.left <- grid.left + (seq_len(n.seq) - 1) * (col.width + col.gap)
    col.center <- col.left + col.width / 2
    col.right <- col.left + col.width
    grid.width <- plot.right - grid.left

    ## Text content (defined here so box heights can be derived from it)
    elig.label <- "Assessed for eligibility (n=no of clusters)"
    rand.label <- "Randomised (n=no of clusters)"
    excluded.label <- paste(
        "Excluded (n=no of clusters):",
        "    Not meeting inclusion criteria (n=)",
        "    Declined to participate (n=)",
        "    Other reasons (n=)",
        sep = "\n"
    )
    seq.title.label <- paste0("Sequence ", n.seq) # widest sequence title
    seq.sub.label <- "Clusters allocated (n=)"
    cell.label <- paste(
        "Assessed for eligibility (n=)",
        "Received intervention (n=)",
        "Did not receive intervention (n=)",
        sep = "\n"
    )

    ## Box heights derived from the text
    excl.width <- col.width * 2
    elig.h <- box_height(elig.label, grid.width)
    rand.h <- box_height(rand.label, grid.width)
    excl.h <- box_height(excluded.label, excl.width)
    head.title.h <- box_height(seq.title.label, col.width)
    head.sub.h <- box_height(seq.sub.label, col.width)
    head.h <- head.title.h + head.sub.h
    period.h <- box_height(cell.label, col.width)
    legend.h <- box_height("legend", 10)

    ## Vertical gaps (fixed); the eligibility-to-randomised gap must clear the
    ## excluded box that sits between them.
    excl.margin <- 1
    gap.elig.rand <- excl.h + 2 * excl.margin
    gap.rand.head <- 6
    gap.head.grid <- 3
    period.gap <- 1
    gap.grid.legend <- 5

    ## Lay out the vertical positions from the top going down
    y <- 0
    elig.cy <- y - elig.h / 2
    y <- y - elig.h
    y <- y - gap.elig.rand
    rand.cy <- y - rand.h / 2
    y <- y - rand.h
    bus.y <- y - gap.rand.head / 2 # horizontal bus line for branching arrows
    y <- y - gap.rand.head
    head.cy <- y - head.h / 2
    y <- y - head.h
    y <- y - gap.head.grid
    period.top <- numeric(n.per)
    period.bottom <- numeric(n.per)
    for (p in seq_len(n.per)) {
        period.top[p] <- y
        period.bottom[p] <- y - period.h
        y <- y - period.h - period.gap
    }
    grid.bottom <- min(period.bottom)
    legend.y <- grid.bottom - gap.grid.legend

    ## Collect boxes, fitted box text, free text labels and connector segments
    boxes <- data.frame()
    fit.texts <- data.frame()
    texts <- data.frame()
    segments <- data.frame()

    add_box <- function(xmin, xmax, ymin, ymax, fill) {
        boxes <<- rbind(boxes, data.frame(
            xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill
        ))
    }
    ## Text that must fit inside a box (xmin/xmax/ymin/ymax define the bounds)
    add_fit <- function(xmin, xmax, ymin, ymax, label, place = "topleft", fontface = "plain") {
        fit.texts <<- rbind(fit.texts, data.frame(
            xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
            label = label, place = place, fontface = fontface
        ))
    }
    ## Free-floating text (labels outside boxes)
    add_text <- function(x, y, label, hjust = 0, vjust = 1, fontface = "plain", size = text.size.mm) {
        texts <<- rbind(texts, data.frame(
            x = x, y = y, label = label, hjust = hjust, vjust = vjust,
            fontface = fontface, size = size
        ))
    }
    add_segment <- function(x, xend, y, yend, arrow = TRUE) {
        segments <<- rbind(segments, data.frame(
            x = x, xend = xend, y = y, yend = yend, arrow = arrow
        ))
    }

    ## Header: assessed for eligibility (spans the grid width)
    add_box(
        xmin = grid.left, xmax = plot.right,
        ymin = elig.cy - elig.h / 2, ymax = elig.cy + elig.h / 2,
        fill = "white"
    )
    add_fit(
        xmin = grid.left, xmax = plot.right,
        ymin = elig.cy - elig.h / 2, ymax = elig.cy + elig.h / 2,
        label = elig.label, place = "centre"
    )

    ## Header: randomised (spans the grid width)
    add_box(
        xmin = grid.left, xmax = plot.right,
        ymin = rand.cy - rand.h / 2, ymax = rand.cy + rand.h / 2,
        fill = "white"
    )
    add_fit(
        xmin = grid.left, xmax = plot.right,
        ymin = rand.cy - rand.h / 2, ymax = rand.cy + rand.h / 2,
        label = rand.label, place = "centre"
    )

    ## Excluded box (top right), centred in the gap between the two banners
    excl.xmin <- plot.right - excl.width
    excl.cy <- (elig.cy - elig.h / 2 + rand.cy + rand.h / 2) / 2
    excl.ymax <- excl.cy + excl.h / 2
    excl.ymin <- excl.cy - excl.h / 2
    add_box(
        xmin = excl.xmin, xmax = plot.right,
        ymin = excl.ymin, ymax = excl.ymax,
        fill = "white"
    )
    add_fit(
        xmin = excl.xmin, xmax = plot.right,
        ymin = excl.ymin, ymax = excl.ymax,
        label = excluded.label, place = "topleft"
    )

    ## Connector: eligibility -> randomised, with branch to excluded box
    connector.x <- (grid.left + plot.right) / 2
    branch.y <- (elig.cy - elig.h / 2 + rand.cy + rand.h / 2) / 2
    add_segment(
        x = connector.x, xend = connector.x,
        y = elig.cy - elig.h / 2, yend = rand.cy + rand.h / 2
    )
    add_segment(
        x = connector.x, xend = excl.xmin,
        y = branch.y, yend = branch.y
    )

    ## Branching arrows: randomised -> a horizontal bus -> each sequence header
    add_segment(
        x = connector.x, xend = connector.x,
        y = rand.cy - rand.h / 2, yend = bus.y, arrow = FALSE
    )
    add_segment(
        x = min(col.center), xend = max(col.center),
        y = bus.y, yend = bus.y, arrow = FALSE
    )
    for (k in seq_len(n.seq)) {
        add_segment(
            x = col.center[k], xend = col.center[k],
            y = bus.y, yend = head.cy + head.h / 2
        )
    }

    ## Sequence header boxes (bold title on top, allocation text below)
    head.split <- head.cy + head.h / 2 - head.title.h
    for (k in seq_len(n.seq)) {
        add_box(
            xmin = col.left[k], xmax = col.right[k],
            ymin = head.cy - head.h / 2, ymax = head.cy + head.h / 2,
            fill = "white"
        )
        add_fit(
            xmin = col.left[k], xmax = col.right[k],
            ymin = head.split, ymax = head.cy + head.h / 2,
            label = paste0("Sequence ", k), place = "centre", fontface = "bold"
        )
        add_fit(
            xmin = col.left[k], xmax = col.right[k],
            ymin = head.cy - head.h / 2, ymax = head.split,
            label = seq.sub.label, place = "centre"
        )
    }

    ## Period rows and per-sequence cells
    for (p in seq_len(n.per)) {
        ## Period label in the left margin
        add_text(
            x = plot.left, y = period.top[p],
            label = paste0("Period ", p), hjust = 0, vjust = 1, fontface = "bold"
        )
        for (k in seq_len(n.seq)) {
            ## Each sequence has a standard care block that lengthens by one
            ## period per sequence, then a transition, then the intervention.
            standard.care.end <- min.standard.care.months + (k - 1)
            transition.end <- standard.care.end + transition.months
            fill <- if (p <= standard.care.end) {
                control.fill
            } else if (p <= transition.end) {
                transition.fill
            } else {
                intervention.fill
            }
            add_box(
                xmin = col.left[k], xmax = col.right[k],
                ymin = period.bottom[p], ymax = period.top[p],
                fill = fill
            )
            add_fit(
                xmin = col.left[k], xmax = col.right[k],
                ymin = period.bottom[p], ymax = period.top[p],
                label = cell.label, place = "topleft"
            )

            ## Arrow from the sequence header into period 1. Consecutive period
            ## rows are stacked directly, so only draw a connector when there is
            ## a gap between them.
            if (p == 1) {
                add_segment(
                    x = col.center[k], xend = col.center[k],
                    y = head.cy - head.h / 2, yend = period.top[p]
                )
            } else if (period.gap > 0) {
                add_segment(
                    x = col.center[k], xend = col.center[k],
                    y = period.bottom[p - 1], yend = period.top[p]
                )
            }
        }
    }

    ## Manual legend (scale_fill_identity does not produce one)
    legend.box <- 2.5
    legend.items <- data.frame(
        fill = c(control.fill, transition.fill, intervention.fill),
        label = c(
            "Cluster under standard care condition",
            "Cluster in transition period",
            "Cluster under intervention condition"
        )
    )
    legend.spacing <- (plot.right - grid.left) / nrow(legend.items)
    for (i in seq_len(nrow(legend.items))) {
        legend.x <- grid.left + (i - 1) * legend.spacing
        add_box(
            xmin = legend.x, xmax = legend.x + legend.box,
            ymin = legend.y - legend.h / 2, ymax = legend.y + legend.h / 2,
            fill = legend.items$fill[i]
        )
        add_text(
            x = legend.x + legend.box + 1, y = legend.y,
            label = legend.items$label[i], hjust = 0, vjust = 0.5
        )
    }

    ## Figure note (keeps the repeated cluster-size detail out of every box)
    fig.bottom <- legend.y - legend.h / 2
    if (nzchar(note)) {
        gap.legend.note <- 3
        note.wrap.chars <- max(10, floor(grid.width / char.width.units))
        note.wrapped <- paste(strwrap(note, width = note.wrap.chars), collapse = "\n")
        note.lines <- length(strsplit(note.wrapped, "\n", fixed = TRUE)[[1]])
        note.top <- legend.y - legend.h / 2 - gap.legend.note
        add_text(
            x = grid.left, y = note.top,
            label = note.wrapped, hjust = 0, vjust = 1
        )
        fig.bottom <- note.top - note.lines * line.height.units
    }

    ## Helper to add a geom_fit_text layer for a given place/fontface group
    fit_layer <- function(layer.place, layer.fontface) {
        data <- fit.texts[fit.texts$place == layer.place & fit.texts$fontface == layer.fontface, ]
        if (nrow(data) == 0) {
            return(NULL)
        }
        geom_fit_text(
            data = data,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = label),
            place = layer.place, fontface = layer.fontface,
            reflow = TRUE, grow = FALSE, min.size = 0, size = text.size,
            padding.x = grid::unit(0.6, "mm"), padding.y = grid::unit(0.6, "mm"),
            lineheight = 0.9
        )
    }

    ## Build the plot
    consort.figure <- ggplot() +
        geom_rect(
            data = boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            color = "grey40", linewidth = 0.3, alpha = 0.8
        ) +
        geom_segment(
            data = subset(segments, !arrow),
            aes(x = x, xend = xend, y = y, yend = yend),
            color = "grey40", linewidth = 0.3
        ) +
        geom_segment(
            data = subset(segments, arrow),
            aes(x = x, xend = xend, y = y, yend = yend),
            color = "grey40", linewidth = 0.3,
            arrow = arrow(length = unit(0.15, "cm"), type = "closed")
        ) +
        fit_layer("topleft", "plain") +
        fit_layer("centre", "plain") +
        fit_layer("centre", "bold") +
        geom_text(
            data = texts,
            aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust, fontface = fontface),
            size = texts$size, lineheight = 0.9
        ) +
        scale_fill_identity() +
        coord_cartesian(
            xlim = c(plot.left, plot.right),
            ylim = c(fig.bottom, elig.cy + elig.h / 2),
            clip = "off"
        ) +
        theme_void()

    ## Save figure
    if (save) {
        file.name <- paste0("consort-diagram-", n.seq, "-sequences.", device)
        y.span <- (elig.cy + elig.h / 2) - fig.bottom
        x.span <- plot.right - plot.left
        ggsave(file.name, consort.figure,
            width = x.span * cm.per.unit,
            height = y.span * cm.per.unit,
            units = "cm", limitsize = FALSE
        )
    }

    ## Return figure or file name
    if (return.figure) {
        return(consort.figure)
    }
    return(file.name)
}
