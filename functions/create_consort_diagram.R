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
#' The number of sequence columns is driven by `sequences` and the number of
#' period rows is `sequences + 1`, so the figure stays in sync with the design.
#'
#' @param sequences Numeric. Number of treatment sequences (columns) in the
#'     design. Must be a length 1 numeric value greater than 0. Default is 5.
#' @param intervention.fill Character. Fill colour for cells under the
#'     intervention condition. Default is a light grey.
#' @param control.fill Character. Fill colour for cells under the control
#'     condition. Default is the project blue.
#' @param text.size Numeric. Base text size (ggplot2 geom_text size units) for
#'     the cell text. Default is 1.9.
#' @param return.figure Logical. If TRUE the function returns the ggplot
#'     object, otherwise it returns the path to the saved file. Default is TRUE.
#' @param save Logical. If TRUE the figure is saved to disk. Default is TRUE.
#' @param device Character. Device passed to ggplot2::ggsave. Default is "pdf".
#'
#' @return A ggplot object (if `return.figure`) or the saved file name.
#'
#' @examples
#' create_consort_diagram(sequences = 5, save = FALSE)
create_consort_diagram <- function(sequences = 5,
                                   intervention.fill = colors()["intervention"] |> unname(),
                                   control.fill = colors()["standard.care"] |> unname(),
                                   text.size = 1.9,
                                   return.figure = TRUE,
                                   save = TRUE,
                                   device = "pdf") {
    ## Load packages
    library(ggplot2)

    ## Check arguments
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.character(intervention.fill) && length(intervention.fill) == 1)
    assertthat::assert_that(is.character(control.fill) && length(control.fill) == 1)
    assertthat::assert_that(is.numeric(text.size) && length(text.size) == 1 && text.size > 0)

    n.seq <- sequences
    n.per <- 13

    ## Layout constants (arbitrary plotting units; the figure aspect ratio is
    ## preserved on save so that one x-unit equals one y-unit in centimetres).
    plot.left <- 1
    plot.right <- 99
    grid.left <- 7 # left margin reserved for the "Period k" labels

    ## Column geometry, one column per sequence
    col.gap <- 1.2
    col.width <- ((plot.right - grid.left) - (n.seq - 1) * col.gap) / n.seq
    col.left <- grid.left + (seq_len(n.seq) - 1) * (col.width + col.gap)
    col.center <- col.left + col.width / 2
    col.right <- col.left + col.width

    ## Text wrapping width, scaled to the column width
    wrap.width <- max(16, round(col.width * 1.25))
    wrap <- function(x, width = wrap.width) {
        vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
            character(1),
            USE.NAMES = FALSE
        )
    }

    ## Vertical geometry (laid out from the top going down)
    elig.h <- 5
    gap.elig.rand <- 9
    rand.h <- 5
    gap.rand.head <- 7
    head.h <- 6
    gap.head.grid <- 4
    period.h <- 16
    period.gap <- 3
    gap.grid.legend <- 6
    legend.h <- 3

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
    period.cy <- numeric(n.per)
    for (p in seq_len(n.per)) {
        period.top[p] <- y
        period.cy[p] <- y - period.h / 2
        period.bottom[p] <- y - period.h
        y <- y - period.h - period.gap
    }
    grid.bottom <- min(period.bottom)
    legend.y <- grid.bottom - gap.grid.legend

    ## Collect boxes, text labels and connector segments
    boxes <- data.frame()
    texts <- data.frame()
    segments <- data.frame()

    add_box <- function(xmin, xmax, ymin, ymax, fill) {
        boxes <<- rbind(boxes, data.frame(
            xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill
        ))
    }
    add_text <- function(x, y, label, hjust = 0, vjust = 1, fontface = "plain", size = text.size) {
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
    add_box(grid.left, plot.right, elig.cy - elig.h / 2, elig.cy + elig.h / 2, "white")
    add_text((grid.left + plot.right) / 2, elig.cy, "Assessed for eligibility (n=no of clusters)",
        hjust = 0.5, vjust = 0.5
    )

    ## Header: randomised (spans the grid width)
    add_box(grid.left, plot.right, rand.cy - rand.h / 2, rand.cy + rand.h / 2, "white")
    add_text((grid.left + plot.right) / 2, rand.cy, "Randomised (n=no of clusters)",
        hjust = 0.5, vjust = 0.5
    )

    ## Excluded box (top right)
    excl.xmin <- plot.right - col.width * 1.6
    excl.ymax <- elig.cy - elig.h / 2 - 1
    excl.ymin <- rand.cy + rand.h / 2 + 1
    add_box(excl.xmin, plot.right, excl.ymin, excl.ymax, "white")
    excluded.label <- paste(c(
        "Excluded (n=no of clusters):",
        wrap("    Not meeting inclusion criteria (n=)"),
        "    Declined to participate (n=)",
        "    Other reasons (n=)"
    ), collapse = "\n")
    add_text(excl.xmin + 0.6, excl.ymax - 0.6, excluded.label, hjust = 0, vjust = 1)

    ## Connector: eligibility -> randomised, with branch to excluded box
    connector.x <- (grid.left + plot.right) / 2
    branch.y <- (elig.cy - elig.h / 2 + rand.cy + rand.h / 2) / 2
    add_segment(connector.x, connector.x, elig.cy - elig.h / 2, rand.cy + rand.h / 2)
    add_segment(connector.x, excl.xmin, branch.y, branch.y)

    ## Branching arrows: randomised -> a horizontal bus -> each sequence header
    add_segment(connector.x, connector.x, rand.cy - rand.h / 2, bus.y, arrow = FALSE)
    add_segment(min(col.center), max(col.center), bus.y, bus.y, arrow = FALSE)
    for (k in seq_len(n.seq)) {
        add_segment(col.center[k], col.center[k], bus.y, head.cy + head.h / 2)
    }

    ## Sequence header boxes
    for (k in seq_len(n.seq)) {
        add_box(col.left[k], col.right[k], head.cy - head.h / 2, head.cy + head.h / 2, "white")
        add_text(col.center[k], head.cy + head.h / 2 - 1.2, paste0("Sequence ", k),
            hjust = 0.5, vjust = 1, fontface = "bold"
        )
        add_text(col.center[k], head.cy - head.h / 2 + 1.2, "Clusters allocated (n=)",
            hjust = 0.5, vjust = 0
        )
    }

    ## Cell text template (assessed / received / did not receive)
    cell.label <- paste(
        wrap(c(
            "Assessed for eligibility (n=)",
            "Received intervention (n=no of clusters, average cluster size, variance of cluster sizes)",
            "Did not receive intervention, give reasons (n=no of clusters, average cluster size, variance of cluster sizes)"
        )),
        collapse = "\n"
    )

    ## Period rows and per-sequence cells
    for (p in seq_len(n.per)) {
        ## Period label in the left margin
        add_text(plot.left, period.top[p], paste0("Period ", p),
            hjust = 0, vjust = 1, fontface = "bold"
        )
        for (k in seq_len(n.seq)) {
            ## A sequence crosses over to the intervention after its own step:
            ## cell is under the intervention condition once period > sequence.
            is.intervention <- p > k
            fill <- if (is.intervention) intervention.fill else control.fill
            add_box(col.left[k], col.right[k], period.bottom[p], period.top[p], fill)
            add_text(col.left[k] + 0.6, period.top[p] - 0.6, cell.label, hjust = 0, vjust = 1)

            ## Arrow from the sequence header (period 1) or the previous cell
            if (p == 1) {
                add_segment(col.center[k], col.center[k], head.cy - head.h / 2, period.top[p])
            } else {
                add_segment(col.center[k], col.center[k], period.bottom[p - 1], period.top[p])
            }
        }
    }

    ## Manual legend (scale_fill_identity does not produce one)
    legend.box <- 2.5
    legend.x1 <- grid.left
    add_box(legend.x1, legend.x1 + legend.box, legend.y - legend.h / 2, legend.y + legend.h / 2, intervention.fill)
    add_text(legend.x1 + legend.box + 1, legend.y, "Cluster under intervention condition", hjust = 0, vjust = 0.5)
    legend.x2 <- legend.x1 + (plot.right - grid.left) / 2
    add_box(legend.x2, legend.x2 + legend.box, legend.y - legend.h / 2, legend.y + legend.h / 2, control.fill)
    add_text(legend.x2 + legend.box + 1, legend.y, "Cluster under control condition", hjust = 0, vjust = 0.5)

    ## Build the plot
    consort.figure <- ggplot() +
        geom_rect(
            data = boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            color = "grey40", linewidth = 0.3
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
        geom_text(
            data = texts,
            aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust, fontface = fontface),
            size = texts$size, lineheight = 0.9
        ) +
        scale_fill_identity() +
        coord_cartesian(
            xlim = c(plot.left, plot.right),
            ylim = c(legend.y - legend.h, elig.cy + elig.h / 2),
            clip = "off"
        ) +
        theme_void()

    ## Save figure
    if (save) {
        file.name <- paste0("consort-diagram-", n.seq, "-sequences.", device)
        y.span <- (elig.cy + elig.h / 2) - (legend.y - legend.h)
        x.span <- plot.right - plot.left
        cm.per.unit <- 0.30
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
