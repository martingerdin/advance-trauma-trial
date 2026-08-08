#' Shared helpers for CONSORT shell diagrams
#'
#' @keywords internal
consort_shell_canvas <- function(page.width.mm = 174,
                                 background = "grey92",
                                 text.size = 8) {
    plot.left <- 0
    plot.right <- 100
    ## Convert point size to plotting units so box heights match rendered text.
    cm.per.unit <- (page.width.mm / 10) / (plot.right - plot.left)
    pt.per.unit <- cm.per.unit * 28.3465
    list(
        plot.left = plot.left,
        plot.right = plot.right,
        page.width.mm = page.width.mm,
        background = background,
        text.size = text.size,
        text.size.mm = text.size / ggplot2::.pt,
        ## Match geom_text lineheight = 0.88 used in consort_render.
        line.h = text.size * 0.84 / pt.per.unit,
        pad = text.size * 0.40 / pt.per.unit,
        boxes = data.frame(),
        texts = data.frame(),
        segments = data.frame(),
        strips = data.frame()
    )
}

#' @keywords internal
consort_add_box <- function(canvas, xmin, xmax, ymin, ymax, fill = "white") {
    canvas$boxes <- rbind(
        canvas$boxes,
        data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)
    )
    canvas
}

#' @keywords internal
consort_add_text <- function(canvas, x, y, label, hjust = 0.5, vjust = 0.5,
                             fontface = "plain", size = NULL) {
    if (is.null(size)) {
        size <- canvas$text.size.mm
    }
    canvas$texts <- rbind(
        canvas$texts,
        data.frame(
            x = x, y = y, label = label, hjust = hjust, vjust = vjust,
            fontface = fontface, size = size
        )
    )
    canvas
}

#' @keywords internal
consort_add_segment <- function(canvas, x, xend, y, yend, arrow = TRUE) {
    canvas$segments <- rbind(
        canvas$segments,
        data.frame(x = x, xend = xend, y = y, yend = yend, arrow = arrow)
    )
    canvas
}

#' @keywords internal
consort_wrap <- function(label, width.chars = 28) {
    paste(strwrap(label, width = width.chars), collapse = "\n")
}

#' Wrap text while preserving existing newlines (for bullet lists)
#'
#' @keywords internal
consort_wrap_preserve <- function(label, width.chars = 28) {
    lines <- strsplit(label, "\n", fixed = TRUE)[[1]]
    wrapped <- unlist(lapply(lines, function(line) {
        if (!nzchar(line)) {
            return("")
        }
        ## Continuation indent for bullet lines
        indent <- if (grepl("^[\u2022\\-\\*]\\s", line)) {
            "  "
        } else {
            ""
        }
        pieces <- strwrap(
            line,
            width = width.chars,
            exdent = nchar(indent),
            simplify = TRUE
        )
        if (length(pieces) == 0L) {
            return(line)
        }
        paste(pieces, collapse = "\n")
    }))
    paste(wrapped, collapse = "\n")
}

#' Build a headed bullet list for exclusion boxes
#'
#' @keywords internal
consort_bullet_list <- function(header, items) {
    bullets <- paste0("\u2022 ", items)
    paste(c(header, bullets), collapse = "\n")
}

#' @keywords internal
consort_box_height <- function(label, width.chars = 28, canvas = NULL,
                               line.h = NULL, pad = NULL) {
    if (!is.null(canvas)) {
        line.h <- canvas$line.h
        pad <- canvas$pad
    }
    if (is.null(line.h)) {
        line.h <- 1.45
    }
    if (is.null(pad)) {
        pad <- 0.5
    }
    wrapped <- consort_wrap_preserve(label, width.chars)
    n.lines <- length(strsplit(wrapped, "\n", fixed = TRUE)[[1]])
    ## pad is total vertical inset (top + bottom); keep boxes close to the text.
    n.lines * line.h + pad
}

#' Mini stepped-wedge strip coordinates for one sequence
#'
#' @keywords internal
consort_sequence_strip <- function(sequence,
                                   sequences,
                                   xmin,
                                   xmax,
                                   ymin,
                                   ymax,
                                   min.standard.care.months,
                                   transition.months,
                                   total.months,
                                   control.fill,
                                   transition.fill,
                                   intervention.fill) {
    standard.care.end <- min.standard.care.months + (sequence - 1L)
    transition.end <- standard.care.end + transition.months
    periods <- seq_len(total.months)
    fills <- vapply(periods, function(p) {
        if (p <= standard.care.end) {
            control.fill
        } else if (p <= transition.end) {
            transition.fill
        } else {
            intervention.fill
        }
    }, character(1))
    w <- (xmax - xmin) / total.months
    data.frame(
        xmin = xmin + (periods - 1L) * w,
        xmax = xmin + periods * w,
        ymin = ymin,
        ymax = ymax,
        fill = fills
    )
}

#' Render and optionally save a CONSORT shell ggplot
#'
#' @keywords internal
consort_render <- function(canvas,
                           y.top,
                           y.bottom,
                           file.name,
                           return.figure = TRUE,
                           save = TRUE,
                           device = "pdf") {
    library(ggplot2)

    shadow.offset <- 0.35
    shadows <- transform(
        canvas$boxes,
        xmin = xmin + shadow.offset, xmax = xmax + shadow.offset,
        ymin = ymin - shadow.offset, ymax = ymax - shadow.offset
    )

    figure <- ggplot() +
        geom_rect(
            data = shadows,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey50", color = NA, alpha = 0.3
        ) +
        geom_rect(
            data = canvas$boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            color = "grey35", linewidth = 0.35
        )

    if (nrow(canvas$strips) > 0L) {
        figure <- figure +
            geom_rect(
                data = canvas$strips,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
                color = "grey40", linewidth = 0.15
            )
    }

    if (nrow(canvas$segments) > 0L) {
        figure <- figure +
            geom_segment(
                data = subset(canvas$segments, !arrow),
                aes(x = x, xend = xend, y = y, yend = yend),
                color = "grey35", linewidth = 0.35
            ) +
            geom_segment(
                data = subset(canvas$segments, arrow),
                aes(x = x, xend = xend, y = y, yend = yend),
                color = "grey35", linewidth = 0.35,
                arrow = arrow(length = unit(0.12, "cm"), type = "closed")
            )
    }

    figure <- figure +
        geom_text(
            data = canvas$texts,
            aes(
                x = x, y = y, label = label, hjust = hjust, vjust = vjust,
                fontface = fontface
            ),
            size = canvas$texts$size, lineheight = 0.88
        ) +
        scale_fill_identity() +
        coord_cartesian(
            xlim = c(canvas$plot.left, canvas$plot.right),
            ylim = c(y.bottom, y.top),
            clip = "off"
        ) +
        theme_void() +
        theme(
            plot.background = element_rect(fill = canvas$background, color = NA),
            panel.background = element_rect(fill = canvas$background, color = NA)
        )

    if (isTRUE(save)) {
        cm.per.unit <- (canvas$page.width.mm / 10) / (canvas$plot.right - canvas$plot.left)
        x.span <- canvas$plot.right - canvas$plot.left
        y.span <- y.top - y.bottom
        ggplot2::ggsave(
            file.name, figure,
            width = x.span * cm.per.unit,
            height = max(8, y.span * cm.per.unit),
            units = "cm", limitsize = FALSE
        )
    }

    if (isTRUE(return.figure)) {
        return(figure)
    }
    file.name
}

#' Create cluster-level CONSORT diagram shell
#'
#' Shell flowchart for cluster (hospital) flow through a batched stepped-wedge
#' trial, following a two-figure reporting approach (cluster figure separate
#' from the patient figure). Placeholders (`n=`) are completed at reporting.
#'
#' @param sequences Numeric. Number of sequences (columns). Default 5.
#' @param batches Numeric. Number of batches (shown in the figure note). Default 6.
#' @param total.months Numeric. Periods shown in each mini stepped-wedge strip.
#' @param min.standard.care.months Numeric. Standard-care periods for sequence 1.
#' @param transition.months Numeric. Transition periods per sequence.
#' @param page.width.mm Numeric. Output width in millimetres. Default 174.
#' @param return.figure Logical. Return ggplot if TRUE. Default TRUE.
#' @param save Logical. Save to disk if TRUE. Default TRUE.
#' @param device Character. Device for `ggsave`. Default `"png"`.
#' @return A ggplot object or the saved file name.
#'
#' @examples
#' \dontrun{
#' noacsr::source_all_functions()
#' create_cluster_consort_diagram(save = FALSE)
#' }
create_cluster_consort_diagram <- function(sequences = 5,
                                           batches = 6,
                                           total.months = 13,
                                           min.standard.care.months = 4,
                                           transition.months = 1,
                                           page.width.mm = 174,
                                           return.figure = TRUE,
                                           save = TRUE,
                                           device = "png") {
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.numeric(batches) && length(batches) == 1 && batches > 0)
    assertthat::assert_that(is.numeric(total.months) && length(total.months) == 1 && total.months > 0)
    assertthat::assert_that(
        is.numeric(min.standard.care.months) && length(min.standard.care.months) == 1 &&
            min.standard.care.months >= 0
    )
    assertthat::assert_that(
        is.numeric(transition.months) && length(transition.months) == 1 &&
            transition.months >= 0
    )

    cols <- colors()
    control.fill <- unname(cols["light.standard.care"])
    transition.fill <- unname(cols["light.transition"])
    intervention.fill <- unname(cols["light.intervention"])
    box.fill <- "#fde6d4"

    canvas <- consort_shell_canvas(page.width.mm = page.width.mm)
    n.seq <- as.integer(sequences)
    margin <- 2
    gap <- 1.2
    col.width <- (100 - 2 * margin - (n.seq - 1) * gap) / n.seq
    col.left <- margin + (seq_len(n.seq) - 1) * (col.width + gap)
    col.right <- col.left + col.width
    col.center <- (col.left + col.right) / 2
    wrap.chars <- max(18, floor(col.width * 1.45))

    top.label <- "Eligible clusters assessed for eligibility (n=)"
    rand.label <- "Clusters randomised (n=)"
    excl.pre.label <- consort_bullet_list(
        "Excluded before randomisation (n=):",
        c(
            "Not meeting inclusion criteria (n=)",
            "Declined to participate (n=)",
            "Other reasons (n=)"
        )
    )
    excl.post.label <- consort_bullet_list(
        "Lost/excluded (n=; reasons):",
        c(
            "Withdrawn (n=)",
            "No outcome data (n=)",
            "Other (n=)"
        )
    )
    included.label <- "Clusters included in primary analysis (n=)"
    total.included.label <- "Clusters included in primary analysis (n=)"
    total.excl.label <- consort_bullet_list(
        "Clusters excluded after randomisation (n=):",
        c(
            "Withdrawn (n=)",
            "No outcome data (n=)",
            "Other (n=)"
        )
    )

    y <- 0
    top.h <- consort_box_height(top.label, 60, canvas = canvas)
    canvas <- consort_add_box(canvas, 20, 80, y - top.h, y, box.fill)
    canvas <- consort_add_text(
        canvas, 50, y - top.h / 2, consort_wrap(top.label, 50)
    )
    y <- y - top.h - 1.2

    excl.pre.h <- consort_box_height(excl.pre.label, 42, canvas = canvas)
    canvas <- consort_add_box(canvas, 58, 98, y - excl.pre.h, y, "white")
    canvas <- consort_add_text(
        canvas, 60, y - canvas$pad / 2, consort_wrap_preserve(excl.pre.label, 40),
        hjust = 0, vjust = 1
    )
    branch.y <- y - excl.pre.h / 2
    canvas <- consort_add_segment(canvas, 50, 50, y + 1.2, y - excl.pre.h - 1.2, arrow = FALSE)
    canvas <- consort_add_segment(canvas, 50, 58, branch.y, branch.y, arrow = TRUE)
    y <- y - excl.pre.h - 1.2

    rand.h <- consort_box_height(rand.label, 50, canvas = canvas)
    canvas <- consort_add_box(canvas, 22, 78, y - rand.h, y, box.fill)
    canvas <- consort_add_text(
        canvas, 50, y - rand.h / 2, consort_wrap(rand.label, 45)
    )
    rand.bottom <- y - rand.h
    y <- rand.bottom - 2.2

    ## Bus to sequences
    canvas <- consort_add_segment(canvas, 50, 50, rand.bottom, y + 0.6, arrow = FALSE)
    canvas <- consort_add_segment(
        canvas, min(col.center), max(col.center), y + 0.6, y + 0.6, arrow = FALSE
    )

    strip.h <- 1.8
    title.h <- 2.4
    excl.post.h <- consort_box_height(excl.post.label, wrap.chars, canvas = canvas)
    included.h <- consort_box_height(included.label, wrap.chars, canvas = canvas)
    seq.block.top <- y
    seq.block.bottom <- y - title.h - strip.h - 0.7 - excl.post.h - 0.6 - included.h

    for (k in seq_len(n.seq)) {
        canvas <- consort_add_segment(
            canvas, col.center[k], col.center[k], y + 0.6, y, arrow = TRUE
        )
        canvas <- consort_add_text(
            canvas, col.center[k], y - 0.15,
            paste0("Sequence ", k), fontface = "bold", vjust = 1
        )
        strip.top <- y - title.h
        strip.bottom <- strip.top - strip.h
        strip <- consort_sequence_strip(
            sequence = k,
            sequences = n.seq,
            xmin = col.left[k] + 0.4,
            xmax = col.right[k] - 0.4,
            ymin = strip.bottom,
            ymax = strip.top,
            min.standard.care.months = min.standard.care.months,
            transition.months = transition.months,
            total.months = total.months,
            control.fill = control.fill,
            transition.fill = transition.fill,
            intervention.fill = intervention.fill
        )
        canvas$strips <- rbind(canvas$strips, strip)

        excl.top <- strip.bottom - 0.7
        canvas <- consort_add_segment(
            canvas, col.center[k], col.center[k],
            strip.bottom, excl.top, arrow = TRUE
        )
        canvas <- consort_add_box(
            canvas, col.left[k], col.right[k],
            excl.top - excl.post.h, excl.top, "white"
        )
        canvas <- consort_add_text(
            canvas, col.left[k] + 0.4, excl.top - canvas$pad / 2,
            consort_wrap_preserve(excl.post.label, wrap.chars),
            hjust = 0, vjust = 1
        )

        incl.top <- excl.top - excl.post.h - 0.6
        canvas <- consort_add_segment(
            canvas, col.center[k], col.center[k],
            excl.top - excl.post.h, incl.top, arrow = TRUE
        )
        canvas <- consort_add_box(
            canvas, col.left[k], col.right[k],
            incl.top - included.h, incl.top, box.fill
        )
        canvas <- consort_add_text(
            canvas, col.center[k], incl.top - included.h / 2,
            consort_wrap(included.label, wrap.chars)
        )
    }

    y <- seq.block.bottom - 1.8
    canvas <- consort_add_segment(
        canvas, min(col.center), max(col.center),
        seq.block.bottom + 0.15, seq.block.bottom + 0.15, arrow = FALSE
    )
    canvas <- consort_add_segment(canvas, 50, 50, seq.block.bottom + 0.15, y, arrow = TRUE)

    tot.incl.h <- consort_box_height(total.included.label, 55, canvas = canvas)
    canvas <- consort_add_box(canvas, 18, 82, y - tot.incl.h, y, box.fill)
    canvas <- consort_add_text(
        canvas, 50, y - tot.incl.h / 2, consort_wrap(total.included.label, 50)
    )
    y <- y - tot.incl.h - 0.8

    tot.excl.h <- consort_box_height(total.excl.label, 55, canvas = canvas)
    canvas <- consort_add_box(canvas, 18, 82, y - tot.excl.h, y, "white")
    canvas <- consort_add_text(
        canvas, 20, y - canvas$pad / 2, consort_wrap_preserve(total.excl.label, 50),
        hjust = 0, vjust = 1
    )
    y <- y - tot.excl.h - 1.8

    ## Legend
    legend.y <- y
    legend.items <- data.frame(
        fill = c(control.fill, transition.fill, intervention.fill),
        label = c("Standard care", "Transition", "Intervention"),
        stringsAsFactors = FALSE
    )
    lx <- margin
    for (i in seq_len(nrow(legend.items))) {
        canvas <- consort_add_box(canvas, lx, lx + 3, legend.y - 1.6, legend.y, legend.items$fill[i])
        canvas <- consort_add_text(
            canvas, lx + 3.5, legend.y - 0.8, legend.items$label[i],
            hjust = 0, vjust = 0.5
        )
        lx <- lx + 22
    }
    y <- legend.y - 2.5

    note <- paste0(
        "Note: cluster-level CONSORT shell for the batched stepped-wedge design ",
        "(", batches, " batches; ", sequences, " sequences). Mini strips show the ",
        "scheduled condition by period. Complete n= and reasons at reporting."
    )
    canvas <- consort_add_text(
        canvas, margin, y, consort_wrap(note, 95),
        hjust = 0, vjust = 1
    )
    y.bottom <- y - consort_box_height(note, 95, canvas = canvas) - 0.5

    consort_render(
        canvas = canvas,
        y.top = 1,
        y.bottom = y.bottom,
        file.name = paste0("consort-diagram-clusters-", n.seq, "-sequences.", device),
        return.figure = return.figure,
        save = save,
        device = device
    )
}

#' Create patient-level CONSORT diagram shell
#'
#' Shell flowchart for patient flow through a batched stepped-wedge trial,
#' separate from the cluster-level CONSORT figure. Shows sequence-specific
#' inclusion/exclusion and before/after intervention aggregates.
#'
#' @inheritParams create_cluster_consort_diagram
#' @return A ggplot object or the saved file name.
#'
#' @examples
#' \dontrun{
#' noacsr::source_all_functions()
#' create_patient_consort_diagram(save = FALSE)
#' }
create_patient_consort_diagram <- function(sequences = 5,
                                           batches = 6,
                                           total.months = 13,
                                           min.standard.care.months = 4,
                                           transition.months = 1,
                                           page.width.mm = 174,
                                           return.figure = TRUE,
                                           save = TRUE,
                                           device = "png") {
    assertthat::assert_that(is.numeric(sequences) && length(sequences) == 1 && sequences > 0)
    assertthat::assert_that(is.numeric(batches) && length(batches) == 1 && batches > 0)
    assertthat::assert_that(is.numeric(total.months) && length(total.months) == 1 && total.months > 0)
    assertthat::assert_that(
        is.numeric(min.standard.care.months) && length(min.standard.care.months) == 1 &&
            min.standard.care.months >= 0
    )
    assertthat::assert_that(
        is.numeric(transition.months) && length(transition.months) == 1 &&
            transition.months >= 0
    )

    cols <- colors()
    control.fill <- unname(cols["light.standard.care"])
    transition.fill <- unname(cols["light.transition"])
    intervention.fill <- unname(cols["light.intervention"])
    box.fill <- "#fde6d4"

    canvas <- consort_shell_canvas(page.width.mm = page.width.mm)
    n.seq <- as.integer(sequences)
    margin <- 2
    gap <- 1.2
    col.width <- (100 - 2 * margin - (n.seq - 1) * gap) / n.seq
    col.left <- margin + (seq_len(n.seq) - 1) * (col.width + gap)
    col.right <- col.left + col.width
    col.center <- (col.left + col.right) / 2
    wrap.chars <- max(18, floor(col.width * 1.45))

    top.label <- "Patients entered the trial (n=)"
    excl.label <- consort_bullet_list(
        "Lost/excluded (n=; reasons):",
        c(
            "Lost to follow-up (n=)",
            "Withdrew consent (n=)",
            "Other (n=)"
        )
    )
    included.label <- "Patients included in primary analysis (n=)"
    before.label <- paste(
        "Before ATLS training",
        "Patients (n=)",
        "Included in primary analysis (n=)",
        consort_bullet_list(
            "Excluded (n=; reasons):",
            c(
                "Lost to follow-up (n=)",
                "Withdrew consent (n=)",
                "Other (n=)"
            )
        ),
        sep = "\n"
    )
    after.label <- paste(
        "After ATLS training",
        "Patients (n=)",
        "Included in primary analysis (n=)",
        consort_bullet_list(
            "Excluded (n=; reasons):",
            c(
                "Lost to follow-up (n=)",
                "Withdrew consent (n=)",
                "Other (n=)"
            )
        ),
        sep = "\n"
    )
    total.included.label <- "Patients included in primary analysis (n=)"
    total.excl.label <- consort_bullet_list(
        "Patients excluded from primary analysis (n=):",
        c(
            "Lost to follow-up (n=)",
            "Withdrew consent (n=)",
            "Other (n=)"
        )
    )

    y <- 0
    top.h <- consort_box_height(top.label, 55, canvas = canvas)
    canvas <- consort_add_box(canvas, 22, 78, y - top.h, y, box.fill)
    canvas <- consort_add_text(
        canvas, 50, y - top.h / 2, consort_wrap(top.label, 50)
    )
    top.bottom <- y - top.h
    y <- top.bottom - 2.2

    canvas <- consort_add_segment(canvas, 50, 50, top.bottom, y + 0.6, arrow = FALSE)
    canvas <- consort_add_segment(
        canvas, min(col.center), max(col.center), y + 0.6, y + 0.6, arrow = FALSE
    )

    strip.h <- 1.8
    title.h <- 2.4
    excl.h <- consort_box_height(excl.label, wrap.chars, canvas = canvas)
    included.h <- consort_box_height(included.label, wrap.chars, canvas = canvas)
    seq.block.bottom <- y - title.h - strip.h - 0.7 - excl.h - 0.6 - included.h

    for (k in seq_len(n.seq)) {
        canvas <- consort_add_segment(
            canvas, col.center[k], col.center[k], y + 0.6, y, arrow = TRUE
        )
        canvas <- consort_add_text(
            canvas, col.center[k], y - 0.15,
            paste0("Sequence ", k), fontface = "bold", vjust = 1
        )
        strip.top <- y - title.h
        strip.bottom <- strip.top - strip.h
        strip <- consort_sequence_strip(
            sequence = k,
            sequences = n.seq,
            xmin = col.left[k] + 0.4,
            xmax = col.right[k] - 0.4,
            ymin = strip.bottom,
            ymax = strip.top,
            min.standard.care.months = min.standard.care.months,
            transition.months = transition.months,
            total.months = total.months,
            control.fill = control.fill,
            transition.fill = transition.fill,
            intervention.fill = intervention.fill
        )
        canvas$strips <- rbind(canvas$strips, strip)

        excl.top <- strip.bottom - 0.7
        canvas <- consort_add_segment(
            canvas, col.center[k], col.center[k],
            strip.bottom, excl.top, arrow = TRUE
        )
        canvas <- consort_add_box(
            canvas, col.left[k], col.right[k],
            excl.top - excl.h, excl.top, "white"
        )
        canvas <- consort_add_text(
            canvas, col.left[k] + 0.4, excl.top - canvas$pad / 2,
            consort_wrap_preserve(excl.label, wrap.chars),
            hjust = 0, vjust = 1
        )

        incl.top <- excl.top - excl.h - 0.6
        canvas <- consort_add_segment(
            canvas, col.center[k], col.center[k],
            excl.top - excl.h, incl.top, arrow = TRUE
        )
        canvas <- consort_add_box(
            canvas, col.left[k], col.right[k],
            incl.top - included.h, incl.top, box.fill
        )
        canvas <- consort_add_text(
            canvas, col.center[k], incl.top - included.h / 2,
            consort_wrap(included.label, wrap.chars)
        )
    }

    y <- seq.block.bottom - 1.8
    canvas <- consort_add_segment(
        canvas, min(col.center), max(col.center),
        seq.block.bottom + 0.15, seq.block.bottom + 0.15, arrow = FALSE
    )
    canvas <- consort_add_segment(canvas, 50, 50, seq.block.bottom + 0.15, y, arrow = TRUE)

    phase.h <- consort_box_height(before.label, 40, canvas = canvas)
    canvas <- consort_add_box(canvas, 8, 48, y - phase.h, y, box.fill)
    canvas <- consort_add_text(
        canvas, 10, y - canvas$pad / 2, consort_wrap_preserve(before.label, 38),
        hjust = 0, vjust = 1
    )
    canvas <- consort_add_box(canvas, 52, 92, y - phase.h, y, box.fill)
    canvas <- consort_add_text(
        canvas, 54, y - canvas$pad / 2, consort_wrap_preserve(after.label, 38),
        hjust = 0, vjust = 1
    )
    phase.bottom <- y - phase.h
    y <- phase.bottom - 1.8

    canvas <- consort_add_segment(canvas, 28, 28, phase.bottom, y + 1, arrow = FALSE)
    canvas <- consort_add_segment(canvas, 72, 72, phase.bottom, y + 1, arrow = FALSE)
    canvas <- consort_add_segment(canvas, 28, 72, y + 1, y + 1, arrow = FALSE)
    canvas <- consort_add_segment(canvas, 50, 50, y + 1, y, arrow = TRUE)

    tot.incl.h <- consort_box_height(total.included.label, 55, canvas = canvas)
    canvas <- consort_add_box(canvas, 18, 82, y - tot.incl.h, y, box.fill)
    canvas <- consort_add_text(
        canvas, 50, y - tot.incl.h / 2, consort_wrap(total.included.label, 50)
    )
    y <- y - tot.incl.h - 0.8

    tot.excl.h <- consort_box_height(total.excl.label, 55, canvas = canvas)
    canvas <- consort_add_box(canvas, 18, 82, y - tot.excl.h, y, "white")
    canvas <- consort_add_text(
        canvas, 20, y - canvas$pad / 2, consort_wrap_preserve(total.excl.label, 50),
        hjust = 0, vjust = 1
    )
    y <- y - tot.excl.h - 1.8

    legend.y <- y
    legend.items <- data.frame(
        fill = c(control.fill, transition.fill, intervention.fill),
        label = c("Standard care", "Transition", "Intervention"),
        stringsAsFactors = FALSE
    )
    lx <- margin
    for (i in seq_len(nrow(legend.items))) {
        canvas <- consort_add_box(canvas, lx, lx + 3, legend.y - 1.6, legend.y, legend.items$fill[i])
        canvas <- consort_add_text(
            canvas, lx + 3.5, legend.y - 0.8, legend.items$label[i],
            hjust = 0, vjust = 0.5
        )
        lx <- lx + 22
    }
    y <- legend.y - 2.5

    note <- paste0(
        "Note: patient-level CONSORT shell for the batched stepped-wedge design ",
        "(", batches, " batches; ", sequences, " sequences). Before/after ATLS ",
        "aggregates summarise receipt of the intended intervention condition. ",
        "Complete n= and reasons at reporting."
    )
    canvas <- consort_add_text(
        canvas, margin, y, consort_wrap(note, 95),
        hjust = 0, vjust = 1
    )
    y.bottom <- y - consort_box_height(note, 95, canvas = canvas) - 0.5

    consort_render(
        canvas = canvas,
        y.top = 1,
        y.bottom = y.bottom,
        file.name = paste0("consort-diagram-patients-", n.seq, "-sequences.", device),
        return.figure = return.figure,
        save = save,
        device = device
    )
}

#' Create CONSORT diagram (deprecated wrapper)
#'
#' Retained for older call sites. Prefer
#' [create_cluster_consort_diagram()] and
#' [create_patient_consort_diagram()].
#'
#' @inheritParams create_cluster_consort_diagram
#' @param periods Ignored; use `total.months`.
#' @param ... Additional arguments passed to `create_cluster_consort_diagram`.
#' @return A ggplot object or saved file name from the cluster diagram.
create_consort_diagram <- function(sequences = 5,
                                   periods = 13,
                                   min.standard.care.months = 4,
                                   transition.months = 1,
                                   ...,
                                   total.months = periods) {
    create_cluster_consort_diagram(
        sequences = sequences,
        total.months = total.months,
        min.standard.care.months = min.standard.care.months,
        transition.months = transition.months,
        ...
    )
}
