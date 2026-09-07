# ---------------------------------------------------------------------------
# R/render.R -- spec + profile -> SVG string.
#
# The SVG is emitted as text by this file rather than by a plotting package, so
# that <text> elements stay real <text> elements (editable, searchable, live in
# the PDF) and so that a one-word change in a spec produces a readable diff.
#
# Output is deterministic: no timestamps, no generated ids, fixed number
# formatting. Rendering the same spec twice gives byte-identical strings.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(stringr)
  library(glue)
})

FONT_STACK <- "system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif"

# -- text metrics -----------------------------------------------------------

# Relative advance widths (em) for the sans stack above. Approximate by design:
# they only need to be good enough to wrap text and to keep boxes honest.
.CHAR_W <- c(
  " " = 0.278, "!" = 0.278, "\"" = 0.355, "#" = 0.556, "$" = 0.556,
  "%" = 0.889, "&" = 0.667, "'" = 0.191, "(" = 0.333, ")" = 0.333,
  "*" = 0.389, "+" = 0.584, "," = 0.278, "-" = 0.333, "." = 0.278,
  "/" = 0.278, "0" = 0.556, "1" = 0.556, "2" = 0.556, "3" = 0.556,
  "4" = 0.556, "5" = 0.556, "6" = 0.556, "7" = 0.556, "8" = 0.556,
  "9" = 0.556, ":" = 0.278, ";" = 0.278, "<" = 0.584, "=" = 0.584,
  ">" = 0.584, "?" = 0.556, "@" = 1.015,
  "A" = 0.667, "B" = 0.667, "C" = 0.722, "D" = 0.722, "E" = 0.667,
  "F" = 0.611, "G" = 0.778, "H" = 0.722, "I" = 0.278, "J" = 0.500,
  "K" = 0.667, "L" = 0.556, "M" = 0.833, "N" = 0.722, "O" = 0.778,
  "P" = 0.667, "Q" = 0.778, "R" = 0.722, "S" = 0.667, "T" = 0.611,
  "U" = 0.722, "V" = 0.667, "W" = 0.944, "X" = 0.667, "Y" = 0.667,
  "Z" = 0.611, "[" = 0.278, "\\" = 0.278, "]" = 0.278, "^" = 0.469,
  "_" = 0.556, "`" = 0.333,
  "a" = 0.556, "b" = 0.556, "c" = 0.500, "d" = 0.556, "e" = 0.556,
  "f" = 0.278, "g" = 0.556, "h" = 0.556, "i" = 0.222, "j" = 0.222,
  "k" = 0.500, "l" = 0.222, "m" = 0.833, "n" = 0.556, "o" = 0.556,
  "p" = 0.556, "q" = 0.556, "r" = 0.333, "s" = 0.500, "t" = 0.278,
  "u" = 0.556, "v" = 0.500, "w" = 0.722, "x" = 0.500, "y" = 0.500,
  "z" = 0.500, "{" = 0.334, "|" = 0.260, "}" = 0.334, "~" = 0.584,
  "·" = 0.333,  #  middle dot
  "—" = 1.000,  #  em dash
  "–" = 0.556,  #  en dash
  "’" = 0.191,  #  right single quote
  "≥" = 0.584,  #  >=
  "≤" = 0.584,  #  <=
  "→" = 0.900,  #  right arrow
  "€" = 0.556   #  euro
)

BOLD_FACTOR <- 1.045

#' Width of a string at a given font size, in the profile's viewBox units.
text_width <- function(s, size, weight = "normal") {
  if (!length(s) || !nzchar(s)) return(0)
  ch <- strsplit(s, "")[[1]]
  w <- unname(.CHAR_W[ch])
  w[is.na(w)] <- 0.55
  total <- sum(w) * size
  if (identical(weight, "bold")) total * BOLD_FACTOR else total
}

#' Greedy word wrap to a maximum width.
wrap_text <- function(s, size, max_w, weight = "normal") {
  s <- stringr::str_squish(s)
  if (!nzchar(s)) return(character(0))
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  lines <- character(0)
  cur <- ""
  for (w in words) {
    cand <- if (nzchar(cur)) paste(cur, w) else w
    if (text_width(cand, size, weight) <= max_w || !nzchar(cur)) {
      cur <- cand
    } else {
      lines <- c(lines, cur)
      cur <- w
    }
  }
  if (nzchar(cur)) lines <- c(lines, cur)
  lines
}

LINE_HEIGHT <- 1.24
TEXT_ASCENT <- 0.80   # baseline offset from the top of a line box

# -- primitives -------------------------------------------------------------
#
# Everything is reduced to rects, polylines and text before emission. Keeping
# one flat list means the vertical compaction pass below can remap every y
# coordinate uniformly, whatever drew it.

.rect <- function(x, y, w, h, fill = NA, stroke = NA, sw = 0, rx = 0, dash = NULL) {
  list(type = "rect", x = x, y = y, w = w, h = h,
       fill = fill, stroke = stroke, sw = sw, rx = rx, dash = dash)
}

.poly <- function(pts, stroke = NA, sw = 0, fill = NA, dash = NULL,
                  marker = NULL, closed = FALSE) {
  list(type = "poly", pts = pts, stroke = stroke, sw = sw, fill = fill,
       dash = dash, marker = marker, closed = closed)
}

.circ <- function(cx, cy, r, fill = NA, stroke = NA, sw = 0) {
  list(type = "circ", cx = cx, cy = cy, r = r, fill = fill, stroke = stroke, sw = sw)
}

.txt <- function(x, y, text, size, fill = INK[["ink0"]], anchor = "middle",
                 weight = "normal") {
  list(type = "text", x = x, y = y, text = text, size = size,
       fill = fill, anchor = anchor, weight = weight)
}

#' Lay out a wrapped block of text and return the text primitives plus height.
.text_block <- function(text, size, max_w, x, y_top, anchor = "middle",
                        fill = INK[["ink0"]], weight = "normal") {
  lines <- wrap_text(text, size, max_w, weight)
  if (!length(lines)) return(list(items = list(), height = 0))
  lh <- size * LINE_HEIGHT
  items <- lapply(seq_along(lines), function(i) {
    .txt(x, y_top + (i - 1) * lh + size * TEXT_ASCENT, lines[[i]],
         size, fill, anchor, weight)
  })
  list(items = items, height = length(lines) * lh)
}

# -- geometry resolution ----------------------------------------------------

.node_by_id <- function(spec, id) {
  for (n in spec$nodes) if (identical(n$id, id)) return(n)
  stop("unknown node id: ", id)
}

.gate_by_id <- function(spec, id) {
  for (g in spec$gates) if (identical(g$id, id)) return(g)
  stop("unknown gate id: ", id)
}

#' Absolute pixel box for a node.
.box <- function(n, p) {
  list(x = n$x * p$width, y = n$y * p$height,
       w = n$w * p$width, h = n$h * p$height,
       cx = (n$x + n$w / 2) * p$width, cy = (n$y + n$h / 2) * p$height,
       x2 = (n$x + n$w) * p$width, y2 = (n$y + n$h) * p$height)
}

#' Labels of an element that this profile shows, in tier order.
visible_labels <- function(labels, profile) {
  if (!length(labels)) return(list())
  keep <- vapply(labels, function(l) shows_tier(profile, l$tier), logical(1))
  labels <- labels[keep]
  if (!length(labels)) return(list())
  ord <- order(match(vapply(labels, function(l) l$tier, character(1)), TIER_ORDER))
  labels[ord]
}

#' Where a gate sits: the middle of the gap between the two nodes it is on.
.gate_pos <- function(spec, g, p) {
  a <- .box(.node_by_id(spec, unlist(g$on)[[1]]), p)
  b <- .box(.node_by_id(spec, unlist(g$on)[[2]]), p)
  left  <- if (a$cx <= b$cx) a else b
  right <- if (a$cx <= b$cx) b else a
  list(x = (left$x2 + right$x) / 2,
       y = (left$cy + right$cy) / 2,
       hh = max(left$h, right$h) * 0.42)
}

# -- the renderer -----------------------------------------------------------

#' Render one figure.
#'
#' @param spec a spec from `read_spec()`
#' @param profile a profile from `get_profile()`
#' @return a single character string holding the whole SVG document
render_figure <- function(spec, profile) {
  stopifnot(inherits(profile, "figure_profile"))
  items <- .resolve(spec, profile)
  items <- .compact_y(items, profile)
  .emit_svg(items, spec, profile)
}

#' Padding inside a node box, proportional to the box rather than the canvas.
.inner_pad <- function(box_w) max(4, 0.055 * box_w)

#' Grow node boxes so an L1 label always fits inside its box.
#'
#' Font size is fixed by the profile, so a label that needs three lines on a
#' slide and two on A4 would otherwise burst its box on the slide. Boxes that
#' share a row grow together, so the row stays aligned.
.fit_rows <- function(spec, p) {
  nodes <- spec$nodes
  need <- vapply(nodes, function(n) {
    l1 <- Filter(function(l) l$tier == "L1", n$labels)
    if (!length(l1)) return(0)
    size <- font_for(p, l1[[1]]$role)
    bw <- n$w * p$width
    pad <- .inner_pad(bw)
    lines <- wrap_text(l1[[1]]$text, size, bw - 2 * pad)
    length(lines) * size * LINE_HEIGHT + pad
  }, numeric(1))

  key <- vapply(nodes, function(n) sprintf("%.4f:%.4f", n$y, n$h), character(1))
  for (k in unique(key)) {
    idx <- which(key == k)
    h_px <- nodes[[idx[[1]]]]$h * p$height
    want <- max(need[idx])
    if (want > h_px) {
      grow <- (want - h_px) / p$height
      for (i in idx) {
        nodes[[i]]$y <- nodes[[i]]$y - grow / 2
        nodes[[i]]$h <- nodes[[i]]$h + grow
      }
    }
  }
  nodes
}

.resolve <- function(spec, p) {
  spec$nodes <- .fit_rows(spec, p)

  items <- list()
  add <- function(...) items <<- c(items, list(...))
  addl <- function(l) items <<- c(items, l)

  pad  <- 0.012 * p$width
  gap  <- 0.012 * p$height
  margin <- 0.022 * p$width

  # --- title (the figure's message) ---------------------------------------
  tb <- .text_block(spec$message, font_for(p, "title"), p$width * 0.94,
                    x = 0.03 * p$width, y_top = 0.028 * p$height,
                    anchor = "start", fill = INK[["ink0"]], weight = "bold")
  addl(tb$items)

  # --- brackets ------------------------------------------------------------
  for (b in spec$brackets) {
    span <- .span_members(spec, b$spans)
    boxes <- lapply(span, function(id) .box(.node_by_id(spec, id), p))
    x1 <- min(vapply(boxes, function(v) v$x, numeric(1)))
    x2 <- max(vapply(boxes, function(v) v$x2, numeric(1)))
    yl <- (b$y %||% 0.14) * p$height
    tick <- 0.018 * p$height
    add(.poly(cbind(c(x1, x1, x2, x2), c(yl + tick, yl, yl, yl + tick)),
              stroke = INK[["ink2"]], sw = p$stroke[["rule"]]))
    labs <- visible_labels(b$labels, p)
    if (length(labs)) {
      # stack the bracket's labels upward from just above the rule. `at` lets a
      # label sit at one end of the span rather than over its middle, which is
      # what a note about a handover *between* two brackets needs.
      blocks <- list()
      total <- 0
      for (l in labs) {
        size <- font_for(p, l$role)
        at <- l$at %||% "middle"
        anchor <- switch(at, start = "start", end = "end", "middle")
        ax <- switch(at, start = x1, end = x2, (x1 + x2) / 2)
        bl <- .text_block(l$text, size, x2 - x1, ax, 0,
                          anchor = anchor,
                          fill = if (l$tier == "L1") INK[["ink0"]] else INK[["ink1"]],
                          weight = if (l$tier == "L1") "bold" else "normal")
        blocks[[length(blocks) + 1]] <- bl
        total <- total + bl$height
      }
      y_top <- yl - gap * 0.5 - total
      for (bl in blocks) {
        addl(lapply(bl$items, function(it) { it$y <- it$y + y_top; it }))
        y_top <- y_top + bl$height
      }
    }
  }

  # --- bands ---------------------------------------------------------------
  for (b in spec$bands) {
    span <- .span_members(spec, b$spans)
    boxes <- lapply(span, function(id) .box(.node_by_id(spec, id), p))
    x1 <- min(vapply(boxes, function(v) v$x, numeric(1))) - pad
    x2 <- max(vapply(boxes, function(v) v$x2, numeric(1))) + pad
    by <- (b$y %||% 0.82) * p$height
    bh <- (b$h %||% 0.07) * p$height
    add(.rect(x1, by, x2 - x1, bh, fill = INK[["ink3"]],
              stroke = INK[["ink2"]], sw = p$stroke[["hair"]], rx = 0.006 * p$width))
    labs <- visible_labels(b$labels, p)
    y_cursor <- by + gap * 0.45
    for (l in labs) {
      size <- font_for(p, l$role)
      bl <- .text_block(l$text, size, (x2 - x1) - 4 * pad, (x1 + x2) / 2, y_cursor,
                        anchor = "middle",
                        fill = if (l$tier == "L1") INK[["ink0"]] else INK[["ink1"]],
                        weight = if (l$tier == "L1") "bold" else "normal")
      addl(bl$items)
      y_cursor <- y_cursor + bl$height
    }
  }

  # --- edges (drawn under the boxes) --------------------------------------
  for (e in spec$edges) {
    addl(.resolve_edge(spec, e, p))
  }

  # --- nodes ---------------------------------------------------------------
  for (n in spec$nodes) {
    bx <- .box(n, p)
    stroke_col <- INK[["ink0"]]
    sw <- p$stroke[["edge"]]
    if (!is.null(n$budget)) {
      # the only place a reserved hue is allowed: an element the spec marks
      # with a budget tier
      if (!n$budget %in% names(TIER_HUES)) {
        stop("node ", n$id, ": unknown budget tier '", n$budget, "'")
      }
      stroke_col <- unname(TIER_HUES[[n$budget]])
      sw <- p$stroke[["heavy"]]
    }
    rx <- switch(n$shape, terminal = bx$h / 2, branch = 0.008 * p$width, 0)
    dash <- if (identical(n$shape, "branch")) c(0.010, 0.007) * p$width else NULL
    add(.rect(bx$x, bx$y, bx$w, bx$h,
              fill = PAPER, stroke = stroke_col, sw = sw, rx = rx, dash = dash))

    labs <- visible_labels(n$labels, p)
    l1 <- Filter(function(l) l$tier == "L1", labs)
    rest <- Filter(function(l) l$tier != "L1", labs)

    # L1 goes inside the box, vertically centred
    if (length(l1)) {
      size <- font_for(p, l1[[1]]$role)
      lines <- wrap_text(l1[[1]]$text, size, bx$w - 2 * .inner_pad(bx$w))
      lh <- size * LINE_HEIGHT
      y_top <- bx$y + (bx$h - length(lines) * lh) / 2
      for (i in seq_along(lines)) {
        add(.txt(bx$cx, y_top + (i - 1) * lh + size * TEXT_ASCENT,
                 lines[[i]], size, INK[["ink0"]], "middle", "bold"))
      }
    }

    # L2/L3 hang below (or above) the box in a column of their own. The column
    # is clamped to the canvas, so annotation on the outermost node stays on
    # the page instead of running off the right-hand edge.
    if (length(rest)) {
      col_w <- min(n$label_w * p$width, p$width - 2 * margin)
      col_cx <- min(max(bx$cx, margin + col_w / 2), p$width - margin - col_w / 2)
      above <- identical(n$label_side %||% "below", "above")
      y_top <- if (!is.null(n$label_y)) {
        n$label_y * p$height
      } else if (above) {
        NA_real_
      } else {
        bx$y2 + gap * 0.55
      }
      blocks <- list()
      total <- 0
      for (i in seq_along(rest)) {
        l <- rest[[i]]
        size <- font_for(p, l$role)
        bl <- .text_block(l$text, size, col_w, col_cx, 0, anchor = "middle",
                          fill = INK[["ink1"]])
        blocks[[length(blocks) + 1]] <- bl
        total <- total + bl$height + if (i < length(rest)) size * 0.30 else 0
      }
      if (is.na(y_top)) y_top <- bx$y - gap * 0.55 - total
      for (i in seq_along(blocks)) {
        bl <- blocks[[i]]
        addl(lapply(bl$items, function(it) { it$y <- it$y + y_top; it }))
        # a hairline of space between tiers, so L2 and L3 read as two notes
        # rather than one run-on paragraph
        y_top <- y_top + bl$height +
          if (i < length(blocks)) font_for(p, rest[[i]]$role) * 0.30 else 0
      }
    }
  }

  # --- gates ---------------------------------------------------------------
  # A gate with nothing to say at this profile's tiers is dropped whole, the
  # same rule labels get. Its number is its position in the spec, so a gate
  # keeps the same number in every medium even when a neighbour is dropped.
  for (i in seq_along(spec$gates)) {
    if (!length(visible_labels(spec$gates[[i]]$labels, p))) next
    addl(.resolve_gate(spec, spec$gates[[i]], i, p))
  }

  # --- legend --------------------------------------------------------------
  addl(.resolve_legend(spec, p))

  items
}

.resolve_edge <- function(spec, e, p) {
  a <- .box(.node_by_id(spec, e$from), p)
  b <- .box(.node_by_id(spec, e$to), p)
  marker <- paste0("arrow-", e$kind)
  sw <- p$stroke[["rule"]]
  col <- INK[["ink1"]]

  same_row <- abs(a$cy - b$cy) < max(a$h, b$h) * 0.5

  if (same_row) {
    if (a$cx <= b$cx) {
      pts <- cbind(c(a$x2, b$x), c(a$cy, b$cy))
    } else {
      pts <- cbind(c(a$x, b$x2), c(a$cy, b$cy))
    }
    return(list(.poly(pts, stroke = col, sw = sw, marker = marker)))
  }

  # not on the same row: leave sideways, run across, drop in from above/below.
  # When the edge names a gate, the gate marker is the visual destination.
  tx <- b$cx
  ty_stop <- if (a$cy < b$cy) b$y else b$y2
  if (!is.null(e$gate)) {
    gp <- .gate_pos(spec, .gate_by_id(spec, e$gate), p)
    tx <- gp$x
    ty_stop <- if (a$cy < gp$y) gp$y - gp$hh else gp$y + gp$hh
  }
  exit_x <- if (tx >= a$cx) a$x2 else a$x
  pts <- cbind(c(exit_x, tx, tx), c(a$cy, a$cy, ty_stop))
  list(.poly(pts, stroke = col, sw = sw, marker = marker))
}

.resolve_gate <- function(spec, g, index, p) {
  out <- list()
  gp <- .gate_pos(spec, g, p)
  notch <- 0.006 * p$width

  # a notched vertical bar across the connector: reads as a checkpoint without
  # relying on colour, and survives greyscale
  pts <- cbind(
    c(gp$x, gp$x, gp$x + notch, gp$x, gp$x),
    c(gp$y - gp$hh, gp$y - gp$hh * 0.34, gp$y, gp$y + gp$hh * 0.34, gp$y + gp$hh)
  )
  out <- c(out, list(.poly(pts, stroke = INK[["ink0"]], sw = p$stroke[["heavy"]])))

  # the number, in a ring below the bar. Numbering is positional along the
  # figure, so a reader counts 1, 2, 3 ... whatever the gate ids happen to be.
  r <- font_for(p, "gate") * 0.62
  cy <- gp$y + gp$hh + r * 1.05
  out <- c(out, list(
    .circ(gp$x, cy, r, fill = PAPER, stroke = INK[["ink0"]], sw = p$stroke[["hair"]]),
    .txt(gp$x, cy + font_for(p, "gate") * 0.35, as.character(index),
         font_for(p, "gate"), INK[["ink0"]], "middle", "bold")
  ))

  labs <- visible_labels(g$labels, p)
  if (length(labs)) {
    col_w <- (g$label_w %||% 0.15) * p$width
    y_top <- if (!is.null(g$label_y)) g$label_y * p$height else cy + r * 1.4
    for (l in labs) {
      size <- font_for(p, l$role)
      bl <- .text_block(l$text, size, col_w, gp$x, y_top, anchor = "middle",
                        fill = if (l$tier == "L1") INK[["ink0"]] else INK[["ink1"]],
                        weight = if (l$tier == "L1") "bold" else "normal")
      out <- c(out, bl$items)
      y_top <- y_top + bl$height
    }
  }
  out
}

.resolve_legend <- function(spec, p) {
  entries <- Filter(function(l) shows_tier(p, l$tier), spec$legend)
  if (!length(entries)) return(list())

  size <- font_for(p, "legend")
  x0 <- 0.03 * p$width
  key_w <- size * 2.6
  gap_x <- size * 1.6
  row_step <- size * LINE_HEIGHT * 1.15

  # First work out how many rows the entries need, then place the block. The
  # legend sits on the bottom margin rather than at a fixed unit y, because how
  # many rows it takes depends on the profile: a slide shows three entries on
  # one row where a poster shows six on two.
  rows <- 1
  x <- x0
  for (l in entries) {
    tw <- text_width(l$text, size)
    if (x + key_w + tw > 0.97 * p$width && x > x0) {
      rows <- rows + 1
      x <- x0
    }
    x <- x + key_w + tw + gap_x
  }

  y <- if (!is.null(spec$layout$legend_y)) {
    spec$layout$legend_y * p$height
  } else {
    bottom <- p$height - 0.022 * p$height
    bottom - size * (LINE_HEIGHT - TEXT_ASCENT) - (rows - 1) * row_step
  }

  x <- x0
  out <- list()

  for (l in entries) {
    tw <- text_width(l$text, size)
    if (x + key_w + tw > 0.97 * p$width && x > x0) {
      x <- x0
      y <- y + row_step
    }
    if (!is.null(l$kind)) {
      out <- c(out, list(.poly(
        cbind(c(x, x + key_w * 0.78), c(y, y)),
        stroke = INK[["ink1"]], sw = p$stroke[["rule"]],
        marker = paste0("arrow-", l$kind)
      )))
    } else if (!is.null(l$budget)) {
      out <- c(out, list(.rect(x, y - size * 0.42, key_w * 0.62, size * 0.84,
                               fill = PAPER,
                               stroke = unname(TIER_HUES[[l$budget]]),
                               sw = p$stroke[["edge"]])))
    } else if (identical(l$key, "gate")) {
      # a miniature of the gate mark itself, so the key looks like the thing
      hh <- size * 0.52
      nx <- size * 0.20
      cx <- x + key_w * 0.34
      out <- c(out, list(.poly(
        cbind(c(cx, cx, cx + nx, cx, cx),
              c(y - hh, y - hh * 0.34, y, y + hh * 0.34, y + hh)),
        stroke = INK[["ink0"]], sw = p$stroke[["edge"]]
      )))
    } else {
      out <- c(out, list(.poly(
        cbind(c(x, x + key_w * 0.78), c(y, y)),
        stroke = INK[["ink2"]], sw = p$stroke[["rule"]]
      )))
    }
    out <- c(out, list(.txt(x + key_w, y + size * 0.34, l$text, size,
                            INK[["ink1"]], "start")))
    x <- x + key_w + tw + gap_x
  }
  out
}

# -- vertical compaction ----------------------------------------------------
#
# A profile that hides L2 and L3 leaves holes where the annotation would have
# been. Rather than have specs carry per-profile geometry, we squeeze any
# vertical run of empty canvas down to a fixed gutter and re-centre. The map is
# monotone and applied to every y, so lines and boxes move together.

.y_extent <- function(it) {
  switch(it$type,
    rect = c(it$y, it$y + it$h),
    circ = c(it$cy - it$r, it$cy + it$r),
    poly = range(it$pts[, 2]),
    text = c(it$y - it$size * TEXT_ASCENT, it$y + it$size * (LINE_HEIGHT - TEXT_ASCENT)),
    c(NA_real_, NA_real_)
  )
}

GAP_TRIGGER <- 0.10   # collapse empty runs taller than this fraction of canvas
GAP_TARGET  <- 0.055  # ... down to this fraction

.compact_y <- function(items, p) {
  if (!length(items)) return(items)
  ext <- vapply(items, .y_extent, numeric(2))
  # pad by a hair so a horizontal rule (zero height) still forms a real
  # interval; without this the breakpoint list can repeat an x and approx()
  # loses monotonicity
  lo <- ext[1, ] - 0.25; hi <- ext[2, ] + 0.25

  ord <- order(lo)
  merged <- list()
  for (i in ord) {
    if (!length(merged)) {
      merged[[1]] <- c(lo[i], hi[i])
    } else {
      last <- merged[[length(merged)]]
      if (lo[i] <= last[2]) {
        merged[[length(merged)]] <- c(last[1], max(last[2], hi[i]))
      } else {
        merged[[length(merged) + 1]] <- c(lo[i], hi[i])
      }
    }
  }
  if (length(merged) < 2) return(items)

  trigger <- GAP_TRIGGER * p$height
  target  <- GAP_TARGET * p$height

  # breakpoints: source y -> destination y, piecewise linear and monotone
  src <- merged[[1]][1]
  dst <- merged[[1]][1]
  bx <- src; by <- dst
  for (k in seq(2, length(merged))) {
    span <- merged[[k - 1]][2] - src
    src <- merged[[k - 1]][2]
    dst <- dst + span
    bx <- c(bx, src); by <- c(by, dst)
    hole <- merged[[k]][1] - merged[[k - 1]][2]
    keep <- if (hole > trigger) target else hole
    src <- merged[[k]][1]
    dst <- dst + keep
    bx <- c(bx, src); by <- c(by, dst)
  }
  span <- merged[[length(merged)]][2] - src
  bx <- c(bx, src + span); by <- c(by, dst + span)

  content_h <- by[length(by)] - by[1]
  shift <- (p$height - content_h) / 2 - by[1]
  map <- function(y) stats::approx(bx, by, xout = y, rule = 2)$y + shift

  lapply(items, function(it) {
    switch(it$type,
      rect = { y2 <- map(it$y + it$h); it$y <- map(it$y); it$h <- y2 - it$y; it },
      circ = { it$cy <- map(it$cy); it },
      poly = { it$pts[, 2] <- map(it$pts[, 2]); it },
      text = { it$y <- map(it$y); it },
      it
    )
  })
}

# -- emission ---------------------------------------------------------------

fnum <- function(v) formatC(round(v, 2), format = "f", digits = 2)

xml_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  gsub("\"", "&quot;", s, fixed = TRUE)
}

.attr_paint <- function(fill, stroke, sw, dash = NULL) {
  a <- character(0)
  a <- c(a, sprintf('fill="%s"', if (is.na(fill)) "none" else fill))
  if (!is.na(stroke)) {
    a <- c(a, sprintf('stroke="%s"', stroke),
           sprintf('stroke-width="%s"', fnum(sw)))
  }
  if (!is.null(dash)) {
    a <- c(a, sprintf('stroke-dasharray="%s"', paste(fnum(dash), collapse = " ")))
  }
  paste(a, collapse = " ")
}

.emit_item <- function(it) {
  switch(it$type,
    rect = sprintf('<rect x="%s" y="%s" width="%s" height="%s"%s %s/>',
                   fnum(it$x), fnum(it$y), fnum(it$w), fnum(it$h),
                   if (it$rx > 0) sprintf(' rx="%s"', fnum(it$rx)) else "",
                   .attr_paint(it$fill, it$stroke, it$sw, it$dash)),
    circ = sprintf('<circle cx="%s" cy="%s" r="%s" %s/>',
                   fnum(it$cx), fnum(it$cy), fnum(it$r),
                   .attr_paint(it$fill, it$stroke, it$sw)),
    poly = sprintf('<%s points="%s" %s stroke-linejoin="round" stroke-linecap="butt"%s/>',
                   if (it$closed) "polygon" else "polyline",
                   paste(sprintf("%s,%s", fnum(it$pts[, 1]), fnum(it$pts[, 2])),
                         collapse = " "),
                   .attr_paint(it$fill, it$stroke, it$sw, it$dash),
                   if (is.null(it$marker)) "" else
                     sprintf(' marker-end="url(#%s)"', it$marker)),
    text = sprintf('<text x="%s" y="%s" font-size="%s" fill="%s" text-anchor="%s"%s>%s</text>',
                   fnum(it$x), fnum(it$y), fnum(it$size), it$fill, it$anchor,
                   if (identical(it$weight, "bold")) ' font-weight="600"' else "",
                   xml_escape(it$text)),
    stop("unknown primitive: ", it$type)
  )
}

.markers <- function(p) {
  # markerUnits defaults to strokeWidth, so a marker already grows with the
  # profile through the stroke. Scaling markerWidth as well would square that.
  c(
    # flow: filled arrowhead
    sprintf(paste0('<marker id="arrow-flow" viewBox="0 0 10 10" refX="9" refY="5" ',
                   'markerWidth="6" markerHeight="6" orient="auto-start-reverse">',
                   '<path d="M 0 0 L 10 5 L 0 10 z" fill="%s"/></marker>'),
            INK[["ink1"]]),
    # feeds: open arrowhead -- same line, different head, so the distinction
    # survives greyscale and colour blindness
    sprintf(paste0('<marker id="arrow-feeds" viewBox="0 0 10 10" refX="9" refY="5" ',
                   'markerWidth="7" markerHeight="7" orient="auto-start-reverse">',
                   '<path d="M 0.5 0.5 L 9 5 L 0.5 9.5" fill="none" stroke="%s" ',
                   'stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"/>',
                   '</marker>'),
            INK[["ink1"]])
  )
}

.emit_svg <- function(items, spec, p) {
  body <- vapply(items, .emit_item, character(1))
  lines <- c(
    sprintf(paste0('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" ',
                   'width="%d" height="%d" role="img" aria-label="%s">'),
            p$width, p$height, p$width, p$height, xml_escape(spec$alt)),
    sprintf("  <title>%s</title>", xml_escape(spec$message)),
    sprintf("  <desc>%s</desc>", xml_escape(spec$caption)),
    "  <defs>",
    paste0("    ", .markers(p)),
    "  </defs>",
    sprintf('  <rect x="0" y="0" width="%d" height="%d" fill="%s"/>',
            p$width, p$height, PAPER),
    sprintf('  <g font-family="%s">', FONT_STACK),
    paste0("    ", body),
    "  </g>",
    "</svg>"
  )
  paste0(paste(lines, collapse = "\n"), "\n")
}

# -- writing ----------------------------------------------------------------

#' Render a spec to every profile and write the SVG files.
#'
#' @return the paths written, in profile order
render_all <- function(spec, profiles = all_profiles(spec$orientation),
                       outdir = "out") {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  vapply(names(profiles), function(nm) {
    svg <- render_figure(spec, profiles[[nm]])
    path <- file.path(outdir, sprintf("%s-%s.svg", spec$id, nm))
    con <- file(path, open = "wb")
    on.exit(close(con), add = TRUE)
    writeBin(charToRaw(enc2utf8(svg)), con)
    path
  }, character(1), USE.NAMES = FALSE)
}
