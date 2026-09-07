# ---------------------------------------------------------------------------
# R/spec.R -- read and validate one figure spec.
#
# A spec is the single declarative source for a figure: content, geometry in a
# unit square, caption and alt text. read_spec() errors loudly rather than
# rendering something subtly wrong, because a figure that renders is a figure
# that ships.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(yaml)
  library(glue)
})

MAX_CLUSTER <- 7

# YAML 1.1 reads the bare tokens y, n, on, off, yes and no as booleans -- which
# would silently turn the keys `y:` and `on:` in our schema into TRUE, and make
# two of them collide. Specs have no use for booleans, so we switch the implicit
# bool resolution off and get the literal text back instead.
.YAML_HANDLERS <- list(
  "bool#yes" = function(x) x,
  "bool#no"  = function(x) x
)

#' Read, normalise and validate a figure spec.
#'
#' @param path path to a .yml spec in figures/
#' @return the spec as a list, with defaults filled in
read_spec <- function(path) {
  if (!file.exists(path)) stop("no such spec: ", path)
  spec <- yaml::yaml.load_file(path, handlers = .YAML_HANDLERS)
  spec$path <- path
  spec <- .spec_defaults(spec)
  .validate_spec(spec)
  spec
}

.spec_defaults <- function(spec) {
  spec$orientation <- spec$orientation %||% "landscape"
  for (f in c("nodes", "edges", "gates", "brackets", "bands", "legend")) {
    if (is.null(spec[[f]])) spec[[f]] <- list()
  }

  spec$nodes <- lapply(spec$nodes, function(n) {
    n$shape  <- n$shape %||% "block"
    n$labels <- n$labels %||% list()
    # width of the annotation column beneath a node; wider than the box so L2
    # text has somewhere to go without the box having to grow.
    n$label_w <- n$label_w %||% (n$w * 1.35)
    n$labels <- lapply(n$labels, .label_defaults)
    n
  })
  spec$gates    <- lapply(spec$gates, function(g) {
    g$labels <- lapply(g$labels %||% list(), .label_defaults); g
  })
  spec$brackets <- lapply(spec$brackets, function(b) {
    b$side <- b$side %||% "top"
    b$labels <- lapply(b$labels %||% list(), .label_defaults); b
  })
  spec$bands <- lapply(spec$bands, function(b) {
    b$side <- b$side %||% "bottom"
    b$labels <- lapply(b$labels %||% list(), .label_defaults); b
  })
  spec$legend <- lapply(spec$legend, function(l) {
    l$tier <- l$tier %||% "L1"; l
  })
  spec
}

.label_defaults <- function(l) {
  l$tier <- l$tier %||% "L1"
  l$role <- l$role %||% "block"
  l
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# -- validation -------------------------------------------------------------

.stop_spec <- function(spec, ...) {
  stop(glue::glue("{basename(spec$path %||% '<spec>')}: "), ..., call. = FALSE)
}

.validate_spec <- function(spec) {
  for (f in c("id", "message", "caption", "alt")) {
    if (is.null(spec[[f]]) || !nzchar(trimws(spec[[f]]))) {
      .stop_spec(spec, "missing required field '", f, "'")
    }
  }
  if (nchar(spec$alt) >= 250) {
    .stop_spec(spec, "alt text is ", nchar(spec$alt),
               " characters; the limit is 250")
  }
  if (!spec$orientation %in% c("landscape", "portrait")) {
    .stop_spec(spec, "orientation must be 'landscape' or 'portrait', got '",
               spec$orientation, "'")
  }
  if (!length(spec$nodes)) .stop_spec(spec, "has no nodes")

  ids <- vapply(spec$nodes, function(n) n$id %||% NA_character_, character(1))
  if (anyNA(ids)) .stop_spec(spec, "every node needs an id")
  if (anyDuplicated(ids)) {
    .stop_spec(spec, "duplicate node id(s): ",
               paste(unique(ids[duplicated(ids)]), collapse = ", "))
  }

  .validate_nodes(spec, ids)
  .validate_edges(spec, ids)
  .validate_gates(spec, ids)
  .validate_groups(spec, ids)
  .validate_clusters(spec)
  invisible(TRUE)
}

.validate_nodes <- function(spec, ids) {
  for (n in spec$nodes) {
    if (!n$shape %in% c("block", "branch", "terminal")) {
      .stop_spec(spec, "node ", n$id, ": shape must be block|branch|terminal, got '",
                 n$shape, "'")
    }
    # every node has at least one L1 label -- a node that cannot appear on a
    # slide has no business being in the figure
    tiers <- vapply(n$labels, function(l) l$tier, character(1))
    if (!"L1" %in% tiers) {
      .stop_spec(spec, "node ", n$id, " has no L1 label")
    }
    for (l in n$labels) .validate_label(spec, l, glue::glue("node {n$id}"))

    for (k in c("x", "y", "w", "h")) {
      v <- n[[k]]
      if (is.null(v) || !is.numeric(v)) {
        .stop_spec(spec, "node ", n$id, ": '", k, "' must be numeric")
      }
    }
    if (n$x < 0 || n$y < 0 || n$x + n$w > 1 || n$y + n$h > 1) {
      .stop_spec(spec, "node ", n$id, " is outside the unit square: ",
                 glue::glue("x={n$x} y={n$y} w={n$w} h={n$h}"))
    }
    if (n$w <= 0 || n$h <= 0) {
      .stop_spec(spec, "node ", n$id, ": w and h must be positive")
    }
  }
  .validate_no_overlap(spec)
}

.validate_no_overlap <- function(spec) {
  ns <- spec$nodes
  if (length(ns) < 2) return(invisible(TRUE))
  for (i in seq_len(length(ns) - 1)) {
    for (j in seq(i + 1, length(ns))) {
      a <- ns[[i]]; b <- ns[[j]]
      ox <- min(a$x + a$w, b$x + b$w) - max(a$x, b$x)
      oy <- min(a$y + a$h, b$y + b$h) - max(a$y, b$y)
      if (ox > 1e-9 && oy > 1e-9) {
        .stop_spec(spec, "nodes ", a$id, " and ", b$id, " overlap by ",
                   sprintf("%.4f x %.4f", ox, oy), " of the unit square")
      }
    }
  }
  invisible(TRUE)
}

.validate_label <- function(spec, l, where) {
  if (is.null(l$text) || !nzchar(trimws(l$text))) {
    .stop_spec(spec, where, ": label with empty text")
  }
  if (!l$tier %in% TIER_ORDER) {
    .stop_spec(spec, where, ": tier must be one of ",
               paste(TIER_ORDER, collapse = "/"), ", got '", l$tier, "'")
  }
  if (!l$role %in% ROLES) {
    .stop_spec(spec, where, ": role must be one of ",
               paste(ROLES, collapse = "/"), ", got '", l$role, "'")
  }
}

.validate_edges <- function(spec, ids) {
  gate_ids <- vapply(spec$gates, function(g) g$id %||% NA_character_, character(1))
  for (e in spec$edges) {
    for (k in c("from", "to")) {
      if (is.null(e[[k]])) .stop_spec(spec, "edge is missing '", k, "'")
      if (!e[[k]] %in% ids) {
        .stop_spec(spec, "edge ", e$from, "->", e$to, " references unknown node '",
                   e[[k]], "'")
      }
    }
    # every edge has a stated kind: an arrow whose meaning is not stated is an
    # arrow the reader has to guess at
    if (is.null(e$kind) || !nzchar(e$kind)) {
      .stop_spec(spec, "edge ", e$from, "->", e$to, " has no 'kind'")
    }
    if (!e$kind %in% c("flow", "feeds")) {
      .stop_spec(spec, "edge ", e$from, "->", e$to, ": kind must be flow|feeds, got '",
                 e$kind, "'")
    }
    if (!is.null(e$gate) && !e$gate %in% gate_ids) {
      .stop_spec(spec, "edge ", e$from, "->", e$to, " references unknown gate '",
                 e$gate, "'")
    }
  }
  invisible(TRUE)
}

.validate_gates <- function(spec, ids) {
  gate_ids <- vapply(spec$gates, function(g) g$id %||% NA_character_, character(1))
  if (anyNA(gate_ids)) .stop_spec(spec, "every gate needs an id")
  if (anyDuplicated(gate_ids)) {
    .stop_spec(spec, "duplicate gate id(s): ",
               paste(unique(gate_ids[duplicated(gate_ids)]), collapse = ", "))
  }
  for (g in spec$gates) {
    if (length(g$on) != 2 || !all(unlist(g$on) %in% ids)) {
      .stop_spec(spec, "gate ", g$id,
                 ": 'on' must name two existing nodes, got ",
                 paste(unlist(g$on), collapse = ", "))
    }
    if (!length(g$labels)) .stop_spec(spec, "gate ", g$id, " has no labels")
    for (l in g$labels) .validate_label(spec, l, glue::glue("gate {g$id}"))
  }
  invisible(TRUE)
}

.validate_groups <- function(spec, ids) {
  for (what in c("brackets", "bands")) {
    for (b in spec[[what]]) {
      if (is.null(b$id)) .stop_spec(spec, "every ", what, " entry needs an id")
      if (length(b$spans) != 2 || !all(unlist(b$spans) %in% ids)) {
        .stop_spec(spec, what, " ", b$id,
                   ": 'spans' must name two existing nodes, got ",
                   paste(unlist(b$spans), collapse = ", "))
      }
      if (!b$side %in% c("top", "bottom")) {
        .stop_spec(spec, what, " ", b$id, ": side must be top|bottom")
      }
      for (l in b$labels) .validate_label(spec, l, glue::glue("{what} {b$id}"))
    }
  }
  for (l in spec$legend) {
    if (is.null(l$text) || !nzchar(l$text)) {
      .stop_spec(spec, "legend entry with empty text")
    }
    if (!l$tier %in% TIER_ORDER) {
      .stop_spec(spec, "legend entry '", l$text, "': bad tier '", l$tier, "'")
    }
  }
  invisible(TRUE)
}

# -- clusters ---------------------------------------------------------------

#' The visual clusters in a spec.
#'
#' A cluster is a set of elements the reader has to hold in mind at once. A
#' left-to-right chain is not one -- it is read in sequence -- so the spine is
#' not a cluster even when it is long. What does count:
#'
#'   * the nodes a bracket or band groups together under one label
#'   * the legend, read as a block
#'   * the stack of labels hung on a single element
#'
#' @return named list of character vectors
clusters <- function(spec) {
  out <- list()
  ids <- vapply(spec$nodes, function(n) n$id, character(1))

  for (what in c("brackets", "bands")) {
    for (b in spec[[what]]) {
      span <- .span_members(spec, b$spans)
      out[[paste0(what, ":", b$id)]] <- span
    }
  }
  if (length(spec$legend)) {
    out[["legend"]] <- vapply(spec$legend, function(l) l$text, character(1))
  }
  for (n in spec$nodes) {
    if (length(n$labels) > 1) {
      out[[paste0("labels:", n$id)]] <-
        vapply(n$labels, function(l) paste0(l$tier, ":", l$role), character(1))
    }
  }
  for (g in spec$gates) {
    if (length(g$labels) > 1) {
      out[[paste0("labels:", g$id)]] <-
        vapply(g$labels, function(l) paste0(l$tier, ":", l$role), character(1))
    }
  }
  out
}

#' Nodes covered by a bracket/band, in left-to-right (or top-to-bottom) order.
.span_members <- function(spec, spans) {
  spans <- unlist(spans)
  ns <- spec$nodes
  ids <- vapply(ns, function(n) n$id, character(1))
  xs  <- vapply(ns, function(n) n$x, numeric(1))
  a <- ns[[match(spans[[1]], ids)]]
  b <- ns[[match(spans[[2]], ids)]]
  lo <- min(a$x, b$x)
  hi <- max(a$x + a$w, b$x + b$w)
  # only nodes on the same horizontal run as the span endpoints
  ymid_a <- a$y + a$h / 2
  keep <- vapply(ns, function(n) {
    n$x >= lo - 1e-9 && n$x + n$w <= hi + 1e-9 &&
      abs((n$y + n$h / 2) - ymid_a) < max(a$h, n$h)
  }, logical(1))
  ids[keep][order(xs[keep])]
}

.validate_clusters <- function(spec) {
  cl <- clusters(spec)
  too_big <- cl[lengths(cl) > MAX_CLUSTER]
  if (length(too_big)) {
    msg <- paste(sprintf("  %s holds %d elements", names(too_big), lengths(too_big)),
                 collapse = "\n")
    .stop_spec(spec, "cluster(s) over ", MAX_CLUSTER, " elements:\n", msg)
  }
  invisible(TRUE)
}

#' Every spec in a directory, read and validated.
read_specs <- function(dir = "figures") {
  paths <- sort(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE))
  stats::setNames(lapply(paths, read_spec), basename(paths))
}
