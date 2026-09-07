# ---------------------------------------------------------------------------
# R/checks.R -- the parsing helpers the tests are written against.
#
# The tests read rendered SVG as XML, never as a string with a regex over it:
# a regex cannot tell a fill on a <rect> from the word "#1F4E79" inside a
# <desc>, and the difference is exactly what rule 2 is about.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(xml2)
  library(stringr)
  library(tibble)
  library(dplyr)
})

SVG_NS <- c(svg = "http://www.w3.org/2000/svg")

#' Parse a rendered SVG string.
svg_doc <- function(svg) xml2::read_xml(svg)

#' Every font-size actually set on a <text> element, in viewBox units.
font_sizes <- function(svg) {
  doc <- if (inherits(svg, "xml_document")) svg else svg_doc(svg)
  nodes <- xml2::xml_find_all(doc, "//svg:text", SVG_NS)
  if (!length(nodes)) return(numeric(0))
  as.numeric(xml2::xml_attr(nodes, "font-size"))
}

#' Every colour used as paint, by element.
#'
#' Returns one row per painted attribute so a test can say *where* a hue was
#' used, not just that it appeared somewhere in the file.
hex_codes <- function(svg) {
  doc <- if (inherits(svg, "xml_document")) svg else svg_doc(svg)
  nodes <- xml2::xml_find_all(doc, "//svg:*[@fill or @stroke]", SVG_NS)
  if (!length(nodes)) return(tibble::tibble(element = character(0),
                                            attr = character(0),
                                            hex = character(0)))
  out <- lapply(c("fill", "stroke"), function(a) {
    v <- xml2::xml_attr(nodes, a)
    keep <- !is.na(v) & grepl("^#", v)
    tibble::tibble(
      element = xml2::xml_name(nodes)[keep],
      attr    = a,
      hex     = toupper(v[keep])
    )
  })
  dplyr::bind_rows(out)
}

#' Every text string that actually made it into the rendered SVG.
svg_texts <- function(svg) {
  doc <- if (inherits(svg, "xml_document")) svg else svg_doc(svg)
  xml2::xml_text(xml2::xml_find_all(doc, "//svg:text", SVG_NS))
}

# -- spec-side label access -------------------------------------------------

#' Every label in a spec, flattened.
#'
#' @return tibble(owner, kind, tier, role, text)
all_labels <- function(spec) {
  rows <- list()
  push <- function(owner, kind, labels) {
    for (l in labels) {
      rows[[length(rows) + 1]] <<- tibble::tibble(
        owner = owner, kind = kind,
        tier = l$tier %||% "L1", role = l$role %||% "block",
        text = l$text
      )
    }
  }
  for (n in spec$nodes)    push(n$id, "node", n$labels)
  for (g in spec$gates)    push(g$id, "gate", g$labels)
  for (b in spec$brackets) push(b$id, "bracket", b$labels)
  for (b in spec$bands)    push(b$id, "band", b$labels)
  for (l in spec$legend) {
    rows[[length(rows) + 1]] <- tibble::tibble(
      owner = "legend", kind = "legend",
      tier = l$tier %||% "L1", role = "legend", text = l$text
    )
  }
  if (!length(rows)) {
    return(tibble::tibble(owner = character(0), kind = character(0),
                          tier = character(0), role = character(0),
                          text = character(0)))
  }
  dplyr::bind_rows(rows)
}

#' Label texts at exactly one tier.
labels_at_tier <- function(spec, tier) {
  lab <- all_labels(spec)
  lab$text[lab$tier == tier]
}

#' Label texts at `tier` or any tier shown before it (L1 <= L2 <= L3).
labels_at_or_above <- function(spec, tier) {
  lab <- all_labels(spec)
  lab$text[match(lab$tier, TIER_ORDER) <= match(tier, TIER_ORDER)]
}

#' The L1 label of every node, in spec order.
l1_node_labels <- function(spec) {
  out <- lapply(spec$nodes, function(n) {
    txt <- vapply(Filter(function(l) l$tier == "L1", n$labels),
                  function(l) l$text, character(1))
    tibble::tibble(node = n$id, text = txt)
  })
  dplyr::bind_rows(out)
}

# -- token extraction -------------------------------------------------------

TBC <- "[TBC]"

#' Numeric tokens in a string: runs of digits, with internal . or , kept.
#'
#' "1143/2014" yields two tokens, because the proposal writes the regulation
#' number and the year as separate facts; "A1.2/A1.3" yields "1.2" and "1.3".
numeric_tokens <- function(text) {
  m <- stringr::str_extract_all(text, "[0-9]+(?:[.,][0-9]+)*")
  unique(unlist(m))
}

#' Milestone / deliverable / objective / risk / activity IDs in a string.
id_tokens <- function(text) {
  m <- stringr::str_extract_all(
    text, "(?<![A-Za-z0-9])[MDORA][0-9]+(?:\\.[0-9]+)?(?![A-Za-z0-9])"
  )
  unique(unlist(m))
}

#' All-caps tokens in a string.
acronym_tokens <- function(text) {
  m <- stringr::str_extract_all(text, "(?<![A-Za-z0-9])[A-Z]{2,}(?![A-Za-z0-9])")
  unique(unlist(m))
}

#' Compare numbers without arguing about thousands separators.
#'
#' The proposal writes "5000"; a caption reads better as "5,000". Both denote
#' the same fact, so the check normalises before comparing.
normalise_number <- function(x) gsub(",", "", x, fixed = TRUE)

#' Strip the [TBC] marker before token extraction.
without_tbc <- function(text) gsub("[TBC]", " ", text, fixed = TRUE)

#' Is this acronym immediately flagged as unverified, as in "AMI [TBC]"?
tbc_flagged <- function(text, acr) {
  grepl(paste0("(?<![A-Za-z0-9])", acr, "\\s*\\[TBC\\]"), text, perl = TRUE)
}

# -- acronym expansion within a figure --------------------------------------

#' Acronyms a figure expands itself, and the tier the expansion sits at.
#'
#' Recognises both "Expansion (ACRONYM)" and "ACRONYM (Expansion)". The
#' expansion has to be in the figure, not merely in the proposal: a reader
#' looking at a slide does not have the proposal open.
#'
#' @return named integer vector: acronym -> tier index of its expansion
figure_expansions <- function(spec) {
  lab <- all_labels(spec)
  out <- integer(0)
  for (i in seq_len(nrow(lab))) {
    txt <- lab$text[i]
    ti  <- match(lab$tier[i], TIER_ORDER)

    # ACRONYM (Expansion)
    m <- stringr::str_match_all(
      txt, "(?<![A-Za-z0-9])([A-Z]{2,})\\s*\\(([^)]{2,90})\\)"
    )[[1]]
    if (nrow(m)) {
      for (r in seq_len(nrow(m))) {
        if (.initials_match(m[r, 3], m[r, 2])) {
          out <- .keep_min(out, m[r, 2], ti)
        }
      }
    }
    # Expansion (ACRONYM)
    m <- stringr::str_match_all(
      txt, "((?:[A-Za-z][A-Za-z'’-]*\\s+){1,7})\\(\\s*([A-Z]{2,})\\s*\\)"
    )[[1]]
    if (nrow(m)) {
      for (r in seq_len(nrow(m))) {
        if (.initials_match(m[r, 2], m[r, 3])) {
          out <- .keep_min(out, m[r, 3], ti)
        }
      }
    }
  }
  out
}

.keep_min <- function(v, key, val) {
  if (is.na(v[key]) || is.null(v[[key]]) || !key %in% names(v)) {
    v[key] <- val
  } else {
    v[key] <- min(v[[key]], val)
  }
  v
}

#' Do the initials of `phrase` spell `acr`? Stopwords may be skipped.
.initials_match <- function(phrase, acr) {
  words <- stringr::str_extract_all(phrase, "[A-Za-z][A-Za-z'’-]*")[[1]]
  if (!length(words)) return(FALSE)
  spell <- function(w) toupper(paste(substr(w, 1, 1), collapse = ""))
  keep <- words[!tolower(words) %in% c("of", "the", "and", "for", "in", "on", "a")]
  n <- nchar(acr)
  tails <- list(words, keep,
                utils::tail(words, n), utils::tail(keep, n))
  any(vapply(tails, function(w) identical(spell(w), acr), logical(1)))
}

# -- L1 phrasing ------------------------------------------------------------

#' Word count of a label.
n_words <- function(text) {
  length(stringr::str_extract_all(stringr::str_squish(text), "[^ ]+")[[1]])
}

#' Does this read as a sentence with more than one clause?
#'
#' A slide label is a caption, not prose. Anything with a clause joiner or
#' terminal punctuation is prose that has wandered into a box.
multi_clause <- function(text) {
  t <- stringr::str_squish(text)
  grepl("[.;:]\\s|[.;:]$", t) ||
    grepl("(?i)\\b(which|that|because|so that|while|whereas|although|and then)\\b",
          t, perl = TRUE) ||
    lengths(regmatches(t, gregexpr(",", t, fixed = TRUE))) > 1
}

# -- misc -------------------------------------------------------------------

#' Distinct edge kinds used in a figure.
edge_kinds <- function(spec) {
  unique(vapply(spec$edges, function(e) e$kind, character(1)))
}

#' Edge kinds the legend accounts for.
legend_kinds <- function(spec) {
  out <- character(0)
  for (l in spec$legend) {
    if (!is.null(l$kind)) out <- c(out, l$kind)
    for (k in c("flow", "feeds")) {
      if (grepl(paste0("\\b", k, "\\b"), l$text)) out <- c(out, k)
    }
  }
  unique(out)
}

#' Budget tiers a spec marks elements with (the only licence for a reserved hue).
budget_tiers_used <- function(spec) {
  unique(unlist(lapply(spec$nodes, function(n) n$budget)))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
