# ---------------------------------------------------------------------------
# R/extract.R -- turn the proposal .docx into the three fact tables in data/.
#
# The proposal is the only permitted source of facts for any figure. Everything
# a label is allowed to quote has to come out of here first; the tests in
# tests/testthat/ then refuse to render a label that quotes anything else.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(officer)
  library(xml2)
  library(readr)
})

# -- shared helpers ---------------------------------------------------------

#' Flatten a .docx into one row per text run, carrying the section it sits in.
#'
#' `officer::docx_summary()` returns paragraphs and table cells in document
#' order, so we can carry the most recent heading forward as the section.
docx_blocks <- function(docx) {
  stopifnot(file.exists(docx))
  d <- officer::docx_summary(officer::read_docx(docx))

  is_heading <- !is.na(d$style_name) & grepl("^heading", tolower(d$style_name))
  section <- rep(NA_character_, nrow(d))
  current <- NA_character_
  for (i in seq_len(nrow(d))) {
    if (is_heading[i] && nzchar(trimws(d$text[i]))) current <- trimws(d$text[i])
    section[i] <- current
  }

  tibble::tibble(
    doc_index = d$doc_index,
    text      = trimws(d$text),
    section   = section
  ) |>
    dplyr::filter(nzchar(.data$text)) |>
    dplyr::mutate(section = dplyr::coalesce(.data$section, "(front matter)"))
}

#' Split a block of text into sentences, for the `context` column.
sentences <- function(x) {
  parts <- stringr::str_split(x, "(?<=[.;!?])\\s+(?=[A-Z0-9(•·])")[[1]]
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

#' The sentence within `text` that contains the match at `start`.
sentence_at <- function(text, start) {
  ss <- sentences(text)
  if (!length(ss)) return(text)
  # locate each sentence in the original string and pick the one spanning start
  pos <- 1L
  for (s in ss) {
    at <- stringr::str_locate(substring(text, pos), stringr::fixed(s))
    if (is.na(at[1, "start"])) next
    from <- pos + at[1, "start"] - 1L
    to   <- pos + at[1, "end"] - 1L
    if (start >= from && start <= to) return(s)
    pos <- to + 1L
  }
  ss[[1]]
}

#' Collect every match of `pattern` across the blocks, with sentence context.
harvest <- function(blocks, pattern, kind) {
  purrr::pmap_dfr(
    list(blocks$text, blocks$section),
    function(text, section) {
      m <- stringr::str_locate_all(text, pattern)[[1]]
      if (!nrow(m)) return(NULL)
      vals <- stringr::str_sub(text, m[, "start"], m[, "end"])
      tibble::tibble(
        value   = trimws(vals),
        kind    = kind,
        context = vapply(m[, "start"], function(s) sentence_at(text, s), character(1)),
        section = section
      )
    }
  )
}

# -- 1. facts ---------------------------------------------------------------

FACT_PATTERNS <- list(
  milestone   = "(?<![A-Za-z0-9])M[1-9][0-9]*\\.[0-9]+(?![A-Za-z0-9])",
  deliverable = "(?<![A-Za-z0-9])D[1-9][0-9]*\\.[0-9]+(?![A-Za-z0-9])",
  objective   = "(?<![A-Za-z0-9])O[1-9][0-9]*(?![A-Za-z0-9.])",
  risk        = "(?<![A-Za-z0-9])R[1-9][0-9]*(?![A-Za-z0-9.])",
  activity    = "(?<![A-Za-z0-9])A[1-9][0-9]*\\.[0-9]+(?![A-Za-z0-9])",
  euro        = paste0(
    "€\\s?[0-9][0-9., ]*[0-9]",
    "|[0-9][0-9.,]*[0-9]\\s?(?:€|EUR)"
  ),
  date        = paste0(
    "(?<![0-9])[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}(?![0-9])",
    "|(?<![A-Za-z])(?:Jan|Feb|Mar|Apr|May|Jun|June|Jul|July|Aug|Sept|Sep|Oct|Nov|Dec)",
    "\\.?\\s?(?:[0-9]{4}|[0-9]{2})(?![0-9])",
    "|(?<![0-9])20[0-9]{2}\\s?Q[1-4](?![0-9])",
    "|(?<![0-9])(?:19|20)[0-9]{2}(?![0-9])"
  ),
  number      = "(?<![A-Za-z0-9.,/])[0-9]+(?:[.,][0-9]+)*(?![A-Za-z0-9])"
)

#' Every atom a figure may legitimately quote.
#'
#' @return tibble(value, kind, context, section), also written to data/facts.csv
extract_facts <- function(docx, out = "data/facts.csv") {
  blocks <- docx_blocks(docx)

  facts <- purrr::imap_dfr(FACT_PATTERNS, function(pattern, kind) {
    harvest(blocks, pattern, kind)
  })

  facts <- facts |>
    dplyr::mutate(
      value   = trimws(.data$value),
      context = stringr::str_squish(.data$context)
    ) |>
    dplyr::filter(nzchar(.data$value)) |>
    dplyr::distinct(.data$value, .data$kind, .data$context, .data$section) |>
    dplyr::arrange(.data$kind, .data$value, .data$section, .data$context)

  if (!is.null(out)) {
    dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(facts, out, na = "")
  }
  facts
}

# -- 2. acronyms ------------------------------------------------------------

ACRONYM_RE <- "(?<![A-Za-z0-9])[A-Z]{2,}(?![A-Za-z0-9])"

#' Look for `Expansion (ACRONYM)` immediately before position `at`.
#'
#' Takes the `nchar(acr)` words in front of the parenthesis and keeps them only
#' if their initials spell the acronym -- so "Nature Conservation Agency (CZ)"
#' is correctly rejected while "... Coordination Centre (EBOCC)" is kept.
expansion_before <- function(text, acr, at) {
  before <- substring(text, 1, at - 1)
  if (!grepl("\\(\\s*$", before)) return(NA_character_)
  before <- sub("\\s*\\(\\s*$", "", before)
  words <- stringr::str_extract_all(before, "[A-Za-z][A-Za-z'’-]*")[[1]]
  n <- nchar(acr)
  if (length(words) < n) return(NA_character_)
  cand <- utils::tail(words, n)
  if (!identical(toupper(paste(substr(cand, 1, 1), collapse = "")), acr)) {
    return(NA_character_)
  }
  paste(cand, collapse = " ")
}

#' Look for `ACRONYM (Expansion)` immediately after the acronym.
expansion_after <- function(text, acr, end) {
  after <- substring(text, end + 1)
  m <- stringr::str_match(after, "^\\s*\\(([^)]{2,90})\\)")
  if (is.na(m[1, 1])) return(NA_character_)
  cand <- trimws(m[1, 2])
  words <- stringr::str_extract_all(cand, "[A-Za-z][A-Za-z'’-]*")[[1]]
  if (!length(words)) return(NA_character_)
  # accept when the initials of the significant words spell the acronym
  keep <- words[!tolower(words) %in% c("of", "the", "and", "for", "in", "on", "a")]
  spell <- function(w) toupper(paste(substr(w, 1, 1), collapse = ""))
  if (identical(spell(words), acr) || identical(spell(keep), acr)) return(cand)
  NA_character_
}

#' Every all-caps token in the proposal, with its expansion where one is given.
#'
#' AMI and FAIR are expected to come back with `expansion = NA`: the proposal
#' never spells them out. That is a fact about the proposal, not a bug here.
#'
#' @return tibble(term, expansion, section), also written to data/acronyms.csv
extract_acronyms <- function(docx, out = "data/acronyms.csv") {
  blocks <- docx_blocks(docx)

  hits <- purrr::pmap_dfr(
    list(blocks$text, blocks$section),
    function(text, section) {
      m <- stringr::str_locate_all(text, ACRONYM_RE)[[1]]
      if (!nrow(m)) return(NULL)
      terms <- stringr::str_sub(text, m[, "start"], m[, "end"])
      exps <- vapply(seq_len(nrow(m)), function(i) {
        b <- expansion_before(text, terms[i], m[i, "start"])
        if (!is.na(b)) return(b)
        expansion_after(text, terms[i], m[i, "end"])
      }, character(1))
      tibble::tibble(term = terms, expansion = exps, section = section)
    }
  )

  # one row per term/section; keep an expansion if the proposal gives one anywhere
  best <- hits |>
    dplyr::group_by(.data$term) |>
    dplyr::summarise(
      expansion = {
        e <- .data$expansion[!is.na(.data$expansion)]
        if (length(e)) e[[1]] else NA_character_
      },
      .groups = "drop"
    )

  acronyms <- hits |>
    dplyr::distinct(.data$term, .data$section) |>
    dplyr::left_join(best, by = "term") |>
    dplyr::select("term", "expansion", "section") |>
    dplyr::arrange(.data$term, .data$section)

  if (!is.null(out)) {
    dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(acronyms, out, na = "")
  }
  acronyms
}

# -- 3. tiers ---------------------------------------------------------------

# Section 6.1 keys the three budget tiers to cell shading and says so in its own
# header cell ("blue = CORE, green = enhanced, orange = SCALABLE"). Any markdown
# conversion drops that shading, so we read it out of word/document.xml.

# Hue classification is done on the fill itself rather than on a hard-coded list
# of hex values, so a re-shaded document still resolves correctly.
classify_hue <- function(fill) {
  rgb <- grDevices::col2rgb(paste0("#", fill))
  hsv <- grDevices::rgb2hsv(rgb)
  h <- hsv["h", 1] * 360
  s <- hsv["s", 1]
  if (s < 0.08) return(NA_character_)      # white / near-white / grey
  if (h >= 170 && h < 260) return("CORE")      # blue
  if (h >= 70  && h < 170) return("ENHANCED")  # green
  if (h < 70 || h >= 330) return("SCALABLE")   # orange / red
  NA_character_
}

ACTIVITY_CELL_RE <- "^A?[123]\\.[1-7]"

#' Activity -> budget tier, recovered from Section 6.1 cell shading.
#'
#' @return tibble(activity, fill, tier), also written to data/tiers.csv
extract_tiers <- function(docx, out = "data/tiers.csv", verbose = TRUE) {
  x  <- xml2::read_xml(unz(docx, "word/document.xml"))
  ns <- xml2::xml_ns(x)

  tbls <- xml2::xml_find_all(x, "//w:tbl", ns)

  # The Section 6.1 table is the one whose header cell states the colour key.
  is_key_table <- vapply(tbls, function(tb) {
    first <- xml2::xml_find_first(tb, ".//w:tc", ns)
    txt <- if (inherits(first, "xml_missing")) "" else xml2::xml_text(first)
    grepl("blue", txt, ignore.case = TRUE) &&
      grepl("CORE", txt) && grepl("SCALABLE", txt)
  }, logical(1))

  if (!any(is_key_table)) {
    stop("extract_tiers(): could not find the Section 6.1 activity table ",
         "(no table header states the blue/green/orange colour key).")
  }
  tb <- tbls[[which(is_key_table)[[1]]]]

  cells <- xml2::xml_find_all(tb, ".//w:tc", ns)
  txt   <- trimws(xml2::xml_text(cells))
  fill  <- xml2::xml_attr(xml2::xml_find_first(cells, ".//w:shd", ns), "fill")

  keep <- grepl(ACTIVITY_CELL_RE, txt)
  activity <- stringr::str_extract(txt[keep], "^A?[123]\\.[1-7]")
  activity <- ifelse(startsWith(activity, "A"), activity, paste0("A", activity))

  tiers <- tibble::tibble(activity = activity, fill = tolower(fill[keep])) |>
    dplyr::mutate(tier = vapply(.data$fill, classify_hue, character(1))) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$activity)

  if (verbose) {
    cat("\n-- extract_tiers(): Section 6.1 cell shading ------------------\n")
    cat("distinct non-white fills found and the tier each was mapped to:\n")
    map <- tiers |>
      dplyr::distinct(.data$fill, .data$tier) |>
      dplyr::arrange(.data$tier)
    for (i in seq_len(nrow(map))) {
      cat(sprintf("  #%s -> %s\n", map$fill[i], map$tier[i]))
    }
    cat("\nactivity -> tier:\n")
    print(as.data.frame(tiers), row.names = FALSE, right = FALSE)
  }

  if (anyNA(tiers$tier)) {
    stop("extract_tiers(): unmapped fill(s): ",
         paste(unique(tiers$fill[is.na(tiers$tier)]), collapse = ", "))
  }

  if (!is.null(out)) {
    dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(tiers, out, na = "")
  }
  tiers
}

# The SCALABLE budget table in Section 11.1 names the activities it pays for.
# Section 6.1's shading and that budget line are two independent statements
# about the same thing, so we check them against each other.
BUDGET_SCALABLE_RE <- "([123]\\.[1-7])"

#' Which activities the Section 11.1 SCALABLE budget line pays for.
budget_scalable <- function(docx) {
  blocks <- docx_blocks(docx)
  row <- blocks$text[grepl("scaling contributions", blocks$text, ignore.case = TRUE) &
                       grepl("\\+", blocks$text)]
  if (!length(row)) return(character(0))
  ids <- unlist(stringr::str_extract_all(row[[1]], BUDGET_SCALABLE_RE))
  sort(unique(paste0("A", ids)))
}

#' Cross-check the shading-derived tiers against the budget section.
#'
#' Returns a list describing the comparison. Callers decide what to do with a
#' disagreement; this function never silently picks a winner.
check_tiers_against_budget <- function(tiers, docx, verbose = TRUE) {
  from_shading <- sort(tiers$activity[tiers$tier == "SCALABLE"])
  from_budget  <- budget_scalable(docx)

  only_budget  <- setdiff(from_budget, from_shading)
  only_shading <- setdiff(from_shading, from_budget)
  agree <- length(only_budget) == 0 && length(only_shading) == 0

  if (verbose) {
    cat("\n-- sanity check against the Section 11.1 budget ---------------\n")
    cat("SCALABLE per 6.1 shading : ", paste(from_shading, collapse = ", "), "\n")
    cat("SCALABLE per 11.1 budget : ", paste(from_budget, collapse = ", "), "\n")
    if (agree) {
      cat("the two sources agree.\n")
    } else {
      cat("\n*** THE TWO SOURCES DISAGREE ***\n")
      if (length(only_budget)) {
        cat("  budget-only (shaded as something else in 6.1): ",
            paste(only_budget, collapse = ", "), "\n")
      }
      if (length(only_shading)) {
        cat("  shading-only (not in the budget line):        ",
            paste(only_shading, collapse = ", "), "\n")
      }
      cat("  data/tiers.csv reports the Section 6.1 shading, which is the\n",
          "  source that carries its own colour key. The disagreement is\n",
          "  recorded in MISSING-INPUTS.md and needs a human decision.\n", sep = "")
    }
  }

  list(
    agree        = agree,
    from_shading = from_shading,
    from_budget  = from_budget,
    only_budget  = only_budget,
    only_shading = only_shading
  )
}
