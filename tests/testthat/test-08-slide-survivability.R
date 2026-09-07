# Rule 8 -- Slide survivability.
#
# Every node carries an L1 label, no L1 label runs past eight words, and no L1
# label is a sentence with more than one clause. A slide label is read in about
# a second from the back of a room; prose that has wandered into a box does not
# survive that.

MAX_L1_WORDS <- 8

test_that("every node carries an L1 label", {
  for_each_spec(function(spec, nm) {
    for (n in spec$nodes) {
      tiers <- vapply(n$labels, function(l) l$tier, character(1))
      expect_true("L1" %in% tiers,
                  info = sprintf("%s: node %s has no L1 label", nm, n$id))
    }
  })
})

test_that("no L1 label exceeds eight words", {
  for_each_spec(function(spec, nm) {
    lab <- all_labels(spec)
    l1 <- lab[lab$tier == "L1", ]
    for (i in seq_len(nrow(l1))) {
      # legend entries are a key, not a caption in a box; they are read at rest
      if (l1$kind[i] == "legend") next
      expect_lte(
        n_words(l1$text[i]), MAX_L1_WORDS,
        label = sprintf("%s / %s: L1 label is %d words -- '%s'",
                        nm, l1$owner[i], n_words(l1$text[i]), l1$text[i])
      )
    }
  })
})

test_that("no L1 label is a multi-clause sentence", {
  for_each_spec(function(spec, nm) {
    lab <- all_labels(spec)
    l1 <- lab[lab$tier == "L1" & lab$kind != "legend", ]
    for (i in seq_len(nrow(l1))) {
      expect_false(
        multi_clause(l1$text[i]),
        label = sprintf("%s / %s: L1 label reads as prose -- '%s'",
                        nm, l1$owner[i], l1$text[i])
      )
    }
  })
})

test_that("the slide profile renders L1 and nothing else", {
  for (nm in names(SPECS)) {
    spec <- SPECS[[nm]]
    svg <- RENDERED[[nm]][["slide"]]
    rendered <- paste(svg_texts(svg), collapse = " | ")

    lab <- all_labels(spec)
    for (i in seq_len(nrow(lab))) {
      if (lab$tier[i] == "L1") next
      # an L2/L3 label must be absent from the slide entirely, not merely hidden
      probe <- strsplit(stringr::str_squish(lab$text[i]), " ")[[1]]
      probe <- probe[nchar(probe) > 6][1]
      if (is.na(probe)) next
      # skip probes that also occur in an L1 label of the same figure
      if (any(grepl(probe, lab$text[lab$tier == "L1"], fixed = TRUE))) next
      expect_false(
        grepl(probe, rendered, fixed = TRUE),
        label = sprintf("%s: %s text '%s' leaked onto the slide",
                        nm, lab$tier[i], lab$text[i])
      )
    }
  }
})

test_that("every node's L1 label fits inside its box on every profile", {
  for (nm in names(SPECS)) {
    spec <- SPECS[[nm]]
    for (pn in PROFILE_NAMES) {
      p <- get_profile(pn, spec$orientation)
      fitted <- .fit_rows(spec, p)
      for (n in fitted) {
        l1 <- Filter(function(l) l$tier == "L1", n$labels)
        if (!length(l1)) next
        size <- font_for(p, l1[[1]]$role)
        bw <- n$w * p$width
        lines <- wrap_text(l1[[1]]$text, size, bw - 2 * .inner_pad(bw))
        expect_lte(
          length(lines) * size * LINE_HEIGHT, n$h * p$height,
          label = sprintf("%s / %s: node %s label overflows its box",
                          nm, pn, n$id)
        )
      }
    }
  }
})
