# Rule 1 -- No invented facts.
#
# Every numeric token and every M/D/O/R/A ID in any label exists in
# data/facts.csv, or is the literal string [TBC]. This is the rule the whole
# project exists to enforce: a figure is a claim about the proposal, and a
# figure that quotes a number the proposal does not contain is a figure that
# will embarrass someone in a review meeting.

test_that("data/facts.csv was extracted and is non-trivial", {
  expect_gt(nrow(FACTS), 100)
  expect_setequal(
    intersect(unique(FACTS$kind),
              c("activity", "milestone", "deliverable", "objective", "risk")),
    c("activity", "milestone", "deliverable", "objective", "risk")
  )
})

known_numbers <- unique(normalise_number(FACTS$value))
known_ids <- unique(FACTS$value)

test_that("every numeric token in a label comes from the proposal", {
  for_each_spec(function(spec, nm) {
    lab <- all_labels(spec)
    for (i in seq_len(nrow(lab))) {
      toks <- numeric_tokens(without_tbc(lab$text[i]))
      unknown <- setdiff(normalise_number(toks), known_numbers)
      expect_identical(
        unknown, character(0),
        info = sprintf(
          "%s / %s (%s): %s not in data/facts.csv -- label: %s",
          nm, lab$owner[i], lab$tier[i],
          paste(unknown, collapse = ", "), lab$text[i]
        )
      )
    }
  })
})

test_that("every M/D/O/R/A id in a label comes from the proposal", {
  for_each_spec(function(spec, nm) {
    lab <- all_labels(spec)
    for (i in seq_len(nrow(lab))) {
      ids <- id_tokens(without_tbc(lab$text[i]))
      unknown <- setdiff(ids, known_ids)
      expect_identical(
        unknown, character(0),
        info = sprintf(
          "%s / %s (%s): %s not in data/facts.csv -- label: %s",
          nm, lab$owner[i], lab$tier[i],
          paste(unknown, collapse = ", "), lab$text[i]
        )
      )
    }
  })
})

test_that("the rule actually bites -- an invented figure is rejected", {
  spec <- SPECS[[1]]
  spec$nodes[[1]]$labels[[1]]$text <- "Sensors in 41 countries"
  lab <- all_labels(spec)
  toks <- normalise_number(numeric_tokens(lab$text[1]))
  expect_false(all(toks %in% known_numbers))
})

test_that("[TBC] is accepted where the proposal has no fact to quote", {
  expect_identical(numeric_tokens(without_tbc("AMI [TBC] traps")), character(0))
  # ... and MISSING-INPUTS.md records why each one is there
  expect_true(file.exists(file.path(ROOT, "MISSING-INPUTS.md")))
})
