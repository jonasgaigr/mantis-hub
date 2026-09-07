# Rule 4 -- Acronyms.
#
# Every all-caps token in a label is either expanded within the same figure at
# the same-or-lower tier, or carries [TBC], or is on the allow-list. "Within
# the same figure" is the strict part and the point of the rule: a reader
# looking at a slide does not have the proposal open, and an expansion that
# only exists at L3 is no use to someone reading the L1 slide.

test_that("the allow-list is the one the brief names", {
  expect_setequal(ACRONYM_ALLOW, c("MANTIS", "EU", "AI", "GBIF", "TBC"))
})

test_that("every acronym is expanded, flagged or allow-listed", {
  for_each_spec(function(spec, nm) {
    exps <- figure_expansions(spec)
    lab <- all_labels(spec)

    for (i in seq_len(nrow(lab))) {
      txt <- lab$text[i]
      tier_i <- match(lab$tier[i], TIER_ORDER)
      for (acr in acronym_tokens(txt)) {
        if (acr %in% ACRONYM_ALLOW) next
        if (tbc_flagged(txt, acr)) next

        expanded <- acr %in% names(exps)
        expect_true(
          expanded,
          info = sprintf(
            "%s / %s (%s): '%s' is never expanded in this figure -- label: %s",
            nm, lab$owner[i], lab$tier[i], acr, txt
          )
        )
        if (expanded) {
          expect_lte(
            exps[[acr]], tier_i,
            label = sprintf(
              "%s: '%s' used at %s but only expanded at %s",
              nm, acr, lab$tier[i], TIER_ORDER[exps[[acr]]]
            )
          )
        }
      }
    }
  })
})

test_that("an expansion is only accepted when the initials actually match", {
  expect_true(.initials_match("Camera Trap Data Package", "DP") ||
                .initials_match("Data Package", "DP"))
  expect_true(.initials_match("Invasive Alien Species", "IAS"))
  expect_true(.initials_match("Nature Restoration Regulation", "NRR"))
  # "Nature Conservation Agency (CZ)" must not be read as expanding CZ
  expect_false(.initials_match("Nature Conservation Agency", "CZ"))
})

test_that("the rule bites -- an unexpanded acronym is caught", {
  spec <- SPECS[[1]]
  spec$nodes[[1]]$labels[[1]]$text <- "Sensors feed the EBOCC hub"
  exps <- figure_expansions(spec)
  acrs <- setdiff(acronym_tokens("Sensors feed the EBOCC hub"), ACRONYM_ALLOW)
  expect_identical(acrs, "EBOCC")
  expect_false("EBOCC" %in% names(exps))
})

test_that("AMI is flagged rather than invented", {
  # the proposal never spells AMI out; data/acronyms.csv should agree
  acr <- readr::read_csv(file.path(ROOT, "data", "acronyms.csv"),
                         show_col_types = FALSE,
                         col_types = readr::cols(.default = readr::col_character()))
  ami <- acr[acr$term == "AMI", ]
  expect_gt(nrow(ami), 0)
  expect_true(all(is.na(ami$expansion)))

  used <- unlist(lapply(SPECS, function(s) all_labels(s)$text))
  with_ami <- used[grepl("(?<![A-Za-z0-9])AMI(?![A-Za-z0-9])", used, perl = TRUE)]
  for (txt in with_ami) {
    expect_true(tbc_flagged(txt, "AMI"),
                info = sprintf("AMI used without [TBC]: %s", txt))
  }
})
