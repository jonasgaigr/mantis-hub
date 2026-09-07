# The extraction layer, and the one place the proposal contradicts itself.
#
# These are not one of the nine rules, but the nine rules are only as good as
# data/facts.csv: rule 1 measures labels against it, so it has to be right.

test_that("the three data tables exist and are shaped as promised", {
  facts <- FACTS
  expect_named(facts, c("value", "kind", "context", "section"))

  acr <- readr::read_csv(file.path(ROOT, "data", "acronyms.csv"),
                         show_col_types = FALSE,
                         col_types = readr::cols(.default = readr::col_character()))
  expect_named(acr, c("term", "expansion", "section"))

  tiers <- readr::read_csv(file.path(ROOT, "data", "tiers.csv"),
                           show_col_types = FALSE,
                           col_types = readr::cols(.default = readr::col_character()))
  expect_named(tiers, c("activity", "fill", "tier"))
})

test_that("all 19 activities are tiered, from the three shaded fills", {
  tiers <- readr::read_csv(file.path(ROOT, "data", "tiers.csv"),
                           show_col_types = FALSE,
                           col_types = readr::cols(.default = readr::col_character()))
  expect_equal(nrow(tiers), 19)
  expect_setequal(
    tiers$activity,
    c(paste0("A1.", 1:7), paste0("A2.", 1:5), paste0("A3.", 1:7))
  )
  expect_setequal(unique(tiers$tier), c("CORE", "ENHANCED", "SCALABLE"))
  expect_equal(length(unique(tiers$fill)), 3)
  expect_false(anyNA(tiers$tier))
})

test_that("hue classification maps the shading the way Section 6.1 says", {
  expect_identical(classify_hue("cfe2f3"), "CORE")      # light blue
  expect_identical(classify_hue("d9ead3"), "ENHANCED")  # light green
  expect_identical(classify_hue("fce5cd"), "SCALABLE")  # light orange
  expect_true(is.na(classify_hue("ffffff")))
  expect_true(is.na(classify_hue("f7fafc")))
})

test_that("the proposal's own two statements about SCALABLE still disagree", {
  # This is a documented contradiction in the source, not a bug in the
  # pipeline: Section 6.1's shading and the Section 11.1 budget line differ on
  # A1.5 and A1.6. The test pins the disagreement so that if someone fixes the
  # .docx, this fails and MISSING-INPUTS.md gets updated rather than going
  # quietly stale.
  skip_if_not(file.exists(file.path(ROOT, "source")), "no source/ directory")
  docx <- list.files(file.path(ROOT, "source"), pattern = "\\.docx$",
                     full.names = TRUE)
  skip_if(length(docx) == 0, "proposal .docx not present")

  source(file.path(ROOT, "R", "extract.R"))
  tiers <- readr::read_csv(file.path(ROOT, "data", "tiers.csv"),
                           show_col_types = FALSE,
                           col_types = readr::cols(.default = readr::col_character()))
  chk <- check_tiers_against_budget(tiers, docx[[1]], verbose = FALSE)

  expect_setequal(chk$from_shading, c("A1.3", "A1.4", "A1.7", "A2.4"))
  expect_setequal(chk$from_budget,
                  c("A1.3", "A1.4", "A1.5", "A1.6", "A1.7", "A2.4"))
  expect_setequal(chk$only_budget, c("A1.5", "A1.6"))
  expect_false(chk$agree)

  missing <- paste(readLines(file.path(ROOT, "MISSING-INPUTS.md"),
                             warn = FALSE), collapse = "\n")
  expect_true(grepl("A1.5", missing, fixed = TRUE))
  expect_true(grepl("A1.6", missing, fixed = TRUE))
})

test_that("facts carry the sentence each was found in", {
  ids <- FACTS[FACTS$kind %in% c("milestone", "deliverable"), ]
  expect_gt(nrow(ids), 20)
  expect_true(all(nzchar(ids$context)))
  expect_true(all(nzchar(ids$section)))
})
