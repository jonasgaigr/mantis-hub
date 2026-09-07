# Rule 9 -- Determinism.
#
# The same spec and profile give byte-identical SVG. Without this, `targets`
# cannot tell a real change from noise, and a one-word edit produces a diff
# nobody can read.

test_that("rendering twice gives an identical string", {
  for (nm in names(SPECS)) {
    spec <- SPECS[[nm]]
    for (pn in PROFILE_NAMES) {
      p <- get_profile(pn, spec$orientation)
      expect_identical(
        render_figure(spec, p), render_figure(spec, p),
        label = sprintf("%s / %s renders differently on a second pass", nm, pn)
      )
    }
  }
})

test_that("re-reading the spec from disk changes nothing", {
  for (nm in names(SPECS)) {
    again <- read_spec(file.path(ROOT, "figures", nm))
    p <- get_profile("a4", again$orientation)
    expect_identical(render_figure(again, p), RENDERED[[nm]][["a4"]])
  }
})

test_that("the output carries no timestamp, no generated id and no random state", {
  for_each_render(function(svg, spec, profile, nm) {
    expect_false(grepl("\\b(19|20)[0-9]{2}-[0-9]{2}-[0-9]{2}T", svg))
    expect_false(grepl("(?i)generated|timestamp|created on", svg, perl = TRUE))

    doc <- svg_doc(svg)
    ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//svg:*[@id]", SVG_NS), "id")
    expect_setequal(ids, c("arrow-flow", "arrow-feeds"))
  })
})

test_that("writing to disk is deterministic too", {
  out <- file.path(tempdir(), "det")
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  spec <- SPECS[[1]]
  a <- render_all(spec, outdir = out)
  first <- lapply(a, function(p) readBin(p, "raw", file.size(p)))
  b <- render_all(spec, outdir = out)
  second <- lapply(b, function(p) readBin(p, "raw", file.size(p)))
  expect_identical(first, second)
  expect_identical(basename(a),
                   sprintf("%s-%s.svg", spec$id, PROFILE_NAMES))
})

test_that("a one-label edit changes all three variants and nothing else", {
  spec <- SPECS[["fig01-connective-tissue.yml"]]
  edited <- spec
  edited$nodes[[5]]$labels[[1]]$text <- "Open data published"

  for (pn in PROFILE_NAMES) {
    p <- get_profile(pn, spec$orientation)
    expect_false(identical(render_figure(edited, p), render_figure(spec, p)),
                 label = sprintf("%s did not change after a label edit", pn))
  }
  # the untouched nodes still render the same text
  before <- svg_texts(RENDERED[["fig01-connective-tissue.yml"]][["a4"]])
  after <- svg_texts(render_figure(edited, get_profile("a4")))
  expect_true("Policy" %in% intersect(before, after))
})
