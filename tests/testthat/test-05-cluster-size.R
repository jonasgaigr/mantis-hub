# Rule 5 -- Cluster size.
#
# No visual cluster exceeds seven elements. A cluster is a set the reader has
# to hold in mind at once: the nodes a bracket or band groups under one label,
# the legend, the stack of labels on one element. A left-to-right chain is not
# a cluster -- it is read in sequence -- which is why an eight-box spine passes
# and an eight-entry legend does not.

test_that("MAX_CLUSTER is seven", {
  expect_identical(MAX_CLUSTER, 7)
})

test_that("no cluster exceeds seven elements", {
  for_each_spec(function(spec, nm) {
    cl <- clusters(spec)
    expect_gt(length(cl), 0)
    for (k in names(cl)) {
      expect_lte(
        length(cl[[k]]), MAX_CLUSTER,
        label = sprintf("%s: cluster '%s' holds %d elements",
                        nm, k, length(cl[[k]]))
      )
    }
  })
})

test_that("read_spec() refuses a spec with an oversized cluster", {
  path <- file.path(tempdir(), "too-big.yml")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    "id: bad",
    "message: A figure with too many legend entries.",
    "caption: Caption.",
    "alt: Alt text.",
    "nodes:",
    "  - id: N1",
    "    x: 0.1",
    "    'y': 0.1",
    "    w: 0.2",
    "    h: 0.2",
    "    labels:",
    "      - {tier: L1, role: block, text: One}",
    "legend:",
    paste0("  - {tier: L1, text: entry", 1:8, "}")
  ), path)
  expect_error(read_spec(path), "cluster", ignore.case = TRUE)
})

test_that("a long chain is not counted as a cluster", {
  # the fig01 spine is longer than seven elements end to end, and that is fine
  spec <- SPECS[["fig01-connective-tissue.yml"]]
  expect_gt(length(spec$nodes), MAX_CLUSTER)
  expect_lte(max(lengths(clusters(spec))), MAX_CLUSTER)
})
