# Rule 3 -- Arrows are labelled.
#
# Every edge states a kind, and every kind used in a figure is accounted for in
# that figure's legend. An arrow whose meaning is not stated somewhere on the
# page is decoration, and decoration in a proposal figure is a reader silently
# guessing at a relationship.

test_that("every edge states a kind", {
  for_each_spec(function(spec, nm) {
    for (e in spec$edges) {
      expect_true(
        !is.null(e$kind) && nzchar(e$kind),
        info = sprintf("%s: edge %s -> %s has no kind", nm, e$from, e$to)
      )
      expect_true(
        e$kind %in% c("flow", "feeds"),
        info = sprintf("%s: edge %s -> %s has unknown kind '%s'",
                       nm, e$from, e$to, e$kind)
      )
    }
  })
})

test_that("every kind used appears in the figure's legend", {
  for_each_spec(function(spec, nm) {
    missing <- setdiff(edge_kinds(spec), legend_kinds(spec))
    expect_identical(
      missing, character(0),
      info = sprintf("%s: edge kind(s) %s used but not in the legend",
                     nm, paste(missing, collapse = ", "))
    )
  })
})

test_that("each kind renders with its own marker, and every drawn arrow has one", {
  for_each_render(function(svg, spec, profile, nm) {
    doc <- svg_doc(svg)
    marks <- xml2::xml_attr(
      xml2::xml_find_all(doc, "//svg:marker", SVG_NS), "id"
    )
    for (k in edge_kinds(spec)) {
      expect_true(
        paste0("arrow-", k) %in% marks,
        info = sprintf("%s / %s: no marker defined for kind '%s'",
                       nm, profile$name, k)
      )
    }
    # the two markers must differ, or shape is not carrying the meaning
    defs <- xml2::xml_find_all(doc, "//svg:marker//svg:path", SVG_NS)
    ds <- xml2::xml_attr(defs, "d")
    expect_equal(length(unique(ds)), length(ds),
                 info = sprintf("%s / %s: two markers share a shape",
                                nm, profile$name))
  })
})

test_that("no connector is drawn without a marker", {
  # every polyline that is an edge carries marker-end; brackets and gate bars
  # are not edges and are identified by having no marker at all, so we check
  # the count of marked polylines against the edge count instead.
  for_each_render(function(svg, spec, profile, nm) {
    doc <- svg_doc(svg)
    marked <- xml2::xml_find_all(doc, "//svg:polyline[@marker-end]", SVG_NS)
    n_legend_keys <- sum(vapply(
      Filter(function(l) shows_tier(profile, l$tier), spec$legend),
      function(l) !is.null(l$kind), logical(1)
    ))
    expect_equal(
      length(marked), length(spec$edges) + n_legend_keys,
      info = sprintf("%s / %s: %d marked connectors for %d edges + %d legend keys",
                     nm, profile$name, length(marked),
                     length(spec$edges), n_legend_keys)
    )
  })
})
