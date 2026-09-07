# Rule 7 -- Type size.
#
# In each profile the smallest font-size actually rendered is at or above that
# profile's minimum. The minimum is a property of the medium -- how far the
# reader sits from it -- so it lives in the profile and a spec cannot talk its
# way past it.

test_that("the three profiles carry the minimums the brief sets", {
  expect_equal(get_profile("slide")$min_font, 32)
  expect_equal(get_profile("a4")$min_font, 11)
  expect_equal(get_profile("poster")$min_font, 24)
})

test_that("no rendered text falls below the profile minimum", {
  for_each_render(function(svg, spec, profile, nm) {
    fs <- font_sizes(svg)
    expect_gt(length(fs), 0)
    expect_gte(
      min(fs), profile$min_font,
      label = sprintf("%s / %s: smallest rendered type is %.1f, minimum is %g",
                      nm, profile$name, min(fs), profile$min_font)
    )
  })
})

test_that("every role in every profile clears that profile's minimum", {
  for (pn in PROFILE_NAMES) {
    for (orient in c("landscape", "portrait")) {
      p <- get_profile(pn, orient)
      for (role in ROLES) {
        expect_gte(
          font_for(p, role), p$min_font,
          label = sprintf("profile %s (%s): role '%s' is %g, minimum is %g",
                          pn, orient, role, font_for(p, role), p$min_font)
        )
      }
    }
  }
})

test_that("font size comes from the profile, never from the spec", {
  spec <- SPECS[[1]]
  # a spec that tries to set its own size is ignored: only `role` is consulted
  spec$nodes[[1]]$labels[[1]]$size <- 4
  svg <- render_figure(spec, get_profile("a4", spec$orientation))
  expect_gte(min(font_sizes(svg)), get_profile("a4")$min_font)
})

test_that("stroke weights hold up in the A4 viewBox", {
  a4 <- get_profile("a4")
  expect_gte(min(a4$stroke), 1.5)
  for (pn in PROFILE_NAMES) {
    p <- get_profile(pn)
    expect_gte(min(p$stroke), 1.5 * p$scale - 1e-9)
  }
  for_each_render(function(svg, spec, profile, nm) {
    doc <- svg_doc(svg)
    # only strokes drawn in the figure's own coordinate space; a marker's
    # internal stroke lives in the marker viewBox and scales with the line it
    # is attached to, so the viewBox floor does not apply to it
    widths <- as.numeric(xml2::xml_attr(
      xml2::xml_find_all(doc, "//svg:g[@font-family]//svg:*[@stroke-width]",
                         SVG_NS),
      "stroke-width"
    ))
    widths <- widths[!is.na(widths)]
    expect_gte(
      min(widths), 1.5 * profile$scale - 0.01,
      label = sprintf("%s / %s: thinnest stroke is %.2f",
                      nm, profile$name, min(widths))
    )
  })
})
