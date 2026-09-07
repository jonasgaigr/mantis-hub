# Rule 2 -- Reserved hues.
#
# #1F4E79, #2E7D32 and #C55A11 mean CORE, ENHANCED and SCALABLE and nothing
# else. They appear in a rendered figure only where the spec marks an element
# with a budget tier. Packages, connectors, emphasis and decoration all use the
# neutral ink ramp, so a reader who has learned the three hues once can trust
# them everywhere.

test_that("the reserved hues are the three the brief names", {
  expect_identical(
    unname(TIER_HUES[c("CORE", "ENHANCED", "SCALABLE")]),
    c("#1F4E79", "#2E7D32", "#C55A11")
  )
})

test_that("a reserved hue appears only where a budget tier is declared", {
  for_each_render(function(svg, spec, profile, nm) {
    used <- hex_codes(svg)
    present <- intersect(toupper(unname(TIER_HUES)), used$hex)
    declared <- budget_tiers_used(spec)
    allowed <- toupper(unname(TIER_HUES[declared]))

    expect_identical(
      setdiff(present, allowed), character(0),
      info = sprintf(
        "%s / %s: reserved hue %s rendered, but the spec declares no such tier",
        nm, profile$name, paste(setdiff(present, allowed), collapse = ", ")
      )
    )
  })
})

test_that("everything else is drawn from the neutral ink ramp", {
  palette <- toupper(c(unname(INK), PAPER, unname(TIER_HUES)))
  for_each_render(function(svg, spec, profile, nm) {
    used <- unique(hex_codes(svg)$hex)
    expect_identical(
      setdiff(used, palette), character(0),
      info = sprintf("%s / %s: off-palette colour %s", nm, profile$name,
                     paste(setdiff(used, palette), collapse = ", "))
    )
  })
})

test_that("the check reads paint, not the file as a string", {
  # a hue named in the caption must not count as the hue being used
  spec <- SPECS[[1]]
  spec$caption <- paste(spec$caption, "The CORE tier is #1F4E79.")
  svg <- render_figure(spec, get_profile("a4", spec$orientation))
  expect_true(grepl("#1F4E79", svg, fixed = TRUE))          # it is in the text
  expect_false("#1F4E79" %in% hex_codes(svg)$hex)           # but nothing is painted with it
})
