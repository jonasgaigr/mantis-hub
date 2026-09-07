# Rule 6 -- Print safety.
#
# The rendered SVG contains no raster, no gradient, no filter, no script and no
# external reference. A figure that depends on anything outside its own file
# will eventually be opened somewhere that cannot fetch it, and a gradient or a
# drop shadow is the first thing to turn into mud on a press.

FORBIDDEN_ELEMENTS <- c(
  "image", "linearGradient", "radialGradient", "filter", "script",
  "foreignObject", "use", "pattern"
)

test_that("no forbidden element appears in any rendered figure", {
  for_each_render(function(svg, spec, profile, nm) {
    doc <- svg_doc(svg)
    for (el in FORBIDDEN_ELEMENTS) {
      found <- xml2::xml_find_all(doc, paste0("//svg:", el), SVG_NS)
      expect_length(found, 0)
      if (length(found)) {
        fail(sprintf("%s / %s: <%s> present", nm, profile$name, el))
      }
    }
  })
})

test_that("nothing references anything outside the file", {
  for_each_render(function(svg, spec, profile, nm) {
    doc <- svg_doc(svg)
    hrefs <- c(
      xml2::xml_attr(xml2::xml_find_all(doc, "//svg:*[@href]", SVG_NS), "href"),
      xml2::xml_attr(xml2::xml_find_all(doc, "//*[@xlink:href]",
                                        c(SVG_NS, xlink = "http://www.w3.org/1999/xlink")),
                     "href")
    )
    hrefs <- hrefs[!is.na(hrefs)]
    expect_identical(
      hrefs, character(0),
      info = sprintf("%s / %s: external reference %s", nm, profile$name,
                     paste(hrefs, collapse = ", "))
    )

    # marker references are the one url() we allow, and they point inside
    urls <- xml2::xml_attr(
      xml2::xml_find_all(doc, "//svg:*[@marker-end]", SVG_NS), "marker-end"
    )
    ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//svg:marker", SVG_NS), "id")
    for (u in unique(urls)) {
      target <- sub("^url\\(#(.*)\\)$", "\\1", u)
      expect_true(target %in% ids,
                  info = sprintf("%s / %s: dangling marker %s", nm, profile$name, u))
    }
  })
})

test_that("text is real text, not outlined paths", {
  for_each_render(function(svg, spec, profile, nm) {
    doc <- svg_doc(svg)
    texts <- xml2::xml_find_all(doc, "//svg:text", SVG_NS)
    expect_gt(length(texts), 0)

    fam <- xml2::xml_attr(
      xml2::xml_find_first(doc, "//svg:g[@font-family]", SVG_NS), "font-family"
    )
    expect_identical(fam, FONT_STACK)

    # every L1 label the profile shows must be findable as characters
    rendered <- paste(svg_texts(svg), collapse = " ")
    for (n in spec$nodes) {
      l1 <- Filter(function(l) l$tier == "L1", n$labels)
      if (!length(l1)) next
      first_word <- strsplit(stringr::str_squish(l1[[1]]$text), " ")[[1]][1]
      expect_true(
        grepl(first_word, rendered, fixed = TRUE),
        info = sprintf("%s / %s: node %s label not present as text",
                       nm, profile$name, n$id)
      )
    }
  })
})

test_that("the document declares a title and alt text for a screen reader", {
  for_each_render(function(svg, spec, profile, nm) {
    doc <- svg_doc(svg)
    expect_identical(xml2::xml_attr(doc, "role"), "img")
    expect_identical(xml2::xml_attr(doc, "aria-label"), spec$alt)
    expect_identical(
      xml2::xml_text(xml2::xml_find_first(doc, "/svg:svg/svg:title", SVG_NS)),
      spec$message
    )
  })
})
