# Loaded before every test file. Finds the project root whether the suite is
# run from the project directory (`testthat::test_dir("tests/testthat")`) or
# from tests/testthat itself, sources R/, and renders every spec once so the
# rules below all read the same SVG.

find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in 1:6) {
    if (dir.exists(file.path(d, "figures")) && dir.exists(file.path(d, "R"))) {
      return(d)
    }
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("could not locate the project root from ", start)
}

ROOT <- find_root()

for (f in c("profiles", "spec", "render", "checks", "extract")) {
  source(file.path(ROOT, "R", paste0(f, ".R")))
}

SPEC_PATHS <- sort(list.files(file.path(ROOT, "figures"),
                              pattern = "\\.ya?ml$", full.names = TRUE))
SPECS <- stats::setNames(lapply(SPEC_PATHS, read_spec), basename(SPEC_PATHS))

# render every spec x profile once; the rules are assertions about these
RENDERED <- lapply(SPECS, function(spec) {
  profs <- all_profiles(spec$orientation)
  lapply(profs, function(p) render_figure(spec, p))
})

FACTS <- readr::read_csv(file.path(ROOT, "data", "facts.csv"),
                         show_col_types = FALSE,
                         col_types = readr::cols(.default = readr::col_character()))

# Acronyms a figure may use without expanding: the project's own name, and
# terms so widely current in this field that expanding them would be noise.
ACRONYM_ALLOW <- c("MANTIS", "EU", "AI", "GBIF", "TBC")

#' Run `f(spec, name)` for every spec, so one failure names its figure.
for_each_spec <- function(f) {
  for (nm in names(SPECS)) f(SPECS[[nm]], nm)
}

#' Run `f(svg, spec, profile, name)` for every spec x profile.
for_each_render <- function(f) {
  for (nm in names(SPECS)) {
    spec <- SPECS[[nm]]
    profs <- all_profiles(spec$orientation)
    for (pn in names(profs)) f(RENDERED[[nm]][[pn]], spec, profs[[pn]], nm)
  }
}
