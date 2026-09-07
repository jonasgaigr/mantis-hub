# ---------------------------------------------------------------------------
# R/document.R -- the glue between the built SVGs and the Quarto documents.
#
# Captions and alt text are read out of the spec at render time rather than
# retyped into the .qmd, so a caption cannot drift away from the figure it
# describes: there is only one copy of it, in the spec.
# ---------------------------------------------------------------------------

#' Every spec, in file order.
document_specs <- function(dir = "figures") {
  paths <- sort(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE))
  lapply(paths, read_spec)
}

#' Path to a built variant of a figure.
figure_path <- function(spec, profile, outdir = "out") {
  path <- file.path(outdir, sprintf("%s-%s.svg", spec$id, profile))
  if (!file.exists(path)) {
    stop("missing ", path, " -- run targets::tar_make() first")
  }
  path
}

#' Convert an SVG to PDF, keeping the text live.
#'
#' LaTeX cannot place an SVG directly, and the usual workarounds either
#' rasterise it or outline the glyphs, which is exactly what this pipeline
#' exists to avoid. Two converters are accepted, both of which write a PDF
#' whose <text> is still text:
#'
#'   1. rsvg-convert (preferred). One job, does it well, no GUI stack.
#'   2. a headless Chromium print, as a fallback for machines where librsvg
#'      is awkward to obtain -- Windows in particular. The SVG markup is
#'      inlined into the page rather than referenced through an <img>, because
#'      Chromium rasterises images when printing but keeps inline SVG live.
#'
#' Set MANTIS_SVG2PDF to "rsvg" or "chromium" to force one of them.
svg_to_pdf <- function(svg_path) {
  pdf_path <- sub("\\.svg$", ".pdf", svg_path)
  if (file.exists(pdf_path) && file.mtime(pdf_path) >= file.mtime(svg_path)) {
    return(pdf_path)
  }

  want <- Sys.getenv("MANTIS_SVG2PDF", "")
  rsvg <- Sys.which("rsvg-convert")
  chromium <- if (identical(want, "rsvg")) "" else find_chromium()

  if (nzchar(rsvg) && !identical(want, "chromium")) {
    status <- system2(rsvg, c("-f", "pdf", "-o", shQuote(pdf_path),
                              shQuote(svg_path)))
    if (!identical(status, 0L) || !file.exists(pdf_path)) {
      stop("rsvg-convert failed on ", svg_path, call. = FALSE)
    }
    return(pdf_path)
  }

  if (nzchar(chromium)) {
    .chromium_svg_to_pdf(chromium, svg_path, pdf_path)
    return(pdf_path)
  }

  stop("no SVG-to-PDF converter found. PDF output needs either rsvg-convert ",
       "(macOS: brew install librsvg; Debian/Ubuntu: apt install librsvg2-bin) ",
       "or a Chromium/Chrome/Edge binary on PATH. HTML and revealjs need ",
       "neither -- they embed the SVG directly.", call. = FALSE)
}

#' Locate a Chromium-family browser to print with.
find_chromium <- function() {
  for (n in c("chromium", "chromium-browser", "google-chrome", "chrome",
              "msedge")) {
    p <- Sys.which(n)
    if (nzchar(p)) return(unname(p))
  }
  candidates <- c(
    file.path(Sys.getenv("PROGRAMFILES"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "Microsoft/Edge/Application/msedge.exe"),
    file.path(Sys.getenv("PROGRAMFILES"), "Microsoft/Edge/Application/msedge.exe"),
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  )
  for (p in candidates) if (nzchar(p) && file.exists(p)) return(p)
  ""
}

.chromium_svg_to_pdf <- function(exe, svg_path, pdf_path) {
  svg <- paste(readLines(svg_path, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  dims <- as.numeric(c(
    sub('^.*\\bwidth="([0-9.]+)".*$', "\\1", strsplit(svg, "\n")[[1]][1]),
    sub('^.*\\bheight="([0-9.]+)".*$', "\\1", strsplit(svg, "\n")[[1]][1])
  ))
  if (anyNA(dims)) stop("could not read width/height from ", svg_path)

  html <- file.path(tempdir(), paste0(basename(svg_path), ".html"))
  on.exit(unlink(html), add = TRUE)
  writeLines(enc2utf8(c(
    '<!doctype html><meta charset="utf-8"><style>',
    sprintf("@page { size: %fpx %fpx; margin: 0 }", dims[1], dims[2]),
    "html,body{margin:0;padding:0}svg{display:block}",
    "</style>", svg
  )), html, useBytes = TRUE)

  status <- system2(exe, c(
    "--headless", "--disable-gpu", "--no-pdf-header-footer",
    sprintf("--print-to-pdf=%s", shQuote(normalizePath(pdf_path,
                                                      winslash = "/",
                                                      mustWork = FALSE))),
    shQuote(paste0("file:///", normalizePath(html, winslash = "/")))
  ), stdout = FALSE, stderr = FALSE)

  if (!file.exists(pdf_path)) {
    stop("headless Chromium failed to print ", svg_path,
         " (exit ", status, ")", call. = FALSE)
  }
  pdf_path
}

#' Markdown for one figure, with its caption and alt text taken from the spec.
#'
#' @param fmt "html" or "pdf"; PDF gets a converted copy, HTML the SVG itself
figure_block <- function(spec, profile = "a4", fmt = "html", outdir = "out") {
  path <- figure_path(spec, profile, outdir)
  if (identical(fmt, "pdf")) path <- svg_to_pdf(path)
  sprintf('![%s](%s){fig-alt="%s"}\n', spec$caption, path, spec$alt)
}

#' Is Quarto rendering to PDF right now?
output_is_pdf <- function() {
  fmt <- tryCatch(knitr::opts_knit$get("rmarkdown.pandoc.to"),
                  error = function(e) NULL)
  identical(fmt, "latex")
}
