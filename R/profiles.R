# ---------------------------------------------------------------------------
# R/profiles.R -- the three output profiles.
#
# There are exactly three. Geometry in a spec is expressed in a unit square and
# multiplied by the canvas, so one spec drives all three media. Font size never
# comes from the spec: it comes from a role -> size lookup here, so a label
# cannot make itself unreadable on a slide by being written for A4.
# ---------------------------------------------------------------------------

PROFILE_NAMES <- c("slide", "a4", "poster")

#' The reserved hues. These three mean CORE / ENHANCED / SCALABLE and nothing
#' else, in any figure. Everything else uses the neutral ink ramp below, which
#' is distinguished additionally by position, stroke style and label so the
#' figures survive greyscale printing and colour blindness.
TIER_HUES <- c(
  CORE     = "#1F4E79",
  ENHANCED = "#2E7D32",
  SCALABLE = "#C55A11"
)

INK <- c(
  ink0 = "#111111",  # primary text, box borders
  ink1 = "#555555",  # secondary text, connectors
  ink2 = "#999999",  # tertiary rules, brackets
  ink3 = "#E6E6E6"   # fills, bands
)

PAPER <- "#FFFFFF"

ROLES <- c("title", "block", "gate", "caption", "note", "legend")

TIER_ORDER <- c("L1", "L2", "L3")

.profile_defs <- list(
  slide = list(
    canvas = c(1920, 1080),
    tiers  = "L1",
    min_font = 32,
    font = c(title = 46, block = 34, gate = 34, caption = 34, note = 34, legend = 32)
  ),
  a4 = list(
    canvas = c(1000, 700),
    tiers  = c("L1", "L2"),
    min_font = 11,
    font = c(title = 22, block = 15, gate = 12, caption = 11, note = 11, legend = 11)
  ),
  poster = list(
    # landscape; flipped for a portrait spec (see get_profile())
    canvas = c(3370, 2384),
    tiers  = c("L1", "L2", "L3"),
    min_font = 24,
    # the extra space a poster buys goes to annotation, not to larger boxes:
    # `block` grows less than the canvas does, `caption`/`note` carry the gain.
    font = c(title = 76, block = 46, gate = 38, caption = 30, note = 28, legend = 26)
  )
)

#' Build a profile.
#'
#' @param name one of `PROFILE_NAMES`
#' @param orientation "landscape" or "portrait"; only the poster changes shape
#' @return a list with canvas, tiers, fonts, strokes and geometry helpers
get_profile <- function(name, orientation = c("landscape", "portrait")) {
  orientation <- match.arg(orientation)
  if (!name %in% PROFILE_NAMES) {
    stop("unknown profile '", name, "'; expected one of ",
         paste(PROFILE_NAMES, collapse = ", "))
  }
  def <- .profile_defs[[name]]

  canvas <- def$canvas
  if (name == "poster" && orientation == "portrait") canvas <- rev(canvas)

  width  <- canvas[[1]]
  height <- canvas[[2]]

  # Strokes are quoted relative to the A4 viewBox and scaled with the canvas,
  # so the thinnest rule is 1.5px on A4 and proportionally heavier elsewhere.
  scale <- width / .profile_defs$a4$canvas[[1]]
  stroke <- c(hair = 1.5, rule = 2.0, edge = 2.5, heavy = 4.0) * scale

  structure(
    list(
      name        = name,
      orientation = orientation,
      width       = width,
      height      = height,
      tiers       = def$tiers,
      min_font    = def$min_font,
      font        = def$font,
      stroke      = stroke,
      scale       = scale
    ),
    class = "figure_profile"
  )
}

#' All three profiles for a spec's orientation.
all_profiles <- function(orientation = "landscape") {
  stats::setNames(
    lapply(PROFILE_NAMES, get_profile, orientation = orientation),
    PROFILE_NAMES
  )
}

#' Font size for a role, in the profile's viewBox units.
font_for <- function(profile, role) {
  if (!role %in% names(profile$font)) {
    stop("unknown role '", role, "'; expected one of ", paste(ROLES, collapse = ", "))
  }
  unname(profile$font[[role]])
}

#' Is a label at `tier` shown in this profile?
shows_tier <- function(profile, tier) tier %in% profile$tiers

print.figure_profile <- function(x, ...) {
  cat(sprintf("<profile %s  %dx%d  tiers: %s  min font: %g>\n",
              x$name, x$width, x$height,
              paste(x$tiers, collapse = "+"), x$min_font))
  invisible(x)
}
