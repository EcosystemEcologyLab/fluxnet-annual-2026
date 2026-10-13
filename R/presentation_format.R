#' Shared presentation-mode constants and helpers for talk slide decks
#'
#' Parallel to `R/nature_format.R`, but for 16:9 PowerPoint-ready PNG slides
#' rather than Nature-journal-spec figures -- never sourced BY
#' `nature_format.R`, and does not alter anything in it. Used only when a
#' figure script's `FLUXNET_PRESENTATION_MODE=1` block runs (see
#' `scripts/build_presentation_slides.R`); the default (unset) path for every
#' figure script is completely unaffected by this file's existence.
#'
#' Slide canvas is 13.333 x 7.5 in (16:9, PowerPoint's own default). Each
#' figure image is rendered at 12.8 x 6.3 in, leaving a ~1.2in top band for a
#' slide title placeholder added later by `officer` -- no title is drawn
#' inside the image itself. Text: tick labels >= 18pt, axis titles 20-24pt,
#' nothing below 14pt anywhere. Lines: data lines 2-3pt as rendered.

PRESENTATION_FONT             <- "Helvetica"
PRESENTATION_SLIDE_WIDTH_IN   <- 13.333
PRESENTATION_SLIDE_HEIGHT_IN  <- 7.5
PRESENTATION_IMG_WIDTH_IN     <- 12.8
PRESENTATION_IMG_HEIGHT_IN    <- 6.3
PRESENTATION_DPI              <- 200
PRESENTATION_TICK_PT          <- 18   # axis.text floor
PRESENTATION_AXIS_TITLE_PT    <- 22   # axis.title / legend.title (20-24pt range)
PRESENTATION_LEGEND_PT        <- 18   # legend.text
PRESENTATION_MIN_PT           <- 14   # hard floor, nothing may render smaller
PRESENTATION_LINEWIDTH_MIN    <- 2    # rendered pt
PRESENTATION_LINEWIDTH_MAX    <- 3    # rendered pt
PRESENTATION_LWD_TO_PT        <- ggplot2::.pt * 0.75   # same physical conversion as NATURE_LWD_TO_PT

#' Convert a target rendered line weight (pt) to a ggplot2 `linewidth=` value
#'
#' Same physical conversion as [nature_lwd()] (`R/nature_format.R`), just
#' without that function's 0.25-1.0pt ceiling -- presentation data lines run
#' 2-3pt.
#' @export
presentation_lwd <- function(pt) pt / PRESENTATION_LWD_TO_PT

#' ggplot2 theme for presentation slides
#'
#' Standalone builder, not a [nature_theme()] wrapper -- `nature_theme()`
#' hard-`stop()`s outside its 5-7pt range, so it cannot be reused at
#' presentation scale. Add after any other `theme()` calls so it wins. Does
#' not set `legend.position` -- callers keep full control of layout.
#'
#' @param base_size Axis-title / legend-title size in pt (default
#'   [PRESENTATION_AXIS_TITLE_PT]).
#' @param tick_size Axis-text (tick label) size in pt (default
#'   [PRESENTATION_TICK_PT]).
#' @param legend_size Legend text size in pt (default
#'   [PRESENTATION_LEGEND_PT]).
#' @export
presentation_theme <- function(base_size = PRESENTATION_AXIS_TITLE_PT,
                                tick_size = PRESENTATION_TICK_PT,
                                legend_size = PRESENTATION_LEGEND_PT) {
  if (base_size < PRESENTATION_MIN_PT || tick_size < PRESENTATION_MIN_PT || legend_size < PRESENTATION_MIN_PT) {
    stop("presentation_theme(): no text size may be below ", PRESENTATION_MIN_PT, "pt.")
  }
  ggplot2::theme(
    text              = ggplot2::element_text(family = PRESENTATION_FONT, size = base_size, colour = "grey10"),
    axis.text         = ggplot2::element_text(family = PRESENTATION_FONT, size = tick_size, colour = "grey10"),
    axis.title        = ggplot2::element_text(family = PRESENTATION_FONT, size = base_size, colour = "grey10"),
    legend.text       = ggplot2::element_text(family = PRESENTATION_FONT, size = legend_size, colour = "grey10"),
    legend.title      = ggplot2::element_text(family = PRESENTATION_FONT, size = base_size, colour = "grey10"),
    strip.text        = ggplot2::element_text(family = PRESENTATION_FONT, size = base_size, colour = "grey10"),
    plot.title        = ggplot2::element_blank(),
    plot.subtitle     = ggplot2::element_blank(),
    plot.caption      = ggplot2::element_blank(),
    panel.border      = ggplot2::element_blank(),
    axis.line         = ggplot2::element_line(linewidth = presentation_lwd(1.5), colour = "black"),
    axis.ticks        = ggplot2::element_line(linewidth = presentation_lwd(1.5), colour = "black"),
    plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background  = ggplot2::element_rect(fill = "white", colour = NA),
    plot.margin       = ggplot2::margin(60, 20, 18, 18, "pt")
  )
}

#' Save a plot as a single presentation-slide PNG
#'
#' No Nature size/dpi gates, no PDF/JPEG -- just one PNG at slide-image size.
#'
#' @param plot A ggplot object (or a grid/gtable grob -- passed straight to
#'   `ggplot2::ggsave()`, which accepts both).
#' @param path_stem Output path without extension.
#' @param width_in,height_in Image size in inches (defaults: the 12.8x6.3in
#'   slide-image area).
#' @param dpi Default [PRESENTATION_DPI] (200).
#' @return (Invisibly) the PNG path written.
#' @export
save_presentation_figure <- function(plot, path_stem,
                                      width_in = PRESENTATION_IMG_WIDTH_IN,
                                      height_in = PRESENTATION_IMG_HEIGHT_IN,
                                      dpi = PRESENTATION_DPI) {
  fs::dir_create(dirname(path_stem))
  png_path <- paste0(path_stem, ".png")
  ggplot2::ggsave(png_path, plot, width = width_in, height = height_in, units = "in",
                   dpi = dpi, device = ragg::agg_png, bg = "white")
  invisible(png_path)
}
