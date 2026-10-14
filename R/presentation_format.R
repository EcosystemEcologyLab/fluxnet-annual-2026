#' Shared presentation-mode constants and helpers for talk slide PNGs
#'
#' Parallel to `R/nature_format.R`, but for standalone talk-slide PNGs rather
#' than Nature-journal-spec figures -- never sourced BY `nature_format.R`,
#' and does not alter anything in it. Used only when a figure script's
#' `FLUXNET_PRESENTATION_MODE=1` block runs; the default (unset) path for
#' every figure script is completely unaffected by this file's existence.
#'
#' Design rule (v2, 2026-10-13): a presentation slide is the *same* figure as
#' the paper -- same layout, legends, panel letters, annotations, colours --
#' just bigger. Nothing is restyled, stripped, or replaced with a different
#' representation (e.g. direct labels instead of a legend). The only
#' permitted change is a uniform size scale-up of text, lines and points.
#'
#' [PRESENTATION_SCALE] is that scale factor, chosen so that applying it to
#' every text/line/point size already used in the manuscript figures lands
#' in the requested ranges without per-figure tuning:
#'   - manuscript tick/axis text is `NATURE_BASE_PT` (7pt) -> `7 * 2.5 = 17.5pt`,
#'     inside the requested 16-18pt tick-label window.
#'   - manuscript dense/small text is `NATURE_SMALL_PT` (5pt) -> `5 * 2.5 = 12.5pt`,
#'     just clear of the 12pt hard floor.
#'   - manuscript panel letters are `NATURE_PANEL_LETTER_PT` (8pt) -> 20pt.
#' A figure whose own smallest baked-in text is below 5pt (checked per figure
#' when the slide is built) needs a locally larger scale to clear the floor;
#' [presentation_scale_ggplot()] takes `scale` as an argument for that case.
#'
#' Image sizing: every slide PNG keeps the aspect ratio of the paper figure
#' (or paper sub-layout) it comes from and is sized to fit inside a
#' [PRESENTATION_MAX_IMG_WIDTH_IN] x [PRESENTATION_MAX_IMG_HEIGHT_IN] (12.3 x
#' 6.5in) box at [PRESENTATION_DPI] (200dpi) -- most paper figures are wider
#' than tall relative to that box, so sizing is height-limited in practice.
#' No fixed 16:9 canvas, no title band baked into the image -- slide titles
#' and any additional text are added later in PowerPoint, outside this file's
#' scope.

PRESENTATION_FONT             <- "Helvetica"
PRESENTATION_MAX_IMG_WIDTH_IN  <- 12.3
PRESENTATION_MAX_IMG_HEIGHT_IN <- 6.5
PRESENTATION_DPI              <- 200
PRESENTATION_SCALE            <- 2.5   # uniform text/line/point scale factor; see header
PRESENTATION_MIN_PT           <- 12    # hard floor, nothing may render smaller
PRESENTATION_LWD_TO_PT        <- ggplot2::.pt * 0.75   # same physical conversion as NATURE_LWD_TO_PT

#' Convert a target rendered line weight (pt) to a ggplot2 `linewidth=` value
#' @export
presentation_lwd <- function(pt) pt / PRESENTATION_LWD_TO_PT

#' Compute the slide-image size (inches) for a given paper-figure aspect ratio
#'
#' Fits `aspect_w:aspect_h` inside the [PRESENTATION_MAX_IMG_WIDTH_IN] x
#' [PRESENTATION_MAX_IMG_HEIGHT_IN] box, preserving aspect ratio exactly
#' (one dimension will equal the box limit, the other will be smaller).
#'
#' @param aspect_w,aspect_h Width/height of the source paper figure (or
#'   paper sub-layout), in any consistent unit (mm, in, px, ratio).
#' @export
presentation_fit_dims <- function(aspect_w, aspect_h,
                                   max_w_in = PRESENTATION_MAX_IMG_WIDTH_IN,
                                   max_h_in = PRESENTATION_MAX_IMG_HEIGHT_IN) {
  ratio <- aspect_w / aspect_h
  box_ratio <- max_w_in / max_h_in
  if (ratio >= box_ratio) {
    list(width_in = max_w_in, height_in = max_w_in / ratio)
  } else {
    list(width_in = max_h_in * ratio, height_in = max_h_in)
  }
}

#' Scale every fixed text/line/point size already set on a ggplot object
#'
#' Walks the plot's theme and layers and multiplies any *already-set, fixed*
#' size by `scale` -- it does not add, remove, or restyle anything, and does
#' not touch aesthetics mapped from data (e.g. `aes(size = n)`), legends,
#' colours, breaks, or layout. This is the mechanism behind the "only size
#' changes" rule: build the exact manuscript plot object (or panel, before
#' composition), then call this on it.
#'
#' Theme elements scaled (if explicitly set, i.e. not `NULL`/inherited):
#' `text`, `axis.text`, `axis.title`, `legend.text`, `legend.title`,
#' `strip.text` (`$size`), and `axis.line`, `axis.ticks`, `panel.border`
#' (`$linewidth`).
#'
#' Layers scaled: any layer with a fixed (non-mapped) `size` and/or
#' `linewidth` in `aes_params` (covers `geom_text()`/`geom_point()`'s `size`
#' and `geom_line()`/`geom_path()`/`geom_segment()`/etc.'s `linewidth`, which
#' is how every line/point weight in these figures is set -- see
#' `R/nature_format.R`'s header). A panel-letter or in-panel-note
#' `annotate("text", ...)` layer is a `GeomText` layer and is covered by the
#' same `size` handling as any other text layer.
#'
#' @param plot A single ggplot object (not a composed patchwork object --
#'   scale each panel before composing it, so the manuscript's own
#'   composition code can be reused unchanged on the pre-scaled panels).
#' @param scale Multiplier (default [PRESENTATION_SCALE]).
#' @export
presentation_scale_ggplot <- function(plot, scale = PRESENTATION_SCALE) {
  text_elements <- c("text", "axis.text", "axis.title", "legend.text",
                      "legend.title", "strip.text")
  for (nm in text_elements) {
    el <- plot$theme[[nm]]
    if (!is.null(el) && !is.null(el$size)) {
      el$size <- el$size * scale
      plot$theme[[nm]] <- el
    }
  }
  line_elements <- c("axis.line", "axis.ticks", "panel.border")
  for (nm in line_elements) {
    el <- plot$theme[[nm]]
    if (!is.null(el) && !is.null(el$linewidth)) {
      el$linewidth <- el$linewidth * scale
      plot$theme[[nm]] <- el
    }
  }
  for (i in seq_along(plot$layers)) {
    ap <- plot$layers[[i]]$aes_params
    if (!is.null(ap$size) && is.numeric(ap$size)) {
      ap$size <- ap$size * scale
    }
    if (!is.null(ap$linewidth) && is.numeric(ap$linewidth)) {
      ap$linewidth <- ap$linewidth * scale
    }
    plot$layers[[i]]$aes_params <- ap
  }
  plot
}

#' Save a plot as a single presentation-slide PNG
#'
#' Sizes the image to fit the paper figure's own aspect ratio inside the
#' 12.3 x 6.5in box (see [presentation_fit_dims()]) and saves one PNG, no
#' PDF/JPEG, no Nature size gates.
#'
#' @param plot A ggplot object (or a grid/gtable grob -- passed straight to
#'   `ggplot2::ggsave()`, which accepts both).
#' @param path_stem Output path without extension.
#' @param aspect_w,aspect_h Width/height of the source paper figure or
#'   sub-layout (any consistent unit) -- determines the output aspect ratio.
#' @param dpi Default [PRESENTATION_DPI] (200).
#' @return (Invisibly) the PNG path written.
#' @export
save_presentation_figure <- function(plot, path_stem, aspect_w, aspect_h,
                                      dpi = PRESENTATION_DPI) {
  dims <- presentation_fit_dims(aspect_w, aspect_h)
  fs::dir_create(dirname(path_stem))
  png_path <- paste0(path_stem, ".png")
  ggplot2::ggsave(png_path, plot, width = dims$width_in, height = dims$height_in,
                   units = "in", dpi = dpi, device = ragg::agg_png, bg = "white")
  invisible(png_path)
}
