#' Shared Nature-format constants and helpers for draft-manuscript figures
#'
#' As `scripts/figure4_representativeness.R` applies them: Helvetica
#' throughout; all text 5-7 pt at final render size; panel letters 8 pt bold
#' lower case; superscripts via plotmath/`expression()`, never a Unicode
#' superscript glyph (the PDF export's base PostScript Helvetica has no
#' usable glyph for some of them); no caption or explanatory text drawn
#' inside the figure itself (put it in the `.legend.txt` file instead); line
#' weights 0.25-1 pt **as measured in the rendered PDF** (by
#' `scripts/check_figure_format.R`, via `pdftocairo -svg`'s `stroke-width`) --
#' NOT the same number as ggplot2's `linewidth` aesthetic. A ggplot2
#' `linewidth` of 1 mm renders as `linewidth * .pt * 0.75` PDF points (`.pt`
#' converts mm to "big points", `* 0.75` converts big points, 1/96in, to true
#' PDF points, 1/72in) -- empirically confirmed at ~2.134 pt per linewidth
#' unit by rendering known linewidths and measuring the output SVG. Use
#' [nature_lwd()] to convert a target *output* pt value to the `linewidth=`
#' argument a geom or theme element needs; never set `linewidth=` to a raw
#' 0.25-1 number directly, since that silently renders ~2.13x too thick and
#' will fail `scripts/check_figure_format.R`. A vector PDF beside every PNG.
#'
#' Main-text figures: 89 or 183 mm wide, at most 247 mm tall. Extended Data
#' figures: at most 180 mm wide and 240 mm tall, plus a 300 p.p.i. JPEG
#' alongside the PNG/PDF (use `extended_data = TRUE` in
#' [save_nature_figure()]).

NATURE_FONT             <- "Helvetica"
NATURE_BASE_PT          <- 7     # default body/axis text, within the 5-7 pt range
NATURE_SMALL_PT         <- 5     # dense tick labels/legend text, lower bound of the range
NATURE_PANEL_LETTER_PT  <- 8     # bold lower-case panel letters
NATURE_LINEWIDTH_MIN    <- 0.25
NATURE_LINEWIDTH_MAX    <- 1
NATURE_WIDTH_SINGLE_MM  <- 89
NATURE_WIDTH_DOUBLE_MM  <- 183
NATURE_MAX_HEIGHT_MM    <- 247
NATURE_ED_MAX_WIDTH_MM  <- 180
NATURE_ED_MAX_HEIGHT_MM <- 240
NATURE_ED_JPEG_DPI      <- 300
NATURE_LWD_TO_PT        <- ggplot2::.pt * 0.75   # ggplot2 linewidth unit -> rendered PDF pt; see header

#' Convert a target rendered line weight (pt) to a ggplot2 `linewidth=` value
#'
#' @param pt Desired weight in the final PDF, in points (should be within
#'   `[`[NATURE_LINEWIDTH_MIN]`, `[NATURE_LINEWIDTH_MAX]`]`).
#' @export
nature_lwd <- function(pt) pt / NATURE_LWD_TO_PT

#' Axis-label formatter using a true minus sign (not ASCII hyphen-minus)
#'
#' Pass as `labels = nature_minus_labels()` to any `scale_x/y_continuous()`
#' whose values can be negative -- R's/ggplot2's default tick-label formatter
#' renders negative numbers with ASCII "-" (U+002D), not the typographic
#' minus sign (U+2212) Nature format requires. A literal U+2212 CHARACTER in
#' a plain-text label does not fix this: confirmed directly that the base
#' `grDevices::pdf()` device (PostScript Helvetica, as [save_nature_figure()]
#' uses for the PDF output) cannot encode U+2212 as plain text and silently
#' substitutes ASCII "-" back in, with a `mbcsToSbcs` warning -- the same
#' glyph-availability gap [panel_letter()]'s own doc note and this file's
#' header describe for axis-title superscripts. The fix is the same one
#' already used for axis titles throughout this codebase: route the minus
#' sign through plotmath's unary-minus operator (rendered via the Symbol
#' font's own minus glyph, not character encoding) instead of embedding
#' U+2212 as a character -- this returns a label function producing
#' `expression()`s, not plain strings, which ggplot2/grid render as plotmath.
#' @export
nature_minus_labels <- function(...) {
  base_fn <- scales::label_number(...)
  function(x) {
    txt <- base_fn(x)
    parsed <- lapply(txt, function(t) {
      e <- tryCatch(parse(text = t)[[1]], error = function(e) NULL)
      ## Falls back to a quoted plotmath string for text parse() can't
      ## handle as a bare numeric literal (e.g. a space-grouped "4 000") --
      ## such text contains no minus sign needing glyph substitution anyway.
      if (is.null(e)) str2lang(deparse(t)) else e
    })
    do.call(expression, parsed)
  }
}

#' ggplot2 theme add-on enforcing the Nature text/line rules
#'
#' Add after any other `theme()` calls so it wins. Does not set
#' `legend.position`, panel background, or anything content-specific --
#' callers keep full control of layout; this only pins font family/sizes and
#' line weights to the allowed ranges.
#'
#' @param base_size Base text size in pt (default [NATURE_BASE_PT]). Must be
#'   within `[5, 7]`.
#' @param panel_border_pt Rendered weight (pt) for `panel.border` (default
#'   the midpoint of the allowed range).
#' @export
nature_theme <- function(base_size = NATURE_BASE_PT, panel_border_pt = (NATURE_LINEWIDTH_MIN + NATURE_LINEWIDTH_MAX) / 2) {
  if (base_size < NATURE_SMALL_PT || base_size > NATURE_BASE_PT) {
    stop("nature_theme(): base_size must be within [", NATURE_SMALL_PT, ", ",
         NATURE_BASE_PT, "] pt (Nature's 5-7 pt text rule). Got: ", base_size)
  }
  ggplot2::theme(
    text              = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    axis.text         = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    axis.title        = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    ## NOTE: does not set axis.title.x/.y explicitly. R/plot_constants.R::
    ## fluxnet_theme() sets those to ggtext::element_markdown(); ggplot2
    ## refuses to merge an element_markdown() with a later element_text()
    ## override (`+` on two different element classes errors: "Only elements
    ## of the same class can be merged") -- confirmed by direct reproduction
    ## while building scripts/figure_flux_comparison_combo.R. A caller whose
    ## theme chain includes fluxnet_theme() AND needs a plotmath
    ## expression()/bquote() axis title (which element_markdown() cannot
    ## render -- it deparses them as literal text instead) must avoid
    ## fluxnet_theme() for that plot, not rely on nature_theme() to override
    ## it -- see combo_theme() in figure_flux_comparison_combo.R for the
    ## pattern (theme_classic() rebuilt directly, without ggtext).
    legend.text       = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    legend.title      = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    strip.text        = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    plot.title        = ggplot2::element_blank(),
    plot.subtitle     = ggplot2::element_blank(),
    plot.caption      = ggplot2::element_blank(),
    panel.border      = ggplot2::element_rect(colour = "black", fill = NA, linewidth = nature_lwd(panel_border_pt)),
    axis.line         = ggplot2::element_line(linewidth = nature_lwd(NATURE_LINEWIDTH_MIN), colour = "black"),
    axis.ticks        = ggplot2::element_line(linewidth = nature_lwd(NATURE_LINEWIDTH_MIN), colour = "black"),
    plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background  = ggplot2::element_rect(fill = "white", colour = NA)
  )
}

#' Bold lower-case panel-letter annotation layer
#'
#' @param letter Single letter, any case (lower-cased automatically).
#' @param size_pt Font size in pt (default [NATURE_PANEL_LETTER_PT]).
#' @param x,y,hjust,vjust npc-style panel-relative placement passed to
#'   `annotate("text", ...)`; defaults put the letter just inside the
#'   top-left corner.
#' @export
panel_letter <- function(letter, size_pt = NATURE_PANEL_LETTER_PT,
                          x = -Inf, y = Inf, hjust = -0.4, vjust = 1.4) {
  ggplot2::annotate(
    "text", x = x, y = y, label = tolower(letter),
    hjust = hjust, vjust = vjust, fontface = "bold",
    family = NATURE_FONT, size = size_pt / ggplot2::.pt, colour = "black"
  )
}

#' Save a plot as PNG + PDF (+ JPEG for Extended Data) under the Nature rules
#'
#' Checks the requested width/height against the main-text or Extended Data
#' limits (stops rather than silently exceeding them) and writes a matched
#' PNG (`ragg::agg_png`, 600 dpi) and PDF (base `grDevices::pdf`, Helvetica --
#' the only family name base `pdf()` accepts from the 14 PostScript
#' standard names) at the same physical size. `extended_data = TRUE` also
#' writes a 300 p.p.i. JPEG beside them.
#'
#' @param plot A ggplot object.
#' @param path_stem Output path without extension, e.g.
#'   `"review/figures/draft_manuscript_v1/fig_01a_map_current_network"`.
#' @param width_mm,height_mm Final figure size in mm.
#' @param extended_data Logical (default `FALSE`). `TRUE` applies the
#'   Extended Data width/height limits and also writes a JPEG.
#' @return (Invisibly) a named list of the file paths written.
#' @export
save_nature_figure <- function(plot, path_stem, width_mm, height_mm, extended_data = FALSE) {
  if (extended_data) {
    if (width_mm > NATURE_ED_MAX_WIDTH_MM || height_mm > NATURE_ED_MAX_HEIGHT_MM) {
      stop("save_nature_figure(): Extended Data figure ", width_mm, "x", height_mm,
           " mm exceeds the ", NATURE_ED_MAX_WIDTH_MM, "x", NATURE_ED_MAX_HEIGHT_MM,
           " mm limit.")
    }
  } else {
    if (!isTRUE(all.equal(width_mm, NATURE_WIDTH_SINGLE_MM)) &&
        !isTRUE(all.equal(width_mm, NATURE_WIDTH_DOUBLE_MM))) {
      stop("save_nature_figure(): main-text figure width must be ", NATURE_WIDTH_SINGLE_MM,
           " or ", NATURE_WIDTH_DOUBLE_MM, " mm. Got: ", width_mm)
    }
    if (height_mm > NATURE_MAX_HEIGHT_MM) {
      stop("save_nature_figure(): main-text figure height ", height_mm, " mm exceeds the ",
           NATURE_MAX_HEIGHT_MM, " mm limit.")
    }
  }

  fs::dir_create(dirname(path_stem))
  png_path  <- paste0(path_stem, ".png")
  pdf_path  <- paste0(path_stem, ".pdf")
  jpeg_path <- paste0(path_stem, ".jpg")

  ggplot2::ggsave(png_path, plot, width = width_mm, height = height_mm, units = "mm",
                   dpi = 600, device = ragg::agg_png, bg = "white")
  ggplot2::ggsave(pdf_path, plot, width = width_mm, height = height_mm, units = "mm",
                   device = grDevices::pdf, family = "Helvetica", bg = "white")

  out <- list(png = png_path, pdf = pdf_path)
  if (extended_data) {
    ggplot2::ggsave(jpeg_path, plot, width = width_mm, height = height_mm, units = "mm",
                     dpi = NATURE_ED_JPEG_DPI, device = ragg::agg_jpeg, bg = "white", quality = 95)
    out$jpeg <- jpeg_path
  }
  invisible(out)
}
