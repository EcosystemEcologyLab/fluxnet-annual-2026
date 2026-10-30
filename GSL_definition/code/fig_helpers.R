## fig_helpers.R — shared helpers for the GSL figure pack.
## Sourced by every 0X_fig*.R / 09_benchmark.R / 10_supplementary.R script
## (which have already sourced 00_config.R). Not run directly.
##
## PeerJ restyle (2026-10-30): GSL figures now follow PeerJ's own author
## instructions (170 mm width, >=8 pt text, >=300 dpi, one PNG+PDF per
## figure named "Figure N"/"Supplemental Figure SN"), NOT
## R/nature_format.R's Nature conventions (89/183 mm, 5-7 pt text, <=1 pt
## lines, "fig_0N_..." stems) -- that file is still sourced (its
## `nature_lwd()` unit-conversion math and `NATURE_FONT`/`panel_letter()`
## are generic, not Nature-specific limits) but `nature_theme()` and
## `save_nature_figure()` are NOT used below; this file defines its own
## PeerJ-sized equivalents instead so the two paper's figure conventions
## stay fully independent.

source("R/nature_format.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

GSL_FIG_DIR <- file.path(GSL_ROOT, "figures")
if (!dir.exists(GSL_FIG_DIR)) dir.create(GSL_FIG_DIR, recursive = TRUE)

## ---- PeerJ page/figure/text constants --------------------------------------
## Author instructions (read 2026-10-30): one file per figure, >=300 dpi at
## final size, fit an 8.5x11in page, no text below 8pt, figure width ~170mm.
PEERJ_PAGE_WIDTH_MM     <- 215.9  # 8.5 in
PEERJ_PAGE_HEIGHT_MM    <- 279.4  # 11 in
PEERJ_FIG_WIDTH_MM      <- 170    # "about 170 mm"
## Leaves ~40 mm of the 279.4 mm page height for margins/caption -- a figure
## at the full page height would leave no room for either.
PEERJ_FIG_MAX_HEIGHT_MM <- 240
PEERJ_DPI               <- 600    # well above the >=300 dpi floor

PEERJ_BASE_PT         <- 8   # tick labels, legend text/title -- the >=8pt floor
PEERJ_AXIS_TITLE_PT   <- 9
PEERJ_PANEL_LETTER_PT <- 10  # bold
PEERJ_ANNOTATION_PT   <- 8   # in-panel text (site names, bracket labels) -- same >=8pt floor

## Line weights, as target RENDERED pt (convert via nature_lwd(), whose
## ggplot-linewidth-to-PDF-pt math is a generic unit conversion, not a
## Nature-specific limit).
PEERJ_LWD_DATA       <- 1.8  # main data lines: 1.5-2 pt
PEERJ_LWD_REF        <- 1.0  # reference/threshold lines, panel border, axis lines/ticks: ~1 pt
PEERJ_LWD_OTHERYEARS <- 0.6  # Figure 1's faint "other years" lines: thicker/darker than before,
                             # still clearly lighter than the highlighted year's PEERJ_LWD_DATA line

## ---- One functional-group palette, used in every figure that colours by
## group (Figures 2, 3, 4; Okabe-Ito colourblind-safe set). The fixed colour
## MAPPING (name -> colour) is the same everywhere via `values=`; `limits=`
## is deliberately NOT set here, so each figure's legend shows only the
## groups actually present in its own data (e.g. Figure 3's 21 trend sites
## have no mixed-forest member, so "mixed forest" does not appear in its
## legend) while the colour each group gets never changes between figures. ----
GSL_GROUP_ORDER <- c("grassland", "evergreen forest", "deciduous forest",
                      "woody-grass mosaic", "mixed forest")
GSL_GROUP_COLORS <- c(
  "grassland"           = "#E69F00",
  "evergreen forest"    = "#009E73",
  "deciduous forest"    = "#56B4E9",
  "woody-grass mosaic"  = "#D55E00",
  "mixed forest"        = "#CC79A7"
)
scale_color_gsl_group <- function(..., drop = TRUE) {
  scale_color_manual(values = GSL_GROUP_COLORS, breaks = GSL_GROUP_ORDER, drop = drop, ...)
}
scale_fill_gsl_group <- function(..., drop = TRUE) {
  scale_fill_manual(values = GSL_GROUP_COLORS, breaks = GSL_GROUP_ORDER, drop = drop, ...)
}

## ---- One colourblind-safe, discrete skewness-THIRD colour scale (PeerJ
## restyle, 2026-10-30), used everywhere skewness is shown by colour
## (Figures 1, 2a, 2d, 4) -- replaces the earlier continuous-viridis,
## capped-at-2 scale (a reviewer could not reliably distinguish most sites,
## clustered in 0.2-1.5, by colour alone). Three discrete viridis stops
## (not the full continuous ramp): colourblind-safe by construction, and
## `begin`/`end` keep clear of viridis's near-black and near-yellow
## extremes so the three colours stay visually distinct (confirmed
## monotonic greyscale luminance: ~60/109/180 of 255). Exact numeric
## skewness values are not dropped -- they remain in each figure's
## .legend.txt and in tables/derived_site.csv (every figure's input_sources
## already cites that table). ----
GSL_SKEW_THIRD_LEVELS <- c("low", "middle", "high")
GSL_SKEW_THIRD_COLORS <- setNames(
  substr(viridisLite::viridis(3, begin = 0.12, end = 0.85), 1, 7),
  GSL_SKEW_THIRD_LEVELS
)
scale_color_gsl_skew_third <- function(..., name = "Skewness third", labels = c("Low", "Middle", "High")) {
  scale_color_manual(values = GSL_SKEW_THIRD_COLORS, breaks = GSL_SKEW_THIRD_LEVELS,
                      labels = labels, name = name, ...)
}
scale_fill_gsl_skew_third <- function(..., name = "Skewness third", labels = c("Low", "Middle", "High")) {
  scale_fill_manual(values = GSL_SKEW_THIRD_COLORS, breaks = GSL_SKEW_THIRD_LEVELS,
                     labels = labels, name = name, ...)
}

## ---- One marker convention, used everywhere a figure distinguishes the
## Z17 (10% of P99) and T95 thresholds with a point: Z17 = black circle,
## T95 = red triangle (Figures 2b, 2c, 3a, 4c). `breaks=` is set explicitly
## (not left to ggplot's default alphabetical sort of "T95"/"Z17", which
## would put T95 first and silently swap override.aes's colours onto the
## wrong legend row -- found by inspecting Figure 2's first render). ----
GSL_MARKER_SHAPES <- c("Z17" = 16, "T95" = 17)
GSL_MARKER_COLORS <- c("Z17" = "black", "T95" = "red")
scale_shape_gsl_marker <- function(...) {
  scale_shape_manual(
    values = GSL_MARKER_SHAPES, breaks = c("Z17", "T95"), name = NULL,
    guide = guide_legend(override.aes = list(colour = unname(GSL_MARKER_COLORS[c("Z17", "T95")]))),
    ...
  )
}

## ---- Sensitivity fill scale, capped near the 95th percentile of the
## values actually being plotted (Figure 2b and its 10_supplementary.R
## duplicates) -- without a cap, a handful of extreme low-threshold tiles
## (seen e.g. in the GPP_DT_VUT_REF supplementary variant) compress the
## bulk of the heat map into a narrow, washed-out sliver of the palette.
## Values above the cap are squished to the top colour (not greyed out),
## and the top break is labelled "≥ <cap>" so the cap stays visible to the
## reader. Computed fresh from each call's own `values` (not a fixed
## constant), since the main run and each supplementary variant have their
## own distributions. ----
scale_fill_gsl_sensitivity <- function(values, palette = "Blues", direction = 1,
                                        name = "Sensitivity\n(days/%P99,\nabs. value)",
                                        cap_quantile = 0.95, ...) {
  cap <- unname(stats::quantile(values, cap_quantile, na.rm = TRUE))
  brks <- pretty(c(0, cap), n = 4)
  brks <- brks[brks < cap]
  brks <- c(brks, cap)
  labs <- as.character(round(brks, 1))
  ## ASCII ">=", not U+2265 -- the base pdf() device's PostScript Helvetica
  ## cannot encode it as plain text and silently substitutes ">=" back in
  ## with an `mbcsToSbcs` warning anyway (same glyph-availability gap
  ## documented for the minus sign in R/nature_format.R).
  labs[length(labs)] <- paste0(">= ", round(cap, 1))
  scale_fill_distiller(palette = palette, direction = direction, name = name,
                        limits = c(0, cap), breaks = brks, labels = labs,
                        oob = scales::squish, ...)
}

## ---- Theme: PeerJ sizing, independent of nature_theme() (which stops if
## base_size > 7pt -- PeerJ requires >=8pt, so that function cannot be
## reused here; this mirrors its structure with PeerJ's own constants and
## no upper-bound check). ----
theme_gsl <- function(base_size = PEERJ_BASE_PT, axis_title_pt = PEERJ_AXIS_TITLE_PT,
                       panel_border_pt = PEERJ_LWD_REF) {
  ggplot2::theme(
    text              = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    axis.text         = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    axis.title        = ggplot2::element_text(family = NATURE_FONT, size = axis_title_pt, colour = "grey10"),
    legend.text       = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    legend.title      = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    strip.text        = ggplot2::element_text(family = NATURE_FONT, size = base_size, colour = "grey10"),
    plot.title        = ggplot2::element_blank(),
    plot.subtitle     = ggplot2::element_blank(),
    plot.caption      = ggplot2::element_blank(),
    panel.border      = ggplot2::element_rect(colour = "black", fill = NA, linewidth = nature_lwd(panel_border_pt)),
    axis.line         = ggplot2::element_line(linewidth = nature_lwd(panel_border_pt), colour = "black"),
    axis.ticks        = ggplot2::element_line(linewidth = nature_lwd(panel_border_pt), colour = "black"),
    plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background  = ggplot2::element_rect(fill = "white", colour = NA)
  )
}

#' Bold lower-case panel-letter annotation at PeerJ's 10 pt size
#'
#' Thin wrapper around R/nature_format.R's panel_letter() (generic
#' annotate("text",...) placement logic, not Nature-specific) with PeerJ's
#' size substituted for Nature's 8pt default. Pass `x=`/`hjust=` explicitly
#' under a log-x panel (panel_letter()'s default x=-Inf is NaN under
#' scale_x_log10() -- see 06_fig2.R).
gsl_panel_letter <- function(letter, size_pt = PEERJ_PANEL_LETTER_PT, ...) {
  panel_letter(letter, size_pt = size_pt, ...)
}

#' Save a GSL figure (PNG + PDF, PeerJ sizing: 170 mm wide by default,
#' >=300 dpi) and write its .legend.txt alongside it, plus a .meta.json via
#' the local write_meta() (NOT R/utils.R::write_output_metadata() -- see
#' 00_config.R). NOT save_nature_figure() -- that enforces Nature's fixed
#' 89/183 mm widths and errors above them.
#'
#' @param plot ggplot/patchwork object.
#' @param stem Output path stem, e.g. "Figure 1" or "Supplemental Figure
#'   S1" (no directory, no extension) -- written under GSL_FIG_DIR, per
#'   PeerJ's one-file-per-figure naming.
#' @param width_mm,height_mm Final figure size in mm (defaults: PeerJ's
#'   170 mm width; height capped at PEERJ_FIG_MAX_HEIGHT_MM to fit the page).
#' @param legend_lines Character vector, one .legend.txt line per element.
#' @param input_sources,notes Passed to write_meta().
save_gsl_figure <- function(plot, stem, width_mm = PEERJ_FIG_WIDTH_MM, height_mm,
                             legend_lines, input_sources, notes = "") {
  if (width_mm > PEERJ_PAGE_WIDTH_MM) {
    stop("save_gsl_figure(): width ", width_mm, " mm exceeds the ", PEERJ_PAGE_WIDTH_MM, " mm page width.")
  }
  if (height_mm > PEERJ_FIG_MAX_HEIGHT_MM) {
    stop("save_gsl_figure(): height ", height_mm, " mm exceeds the ", PEERJ_FIG_MAX_HEIGHT_MM, " mm limit.")
  }
  path_stem <- file.path(GSL_FIG_DIR, stem)
  fs::dir_create(dirname(path_stem))
  png_path <- paste0(path_stem, ".png")
  pdf_path <- paste0(path_stem, ".pdf")
  ggplot2::ggsave(png_path, plot, width = width_mm, height = height_mm, units = "mm",
                   dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
  ggplot2::ggsave(pdf_path, plot, width = width_mm, height = height_mm, units = "mm",
                   device = grDevices::pdf, family = "Helvetica", bg = "white")
  writeLines(legend_lines, paste0(path_stem, ".legend.txt"))
  write_meta(png_path, input_sources = input_sources, notes = notes)
  message("Saved: ", path_stem, ".png/.pdf/.legend.txt/.meta.json")
  invisible(list(png = png_path, pdf = pdf_path))
}
