# R/plot_constants.R
# Shared plotting constants, theme, colour palettes, axis labels, and geom
# wrappers for the FLUXNET Annual Paper 2026 pipeline.
#
# Ported from legacy/poster_constants.R and legacy/AMFOct25_poster.R.
# Reference only — do not edit legacy/ originals.
#
# Unit note: axis labels show g C m⁻² yr⁻¹ (and equivalent SI units). The new
# pipeline converts native FLUXNET units via fluxnet_convert_units() so labels
# now match the data. Do not change label strings.

library(ggplot2)
library(ggtext)
library(colorspace)
library(RColorBrewer)
library(Polychrome)

# ---- Poster / figure geometry sizes ----------------------------------------

poster_point_size <- 3.8
poster_linewidth  <- 1.0

# ---- IGBP constants --------------------------------------------------------

#' IGBP class full names
#'
#' Named character vector mapping IGBP codes to full English names.
IGBP_names <- c(
  ENF = "Evergreen Needleleaf Forest",
  EBF = "Evergreen Broadleaf Forest",
  DNF = "Deciduous Needleleaf Forest",
  DBF = "Deciduous Broadleaf Forest",
  MF  = "Mixed Forest",
  WSA = "Woody Savanna",
  SAV = "Savanna",
  GRA = "Grassland",
  SHR = "Shrubland",
  OSH = "Open Shrubland",
  CRO = "Cropland",
  WET = "Wetland",
  URB = "Urban",
  NV  = "Non-Vegetated",
  CSH = "Closed Shrubland"
)

#' IGBP class factor order
#'
#' Forests first, then open/savanna, then agriculture/wetland/other.
IGBP_order <- c(
  "ENF", "EBF", "DNF", "DBF", "MF",
  "WSA", "SAV", "GRA", "SHR", "OSH",
  "CRO", "WET", "URB", "NV", "CSH"
)

#' Shape set for IGBP classes
#'
#' 15-element integer vector with good print separation.
shape_igbp <- c(16, 15, 17, 18, 1, 2, 0, 5, 6, 7, 9, 10, 8, 4, 3)

# ---- Axis labels -----------------------------------------------------------
# Labels use Unicode superscript characters (e.g. \u207b\u00b2 = ⁻², \u207b\u00b9 = ⁻¹).
# These render correctly in element_text(), element_markdown(), and colorbar
# guide titles — no HTML <sup> tags required.
# Units reflect post-conversion values from fluxnet_convert_units().

lab_precip_annual <- "Precipitation (mm yr\u207b\u00b9)"
lab_temp_annual   <- "Temperature (\u00b0C)"

lab_gpp_daily     <- "GPP (g C m\u207b\u00b2 d\u207b\u00b9)"
lab_reco_daily    <- "RECO (g C m\u207b\u00b2 d\u207b\u00b9)"
lab_nee_daily     <- "NEE (g C m\u207b\u00b2 d\u207b\u00b9)"

lab_nee_weekly    <- "NEE (g C m\u207b\u00b2 wk\u207b\u00b9)"
lab_nee_monthly   <- "NEE (g C m\u207b\u00b2 mo\u207b\u00b9)"

lab_gpp_annual    <- "GPP (g C m\u207b\u00b2 yr\u207b\u00b9)"
lab_reco_annual   <- "RECO (g C m\u207b\u00b2 yr\u207b\u00b9)"
lab_nee_annual    <- "NEE (g C m\u207b\u00b2 yr\u207b\u00b9)"

# ---- Colour palettes -------------------------------------------------------

#' Fixed IGBP colour palette — stable across all figures
#'
#' Named character vector mapping IGBP codes to hex colours.
#' Forests: dark → light green; Shrublands: olive; Savannas/Grasslands: gold → yellow;
#' Wetland: blue; Cropland: orange-brown; Urban: purple; Barren: greys.
#' Use [scale_fill_igbp()] and [scale_color_igbp()] in ggplot2 figures.
IGBP_colours <- c(
  ENF = "#1B5E20",
  EBF = "#2E7D32",
  DNF = "#388E3C",
  DBF = "#66BB6A",
  MF  = "#A5D6A7",
  CSH = "#827717",
  OSH = "#AFA21A",
  WSA = "#D4A017",
  SAV = "#F9A825",
  GRA = "#FDD835",
  WET = "#1565C0",
  CRO = "#BF360C",
  URB = "#6A1B9A",
  NV  = "#757575",
  BSV = "#BDBDBD"
)

#' Pattern assignment by ecological group
#'
#' Named character vector mapping IGBP codes to ggpattern pattern strings.
IGBP_patterns <- c(
  ENF = "none", EBF = "none", DNF = "none", DBF = "none", MF = "none",
  CSH = "stripe", OSH = "stripe",
  WSA = "circle", SAV = "circle", GRA = "circle",
  WET = "crosshatch",
  CRO = "horizontal stripe",
  URB = "none", NV = "none", BSV = "none"
)

#' Discrete fill scale for IGBP classes
#'
#' Uses the fixed [IGBP_colours] palette — stable regardless of which classes
#' are present in the data.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_fill_manual()].
#' @return A ggplot2 scale object.
scale_fill_igbp <- function(...) {
  ggplot2::scale_fill_manual(values = IGBP_colours, ...)
}

#' Discrete colour scale for IGBP classes
#'
#' Uses the fixed [IGBP_colours] palette — stable regardless of which classes
#' are present in the data.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_color_manual()].
#' @return A ggplot2 scale object.
scale_color_igbp <- function(...) {
  ggplot2::scale_color_manual(values = IGBP_colours, ...)
}

# DEPRECATED — use scale_fill_igbp() or scale_color_igbp() instead
#' Qualitative HCL palette for IGBP classes (deprecated)
#'
#' @param n Integer. Number of colours required.
#' @return Character vector of hex colour codes.
poster_pal <- function(n) {
  colorspace::qualitative_hcl(n, palette = "Dynamic", c = 90, l = 60)
}

#' High-contrast palette for country-level plots
#'
#' Uses Polychrome with a fixed seed for reproducible colour assignment.
#'
#' @param n Integer. Number of colours required.
#' @return Named character vector of hex colour codes.
poster_pal_country <- function(n) {
  set.seed(42)
  Polychrome::createPalette(
    n,
    seedcolors = c("#9DB9F1", "#FF0000", "#00FF00", "#0000FF")
  )
}

#' Discrete fill scale for country-level plots
#'
#' @param levels Character vector of country names (sets colour mapping).
#' @param ... Additional arguments passed to [ggplot2::scale_fill_manual()].
#' @return A ggplot2 scale object.
scale_country_fill <- function(levels, ...) {
  cols <- poster_pal_country(length(levels))
  vals <- stats::setNames(cols, levels)
  ggplot2::scale_fill_manual(values = vals, ...)
}

# DEPRECATED — use scale_fill_igbp() or scale_color_igbp() instead
#' Discrete colour scale for IGBP classes (deprecated)
#'
#' @param ... Additional arguments passed to
#'   [colorspace::scale_color_discrete_qualitative()].
#' @return A ggplot2 scale object.
scale_igbp_color <- function(...) {
  colorspace::scale_color_discrete_qualitative(...)
}

# ---- Theme -----------------------------------------------------------------

#' FLUXNET figure theme
#'
#' `theme_classic()` base with inward ticks, a crisp panel border, and
#' print-friendly defaults. This is the primary theme for all pipeline figures.
#'
#' @param base_size Numeric. Base font size in points (default 16).
#' @return A [ggplot2::theme()] object.
#' @examples
#' ggplot2::ggplot() + fluxnet_theme()
fluxnet_theme <- function(base_size = 16) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      panel.border        = ggplot2::element_rect(color = "black", fill = NA,
                                                  linewidth = 0.8),
      panel.background    = ggplot2::element_blank(),
      axis.text           = ggplot2::element_text(color = "black"),
      axis.ticks          = ggplot2::element_line(color = "black"),
      axis.ticks.length   = grid::unit(-4, "pt"),
      axis.ticks.length.x = grid::unit(-4, "pt"),
      axis.ticks.length.y = grid::unit(-4, "pt"),
      # Render HTML superscripts/subscripts in axis titles globally.
      # Plain-text labels are unaffected; HTML labels (e.g. "mm yr<sup>-1</sup>")
      # render correctly without per-function element_markdown() overrides.
      axis.title.x        = ggtext::element_markdown(),
      axis.title.y        = ggtext::element_markdown(),
      legend.position     = "bottom"
    )
}

#' Alias for [fluxnet_theme()]
#'
#' Retained for compatibility with legacy poster scripts.
#' Prefer [fluxnet_theme()] in new code.
#'
#' @inheritParams fluxnet_theme
#' @return A [ggplot2::theme()] object.
poster_theme <- fluxnet_theme

# ---- Geom wrappers ---------------------------------------------------------

#' Point geom with poster-friendly defaults
#'
#' @param ... Additional arguments passed to [ggplot2::geom_point()].
#' @return A ggplot2 layer.
poster_geom_point <- function(...) {
  ggplot2::geom_point(size = poster_point_size, stroke = 0.7, alpha = 0.8, ...)
}

#' Line geom with poster-friendly defaults
#'
#' @param ... Additional arguments passed to [ggplot2::geom_line()].
#' @return A ggplot2 layer.
poster_geom_line <- function(...) {
  ggplot2::geom_line(linewidth = poster_linewidth, alpha = 0.9, ...)
}

# ---- Save utilities --------------------------------------------------------

#' Save a draft figure to the figures directory
#'
#' Saves a PNG at 300 dpi to `<FLUXNET_DATA_ROOT>/figures/`.
#'
#' @param p A ggplot object.
#' @param filename Character. Output filename (e.g. `"fig1_igbp.png"`).
#' @param width,height Numeric. Plot dimensions in inches (default 8 × 10).
#' @param dpi Integer. Resolution in dots per inch (default 300).
#' @return Invisibly returns the output path.
save_plot_draft <- function(p, filename, width = 8, height = 10, dpi = 300) {
  out_dir <- file.path(Sys.getenv("FLUXNET_DATA_ROOT", "data"), "figures")
  fs::dir_create(out_dir)
  out_path <- file.path(out_dir, filename)
  ggplot2::ggsave(out_path, plot = p, width = width, height = height,
                  units = "in", dpi = dpi, limitsize = FALSE)
  invisible(out_path)
}

#' Save a final publication figure to the figures directory
#'
#' Saves both PNG (600 dpi) and PDF (cairo) versions to
#' `<FLUXNET_DATA_ROOT>/figures/`.
#'
#' @param p A ggplot object.
#' @param filename_stub Character. Base filename without extension
#'   (e.g. `"fig1_igbp"`).
#' @param width,height Numeric. Plot dimensions in inches (default 8 × 10).
#' @param dpi Integer. PNG resolution in dots per inch (default 600).
#' @return Invisibly returns a named character vector of output paths.
save_plot_final <- function(p, filename_stub, width = 8, height = 10,
                            dpi = 600) {
  out_dir <- file.path(Sys.getenv("FLUXNET_DATA_ROOT", "data"), "figures")
  fs::dir_create(out_dir)
  png_path <- file.path(out_dir, paste0(filename_stub, ".png"))
  pdf_path <- file.path(out_dir, paste0(filename_stub, ".pdf"))
  ggplot2::ggsave(png_path, plot = p, width = width, height = height,
                  units = "in", dpi = dpi, limitsize = FALSE)
  ggplot2::ggsave(pdf_path, plot = p, width = width, height = height,
                  units = "in", device = grDevices::cairo_pdf, limitsize = FALSE)
  invisible(c(png = png_path, pdf = pdf_path))
}
