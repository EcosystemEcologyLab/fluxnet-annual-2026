# R/poster_constants.R
# At top of poster_constants.R (if not already loaded)
library(RColorBrewer)

# Large, print-friendly sizes (pair with theme in AMFOct25_poster.R)
poster_point_size <- 3.8
poster_linewidth  <- 1.0

# IGBP code → full name (edit as desired)
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
  CSH = "Closed Shrubland" # if present in your set
)

# consistent factor order (forests first, then open, then ag/wet/other)
IGBP_order <- c("ENF","EBF","DNF","DBF","MF","WSA","SAV","GRA","SHR","OSH","CRO","WET","URB","NV","CSH")

# Axis labels (ggtext-ready)
lab_precip_annual <- "Precipitation (mm y<sup>-1</sup>)"
lab_temp_annual   <- "Temperature (°C)"
lab_gpp_daily     <- "GPP (g C m<sup>-2</sup> d<sup>-1</sup>)"
lab_reco_daily    <- "RECO (g C m<sup>-2</sup> d<sup>-1</sup>)"
lab_nee_daily     <- "NEE (g C m<sup>-2</sup> d<sup>-1</sup>)"
lab_nee_weekly   <- "NEE (g C m<sup>-2</sup> wk<sup>-1</sup>)"
lab_nee_monthly   <- "NEE (g C m<sup>-2</sup> m<sup>-1</sup>)"

lab_gpp_annual    <- "GPP (g C m<sup>-2</sup> yr<sup>-1</sup>)"
lab_reco_annual   <- "RECO (g C m<sup>-2</sup> yr<sup>-1</sup>)"
lab_nee_annual    <- "NEE (g C m<sup>-2</sup> yr<sup>-1</sup>)"

# A consistent discrete color scale helper (colorspace)
scale_igbp_color <- function(...) ggplot2::scale_color_discrete_qualitative(...)

# A shape set with good separation in print (you can customize)
shape_igbp <- c(16, 15, 17, 18, 1, 2, 0, 5, 6, 7, 9, 10, 8, 4, 3)

# --- poster_constants.R additions/edits ---

# Palette helper (already present, keep it)
# poster_pal <- function(n) colorspace::qualitative_hcl(n, palette = "Dark 3")
# 
# # Discrete color scale using the palette above
# # pass levels = levels(df$IGBP) so colors are stable & named
# scale_igbp_color <- function(levels, ...) {
#   vals <- setNames(poster_pal(length(levels)), levels)
#   ggplot2::scale_color_manual(values = vals, ...)
# }


poster_pal <- function(n) {
  colorspace::qualitative_hcl(
    n,
    palette = "Dynamic",
    c = 90,
    l = 60
  )
}

# Country colors: high-contrast palette using Polychrome
# (install once if needed: install.packages("Polychrome"))
library(Polychrome)

poster_pal_country <- function(n) {
  # fixed seed so colors are stable across runs
  set.seed(42)
  Polychrome::createPalette(
    n,
    seedcolors = c("#9DB9F1", "#FF0000", "#00FF00", "#0000FF")  # light blue + primaries
  )
}

scale_country_fill <- function(levels, ...) {
  cols <- poster_pal_country(length(levels))
  vals <- setNames(cols, levels)
  ggplot2::scale_fill_manual(values = vals, ...)
}

# Inward ticks + crisp border, print-friendly sizes
poster_theme <- function(base_size = 16) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      panel.border       = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.background   = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(color = "black"),
      axis.ticks         = ggplot2::element_line(color = "black"),
      # inward ticks via negative lengths (supported in ggplot2)
      axis.ticks.length  = grid::unit(-4, "pt"),
      axis.ticks.length.x = grid::unit(-4, "pt"),
      axis.ticks.length.y = grid::unit(-4, "pt"),
      legend.position    = "bottom"
    )
}

# Geom wrappers (keep or tweak to your taste)
poster_geom_point <- function(...) ggplot2::geom_point(size = poster_point_size, stroke = 0.7, alpha = 0.8, ...)
poster_geom_line  <- function(...) ggplot2::geom_line(linewidth = poster_linewidth, alpha = 0.9, ...)
