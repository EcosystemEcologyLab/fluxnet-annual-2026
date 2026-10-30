## fig_helpers.R — shared helpers for the GSL figure pack (Stage 3).
## Sourced by every 0X_fig*.R / 09_benchmark.R / 10_supplementary.R script
## (which have already sourced 00_config.R). Not run directly.
##
## Follows R/nature_format.R conventions (89/183 mm widths, 5-7 pt text,
## <=1 pt lines, PNG+PDF via save_nature_figure()) -- sourced here, not
## reimplemented, since save_nature_figure() has no side effect outside the
## path_stem the caller gives it (unlike write_output_metadata(), which
## always appends to the repo-root outputs/session_info.txt -- see
## 00_config.R's local write_meta() for why that one IS reimplemented).

source("R/nature_format.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

GSL_FIG_DIR <- file.path(GSL_ROOT, "figures")
if (!dir.exists(GSL_FIG_DIR)) dir.create(GSL_FIG_DIR, recursive = TRUE)

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

## ---- One colourblind-safe skewness colour scale, used in every figure
## that colours by skewness (Figures 1, 2, 4; viridis, perceptually uniform
## and colourblind-safe by design). Capped at 2 (most sites sit in 0.2-1.5;
## the few sites above 2 would otherwise compress that whole range into a
## narrow sliver of the palette) -- values above the cap are squished to the
## top colour, not greyed out as NA, and the top break is labelled ">2" so
## the cap is visible to the reader. Fixed limits/breaks across every figure
## so the same skewness value always maps to the same colour everywhere. ----
GSL_SKEW_CAP <- 2
GSL_SKEW_LIMITS <- c(0, GSL_SKEW_CAP)
GSL_SKEW_BREAKS <- c(0, 0.5, 1, 1.5, GSL_SKEW_CAP)
GSL_SKEW_LABELS <- c("0", "0.5", "1", "1.5", paste0(">", GSL_SKEW_CAP))
scale_color_gsl_skew <- function(...) {
  scale_color_viridis_c(limits = GSL_SKEW_LIMITS, breaks = GSL_SKEW_BREAKS,
                         labels = GSL_SKEW_LABELS, oob = scales::squish, name = "Skewness", ...)
}
scale_fill_gsl_skew <- function(...) {
  scale_fill_viridis_c(limits = GSL_SKEW_LIMITS, breaks = GSL_SKEW_BREAKS,
                        labels = GSL_SKEW_LABELS, oob = scales::squish, name = "Skewness", ...)
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

## ---- Theme: Nature conventions, GSL defaults ----
theme_gsl <- function(base_size = NATURE_BASE_PT) {
  nature_theme(base_size = base_size)
}

#' Save a GSL figure (PNG + PDF via save_nature_figure()) and write its
#' .legend.txt alongside it, plus a .meta.json via the local write_meta()
#' (NOT R/utils.R::write_output_metadata() -- see 00_config.R).
#'
#' @param plot ggplot/patchwork object.
#' @param stem Output path stem, e.g. "fig_01_mechanism" (no directory, no
#'   extension) -- written under GSL_FIG_DIR.
#' @param width_mm,height_mm Passed to save_nature_figure() (89 or 183 mm).
#' @param legend_lines Character vector, one .legend.txt line per element.
#' @param input_sources,notes Passed to write_meta().
save_gsl_figure <- function(plot, stem, width_mm, height_mm, legend_lines,
                             input_sources, notes = "") {
  path_stem <- file.path(GSL_FIG_DIR, stem)
  out <- save_nature_figure(plot, path_stem, width_mm = width_mm, height_mm = height_mm)
  writeLines(legend_lines, paste0(path_stem, ".legend.txt"))
  write_meta(paste0(path_stem, ".png"), input_sources = input_sources, notes = notes)
  message("Saved: ", path_stem, ".png/.pdf/.legend.txt/.meta.json")
  invisible(out)
}
