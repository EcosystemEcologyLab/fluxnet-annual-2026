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
## group (Figures 2, 3, 4; Okabe-Ito colourblind-safe set). ----
GSL_GROUP_ORDER <- c("grassland", "evergreen forest", "deciduous forest",
                      "woody-grass mosaic", "mixed forest")
GSL_GROUP_COLORS <- c(
  "grassland"           = "#E69F00",
  "evergreen forest"    = "#009E73",
  "deciduous forest"    = "#56B4E9",
  "woody-grass mosaic"  = "#D55E00",
  "mixed forest"        = "#CC79A7"
)
scale_color_gsl_group <- function(...) {
  scale_color_manual(values = GSL_GROUP_COLORS, limits = GSL_GROUP_ORDER, ...)
}
scale_fill_gsl_group <- function(...) {
  scale_fill_manual(values = GSL_GROUP_COLORS, limits = GSL_GROUP_ORDER, ...)
}

## ---- One colourblind-safe skewness colour scale, used in every figure
## that colours by skewness (Figures 1, 2, 4; viridis, perceptually uniform
## and colourblind-safe by design). Fixed limits across all figures so the
## same skewness value always maps to the same colour everywhere. ----
GSL_SKEW_LIMITS <- c(0, 4) # covers the observed 0.1932-3.8757 (main run) with headroom
scale_color_gsl_skew <- function(...) {
  scale_color_viridis_c(limits = GSL_SKEW_LIMITS, name = "Skewness", ...)
}
scale_fill_gsl_skew <- function(...) {
  scale_fill_viridis_c(limits = GSL_SKEW_LIMITS, name = "Skewness", ...)
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
