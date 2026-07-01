## figure_flux_comparison_combo.R
## Three-panel combination figure stacking the NEP, ET, and H FLUXNET2015-vs-
## Shuttle comparison plots vertically, for a single-column journal layout.
## Panel A = NEP, B = ET, C = H (top to bottom).
##
## Reads the same underlying comparison data as
## scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R — specifically its
## output table, data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv
## (one row per IGBP class x flux, with fluxnet2015/shuttle median, sd, n,
## and the excluded flag), rather than recomputing per-class statistics from
## the raw per-site CSVs a second time. This guarantees the combo panels show
## exactly the same numbers as the standalone per-flux figures.
##
## Each panel keeps the same aesthetics as the standalone plots (1:1 dashed
## line, +/-1 SD error bars both directions, IGBP-coloured points, ggrepel
## class labels, four-sided inward ticks, no gridlines, equal x/y range with
## 10% padding per panel) but drops the per-panel caption in favour of one
## shared caption below panel C, and adds a bold A/B/C panel tag inside the
## top-left of each panel via patchwork.
##
## Output:
##   review/figures/flux_medians/fig_flux_comparison_combo_nep_et_h.png
##   (300 dpi, white background, 3.5 in wide single-column)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
})

source("R/plot_constants.R")

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

# ---- Constants ----------------------------------------------------------------
CMP_CSV    <- "data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv"
OUT_FIG    <- "review/figures/flux_medians/fig_flux_comparison_combo_nep_et_h.png"
FIG_WIDTH  <- 3.5    # in, single-column (88 mm)
FIG_HEIGHT <- 9.5    # in, tuned for three roughly-square panels + caption

PANELS <- list(
  list(flux = "NEP", unit = "gC m⁻² yr⁻¹", tag = "A"),
  list(flux = "ET",  unit = "mm yr⁻¹",     tag = "B"),
  list(flux = "H",   unit = "W m⁻²",       tag = "C")
)

CAPTION <- paste0(
  "CVM excluded: absent from FLUXNET2015 release. CSH excluded: n=2 sites ",
  "in FLUXNET2015, below n≥5 threshold."
)

msg("=== FLUXNET2015 vs Shuttle: NEP/ET/H combo figure ===")

# ---- Load comparison table ------------------------------------------------------
msg("Loading: ", CMP_CSV)
cmp <- read_csv(CMP_CSV, show_col_types = FALSE)

# ---- Per-panel plot builder -----------------------------------------------------
combo_theme <- function() {
  fluxnet_theme(base_size = 8) +
    theme(
      legend.position    = "none",
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_rect(fill = "white", colour = NA),
      axis.title         = element_text(size = 7.5),
      axis.text          = element_text(size = 6.5)
    )
}

make_panel <- function(flux_code, unit_str, tag) {
  df <- cmp |> filter(flux == flux_code, !excluded)

  all_vals <- c(df$fluxnet2015_median - df$fluxnet2015_sd,
                df$fluxnet2015_median + df$fluxnet2015_sd,
                df$shuttle_median - df$shuttle_sd,
                df$shuttle_median + df$shuttle_sd,
                df$fluxnet2015_median, df$shuttle_median)
  all_vals <- all_vals[is.finite(all_vals)]
  rng  <- range(all_vals)
  pad  <- diff(rng) * 0.10
  lims <- c(rng[1] - pad, rng[2] + pad)

  ggplot(df, aes(x = fluxnet2015_median, y = shuttle_median)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey70", linewidth = 0.4) +
    geom_errorbar(aes(xmin = fluxnet2015_median - fluxnet2015_sd,
                       xmax = fluxnet2015_median + fluxnet2015_sd),
                   orientation = "y", width = 0, colour = "black", linewidth = 0.25) +
    geom_errorbar(aes(ymin = shuttle_median - shuttle_sd,
                       ymax = shuttle_median + shuttle_sd),
                   width = 0, colour = "black", linewidth = 0.25) +
    geom_point(aes(fill = igbp_class), shape = 21, size = 2, colour = "black",
               stroke = 0.3) +
    ggrepel::geom_text_repel(aes(label = igbp_class), size = 2.2, colour = "black",
              seed = 42, min.segment.length = 0.3, segment.size = 0.2,
              segment.colour = "grey50", box.padding = 0.3, point.padding = 0.2) +
    scale_fill_igbp() +
    scale_x_continuous(limits = lims, expand = expansion(mult = 0),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    scale_y_continuous(limits = lims, expand = expansion(mult = 0),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    # Panel tag anchored to this panel's own plot area (-Inf/Inf + hjust/vjust),
    # not patchwork's plot-level tag (which is positioned relative to the full
    # subplot including axis text, and collided with the y-axis tick labels).
    annotate("text", x = -Inf, y = Inf, label = tag, hjust = -0.5, vjust = 1.6,
             fontface = "bold", size = 3.2, colour = "black") +
    labs(
      x = paste0("FLUXNET2015 median ", flux_code, " ± SD (", unit_str, ")"),
      y = paste0("FLUXNET Shuttle median ", flux_code, " ± SD (", unit_str, ")")
    ) +
    combo_theme()
}

# ---- Build panels and stack ------------------------------------------------------
msg("Building panels: ", paste(vapply(PANELS, `[[`, "", "flux"), collapse = ", "))

panel_plots <- lapply(PANELS, function(p) {
  msg("  Panel ", p$tag, " (", p$flux, "): n=",
      sum(cmp$flux == p$flux & !cmp$excluded))
  make_panel(p$flux, p$unit, p$tag)
})

combo <- (panel_plots[[1]] / panel_plots[[2]] / panel_plots[[3]]) +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(
    caption = paste(strwrap(CAPTION, width = 60), collapse = "\n")
  ) &
  theme(
    plot.caption = element_text(size = 6, colour = "grey30", hjust = 0,
                                 face = "italic", margin = margin(t = 8))
  )

dir.create(dirname(OUT_FIG), showWarnings = FALSE, recursive = TRUE)
ggsave(OUT_FIG, combo, width = FIG_WIDTH, height = FIG_HEIGHT, dpi = 300, bg = "white")
msg("Saved: ", OUT_FIG, " (", FIG_WIDTH, " x ", FIG_HEIGHT, " in, 300 dpi)")

msg("\n=== Combo figure complete ===")
