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
source("R/nature_format.R")
source("R/presentation_format.R")

## Plotmath axis labels (bquote(), below) embed R string literals; with
## fancy quotes on (R's interactive-session default), grid's plotmath
## renderer deparses those literals using curly Unicode quotes (U+201C/
## U+201D), which the base PDF device's PostScript Helvetica can't render
## either -- same class of mbcsToSbcs conversion-failure warning as the
## superscript-minus issue the plotmath switch was meant to fix. Off for
## this whole script, not just the labels, since it's a global option.
options(useFancyQuotes = FALSE)

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

# ---- Constants ----------------------------------------------------------------
CMP_CSV    <- "data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv"
OUT_STEM   <- "review/figures/flux_medians/fig_flux_comparison_combo_nep_et_h"
FIG_WIDTH_MM  <- NATURE_WIDTH_SINGLE_MM   # 89mm, single-column
FIG_HEIGHT_MM <- 228                      # 3 roughly-square panels, no in-figure caption (moved to legend, 2026-10-02)

## Units as plotmath (not Unicode superscript-minus, e.g. "m⁻²"): base
## grDevices::pdf()'s PostScript Helvetica has no usable glyph for U+207B,
## confirmed by mbcsToSbcs conversion-failure warnings on the first Nature-
## format render (2026-10-02) -- same issue and fix as
## scripts/figure4_representativeness.R's own panel titles.
PANELS <- list(
  list(flux = "NEP", unit = quote("g C"~m^{-2}~yr^{-1}), tag = "a"),
  list(flux = "ET",  unit = quote(mm~yr^{-1}),        tag = "b"),
  list(flux = "H",   unit = quote(W~m^{-2}),          tag = "c")
)

## Exclusion note (task 2, 2026-10-02): moved out of the figure (no caption
## or explanatory text is drawn inside the figure itself) into
## fig_flux_comparison_combo_nep_et_h.legend.txt instead -- see that file's
## "CLASSIFICATION SCHEME AND EXCLUSIONS" section for the full statement
## this used to render as plot_annotation(caption = ...).

msg("=== FLUXNET2015 vs Shuttle: NEP/ET/H combo figure ===")

# ---- Load comparison table ------------------------------------------------------
msg("Loading: ", CMP_CSV)
cmp <- read_csv(CMP_CSV, show_col_types = FALSE)

# ---- Per-panel plot builder -----------------------------------------------------
## Builds the base theme directly from theme_classic() (2026-10-02) instead
## of R/plot_constants.R::fluxnet_theme() -- that function sets axis.title.x/
## .y to ggtext::element_markdown(), which ggplot2 cannot merge with a later
## plain element_text() override (`+` on two different element classes
## errors: "Only elements of the same class can be merged"), and which
## itself does not parse plotmath expression()/bquote() axis titles (treats
## them as literal deparsed text instead) -- both needed for this figure's
## superscript units. Reproduces fluxnet_theme()'s panel border/background/
## tick styling directly, without the ggtext dependency.
combo_theme <- function() {
  ggplot2::theme_classic(base_size = 8) +
    ggplot2::theme(
      panel.border        = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.8),
      panel.background    = ggplot2::element_blank(),
      axis.text           = ggplot2::element_text(colour = "black"),
      axis.ticks          = ggplot2::element_line(colour = "black"),
      axis.ticks.length   = grid::unit(-4, "pt"),
      legend.position     = "none"
    ) +
    nature_theme()   # Nature format, 2026-10-02: all text 7pt, Helvetica
}

## `two_line_titles` (presentation mode only): the rotated y-axis title's
## rendered length (since rotated 90 degrees) maps to vertical space on the
## page. This figure's 3-panel vertical stack is tall and narrow even in the
## manuscript (89 x 228mm) -- fitted into the 12.3x6.5in presentation box at
## that same aspect ratio, each panel's row is physically NARROWER than the
## manuscript's own 89mm page width, so a single-line axis title at
## presentation scale overlaps the adjacent panel's title. `atop()` renders
## the title as two stacked plotmath lines instead of one, roughly halving
## the per-line rendered length. Manuscript (default FALSE) path is
## byte-for-byte unchanged -- every other aesthetic (point size, error bars,
## repel tuning, panel letter) is identical between both paths; only the
## axis-title line count differs, and only in presentation mode.
make_panel <- function(flux_code, unit_str, tag, two_line_titles = FALSE) {
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

  p <- ggplot(df, aes(x = fluxnet2015_median, y = shuttle_median)) +
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
    scale_fill_paper_igbp() +
    scale_x_continuous(limits = lims, expand = expansion(mult = 0),
                        labels = nature_minus_labels(),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    scale_y_continuous(limits = lims, expand = expansion(mult = 0),
                        labels = nature_minus_labels(),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    labs(
      ## as.expression() is required -- labs() silently deparses a bare
      ## bquote() call object to a literal text string instead of rendering
      ## it as plotmath (confirmed by direct reproduction: without this, the
      ## axis showed the raw unparsed call text, e.g. literal quote marks
      ## and "^{-2}", not a superscript).
      x = if (two_line_titles) {
        as.expression(bquote(atop("FLUXNET2015 median" ~ .(flux_code), "± SD (" * .(unit_str) * ")")))
      } else {
        as.expression(bquote("FLUXNET2015 median" ~ .(flux_code) ~ "± SD (" * .(unit_str) * ")"))
      },
      y = if (two_line_titles) {
        as.expression(bquote(atop("FLUXNET Shuttle median" ~ .(flux_code), "± SD (" * .(unit_str) * ")")))
      } else {
        as.expression(bquote("FLUXNET Shuttle median" ~ .(flux_code) ~ "± SD (" * .(unit_str) * ")"))
      }
    ) +
    combo_theme()

  # Panel tag anchored to this panel's own plot area (-Inf/Inf + hjust/vjust),
  # not patchwork's plot-level tag (which is positioned relative to the full
  # subplot including axis text, and collided with the y-axis tick labels).
  # Lower-case, 8pt bold (Nature format, 2026-10-02 -- was uppercase 3.2mm
  # i.e. ~9.1pt, both non-compliant).
  p + panel_letter(tag, x = -Inf, y = Inf, hjust = -0.5, vjust = 1.6)
}

# ---- Build panels and stack ------------------------------------------------------
msg("Building panels: ", paste(vapply(PANELS, `[[`, "", "flux"), collapse = ", "))

panel_plots <- lapply(PANELS, function(p) {
  msg("  Panel ", p$tag, " (", p$flux, "): n=",
      sum(cmp$flux == p$flux & !cmp$excluded))
  make_panel(p$flux, p$unit, p$tag)
})

combo <- (panel_plots[[1]] / panel_plots[[2]] / panel_plots[[3]]) +
  plot_layout(heights = c(1, 1, 1))
# No plot_annotation(caption=...) (2026-10-02): the exclusion note moved out
# of the figure into fig_flux_comparison_combo_nep_et_h.legend.txt -- no
# caption or explanatory text is drawn inside the figure itself.

saved <- save_nature_figure(combo, OUT_STEM, width_mm = FIG_WIDTH_MM, height_mm = FIG_HEIGHT_MM)
msg("Saved: ", saved$png, " and ", saved$pdf, " (", FIG_WIDTH_MM, " x ", FIG_HEIGHT_MM, " mm, 600 dpi)")

# ---- Presentation mode (v2, 2026-10-13): one full-figure slide -----------------
## Talk-ready PNG of the complete NEP/ET/H combo, same layout/legend-free
## direct-label design/panel letters/colours as the manuscript figure, text/
## lines/points uniformly scaled up (R/presentation_format.R). Each panel is
## scaled individually, BEFORE composing, so the manuscript's own composition
## expression can be reused unchanged on the pre-scaled panels. Only runs
## when explicitly requested so the manuscript output above is unaffected by
## default.
presentation_mode <- identical(Sys.getenv("FLUXNET_PRESENTATION_MODE"), "1")
if (presentation_mode) {
  msg("--- Presentation mode: Figure 4 full-figure slide ---")
  PRES_SLIDE_DIR <- "review/figures/presentation_figures/slides"
  ## This figure's own aspect ratio (89 x 228mm, a tall 3-panel vertical
  ## stack) is far narrower than the 12.3x6.5in presentation box's own
  ## ratio. Fitting it at that exact aspect ratio via the box-fit helper
  ## (presentation_fit_dims(), height-capped at 6.5in) forces a canvas only
  ## 2.54in WIDE -- physically narrower than the manuscript's own print page
  ## width (89mm = 3.5in) -- while still asking for enlarged (2x) text. That
  ## combination (bigger text, smaller canvas) is self-contradictory and
  ## produced real overlapping/garbled axis titles when first tried (caught
  ## on visual QA, not merely "tight"). The fix used here: scale the CANVAS
  ## by the same factor as the text (so the slide is a uniform photographic
  ## enlargement of the manuscript page -- same relative proportions, same
  ## freedom from overlap the manuscript page already has), bypassing the
  ## 12.3x6.5in box for this one outlier figure rather than squeezing into
  ## it. The resulting slide is taller than the nominal box (height-only
  ## deviation, width stays a modest 7in) -- the same category of deviation
  ## already accepted for fig07_1 (that slide also exceeds 6.5in to keep
  ## text off the 12pt floor's wrong side intact rather than clip).
  ##
  ## FIG4_PRES_SCALE: the smallest multiplier that clears the 12pt floor on
  ## this figure's own smallest fixed element (the 2.2mm/6.26pt ggrepel
  ## IGBP-class label, smaller than the 7pt axis text): 12 / 6.26 = 1.92,
  ## rounded up to 2.0 (axis text/titles land at 14pt, panel letters 16pt).
  FIG4_PRES_SCALE <- 2.0
  pres_panels <- lapply(PANELS, function(p) {
    presentation_scale_ggplot(make_panel(p$flux, p$unit, p$tag, two_line_titles = TRUE), scale = FIG4_PRES_SCALE)
  })
  pres_combo <- (pres_panels[[1]] / pres_panels[[2]] / pres_panels[[3]]) +
    plot_layout(heights = c(1, 1, 1))
  pres_width_in  <- (FIG_WIDTH_MM  / 25.4) * FIG4_PRES_SCALE
  pres_height_in <- (FIG_HEIGHT_MM / 25.4) * FIG4_PRES_SCALE
  fs::dir_create(PRES_SLIDE_DIR)
  out <- file.path(PRES_SLIDE_DIR, "fig04_1_full.png")
  ggplot2::ggsave(out, pres_combo, width = pres_width_in, height = pres_height_in,
                   units = "in", dpi = PRESENTATION_DPI, device = ragg::agg_png, bg = "white")
  msg("  Saved presentation slide: ", out, " (", round(pres_width_in, 2), "x",
      round(pres_height_in, 2), "in, scale ", FIG4_PRES_SCALE, "x -- taller than the ",
      "12.3x6.5in box by design, see comment above)")
}

msg("\n=== Combo figure complete ===")
