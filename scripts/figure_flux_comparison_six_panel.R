## figure_flux_comparison_six_panel.R
## Extended Data figure: six-panel FLUXNET2015-vs-Shuttle comparison, rows
## NEP/ET/H, left column the primary Figure 3 panels (all qualifying years,
## independently per dataset), right column the matched-site-years panels
## from task 5 (same site AND same calendar year required on both axes).
## Reads both already-computed comparison tables directly -- does not
## recompute either; this figure is a side-by-side re-plot, not a new
## analysis.
##
## Letters a-f across rows: a NEP/all, b NEP/matched, c ET/all, d ET/matched,
## e H/all, f H/matched. Axis limits are identical within a row (shared
## between the two columns), unlike the standalone Figure 3 and matched-
## site-years figures, where each panel's limits are independently padded.
## The classes plotted can differ between columns (matching n>=5 differs
## independently per table) -- stated in the legend, not inferred from the
## panels.
##
## Output: review/figures/draft_manuscript_v1/SupFigs/
##   supp_flux_comparison_six_panel.png/.pdf/.jpg + .legend.txt

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/nature_format.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(ggrepel); library(patchwork)
})
options(useFancyQuotes = FALSE)

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Extended Data: six-panel FLUXNET2015 vs Shuttle comparison ===")

ALL_CSV     <- "data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv"
MATCHED_CSV <- "data/snapshots/flux_comparison_fluxnet2015_vs_shuttle_common_siteyears.csv"
OUT_DIR  <- file.path("review", "figures", "draft_manuscript_v1", "SupFigs")
OUT_STEM <- file.path(OUT_DIR, "supp_flux_comparison_six_panel")
fs::dir_create(OUT_DIR)

for (f in c(ALL_CSV, MATCHED_CSV)) {
  if (!file.exists(f)) {
    stop("Required input not found: ", f, " -- run scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R ",
         "and/or scripts/figure_flux_comparison_combo_alt_common_siteyears.R first.", call. = FALSE)
  }
}
all_tbl     <- read_csv(ALL_CSV, show_col_types = FALSE) |> mutate(source = "all")
matched_tbl <- read_csv(MATCHED_CSV, show_col_types = FALSE) |> mutate(source = "matched")
msg("Loaded: ", ALL_CSV, " (", nrow(all_tbl), " rows), ", MATCHED_CSV, " (", nrow(matched_tbl), " rows)")

FLUXES <- c("NEP", "ET", "H")
UNITS  <- list(NEP = quote(gC~m^{-2}~yr^{-1}), ET = quote(mm~yr^{-1}), H = quote(W~m^{-2}))
LETTERS6 <- matrix(c("a","b","c","d","e","f"), nrow = 3, ncol = 2, byrow = TRUE,
                    dimnames = list(FLUXES, c("all", "matched")))

combo_theme <- function() {
  ggplot2::theme_classic(base_size = 7) +
    ggplot2::theme(
      panel.border       = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.8),
      panel.background   = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(colour = "black"),
      axis.ticks         = ggplot2::element_line(colour = "black"),
      axis.ticks.length  = grid::unit(-4, "pt"),
      legend.position    = "none"
    ) +
    nature_theme()
}

make_panel <- function(df, flux_code, unit_expr, tag, lims, column_title = NULL) {
  ggplot(df, aes(x = fluxnet2015_median, y = shuttle_median)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
    geom_errorbar(aes(xmin = fluxnet2015_median - fluxnet2015_sd, xmax = fluxnet2015_median + fluxnet2015_sd),
                  orientation = "y", width = 0, colour = "black", linewidth = 0.25) +
    geom_errorbar(aes(ymin = shuttle_median - shuttle_sd, ymax = shuttle_median + shuttle_sd),
                  width = 0, colour = "black", linewidth = 0.25) +
    geom_point(aes(fill = igbp_class), shape = 21, size = 1.6, colour = "black", stroke = 0.3) +
    ggrepel::geom_text_repel(aes(label = igbp_class), size = 1.9, colour = "black", seed = 42,
                              min.segment.length = 0.3, segment.size = 0.2, segment.colour = "grey50",
                              box.padding = 0.25, point.padding = 0.15) +
    scale_fill_igbp() +
    scale_x_continuous(limits = lims, expand = expansion(mult = 0),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    scale_y_continuous(limits = lims, expand = expansion(mult = 0),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    panel_letter(tag, x = -Inf, y = Inf, hjust = -0.5, vjust = 1.6) +
    labs(
      x = as.expression(bquote("FLUXNET2015 median" ~ .(flux_code) ~ "(" * .(unit_expr) * ")")),
      y = as.expression(bquote("Shuttle median" ~ .(flux_code) ~ "(" * .(unit_expr) * ")")),
      title = column_title
    ) +
    combo_theme() +
    (if (!is.null(column_title)) {
      ggplot2::theme(plot.title = ggplot2::element_text(size = NATURE_BASE_PT, hjust = 0.5, face = "plain"))
    } else NULL)
}

panels <- list()
n_report <- list()
for (fx in FLUXES) {
  df_all     <- all_tbl     |> filter(flux == fx, !excluded)
  df_matched <- matched_tbl |> filter(flux == fx, !excluded)

  all_vals <- c(df_all$fluxnet2015_median - df_all$fluxnet2015_sd, df_all$fluxnet2015_median + df_all$fluxnet2015_sd,
                df_all$shuttle_median - df_all$shuttle_sd, df_all$shuttle_median + df_all$shuttle_sd,
                df_matched$fluxnet2015_median - df_matched$fluxnet2015_sd, df_matched$fluxnet2015_median + df_matched$fluxnet2015_sd,
                df_matched$shuttle_median - df_matched$shuttle_sd, df_matched$shuttle_median + df_matched$shuttle_sd)
  all_vals <- all_vals[is.finite(all_vals)]
  rng  <- range(all_vals)
  pad  <- diff(rng) * 0.10
  lims <- c(rng[1] - pad, rng[2] + pad)
  msg(fx, ": shared row limits = [", round(lims[1], 1), ", ", round(lims[2], 1), "], classes: all=",
      nrow(df_all), " (", paste(sort(df_all$igbp_class), collapse = ","), "), matched=", nrow(df_matched),
      " (", paste(sort(df_matched$igbp_class), collapse = ","), ")")

  col_title_all     <- if (fx == "NEP") "All qualifying site-years (independent per dataset)" else NULL
  col_title_matched <- if (fx == "NEP") "Matched site-years (same site and year, both datasets)" else NULL

  panels[[paste0(fx, "_all")]]     <- make_panel(df_all, fx, UNITS[[fx]], LETTERS6[fx, "all"], lims, col_title_all)
  panels[[paste0(fx, "_matched")]] <- make_panel(df_matched, fx, UNITS[[fx]], LETTERS6[fx, "matched"], lims, col_title_matched)
  n_report[[fx]] <- list(all = nrow(df_all), matched = nrow(df_matched),
                          classes_all = sort(df_all$igbp_class), classes_matched = sort(df_matched$igbp_class))
}

combo <- (panels$NEP_all | panels$NEP_matched) /
  (panels$ET_all | panels$ET_matched) /
  (panels$H_all | panels$H_matched)

saved <- save_nature_figure(combo, OUT_STEM, width_mm = NATURE_ED_MAX_WIDTH_MM, height_mm = 220,
                             extended_data = TRUE)
msg("Saved: ", saved$png, ", ", saved$pdf, ", ", saved$jpeg)

# ---- Legend --------------------------------------------------------------------
class_line <- function(fx) {
  nr <- n_report[[fx]]
  paste0("  ", fx, ": left (all) n=", nr$all, " classes {", paste(nr$classes_all, collapse = ", "),
         "}; right (matched) n=", nr$matched, " classes {", paste(nr$classes_matched, collapse = ", "), "}")
}
legend_lines <- c(
  "FIGURE LEGEND — supp_flux_comparison_six_panel.png",
  strrep("=", 60), "",
  "TITLE: Extended Data Figure — FLUXNET2015 vs. Shuttle per-IGBP-class median flux",
  "comparison, all qualifying site-years vs. matched site-years, side by side", "",
  "DESCRIPTION:",
  "Six panels, 3 rows (NEP, ET, H) x 2 columns, re-plotting the two comparison tables already",
  "built by scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R (left column, \"all",
  "qualifying site-years\": each dataset's per-class median computed independently, over",
  "whatever site-years qualify in that dataset alone -- the data behind Figure 3) and",
  "scripts/figure_flux_comparison_combo_alt_common_siteyears.R (right column, \"matched",
  "site-years\": each site's median on both axes computed only over the calendar years where",
  "BOTH datasets have a qualifying value -- the data behind the matched-site-years Extended",
  "Data figure). This figure recomputes nothing; it reads both already-computed comparison",
  "tables and re-plots them with shared axis limits for direct visual comparison. No plot",
  "title is drawn except the two column headers (above row 1 only, identifying which column",
  "is which method); no other caption or explanatory text is drawn inside the figure.",
  "",
  "PANELS AND AXES:",
  "  Rows: a/b = NEP, c/d = ET, e/f = H (bold lower-case letters, 8pt, top-left of each panel)",
  "  Columns: left (a, c, e) = all qualifying site-years; right (b, d, f) = matched site-years",
  "  Within each row, both panels share IDENTICAL x and y axis limits (equal x/y, 10% padding",
  "  on the combined range of both columns' values for that flux) -- unlike the standalone",
  "  Figure 3 and matched-site-years figures, where each panel's limits are computed",
  "  independently. Axis titles use plotmath, not a Unicode superscript-minus character.",
  "",
  "CLASSES PLOTTED MAY DIFFER BETWEEN COLUMNS (the n>=5 reliability threshold is applied",
  "independently to each table, and matching further removes some sites/classes):",
  class_line("NEP"), class_line("ET"), class_line("H"),
  "",
  "COLOUR CODING: scale_fill_igbp() (R/plot_constants.R), one point per IGBP class per panel.",
  "",
  "DATA SOURCES (recomputes nothing; both tables built elsewhere):",
  paste0("  - Left column:  ", ALL_CSV),
  paste0("  - Right column: ", MATCHED_CSV),
  "  See scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R and",
  "  scripts/figure_flux_comparison_combo_alt_common_siteyears.R (and their own legends/",
  "  methods notes) for how each table's QC gate, VUT/CUT source, and site medians are",
  "  computed -- both now QC_THRESHOLD_YY via R/site_annual_fluxes.R::",
  "  compute_site_annual_fluxes()/compute_site_annual_fluxes_from_df() (2026-10-02).",
  "",
  "REPRODUCIBILITY:",
  "Script: scripts/figure_flux_comparison_six_panel.R",
  paste0("DIMENSIONS: ", NATURE_ED_MAX_WIDTH_MM, " x 220 mm, 600 dpi PNG + vector PDF + 300 ppi JPEG,"),
  "Helvetica, white background. All text 5-7pt except the bold lower-case panel letters (8pt)."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))

msg("\n=== Done ===")
