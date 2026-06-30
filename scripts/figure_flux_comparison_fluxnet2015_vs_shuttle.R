## figure_flux_comparison_fluxnet2015_vs_shuttle.R
## Five scatter plots comparing FLUXNET2015 release IGBP-class median fluxes
## (x axis) to current FLUXNET Shuttle IGBP-class median fluxes (y axis), one
## per flux: NEP, GPP, TER, ET, H.
##
## Data sources:
##   data/snapshots/site_flux_medians_fluxnet2015.csv  (x axis, per-site medians)
##   data/snapshots/site_flux_medians_shuttle.csv       (y axis, per-site medians)
## Per-class statistics (median, sd, n) are computed directly from these two
## per-site files rather than read from igbp_class_flux_distributions_*.csv:
## those files store cv (sd/|mean|) but not a literal std_dev column, and the
## raw per-class mean needed to back one out isn't stored either. Recomputing
## std_dev = sd(per-site medians) directly from the same per-site source used
## by the distributions scripts keeps n_sites/median consistent with them
## while adding the std_dev this figure needs.
##
## Exclusions (all five plots, and the companion CSV's `excluded` flag):
##   CVM  - absent from the FLUXNET2015 release site list entirely (no x).
##   CSH  - n=2 in FLUXNET2015, below the n>=5 reliability threshold.
##   BSV/DNF/SNO (non-standard IGBP labels) - excluded by construction: class
##     statistics are computed only over the 12 STANDARD_IGBP labels, so sites
##     carrying these labels never contribute to any plotted class.
## Remaining 10 plotted classes: EBF, MF, DBF, ENF, OSH, WSA, SAV, GRA, WET, CRO.
##
## Outputs:
##   review/figures/flux_medians/fig_flux_comparison_{nep,gpp,ter,et,h}.png (300 dpi)
##   data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv + .meta.json
##   review/figures/methods_flux_medians.md (extended with a new section)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(jsonlite)
  library(ggrepel)
})

source("R/plot_constants.R")

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

# ---- Constants ----------------------------------------------------------------
F15_CSV     <- "data/snapshots/site_flux_medians_fluxnet2015.csv"
SH_CSV      <- "data/snapshots/site_flux_medians_shuttle.csv"
OUT_FIG_DIR <- "review/figures/flux_medians"
OUT_CSV     <- "data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv"
METHODS_MD  <- "review/figures/methods_flux_medians.md"

dir.create(OUT_FIG_DIR, showWarnings = FALSE, recursive = TRUE)

STANDARD_IGBP <- c("EBF","MF","DBF","ENF","CSH","OSH",
                    "WSA","SAV","GRA","WET","CRO","CVM")
EXCLUDED_CLASSES <- c("CVM","CSH")
PLOT_CLASSES <- setdiff(STANDARD_IGBP, EXCLUDED_CLASSES)

EXCLUDE_REASON <- c(
  CVM = "absent from FLUXNET2015 release site list (no x-coordinate)",
  CSH = "n=2 in FLUXNET2015, below the n>=5 reliability threshold"
)

FLUXES <- list(
  nep = list(col = "nep_median", title = "Net Ecosystem Production (NEP)",
             unit = "gC m⁻² yr⁻¹", file = "fig_flux_comparison_nep.png"),
  gpp = list(col = "gpp_median", title = "Gross Primary Productivity (GPP)",
             unit = "gC m⁻² yr⁻¹", file = "fig_flux_comparison_gpp.png"),
  ter = list(col = "ter_median", title = "Total Ecosystem Respiration (TER)",
             unit = "gC m⁻² yr⁻¹", file = "fig_flux_comparison_ter.png"),
  et  = list(col = "et_median",  title = "Evapotranspiration (ET)",
             unit = "mm yr⁻¹", file = "fig_flux_comparison_et.png"),
  h   = list(col = "h_median",   title = "Sensible heat flux (H)",
             unit = "W m⁻²", file = "fig_flux_comparison_h.png")
)

CAPTION <- paste0(
  "CVM excluded: absent from the FLUXNET2015 release site list. ",
  "CSH excluded: n=2 in FLUXNET2015, below the n≥ 5 reliability threshold. ",
  "Dashed line: 1:1 (x = y). Error bars: ± 1 SD (cross-site spread within class)."
)

msg("=== FLUXNET2015 vs Shuttle: IGBP-class flux comparison plots ===")

# ---- Load per-site medians -----------------------------------------------------
msg("Loading: ", F15_CSV)
f15 <- read_csv(F15_CSV, show_col_types = FALSE)
msg("Loading: ", SH_CSV)
sh  <- read_csv(SH_CSV, show_col_types = FALSE)

# ---- Per-class summary (median of site medians, sd, n) -------------------------
class_summary <- function(df, val_col) {
  df |>
    filter(igbp_class %in% STANDARD_IGBP) |>
    group_by(igbp_class) |>
    summarise(
      n_sites = sum(!is.na(.data[[val_col]])),
      median  = median(.data[[val_col]], na.rm = TRUE),
      sd      = sd(.data[[val_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      median = ifelse(is.nan(median), NA_real_, median),
      sd     = ifelse(n_sites < 2L, NA_real_, sd)
    )
}

# ---- Build combined comparison table (all 5 fluxes x 12 standard classes) ------
comparison_rows <- list()
plot_data_by_flux <- list()

for (flux_key in names(FLUXES)) {
  fd <- FLUXES[[flux_key]]

  f15_sum <- class_summary(f15, fd$col) |>
    rename(fluxnet2015_median = median, fluxnet2015_sd = sd, fluxnet2015_n_sites = n_sites)
  sh_sum  <- class_summary(sh, fd$col) |>
    rename(shuttle_median = median, shuttle_sd = sd, shuttle_n_sites = n_sites)

  cmp <- data.frame(igbp_class = STANDARD_IGBP, stringsAsFactors = FALSE) |>
    left_join(f15_sum, by = "igbp_class") |>
    left_join(sh_sum, by = "igbp_class") |>
    mutate(
      fluxnet2015_n_sites = ifelse(is.na(fluxnet2015_n_sites), 0L, fluxnet2015_n_sites),
      shuttle_n_sites      = ifelse(is.na(shuttle_n_sites), 0L, shuttle_n_sites),
      flux      = toupper(flux_key),
      diff      = shuttle_median - fluxnet2015_median,
      pct_diff  = 100 * diff / abs(fluxnet2015_median),
      excluded  = igbp_class %in% EXCLUDED_CLASSES,
      notes     = ifelse(excluded, EXCLUDE_REASON[igbp_class], "")
    ) |>
    select(igbp_class, flux, fluxnet2015_median, fluxnet2015_sd, fluxnet2015_n_sites,
           shuttle_median, shuttle_sd, shuttle_n_sites, diff, pct_diff, excluded, notes)

  comparison_rows[[flux_key]] <- cmp
  plot_data_by_flux[[flux_key]] <- cmp |> filter(igbp_class %in% PLOT_CLASSES)
}

comparison_table <- bind_rows(comparison_rows) |>
  mutate(
    flux = factor(flux, levels = toupper(names(FLUXES))),
    igbp_class = factor(igbp_class, levels = STANDARD_IGBP)
  ) |>
  arrange(flux, igbp_class) |>
  mutate(igbp_class = as.character(igbp_class), flux = as.character(flux)) |>
  mutate(across(c(fluxnet2015_median, fluxnet2015_sd, shuttle_median, shuttle_sd,
                   diff, pct_diff), ~ round(.x, 3)))

write_csv(comparison_table, OUT_CSV)
msg("Saved: ", OUT_CSV)

# ---- Companion meta.json --------------------------------------------------------
meta <- list(
  run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  pipeline_version = system("git rev-parse --short HEAD", intern = TRUE),
  data_sources = list(
    fluxnet2015 = F15_CSV,
    shuttle     = SH_CSV
  ),
  exclusion_rules = list(
    CVM = EXCLUDE_REASON[["CVM"]],
    CSH = EXCLUDE_REASON[["CSH"]],
    non_standard_igbp = paste0(
      "Sites labelled BSV, DNF, or SNO are excluded by construction: class ",
      "statistics are computed only over the 12 STANDARD_IGBP labels (",
      paste(STANDARD_IGBP, collapse = ", "), "); these sites never ",
      "contribute to any plotted class.")
  ),
  std_dev_definition = paste0(
    "sd of per-site median values within an IGBP class (cross-site spread), ",
    "NOT the within-site year-to-year spread and NOT read from a literal ",
    "std_dev column (igbp_class_flux_distributions_*.csv stores cv = ",
    "sd/|mean|, not sd itself, and does not store the class mean needed to ",
    "back sd out of cv). Recomputed directly here from the same per-site ",
    "source CSVs (site_flux_medians_fluxnet2015.csv / site_flux_medians_shuttle.csv) ",
    "those distribution files use, so n_sites and median are consistent ",
    "with them."),
  diff_definition = "diff = shuttle_median - fluxnet2015_median; pct_diff = 100 * diff / abs(fluxnet2015_median)",
  notes = paste0(
    "Companion table to fig_flux_comparison_{nep,gpp,ter,et,h}.png. Includes ",
    "all 12 STANDARD_IGBP classes (CVM and CSH flagged excluded=TRUE, not ",
    "dropped) for transparency; only the 10 non-excluded classes appear in ",
    "the figures.")
)
jsonlite::write_json(meta, paste0(OUT_CSV, ".meta.json"), pretty = TRUE, auto_unbox = TRUE)
msg("Saved: ", paste0(OUT_CSV, ".meta.json"))

# ---- Plotting helper -------------------------------------------------------------
comparison_theme <- function() {
  fluxnet_theme(base_size = 11) +
    theme(
      legend.position  = "none",
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.caption     = element_text(size = 7, colour = "grey30", hjust = 0,
                                       margin = margin(t = 8), lineheight = 1.2),
      axis.title       = element_text(size = 10)
    )
}

make_comparison_plot <- function(df, flux_code, unit_str, out_file) {
  # Joint range across x, y, and their error-bar extents, with small padding.
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
                   orientation = "y", width = 0, colour = "black", linewidth = 0.3) +
    geom_errorbar(aes(ymin = shuttle_median - shuttle_sd,
                       ymax = shuttle_median + shuttle_sd),
                   width = 0, colour = "black", linewidth = 0.3) +
    geom_point(aes(fill = igbp_class), shape = 21, size = 3, colour = "black",
               stroke = 0.4) +
    ggrepel::geom_text_repel(aes(label = igbp_class), size = 3.0, colour = "black",
              seed = 42, min.segment.length = 0.3, segment.size = 0.25,
              segment.colour = "grey50", box.padding = 0.4, point.padding = 0.3) +
    scale_fill_igbp() +
    scale_x_continuous(limits = lims, expand = expansion(mult = 0),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    scale_y_continuous(limits = lims, expand = expansion(mult = 0),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    labs(
      x       = paste0("FLUXNET2015 median ", flux_code, " ± SD (", unit_str, ")"),
      y       = paste0("FLUXNET Shuttle median ", flux_code, " ± SD (", unit_str, ")"),
      caption = paste(strwrap(CAPTION, width = 100), collapse = "\n")
    ) +
    comparison_theme()

  out_path <- file.path(OUT_FIG_DIR, out_file)
  ggsave(out_path, p, width = 7, height = 5, dpi = 300, bg = "white")
  msg("  Figure: ", out_path)
  invisible(p)
}

# ---- Build all five figures, and collect summary stats for SESSION_LOG ---------
msg("\n=== Building comparison plots ===")
flux_summaries <- list()

for (flux_key in names(FLUXES)) {
  fd <- FLUXES[[flux_key]]
  df <- plot_data_by_flux[[flux_key]]

  msg("\n--- ", toupper(flux_key), " ---")
  msg("  Points plotted: ", nrow(df), " classes")
  msg(sprintf("  X range (FLUXNET2015): %.2f to %.2f",
              min(df$fluxnet2015_median, na.rm = TRUE),
              max(df$fluxnet2015_median, na.rm = TRUE)))
  msg(sprintf("  Y range (Shuttle):     %.2f to %.2f",
              min(df$shuttle_median, na.rm = TRUE),
              max(df$shuttle_median, na.rm = TRUE)))

  df_ranked <- df |> filter(!is.na(diff)) |> arrange(desc(abs(diff)))
  top3 <- head(df_ranked, 3L)
  for (i in seq_len(nrow(top3))) {
    direction <- if (top3$diff[i] > 0) "higher" else "lower"
    msg(sprintf("  Furthest from 1:1: %s (diff=%+.2f, %+.1f%%, Shuttle %s)",
                top3$igbp_class[i], top3$diff[i], top3$pct_diff[i], direction))
  }

  flux_summaries[[flux_key]] <- list(
    n_points = nrow(df),
    x_range  = range(df$fluxnet2015_median, na.rm = TRUE),
    y_range  = range(df$shuttle_median, na.rm = TRUE),
    top_shift = if (nrow(df_ranked) > 0L) df_ranked[1, ] else NULL
  )

  make_comparison_plot(df, toupper(flux_key), fd$unit, fd$file)
}

# ---- Extend methods document ----------------------------------------------------
msg("\n=== Updating methods document ===")

methods_addendum <- paste0(
"\n---\n\n",
"## Comparison plots: FLUXNET2015 release vs current Shuttle\n\n",
"*Added 2026-06-30. Source script: ",
"`scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R`.*\n\n",
"Five scatter plots (`review/figures/flux_medians/fig_flux_comparison_",
"{nep,gpp,ter,et,h}.png`) compare IGBP-class median fluxes between the ",
"original FLUXNET2015 release and the current FLUXNET Shuttle ",
"reprocessing, one point per IGBP class.\n\n",
"### Data source per axis\n\n",
"- **X axis:** `data/snapshots/site_flux_medians_fluxnet2015.csv` — ",
"per-site medians computed from the FLUXNET2015 release (Pastorello et ",
"al. 2020) FULLSET YY product, 206 of 212 sites (see SESSION_LOG.md ",
"2026-06-30, \"FLUXNET2015 release: extraction and IGBP-class flux ",
"assessment\").\n",
"- **Y axis:** `data/snapshots/site_flux_medians_shuttle.csv` — per-site ",
"medians computed from the current FLUXNET Shuttle YY product (FLUXMET ",
"YY v1.3_r1), 767 sites.\n",
"- Both axes use identical per-site aggregation logic (VUT preferred / ",
"CUT fallback; NT preferred / DT fallback per site; QC ≥ 0.80; median ",
"across qualifying years per site), so the comparison isolates ",
"differences in the underlying data and processing rather than ",
"differences in aggregation method.\n\n",
"### Excluded classes\n\n",
"- **CVM** — absent from the FLUXNET2015 release site list entirely (0 ",
"sites); the 9 CVM sites in the current Shuttle network were added or ",
"reclassified after the 2015/2020 release, so there is no FLUXNET2015 ",
"x-coordinate to plot.\n",
"- **CSH** — n=2 in FLUXNET2015, below the n≥ 5 reliability threshold ",
"used elsewhere in this methods document (see \"IGBP class scheme\" ",
"above); plotted positions for n=2 classes are not considered reliable ",
"enough for a network-comparison figure.\n",
"- **BSV, DNF, SNO** (non-standard IGBP labels) — excluded by ",
"construction: class statistics are computed only over the 12 standard ",
"IGBP labels, so sites carrying these labels never contribute to any ",
"plotted class.\n",
"- Remaining 10 plotted classes: EBF, MF, DBF, ENF, OSH, WSA, SAV, GRA, ",
"WET, CRO. All exclusions and their rationale are also stated in each ",
"figure's caption.\n\n",
"### Std-dev framing\n\n",
"Error bars are ± 1 SD of the **cross-site spread of per-site median ",
"values within each IGBP class** — i.e. how much sites within a class ",
"disagree with each other — not the within-site year-to-year variability, ",
"and not a measurement uncertainty. A class with a wide error bar has ",
"ecologically or methodologically heterogeneous sites; a class with a ",
"narrow error bar has consistent sites. `igbp_class_flux_distributions_",
"*.csv` stores `cv` (= sd/|mean|) rather than a literal `std_dev` column, ",
"and does not store the class mean needed to back sd out of cv, so this ",
"std_dev was recomputed directly from the per-site median CSVs that those ",
"distribution files are themselves built from (same n_sites and median, ",
"by construction).\n\n",
"### 1:1 line interpretation\n\n",
"The dashed diagonal is x = y (FLUXNET2015 median = Shuttle median). A ",
"class plotted **above** the line has a higher median flux in the current ",
"Shuttle reprocessing than in the FLUXNET2015 release; a class **below** ",
"the line has a lower median flux in the current reprocessing. Distance ",
"from the line reflects the combined effect of (a) genuine network ",
"evolution (different sites, different site-years, longer records) and ",
"(b) ONEFlux processing-pipeline differences between the 2020 FLUXNET2015 ",
"release and the current Shuttle processing — this figure cannot, by ",
"itself, separate those two effects.\n\n",
"### Processing-version confound\n\n",
"The FLUXNET2015 release and the current Shuttle reprocessing are built ",
"from different ONEFlux pipeline versions in addition to different site/",
"year coverage (see CLAUDE.md hard rule #1 and SESSION_LOG.md 2026-06-30, ",
"\"FLUXNET2015 release YY data inventory\" — these are explicitly distinct ",
"data products, never to be conflated as repeated measurements of the ",
"same underlying processing). A finding here (\"current Shuttle GPP is X% ",
"lower for class Y\") should be read as the combined effect of network ",
"evolution and processing changes, not attributed to either cause alone ",
"without further investigation (e.g. re-running ONEFlux at the current ",
"version on the FLUXNET2015-era site-years, which this figure does not ",
"do).\n\n",
"### GRA and EBF: large-n, not small-sample noise\n\n",
"GRA and EBF are the two largest-n classes in both networks (GRA: ",
"n=34 FLUXNET2015 / n=120 Shuttle for NEP; EBF: n=14 / n=38) and both ",
"show consistent, substantial shifts across multiple fluxes (GRA: GPP/TER ",
"roughly 37-44% lower in FLUXNET2015 than current Shuttle; EBF: TER ~27% ",
"lower, ET ~11% lower in FLUXNET2015). Because these are the best-sampled ",
"classes in the comparison, these shifts are unlikely to be small-sample ",
"artifacts in the way the CSH/SAV shifts plausibly are (n=2-8), and are ",
"flagged here as the shifts most worth a deeper methodological look.\n\n",
"### Citation requirements\n\n",
"Any use of this comparison (including the figures and companion table) ",
"must cite Pastorello et al. 2020 (doi:10.1038/s41597-020-0534-3) for the ",
"FLUXNET2015 release data, per the CC-BY-4.0 data policy this download ",
"was scoped to (SESSION_LOG.md 2026-06-30, \"FLUXNET2015 release portal ",
"investigation and download launch\"). Per-site tower DOIs for citation ",
"in the eventual paper are recorded in the PI-contacts log written during ",
"download (`logs/fluxnet2015_pi_contacts_20260630_130019.csv` and the two ",
"earlier smoke-test logs), and individual site DOIs are available from ",
"the FLUXNET2015 download manifest for each site.\n"
)

if (file.exists(METHODS_MD)) {
  existing <- readChar(METHODS_MD, file.info(METHODS_MD)$size)
  if (!grepl("Comparison plots: FLUXNET2015 release vs current Shuttle", existing, fixed = TRUE)) {
    cat(existing, methods_addendum, sep = "", file = METHODS_MD)
    msg("Extended: ", METHODS_MD)
  } else {
    msg("  Methods doc already contains the comparison-plots section; not duplicating.")
  }
} else {
  msg("  WARNING: ", METHODS_MD, " not found; comparison-plots section not written.")
}

msg("\n=== All five comparison figures and companion table complete ===")
