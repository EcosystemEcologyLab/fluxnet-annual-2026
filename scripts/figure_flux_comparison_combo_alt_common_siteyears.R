## figure_flux_comparison_combo_alt_common_siteyears.R
## Extended Data figure: FLUXNET2015-vs-Shuttle NEP/ET/H comparison restricted
## to the SAME sites *and* the same qualifying calendar years on both axes,
## matched independently per flux -- isolates ONEFlux processing-version
## differences from network-composition/coverage differences (see
## review/figures/methods_flux_medians.md, "Processing-version confound").
##
## Revised 2026-10-02 (task 5): rebuilt on the shared functions instead of its
## own loose-file read + hardcoded QC_THRESH=0.80 -- Shuttle via
## R/site_annual_fluxes.R::compute_site_annual_fluxes() (DuckDB `annual`
## table), FLUXNET2015 via compute_site_annual_fluxes_from_df() (this
## project's own already-extracted FLUXNET2015 YY CSVs,
## data/fluxnet2015_comparison/), both gated on QC_THRESHOLD_YY
## (R/pipeline_config.R) and both with h_unit = "W_m2" (H plotted in its
## native W m-2 mean rate, matching the primary combo figure). Matching rule
## unchanged: per flux, a site-year counts only if BOTH datasets have a
## QC_THRESHOLD_YY-qualifying value for that site and calendar year; site
## median computed only over the matched years, on both axes; class
## statistics and the n >= 5 reliability threshold as in the primary figure
## (fig_flux_comparison_combo_nep_et_h.png).
##
## Now a Supplementary Figure (Nature-style format: 89mm wide, PDF + 300ppi
## JPEG alongside the PNG, lower-case panel letters, no in-figure caption)
## rather than a plain candidate PNG -- see docs/known_issues.md Sec 10 and
## SESSION_LOG.md 2026-10-02. Renumbered Supplementary Figure S2 under figure
## stage 6 (2026-10-02; target journal Scientific Data has no Extended Data
## concept) -- this script's own name is unchanged, see docs/figure_inventory.md.
##
## Outputs:
##   data/snapshots/flux_comparison_fluxnet2015_vs_shuttle_common_siteyears.csv
##     + .meta.json
##   review/figures/draft_manuscript_v1/SupFigs/figS2_flux_comparison_matched_siteyears.png/.pdf/.jpg
##     + .legend.txt (renumbered from supp_flux_comparison_matched_siteyears.*)

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/units.R")
source("R/site_annual_fluxes.R")
source("R/nature_format.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(jsonlite)
  library(duckdb)
  library(DBI)
})
options(useFancyQuotes = FALSE)

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

# ---- Constants ----------------------------------------------------------------
NA_FLAG     <- -9999
F15_DIR     <- "data/fluxnet2015_comparison"   # already extracted by assess_flux_data_by_igbp_fluxnet2015.R
DB_PATH     <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
SHUTTLE_MEDIANS_CSV <- "data/snapshots/site_flux_medians_shuttle.csv"
OUT_CSV     <- "data/snapshots/flux_comparison_fluxnet2015_vs_shuttle_common_siteyears.csv"
OUT_DIR     <- file.path("review", "figures", "draft_manuscript_v1", "SupFigs")
OUT_STEM    <- file.path(OUT_DIR, "figS2_flux_comparison_matched_siteyears")
fs::dir_create(OUT_DIR)

STANDARD_IGBP    <- c("EBF","MF","DBF","ENF","CSH","OSH",
                       "WSA","SAV","GRA","WET","CRO","CVM")
RELIABILITY_MIN  <- 5L   # same n>=5 reliability threshold as the primary figure

FLUXES <- c(nep = "NEP", et = "ET", h = "H")
UNITS  <- list(nep = quote("g C"~m^{-2}~yr^{-1}), et = quote(mm~yr^{-1}), h = quote(W~m^{-2}))

msg("=== Extended Data: FLUXNET2015 vs Shuttle, matched site-years ===")

# ---- Step 0: Shuttle site-years from the shared function (DuckDB) -------------
msg("Querying DuckDB `annual` table for Shuttle site-years ...")
con <- dbConnect(duckdb(), DB_PATH, read_only = TRUE)
sh_fluxes <- compute_site_annual_fluxes(con, site_ids = NULL, h_unit = "W_m2")
dbDisconnect(con, shutdown = TRUE)
sh_year <- sh_fluxes$site_year |> transmute(site_id, year, NEP = -NEE, ET, H)
msg("  Shuttle: ", nrow(sh_year), " site-year rows, ", dplyr::n_distinct(sh_year$site_id), " sites")

# ---- Step 1: FLUXNET2015 site-years from the shared function (own extracted files) ----
msg("Reading already-extracted FLUXNET2015 YY files under ", F15_DIR)
f15_files <- list.files(F15_DIR, pattern = "_FLUXNET2015_FULLSET_YY_.*\\.csv$",
                        recursive = TRUE, full.names = TRUE)
f15_lookup <- data.frame(
  site_id = basename(dirname(f15_files)), path = f15_files, stringsAsFactors = FALSE
) |> distinct(site_id, .keep_all = TRUE)
msg("  FLUXNET2015 sites with an extracted YY file: ", nrow(f15_lookup))

needed_cols <- c("NEE_VUT_REF","NEE_VUT_REF_QC","NEE_CUT_REF","NEE_CUT_REF_QC",
                  "GPP_NT_VUT_REF","GPP_NT_CUT_REF","GPP_DT_VUT_REF","GPP_DT_CUT_REF",
                  "RECO_NT_VUT_REF","RECO_NT_CUT_REF","RECO_DT_VUT_REF","RECO_DT_CUT_REF",
                  "LE_F_MDS","LE_F_MDS_QC","H_F_MDS","H_F_MDS_QC")
na_to_na <- function(x) ifelse(is.na(x) | x == NA_FLAG, NA_real_, as.numeric(x))

read_one_yy <- function(path, site_id) {
  yy <- tryCatch(read_csv(path, show_col_types = FALSE, na = as.character(NA_FLAG)),
                 error = function(e) NULL)
  if (is.null(yy) || nrow(yy) == 0L || !"TIMESTAMP" %in% names(yy)) return(NULL)
  missing_cols <- setdiff(needed_cols, names(yy))
  for (col in missing_cols) yy[[col]] <- NA_real_
  for (col in needed_cols) yy[[col]] <- na_to_na(yy[[col]])
  yy |> transmute(site_id = site_id, year = as.integer(TIMESTAMP),
                   !!!setNames(lapply(needed_cols, as.name), needed_cols))
}
f15_rows <- vector("list", nrow(f15_lookup))
for (i in seq_len(nrow(f15_lookup))) {
  if (i %% 50 == 0L) msg("  Reading FLUXNET2015 site ", i, " / ", nrow(f15_lookup))
  f15_rows[[i]] <- read_one_yy(f15_lookup$path[i], f15_lookup$site_id[i])
}
annual_f15 <- bind_rows(Filter(Negate(is.null), f15_rows))
f15_fluxes <- compute_site_annual_fluxes_from_df(annual_f15, h_unit = "W_m2")
f15_year <- f15_fluxes$site_year |> transmute(site_id, year, NEP = -NEE, ET, H)
msg("  FLUXNET2015: ", nrow(f15_year), " site-year rows, ", dplyr::n_distinct(f15_year$site_id), " sites")

common_sites <- intersect(unique(f15_year$site_id), unique(sh_year$site_id))
msg("  Sites present in both datasets (any year): ", length(common_sites))

# ---- IGBP class: current Shuttle classification, single label per site --------
sh_class <- read_csv(SHUTTLE_MEDIANS_CSV, show_col_types = FALSE) |>
  select(site_id, igbp_class) |>
  distinct(site_id, .keep_all = TRUE)

# ---- Step 2: matched years per site per flux, site median over matched years --
msg("\n=== Matching years per site per flux ===")

match_one_flux <- function(fx_col) {
  f15_sub <- f15_year |> filter(site_id %in% common_sites) |> select(site_id, year, value = all_of(fx_col))
  sh_sub  <- sh_year  |> filter(site_id %in% common_sites) |> select(site_id, year, value = all_of(fx_col))
  matched <- inner_join(
    f15_sub |> filter(!is.na(value)) |> rename(fluxnet2015_val = value),
    sh_sub  |> filter(!is.na(value)) |> rename(shuttle_val = value),
    by = c("site_id", "year")
  )
  matched |>
    group_by(site_id) |>
    summarise(
      fluxnet2015_val = median(fluxnet2015_val, na.rm = TRUE),
      shuttle_val     = median(shuttle_val, na.rm = TRUE),
      n_matched_years = n(),
      .groups = "drop"
    ) |>
    mutate(flux = toupper(fx_col))
}

site_flux <- bind_rows(lapply(unname(FLUXES), match_one_flux)) |>
  left_join(sh_class, by = "site_id")
msg("  Site-flux rows with >=1 matched year: ", nrow(site_flux))

# ---- Step 3: class-level comparison table (same shape as the primary CSV) -----
msg("\n=== Class-level stats ===")

comparison_table <- site_flux |>
  filter(igbp_class %in% STANDARD_IGBP) |>
  group_by(flux, igbp_class) |>
  summarise(
    fluxnet2015_median = median(fluxnet2015_val, na.rm = TRUE),
    fluxnet2015_sd      = if (n() < 2L) NA_real_ else sd(fluxnet2015_val, na.rm = TRUE),
    fluxnet2015_n_sites = n(),
    shuttle_median      = median(shuttle_val, na.rm = TRUE),
    shuttle_sd          = if (n() < 2L) NA_real_ else sd(shuttle_val, na.rm = TRUE),
    shuttle_n_sites     = n(),
    .groups = "drop"
  ) |>
  mutate(
    diff     = shuttle_median - fluxnet2015_median,
    pct_diff = 100 * diff / abs(fluxnet2015_median),
    excluded = fluxnet2015_n_sites < RELIABILITY_MIN,
    notes    = ifelse(excluded,
                       paste0("n=", fluxnet2015_n_sites,
                              " sites with >=1 common site-year, below n>=",
                              RELIABILITY_MIN, " reliability threshold"), "")
  ) |>
  arrange(flux, igbp_class)

full_grid <- expand.grid(flux = toupper(unname(FLUXES)), igbp_class = STANDARD_IGBP,
                          stringsAsFactors = FALSE)
comparison_table <- full_grid |>
  left_join(comparison_table, by = c("flux", "igbp_class")) |>
  mutate(
    fluxnet2015_n_sites = ifelse(is.na(fluxnet2015_n_sites), 0L, fluxnet2015_n_sites),
    shuttle_n_sites      = ifelse(is.na(shuttle_n_sites), 0L, shuttle_n_sites),
    excluded = ifelse(is.na(excluded), TRUE, excluded),
    notes    = ifelse(is.na(notes) | notes == "",
                       ifelse(fluxnet2015_n_sites == 0L,
                              "0 sites with any common site-year for this class/flux",
                              notes),
                       notes)
  ) |>
  arrange(flux, igbp_class)

for (fx in unname(FLUXES)) {
  sub <- comparison_table |> filter(flux == fx, !excluded)
  total_matched_sites      <- sum(site_flux$flux == fx)
  total_matched_siteyears  <- sum(site_flux$n_matched_years[site_flux$flux == fx], na.rm = TRUE)
  msg(sprintf("  %s: %d classes plotted (n_sites range %d-%d); total matched sites = %d, site-years = %d",
              fx, nrow(sub),
              if (nrow(sub) > 0L) min(sub$fluxnet2015_n_sites) else NA,
              if (nrow(sub) > 0L) max(sub$fluxnet2015_n_sites) else NA,
              total_matched_sites, total_matched_siteyears))
  excl <- comparison_table |> filter(flux == fx, excluded, fluxnet2015_n_sites > 0L)
  if (nrow(excl) > 0L) {
    msg("    Excluded (n>0 but below threshold): ",
        paste(sprintf("%s(n=%d)", excl$igbp_class, excl$fluxnet2015_n_sites), collapse = ", "))
  }
}

write_csv(comparison_table, OUT_CSV)
msg("Saved: ", OUT_CSV)

meta <- list(
  run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  pipeline_version = system("git rev-parse --short HEAD", intern = TRUE),
  data_sources = list(
    fluxnet2015_raw = F15_DIR,
    shuttle_source   = paste0(DB_PATH, " (`annual` table, via compute_site_annual_fluxes())"),
    shuttle_igbp_class_source = SHUTTLE_MEDIANS_CSV
  ),
  qc_threshold = QC_THRESHOLD_YY,
  method = paste0(
    "Revised 2026-10-02: restricts each site's FLUXNET2015 and Shuttle per-flux median to ",
    "ONLY the calendar years where BOTH datasets have a QC_THRESHOLD_YY=", QC_THRESHOLD_YY,
    "-qualifying value for that flux (R/site_annual_fluxes.R::compute_site_annual_fluxes()/",
    "compute_site_annual_fluxes_from_df()), matched independently per flux (NEP/ET/H). ",
    "fluxnet2015_n_sites == shuttle_n_sites by construction (same site set contributes to ",
    "both axes). Isolates ONEFlux processing-version differences from network-composition/",
    "coverage differences -- see review/figures/methods_flux_medians.md, ",
    "'Processing-version confound'."),
  reliability_threshold = RELIABILITY_MIN,
  igbp_class_source = "current Shuttle classification only (site_flux_medians_shuttle.csv), one label per site used for both axes",
  notes = "Extended Data figure (task 5, 2026-10-02); not part of the numbered pipeline. Does not modify or depend on figure_flux_comparison_combo.R or its outputs."
)
jsonlite::write_json(meta, paste0(OUT_CSV, ".meta.json"), pretty = TRUE, auto_unbox = TRUE)
msg("Saved: ", paste0(OUT_CSV, ".meta.json"))

# ---- Step 4: build the 3-panel combo figure (Nature format, Extended Data) -----
msg("\n=== Building ED combo figure ===")

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
    nature_theme()
}

make_panel <- function(flux_code, unit_expr, tag) {
  df <- comparison_table |> filter(flux == flux_code, !excluded)

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
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
    geom_errorbar(aes(xmin = fluxnet2015_median - fluxnet2015_sd, xmax = fluxnet2015_median + fluxnet2015_sd),
                  orientation = "y", width = 0, colour = "black", linewidth = 0.25) +
    geom_errorbar(aes(ymin = shuttle_median - shuttle_sd, ymax = shuttle_median + shuttle_sd),
                  width = 0, colour = "black", linewidth = 0.25) +
    geom_point(aes(fill = igbp_class), shape = 21, size = 2, colour = "black", stroke = 0.3) +
    ggrepel::geom_text_repel(aes(label = igbp_class), size = 2.2, colour = "black", seed = 42,
                              min.segment.length = 0.3, segment.size = 0.2, segment.colour = "grey50",
                              box.padding = 0.3, point.padding = 0.2) +
    scale_fill_paper_igbp() +
    scale_x_continuous(limits = lims, expand = expansion(mult = 0),
                        labels = nature_minus_labels(),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    scale_y_continuous(limits = lims, expand = expansion(mult = 0),
                        labels = nature_minus_labels(),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    panel_letter(tag, x = -Inf, y = Inf, hjust = -0.5, vjust = 1.6) +
    labs(
      x = as.expression(bquote("FLUXNET2015 median" ~ .(flux_code) ~ "± SD (" * .(unit_expr) * ")")),
      y = as.expression(bquote("the snapshot median" ~ .(flux_code) ~ "± SD (" * .(unit_expr) * ")"))
    ) +
    combo_theme()
}

panel_plots <- list(
  make_panel("NEP", UNITS[["nep"]], "a"),
  make_panel("ET",  UNITS[["et"]],  "b"),
  make_panel("H",   UNITS[["h"]],   "c")
)
combo <- (panel_plots[[1]] / panel_plots[[2]] / panel_plots[[3]]) + plot_layout(heights = c(1, 1, 1))

saved <- save_nature_figure(combo, OUT_STEM, width_mm = NATURE_WIDTH_SINGLE_MM, height_mm = 228,
                             extended_data = TRUE)
msg("Saved: ", saved$png, ", ", saved$pdf, ", ", saved$jpeg)

# ---- Legend --------------------------------------------------------------------
n_matched <- site_flux |> group_by(flux) |> summarise(n_sites = n(), n_site_years = sum(n_matched_years))
legend_lines <- c(
  "FIGURE LEGEND — figS2_flux_comparison_matched_siteyears.png",
  strrep("=", 60), "",
  "TITLE: Supplementary Figure S2 — FLUXNET2015 vs. the snapshot per-IGBP-class median",
  "flux comparison, restricted to matched site-years", "",
  "PUBLICATION LEGEND:",
  "Supplementary Figure S2. Comparison of per-IGBP-class median net ecosystem production (NEP),",
  "evapotranspiration (ET) and sensible heat flux (H) between the FLUXNET2015 release and the",
  "snapshot, restricted to site-years present and qualifying in both datasets for a given site",
  "and calendar year. This matched comparison isolates differences arising from updated",
  "flux-processing software from differences arising from network composition change,",
  "complementing the full-network comparison in the main text. Data used in the analyses are",
  "described in Section 2.5.", "",
  "DESCRIPTION:",
  "Three vertically-stacked panels (a NEP, b ET, c H -- bold lower-case, 8pt) comparing, for",
  "each IGBP vegetation class, the median flux value computed from the FLUXNET2015 release",
  "against the median value from the snapshot -- but unlike the",
  "primary comparison figure (fig_04_flux_comparison_combo_nep_et_h.png), each site's",
  "median on BOTH axes is computed only over the calendar years where BOTH datasets have a",
  "QC_THRESHOLD_YY-qualifying value for that flux (matched independently per flux), so any",
  "shift between axes isolates ONEFlux processing-version differences from network-",
  "composition/coverage change -- see review/figures/methods_flux_medians.md,",
  "'Processing-version confound'. fluxnet2015_n_sites == shuttle_n_sites per class by",
  "construction (the same matched site set contributes to both axes). No plot title,",
  "subtitle, or caption is drawn inside the figure; the exclusion rule is stated here only.",
  "",
  "PANELS AND AXES:",
  paste0("  a (top)    — Net Ecosystem Production (NEP) — n = ", n_matched$n_sites[n_matched$flux=="NEP"],
         " matched sites,"),
  paste0("               ", n_matched$n_site_years[n_matched$flux=="NEP"], " matched site-years"),
  paste0("  b (middle) — Evapotranspiration (ET) — n = ", n_matched$n_sites[n_matched$flux=="ET"],
         " matched sites, ", n_matched$n_site_years[n_matched$flux=="ET"], " matched site-years"),
  paste0("  c (bottom) — Sensible heat flux (H) — n = ", n_matched$n_sites[n_matched$flux=="H"],
         " matched sites, ", n_matched$n_site_years[n_matched$flux=="H"], " matched site-years"),
  "Each panel has an independently-computed axis range (equal x/y limits with 10% padding)",
  "and four-sided black inward tick marks (duplicated secondary axis, no labels), solid",
  "black panel border, no gridlines. Axis titles use plotmath, not a Unicode superscript-",
  "minus character (same reason as the primary combo figure).",
  "",
  "COLOUR CODING: scale_fill_paper_igbp() (R/plot_constants.R), one point per IGBP class per panel.",
  "",
  "CLASSIFICATION SCHEME AND EXCLUSIONS:",
  "Class statistics computed only over the 12 STANDARD_IGBP labels; a class is excluded",
  paste0("(flagged, not dropped from the companion CSV) when fewer than ", RELIABILITY_MIN,
         " sites have >=1 matched"),
  "site-year for that flux -- recomputed per flux here, so the excluded-class set can differ",
  "from the primary figure's fixed {CVM, CSH} set. See the companion CSV",
  "(flux_comparison_fluxnet2015_vs_shuttle_common_siteyears.csv) for exact per-class/per-flux",
  "n and exclusion reasons.",
  "",
  "DATA SOURCES AND QC (revised 2026-10-02):",
  paste0("Shuttle: R/site_annual_fluxes.R::compute_site_annual_fluxes(), DuckDB `annual` table ",
         "(FLUXMET"),
  "dataset). FLUXNET2015: compute_site_annual_fluxes_from_df(), this project's own already-",
  "extracted FLUXNET2015 YY CSVs (data/fluxnet2015_comparison/, comparison-only data under",
  paste0("CLAUDE.md Hard Rule 1). Both gated on QC_THRESHOLD_YY=", QC_THRESHOLD_YY,
         " (R/pipeline_config.R),"),
  "the per-site VUT/CUT rule in scripts/04_qc.R for NEP; H plotted in its native W m⁻² mean",
  "rate (h_unit = \"W_m2\"), matching the primary combo figure's panel c -- not the shared",
  "function's default pre-integrated MJ m⁻² yr⁻¹ total. IGBP class: current Shuttle",
  "classification only (site_flux_medians_shuttle.csv), one label per site used for both axes.",
  "",
  "REPRODUCIBILITY:",
  "Script: scripts/figure_flux_comparison_combo_alt_common_siteyears.R",
  "Companion table: data/snapshots/",
  "  flux_comparison_fluxnet2015_vs_shuttle_common_siteyears.csv",
  paste0("DIMENSIONS: ", NATURE_WIDTH_SINGLE_MM, " x 228 mm, 600 dpi PNG + vector PDF + 300 ppi JPEG,"),
  "Helvetica, white background. All text 7pt (axis titles/text; IGBP class point labels",
  "confirmed at 6.3pt) except the bold lower-case panel letters (8pt)."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))

msg("\n=== ED combo figure complete ===")
