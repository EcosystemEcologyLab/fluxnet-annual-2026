## supp_stage3_sampling_ratios.R
##
## Unattended supplementary run, Stage 3: sampling ratios behind Figure 5 /
## Figure S4 (figure4_representativeness.R's six representativeness axes).
##
## Built strictly from already-committed per-site classification files --
## site_*_fig4.csv and site_biomass_cci_v7.csv for towers, the
## *_global_distribution.csv files for land -- no raster re-extraction, no
## reads of any other snapshot file (e.g. site_koppen_beck2023.csv,
## site_aridity.csv), even though figure4_representativeness.R itself uses
## those for two of its twelve panels. That restriction means two of the
## twelve axis x comparison combinations cannot be faithfully reconstructed
## from the permitted files alone:
##   - koppen geo_vs_geo: figure4_representativeness.R classifies this from
##     site_koppen_beck2023.csv (Beck 2023 1km raster at tower); the one
##     column in the permitted site_koppen_era5_fig4.csv that was meant to
##     carry this (beck2023_kg_class) is entirely NA for all 781 sites --
##     not computable from the permitted file set at all.
##   - aridity geo_vs_geo: figure4_representativeness.R classifies this from
##     site_aridity.csv (CGIAR Aridity Index v3.1 raster at tower); the only
##     permitted aridity file (site_aridity_era5_fig4.csv) carries the ERA5-
##     derived AI (P_ERA/FAO-56 PET) used for the Geo-vs-Data panel instead.
##     Used here as a documented substitute -- numerically different from
##     the published metric by construction, not a bug.
## Both are flagged explicitly in the Jaccard-check output; see
## review/supp_run_status.md for how this affects the gated output.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Stage 3: sampling ratios (Figure 5 / Figure S4) ===")

SNAP_DIR <- "data/snapshots"
OUT_DIR  <- "review/figures/draft_manuscript_v1/SupTables"
fs::dir_create(OUT_DIR)

weighted_jaccard <- function(p, q) sum(pmin(p, q)) / sum(pmax(p, q))

## counts_df: tibble(class = <chr>, n = <int>), already filtered to non-NA
## classifications within the relevant eligible pool. land_df: tibble(class,
## global_land_fraction) -- NOT necessarily unique per class (caller may pass
## multiple rows per class, e.g. 3-letter Koppen rows under a 2-letter
## class; summed here).
build_axis_rows <- function(axis, comparison, counts_df, land_df, note = NA_character_) {
  n_classified <- sum(counts_df$n)
  if (n_classified == 0L) {
    msg(axis, " / ", comparison, ": 0 sites classified -- not computable from the permitted ",
        "file set. Skipped (see note: ", note, ").")
    return(NULL)
  }
  land <- land_df |>
    dplyr::group_by(class) |>
    dplyr::summarise(land_share = sum(global_land_fraction), .groups = "drop")
  merged <- land |>
    dplyr::full_join(counts_df, by = "class") |>
    dplyr::mutate(
      n          = as.integer(dplyr::coalesce(n, 0L)),
      land_share = dplyr::coalesce(land_share, 0),
      tower_share = n / n_classified,
      ## Task instruction: classes with land and no towers get ratio 0 (not
      ## NA, as figure4_representativeness.R's own build_panel_df() does).
      sampling_ratio = dplyr::case_when(
        land_share > 0 & tower_share == 0 ~ 0,
        land_share > 0 & tower_share >  0 ~ tower_share / land_share,
        TRUE ~ NA_real_
      ),
      log2_ratio = dplyr::if_else(!is.na(sampling_ratio) & sampling_ratio > 0,
                                   log2(sampling_ratio), NA_real_)
    )
  tibble::tibble(
    axis = axis, comparison = comparison, class = merged$class,
    land_share = merged$land_share, tower_share = merged$tower_share,
    tower_count = merged$n, n_towers_classified = n_classified,
    sampling_ratio = merged$sampling_ratio, log2_ratio = merged$log2_ratio,
    note = note
  )
}

rows <- list()
jcheck <- list()
add_jcheck <- function(axis, comparison, computed_j, note = NA_character_) {
  jcheck[[paste0(axis, "_", comparison)]] <<- tibble::tibble(
    axis = axis, comparison = comparison, computed_j = computed_j, note = note
  )
}

run_simple_axis <- function(axis, comparison, class_vec, land_df, note = NA_character_) {
  counts <- tibble::tibble(class = class_vec) |> dplyr::filter(!is.na(class)) |>
    dplyr::count(class, name = "n")
  r <- build_axis_rows(axis, comparison, counts, land_df, note = note)
  if (!is.null(r)) rows[[length(rows) + 1]] <<- r
  land_summ <- land_df |> dplyr::group_by(class) |> dplyr::summarise(global_land_fraction = sum(global_land_fraction), .groups = "drop")
  merged <- land_summ |> dplyr::full_join(counts, by = "class") |>
    dplyr::mutate(n = dplyr::coalesce(n, 0L), global_land_fraction = dplyr::coalesce(global_land_fraction, 0),
                  frac = n / sum(counts$n))
  j <- weighted_jaccard(merged$global_land_fraction, merged$frac)
  add_jcheck(axis, comparison, j, note)
  invisible(j)
}

## ============================================================================
## Koppen
## ============================================================================
kg_sites <- read_csv(file.path(SNAP_DIR, "site_koppen_era5_fig4.csv"), show_col_types = FALSE)
kg_land  <- read_csv(file.path(SNAP_DIR, "koppen_beck2023_global_distribution.csv"), show_col_types = FALSE) |>
  dplyr::transmute(class = koppen_twoletter, global_land_fraction)

kg_geo_geo_counts <- kg_sites |> dplyr::filter(!is.na(beck2023_kg_class)) |>
  dplyr::count(beck2023_kg_class, name = "n") |> dplyr::rename(class = beck2023_kg_class)
r <- build_axis_rows("koppen", "geo_vs_geo", kg_geo_geo_counts, kg_land,
  note = "NOT COMPUTABLE: beck2023_kg_class is entirely NA in site_koppen_era5_fig4.csv (the only permitted fig4 file for this axis); the true classification lives in site_koppen_beck2023.csv, outside the Stage 3 file restriction.")
if (!is.null(r)) rows[[length(rows) + 1]] <- r
add_jcheck("koppen", "geo_vs_geo", NA_real_,
  "Not computable from permitted files (see table note / review/supp_run_status.md).")

kg_geo_data_eligible <- kg_sites |> dplyr::filter(panel_a_eligible)
run_simple_axis("koppen", "geo_vs_data", kg_geo_data_eligible$panel_a_class_used, kg_land)

## ============================================================================
## IGBP
## ============================================================================
ig_sites <- read_csv(file.path(SNAP_DIR, "site_igbp_fig4.csv"), show_col_types = FALSE)
ig_land  <- read_csv(file.path(SNAP_DIR, "igbp_mcd12c1_global_distribution.csv"), show_col_types = FALSE) |>
  dplyr::select(class, global_land_fraction)

run_simple_axis("igbp", "geo_vs_geo", ig_sites$igbp_modis_class, ig_land)
run_simple_axis("igbp", "geo_vs_data", ig_sites$igbp_pi, ig_land)

## ============================================================================
## Aridity
## ============================================================================
ar_sites <- read_csv(file.path(SNAP_DIR, "site_aridity_era5_fig4.csv"), show_col_types = FALSE)
ar_land  <- read_csv(file.path(SNAP_DIR, "aridity_unep7_global_distribution.csv"), show_col_types = FALSE) |>
  dplyr::transmute(class = unep_class, global_land_fraction)

run_simple_axis("aridity", "geo_vs_geo", ar_sites$unep_class_7, ar_land,
  note = "SUBSTITUTE: uses the ERA5-derived AI classification (site_aridity_era5_fig4.csv), figure4_representativeness.R's own Geo-vs-Data source for this axis, because the true Geo-vs-Geo source (site_aridity.csv, CGIAR raster at tower) is outside the Stage 3 file restriction. Expected to differ from the published metric for this reason.")
ar_eligible <- ar_sites |> dplyr::filter(!excluded_fig4_geo_vs_data)
run_simple_axis("aridity", "geo_vs_data", ar_eligible$unep_class_7, ar_land)

## ============================================================================
## Biomass -- same classification both comparisons (no independent "data" side)
## ============================================================================
bm_sites <- read_csv(file.path(SNAP_DIR, "site_biomass_cci_v7.csv"), show_col_types = FALSE)
bm_land  <- read_csv(file.path(SNAP_DIR, "biomass_cci_v7_global_distribution.csv"), show_col_types = FALSE) |>
  dplyr::transmute(class = as.character(biomass_bin), global_land_fraction)

run_simple_axis("biomass", "geo_vs_geo", as.character(bm_sites$biomass_bin), bm_land)
run_simple_axis("biomass", "geo_vs_data", as.character(bm_sites$biomass_bin), bm_land)

## ============================================================================
## NEE / ET -- bin_geo (model at tower) vs bin_data (QC-qualifying tower value)
## ============================================================================
ne_sites  <- read_csv(file.path(SNAP_DIR, "site_nee_fig4.csv"), show_col_types = FALSE)
et_sites  <- read_csv(file.path(SNAP_DIR, "site_et_fig4.csv"), show_col_types = FALSE)
flux_land <- read_csv(file.path(SNAP_DIR, "nee_et_fig4_global_distribution.csv"), show_col_types = FALSE)

nee_land <- flux_land |> dplyr::filter(flux == "NEE") |> dplyr::transmute(class = as.character(bin), global_land_fraction = land_fraction)
et_land  <- flux_land |> dplyr::filter(flux == "ET")  |> dplyr::transmute(class = as.character(bin), global_land_fraction = land_fraction)

run_simple_axis("nee", "geo_vs_geo",  as.character(ne_sites$bin_geo),  nee_land)
run_simple_axis("nee", "geo_vs_data", as.character(ne_sites$bin_data), nee_land)
run_simple_axis("et",  "geo_vs_geo",  as.character(et_sites$bin_geo),  et_land)
run_simple_axis("et",  "geo_vs_data", as.character(et_sites$bin_data), et_land)

## ============================================================================
## Combine, check against representativeness_metrics_fig4.csv (read-only)
## ============================================================================
long_table <- dplyr::bind_rows(rows)
jcheck_df  <- dplyr::bind_rows(jcheck)

reference <- read_csv(file.path(SNAP_DIR, "representativeness_metrics_fig4.csv"), show_col_types = FALSE) |>
  dplyr::select(axis, comparison, reference_j = weighted_jaccard)

jcheck_df <- jcheck_df |>
  dplyr::left_join(reference, by = c("axis", "comparison")) |>
  dplyr::mutate(
    diff  = computed_j - reference_j,
    agree = !is.na(computed_j) & !is.na(diff) & round(computed_j, 6) == round(reference_j, 6)
  ) |>
  dplyr::arrange(axis, comparison)
print(jcheck_df, n = Inf, width = Inf)

n_agree <- sum(jcheck_df$agree, na.rm = TRUE)
n_total <- nrow(jcheck_df)
msg(n_agree, " / ", n_total, " axis x comparison combinations agree with ",
    "representativeness_metrics_fig4.csv to 6 decimals.")

jcheck_path <- file.path(OUT_DIR, "tableS_sampling_ratio_jaccard_check.csv")
write_csv(jcheck_df, jcheck_path)
write_output_metadata(
  jcheck_path,
  input_sources = c("data/snapshots/representativeness_metrics_fig4.csv",
                     "data/snapshots/site_koppen_era5_fig4.csv", "data/snapshots/site_igbp_fig4.csv",
                     "data/snapshots/site_aridity_era5_fig4.csv", "data/snapshots/site_biomass_cci_v7.csv",
                     "data/snapshots/site_nee_fig4.csv", "data/snapshots/site_et_fig4.csv",
                     "data/snapshots/koppen_beck2023_global_distribution.csv",
                     "data/snapshots/igbp_mcd12c1_global_distribution.csv",
                     "data/snapshots/aridity_unep7_global_distribution.csv",
                     "data/snapshots/biomass_cci_v7_global_distribution.csv",
                     "data/snapshots/nee_et_fig4_global_distribution.csv"),
  notes = paste0(
    "Always written (reports the Stage 3 validation outcome regardless of pass/fail). Weighted ",
    "Jaccard recomputed from a long table built ONLY from committed site_*_fig4.csv / ",
    "site_biomass_cci_v7.csv tower files and *_global_distribution.csv land files (no raster re-",
    "extraction, no other snapshot files), compared against ",
    "data/snapshots/representativeness_metrics_fig4.csv (not modified). ", n_agree, " / ", n_total,
    " agree to 6 decimals. koppen/geo_vs_geo is not computable at all from the permitted file set ",
    "(beck2023_kg_class is entirely NA in site_koppen_era5_fig4.csv); aridity/geo_vs_geo uses the ",
    "ERA5-derived AI as a documented substitute for the CGIAR-raster-at-tower value, so it is ",
    "expected to differ. See each row's own note column and review/supp_run_status.md."
  )
)
msg("Saved: ", jcheck_path)

if (n_agree == n_total) {
  out_path <- file.path(OUT_DIR, "tableS_sampling_ratios_by_axis.csv")
  write_csv(long_table, out_path)
  write_output_metadata(
    out_path,
    input_sources = c("data/snapshots/representativeness_metrics_fig4.csv"),
    notes = "All 12 axis x comparison combinations agreed with representativeness_metrics_fig4.csv to 6 decimals; full long table written."
  )
  msg("All combinations agreed -- wrote: ", out_path)
} else {
  msg("NOT writing tableS_sampling_ratios_by_axis.csv -- ", n_total - n_agree,
      " / ", n_total, " combinations disagree with representativeness_metrics_fig4.csv (see ",
      jcheck_path, " for which, and by how much).")
}

## ============================================================================
## Extremes: per axis x comparison, 3 lowest + 3 highest sampling-ratio
## classes among classes holding >=1% of land. Written unconditionally (does
## not depend on the 12-way Jaccard check above) for every axis x comparison
## this script could actually compute (i.e. excluding koppen/geo_vs_geo).
## ============================================================================
extremes <- long_table |>
  dplyr::filter(land_share >= 0.01, !is.na(sampling_ratio)) |>
  dplyr::group_by(axis, comparison) |>
  dplyr::group_modify(~ {
    d <- dplyr::arrange(.x, sampling_ratio)
    n <- nrow(d)
    k <- min(3L, n)
    dplyr::bind_rows(
      dplyr::mutate(utils::head(d, k), extreme = "lowest"),
      dplyr::mutate(utils::tail(d, k), extreme = "highest")
    )
  }) |>
  dplyr::ungroup() |>
  dplyr::distinct(axis, comparison, class, .keep_all = TRUE) |>
  dplyr::select(axis, comparison, extreme, class, land_share, tower_share, tower_count,
                sampling_ratio, log2_ratio) |>
  dplyr::arrange(axis, comparison, extreme, sampling_ratio)
print(extremes, n = Inf, width = Inf)

extremes_path <- file.path(OUT_DIR, "tableS_sampling_ratio_extremes.csv")
write_csv(extremes, extremes_path)
write_output_metadata(
  extremes_path,
  input_sources = c("(derived from the same sources as tableS_sampling_ratio_jaccard_check.csv)"),
  notes = paste0(
    "Three lowest and three highest sampling-ratio classes per axis x comparison, restricted to ",
    "classes holding >=1% of land. koppen/geo_vs_geo excluded (not computable -- see ",
    "tableS_sampling_ratio_jaccard_check.csv). Written unconditionally, independent of whether ",
    "tableS_sampling_ratios_by_axis.csv was written."
  )
)
msg("Saved: ", extremes_path)

msg("=== Stage 3 done ===")
