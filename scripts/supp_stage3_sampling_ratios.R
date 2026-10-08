## supp_stage3_sampling_ratios.R
##
## Unattended supplementary run, Stage 3: sampling ratios behind Figure 5 /
## Figure S4 (figure4_representativeness.R's six representativeness axes).
##
## Built from already-committed per-site classification files -- site_*_fig4.csv
## and site_biomass_cci_v7.csv for most towers, the *_global_distribution.csv
## files for land -- no raster re-extraction anywhere.
##
## Revised (per task instruction): the koppen/geo_vs_geo and aridity/geo_vs_geo
## panels are now read from the SAME tower-side inputs figure4_representativeness.R
## itself uses for those two panels -- site_koppen_beck2023.csv (koppen_twoletter)
## and site_aridity.csv (unep_class_7) respectively. Both are already-committed
## snapshot CSVs (the Beck 2023 and CGIAR rasters extracted at tower coordinates
## in an earlier pipeline stage), so reading them directly is not a raster
## re-extraction. Both files classify all 781 current-network sites (0 NA), so
## no fallback/substitute is needed for either axis any more -- the original
## restriction to a narrower site_*_fig4.csv file set had excluded exactly these
## two files, which is what previously forced a "not computable" / "substitute"
## result for these two combinations (see git history of this file and
## review/supp_run_status.md Stage 3 for that prior state).

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
## Jaccard-check and extremes tables moved out of SupTables/ into a dedicated
## diagnostics location (supplementary material restructure, 2026-10-08,
## SESSION_LOG.md) -- they are validation/derived-check outputs, not
## supplementary submission tables; tableS3_sampling_ratios_by_axis.csv
## itself stays in OUT_DIR/SupTables.
CHECKS_DIR <- "review/diagnostics/sampling_ratio_checks"
fs::dir_create(OUT_DIR)
fs::dir_create(CHECKS_DIR)

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

## Geo vs Geo: figure4_representativeness.R's own tower-side input for this
## panel (site_koppen_beck2023.csv, column koppen_twoletter) -- a committed
## snapshot CSV, not a raster re-extraction. All 781 sites classified.
kg_beck <- read_csv(file.path(SNAP_DIR, "site_koppen_beck2023.csv"), show_col_types = FALSE)
run_simple_axis("koppen", "geo_vs_geo", kg_beck$koppen_twoletter, kg_land)

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

## Geo vs Geo: figure4_representativeness.R's own tower-side input for this
## panel (site_aridity.csv, column unep_class_7 -- CGIAR Aridity Index v3.1
## raster at tower, its own native coverage) -- a committed snapshot CSV, not
## a raster re-extraction. All 781 sites classified.
ar_geo_geo_sites <- read_csv(file.path(SNAP_DIR, "site_aridity.csv"), show_col_types = FALSE)
run_simple_axis("aridity", "geo_vs_geo", ar_geo_geo_sites$unep_class_7, ar_land)
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

jcheck_path <- file.path(CHECKS_DIR, "tableS_sampling_ratio_jaccard_check.csv")
write_csv(jcheck_df, jcheck_path)
write_output_metadata(
  jcheck_path,
  input_sources = c("data/snapshots/representativeness_metrics_fig4.csv",
                     "data/snapshots/site_koppen_era5_fig4.csv", "data/snapshots/site_koppen_beck2023.csv",
                     "data/snapshots/site_igbp_fig4.csv",
                     "data/snapshots/site_aridity_era5_fig4.csv", "data/snapshots/site_aridity.csv",
                     "data/snapshots/site_biomass_cci_v7.csv",
                     "data/snapshots/site_nee_fig4.csv", "data/snapshots/site_et_fig4.csv",
                     "data/snapshots/koppen_beck2023_global_distribution.csv",
                     "data/snapshots/igbp_mcd12c1_global_distribution.csv",
                     "data/snapshots/aridity_unep7_global_distribution.csv",
                     "data/snapshots/biomass_cci_v7_global_distribution.csv",
                     "data/snapshots/nee_et_fig4_global_distribution.csv"),
  notes = paste0(
    "Always written (reports the Stage 3 validation outcome regardless of pass/fail). Weighted ",
    "Jaccard recomputed from a long table built from committed site_*_fig4.csv / site_biomass_cci_v7.csv ",
    "tower files, plus (for koppen/geo_vs_geo and aridity/geo_vs_geo only) the same tower-side inputs ",
    "figure4_representativeness.R itself uses for those two panels (site_koppen_beck2023.csv, ",
    "site_aridity.csv -- both already-committed snapshot CSVs, no raster re-extraction), and ",
    "*_global_distribution.csv land files throughout, compared against ",
    "data/snapshots/representativeness_metrics_fig4.csv (not modified). ", n_agree, " / ", n_total,
    " agree to 6 decimals. See each row's own note column and review/supp_run_status.md."
  )
)
msg("Saved: ", jcheck_path)

## Renamed tableS3_sampling_ratios_by_axis.csv -> supplementary_data_2_
## sampling_ratios.csv (2026-10-08 follow-up): this full long table is no
## longer one of the numbered Supplementary Tables (S1-S3) assembled into
## the Word Supplementary Information document -- it joins
## supplementary_data_1_sites.csv as its own standalone supplementary data
## file. Old tableS3_sampling_ratios_by_axis.* moved to SupTables/deprecated/.
if (n_agree == n_total) {
  out_path <- file.path(OUT_DIR, "supplementary_data_2_sampling_ratios.csv")
  write_csv(long_table, out_path)
  write_output_metadata(
    out_path,
    input_sources = c("data/snapshots/representativeness_metrics_fig4.csv"),
    notes = "All 12 axis x comparison combinations agreed with representativeness_metrics_fig4.csv to 6 decimals; full long table written."
  )
  msg("All combinations agreed -- wrote: ", out_path)
} else {
  msg("NOT writing supplementary_data_2_sampling_ratios.csv -- ", n_total - n_agree,
      " / ", n_total, " combinations disagree with representativeness_metrics_fig4.csv (see ",
      jcheck_path, " for which, and by how much).")
}

## ============================================================================
## Extremes: per axis x comparison, classes with sampling_ratio < 0.5
## (undersampled) or > 2 (oversampled), among classes holding >=1% of land --
## no longer a fixed three per side. Written only if all 12 axis x comparison
## combinations agreed with representativeness_metrics_fig4.csv above (same
## gate as tableS_sampling_ratios_by_axis.csv, per task instruction), since it
## is built from the same long_table.
## ============================================================================
if (n_agree == n_total) {
  extremes <- long_table |>
    dplyr::filter(land_share >= 0.01, !is.na(sampling_ratio),
                  sampling_ratio < 0.5 | sampling_ratio > 2) |>
    dplyr::mutate(extreme = dplyr::if_else(sampling_ratio < 0.5, "lowest", "highest")) |>
    dplyr::select(axis, comparison, extreme, class, land_share, tower_share, tower_count,
                  sampling_ratio, log2_ratio) |>
    dplyr::arrange(axis, comparison, extreme, sampling_ratio)
  print(extremes, n = Inf, width = Inf)

  extremes_path <- file.path(CHECKS_DIR, "tableS_sampling_ratio_extremes.csv")
  write_csv(extremes, extremes_path)
  write_output_metadata(
    extremes_path,
    input_sources = c("(derived from the same sources as tableS_sampling_ratio_jaccard_check.csv)"),
    notes = paste0(
      "Classes with sampling_ratio < 0.5 (undersampled, extreme='lowest') or > 2 (oversampled, ",
      "extreme='highest') per axis x comparison, restricted to classes holding >=1% of land -- not ",
      "a fixed three per side. Written only because all 12 axis x comparison combinations agreed ",
      "with representativeness_metrics_fig4.csv to 6 decimals (see ",
      "tableS_sampling_ratio_jaccard_check.csv)."
    )
  )
  msg("Saved: ", extremes_path)
} else {
  msg("NOT writing tableS_sampling_ratio_extremes.csv -- ", n_total - n_agree,
      " / ", n_total, " combinations disagree with representativeness_metrics_fig4.csv.")
}

msg("=== Stage 3 done ===")
