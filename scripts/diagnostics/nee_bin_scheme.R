## nee_bin_scheme.R
##
## Diagnose a seven-bin scheme for annual NEE usable identically in both
## versions of Figs 4/5: Geo vs Data (site-measured NEE vs. the global TRENDY
## distribution) and Geo vs Geo (TRENDY-at-site vs. the same global
## distribution).
##
## READ-ONLY with respect to the pipeline, R/pipeline_config.R, and every
## committed figure. No existing script, figure, legend, snapshot CSV, or
## representativeness_metrics.csv is modified. This script only READS
## existing snapshot files and the already-cached per-model TRENDY
## intermediates, and WRITES new files under review/diagnostics/nee_bin_scheme/.
##
## Section 1 reproduces two already-published values EXACTLY before anything
## else runs, per task instructions:
##   - Geo vs Geo:  trendy_nee_median, 7bin_hybrid, current_781 = 0.4931868270307604
##     (data/snapshots/representativeness_metrics.csv, the row written by
##     scripts/figure_representativeness_trendy_compute.R; also recorded in
##     logs/trendy_analysis_complete.marker as "trendy_nee_median J=0.4932").
##   - Geo vs Data: NEE weighted Jaccard = 0.233 (SESSION_LOG.md, "2026-09-17
##     NEE/ET site-vs-TRENDY diagnostic" entry), i.e. the current_781 "nee"
##     panel of scripts/figure_representativeness_supp_sitelevel.R.
## If either fails to reproduce, the script stops and explains why rather
## than proceeding to Schemes 2/3 on an unverified baseline.
##
## Outputs (review/diagnostics/nee_bin_scheme/):
##   report.md
##   table_reproduction_check.csv (+ .meta.json)
##   table_scheme{1,2,3}_global.csv        global area fraction per bin
##   table_scheme{1,2,3}_sites.csv         site count/fraction per bin per network
##   table_scheme{1,2,3}_jaccard.csv       weighted Jaccard per network/version
##   fig_scheme{1,2,3}_bin_fractions.png
##   trendy_nee_signed_mean.tif (+ .meta.json)  new signed ensemble-median
##     mean-annual-nbp raster (see Section 2), the basis for Schemes 2/3

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(jsonlite)
  library(fs)
})

source("R/pipeline_config.R")
check_pipeline_config()

# Local .meta.json writer, mirroring figure_representativeness_trendy_compute.R:27-37
# and extract_historical_sites_representativeness.R:47-57 (both avoid sourcing
# R/utils.R because its write_output_metadata() sink()s to outputs/session_info.txt,
# which nests awkwardly with this script's own log-file sink() below).
write_meta <- function(output_path, input_sources, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = system("git rev-parse --short HEAD", intern = TRUE),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

LOG_FILE <- file.path("logs", paste0("nee_bin_scheme_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
con <- file(LOG_FILE, open = "wt")
sink(con, type = "output")
sink(con, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%H:%M:%S]"), " ", ...)

msg("=== nee_bin_scheme.R ===")
msg("Log: ", LOG_FILE)

SNAP <- "data/snapshots"
OUTD <- "review/diagnostics/nee_bin_scheme"
fs::dir_create(OUTD)

NET_ORDER  <- c("marconi", "la_thuile", "fluxnet2015", "current_781")
NET_NSITES <- c(marconi = 35L, la_thuile = 252L, fluxnet2015 = 212L, current_781 = 781L)
FLUX_MEDIANS_FILE <- c(current_781 = "site_flux_medians_shuttle.csv",
                        fluxnet2015 = "site_flux_medians_fluxnet2015.csv")
SITE_TRENDY_FILE <- c(marconi = "site_trendy_nee_median_marconi.csv",
                       la_thuile = "site_trendy_nee_median_la_thuile.csv",
                       fluxnet2015 = "site_trendy_nee_median_fluxnet2015.csv",
                       current_781 = "site_trendy_nee_median.csv")

# ============================================================================
# Shared helpers (weighted Jaccard / Hellinger / site counting), reused
# verbatim from figure_representativeness_supp_sitelevel.R:206-237 and
# extract_historical_sites_representativeness.R:106-113
# ============================================================================

count_sites <- function(df, class_col, levels_vec = NULL, n_total = NULL) {
  n_total <- if (is.null(n_total)) nrow(df) else n_total
  out <- df |>
    dplyr::filter(!is.na(.data[[class_col]])) |>
    dplyr::count(.data[[class_col]], name = "n") |>
    dplyr::rename(class = 1) |>
    dplyr::mutate(class = as.character(class))
  if (!is.null(levels_vec)) {
    out <- data.frame(class = levels_vec, stringsAsFactors = FALSE) |>
      dplyr::left_join(out, by = "class") |>
      dplyr::mutate(n = dplyr::coalesce(n, 0L))
  }
  out |> dplyr::mutate(network_frac = n / n_total)
}

compute_repr_metrics <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(weighted_jaccard = sum(pmin(p, q)) / sum(pmax(p, q)),
       hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2)))
}

classify_into_bins <- function(x, bin_df) {
  idx <- findInterval(x, bin_df$min_value, all.inside = FALSE)
  idx[idx < 1L] <- NA_integer_
  idx[idx > nrow(bin_df)] <- nrow(bin_df)
  bin_df$bin[idx]
}

j_for <- function(counts, global_df) {
  compute_repr_metrics(
    global_df$global_land_fraction[match(counts$class, global_df$class)],
    counts$network_frac
  )$weighted_jaccard
}

# ============================================================================
# SECTION 1: Reproduction check (Scheme 1 -- existing absolute convention)
# ============================================================================
msg("\n=== SECTION 1: Reproduction check ===")

nee7_bins <- readr::read_csv(file.path(SNAP, "trendy_nee_median_global_distribution.csv"), show_col_types = FALSE)
nee7_global <- nee7_bins |> dplyr::transmute(class = as.character(bin), global_land_fraction)

## --- 1a. Geo vs Geo: current_781, trendy_nee_median value classified into
## the stored 7-bin edges (classify_into_bins applied to the raw
## trendy_nee_median_value column -- an independent re-classification, not
## a trust of the already-stored trendy_nee_median_bin column, though we
## cross-check against it below) ---
site_trendy_current <- readr::read_csv(file.path(SNAP, "site_trendy_nee_median.csv"), show_col_types = FALSE)
stopifnot(nrow(site_trendy_current) == 781L)

reclassified_bin <- classify_into_bins(site_trendy_current$trendy_nee_median_value, nee7_bins)
mismatch_n <- sum(reclassified_bin != site_trendy_current$trendy_nee_median_bin, na.rm = TRUE)
msg("  Cross-check vs. stored trendy_nee_median_bin column: ", mismatch_n, " mismatches / ", nrow(site_trendy_current))

geo_geo_counts_s1 <- count_sites(
  data.frame(bin = as.character(reclassified_bin), stringsAsFactors = FALSE),
  "bin", levels_vec = as.character(1:7), n_total = 781L
)
j_geo_geo_s1 <- j_for(geo_geo_counts_s1, nee7_global)
target_geo_geo <- 0.4931868270307604
msg("  Geo vs Geo (current_781, scheme 1): computed J = ", format(j_geo_geo_s1, digits = 16),
    "  | target (data/snapshots/representativeness_metrics.csv:189) = ", target_geo_geo)
ok_geo_geo <- isTRUE(all.equal(j_geo_geo_s1, target_geo_geo, tolerance = 1e-9)) && mismatch_n == 0L

## --- 1b. Geo vs Data: current_781, nep_median -> abs(nep_median), same
## stored 7-bin edges, matching figure_representativeness_supp_sitelevel.R:
## load_measured_axis()/count_sites()/compute_repr_metrics() at :280-301,:333 ---
flux_current <- readr::read_csv(file.path(SNAP, FLUX_MEDIANS_FILE[["current_781"]]), show_col_types = FALSE) |>
  dplyr::mutate(nee_abs = abs(nep_median))
bin_assigned_data <- classify_into_bins(flux_current$nee_abs, nee7_bins)
geo_data_counts_s1 <- count_sites(
  data.frame(bin = as.character(bin_assigned_data), stringsAsFactors = FALSE),
  "bin", levels_vec = as.character(1:7), n_total = 781L
)
j_geo_data_s1 <- j_for(geo_data_counts_s1, nee7_global)
target_geo_data <- 0.233
msg("  Geo vs Data (current_781, scheme 1): computed J = ", format(j_geo_data_s1, digits = 6),
    "  | target (SESSION_LOG.md:1337) = ", target_geo_data)
ok_geo_data <- abs(round(j_geo_data_s1, 3) - target_geo_data) < 1e-9

repro_table <- data.frame(
  version = c("geo_vs_geo", "geo_vs_data"),
  network = "current_781",
  scheme = "1_absolute_existing",
  computed_jaccard = c(j_geo_geo_s1, j_geo_data_s1),
  target_jaccard = c(target_geo_geo, target_geo_data),
  target_source = c("data/snapshots/representativeness_metrics.csv:189 (also logs/trendy_analysis_complete.marker)",
                     "SESSION_LOG.md:1337"),
  reproduced = c(ok_geo_geo, ok_geo_data)
)
out_repro_csv <- file.path(OUTD, "table_reproduction_check.csv")
readr::write_csv(repro_table, out_repro_csv)
write_meta(out_repro_csv,
           input_sources = c("data/snapshots/representativeness_metrics.csv",
                              "SESSION_LOG.md",
                              file.path(SNAP, "site_trendy_nee_median.csv"),
                              file.path(SNAP, "trendy_nee_median_global_distribution.csv"),
                              file.path(SNAP, FLUX_MEDIANS_FILE[["current_781"]])),
           notes = "Scheme-1 (existing absolute convention) reproduction check, required before proceeding to signed Schemes 2/3.")

if (!ok_geo_geo || !ok_geo_data) {
  stop("REPRODUCTION FAILED -- see ", out_repro_csv, " and ", LOG_FILE,
       ". geo_vs_geo reproduced=", ok_geo_geo, " (mismatch_n=", mismatch_n, ")",
       ", geo_vs_data reproduced=", ok_geo_data,
       ". Stopping per task instructions -- not proceeding to Schemes 2/3 on an unverified baseline.")
}
msg("  BOTH reproduced exactly. Proceeding.")

# ============================================================================
# SECTION 2: NBP/NEE definition, sign convention, and the new signed raster
# ============================================================================
msg("\n=== SECTION 2: Sign convention and signed ensemble raster ===")

## Definition of the existing (absolute) axis, verified by direct reading of
## scripts/figure_representativeness_trendy_compute.R (READ-ONLY -- not
## modified here):
##   - Global nee_median uses TRENDY variable `nbp`, converted to
##     gC m-2 yr-1 by unit conversion only, no sign flip (load_annual(),
##     :138-196).
##   - Stored trendy_nee_median.tif = ensemble-median-across-16-models of
##     (mean of |nbp| across the 34 annual layers 1990-2023, per model) --
##     i.e. mean(|NBP_annual|), NOT |mean(NBP_annual)|. Function
##     compute_mean_abs (:227-238), invoked at :381
##     (compute_ensemble("nee_median", "nbp", compute_mean_abs)). This
##     confirms review/diagnostics/nee_et_site_vs_trendy/report.md's finding
##     of a "mean-of-absolute" convention (T1/T3 there).
##   - TRENDY convention: nbp positive = net land carbon sink/uptake,
##     negative = net source/release to atmosphere -- confirmed by the
##     code's own comment at :543 ("nbp can be negative (source)").
## Standard eddy-covariance NEE convention is the OPPOSITE sign sense:
## positive NEE = net release to atmosphere (source), negative NEE = net
## uptake (sink); by definition NEE = -NEP. This script therefore defines,
## for all signed work below:
##   NEE_signed_global = -1 * (ensemble-median per-cell mean-annual nbp)
##   NEE_signed_site   = -1 * nep_median
## nep_median's units are confirmed gC m-2 yr-1 by
## data/snapshots/site_flux_medians_shuttle.csv.meta.json:
## "unit_nep_gpp_ter": "gC m-2 yr-1 (pre-integrated YY product; NEP = -NEE)"
## -- so NEE_signed_site is directly in the same units and same sign sense
## (positive=source) as NEE_signed_global.

DERIVED_DIR <- "data/external/trendy/derived"
INTER_DIR   <- file.path(DERIVED_DIR, "intermediate")
KG_PATH     <- file.path("data", "external", "koppen_beck2023", "1991_2020", "koppen_geiger_0p5.tif")
N_YEARS     <- 34L

## The exact 16-model ensemble that produced trendy_nee_median.tif (and the
## reproduced J=0.4932 above), per logs/trendy_analysis_complete.marker
## ("models_included:") -- NOT re-derived from MODELS_19 in
## figure_representativeness_trendy_compute.R, because that script's own
## comments (:320-325) and review/diagnostics/nee_et_site_vs_trendy/report.md's
## "Not-found" log both note CLM-FATES has no cached intermediate on disk
## despite being listed in MODELS_19, and ELM's intermediate exists but was
## excluded (lacks 2023 data). Using the marker's authoritative list avoids
## silently re-including either.
MODELS_OK <- c("CABLE-POP", "CLASSIC", "CLM", "DLEM", "ED", "ELM-FATES",
               "IBIS", "ISAM", "JULES-ES", "LPJ-GUESS", "LPJml", "LPJwsl",
               "LPX-Bern", "ORCHIDEE", "TEM", "VISIT-UT")

compute_mean <- function(r) {
  vals <- terra::values(r)
  complete <- rowSums(is.na(vals)) == 0L
  out_vals <- rep(NA_real_, nrow(vals))
  if (any(complete)) out_vals[complete] <- rowMeans(vals[complete, , drop = FALSE])
  r_out <- r[[1L]]
  terra::values(r_out) <- out_vals
  names(r_out) <- "mean"
  r_out
}

SIGNED_RASTER_PATH <- file.path(OUTD, "trendy_nee_signed_mean.tif")
if (file.exists(SIGNED_RASTER_PATH)) {
  msg("  SKIP (exists): ", SIGNED_RASTER_PATH)
  r_nee_signed <- terra::rast(SIGNED_RASTER_PATH)
} else {
  msg("  Computing signed ensemble-median mean-annual NBP from ", length(MODELS_OK),
      " cached per-model intermediates (Step-2-only aggregation; Step 1 regridding NOT rerun) ...")
  model_stats <- vector("list", length(MODELS_OK))
  for (i in seq_along(MODELS_OK)) {
    mdl <- MODELS_OK[[i]]
    inter_path <- file.path(INTER_DIR, paste0(mdl, "_nbp_regridded.tif"))
    if (!file.exists(inter_path)) stop("Missing cached intermediate: ", inter_path)
    r <- terra::rast(inter_path)
    stopifnot(terra::ncol(r) == 720L, terra::nrow(r) == 360L, terra::nlyr(r) == N_YEARS)
    stat_r <- compute_mean(r)
    names(stat_r) <- mdl
    model_stats[[i]] <- stat_r
    rm(r); gc(verbose = FALSE)
    msg("    ", mdl, " done")
  }
  stk <- terra::rast(model_stats)
  ens_nbp_signed <- terra::app(stk, fun = function(v) median(v, na.rm = TRUE))
  r_nee_signed <- -1 * ens_nbp_signed
  names(r_nee_signed) <- "nee_signed_mean"
  terra::writeRaster(r_nee_signed, SIGNED_RASTER_PATH, gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
  write_meta(SIGNED_RASTER_PATH,
             input_sources = file.path(INTER_DIR, paste0(MODELS_OK, "_nbp_regridded.tif")),
             notes = paste0(
               "Signed ensemble-median-across-16-models of mean-annual nbp (gC m-2 yr-1), ",
               "1990-2023, sign-flipped (NEE = -NBP) to standard eddy-covariance convention ",
               "(positive = source/release, negative = sink/uptake). Same 16-model ensemble ",
               "and same per-model regridded intermediates as trendy_nee_median.tif; only the ",
               "per-model temporal aggregator differs (rowMeans() here vs. rowMeans(abs()) there)."
             ))
  msg("  Saved: ", SIGNED_RASTER_PATH)
}

kg_05 <- terra::rast(KG_PATH)
r_land_signed <- terra::mask(r_nee_signed, kg_05)
cell_areas_05 <- terra::cellSize(kg_05, mask = TRUE, unit = "km")

rng <- terra::minmax(r_land_signed)[, 1]
msg("  Signed global land NBP-derived NEE range: [", round(rng[1], 2), ", ", round(rng[2], 2), "] gC m-2 yr-1")

## Area-weighted fine histogram of the SIGNED distribution (0.5 gC m-2 yr-1
## bins across the full observed range), mirroring the unsigned-magnitude
## histogram construction at figure_representativeness_trendy_compute.R:482-501,
## but retaining sign instead of flooring negatives to 0.
HIST_STEP <- 0.1
hist_lo <- seq(floor(rng[1] / HIST_STEP) * HIST_STEP, ceiling(rng[2] / HIST_STEP) * HIST_STEP - HIST_STEP, by = HIST_STEP)
hist_hi <- hist_lo + HIST_STEP
hist_ids <- seq_along(hist_lo)
hist_rcl <- cbind(hist_lo, hist_hi, as.numeric(hist_ids))
r_hist <- terra::classify(r_land_signed, hist_rcl, right = FALSE, include.lowest = TRUE)
hist_areas <- terra::zonal(cell_areas_05, r_hist, fun = "sum", na.rm = TRUE)
names(hist_areas) <- c("bin_id", "area_km2")
hist_areas <- hist_areas[!is.na(hist_areas$bin_id), ]
hist_areas$value <- hist_lo[hist_areas$bin_id]
hist_areas <- hist_areas[order(hist_areas$value), ]
total_land_km2 <- sum(hist_areas$area_km2)
msg("  Total land in signed histogram: ", format(round(total_land_km2), big.mark = ","),
    " km2 (target ~147,322,862)")

# ============================================================================
# SECTION 3: Bin scheme construction
# ============================================================================
msg("\n=== SECTION 3: Bin scheme construction ===")

## ---- Scheme 1: existing absolute convention, existing stored edges ----
scheme1_global <- nee7_bins |>
  dplyr::transmute(scheme = "1_absolute_existing", bin = as.character(bin), bin_label,
                    min_value, max_value, global_land_area_km2, global_land_fraction)

## ---- Scheme 2: signed, sign-anchored (near-zero + 3+3 equal-area quantiles) ----
## Quantile construction on each side mirrors make_bins() in
## figure_representativeness_trendy_compute.R:436-449 (equal-area cumulative
## targets), applied separately to the sink-side (value < -half_width) and
## source-side (value > half_width) subsets of the signed area histogram.
build_scheme2 <- function(half_width) {
  source_side <- hist_areas[hist_areas$value >= half_width, ]
  sink_side   <- hist_areas[hist_areas$value < -half_width, ]
  if (nrow(source_side) == 0L || nrow(sink_side) == 0L)
    stop("half_width=", half_width, " leaves an empty source or sink side")

  q_break <- function(side_df, fracs, ascending) {
    side_df <- side_df[order(side_df$value, decreasing = !ascending), ]
    cum_area <- cumsum(side_df$area_km2)
    total <- sum(side_df$area_km2)
    vapply(fracs, function(f) {
      target <- f * total
      idx <- which(cum_area >= target)[[1L]]
      side_df$value[[idx]]
    }, numeric(1))
  }
  ## Source side (positive): ascending value order, breaks at 1/3, 2/3 of area
  src_breaks <- sort(round(q_break(source_side, c(1, 2) / 3, ascending = TRUE), 2))
  ## Sink side (negative): area accumulated walking outward from -half_width
  ## toward the most negative value; the two returned breakpoints are then
  ## re-sorted ascending (most negative first) to match bin order.
  snk_breaks <- sort(round(q_break(sink_side, c(1, 2) / 3, ascending = FALSE), 2))

  breaks <- c(-Inf, snk_breaks, -half_width, half_width, src_breaks, Inf)
  bin_df <- data.frame(
    bin = 1:7,
    min_value = breaks[1:7],
    max_value = breaks[2:8],
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(bin_label = sprintf("%s–%s", format(min_value), format(max_value)))

  r_bins <- terra::classify(r_land_signed,
                             cbind(bin_df$min_value, bin_df$max_value, bin_df$bin),
                             right = FALSE, include.lowest = TRUE)
  zone_areas <- terra::zonal(cell_areas_05, r_bins, fun = "sum", na.rm = TRUE)
  names(zone_areas) <- c("bin", "global_land_area_km2")
  zone_areas <- zone_areas[!is.na(zone_areas$bin), ]
  bin_df <- bin_df |>
    dplyr::left_join(zone_areas, by = "bin") |>
    dplyr::mutate(global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
                  global_land_fraction = global_land_area_km2 / sum(global_land_area_km2),
                  half_width = half_width)
  bin_df
}

scheme2_halfwidths <- c(10, 25, 50)
scheme2_global <- dplyr::bind_rows(lapply(scheme2_halfwidths, build_scheme2)) |>
  dplyr::mutate(scheme = paste0("2_signed_anchored_hw", half_width), bin = as.character(bin)) |>
  dplyr::select(scheme, half_width, bin, bin_label, min_value, max_value,
                global_land_area_km2, global_land_fraction)

## ---- Scheme 3: signed equal-area septiles, no special zero bin ----
build_scheme3 <- function() {
  ord <- hist_areas[order(hist_areas$value), ]
  cum_area <- cumsum(ord$area_km2)
  total <- sum(ord$area_km2)
  q_breaks <- vapply(seq(1, 6) / 7, function(f) {
    target <- f * total
    idx <- which(cum_area >= target)[[1L]]
    ord$value[[idx]]
  }, numeric(1))
  q_breaks <- round(q_breaks, 2)
  breaks <- c(-Inf, q_breaks, Inf)
  bin_df <- data.frame(bin = 1:7, min_value = breaks[1:7], max_value = breaks[2:8],
                        stringsAsFactors = FALSE) |>
    dplyr::mutate(bin_label = sprintf("%s–%s", format(min_value), format(max_value)))

  r_bins <- terra::classify(r_land_signed,
                             cbind(bin_df$min_value, bin_df$max_value, bin_df$bin),
                             right = FALSE, include.lowest = TRUE)
  zone_areas <- terra::zonal(cell_areas_05, r_bins, fun = "sum", na.rm = TRUE)
  names(zone_areas) <- c("bin", "global_land_area_km2")
  zone_areas <- zone_areas[!is.na(zone_areas$bin), ]
  bin_df |>
    dplyr::left_join(zone_areas, by = "bin") |>
    dplyr::mutate(global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
                  global_land_fraction = global_land_area_km2 / sum(global_land_area_km2))
}
scheme3_global <- build_scheme3() |>
  dplyr::mutate(scheme = "3_signed_septiles", bin = as.character(bin)) |>
  dplyr::select(scheme, bin, bin_label, min_value, max_value, global_land_area_km2, global_land_fraction)

msg("  Scheme 2 half-width breaks:")
for (hw in scheme2_halfwidths) {
  bd <- scheme2_global |> dplyr::filter(half_width == hw)
  msg("    hw=", hw, ": ", paste(round(bd$min_value[is.finite(bd$min_value)], 1), collapse = ", "))
}
msg("  Scheme 3 breaks: ", paste(round(scheme3_global$min_value[is.finite(scheme3_global$min_value)], 1), collapse = ", "))

# ============================================================================
# SECTION 4: Site-level classification (Geo vs Geo, Geo vs Data), all schemes
# ============================================================================
msg("\n=== SECTION 4: Site classification ===")

## Extraction of the new signed raster at each network's site coordinates,
## reusing the coordinates already stored in the existing
## site_trendy_nee_median[_<network>].csv files (built by
## figure_representativeness_trendy_compute.R / extract_historical_sites_representativeness.R)
## rather than re-reading separate site-coordinate files. Nearest-land
## recovery within 3 degrees mirrors extract_sites() /
## figure_representativeness_trendy_compute.R:393-433 and
## extract_with_fallback() / extract_historical_sites_representativeness.R:60-103.
extract_signed_at_sites <- function(sites) {
  coords_mat <- as.matrix(sites[, c("location_long", "location_lat")])
  raw <- terra::extract(r_land_signed, coords_mat, method = "simple")
  vals <- raw[[1]]
  na_idx <- which(is.na(vals))
  if (length(na_idx) > 0L) {
    for (i in na_idx) {
      sx <- coords_mat[i, 1L]; sy <- coords_mat[i, 2L]
      window <- terra::ext(sx - 3, sx + 3, sy - 3, sy + 3)
      r_crop <- terra::crop(r_land_signed, window)
      lv <- terra::values(r_crop)[, 1L]
      ok <- !is.na(lv) & is.finite(lv)
      if (!any(ok)) next
      land_cells <- which(ok)
      land_xy <- terra::xyFromCell(r_crop, land_cells)
      dists <- sqrt((land_xy[, 1L] - sx)^2 + (land_xy[, 2L] - sy)^2)
      best <- which.min(dists)
      vals[i] <- lv[land_cells[[best]]]
    }
  }
  vals
}

site_trendy_signed <- lapply(NET_ORDER, function(net) {
  df <- readr::read_csv(file.path(SNAP, SITE_TRENDY_FILE[[net]]), show_col_types = FALSE)
  df$nee_signed_value <- extract_signed_at_sites(df)
  df$network <- net
  df |> dplyr::select(site_id, location_lat, location_long, network, nee_signed_value)
})
names(site_trendy_signed) <- NET_ORDER
msg("  Extracted signed NEE at all sites, all 4 networks: ",
    paste(sprintf("%s=%d", NET_ORDER, vapply(site_trendy_signed, nrow, integer(1))), collapse = ", "))

site_flux_signed <- lapply(names(FLUX_MEDIANS_FILE), function(net) {
  df <- readr::read_csv(file.path(SNAP, FLUX_MEDIANS_FILE[[net]]), show_col_types = FALSE) |>
    dplyr::mutate(nee_signed_value = -1 * nep_median, network = net) |>
    dplyr::select(site_id, network, nee_signed_value)
  df
})
names(site_flux_signed) <- names(FLUX_MEDIANS_FILE)

## Wide-format bin_df builder: given a "scheme table" (one bin_df per
## scheme/variant, long format as built in Section 3), split into a list of
## bin_dfs keyed by scheme id (and half_width for Scheme 2).
split_scheme <- function(scheme_tbl, key_cols) {
  scheme_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::group_split() |>
    setNames(
      scheme_tbl |> dplyr::distinct(dplyr::across(dplyr::all_of(key_cols))) |>
        dplyr::mutate(key = do.call(paste, c(dplyr::across(dplyr::all_of(key_cols)), sep = "_hw"))) |>
        dplyr::pull(key)
    )
}

all_scheme_bins <- list(
  scheme1 = list("1_absolute_existing" = scheme1_global |>
                   dplyr::mutate(bin = as.integer(bin)) |> dplyr::arrange(bin)),
  scheme2 = setNames(
    lapply(scheme2_halfwidths, function(hw) {
      scheme2_global |> dplyr::filter(half_width == hw) |>
        dplyr::mutate(bin = as.integer(bin)) |> dplyr::arrange(bin)
    }),
    paste0("2_signed_anchored_hw", scheme2_halfwidths)
  ),
  scheme3 = list("3_signed_septiles" = scheme3_global |>
                   dplyr::mutate(bin = as.integer(bin)) |> dplyr::arrange(bin))
)

## For Scheme 1 (absolute), the Geo vs Geo / Geo vs Data classification must
## use the EXISTING abs-valued quantities (trendy_nee_median_value / nee_abs),
## not the new signed raster -- Section 1 already computed these. For Schemes
## 2/3 (signed), classification uses the signed values extracted above.
classify_version <- function(scheme_key, scheme_family, bin_df) {
  is_abs <- scheme_family == "scheme1"
  global_df <- bin_df |> dplyr::transmute(class = as.character(bin), global_land_fraction)

  geo_geo_rows <- lapply(NET_ORDER, function(net) {
    n_tot <- NET_NSITES[[net]]
    if (is_abs) {
      if (net == "current_781") {
        x <- site_trendy_current$trendy_nee_median_value
      } else {
        df <- readr::read_csv(file.path(SNAP, SITE_TRENDY_FILE[[net]]), show_col_types = FALSE)
        x <- df$trendy_nee_median_value
      }
    } else {
      x <- site_trendy_signed[[net]]$nee_signed_value
    }
    bin_assigned <- classify_into_bins(x, bin_df)
    counts <- count_sites(data.frame(bin = as.character(bin_assigned), stringsAsFactors = FALSE),
                           "bin", levels_vec = as.character(bin_df$bin), n_total = n_tot)
    j <- j_for(counts, global_df)
    top2 <- sum(counts$network_frac[counts$class %in% as.character(range(bin_df$bin))])
    data.frame(version = "geo_vs_geo", scheme = scheme_key, network = net, n_sites = n_tot,
               weighted_jaccard = j, outer_two_bin_share = top2)
  })

  geo_data_rows <- lapply(names(FLUX_MEDIANS_FILE), function(net) {
    n_tot <- NET_NSITES[[net]]
    if (is_abs) {
      x <- if (net == "current_781") flux_current$nee_abs else {
        readr::read_csv(file.path(SNAP, FLUX_MEDIANS_FILE[[net]]), show_col_types = FALSE) |>
          dplyr::mutate(nee_abs = abs(nep_median)) |> dplyr::pull(nee_abs)
      }
    } else {
      x <- site_flux_signed[[net]]$nee_signed_value
    }
    bin_assigned <- classify_into_bins(x, bin_df)
    counts <- count_sites(data.frame(bin = as.character(bin_assigned), stringsAsFactors = FALSE),
                           "bin", levels_vec = as.character(bin_df$bin), n_total = n_tot)
    j <- j_for(counts, global_df)
    top2 <- sum(counts$network_frac[counts$class %in% as.character(range(bin_df$bin))])
    data.frame(version = "geo_vs_data", scheme = scheme_key, network = net, n_sites = n_tot,
               weighted_jaccard = j, outer_two_bin_share = top2)
  })

  per_bin_rows <- lapply(NET_ORDER, function(net) {
    n_tot <- NET_NSITES[[net]]
    if (is_abs) {
      x <- if (net == "current_781") site_trendy_current$trendy_nee_median_value else {
        readr::read_csv(file.path(SNAP, SITE_TRENDY_FILE[[net]]), show_col_types = FALSE)$trendy_nee_median_value
      }
    } else x <- site_trendy_signed[[net]]$nee_signed_value
    bin_assigned <- classify_into_bins(x, bin_df)
    counts <- count_sites(data.frame(bin = as.character(bin_assigned), stringsAsFactors = FALSE),
                           "bin", levels_vec = as.character(bin_df$bin), n_total = n_tot)
    counts |> dplyr::mutate(version = "geo_vs_geo", scheme = scheme_key, network = net)
  })
  per_bin_data_rows <- lapply(names(FLUX_MEDIANS_FILE), function(net) {
    n_tot <- NET_NSITES[[net]]
    if (is_abs) {
      x <- if (net == "current_781") flux_current$nee_abs else {
        readr::read_csv(file.path(SNAP, FLUX_MEDIANS_FILE[[net]]), show_col_types = FALSE) |>
          dplyr::mutate(nee_abs = abs(nep_median)) |> dplyr::pull(nee_abs)
      }
    } else x <- site_flux_signed[[net]]$nee_signed_value
    bin_assigned <- classify_into_bins(x, bin_df)
    counts <- count_sites(data.frame(bin = as.character(bin_assigned), stringsAsFactors = FALSE),
                           "bin", levels_vec = as.character(bin_df$bin), n_total = n_tot)
    counts |> dplyr::mutate(version = "geo_vs_data", scheme = scheme_key, network = net)
  })

  list(
    jaccard = dplyr::bind_rows(geo_geo_rows, geo_data_rows),
    sites   = dplyr::bind_rows(per_bin_rows, per_bin_data_rows)
  )
}

results <- list()
for (fam in names(all_scheme_bins)) {
  for (key in names(all_scheme_bins[[fam]])) {
    msg("  Classifying: ", key)
    results[[key]] <- classify_version(key, fam, all_scheme_bins[[fam]][[key]])
  }
}

# ============================================================================
# SECTION 5: Write per-scheme tables and figures
# ============================================================================
msg("\n=== SECTION 5: Writing tables and figures ===")

base_theme <- theme_minimal(base_size = 10) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
        panel.grid.minor = element_blank())

write_scheme_outputs <- function(scheme_num, scheme_keys, global_tbl) {
  jaccard_tbl <- dplyr::bind_rows(lapply(scheme_keys, function(k) results[[k]]$jaccard))
  sites_tbl   <- dplyr::bind_rows(lapply(scheme_keys, function(k) results[[k]]$sites))

  out_global <- file.path(OUTD, sprintf("table_scheme%d_global.csv", scheme_num))
  out_sites  <- file.path(OUTD, sprintf("table_scheme%d_sites.csv", scheme_num))
  out_jacc   <- file.path(OUTD, sprintf("table_scheme%d_jaccard.csv", scheme_num))
  readr::write_csv(global_tbl, out_global)
  readr::write_csv(sites_tbl, out_sites)
  readr::write_csv(jaccard_tbl, out_jacc)
  write_meta(out_global, input_sources = SIGNED_RASTER_PATH, notes = sprintf("Scheme %d global area fraction per bin.", scheme_num))
  write_meta(out_sites, input_sources = c(out_global, unlist(SITE_TRENDY_FILE), unlist(FLUX_MEDIANS_FILE)),
             notes = sprintf("Scheme %d per-bin site counts/fractions, all networks, both versions (geo_vs_geo/geo_vs_data).", scheme_num))
  write_meta(out_jacc, input_sources = c(out_global, out_sites),
             notes = sprintf("Scheme %d weighted Jaccard and outer-two-bin share, all networks, both versions.", scheme_num))

  ## Figure: global area fraction vs. current_781 site fraction per bin,
  ## both versions side by side. Scheme 2 facets by half_width.
  current_sites <- sites_tbl |> dplyr::filter(network == "current_781")
  plot_df <- global_tbl |>
    dplyr::transmute(scheme, dplyr::across(dplyr::any_of("half_width")), bin = as.character(bin), global_land_fraction) |>
    dplyr::left_join(current_sites, by = c("scheme", "bin" = "class")) |>
    tidyr::pivot_longer(c(global_land_fraction, network_frac), names_to = "which", values_to = "fraction") |>
    dplyr::mutate(which = dplyr::recode(which, global_land_fraction = "Global area fraction",
                                         network_frac = "Current network (n=781) site fraction"),
                  bin = factor(bin, levels = as.character(1:7)))

  p <- ggplot(plot_df, aes(x = bin, y = fraction, fill = which)) +
    geom_col(position = "dodge", colour = "black", linewidth = 0.2) +
    { if ("half_width" %in% names(plot_df))
        facet_grid(rows = vars(half_width), cols = vars(version), labeller = label_both)
      else
        facet_grid(cols = vars(version), labeller = label_value)
    } +
    scale_fill_manual(name = NULL, values = c("Global area fraction" = "#009E73",
                                               "Current network (n=781) site fraction" = "#0072B2")) +
    labs(title = sprintf("Scheme %d: global area fraction vs. site fraction per bin", scheme_num),
         x = "Bin", y = "Fraction") +
    base_theme + theme(legend.position = "bottom")
  out_fig <- file.path(OUTD, sprintf("fig_scheme%d_bin_fractions.png", scheme_num))
  ggsave(out_fig, p, width = 9, height = 5.5, dpi = 300, bg = "white")
  msg("  Saved: ", out_fig)

  list(jaccard = jaccard_tbl, sites = sites_tbl, fig = out_fig)
}

out_s1 <- write_scheme_outputs(1, "1_absolute_existing", scheme1_global)
out_s2 <- write_scheme_outputs(2, paste0("2_signed_anchored_hw", scheme2_halfwidths), scheme2_global)
out_s3 <- write_scheme_outputs(3, "3_signed_septiles", scheme3_global)

# ============================================================================
# SECTION 6: Console summary
# ============================================================================
msg("\n=== SUMMARY ===")
for (nm in list(list("Scheme 1", out_s1), list("Scheme 2", out_s2), list("Scheme 3", out_s3))) {
  msg("-- ", nm[[1]], " --")
  print(nm[[2]]$jaccard |> dplyr::select(version, scheme, network, weighted_jaccard, outer_two_bin_share) |>
          dplyr::arrange(version, scheme, match(network, NET_ORDER)))
}

msg("\n=== nee_bin_scheme.R complete ===")
