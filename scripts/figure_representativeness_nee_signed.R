## figure_representativeness_nee_signed.R
##
## Production NEE representativeness axis: signed, 5-bin scheme (near-zero
## +/-h, 3 equal-area quantile bins on the sink side beyond -h, 1 open source
## bin above +h). Companion to figure_representativeness_trendy_compute.R
## (which builds the 4 existing unsigned/magnitude TRENDY axes) and
## figure_representativeness_summary.R (which reads the outputs below via its
## AXES6-style registry). Kept as its own script, matching the repo's
## established one-script-per-axis convention (figure_representativeness_kg.R,
## _biomass.R, _aridity.R, _landcover.R), rather than appended to
## trendy_compute.R's linear ~hours-long 17-model netCDF pipeline: the model
## raster this axis needs is already cached (see below), so this script does
## no netCDF processing and runs in seconds.
##
## Model ("Geo"): S3 ensemble-median flux-based NEE (ra+rh-gpp), 1991-2020
##   mean, 17-model TRENDY v14 ensemble. Reuses the cached raster at
##   data/external/trendy/derived/trendy_nee_fluxbased_median.tif, built by
##   scripts/diagnostics/nee_corrected_axis.R Step 1/2 -- NOT recomputed here.
## Tower ("Data"): Step 3 annual method (mean monthly cycle across all
##   QC-qualifying years, all 12 calendar months required, then summed),
##   with the per-site VUT->CUT fallback (CLAUDE.md QC Flag Reference), read
##   from monthly_converted -- correctly unit-converted as of the 2026-09-28
##   fix (see SESSION_LOG.md; this script would have been silently wrong by
##   ~3.7% before that fix).
## h (near-zero half-width): recomputed here with the SAME VUT-only Step 3/4
##   method as scripts/diagnostics/nee_corrected_axis.R (median half-width
##   |NEE_VUT_25-NEE_VUT_75|/2 across current-network sites with a valid
##   VUT annual value) so this script is self-contained and does not depend
##   on that diagnostic's (gitignored) output surviving. h defines the bin
##   scheme; the VUT->CUT tower values classified into it are a broader set
##   than the VUT-only set h itself was computed from.
##
## Networks:
##   current_781: full current network -- both Geo and Data variants (Fig 4
##     material, per task instruction).
##   fluxnet2015 / la_thuile / marconi: each network's historical site list
##     intersected with current_781 (site still active in the current
##     release) -- Data variant only, using CURRENT-RELEASE tower values
##     (Fig 5 material, per task instruction). Denominator for each network's
##     site fraction is that network's FULL historical site count (not the
##     current-network-intersected count), matching the "unclassified sites
##     dropped from the numerator, full network N kept as the denominator"
##     convention established in nee_corrected_axis.R Step 6.
##
## Outputs (data/snapshots/ and data/external/trendy/derived/, both committed):
##   trendy_nee_signed_global_histogram_0.1step.csv -- raw global signed
##     histogram (intermediate, used to build the bin scheme)
##   trendy_nee_signed5_global_distribution.csv -- global land-area fraction
##     per bin (AXES6-style global_df)
##   site_trendy_nee_signed5_geo_current_781.csv -- current_781, model-classified
##   site_trendy_nee_signed5_data_<network>.csv -- one per network, tower-classified
##   nee_signed5_occupancy_jaccard.csv -- variant x network summary (J, bin fractions)
## Appends 2 rows (nee_signed5_geo, nee_signed5_data) for current_781 to
## representativeness_metrics.csv, matching the schema figure_representativeness_
## trendy_compute.R already writes.

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(duckdb)
  library(lubridate)
  library(jsonlite)
})

source("R/pipeline_config.R")
check_pipeline_config()

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

LOG_START <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE  <- file.path("logs", paste0("figure_representativeness_nee_signed_", LOG_START, ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output")
sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== figure_representativeness_nee_signed.R ===")
msg("Log: ", LOG_FILE)

SNAP_DIR    <- "data/snapshots"
DERIVED_DIR <- "data/external/trendy/derived"
KG_PATH     <- "data/external/koppen_beck2023/1991_2020/koppen_geiger_0p5.tif"
MODEL_TIF   <- file.path(DERIVED_DIR, "trendy_nee_fluxbased_median.tif")
SITE_CSV    <- file.path(SNAP_DIR, "site_biomass_cci_v7.csv")
METRICS_CSV <- file.path(SNAP_DIR, "representativeness_metrics.csv")
QC_THRESH_MM <- 0.80

if (!file.exists(MODEL_TIF)) {
  stop("Model raster not found: ", MODEL_TIF, ". Run scripts/diagnostics/nee_corrected_axis.R ",
       "(Steps 1-2) first to build it.")
}

network_files <- list(
  current_781 = SITE_CSV,
  fluxnet2015 = file.path(SNAP_DIR, "sites_fluxnet2015_clean.csv"),
  la_thuile   = file.path(SNAP_DIR, "sites_la_thuile_clean.csv"),
  marconi     = file.path(SNAP_DIR, "sites_marconi_clean.csv")
)
current_sites <- read_csv(SITE_CSV, show_col_types = FALSE) |>
  select(site_id, location_lat, location_long) |> distinct(site_id, .keep_all = TRUE)
n_sites <- nrow(current_sites)
msg(n_sites, " current-network sites from ", basename(SITE_CSV))

network_full_n <- vapply(network_files, function(f) nrow(read_csv(f, show_col_types = FALSE)), integer(1))
msg("Network full site counts (denominators): ",
    paste(names(network_full_n), network_full_n, sep = "=", collapse = "  "))

## =====================================================================
## STEP 1: Model side ("Geo") -- extract cached ensemble-median raster
## =====================================================================
msg("\n=== STEP 1: Model (Geo) extraction ===")

r_model <- rast(MODEL_TIF)
kg_05   <- rast(KG_PATH)

geo_coords <- as.matrix(current_sites[, c("location_long", "location_lat")])
geo_vals   <- terra::extract(r_model, geo_coords, method = "bilinear")[, 1]
geo_site_df <- current_sites |>
  mutate(nee_signed_model = geo_vals) |>
  filter(!is.na(nee_signed_model))
msg("Model NEE extracted for ", nrow(geo_site_df), " / ", n_sites, " current-network sites.")

## =====================================================================
## STEP 2: Tower side ("Data") -- VUT->CUT fallback, Step 3 annual method
## =====================================================================
msg("\n=== STEP 2: Tower (Data) annual NEE, VUT->CUT fallback ===")

site_ids_sql <- paste(sprintf("'%s'", current_sites$site_id), collapse = ", ")
con <- dbConnect(duckdb::duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
monthly_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP,
         NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_25, NEE_VUT_75,
         NEE_CUT_REF, NEE_CUT_REF_QC
  FROM monthly_converted
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", site_ids_sql))
dbDisconnect(con, shutdown = TRUE)

monthly_raw <- monthly_raw |>
  mutate(TIMESTAMP = as.Date(TIMESTAMP), year = year(TIMESTAMP), month = month(TIMESTAMP))

## ---- Per-site VUT/CUT decision (NEE_*_QC presence only), same convention
## as scripts/diagnostics/flux_tower_model_distributions.R ---------------------
site_carbon_src <- monthly_raw |>
  group_by(site_id) |>
  summarise(any_vut_qc = any(!is.na(NEE_VUT_REF_QC)),
            any_cut_qc = any(!is.na(NEE_CUT_REF_QC)), .groups = "drop") |>
  mutate(carbon_src = case_when(any_vut_qc ~ "VUT", any_cut_qc ~ "CUT", TRUE ~ NA_character_))
n_vut <- sum(site_carbon_src$carbon_src == "VUT", na.rm = TRUE)
n_cut <- sum(site_carbon_src$carbon_src == "CUT", na.rm = TRUE)
msg("Per-site carbon source decision: VUT=", n_vut, "  CUT (fallback)=", n_cut,
    "  neither=", sum(is.na(site_carbon_src$carbon_src)))

monthly_raw <- monthly_raw |> left_join(site_carbon_src |> select(site_id, carbon_src), by = "site_id") |>
  mutate(
    nee_val = if_else(carbon_src == "VUT", NEE_VUT_REF, NEE_CUT_REF),
    nee_qc  = if_else(carbon_src == "VUT", NEE_VUT_REF_QC, NEE_CUT_REF_QC),
    nee_qualifies = !is.na(carbon_src) & !is.na(nee_qc) & nee_qc >= QC_THRESH_MM & !is.na(nee_val),
    # monthly_converted's carbon columns are already gC m-2 month-1 totals
    # (05_units.R's daily-rate x days-in-month conversion) -- use directly.
    nee_gC = if_else(nee_qualifies, nee_val, NA_real_)
  )

## ---- Annual construction: mean monthly cycle (all qualifying years), summed
build_annual <- function(df, value_col) {
  cyc <- df |>
    filter(!is.na(.data[[value_col]])) |>
    group_by(site_id, month) |>
    summarise(mean_month = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  all12 <- cyc |> group_by(site_id) |> summarise(n_months = n(), .groups = "drop") |>
    filter(n_months == 12L) |> pull(site_id)
  n_years <- df |> filter(!is.na(.data[[value_col]]), site_id %in% all12) |>
    distinct(site_id, year) |> count(site_id, name = "n_years")
  cyc |> filter(site_id %in% all12) |>
    group_by(site_id) |> summarise(tower_value = sum(mean_month), .groups = "drop") |>
    left_join(n_years, by = "site_id")
}

data_site_df <- build_annual(monthly_raw, "nee_gC") |>
  rename(nee_signed_tower = tower_value) |>
  left_join(current_sites, by = "site_id") |>
  left_join(site_carbon_src |> select(site_id, tower_carbon_src = carbon_src), by = "site_id")
msg("Tower annual NEE (VUT->CUT fallback), all 12 months, current-network sites: ", nrow(data_site_df))

## ---- h: VUT-only Step 3/4 (self-contained, matches nee_corrected_axis.R) ---
monthly_vut <- monthly_raw |>
  mutate(
    vut_qualifies = !is.na(NEE_VUT_REF_QC) & NEE_VUT_REF_QC >= QC_THRESH_MM & !is.na(NEE_VUT_REF),
    NEE_VUT_REF_gC = if_else(vut_qualifies, NEE_VUT_REF, NA_real_),
    NEE_VUT_25_gC  = if_else(vut_qualifies, NEE_VUT_25,  NA_real_),
    NEE_VUT_75_gC  = if_else(vut_qualifies, NEE_VUT_75,  NA_real_)
  )
vut_ref <- build_annual(monthly_vut, "NEE_VUT_REF_gC") |> rename(nee_annual_ref = tower_value)
vut_25  <- build_annual(monthly_vut, "NEE_VUT_25_gC")  |> rename(nee_annual_25  = tower_value)
vut_75  <- build_annual(monthly_vut, "NEE_VUT_75_gC")  |> rename(nee_annual_75  = tower_value)
h_site_df <- vut_ref |> select(site_id, nee_annual_ref) |>
  inner_join(vut_25 |> select(site_id, nee_annual_25), by = "site_id") |>
  inner_join(vut_75 |> select(site_id, nee_annual_75), by = "site_id") |>
  mutate(half_width_site = abs(nee_annual_75 - nee_annual_25) / 2)

H_HALFWIDTH <- median(h_site_df$half_width_site, na.rm = TRUE)
msg("h (median half-width, VUT-only, n=", nrow(h_site_df), "): ",
    round(H_HALFWIDTH, 3), " gC m-2 yr-1")

## =====================================================================
## STEP 3: Global signed histogram + 5-bin scheme
## =====================================================================
msg("\n=== STEP 3: Global signed histogram + 5-bin scheme (h=", round(H_HALFWIDTH, 2), ") ===")

msg("Computing cell areas (0.5 deg) ...")
cell_areas_05 <- cellSize(kg_05, mask = TRUE, unit = "km")

## Reused verbatim from scripts/diagnostics/nee_corrected_axis.R Step 2
## (build_global_hist(); catch-all bins retained, not dropped, so the total
## area always equals the full land mask total).
build_global_hist <- function(r_map, kg_mask, cell_areas, step = 0.1, hist_max = 1000) {
  r_land <- mask(r_map, kg_mask)
  lo <- seq(-hist_max, hist_max - step, by = step)
  hi <- lo + step
  ids <- seq_along(lo)
  catch_lo_id <- 0L
  catch_hi_id <- max(ids) + 1L
  rcl <- rbind(cbind(lo, hi, as.numeric(ids)),
               c(-1e9, -hist_max, catch_lo_id),
               c(hist_max, 1e9, catch_hi_id))
  r_hist <- classify(r_land, rcl, right = FALSE, include.lowest = TRUE)
  areas <- zonal(cell_areas, r_hist, fun = "sum", na.rm = TRUE)
  names(areas) <- c("bin_id", "area_km2")
  areas <- areas[!is.na(areas$bin_id), ]
  bin_lo_vec <- c(catch_lo_id = -hist_max, lo, catch_hi_id = hist_max)
  names(bin_lo_vec) <- as.character(c(catch_lo_id, ids, catch_hi_id))
  areas$value <- bin_lo_vec[as.character(areas$bin_id)]
  areas[order(areas$value), c("value", "area_km2")]
}

global_hist <- build_global_hist(r_model, kg_05, cell_areas_05)
hist_out <- file.path(DERIVED_DIR, "trendy_nee_signed_global_histogram_0.1step.csv")
write_csv(global_hist, hist_out)
write_meta(hist_out, input_sources = MODEL_TIF,
           notes = "0.1 gC m-2 yr-1 step global signed histogram of trendy_nee_fluxbased_median.tif, masked by the Koppen-Geiger land mask. Intermediate for the 5-bin scheme below.")

## 5-bin scheme: near-zero +/-h, 3 equal-area sink quantile bins beyond -h,
## 1 open source bin above +h (no source-side quantile split, unlike the
## 7-bin diagnostic scheme in nee_corrected_axis.R Step 5).
make_signed_bins_5 <- function(hist_df, h) {
  sink_side  <- hist_df[hist_df$value < -h, ]
  sink_side  <- sink_side[order(-sink_side$value), ]  # closest to -h first
  cum_sink   <- cumsum(sink_side$area_km2)
  total_sink <- sum(sink_side$area_km2)
  sink_breaks <- sort(vapply(c(1, 2) / 3, function(f) {
    idx <- which(cum_sink >= f * total_sink)[1L]
    sink_side$value[idx]
  }, numeric(1)))
  list(sink_breaks = sink_breaks, total_sink_area = total_sink)
}

bins_info <- make_signed_bins_5(global_hist, H_HALFWIDTH)
BREAKS <- c(-Inf, bins_info$sink_breaks, -H_HALFWIDTH, H_HALFWIDTH, Inf)
msg("Bin edges (gC m-2 yr-1): ", paste(round(BREAKS, 2), collapse = " | "))

classify_signed5 <- function(x, breaks) {
  b <- findInterval(x, breaks[-length(breaks)], left.open = FALSE)
  b[b < 1L] <- 1L
  b[b > 5L] <- 5L
  as.integer(b)
}

total_land_area <- sum(global_hist$area_km2)
global_hist$bin <- classify_signed5(global_hist$value, BREAKS)
BIN_LABELS <- c(
  sprintf("sink, outer third (< %.1f)", bins_info$sink_breaks[1]),
  sprintf("sink, middle third (%.1f to %.1f)", bins_info$sink_breaks[1], bins_info$sink_breaks[2]),
  sprintf("sink, inner third (%.1f to -%.1f)", bins_info$sink_breaks[2], H_HALFWIDTH),
  sprintf("near-zero (+/-%.1f)", H_HALFWIDTH),
  sprintf("source (> %.1f)", H_HALFWIDTH)
)
global_bin_frac <- global_hist |> group_by(bin) |> summarise(area_km2 = sum(area_km2), .groups = "drop") |>
  mutate(global_land_fraction = area_km2 / total_land_area,
         bin_label = BIN_LABELS[bin]) |>
  arrange(bin) |>
  select(bin, bin_label, area_km2, global_land_fraction)

dist_out <- file.path(SNAP_DIR, "trendy_nee_signed5_global_distribution.csv")
write_csv(global_bin_frac, dist_out)
write_meta(dist_out, input_sources = hist_out,
           notes = paste0("5-bin signed NEE scheme. h=", round(H_HALFWIDTH, 3),
                           " gC m-2 yr-1 (VUT-only Step 3/4 median half-width). Bin edges: ",
                           paste(round(BREAKS, 3), collapse = ", "), "."))
msg("Global area fraction per bin:")
print(as.data.frame(global_bin_frac))

## =====================================================================
## STEP 4: Classify sites, build occupancy/Jaccard table per network x variant
## =====================================================================
msg("\n=== STEP 4: Site classification + occupancy/Jaccard ===")

geo_site_df$nee_signed5_bin  <- classify_signed5(geo_site_df$nee_signed_model, BREAKS)
data_site_df$nee_signed5_bin <- classify_signed5(data_site_df$nee_signed_tower, BREAKS)

write_csv(geo_site_df, file.path(SNAP_DIR, "site_trendy_nee_signed5_geo_current_781.csv"))
write_meta(file.path(SNAP_DIR, "site_trendy_nee_signed5_geo_current_781.csv"),
           input_sources = c(MODEL_TIF, SITE_CSV),
           notes = "Model (Geo) NEE, bilinear-extracted at current-network site coordinates, classified into the 5-bin signed scheme.")

site_fracs <- function(bins, n_total) {
  tab <- table(factor(bins, levels = 1:5))
  as.numeric(tab) / n_total
}
weighted_jaccard <- function(p, q) sum(pmin(p, q)) / sum(pmax(p, q))
global_frac_vec <- global_bin_frac$global_land_fraction[match(1:5, global_bin_frac$bin)]

## Historical network site lists (all sites of that historical dataset;
## used only to define membership -- values always come from the current
## Shuttle release, per task instruction).
hist_site_lists <- lapply(network_files[c("fluxnet2015", "la_thuile", "marconi")], function(f) {
  read_csv(f, show_col_types = FALSE) |> select(site_id)
})

occupancy_rows <- list()

## ---- current_781: Geo vs Geo AND Geo vs Data (Fig 4 material) -------------
fr_geo <- site_fracs(geo_site_df$nee_signed5_bin, n_sites)
j_geo  <- weighted_jaccard(global_frac_vec, fr_geo)
occupancy_rows[["geo_current_781"]] <- data.frame(
  variant = "geo_vs_geo", network = "current_781", n_total = n_sites,
  n_classified = nrow(geo_site_df), bin = 1:5, bin_label = BIN_LABELS,
  site_frac = fr_geo, global_frac = global_frac_vec,
  weighted_jaccard = j_geo
)

fr_data_current <- site_fracs(data_site_df$nee_signed5_bin, n_sites)
j_data_current  <- weighted_jaccard(global_frac_vec, fr_data_current)
occupancy_rows[["data_current_781"]] <- data.frame(
  variant = "geo_vs_data", network = "current_781", n_total = n_sites,
  n_classified = nrow(data_site_df), bin = 1:5, bin_label = BIN_LABELS,
  site_frac = fr_data_current, global_frac = global_frac_vec,
  weighted_jaccard = j_data_current
)
write_csv(data_site_df,
          file.path(SNAP_DIR, "site_trendy_nee_signed5_data_current_781.csv"))
write_meta(file.path(SNAP_DIR, "site_trendy_nee_signed5_data_current_781.csv"),
           input_sources = c("data/duckdb/fluxnet.duckdb (monthly_converted)", SITE_CSV),
           notes = paste0("Tower (Data) annual NEE, Step 3 method (mean monthly cycle, all 12 months, ",
                           "QC>=", QC_THRESH_MM, "), per-site VUT->CUT fallback. Classified into the ",
                           "5-bin signed scheme (h=", round(H_HALFWIDTH, 3), " gC m-2 yr-1)."))

## ---- Historical networks: Geo vs Data only, current-release data for
## sites still in the current network (Fig 5 material) -----------------------
for (net in c("fluxnet2015", "la_thuile", "marconi")) {
  hist_ids <- hist_site_lists[[net]]$site_id
  n_total  <- network_full_n[[net]]
  net_site_df <- data_site_df |> filter(site_id %in% hist_ids)
  fr <- site_fracs(net_site_df$nee_signed5_bin, n_total)
  j  <- weighted_jaccard(global_frac_vec, fr)
  msg(net, ": ", length(hist_ids), " historical sites, ", nrow(net_site_df),
      " still in current network with a qualifying VUT/CUT annual NEE ",
      "(denominator = ", n_total, ", the full historical network).")
  occupancy_rows[[paste0("data_", net)]] <- data.frame(
    variant = "geo_vs_data", network = net, n_total = n_total,
    n_classified = nrow(net_site_df), bin = 1:5, bin_label = BIN_LABELS,
    site_frac = fr, global_frac = global_frac_vec,
    weighted_jaccard = j
  )
  write_csv(net_site_df, file.path(SNAP_DIR, paste0("site_trendy_nee_signed5_data_", net, ".csv")))
  write_meta(file.path(SNAP_DIR, paste0("site_trendy_nee_signed5_data_", net, ".csv")),
             input_sources = c("data/duckdb/fluxnet.duckdb (monthly_converted)", network_files[[net]], SITE_CSV),
             notes = paste0(net, " historical site list (", length(hist_ids), " sites) intersected with the ",
                             "current network; tower (Data) values are current-release Shuttle data, not the ",
                             net, " product's own values, per Hard Rule 1."))
}

occupancy_df <- bind_rows(occupancy_rows)
occ_out <- file.path(SNAP_DIR, "nee_signed5_occupancy_jaccard.csv")
write_csv(occupancy_df, occ_out)
write_meta(occ_out,
           input_sources = c(dist_out, "data/duckdb/fluxnet.duckdb (monthly_converted)", unlist(network_files)),
           notes = paste0("variant x network occupancy and weighted Jaccard for the 5-bin signed NEE scheme. ",
                           "geo_vs_geo (current_781 only): model NEE at site coords vs global model distribution. ",
                           "geo_vs_data: tower-measured annual NEE (VUT->CUT fallback) vs the same global model ",
                           "distribution. Historical networks restricted to sites still active in the current ",
                           "release; denominator is each network's full historical site count."))
msg("\nOccupancy/Jaccard summary:")
print(occupancy_df |> distinct(variant, network, n_total, n_classified, weighted_jaccard))

## =====================================================================
## STEP 5: Append to representativeness_metrics.csv
## =====================================================================
msg("\n=== STEP 5: Append representativeness_metrics.csv ===")

hellinger <- function(p, q) (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))

existing_metrics <- if (file.exists(METRICS_CSV)) {
  readr::read_csv(METRICS_CSV, show_col_types = FALSE) |>
    dplyr::filter(!(axis %in% c("nee_signed5_geo", "nee_signed5_data") &
                    aggregation_level == "5bin_signed"))
} else {
  data.frame(axis = character(), aggregation_level = character(),
             n_classes = integer(), weighted_jaccard = numeric(),
             hellinger_distance = numeric(), network = character(),
             n_sites = integer())
}

new_rows <- bind_rows(
  data.frame(axis = "nee_signed5_geo", aggregation_level = "5bin_signed", n_classes = 5L,
             weighted_jaccard = j_geo, hellinger_distance = hellinger(global_frac_vec, fr_geo),
             network = "current_781", n_sites = n_sites),
  data.frame(axis = "nee_signed5_data", aggregation_level = "5bin_signed", n_classes = 5L,
             weighted_jaccard = j_data_current, hellinger_distance = hellinger(global_frac_vec, fr_data_current),
             network = "current_781", n_sites = n_sites)
)
metrics_final <- bind_rows(existing_metrics, new_rows)
write_csv(metrics_final, METRICS_CSV)
msg("Saved metrics: ", METRICS_CSV)
print(as.data.frame(new_rows))

msg("\n=== figure_representativeness_nee_signed.R complete ===")
