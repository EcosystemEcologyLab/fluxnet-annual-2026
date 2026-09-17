## scripts/diagnostics/kg_source_consistency.R
##
## Read-and-report diagnostic. Does NOT modify any pipeline script, figure,
## legend, snapshot CSV, or data/snapshots/representativeness_metrics.csv.
## All outputs are written under review/diagnostics/kg_source_consistency/.
##
## BACKGROUND (see review/diagnostics/kg_source_consistency/report.md for the
## full investigation): the 2026-08-20 session log switched the current
## network's Kppen-Geiger (KG) SITE classification (q) from Beck et al.
## (2023) 1 km raster extraction to a locally computed ERA5-based
## classification (data/snapshots/site_koppen_era5.csv), while the GLOBAL
## land-area distribution (p, data/snapshots/koppen_beck2023_global_distribution.csv)
## stayed on the Beck 2023 raster. This script quantifies how much of the
## resulting change in weighted Jaccard (J) is attributable to that source
## switch, independent of (a) sample coverage differences and (b) the
## separate 767->781 network-size change.
##
## site_koppen_beck2023.csv on disk has only 767 rows: it was extracted
## against a hardcoded 767-site snapshot path in
## scripts/step4_extract_koppen_beck2023.R (line 88) that was never updated
## when the network grew to 781 sites. The 781-site network is confirmed
## (below) to be a strict superset of the 767 (767 unchanged + 14 new sites),
## so this script re-extracts Beck 2023 KG class for just the 14 new sites,
## using the same raster and the same extraction method as step4 (exact
## pixel, then buffer-modal fallback, then nearest-land-pixel fallback), and
## merges with the existing 767 rows to build a like-for-like 781-site
## Beck-raster comparison set. This new merged file is written under this
## script's own output directory, not to data/snapshots/.
##
## Three counterfactual variants, all at 2-letter / 5-class / 30-class levels:
##   (a) as built now      : ERA5 site classes            vs Beck global dist.
##   (b) consistent raster : Beck-raster site classes (781) vs Beck global dist.
##   (c) same sample,        Beck-raster site classes, restricted to the same
##       consistent source : 755 sites that have an ERA5 class vs Beck global dist.
##
## All three variants use network_frac = n_in_class / 781 (the full network
## size), matching the convention actually used by the pipeline scripts
## (figure_representativeness_summary.R:279-286 count_sites();
## figure_representativeness_kg.R:107-113 site_fracs() via table()/n_sites) --
## i.e. sites with no class in a given variant are dropped from the numerator
## but NOT from the denominator, diluting that variant's fractions. Holding
## this denominator convention identical across a/b/c isolates: (a) vs (c) =
## classification-source effect only (identical 755-site sample, identical
## 781 denominator); (b) vs (c) = site-coverage effect only (identical Beck
## classification, denominator fixed at 781, membership 781 vs 755).
##
## Outputs -> review/diagnostics/kg_source_consistency/
##   site_koppen_beck2023_current_781.csv   Beck-raster class for all 781 sites
##                                           (767 existing + 14 newly extracted)
##   kg_counterfactual_metrics.csv          J, Hellinger, n_classified per
##                                           axis level x variant (a/b/c)
##   kg_sampling_ratios.csv                 per-class p, q, sampling ratio,
##                                           for every level x variant
##   kg_confusion_twoletter.csv             ERA5 x Beck two-letter confusion
##                                           matrix (755 sites with both)
##   kg_class_shift_drivers.csv             two-letter classes ranked by
##                                           |network_frac(b) - network_frac(a)|,
##                                           with the specific sites driving
##                                           each shift

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(terra)
  library(fs)
})

SNAP <- "data/snapshots"
EXT  <- "data/external"
OUTD <- "review/diagnostics/kg_source_consistency"
fs::dir_create(OUTD)

message("=== kg_source_consistency.R ===")

# ============================================================================
# 1. CONFIRM THE 781-SITE NETWORK IS A STRICT SUPERSET OF THE 767
# ============================================================================

snap_767_path <- file.path(SNAP, "fluxnet_shuttle_snapshot_20260624T095651.csv")
snap_781_path <- file.path(SNAP, "fluxnet_shuttle_snapshot_20260901T094522.csv")

snap_767 <- readr::read_csv(snap_767_path, show_col_types = FALSE)
snap_781 <- readr::read_csv(snap_781_path, show_col_types = FALSE)

ids_767 <- unique(snap_767$site_id)
ids_781 <- unique(snap_781$site_id)

removed <- setdiff(ids_767, ids_781)
added   <- setdiff(ids_781, ids_767)

message("767-snapshot sites: ", length(ids_767))
message("781-snapshot sites: ", length(ids_781))
message("Removed (767 not in 781): ", length(removed),
        if (length(removed) > 0) paste0(" [", paste(removed, collapse = ", "), "]") else "")
message("Added   (781 not in 767): ", length(added),
        " [", paste(sort(added), collapse = ", "), "]")

if (length(removed) > 0L) {
  stop("781-site network is NOT a strict superset of the 767-site network -- ",
       "the site_koppen_beck2023.csv reuse-and-extend approach below is invalid. ",
       "Re-extract the full 781-site Beck2023 KG set instead.", call. = FALSE)
}

# ============================================================================
# 2. BUILD THE 781-SITE BECK-RASTER SITE CLASSIFICATION
#    (767 existing rows + 14 newly extracted rows)
# ============================================================================

beck_767 <- readr::read_csv(file.path(SNAP, "site_koppen_beck2023.csv"),
                             show_col_types = FALSE)
message("\nExisting site_koppen_beck2023.csv: ", nrow(beck_767), " rows")
stopifnot(setequal(beck_767$site_id, ids_767))

new_coords <- snap_781 |>
  dplyr::filter(.data$site_id %in% added) |>
  dplyr::distinct(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))
message("New sites with coordinates to extract: ", nrow(new_coords))
stopifnot(nrow(new_coords) == length(added))

# ---- Legend (same parse as step4_extract_koppen_beck2023.R:42-75) ----------
kg_dir    <- file.path(EXT, "koppen_beck2023")
rast_path <- file.path(kg_dir, "1991_2020", "koppen_geiger_0p00833333.tif")
leg_path  <- file.path(kg_dir, "legend.txt")

leg_lines <- readLines(leg_path)
leg_data  <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]
legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
  m <- regmatches(ln, regexec(
    "^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[", ln, perl = TRUE
  ))[[1]]
  if (length(m) < 4L) return(NULL)
  data.frame(
    koppen_class_code = as.integer(m[2]),
    koppen_class       = trimws(m[3]),
    koppen_class_name  = trimws(m[4]),
    stringsAsFactors   = FALSE
  )
}))
main_map <- c(A = "Tropical", B = "Arid", C = "Temperate", D = "Cold", E = "Polar")
legend_df <- legend_df |>
  dplyr::mutate(
    koppen_main      = substr(koppen_class, 1L, 1L),
    koppen_main_name = main_map[koppen_main]
  )

# ---- Raster extraction: exact pixel, then buffer/nearest-land fallback -----
# (method replicated from scripts/step4_extract_koppen_beck2023.R:101-187;
# same raster file, same fallback stages, applied here only to the 14 new
# sites rather than the full network)
kg_rast <- terra::rast(rast_path)
message("KG raster: ", rast_path,
        " | res=", paste(round(terra::res(kg_rast), 8), collapse = " x "), "deg")

pts <- terra::vect(
  data.frame(x = new_coords$location_long, y = new_coords$location_lat),
  geom = c("x", "y"), crs = "EPSG:4326"
)
kg_raw <- terra::extract(kg_rast, pts, ID = FALSE)
names(kg_raw)[1] <- "koppen_class_code"
new_coords$koppen_class_code <- kg_raw$koppen_class_code
new_coords$koppen_method     <- "exact"

na_idx <- which(is.na(new_coords$koppen_class_code))
message("NA after exact extraction (new sites): ", length(na_idx))
if (length(na_idx) > 0L) {
  buffer_degs <- c(0.01, 0.05, 0.1, 0.25, 0.5)
  int_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(NA_integer_)
    as.integer(names(sort(table(x), decreasing = TRUE))[[1L]])
  }
  still_na <- na_idx
  recovered_any <- logical(length(still_na))
  for (j in seq_along(still_na)) {
    pt_j <- pts[still_na[j], ]
    for (buf in buffer_degs) {
      buf_vals <- terra::extract(kg_rast, pt_j, buffer = buf, ID = TRUE)
      mode_val <- int_mode(buf_vals[[2L]])
      if (!is.na(mode_val)) {
        new_coords$koppen_class_code[still_na[j]] <- mode_val
        new_coords$koppen_method[still_na[j]] <- paste0("buffer_", round(buf * 111, 0), "km")
        recovered_any[j] <- TRUE
        break
      }
    }
  }
  still_na <- still_na[!recovered_any]
  if (length(still_na) > 0L) {
    for (j in seq_along(still_na)) {
      idx <- still_na[j]
      lng <- new_coords$location_long[idx]; lat <- new_coords$location_lat[idx]
      search_ext <- terra::ext(lng - 3, lng + 3, lat - 3, lat + 3)
      local_rast <- terra::crop(kg_rast, search_ext)
      land_pts   <- terra::as.points(local_rast, na.rm = TRUE)
      if (terra::nrow(land_pts) == 0L) next
      pt_j <- pts[idx, ]
      dists <- terra::distance(pt_j, land_pts)
      nearest_idx <- which.min(dists)
      new_coords$koppen_class_code[idx] <- as.integer(terra::values(land_pts)[[1]][nearest_idx])
      new_coords$koppen_method[idx] <- paste0("nearest_land_", round(min(dists) / 1000, 1), "km")
    }
  }
}

new_out <- new_coords |>
  dplyr::left_join(legend_df, by = "koppen_class_code") |>
  dplyr::mutate(koppen_twoletter = substr(koppen_class, 1L, 2L)) |>
  dplyr::select(site_id, location_lat, location_long, koppen_class_code,
                koppen_class, koppen_class_name, koppen_twoletter,
                koppen_main, koppen_main_name, koppen_method)

cat("\n--- Newly extracted sites (Beck 2023 raster) ---\n")
print(as.data.frame(new_out[, c("site_id", "koppen_class", "koppen_method")]))

beck_781 <- dplyr::bind_rows(beck_767, new_out)
stopifnot(nrow(beck_781) == 781L, !anyDuplicated(beck_781$site_id))
n_beck_na <- sum(is.na(beck_781$koppen_class))
message("\nBeck-raster 781-site set: ", nrow(beck_781), " rows, ",
        n_beck_na, " unclassified (NA)")

out_beck_781 <- file.path(OUTD, "site_koppen_beck2023_current_781.csv")
readr::write_csv(beck_781, out_beck_781)
write_output_metadata(
  out_beck_781,
  input_sources = c(snap_767_path, snap_781_path,
                     file.path(SNAP, "site_koppen_beck2023.csv"), rast_path),
  notes = paste0(
    "Diagnostic-only reconstruction for scripts/diagnostics/kg_source_consistency.R. ",
    "767 rows reused verbatim from data/snapshots/site_koppen_beck2023.csv; ",
    "14 new rows (781-site network minus 767-site network) extracted here ",
    "with the same raster and method as scripts/step4_extract_koppen_beck2023.R. ",
    "Not used by any pipeline script; not a replacement for the pinned ",
    "767-site data/snapshots/site_koppen_beck2023.csv."
  )
)
message("Saved: ", out_beck_781)

# ============================================================================
# 3. LOAD ERA5 SITE CLASSES AND THE GLOBAL (BECK) DISTRIBUTION
# ============================================================================

era5_781 <- readr::read_csv(file.path(SNAP, "site_koppen_era5.csv"), show_col_types = FALSE)
stopifnot(nrow(era5_781) == 781L)

global_df <- readr::read_csv(file.path(SNAP, "koppen_beck2023_global_distribution.csv"),
                              show_col_types = FALSE)

n_era5_classified <- sum(!is.na(era5_781$koppen_class))
n_era5_na         <- sum(is.na(era5_781$koppen_class))
message("\nERA5 site classes: ", n_era5_classified, " classified / ",
        n_era5_na, " unclassified (of 781)")

era5_classified_ids <- era5_781 |>
  dplyr::filter(!is.na(koppen_class)) |>
  dplyr::pull(site_id)
stopifnot(length(era5_classified_ids) == 755L)

# ============================================================================
# 4. METRIC HELPERS (identical formulas to figure_representativeness_kg.R:96-104)
# ============================================================================

compute_repr_metrics <- function(p, q) {
  p[is.na(p)] <- 0
  q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

TL_ORDER   <- c("Af","Am","Aw","BS","BW","Cf","Cs","Cw","Df","Ds","Dw","EF","ET")
MAIN_ORDER <- c("A","B","C","D","E")
CLASS_ORDER <- legend_df$koppen_class  # 30-class, code order

N_TOTAL <- 781L  # fixed denominator for all three variants -- see header note

# network_frac for a given site-classification frame, restricted to `member_ids`
# (site_ids allowed to contribute to the numerator), against a fixed N_TOTAL
# denominator. class_col: "koppen_twoletter" | "koppen_main" | "koppen_class"
site_fracs_fixed_denom <- function(df, class_col, levels_vec, member_ids = NULL) {
  d <- df
  if (!is.null(member_ids)) d <- dplyr::filter(d, site_id %in% member_ids)
  d <- dplyr::filter(d, !is.na(.data[[class_col]]))
  counts <- table(factor(d[[class_col]], levels = levels_vec))
  as.numeric(counts) / N_TOTAL
}

p_tl <- vapply(TL_ORDER, function(tl)
  sum(global_df$global_land_fraction[global_df$koppen_twoletter == tl], na.rm = TRUE),
  numeric(1L))
p_5  <- vapply(MAIN_ORDER, function(m)
  sum(global_df$global_land_fraction[global_df$koppen_main == m], na.rm = TRUE),
  numeric(1L))
p_30 <- global_df$global_land_fraction[match(CLASS_ORDER, global_df$koppen_class)]
p_30[is.na(p_30)] <- 0

# ============================================================================
# 5. THREE VARIANTS x THREE LEVELS
# ============================================================================

variants <- list(
  a_era5_as_built = list(
    label = "(a) as built now: ERA5 site classes vs Beck global",
    df = era5_781, member_ids = NULL
  ),
  b_beck_consistent = list(
    label = "(b) consistent raster: Beck-raster site classes (781) vs Beck global",
    df = beck_781, member_ids = NULL
  ),
  c_beck_same_sample = list(
    label = "(c) same sample, consistent source: Beck-raster classes, restricted to the 755 ERA5-classified sites, vs Beck global",
    df = beck_781, member_ids = era5_classified_ids
  )
)

levels_spec <- list(
  twoletter = list(class_col = "koppen_twoletter", levels_vec = TL_ORDER, p = p_tl,  n_classes = 13L),
  class5    = list(class_col = "koppen_main",      levels_vec = MAIN_ORDER, p = p_5, n_classes = 5L),
  class30   = list(class_col = "koppen_class",     levels_vec = CLASS_ORDER, p = p_30, n_classes = 30L)
)

metrics_rows <- list()
ratio_rows   <- list()

for (vname in names(variants)) {
  v <- variants[[vname]]
  n_classified <- v$df |>
    { \(d) if (!is.null(v$member_ids)) dplyr::filter(d, site_id %in% v$member_ids) else d }() |>
    dplyr::filter(!is.na(koppen_class)) |>
    nrow()

  for (lname in names(levels_spec)) {
    ls <- levels_spec[[lname]]
    q <- site_fracs_fixed_denom(v$df, ls$class_col, ls$levels_vec, v$member_ids)
    m <- compute_repr_metrics(ls$p, q)

    metrics_rows[[paste(vname, lname)]] <- data.frame(
      variant             = vname,
      variant_label       = v$label,
      aggregation_level   = lname,
      n_classes           = ls$n_classes,
      n_classified_sites  = n_classified,
      n_total_denominator = N_TOTAL,
      weighted_jaccard     = m$weighted_jaccard,
      hellinger_distance   = m$hellinger_distance,
      stringsAsFactors = FALSE
    )

    ratio_rows[[paste(vname, lname)]] <- data.frame(
      variant           = vname,
      aggregation_level  = lname,
      class              = ls$levels_vec,
      p_global           = ls$p,
      q_network          = q,
      sampling_ratio      = ifelse(ls$p > 0 & q > 0, q / ls$p, NA_real_),
      stringsAsFactors = FALSE
    )
  }
}

metrics_df <- dplyr::bind_rows(metrics_rows) |> dplyr::arrange(aggregation_level, variant)
ratio_df   <- dplyr::bind_rows(ratio_rows)   |> dplyr::arrange(aggregation_level, variant, class)

out_metrics <- file.path(OUTD, "kg_counterfactual_metrics.csv")
readr::write_csv(metrics_df, out_metrics)
write_output_metadata(
  out_metrics,
  input_sources = c(file.path(SNAP, "site_koppen_era5.csv"), out_beck_781,
                     file.path(SNAP, "koppen_beck2023_global_distribution.csv")),
  notes = "Task 4 counterfactual: weighted Jaccard / Hellinger for variants (a)/(b)/(c), 2-letter/5-class/30-class levels. All use a fixed n=781 denominator (see script header)."
)
message("\nSaved: ", out_metrics)

out_ratios <- file.path(OUTD, "kg_sampling_ratios.csv")
readr::write_csv(ratio_df, out_ratios)
write_output_metadata(
  out_ratios,
  input_sources = c(file.path(SNAP, "site_koppen_era5.csv"), out_beck_781,
                     file.path(SNAP, "koppen_beck2023_global_distribution.csv")),
  notes = "Per-class p (global Beck fraction), q (network fraction), sampling ratio, for each of variants (a)/(b)/(c) x levels (twoletter/class5/class30)."
)
message("Saved: ", out_ratios)

cat("\n================ SUMMARY: weighted Jaccard by variant x level ================\n")
print(as.data.frame(
  metrics_df |>
    dplyr::select(aggregation_level, variant, weighted_jaccard, n_classified_sites) |>
    tidyr::pivot_wider(names_from = variant, values_from = c(weighted_jaccard, n_classified_sites))
))

# ============================================================================
# 6. SITE-LEVEL CONFUSION TABLE (ERA5 vs Beck, two-letter, 755 sites with both)
# ============================================================================

confusion_base <- era5_781 |>
  dplyr::filter(site_id %in% era5_classified_ids) |>
  dplyr::select(site_id, era5_twoletter = koppen_twoletter, era5_class = koppen_class) |>
  dplyr::inner_join(
    beck_781 |> dplyr::select(site_id, beck_twoletter = koppen_twoletter, beck_class = koppen_class),
    by = "site_id"
  )
stopifnot(nrow(confusion_base) == 755L)  # all 755 ERA5-classified sites also have a Beck class

confusion_tbl <- confusion_base |>
  dplyr::mutate(
    era5_twoletter = factor(era5_twoletter, levels = TL_ORDER),
    beck_twoletter = factor(beck_twoletter, levels = TL_ORDER)
  ) |>
  dplyr::count(era5_twoletter, beck_twoletter, name = "n_sites") |>
  tidyr::complete(era5_twoletter, beck_twoletter, fill = list(n_sites = 0L))

out_confusion <- file.path(OUTD, "kg_confusion_twoletter.csv")
readr::write_csv(confusion_tbl, out_confusion)
write_output_metadata(
  out_confusion,
  input_sources = c(file.path(SNAP, "site_koppen_era5.csv"), out_beck_781),
  notes = "Site-level two-letter KG confusion matrix, ERA5-local (rows) vs Beck-2023-raster (cols), for the 755 sites classified by both methods."
)
message("Saved: ", out_confusion)

n_agree <- sum(confusion_base$era5_twoletter == confusion_base$beck_twoletter)
message(sprintf("Two-letter agreement (755 comparable sites): %d/%d = %.1f%%",
                n_agree, nrow(confusion_base), 100 * n_agree / nrow(confusion_base)))

# ============================================================================
# 7. CLASSES DRIVING THE (a) vs (b) NETWORK-FRACTION SHIFT
# ============================================================================

q_a <- ratio_df |> dplyr::filter(variant == "a_era5_as_built", aggregation_level == "twoletter") |>
  dplyr::select(class, q_a = q_network)
q_b <- ratio_df |> dplyr::filter(variant == "b_beck_consistent", aggregation_level == "twoletter") |>
  dplyr::select(class, q_b = q_network)

shift_df <- q_a |>
  dplyr::inner_join(q_b, by = "class") |>
  dplyr::mutate(delta_frac = q_b - q_a, delta_n_sites = round(delta_frac * N_TOTAL)) |>
  dplyr::arrange(dplyr::desc(abs(delta_frac)))

cat("\n================ Two-letter classes ranked by |delta network_frac|, (a) vs (b) ================\n")
print(as.data.frame(shift_df))

# For each class in shift_df, identify the specific sites whose ERA5 class
# and Beck class disagree and that "moved into" or "moved out of" that class.
driver_rows <- lapply(seq_len(nrow(shift_df)), function(i) {
  cl <- shift_df$class[i]
  moved_in  <- confusion_base |> dplyr::filter(beck_twoletter == cl, era5_twoletter != cl) |>
    dplyr::mutate(direction = "moved_in_under_beck", target_class = cl)
  moved_out <- confusion_base |> dplyr::filter(era5_twoletter == cl, beck_twoletter != cl) |>
    dplyr::mutate(direction = "moved_out_under_beck", target_class = cl)
  dplyr::bind_rows(moved_in, moved_out) |>
    dplyr::mutate(rank_by_abs_delta = i, delta_frac = shift_df$delta_frac[i])
})
drivers_df <- dplyr::bind_rows(driver_rows) |>
  dplyr::select(rank_by_abs_delta, target_class, delta_frac, direction, site_id,
                era5_twoletter, era5_class, beck_twoletter, beck_class)

out_drivers <- file.path(OUTD, "kg_class_shift_drivers.csv")
readr::write_csv(drivers_df, out_drivers)
write_output_metadata(
  out_drivers,
  input_sources = c(out_ratios, out_confusion),
  notes = "Two-letter classes ranked by |network_frac(b) - network_frac(a)|, with the specific sites whose ERA5 vs Beck classification disagreement drives each class's shift."
)
message("Saved: ", out_drivers)

message("\n=== kg_source_consistency.R complete ===")
