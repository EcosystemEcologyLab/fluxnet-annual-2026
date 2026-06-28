## extract_historical_sites_representativeness.R
##
## Extract per-site representativeness classifications for three historical
## FLUXNET networks (Marconi, La Thuile, FLUXNET2015) across all 11
## representativeness axes used in the current-network (767-site) analysis.
##
## Axes:
##   KG present-day (Beck 2023 1991-2020): 30-class, 13-class, 5-class
##   KG SSP2-4.5 2041-2070: same three aggregations
##   KG SSP5-8.5 2071-2099: same three aggregations
##   Aridity UNEP 5-class + 7-class (CGIAR v3.1)
##   Biomass CCI v7 7-bin hybrid (canonical breakpoints: 5,13,27,51,94,171 Mg/ha)
##   LULC 10-class high-level, 22-class level2, 37-class native (ESA CCI 2022)
##   TRENDY v14 NEE-IAV, NEE-median, ET-IAV, ET-median (7-bin hybrid, canonical)
##
## Outputs per network (marconi / la_thuile / fluxnet2015):
##   data/snapshots/site_<axis>_<network>.csv + .meta.json (10 site CSVs × 3 nets)
##   data/snapshots/representativeness_metrics.csv updated with network column
##
## Column structure of site CSVs matches the corresponding current-network CSV.
## Global distributions are reused from disk — NOT recomputed.

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(jsonlite)
})

SNAP_DIR <- "data/snapshots"
EXT_DIR  <- "data/external"

LOG_FILE <- file.path("logs",
  paste0("extract_historical_representativeness_",
         format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
con <- file(LOG_FILE, open = "wt")
sink(con, type = "output")
sink(con, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con) }, add = TRUE)

msg <- function(...) message(format(Sys.time(), "[%H:%M:%S]"), " ", ...)

msg("=== Historical sites representativeness extraction ===")
msg("Log: ", LOG_FILE)

# ---- Helper: write .meta.json -----------------------------------------------
write_meta <- function(out_path, input_sources, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = system("git rev-parse --short HEAD", intern = TRUE),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(out_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

# ---- Helper: extract + nearest-land recovery (within 3°) -------------------
extract_with_fallback <- function(r_map, sites) {
  pts <- terra::vect(
    data.frame(x = sites$location_long, y = sites$location_lat),
    geom = c("x", "y"), crs = "EPSG:4326"
  )
  raw     <- terra::extract(r_map, pts, ID = FALSE)
  vals    <- raw[[1]]
  methods <- rep("exact", nrow(sites))

  na_idx <- which(is.na(vals) | !is.finite(vals))
  if (length(na_idx) > 0L) {
    msg("    Nearest-land recovery for ", length(na_idx), " NA site(s) ...")
    for (i in na_idx) {
      sx <- sites$location_long[i]
      sy <- sites$location_lat[i]
      window  <- terra::ext(sx - 3, sx + 3, sy - 3, sy + 3)
      r_crop  <- terra::crop(r_map, window)
      lv      <- terra::values(r_crop)[, 1L]
      ok      <- !is.na(lv) & is.finite(lv)
      if (!any(ok)) {
        methods[i] <- "no_land_within_3deg"
        msg("      ", sites$site_id[i], ": no land within 3° — remains NA")
        next
      }
      land_cells <- which(ok)
      land_xy    <- terra::xyFromCell(r_crop, land_cells)
      dists      <- sqrt((land_xy[, 1L] - sx)^2 + (land_xy[, 2L] - sy)^2)
      best       <- which.min(dists)
      vals[i]    <- lv[land_cells[[best]]]
      methods[i] <- sprintf("nearest_land_%.3fdeg", dists[[best]])
      msg("      ", sites$site_id[i], ": recovered at ",
          round(dists[[best]], 3), "° (val=", round(vals[i], 4), ")")
    }
  }

  data.frame(
    site_id      = sites$site_id,
    location_lat  = sites$location_lat,
    location_long = sites$location_long,
    value         = vals,
    method        = methods,
    stringsAsFactors = FALSE
  )
}

# ---- Helper: compute weighted Jaccard and Hellinger -------------------------
compute_repr <- function(p, q) {
  p[is.na(p)] <- 0
  q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

# ---- Helper: align two named numeric vectors by keys, compute metrics ------
metrics_from_tables <- function(p_named, q_named,
                                axis, agg_level, n_classes, network, n_sites) {
  all_k <- unique(c(names(p_named), names(q_named)))
  pv    <- as.numeric(p_named[all_k]); pv[is.na(pv)] <- 0
  qv    <- as.numeric(q_named[all_k]); qv[is.na(qv)] <- 0
  m     <- compute_repr(pv, qv)
  data.frame(
    axis               = axis,
    aggregation_level  = agg_level,
    n_classes          = as.integer(n_classes),
    weighted_jaccard   = m$weighted_jaccard,
    hellinger_distance = m$hellinger_distance,
    network            = network,
    n_sites            = as.integer(n_sites),
    stringsAsFactors   = FALSE
  )
}

# ---- Step 1: Verify and load historical site CSVs ---------------------------
msg("\n=== Step 1: Load historical site CSVs ===")

network_defs <- list(
  marconi = list(
    csv   = file.path(SNAP_DIR, "sites_marconi_clean.csv"),
    n_exp = 35L
  ),
  la_thuile = list(
    csv   = file.path(SNAP_DIR, "sites_la_thuile_clean.csv"),
    n_exp = 252L
  ),
  fluxnet2015 = list(
    csv   = file.path(SNAP_DIR, "sites_fluxnet2015_clean.csv"),
    n_exp = 212L
  )
)

networks <- list()
for (nm in names(network_defs)) {
  def <- network_defs[[nm]]
  if (!file.exists(def$csv)) stop("Missing: ", def$csv)

  raw <- readr::read_csv(def$csv, show_col_types = FALSE)
  msg(nm, ": ", nrow(raw), " rows | cols: ", paste(names(raw), collapse = ", "))

  # Harmonize lat/lon names to location_lat / location_long
  rn <- names(raw)
  if ("latitude"  %in% rn) raw <- dplyr::rename(raw, location_lat  = latitude)
  if ("longitude" %in% rn) raw <- dplyr::rename(raw, location_long = longitude)
  if ("lat"       %in% rn) raw <- dplyr::rename(raw, location_lat  = lat)
  if ("lon"       %in% rn) raw <- dplyr::rename(raw, location_long = lon)
  if ("long"      %in% rn) raw <- dplyr::rename(raw, location_long = long)

  req <- c("site_id", "location_lat", "location_long")
  if (!all(req %in% names(raw))) {
    stop(nm, " missing columns: ", paste(setdiff(req, names(raw)), collapse=", "))
  }

  n_na <- sum(is.na(raw$location_lat) | is.na(raw$location_long))
  if (n_na > 0L) msg("  WARNING: ", n_na, " row(s) with NA coordinates — excluded")

  sites <- raw |>
    dplyr::select(site_id, location_lat, location_long) |>
    dplyr::filter(!is.na(location_lat), !is.na(location_long))

  msg("  Sites with valid coords: ", nrow(sites),
      " (expected ~", def$n_exp, ")")

  networks[[nm]] <- list(sites = sites, n = nrow(sites))
}

msg("All historical CSVs verified.")

# ---- Step 2: Load rasters (once, shared across all networks) ----------------
msg("\n=== Step 2: Load rasters ===")

kg_path     <- file.path(EXT_DIR, "koppen_beck2023", "1991_2020",
                          "koppen_geiger_0p00833333.tif")
ssp245_path <- file.path(EXT_DIR, "koppen_beck2023", "2041_2070", "ssp245",
                          "koppen_geiger_0p00833333.tif")
ssp585_path <- file.path(EXT_DIR, "koppen_beck2023", "2071_2099", "ssp585",
                          "koppen_geiger_0p00833333.tif")
ai_path     <- file.path(EXT_DIR, "aridity", "Global-AI_ET0__annual_v3_1",
                          "ai_v31_yr.tif")
bio_path    <- file.path(EXT_DIR, "cci_biomass",
                          "ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif")
lulc_path   <- file.path(EXT_DIR, "cci_landcover", "v2.1.1",
                          "cci_lc_2022_kg_aligned_native.tif")

for (p in c(kg_path, ssp245_path, ssp585_path, ai_path, bio_path, lulc_path)) {
  if (!file.exists(p)) stop("Raster not found: ", p)
}

kg_rast  <- terra::rast(kg_path)
kg_245   <- terra::rast(ssp245_path)
kg_585   <- terra::rast(ssp585_path)
ai_rast  <- terra::rast(ai_path)
bio_rast <- terra::rast(bio_path)[[18L]]   # band 18 = merged AGB estimate
lulc_rast <- terra::rast(lulc_path)

msg("KG rasters: present-day + SSP2-4.5 + SSP5-8.5 loaded")
msg("Aridity raster loaded")
msg("Biomass raster loaded (band 18)")
msg("LULC raster loaded")

trendy_keys  <- c("nee_iav", "et_iav", "nee_median", "et_median")
trendy_paths <- setNames(
  file.path(EXT_DIR, "trendy", "derived",
            paste0("trendy_", trendy_keys, ".tif")),
  trendy_keys
)
for (p in trendy_paths) if (!file.exists(p)) stop("Raster not found: ", p)
trendy_rasts <- lapply(trendy_paths, terra::rast)
msg("TRENDY derived rasters loaded: ", paste(trendy_keys, collapse=", "))

# ---- Load KG legend ---------------------------------------------------------
leg_path  <- file.path(EXT_DIR, "koppen_beck2023", "legend.txt")
leg_lines <- readLines(leg_path)
leg_data  <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]
legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
  m <- regmatches(ln, regexec(
    "^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[", ln, perl = TRUE
  ))[[1]]
  if (length(m) < 4L) return(NULL)
  data.frame(koppen_class_code = as.integer(m[2]),
             koppen_class      = trimws(m[3]),
             koppen_class_name = trimws(m[4]),
             stringsAsFactors  = FALSE)
}))
main_map <- c(A="Tropical", B="Arid", C="Temperate", D="Cold", E="Polar")
legend_df <- legend_df |>
  dplyr::mutate(
    koppen_twoletter = substr(koppen_class, 1L, 2L),
    koppen_main      = substr(koppen_class, 1L, 1L),
    koppen_main_name = main_map[koppen_main]
  )
msg("KG legend loaded: ", nrow(legend_df), " classes")

# ---- Load global distributions (canonical — NOT recomputed) -----------------
msg("\n=== Loading canonical global distributions ===")

read_glob <- function(fname) {
  path <- file.path(SNAP_DIR, fname)
  if (!file.exists(path)) stop("Global distribution missing: ", path)
  readr::read_csv(path, show_col_types = FALSE)
}

glob_kg_pres   <- read_glob("koppen_beck2023_global_distribution.csv")
glob_kg_245    <- read_glob("koppen_beck2023_ssp245_2041_2070_global_distribution.csv")
glob_kg_585    <- read_glob("koppen_beck2023_ssp585_2071_2099_global_distribution.csv")
glob_ai5       <- read_glob("aridity_unep5_global_distribution.csv")
glob_ai7       <- read_glob("aridity_unep7_global_distribution.csv")
glob_bio       <- read_glob("biomass_cci_v7_global_distribution.csv")
glob_lulc_hl   <- read_glob("landcover_cci_highlevel_global_distribution.csv")
glob_lulc_l2   <- read_glob("landcover_cci_level2_global_distribution.csv")
glob_lulc_nat  <- read_glob("landcover_cci_native_global_distribution.csv")
glob_tr_nee_iav    <- read_glob("trendy_nee_iav_global_distribution.csv")
glob_tr_et_iav     <- read_glob("trendy_et_iav_global_distribution.csv")
glob_tr_nee_med    <- read_glob("trendy_nee_median_global_distribution.csv")
glob_tr_et_med     <- read_glob("trendy_et_median_global_distribution.csv")

msg("All global distributions loaded.")

# ---- Load LULC aggregation lookup -------------------------------------------
lulc_lookup <- readr::read_csv(
  file.path(SNAP_DIR, "cci_landcover_aggregation_lookup.csv"),
  show_col_types = FALSE
)

# ---- Classification helpers --------------------------------------------------

classify_aridity <- function(raw_vals) {
  ai <- raw_vals * 0.0001   # CGIAR v3.1 scale factor
  ai[ai == 0 | !is.finite(ai)] <- NA_real_   # ocean / no-data
  list(
    ai_value = ai,
    unep_class_5 = dplyr::case_when(
      is.na(ai)  ~ NA_character_,
      ai < 0.05  ~ "Hyper-Arid",
      ai < 0.20  ~ "Arid",
      ai < 0.50  ~ "Semi-Arid",
      ai < 0.65  ~ "Dry Sub-Humid",
      TRUE       ~ "Humid"
    ),
    unep_class_7 = dplyr::case_when(
      is.na(ai)  ~ NA_character_,
      ai < 0.05  ~ "Hyper-Arid",
      ai < 0.20  ~ "Arid",
      ai < 0.50  ~ "Semi-Arid",
      ai < 0.65  ~ "Dry Sub-Humid",
      ai < 1.00  ~ "Humid (low)",
      ai < 2.00  ~ "Humid (moderate)",
      TRUE       ~ "Hyper-Humid"
    )
  )
}

# Canonical biomass breaks from meta.json (5,13,27,51,94,171 Mg/ha)
BIOMASS_BREAKS <- c(0, 5, 13, 27, 51, 94, 171, Inf)
BIOMASS_LABELS <- c(
  "0–5 Mg/ha", "5–13.0 Mg/ha", "13.0–27.0 Mg/ha",
  "27.0–51.0 Mg/ha", "51.0–94.0 Mg/ha",
  "94.0–171.0 Mg/ha", ">171.0 Mg/ha"
)

classify_biomass <- function(val) {
  v <- ifelse(is.na(val) | val < 0, 0, val)
  b <- findInterval(v, BIOMASS_BREAKS[-length(BIOMASS_BREAKS)],
                    left.open = FALSE)
  b[b == 0L] <- 1L
  b[b > 7L]  <- 7L
  list(bin = as.integer(b), bin_label = BIOMASS_LABELS[b])
}

classify_trendy_bins <- function(val, glob_dist) {
  breaks <- c(glob_dist$min_value, Inf)
  breaks[is.na(breaks)] <- Inf
  labels <- glob_dist$bin_label
  v <- ifelse(is.na(val) | val < 0, 0, val)
  b <- findInterval(v, breaks[-length(breaks)], left.open = FALSE)
  b[b == 0L] <- 1L
  b[b > 7L]  <- 7L
  list(bin = as.integer(b), bin_label = labels[b])
}

# ---- KG metrics helper: all 3 aggregation levels from one site CSV ----------
kg_metrics <- function(site_df, glob_df, axis_nm, network, n_sites) {
  rows <- vector("list", 3L)

  # 30-class
  p30 <- setNames(glob_df$global_land_fraction,
                   as.character(glob_df$koppen_class_code))
  q30_raw <- table(as.character(site_df$koppen_class_code))
  q30     <- as.numeric(q30_raw) / sum(q30_raw)
  names(q30) <- names(q30_raw)
  rows[[1]] <- metrics_from_tables(p30, q30, axis_nm, "30class", 30L, network, n_sites)

  # 13-class (twoletter)
  tl_col <- if ("koppen_twoletter" %in% names(site_df)) "koppen_twoletter" else {
    # derive on-the-fly for future KG CSVs that omit it
    substr(site_df$koppen_class, 1L, 2L)
  }
  p13_df <- glob_df |>
    dplyr::group_by(koppen_twoletter) |>
    dplyr::summarise(frac = sum(global_land_fraction), .groups = "drop")
  p13 <- setNames(p13_df$frac, p13_df$koppen_twoletter)
  tl_vals <- if (is.character(tl_col)) site_df[[tl_col]] else tl_col
  q13_raw <- table(tl_vals)
  q13 <- as.numeric(q13_raw) / sum(q13_raw)
  names(q13) <- names(q13_raw)
  rows[[2]] <- metrics_from_tables(p13, q13, axis_nm, "13class_twoletter", 13L, network, n_sites)

  # 5-class
  p5_df <- glob_df |>
    dplyr::group_by(koppen_main) |>
    dplyr::summarise(frac = sum(global_land_fraction), .groups = "drop")
  p5 <- setNames(p5_df$frac, p5_df$koppen_main)
  q5_raw <- table(site_df$koppen_main)
  q5 <- as.numeric(q5_raw) / sum(q5_raw)
  names(q5) <- names(q5_raw)
  rows[[3]] <- metrics_from_tables(p5, q5, axis_nm, "5class", 5L, network, n_sites)

  dplyr::bind_rows(rows)
}

# ---- Step 3: Main extraction loop -------------------------------------------
msg("\n=== Step 3: Per-network extraction across all axes ===")

all_metrics <- list()

for (nm in names(networks)) {
  sites <- networks[[nm]]$sites
  n     <- networks[[nm]]$n
  msg("\n+++ Network: ", nm, " (", n, " sites) +++")

  # ---------- KG present-day --------------------------------------------------
  msg("  [KG present-day] extracting ...")
  ex_kg <- extract_with_fallback(kg_rast, sites)
  kg_pres <- ex_kg |>
    dplyr::rename(koppen_class_code = value, koppen_method = method) |>
    dplyr::mutate(koppen_class_code = as.integer(koppen_class_code)) |>
    dplyr::left_join(legend_df, by = "koppen_class_code") |>
    dplyr::select(site_id, location_lat, location_long,
                  koppen_class_code, koppen_class, koppen_class_name,
                  koppen_twoletter, koppen_main, koppen_main_name, koppen_method)

  out <- file.path(SNAP_DIR, paste0("site_koppen_beck2023_", nm, ".csv"))
  readr::write_csv(kg_pres, out)
  write_meta(out, input_sources = kg_path,
    notes = paste0("Historical network: ", nm, " (", n, " sites). ",
                   "Beck et al. (2023) KG 1991-2020. terra::extract(); ",
                   "nearest-land recovery within 3° for NA sites."))
  msg("  Saved: ", basename(out))

  na_pres <- sum(is.na(kg_pres$koppen_class_code))
  if (na_pres > 0L) msg("  WARNING: ", na_pres, " sites remain NA")

  all_metrics[[paste0(nm, "_kg_pres")]] <-
    kg_metrics(kg_pres, glob_kg_pres, "koppen_beck2023", nm, n)

  # ---------- KG SSP2-4.5 2041-2070 ------------------------------------------
  msg("  [KG SSP2-4.5] extracting ...")
  ex_245 <- extract_with_fallback(kg_245, sites)
  kg245 <- ex_245 |>
    dplyr::rename(koppen_class_code = value, koppen_method = method) |>
    dplyr::mutate(koppen_class_code = as.integer(koppen_class_code)) |>
    dplyr::left_join(
      legend_df |> dplyr::select(koppen_class_code, koppen_class,
                                  koppen_class_name, koppen_main, koppen_main_name),
      by = "koppen_class_code"
    ) |>
    dplyr::select(site_id, location_lat, location_long,
                  koppen_class_code, koppen_class, koppen_class_name,
                  koppen_main, koppen_main_name, koppen_method)

  out245 <- file.path(SNAP_DIR,
    paste0("site_koppen_beck2023_ssp245_2041_2070_", nm, ".csv"))
  readr::write_csv(kg245, out245)
  write_meta(out245, input_sources = ssp245_path,
    notes = paste0("Historical network: ", nm, " (", n, " sites). ",
                   "Beck et al. (2023) KG SSP2-4.5 2041-2070. ",
                   "Nearest-land recovery within 3°."))
  msg("  Saved: ", basename(out245))

  kg245_m <- kg245 |>
    dplyr::mutate(koppen_twoletter = substr(koppen_class, 1L, 2L))
  all_metrics[[paste0(nm, "_kg_245")]] <- kg_metrics(
    kg245_m, glob_kg_245, "koppen_beck2023_future_ssp245_2041_2070", nm, n
  )

  # ---------- KG SSP5-8.5 2071-2099 ------------------------------------------
  msg("  [KG SSP5-8.5] extracting ...")
  ex_585 <- extract_with_fallback(kg_585, sites)
  kg585 <- ex_585 |>
    dplyr::rename(koppen_class_code = value, koppen_method = method) |>
    dplyr::mutate(koppen_class_code = as.integer(koppen_class_code)) |>
    dplyr::left_join(
      legend_df |> dplyr::select(koppen_class_code, koppen_class,
                                  koppen_class_name, koppen_main, koppen_main_name),
      by = "koppen_class_code"
    ) |>
    dplyr::select(site_id, location_lat, location_long,
                  koppen_class_code, koppen_class, koppen_class_name,
                  koppen_main, koppen_main_name, koppen_method)

  out585 <- file.path(SNAP_DIR,
    paste0("site_koppen_beck2023_ssp585_2071_2099_", nm, ".csv"))
  readr::write_csv(kg585, out585)
  write_meta(out585, input_sources = ssp585_path,
    notes = paste0("Historical network: ", nm, " (", n, " sites). ",
                   "Beck et al. (2023) KG SSP5-8.5 2071-2099. ",
                   "Nearest-land recovery within 3°."))
  msg("  Saved: ", basename(out585))

  kg585_m <- kg585 |>
    dplyr::mutate(koppen_twoletter = substr(koppen_class, 1L, 2L))
  all_metrics[[paste0(nm, "_kg_585")]] <- kg_metrics(
    kg585_m, glob_kg_585, "koppen_beck2023_future_ssp585_2071_2099", nm, n
  )

  # ---------- Aridity UNEP 5-class + 7-class ----------------------------------
  msg("  [Aridity] extracting ...")
  ex_ai <- extract_with_fallback(ai_rast, sites)
  ai_cls <- classify_aridity(ex_ai$value)
  ai_out <- data.frame(
    site_id       = ex_ai$site_id,
    location_lat  = ex_ai$location_lat,
    location_long = ex_ai$location_long,
    ai_value      = ai_cls$ai_value,
    unep_class_5  = ai_cls$unep_class_5,
    unep_class_7  = ai_cls$unep_class_7,
    aridity_method = ex_ai$method,
    stringsAsFactors = FALSE
  )

  out_ai <- file.path(SNAP_DIR, paste0("site_aridity_", nm, ".csv"))
  readr::write_csv(ai_out, out_ai)
  write_meta(out_ai, input_sources = ai_path,
    notes = paste0("Historical network: ", nm, " (", n, " sites). ",
                   "CGIAR Aridity v3.1. ai_value = raw * 0.0001. ",
                   "UNEP 5-class thresholds: 0.05/0.20/0.50/0.65. ",
                   "UNEP 7-class adds FAO humid subdivisions at 1.0 and 2.0. ",
                   "Nearest-land recovery within 3°."))
  msg("  Saved: ", basename(out_ai))

  # Aridity metrics — both classification levels
  for (cl in c("unep_class_5", "unep_class_7")) {
    gl      <- if (cl == "unep_class_5") glob_ai5 else glob_ai7
    ax      <- if (cl == "unep_class_5") "aridity_unep5" else "aridity_unep7"
    agg     <- if (cl == "unep_class_5") "unep5" else "unep7"
    n_cls   <- if (cl == "unep_class_5") 5L else 7L
    p_ai    <- setNames(gl$global_land_fraction, gl$unep_class)
    q_raw   <- table(ai_out[[cl]])
    q_ai    <- as.numeric(q_raw) / sum(q_raw)
    names(q_ai) <- names(q_raw)
    all_metrics[[paste0(nm, "_", ax)]] <-
      metrics_from_tables(p_ai, q_ai, ax, agg, n_cls, nm, n)
  }

  # ---------- Biomass CCI v7 --------------------------------------------------
  msg("  [Biomass] extracting ...")
  ex_bio <- extract_with_fallback(bio_rast, sites)
  bio_cls <- classify_biomass(ex_bio$value)
  bio_out <- data.frame(
    site_id            = ex_bio$site_id,
    location_lat       = ex_bio$location_lat,
    location_long      = ex_bio$location_long,
    biomass_value_mg_ha = ex_bio$value,
    biomass_bin        = bio_cls$bin,
    biomass_bin_label  = bio_cls$bin_label,
    biomass_method     = ex_bio$method,
    stringsAsFactors   = FALSE
  )

  out_bio <- file.path(SNAP_DIR, paste0("site_biomass_cci_v7_", nm, ".csv"))
  readr::write_csv(bio_out, out_bio)
  write_meta(out_bio, input_sources = bio_path,
    notes = paste0("Historical network: ", nm, " (", n, " sites). ",
                   "ESA CCI Biomass v7.0, band 18. ",
                   "Hybrid bins: fixed bin 1 (0-5 Mg/ha); bins 2-7 canonical ",
                   "quantile breakpoints: 13, 27, 51, 94, 171 Mg/ha. ",
                   "Nearest-land recovery within 3°."))
  msg("  Saved: ", basename(out_bio))

  p_bio   <- setNames(glob_bio$global_land_fraction,
                       as.character(glob_bio$biomass_bin))
  q_raw   <- table(as.character(bio_out$biomass_bin))
  q_bio   <- as.numeric(q_raw) / sum(q_raw)
  names(q_bio) <- names(q_raw)
  all_metrics[[paste0(nm, "_biomass")]] <-
    metrics_from_tables(p_bio, q_bio, "biomass_cci_v7", "7bin_hybrid", 7L, nm, n)

  # ---------- LULC (three aggregation levels) ---------------------------------
  msg("  [LULC] extracting ...")
  ex_lulc <- extract_with_fallback(lulc_rast, sites)
  lulc_raw <- data.frame(
    site_id           = ex_lulc$site_id,
    location_lat      = ex_lulc$location_lat,
    location_long     = ex_lulc$location_long,
    cci_native_class  = as.integer(ex_lulc$value),
    landcover_method  = ex_lulc$method,
    stringsAsFactors  = FALSE
  )
  # Join aggregation lookup
  lulc_out <- lulc_raw |>
    dplyr::left_join(
      lulc_lookup |> dplyr::select(
        lulc_native, lulc_native_name,
        lulc_level2, lulc_level2_name,
        lulc_highlevel, lulc_highlevel_name
      ),
      by = c("cci_native_class" = "lulc_native")
    ) |>
    dplyr::mutate(
      cci_native_class_name     = lulc_native_name,
      cci_high_level_class      = lulc_highlevel,
      cci_high_level_class_name = lulc_highlevel_name,
      lulc_native               = cci_native_class
    ) |>
    dplyr::select(
      site_id, location_lat, location_long,
      cci_native_class, cci_native_class_name,
      cci_high_level_class, cci_high_level_class_name,
      landcover_method,
      lulc_native, lulc_native_name,
      lulc_level2, lulc_level2_name,
      lulc_highlevel, lulc_highlevel_name
    )

  out_lulc <- file.path(SNAP_DIR, paste0("site_landcover_cci_", nm, ".csv"))
  readr::write_csv(lulc_out, out_lulc)
  write_meta(out_lulc,
    input_sources = c(lulc_path, file.path(SNAP_DIR, "cci_landcover_aggregation_lookup.csv")),
    notes = paste0("Historical network: ", nm, " (", n, " sites). ",
                   "ESA CCI Land Cover v2.1.1, 2022. Three aggregation levels: ",
                   "native LCCS (37 classes), level2 (22 classes), high-level (10 classes). ",
                   "Aggregation via cci_landcover_aggregation_lookup.csv. ",
                   "Nearest-land recovery within 3°."))
  msg("  Saved: ", basename(out_lulc))

  # LULC metrics — three aggregation levels
  lulc_agg_defs <- list(
    list(col = "cci_high_level_class", glob = glob_lulc_hl,
         glob_key = "cci_high_level_class",
         axis = "landcover_cci", agg = "high_level", n_cls = 10L),
    list(col = "lulc_level2", glob = glob_lulc_l2,
         glob_key = "lulc_level2",
         axis = "landcover_cci", agg = "level2", n_cls = 22L),
    list(col = "lulc_native", glob = glob_lulc_nat,
         glob_key = "lulc_native",
         axis = "landcover_cci", agg = "native", n_cls = 37L)
  )
  for (def in lulc_agg_defs) {
    p_lc  <- setNames(def$glob$global_land_fraction,
                       as.character(def$glob[[def$glob_key]]))
    q_raw <- table(as.character(lulc_out[[def$col]]))
    q_lc  <- as.numeric(q_raw) / sum(q_raw)
    names(q_lc) <- names(q_raw)
    all_metrics[[paste0(nm, "_lulc_", def$agg)]] <-
      metrics_from_tables(p_lc, q_lc, def$axis, def$agg, def$n_cls, nm, n)
  }

  # ---------- TRENDY (four axes) ----------------------------------------------
  trendy_defs <- list(
    list(key    = "nee_iav",
         val_col = "trendy_nee_iav_value",
         mth_col = "trendy_nee_iav_method",
         bin_col = "trendy_nee_iav_bin",
         lbl_col = "trendy_nee_iav_bin_label",
         glob    = glob_tr_nee_iav,
         axis_nm = "trendy_nee_iav"),
    list(key    = "et_iav",
         val_col = "trendy_et_iav_value",
         mth_col = "trendy_et_iav_method",
         bin_col = "trendy_et_iav_bin",
         lbl_col = "trendy_et_iav_bin_label",
         glob    = glob_tr_et_iav,
         axis_nm = "trendy_et_iav"),
    list(key    = "nee_median",
         val_col = "trendy_nee_median_value",
         mth_col = "trendy_nee_median_method",
         bin_col = "trendy_nee_median_bin",
         lbl_col = "trendy_nee_median_bin_label",
         glob    = glob_tr_nee_med,
         axis_nm = "trendy_nee_median"),
    list(key    = "et_median",
         val_col = "trendy_et_median_value",
         mth_col = "trendy_et_median_method",
         bin_col = "trendy_et_median_bin",
         lbl_col = "trendy_et_median_bin_label",
         glob    = glob_tr_et_med,
         axis_nm = "trendy_et_median")
  )

  for (td in trendy_defs) {
    msg("  [TRENDY ", td$key, "] extracting ...")
    ex_tr  <- extract_with_fallback(trendy_rasts[[td$key]], sites)
    tr_cls <- classify_trendy_bins(ex_tr$value, td$glob)

    tr_out <- data.frame(
      site_id       = ex_tr$site_id,
      location_lat  = ex_tr$location_lat,
      location_long = ex_tr$location_long,
      stringsAsFactors = FALSE
    )
    tr_out[[td$val_col]] <- ex_tr$value
    tr_out[[td$mth_col]] <- ex_tr$method
    tr_out[[td$bin_col]] <- tr_cls$bin
    tr_out[[td$lbl_col]] <- tr_cls$bin_label

    out_tr <- file.path(SNAP_DIR,
      paste0("site_", td$axis_nm, "_", nm, ".csv"))
    readr::write_csv(tr_out, out_tr)
    write_meta(out_tr, input_sources = trendy_paths[[td$key]],
      notes = paste0("Historical network: ", nm, " (", n, " sites). ",
                     "TRENDY v14 ", td$axis_nm, ". ",
                     "7-bin hybrid with canonical breakpoints from current-network analysis. ",
                     "Nearest-land recovery within 3°."))
    msg("  Saved: ", basename(out_tr))

    p_tr  <- setNames(td$glob$global_land_fraction,
                       as.character(td$glob$bin))
    q_raw <- table(as.character(tr_out[[td$bin_col]]))
    q_tr  <- as.numeric(q_raw) / sum(q_raw)
    names(q_tr) <- names(q_raw)
    all_metrics[[paste0(nm, "_", td$axis_nm)]] <-
      metrics_from_tables(p_tr, q_tr, td$axis_nm, "7bin_hybrid", 7L, nm, n)
  }

  msg("--- ", nm, " COMPLETE ---")
}

# ---- Step 4: Update representativeness_metrics.csv -------------------------
msg("\n=== Step 4: Update representativeness_metrics.csv ===")

metrics_path <- file.path(SNAP_DIR, "representativeness_metrics.csv")
existing     <- readr::read_csv(metrics_path, show_col_types = FALSE)

# Add network and n_sites to existing rows if absent
if (!"network" %in% names(existing)) existing$network <- "current_767"
if (!"n_sites" %in% names(existing)) existing$n_sites <- 767L

new_rows      <- dplyr::bind_rows(all_metrics)
metrics_final <- dplyr::bind_rows(existing, new_rows) |>
  dplyr::arrange(axis, aggregation_level, network)

readr::write_csv(metrics_final, metrics_path)
msg("Updated metrics: ", metrics_path)
msg("Rows: ", nrow(existing), " existing + ", nrow(new_rows),
    " new = ", nrow(metrics_final), " total")

# ---- Step 5: Summary --------------------------------------------------------
msg("\n=== Summary ===")

for (nm in names(networks)) {
  n <- networks[[nm]]$n
  net_m <- dplyr::filter(metrics_final, network == nm) |>
    dplyr::arrange(dplyr::desc(weighted_jaccard))
  msg("\n", nm, " (n=", n, ") — top 3 axes by weighted Jaccard:")
  for (i in seq_len(min(3L, nrow(net_m)))) {
    msg("  ", net_m$axis[i], " (", net_m$aggregation_level[i], "): ",
        "J=", round(net_m$weighted_jaccard[i], 3),
        "  H=", round(net_m$hellinger_distance[i], 3))
  }
}

# Cross-network J comparison per axis (5-class KG as example)
msg("\nWeighted Jaccard (KG 5-class) across all networks:")
kg5 <- dplyr::filter(metrics_final,
  axis == "koppen_beck2023", aggregation_level == "5class") |>
  dplyr::arrange(dplyr::desc(weighted_jaccard))
print(as.data.frame(
  dplyr::select(kg5, network, n_sites, weighted_jaccard, hellinger_distance)
))

msg("\nDone! All site CSVs and metrics written.")
msg("Output dir: ", SNAP_DIR)

# ---- Disk usage summary -----------------------------------------------------
new_files <- list.files(SNAP_DIR,
  pattern = paste0("_(marconi|la_thuile|fluxnet2015)\\.(csv|json)$"))
msg("\nNew files written (", length(new_files), "):")
for (f in sort(new_files)) msg("  ", f)

total_kb <- sum(file.size(file.path(SNAP_DIR, new_files))) / 1024
msg("\nTotal new data: ", round(total_kb, 1), " KB")
