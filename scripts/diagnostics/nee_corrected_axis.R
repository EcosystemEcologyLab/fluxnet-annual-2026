## nee_corrected_axis.R
## Corrected NEE axis for Figs 4/5 (Geo vs Data / Geo vs Geo), built from
## TRENDY v14 gpp/ra/rh flux components instead of NBP -- replaces the
## NBP-based axis examined in review/diagnostics/nee_bin_scheme/, which
## compared different quantities and different statistics on its two sides.
##
## Read-only w.r.t. the pipeline, R/pipeline_config.R, and every committed
## figure. Does not download anything. Steps 0-7 per the task spec (see
## review/diagnostics/nee_corrected_axis/report.md for the full write-up).
##
## Checkpointed throughout (skip-if-exists on cached intermediates) so an
## interrupted run resumes rather than restarting -- this script's Part A
## (Steps 0-2) is I/O-heavy (17 models x 3 new TRENDY variables) and is
## expected to take several hours on first run.

suppressPackageStartupMessages({
  library(terra); library(dplyr); library(readr); library(tidyr)
  library(purrr); library(jsonlite); library(ggplot2)
  library(DBI); library(duckdb); library(lubridate)
})

source("R/pipeline_config.R")
check_pipeline_config()

write_meta <- function(output_path, input_sources, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                                 error = function(e) NA_character_),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

dir.create("logs", showWarnings = FALSE)
LOG_START <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE  <- file.path("logs", paste0("nee_corrected_axis_", LOG_START, ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output")
sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== NEE corrected axis diagnostic (Steps 0-7) ===")
msg("Log: ", LOG_FILE)

OUT_DIR <- "review/diagnostics/nee_corrected_axis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

BASE_DIR    <- "data/external/trendy/v14-gcb2025"
DERIVED_DIR <- "data/external/trendy/derived"
INTER_DIR   <- file.path(DERIVED_DIR, "intermediate")
SNAP_DIR    <- "data/snapshots"
KG_PATH     <- "data/external/koppen_beck2023/1991_2020/koppen_geiger_0p5.tif"
dir.create(INTER_DIR, recursive = TRUE, showWarnings = FALSE)

WIN_START <- 1991L; WIN_END <- 2020L; N_YEARS_WIN <- WIN_END - WIN_START + 1L  # 30
SECS_MONTH <- 30.4375 * 86400
SECS_YEAR  <- 365.25  * 86400
KG_TO_G    <- 1000.0

MODEL_START_YR <- c(
  "CABLE-POP" = 1700L, "CLASSIC" = 1700L, "CLM" = 1700L, "CLM-FATES" = 1701L,
  "DLEM" = 1700L, "ED" = 1700L, "ELM" = 1698L, "ELM-FATES" = 1701L,
  "IBIS" = 1700L, "ISAM" = 1700L, "JSBACH" = 1701L, "JULES-ES" = 1700L,
  "LPJ-GUESS" = 1700L, "LPJml" = 1700L, "LPJwsl" = 1699L, "LPX-Bern" = 1700L,
  "ORCHIDEE" = 1700L, "TEM" = 1700L, "VISIT-UT" = 1700L
)
LON360_MODELS <- c("CLM", "ISAM", "ELM-FATES")

## =====================================================================
## STEP 0: Inventory -- what is on disk
## =====================================================================
msg("\n=== STEP 0: Inventory ===")

MODELS_EXCLUDED_STRUCTURAL <- c("CLM-FATES", "JSBACH")
MODELS_EXCLUDED_WINDOW     <- c("CARDAMOM")
MODELS_TARGET <- c("CABLE-POP", "CLASSIC", "CLM", "DLEM", "ED", "ELM", "ELM-FATES",
                    "IBIS", "ISAM", "JULES-ES", "LPJ-GUESS", "LPJml", "LPJwsl",
                    "LPX-Bern", "ORCHIDEE", "TEM", "VISIT-UT")  # 17 models

STEP0_NOTES <- list(
  simulation = paste(
    "S3 only -- no '_S2_' file exists anywhere under", BASE_DIR,
    "or in data/external/trendy/download_manifest.csv, for any model or",
    "variable. This is stronger than 'S3 primary, S2 secondary': S2 is",
    "completely absent, so no land-use sensitivity test is possible."
  ),
  fire_luc = paste(
    "fFire and fLuc are absent for every model -- not in the manifest,",
    "not on disk. Per the task's 'do not download' constraint, Step 1's",
    "fFire+fLuc residual comparison cannot be performed for any model."
  ),
  lpj_guess_rh = paste(
    "LPJ-GUESS has no '*_S3_rh.nc' file; its heterotrophic respiration is",
    "stored as '*_S3_arh.nc' (varname 'arh', long_name",
    "'annual heterotrophic respiration', 325 annual layers, 1700-2024).",
    "Treated as LPJ-GUESS's rh throughout."
  ),
  clm_fates_jsbach = paste(
    "CLM-FATES (80x144 grid, longitude not regularly spaced -- a",
    "curvilinear/irregular native grid) and JSBACH (latitude not regularly",
    "spaced) both fail terra::rast() on every variable. Excluded from the",
    "ensemble -- confirmed structurally (not inherited from the prior nbp/et",
    "ensemble, which also lacked them but never stated why)."
  ),
  cardamom = paste(
    "Not in MODELS_19 (excluded there for a different reason: only 22",
    "years, too short for detrended-SD IAV analysis). Independently",
    "confirmed here: its S3 .nc files bundle 9 sub-variables per file",
    "(median 'gpp' + 6 percentile bands 'gpp_2.5pc'...'gpp_97.5pc' +",
    "'grid_area' + 'land_fraction'); the primary 'gpp' subdataset has only",
    "264 monthly layers = 22 years, shorter than the 30-year 1991-2020",
    "window this diagnostic needs regardless of where those 22 years fall",
    "on the calendar. Excluded for this window-coverage reason."
  ),
  elm = paste(
    "Included independently for this diagnostic, unlike the prior nbp/et",
    "ensemble which excluded ELM only for missing 2023 data -- irrelevant",
    "to a window ending 2020. ELM's nbp has no cached regridded",
    "intermediate (the prior ensemble excluded it before caching), so it is",
    "regridded fresh here."
  ),
  timestep_note = paste(
    "Per-model, per-variable cadence (monthly vs annual) was checked",
    "independently for gpp/ra/rh via terra::nlyr() (325/326 layers =",
    "annual, all others monthly) -- NOT inherited from",
    "figure_representativeness_trendy_compute.R's ANNUAL_NBP_MODELS",
    "constant (DLEM, LPJ-GUESS, LPJml), which was only ever validated for",
    "nbp. Confirmed: DLEM and LPJml have monthly gpp/ra/rh (only their nbp",
    "is annual); LPJ-GUESS has monthly gpp/ra but annual rh (arh) and nbp."
  ),
  regrid_note = paste(
    "12 of the 17 target models are natively on the same 0.5deg/720x360",
    "grid as the KG land mask (no resampling needed); 5 need bilinear",
    "resampling (CABLE-POP, CLASSIC 1deg; CLM, ELM 1.25x0.94deg;",
    "ELM-FATES 2.5x1.89deg)."
  )
)
write_lines(jsonlite::toJSON(STEP0_NOTES, pretty = TRUE, auto_unbox = TRUE),
            file.path(OUT_DIR, "step0_notes.json"))
msg("Step 0 notes written. Target ensemble (", length(MODELS_TARGET), " models): ",
    paste(MODELS_TARGET, collapse = ", "))

## =====================================================================
## Shared helpers
## =====================================================================
find_nc <- function(model, var) {
  d <- file.path(BASE_DIR, model, "S3")
  v <- if (model == "LPJ-GUESS" && var == "rh") "arh" else var
  hits <- list.files(d, pattern = paste0(".*_S3_", v, "\\.nc$"), full.names = TRUE)
  if (length(hits) == 0L) return(NA_character_)
  hits[[1L]]
}

get_years <- function(r, model) {
  n  <- nlyr(r)
  sy <- MODEL_START_YR[[model]]
  if (n %in% c(325L, 326L)) return(sy:(sy + n - 1L))
  n_full_years <- ceiling(n / 12L)
  as.integer(rep(seq(sy, by = 1L, length.out = n_full_years), each = 12L)[seq_len(n)])
}

TARGET <- rast(nrows = 360L, ncols = 720L, xmin = -180, xmax = 180,
                ymin = -90, ymax = 90, crs = "EPSG:4326")

## Loads a variable's annual stack (gC m-2 yr-1) at NATIVE resolution for
## years yr_lo:yr_hi. Returns NULL (and logs why) if the file is missing or
## the window isn't fully covered by complete 12-month years.
load_annual_native <- function(model, var, yr_lo, yr_hi) {
  path <- find_nc(model, var)
  if (is.na(path)) { msg("    ", model, " ", var, ": file not found"); return(NULL) }
  r <- tryCatch(rast(path), error = function(e) { msg("    ", model, " ", var,
                ": rast() error -- ", conditionMessage(e)); NULL })
  if (is.null(r)) return(NULL)
  yr_all <- get_years(r, model)
  is_annual <- nlyr(r) %in% c(325L, 326L)

  if (is_annual) {
    idx <- which(yr_all >= yr_lo & yr_all <= yr_hi)
    if (length(idx) < (yr_hi - yr_lo + 1L)) {
      msg("    ", model, " ", var, ": annual product covers only ", length(idx),
          "/", yr_hi - yr_lo + 1L, " target years -- excluded")
      return(NULL)
    }
    r_sub <- r[[idx]]
    if (model %in% LON360_MODELS) r_sub <- rotate(r_sub)
    r_sub <- r_sub * (SECS_YEAR * KG_TO_G)
    names(r_sub) <- as.character(yr_all[idx])
    return(r_sub)
  }

  # Subset to the ~360 layers this window needs BEFORE rotating, so rotate()
  # (and everything after it) only ever touches what this diagnostic uses,
  # not the model's full ~3900-layer native record.
  win_idx <- which(yr_all >= yr_lo & yr_all <= yr_hi)
  r <- r[[win_idx]]
  yr_win <- yr_all[win_idx]
  if (model %in% LON360_MODELS) r <- rotate(r)

  n_years <- yr_hi - yr_lo + 1L
  annual_list <- vector("list", n_years)
  for (i in seq_len(n_years)) {
    y <- yr_lo + i - 1L
    mo_idx <- which(yr_win == y)
    if (length(mo_idx) != 12L) {
      msg("    ", model, " ", var, ": year ", y, " has ", length(mo_idx),
          " months (need 12) -- excluded")
      return(NULL)
    }
    r_yr <- sum(r[[mo_idx]] * (SECS_MONTH * KG_TO_G), na.rm = FALSE)
    names(r_yr) <- as.character(y)
    annual_list[[i]] <- r_yr
  }
  rast(annual_list)
}

## Complete-case temporal mean (all layers must be non-NA at a pixel).
complete_mean <- function(r) {
  vals <- values(r)
  complete <- rowSums(is.na(vals)) == 0L
  out <- rep(NA_real_, nrow(vals))
  if (any(complete)) out[complete] <- rowMeans(vals[complete, , drop = FALSE])
  r1 <- r[[1L]]; values(r1) <- out; names(r1) <- "mean"
  r1
}

regrid_mean_to_target <- function(r_mean_native, kg_05) {
  if (!isTRUE(all.equal(res(r_mean_native), c(0.5, 0.5)))) {
    r_mean_native <- resample(r_mean_native, TARGET, method = "bilinear", threads = TRUE)
  }
  mask(r_mean_native, kg_05)
}

## =====================================================================
## PART A -- STEP 1 & 2: per-model NEE, residual check, global ensemble
## (I/O-heavy; checkpointed per model x variable)
## =====================================================================
msg("\n=== Loading KG land mask (0.5deg) ===")
kg_05 <- rast(KG_PATH)
cell_areas_05 <- cellSize(kg_05, mask = TRUE, unit = "km")

msg("\n=== STEP 1/2: Per-model flux-based NEE (gC m-2 yr-1, ", WIN_START, "-", WIN_END, ") ===")

model_status <- list()
model_mean_native <- list()   # native-res mean-annual layer per model, for gpp/ra/rh
model_annual_native <- list() # native-res 30-layer annual stack per model, for gpp/ra/rh (site extraction)

for (mdl in MODELS_TARGET) {
  msg("-- ", mdl, " --")
  vars_native <- list()
  vars_annual <- list()
  ok <- TRUE
  for (v in c("gpp", "ra", "rh")) {
    cache_native <- file.path(INTER_DIR, paste0(mdl, "_", v, "_annual_native_", WIN_START, "_", WIN_END, ".tif"))
    if (file.exists(cache_native)) {
      r_ann <- tryCatch(rast(cache_native), error = function(e) NULL)
      if (!is.null(r_ann) && nlyr(r_ann) == N_YEARS_WIN) {
        msg("    ", v, ": cached native stack loaded")
        vars_annual[[v]] <- r_ann
        next
      }
    }
    r_ann <- load_annual_native(mdl, v, WIN_START, WIN_END)
    if (is.null(r_ann)) { ok <- FALSE; break }
    writeRaster(r_ann, cache_native, gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
    vars_annual[[v]] <- r_ann
    rm(r_ann); gc(verbose = FALSE)
  }
  if (!ok) {
    model_status[[mdl]] <- "missing_or_incomplete_gpp_ra_rh"
    next
  }

  # Positivity check (Step 1)
  pos_frac <- sapply(vars_annual, function(r) {
    v <- values(complete_mean(r))
    v <- v[!is.na(v)]
    if (length(v) == 0L) return(NA_real_)
    mean(v >= 0)
  })
  msg("    positive-fraction (gpp,ra,rh): ", paste(round(pos_frac, 4), collapse = ", "))

  r_gpp_mean <- complete_mean(vars_annual[["gpp"]])
  r_ra_mean  <- complete_mean(vars_annual[["ra"]])
  r_rh_mean  <- complete_mean(vars_annual[["rh"]])
  r_nee_mean_native <- r_ra_mean + r_rh_mean - r_gpp_mean
  names(r_nee_mean_native) <- "nee_flux_based_mean"

  model_mean_native[[mdl]] <- list(gpp = r_gpp_mean, ra = r_ra_mean, rh = r_rh_mean,
                                     nee = r_nee_mean_native, pos_frac = pos_frac)
  r_nee_annual_native <- vars_annual[["ra"]] + vars_annual[["rh"]] - vars_annual[["gpp"]]
  names(r_nee_annual_native) <- names(vars_annual[["gpp"]])
  model_annual_native[[mdl]] <- r_nee_annual_native
  model_status[[mdl]] <- "ok"
  msg("    NEE = ra+rh-gpp computed (native res, mean + ", N_YEARS_WIN, "-yr stack)")
}

msg("\nModel status:")
for (m in names(model_status)) msg("  ", m, ": ", model_status[[m]])
MODELS_OK <- names(model_status)[model_status == "ok"]
msg("Models retained for flux-based NEE ensemble: ", length(MODELS_OK), " -- ", paste(MODELS_OK, collapse = ", "))
if (length(MODELS_OK) < 10L) stop("Too few models retained (", length(MODELS_OK), ") -- investigate before continuing.")

## ---- nbp comparator (reuse cache; regrid ELM fresh if needed) --------------
msg("\n=== Ensuring -NBP comparator is available for residual check ===")
nbp_native_by_model <- list()
for (mdl in MODELS_OK) {
  cache_nbp_regridded <- file.path(INTER_DIR, paste0(mdl, "_nbp_regridded.tif"))
  if (file.exists(cache_nbp_regridded)) {
    r_nbp_full <- tryCatch(rast(cache_nbp_regridded), error = function(e) NULL)
    if (!is.null(r_nbp_full)) {
      yrs <- as.integer(names(r_nbp_full))
      idx <- which(yrs >= WIN_START & yrs <= WIN_END)
      if (length(idx) == N_YEARS_WIN) {
        nbp_native_by_model[[mdl]] <- list(regridded = r_nbp_full[[idx]], source = "cached_regridded")
        next
      }
    }
  }
  # Not cached (ELM) -- regrid fresh, same convention as the original 34-yr cache
  # but restricted to this diagnostic's window since that's all we need here.
  msg("  ", mdl, ": no cached nbp_regridded.tif -- regridding fresh (window only)")
  r_ann <- load_annual_native(mdl, "nbp", WIN_START, WIN_END)
  if (is.null(r_ann)) { msg("    FAILED -- ", mdl, " dropped from residual check"); next }
  r_mean <- complete_mean(r_ann)
  r_mean_regridded <- regrid_mean_to_target(r_mean, kg_05)
  nbp_native_by_model[[mdl]] <- list(mean_regridded_only = r_mean_regridded, source = "fresh_mean_only")
}

## ---- Step 1: residual = NEE_flux_based - (-NBP) = NEE_flux_based + NBP -----
msg("\n=== STEP 1: Residual check (NEE_flux_based vs -NBP) ===")
residual_rows <- list()
for (mdl in MODELS_OK) {
  if (is.null(nbp_native_by_model[[mdl]])) next
  nbp_entry <- nbp_native_by_model[[mdl]]
  nee_mean_regridded <- regrid_mean_to_target(model_mean_native[[mdl]]$nee, kg_05)
  if (!is.null(nbp_entry$regridded)) {
    nbp_mean_regridded <- regrid_mean_to_target(complete_mean(nbp_entry$regridded), kg_05)
  } else {
    nbp_mean_regridded <- nbp_entry$mean_regridded_only
  }
  resid_r <- nee_mean_regridded + nbp_mean_regridded
  resid_vals <- values(mask(resid_r, kg_05))
  resid_vals <- resid_vals[!is.na(resid_vals)]
  residual_rows[[mdl]] <- data.frame(
    model = mdl,
    global_land_mean_residual_gC_m2_yr = mean(resid_vals),
    global_land_median_residual_gC_m2_yr = median(resid_vals),
    pos_frac_gpp = model_mean_native[[mdl]]$pos_frac[["gpp"]],
    pos_frac_ra  = model_mean_native[[mdl]]$pos_frac[["ra"]],
    pos_frac_rh  = model_mean_native[[mdl]]$pos_frac[["rh"]]
  )
}
residual_df <- bind_rows(residual_rows)
write_csv(residual_df, file.path(OUT_DIR, "table_step1_residual_and_positivity.csv"))
write_meta(file.path(OUT_DIR, "table_step1_residual_and_positivity.csv"),
           input_sources = c(BASE_DIR, INTER_DIR),
           notes = paste("residual = NEE_flux_based(ra+rh-gpp) - (-NBP) = NEE_flux_based + NBP.",
                          "Interpreted as the model's implied fire+land-use-change flux",
                          "(NBP = -NEE - fFire - fLuc identity => residual = -(fFire+fLuc)),",
                          "since fFire/fLuc are not on disk and cannot be checked directly (Step 0).",
                          "pos_frac_* = fraction of complete-case land pixels with mean-annual value >= 0."))
msg("Residual table written. Summary:")
print(residual_df)

## =====================================================================
## STEP 2: Global distribution (ensemble median + IQR, area-total check)
## =====================================================================
msg("\n=== STEP 2: Global ensemble distribution ===")

nee_mean_regridded_stack <- rast(lapply(MODELS_OK, function(m) regrid_mean_to_target(model_mean_native[[m]]$nee, kg_05)))
names(nee_mean_regridded_stack) <- MODELS_OK
writeRaster(nee_mean_regridded_stack, file.path(DERIVED_DIR, "trendy_nee_fluxbased_per_model_mean.tif"),
            gdal = "COMPRESS=DEFLATE", overwrite = TRUE)

ens_median <- app(nee_mean_regridded_stack, fun = function(v) median(v, na.rm = TRUE))
ens_q25    <- app(nee_mean_regridded_stack, fun = function(v) quantile(v, 0.25, na.rm = TRUE))
ens_q75    <- app(nee_mean_regridded_stack, fun = function(v) quantile(v, 0.75, na.rm = TRUE))
names(ens_median) <- "nee_fluxbased_median"; names(ens_q25) <- "q25"; names(ens_q75) <- "q75"

writeRaster(ens_median, file.path(DERIVED_DIR, "trendy_nee_fluxbased_median.tif"),
            gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
write_meta(file.path(DERIVED_DIR, "trendy_nee_fluxbased_median.tif"),
           input_sources = MODELS_OK,
           notes = paste0("Ensemble-median mean-annual (", WIN_START, "-", WIN_END,
                           ") NEE = ra+rh-gpp, gC m-2 yr-1, positive to atmosphere, ",
                           length(MODELS_OK), " models."))

## Area-total cross-check against the existing nbp-based distribution
existing_dist_path <- file.path(SNAP_DIR, "trendy_nee_median_global_distribution.csv")
existing_dist <- read_csv(existing_dist_path, show_col_types = FALSE)
existing_total_km2 <- sum(existing_dist$area_km2, na.rm = TRUE)
my_total_km2 <- sum(values(cell_areas_05)[!is.na(values(mask(ens_median, kg_05)))])
msg("Existing trendy_nee_median_global_distribution.csv total land area: ",
    format(round(existing_total_km2), big.mark = ","), " km2")
msg("This diagnostic's masked total land area: ", format(round(my_total_km2), big.mark = ","), " km2")
area_match <- isTRUE(all.equal(existing_total_km2, my_total_km2, tolerance = 1e-6))
msg("Area totals match exactly: ", area_match)
if (!area_match) {
  msg("STOPPING: land area totals do not match -- grid/mask mismatch must be resolved before proceeding.")
  stop("Step 2 area-total check failed: ", my_total_km2, " vs ", existing_total_km2)
}

## =====================================================================
## make_bins-equivalent quantile construction (reused convention from
## figure_representativeness_trendy_compute.R:436-449)
## =====================================================================
build_global_hist <- function(r_map, kg_05, cell_areas_05, step = 0.1, hist_max = 1000) {
  # Two catch-all bins (id 0 for < -hist_max, id max(ids)+1 for >= hist_max)
  # are RETAINED (not dropped) so the total area always equals the full land
  # mask total -- matching the precedent in
  # figure_representativeness_trendy_compute.R's own hist_rcl/hist_areas,
  # which keeps its single catch-all bin rather than discarding it. Dropping
  # them here would silently shrink the area total and could spuriously fail
  # (or spuriously pass) the Step 2 "area totals must match exactly" check.
  r_land <- mask(r_map, kg_05)
  lo <- seq(-hist_max, hist_max - step, by = step)
  hi <- lo + step
  ids <- seq_along(lo)
  catch_lo_id <- 0L
  catch_hi_id <- max(ids) + 1L
  rcl <- rbind(cbind(lo, hi, as.numeric(ids)),
               c(-1e9, -hist_max, catch_lo_id),
               c(hist_max, 1e9, catch_hi_id))
  r_hist <- classify(r_land, rcl, right = FALSE, include.lowest = TRUE)
  areas <- zonal(cell_areas_05, r_hist, fun = "sum", na.rm = TRUE)
  names(areas) <- c("bin_id", "area_km2")
  areas <- areas[!is.na(areas$bin_id), ]
  bin_lo_vec <- c(catch_lo_id = -hist_max, lo, catch_hi_id = hist_max)
  names(bin_lo_vec) <- as.character(c(catch_lo_id, ids, catch_hi_id))
  areas$value <- bin_lo_vec[as.character(areas$bin_id)]
  if (any(areas$bin_id %in% c(catch_lo_id, catch_hi_id))) {
    n_outlier <- sum(areas$area_km2[areas$bin_id %in% c(catch_lo_id, catch_hi_id)])
    msg("  NOTE: ", format(round(n_outlier)), " km2 of land falls outside +/-", hist_max,
        " gC m-2 yr-1 (kept in catch-all bins, not dropped)")
  }
  areas[order(areas$value), c("value", "area_km2")]
}

write_csv(build_global_hist(ens_median, kg_05, cell_areas_05),
          file.path(OUT_DIR, "step2_global_signed_histogram_0.1step.csv"))

msg("=== PART A (Steps 0-2) complete ===")
saveRDS(list(model_status = model_status, MODELS_OK = MODELS_OK,
             ens_median = wrap(ens_median), ens_q25 = wrap(ens_q25), ens_q75 = wrap(ens_q75),
             model_annual_native = lapply(model_annual_native, wrap),
             residual_df = residual_df),
        file.path(OUT_DIR, "partA_checkpoint.rds"))
msg("Checkpoint saved: ", file.path(OUT_DIR, "partA_checkpoint.rds"))

## =====================================================================
## STEP 3: Tower NEE, current network (VUT only; FLUXNET2015 fallback check)
## =====================================================================
msg("\n=== STEP 3: Tower annual NEE from monthly VUT_REF/25/75 ===")

## IMPORTANT DATA-QUALITY FINDING (documented in full in report.md):
## data/duckdb/fluxnet.duckdb's monthly_qc / monthly_converted NEE_VUT_REF,
## NEE_VUT_25, NEE_VUT_75 are NOT unit-converted despite the table name.
## Verified directly: `monthly` (raw) and `monthly_converted` are
## byte-identical for these columns at a sample site (BE-Vie), and the
## values (-3.25, -4.97, ... ) are µmol CO2 m-2 s-1-scale rates, not
## gC m-2 month-1 sums -- confirmed by cross-checking against that same
## site's annual_converted NEE_VUT_REF (-277..-752 gC m-2 yr-1, correctly
## pre-integrated), and by the arithmetic: -3.25 umol m-2 s-1 * 12 gC/mol *
## 2,629,800 s/month * 1e-6 = -102.6 gC m-2 month-1, consistent with the
## annual magnitude. R/units.R's fluxnet_convert_units() checks the SOURCE
## unit via read_bifvarinfo_units() before deciding whether to convert; with
## no BIFVARINFO_YY file present in data/extracted/ on this machine (it is
## gitignored and not currently populated), it falls back to
## .bifvarinfo_hardcoded_lookup(), which declares NEE_VUT_REF = "gC m-2 y-1"
## unconditionally (not resolution-aware) -- so calling that function here
## would silently skip the conversion this data actually needs at MM (and DD)
## resolution. This script does NOT call fluxnet_convert_units() for this
## reason and instead applies the umol->gC conversion explicitly below,
## using the exact number of seconds in each calendar month.

QC_THRESH_MM <- 0.80  # same threshold used for site_flux_medians (assess_flux_data_by_igbp_shuttle.R:43),
                       # applied here at monthly rather than annual granularity, per task instruction.

con <- dbConnect(duckdb::duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
monthly_raw <- dbGetQuery(con, "
  SELECT site_id, TIMESTAMP, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_25, NEE_VUT_75
  FROM monthly_converted
  WHERE NEE_VUT_REF IS NOT NULL AND NEE_VUT_REF_QC IS NOT NULL
")
dbDisconnect(con, shutdown = TRUE)

monthly_raw <- monthly_raw |>
  mutate(
    TIMESTAMP    = as.Date(TIMESTAMP),
    year         = year(TIMESTAMP),
    month        = month(TIMESTAMP),
    sec_in_month = lubridate::days_in_month(TIMESTAMP) * 86400
  )

to_gC_per_period <- function(umol_co2_m2_s, secs) umol_co2_m2_s * 1e-6 * 12 * secs

monthly_raw <- monthly_raw |>
  mutate(
    NEE_VUT_REF_gC = to_gC_per_period(NEE_VUT_REF, sec_in_month),
    NEE_VUT_25_gC  = to_gC_per_period(NEE_VUT_25,  sec_in_month),
    NEE_VUT_75_gC  = to_gC_per_period(NEE_VUT_75,  sec_in_month)
  )

qualifying <- monthly_raw |> filter(NEE_VUT_REF_QC >= QC_THRESH_MM)

msg("Sites with any monthly NEE_VUT_REF row: ", n_distinct(monthly_raw$site_id),
    "; sites with >=1 QC>=", QC_THRESH_MM, " month: ", n_distinct(qualifying$site_id))

site_monthly_cycle <- qualifying |>
  group_by(site_id, month) |>
  summarise(mean_REF = mean(NEE_VUT_REF_gC, na.rm = TRUE),
            mean_25  = mean(NEE_VUT_25_gC,  na.rm = TRUE),
            mean_75  = mean(NEE_VUT_75_gC,  na.rm = TRUE),
            n_years  = dplyr::n(), .groups = "drop")

site_month_count <- site_monthly_cycle |> group_by(site_id) |> summarise(n_months = dplyr::n(), .groups = "drop")
sites_all12 <- site_month_count |> filter(n_months == 12L) |> pull(site_id)
msg("Sites with all 12 calendar months represented at least once: ", length(sites_all12),
    " / ", n_distinct(qualifying$site_id), " QC-qualifying sites")

site_annual_nee <- site_monthly_cycle |>
  filter(site_id %in% sites_all12) |>
  group_by(site_id) |>
  summarise(nee_annual_ref = sum(mean_REF),
            nee_annual_25  = sum(mean_25),
            nee_annual_75  = sum(mean_75), .groups = "drop") |>
  mutate(half_width_site = abs(nee_annual_75 - nee_annual_25) / 2)

## Per-site qualifying years (for Step 6 variant 2 / Step 7 "own years")
site_years <- qualifying |> filter(site_id %in% sites_all12) |>
  distinct(site_id, year) |> arrange(site_id, year)

current_sites <- read_csv(file.path(SNAP_DIR, "site_biomass_cci_v7.csv"), show_col_types = FALSE) |>
  select(site_id, location_lat, location_long)

site_annual_nee_current <- site_annual_nee |> inner_join(current_sites, by = "site_id")
sites_no_vut_rows <- setdiff(current_sites$site_id, unique(monthly_raw$site_id))
msg("Current-network (n=", nrow(current_sites), ") sites with a valid annual NEE ",
    "(VUT, QC>=", QC_THRESH_MM, ", all 12 months): ", nrow(site_annual_nee_current))
msg(length(sites_no_vut_rows), " current-network sites have zero monthly NEE_VUT_REF rows ",
    "at all (CUT-only or no flux time series ingested) -- excluded per the task's VUT-only instruction.")

write_csv(site_annual_nee_current, file.path(OUT_DIR, "table_step3_tower_annual_nee.csv"))
write_meta(file.path(OUT_DIR, "table_step3_tower_annual_nee.csv"),
           input_sources = "data/duckdb/fluxnet.duckdb (monthly_converted), data/snapshots/site_biomass_cci_v7.csv",
           notes = paste0("QC threshold NEE_VUT_REF_QC >= ", QC_THRESH_MM, " per month. ",
             "Unit conversion (umol CO2 m-2 s-1 -> gC m-2 month-1) applied explicitly by this ",
             "script, NOT via fluxnet_convert_units() -- see report.md Section 3 for the ",
             "monthly-carbon-unit discrepancy this diagnostic found in the DuckDB tables. ",
             "VUT_25/VUT_75 use the SAME QC-qualifying months as VUT_REF (not filtered by their ",
             "own _25_QC/_75_QC columns) so REF and the uncertainty band are directly comparable ",
             "on one measured record. VUT only, no CUT fallback, per task instruction. ",
             length(sites_no_vut_rows), " current-network sites excluded for zero VUT rows."))

## FLUXNET2015 monthly-data check (task's explicit fallback)
fluxnet2015_mm_files <- list.files("data/fluxnet2015_comparison", pattern = "_MM_.*\\.csv$",
                                     recursive = TRUE, full.names = TRUE)
FLUXNET2015_MONTHLY_AVAILABLE <- length(fluxnet2015_mm_files) > 0L
msg("FLUXNET2015 monthly (MM) files found on disk: ", length(fluxnet2015_mm_files),
    " -- ", if (FLUXNET2015_MONTHLY_AVAILABLE) "proceeding with FLUXNET2015 too" else
    "only YY-resolution FLUXNET2015 files were ever extracted to data/fluxnet2015_comparison/ ",
    "(confirmed by directory listing) -- proceeding with the current network only for Geo vs Data, per task fallback.")

## =====================================================================
## STEP 4: Central bin half-width h
## =====================================================================
msg("\n=== STEP 4: Central bin half-width from tower uncertainty ===")
hw_dist <- site_annual_nee_current$half_width_site
hw_dist <- hw_dist[is.finite(hw_dist)]
H_HALFWIDTH <- median(hw_dist)
msg("Half-width (|VUT_75-VUT_25|/2) distribution across ", length(hw_dist), " sites: ",
    "min=", round(min(hw_dist), 2), " q25=", round(quantile(hw_dist, .25), 2),
    " median=", round(H_HALFWIDTH, 2), " q75=", round(quantile(hw_dist, .75), 2),
    " max=", round(max(hw_dist), 2), " gC m-2 yr-1")
write_csv(data.frame(site_id = site_annual_nee_current$site_id, half_width = hw_dist),
          file.path(OUT_DIR, "table_step4_halfwidth_distribution.csv"))
write_meta(file.path(OUT_DIR, "table_step4_halfwidth_distribution.csv"),
           input_sources = "table_step3_tower_annual_nee.csv",
           notes = paste0("h (median) = ", round(H_HALFWIDTH, 3), " gC m-2 yr-1; used as the near-zero bin half-width in Step 5."))

## =====================================================================
## STEP 5: Bins from the global area distribution only
## =====================================================================
msg("\n=== STEP 5: Signed 7-bin scheme, h=", round(H_HALFWIDTH, 2), " ===")

global_hist <- read_csv(file.path(OUT_DIR, "step2_global_signed_histogram_0.1step.csv"), show_col_types = FALSE)

make_signed_bins <- function(hist_df, h) {
  sink_side <- hist_df[hist_df$value < -h, ]
  sink_side <- sink_side[order(-sink_side$value), ]  # closest to -h first
  cum_sink  <- cumsum(sink_side$area_km2)
  total_sink <- sum(sink_side$area_km2)
  sink_breaks <- sort(vapply(c(1, 2) / 3, function(f) {
    idx <- which(cum_sink >= f * total_sink)[1L]
    sink_side$value[idx]
  }, numeric(1)))

  source_side <- hist_df[hist_df$value >= h, ]
  source_side <- source_side[order(source_side$value), ]
  cum_source  <- cumsum(source_side$area_km2)
  total_source <- sum(source_side$area_km2)
  source_breaks <- sort(vapply(c(1, 2) / 3, function(f) {
    idx <- which(cum_source >= f * total_source)[1L]
    source_side$value[idx]
  }, numeric(1)))

  list(sink_breaks = sink_breaks, source_breaks = source_breaks,
       total_sink_area = total_sink, total_source_area = total_source,
       near_zero_area = sum(hist_df$area_km2[hist_df$value >= -h & hist_df$value < h]))
}

bins_info <- make_signed_bins(global_hist, H_HALFWIDTH)
BREAKS <- c(-Inf, bins_info$sink_breaks, -H_HALFWIDTH, H_HALFWIDTH, bins_info$source_breaks, Inf)
msg("Bin edges (gC m-2 yr-1): ", paste(round(BREAKS, 2), collapse = " | "))

classify_signed <- function(x, breaks) {
  b <- findInterval(x, breaks[-length(breaks)], left.open = FALSE)
  b[b < 1L] <- 1L
  b[b > 7L] <- 7L
  as.integer(b)
}

total_land_area <- sum(global_hist$area_km2)
global_hist$bin <- classify_signed(global_hist$value, BREAKS)
global_bin_frac <- global_hist |> group_by(bin) |> summarise(area_km2 = sum(area_km2), .groups = "drop") |>
  mutate(area_frac = area_km2 / total_land_area) |> arrange(bin)
write_csv(global_bin_frac, file.path(OUT_DIR, "table_step5_global_bin_fractions.csv"))
write_meta(file.path(OUT_DIR, "table_step5_global_bin_fractions.csv"),
           input_sources = "step2_global_signed_histogram_0.1step.csv",
           notes = paste0("h=", round(H_HALFWIDTH, 3), " gC m-2 yr-1 (Step 4). Bin edges: ",
                           paste(round(BREAKS, 3), collapse = ", ")))
msg("Global area fraction per bin:")
print(global_bin_frac)

## =====================================================================
## STEP 6: Occupancy and Jaccard, three variants
## =====================================================================
msg("\n=== STEP 6: Occupancy and weighted Jaccard ===")

weighted_jaccard <- function(p, q) sum(pmin(p, q)) / sum(pmax(p, q))

site_fracs <- function(bins, n_total) {
  tab <- table(factor(bins, levels = 1:7))
  as.numeric(tab) / n_total
}

network_files <- list(
  current_781 = "data/snapshots/site_biomass_cci_v7.csv",
  fluxnet2015 = "data/snapshots/sites_fluxnet2015_clean.csv",
  la_thuile   = "data/snapshots/sites_la_thuile_clean.csv",
  marconi     = "data/snapshots/sites_marconi_clean.csv"
)
network_coords <- imap(network_files, function(f, net) {
  read_csv(f, show_col_types = FALSE) |> select(site_id, location_lat, location_long) |> mutate(network = net)
}) |> bind_rows()

## ---- Variant 1: Geo vs Geo, fixed period (1991-2020, ensemble-median) -----
msg("-- Variant 1: Geo vs Geo, fixed period --")
per_model_site_mean <- imap(model_mean_native[MODELS_OK], function(entry, mdl) {
  coords <- as.matrix(network_coords[, c("location_long", "location_lat")])
  vals <- terra::extract(entry$nee, coords, method = "bilinear")[, 1]
  data.frame(site_id = network_coords$site_id, network = network_coords$network, model = mdl, nee = vals)
}) |> bind_rows()

variant1_site <- per_model_site_mean |> group_by(site_id, network) |>
  summarise(nee_ensemble_median = median(nee, na.rm = TRUE), n_models = sum(!is.na(nee)), .groups = "drop") |>
  filter(n_models >= 1L)
variant1_site$bin <- classify_signed(variant1_site$nee_ensemble_median, BREAKS)

## ---- Variant 2: Geo vs Geo, tower years (current network only -- see report.md) ----
msg("-- Variant 2: Geo vs Geo, tower years (current_781 only) --")
current_site_years <- site_years |> filter(site_id %in% site_annual_nee_current$site_id)
variant2_rows <- list()
for (sid in unique(current_site_years$site_id)) {
  yrs <- current_site_years$year[current_site_years$site_id == sid]
  yrs <- yrs[yrs >= WIN_START & yrs <= WIN_END]
  if (length(yrs) == 0L) next
  crow <- network_coords[network_coords$site_id == sid & network_coords$network == "current_781", ]
  if (nrow(crow) == 0L) next
  coord <- as.matrix(crow[1, c("location_long", "location_lat")])
  model_vals <- sapply(MODELS_OK, function(mdl) {
    r <- model_annual_native[[mdl]]
    yr_idx <- which(as.integer(names(r)) %in% yrs)
    if (length(yr_idx) == 0L) return(NA_real_)
    v <- terra::extract(r[[yr_idx]], coord, method = "bilinear")[1, ]
    mean(as.numeric(v), na.rm = TRUE)
  })
  variant2_rows[[sid]] <- data.frame(site_id = sid, network = "current_781",
                                       nee_ensemble_median = median(model_vals, na.rm = TRUE),
                                       n_models = sum(!is.na(model_vals)), n_own_years = length(yrs))
}
variant2_site <- bind_rows(variant2_rows) |> filter(n_models >= 1L)
variant2_site$bin <- classify_signed(variant2_site$nee_ensemble_median, BREAKS)

## ---- Variant 3: Geo vs Data --------------------------------------------
msg("-- Variant 3: Geo vs Data --")
variant3_site <- site_annual_nee_current |> transmute(site_id, network = "current_781", nee = nee_annual_ref)
variant3_site$bin <- classify_signed(variant3_site$nee, BREAKS)

## ---- Occupancy + Jaccard summary table ----------------------------------
occupancy_rows <- list()
for (net in names(network_files)) {
  n_total <- sum(network_coords$network == net)

  v1 <- variant1_site |> filter(network == net)
  fr1 <- site_fracs(v1$bin, n_total)
  j1 <- weighted_jaccard(global_bin_frac$area_frac[match(1:7, global_bin_frac$bin)], fr1)
  occupancy_rows[[paste0("v1_", net)]] <- data.frame(
    variant = "geo_vs_geo_fixed_period", network = net, n_total = n_total,
    n_classified = nrow(v1), bin = 1:7, site_frac = fr1,
    global_frac = global_bin_frac$area_frac[match(1:7, global_bin_frac$bin)],
    weighted_jaccard = j1,
    outer_two_bin_share = fr1[1] + fr1[7]
  )

  if (net == "current_781") {
    fr2 <- site_fracs(variant2_site$bin, n_total)
    j2 <- weighted_jaccard(global_bin_frac$area_frac[match(1:7, global_bin_frac$bin)], fr2)
    occupancy_rows[[paste0("v2_", net)]] <- data.frame(
      variant = "geo_vs_geo_tower_years", network = net, n_total = n_total,
      n_classified = nrow(variant2_site), bin = 1:7, site_frac = fr2,
      global_frac = global_bin_frac$area_frac[match(1:7, global_bin_frac$bin)],
      weighted_jaccard = j2, outer_two_bin_share = fr2[1] + fr2[7]
    )

    fr3 <- site_fracs(variant3_site$bin, n_total)
    j3 <- weighted_jaccard(global_bin_frac$area_frac[match(1:7, global_bin_frac$bin)], fr3)
    occupancy_rows[[paste0("v3_", net)]] <- data.frame(
      variant = "geo_vs_data", network = net, n_total = n_total,
      n_classified = nrow(variant3_site), bin = 1:7, site_frac = fr3,
      global_frac = global_bin_frac$area_frac[match(1:7, global_bin_frac$bin)],
      weighted_jaccard = j3, outer_two_bin_share = fr3[1] + fr3[7]
    )
  }
}
occupancy_df <- bind_rows(occupancy_rows)
write_csv(occupancy_df, file.path(OUT_DIR, "table_step6_occupancy_jaccard.csv"))
write_meta(file.path(OUT_DIR, "table_step6_occupancy_jaccard.csv"),
           input_sources = c("table_step5_global_bin_fractions.csv", "table_step3_tower_annual_nee.csv",
                              "trendy_nee_fluxbased_per_model_mean.tif"),
           notes = paste("Denominator convention: unclassified sites dropped from the numerator,",
                          "full network N kept as the denominator (site_fracs()/count_sites() precedent,",
                          "figure_representativeness_trendy_compute.R and review/diagnostics/nee_bin_scheme/).",
                          "Variant 2 (tower years) and Variant 3 (Geo vs Data) are current_781-only:",
                          "la_thuile/marconi/fluxnet2015 have no per-site measurement-year records in this",
                          "repo (coordinates only), and FLUXNET2015 has no monthly flux data on disk (Step 3)."))
msg("Occupancy/Jaccard summary (weighted Jaccard by variant x network):")
print(occupancy_df |> distinct(variant, network, weighted_jaccard, outer_two_bin_share))

## =====================================================================
## STEP 7: Paired check (diagnostic only)
## =====================================================================
msg("\n=== STEP 7: Paired tower-vs-model check ===")
paired_rows <- list()
for (sid in unique(current_site_years$site_id)) {
  yrs <- current_site_years$year[current_site_years$site_id == sid]
  yrs <- yrs[yrs >= WIN_START & yrs <= WIN_END]
  if (length(yrs) == 0L) next
  tower_row <- site_annual_nee_current[site_annual_nee_current$site_id == sid, ]
  if (nrow(tower_row) == 0L) next
  crow <- network_coords[network_coords$site_id == sid & network_coords$network == "current_781", ]
  if (nrow(crow) == 0L) next
  coord <- as.matrix(crow[1, c("location_long", "location_lat")])
  model_vals <- sapply(MODELS_OK, function(mdl) {
    r <- model_annual_native[[mdl]]
    yr_idx <- which(as.integer(names(r)) %in% yrs)
    if (length(yr_idx) == 0L) return(NA_real_)
    mean(as.numeric(terra::extract(r[[yr_idx]], coord, method = "bilinear")[1, ]), na.rm = TRUE)
  })
  model_vals <- model_vals[!is.na(model_vals)]
  if (length(model_vals) == 0L) next
  paired_rows[[sid]] <- data.frame(
    site_id = sid, n_own_years = length(yrs),
    tower_nee = tower_row$nee_annual_ref[1], tower_half_width = tower_row$half_width_site[1],
    model_median = median(model_vals), model_min = min(model_vals), model_max = max(model_vals),
    n_models = length(model_vals)
  )
}
paired_df <- bind_rows(paired_rows) |> mutate(diff_tower_minus_model = tower_nee - model_median)
write_csv(paired_df, file.path(OUT_DIR, "table_step7_paired_check.csv"))
write_meta(file.path(OUT_DIR, "table_step7_paired_check.csv"),
           input_sources = c("table_step3_tower_annual_nee.csv", "trendy_nee_fluxbased_per_model_mean.tif"),
           notes = "Diagnostic only, not used for Step 5/6 bins or Jaccard. diff = tower_nee - model_median, evaluated over each tower's own QC-qualifying years.")
msg("Step 7 paired difference (tower - model median): n=", nrow(paired_df),
    " median=", round(median(paired_df$diff_tower_minus_model), 2),
    " IQR=[", round(quantile(paired_df$diff_tower_minus_model, .25), 2), ", ",
    round(quantile(paired_df$diff_tower_minus_model, .75), 2), "]")

## =====================================================================
## FIGURES
## =====================================================================
msg("\n=== Figures ===")
theme_set(theme_bw(base_size = 11))

## Fig A: global area fraction vs site fraction per bin, per variant
fig_a_df <- occupancy_df |>
  filter((variant == "geo_vs_geo_fixed_period" & network == "current_781") |
         variant %in% c("geo_vs_geo_tower_years", "geo_vs_data")) |>
  select(variant, bin, global_frac, site_frac) |>
  pivot_longer(c(global_frac, site_frac), names_to = "series", values_to = "frac")

p_a <- ggplot(fig_a_df, aes(x = factor(bin), y = frac, fill = series)) +
  geom_col(position = "dodge") +
  facet_wrap(~variant, ncol = 1) +
  labs(x = "Bin (1=most negative/sink .. 7=most positive/source)", y = "Fraction",
       fill = NULL, title = "Global area fraction vs. site fraction per bin, corrected NEE axis") +
  scale_fill_manual(values = c(global_frac = "grey60", site_frac = "#D55E00"),
                     labels = c(global_frac = "Global area", site_frac = "Sites"))
ggsave(file.path(OUT_DIR, "fig_step6_bin_fractions.png"), p_a, width = 8, height = 9, dpi = 150, bg = "white")

## Fig B: global signed distribution with bin edges + tower values overlaid
p_b <- ggplot(global_hist, aes(x = value, y = area_km2)) +
  geom_col(width = 0.1, fill = "grey70") +
  geom_vline(xintercept = BREAKS[is.finite(BREAKS)], linetype = "dashed", color = "steelblue") +
  geom_rug(data = site_annual_nee_current, aes(x = nee_annual_ref, y = NULL), color = "#D55E00", alpha = 0.4) +
  coord_cartesian(xlim = c(-300, 300)) +
  labs(x = "NEE (gC m-2 yr-1), positive = source", y = "Land area (km2, 0.1 gC bin)",
       title = "Global signed NEE distribution (flux-based), bin edges, tower NEE overlay (rug)")
ggsave(file.path(OUT_DIR, "fig_step5_global_distribution.png"), p_b, width = 9, height = 5, dpi = 150, bg = "white")

## Fig C: paired scatter with 1:1 line
p_c <- ggplot(paired_df, aes(x = model_median, y = tower_nee)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(aes(ymin = tower_nee - tower_half_width, ymax = tower_nee + tower_half_width), width = 0, alpha = 0.4) +
  geom_errorbarh(aes(xmin = model_min, xmax = model_max), height = 0, alpha = 0.3, color = "steelblue") +
  geom_point(size = 1.6, alpha = 0.7) +
  labs(x = "Model NEE (ensemble median, tower's own years), gC m-2 yr-1",
       y = "Tower NEE (VUT_REF, own years), gC m-2 yr-1",
       title = "Paired tower-vs-model NEE (Step 7 diagnostic)") +
  coord_equal()
ggsave(file.path(OUT_DIR, "fig_step7_paired_scatter.png"), p_c, width = 7, height = 7, dpi = 150, bg = "white")

msg("\n=== nee_corrected_axis.R complete ===")
