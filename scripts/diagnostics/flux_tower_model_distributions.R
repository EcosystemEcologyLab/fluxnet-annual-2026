## flux_tower_model_distributions.R
## Tower-vs-model distribution diagnostics for NEE, GPP, TER (RECO), ET --
## companion to nee_corrected_axis.R, reusing its cached TRENDY S3 annual
## rasters (data/external/trendy/derived/intermediate/) and its Step 3
## tower-annual method. Does not recompute anything already cached on disk:
## per-model gpp/ra/rh 1991-2020 native annual stacks are loaded from
## nee_corrected_axis.R's cache; per-model evapotranspiration comes from the
## existing 1990-2023 regridded cache built by
## figure_representativeness_trendy_compute.R (subset to 1991-2020 here,
## not recomputed from source).
##
## Tower side (all qualifying years, no 1991-2020 restriction):
##   NEE: per-site VUT->CUT fallback (CLAUDE.md QC Flag Reference) -- a site
##        with any non-NA NEE_VUT_REF_QC uses VUT_REF/VUT_REF_QC throughout;
##        CUT_REF/CUT_REF_QC only for sites where VUT QC is entirely NA.
##        Never mixed within a site.
##   GPP/TER: same per-site VUT/CUT choice as NEE (not decided independently),
##        plus a per-site NT->DT partitioning fallback (pattern from
##        scripts/assess_flux_data_by_igbp_shuttle.R): NT preferred
##        (GPP_NT_{VUT/CUT}_REF, RECO_NT_{VUT/CUT}_REF); DT fallback only
##        when NT yields zero qualifying months for that site. GPP/RECO have
##        no dedicated QC column at MM resolution, so the NEE QC gate
##        (NEE_{VUT/CUT}_REF_QC >= QC_THRESH_MM) is reused, exactly as the
##        YY-resolution reference script reuses the same gate for DT.
##   ET:  LE_F_MDS, gated on its own LE_F_MDS_QC (independent of VUT/CUT).
##        Unlike NEE_VUT_REF (documented bug: unconverted at MM resolution
##        in monthly_converted -- see nee_corrected_axis.R Step 3), LE_F_MDS
##        IS correctly unit-converted there: 05_units.R's DuckDB pipeline
##        applies the LE conversion (* spp / 2.45e6) unconditionally, with no
##        is_coarse guard (unlike carbon). Verified directly: BE-Vie's
##        LE_F_MDS / LE_F_MDS_native ratio is ~1.0734 for several months,
##        exactly spp_month(2,629,800s) / 2.45e6 -- so LE_F_MDS here is
##        already mm H2O per month and is used as-is, no explicit reconversion.
##   Carbon (NEE/GPP/RECO) at MM resolution IS subject to the same bug as
##        NEE_VUT_REF (raw values are umol CO2 m-2 s-1-scale, not
##        pre-integrated gC, despite 05_units.R's is_coarse guard assuming
##        otherwise) -- confirmed directly for GPP_NT_VUT_REF at BE-Vie 2010
##        (monthly raw values sum to the right order of magnitude only after
##        applying the umol->gC conversion; annual_converted's independently-
##        computed GPP_NT_VUT_REF for the same site-year is pre-integrated
##        and gives the same total). This script applies the same explicit
##        to_gC_per_period() conversion nee_corrected_axis.R uses for NEE, to
##        every raw GPP/RECO VUT/CUT/NT/DT column, for the same reason
##        (fluxnet_convert_units() is not called here either).
##   Annual value construction: identical to nee_corrected_axis.R Step 3 --
##        mean monthly cycle across all qualifying years (QC >= QC_THRESH_MM),
##        all 12 calendar months required, then summed. Independent per flux
##        (NEE/GPP/TER/ET each have their own qualifying-months set).
##
## Model side (fixed 1991-2020 mean, ensemble median at each tower cell,
## same 17 models as nee_corrected_axis.R Step 1 -- method matches that
## script's Variant 1):
##   NEE = ra + rh - gpp   (native-resolution per-model mean, bilinear
##                          extraction at tower coordinates, no common-grid
##                          regridding needed for point extraction)
##   TER = ra + rh
##   GPP = gpp
##   ET  = evapotranspiration, from the existing regridded (0.5 deg common
##         grid) 1990-2023 per-model stack, subset to 1991-2020 -- this one
##         variable is therefore extracted from a regridded common grid
##         rather than each model's native grid, unlike NEE/GPP/TER. This is
##         a genuine methodological difference from the carbon variables
##         (forced by what is already cached), not an inconsistency to fix.
##   Ensemble value per site = median across models with a non-NA value at
##   that cell. Any model missing a variable (cache absent or incomplete
##   1991-2020 coverage) is listed and excluded from that flux's median only.
##
## Units: gC m-2 yr-1 (carbon), mm yr-1 (ET). NEE positive to atmosphere on
## both sides (no sign flip needed -- FLUXNET NEE_VUT/CUT_REF and TRENDY
## ra+rh-gpp share the same sign convention).
##
## Outputs (review/diagnostics/nee_corrected_axis/):
##   table_dist_tower_vs_model.csv + .meta.json  (long: site x flux)
##   fig_dist_histograms.png       (4-panel: NEE/GPP/TER/ET, tower vs model)
##   fig_dist_latitude.png         (4-panel: flux vs latitude, tower vs model)
##   fig_dist_scatter_1to1.png     (4-panel: tower (y) vs model (x), 1:1 line)

suppressPackageStartupMessages({
  library(terra); library(dplyr); library(readr); library(tidyr)
  library(purrr); library(jsonlite); library(ggplot2); library(patchwork)
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
LOG_FILE  <- file.path("logs", paste0("flux_tower_model_distributions_", LOG_START, ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output")
sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== Flux tower-vs-model distribution diagnostics ===")
msg("Log: ", LOG_FILE)

OUT_DIR <- "review/diagnostics/nee_corrected_axis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

BASE_DIR  <- "data/external/trendy/v14-gcb2025"
INTER_DIR <- "data/external/trendy/derived/intermediate"
SNAP_DIR  <- "data/snapshots"

WIN_START <- 1991L; WIN_END <- 2020L; N_YEARS_WIN <- WIN_END - WIN_START + 1L
QC_THRESH_MM <- 0.80

MODELS_TARGET <- c("CABLE-POP", "CLASSIC", "CLM", "DLEM", "ED", "ELM", "ELM-FATES",
                    "IBIS", "ISAM", "JULES-ES", "LPJ-GUESS", "LPJml", "LPJwsl",
                    "LPX-Bern", "ORCHIDEE", "TEM", "VISIT-UT")  # same 17 as Step 1

## =====================================================================
## Shared helper (same as nee_corrected_axis.R)
## =====================================================================
complete_mean <- function(r) {
  vals <- values(r)
  complete <- rowSums(is.na(vals)) == 0L
  out <- rep(NA_real_, nrow(vals))
  if (any(complete)) out[complete] <- rowMeans(vals[complete, , drop = FALSE])
  r1 <- r[[1L]]; values(r1) <- out; names(r1) <- "mean"
  r1
}

## =====================================================================
## MODEL SIDE -- load cached per-model means (no recomputation)
## =====================================================================
msg("\n=== Model side: loading cached per-model means (", WIN_START, "-", WIN_END, ") ===")

model_missing <- list()  # mdl -> character vector of missing flux reasons
model_mean_gpp <- list(); model_mean_ra <- list(); model_mean_rh <- list(); model_mean_et <- list()

for (mdl in MODELS_TARGET) {
  miss <- character(0)

  carbon_ok <- TRUE
  for (v in c("gpp", "ra", "rh")) {
    f <- file.path(INTER_DIR, paste0(mdl, "_", v, "_annual_native_", WIN_START, "_", WIN_END, ".tif"))
    if (!file.exists(f)) { miss <- c(miss, paste0(v, ": cache absent")); carbon_ok <- FALSE; next }
    r <- tryCatch(rast(f), error = function(e) NULL)
    if (is.null(r) || nlyr(r) != N_YEARS_WIN) {
      miss <- c(miss, paste0(v, ": cache invalid/incomplete")); carbon_ok <- FALSE; next
    }
    assign(paste0("r_", v), complete_mean(r))
  }
  if (carbon_ok) {
    model_mean_gpp[[mdl]] <- r_gpp
    model_mean_ra[[mdl]]  <- r_ra
    model_mean_rh[[mdl]]  <- r_rh
  }

  f_et <- file.path(INTER_DIR, paste0(mdl, "_evapotrans_regridded.tif"))
  if (!file.exists(f_et)) {
    miss <- c(miss, "evapotrans: cache absent")
  } else {
    r_et <- tryCatch(rast(f_et), error = function(e) NULL)
    if (is.null(r_et)) {
      miss <- c(miss, "evapotrans: cache unreadable")
    } else {
      yrs_et <- suppressWarnings(as.integer(names(r_et)))
      idx <- which(yrs_et >= WIN_START & yrs_et <= WIN_END)
      if (length(idx) < N_YEARS_WIN) {
        miss <- c(miss, sprintf("evapotrans: only %d/%d target years cached", length(idx), N_YEARS_WIN))
      } else {
        model_mean_et[[mdl]] <- complete_mean(r_et[[idx]])
      }
    }
  }

  if (length(miss) > 0L) model_missing[[mdl]] <- miss
}

MODELS_OK_CARBON <- names(model_mean_gpp)
MODELS_OK_ET     <- names(model_mean_et)
msg("Models with usable gpp/ra/rh (NEE/GPP/TER): ", length(MODELS_OK_CARBON), " / ", length(MODELS_TARGET),
    " -- ", paste(MODELS_OK_CARBON, collapse = ", "))
msg("Models with usable 1991-2020 evapotranspiration: ", length(MODELS_OK_ET), " / ", length(MODELS_TARGET),
    " -- ", paste(MODELS_OK_ET, collapse = ", "))
if (length(model_missing) == 0L) {
  msg("No model is missing any variable.")
} else {
  msg("Models missing a variable:")
  for (mdl in names(model_missing)) msg("  ", mdl, ": ", paste(model_missing[[mdl]], collapse = "; "))
}

## ---- Extract model values at every current-network tower cell -------------
current_sites <- read_csv(file.path(SNAP_DIR, "site_biomass_cci_v7.csv"), show_col_types = FALSE) |>
  select(site_id, location_lat, location_long) |>
  distinct(site_id, .keep_all = TRUE)
coords <- as.matrix(current_sites[, c("location_long", "location_lat")])
msg("\nCurrent-network sites for model extraction: ", nrow(current_sites))

extract_per_model <- function(model_list, models_ok) {
  out <- imap(model_list[models_ok], function(r, mdl) {
    vals <- terra::extract(r, coords, method = "bilinear")[, 1]
    data.frame(site_id = current_sites$site_id, model = mdl, value = vals)
  }) |> bind_rows()
  out
}

gpp_by_model <- extract_per_model(model_mean_gpp, MODELS_OK_CARBON) |> rename(gpp = value)
ra_by_model  <- extract_per_model(model_mean_ra,  MODELS_OK_CARBON) |> rename(ra  = value)
rh_by_model  <- extract_per_model(model_mean_rh,  MODELS_OK_CARBON) |> rename(rh  = value)
et_by_model  <- extract_per_model(model_mean_et,  MODELS_OK_ET)     |> rename(et  = value)

carbon_by_model <- gpp_by_model |>
  inner_join(ra_by_model, by = c("site_id", "model")) |>
  inner_join(rh_by_model, by = c("site_id", "model")) |>
  mutate(nee = ra + rh - gpp, ter = ra + rh)

model_median_site <- function(df, val_col) {
  df |>
    group_by(site_id) |>
    summarise(model_value = median(.data[[val_col]], na.rm = TRUE),
              n_models = sum(!is.na(.data[[val_col]])), .groups = "drop")
}

model_nee <- model_median_site(carbon_by_model, "nee") |> mutate(flux = "NEE")
model_gpp <- model_median_site(carbon_by_model, "gpp") |> mutate(flux = "GPP")
model_ter <- model_median_site(carbon_by_model, "ter") |> mutate(flux = "TER")
model_et  <- model_median_site(et_by_model,     "et")  |> mutate(flux = "ET")
model_all <- bind_rows(model_nee, model_gpp, model_ter, model_et)
msg("Model-side ensemble medians computed for NEE/GPP/TER/ET at ", nrow(current_sites), " sites.")

## =====================================================================
## TOWER SIDE -- VUT/CUT + NT/DT fallback, all qualifying years
## =====================================================================
msg("\n=== Tower side: VUT/CUT + NT/DT fallback (QC >= ", QC_THRESH_MM, ") ===")

site_ids_sql <- paste(sprintf("'%s'", current_sites$site_id), collapse = ", ")
con <- dbConnect(duckdb::duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
monthly_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP,
         NEE_VUT_REF, NEE_VUT_REF_QC, NEE_CUT_REF, NEE_CUT_REF_QC,
         GPP_NT_VUT_REF, GPP_DT_VUT_REF, GPP_NT_CUT_REF, GPP_DT_CUT_REF,
         RECO_NT_VUT_REF, RECO_DT_VUT_REF, RECO_NT_CUT_REF, RECO_DT_CUT_REF,
         LE_F_MDS, LE_F_MDS_QC
  FROM monthly_converted
  WHERE site_id IN (%s)
", site_ids_sql))
dbDisconnect(con, shutdown = TRUE)

monthly_raw <- monthly_raw |>
  mutate(TIMESTAMP = as.Date(TIMESTAMP), year = year(TIMESTAMP), month = month(TIMESTAMP),
         sec_in_month = lubridate::days_in_month(TIMESTAMP) * 86400)

to_gC_per_period <- function(umol_co2_m2_s, secs) umol_co2_m2_s * 1e-6 * 12 * secs

## ---- Per-site VUT/CUT decision (NEE_*_QC presence only) --------------------
site_carbon_src <- monthly_raw |>
  group_by(site_id) |>
  summarise(any_vut_qc = any(!is.na(NEE_VUT_REF_QC)),
            any_cut_qc = any(!is.na(NEE_CUT_REF_QC)), .groups = "drop") |>
  mutate(carbon_src = case_when(any_vut_qc ~ "VUT", any_cut_qc ~ "CUT", TRUE ~ NA_character_))

n_vut <- sum(site_carbon_src$carbon_src == "VUT", na.rm = TRUE)
n_cut <- sum(site_carbon_src$carbon_src == "CUT", na.rm = TRUE)
n_none <- sum(is.na(site_carbon_src$carbon_src))
msg("Per-site carbon source decision: VUT=", n_vut, "  CUT (fallback)=", n_cut,
    "  neither (no VUT or CUT QC at all)=", n_none, "  / ", nrow(site_carbon_src), " sites")

monthly_raw <- monthly_raw |> left_join(site_carbon_src |> select(site_id, carbon_src), by = "site_id")

## ---- Row-level carbon qualification + gC conversion, per site's VUT/CUT ---
monthly_raw <- monthly_raw |>
  mutate(
    nee_val  = if_else(carbon_src == "VUT", NEE_VUT_REF, NEE_CUT_REF),
    nee_qc   = if_else(carbon_src == "VUT", NEE_VUT_REF_QC, NEE_CUT_REF_QC),
    gpp_nt   = if_else(carbon_src == "VUT", GPP_NT_VUT_REF, GPP_NT_CUT_REF),
    gpp_dt   = if_else(carbon_src == "VUT", GPP_DT_VUT_REF, GPP_DT_CUT_REF),
    ter_nt   = if_else(carbon_src == "VUT", RECO_NT_VUT_REF, RECO_NT_CUT_REF),
    ter_dt   = if_else(carbon_src == "VUT", RECO_DT_VUT_REF, RECO_DT_CUT_REF),
    carbon_qualifies = !is.na(carbon_src) & !is.na(nee_qc) & nee_qc >= QC_THRESH_MM & !is.na(nee_val),
    nee_gC   = if_else(carbon_qualifies, to_gC_per_period(nee_val, sec_in_month), NA_real_),
    gpp_nt_gC = if_else(carbon_qualifies, to_gC_per_period(gpp_nt, sec_in_month), NA_real_),
    gpp_dt_gC = if_else(carbon_qualifies, to_gC_per_period(gpp_dt, sec_in_month), NA_real_),
    ter_nt_gC = if_else(carbon_qualifies, to_gC_per_period(ter_nt, sec_in_month), NA_real_),
    ter_dt_gC = if_else(carbon_qualifies, to_gC_per_period(ter_dt, sec_in_month), NA_real_),
    et_qualifies = !is.na(LE_F_MDS) & !is.na(LE_F_MDS_QC) & LE_F_MDS_QC >= QC_THRESH_MM,
    et_mm    = if_else(et_qualifies, LE_F_MDS, NA_real_)  # already mm/month -- see header note
  )

## ---- Per-site NT/DT decision for GPP and TER (independent) -----------------
site_partition <- monthly_raw |>
  group_by(site_id) |>
  summarise(
    n_gpp_nt = sum(!is.na(gpp_nt_gC)), n_gpp_dt = sum(!is.na(gpp_dt_gC)),
    n_ter_nt = sum(!is.na(ter_nt_gC)), n_ter_dt = sum(!is.na(ter_dt_gC)),
    .groups = "drop"
  ) |>
  mutate(
    gpp_partition = case_when(n_gpp_nt > 0L ~ "NT", n_gpp_dt > 0L ~ "DT", TRUE ~ NA_character_),
    ter_partition = case_when(n_ter_nt > 0L ~ "NT", n_ter_dt > 0L ~ "DT", TRUE ~ NA_character_)
  ) |>
  select(site_id, gpp_partition, ter_partition)

monthly_raw <- monthly_raw |> left_join(site_partition, by = "site_id") |>
  mutate(
    gpp_gC = case_when(gpp_partition == "NT" ~ gpp_nt_gC, gpp_partition == "DT" ~ gpp_dt_gC, TRUE ~ NA_real_),
    ter_gC = case_when(ter_partition == "NT" ~ ter_nt_gC, ter_partition == "DT" ~ ter_dt_gC, TRUE ~ NA_real_)
  )

n_gpp_nt_sites <- sum(site_partition$gpp_partition == "NT", na.rm = TRUE)
n_gpp_dt_sites <- sum(site_partition$gpp_partition == "DT", na.rm = TRUE)
n_ter_nt_sites <- sum(site_partition$ter_partition == "NT", na.rm = TRUE)
n_ter_dt_sites <- sum(site_partition$ter_partition == "DT", na.rm = TRUE)
msg("GPP partitioning (site-level): NT=", n_gpp_nt_sites, "  DT (fallback)=", n_gpp_dt_sites)
msg("TER partitioning (site-level): NT=", n_ter_nt_sites, "  DT (fallback)=", n_ter_dt_sites)

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

tower_nee <- build_annual(monthly_raw, "nee_gC") |> mutate(flux = "NEE") |>
  left_join(site_carbon_src |> select(site_id, tower_carbon_src = carbon_src), by = "site_id") |>
  mutate(tower_partition = NA_character_)
tower_gpp <- build_annual(monthly_raw, "gpp_gC") |> mutate(flux = "GPP") |>
  left_join(site_carbon_src |> select(site_id, tower_carbon_src = carbon_src), by = "site_id") |>
  left_join(site_partition |> select(site_id, tower_partition = gpp_partition), by = "site_id")
tower_ter <- build_annual(monthly_raw, "ter_gC") |> mutate(flux = "TER") |>
  left_join(site_carbon_src |> select(site_id, tower_carbon_src = carbon_src), by = "site_id") |>
  left_join(site_partition |> select(site_id, tower_partition = ter_partition), by = "site_id")
tower_et  <- build_annual(monthly_raw, "et_mm")  |> mutate(flux = "ET",
                                                            tower_carbon_src = NA_character_,
                                                            tower_partition  = NA_character_)
tower_all <- bind_rows(tower_nee, tower_gpp, tower_ter, tower_et)
msg("Tower-side valid annual values: NEE=", nrow(tower_nee), " GPP=", nrow(tower_gpp),
    " TER=", nrow(tower_ter), " ET=", nrow(tower_et))

## ---- CUT-fallback impact vs Step 3 (VUT-only) ------------------------------
step3_path <- file.path(OUT_DIR, "table_step3_tower_annual_nee.csv")
if (file.exists(step3_path)) {
  step3_sites <- read_csv(step3_path, show_col_types = FALSE)$site_id
  n_cut_added <- tower_nee |> filter(tower_carbon_src == "CUT") |> filter(!site_id %in% step3_sites) |> nrow()
  n_cut_valid <- sum(tower_nee$tower_carbon_src == "CUT", na.rm = TRUE)
  msg("CUT fallback: ", n_cut_valid, " sites have a valid annual NEE via CUT; ",
      n_cut_added, " of those are NOT in Step 3's VUT-only table (", basename(step3_path), ", n=",
      length(step3_sites), ") -- i.e. added by this script's CUT fallback.")
} else {
  msg("Step 3 table not found at ", step3_path, " -- skipping CUT-fallback-vs-Step-3 comparison.")
}

## =====================================================================
## Combine tower + model, restrict to sites with both, write table
## =====================================================================
msg("\n=== Combining tower + model, sites with both values only ===")

combined <- tower_all |>
  select(site_id, flux, tower_value, n_years, tower_carbon_src, tower_partition) |>
  inner_join(model_all |> select(site_id, flux, model_value, n_models), by = c("site_id", "flux")) |>
  left_join(current_sites, by = "site_id") |>
  filter(!is.na(tower_value), !is.na(model_value)) |>
  select(site_id, flux, location_lat, location_long, tower_value, model_value,
         n_tower_years = n_years, tower_carbon_src, tower_partition, n_models)

for (fv in c("NEE", "GPP", "TER", "ET")) {
  msg("  ", fv, ": n sites with both tower and model value = ", sum(combined$flux == fv))
}

write_csv(combined, file.path(OUT_DIR, "table_dist_tower_vs_model.csv"))
write_meta(file.path(OUT_DIR, "table_dist_tower_vs_model.csv"),
           input_sources = c("data/duckdb/fluxnet.duckdb (monthly_converted)",
                              "data/snapshots/site_biomass_cci_v7.csv",
                              file.path(INTER_DIR, "<model>_{gpp,ra,rh}_annual_native_1991_2020.tif"),
                              file.path(INTER_DIR, "<model>_evapotrans_regridded.tif")),
           notes = paste(
             "Long table, one row per site x flux (NEE/GPP/TER/ET), restricted to sites with",
             "both a tower and a model value. Tower: per-site VUT->CUT fallback for carbon",
             "(never mixed within a site); GPP/TER share that VUT/CUT choice plus an independent",
             "per-site NT->DT partitioning fallback; ET from LE_F_MDS (own QC). Annual value =",
             "mean monthly cycle (QC >=", QC_THRESH_MM, ", all 12 calendar months required) summed,",
             "over all qualifying tower years (no 1991-2020 restriction). Model: ensemble median",
             "across the same 17 models as nee_corrected_axis.R Step 1, each model's 1991-2020",
             "mean bilinear-extracted at the tower coordinate (native grid for NEE/GPP/TER; the",
             "existing regridded 0.5deg common grid, subset to 1991-2020, for ET). Units: gC m-2",
             "yr-1 (NEE/GPP/TER), mm yr-1 (ET). NEE positive to atmosphere on both sides."))

## =====================================================================
## FIGURES
## =====================================================================
msg("\n=== Figures ===")
theme_set(theme_bw(base_size = 11))
FLUXES <- c("NEE", "GPP", "TER", "ET")
FLUX_UNIT <- c(NEE = "gC m-2 yr-1", GPP = "gC m-2 yr-1", TER = "gC m-2 yr-1", ET = "mm yr-1")

## ---- shared clip bounds: pooled tower+model values per flux, 1st-99th pctile
flux_bounds <- map(FLUXES, function(fv) {
  sub <- combined |> filter(flux == fv)
  pooled <- c(sub$tower_value, sub$model_value)
  as.numeric(quantile(pooled, c(0.01, 0.99), na.rm = TRUE))
})
names(flux_bounds) <- FLUXES
## GPP and TER share axis limits (both gC m-2 yr-1, comparable magnitude)
gpp_ter_bounds <- range(flux_bounds$GPP, flux_bounds$TER)
flux_bounds$GPP <- gpp_ter_bounds
flux_bounds$TER <- gpp_ter_bounds

n_clipped_for <- function(sub, lo, hi) {
  pooled <- c(sub$tower_value, sub$model_value)
  sum(pooled < lo | pooled > hi, na.rm = TRUE)
}

## ---- Fig 1: histograms, flux on y, count on x, tower/model dodged ---------
panel_hist <- function(fv) {
  sub <- combined |> filter(flux == fv)
  b <- flux_bounds[[fv]]
  n_clip <- n_clipped_for(sub, b[1], b[2])
  long <- bind_rows(
    sub |> transmute(series = "Tower", value = tower_value),
    sub |> transmute(series = "Model", value = model_value)
  )
  ggplot(long, aes(y = value, fill = series)) +
    geom_histogram(position = "dodge", bins = 30) +
    coord_cartesian(ylim = b) +
    scale_fill_manual(values = c(Tower = "#D55E00", Model = "grey40")) +
    labs(title = fv, y = paste0(fv, " (", FLUX_UNIT[[fv]], ")"), x = "Count", fill = NULL,
         subtitle = sprintf("n=%d sites; %d/%d pooled points outside [%.0f, %.0f]",
                             nrow(sub), n_clip, 2 * nrow(sub), b[1], b[2]))
}
fig1 <- wrap_plots(map(FLUXES, panel_hist), ncol = 2) +
  plot_annotation(title = "Tower vs. model NEE/GPP/TER/ET distributions")
ggsave(file.path(OUT_DIR, "fig_dist_histograms.png"), fig1, width = 11, height = 9, dpi = 150, bg = "white")

## ---- Fig 2: flux vs latitude, tower/model overlaid ------------------------
panel_lat <- function(fv) {
  sub <- combined |> filter(flux == fv)
  b <- flux_bounds[[fv]]
  n_clip <- n_clipped_for(sub, b[1], b[2])
  long <- bind_rows(
    sub |> transmute(series = "Tower", value = tower_value, lat = location_lat),
    sub |> transmute(series = "Model", value = model_value, lat = location_lat)
  )
  ggplot(long, aes(x = value, y = lat, color = series)) +
    geom_point(size = 1.2, alpha = 0.6) +
    coord_cartesian(xlim = b) +
    scale_color_manual(values = c(Tower = "#D55E00", Model = "steelblue")) +
    labs(title = fv, x = paste0(fv, " (", FLUX_UNIT[[fv]], ")"), y = "Latitude", color = NULL,
         subtitle = sprintf("n=%d sites; %d/%d pooled points outside [%.0f, %.0f]",
                             nrow(sub), n_clip, 2 * nrow(sub), b[1], b[2]))
}
fig2 <- wrap_plots(map(FLUXES, panel_lat), ncol = 2) +
  plot_annotation(title = "Tower vs. model NEE/GPP/TER/ET against latitude")
ggsave(file.path(OUT_DIR, "fig_dist_latitude.png"), fig2, width = 11, height = 9, dpi = 150, bg = "white")

## ---- Fig 3: tower (y) vs model (x), 1:1 line, equal axis limits -----------
panel_scatter <- function(fv) {
  sub <- combined |> filter(flux == fv)
  b <- flux_bounds[[fv]]
  n_clip <- n_clipped_for(sub, b[1], b[2])
  ggplot(sub, aes(x = model_value, y = tower_value)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(size = 1.2, alpha = 0.6, color = "#0072B2") +
    coord_equal(xlim = b, ylim = b) +
    labs(title = fv, x = paste0("Model (", FLUX_UNIT[[fv]], ")"), y = paste0("Tower (", FLUX_UNIT[[fv]], ")"),
         subtitle = sprintf("n=%d sites; %d/%d pooled points outside [%.0f, %.0f]",
                             nrow(sub), n_clip, 2 * nrow(sub), b[1], b[2]))
}
fig3 <- wrap_plots(map(FLUXES, panel_scatter), ncol = 2) +
  plot_annotation(title = "Tower vs. model NEE/GPP/TER/ET, 1:1 line")
ggsave(file.path(OUT_DIR, "fig_dist_scatter_1to1.png"), fig3, width = 10, height = 10, dpi = 150, bg = "white")

msg("\n=== flux_tower_model_distributions.R complete ===")
