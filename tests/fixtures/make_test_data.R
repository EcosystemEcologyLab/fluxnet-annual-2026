## tests/fixtures/make_test_data.R
##
## Generates mock processed data for 3 test sites in the format produced by
## 05_units.R (flux_data_converted_*.rds). Sites are taken from the committed
## snapshot CSV so metadata joins work in the diagnostic report.
##
## Run once before scripts/00_diagnostics.R to populate data/processed/.

set.seed(42)

FLUXNET_DATA_ROOT <- Sys.getenv("FLUXNET_DATA_ROOT", unset = "data")
processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Test site specifications (all present in committed snapshot CSVs) ----
sites <- list(
  list(site_id = "AR-CCg", igbp = "GRA", hub = "AmeriFlux",
       years = 2018:2024, lat = -35.92, lon = -61.19,
       nee_mu = -0.22, nee_sd = 0.07,
       gpp_mu = 0.85, gpp_sd = 0.12,
       le_mu  = 42,   le_sd  = 6),
  list(site_id = "AR-TF1", igbp = "WET", hub = "AmeriFlux",
       years = 2016:2018, lat = -54.97, lon = -66.73,
       nee_mu = -0.12, nee_sd = 0.05,
       gpp_mu = 0.52, gpp_sd = 0.08,
       le_mu  = 28,   le_sd  = 4),
  list(site_id = "BR-CST", igbp = "DNF", hub = "AmeriFlux",
       years = 2014:2015, lat = -7.97, lon = -38.38,
       nee_mu = -0.35, nee_sd = 0.10,
       gpp_mu = 1.30, gpp_sd = 0.18,
       le_mu  = 58,   le_sd  = 8)
)

# ---- Conversion factors ----
secs_yy  <- function(yr) ifelse(lubridate::leap_year(yr), 366, 365) * 86400
secs_mm  <- round(30.4375 * 86400)
secs_dd  <- 86400
C_FACTOR <- 12e-6          # µmol CO2 m-2 s-1  → gC m-2 per second
LE_DIV   <- 2.45e6         # W m-2 → mm H2O (denominator)

make_row <- function(site, nee_nat, gpp_nat, le_nat, qc, secs) {
  gpp_dt <- gpp_nat * runif(1, 0.93, 1.07)  # slight DT/NT divergence
  reco   <- gpp_nat + nee_nat               # RECO = GPP + NEE (sign convention)
  list(
    site_id             = site$site_id,
    NEE_VUT_REF         = nee_nat * C_FACTOR * secs,
    NEE_VUT_REF_native  = nee_nat,
    NEE_VUT_REF_QC      = pmin(pmax(qc, 0), 1),
    GPP_NT_VUT_REF      = gpp_nat  * C_FACTOR * secs,
    GPP_DT_VUT_REF      = gpp_dt   * C_FACTOR * secs,
    RECO_NT_VUT_REF     = reco     * C_FACTOR * secs,
    LE_F_MDS            = le_nat * secs / LE_DIV,
    LE_F_MDS_native     = le_nat,
    LE_F_MDS_QC         = pmin(pmax(qc + rnorm(1, 0, 0.05), 0), 1)
  )
}

# ============================================================
# YY — one row per site-year
# ============================================================
yy_rows <- list()
for (s in sites) {
  for (yr in s$years) {
    nee_nat <- rnorm(1, s$nee_mu, s$nee_sd)
    gpp_nat <- rnorm(1, s$gpp_mu, s$gpp_sd)
    le_nat  <- rnorm(1, s$le_mu,  s$le_sd)
    qc      <- rbeta(1, 8, 2)           # mostly high quality
    row     <- make_row(s, nee_nat, gpp_nat, le_nat, qc, secs_yy(yr))
    row$TIMESTAMP <- as.integer(yr)
    yy_rows[[length(yy_rows) + 1]] <- as.data.frame(row)
  }
}
yy <- do.call(rbind, yy_rows)
attr(yy, "flux_resolution") <- "YY"
saveRDS(yy, file.path(processed_dir, "flux_data_converted_yy.rds"))
message("Saved flux_data_converted_yy.rds  (", nrow(yy), " rows)")

# ============================================================
# MM — one row per site-year-month
# ============================================================
mm_rows <- list()
for (s in sites) {
  for (yr in s$years) {
    for (mo in 1:12) {
      # Seasonal amplitude: NEE most negative in growing season (austral summer ≈ Jan)
      seas  <- cos((mo - 1) * 2 * pi / 12)
      nee_nat <- rnorm(1, s$nee_mu + 0.10 * seas, s$nee_sd * 0.6)
      gpp_nat <- rnorm(1, s$gpp_mu * (1 + 0.3 * seas), s$gpp_sd * 0.6)
      le_nat  <- rnorm(1, s$le_mu  * (1 + 0.25 * seas), s$le_sd * 0.6)
      qc      <- rbeta(1, 8, 2)
      row     <- make_row(s, nee_nat, gpp_nat, le_nat, qc, secs_mm)
      row$TIMESTAMP <- as.integer(yr * 100 + mo)
      mm_rows[[length(mm_rows) + 1]] <- as.data.frame(row)
    }
  }
}
mm <- do.call(rbind, mm_rows)
attr(mm, "flux_resolution") <- "MM"
saveRDS(mm, file.path(processed_dir, "flux_data_converted_mm.rds"))
message("Saved flux_data_converted_mm.rds  (", nrow(mm), " rows)")

# ============================================================
# DD — one row per site-year-day (limited to 2 most recent years per site
#      to keep file size reasonable)
# ============================================================
dd_rows <- list()
for (s in sites) {
  yr_range <- tail(s$years, 2)
  for (yr in yr_range) {
    n_days <- if (lubridate::leap_year(yr)) 366L else 365L
    for (doy in seq_len(n_days)) {
      seas    <- cos((doy - 1) * 2 * pi / n_days)
      nee_nat <- rnorm(1, s$nee_mu + 0.15 * seas, s$nee_sd * 0.8)
      gpp_nat <- rnorm(1, s$gpp_mu * (1 + 0.35 * seas), s$gpp_sd * 0.8)
      le_nat  <- rnorm(1, s$le_mu  * (1 + 0.30 * seas), s$le_sd * 0.8)
      qc      <- rbeta(1, 7, 2)
      row     <- make_row(s, nee_nat, gpp_nat, le_nat, qc, secs_dd)
      date_val   <- as.Date(paste0(yr, "-01-01")) + (doy - 1)
      row$TIMESTAMP <- as.integer(format(date_val, "%Y%m%d"))
      row$DOY    <- doy
      dd_rows[[length(dd_rows) + 1]] <- as.data.frame(row)
    }
  }
}
dd <- do.call(rbind, dd_rows)
attr(dd, "flux_resolution") <- "DD"
saveRDS(dd, file.path(processed_dir, "flux_data_converted_dd.rds"))
message("Saved flux_data_converted_dd.rds  (", nrow(dd), " rows)")

# Minimal file_inventory so load_best() fallback paths work
inv <- data.frame(
  site_id          = rep(sapply(sites, `[[`, "site_id"), each = 3),
  time_resolution  = rep(c("YY","MM","DD"), length(sites)),
  dataset          = "FLUXNET",
  path             = NA_character_,
  stringsAsFactors = FALSE
)
saveRDS(inv, file.path(processed_dir, "file_inventory.rds"))
message("Saved file_inventory.rds")
message("Done. Run scripts/00_diagnostics.R to generate the report.")
