## precip_site_filter.R
##
## Evidence-building for a defensible site-inclusion rule for the
## precipitation input to the site-side Koppen classification, to replace
## the flat KG_ERA5_MAP_MAX_MM = 5000 cutoff in R/pipeline_config.R.
##
## Read-only: this script does not edit R/pipeline_config.R, any pipeline
## script (01-07), or any already-committed figure. It does not decide
## inclusion/exclusion and does not extend to any axis other than
## precipitation.
##
## Resolution scope: the core site-level precipitation table (four MAP
## estimates) and plots 1-6 are built from YY-resolution DuckDB tables only
## (`annual_converted`) -- DD, MM and HH FLUXMET/ERA5 files are not read for
## that purpose. The one deliberate exception is the candidate-rule ->
## Koppen-reclassification step (section 8), which necessarily reuses the
## pipeline's own climate-normal machinery in R/climate_classification.R
## (`compute_era5_monthly_climatology()`), which operates on MM-resolution
## ERA5 data by construction -- Koppen classification itself requires a
## 12-month climatology and was not reimplemented at YY resolution here,
## since that would fork the classification method, not just this
## diagnostic's precipitation input screen. This is flagged, not silent.
##
## Network-wide (781 sites), DuckDB/vectorized -- no per-site loops reading
## raw CSVs, except one small grep-based scan of the 781 already-deduplicated
## BIF files for BADM MAP / elevation / real ONEFlux processing version
## (data/processed/badm.rds is NOT used -- it was last built 2026-06-02,
## before the 2026-09-20 store refresh re-extracted several sites' BIF files;
## see report.md).

source("R/pipeline_config.R")
source("R/utils.R")
source("R/climate_classification.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(fs)
  library(ggplot2)
  library(duckdb)
  library(DBI)
  library(scales)
  library(tibble)
})

OUTD <- "review/diagnostics/precip_site_filter"
fs::dir_create(OUTD)

message("=== precip_site_filter.R ===")

# ============================================================================
# What this analysis cannot decide (stated up front; also in report.md)
# ============================================================================
# 1. Whether a site SHOULD be excluded -- only how many sites/site-years each
#    candidate rule affects and how classification would shift, not whether
#    that trade-off is worth accepting.
# 2. Whether "measured" P_F (P_F_QC above threshold) is itself unbiased --
#    the same small-contaminated-fraction mechanism found at HH resolution
#    for US-HB4 (it_mbo_parsimony/report.md) can still operate inside a
#    nominally "measured" YY value if the annual P_F_QC fraction is high but
#    not 1.0.
# 3. Anything about the ~123-site 4x/8x cluster/factor question -- explicitly
#    out of scope here (store_refresh_20260920/report.md, Stage 6).
# 4. Whether WorldClim BIO12 or BADM MAP are themselves "ground truth" -- both
#    disagree with each other too (plot 2's BADM/BIO12 panel is the reference
#    for how much).

# ============================================================================
# 1. Canonical, deduplicated file manifest (current on-disk state)
# ============================================================================
message("\n================ Section 1: file manifest ================")

dup_msg <- NULL
file_manifest <- withCallingHandlers(
  fluxnet::flux_discover_files(data_dir = file.path(FLUXNET_DATA_ROOT, "extracted")),
  warning = function(w) {
    if (grepl("duplicate files removed", conditionMessage(w))) {
      dup_msg <<- conditionMessage(w)
    }
    invokeRestart("muffleWarning")
  }
)
n_dup <- if (!is.null(dup_msg)) {
  as.integer(sub("^([0-9]+) duplicate.*", "\\1", dup_msg))
} else {
  0L
}
message(n_dup, " duplicate/stale extraction-directory files were present on ",
        "disk and excluded by flux_discover_files() (which keeps the most ",
        "recent release per site).")

bif_manifest <- file_manifest |> dplyr::filter(dataset == "BIF")
message("Canonical BIF files (one per site): ", nrow(bif_manifest))

site_meta <- bif_manifest |>
  dplyr::distinct(site_id, data_hub, network, oneflux_code_version,
                   release_version, product_id, location_lat, location_long,
                   igbp, path) |>
  dplyr::rename(bif_path = path, oneflux_code_version_manifest = oneflux_code_version)

n_sites <- nrow(site_meta)
message("Sites in current network (from canonical BIF manifest): ", n_sites)
if (n_sites != 781L) {
  message("NOTE: expected 781 sites per the live snapshot/store-refresh report; got ", n_sites,
          " -- reported as a discrepancy below, not silently reconciled.")
}

# ============================================================================
# 2. BADM MAP / elevation / real ONEFlux processing version, from the
#    canonical (deduplicated) BIF files -- NOT from data/processed/badm.rds
# ============================================================================
message("\n================ Section 2: BADM MAP / elevation / ONEFlux version (fresh from BIF) ================")

badm_lines <- system2("grep",
  args = c("-hE", shQuote(",(MAP|LOCATION_ELEV|PRODUCT_ONEFLUX_VERSION),"),
           shQuote(site_meta$bif_path)),
  stdout = TRUE)
message("BADM rows matched across ", nrow(site_meta), " canonical BIF files: ", length(badm_lines))

badm_raw <- readr::read_csv(
  I(badm_lines),
  col_names = c("site_id", "group_id", "variable_group", "variable", "datavalue"),
  col_types = readr::cols(.default = "c"),
  show_col_types = FALSE
)

# A few sites carry more than one GRP_CLIM_AVG/GRP_LOCATION block (e.g.
# re-surveyed elevation); keep the first occurrence per site x variable and
# report how many sites had more than one so this is visible, not silent.
badm_multi <- badm_raw |> dplyr::count(site_id, variable) |> dplyr::filter(n > 1L)
if (nrow(badm_multi) > 0L) {
  message(dplyr::n_distinct(badm_multi$site_id), " site(s) had more than one BIF row for ",
          "at least one of MAP/LOCATION_ELEV/PRODUCT_ONEFLUX_VERSION -- first occurrence kept.")
}

badm_wide <- badm_raw |>
  dplyr::group_by(site_id, variable) |>
  dplyr::slice(1L) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(id_cols = site_id, names_from = variable, values_from = datavalue) |>
  dplyr::transmute(
    site_id,
    badm_map_mm            = suppressWarnings(as.numeric(.data[["MAP"]])),
    elevation_m            = suppressWarnings(as.numeric(.data[["LOCATION_ELEV"]])),
    oneflux_code_version   = .data[["PRODUCT_ONEFLUX_VERSION"]]
  )

message("BADM MAP present for ", sum(!is.na(badm_wide$badm_map_mm)), " / ", n_sites, " sites.")
message("LOCATION_ELEV present for ", sum(!is.na(badm_wide$elevation_m)), " / ", n_sites, " sites.")
message("PRODUCT_ONEFLUX_VERSION present for ", sum(!is.na(badm_wide$oneflux_code_version)), " / ", n_sites, " sites; ",
        "distinct values: ", dplyr::n_distinct(badm_wide$oneflux_code_version, na.rm = TRUE))

# Spot-check against the three previously-studied sites' known BADM values
# (it_mbo_parsimony/report.md: IT-MBo 1365, US-HB4 1429, FI-Hyy 711 mm/yr).
spot <- badm_wide |> dplyr::filter(site_id %in% c("IT-MBo", "US-HB4", "FI-Hyy"))
message("Spot check (expect IT-MBo 1365, US-HB4 1429, FI-Hyy 711):")
print(as.data.frame(spot))

# ============================================================================
# 3. WorldClim BIO12 -- reused from the existing extraction (static raster,
#    coordinate-derived, not sensitive to the store refresh)
# ============================================================================
message("\n================ Section 3: WorldClim BIO12 (reused extraction) ================")

wc_path <- "data/snapshots/site_worldclim.csv"
if (!file.exists(wc_path)) stop("WorldClim extraction not found: ", wc_path)
worldclim <- readr::read_csv(wc_path, show_col_types = FALSE) |>
  dplyr::select(site_id, bio12_mm = map_worldclim)
message("WorldClim BIO12 present for ", sum(!is.na(worldclim$bio12_mm)), " sites in ", wc_path,
        " (dated ", format(file.info(wc_path)$mtime, "%Y-%m-%d"), ").")

# ============================================================================
# 4. Core YY-resolution precipitation series, from the current (post-refresh)
#    DuckDB `annual_converted` table
# ============================================================================
message("\n================ Section 4: YY-resolution P_ERA / P_F from DuckDB ================")

duckdb_path <- file.path(FLUXNET_DATA_ROOT, "duckdb", "fluxnet.duckdb")
if (!file.exists(duckdb_path)) stop("DuckDB not found: ", duckdb_path)
message("DuckDB: ", duckdb_path, " (dated ", format(file.info(duckdb_path)$mtime, "%Y-%m-%d %H:%M"), ")")

con <- dbConnect(duckdb(), dbdir = duckdb_path, read_only = TRUE)
era5_yy_raw <- dbGetQuery(con,
  "SELECT site_id, TIMESTAMP AS year, P_ERA FROM annual_converted WHERE dataset = 'ERA5'")
fm_yy_raw <- dbGetQuery(con,
  "SELECT site_id, TIMESTAMP AS year, P_F, P_F_QC FROM annual_converted WHERE dataset = 'FLUXMET'")
dbDisconnect(con, shutdown = TRUE)

era5_yy <- era5_yy_raw |> dplyr::semi_join(site_meta, by = "site_id")
fm_yy   <- fm_yy_raw   |> dplyr::semi_join(site_meta, by = "site_id")
message("ERA5 YY rows: ", nrow(era5_yy), " (", dplyr::n_distinct(era5_yy$site_id), " sites); ",
        "FLUXMET YY rows: ", nrow(fm_yy), " (", dplyr::n_distinct(fm_yy$site_id), " sites).")

# ---- Empirical P_F_QC polarity check at YY resolution (verify, don't assume --
#      CLAUDE.md flags the coarse-resolution fraction field as a common source
#      of errors, and a prior polarity flip was found at MM resolution but not
#      HH; YY has not previously been checked in this repo) ----
message("\n---- Empirical P_F_QC polarity check at YY ----")
qc_check <- fm_yy |>
  dplyr::inner_join(era5_yy, by = c("site_id", "year")) |>
  dplyr::filter(!is.na(P_F_QC), !is.na(P_F), !is.na(P_ERA))
qc_cut <- stats::quantile(qc_check$P_F_QC, probs = c(0.1, 0.9), na.rm = TRUE)
low_qc  <- qc_check |> dplyr::filter(P_F_QC <= qc_cut[1])
high_qc <- qc_check |> dplyr::filter(P_F_QC >= qc_cut[2])
frac_era_identical <- function(d) mean(abs(d$P_F - d$P_ERA) < 1e-6, na.rm = TRUE)
message(sprintf(
  "Bottom decile of P_F_QC (<= %.3f, n=%d): P_F == P_ERA exactly in %.1f%% of site-years.",
  qc_cut[1], nrow(low_qc), 100 * frac_era_identical(low_qc)))
message(sprintf(
  "Top decile of P_F_QC (>= %.3f, n=%d): P_F == P_ERA exactly in %.1f%% of site-years.",
  qc_cut[2], nrow(high_qc), 100 * frac_era_identical(high_qc)))
polarity_confirmed <- frac_era_identical(low_qc) > frac_era_identical(high_qc)
message("YY P_F_QC polarity (higher = more measured) ",
        if (polarity_confirmed) "CONFIRMED" else "NOT CONFIRMED -- see report.md",
        " by this empirical check.")

# ---- Site-level aggregates ----
era5_agg <- era5_yy |>
  dplyr::filter(!is.na(P_ERA)) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    p_era_mean_mm            = mean(P_ERA),
    p_era_sd_mm              = stats::sd(P_ERA),
    n_years_era_full_record  = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(p_era_cv = p_era_sd_mm / p_era_mean_mm)

measured_years <- fm_yy |>
  dplyr::filter(!is.na(P_F), !is.na(P_F_QC), P_F_QC > QC_THRESHOLD_YY)
measured_agg <- measured_years |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(p_measured_mean_mm = mean(P_F), n_years_measured = dplyr::n(), .groups = "drop")

fm_totals <- fm_yy |>
  dplyr::filter(!is.na(P_F)) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_years_fm_total = dplyr::n(), .groups = "drop")

site_table <- site_meta |>
  dplyr::select(site_id, data_hub, network, oneflux_code_version_manifest, release_version,
                product_id, location_lat, location_long, igbp) |>
  dplyr::left_join(era5_agg,   by = "site_id") |>
  dplyr::left_join(measured_agg, by = "site_id") |>
  dplyr::left_join(fm_totals,  by = "site_id") |>
  dplyr::left_join(worldclim,  by = "site_id") |>
  dplyr::left_join(badm_wide,  by = "site_id") |>
  dplyr::mutate(
    frac_measured  = n_years_measured / n_years_fm_total,
    record_length_years = n_years_fm_total,
    log_ratio_era_bio12    = log10(p_era_mean_mm / bio12_mm),
    log_ratio_measured_bio12 = log10(p_measured_mean_mm / bio12_mm),
    log_ratio_era_measured = log10(p_era_mean_mm / p_measured_mean_mm),
    log_ratio_badm_bio12   = log10(badm_map_mm / bio12_mm)
  )

out_site_table <- file.path(OUTD, "table_1_site_level_precip_estimates.csv")
readr::write_csv(site_table, out_site_table)
write_output_metadata(out_site_table,
  input_sources = c(duckdb_path, wc_path, "canonical BIF files via flux_discover_files() (see file_name list in table_5_provenance.csv)"),
  notes = paste0(
    "One row per current-network site (n=", n_sites, "). p_era_mean_mm: mean of YY-resolution ",
    "P_ERA over its full ERA5 record (all non-NA years, typically ~1981-2025). ",
    "p_measured_mean_mm: mean of YY-resolution P_F over years where P_F_QC > QC_THRESHOLD_YY (",
    QC_THRESHOLD_YY, ", the existing pipeline-wide coarse-resolution QC threshold from ",
    "R/pipeline_config.R), NOT the NEE-based row exclusion in annual_qc/04_qc.R (that targets ",
    "flux variables, not precipitation). bio12_mm: WorldClim v2.1 BIO12 at the tower cell ",
    "(data/snapshots/site_worldclim.csv, 2026-09-01, static raster). badm_map_mm/elevation_m/",
    "oneflux_code_version: freshly extracted from each site's currently-canonical BIF file ",
    "(flux_discover_files()-deduplicated, i.e. post-2026-09-20-refresh where applicable), NOT ",
    "from data/processed/badm.rds (built 2026-06-02, predates the refresh for several ",
    "re-extracted sites). Annual resolution only throughout; DD/MM/HH FLUXMET/ERA5 files were ",
    "not read for this table."
  ))
message("Saved: ", out_site_table)

# Coverage report
coverage <- tibble::tibble(
  column = names(site_table),
  n_non_na = purrr::map_int(site_table, ~ sum(!is.na(.x))),
  pct_non_na = round(100 * n_non_na / n_sites, 1)
)
out_coverage <- file.path(OUTD, "table_2_column_coverage.csv")
readr::write_csv(coverage, out_coverage)
write_output_metadata(out_coverage, input_sources = out_site_table,
  notes = "Non-NA coverage of every column in table_1_site_level_precip_estimates.csv, out of 781 sites -- a candidate rule that depends on a field missing at many sites cannot be applied network-wide.")
message("\nColumn coverage:")
print(as.data.frame(coverage))

CLR_ERA <- "#D55E00"      # matches it_mbo_parsimony.R's P_ERA colour
CLR_MEASURED <- "#0072B2" # matches it_mbo_parsimony.R's P_F colour

theme_precip <- function() {
  ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.background = ggplot2::element_rect(fill = "grey85", color = NA),
      legend.position   = "bottom"
    )
}

save_fig <- function(path, plot, width, height, notes, input_sources = out_site_table) {
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 200, bg = "white")
  write_output_metadata(path, input_sources = input_sources, notes = notes)
  message("Saved: ", path)
}

# ============================================================================
# 5. Plot 1: the four estimates against each other, log axes, 1:1 line
# ============================================================================
message("\n================ Section 5: Plot 1 (pairwise comparison) ================")

pairwise_long <- function(df, xvar, yvar, xlab, ylab) {
  df |>
    dplyr::filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]]),
                  .data[[xvar]] > 0, .data[[yvar]] > 0) |>
    dplyr::transmute(site_id, x = .data[[xvar]], y = .data[[yvar]],
                      panel = paste0(ylab, " vs. ", xlab))
}

p1_data <- dplyr::bind_rows(
  pairwise_long(site_table, "bio12_mm", "p_era_mean_mm", "BIO12", "P_ERA"),
  pairwise_long(site_table, "badm_map_mm", "p_era_mean_mm", "BADM", "P_ERA"),
  pairwise_long(site_table, "p_measured_mean_mm", "p_era_mean_mm", "measured", "P_ERA"),
  pairwise_long(site_table, "bio12_mm", "p_measured_mean_mm", "BIO12", "measured"),
  pairwise_long(site_table, "badm_map_mm", "p_measured_mean_mm", "BADM", "measured"),
  pairwise_long(site_table, "bio12_mm", "badm_map_mm", "BIO12", "BADM")
)

p1 <- ggplot2::ggplot(p1_data, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  ggplot2::geom_point(alpha = 0.35, size = 1.2, color = "grey20") +
  ggplot2::scale_x_log10(labels = scales::label_number(big.mark = " ")) +
  ggplot2::scale_y_log10(labels = scales::label_number(big.mark = " ")) +
  ggplot2::facet_wrap(~panel, scales = "free", ncol = 3) +
  ggplot2::labs(x = "mm/yr (log scale)", y = "mm/yr (log scale)",
                title = "Four mean-annual-precipitation estimates against each other, 781 sites",
                subtitle = "Dashed line: 1:1. Each panel: y-axis estimate vs. x-axis estimate.") +
  theme_precip() + ggplot2::theme(legend.position = "none")

save_fig(file.path(OUTD, "fig_1_pairwise_comparison.png"), p1, width = 11, height = 7.5,
  notes = "Six pairwise log-log scatterplots (P_ERA, measured P_F, BIO12, BADM), 1:1 reference line, 781 sites. From table_1_site_level_precip_estimates.csv; no value recomputed for the plot.")

# ============================================================================
# 6. Plot 2: histograms of log ratios, data-driven (Freedman-Diaconis) bins,
#    no integer-factor bin edges, no fitted factors, no tolerance windows
# ============================================================================
message("\n================ Section 6: Plot 2 (log-ratio histograms) ================")

fd_hist_df <- function(x, label) {
  x <- x[is.finite(x)]
  h <- graphics::hist(x, breaks = "FD", plot = FALSE)
  tibble::tibble(
    panel     = label,
    mid       = h$mids,
    count     = h$counts,
    bin_width = diff(h$breaks)[1],
    n         = length(x)
  )
}

hist_df <- dplyr::bind_rows(
  fd_hist_df(site_table$log_ratio_era_bio12,      "log10(P_ERA / BIO12)"),
  fd_hist_df(site_table$log_ratio_measured_bio12, "log10(measured / BIO12)"),
  fd_hist_df(site_table$log_ratio_era_measured,   "log10(P_ERA / measured)"),
  fd_hist_df(site_table$log_ratio_badm_bio12,     "log10(BADM / BIO12) -- reference")
)
hist_df$panel <- factor(hist_df$panel, levels = c(
  "log10(BADM / BIO12) -- reference", "log10(P_ERA / BIO12)",
  "log10(measured / BIO12)", "log10(P_ERA / measured)"))

n_by_panel <- hist_df |> dplyr::distinct(panel, n)
message("Freedman-Diaconis histogram sample sizes: ")
print(as.data.frame(n_by_panel))

p2 <- ggplot2::ggplot(hist_df, ggplot2::aes(x = mid, y = count, width = bin_width * 0.95)) +
  ggplot2::geom_col(fill = "grey40") +
  ggplot2::geom_vline(xintercept = 0, color = CLR_ERA, linewidth = 0.6) +
  ggplot2::facet_wrap(~panel, scales = "free", ncol = 2) +
  ggplot2::labs(x = "log10 ratio", y = "n sites",
                title = "Log-ratio distributions, 781 sites (Freedman-Diaconis bin width per panel)",
                subtitle = "Orange line: ratio = 1 (log10 = 0). Top-left panel: natural BADM-vs-BIO12 disagreement, no tower data involved.") +
  theme_precip()

save_fig(file.path(OUTD, "fig_2_log_ratio_histograms.png"), p2, width = 10, height = 7.5,
  notes = "Histograms of four log10 ratios, Freedman-Diaconis bin width chosen per panel from that panel's own data (graphics::hist(breaks='FD')) -- no integer-factor bin edges, no fitted factors, no tolerance windows. From table_1_site_level_precip_estimates.csv.")

# ============================================================================
# 7. Plot 3: |log ratio (P_ERA vs measured)| against the expected
#    sampling-error envelope (CV / sqrt(n_years_measured))
# ============================================================================
message("\n================ Section 7: Plot 3 (sampling-error envelope) ================")

p3_data <- site_table |>
  dplyr::filter(!is.na(log_ratio_era_measured), !is.na(p_era_cv), !is.na(n_years_measured),
                n_years_measured > 0) |>
  dplyr::mutate(
    abs_log_ratio = abs(log_ratio_era_measured),
    sampling_envelope = p_era_cv / sqrt(n_years_measured)
  )

p3 <- ggplot2::ggplot(p3_data, ggplot2::aes(x = sampling_envelope, y = abs_log_ratio)) +
  ggplot2::geom_point(alpha = 0.4, size = 1.3, color = "grey20") +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = CLR_MEASURED, linewidth = 0.7) +
  ggplot2::geom_abline(slope = 1.96, intercept = 0, color = CLR_MEASURED, linewidth = 0.5, linetype = "dashed") +
  ggplot2::annotate("text", x = max(p3_data$sampling_envelope, na.rm = TRUE),
                     y = max(p3_data$sampling_envelope, na.rm = TRUE), label = "1x envelope",
                     color = CLR_MEASURED, hjust = 1, vjust = -0.5, size = 3) +
  ggplot2::labs(
    x = expression(paste("Expected sampling error of a short-record mean:  CV(", P[ERA], ") / ", sqrt(n[measured]))),
    y = "abs(log10(P_ERA / measured))",
    title = "Disagreement vs. expected sampling error, 781 sites",
    subtitle = "Solid: 1x envelope. Dashed: ~95% band (1.96x). Points above the line disagree by\nmore than short-record sampling error alone explains."
  ) +
  theme_precip()

save_fig(file.path(OUTD, "fig_3_sampling_envelope.png"), p3, width = 8, height = 6.5,
  notes = "abs(log10(P_ERA/measured)) against CV(P_ERA, full record)/sqrt(n_years_measured), the expected relative sampling error of a short-record mean. Reference lines at slope 1 (envelope itself) and 1.96 (~95% band), not fitted to this dataset. From table_1_site_level_precip_estimates.csv.")

n_above_envelope <- sum(p3_data$abs_log_ratio > p3_data$sampling_envelope, na.rm = TRUE)
n_above_95 <- sum(p3_data$abs_log_ratio > 1.96 * p3_data$sampling_envelope, na.rm = TRUE)
message(sprintf("Sites with |log ratio| exceeding the 1x sampling envelope: %d / %d",
                n_above_envelope, nrow(p3_data)))
message(sprintf("Sites with |log ratio| exceeding the ~95%% (1.96x) band: %d / %d",
                n_above_95, nrow(p3_data)))

# ============================================================================
# 8. Plot 4: P_ERA against measured precipitation (QC-selected years only)
# ============================================================================
message("\n================ Section 8: Plot 4 (P_ERA vs. QC-measured precip) ================")
# Interpretive choice, stated explicitly: the task's item 4 says "computed
# only over the months the QC flags call measured" but also restricts this
# whole analysis to YY tables (no DD/MM/HH reads). At YY resolution there is
# no month-level QC field to drop down to, so "measured" here is read as the
# same annual, QC-selected series already defined above (P_F_QC >
# QC_THRESHOLD_YY), not a monthly-QC-filtered annual total. This is the
# closest annual-only equivalent and is used consistently throughout this
# script, not just for this one plot.

p4_data <- site_table |>
  dplyr::filter(!is.na(p_era_mean_mm), !is.na(p_measured_mean_mm),
                p_era_mean_mm > 0, p_measured_mean_mm > 0)

p4 <- ggplot2::ggplot(p4_data, ggplot2::aes(x = p_measured_mean_mm, y = p_era_mean_mm)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  ggplot2::geom_point(alpha = 0.4, size = 1.4, color = CLR_ERA) +
  ggplot2::scale_x_log10(labels = scales::label_number(big.mark = " ")) +
  ggplot2::scale_y_log10(labels = scales::label_number(big.mark = " ")) +
  ggplot2::labs(x = "Measured precipitation, QC-selected years (mm/yr, log scale)",
                y = "P_ERA (mm/yr, log scale)",
                title = "P_ERA against measured precipitation, QC-selected years only",
                subtitle = sprintf("%d sites with a qualifying measured value (P_F_QC > %.2f in >=1 year). Dashed: 1:1.",
                                    nrow(p4_data), QC_THRESHOLD_YY)) +
  theme_precip() + ggplot2::theme(legend.position = "none")

save_fig(file.path(OUTD, "fig_4_era_vs_measured.png"), p4, width = 7, height = 6.5,
  notes = "P_ERA (full-record mean) against measured P_F (mean over P_F_QC > QC_THRESHOLD_YY years), log-log, 1:1 line. 'Measured' is annual-resolution QC-selected, not month-level (see script header comment on this interpretive choice -- YY-only scope, no MM read). From table_1_site_level_precip_estimates.csv.")

# ============================================================================
# 9. Plot 5: the ten sites with the largest P_ERA/measured ratio -- full
#    annual record of P_ERA and measured P_F, to check constant offset vs.
#    tracking
# ============================================================================
message("\n================ Section 9: Plot 5 (top-10 P_ERA/measured ratio, time series) ================")

top10_sites <- site_table |>
  dplyr::filter(!is.na(log_ratio_era_measured)) |>
  dplyr::arrange(dplyr::desc(log_ratio_era_measured)) |>
  dplyr::slice_head(n = 10) |>
  dplyr::pull(site_id)
message("Top 10 sites by P_ERA/measured ratio: ", paste(top10_sites, collapse = ", "))

p5_era <- era5_yy |> dplyr::filter(site_id %in% top10_sites) |>
  dplyr::transmute(site_id, year, value = P_ERA, series = "P_ERA (full record)")
p5_fm  <- fm_yy |> dplyr::filter(site_id %in% top10_sites, !is.na(P_F)) |>
  dplyr::transmute(site_id, year, value = P_F, series = "P_F (all years, not QC-restricted)")
p5_data <- dplyr::bind_rows(p5_era, p5_fm) |>
  dplyr::mutate(site_id = factor(site_id, levels = top10_sites))

p5 <- ggplot2::ggplot(p5_data, ggplot2::aes(x = year, y = value, color = series)) +
  ggplot2::geom_line(linewidth = 0.6) +
  ggplot2::geom_point(size = 1.2) +
  ggplot2::scale_color_manual(values = c("P_ERA (full record)" = CLR_ERA,
                                          "P_F (all years, not QC-restricted)" = CLR_MEASURED),
                               name = NULL) +
  ggplot2::facet_wrap(~site_id, scales = "free_y", ncol = 5) +
  ggplot2::labs(x = "Year", y = "Annual precipitation (mm/yr)",
                title = "Ten sites with the largest P_ERA/measured ratio: full annual record",
                subtitle = "Constant vertical offset would suggest a scale/factor artifact; parallel tracking would not.") +
  theme_precip()

save_fig(file.path(OUTD, "fig_5_top10_ratio_timeseries.png"), p5, width = 13, height = 6,
  notes = paste0("Full annual P_ERA and P_F record (all years, not just QC-measured) for the ",
                  "10 sites with the largest P_ERA/measured ratio: ", paste(top10_sites, collapse = ", "),
                  ". From the same DuckDB annual_converted query as table_1; no value recomputed."))

# ============================================================================
# 10. Plot 6: log ratio (P_ERA/BIO12, broadest coverage) against BIO12,
#     record length, fraction measured, hub, and ONeflux code version
# ============================================================================
message("\n================ Section 10: Plot 6 (log ratio vs. covariates) ================")
# Interpretive choice: "log ratio" for this panel uses log10(P_ERA/BIO12),
# not log10(P_ERA/measured), because BIO12 is available for all 781 sites
# while "measured" is only available for 587 -- using the broader-coverage
# ratio here avoids silently restricting this covariate scan to a 75%
# subsample. Both ratios are in table_1 for anyone who wants the other view.
# "hub" uses the manifest's data_hub field (AmeriFlux/ICOS/TERN, 3 clean
# categories), not the multi-valued `network` field, and never infers hub
# from site_id prefix (CLAUDE.md Hard Rule #2).

p6a <- ggplot2::ggplot(site_table, ggplot2::aes(x = bio12_mm, y = log_ratio_era_bio12)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey50") +
  ggplot2::geom_point(alpha = 0.35, size = 1.1, color = "grey20") +
  ggplot2::scale_x_log10(labels = scales::label_number(big.mark = " ")) +
  ggplot2::labs(x = "BIO12 (mm/yr, log scale)", y = "log10(P_ERA / BIO12)") + theme_precip()

p6b <- ggplot2::ggplot(site_table, ggplot2::aes(x = record_length_years, y = log_ratio_era_bio12)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey50") +
  ggplot2::geom_point(alpha = 0.35, size = 1.1, color = "grey20") +
  ggplot2::labs(x = "Record length (years, FLUXMET YY rows)", y = "log10(P_ERA / BIO12)") + theme_precip()

p6c <- ggplot2::ggplot(site_table, ggplot2::aes(x = frac_measured, y = log_ratio_era_bio12)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey50") +
  ggplot2::geom_point(alpha = 0.35, size = 1.1, color = "grey20") +
  ggplot2::labs(x = "Fraction of years QC-measured", y = "log10(P_ERA / BIO12)") + theme_precip()

p6d <- ggplot2::ggplot(site_table, ggplot2::aes(x = data_hub, y = log_ratio_era_bio12)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey50") +
  ggplot2::geom_boxplot(outlier.alpha = 0.4, fill = "grey90") +
  ggplot2::labs(x = "Data hub", y = "log10(P_ERA / BIO12)") + theme_precip()

p6e <- ggplot2::ggplot(site_table |> dplyr::filter(!is.na(oneflux_code_version)),
                        ggplot2::aes(x = oneflux_code_version, y = log_ratio_era_bio12)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey50") +
  ggplot2::geom_boxplot(outlier.alpha = 0.4, fill = "grey90") +
  ggplot2::labs(x = "ONEFlux processing version (from BIF PRODUCT_ONEFLUX_VERSION)",
                y = "log10(P_ERA / BIO12)") +
  theme_precip() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

p6_has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
if (!p6_has_patchwork) {
  message("patchwork not installed -- saving plot 6's five panels as separate files instead of one composite.")
  save_fig(file.path(OUTD, "fig_6a_ratio_vs_bio12.png"), p6a, width = 6, height = 5,
    notes = "log10(P_ERA/BIO12) against BIO12. From table_1_site_level_precip_estimates.csv.")
  save_fig(file.path(OUTD, "fig_6b_ratio_vs_record_length.png"), p6b, width = 6, height = 5,
    notes = "log10(P_ERA/BIO12) against record length (years). From table_1_site_level_precip_estimates.csv.")
  save_fig(file.path(OUTD, "fig_6c_ratio_vs_frac_measured.png"), p6c, width = 6, height = 5,
    notes = "log10(P_ERA/BIO12) against fraction of years QC-measured. From table_1_site_level_precip_estimates.csv.")
  save_fig(file.path(OUTD, "fig_6d_ratio_vs_hub.png"), p6d, width = 6, height = 5,
    notes = "log10(P_ERA/BIO12) against data_hub (AmeriFlux/ICOS/TERN; manifest field, not inferred from site_id). From table_1_site_level_precip_estimates.csv.")
  save_fig(file.path(OUTD, "fig_6e_ratio_vs_oneflux_version.png"), p6e, width = 7, height = 5,
    notes = "log10(P_ERA/BIO12) against ONEFlux processing version (BIF PRODUCT_ONEFLUX_VERSION, finer-grained than the uniform 'v1.3' manifest field). From table_1_site_level_precip_estimates.csv.")
} else {
  requireNamespace("patchwork")
  p6 <- (p6a | p6b) / (p6c | p6d) / (p6e | patchwork::plot_spacer())
  save_fig(file.path(OUTD, "fig_6_ratio_vs_covariates.png"), p6, width = 11, height = 13,
    notes = "log10(P_ERA/BIO12) against BIO12, record length, fraction measured, data hub, and ONEFlux processing version (BIF PRODUCT_ONEFLUX_VERSION). From table_1_site_level_precip_estimates.csv.")
}

message("\n=== Sections 5-10 (plots 1-6) complete ===")

# ============================================================================
# 11. Baseline: quantify the current KG_ERA5_MAP_MAX_MM = 5000 filter
#    (This is the one place this script deliberately reads MM-resolution
#    ERA5 data -- see the script header: Koppen reclassification necessarily
#    reuses R/climate_classification.R's own MM-resolution climate-normal
#    machinery, unchanged, to test candidate rules' downstream effect.)
# ============================================================================
message("\n================ Section 11: baseline filter quantification ================")

con <- dbConnect(duckdb(), dbdir = duckdb_path, read_only = TRUE)
monthly_era5 <- dbGetQuery(con,
  "SELECT site_id, TIMESTAMP, TA_ERA, P_ERA FROM monthly WHERE dataset = 'ERA5'")
dbDisconnect(con, shutdown = TRUE)
monthly_era5 <- monthly_era5 |> dplyr::semi_join(site_meta, by = "site_id")
message("ERA5 MM rows (current network, for classification only): ", nrow(monthly_era5))

clim_baseline <- compute_era5_monthly_climatology(
  monthly_era5, map_max_mm = KG_ERA5_MAP_MAX_MM,
  excluded_by = "precip_site_filter.R baseline (map_max_mm=5000, matches current pipeline default)")
clim_noscreen <- compute_era5_monthly_climatology(
  monthly_era5, map_max_mm = Inf,
  excluded_by = "precip_site_filter.R baseline (map_max_mm=Inf, no year-level screen)")

years_cmp <- dplyr::full_join(
  dplyr::select(clim_baseline, site_id, n_years_used_5000 = n_years_used),
  dplyr::select(clim_noscreen, site_id, n_years_used_noscreen = n_years_used),
  by = "site_id"
)
n_site_years_removed <- sum(years_cmp$n_years_used_noscreen - years_cmp$n_years_used_5000, na.rm = TRUE)
sites_now_unclassifiable <- years_cmp |>
  dplyr::filter(n_years_used_noscreen >= KG_ERA5_MIN_YEARS, n_years_used_5000 < KG_ERA5_MIN_YEARS)
message(sprintf("Current filter (KG_ERA5_MAP_MAX_MM=%d) removes %d site-years across the network (%d/30-candidate-year basis).",
                KG_ERA5_MAP_MAX_MM, n_site_years_removed, KG_ERA5_PERIOD[2] - KG_ERA5_PERIOD[1] + 1))
message(sprintf("Sites the current filter alone pushes below KG_ERA5_MIN_YEARS (%d): %d",
                KG_ERA5_MIN_YEARS, nrow(sites_now_unclassifiable)))
if (nrow(sites_now_unclassifiable) > 0) print(as.data.frame(sites_now_unclassifiable))

# "How many sites with implausible P_ERA it lets through": sites where the
# 5000mm/yr screen never removed a single year (n_years_used identical with
# and without the screen), yet the site's full-record P_ERA still disagrees
# with BIO12 by more than the BADM/BIO12 reference distribution's own natural
# disagreement (99th percentile of |log10(BADM/BIO12)|) -- the same reference
# threshold used for candidate Rule B below, not a separately invented one.
natural_disagreement_p99 <- stats::quantile(abs(site_table$log_ratio_badm_bio12), 0.99, na.rm = TRUE)
message(sprintf("Reference: 99th percentile of |log10(BADM/BIO12)| across %d sites with BADM = %.3f (factor %.2fx)",
                sum(!is.na(site_table$log_ratio_badm_bio12)), natural_disagreement_p99, 10^natural_disagreement_p99))

unflagged_sites <- years_cmp |>
  dplyr::filter(!is.na(n_years_used_5000), !is.na(n_years_used_noscreen),
                n_years_used_5000 == n_years_used_noscreen) |>
  dplyr::pull(site_id)
implausible_let_through <- site_table |>
  dplyr::filter(site_id %in% unflagged_sites, abs(log_ratio_era_bio12) > natural_disagreement_p99)
message(sprintf("Sites never touched by the current year-level filter, but with |log10(P_ERA/BIO12)| beyond the natural-disagreement reference: %d / %d unflagged sites",
                nrow(implausible_let_through), length(unflagged_sites)))

out_baseline <- file.path(OUTD, "table_3_baseline_filter_quantification.csv")
readr::write_csv(years_cmp |> dplyr::left_join(site_table |> dplyr::select(site_id, log_ratio_era_bio12), by = "site_id"),
                  out_baseline)
write_output_metadata(out_baseline, input_sources = c(duckdb_path, out_site_table),
  notes = paste0(
    "Per site: n_years_used with the current KG_ERA5_MAP_MAX_MM=", KG_ERA5_MAP_MAX_MM,
    " year-level screen vs. with no screen (map_max_mm=Inf), both from R/climate_classification.R's ",
    "compute_era5_monthly_climatology() applied to the current (post-2026-09-20-refresh) DuckDB monthly ",
    "ERA5 table -- the network total difference is the site-years the current filter removes (",
    n_site_years_removed, "). log_ratio_era_bio12 carried over from table_1 for the 'implausible but ",
    "let through' check (", nrow(implausible_let_through), " sites, natural-disagreement threshold ",
    round(natural_disagreement_p99, 3), " from the BADM/BIO12 reference distribution's 99th percentile)."
  ))
message("Saved: ", out_baseline)

# ============================================================================
# 12. Candidate site-inclusion rules
# ============================================================================
message("\n================ Section 12: candidate rules ================")

# Rule A: CV-envelope rule -- exclude a site if its disagreement between
# P_ERA and its own QC-measured precipitation exceeds 3x the expected
# sampling error of a short-record mean (a generic statistical convention,
# not fitted to this dataset's climate ratios). Cannot be evaluated for
# sites with no qualifying measured year -- a real, stated coverage
# limitation, not silently worked around.
rule_a_evaluable <- site_table |> dplyr::filter(!is.na(log_ratio_era_measured), !is.na(p_era_cv), n_years_measured > 0)
rule_a_excluded <- rule_a_evaluable |>
  dplyr::filter(abs(log_ratio_era_measured) > 3 * (p_era_cv / sqrt(n_years_measured))) |>
  dplyr::pull(site_id)
rule_a_not_evaluable <- setdiff(site_table$site_id, rule_a_evaluable$site_id)

# Rule B: BIO12-disagreement rule -- exclude a site if |log10(P_ERA/BIO12)|
# exceeds the 99th percentile of the natural BADM-vs-BIO12 disagreement
# (computed above, reused here rather than re-derived). Available wherever
# BIO12 and P_ERA are both present (site_table: BIO12 100%, P_ERA 100%).
rule_b_excluded <- site_table |>
  dplyr::filter(!is.na(log_ratio_era_bio12), abs(log_ratio_era_bio12) > natural_disagreement_p99) |>
  dplyr::pull(site_id)

# Rule C: union of A and B -- exclude a site if either rule flags it (A where
# evaluable, B everywhere). Presented as a hybrid that trades a larger
# exclusion count for not silently passing sites Rule A cannot evaluate.
rule_c_excluded <- union(rule_a_excluded, rule_b_excluded)

candidate_rules <- list(
  rule_a_cv_envelope   = rule_a_excluded,
  rule_b_bio12_disagreement = rule_b_excluded,
  rule_c_union_a_or_b  = rule_c_excluded
)

message(sprintf("Rule A (CV envelope, 3x): evaluable for %d/%d sites; excludes %d of those.",
                nrow(rule_a_evaluable), n_sites, length(rule_a_excluded)))
message(sprintf("Rule B (BIO12 disagreement, natural-threshold): evaluable for %d/%d sites; excludes %d.",
                sum(!is.na(site_table$log_ratio_era_bio12)), n_sites, length(rule_b_excluded)))
message(sprintf("Rule C (union): excludes %d.", length(rule_c_excluded)))

# ---- Downstream Koppen reclassification effect of each candidate rule ----
message("\n---- Koppen reclassification effect of each candidate rule ----")

current_kg_path <- "data/snapshots/site_koppen_era5.csv"
if (!file.exists(current_kg_path)) stop("Current KG classification not found: ", current_kg_path)
current_kg <- readr::read_csv(current_kg_path, show_col_types = FALSE) |>
  dplyr::select(site_id, kg_class_current = kg_class)
message("Current classification file: ", current_kg_path, " (dated ",
        format(file.info(current_kg_path)$mtime, "%Y-%m-%d %H:%M"), ")")

noscreen_kg <- compute_site_koppen_era5(
  monthly_era5, badm = NULL, beck2023 = NULL, legend = NULL,
  map_max_mm = Inf,
  excluded_by = "precip_site_filter.R candidate-rule reclassification (no year-level MAP screen; site-level inclusion decided separately)"
) |> dplyr::select(site_id, kg_class_noscreen = kg_class)

kg_comparison <- current_kg |> dplyr::left_join(noscreen_kg, by = "site_id")

rule_kg_effect <- purrr::imap_dfr(candidate_rules, function(excluded, rule_name) {
  included <- setdiff(site_meta$site_id, excluded)
  comparable <- kg_comparison |>
    dplyr::filter(site_id %in% included, !is.na(kg_class_current), !is.na(kg_class_noscreen))
  changed <- comparable |> dplyr::filter(kg_class_current != kg_class_noscreen)
  tibble::tibble(
    rule = rule_name,
    n_sites_excluded = length(excluded),
    n_sites_remaining = length(included),
    n_comparable_both_classified = nrow(comparable),
    n_kg_class_changed = nrow(changed)
  )
})
print(as.data.frame(rule_kg_effect))

out_rules <- file.path(OUTD, "table_4_candidate_rules.csv")
readr::write_csv(rule_kg_effect, out_rules)
write_output_metadata(out_rules, input_sources = c(out_site_table, current_kg_path, duckdb_path),
  notes = paste0(
    "Three candidate site-inclusion rules replacing the flat KG_ERA5_MAP_MAX_MM=", KG_ERA5_MAP_MAX_MM,
    " year-level screen with a once-per-site decision (excluded sites contribute no years; included ",
    "sites' full ERA5 record is used with no year-level MAP screen). Rule A: |log10(P_ERA/measured)| > ",
    "3x the CV(P_ERA)/sqrt(n_measured) sampling-error envelope (not evaluable for sites lacking a ",
    "qualifying measured year -- see report.md). Rule B: |log10(P_ERA/BIO12)| beyond the 99th percentile ",
    "of the natural BADM-vs-BIO12 disagreement (", round(natural_disagreement_p99, 3), "). Rule C: union ",
    "of A and B. n_kg_class_changed counts sites classified under both the current (map_max_mm=", KG_ERA5_MAP_MAX_MM,
    ") snapshot (", current_kg_path, ") and the no-year-screen recomputation, among sites the rule keeps, ",
    "whose kg_class differs between the two."
  ))
message("Saved: ", out_rules)

message("\n=== Section 11-12 (baseline + candidate rules) complete ===")

# ============================================================================
# 13. Provenance
# ============================================================================
message("\n================ Section 13: provenance ================")

sha256_of <- function(paths) {
  vapply(paths, function(p) {
    h <- tryCatch(system2("shasum", c("-a", "256", shQuote(p)), stdout = TRUE), error = function(e) NA_character_)
    if (length(h) == 1L) sub("\\s.*$", "", h) else NA_character_
  }, character(1L))
}

provenance <- tibble::tibble(
  path = c(duckdb_path, wc_path, current_kg_path, "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"),
  feeds = c(
    "table_1 (P_ERA/P_F YY series), section 11-12 (MM ERA5 for classification)",
    "table_1 (bio12_mm)",
    "table_4 (kg_class_current)",
    "reference only -- not read by this script; live network membership comes from flux_discover_files() on data/extracted directly"
  )
) |>
  dplyr::mutate(
    exists  = file.exists(path),
    mtime   = as.character(ifelse(exists, format(file.info(path)$mtime, "%Y-%m-%dT%H:%M:%S"), NA)),
    size_bytes = ifelse(exists, file.info(path)$size, NA)
  )

bif_provenance <- tibble::tibble(
  path = site_meta$bif_path,
  feeds = "table_1 (badm_map_mm, elevation_m, oneflux_code_version)"
) |>
  dplyr::mutate(
    exists = file.exists(path),
    mtime  = format(file.info(path)$mtime, "%Y-%m-%dT%H:%M:%S"),
    size_bytes = file.info(path)$size
  )

provenance_full <- dplyr::bind_rows(provenance, bif_provenance)
message("Computing sha256 for ", nrow(provenance), " top-level inputs (not the 781 BIF files -- byte size + mtime only, for volume).")
provenance$sha256 <- sha256_of(provenance$path)

out_prov <- file.path(OUTD, "table_5_provenance.csv")
readr::write_csv(provenance, out_prov)
out_prov_bif <- file.path(OUTD, "table_5b_provenance_bif_files.csv")
readr::write_csv(bif_provenance, out_prov_bif)
write_output_metadata(out_prov, input_sources = "every top-level input this script read",
  notes = "Path, mtime, byte size, sha256, and which output table/section each of this script's top-level inputs feeds.")
write_output_metadata(out_prov_bif, input_sources = "the 781 canonical BIF files identified by flux_discover_files()",
  notes = "Path, mtime, byte size (no sha256 -- 781 files) for every canonical BIF file this script grepped for BADM MAP/LOCATION_ELEV/PRODUCT_ONEFLUX_VERSION.")
message("Saved: ", out_prov, " and ", out_prov_bif)

message("\n=== precip_site_filter.R complete ===")
message("See report.md for the full write-up, what this analysis cannot decide, and the recommendation.")

