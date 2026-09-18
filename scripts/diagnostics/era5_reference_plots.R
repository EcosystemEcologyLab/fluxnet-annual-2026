## era5_reference_plots.R
##
## Plotting task only: no new verdict, no correction, no reclassification.
## Replaces the first version of this script/figures (era5-vs-BIO12-vs-BADM
## only) with a four-estimate version that adds tower-measured MAP. This
## revision overwrites fig1_ratio_histograms_version{A,B}.png (2 panels ->
## 3 panels), overwrites fig2_scatter_version{A,B}.png (3 panels -> 6
## panels), overwrites table_site_reference_comparison.csv and
## table_panel_exclusions.csv, and adds one new figure,
## fig3_month_matched_ratio_version{A,B}.png, that did not exist before.
##
## FOUR ESTIMATES PER SITE (all 781 current-network sites):
##  1. era5_map_mm      -- pipeline's existing sum(P_ERA*days_in_month) over
##                         complete calendar years, 1991-2020 (KG_ERA5_PERIOD).
##                         Unchanged value, reused from table_b1_factor_estimates.csv.
##  2. bio12_mm          -- WorldClim BIO12. Unchanged value, reused as above.
##  3. badm_map_mm       -- BADM PI-reported MAP. Unchanged value, reused as above.
##  4. measured_map_mm   -- NEW. Tower P_F, from the DuckDB monthly FLUXMET
##                         table, restricted to calendar years where all 12
##                         months have P_F_QC >= 0.9 (the "genuinely measured"
##                         cutoff established empirically in
##                         era5_precip_units_v3 Part A; P_F_QC is a fraction
##                         at this resolution, not the HH-resolution integer
##                         flag -- see CLAUDE.md QC Flag Reference). No
##                         calendar-year window restriction is applied (unlike
##                         estimate 1): all of a site's available years are
##                         eligible, since restricting to 1991-2020 would only
##                         shrink an already-thin tower record. Partial years
##                         are never annualised -- a year contributes only if
##                         all 12 of its months qualify.
##
## A fifth per-site value, ratio_to_measured_month_matched, is also computed:
## for every site-month with P_F_QC >= 0.9 (not requiring a complete year),
## the ratio of that month's P_ERA to that month's P_F, aggregated to the
## per-site median. Because it is a ratio of two same-month rates, no
## day-weighting is needed and no annualisation of partial years occurs.
##
## Provenance: era5_map_mm / bio12_mm / badm_map_mm / ratio_to_bio12 /
## ratio_to_badm_map / data_hub / product_source_network / nearest_cluster
## are unchanged values from table_b1_factor_estimates.csv (v3), itself
## unmodified from table_t2_ratios.csv (v2) on every shared column (checked
## before writing the first version of this script). measured_map_mm,
## n_complete_measured_years, n_matched_months, ratio_to_measured, and
## ratio_to_measured_month_matched are computed by this script directly from
## data/duckdb/fluxnet.duckdb (read-only) -- nothing here is present in any
## prior diagnostic output. Spearman rho/n per scatter panel and all
## per-panel exclusion sets are likewise computed here, as before.
##
## Read-only w.r.t. the pipeline and all prior diagnostics: does not modify
## R/climate_classification.R, any numbered pipeline script, any figure,
## legend, or snapshot CSV, or any v1-v4 era5_precip_units output, or
## data/duckdb/fluxnet.duckdb itself (opened read_only = TRUE).

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(fs)
  library(duckdb)
  library(DBI)
  library(lubridate)
})

OUTD <- "review/diagnostics/era5_reference_plots"
V3D  <- "review/diagnostics/era5_precip_units_v3"
fs::dir_create(OUTD)

message("=== era5_reference_plots.R ===")

MEASURED_QC_CUTOFF <- 0.9

# ============================================================================
# 1. LOAD BASE THREE ESTIMATES (no recomputation of any base value or ratio)
# ============================================================================

b1_path <- file.path(V3D, "table_b1_factor_estimates.csv")
b1 <- readr::read_csv(b1_path, show_col_types = FALSE)
stopifnot(nrow(b1) == 781)

base <- b1 |>
  dplyr::transmute(
    site_id, data_hub, product_source_network,
    era5_map_mm = map_era5,
    bio12_mm,
    badm_map_mm,
    ratio_to_bio12,
    ratio_to_badm_map,
    nearest_cluster,
    cluster_membership = dplyr::case_when(
      nearest_cluster == "4" ~ "4x_cluster",
      nearest_cluster == "8" ~ "8x_cluster",
      TRUE ~ "not_in_4x_or_8x_cluster"
    )
  )

# ============================================================================
# 2. NEW: TOWER-MEASURED MAP, FROM DUCKDB MONTHLY (complete years only)
# ============================================================================

message("\n================ Tower-measured MAP from DuckDB (read-only) ================")

con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
pf  <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_F, P_F_QC FROM monthly WHERE dataset = 'FLUXMET'")
era <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_ERA FROM monthly WHERE dataset = 'ERA5'")
dbDisconnect(con, shutdown = TRUE)

pf <- pf |>
  dplyr::filter(site_id %in% base$site_id, !is.na(P_F), !is.na(P_F_QC)) |>
  dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP),
                year = lubridate::year(TIMESTAMP),
                days = lubridate::days_in_month(TIMESTAMP),
                qualifies = P_F_QC >= MEASURED_QC_CUTOFF)
era <- era |> dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP))

message(sprintf("Monthly FLUXMET rows (non-NA P_F & P_F_QC): %d, %d sites | qualifying months (P_F_QC >= %.1f): %d",
                 nrow(pf), length(unique(pf$site_id)), MEASURED_QC_CUTOFF, sum(pf$qualifies)))

## ---- 2a. Complete-year annual totals (all 12 months qualify) -------------
site_year <- pf |>
  dplyr::filter(qualifies) |>
  dplyr::group_by(site_id, year) |>
  dplyr::summarise(n_qual_months = dplyr::n(), annual_mm = sum(P_F * days), .groups = "drop") |>
  dplyr::filter(n_qual_months == 12L)

measured_annual <- site_year |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(measured_map_mm = mean(annual_mm),
                    n_complete_measured_years = dplyr::n(), .groups = "drop")

n_sites_with_complete_year <- nrow(measured_annual)
message(sprintf("Complete measured site-years (all 12 months, P_F_QC>=%.1f): %d, across %d sites",
                 MEASURED_QC_CUTOFF, nrow(site_year), n_sites_with_complete_year))
message("Distribution of n_complete_measured_years across those sites:")
print(summary(measured_annual$n_complete_measured_years))

## ---- 2b. Month-matched ratio (every qualifying month, no annualisation) --
month_matched <- pf |>
  dplyr::filter(qualifies) |>
  dplyr::inner_join(era, by = c("site_id", "TIMESTAMP"))

n_qual_joined <- nrow(month_matched)
n_zero_pf     <- sum(month_matched$P_F == 0)
message(sprintf("Qualifying months joined to ERA5: %d | excluded (P_F == 0, undefined ratio): %d",
                 n_qual_joined, n_zero_pf))

measured_monthly <- month_matched |>
  dplyr::filter(P_F != 0) |>
  dplyr::mutate(month_ratio = P_ERA / P_F) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(ratio_to_measured_month_matched = median(month_ratio),
                    n_matched_months = dplyr::n(), .groups = "drop")

message(sprintf("Sites with >=1 valid matched month: %d", nrow(measured_monthly)))
message("Distribution of n_matched_months across those sites:")
print(summary(measured_monthly$n_matched_months))

# ============================================================================
# 3. ASSEMBLE COMPANION TABLE
# ============================================================================

d <- base |>
  dplyr::left_join(measured_annual, by = "site_id") |>
  dplyr::left_join(measured_monthly, by = "site_id") |>
  dplyr::mutate(
    n_complete_measured_years = dplyr::coalesce(n_complete_measured_years, 0L),
    n_matched_months          = dplyr::coalesce(n_matched_months, 0L),
    ratio_to_measured         = era5_map_mm / measured_map_mm,
    era5_positive  = !is.na(era5_map_mm) & era5_map_mm > 0,
    bio12_positive = !is.na(bio12_mm) & bio12_mm > 0,
    badm_present   = !is.na(badm_map_mm),
    badm_positive  = badm_present & badm_map_mm > 0,
    meas_present   = !is.na(measured_map_mm),
    meas_positive  = meas_present & measured_map_mm > 0
  )

n_total          <- nrow(d)
n_missing_bio12   <- sum(!d$bio12_positive & is.na(d$bio12_mm))
n_missing_badm    <- sum(!d$badm_present)
n_missing_measured <- sum(!d$meas_present)
n_missing_none    <- sum(is.na(d$bio12_mm) & is.na(d$badm_map_mm) & is.na(d$measured_map_mm))
n_cluster         <- sum(d$cluster_membership != "not_in_4x_or_8x_cluster")

message(sprintf(
  "\nSites: %d total | missing BIO12: %d | missing BADM: %d | missing measured (no complete year): %d | missing all three references: %d | in 4x/8x cluster: %d",
  n_total, n_missing_bio12, n_missing_badm, n_missing_measured, n_missing_none, n_cluster
))

# ============================================================================
# 4. PER-PANEL EXCLUSION SETS
#    A log axis is undefined at zero/negative/missing values. era5_map_mm is
#    exactly 0 at one site (CA-TP2); badm_map_mm is exactly 0 at five sites
#    (CZ-LnG, DE-Lnf, ES-Agu, ES-Amo, KE-Kpt) in addition to being missing at
#    144 sites; measured_map_mm is missing (no complete measured year) at
#    312 sites and was never found to be exactly 0 in this network. bio12_mm
#    has no zero, negative, or missing values in this network.
# ============================================================================

excl_hist_bio12 <- d |> dplyr::filter(!era5_positive) |> dplyr::pull(site_id)
excl_hist_badm  <- d |> dplyr::filter(!era5_positive | !badm_present | !badm_positive) |> dplyr::pull(site_id)
excl_hist_meas  <- d |> dplyr::filter(!era5_positive | !meas_present | !meas_positive) |> dplyr::pull(site_id)

excl_scatter_era5_badm  <- excl_hist_badm
excl_scatter_era5_bio12 <- excl_hist_bio12
excl_scatter_era5_meas  <- excl_hist_meas
excl_scatter_badm_bio12 <- d |> dplyr::filter(!badm_present | !badm_positive) |> dplyr::pull(site_id)
excl_scatter_badm_meas  <- d |> dplyr::filter(!badm_present | !badm_positive | !meas_present | !meas_positive) |> dplyr::pull(site_id)
excl_scatter_bio12_meas <- d |> dplyr::filter(!meas_present | !meas_positive) |> dplyr::pull(site_id)

excl_month_matched <- d |> dplyr::filter(n_matched_months == 0) |> dplyr::pull(site_id)

reasons <- dplyr::bind_rows(
  d |> dplyr::filter(!era5_positive) |>
    dplyr::transmute(site_id, panel = "hist:ERA5/BIO12, hist:ERA5/measured, scatter:ERA5-vs-BIO12, scatter:ERA5-vs-BADM, scatter:ERA5-vs-measured",
                      reason = "era5_map_mm == 0"),
  d |> dplyr::filter(!badm_present) |>
    dplyr::transmute(site_id, panel = "hist:ERA5/BADM, scatter:ERA5-vs-BADM, scatter:BADM-vs-BIO12, scatter:BADM-vs-measured",
                      reason = "badm_map_mm missing (NA)"),
  d |> dplyr::filter(badm_present & !badm_positive) |>
    dplyr::transmute(site_id, panel = "hist:ERA5/BADM, scatter:ERA5-vs-BADM, scatter:BADM-vs-BIO12, scatter:BADM-vs-measured",
                      reason = "badm_map_mm == 0"),
  d |> dplyr::filter(!meas_present) |>
    dplyr::transmute(site_id, panel = "hist:ERA5/measured, scatter:ERA5-vs-measured, scatter:BADM-vs-measured, scatter:BIO12-vs-measured",
                      reason = "measured_map_mm missing (no calendar year with all 12 months at P_F_QC >= 0.9)"),
  d |> dplyr::filter(n_matched_months == 0) |>
    dplyr::transmute(site_id, panel = "fig3_month_matched_ratio",
                      reason = "no site-month with P_F_QC >= 0.9 and P_F != 0")
)

message("\n-- Panel n --")
message(sprintf("hist ERA5/BIO12: n=%d | hist ERA5/BADM: n=%d | hist ERA5/measured: n=%d",
                 n_total - length(excl_hist_bio12), n_total - length(excl_hist_badm), n_total - length(excl_hist_meas)))
message(sprintf("scatter ERA5-BADM: n=%d | ERA5-BIO12: n=%d | ERA5-measured: n=%d | BADM-BIO12: n=%d | BADM-measured: n=%d | BIO12-measured: n=%d",
                 n_total - length(excl_scatter_era5_badm), n_total - length(excl_scatter_era5_bio12), n_total - length(excl_scatter_era5_meas),
                 n_total - length(excl_scatter_badm_bio12), n_total - length(excl_scatter_badm_meas), n_total - length(excl_scatter_bio12_meas)))
message(sprintf("fig3_month_matched_ratio: n=%d", n_total - length(excl_month_matched)))

# ============================================================================
# 5. SPEARMAN RHO PER SCATTER PANEL
# ============================================================================

spearman <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman"))
  list(rho = unname(ct$estimate), p = ct$p.value, n = sum(ok))
}
fmt_p <- function(p) if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)

dat_era5_badm  <- d |> dplyr::filter(!(site_id %in% excl_scatter_era5_badm))
dat_era5_bio12 <- d |> dplyr::filter(!(site_id %in% excl_scatter_era5_bio12))
dat_era5_meas  <- d |> dplyr::filter(!(site_id %in% excl_scatter_era5_meas))
dat_badm_bio12 <- d |> dplyr::filter(!(site_id %in% excl_scatter_badm_bio12))
dat_badm_meas  <- d |> dplyr::filter(!(site_id %in% excl_scatter_badm_meas))
dat_bio12_meas <- d |> dplyr::filter(!(site_id %in% excl_scatter_bio12_meas))

rho_era5_badm  <- spearman(dat_era5_badm$badm_map_mm,  dat_era5_badm$era5_map_mm)
rho_era5_bio12 <- spearman(dat_era5_bio12$bio12_mm,    dat_era5_bio12$era5_map_mm)
rho_era5_meas  <- spearman(dat_era5_meas$measured_map_mm, dat_era5_meas$era5_map_mm)
rho_badm_bio12 <- spearman(dat_badm_bio12$bio12_mm,    dat_badm_bio12$badm_map_mm)
rho_badm_meas  <- spearman(dat_badm_meas$measured_map_mm, dat_badm_meas$badm_map_mm)
rho_bio12_meas <- spearman(dat_bio12_meas$measured_map_mm, dat_bio12_meas$bio12_mm)

for (nm in c("era5_badm", "era5_bio12", "era5_meas", "badm_bio12", "badm_meas", "bio12_meas")) {
  r <- get(paste0("rho_", nm))
  message(sprintf("Spearman %s: n=%d rho=%.3f p%s", nm, r$n, r$rho, fmt_p(r$p)))
}

# ============================================================================
# 6. SHARED STYLE
# ============================================================================

COL_ALL   <- "#4C72B0"
COL_OTHER <- "#4C72B0"
COL_CLUST <- "#D55E00"
CLUSTER_LEGEND_TITLE <-
  paste0(
    "Membership rule (verbatim, table_b1_factor_estimates.csv):\n",
    "factor_estimate = median(ratio_to_bio12, ratio_to_badm_map);\n",
    "assigned to nearest of candidate factors {1, 4, 8, 24, 1000}\n",
    "if within 15% of it (tol = 0.15), else 'elsewhere'.\n",
    "Shown = sites with nearest_cluster %in% c('4','8')\n",
    sprintf("(n = %d: 113 near 4x + 10 near 8x)", n_cluster)
  )

theme_diag <- function() {
  theme_minimal(base_size = 9) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          legend.key.size = unit(0.3, "cm"),
          legend.text = element_text(size = 6.5),
          legend.title = element_text(size = 6.5))
}

add_cluster_col <- function(df) {
  df |> dplyr::mutate(cluster = factor(cluster_membership != "not_in_4x_or_8x_cluster",
                                        levels = c(FALSE, TRUE),
                                        labels = c("not in 4x/8x cluster", "in 4x/8x cluster")))
}

# ============================================================================
# 7. FIGURE 1 (OVERWRITTEN: 2 panels -> 3 panels) -- HISTOGRAMS OF log10(ratio)
# ============================================================================

BINWIDTH <- 0.1  # log10 units; 10^0.1 ~= 1.259, i.e. ~26% per bin
REF_LINES <- log10(c(1, 4, 8))
HIST_BRKS <- log10(c(0.001, 0.01, 0.1, 1, 4, 8, 100, 1000))
HIST_LBLS <- c("0.001x", "0.01x", "0.1x", "1x", "4x", "8x", "100x", "1000x")

make_hist <- function(df_sub, ratio_col, panel_label, n_panel, highlight) {
  df_sub <- df_sub |> dplyr::mutate(log10_ratio = log10(.data[[ratio_col]]))
  subtitle <- sprintf("n = %d | bin width = %.2g log10 units (ratio x %.3f per bin)", n_panel, BINWIDTH, 10^BINWIDTH)
  if (!highlight) {
    p <- ggplot(df_sub, aes(x = log10_ratio)) +
      geom_histogram(binwidth = BINWIDTH, boundary = 0, fill = COL_ALL, colour = "white", linewidth = 0.15)
  } else {
    df_sub <- add_cluster_col(df_sub)
    p <- ggplot(df_sub, aes(x = log10_ratio, fill = cluster)) +
      geom_histogram(binwidth = BINWIDTH, boundary = 0, colour = "white", linewidth = 0.15, position = "stack") +
      scale_fill_manual(values = c("not in 4x/8x cluster" = COL_OTHER, "in 4x/8x cluster" = COL_CLUST),
                         name = CLUSTER_LEGEND_TITLE)
  }
  p +
    geom_vline(xintercept = REF_LINES, linetype = "dashed", colour = "grey30", linewidth = 0.35) +
    scale_x_continuous(name = paste0(panel_label, "  [log10(ratio) scale]"), breaks = HIST_BRKS, labels = HIST_LBLS) +
    labs(y = "number of sites", subtitle = subtitle) +
    theme_diag()
}

build_fig1 <- function(highlight) {
  p1 <- make_hist(d |> dplyr::filter(!(site_id %in% excl_hist_bio12)),
                   "ratio_to_bio12", "ERA5 MAP / WorldClim BIO12", n_total - length(excl_hist_bio12), highlight)
  p2 <- make_hist(d |> dplyr::filter(!(site_id %in% excl_hist_badm)),
                   "ratio_to_badm_map", "ERA5 MAP / BADM PI-reported MAP", n_total - length(excl_hist_badm), highlight)
  p3 <- make_hist(d |> dplyr::filter(!(site_id %in% excl_hist_meas)),
                   "ratio_to_measured", "ERA5 MAP / tower-measured MAP", n_total - length(excl_hist_meas), highlight)
  cap <- paste0(
    "Figure 1. Histograms of the per-site ratio of ERA5-derived mean annual precipitation (MAP) to three ",
    "independent references, ", n_total, " current-network sites. Panel 1 (ERA5/BIO12): n=",
    n_total - length(excl_hist_bio12), "; excludes 1 site with era5_map_mm == 0 (CA-TP2). ",
    "Panel 2 (ERA5/BADM): n=", n_total - length(excl_hist_badm), "; excludes ", length(excl_hist_badm),
    " sites (144 no BADM value, 5 BADM MAP == 0, 1 era5_map_mm == 0). ",
    "Panel 3 (ERA5/tower-measured, NEW): n=", n_total - length(excl_hist_meas), "; excludes ", length(excl_hist_meas),
    " sites (312 with no calendar year where all 12 months have P_F_QC >= 0.9, 1 era5_map_mm == 0; measured_map_mm ",
    "was never exactly 0 in this network). X-axis is log10(ratio); bin width 0.1 log10 units (~26% per bin); ",
    "range not clipped, extends to the full observed range in each panel. Dashed reference lines at ratio = 1x, 4x, 8x.",
    if (highlight) paste0(" Version B: the ", n_cluster, " sites in the 4x/8x cluster are shown in a second ",
                           "colour; see legend for the membership rule.") else " Version A: all sites, one colour, no grouping."
  )
  fig <- gridExtra::arrangeGrob(p1, p2, p3, ncol = 3)
  list(fig = fig, caption = cap)
}

fig1A <- build_fig1(highlight = FALSE)
fig1B <- build_fig1(highlight = TRUE)

out_fig1A <- file.path(OUTD, "fig1_ratio_histograms_versionA.png")
out_fig1B <- file.path(OUTD, "fig1_ratio_histograms_versionB.png")
ggsave(out_fig1A, fig1A$fig, width = 16, height = 5, dpi = 300, bg = "white")
ggsave(out_fig1B, fig1B$fig, width = 16, height = 5, dpi = 300, bg = "white")
message("Saved (overwritten): ", out_fig1A)
message("Saved (overwritten): ", out_fig1B)

# ============================================================================
# 8. FIGURE 3 (NEW) -- MONTH-MATCHED RATIO HISTOGRAM (single panel)
# ============================================================================

build_fig_mm <- function(highlight) {
  df_sub <- d |> dplyr::filter(!(site_id %in% excl_month_matched)) |>
    dplyr::mutate(log10_ratio = log10(ratio_to_measured_month_matched))
  n_panel <- nrow(df_sub)
  med_months <- median(df_sub$n_matched_months)
  subtitle <- sprintf("n sites = %d | median matched months per site = %g | bin width = %.2g log10 units (ratio x %.3f per bin)",
                       n_panel, med_months, BINWIDTH, 10^BINWIDTH)
  if (!highlight) {
    p <- ggplot(df_sub, aes(x = log10_ratio)) +
      geom_histogram(binwidth = BINWIDTH, boundary = 0, fill = COL_ALL, colour = "white", linewidth = 0.15)
  } else {
    df_sub <- add_cluster_col(df_sub)
    p <- ggplot(df_sub, aes(x = log10_ratio, fill = cluster)) +
      geom_histogram(binwidth = BINWIDTH, boundary = 0, colour = "white", linewidth = 0.15, position = "stack") +
      scale_fill_manual(values = c("not in 4x/8x cluster" = COL_OTHER, "in 4x/8x cluster" = COL_CLUST),
                         name = CLUSTER_LEGEND_TITLE)
  }
  p <- p +
    geom_vline(xintercept = REF_LINES, linetype = "dashed", colour = "grey30", linewidth = 0.35) +
    scale_x_continuous(name = "per-site median of (month's ERA5 P) / (month's measured P), qualifying months only  [log10(ratio) scale]",
                        breaks = HIST_BRKS, labels = HIST_LBLS) +
    labs(y = "number of sites", subtitle = subtitle) +
    theme_diag()
  cap <- paste0(
    "Figure 3 (new). Per-site median of the month-matched ratio (ERA5 P_ERA / tower P_F) across every site-month ",
    "with P_F_QC >= ", MEASURED_QC_CUTOFF, ", excluding site-months where P_F == 0 (undefined ratio; ", n_zero_pf,
    " such site-months excluded network-wide, at sites otherwise retained via their remaining qualifying months). ",
    "No annualisation: uses individual matched months directly, not complete years. n sites = ", n_panel,
    "; excludes ", length(excl_month_matched), " sites with zero valid matched months. Median matched months per ",
    "site among those plotted = ", med_months, ". Bin width 0.1 log10 units; range not clipped. Reference lines at ",
    "ratio = 1x, 4x, 8x shown for comparability with Figure 1.",
    if (highlight) paste0(" Version B: the ", n_cluster, " sites in the 4x/8x cluster are shown in a second ",
                           "colour; see legend for the membership rule.") else " Version A: all sites, one colour, no grouping."
  )
  list(fig = p, caption = cap, n_panel = n_panel, med_months = med_months)
}

figMM_A <- build_fig_mm(highlight = FALSE)
figMM_B <- build_fig_mm(highlight = TRUE)

out_figMM_A <- file.path(OUTD, "fig3_month_matched_ratio_versionA.png")
out_figMM_B <- file.path(OUTD, "fig3_month_matched_ratio_versionB.png")
ggsave(out_figMM_A, figMM_A$fig, width = 7, height = 5, dpi = 300, bg = "white")
ggsave(out_figMM_B, figMM_B$fig, width = 8.5, height = 5, dpi = 300, bg = "white")
message("Saved (new): ", out_figMM_A)
message("Saved (new): ", out_figMM_B)

# ============================================================================
# 9. FIGURE 2 (OVERWRITTEN: 3 panels -> 6 panels) -- SCATTER PLOTS
#    y = the first-named variable, x = the second-named variable, matching
#    Figure 1's ratio direction (ERA5/reference) where ERA5 is involved.
# ============================================================================

make_scatter <- function(df_sub, xcol, ycol, xlab, ylab, rho_info, highlight, panel_letter) {
  rng <- range(c(df_sub[[xcol]], df_sub[[ycol]]), na.rm = TRUE)
  ann <- sprintf("(%s) n = %d\nSpearman rho = %.3f\np %s", panel_letter, rho_info$n, rho_info$rho, fmt_p(rho_info$p))
  if (!highlight) {
    p <- ggplot(df_sub, aes(x = .data[[xcol]], y = .data[[ycol]])) +
      geom_point(alpha = 0.55, size = 1.2, colour = COL_ALL)
  } else {
    df_sub <- add_cluster_col(df_sub)
    p <- ggplot(df_sub, aes(x = .data[[xcol]], y = .data[[ycol]], colour = cluster)) +
      geom_point(alpha = 0.6, size = 1.2) +
      scale_colour_manual(values = c("not in 4x/8x cluster" = COL_OTHER, "in 4x/8x cluster" = COL_CLUST),
                           name = CLUSTER_LEGEND_TITLE)
  }
  p +
    geom_abline(slope = 1, intercept = 0, linetype = "solid", colour = "grey35", linewidth = 0.4) +
    geom_abline(slope = 1, intercept = log10(4),  linetype = "dashed", colour = "grey50", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = -log10(4), linetype = "dashed", colour = "grey50", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = log10(8),  linetype = "dotted", colour = "grey50", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = -log10(8), linetype = "dotted", colour = "grey50", linewidth = 0.3) +
    scale_x_log10(limits = rng, name = xlab) +
    scale_y_log10(limits = rng, name = ylab) +
    annotate("text", x = rng[1] * 1.3, y = rng[2] / 1.15, label = ann, hjust = 0, vjust = 1, size = 2.3, lineheight = 1.05) +
    theme_diag()
}

build_fig2 <- function(highlight) {
  rng <- function(df, c1, c2) round(range(c(df[[c1]], df[[c2]]), na.rm = TRUE), 1)
  rng_eb <- rng(dat_era5_badm,  "badm_map_mm",     "era5_map_mm")
  rng_ei <- rng(dat_era5_bio12, "bio12_mm",        "era5_map_mm")
  rng_em <- rng(dat_era5_meas,  "measured_map_mm", "era5_map_mm")
  rng_bi <- rng(dat_badm_bio12, "bio12_mm",        "badm_map_mm")
  rng_bm <- rng(dat_badm_meas,  "measured_map_mm", "badm_map_mm")
  rng_im <- rng(dat_bio12_meas, "measured_map_mm", "bio12_mm")

  p1 <- make_scatter(dat_era5_badm,  "badm_map_mm",     "era5_map_mm",     "BADM MAP (mm/yr)",     "ERA5 MAP (mm/yr)",     rho_era5_badm,  highlight, "a")
  p2 <- make_scatter(dat_era5_bio12, "bio12_mm",        "era5_map_mm",     "WorldClim BIO12 (mm/yr)", "ERA5 MAP (mm/yr)", rho_era5_bio12, highlight, "b")
  p3 <- make_scatter(dat_era5_meas,  "measured_map_mm", "era5_map_mm",     "Tower-measured MAP (mm/yr)", "ERA5 MAP (mm/yr)", rho_era5_meas, highlight, "c")
  p4 <- make_scatter(dat_badm_bio12, "bio12_mm",        "badm_map_mm",     "WorldClim BIO12 (mm/yr)", "BADM MAP (mm/yr)", rho_badm_bio12, highlight, "d")
  p5 <- make_scatter(dat_badm_meas,  "measured_map_mm", "badm_map_mm",     "Tower-measured MAP (mm/yr)", "BADM MAP (mm/yr)", rho_badm_meas, highlight, "e")
  p6 <- make_scatter(dat_bio12_meas, "measured_map_mm", "bio12_mm",        "Tower-measured MAP (mm/yr)", "WorldClim BIO12 (mm/yr)", rho_bio12_meas, highlight, "f")

  cap <- paste0(
    "Figure 2. Pairwise comparison of the four MAP estimates (mm/yr), log-log axes, identical x/y range within ",
    "each panel (not clipped). Solid line = 1:1; dashed = 4x/0.25x offset; dotted = 8x/0.125x offset. ",
    "(a) ERA5 vs BADM: n=", rho_era5_badm$n, ", rho=", sprintf("%.3f", rho_era5_badm$rho), ", range [", rng_eb[1], ", ", rng_eb[2], "]. ",
    "(b) ERA5 vs BIO12: n=", rho_era5_bio12$n, ", rho=", sprintf("%.3f", rho_era5_bio12$rho), ", range [", rng_ei[1], ", ", rng_ei[2], "]. ",
    "(c) ERA5 vs tower-measured (new): n=", rho_era5_meas$n, ", rho=", sprintf("%.3f", rho_era5_meas$rho), ", range [", rng_em[1], ", ", rng_em[2], "]. ",
    "(d) BADM vs BIO12: n=", rho_badm_bio12$n, ", rho=", sprintf("%.3f", rho_badm_bio12$rho), ", range [", rng_bi[1], ", ", rng_bi[2], "]. ",
    "(e) BADM vs tower-measured (new): n=", rho_badm_meas$n, ", rho=", sprintf("%.3f", rho_badm_meas$rho), ", range [", rng_bm[1], ", ", rng_bm[2], "]. ",
    "(f) BIO12 vs tower-measured (new): n=", rho_bio12_meas$n, ", rho=", sprintf("%.3f", rho_bio12_meas$rho), ", range [", rng_im[1], ", ", rng_im[2], "]. ",
    "Exclusions per panel listed in report.md / table_panel_exclusions.csv.",
    if (highlight) paste0(" Version B: the ", n_cluster, " sites in the 4x/8x cluster are shown in a second ",
                           "colour; see legend for the membership rule.") else " Version A: all sites, one colour, no grouping."
  )
  fig <- gridExtra::arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)
  list(fig = fig, caption = cap,
       rng_eb = rng_eb, rng_ei = rng_ei, rng_em = rng_em, rng_bi = rng_bi, rng_bm = rng_bm, rng_im = rng_im)
}

fig2A <- build_fig2(highlight = FALSE)
fig2B <- build_fig2(highlight = TRUE)

out_fig2A <- file.path(OUTD, "fig2_scatter_versionA.png")
out_fig2B <- file.path(OUTD, "fig2_scatter_versionB.png")
ggsave(out_fig2A, fig2A$fig, width = 15, height = 10.5, dpi = 300, bg = "white")
ggsave(out_fig2B, fig2B$fig, width = 15, height = 10.5, dpi = 300, bg = "white")
message("Saved (overwritten): ", out_fig2A)
message("Saved (overwritten): ", out_fig2B)

# ============================================================================
# 10. COMPANION CSV (OVERWRITTEN)
# ============================================================================

companion <- d |>
  dplyr::select(site_id, data_hub, product_source_network,
                 era5_map_mm, bio12_mm, badm_map_mm, measured_map_mm,
                 n_complete_measured_years, n_matched_months,
                 ratio_to_bio12, ratio_to_badm_map, ratio_to_measured, ratio_to_measured_month_matched,
                 cluster_membership)

out_csv <- file.path(OUTD, "table_site_reference_comparison.csv")
readr::write_csv(companion, out_csv)
write_output_metadata(
  out_csv,
  input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = paste0(
    "One row per current-network site (n=781), replaces the 3-reference version of this file. era5_map_mm/",
    "bio12_mm/badm_map_mm/ratio_to_bio12/ratio_to_badm_map/cluster_membership are unchanged from the prior ",
    "version (ultimately from table_b1_factor_estimates.csv, v3). measured_map_mm/n_complete_measured_years/",
    "ratio_to_measured are NEW: mean of sum(P_F*days_in_month) over calendar years where all 12 months have ",
    "P_F_QC >= 0.9 (DuckDB monthly FLUXMET table), no annualisation of partial years, no calendar-year-window ",
    "restriction. n_matched_months/ratio_to_measured_month_matched are NEW: per-site median of P_ERA/P_F over ",
    "every site-month with P_F_QC >= 0.9 and P_F != 0 (no annualisation, no complete-year requirement). Missing ",
    "BADM (144 sites) and no-complete-measured-year (312 sites) are carried as NA, not dropped. Plotting-only ",
    "diagnostic; no new verdict, correction, or reclassification of any site."
  )
)
message("Saved (overwritten): ", out_csv)

# ============================================================================
# 11. METADATA FOR FIGURES
# ============================================================================

write_output_metadata(out_fig1A, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = paste0("Version A (no grouping) of Figure 1, overwritten to add a 3rd panel (ERA5/tower-measured). ", fig1A$caption))
write_output_metadata(out_fig1B, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = paste0("Version B (4x/8x cluster highlighted) of Figure 1, overwritten to add a 3rd panel. ", fig1B$caption))
write_output_metadata(out_fig2A, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = paste0("Version A (no grouping) of Figure 2, overwritten to go from 3 to 6 panels. ", fig2A$caption))
write_output_metadata(out_fig2B, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = paste0("Version B (4x/8x cluster highlighted) of Figure 2, overwritten to go from 3 to 6 panels. ", fig2B$caption))
write_output_metadata(out_figMM_A, input_sources = "data/duckdb/fluxnet.duckdb (monthly table, read-only)",
  notes = paste0("Version A (no grouping) of Figure 3 -- NEW figure, did not exist in the prior version. ", figMM_A$caption))
write_output_metadata(out_figMM_B, input_sources = "data/duckdb/fluxnet.duckdb (monthly table, read-only)",
  notes = paste0("Version B (4x/8x cluster highlighted) of Figure 3 -- NEW figure, did not exist in the prior version. ", figMM_B$caption))

# ============================================================================
# 12. EXCLUSION SUMMARY (OVERWRITTEN)
# ============================================================================

out_excl <- file.path(OUTD, "table_panel_exclusions.csv")
readr::write_csv(reasons, out_excl)
write_output_metadata(out_excl, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = "Sites excluded from one or more panels of Figure 1, Figure 2, or Figure 3 because a value used on a log axis is exactly zero or missing, or (Figure 3 only) because no valid matched month exists, and the specific reason. Overwrites the prior (3-reference) version of this file. Not an exclusion from the companion CSV, which carries all 781 sites.")
message("Saved (overwritten): ", out_excl)

message("\n=== era5_reference_plots.R complete ===")
