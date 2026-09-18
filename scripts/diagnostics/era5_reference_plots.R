## era5_reference_plots.R
##
## Plotting task only: no new verdict, no correction, no reclassification.
## This version is a LEGIBILITY REFORMAT of the four-estimate figures --
## same data, same values, same membership rule as the prior version.
## Nothing in sections 1-5 or 10 below (data assembly, exclusion sets,
## Spearman rho, companion CSV) changed from the prior version. What
## changed is purely presentation:
##  - The membership-rule text is no longer drawn inside every panel (it
##    consumed roughly half the panel width). It now lives in a companion
##    <basename>.legend.txt file per version-B figure, with only a single
##    short footnote line beneath the whole figure and one shared colour
##    legend (collected via patchwork, not duplicated per panel).
##  - Figure 1 and Figure 3's histograms use a fixed core x-range (0.1x to
##    20x ratio) with a single marked (black-outlined) overflow bin at each
##    end for points outside it, instead of letting a handful of extreme
##    ratios stretch the axis across 6 decades.
##  - Figure 2's six scatter panels share one common x/y range (10 to
##    10,000 mm/yr, chosen from the bulk of the pooled data) instead of a
##    different per-panel range; points outside it are shown clamped to the
##    edge as a distinct (triangle) shape rather than silently dropped.
##  - Scatter points are smaller and more transparent; in version B the
##    4x/8x-cluster points are drawn on top of the rest.
##  - Larger base font sizes and wider margins for legibility at ~1000px
##    display width; the 4x/8x histogram reference-tick labels no longer
##    collide.
##
## FOUR ESTIMATES PER SITE (all 781 current-network sites) -- unchanged
## from the prior version:
##  1. era5_map_mm      -- pipeline's existing sum(P_ERA*days_in_month) over
##                         complete calendar years, 1991-2020 (KG_ERA5_PERIOD).
##                         Unchanged value, reused from table_b1_factor_estimates.csv.
##  2. bio12_mm          -- WorldClim BIO12. Unchanged value, reused as above.
##  3. badm_map_mm       -- BADM PI-reported MAP. Unchanged value, reused as above.
##  4. measured_map_mm   -- Tower P_F, from the DuckDB monthly FLUXMET table,
##                         restricted to calendar years where all 12 months
##                         have P_F_QC >= 0.9. No calendar-year window
##                         restriction; no partial year annualised.
## A fifth per-site value, ratio_to_measured_month_matched, is the per-site
## median of the month-matched ERA5/measured ratio (see prior version's
## header for full detail, unchanged here).
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
  library(patchwork)
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
# 2. TOWER-MEASURED MAP, FROM DUCKDB MONTHLY (complete years only)
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
# 4. PER-PANEL EXCLUSION SETS (unchanged from prior version)
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
# 5. SPEARMAN RHO PER SCATTER PANEL (unchanged from prior version)
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
# 6. SHARED STYLE (rewritten for legibility)
# ============================================================================

COL_ALL   <- "#4C72B0"
COL_OTHER <- "#4C72B0"
COL_CLUST <- "#D55E00"

MEMBERSHIP_RULE_TEXT <- paste0(
  "Membership rule (verbatim, table_b1_factor_estimates.csv):\n",
  "factor_estimate = median(ratio_to_bio12, ratio_to_badm_map);\n",
  "assigned to nearest of candidate factors {1, 4, 8, 24, 1000}\n",
  "if within 15% of it (tol = 0.15), else 'elsewhere'.\n",
  "Shown = sites with nearest_cluster %in% c('4','8')\n",
  sprintf("(n = %d: 113 near 4x + 10 near 8x)", n_cluster)
)
FOOTNOTE <- function(basename) {
  sprintf("4x/8x cluster: factor_estimate within 15%% of 4 or 8 (n=%d). Full rule: %s.legend.txt",
          n_cluster, basename)
}
write_legend_txt <- function(basename) {
  writeLines(MEMBERSHIP_RULE_TEXT, file.path(OUTD, paste0(basename, ".legend.txt")))
}
TITLE_TXT <- "Bins: 0.1 log10 units | dashed lines = 1x, 4x, 8x | black outline = overflow bin (see caption)"

theme_diag <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(plot.background  = element_rect(fill = "white", colour = NA),
          plot.margin      = margin(t = 8, r = 14, b = 6, l = 6),
          plot.subtitle    = element_text(size = base_size * 0.7, colour = "grey30"),
          axis.title       = element_text(size = base_size * 0.95),
          legend.key.size  = unit(0.45, "cm"),
          legend.text      = element_text(size = base_size * 0.8),
          legend.title     = element_text(size = base_size * 0.8))
}

add_cluster_col <- function(df) {
  df |> dplyr::mutate(cluster = factor(cluster_membership != "not_in_4x_or_8x_cluster",
                                        levels = c(FALSE, TRUE),
                                        labels = c("not in 4x/8x cluster", "in 4x/8x cluster")))
}

## A single shared ggplot theme applied to every combined figure via `&`,
## so the collected legend (patchwork guides = "collect") renders once,
## below all panels, at a legible size.
SHARED_LEGEND_THEME <- theme(legend.position = "bottom",
                              legend.title = element_blank(),
                              legend.text = element_text(size = 11))

# ============================================================================
# 7. FIGURE 1 (reformatted) -- HISTOGRAMS OF log10(ratio), fixed core range
#    with one marked overflow bin at each end.
# ============================================================================

BINWIDTH <- 0.1        # log10 units; 10^0.1 ~= 1.259, i.e. ~26% per bin
REF_LINES <- log10(c(1, 4, 8))
BIN_LO <- -1           # log10(0.1) -- core range lower edge
BIN_HI <- 1.3          # log10(20)  -- core range upper edge (10^1.3 = 19.95, "20x")
UNDER_POS <- BIN_LO - BINWIDTH / 2
OVER_POS  <- BIN_HI + BINWIDTH / 2
HIST_BRKS <- c(UNDER_POS, log10(0.2), 0, log10(4), log10(8), OVER_POS)
HIST_LBLS <- c("<0.1x", "0.2x", "1x", "4x", "8x", ">20x")

clamp_ratio <- function(x) dplyr::case_when(x < BIN_LO ~ UNDER_POS, x > BIN_HI ~ OVER_POS, TRUE ~ x)

## Returns the plot, plus the under/over site lists (for the caption/report).
make_hist <- function(df_sub, ratio_col, panel_label, highlight, base_size = 13) {
  df_sub <- df_sub |>
    dplyr::mutate(log10_ratio_raw = log10(.data[[ratio_col]]),
                  log10_ratio = clamp_ratio(log10_ratio_raw))
  n_panel <- nrow(df_sub)
  under_sites <- df_sub$site_id[df_sub$log10_ratio_raw < BIN_LO]
  over_sites  <- df_sub$site_id[df_sub$log10_ratio_raw > BIN_HI]
  n_flag_lab <- data.frame(x = c(UNDER_POS, OVER_POS), y = c(length(under_sites), length(over_sites))) |>
    dplyr::filter(y > 0)

  subtitle <- sprintf("n=%d  |  %d<0.1x, %d>20x", n_panel, length(under_sites), length(over_sites))

  if (!highlight) {
    p <- ggplot(df_sub, aes(x = log10_ratio)) +
      geom_histogram(binwidth = BINWIDTH, boundary = BIN_LO, fill = COL_ALL, colour = "white", linewidth = 0.15, na.rm = TRUE)
  } else {
    df_sub <- add_cluster_col(df_sub)
    p <- ggplot(df_sub, aes(x = log10_ratio, fill = cluster)) +
      geom_histogram(binwidth = BINWIDTH, boundary = BIN_LO, colour = "white", linewidth = 0.15, position = "stack", na.rm = TRUE) +
      scale_fill_manual(values = c("not in 4x/8x cluster" = COL_OTHER, "in 4x/8x cluster" = COL_CLUST), name = NULL)
  }
  ## Outline the under/over bins in black, drawn as explicit bars at exactly
  ## the two flagged x-positions -- NOT via geom_histogram() on the sparse
  ## flagged subset, which would re-run stat_bin over the whole core range
  ## and draw a zero-height (but still bordered) rectangle for every empty
  ## bin in between, producing a spurious horizontal line along y = 0.
  if (nrow(n_flag_lab) > 0) {
    p <- p + geom_col(data = n_flag_lab, aes(x = x, y = y), inherit.aes = FALSE,
                       width = BINWIDTH, fill = NA, colour = "black", linewidth = 0.9)
  }
  p <- p +
    geom_vline(xintercept = REF_LINES, linetype = "dashed", colour = "grey40", linewidth = 0.35)
  if (nrow(n_flag_lab) > 0) {
    p <- p + geom_text(data = n_flag_lab, aes(x = x, y = y, label = y), inherit.aes = FALSE,
                        vjust = -0.4, size = base_size * 0.22)
  }
  p <- p +
    scale_x_continuous(name = panel_label, breaks = HIST_BRKS, labels = HIST_LBLS,
                        limits = c(UNDER_POS - BINWIDTH, OVER_POS + BINWIDTH), expand = c(0.02, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(y = "number of sites", subtitle = subtitle) +
    theme_diag(base_size = base_size)
  list(plot = p, under_sites = under_sites, over_sites = over_sites)
}

build_fig1 <- function(highlight) {
  h1 <- make_hist(d |> dplyr::filter(!(site_id %in% excl_hist_bio12)), "ratio_to_bio12",
                   "ERA5 MAP / WorldClim BIO12", highlight)
  h2 <- make_hist(d |> dplyr::filter(!(site_id %in% excl_hist_badm)), "ratio_to_badm_map",
                   "ERA5 MAP / BADM PI-reported MAP", highlight)
  h3 <- make_hist(d |> dplyr::filter(!(site_id %in% excl_hist_meas)), "ratio_to_measured",
                   "ERA5 MAP / tower-measured MAP", highlight)

  cap <- paste0(
    "Figure 1. Histograms of the per-site ratio of ERA5-derived MAP to three independent references, ", n_total,
    " current-network sites, core x-range fixed at 0.1x-20x (log10(ratio), bin width 0.1 log10 units); points ",
    "outside the core range are placed in a single black-outlined bin at that end (<0.1x on the left, >20x on the ",
    "right), not dropped, with the on-plot count shown above each such bin. ",
    "Panel 1 (ERA5/BIO12): n=", nrow(h1$plot$data), "; <0.1x (n=", length(h1$under_sites), "): ",
    paste(h1$under_sites, collapse = ", "), "; >20x (n=", length(h1$over_sites), "): ",
    paste(h1$over_sites, collapse = ", "), ". ",
    "Panel 2 (ERA5/BADM): n=", nrow(h2$plot$data), "; <0.1x (n=", length(h2$under_sites), "): ",
    paste(h2$under_sites, collapse = ", "), "; >20x (n=", length(h2$over_sites), "): ",
    paste(h2$over_sites, collapse = ", "), ". ",
    "Panel 3 (ERA5/tower-measured): n=", nrow(h3$plot$data), "; <0.1x (n=", length(h3$under_sites), "); >20x (n=",
    length(h3$over_sites), "): ", paste(h3$over_sites, collapse = ", "), ". ",
    "Dashed reference lines at ratio = 1x, 4x, 8x.",
    if (highlight) " Version B: 4x/8x-cluster sites in a second colour; membership rule in the shared legend and .legend.txt."
    else " Version A: all sites, one colour, no grouping."
  )

  title_txt <- TITLE_TXT
  if (!highlight) {
    fig <- (h1$plot | h2$plot | h3$plot) +
      patchwork::plot_annotation(title = title_txt,
                                  theme = theme(plot.title = element_text(size = 10, colour = "grey30", hjust = 0)))
  } else {
    fig <- (h1$plot | h2$plot | h3$plot) +
      patchwork::plot_layout(guides = "collect") &
      SHARED_LEGEND_THEME
    fig <- fig + patchwork::plot_annotation(title = title_txt, caption = FOOTNOTE("fig1_ratio_histograms_versionB"),
                                             theme = theme(plot.title = element_text(size = 10, colour = "grey30", hjust = 0),
                                                            plot.caption = element_text(size = 9, colour = "grey30", hjust = 0)))
  }
  list(fig = fig, caption = cap,
       under_over = list(h1 = h1[c("under_sites","over_sites")], h2 = h2[c("under_sites","over_sites")], h3 = h3[c("under_sites","over_sites")]))
}

fig1A <- build_fig1(highlight = FALSE)
fig1B <- build_fig1(highlight = TRUE)

out_fig1A <- file.path(OUTD, "fig1_ratio_histograms_versionA.png")
out_fig1B <- file.path(OUTD, "fig1_ratio_histograms_versionB.png")
ggsave(out_fig1A, fig1A$fig, width = 10, height = 4, dpi = 300, bg = "white")
ggsave(out_fig1B, fig1B$fig, width = 10, height = 4.7, dpi = 300, bg = "white")
write_legend_txt("fig1_ratio_histograms_versionB")
message("Saved (reformatted): ", out_fig1A)
message("Saved (reformatted): ", out_fig1B)

# ============================================================================
# 8. FIGURE 3 (reformatted) -- MONTH-MATCHED RATIO HISTOGRAM (single panel)
# ============================================================================

build_fig_mm <- function(highlight) {
  df_full <- d |> dplyr::filter(!(site_id %in% excl_month_matched)) |>
    dplyr::mutate(ratio_to_measured_month_matched = ratio_to_measured_month_matched)
  med_months <- median(df_full$n_matched_months)

  h <- make_hist(df_full, "ratio_to_measured_month_matched",
                  "per-site median month-matched ratio (ERA5/measured)", highlight)
  h$plot <- h$plot +
    labs(subtitle = paste0(h$plot$labels$subtitle, "  |  median matched months/site = ", med_months),
         title = TITLE_TXT) +
    theme(plot.title = element_text(size = 10, colour = "grey30"))

  cap <- paste0(
    "Figure 3. Per-site median of the month-matched ratio (ERA5 P_ERA / tower P_F) across every site-month with ",
    "P_F_QC >= ", MEASURED_QC_CUTOFF, " and P_F != 0 (", n_zero_pf, " site-months with P_F == 0 excluded ",
    "network-wide; no annualisation, no complete-year requirement). n sites = ", nrow(h$plot$data), "; excludes ",
    length(excl_month_matched), " sites with zero valid matched months. Median matched months per site among ",
    "those plotted = ", med_months, ". Core x-range fixed at 0.1x-20x, bin width 0.1 log10 units; points outside ",
    "it are in a single black-outlined bin at that end: <0.1x (n=", length(h$under_sites), "); >20x (n=",
    length(h$over_sites), "): ", paste(h$over_sites, collapse = ", "), ". Reference lines at ratio = 1x, 4x, 8x.",
    if (highlight) " Version B: 4x/8x-cluster sites in a second colour; membership rule in the legend and .legend.txt."
    else " Version A: all sites, one colour, no grouping."
  )

  if (highlight) {
    h$plot <- h$plot + SHARED_LEGEND_THEME +
      labs(caption = FOOTNOTE("fig3_month_matched_ratio_versionB")) +
      theme(plot.caption = element_text(size = 9, colour = "grey30", hjust = 0))
  }
  list(fig = h$plot, caption = cap, under_sites = h$under_sites, over_sites = h$over_sites, med_months = med_months)
}

figMM_A <- build_fig_mm(highlight = FALSE)
figMM_B <- build_fig_mm(highlight = TRUE)

out_figMM_A <- file.path(OUTD, "fig3_month_matched_ratio_versionA.png")
out_figMM_B <- file.path(OUTD, "fig3_month_matched_ratio_versionB.png")
ggsave(out_figMM_A, figMM_A$fig, width = 7, height = 5, dpi = 300, bg = "white")
ggsave(out_figMM_B, figMM_B$fig, width = 8, height = 5.8, dpi = 300, bg = "white")
write_legend_txt("fig3_month_matched_ratio_versionB")
message("Saved (reformatted): ", out_figMM_A)
message("Saved (reformatted): ", out_figMM_B)

# ============================================================================
# 9. FIGURE 2 (reformatted) -- SCATTER PLOTS, one shared axis range
#    y = the first-named variable, x = the second-named variable, matching
#    Figure 1's ratio direction (ERA5/reference) where ERA5 is involved.
# ============================================================================

SCATTER_LO <- 10
SCATTER_HI <- 10000
clamp_val <- function(x) pmin(pmax(x, SCATTER_LO * 1.08), SCATTER_HI * 0.92)

make_scatter <- function(df_sub, xcol, ycol, xlab, ylab, rho_info, highlight, panel_letter, base_size = 12) {
  df_sub <- df_sub |>
    dplyr::mutate(x_raw = .data[[xcol]], y_raw = .data[[ycol]],
                  x_plot = clamp_val(x_raw), y_plot = clamp_val(y_raw),
                  offscale = x_raw < SCATTER_LO | x_raw > SCATTER_HI | y_raw < SCATTER_LO | y_raw > SCATTER_HI)
  off_sites <- df_sub$site_id[df_sub$offscale]
  ann <- sprintf("(%s) n=%d, rho=%.3f, p%s%s", panel_letter, rho_info$n, rho_info$rho, fmt_p(rho_info$p),
                 if (length(off_sites) > 0) sprintf("\n%d off-scale (see caption)", length(off_sites)) else "")

  if (!highlight) {
    p <- ggplot(df_sub, aes(x = x_plot, y = y_plot, shape = offscale)) +
      geom_point(alpha = 0.4, size = 1.1, colour = COL_ALL)
  } else {
    df_sub <- add_cluster_col(df_sub) |> dplyr::arrange(cluster)  # non-cluster first, cluster drawn last (on top)
    p <- ggplot(df_sub, aes(x = x_plot, y = y_plot, shape = offscale, colour = cluster)) +
      geom_point(alpha = 0.45, size = 1.1) +
      scale_colour_manual(values = c("not in 4x/8x cluster" = COL_OTHER, "in 4x/8x cluster" = COL_CLUST), name = NULL)
  }
  p <- p +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
    geom_abline(slope = 1, intercept = 0, linetype = "solid", colour = "grey35", linewidth = 0.4) +
    geom_abline(slope = 1, intercept = log10(4),  linetype = "dashed", colour = "grey50", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = -log10(4), linetype = "dashed", colour = "grey50", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = log10(8),  linetype = "dotted", colour = "grey50", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = -log10(8), linetype = "dotted", colour = "grey50", linewidth = 0.3) +
    scale_x_log10(limits = c(SCATTER_LO, SCATTER_HI), name = xlab, expand = c(0.02, 0)) +
    scale_y_log10(limits = c(SCATTER_LO, SCATTER_HI), name = ylab, expand = c(0.02, 0)) +
    annotate("text", x = SCATTER_LO * 1.3, y = SCATTER_HI / 1.4, label = ann, hjust = 0, vjust = 1,
             size = base_size * 0.28, lineheight = 1.05) +
    theme_diag(base_size = base_size)
  list(plot = p, off_sites = off_sites, off_x = df_sub$x_raw[df_sub$offscale], off_y = df_sub$y_raw[df_sub$offscale])
}

build_fig2 <- function(highlight) {
  s1 <- make_scatter(dat_era5_badm,  "badm_map_mm",     "era5_map_mm",     "BADM MAP (mm/yr)",           "ERA5 MAP (mm/yr)",     rho_era5_badm,  highlight, "a")
  s2 <- make_scatter(dat_era5_bio12, "bio12_mm",        "era5_map_mm",     "WorldClim BIO12 (mm/yr)",    "ERA5 MAP (mm/yr)",     rho_era5_bio12, highlight, "b")
  s3 <- make_scatter(dat_era5_meas,  "measured_map_mm", "era5_map_mm",     "Tower-measured MAP (mm/yr)", "ERA5 MAP (mm/yr)",     rho_era5_meas,  highlight, "c")
  s4 <- make_scatter(dat_badm_bio12, "bio12_mm",        "badm_map_mm",     "WorldClim BIO12 (mm/yr)",    "BADM MAP (mm/yr)",     rho_badm_bio12, highlight, "d")
  s5 <- make_scatter(dat_badm_meas,  "measured_map_mm", "badm_map_mm",     "Tower-measured MAP (mm/yr)", "BADM MAP (mm/yr)",     rho_badm_meas,  highlight, "e")
  s6 <- make_scatter(dat_bio12_meas, "measured_map_mm", "bio12_mm",        "Tower-measured MAP (mm/yr)", "WorldClim BIO12 (mm/yr)", rho_bio12_meas, highlight, "f")

  fmt_off <- function(s) if (length(s$off_sites) == 0) "none" else
    paste(sprintf("%s (%.1f, %.1f)", s$off_sites, s$off_x, s$off_y), collapse = "; ")

  cap <- paste0(
    "Figure 2. Pairwise comparison of the four MAP estimates (mm/yr), log-log axes, one common x/y range shared ",
    "across all six panels (", SCATTER_LO, " to ", SCATTER_HI, " mm/yr, chosen from the bulk of the pooled data, ",
    "not the extremes). Solid line = 1:1; dashed = 4x/0.25x offset; dotted = 8x/0.125x offset. Points with either ",
    "coordinate outside this range are shown clamped to the edge as a triangle (not dropped) and named here as ",
    "(x, y) in their true, unclamped mm/yr values. ",
    "(a) ERA5 vs BADM: n=", rho_era5_badm$n, ", rho=", sprintf("%.3f", rho_era5_badm$rho), "; off-scale: ", fmt_off(s1), ". ",
    "(b) ERA5 vs BIO12: n=", rho_era5_bio12$n, ", rho=", sprintf("%.3f", rho_era5_bio12$rho), "; off-scale: ", fmt_off(s2), ". ",
    "(c) ERA5 vs tower-measured: n=", rho_era5_meas$n, ", rho=", sprintf("%.3f", rho_era5_meas$rho), "; off-scale: ", fmt_off(s3), ". ",
    "(d) BADM vs BIO12: n=", rho_badm_bio12$n, ", rho=", sprintf("%.3f", rho_badm_bio12$rho), "; off-scale: ", fmt_off(s4), ". ",
    "(e) BADM vs tower-measured: n=", rho_badm_meas$n, ", rho=", sprintf("%.3f", rho_badm_meas$rho), "; off-scale: ", fmt_off(s5), ". ",
    "(f) BIO12 vs tower-measured: n=", rho_bio12_meas$n, ", rho=", sprintf("%.3f", rho_bio12_meas$rho), "; off-scale: ", fmt_off(s6), ".",
    if (highlight) " Version B: 4x/8x-cluster sites in a second colour, drawn on top; membership rule in the shared legend and .legend.txt."
    else " Version A: all sites, one colour, no grouping."
  )

  if (!highlight) {
    fig <- (s1$plot | s2$plot | s3$plot) / (s4$plot | s5$plot | s6$plot)
  } else {
    fig <- ((s1$plot | s2$plot | s3$plot) / (s4$plot | s5$plot | s6$plot)) +
      patchwork::plot_layout(guides = "collect") &
      SHARED_LEGEND_THEME
    fig <- fig + patchwork::plot_annotation(caption = FOOTNOTE("fig2_scatter_versionB"),
                                             theme = theme(plot.caption = element_text(size = 9, colour = "grey30", hjust = 0)))
  }
  list(fig = fig, caption = cap)
}

fig2A <- build_fig2(highlight = FALSE)
fig2B <- build_fig2(highlight = TRUE)

out_fig2A <- file.path(OUTD, "fig2_scatter_versionA.png")
out_fig2B <- file.path(OUTD, "fig2_scatter_versionB.png")
ggsave(out_fig2A, fig2A$fig, width = 10, height = 9, dpi = 300, bg = "white")
ggsave(out_fig2B, fig2B$fig, width = 10, height = 9.8, dpi = 300, bg = "white")
write_legend_txt("fig2_scatter_versionB")
message("Saved (reformatted): ", out_fig2A)
message("Saved (reformatted): ", out_fig2B)

# ============================================================================
# 10. COMPANION CSV (unchanged from prior version -- not touched by this
#     reformat; re-written verbatim so the file's mtime/git diff make clear
#     no values changed)
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
    "One row per current-network site (n=781). Values unchanged by the legibility reformat of the figures in ",
    "this script (era5_map_mm/bio12_mm/badm_map_mm/ratio_to_bio12/ratio_to_badm_map/cluster_membership from ",
    "table_b1_factor_estimates.csv, v3; measured_map_mm/n_complete_measured_years/ratio_to_measured/",
    "n_matched_months/ratio_to_measured_month_matched computed from data/duckdb/fluxnet.duckdb as described in ",
    "the script header). Missing BADM (144 sites) and no-complete-measured-year (312 sites) are carried as NA, ",
    "not dropped. Plotting-only diagnostic; no new verdict, correction, or reclassification of any site."
  )
)
message("Saved: ", out_csv)

# ============================================================================
# 11. METADATA FOR FIGURES
# ============================================================================

write_output_metadata(out_fig1A, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = paste0("Version A (no grouping) of Figure 1, reformatted for legibility (fixed 0.1x-20x core range with marked overflow bins; no other value changed). ", fig1A$caption))
write_output_metadata(out_fig1B, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = paste0("Version B (4x/8x cluster highlighted) of Figure 1, reformatted for legibility: membership-rule text moved off the panels into fig1_ratio_histograms_versionB.legend.txt and a one-line footnote, one shared collected legend below all panels. ", fig1B$caption))
write_output_metadata(out_fig2A, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = paste0("Version A (no grouping) of Figure 2, reformatted for legibility (one shared 10-10000 mm/yr axis range across all 6 panels, off-scale points marked, smaller/more transparent points; no other value changed). ", fig2A$caption))
write_output_metadata(out_fig2B, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = paste0("Version B (4x/8x cluster highlighted, drawn on top) of Figure 2, reformatted for legibility: membership-rule text moved off the panels into fig2_scatter_versionB.legend.txt and a one-line footnote, one shared collected legend below all panels. ", fig2B$caption))
write_output_metadata(out_figMM_A, input_sources = "data/duckdb/fluxnet.duckdb (monthly table, read-only)",
  notes = paste0("Version A (no grouping) of Figure 3, reformatted for legibility (fixed 0.1x-20x core range with marked overflow bins; no value changed). ", figMM_A$caption))
write_output_metadata(out_figMM_B, input_sources = "data/duckdb/fluxnet.duckdb (monthly table, read-only)",
  notes = paste0("Version B (4x/8x cluster highlighted) of Figure 3, reformatted for legibility: membership-rule text moved into fig3_month_matched_ratio_versionB.legend.txt and a one-line footnote. ", figMM_B$caption))

# ============================================================================
# 12. EXCLUSION SUMMARY (unchanged from prior version)
# ============================================================================

out_excl <- file.path(OUTD, "table_panel_exclusions.csv")
readr::write_csv(reasons, out_excl)
write_output_metadata(out_excl, input_sources = c(b1_path, "data/duckdb/fluxnet.duckdb (monthly table, read-only)"),
  notes = "Sites excluded from one or more panels of Figure 1, Figure 2, or Figure 3 because a value used on a log axis is exactly zero or missing, or (Figure 3 only) because no valid matched month exists, and the specific reason. Unchanged by the legibility reformat of the figures. Not an exclusion from the companion CSV, which carries all 781 sites. Overflow/off-scale sites for the reformatted axis ranges are separate from this table and are listed instead in each figure's own .meta.json caption and in report.md.")
message("Saved: ", out_excl)

message("\n=== era5_reference_plots.R complete ===")
