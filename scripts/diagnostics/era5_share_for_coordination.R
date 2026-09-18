## era5_share_for_coordination.R
##
## Assembles a self-contained, externally-shareable package summarising the
## ERA5-vs-reference precipitation pattern, for the FLUXNET Coordination
## Project and hub data managers. Read-only with respect to every existing
## output: reuses table_site_reference_comparison.csv (era5_reference_plots)
## and table_coordination_project_evidence.csv (era5_precip_units_v3)
## unmodified, and reads the earlier era5_cumulative_test report only to
## quote its already-computed numbers -- nothing in any other diagnostics
## folder, the pipeline, or any figure is written to. All new files go
## under review/diagnostics/era5_share_for_coordination/.
##
## Raw per-month ERA5 (P_ERA) and tower-measured (P_F, P_F_QC) values are
## read directly from data/extracted/*/*_ERA5_MM_*.csv and
## *_FLUXMET_MM_*.csv (not DuckDB), consistent with how those figures'
## source values were established.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(fs)
  library(lubridate)
})

OUTD <- "review/diagnostics/era5_share_for_coordination"
fs::dir_create(OUTD)

message("=== era5_share_for_coordination.R ===")

MEASURED_QC_CUTOFF <- 0.9

# ============================================================================
# 0. SOURCE TABLES (read-only) AND RAW-FILE READERS
# ============================================================================

comparison <- readr::read_csv("review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv", show_col_types = FALSE)
stopifnot(nrow(comparison) == 781)

extracted_dirs <- list.dirs("data/extracted", recursive = FALSE, full.names = TRUE)
dir_site_ids <- sub("^[A-Za-z0-9]+_([A-Za-z]{2}-[A-Za-z0-9]+)_FLUXNET_.*$", "\\1", basename(extracted_dirs))
site_dir_lookup <- setNames(extracted_dirs, dir_site_ids)

read_monthly_csv <- function(site_id, pattern) {
  d <- site_dir_lookup[[site_id]]
  if (is.null(d) || is.na(d)) return(NULL)
  f <- list.files(d, pattern = pattern, full.names = TRUE)
  if (length(f) != 1) return(NULL)
  readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
}

read_era5_mm <- function(site_id) {
  raw <- read_monthly_csv(site_id, "_ERA5_MM_.*\\.csv$")
  if (is.null(raw) || !all(c("TIMESTAMP", "P_ERA") %in% names(raw))) return(NULL)
  raw |> dplyr::transmute(site_id = site_id, year = TIMESTAMP %/% 100L, month = TIMESTAMP %% 100L,
                            P_ERA_mm_day = P_ERA,
                            days = lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", year, month))),
                            P_ERA_mm_month = P_ERA * days)
}

read_fluxmet_mm <- function(site_id) {
  raw <- read_monthly_csv(site_id, "_FLUXMET_MM_.*\\.csv$")
  if (is.null(raw) || !all(c("TIMESTAMP", "P_F", "P_F_QC") %in% names(raw))) return(NULL)
  raw |> dplyr::transmute(site_id = site_id, year = TIMESTAMP %/% 100L, month = TIMESTAMP %% 100L,
                            P_F_mm_day = P_F, P_F_QC,
                            days = lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", year, month))),
                            P_F_mm_month = P_F * days,
                            measured = !is.na(P_F_QC) & P_F_QC >= MEASURED_QC_CUTOFF)
}

theme_share <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(t = 8, r = 12, b = 6, l = 6),
          plot.subtitle = element_text(size = base_size * 0.75, colour = "grey30"))
}

# ============================================================================
# 1. SITE LIST: 123 flagged (4x/8x cluster) + US-HB4 + IT-MBo
# ============================================================================

message("\n================ Site list ================")

flagged <- comparison |> dplyr::filter(cluster_membership %in% c("4x_cluster", "8x_cluster"))
stopifnot(nrow(flagged) == 123)
extra <- comparison |> dplyr::filter(site_id %in% c("US-HB4", "IT-MBo"))
stopifnot(nrow(extra) == 2)

site_list <- dplyr::bind_rows(flagged, extra) |>
  dplyr::mutate(
    flag_basis = dplyr::case_when(
      cluster_membership == "4x_cluster" ~ "empirical factor near 4x",
      cluster_membership == "8x_cluster" ~ "empirical factor near 8x",
      TRUE ~ "extreme outlier (not part of the 4x/8x grouping; added separately)"
    )
  ) |>
  dplyr::select(site_id, data_hub, product_source_network,
                 era5_annual_precip_mm = era5_map_mm,
                 worldclim_bio12_mm = bio12_mm,
                 badm_map_mm,
                 measured_annual_precip_mm = measured_map_mm,
                 n_complete_measured_years,
                 ratio_era5_to_bio12 = ratio_to_bio12,
                 ratio_era5_to_badm = ratio_to_badm_map,
                 ratio_era5_to_measured = ratio_to_measured,
                 flag_basis)

message(sprintf("Site list: %d rows (123 flagged + 2 isolated outliers)", nrow(site_list)))

out_sitelist <- file.path(OUTD, "site_list.csv")
readr::write_csv(site_list, out_sitelist)
message("Saved: ", out_sitelist)

dict_text <- c(
  "DATA DICTIONARY -- site_list.csv",
  "",
  "site_id                    FLUXNET site ID.",
  "data_hub                   The data-distributing hub (e.g. AmeriFlux, ICOS, TERN).",
  "product_source_network     The contributing regional network that produced this",
  "                           site's data product (e.g. AMF, ICOS, JPF, CNF, EUF,",
  "                           FLX, KOF, SAEON, TERN). Not derivable from the site ID.",
  "era5_annual_precip_mm      Mean annual precipitation from ERA5 reanalysis at the",
  "                           site's coordinates, mm/year. Computed as the mean, over",
  "                           all complete calendar years 1991-2020, of the sum of",
  "                           (that month's ERA5 rate x days in that month).",
  "worldclim_bio12_mm         WorldClim v2.1 BIO12 (annual precipitation), mm/year, a",
  "                           gridded, station-interpolated climatology, ~5 km pixel.",
  "badm_map_mm                Mean annual precipitation as reported by the site PI in",
  "                           the site's own BADM metadata, mm/year.",
  "measured_annual_precip_mm  Mean annual precipitation from the tower's own gauge,",
  "                           mm/year, present only where the site has at least one",
  "                           complete calendar year (all 12 months) meeting the",
  "                           measured-data-quality threshold described in README.md.",
  "                           Blank where no such year exists.",
  "n_complete_measured_years  Number of calendar years contributing to",
  "                           measured_annual_precip_mm. 0 where that column is blank.",
  "ratio_era5_to_bio12        era5_annual_precip_mm / worldclim_bio12_mm.",
  "ratio_era5_to_badm         era5_annual_precip_mm / badm_map_mm.",
  "ratio_era5_to_measured     era5_annual_precip_mm / measured_annual_precip_mm.",
  "                           Blank where measured_annual_precip_mm is blank.",
  "flag_basis                 Why the site is in this list: 'empirical factor near",
  "                           4x' or 'near 8x' (see README.md for the exact rule), or",
  "                           'extreme outlier' for the 2 sites added individually",
  "                           (US-HB4, IT-MBo), which do not fall into either group."
)
out_dict <- file.path(OUTD, "data_dictionary.txt")
writeLines(dict_text, out_dict)
message("Saved: ", out_dict)

# ============================================================================
# 2. FIGURE 1 -- US-HB4 and IT-MBo, monthly ERA5 vs. measured, full record
# ============================================================================

message("\n================ Figure 1: US-HB4 and IT-MBo, ERA5 vs measured ================")

f1_sites <- c("US-HB4", "IT-MBo")
f1_era5 <- purrr::map(f1_sites, read_era5_mm) |> dplyr::bind_rows()
f1_meas <- purrr::map(f1_sites, read_fluxmet_mm) |> dplyr::bind_rows() |> dplyr::filter(measured)

f1_range <- f1_meas |> dplyr::group_by(site_id) |>
  dplyr::summarise(y0 = min(year), y1 = max(year), .groups = "drop")
cat("\nFigure 1 date range (measured months only):\n")
print(as.data.frame(f1_range))

f1_long <- dplyr::bind_rows(
  f1_era5 |> dplyr::inner_join(f1_range, by = "site_id") |> dplyr::filter(year >= y0, year <= y1) |>
    dplyr::transmute(site_id, date = as.Date(sprintf("%04d-%02d-01", year, month)),
                       mm_month = P_ERA_mm_month, series = "ERA5"),
  f1_meas |> dplyr::transmute(site_id, date = as.Date(sprintf("%04d-%02d-01", year, month)),
                                mm_month = P_F_mm_month, series = "Tower-measured")
)

f1_n <- f1_meas |> dplyr::count(site_id, name = "n_measured_months")
cat("\nn genuinely-measured months per site:\n"); print(as.data.frame(f1_n))

fig1 <- ggplot(f1_long, aes(x = date, y = mm_month, colour = series)) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  geom_point(size = 0.9, na.rm = TRUE) +
  facet_wrap(~site_id, ncol = 1, scales = "free_x") +
  scale_y_log10(name = "precipitation (mm per month, log scale)") +
  scale_colour_manual(values = c("ERA5" = "#4C72B0", "Tower-measured" = "#D55E00"), name = NULL) +
  labs(x = NULL, subtitle = "Gaps in the tower-measured line are months that do not meet the measured-data-quality threshold (see README.md)") +
  theme_share(base_size = 13) +
  theme(legend.position = "bottom")

out_fig1 <- file.path(OUTD, "fig1_hb4_mbo_era5_vs_measured.png")
ggsave(out_fig1, fig1, width = 8.5, height = 7, dpi = 300, bg = "white")
write_output_metadata(out_fig1, input_sources = c("data/extracted/AMF_US-HB4_*/*.csv", "data/extracted/ICOS_IT-MBo_*/*.csv"),
  notes = "Monthly ERA5 and tower-measured precipitation, mm/month, log y-axis, for the full record where the tower has at least one genuinely-measured month, at US-HB4 and IT-MBo.")
message("Saved: ", out_fig1)

# ============================================================================
# 3. FIGURE 2 -- ratio histograms, all sites, ERA5/BIO12 and ERA5/BADM
# ============================================================================

message("\n================ Figure 2: ratio histograms, all sites ================")

BINWIDTH <- 0.1
DISP_LO <- 0.2
DISP_HI <- 10

make_ratio_hist <- function(ratios, label) {
  df <- data.frame(ratio = ratios) |> dplyr::filter(is.finite(ratio), ratio > 0)
  n_total <- nrow(df)
  n_outside <- sum(df$ratio < DISP_LO | df$ratio > DISP_HI)
  df_shown <- df |> dplyr::filter(ratio >= DISP_LO, ratio <= DISP_HI) |> dplyr::mutate(log10_ratio = log10(ratio))
  bins <- c(sum(df$ratio >= 0.5 & df$ratio < 1.5), sum(df$ratio >= 1.5 & df$ratio < 3.0), sum(df$ratio >= 3.0 & df$ratio < 6.0))
  subtitle <- sprintf("n=%d | 0.5-1.5x: %d | 1.5-3x: %d | 3-6x: %d", n_total, bins[1], bins[2], bins[3])
  p <- ggplot(df_shown, aes(x = log10_ratio)) +
    geom_histogram(binwidth = BINWIDTH, boundary = log10(DISP_LO), fill = "#4C72B0", colour = "white", linewidth = 0.15) +
    geom_vline(xintercept = log10(c(1, 4)), linetype = "dashed", colour = "grey30", linewidth = 0.4) +
    scale_x_continuous(name = label, breaks = log10(c(0.2, 0.5, 1, 4, 10)), labels = c("0.2x", "0.5x", "1x", "4x", "10x")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(y = "number of sites", subtitle = subtitle) +
    theme_share(base_size = 13)
  list(plot = p, n_total = n_total, n_outside = n_outside, bins = bins)
}

h1 <- make_ratio_hist(comparison$ratio_to_bio12[comparison$era5_map_mm > 0], "ERA5 annual precipitation / WorldClim BIO12")
h2 <- make_ratio_hist(comparison$ratio_to_badm_map[comparison$era5_map_mm > 0 & !is.na(comparison$badm_map_mm) & comparison$badm_map_mm > 0],
                        "ERA5 annual precipitation / BADM PI-reported MAP")

cat(sprintf("\nPanel 1 (ERA5/BIO12): n=%d, 0.5-1.5x=%d, 1.5-3x=%d, 3-6x=%d, outside range=%d\n",
             h1$n_total, h1$bins[1], h1$bins[2], h1$bins[3], h1$n_outside))
cat(sprintf("Panel 2 (ERA5/BADM):  n=%d, 0.5-1.5x=%d, 1.5-3x=%d, 3-6x=%d, outside range=%d\n",
             h2$n_total, h2$bins[1], h2$bins[2], h2$bins[3], h2$n_outside))

fig2 <- (h1$plot | h2$plot) +
  patchwork::plot_annotation(
    caption = sprintf("Display range 0.2x-10x; %d sites (panel 1) and %d sites (panel 2) outside it are not shown.", h1$n_outside, h2$n_outside),
    theme = theme(plot.caption = element_text(size = 9, colour = "grey30", hjust = 0)))
out_fig2 <- file.path(OUTD, "fig2_ratio_histograms.png")
ggsave(out_fig2, fig2, width = 10, height = 4.6, dpi = 300, bg = "white")
write_output_metadata(out_fig2, input_sources = "review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv",
  notes = sprintf("Ratio histograms, log10 x-axis, bin width 0.1 log10 units, all current-network sites. Panel 1 (ERA5/BIO12): n=%d. Panel 2 (ERA5/BADM): n=%d. Dashed lines at ratio=1x,4x. Display range [%.1fx, %dx]; %d and %d sites respectively fall outside it and are not shown.",
                    h1$n_total, h2$n_total, DISP_LO, DISP_HI, h1$n_outside, h2$n_outside))
message("Saved: ", out_fig2)

# ============================================================================
# 4. FIGURE 3 -- small multiples, 5 affected + 2 control sites, shared scale
# ============================================================================

message("\n================ Figure 3: small multiples, shared y-scale ================")

AFFECTED5 <- c("JP-Tak", "JP-Mse", "KH-Kmp", "PE-QFR", "BR-Ji3")
CONTROLS2 <- c("US-Ha1", "DE-Tha")
F3_SITES <- c(AFFECTED5, CONTROLS2)

last_three_consecutive <- function(years) {
  years <- sort(unique(years), decreasing = TRUE)
  for (y in years) if ((y - 1) %in% years && (y - 2) %in% years) return(c(y - 2, y - 1, y))
  NA_integer_
}

f3_era5 <- purrr::map(F3_SITES, read_era5_mm) |> dplyr::bind_rows()
f3_complete <- f3_era5 |> dplyr::filter(!is.na(P_ERA_mm_day)) |>
  dplyr::group_by(site_id, year) |> dplyr::filter(dplyr::n_distinct(month) == 12L) |> dplyr::ungroup()
f3_years <- f3_complete |> dplyr::group_by(site_id) |> dplyr::summarise(years3 = list(last_three_consecutive(year)), .groups = "drop")

f3_plot_df <- f3_complete |> dplyr::inner_join(f3_years, by = "site_id") |>
  dplyr::rowwise() |> dplyr::filter(year %in% years3) |> dplyr::ungroup() |>
  dplyr::mutate(role = ifelse(site_id %in% AFFECTED5, "affected", "control"))

f3_totals <- f3_plot_df |> dplyr::group_by(site_id, year) |>
  dplyr::summarise(annual_mm = sum(P_ERA_mm_month), .groups = "drop")
cat("\nAnnual ERA5 totals, 3 plotted years per site:\n")
print(as.data.frame(f3_totals))

annot <- f3_totals |> dplyr::group_by(site_id) |>
  dplyr::summarise(label = paste0("Annual total, mm/yr: ", paste(sprintf("%d=%.0f", year, annual_mm), collapse = "  |  ")), .groups = "drop")

y_max <- max(f3_plot_df$P_ERA_mm_month) * 1.05

## Each site gets its own panel (not facet_wrap) so its annual-total label can
## sit in the panel's subtitle -- above the data region, not inside it -- while
## every panel still shares an identical, explicitly fixed y-axis.
make_panel <- function(sid, role) {
  df <- f3_plot_df |> dplyr::filter(site_id == sid)
  lab <- annot$label[annot$site_id == sid]
  ggplot(df, aes(x = month, y = P_ERA_mm_month, colour = factor(year))) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.1) +
    scale_x_continuous(breaks = c(1, 4, 7, 10, 12), limits = c(1, 12)) +
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.02))) +
    scale_colour_manual(values = c("#4C72B0", "#55A868", "#C44E52"), name = "year") +
    labs(x = "month", y = "ERA5 mm/month", title = sprintf("%s (%s)", sid, role), subtitle = lab) +
    theme_share(base_size = 11) +
    theme(plot.title = element_text(size = 11, face = "bold"), plot.subtitle = element_text(size = 8.3))
}

panels <- purrr::map2(F3_SITES, ifelse(F3_SITES %in% AFFECTED5, "affected", "control"), make_panel)

fig3 <- (panels[[1]] | panels[[2]] | panels[[3]]) /
        (panels[[4]] | panels[[5]] | plot_spacer()) /
        (panels[[6]] | panels[[7]] | plot_spacer()) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
fig3 <- fig3 + patchwork::plot_annotation(
  caption = "All panels share the same y-axis. Rows 1-2: sites with an elevated ERA5-to-reference ratio. Row 3: two unaffected sites.",
  theme = theme(plot.caption = element_text(size = 9, colour = "grey30", hjust = 0)))

out_fig3 <- file.path(OUTD, "fig3_seasonal_cycle_small_multiples.png")
ggsave(out_fig3, fig3, width = 12, height = 9.5, dpi = 300, bg = "white")
write_output_metadata(out_fig3, input_sources = "data/extracted/*/*_ERA5_MM_*.csv",
  notes = "Monthly ERA5 precipitation, mm/month, 3 consecutive complete years, 5 flagged + 2 unaffected sites, one shared y-axis across all 7 panels. Each panel's own annual total per plotted year is given as an in-panel-header annotation (not inside the data region).")
message("Saved: ", out_fig3)

message("\n=== era5_share_for_coordination.R complete (figures + CSV + dictionary; README.md written separately) ===")
