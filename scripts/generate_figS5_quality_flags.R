## generate_figS5_quality_flags.R
##
## Supplementary Figure S5: quality flags, promoted from the interim
## technical-validation candidate (scripts/technical_validation_interim.R,
## review/technical_validation_interim/) into the paper's own supplementary
## material, per logs/prompts/2026-10-12_renumber_figures.md.
##
## Reuses that script's Figure 1 ("Gaps by variable and time step") data and
## panel-building logic, with two differences:
##   - Panel a keeps only the daily and monthly ECDF sub-panels; the annual
##     sub-panel is dropped here (it moves to Figure 6, not built in this run).
##   - Panel b (pooled sub-daily measured/gap-filled shares at the 31 sites
##     with sub-daily FLUXMET files extracted) is unchanged.
## Does NOT re-run any dq_stage*.R diagnostic or touch
## review/technical_validation_interim/ -- reads only its already-written
## table CSVs and the DuckDB store directly (same two sources
## technical_validation_interim.R itself uses for this figure), read-only.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr); library(tidyr)
  library(ggplot2); library(patchwork); library(fs)
})
source("R/utils.R")
source("R/plot_constants.R")
source("R/nature_format.R")

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Supplementary Figure S5: quality flags ===")

DIAG_DIR <- "review/diagnostics/data_quality_uncertainty"
OUT_DIR  <- "review/figures/draft_manuscript_v1/SupFigs"
fs::dir_create(OUT_DIR)
OUT_STEM <- file.path(OUT_DIR, "figS5_quality_flags")
db_path  <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
SNAPSHOT_OF_RECORD <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"

## Local theme/palettes -- copied verbatim from technical_validation_interim.R
## (that script's own comment: no dedicated VUT/CUT or 4-variable palette
## exists yet in R/plot_constants.R), so this figure matches fig_tv1 exactly.
tv_theme <- function() {
  ggplot2::theme_classic(base_size = 7) +
    ggplot2::theme(
      panel.border      = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.8),
      panel.background  = ggplot2::element_blank(),
      axis.text         = ggplot2::element_text(colour = "black"),
      axis.ticks        = ggplot2::element_line(colour = "black"),
      axis.ticks.length = grid::unit(-3, "pt")
    ) +
    nature_theme()
}
FOUR_VAR_COLOURS <- c(NEE_VUT = "#2C7BB6", NEE_CUT = "#D7301F", LE = "#1B9E77", H = "#E6AB02")
## Display labels for the four QC variables -- same order/colours as
## FOUR_VAR_COLOURS, matched by name so scale_*_manual(labels=) resolves
## correctly regardless of break order. Matches main Figure 6.
VAR_LABELS <- c(NEE_VUT = "NEE (VUT)", NEE_CUT = "NEE (CUT)", LE = "LE", H = "H")
QC_FLAG_COLOURS <- c(measured = "#2166AC", good_gapfill = "#92C5DE",
                      medium_gapfill = "#F4A582", poor_gapfill = "#B2182B")
QC_FLAG_LABELS  <- c(measured = "measured", good_gapfill = "good gap-fill",
                      medium_gapfill = "medium gap-fill", poor_gapfill = "poor gap-fill")

## ---- Panel a: daily/monthly ECDFs of the QC flag fraction, 4 variables ----
## DIRECT DUCKDB READ, read-only -- same dataset='FLUXMET' scope and query as
## technical_validation_interim.R's Figure 1a (the existing summary table
## only stores quantiles, not the full per-site-period distribution an ECDF
## needs). Annual is loaded by that script too but dropped here.
con <- dbConnect(duckdb(), db_path, read_only = TRUE)
qc_cols <- c(NEE_VUT = "NEE_VUT_REF_QC", NEE_CUT = "NEE_CUT_REF_QC",
             LE = "LE_F_MDS_QC", H = "H_F_MDS_QC")
resolutions <- c(daily = "daily", monthly = "monthly")

ecdf_data <- bind_rows(lapply(names(resolutions), function(res_name) {
  tbl <- resolutions[[res_name]]
  bind_rows(lapply(names(qc_cols), function(var_name) {
    col <- qc_cols[[var_name]]
    d <- dbGetQuery(con, sprintf(
      "SELECT %s AS qc FROM %s WHERE dataset = 'FLUXMET' AND %s IS NOT NULL", col, tbl, col))
    if (nrow(d) == 0L) return(NULL)
    tibble(resolution = res_name, variable = var_name, qc = d$qc)
  }))
}))
dbDisconnect(con, shutdown = TRUE)
msg("Panel a: ", nrow(ecdf_data), " QC values pulled (daily/monthly x 4 variables).")

## Variable order fixed to NEE_VUT, NEE_CUT, LE, H (matches panel b, which
## already sets this factor order on subdaily_net$variable below).
ecdf_data <- ecdf_data |> mutate(resolution = factor(resolution, levels = c("daily", "monthly")),
                                   variable = factor(variable, levels = c("NEE_VUT", "NEE_CUT", "LE", "H")))

make_ecdf_panel <- function(res_name, show_legend) {
  d <- ecdf_data |> filter(resolution == res_name)
  p <- ggplot(d, aes(x = qc, colour = variable)) +
    stat_ecdf(geom = "step", linewidth = nature_lwd(1.0)) +
    geom_vline(xintercept = 0.50, linetype = "dashed", colour = "grey50", linewidth = nature_lwd(0.3)) +
    scale_colour_manual(values = FOUR_VAR_COLOURS, name = "Variable", labels = VAR_LABELS) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = paste0(tools::toTitleCase(res_name), " QC flag (fraction)"), y = "Cumulative share") +
    tv_theme()
  if (show_legend) p + theme(legend.position = "right") else p + theme(legend.position = "none")
}
p_daily   <- make_ecdf_panel("daily",   show_legend = FALSE)
p_monthly <- make_ecdf_panel("monthly", show_legend = TRUE)

panel_a <- patchwork::wrap_elements(full = p_daily | p_monthly)

## ---- Panel b: pooled sub-daily QC 0/1/2/3 shares, 31 sites ---------------
## Unchanged from fig_tv1 panel b.
subdaily_net <- read_csv(file.path(DIAG_DIR, "table_stage1_subdaily_qc_network_summary.csv"),
                          show_col_types = FALSE) |>
  pivot_longer(cols = -variable, names_to = "flag", values_to = "share") |>
  mutate(flag = factor(flag, levels = c("measured", "good_gapfill", "medium_gapfill", "poor_gapfill")),
         variable = factor(variable, levels = c("NEE_VUT", "NEE_CUT", "LE", "H")))

panel_b <- ggplot(subdaily_net, aes(x = variable, y = share, fill = flag)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_x_discrete(labels = VAR_LABELS) +
  scale_fill_manual(values = QC_FLAG_COLOURS, labels = QC_FLAG_LABELS, name = NULL) +
  scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Share of sub-daily records (31 sites)") +
  tv_theme() +
  theme(legend.position = "bottom")

fig <- (panel_a / panel_b) + plot_layout(heights = c(1, 0.8)) +
  plot_annotation(tag_levels = "a") &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 8, family = NATURE_FONT))

## 180mm, not NATURE_WIDTH_DOUBLE_MM (183mm): SupFigs/ convention -- see
## scripts/supp_stage2_record_length_collections_figure.R's own comment on
## this -- check_figure_format.R enforces <=180mm for every figure under
## SupFigs/, and extended_data = TRUE both applies that ED width/height limit
## and writes the companion JPEG every "ed"-kind figure needs.
saved <- save_nature_figure(fig, OUT_STEM, width_mm = 180, height_mm = 150, extended_data = TRUE)
msg("Saved: ", saved$png, ", ", saved$pdf)

## ---- 31-site sub-daily subset: computed ICOS count, and the US-MMS note ---
## CLAUDE.md Hard Rule 2: data_hub, not site ID prefix.
hh_hr_sites <- read_csv(file.path(DIAG_DIR, "table_stage0_hh_hr_sites.csv"), show_col_types = FALSE)
snap <- read_csv(SNAPSHOT_OF_RECORD, show_col_types = FALSE) |>
  distinct(site_id, .keep_all = TRUE) |> select(site_id, data_hub)
subdaily_hub <- hh_hr_sites |> left_join(snap, by = "site_id")
n_subdaily  <- nrow(subdaily_hub)
n_icos      <- sum(subdaily_hub$data_hub == "ICOS", na.rm = TRUE)
us_mms_row  <- hh_hr_sites |> filter(site_id == "US-MMS")
us_mms_res  <- if (nrow(us_mms_row) == 1L) us_mms_row$time_resolution else NA_character_
msg("Sub-daily 31-site subset: ", n_icos, " of ", n_subdaily, " are ICOS sites (",
    round(100 * n_icos / n_subdaily, 1), "%). US-MMS resolution: ", us_mms_res,
    " (extracted 2026-05-25, confirmed from data/extracted/AMF_US-MMS_FLUXNET_1999-2023_v1.3_r1/ file dates).")

## ---- Legend ----------------------------------------------------------------
legend_lines <- c(
  "FIGURE LEGEND -- figS5_quality_flags.png",
  strrep("=", 60), "",
  "TITLE: Supplementary Figure S5 -- Quality flags by variable and time step", "",
  "PUBLICATION LEGEND:",
  "Supplementary Figure S5. Quality-control (QC) flag distributions for four flux variables,",
  "and the measured/gap-filled share of sub-daily records at the sites with sub-daily data",
  "extracted.", "",
  "DESCRIPTION:",
  sprintf("(a) Empirical cumulative distribution functions (ECDFs) of the QC flag fraction for"),
  "NEE_VUT, NEE_CUT, LE and H -- shown in that fixed order in both panels -- one panel per",
  "temporal resolution (daily, monthly; left to right), across all 781-site FLUXMET",
  "site-periods. The QC flag fraction runs 1 (every underlying half-hourly/hourly record for",
  "that site-period was measured or good-quality gap-filled) down to 0 (every record is",
  "medium- or poor-quality gap-filled, or missing) -- higher is better. The annual panel (and",
  "weekly, excluded throughout for the same reason as daily/monthly here) is not shown here --",
  "annual QC moved to Figure 6 (review/figures/draft_manuscript_v1/fig_06_quality_uncertainty.png,",
  "built 2026-10-13). The dashed vertical line marks QC =",
  "0.50, the 0.5 rule applied throughout this synthesis (QC_THRESHOLD_YY/MM/DD/WW,",
  "R/pipeline_config.R).",
  sprintf("(b) Pooled share of sub-daily (half-hourly/hourly) records in each of the four true"),
  sprintf("QC categories (measured, good/medium/poor gap-fill), for the %d of 781 sites with", n_subdaily),
  sprintf("sub-daily FLUXMET files extracted on disk -- %d of these %d sites (%.0f%%) are ICOS", n_icos, n_subdaily, 100*n_icos/n_subdaily),
  "sites (computed from data_hub in the snapshot of record, not inferred from site ID prefix --",
  "CLAUDE.md Hard Rule 2), so this subset is hub-skewed toward ICOS relative to the network as a",
  "whole, though roughly IGBP-representative -- read as an ICOS-weighted estimate, not a network",
  sprintf("average. One of the %d, US-MMS, is hourly (HR) rather than half-hourly (HH), from its", n_subdaily),
  "May 2026 extraction (data/extracted/AMF_US-MMS_FLUXNET_1999-2023_v1.3_r1/, files dated",
  "2026-05-25).", "",
  "COLOUR CODING:",
  "(a) NEE_VUT/NEE_CUT/LE/H, fixed 4-colour palette (this script's own FOUR_VAR_COLOURS;",
  "no dedicated palette yet exists in R/plot_constants.R for these four variables), displayed",
  "in the legend (a) and axis labels (b) as 'NEE (VUT)', 'NEE (CUT)', 'LE', 'H' (same order and",
  "colours as the underlying column names).",
  "(b) QC category, blue (measured) to red (poor gap-fill), ordinal.", "",
  "DATA SOURCE:",
  "(a) Direct read of the daily/monthly DuckDB tables (dataset = 'FLUXMET'), read-only -- the",
  "existing table_stage1_qc_distribution.csv only stores summary quantiles, not the full",
  "per-site-period distribution an ECDF needs.",
  "(b) review/diagnostics/data_quality_uncertainty/table_stage1_subdaily_qc_network_summary.csv,",
  "unchanged. ICOS count computed here from table_stage0_hh_hr_sites.csv joined to the",
  sprintf("snapshot of record (%s) on data_hub.", basename(SNAPSHOT_OF_RECORD)),
  "Both ultimately trace to data/duckdb/fluxnet.duckdb.", "",
  "PROVENANCE: promoted from the interim technical-validation candidate",
  "(scripts/technical_validation_interim.R, review/technical_validation_interim/fig_tv1_*),",
  "same data and panel-building logic, annual sub-panel dropped (see DESCRIPTION above).", "",
  "SOURCE: scripts/generate_figS5_quality_flags.R.",
  "DIMENSIONS: 180 x 150 mm, 600 dpi PNG + vector PDF, Helvetica, white background."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))

write_output_metadata(
  saved$png,
  input_sources = c(db_path, SNAPSHOT_OF_RECORD,
                     file.path(DIAG_DIR, "table_stage1_subdaily_qc_network_summary.csv"),
                     file.path(DIAG_DIR, "table_stage0_hh_hr_sites.csv")),
  notes = paste0("Supplementary Figure S5: quality flags, promoted from the interim technical-",
                  "validation candidate's fig_tv1 (daily/monthly ECDF panels only; annual moves to ",
                  "Figure 6). ", n_icos, " of ", n_subdaily, " sub-daily sites are ICOS; US-MMS is ",
                  "hourly from the 2026-05-25 extraction. See .legend.txt.")
)

msg("=== Done ===")
