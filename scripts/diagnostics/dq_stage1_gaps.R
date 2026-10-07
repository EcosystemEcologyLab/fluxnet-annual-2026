## dq_stage1_gaps.R
##
## Data quality / uncertainty diagnostic -- Stage 1: gaps.
##
## Unattended background run. Diagnostic only, writes to
## review/diagnostics/data_quality_uncertainty/. Reads pre-QC DuckDB tables
## (dataset = 'FLUXMET') only.
##
## For NEE (VUT, CUT), LE, H at daily/weekly/monthly/annual: distribution of
## the QC flag (fraction measured-or-well-gapfilled, 0-1) across site-periods
## -- quantiles, share >= 0.50, share >= 0.75, share == 1 -- overall, by IGBP
## class, and by hub (data_hub; see Stage 0 report for why). For the 31 sites
## with HH/HR files (Stage 0), reads only the integer *_QC columns from the
## extracted CSVs directly and reports the true measured/good/medium/poor
## split, compared with the network's IGBP/hub composition.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr)
  library(purrr); library(jsonlite); library(fs); library(ggplot2); library(tidyr)
})

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

OUT_DIR <- "review/diagnostics/data_quality_uncertainty"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

dir.create("logs", showWarnings = FALSE)
LOG_FILE <- file.path("logs", paste0("dq_stage1_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output"); sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== Stage 1: Gaps ===")

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
con <- dbConnect(duckdb(), db_path, read_only = TRUE)

site_lookup <- dbGetQuery(con, "
  SELECT site_id, max(igbp) AS igbp, max(data_hub) AS data_hub
  FROM manifest GROUP BY site_id
") |> as_tibble()

qc_cols <- c(
  NEE_VUT = "NEE_VUT_REF_QC",
  NEE_CUT = "NEE_CUT_REF_QC",
  LE      = "LE_F_MDS_QC",
  H       = "H_F_MDS_QC"
)

resolutions <- c(daily = "daily", weekly = "weekly", monthly = "monthly", annual = "annual")

## ---------------------------------------------------------------------------
## Pull site_id + QC columns per table (small: <= ~2.3M rows for daily, one
## table at a time), join site-level igbp/data_hub, compute distribution
## stats overall / by igbp / by hub.
## ---------------------------------------------------------------------------
summarise_qc <- function(df, qc_col) {
  bind_rows(
    df |> summarise(
      group_type = "overall", group_value = "ALL",
      n = sum(!is.na(.data[[qc_col]])),
      q05 = quantile(.data[[qc_col]], 0.05, na.rm = TRUE),
      q25 = quantile(.data[[qc_col]], 0.25, na.rm = TRUE),
      q50 = quantile(.data[[qc_col]], 0.50, na.rm = TRUE),
      q75 = quantile(.data[[qc_col]], 0.75, na.rm = TRUE),
      q95 = quantile(.data[[qc_col]], 0.95, na.rm = TRUE),
      share_ge_050 = mean(.data[[qc_col]] >= 0.50, na.rm = TRUE),
      share_ge_075 = mean(.data[[qc_col]] >= 0.75, na.rm = TRUE),
      share_eq_1   = mean(.data[[qc_col]] == 1, na.rm = TRUE)
    ),
    df |> filter(!is.na(igbp)) |> group_by(group_value = igbp) |> summarise(
      n = sum(!is.na(.data[[qc_col]])),
      q05 = quantile(.data[[qc_col]], 0.05, na.rm = TRUE),
      q25 = quantile(.data[[qc_col]], 0.25, na.rm = TRUE),
      q50 = quantile(.data[[qc_col]], 0.50, na.rm = TRUE),
      q75 = quantile(.data[[qc_col]], 0.75, na.rm = TRUE),
      q95 = quantile(.data[[qc_col]], 0.95, na.rm = TRUE),
      share_ge_050 = mean(.data[[qc_col]] >= 0.50, na.rm = TRUE),
      share_ge_075 = mean(.data[[qc_col]] >= 0.75, na.rm = TRUE),
      share_eq_1   = mean(.data[[qc_col]] == 1, na.rm = TRUE),
      .groups = "drop"
    ) |> mutate(group_type = "igbp"),
    df |> filter(!is.na(data_hub)) |> group_by(group_value = data_hub) |> summarise(
      n = sum(!is.na(.data[[qc_col]])),
      q05 = quantile(.data[[qc_col]], 0.05, na.rm = TRUE),
      q25 = quantile(.data[[qc_col]], 0.25, na.rm = TRUE),
      q50 = quantile(.data[[qc_col]], 0.50, na.rm = TRUE),
      q75 = quantile(.data[[qc_col]], 0.75, na.rm = TRUE),
      q95 = quantile(.data[[qc_col]], 0.95, na.rm = TRUE),
      share_ge_050 = mean(.data[[qc_col]] >= 0.50, na.rm = TRUE),
      share_ge_075 = mean(.data[[qc_col]] >= 0.75, na.rm = TRUE),
      share_eq_1   = mean(.data[[qc_col]] == 1, na.rm = TRUE),
      .groups = "drop"
    ) |> mutate(group_type = "hub")
  )
}

all_qc_rows <- list()

for (res_name in names(resolutions)) {
  tbl <- resolutions[[res_name]]
  for (var_name in names(qc_cols)) {
    col <- qc_cols[[var_name]]
    cols_present <- dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 0", tbl)) |> names()
    if (!col %in% cols_present) {
      msg(res_name, "/", var_name, ": column ", col, " not present, skipping.")
      next
    }
    d <- dbGetQuery(con, sprintf(
      "SELECT site_id, %s FROM %s WHERE dataset = 'FLUXMET' AND %s IS NOT NULL",
      col, tbl, col
    )) |> as_tibble() |> left_join(site_lookup, by = "site_id")

    if (nrow(d) == 0L) {
      msg(res_name, "/", var_name, ": no non-NA rows, skipping.")
      next
    }

    stats <- summarise_qc(d, col) |>
      mutate(resolution = res_name, variable = var_name, .before = 1)
    all_qc_rows[[paste(res_name, var_name)]] <- stats
    msg(res_name, "/", var_name, ": n=", nrow(d), " site-periods, ",
        "median QC=", round(stats$q50[stats$group_type == "overall"], 3))
  }
}

qc_distribution <- bind_rows(all_qc_rows) |>
  select(resolution, variable, group_type, group_value, n, q05, q25, q50, q75, q95,
         share_ge_050, share_ge_075, share_eq_1)

write_csv(qc_distribution, file.path(OUT_DIR, "table_stage1_qc_distribution.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage1_qc_distribution.csv"),
  input_sources = db_path,
  notes = paste0(
    "QC flag = fraction of underlying records measured or good-quality gap-filled ",
    "(DD/WW/MM/YY resolution; see CLAUDE.md QC Flag Reference System 1). This fraction ",
    "cannot separate 'measured' from 'well gap-filled' -- both count toward it equally. ",
    "weekly resolution has only 1 site (US-MMS) in the current DuckDB store, not a ",
    "network-representative sample (see Stage 0 / Stage 1 report notes). group_type: ",
    "overall (ALL FLUXMET site-periods with a non-NA QC value), igbp (by manifest IGBP ",
    "class), hub (by manifest data_hub). n = count of non-NA QC values in that group."
  )
)

dbDisconnect(con, shutdown = TRUE)

## ---------------------------------------------------------------------------
## Sub-daily: true measured/good/medium/poor split from the 31 sites with
## HH/HR files extracted (Stage 0's table_stage0_hh_hr_sites.csv).
## ---------------------------------------------------------------------------
hh_hr_sites <- read_csv(file.path(OUT_DIR, "table_stage0_hh_hr_sites.csv"), show_col_types = FALSE)
msg("Sub-daily: ", n_distinct(hh_hr_sites$site_id), " sites, ", nrow(hh_hr_sites), " files.")

subdaily_cols <- unname(qc_cols)
read_subdaily_qc <- function(path, site_id) {
  tryCatch({
    hdr <- names(read_csv(path, n_max = 0, show_col_types = FALSE))
    use_cols <- intersect(subdaily_cols, hdr)
    if (length(use_cols) == 0L) return(NULL)
    d <- read_csv(path, col_select = all_of(use_cols), show_col_types = FALSE, progress = FALSE)
    d$site_id <- site_id
    d
  }, error = function(e) {
    msg("  failed to read ", path, ": ", conditionMessage(e))
    NULL
  })
}

subdaily_data <- map2(hh_hr_sites$path, hh_hr_sites$site_id, read_subdaily_qc) |>
  compact() |> bind_rows()

msg("Sub-daily rows read: ", nrow(subdaily_data))

flag_labels <- c("0" = "measured", "1" = "good_gapfill", "2" = "medium_gapfill", "3" = "poor_gapfill")

subdaily_long <- subdaily_data |>
  select(site_id, any_of(subdaily_cols)) |>
  pivot_longer(cols = -site_id, names_to = "column", values_to = "flag") |>
  filter(!is.na(flag), flag %in% 0:3) |>
  mutate(
    variable = case_when(
      column == "NEE_VUT_REF_QC" ~ "NEE_VUT",
      column == "NEE_CUT_REF_QC" ~ "NEE_CUT",
      column == "LE_F_MDS_QC"    ~ "LE",
      column == "H_F_MDS_QC"     ~ "H"
    ),
    flag_label = flag_labels[as.character(flag)]
  )

subdaily_site_summary <- subdaily_long |>
  count(site_id, variable, flag_label, name = "n") |>
  pivot_wider(names_from = flag_label, values_from = n, values_fill = 0) |>
  mutate(n_total = measured + good_gapfill + medium_gapfill + poor_gapfill)

write_csv(subdaily_site_summary, file.path(OUT_DIR, "table_stage1_subdaily_qc_by_site.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage1_subdaily_qc_by_site.csv"),
  input_sources = "data/extracted/*_FLUXMET_HH_*.csv, *_FLUXMET_HR_*.csv (31 sites)",
  notes = "True measured(0)/good(1)/medium(2)/poor(3) gap-fill split per site per variable, read directly from extracted sub-daily FLUXMET CSVs (QC columns only)."
)

subdaily_network_summary <- subdaily_long |>
  count(variable, flag_label, name = "n") |>
  group_by(variable) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  pivot_wider(id_cols = variable, names_from = flag_label, values_from = share, values_fill = 0)

write_csv(subdaily_network_summary, file.path(OUT_DIR, "table_stage1_subdaily_qc_network_summary.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage1_subdaily_qc_network_summary.csv"),
  input_sources = "table_stage1_subdaily_qc_by_site.csv (derived)",
  notes = "Share of all sub-daily records (across the 31 sites, pooled) in each measured/good/medium/poor gap-fill category, per variable."
)

## Representativeness of the 31-site subset vs the 781-site network, by IGBP/hub.
subdaily_sites_meta <- tibble(site_id = unique(hh_hr_sites$site_id)) |>
  left_join(site_lookup, by = "site_id")

network_igbp <- site_lookup |> count(igbp, name = "n_network") |> mutate(share_network = n_network / sum(n_network))
subdaily_igbp <- subdaily_sites_meta |> count(igbp, name = "n_subdaily") |> mutate(share_subdaily = n_subdaily / sum(n_subdaily))
igbp_compare <- full_join(network_igbp, subdaily_igbp, by = "igbp") |>
  mutate(across(c(n_network, n_subdaily), ~ coalesce(.x, 0L)),
         across(c(share_network, share_subdaily), ~ coalesce(.x, 0))) |>
  arrange(desc(n_network))

network_hub <- site_lookup |> count(data_hub, name = "n_network") |> mutate(share_network = n_network / sum(n_network))
subdaily_hub <- subdaily_sites_meta |> count(data_hub, name = "n_subdaily") |> mutate(share_subdaily = n_subdaily / sum(n_subdaily))
hub_compare <- full_join(network_hub, subdaily_hub, by = "data_hub") |>
  mutate(across(c(n_network, n_subdaily), ~ coalesce(.x, 0L)),
         across(c(share_network, share_subdaily), ~ coalesce(.x, 0))) |>
  arrange(desc(n_network))

write_csv(igbp_compare, file.path(OUT_DIR, "table_stage1_subdaily_vs_network_igbp.csv"))
write_meta(file.path(OUT_DIR, "table_stage1_subdaily_vs_network_igbp.csv"),
           input_sources = c(db_path, "table_stage0_hh_hr_sites.csv"),
           notes = "IGBP-class composition of the 31 sub-daily-extracted sites vs the full 781-site network.")
write_csv(hub_compare, file.path(OUT_DIR, "table_stage1_subdaily_vs_network_hub.csv"))
write_meta(file.path(OUT_DIR, "table_stage1_subdaily_vs_network_hub.csv"),
           input_sources = c(db_path, "table_stage0_hh_hr_sites.csv"),
           notes = "Hub (data_hub) composition of the 31 sub-daily-extracted sites vs the full 781-site network.")

## ---------------------------------------------------------------------------
## Figures
## ---------------------------------------------------------------------------
fig_qc <- qc_distribution |>
  filter(group_type == "overall") |>
  mutate(resolution = factor(resolution, levels = c("daily", "weekly", "monthly", "annual")))

p1 <- ggplot(fig_qc, aes(x = resolution, y = q50, color = variable, group = variable)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.15, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = c(0.5, 0.75), linetype = "dashed", color = "grey50") +
  labs(title = "QC flag distribution by resolution and variable (overall)",
       subtitle = "Points = median; bars = IQR (25th-75th pctile).\nDashed lines: QC_THRESHOLD=0.50 (paper default) and 0.75 (stricter alternative).",
       x = "Temporal resolution", y = "QC flag (fraction measured or well gap-filled)",
       color = "Variable") +
  theme_bw()
ggsave(file.path(OUT_DIR, "fig_stage1_qc_flag_distribution.png"), p1, width = 9, height = 5.5, dpi = 150, bg = "white")

p2 <- subdaily_network_summary |>
  pivot_longer(cols = -variable, names_to = "flag_label", values_to = "share") |>
  mutate(flag_label = factor(flag_label, levels = c("measured", "good_gapfill", "medium_gapfill", "poor_gapfill"))) |>
  ggplot(aes(x = variable, y = share, fill = flag_label)) +
  geom_col() +
  labs(title = "True measured/gap-fill split, 31 sites with HH/HR files extracted",
       x = "Variable", y = "Share of sub-daily records", fill = "Flag") +
  theme_bw()
ggsave(file.path(OUT_DIR, "fig_stage1_subdaily_qc_split.png"), p2, width = 7, height = 5, dpi = 150, bg = "white")

msg("Stage 1 complete.")

## ---------------------------------------------------------------------------
## Status + report
## ---------------------------------------------------------------------------
status_path <- file.path(OUT_DIR, "status.md")
report_path <- file.path(OUT_DIR, "report.md")

overall_median_by_res_var <- qc_distribution |>
  filter(group_type == "overall") |>
  transmute(resolution, variable, q50 = round(q50, 3), share_ge_050 = round(share_ge_050, 3),
            share_ge_075 = round(share_ge_075, 3), share_eq_1 = round(share_eq_1, 3), n)

status_entry <- c(
  "",
  "## Stage 1 -- Gaps",
  "",
  paste0("Completed ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "."),
  "",
  paste0("- QC flag distribution computed for NEE_VUT, NEE_CUT, LE, H at daily/weekly/monthly/annual, ",
         "overall + by IGBP + by hub (data_hub). ", nrow(qc_distribution), " group rows written."),
  "- weekly resolution: only 1 site (US-MMS) in the DuckDB store -- not network-representative; flagged in report.md.",
  paste0("- Sub-daily QC split read from 31 sites' extracted HH/HR CSVs (", nrow(subdaily_data),
         " sub-daily records pooled); compared against network IGBP/hub composition."),
  "",
  "Outputs: table_stage1_qc_distribution.csv, table_stage1_subdaily_qc_by_site.csv, ",
  "table_stage1_subdaily_qc_network_summary.csv, table_stage1_subdaily_vs_network_igbp.csv, ",
  "table_stage1_subdaily_vs_network_hub.csv, fig_stage1_qc_flag_distribution.png, ",
  "fig_stage1_subdaily_qc_split.png (+ .meta.json each).",
  ""
)
write_lines(status_entry, status_path, append = TRUE)

cat("\n--- dq_stage1_gaps.R: done. See ", LOG_FILE, " ---\n")
print(overall_median_by_res_var)
print(subdaily_network_summary)
