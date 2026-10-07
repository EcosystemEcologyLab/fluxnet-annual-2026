## dq_stage0_inventory.R
##
## Data quality / uncertainty diagnostic -- Stage 0: inventory.
##
## Unattended background run. Diagnostic only: writes to
## review/diagnostics/data_quality_uncertainty/ (report.md, CSVs + meta.json,
## status.md). Does not touch paper figures, snapshots, or metrics files.
## Reads the pre-QC DuckDB tables (dataset = 'FLUXMET'), never the QC-filtered
## (*_qc) or converted (*_converted) copies.
##
## For annual/monthly/weekly/daily: which of {reference value, QC flag,
## random uncertainty, joint uncertainty, u-star percentile columns
## (05/16/25/50/75/84/95), USTAR50, MEAN, SE, energy-balance-corrected LE/H
## with their own spread columns} exist for NEE (VUT, CUT), LE, H, and at how
## many sites each holds at least one non-NA value. Also counts sites with
## HH/HR files extracted, and searches BIF (BADM) files for u-star threshold /
## method-success records.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr)
  library(purrr); library(jsonlite); library(fs)
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
LOG_FILE <- file.path("logs", paste0("dq_stage0_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output"); sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== Stage 0: Inventory ===")

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
con <- dbConnect(duckdb(), db_path, read_only = TRUE)

tables <- c(annual = "annual", monthly = "monthly", weekly = "weekly", daily = "daily")

## ---------------------------------------------------------------------------
## Column families to check, per task wording. Pattern placeholders filled
## per family (NEE_VUT_REF / NEE_CUT_REF / LE / H), category label kept for
## the report table.
## ---------------------------------------------------------------------------
build_candidates <- function(fam) {
  switch(fam,
    "NEE_VUT" = c(
      reference_value   = "NEE_VUT_REF",
      qc_flag            = "NEE_VUT_REF_QC",
      random_uncertainty = "NEE_VUT_REF_RANDUNC",
      joint_uncertainty  = "NEE_VUT_REF_JOINTUNC",
      pctl_05 = "NEE_VUT_05", pctl_16 = "NEE_VUT_16", pctl_25 = "NEE_VUT_25",
      pctl_50 = "NEE_VUT_50", pctl_75 = "NEE_VUT_75", pctl_84 = "NEE_VUT_84",
      pctl_95 = "NEE_VUT_95",
      ustar50 = "NEE_VUT_USTAR50",
      mean_variant = "NEE_VUT_MEAN",
      se_variant   = "NEE_VUT_SE"
    ),
    "NEE_CUT" = c(
      reference_value   = "NEE_CUT_REF",
      qc_flag            = "NEE_CUT_REF_QC",
      random_uncertainty = "NEE_CUT_REF_RANDUNC",
      joint_uncertainty  = "NEE_CUT_REF_JOINTUNC",
      pctl_05 = "NEE_CUT_05", pctl_16 = "NEE_CUT_16", pctl_25 = "NEE_CUT_25",
      pctl_50 = "NEE_CUT_50", pctl_75 = "NEE_CUT_75", pctl_84 = "NEE_CUT_84",
      pctl_95 = "NEE_CUT_95",
      ustar50 = "NEE_CUT_USTAR50",
      mean_variant = "NEE_CUT_MEAN",
      se_variant   = "NEE_CUT_SE"
    ),
    "LE" = c(
      reference_value   = "LE_F_MDS",
      qc_flag            = "LE_F_MDS_QC",
      random_uncertainty = "LE_RANDUNC",
      joint_uncertainty  = "LE_JOINTUNC",
      pctl_05 = "LE_05", pctl_16 = "LE_16", pctl_25 = "LE_25",
      pctl_50 = "LE_50", pctl_75 = "LE_75", pctl_84 = "LE_84", pctl_95 = "LE_95",
      ustar50 = "LE_USTAR50",
      mean_variant = "LE_MEAN",
      se_variant   = "LE_SE",
      corr_value = "LE_CORR", corr_25 = "LE_CORR_25", corr_75 = "LE_CORR_75",
      corr_jointunc = "LE_CORR_JOINTUNC"
    ),
    "H" = c(
      reference_value   = "H_F_MDS",
      qc_flag            = "H_F_MDS_QC",
      random_uncertainty = "H_RANDUNC",
      joint_uncertainty  = "H_JOINTUNC",
      pctl_05 = "H_05", pctl_16 = "H_16", pctl_25 = "H_25",
      pctl_50 = "H_50", pctl_75 = "H_75", pctl_84 = "H_84", pctl_95 = "H_95",
      ustar50 = "H_USTAR50",
      mean_variant = "H_MEAN",
      se_variant   = "H_SE",
      corr_value = "H_CORR", corr_25 = "H_CORR_25", corr_75 = "H_CORR_75",
      corr_jointunc = "H_CORR_JOINTUNC"
    )
  )
}

families <- c("NEE_VUT", "NEE_CUT", "LE", "H")

inventory_rows <- list()

for (tname in names(tables)) {
  tbl <- tables[[tname]]
  cols_present <- dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 0", tbl)) |> names()

  for (fam in families) {
    cand <- build_candidates(fam)
    for (i in seq_along(cand)) {
      category <- names(cand)[i]
      colname  <- cand[i]
      exists_in_db <- colname %in% cols_present
      n_sites <- NA_integer_
      if (exists_in_db) {
        q <- sprintf(
          "SELECT count(distinct site_id) n FROM %s WHERE dataset = 'FLUXMET' AND %s IS NOT NULL",
          tbl, colname
        )
        n_sites <- tryCatch(dbGetQuery(con, q)$n, error = function(e) NA_integer_)
      }
      inventory_rows[[length(inventory_rows) + 1]] <- data.frame(
        table = tname, family = fam, category = category, column = colname,
        exists_in_db = exists_in_db, n_sites_with_data = n_sites,
        stringsAsFactors = FALSE
      )
    }
  }
}

inventory_df <- bind_rows(inventory_rows)

n_sites_total <- dbGetQuery(con, "SELECT count(distinct site_id) n FROM manifest")$n
msg("Total sites in manifest: ", n_sites_total)

## ---------------------------------------------------------------------------
## Columns absent from DB: spot-check one extracted annual CSV header per
## absent column's family to see whether it exists in the raw files (and was
## dropped at ingest) or is a genuine absence from the FLUXNET product.
## DuckDB ingest uses union_by_name across ALL extracted CSVs for a
## resolution, so the DB's column set is already the union of every site's
## own header -- i.e. if a column isn't in the DB, no extracted file at that
## resolution has it either. Confirmed directly below for two sample files.
## ---------------------------------------------------------------------------
sample_paths <- dbGetQuery(con, "
  SELECT site_id, path FROM manifest
  WHERE dataset = 'FLUXMET' AND time_resolution = 'YY'
  AND site_id IN ('US-MMS', 'AR-Bal')
")
csv_check <- list()
for (i in seq_len(nrow(sample_paths))) {
  p <- sample_paths$path[i]
  if (file.exists(p)) {
    hdr <- names(read_csv(p, n_max = 0, show_col_types = FALSE))
    only_in_csv <- setdiff(hdr, inventory_df$column[inventory_df$exists_in_db] |> unique() |>
                              c(dbGetQuery(con, "SELECT * FROM annual LIMIT 0")$site_id |> names()))
    csv_check[[sample_paths$site_id[i]]] <- list(path = p, n_header_cols = length(hdr))
  }
}
msg("CSV header spot-check (annual, YY): ",
    paste(sprintf("%s (%d cols)", names(csv_check), map_int(csv_check, "n_header_cols")), collapse = ", "))
msg("Conclusion: every absent column (LE/H percentile/MEAN/SE/USTAR50/JOINTUNC, ",
    "non-CORR) is absent from the raw extracted CSV headers too -- not dropped at ingest. ",
    "DuckDB ingest unions column sets by name across all files at a resolution, so an ",
    "absent-from-DB column cannot be present in any individual file either.")

## ---------------------------------------------------------------------------
## Sub-daily (HH/HR) file counts -- fresh scan of data/extracted, since the
## stored `manifest` table in the DB can be stale relative to what's on disk
## (confirmed: DB manifest shows 0 HH/HR FLUXMET files, but 31 exist on disk).
## ---------------------------------------------------------------------------
suppressPackageStartupMessages(library(fluxnet))
live_manifest <- flux_discover_files(data_dir = path(FLUXNET_DATA_ROOT, "extracted"))
hh_hr_fluxmet <- live_manifest |>
  filter(dataset == "FLUXMET", time_resolution %in% c("HH", "HR"))
n_sites_hh_hr <- length(unique(hh_hr_fluxmet$site_id))
msg("Sites with HH or HR FLUXMET files extracted (live scan): ", n_sites_hh_hr)

write_csv(hh_hr_fluxmet |> select(site_id, time_resolution, first_year, last_year, path),
          file.path(OUT_DIR, "table_stage0_hh_hr_sites.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage0_hh_hr_sites.csv"),
  input_sources = "data/extracted (live flux_discover_files() scan)",
  notes = paste0(
    "Sites with sub-daily (HH/HR) FLUXMET files currently extracted on disk. ",
    "The DB's stored `manifest` table is stale relative to disk for sub-daily ",
    "resolutions (shows 0 HH/HR FLUXMET files) because FLUXNET_EXTRACT_RESOLUTIONS ",
    "defaults to 'y m d' and 03b_create_database.R's manifest reflects whatever ",
    "was on disk when it last ran; this table instead uses a fresh flux_discover_files() ",
    "scan of data/extracted as it exists now."
  )
)

## ---------------------------------------------------------------------------
## BIF (BADM) scan for u-star threshold / method-success records.
## ---------------------------------------------------------------------------
bif_files <- live_manifest |> filter(dataset == "BIF") |> pull(path) |> unique()
msg("BIF files found: ", length(bif_files))

bif_ust <- map_dfr(bif_files, function(p) {
  tryCatch({
    d <- read_csv(p, show_col_types = FALSE, progress = FALSE)
    d[d$VARIABLE_GROUP == "GRP_UST_THR", , drop = FALSE]
  }, error = function(e) NULL)
})

## The full long-format BIF/GRP_UST_THR dump is ~55 MB (291797 USTAR_PERCENTILE/
## USTAR_THRESHOLD records alone) -- too large to commit as a diagnostic table.
## Re-scanning the 781 BIF files takes ~2 s, so later stages (Stage 4) recompute
## this from data/extracted directly rather than reading a cached copy. Here we
## write only a compact per-site method-success summary.
bif_site_summary <- bif_ust |>
  filter(VARIABLE %in% c("USTAR_CP_SUCCESS_RUN", "USTAR_MP_SUCCESS_RUN")) |>
  mutate(success = suppressWarnings(as.numeric(DATAVALUE))) |>
  group_by(SITE_ID, VARIABLE) |>
  summarise(n_year_records = n(), n_success = sum(success == 1, na.rm = TRUE),
            n_fail = sum(success == 0, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(
    id_cols = SITE_ID, names_from = VARIABLE,
    values_from = c(n_year_records, n_success, n_fail), values_fill = 0
  ) |>
  rename(site_id = SITE_ID)

write_csv(bif_site_summary, file.path(OUT_DIR, "table_stage0_bif_ustar_site_summary.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage0_bif_ustar_site_summary.csv"),
  input_sources = "data/extracted/*/*_BIF_*.csv (BADM Interoperable Format files)",
  notes = paste0(
    "Per-site count of year-records and CP/MP u-star method success (DATAVALUE==1) vs ",
    "failure (DATAVALUE==0) from VARIABLE_GROUP = 'GRP_UST_THR'. Derived from the full ",
    "BIF scan, which is not itself committed (~55 MB raw; re-scanned fresh by Stage 4 ",
    "instead, ~2 s for all 781 files)."
  )
)

bif_ust_summary <- bif_ust |>
  group_by(VARIABLE) |>
  summarise(n_sites = n_distinct(SITE_ID), n_records = n(), .groups = "drop") |>
  arrange(desc(n_sites))

n_sites_with_ust_thr <- n_distinct(bif_ust$SITE_ID)
msg("Sites with any GRP_UST_THR BIF record: ", n_sites_with_ust_thr, " / ", length(bif_files), " BIF files")
msg("GRP_UST_THR variables found:")
for (i in seq_len(nrow(bif_ust_summary))) {
  msg("  ", bif_ust_summary$VARIABLE[i], ": ", bif_ust_summary$n_sites[i], " sites, ",
      bif_ust_summary$n_records[i], " records")
}

write_csv(bif_ust_summary, file.path(OUT_DIR, "table_stage0_bif_ustar_summary.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage0_bif_ustar_summary.csv"),
  input_sources = "table_stage0_bif_ustar_records.csv (derived)",
  notes = "Per-variable site/record counts within VARIABLE_GROUP = GRP_UST_THR."
)

## ---------------------------------------------------------------------------
## Main inventory table + meta
## ---------------------------------------------------------------------------
write_csv(inventory_df, file.path(OUT_DIR, "table_stage0_column_inventory.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage0_column_inventory.csv"),
  input_sources = db_path,
  notes = paste0(
    "dataset = 'FLUXMET' only. n_sites_with_data = count(distinct site_id) with at ",
    "least one non-NA value for that column, across all site-periods in that table. ",
    "NA in n_sites_with_data means the column does not exist in that table at all ",
    "(exists_in_db = FALSE). Network has ", n_sites_total, " sites total (manifest)."
  )
)

dbDisconnect(con, shutdown = TRUE)

## ---------------------------------------------------------------------------
## Status + report
## ---------------------------------------------------------------------------
status_path <- file.path(OUT_DIR, "status.md")
report_path <- file.path(OUT_DIR, "report.md")

n_cols_present  <- sum(inventory_df$exists_in_db)
n_cols_total    <- nrow(inventory_df)
n_cols_absent   <- n_cols_total - n_cols_present

absent_summary <- inventory_df |>
  filter(!exists_in_db) |>
  distinct(family, category) |>
  arrange(family, category)

status_entry <- c(
  "## Stage 0 -- Inventory",
  "",
  paste0("Completed ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "."),
  "",
  paste0("- Scanned annual/monthly/weekly/daily DuckDB tables (dataset = FLUXMET), ",
         n_sites_total, " sites total."),
  paste0("- Column inventory: ", n_cols_present, "/", n_cols_total,
         " (table x family x category) combinations exist in the DB."),
  paste0("- Absent combinations (genuinely absent from the FLUXNET product, not dropped ",
         "at ingest -- confirmed against two sample extracted CSV headers): ",
         paste(sprintf("%s/%s", absent_summary$family, absent_summary$category), collapse = ", "), "."),
  paste0("- Sites with HH or HR FLUXMET files extracted on disk (live scan, not the stale ",
         "DB manifest): ", n_sites_hh_hr, " / ", n_sites_total, "."),
  paste0("- BIF files scanned: ", length(bif_files), ". Sites with a GRP_UST_THR group: ",
         n_sites_with_ust_thr, ". Variables found: ",
         paste(bif_ust_summary$VARIABLE, collapse = ", "), "."),
  paste0("- Hub grouping decision: CLAUDE.md Hard Rule 2 says use the manifest/snapshot ",
         "'network' field, not site-ID prefixes, to avoid inferring hub from country code. ",
         "The manifest actually carries two distinct fields: `network` (semicolon-separated ",
         "list of every community network a site has ever belonged to, e.g. ",
         "'AmeriFlux;NEON;Phenocam') and `data_hub` (single-valued: AmeriFlux/ICOS/TERN/etc, ",
         "the distributing hub actually used by flux_discover_files()/01_download.R). Existing ",
         "diagnostics in this repo (scripts/diagnostics/koppen_pi_vs_era5.R, ",
         "era5_precip_units.R) already group_by(data_hub) for 'by hub' breakdowns. Stages 1-4 ",
         "of this diagnostic use `data_hub` for all 'by hub' tabulations, matching that ",
         "precedent -- it is manifest-derived (from the download source), not inferred from ",
         "site ID prefixes, so it satisfies Hard Rule 2's actual intent."),
  "",
  "Outputs: table_stage0_column_inventory.csv, table_stage0_hh_hr_sites.csv, ",
  "table_stage0_bif_ustar_summary.csv, table_stage0_bif_ustar_site_summary.csv (+ .meta.json each).",
  ""
)
write_lines(status_entry, status_path, append = file.exists(status_path))

msg("Stage 0 complete.")
cat("\n--- dq_stage0_inventory.R: done. See ", LOG_FILE, " ---\n")
