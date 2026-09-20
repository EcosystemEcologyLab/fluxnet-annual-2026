## store_refresh_stage4_duckdb_no_hourly.R
##
## WHY THIS SCRIPT EXISTS (decision recorded): scripts/duckdb_update.R's
## `hourly` block has now failed twice for two DIFFERENT reasons on this
## 16 GB RAM machine: (1) a schema mismatch (fixed, see
## store_refresh_stage4_schema_patch.R), and (2) after that fix, a genuine
## resource exhaustion -- "Out of Memory Error: failed to offload data
## block ... (92.6 GiB/92.6 GiB used). This limit was set by
## 'max_temp_directory_size'" -- DuckDB's temp-spill directory consumed
## essentially all free disk space while upserting the now-272-column
## `hourly` table across 30 HH + 1 HR site (a wide, half-hourly-resolution
## table). annual/monthly/weekly/daily have succeeded cleanly on every
## attempt; only hourly fails.
##
## This is not in scope for this refresh regardless: FLUXNET_EXTRACT_
## RESOLUTIONS is "y m d" (CLAUDE.md, R/pipeline_config.R) -- the HH/HR
## files currently on disk are incidental leftovers from unrelated
## same-day diagnostics (it_mbo_bug_hunt, it_mbo_parsimony,
## cluster_resolution_sample, store_audit), not part of this store
## refresh's actual target. None of stage 4's downstream scripts consume
## the `hourly` table: 04_qc.R/05_units.R operate on the DD/WW/MM/YY
## thresholds documented in CLAUDE.md (QC_THRESHOLD_DD/WW/MM/YY -- no HH
## threshold exists), and 07_figures.R's documented figures are DD-and-
## coarser. Per "take the option that changes least": rather than editing
## duckdb_update.R (forbidden), this script reproduces its exact
## annual/monthly/weekly/daily logic VERBATIM (same SQL, same upsert
## semantics) from a fresh read of the file, and skips only the `hourly`
## block. The final `manifest` table write EXCLUDES HH/HR rows (unlike
## duckdb_update.R's own unconditional full-manifest write), specifically
## so that a future real run of duckdb_update.R still sees the HH/HR
## files as unsynced and retries them -- it does not silently mark
## `hourly` as caught-up when it isn't.

if (file.exists(".env")) { library(dotenv); dotenv::load_dot_env() }
library(duckdb); library(fs); library(glue); library(purrr); library(dplyr); library(fluxnet); library(cli)

con <- dbConnect(duckdb(), dbdir = "data/duckdb/fluxnet.duckdb")
manifest_old <- tbl(con, "manifest") |> collect()
manifest_new_full <- flux_discover_files("data/extracted") |>
  filter(dataset %in% c("ERA5", "FLUXMET")) |>
  mutate(time_resolution = replace_values(time_resolution, "HR" ~ "HH"))

# Everything EXCEPT hourly for the actual data ingest.
manifest_new <- manifest_new_full |> filter(time_resolution != "HH")

to_add <- anti_join(
  manifest_new, manifest_old,
  by = c("site_id", "dataset", "time_resolution", "first_year", "last_year",
         "oneflux_code_version", "release_version", "product_id")
)
message("to_add (excluding HH/HR): ", nrow(to_add), " file(s).")

if (nrow(to_add) > 0) {
  files_resolutions <- split(to_add$path, to_add$time_resolution)
  files_strings <- map(files_resolutions, \(x) glue_collapse(glue("'{x}'"), sep = ", "))

  if (!is.null(files_strings$YY)) {
    cli_progress_step("Reading in annual data CSVs")
    dbExecute(con, glue("
      CREATE OR REPLACE TEMP TABLE annual_ingest AS
      SELECT *, split_part(parse_filename(path), '_', 1) as data_hub,
        split_part(parse_filename(path), '_', 2) as site_id,
        split_part(parse_filename(path), '_', 4) as dataset,
      FROM read_csv([{files_strings$YY}], union_by_name = true, filename = 'path',
        nullstr = ['NA', '-9999'], parallel = true, types = {{'TIMESTAMP': 'INTEGER'}});
      ALTER TABLE annual_ingest ADD PRIMARY KEY (site_id, dataset, TIMESTAMP);
    "))
    cli_progress_step("Updating/Inserting annual data into database")
    dbExecute(con, "INSERT OR REPLACE INTO annual BY NAME (FROM annual_ingest)")
  }

  if (!is.null(files_strings$MM)) {
    cli_progress_step("Reading in monthly data CSVs")
    dbExecute(con, glue("
      CREATE OR REPLACE TEMP TABLE monthly_ingest AS
      SELECT *, split_part(parse_filename(path), '_', 1) as data_hub,
        split_part(parse_filename(path), '_', 2) as site_id,
        split_part(parse_filename(path), '_', 4) as dataset,
      FROM read_csv([{files_strings$MM}], union_by_name = true, filename = 'path',
        nullstr = ['NA', '-9999'], parallel = true, types = {{'TIMESTAMP': 'DATE'}}, dateformat = '%Y%m');
      ALTER TABLE monthly_ingest ADD PRIMARY KEY (site_id, dataset, TIMESTAMP);
    "))
    cli_progress_step("Updating/Inserting monthly data into database")
    dbExecute(con, "INSERT OR REPLACE INTO monthly BY NAME (FROM monthly_ingest)")
  }

  if (!is.null(files_strings$WW)) {
    cli_progress_step("Reading in weekly data CSVs")
    dbExecute(con, glue("
      CREATE OR REPLACE TEMP TABLE weekly_ingest AS
      SELECT *, split_part(parse_filename(path), '_', 1) as data_hub,
        split_part(parse_filename(path), '_', 2) as site_id,
        split_part(parse_filename(path), '_', 4) as dataset,
      FROM read_csv([{files_strings$WW}], union_by_name = true, filename = 'path',
        nullstr = ['NA', '-9999'], parallel = true,
        types = {{'TIMESTAMP_START': 'DATE', 'TIMESTAMP_END': 'DATE'}}, dateformat = '%Y%m%d');
      ALTER TABLE weekly_ingest ADD PRIMARY KEY (site_id, dataset, TIMESTAMP_START);
    "))
    cli_progress_step("Updating/Inserting weekly data into database")
    dbExecute(con, "INSERT OR REPLACE INTO weekly BY NAME (FROM weekly_ingest)")
  }

  if (!is.null(files_strings$DD)) {
    cli_progress_step("Reading in daily data CSVs")
    dbExecute(con, glue("
      CREATE OR REPLACE TEMP TABLE daily_ingest AS
      SELECT *, split_part(parse_filename(path), '_', 1) as data_hub,
        split_part(parse_filename(path), '_', 2) as site_id,
        split_part(parse_filename(path), '_', 4) as dataset,
      FROM read_csv([{files_strings$DD}], union_by_name = true, filename = 'path',
        nullstr = ['NA', '-9999'], parallel = true,
        types = {{'TIMESTAMP': 'DATE'}}, dateformat = '%Y%m%d');
      ALTER TABLE daily_ingest ADD PRIMARY KEY (site_id, dataset, TIMESTAMP);
    "))
    cli_progress_step("Updating/Inserting daily data into database")
    dbExecute(con, "INSERT OR REPLACE INTO daily BY NAME (FROM daily_ingest)")
  }

  # hourly deliberately skipped -- see header.

  cli_progress_step("Updating CSV manifest in database (excluding HH/HR)")
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(manifest_new, tmp)  # manifest_new already excludes HH/HR
  dbExecute(con, glue("
    CREATE OR REPLACE TABLE manifest AS
    SELECT * FROM read_csv('{tmp}', nullstr = ['NA'],
      types = {{'location_lat': 'DOUBLE', 'location_long': 'DOUBLE'}})
  "))
  message("Manifest updated with ", nrow(manifest_new), " non-HH/HR file rows. ",
          sum(manifest_new_full$time_resolution == "HH"), " HH/HR file(s) left unsynced (by design) -- ",
          "a future real run of duckdb_update.R will still see them as pending.")
}
cli_progress_step("Done! Closing connection.")
dbDisconnect(con, shutdown = TRUE)
