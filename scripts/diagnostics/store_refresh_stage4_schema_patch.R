## store_refresh_stage4_schema_patch.R
##
## WHY THIS SCRIPT EXISTS (decision recorded): scripts/duckdb_update.R's own
## top-of-file comment already documents this exact failure mode as a known
## FIXME -- "this will fail if new CSVs have columns not already in the
## database. There needs to be some step that updates the schemas when
## additional columns are added." Stage 4's first run hit exactly that:
## `INSERT OR REPLACE INTO hourly BY NAME` failed with "Referenced update
## column PPFD_OUT not found in table" -- new HH-resolution sites extracted
## today (deeper soil sensor profiles: TS/SWC_F_MDS_{2..6} and their _QC
## columns, plus PPFD_OUT) have columns the `hourly` table's schema, built
## from an earlier and smaller HH site set, does not have.
##
## Per this run's "take the option that changes least": rather than editing
## duckdb_update.R (forbidden -- it is the script 03b_create_database.R
## sources, a pipeline script), this adds the 19 missing columns to the
## EXISTING `hourly` table via ALTER TABLE ... ADD COLUMN, typed DOUBLE to
## match the same column names' type in the `daily` table (confirmed
## identical for all 19). This changes the database schema only, not any
## script's logic, and is the documented gap that FIXME comment predicted
## -- not something this refresh introduced. duckdb_update.R is then
## re-sourced unmodified; annual/monthly/weekly/daily upserts (which
## already succeeded before hourly failed) simply re-run harmlessly
## (INSERT OR REPLACE is idempotent).

if (file.exists(".env")) { library(dotenv); dotenv::load_dot_env() }
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({ library(duckdb); library(DBI); library(readr) })

con <- dbConnect(duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = FALSE)

db_cols <- dbGetQuery(con, "PRAGMA table_info(hourly)")$name
daily_types <- dbGetQuery(con, "PRAGMA table_info(daily)")

f <- "data/extracted/ICOS_IT-MBo_FLUXNET_2003-2025_v1.3_r1/ICOS_IT-MBo_FLUXNET_FLUXMET_HH_2003-2025_v1.3_r1.csv"
csv_cols <- names(readr::read_csv(f, n_max = 0, show_col_types = FALSE))
missing <- setdiff(csv_cols, db_cols)

message("Missing columns in `hourly` table: ", length(missing))
for (col in missing) {
  ty <- daily_types$type[daily_types$name == col]
  ty <- if (length(ty) == 1) ty else "DOUBLE"
  sql <- sprintf('ALTER TABLE hourly ADD COLUMN "%s" %s', col, ty)
  message("  ", sql)
  dbExecute(con, sql)
}

dbDisconnect(con, shutdown = TRUE)
message("Schema patch complete: ", length(missing), " column(s) added to `hourly`.")
