# FIXME: this will fail if new CSVs have columns not already in the database.
# There needs to be some step that updates the schemas when additional columns
# are added.
library(duckdb)
library(fs)
library(glue)
library(purrr)
library(dplyr)
library(fluxnet)
library(cli) # for nicely formatted messages

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

# If database already exists, connect to it, read the manifest recorded at time
# of creation and compare to current manifest.  Read any changed CSVs into
# temporary tables, then upsert into tables using site_id (dataset?) and
# timestamp as composite primary key.

con <- dbConnect(duckdb(), dbdir = "data/duckdb/fluxnet.duckdb")
manifest_old <- tbl(con, "manifest") |> collect()
manifest_new <- flux_discover_files("data/extracted") |>
  filter(dataset %in% c("ERA5", "FLUXMET")) |>
  # Treat HR and HH the same
  mutate(time_resolution = replace_values(time_resolution, "HR" ~ "HH"))

to_add <- anti_join(
  manifest_new,
  manifest_old,
  by = c(
    "site_id",
    "dataset",
    "time_resolution",
    "first_year",
    "last_year",
    "oneflux_code_version",
    "release_version",
    "product_id"
  )
)

if (nrow(to_add) > 0) {
  files_resolutions <- split(to_add$path, to_add$time_resolution)

  # Convert vector of file paths to comma separated string for input to SQL
  # command
  files_strings <- map(files_resolutions, \(x) {
    glue_collapse(glue("'{x}'"), sep = ", ")
  })

  tables <- dbListTables(con)

  # Update annual table if there is new annual data
  if (!is.null(files_strings$YY)) {
    cli_progress_step("Reading in annual data CSVs")
    dbExecute(
      con,
      glue(
        "
      CREATE OR REPLACE TEMP TABLE annual_ingest AS
      SELECT *,
        split_part(parse_filename(path), '_', 1) as data_hub,
        split_part(parse_filename(path), '_', 2) as site_id,
        split_part(parse_filename(path), '_', 4) as dataset,
      FROM 
        read_csv(
          [{files_strings$YY}],
          union_by_name = true,
          filename = 'path',
          nullstr = ['NA', '-9999'],
          parallel = true,
          types = {{'TIMESTAMP': 'INTEGER'}}
        );
      ALTER TABLE annual_ingest
        ADD PRIMARY KEY (site_id, dataset, TIMESTAMP);
    "
      )
    )
    # Upsert (update/insert) by site_id, dataset, and timestamp. 'BY NAME' is
    # needed because the ingest table may have different column names from
    # the database.
    cli_progress_step("Updating/Inserting annual data into database")
    dbExecute(
      con,
      "
      INSERT OR REPLACE INTO annual
      BY NAME (FROM annual_ingest)
      "
    )
  }

  # Update monthly table if there is new monthly data
  if (!is.null(files_strings$MM)) {
    cli_progress_step("Reading in monthly data CSVs")
    dbExecute(
      con,
      glue(
        "
      CREATE OR REPLACE TEMP TABLE monthly_ingest AS
      SELECT *,
        split_part(parse_filename(path), '_', 1) as data_hub,
        split_part(parse_filename(path), '_', 2) as site_id,
        split_part(parse_filename(path), '_', 4) as dataset,
      FROM 
        read_csv(
          [{files_strings$MM}],
          union_by_name = true,
          filename = 'path',
          nullstr = ['NA', '-9999'],
          parallel = true,
          types = {{'TIMESTAMP': 'DATE'}},
          dateformat = '%Y%m'
        );
      ALTER TABLE monthly_ingest
        ADD PRIMARY KEY (site_id, dataset, TIMESTAMP);
    "
      )
    )
    # Upsert (update/insert) by site_id, dataset, and timestamp. 'BY NAME' is
    # needed because the ingest table may have different column names from
    # the database.
    cli_progress_step("Updating/Inserting monthly data into database")
    dbExecute(
      con,
      "
      INSERT OR REPLACE INTO monthly
      BY NAME (FROM monthly_ingest)
      "
    )
  }

  # Update weekly table if there is new weekly data
  if (!is.null(files_strings$WW)) {
    cli_progress_step("Reading in weekly data CSVs")
    dbExecute(
      con,
      glue(
        "
      CREATE OR REPLACE TEMP TABLE weekly_ingest AS
      SELECT *,
        split_part(parse_filename(path), '_', 1) as data_hub,
        split_part(parse_filename(path), '_', 2) as site_id,
        split_part(parse_filename(path), '_', 4) as dataset,
      FROM 
        read_csv(
          [{files_strings$WW}],
          union_by_name = true,
          filename = 'path',
          nullstr = ['NA', '-9999'],
          parallel = true,
          types = {{'TIMESTAMP_START': 'DATE', 'TIMESTAMP_END': 'DATE'}},
          dateformat = '%Y%m%d'
        );
      ALTER TABLE weekly_ingest
        ADD PRIMARY KEY (site_id, dataset, TIMESTAMP_START);
    "
      )
    )
    # Upsert (update/insert) by site_id, dataset, and timestamp. 'BY NAME' is
    # needed because the weekly_ingest table may have different column names from
    # the database.
    cli_progress_step("Updating/Inserting weekly data into database")
    dbExecute(
      con,
      "
      INSERT OR REPLACE INTO weekly
      BY NAME (FROM weekly_ingest)
      "
    )
  }

  # Update daily table if there is new daily data
  if (!is.null(files_strings$DD)) {
    cli_progress_step("Reading in daily data CSVs")
    dbExecute(
      con,
      glue(
        "
      CREATE OR REPLACE TEMP TABLE daily_ingest AS
      SELECT *,
        split_part(parse_filename(path), '_', 1) as data_hub,
        split_part(parse_filename(path), '_', 2) as site_id,
        split_part(parse_filename(path), '_', 4) as dataset,
      FROM 
        read_csv(
          [{files_strings$DD}],
          union_by_name = true,
          filename = 'path',
          nullstr = ['NA', '-9999'],
          parallel = true,
          types = {{'TIMESTAMP': 'DATE'}},
          dateformat = '%Y%m%d'
        );
      ALTER TABLE daily_ingest
        ADD PRIMARY KEY (site_id, dataset, TIMESTAMP);
    "
      )
    )
    # Upsert (update/insert) by site_id, dataset, and timestamp. 'BY NAME' is
    # needed because the ingest table may have different column names from the
    # database.
    cli_progress_step("Updating/Inserting daily data into database")
    dbExecute(
      con,
      "
      INSERT OR REPLACE INTO daily
      BY NAME (FROM daily_ingest)
      "
    )
  }

  # Update hourly/half-hourly table if there is new data
  if (!is.null(files_strings$HH)) {
    cli_progress_step("Reading in hourly/half-hourly data CSVs")
    dbExecute(
      con,
      glue(
        "
      CREATE OR REPLACE TEMP TABLE hourly_ingest AS
      SELECT *,
        split_part(parse_filename(path), '_', 1) as data_hub,
        split_part(parse_filename(path), '_', 2) as site_id,
        split_part(parse_filename(path), '_', 4) as dataset,
      FROM 
        read_csv(
          [{files_strings$HH}],
          union_by_name = true,
          filename = 'path',
          nullstr = ['NA', '-9999'],
          parallel = true,
          types = {{'TIMESTAMP_START': 'DATETIME', 'TIMESTAMP_END': 'DATETIME'}},
          timestampformat = '%Y%m%d%H%M'
        );
      ALTER TABLE hourly_ingest
        ADD PRIMARY KEY (site_id, dataset, TIMESTAMP_START);
    "
      )
    )
    # Upsert (update/insert) by site_id, dataset, and timestamp. 'BY NAME' is
    # needed because the ingest table may have different column names from the
    # database.
    cli_progress_step("Updating/Inserting hourly data into database")
    dbExecute(
      con,
      "
      INSERT OR REPLACE INTO hourly
      BY NAME (FROM hourly_ingest)
      "
    )
  }

  # If all went well, update the manifest

  cli_progress_step("Updating CSV manifest in database")
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(manifest_new, tmp)
  dbExecute(
    con,
    glue(
      "
      CREATE OR REPLACE TABLE manifest AS
      SELECT * FROM read_csv(
        '{tmp}',
        nullstr = ['NA'],
        types = {{'location_lat': 'DOUBLE', 'location_long': 'DOUBLE'}}
      )
      "
    )
  )
}
cli_progress_step("Done! Closing connection.")
dbDisconnect(con)
