# Generate a DuckDB database to house fluxnet data

# This would be run *instead* of 03_read.R, but everything downstream would need
# to be modified to open a connection to a table instead of reading in a .rds
# file. Generating the table with annual, montly, and daily data takes ~1hr on
# my intel macbook pro with 32GB of RAM.  If additional data is downloaded from
# fluxnet, running this again won't add the data.  You'll need to delete the
# data/duckdb folder and regenerate the database.  In the future, I might be
# able to create some code to add or update data in existing tables.

library(duckdb)
library(fs)
library(glue)
library(purrr)
library(dplyr)
library(fluxnet)

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

# Check what data files are extracted.  Using `flux_discover_files()` for this
# is ideal because it deduplicates and chooses the most recent release for each
# site.
manifest <- flux_discover_files(
  data_dir = path(Sys.getenv("FLUXNET_DATA_ROOT"), "extracted")
)
flux_manifest <- manifest |>
  filter(dataset %in% c("ERA5", "FLUXMET"))

# Treat HR and HH the same
flux_manifest <- flux_manifest |>
  mutate(time_resolution = replace_values(time_resolution, "HR" ~ "HH"))

files_resolutions <- split(flux_manifest$path, flux_manifest$time_resolution)

# Convert vector of file paths to comma separated string for input to SQL
# command
files_strings <- map(files_resolutions, \(x) {
  glue_collapse(glue("'{x}'"), sep = ", ")
})

# TODO: create temporary database for ingesting CSVs but then upsert them into a
# different, permanent database.

# TODO: add cli messages to make more verbose

# Initialize database connection on disk
dir_create("data/duckdb")
con <- dbConnect(duckdb(), dbdir = "data/duckdb/fluxnet.duckdb")

# Ingest CSVs using a series of SQL commands.  `union_by_name = true` is
# necessary since FLUXMET CSVs have different numbers of columns. Site IDs, data
# hub, and dataset (ERA5 or FLUXMET) is extracted from filenames, -9999 is
# converted to NA, and TIMESTAMP is parsed (depending on time interval) on
# ingest.

# Create annual table if annual data exists
if (!is.null(files_strings$YY)) {
  dbExecute(
    con,
    glue(
      "
      CREATE TABLE annual AS
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
        ) 
    "
    )
  )
}

# Create monthly data table if monthly data exists
if (!is.null(files_strings$MM)) {
  dbExecute(
    con,
    glue(
      "
      CREATE TABLE monthly AS
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
        ) 
    "
    )
  )
}

# Create weekly table if weekly data exists
if (!is.null(files_strings$WW)) {
  dbExecute(
    con,
    glue(
      "
      CREATE TABLE weekly AS
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
        ) 
    "
    )
  )
}

# Create daily table if daily data exists
if (!is.null(files_strings$DD)) {
  dbExecute(
    con,
    glue(
      "
      CREATE TABLE daily AS
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
        ) 
    "
    )
  )
}

# Create hourly table if hourly data exists
if (!is.null(files_strings$HH)) {
  dbExecute(
    con,
    glue(
      "
      CREATE TABLE hourly AS
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
        ) 
    "
    )
  )
}

dbDisconnect(con)
