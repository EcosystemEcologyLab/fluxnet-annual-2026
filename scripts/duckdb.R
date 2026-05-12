library(duckdb)
library(fs)
library(glue)

files_daily <- dir_ls("data/extracted", recurse = TRUE, regexp = "(ERA5|FLUXMET)_DD.+\\.csv$")
# fs::file_size(files_daily) |> sum()

files_str <- glue_collapse(glue("'{files_daily}'"), sep = ", ")

# dir_create("data/duckdb")
con <- dbConnect(duckdb(), dbdir = "data/duckdb/fluxnet.duckdb")


dbExecute(
  con,
  glue::glue(
    "
    CREATE TABLE daily AS
    SELECT *,
      split_part(parse_filename(path), '_', 1) as data_hub,
      split_part(parse_filename(path), '_', 2) as site_id,
      split_part(parse_filename(path), '_', 4) as dataset,
      split_part(parse_filename(path), '_', 5) as time_resolution,
    FROM 
      read_csv(
        [{files_str}],
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
dbDisconnect(con)

library(dplyr)
con <- dbConnect(duckdb(), dbdir = "data/duckdb/fluxnet.duckdb")
# tbl(con, "daily") |> collect() # overflows memory

subset <- tbl(con, "daily") |>
  rename(DATE = TIMESTAMP) |> 
  filter(DATE > as.Date("2020-01-01")) |> 
  select(DATE, site_id, NEE_VUT_REF, NEE_VUT_REF_QC) |> 
  collect()

subset

# TODO:
# - Test joining in metadata in a tibble (from flux_listall())
# - Add tables for yearly, monthly, weekly, hourly/half-hourly data
# - Consider writing out to hive-partitioned parquet files instead of .duckdb file
# - Figure out what to do when new data needs to be added (might be easier with parquet files)

