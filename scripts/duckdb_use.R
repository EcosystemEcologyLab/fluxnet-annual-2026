# After database is created, in a fresh R session:

library(dplyr)
library(duckdb)
library(connections)

# Enable nicer DuckDB-specific connection pane options
options("duckdb.enable_rstudio_connection_pane" = TRUE)

# Open connection in RStudio/Positron connection pane
con <- connections::connection_open(
  duckdb(),
  dbdir = "data/duckdb/fluxnet.duckdb"
)

# Access a table without loading it into memory
daily <- tbl(con, "daily")
daily

# Perform filtering, subetting, and calculations with dplyr all without pulling
# into memory.
subset <- daily |>
  rename(DATE = TIMESTAMP) |>
  filter(DATE > as.Date("2020-01-01")) |>
  filter(dataset == "FLUXMET") |>
  select(DATE, site_id, NEE_VUT_REF, NEE_VUT_REF_QC) |>
  mutate(NEE_QC_OK = NEE_VUT_REF_QC > 0.5)

subset

# "Materialize" the table into memory with `collect()`

subset |> collect()

# TODO:
# - Test joining in metadata in a tibble (from flux_listall())
