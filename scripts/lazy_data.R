# Open CSVs as dataset instead of loading into memory

library(fs)
library(dplyr)
library(tidyr)
library(glue)
library(arrow)

# Easiest if files are reorganized first
daily_csvs <- dir_ls(
  "data/extracted",
  regexp = "(FLUXMET_DD|ERA5_DD)",
  recurse = TRUE
)

# I'll use hive-style partitioning here where variable name and value are
# encoded in file path. `arrow` can automatically recognize this format.
copy <- tibble(path = daily_csvs) |>
  mutate(file = path_file(path)) |>
  separate_wider_delim(
    file,
    delim = "_",
    names = c(
      "network",
      "site_id",
      "FLUXNET",
      "dataset",
      "resolution",
      "yr_range",
      "oneflux_version",
      "release_version"
    ),
    cols_remove = FALSE
  ) |>
  mutate(
    new_path = path(
      "data/hive/",
      glue("resolution={resolution}"),
      glue("dataset={dataset}"),
      glue("network={network}"),
      glue("site_id={site_id}"),
      file
    )
  ) |>
  select(path, new_path)

dir_create(path_dir(copy$new_path))
fs::file_copy(path = copy$path, new_path = copy$new_path)

# Because the ERA5 and FLUXMET data have a different structure, they have to be
# opened separately and combined.

daily_era5 <- open_csv_dataset(
  "data/hive/resolution=DD/dataset=ERA5/",
  na = c("", "NA", "-9999")
)

daily_fluxmet <- open_csv_dataset(
  "data/hive/resolution=DD/dataset=FLUXMET/",
  na = c("", "NA", "-9999")
)

daily <- c(daily_era5, daily_fluxmet)

# In order to return a query as a tibble, you need to `collect()` the data.
head(daily) |> collect()

# Perform queries and then collect into memory
daily_nee_qc <- daily |>
  mutate(date = lubridate::ymd(TIMESTAMP), .before = TIMESTAMP) |>
  select(date, NEE_VUT_REF, NEE_VUT_REF_QC) |>
  filter(NEE_VUT_REF_QC < 0.1) |>
  collect()

daily_nee_qc
