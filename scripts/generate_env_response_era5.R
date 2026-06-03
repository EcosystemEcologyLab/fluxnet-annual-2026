## generate_env_response_era5.R
## Generate fig_environmental_response_era5 using site-year observations.
## Output: review/figures/climate/fig_environmental_response_era5.png

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()
source("R/plot_constants.R")
source("R/figures/fig_environmental_response.R")

library(patchwork)
library(dplyr)
library(duckdb)
library(readr)

# ---- Load data from DuckDB ---------------------------------------------------
# DuckDB port (second of five generate_*.R migrations).
# Pattern: open read-only connection, filter + select only needed columns,
# collect, close. The rest of the script is unchanged.
#
# annual_converted has TIMESTAMP (integer year) not YEAR — schema-translation
# patch applied post-collect, same as scripts/07_figures.R.
# QC (incl. CUT fallback) already applied in annual_converted (commit ad7464f).
# ERA5 columns (TA_ERA, P_ERA, VPD_ERA) confirmed present in annual_converted.
db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
if (!file.exists(db_path)) {
  stop("DuckDB database not found: ", db_path,
       "\nRun 03b_create_database.R → 04_qc.R → 05_units.R first.")
}
con <- dbConnect(duckdb(), db_path, read_only = TRUE)
if (!"annual_converted" %in% dbListTables(con)) {
  dbDisconnect(con)
  stop("annual_converted table missing. Run 04_qc.R → 05_units.R first.")
}

data_yy <- dplyr::tbl(con, "annual_converted") |>
  dplyr::filter(dataset == "FLUXMET") |>
  dplyr::select(site_id, TIMESTAMP,
                NEE_VUT_REF, LE_F_MDS, H_F_MDS,
                TA_ERA, P_ERA, VPD_ERA) |>
  dplyr::collect() |>
  dplyr::mutate(YEAR = as.integer(TIMESTAMP))  # schema-translation patch

dbDisconnect(con)
message("Collected annual_converted (FLUXMET): ",
        format(nrow(data_yy), big.mark = ","), " rows")

snap_file <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)[[1]]
metadata <- readr::read_csv(snap_file, show_col_types = FALSE)

site_aridity <- readr::read_csv(
  file.path(FLUXNET_DATA_ROOT, "snapshots", "site_aridity.csv"),
  show_col_types = FALSE
)

message("Building fig_environmental_response_era5 ...")
p <- fig_environmental_response_era5(data_yy, metadata = metadata,
                                     aridity_data = site_aridity)

out_dir <- file.path("review", "figures", "climate")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "fig_environmental_response_era5.png")

# 4 columns (TA, P, VPD, AI) × 3 rows (NEE, LE, H)
ggplot2::ggsave(out_path, plot = p, width = 18, height = 12,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_path)
