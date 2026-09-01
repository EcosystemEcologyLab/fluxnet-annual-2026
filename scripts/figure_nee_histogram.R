## scripts/figure_nee_histogram.R
##
## Simple histogram of annual NEE across all FLUXNET Shuttle sites.
##
## Data source: data/duckdb/fluxnet.duckdb, annual_converted table (Hard Rule
## #1 — Shuttle-derived, QC-filtered, unit-converted; see 03b/04/05 pipeline
## scripts). NEE_VUT_REF is the primary variable; NEE_CUT_REF is used as a
## per-row fallback for CUT-only sites, matching the convention in
## scripts/00_candidate_figures.R (build_s1, "NEE — coalesce VUT and CUT").
##
## Output: review/figures/fig_yy_nee_histogram.png

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

source("R/plot_constants.R")

library(dplyr)
library(duckdb)
library(ggplot2)

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
if (!file.exists(db_path)) {
  stop("DuckDB database not found: ", db_path,
       "\nRun 03b_create_database.R, 04_qc.R, 05_units.R first.")
}
con <- dbConnect(duckdb(), db_path, read_only = TRUE)
if (!"annual_converted" %in% dbListTables(con)) {
  dbDisconnect(con)
  stop("annual_converted table missing from DuckDB — run 03b -> 04 -> 05 first.")
}

data_yy <- tbl(con, "annual_converted") |>
  dplyr::filter(dataset == "FLUXMET") |>
  dplyr::select(site_id, TIMESTAMP, NEE_VUT_REF, NEE_CUT_REF) |>
  collect()

dbDisconnect(con)

data_yy <- data_yy |>
  dplyr::mutate(NEE_ref = dplyr::coalesce(.data$NEE_VUT_REF, .data$NEE_CUT_REF)) |>
  dplyr::filter(!is.na(.data$NEE_ref))

n_site_years <- nrow(data_yy)
n_sites      <- dplyr::n_distinct(data_yy$site_id)
message("Site-years with non-NA annual NEE: ", n_site_years,
        " across ", n_sites, " sites")

p <- ggplot(data_yy, aes(x = .data$NEE_ref)) +
  geom_histogram(bins = 40, fill = "#2c7fb8", colour = "white", linewidth = 0.2) +
  labs(
    title = "Distribution of annual NEE — all FLUXNET Shuttle sites",
    subtitle = paste0(n_site_years, " site-years across ", n_sites, " sites (VUT ref, CUT fallback)"),
    x = "Annual NEE (gC m⁻² yr⁻¹)",
    y = "Count (site-years)"
  ) +
  fluxnet_theme()

out_dir <- "review/figures"
fs::dir_create(out_dir)
out_path <- file.path(out_dir, "fig_yy_nee_histogram.png")
ggplot2::ggsave(out_path, plot = p, width = 9, height = 6,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_path)
