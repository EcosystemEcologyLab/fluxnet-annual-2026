## store_refresh_stage6_cluster.R — Stage 6 of the 2026-09-20 store refresh.
## Data only: recompute ERA5 annual precipitation (map_era5) against
## WorldClim BIO12 and BADM MAP for all sites on the REFRESHED store, using
## the exact formula era5_precip_units_v2.R established (sum(P_ERA*days)
## per site-year, complete 12-month years only, 1991-2020, mean across
## years) and the exact clustering rule era5_precip_units_v3_partB.R used
## (nearest of candidate factors {1,4,8,24,1000} within 15%, else
## "elsewhere"). No interpretation, no reclassification, no exclusions --
## this is a pure recount against the same bins already established.

if (file.exists(".env")) { library(dotenv); dotenv::load_dot_env() }
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(duckdb); library(DBI); library(lubridate)
})

OUTD <- "review/diagnostics/store_refresh_20260920/stage6"
fs::dir_create(OUTD)
message("=== store_refresh_stage6_cluster.R : ", Sys.time(), " ===")

KG_ERA5_PERIOD <- c(1991L, 2020L)
CANDIDATE_FACTORS <- c(1, 4, 8, 24, 1000)

con <- dbConnect(duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
mo_era5 <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_ERA FROM monthly WHERE dataset = 'ERA5'")
dbDisconnect(con, shutdown = TRUE)

mo_era5 <- mo_era5 |>
  dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP),
                days = lubridate::days_in_month(TIMESTAMP)) |>
  dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2], !is.na(P_ERA))

site_year_era5 <- mo_era5 |> dplyr::group_by(site_id, year) |>
  dplyr::summarise(n_months = dplyr::n(), map_era5_year = sum(P_ERA * days), .groups = "drop") |>
  dplyr::filter(n_months == 12L)

site_era5_after <- site_year_era5 |> dplyr::group_by(site_id) |>
  dplyr::summarise(n_years_era5 = dplyr::n(), map_era5_after = mean(map_era5_year), .groups = "drop")

# Static references (BIO12, BADM) -- unchanged, reused from the existing table.
refs <- readr::read_csv("review/diagnostics/era5_precip_units_v2/table_t2_ratios.csv", show_col_types = FALSE) |>
  dplyr::select(site_id, bio12_mm, badm_map_mm, map_era5_before = map_era5)

d <- dplyr::full_join(site_era5_after, refs, by = "site_id") |>
  dplyr::mutate(
    ratio_to_bio12_after = map_era5_after / bio12_mm,
    ratio_to_badm_after  = map_era5_after / badm_map_mm
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(factor_estimate_after = median(c(ratio_to_bio12_after, ratio_to_badm_after), na.rm = TRUE)) |>
  dplyr::ungroup()

nearest_factor <- function(x, factors = CANDIDATE_FACTORS, tol = 0.15) {
  if (is.na(x) || is.infinite(x)) return(NA_character_)
  dd <- abs(x - factors) / factors
  if (min(dd) <= tol) return(as.character(factors[which.min(dd)]))
  "elsewhere"
}
d$nearest_cluster_after <- vapply(d$factor_estimate_after, nearest_factor, character(1L))

readr::write_csv(d, file.path(OUTD, "table_cluster_after_refresh.csv"))

cluster_summary <- d |> dplyr::count(nearest_cluster_after, name = "n_sites") |>
  dplyr::arrange(dplyr::desc(n_sites))
readr::write_csv(cluster_summary, file.path(OUTD, "table_cluster_summary_after.csv"))

message("Cluster summary (after refresh):")
print(cluster_summary)

# Headline binning per the user's "near 1 / near 4 / elsewhere" phrasing:
# "near 4" combines the 4x and 8x candidate clusters (both non-unity,
# non-elsewhere groupings era5_reference_plots.R treats together as
# "cluster_membership" in its own summary).
headline <- tibble::tibble(
  bin = c("near_1x", "near_4x_or_8x", "elsewhere_or_other_factor", "NA_insufficient_data"),
  n_sites = c(
    sum(d$nearest_cluster_after == "1", na.rm = TRUE),
    sum(d$nearest_cluster_after %in% c("4", "8"), na.rm = TRUE),
    sum(d$nearest_cluster_after %in% c("24", "1000", "elsewhere"), na.rm = TRUE),
    sum(is.na(d$nearest_cluster_after))
  )
)
readr::write_csv(headline, file.path(OUTD, "table_cluster_headline.csv"))
message("Headline (near 1 / near 4-8 / elsewhere):")
print(headline)

writeLines("STAGE6_COMPLETE", file.path(OUTD, "STAGE6_STATUS.txt"))
message("=== Stage 6 complete: ", Sys.time(), " ===")
