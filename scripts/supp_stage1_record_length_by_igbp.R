## supp_stage1_record_length_by_igbp.R
##
## Unattended supplementary run, Stage 1: record length of the current
## (781-site) network, two independent measures, by IGBP class.
##
## Measure 1 ("years with data"): per site, the count of years with
## has_data == TRUE from compute_site_year_presence() (R/utils.R) -- "any
## of the 12 broad flux vars non-NA in at least one month of that year",
## over that site's own full first_year:last_year window (gap years count
## toward the window but not toward the has_data count). Reuses
## data/snapshots/site_year_data_presence.csv directly rather than
## recomputing: confirmed it is the exact compute_site_year_presence()
## output for the current pinned 781-site network (identical site_id set,
## last refreshed 2026-10-02 by scripts/refresh_site_year_presence.R) --
## recomputing from DuckDB would reproduce the same table at the cost of a
## full monthly-table scan.
##
## Measure 2 ("years with a qualifying annual NEE"): per site, the count of
## site-years in compute_site_annual_fluxes()$site_year (R/site_annual_fluxes.R)
## with a non-NA NEE -- i.e. n_years_nee from that function's own
## site_summary, which applies the paper's QC_THRESHOLD_YY gate and the
## per-site VUT/CUT fallback rule.
##
## Deliberately NOT data/snapshots/site_record_length.csv: that file applies
## a different, stricter definition (QC_THRESHOLD_MM-passing monthly data,
## >=6 months/year) for a different purpose (manuscript "N sites hold >=10
## years" prose) and is flagged stale for this task.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")
source("R/units.R")
source("R/site_annual_fluxes.R")
source("R/plot_constants.R")

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(DBI); library(duckdb)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Stage 1: record length by IGBP class ===")

OUT_DIR <- "review/figures/draft_manuscript_v1/SupTables"
fs::dir_create(OUT_DIR)

CURRENT_SNAPSHOT <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
PRESENCE_PATH    <- "data/snapshots/site_year_data_presence.csv"
DB_PATH          <- file.path(FLUXNET_DATA_ROOT, "duckdb", "fluxnet.duckdb")

current_sites <- read_csv(CURRENT_SNAPSHOT, show_col_types = FALSE) |>
  distinct(site_id, .keep_all = TRUE) |>
  select(site_id, igbp)
n_sites <- nrow(current_sites)
msg("Current network: ", n_sites, " sites")
if (n_sites != 781L) {
  warning("Expected 781 current-network sites, got ", n_sites)
}

## ---- Measure 1: years with data (compute_site_year_presence()) -----------
presence <- read_csv(PRESENCE_PATH, show_col_types = FALSE) |>
  mutate(year = as.integer(year), has_data = as.logical(has_data))
if (!setequal(unique(presence$site_id), current_sites$site_id)) {
  stop("site_year_data_presence.csv site set does not match the pinned current-network ",
       "snapshot -- refresh scripts/refresh_site_year_presence.R before rerunning this stage.")
}

n_years_data <- presence |>
  group_by(site_id) |>
  summarise(
    first_year = min(year), last_year = max(year),
    n_years_with_data = sum(has_data),
    .groups = "drop"
  )
msg("Measure 1 (years with data): computed for ", nrow(n_years_data), " sites, ",
    "range ", min(n_years_data$n_years_with_data), "-", max(n_years_data$n_years_with_data))

## ---- Measure 2: years with a qualifying annual NEE ------------------------
if (!file.exists(DB_PATH)) stop("DuckDB database not found: ", DB_PATH)
con <- dbConnect(duckdb(), dbdir = DB_PATH, read_only = TRUE)
fluxes <- compute_site_annual_fluxes(con, site_ids = current_sites$site_id)
dbDisconnect(con, shutdown = TRUE)

n_years_nee <- fluxes$site_summary |>
  select(site_id, n_years_nee_qualifying = n_years_nee)
msg("Measure 2 (years with qualifying annual NEE): computed for ", nrow(n_years_nee), " sites, ",
    "range ", min(n_years_nee$n_years_nee_qualifying), "-", max(n_years_nee$n_years_nee_qualifying))

## ---- Combine, join IGBP class ---------------------------------------------
site_record <- current_sites |>
  left_join(n_years_data, by = "site_id") |>
  left_join(n_years_nee, by = "site_id") |>
  mutate(igbp = if_else(igbp %in% PAPER_IGBP_ORDER, igbp, NA_character_))

n_nonstandard <- sum(is.na(site_record$igbp))
if (n_nonstandard > 0L) {
  msg("Sites with non-standard/missing IGBP class (excluded from by-class rows, ",
      "still counted in Total): ", n_nonstandard)
}

THRESHOLDS <- c(5L, 10L, 20L)

count_row <- function(df, label) {
  row <- tibble::tibble(igbp_class = label, n_sites = nrow(df))
  for (thr in THRESHOLDS) {
    row[[paste0("data_ge", thr)]] <- sum(df$n_years_with_data >= thr)
    row[[paste0("nee_ge", thr)]]  <- sum(df$n_years_nee_qualifying >= thr)
  }
  row
}

by_class <- lapply(PAPER_IGBP_ORDER, function(cl) {
  count_row(site_record |> filter(igbp == cl), cl)
}) |> bind_rows() |> filter(n_sites > 0L)

total_row <- count_row(site_record, "Total")

table1 <- bind_rows(by_class, total_row)
print(table1, n = Inf, width = Inf)

out_path <- file.path(OUT_DIR, "tableS_record_length_by_igbp.csv")
write_csv(table1, out_path)
write_output_metadata(
  out_path,
  input_sources = c(CURRENT_SNAPSHOT, PRESENCE_PATH, DB_PATH),
  notes = paste0(
    "Current (781-site) network, two record-length measures by IGBP class (PAPER_IGBP_ORDER; ",
    "R/plot_constants.R) and in total. data_ge<N> = sites with >=N years of has_data==TRUE in ",
    "compute_site_year_presence() output (R/utils.R; 'any month present' of 12 broad flux vars, ",
    "over each site's own first_year:last_year window). nee_ge<N> = sites with >=N site-years of ",
    "non-NA NEE in compute_site_annual_fluxes()$site_summary$n_years_nee (R/site_annual_fluxes.R; ",
    "QC_THRESHOLD_YY-gated, per-site VUT/CUT fallback). Deliberately not ",
    "data/snapshots/site_record_length.csv (stale; different, stricter QC_THRESHOLD_MM/>=6-months ",
    "definition for a different purpose).", if (n_nonstandard > 0L)
      paste0(" ", n_nonstandard, " site(s) have a non-standard/missing IGBP class and are excluded ",
             "from the by-class rows (still counted in Total).") else ""
  )
)
msg("Saved: ", out_path)
msg("=== Stage 1 done ===")
