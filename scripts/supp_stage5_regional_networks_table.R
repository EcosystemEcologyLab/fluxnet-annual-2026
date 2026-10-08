## supp_stage5_regional_networks_table.R
##
## Supplementary Table S1: one row per regional network, from the snapshot
## of record (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv,
## 2026-10-08 supplementary material restructure, SESSION_LOG.md) -- network
## code, network name, processing hub, number of sites, number of
## site-years.
##
## Network code: the first underscore-delimited token of each site's own
## fluxnet_product_name (e.g. "AMF_AR-Bal_FLUXNET_2012-2013_v1.3_r1.zip" ->
## "AMF"). Processing hub: the snapshot's own data_hub field (not inferred
## from the code or from site-ID prefixes -- CLAUDE.md Hard Rule 2).
## Site-years: the paper's own definition, years with any of the twelve
## broad flux variables present in at least one month
## (R/utils.R::compute_site_year_presence()), exactly as Figure 2 uses --
## read from the already-computed data/snapshots/site_year_data_presence.csv
## (confirmed current for this same 781-site network by
## scripts/supp_stage1_record_length_by_igbp.R's own header note), not
## recomputed.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Supplementary Table S1: regional networks ===")

OUT_DIR <- "review/figures/draft_manuscript_v1/SupTables"
fs::dir_create(OUT_DIR)

SNAPSHOT_OF_RECORD <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
PRESENCE_PATH       <- "data/snapshots/site_year_data_presence.csv"

snap     <- read_csv(SNAPSHOT_OF_RECORD, show_col_types = FALSE)
presence <- read_csv(PRESENCE_PATH, show_col_types = FALSE) |>
  mutate(has_data = as.logical(has_data))
msg("Loaded: ", nrow(snap), " sites from ", SNAPSHOT_OF_RECORD)
msg("Loaded: ", nrow(presence), " site-year rows from ", PRESENCE_PATH)

## ---- Network code and name -------------------------------------------------
NETWORK_NAMES <- c(
  AMF   = "AmeriFlux",
  EUF   = "European Fluxes Database",
  ICOS  = "ICOS",
  JPF   = "JapanFlux",
  TERN  = "TERN",
  CNF   = "ChinaFLUX",
  KOF   = "KoFlux",
  FLX   = "not affiliated with a regional network",
  SAEON = "South African Environmental Observation Network"
)

snap <- snap |>
  mutate(network_code = vapply(strsplit(fluxnet_product_name, "_", fixed = TRUE),
                                `[`, character(1), 1))

unknown_codes <- setdiff(unique(snap$network_code), names(NETWORK_NAMES))
if (length(unknown_codes) > 0L) {
  stop("supp_stage5_regional_networks_table.R: product code(s) not in the known list: ",
       paste(unknown_codes, collapse = ", "),
       " -- stopping per task instructions. Add the code/name to NETWORK_NAMES only after ",
       "confirming its correct full name.", call. = FALSE)
}

## ---- Processing hub: snapshot's own data_hub field, per code --------------
## One hub per code is expected (confirmed empirically: every site sharing a
## network code shares the same data_hub); stop rather than silently picking
## one if that ever stops being true.
hub_by_code <- snap |> distinct(network_code, data_hub)
if (anyDuplicated(hub_by_code$network_code) != 0L) {
  dupes <- hub_by_code$network_code[duplicated(hub_by_code$network_code)]
  stop("supp_stage5_regional_networks_table.R: network code(s) span more than one data_hub value: ",
       paste(unique(dupes), collapse = ", "), " -- stopping, table would need a different key.",
       call. = FALSE)
}

## ---- Sites and site-years per code -----------------------------------------
site_counts <- snap |> count(network_code, name = "n_sites")

site_years <- presence |>
  filter(has_data) |>
  left_join(snap |> select(site_id, network_code), by = "site_id") |>
  count(network_code, name = "n_site_years")

table1 <- tibble(network_code = names(NETWORK_NAMES)) |>
  left_join(site_counts, by = "network_code") |>
  left_join(site_years, by = "network_code") |>
  left_join(hub_by_code, by = "network_code") |>
  mutate(
    network_name     = unname(NETWORK_NAMES[network_code]),
    n_sites          = coalesce(n_sites, 0L),
    n_site_years     = coalesce(n_site_years, 0L)
  ) |>
  select(network_code, network_name, processing_hub = data_hub, n_sites, n_site_years) |>
  arrange(match(network_code, names(NETWORK_NAMES)))

print(table1)

## ---- Checks (stop and report if any fails) ---------------------------------
check_sites_total     <- sum(table1$n_sites)
check_site_years_total <- sum(table1$n_site_years)
hub_totals <- table1 |> group_by(processing_hub) |> summarise(n = sum(n_sites), .groups = "drop")
hub_check <- c(AmeriFlux = 381L, ICOS = 348L, TERN = 52L)

checks <- tibble::tribble(
  ~check,                                  ~expected, ~actual,
  "sites sum to 781",                      781L,      check_sites_total,
  "site-years sum to 6200",                6200L,     check_site_years_total,
  "AmeriFlux hub sites = 381",              381L,     hub_totals$n[hub_totals$processing_hub == "AmeriFlux"],
  "ICOS hub sites = 348",                   348L,     hub_totals$n[hub_totals$processing_hub == "ICOS"],
  "TERN hub sites = 52",                    52L,      hub_totals$n[hub_totals$processing_hub == "TERN"]
) |> mutate(ok = expected == actual)
print(checks)

if (!all(checks$ok)) {
  stop("supp_stage5_regional_networks_table.R: one or more checks failed -- stopping per task ",
       "instructions, not writing the table. See printed `checks` table above for details.",
       call. = FALSE)
}
msg("All checks passed: sites=781, site-years=6200, hub totals AmeriFlux=381/ICOS=348/TERN=52.")

## ---- Write -------------------------------------------------------------------
out_path <- file.path(OUT_DIR, "tableS1_regional_networks.csv")
write_csv(table1, out_path)
write_output_metadata(
  out_path,
  input_sources = c(SNAPSHOT_OF_RECORD, PRESENCE_PATH),
  notes = paste0(
    "network_code parsed from each site's own fluxnet_product_name (first underscore-delimited ",
    "token); processing_hub is the snapshot's own data_hub field (CLAUDE.md Hard Rule 2 -- not ",
    "inferred from the code or from site-ID prefixes). n_site_years uses the paper's own ",
    "definition (years with any of the twelve broad flux variables present in at least one ",
    "month, R/utils.R::compute_site_year_presence()), same as Figure 2 -- read from the ",
    "already-computed site_year_data_presence.csv, not recomputed. Checks run and passed: sites ",
    "sum to 781; hub totals AmeriFlux=381, ICOS=348, TERN=52; site-years sum to 6,200."
  )
)
msg("Saved: ", out_path, " (", nrow(table1), " rows)")
msg("=== Done ===")
