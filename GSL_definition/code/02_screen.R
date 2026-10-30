## 02_screen.R — GSL figure rebuild, framework run-order step 3.
##
## For the 62 sites' own site-years: a screening table (days present, NA QC,
## share passing 0.25/0.50, all-days-pass flags, negative-GPP counts).
## Year-screening option counts (sites/site-years kept under each option in
## the framework's open decision). And a network-wide reproduction of Tash's
## screening rule, compared site-by-site and year-by-year against the
## framework's 62 sites / 505 site-years. See ../docs/framework.md,
## "QA/QC and data screening" and run-order step 3.
##
## Does NOT compute GSL or any other derived quantity (framework step 4,
## Stage 2) -- only screening counts.

source("GSL_definition/code/00_config.R")

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(tidyr); library(readr); library(lubridate)
})

daily_extract <- readRDS(file.path(GSL_ROOT, "data", "daily_extract.rds"))
daily_extract$TIMESTAMP <- as.Date(as.character(daily_extract$TIMESTAMP))
daily_extract$year <- lubridate::year(daily_extract$TIMESTAMP)

## Per-site carbon source (VUT/CUT/ungated), same rule as 04_qc.R --
## confirmed in 01_extract_sites.R that all 62 are VUT, recomputed here so
## this script is self-contained.
carbon_src <- daily_extract |>
  group_by(site_id) |>
  summarise(
    n_vut = sum(!is.na(NEE_VUT_REF_QC)), n_cut = sum(!is.na(NEE_CUT_REF_QC)),
    .groups = "drop"
  ) |>
  mutate(carbon_src = case_when(n_vut > 0L ~ "VUT", n_cut > 0L ~ "CUT", TRUE ~ NA_character_)) |>
  select(site_id, carbon_src)

daily_extract <- daily_extract |>
  left_join(carbon_src, by = "site_id") |>
  mutate(qc_val = case_when(
    carbon_src == "VUT" ~ NEE_VUT_REF_QC,
    carbon_src == "CUT" ~ NEE_CUT_REF_QC,
    TRUE ~ NA_real_
  ))

## ============================================================================
## Part A: tables/site_year_screening.csv -- one row per the 62 sites' own
## site-years (505 rows), per framework.md's Sites table.
## ============================================================================

site_years_wanted <- GSL_SITES |>
  tidyr::unnest(years) |>
  rename(year = years) |>
  select(site_id, year)

screen_one <- function(sid, yr) {
  d <- daily_extract |> filter(site_id == sid, year == yr)
  days_present <- nrow(d)
  days_na_qc <- sum(is.na(d$qc_val))

  # qc_passes() returns NA for NA input; summing with na.rm = TRUE counts
  # only TRUE values, i.e. NA days are correctly excluded from the passing
  # count (not counted as passing) without being counted as failing twice.
  pass_025 <- qc_passes(d$qc_val, 0.25)
  pass_050 <- qc_passes(d$qc_val, QC_THRESHOLD_DD)
  n_pass_025 <- sum(pass_025, na.rm = TRUE)
  n_pass_050 <- sum(pass_050, na.rm = TRUE)

  tibble(
    site_id = sid, year = yr,
    days_present = days_present,
    days_na_qc = days_na_qc,
    share_qc_ge_025 = if (days_present > 0) n_pass_025 / days_present else NA_real_,
    share_qc_ge_050 = if (days_present > 0) n_pass_050 / days_present else NA_real_,
    all_days_pass_025 = days_present > 0 && n_pass_025 == days_present,
    all_days_pass_050 = days_present > 0 && n_pass_050 == days_present,
    n_gpp_below_0 = sum(d$GPP_NT_VUT_REF < 0, na.rm = TRUE),
    n_gpp_below_neg1 = sum(d$GPP_NT_VUT_REF < -1, na.rm = TRUE)
  )
}

site_year_screening <- purrr::pmap_dfr(site_years_wanted, function(site_id, year) {
  screen_one(site_id, year)
})

write_csv(site_year_screening, file.path(GSL_ROOT, "tables", "site_year_screening.csv"))

## Log failing-at-project-standard days and NA-QC days, aggregated one row
## per site-year (not one row per day -- see SESSION_LOG.md 2026-10-29, "QC
## unknown follow-up", for why per-record logging at this scale is wrong).
for (i in seq_len(nrow(site_year_screening))) {
  r <- site_year_screening[i, ]
  n_fail_050 <- r$days_present - round(r$share_qc_ge_050 * r$days_present)
  if (n_fail_050 > 0) {
    log_exclusion(
      site_id = r$site_id, variable = "NEE_VUT_REF_QC/NEE_CUT_REF_QC",
      timestamp = as.character(r$year),
      reason = glue::glue("{n_fail_050} of {r$days_present} day(s) in {r$year} below QC_THRESHOLD_DD={QC_THRESHOLD_DD}"),
      threshold = glue::glue("QC_THRESHOLD_DD={QC_THRESHOLD_DD}"),
      excluded_by = "02_screen.R", log_path = GSL_EXCLUSION_LOG
    )
  }
  if (r$days_na_qc > 0) {
    log_unknown(
      record_id = r$site_id,
      reason = glue::glue("{r$days_na_qc} of {r$days_present} day(s) in {r$year} have an NA QC flag"),
      logged_by = "02_screen.R", log_path = GSL_UNKNOWN_LOG
    )
  }
}

message("Saved: ", file.path(GSL_ROOT, "tables", "site_year_screening.csv"),
        " (", nrow(site_year_screening), " site-year rows)")

## ============================================================================
## Part B: tables/year_screening_options.csv -- sites/site-years kept under
## each year-screening option in the framework's open decision. No option is
## chosen here; this is purely a report of counts (framework.md, "Open
## decisions").
## ============================================================================

trend_sites_21 <- GSL_SITES_GE10YR

option_summary <- function(label, keep_vec) {
  kept <- site_year_screening[keep_vec, ]
  sites_kept <- unique(kept$site_id)
  site_year_counts <- kept |> count(site_id, name = "n_years_kept")
  sites_ge10 <- site_year_counts$site_id[site_year_counts$n_years_kept >= 10]
  sites_ge5  <- site_year_counts$site_id[site_year_counts$n_years_kept >= 5]
  trend_sites_dropped <- setdiff(trend_sites_21, sites_ge10)
  tibble(
    option = label,
    sites_kept = length(sites_kept),
    site_years_kept = nrow(kept),
    sites_ge10yr = length(sites_ge10),
    trend_sites_dropped_below_10yr = paste(sort(trend_sites_dropped), collapse = "; "),
    n_trend_sites_dropped = length(trend_sites_dropped),
    sites_ge5yr = length(sites_ge5)
  )
}

year_screening_options <- bind_rows(
  option_summary("(b) every day passes 0.50", site_year_screening$all_days_pass_050),
  option_summary("(d) >=80% of days pass 0.50", site_year_screening$share_qc_ge_050 >= 0.80),
  option_summary("(d) >=70% of days pass 0.50", site_year_screening$share_qc_ge_050 >= 0.70),
  option_summary("(d) >=90% of days pass 0.50", site_year_screening$share_qc_ge_050 >= 0.90),
  option_summary("(a) >=80% of days pass 0.50 (flag level for scaling)",
                  site_year_screening$share_qc_ge_050 >= 0.80)
)

write_csv(year_screening_options, file.path(GSL_ROOT, "tables", "year_screening_options.csv"))
message("Saved: ", file.path(GSL_ROOT, "tables", "year_screening_options.csv"))
print(year_screening_options)

## ============================================================================
## Part C: Tash-rule reproduction across the WHOLE network (not just the 62
## sites) -- framework.md, "QA/QC and data screening": NEE_VUT_REF_QC >= 0.25
## on every day of a year, at least 5 complete years, US-Aud excluded.
## Compared site-by-site and year-by-year against the framework's 62 sites /
## 505 site-years. The rule is NOT tuned to force a match.
## ============================================================================

con <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
network_daily <- dbGetQuery(con, "
  SELECT site_id, TIMESTAMP, NEE_VUT_REF_QC
  FROM daily WHERE dataset = 'FLUXMET'
")
dbDisconnect(con, shutdown = TRUE)

network_daily$TIMESTAMP <- as.Date(as.character(network_daily$TIMESTAMP))
network_daily$year <- lubridate::year(network_daily$TIMESTAMP)

network_year <- network_daily |>
  group_by(site_id, year) |>
  summarise(
    days_present = n(),
    n_pass_025 = sum(qc_passes(NEE_VUT_REF_QC, 0.25), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    all_present_days_pass_025 = days_present > 0 & n_pass_025 == days_present,
    # "Complete year" = every calendar day of the year is present AND all of
    # them pass 0.25 -- a year with missing days is not "complete" even if
    # every day it does have passes. This interpretation is stated here, not
    # tuned, per framework.md's instruction not to force a match.
    expected_days = ifelse(lubridate::leap_year(year), 366L, 365L),
    is_complete_tash_year = all_present_days_pass_025 & (days_present == expected_days)
  )

tash_site_year <- network_year |>
  filter(is_complete_tash_year, site_id != "US-Aud") |>
  group_by(site_id) |>
  mutate(n_complete_years = n()) |>
  ungroup() |>
  filter(n_complete_years >= 5)

tash_sites <- sort(unique(tash_site_year$site_id))
tash_site_years_n <- nrow(tash_site_year)

message(
  "Tash-rule network-wide reproduction: ", length(tash_sites), " site(s), ",
  tash_site_years_n, " site-year(s) (framework target: 62 sites, 505 site-years)."
)

## Site-by-site and year-by-year comparison against the framework's 62/505.
framework_sites <- GSL_SITE_IDS
framework_site_years <- site_years_wanted |> mutate(key = paste(site_id, year))
tash_site_years_keys <- tash_site_year |> mutate(key = paste(site_id, year)) |> pull(key)

sites_added   <- setdiff(tash_sites, framework_sites)
sites_lost    <- setdiff(framework_sites, tash_sites)
years_added   <- setdiff(tash_site_years_keys, framework_site_years$key)
years_lost    <- setdiff(framework_site_years$key, tash_site_years_keys)

tash_reproduction_diff <- bind_rows(
  tibble(type = "site_added", key = sites_added),
  tibble(type = "site_lost", key = sites_lost),
  tibble(type = "site_year_added", key = years_added),
  tibble(type = "site_year_lost", key = years_lost)
)
write_csv(tash_reproduction_diff, file.path(GSL_ROOT, "tables", "tash_rule_reproduction_diff.csv"))

write_meta(
  file.path(GSL_ROOT, "tables", "tash_rule_reproduction_diff.csv"),
  input_sources = c("data/duckdb/fluxnet.duckdb (daily table, dataset = 'FLUXMET', whole network)",
                     "GSL_definition/docs/framework.md (Sites table, 62 sites / 505 site-years target)"),
  notes = paste0(
    "Network-wide reproduction of Tash's screening rule (NEE_VUT_REF_QC >= 0.25 on every ",
    "day of a 'complete' year [all 365/366 calendar days present and passing], >= 5 complete ",
    "years, US-Aud excluded). Reproduced ", length(tash_sites), " sites / ", tash_site_years_n,
    " site-years vs. the framework's 62/505. Not tuned to force a match. 'Complete year' is ",
    "interpreted as full calendar-day coverage (no missing days), not just all delivered days ",
    "passing -- see docs/methods_memo.md."
  )
)
message("Saved: ", file.path(GSL_ROOT, "tables", "tash_rule_reproduction_diff.csv"),
        " (", nrow(tash_reproduction_diff), " row(s): ",
        length(sites_added), " sites added, ", length(sites_lost), " sites lost, ",
        length(years_added), " site-years added, ", length(years_lost), " site-years lost)")

write_meta(
  file.path(GSL_ROOT, "tables", "site_year_screening.csv"),
  input_sources = "GSL_definition/data/daily_extract.rds",
  notes = paste0(
    "Days with an NA QC flag count as not passing at both the 0.25 and 0.50 levels ",
    "(framework.md correction). share_qc_ge_025/050 are computed over days_present ",
    "(NA days in the denominator, not excluded)."
  )
)
write_meta(
  file.path(GSL_ROOT, "tables", "year_screening_options.csv"),
  input_sources = file.path(GSL_ROOT, "tables", "site_year_screening.csv"),
  notes = paste0(
    "No option is chosen here -- counts only, per framework.md's open decision. ",
    "(a) and (d) >=80% give identical sites_kept/site_years_kept because they use the same ",
    "qualifying rule; they differ only in how GSL is later computed (Stage 2), not in which ",
    "site-years qualify."
  )
)

message("--- 02_screen.R: done ---")
