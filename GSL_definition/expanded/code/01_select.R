## 01_select.R — GSL expanded analysis, step 1: apply the chosen selection
## rule (docs/selection_rule_options_2026-10-30.md's recommendation: R2,
## N=14, with the per-site VUT/CUT fallback) and extract the resulting
## sites' daily FLUXMET data.
##
## Reuses the already-computed, committed selection-audit tables
## (GSL_definition/tables/selection_audit_population.csv,
## selection_audit_rule_options_qc_fallback.csv,
## selection_audit_rule_options_site_year.csv) rather than re-screening the
## whole network from scratch -- those tables already contain, per
## population site-year, whether it passes R2 N=14 (pass_r2_g14) and, per
## population site, which QC/GPP column the VUT/CUT fallback resolved to.
## The only new DuckDB read here is the daily FLUXMET pull for the
## resulting ~156-160 candidate sites (read-only).

source("GSL_definition/expanded/code/00_config.R")
suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(tidyr); library(readr); library(lubridate)
})

AUDIT_TABLES <- file.path(GSL_ROOT, "tables") # the existing, committed selection-audit tables

population   <- read_csv(file.path(AUDIT_TABLES, "selection_audit_population.csv"), show_col_types = FALSE)
qc_fallback  <- read_csv(file.path(AUDIT_TABLES, "selection_audit_rule_options_qc_fallback.csv"), show_col_types = FALSE)
site_year_ro <- read_csv(file.path(AUDIT_TABLES, "selection_audit_rule_options_site_year.csv"), show_col_types = FALSE)

## ---- Rule 4 (>=5 passing years) under R2, N=14 -----------------------------
site_passing_years <- site_year_ro |>
  filter(pass_r2_g14) |>
  select(site_id, year)

n_passing_r2g14 <- site_passing_years |> count(site_id, name = "n_passing_years_r2g14")

candidates <- population |>
  select(site_id, igbp, network, location_lat, location_long, first_year, last_year,
         is_long_record, in_gsl62, record_span_years) |>
  left_join(n_passing_r2g14, by = "site_id") |>
  mutate(n_passing_years_r2g14 = coalesce(n_passing_years_r2g14, 0L)) |>
  filter(n_passing_years_r2g14 >= EXP_RULE_MIN_YEARS)

message(nrow(candidates), " candidate site(s) pass rule 4 (>=", EXP_RULE_MIN_YEARS,
        " years) under R2, N=", EXP_RULE_MAX_FAIL_DAYS, " with the VUT/CUT fallback.")
stopifnot(all(GSL_SITE_IDS %in% candidates$site_id)) # the 62 must already be candidates

## ---- Pull full daily FLUXMET records for the candidate sites only
## (read-only; all years, not just passing ones -- derive_one() filters by
## `years` internally, same pattern as the 62-site 01_extract_sites.R). ----
con <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
site_list_sql <- paste(sprintf("'%s'", candidates$site_id), collapse = ", ")
daily_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_CUT_REF, NEE_CUT_REF_QC,
         GPP_NT_VUT_REF, GPP_NT_CUT_REF, GPP_DT_VUT_REF, GPP_DT_CUT_REF, TA_F
  FROM daily
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", site_list_sql))
dbDisconnect(con, shutdown = TRUE)
daily_raw$TIMESTAMP <- as.Date(as.character(daily_raw$TIMESTAMP))
daily_raw$year <- lubridate::year(daily_raw$TIMESTAMP)
message("Pulled ", nrow(daily_raw), " daily FLUXMET row(s) for ", n_distinct(daily_raw$site_id), " candidate site(s).")

## ---- Resolve the per-site VUT/CUT fallback into uniformly-named columns:
## GPP_NT_REF, GPP_DT_REF, NEE_REF (never mixed within a site). TA_F has no
## VUT/CUT distinction (consolidated meteorology) and passes through as-is. --
daily_raw <- daily_raw |>
  left_join(qc_fallback |> select(site_id, qc_col_used, gpp_col_used), by = "site_id") |>
  mutate(
    GPP_NT_REF = ifelse(gpp_col_used == "GPP_NT_VUT_REF", GPP_NT_VUT_REF, GPP_NT_CUT_REF),
    GPP_DT_REF = ifelse(gpp_col_used == "GPP_NT_VUT_REF", GPP_DT_VUT_REF, GPP_DT_CUT_REF),
    NEE_REF    = ifelse(qc_col_used  == "NEE_VUT_REF_QC", NEE_VUT_REF,    NEE_CUT_REF),
    QC_REF     = ifelse(qc_col_used  == "NEE_VUT_REF_QC", NEE_VUT_REF_QC, NEE_CUT_REF_QC)
  )

## ---- Rule 5 (negative GPP) and the "no GPP data at all" drop, both
## computed over the site's FULL daily record (not just R2-passing years) --
## rule 5 is a whole-site rule in the manuscript, not a per-year one. ----
gpp_check <- daily_raw |>
  group_by(site_id) |>
  summarise(
    n_days_gpp_present   = sum(!is.na(GPP_NT_REF)),
    n_days_gpp_below_neg1 = sum(GPP_NT_REF < EXP_RULE_NEGGPP_VAL, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    share_gpp_below_neg1 = ifelse(n_days_gpp_present > 0, n_days_gpp_below_neg1 / n_days_gpp_present, NA_real_),
    fails_rule5   = share_gpp_below_neg1 > EXP_RULE_NEGGPP_SHR,
    no_gpp_data   = n_days_gpp_present == 0L
  )

no_gpp_sites   <- gpp_check$site_id[gpp_check$no_gpp_data]
rule5_fail_sites <- gpp_check$site_id[!is.na(gpp_check$fails_rule5) & gpp_check$fails_rule5]

message(length(no_gpp_sites), " candidate site(s) have NO non-NA GPP values at all -- dropped: ",
        paste(sort(no_gpp_sites), collapse = ", "))
expected_no_gpp <- c("AU-Stp", "ES-LJu", "ID-PaD", "JP-Khw", "JP-Ta2")
if (!setequal(no_gpp_sites, expected_no_gpp)) {
  warning("no-GPP-data site list does not match the prompt's expectation. Expected: ",
          paste(expected_no_gpp, collapse = ", "), "; got: ", paste(sort(no_gpp_sites), collapse = ", "))
}

message(length(rule5_fail_sites), " candidate site(s) fail rule 5 (>", EXP_RULE_NEGGPP_SHR * 100,
        "% of daily GPP < ", EXP_RULE_NEGGPP_VAL, "): ", paste(sort(rule5_fail_sites), collapse = ", "))
stopifnot(!any(GSL_SITE_IDS %in% c(no_gpp_sites, rule5_fail_sites))) # none of the 62 can be dropped here

## ---- Final kept sites ------------------------------------------------------
dropped_sites <- union(no_gpp_sites, rule5_fail_sites)
exp_sites <- candidates |>
  filter(!site_id %in% dropped_sites) |>
  left_join(qc_fallback |> select(site_id, any_vut_qc, any_cut_qc, qc_col_used, gpp_col_used), by = "site_id") |>
  left_join(gpp_check |> select(site_id, n_days_gpp_present, share_gpp_below_neg1), by = "site_id") |>
  mutate(
    group = unname(EXP_IGBP_TO_GROUP[igbp]),
    carbon_src = ifelse(any_vut_qc, "VUT", "CUT")
  ) |>
  select(site_id, igbp, group, network, location_lat, location_long, first_year, last_year,
         record_span_years, n_passing_years_r2g14, carbon_src, qc_col_used, gpp_col_used,
         n_days_gpp_present, share_gpp_below_neg1, is_long_record, in_gsl62)

message(nrow(exp_sites), " site(s) in the final expanded set.")

exp_site_years <- site_passing_years |>
  filter(site_id %in% exp_sites$site_id) |>
  arrange(site_id, year)

## ---- Checks: GSL-62 containment (all 505 site-years), US-Ha1's count ------
gsl62_expanded <- GSL_SITES |> select(site_id, years) |> tidyr::unnest(years) |> rename(year = years)
stopifnot(nrow(gsl62_expanded) == 505L)
gsl62_missing <- gsl62_expanded |> anti_join(exp_site_years, by = c("site_id", "year"))
if (nrow(gsl62_missing) > 0) {
  stop("GSL-62 containment FAILED: ", nrow(gsl62_missing), " site-year(s) missing from the ",
       "expanded set. See exp_site_years / GSL_SITES. First few: ",
       paste(utils::head(paste(gsl62_missing$site_id, gsl62_missing$year), 10), collapse = "; "))
}
message("Confirmed: all 505 GSL-62 site-years are included in the expanded set.")

us_ha1_n <- sum(exp_site_years$site_id == "US-Ha1")
message("US-Ha1: ", us_ha1_n, " passing years in the expanded set.")
if (us_ha1_n != 17L) {
  warning("US-Ha1 was expected to keep 17 years; got ", us_ha1_n, ". Investigate before proceeding.")
}

stopifnot(!any(exp_sites$site_id %in% no_gpp_sites))
stopifnot(all(GSL_SITE_IDS %in% exp_sites$site_id))

## ---- Save the daily extract for kept sites (full record, year-unfiltered,
## same convention as the 62-site 01_extract_sites.R -- derive scripts filter
## by `years` internally). Gitignored (GSL_definition/.gitignore: data/). ----
daily_extract <- daily_raw |>
  filter(site_id %in% exp_sites$site_id) |>
  select(site_id, TIMESTAMP, year, GPP_NT_REF, GPP_DT_REF, NEE_REF, QC_REF, TA_F, qc_col_used, gpp_col_used)
saveRDS(daily_extract, file.path(EXP_DATA, "daily_extract.rds"))
message("Saved: ", file.path(EXP_DATA, "daily_extract.rds"), " (", nrow(daily_extract), " rows, ",
        n_distinct(daily_extract$site_id), " sites)")

## ---- Write tables -----------------------------------------------------------
write_csv(exp_sites, file.path(EXP_TABLES, "sites.csv"))
write_meta(
  file.path(EXP_TABLES, "sites.csv"),
  input_sources = c(file.path(AUDIT_TABLES, "selection_audit_population.csv"),
                     file.path(AUDIT_TABLES, "selection_audit_rule_options_qc_fallback.csv"),
                     file.path(AUDIT_TABLES, "selection_audit_rule_options_site_year.csv"),
                     "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')"),
  notes = paste0(
    "Expanded GSL site list: manuscript rule 1 (IGBP) + per-site VUT/CUT QC fallback (as ",
    "scripts/04_qc.R) + rule 4 (>=5 years) under R2 N=14 (<=14 failing days/year, 0.25 daily ",
    "threshold, NA fails) + rule 5 (<=10% of daily GPP < -1, whole-site) + drop if zero non-NA ",
    "GPP values at all. ", nrow(exp_sites), " sites kept, ", length(no_gpp_sites),
    " dropped for no GPP data (", paste(sort(no_gpp_sites), collapse = ", "), "), ",
    length(rule5_fail_sites), " dropped for rule 5. All 62 GSL sites and all 505 of their ",
    "site-years are included (checked directly)."
  )
)

write_csv(exp_site_years, file.path(EXP_TABLES, "site_years.csv"))
write_meta(
  file.path(EXP_TABLES, "site_years.csv"),
  input_sources = file.path(EXP_TABLES, "sites.csv"),
  notes = paste0(nrow(exp_site_years), " site-years across ", nrow(exp_sites), " sites, passing R2 N=14.")
)

exclusions <- gpp_check |>
  filter(site_id %in% dropped_sites) |>
  mutate(reason = case_when(
    no_gpp_data ~ "no non-NA GPP values at all",
    TRUE ~ sprintf("rule 5: %.2f%% of daily GPP < %d (> %d%% cutoff)",
                    100 * share_gpp_below_neg1, EXP_RULE_NEGGPP_VAL, as.integer(EXP_RULE_NEGGPP_SHR * 100))
  ))
write_csv(exclusions, file.path(EXP_TABLES, "selection_exclusions.csv"))
write_meta(
  file.path(EXP_TABLES, "selection_exclusions.csv"),
  input_sources = file.path(EXP_TABLES, "sites.csv"),
  notes = "Candidate sites (passed rule 1 + rule 4 under R2 N=14) dropped by rule 5 or the no-GPP-data rule, with reason."
)

message("--- 01_select.R: done --- ", nrow(exp_sites), " sites, ", nrow(exp_site_years), " site-years.")
