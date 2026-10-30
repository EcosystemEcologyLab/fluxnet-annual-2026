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

population      <- read_csv(file.path(AUDIT_TABLES, "selection_audit_population.csv"), show_col_types = FALSE)
qc_fallback_old  <- read_csv(file.path(AUDIT_TABLES, "selection_audit_rule_options_qc_fallback.csv"), show_col_types = FALSE)
site_year_ro_old <- read_csv(file.path(AUDIT_TABLES, "selection_audit_rule_options_site_year.csv"), show_col_types = FALSE)

## ============================================================================
## Item 1 (session 2a): per-site VUT/CUT fallback keyed to GPP availability
## as well as QC. a22e893's rule (selection_audit_rule_options_qc_fallback.csv,
## built by GSL_definition/code/11_selection_audit.R, out of scope to edit)
## chose VUT whenever the site had ANY non-NA VUT NEE QC, regardless of
## whether GPP_NT_VUT_REF had any data at all -- a site with VUT QC but an
## entirely-NA VUT GPP_NT column was then dropped downstream for "no GPP
## data", even when its CUT GPP_NT column was populated. Recomputed fresh
## here (read-only, FLUXMET only) rather than patched onto the old table.
## ============================================================================
pop_avail_sql <- paste(sprintf("'%s'", population$site_id), collapse = ", ")
con <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
pop_avail <- dbGetQuery(con, sprintf("
  SELECT site_id,
         COUNT(NEE_VUT_REF_QC)  AS n_vut_qc,
         COUNT(GPP_NT_VUT_REF)  AS n_vut_gpp,
         COUNT(NEE_CUT_REF_QC)  AS n_cut_qc,
         COUNT(GPP_NT_CUT_REF)  AS n_cut_gpp
  FROM daily
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
  GROUP BY site_id
", pop_avail_sql))
dbDisconnect(con, shutdown = TRUE)

if (EXP_CFG_CUT_FALLBACK_USE_GPP) {
  qc_fallback <- pop_avail |>
    mutate(
      carbon_src = case_when(
        n_vut_qc > 0 & n_vut_gpp > 0 ~ "VUT",
        n_cut_qc > 0 & n_cut_gpp > 0 ~ "CUT",
        TRUE ~ "DROP"
      ),
      qc_col_used  = case_when(carbon_src == "VUT" ~ "NEE_VUT_REF_QC", carbon_src == "CUT" ~ "NEE_CUT_REF_QC", TRUE ~ NA_character_),
      gpp_col_used = case_when(carbon_src == "VUT" ~ "GPP_NT_VUT_REF", carbon_src == "CUT" ~ "GPP_NT_CUT_REF", TRUE ~ NA_character_),
      any_vut_qc = n_vut_qc > 0, any_cut_qc = n_cut_qc > 0
    )
} else {
  ## a22e893 behaviour, for comparison: QC availability alone, never drops.
  qc_fallback <- pop_avail |>
    mutate(
      any_vut_qc = n_vut_qc > 0, any_cut_qc = n_cut_qc > 0,
      carbon_src = ifelse(any_vut_qc, "VUT", "CUT"),
      qc_col_used  = ifelse(any_vut_qc, "NEE_VUT_REF_QC", "NEE_CUT_REF_QC"),
      gpp_col_used = ifelse(any_vut_qc, "GPP_NT_VUT_REF", "GPP_NT_CUT_REF")
    )
}

qc_dropped_sites <- qc_fallback$site_id[qc_fallback$carbon_src == "DROP"]
message(length(qc_dropped_sites), " population site(s) have no usable VUT or CUT (QC+GPP) column at all -- dropped before rule 4: ",
        paste(sort(qc_dropped_sites), collapse = ", "))
stopifnot(!any(GSL_SITE_IDS %in% qc_dropped_sites)) # none of the manuscript's 62 can be dropped here

## ---- Sites whose fallback column differs from a22e893's QC-only choice
## (recovered: previously had a column assigned but it was unusable for GPP;
## or flipped: assigned column changed from VUT to CUT or vice versa). ----
fallback_compare <- qc_fallback |>
  select(site_id, carbon_src, qc_col_used) |>
  left_join(qc_fallback_old |> select(site_id, qc_col_used_old = qc_col_used), by = "site_id") |>
  mutate(changed = carbon_src != "DROP" & qc_col_used != qc_col_used_old)
recovered_or_changed <- fallback_compare |> filter(changed | carbon_src == "DROP")
message(nrow(recovered_or_changed), " site(s) recovered or changed vs a22e893's QC-only fallback: ",
        paste(sort(recovered_or_changed$site_id), collapse = ", "))

## ---- Rule 4 (>=5 passing years) under R2, N=14. For sites whose fallback
## QC column is UNCHANGED from a22e893, reuse the already-computed
## site_year_ro_old$pass_r2_g14 (identical by construction: same column,
## same per-day rule). For sites whose column changed (or that a22e893 had
## assigned no CUT fallback to check), recompute the daily calendar-grid
## pass/fail directly -- a small subset, not the whole population. ----
changed_ids <- fallback_compare$site_id[fallback_compare$changed]
unchanged_site_years <- site_year_ro_old |>
  filter(pass_r2_g14, !site_id %in% changed_ids) |>
  select(site_id, year)

recompute_pass_r2g14 <- function(sid) {
  qc_col <- qc_fallback$qc_col_used[qc_fallback$site_id == sid]
  if (is.na(qc_col)) return(tibble(site_id = character(), year = integer()))
  con2 <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
  d <- dbGetQuery(con2, sprintf("
    SELECT TIMESTAMP, %s AS qc_val FROM daily
    WHERE dataset = 'FLUXMET' AND site_id = '%s'
  ", qc_col, sid))
  dbDisconnect(con2, shutdown = TRUE)
  d$TIMESTAMP <- as.Date(as.character(d$TIMESTAMP))
  d$year <- lubridate::year(d$TIMESTAMP)
  yrs <- population$first_year[population$site_id == sid]:population$last_year[population$site_id == sid]
  purrr::map_dfr(yrs, function(y) {
    grid <- tibble(TIMESTAMP = seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by = "day"))
    g <- grid |> left_join(d |> filter(year == y) |> select(TIMESTAMP, qc_val), by = "TIMESTAMP")
    pass_raw <- qc_passes(g$qc_val, EXP_RULE_QC_DAY)
    n_fail <- sum(ifelse(is.na(pass_raw), TRUE, !pass_raw))
    if (n_fail <= EXP_RULE_MAX_FAIL_DAYS) tibble(site_id = sid, year = y) else tibble(site_id = character(), year = integer())
  })
}
message("Recomputing R2/N=14 pass/fail for ", length(changed_ids), " site(s) with a changed fallback column: ",
        paste(sort(changed_ids), collapse = ", "))
changed_site_years <- purrr::map_dfr(changed_ids, recompute_pass_r2g14)

site_passing_years <- bind_rows(unchanged_site_years, changed_site_years) |> distinct() |> arrange(site_id, year)
n_passing_r2g14 <- site_passing_years |> count(site_id, name = "n_passing_years_r2g14")

candidates <- population |>
  select(site_id, igbp, network, location_lat, location_long, first_year, last_year,
         is_long_record, in_gsl62, record_span_years) |>
  left_join(qc_fallback |> select(site_id, carbon_src), by = "site_id") |>
  filter(carbon_src != "DROP") |>
  left_join(n_passing_r2g14, by = "site_id") |>
  mutate(n_passing_years_r2g14 = coalesce(n_passing_years_r2g14, 0L)) |>
  filter(n_passing_years_r2g14 >= EXP_RULE_MIN_YEARS)

message(nrow(candidates), " candidate site(s) pass rule 4 (>=", EXP_RULE_MIN_YEARS,
        " years) under R2, N=", EXP_RULE_MAX_FAIL_DAYS, " with the VUT/CUT (QC+GPP) fallback.")
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
## a22e893's no-GPP-data list (QC-only fallback) was c("AU-Stp","ES-LJu","ID-PaD","JP-Khw","JP-Ta2").
## With the GPP-aware fallback (item 1), a site is only ever assigned a column that already
## has >=1 non-NA GPP value, so this list should now be empty (or at least not contain AU-Cow,
## the prompt's expected recovery) -- checked directly, not asserted.
if ("AU-Cow" %in% no_gpp_sites) {
  warning("AU-Cow is still in the no-GPP-data drop list after the GPP-aware fallback -- expected it to be recovered.")
} else if ("AU-Cow" %in% candidates$site_id) {
  message("Confirmed: AU-Cow is recovered (not dropped for no-GPP-data) under the GPP-aware fallback.")
}
if (EXP_CFG_CUT_FALLBACK_USE_GPP && length(no_gpp_sites) > 0) {
  message("Note: ", length(no_gpp_sites), " site(s) still have no non-NA GPP at all in their CHOSEN ",
          "column despite the GPP-aware fallback -- this can only happen if a site has in-range GPP ",
          "data outside the pulled years or an edge case the fallback's COUNT() check did not catch; ",
          "investigate: ", paste(sort(no_gpp_sites), collapse = ", "))
}

message(length(rule5_fail_sites), " candidate site(s) fail rule 5 (>", EXP_RULE_NEGGPP_SHR * 100,
        "% of daily GPP < ", EXP_RULE_NEGGPP_VAL, "): ", paste(sort(rule5_fail_sites), collapse = ", "))
stopifnot(!any(GSL_SITE_IDS %in% c(no_gpp_sites, rule5_fail_sites))) # none of the 62 can be dropped here

## ---- Final kept sites ------------------------------------------------------
dropped_sites <- union(no_gpp_sites, rule5_fail_sites)
exp_sites <- candidates |>
  filter(!site_id %in% dropped_sites) |>
  left_join(qc_fallback |> select(site_id, qc_col_used, gpp_col_used), by = "site_id") |>
  left_join(gpp_check |> select(site_id, n_days_gpp_present, share_gpp_below_neg1), by = "site_id") |>
  mutate(group = unname(EXP_IGBP_TO_GROUP[igbp])) |>
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

## ---- Item 1 report: every population site recovered or changed by the
## GPP-aware VUT/CUT fallback, vs a22e893's QC-only fallback. ----------------
cut_fallback_report <- fallback_compare |>
  left_join(population |> select(site_id, igbp, in_gsl62), by = "site_id") |>
  mutate(
    in_expanded_set = site_id %in% exp_sites$site_id,
    status = case_when(
      carbon_src == "DROP" ~ "dropped (no usable VUT or CUT column)",
      changed & in_expanded_set ~ "recovered into the expanded set",
      changed & !in_expanded_set ~ "fallback column changed, but still not in the expanded set (other rule)",
      TRUE ~ "unchanged"
    )
  ) |>
  filter(status != "unchanged") |>
  select(site_id, igbp, in_gsl62, qc_col_used_old, qc_col_used_new = qc_col_used, status, in_expanded_set) |>
  arrange(site_id)
write_csv(cut_fallback_report, file.path(EXP_TABLES, "cut_fallback_changes.csv"))
write_meta(
  file.path(EXP_TABLES, "cut_fallback_changes.csv"),
  input_sources = c(file.path(AUDIT_TABLES, "selection_audit_rule_options_qc_fallback.csv"),
                     "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')"),
  notes = paste0(
    "Session 2a item 1: every population site whose VUT/CUT fallback column changed, or that is ",
    "newly dropped, once the fallback is keyed to GPP availability as well as QC availability ",
    "(vs a22e893's QC-only rule, selection_audit_rule_options_qc_fallback.csv). ",
    nrow(cut_fallback_report), " site(s) affected."
  )
)
message(nrow(cut_fallback_report), " population site(s) recovered/changed/dropped by the GPP-aware fallback (see cut_fallback_changes.csv).")

message("--- 01_select.R: done --- ", nrow(exp_sites), " sites, ", nrow(exp_site_years), " site-years.")
