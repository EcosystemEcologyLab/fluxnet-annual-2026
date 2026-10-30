## 11_selection_audit.R — GSL site-selection audit (2026-10-30).
##
## Read-only on the repo-root DuckDB store (dataset = 'FLUXMET' only). Does
## NOT touch the 62-site GSL analysis, its code, or its existing tables --
## see ../README.md and logs/prompts/2026-10-30_gsl_selection_audit.md.
##
## Question: which long-running network sites does the manuscript's
## selection rule exclude, and would a rule that drops days or years
## (rather than whole sites) keep them?
##
## Manuscript rule (as stated in the prompt, not re-derived):
##   (1) drop IGBP CRO, CVM, WET, SNO, BSV
##   (2) a day passes if NEE_VUT_REF_QC >= 0.25 (literal: NEE_VUT_REF_QC
##       only, no CUT fallback -- unlike 04_qc.R's per-site VUT/CUT
##       fallback used for the main 62-site analysis. A site with no VUT
##       data at all therefore fails every day by construction; flagged
##       explicitly below, not silently absorbed into "excluded".)
##   (3) a year passes only if every day passes
##   (4) a site needs >= 5 passing years
##   (5) drop sites with > 10% of daily GPP_NT_VUT_REF below -1 gC/m2/d
##
## "NA counts as failing" (prompt instruction 2) -- both for a day whose
## NEE_VUT_REF_QC is NA and for a calendar day with no row at all (the full
## Jan1-Dec31 grid is built per site-year; a missing row is NA after the
## left join, i.e. failing, same treatment).

source("GSL_definition/code/00_config.R")
source("GSL_definition/code/fig_helpers.R")

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(tidyr); library(readr)
  library(lubridate); library(purrr); library(ggplot2)
})

AUDIT_DATE <- "2026-10-30"
TABLES_DIR <- file.path(GSL_ROOT, "tables")
FIG_DIR    <- file.path(GSL_ROOT, "figures")
DOCS_DIR   <- file.path(GSL_ROOT, "docs")

EXCLUDED_IGBP   <- c("CRO", "CVM", "WET", "SNO", "BSV")
RULE_QC_DAY     <- 0.25
RULE_QC_DAY_50  <- 0.50
RULE_MIN_YEARS  <- 5L
RULE_NEGGPP_VAL <- -1
RULE_NEGGPP_SHR <- 0.10
LONG_RECORD_MIN_YEARS <- 15L

## ============================================================================
## Step 1: population -- 20 Sept snapshot sites passing the IGBP rule, that
## also have daily FLUXMET rows in DuckDB.
## ============================================================================

snapshot <- read_csv(GSL_SNAPSHOT_SEP20, col_types = cols(.default = "c"), show_col_types = FALSE) |>
  mutate(location_lat = as.numeric(location_lat), location_long = as.numeric(location_long),
         first_year = as.integer(first_year), last_year = as.integer(last_year))

igbp_candidates <- snapshot |> filter(!igbp %in% EXCLUDED_IGBP) |> pull(site_id) |> unique()
message("Snapshot sites: ", nrow(snapshot), "; passing IGBP rule 1: ", length(igbp_candidates),
        " (dropped ", nrow(snapshot) - length(igbp_candidates), " for IGBP in ",
        paste(EXCLUDED_IGBP, collapse = ", "), ")")

con <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
network_daily <- dbGetQuery(con, "
  SELECT site_id, TIMESTAMP, NEE_VUT_REF_QC, NEE_CUT_REF_QC, GPP_NT_VUT_REF
  FROM daily WHERE dataset = 'FLUXMET'
")
dbDisconnect(con, shutdown = TRUE)
network_daily$TIMESTAMP <- as.Date(as.character(network_daily$TIMESTAMP))
network_daily$year <- lubridate::year(network_daily$TIMESTAMP)

sites_with_daily <- unique(network_daily$site_id)
population_ids <- intersect(igbp_candidates, sites_with_daily)
message("Of those, ", length(population_ids), " have at least one daily FLUXMET row -- this is the population.")

pop_daily <- network_daily |> filter(site_id %in% population_ids)

## Per-site: any non-NA NEE_VUT_REF_QC (and NEE_CUT_REF_QC) at all? Flags
## the literal-rule-2 edge case: a site with no VUT data fails every day by
## construction under the manuscript's literal rule (no CUT fallback), even
## if it has abundant CUT data.
has_vut <- pop_daily |>
  group_by(site_id) |>
  summarise(
    any_vut_qc = any(!is.na(NEE_VUT_REF_QC)),
    any_cut_qc = any(!is.na(NEE_CUT_REF_QC)),
    .groups = "drop"
  )

## Long-record flag: >= 15 years with 12 months of data present, from
## site_year_data_presence.csv (prompt instruction 1) -- independent of the
## QC screening done below.
presence <- read_csv("data/snapshots/site_year_data_presence.csv", show_col_types = FALSE)
long_record_sites <- presence |>
  filter(n_months_present == 12L) |>
  count(site_id, name = "n_full_months_years") |>
  filter(n_full_months_years >= LONG_RECORD_MIN_YEARS) |>
  pull(site_id)
message(length(long_record_sites), " long-record site(s) network-wide (>= ", LONG_RECORD_MIN_YEARS,
        " years with 12 months of data, any IGBP/network).")

population <- snapshot |>
  filter(site_id %in% population_ids) |>
  select(site_id, igbp, network, location_lat, location_long, first_year, last_year) |>
  left_join(has_vut, by = "site_id") |>
  mutate(
    is_long_record = site_id %in% long_record_sites,
    in_gsl62       = site_id %in% GSL_SITE_IDS,
    record_span_years = last_year - first_year + 1L
  )

write_csv(population, file.path(TABLES_DIR, "selection_audit_population.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_population.csv"),
  input_sources = c(GSL_SNAPSHOT_SEP20, "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')",
                     "data/snapshots/site_year_data_presence.csv"),
  notes = paste0(
    "Population = snapshot sites not in IGBP {", paste(EXCLUDED_IGBP, collapse=","),
    "} AND with >=1 daily FLUXMET row. ", nrow(population), " sites, of which ",
    sum(population$is_long_record), " are long-record (>=", LONG_RECORD_MIN_YEARS,
    " yr w/ 12 months data) and ", sum(population$in_gsl62), " are in the GSL 62. ",
    sum(!population$any_vut_qc), " population site(s) have no non-NA NEE_VUT_REF_QC at all ",
    "and so fail rule 2 on every day by construction, per the literal, no-fallback rule ",
    "stated in the prompt -- of these, ", sum(!population$any_vut_qc & population$any_cut_qc),
    " have substantial NEE_CUT_REF_QC data instead (CUT-only) and ",
    sum(!population$any_vut_qc & !population$any_cut_qc), " have neither (fully ungated)."
  )
)
message("Saved: ", file.path(TABLES_DIR, "selection_audit_population.csv"))

## ============================================================================
## Step 2: per site-year QC screening on the full Jan1-Dec31 calendar grid.
## Missing calendar days and NA QC both count as failing (prompt instr. 2).
## ============================================================================

site_years_present <- pop_daily |> distinct(site_id, year) |> arrange(site_id, year)
message("Population site-years with >=1 day of data: ", nrow(site_years_present))

screen_site_year <- function(sid, yr) {
  d <- pop_daily |> filter(site_id == sid, year == yr) |> select(TIMESTAMP, NEE_VUT_REF_QC)
  expected_days <- if (lubridate::leap_year(yr)) 366L else 365L
  grid <- tibble(TIMESTAMP = seq(as.Date(sprintf("%d-01-01", yr)), as.Date(sprintf("%d-12-31", yr)), by = "day"))
  g <- grid |> left_join(d, by = "TIMESTAMP")

  pass025_raw <- qc_passes(g$NEE_VUT_REF_QC, RULE_QC_DAY)
  pass050_raw <- qc_passes(g$NEE_VUT_REF_QC, RULE_QC_DAY_50)
  fail025 <- ifelse(is.na(pass025_raw), TRUE, !pass025_raw)
  fail050 <- ifelse(is.na(pass050_raw), TRUE, !pass050_raw)

  n_fail_025 <- sum(fail025)
  n_fail_050 <- sum(fail050)

  run_lengths <- function(fail_vec) {
    r <- rle(fail_vec)
    fail_runs <- r$lengths[r$values]
    if (length(fail_runs) == 0) 0L else max(fail_runs)
  }
  longest_run_025 <- run_lengths(fail025)
  longest_run_050 <- run_lengths(fail050)

  month_fail <- tibble(month = lubridate::month(g$TIMESTAMP), fail = fail025) |>
    filter(fail) |> count(month, name = "n_fail_025")

  list(
    summary = tibble(
      site_id = sid, year = yr,
      days_present = nrow(d), expected_days = expected_days,
      n_fail_025 = n_fail_025, n_fail_050 = n_fail_050,
      share_pass_025 = (expected_days - n_fail_025) / expected_days,
      share_pass_050 = (expected_days - n_fail_050) / expected_days,
      longest_fail_run_025 = longest_run_025, longest_fail_run_050 = longest_run_050
    ),
    month = if (nrow(month_fail) > 0) tibble(site_id = sid, year = yr, month_fail) else
      tibble(site_id = character(), year = integer(), month = integer(), n_fail_025 = integer())
  )
}

message("Screening ", nrow(site_years_present), " site-years (full calendar grid each) -- this takes a minute...")
screened <- purrr::pmap(site_years_present, function(site_id, year) screen_site_year(site_id, year))

site_year_screen <- bind_rows(purrr::map(screened, "summary"))
site_year_month_fail <- bind_rows(purrr::map(screened, "month"))

write_csv(site_year_screen, file.path(TABLES_DIR, "selection_audit_site_year_screen.csv"))
write_csv(site_year_month_fail, file.path(TABLES_DIR, "selection_audit_site_year_month_fail.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_site_year_screen.csv"),
  input_sources = "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET', population sites)",
  notes = paste0(
    "One row per population site-year with >=1 day of data. share_pass_0*/n_fail_0*/",
    "longest_fail_run_0* computed over the FULL Jan1-Dec31 calendar grid for that year ",
    "(expected_days = 365 or 366) -- a calendar day with no row, or an NA NEE_VUT_REF_QC, ",
    "both count as failing, per the audit prompt. No CUT fallback (literal rule 2: ",
    "NEE_VUT_REF_QC only)."
  )
)
write_meta(
  file.path(TABLES_DIR, "selection_audit_site_year_month_fail.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_site_year_screen.csv"),
  notes = "Failing-day (0.25 threshold) counts by calendar month, long format; months with zero failing days are omitted."
)
message("Saved: selection_audit_site_year_screen.csv (", nrow(site_year_screen), " rows), ",
        "selection_audit_site_year_month_fail.csv (", nrow(site_year_month_fail), " rows)")

## ============================================================================
## Step 3: rule variants -- per-year pass/fail, then per-site aggregation.
## ============================================================================

variant_defs <- list(
  "i_manuscript"      = quote(share_pass_025 == 1),
  "ii_025_ge95"       = quote(share_pass_025 >= 0.95),
  "ii_025_ge90"       = quote(share_pass_025 >= 0.90),
  "ii_025_ge80"       = quote(share_pass_025 >= 0.80),
  "iii_050_ge95"      = quote(share_pass_050 >= 0.95),
  "iii_050_ge90"      = quote(share_pass_050 >= 0.90),
  "iii_050_ge80"      = quote(share_pass_050 >= 0.80),
  "iv_gap3"           = quote(longest_fail_run_025 <= 3),
  "iv_gap7"           = quote(longest_fail_run_025 <= 7),
  "iv_gap14"          = quote(longest_fail_run_025 <= 14)
)

for (nm in names(variant_defs)) {
  site_year_screen[[paste0("pass_", nm)]] <- eval(variant_defs[[nm]], envir = site_year_screen)
}

## Strictness order for the figure's "strictest variant passed" colour,
## strictest first. Documented, not a formally proven total order: 0.50 is
## nested inside 0.25 at the same %, and smaller %/gap is stricter than
## larger, but the gap-based (iv) and share-based (ii/iii) families are NOT
## strictly nested against each other (a bursty-but-short-lived failure
## pattern can pass a gap variant while failing a share variant at the same
## nominal "tolerance", and vice versa) -- see docs/selection_audit_*.md.
strictness_order <- c(
  "i_manuscript", "iii_050_ge95", "ii_025_ge95", "iv_gap3",
  "iii_050_ge90", "ii_025_ge90", "iv_gap7",
  "iii_050_ge80", "ii_025_ge80", "iv_gap14"
)
stopifnot(setequal(strictness_order, names(variant_defs)))

site_year_screen$strictest_passed <- NA_character_
for (nm in rev(strictness_order)) {
  pass_col <- paste0("pass_", nm)
  site_year_screen$strictest_passed[site_year_screen[[pass_col]]] <- nm
}

write_csv(site_year_screen, file.path(TABLES_DIR, "selection_audit_site_year_screen.csv"))

## Per-site, per-variant: n passing years and >=5 flag.
site_variant <- site_year_screen |>
  select(site_id, year, starts_with("pass_")) |>
  pivot_longer(starts_with("pass_"), names_to = "variant", names_prefix = "pass_", values_to = "pass") |>
  group_by(site_id, variant) |>
  summarise(n_passing_years = sum(pass), .groups = "drop") |>
  mutate(ge5 = n_passing_years >= RULE_MIN_YEARS) |>
  left_join(population |> select(site_id, is_long_record, in_gsl62), by = "site_id")

write_csv(site_variant, file.path(TABLES_DIR, "selection_audit_site_variant.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_site_variant.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_site_year_screen.csv"),
  notes = "One row per population site x rule variant: n_passing_years and whether it reaches the >=5-year threshold (rule 4)."
)

## Variant-level summary: sites/site-years kept (gated on >=5, i.e. the site
## actually qualifies under that variant), and the diff vs the manuscript
## rule (i_manuscript).
kept_sites_by_variant <- function(v) site_variant$site_id[site_variant$variant == v & site_variant$ge5]
kept_site_years_by_variant <- function(v) {
  kept <- kept_sites_by_variant(v)
  sum(site_year_screen[[paste0("pass_", v)]] & site_year_screen$site_id %in% kept)
}

sites_kept_i <- kept_sites_by_variant("i_manuscript")
site_years_kept_i <- kept_site_years_by_variant("i_manuscript")

variant_summary <- purrr::map_dfr(names(variant_defs), function(v) {
  kept <- kept_sites_by_variant(v)
  site_years_kept <- kept_site_years_by_variant(v)
  added_sites <- setdiff(kept, sites_kept_i)
  added_long_record <- intersect(added_sites, population$site_id[population$is_long_record])
  site_years_added_by_new_sites <- sum(
    site_year_screen[[paste0("pass_", v)]] & site_year_screen$site_id %in% added_sites
  )
  tibble(
    variant = v,
    sites_kept = length(kept),
    site_years_kept = site_years_kept,
    sites_added_vs_manuscript = length(added_sites),
    added_sites_list = paste(sort(added_sites), collapse = "; "),
    long_record_sites_added = length(added_long_record),
    long_record_sites_added_list = paste(sort(added_long_record), collapse = "; "),
    site_years_added_by_newly_recovered_sites = site_years_added_by_new_sites,
    site_years_added_total_vs_manuscript = site_years_kept - site_years_kept_i
  )
})

write_csv(variant_summary, file.path(TABLES_DIR, "selection_audit_variant_summary.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_variant_summary.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_site_variant.csv"),
  notes = paste0(
    "sites_kept/site_years_kept are gated on the >=5-passing-year threshold (rule 4) -- a site ",
    "with 1-4 passing years under a variant is not counted as 'kept'. *_vs_manuscript columns ",
    "compare against the 'i_manuscript' row (share_pass_025==1 every year). ",
    "site_years_added_by_newly_recovered_sites counts only the passing years of sites not kept ",
    "under the manuscript rule at all; site_years_added_total_vs_manuscript also includes extra ",
    "qualifying years gained at sites already kept under the manuscript rule."
  )
)
message("Saved: selection_audit_variant_summary.csv")
print(variant_summary |> select(variant, sites_kept, site_years_kept, sites_added_vs_manuscript, long_record_sites_added))

## Long-record sites recovered by any variant (with which variants), and
## long-record sites never recovered (with a reason).
long_record_pop <- population |> filter(is_long_record)
long_record_variant <- site_variant |> filter(is_long_record)

recovered <- long_record_variant |>
  filter(variant != "i_manuscript", ge5) |>
  filter(!site_id %in% sites_kept_i) |>
  group_by(site_id) |>
  summarise(recovered_by = paste(sort(unique(variant)), collapse = "; "), .groups = "drop")

never_recovered_ids <- setdiff(long_record_pop$site_id, union(sites_kept_i, recovered$site_id))

never_recovered_reason <- purrr::map_dfr(never_recovered_ids, function(sid) {
  v <- has_vut$any_vut_qc[has_vut$site_id == sid]
  has_cut <- has_vut$any_cut_qc[has_vut$site_id == sid]
  if (length(v) == 0 || !v) {
    reason <- if (length(has_cut) > 0 && has_cut) {
      "no non-NA NEE_VUT_REF_QC at all -- CUT-only in our FLUXMET data; rule 2 as stated has no CUT fallback, so no day ever passes regardless of variant"
    } else {
      "no non-NA NEE_VUT_REF_QC or NEE_CUT_REF_QC at all (fully ungated)"
    }
  } else {
    best_n <- site_variant |> filter(site_id == sid, variant == "iv_gap14") |> pull(n_passing_years)
    best_share <- site_variant |> filter(site_id == sid, variant == "ii_025_ge80") |> pull(n_passing_years)
    reason <- sprintf(
      "still < %d passing years even at the loosest variants (iv_gap14: %d yr; ii_025_ge80: %d yr)",
      RULE_MIN_YEARS, best_n, best_share
    )
  }
  tibble(site_id = sid, reason = reason)
})

long_record_recovery <- long_record_pop |>
  select(site_id, igbp, network, location_lat, in_gsl62, any_vut_qc, any_cut_qc) |>
  mutate(
    kept_under_manuscript_rule = site_id %in% sites_kept_i,
    recovered_by_variant = site_id %in% recovered$site_id
  ) |>
  left_join(recovered, by = "site_id") |>
  left_join(never_recovered_reason, by = "site_id") |>
  arrange(desc(kept_under_manuscript_rule), desc(recovered_by_variant), site_id)

write_csv(long_record_recovery, file.path(TABLES_DIR, "selection_audit_long_record_recovery.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_long_record_recovery.csv"),
  input_sources = c(file.path(TABLES_DIR, "selection_audit_site_variant.csv"),
                     file.path(TABLES_DIR, "selection_audit_population.csv")),
  notes = paste0(
    length(long_record_pop$site_id), " long-record population sites: ", length(sites_kept_i),
    " already kept under the manuscript rule, ", nrow(recovered), " recovered by at least one ",
    "looser variant, ", length(never_recovered_ids), " still excluded under every variant tried."
  )
)
message("Saved: selection_audit_long_record_recovery.csv -- ", nrow(recovered),
        " long-record site(s) recovered: ", paste(sort(recovered$site_id), collapse = ", "))
message(length(never_recovered_ids), " long-record site(s) still excluded: ",
        paste(sort(never_recovered_ids), collapse = ", "))

## ============================================================================
## Step 4: who the manuscript rule (i) removes -- record length, MAT,
## latitude, IGBP for kept vs. lost population sites; failing-day
## concentration in local winter months at cold vs. warm sites. Numbers
## only, per the prompt.
## ============================================================================

worldclim <- read_csv("data/snapshots/site_worldclim.csv", show_col_types = FALSE)

kept_lost <- population |>
  left_join(worldclim, by = "site_id") |>
  left_join(
    site_years_present |> count(site_id, name = "n_siteyears_with_data"),
    by = "site_id"
  ) |>
  mutate(kept_under_manuscript_rule = site_id %in% sites_kept_i)

write_csv(kept_lost, file.path(TABLES_DIR, "selection_audit_kept_vs_lost.csv"))

kept_lost_summary <- kept_lost |>
  group_by(kept_under_manuscript_rule) |>
  summarise(
    n_sites = n(),
    mean_record_span_years = mean(record_span_years, na.rm = TRUE),
    median_record_span_years = median(record_span_years, na.rm = TRUE),
    mean_n_siteyears_with_data = mean(n_siteyears_with_data, na.rm = TRUE),
    mean_mat_worldclim = mean(mat_worldclim, na.rm = TRUE),
    median_mat_worldclim = median(mat_worldclim, na.rm = TRUE),
    mean_abs_latitude = mean(abs(location_lat), na.rm = TRUE),
    .groups = "drop"
  )

igbp_kept_lost <- kept_lost |> count(kept_under_manuscript_rule, igbp) |>
  pivot_wider(names_from = kept_under_manuscript_rule, values_from = n, values_fill = 0,
              names_prefix = "kept_")

write_csv(kept_lost_summary, file.path(TABLES_DIR, "selection_audit_kept_vs_lost_summary.csv"))
write_csv(igbp_kept_lost, file.path(TABLES_DIR, "selection_audit_kept_vs_lost_igbp.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_kept_vs_lost.csv"),
  input_sources = c(file.path(TABLES_DIR, "selection_audit_population.csv"),
                     "data/snapshots/site_worldclim.csv"),
  notes = "mean annual temperature (mat_worldclim) joined from data/snapshots/site_worldclim.csv by site_id; kept = passes the manuscript rule (i_manuscript) with >=5 years."
)
write_meta(
  file.path(TABLES_DIR, "selection_audit_kept_vs_lost_summary.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_kept_vs_lost.csv"), notes = ""
)
write_meta(
  file.path(TABLES_DIR, "selection_audit_kept_vs_lost_igbp.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_kept_vs_lost.csv"), notes = ""
)
message("Saved: selection_audit_kept_vs_lost*.csv")
print(kept_lost_summary)

## Winter-month concentration: local winter by hemisphere (lat sign), cold
## vs warm sites split at the population median mat_worldclim. Numbers only.
mat_median <- median(kept_lost$mat_worldclim, na.rm = TRUE)

month_fail_annotated <- site_year_month_fail |>
  left_join(kept_lost |> select(site_id, location_lat, mat_worldclim), by = "site_id") |>
  mutate(
    hemisphere = ifelse(location_lat >= 0, "N", "S"),
    is_local_winter = (hemisphere == "N" & month %in% c(12, 1, 2)) |
                       (hemisphere == "S" & month %in% c(6, 7, 8)),
    temp_group = ifelse(mat_worldclim < mat_median, "cold (below median MAT)", "warm (at/above median MAT)")
  )

winter_concentration <- month_fail_annotated |>
  filter(!is.na(temp_group)) |>
  group_by(temp_group) |>
  summarise(
    n_fail_days_total = sum(n_fail_025),
    n_fail_days_local_winter = sum(n_fail_025[is_local_winter]),
    share_fail_days_in_local_winter = n_fail_days_local_winter / n_fail_days_total,
    .groups = "drop"
  )

write_csv(winter_concentration, file.path(TABLES_DIR, "selection_audit_winter_concentration.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_winter_concentration.csv"),
  input_sources = c(file.path(TABLES_DIR, "selection_audit_site_year_month_fail.csv"),
                     "data/snapshots/site_worldclim.csv"),
  notes = sprintf(
    paste0(
      "Local winter = Dec/Jan/Feb for Northern-hemisphere sites (location_lat >= 0), Jun/Jul/Aug ",
      "for Southern. Cold/warm split at the population median mat_worldclim (%.2f degC). A ",
      "uniform (non-concentrated) distribution across 3 of 12 months would give a 25%% share; ",
      "numbers reported as-is, no further interpretation."
    ), mat_median
  )
)
message("Saved: selection_audit_winter_concentration.csv")
print(winter_concentration)

## ============================================================================
## Step 5: rule 5 (negative GPP) check for every long-record site recovered
## by any variant -- confirm none would be dropped for this reason instead.
## ============================================================================

neggpp_check <- pop_daily |>
  filter(site_id %in% recovered$site_id) |>
  group_by(site_id) |>
  summarise(
    n_days_gpp_present = sum(!is.na(GPP_NT_VUT_REF)),
    n_days_gpp_below_neg1 = sum(GPP_NT_VUT_REF < RULE_NEGGPP_VAL, na.rm = TRUE),
    share_gpp_below_neg1 = ifelse(n_days_gpp_present > 0, n_days_gpp_below_neg1 / n_days_gpp_present, NA_real_),
    fails_rule5 = share_gpp_below_neg1 > RULE_NEGGPP_SHR,
    .groups = "drop"
  )

write_csv(neggpp_check, file.path(TABLES_DIR, "selection_audit_recovered_rule5_check.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_recovered_rule5_check.csv"),
  input_sources = "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')",
  notes = paste0(
    "Rule 5 (>10% of daily GPP_NT_VUT_REF < -1 gC/m2/d) checked over EVERY available daily row ",
    "for each site recovered by at least one looser variant (step 3), across its whole record, ",
    "not just the years that pass a given variant. ",
    if (isTRUE(any(neggpp_check$fails_rule5, na.rm = TRUE)))
      paste0(sum(neggpp_check$fails_rule5, na.rm = TRUE), " site(s) WOULD fail rule 5.")
    else "No recovered site fails rule 5.",
    if (any(is.na(neggpp_check$fails_rule5)))
      paste0(" ", sum(is.na(neggpp_check$fails_rule5)), " site(s) have no GPP_NT_VUT_REF data at all, so rule 5 is undetermined (NA), not FALSE, for them: ",
             paste(neggpp_check$site_id[is.na(neggpp_check$fails_rule5)], collapse = ", "), ".")
    else ""
  )
)
message("Saved: selection_audit_recovered_rule5_check.csv -- ",
        sum(neggpp_check$fails_rule5, na.rm = TRUE), " of ", nrow(neggpp_check),
        " recovered site(s) would fail rule 5 (", sum(is.na(neggpp_check$fails_rule5)), " undetermined, no GPP data).")
print(neggpp_check)

## ============================================================================
## Step 6: sites passing the manuscript rule on our data but not in GSL 62.
## ============================================================================

passes_i_not_gsl62 <- population |>
  filter(site_id %in% sites_kept_i, !in_gsl62) |>
  select(site_id, igbp, network, location_lat, record_span_years)

write_csv(passes_i_not_gsl62, file.path(TABLES_DIR, "selection_audit_passes_rule_not_gsl62.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_passes_rule_not_gsl62.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_kept_vs_lost.csv"),
  notes = "Population sites passing the manuscript rule (i_manuscript, >=5 years) on our data that are not among the GSL 62. Listed only -- not resolved (may reflect a manuscript-side exclusion reason not modelled here, e.g. rule 5, a data-vintage difference, or a reason outside this audit's scope)."
)
message("Saved: selection_audit_passes_rule_not_gsl62.csv -- ", nrow(passes_i_not_gsl62),
        " site(s): ", paste(sort(passes_i_not_gsl62$site_id), collapse = ", "))

## ============================================================================
## Step 7: figure -- one row per long-record site, years on x, coloured by
## strictest variant passed; GSL-62 sites marked.
## ============================================================================

fig_data <- site_year_screen |>
  filter(site_id %in% long_record_pop$site_id) |>
  select(site_id, year, strictest_passed) |>
  mutate(strictest_passed = factor(strictest_passed, levels = strictness_order))

site_order <- long_record_pop |>
  arrange(desc(in_gsl62), site_id) |>
  mutate(label = ifelse(in_gsl62, paste0(site_id, " *"), site_id))

fig_data <- fig_data |>
  left_join(site_order |> select(site_id, label), by = "site_id") |>
  mutate(label = factor(label, levels = rev(site_order$label)))

strict_labels <- c(
  i_manuscript = "i: manuscript (0.25, every day)",
  iii_050_ge95 = "iii: 0.50, >=95%", ii_025_ge95 = "ii: 0.25, >=95%", iv_gap3 = "iv: gaps<=3d",
  iii_050_ge90 = "iii: 0.50, >=90%", ii_025_ge90 = "ii: 0.25, >=90%", iv_gap7 = "iv: gaps<=7d",
  iii_050_ge80 = "iii: 0.50, >=80%", ii_025_ge80 = "ii: 0.25, >=80%", iv_gap14 = "iv: gaps<=14d"
)

pal <- setNames(
  grDevices::colorRampPalette(c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"))(length(strictness_order)),
  strictness_order
)

p_audit <- ggplot(fig_data, aes(x = year, y = label, fill = strictest_passed)) +
  geom_tile(colour = "grey90", linewidth = 0.2) +
  scale_fill_manual(values = pal, breaks = strictness_order, labels = strict_labels[strictness_order],
                     na.value = "grey90", name = "Strictest variant passed",
                     na.translate = TRUE) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(x = "Year", y = NULL,
       caption = "* = in the GSL 62. Grey = fails every variant tried (incl. the loosest, iv_gap14).") +
  theme_gsl(base_size = PEERJ_BASE_PT, axis_title_pt = PEERJ_AXIS_TITLE_PT) +
  theme(
    panel.border = element_blank(), axis.ticks.y = element_blank(),
    legend.position = "bottom", legend.text = element_text(size = PEERJ_ANNOTATION_PT),
    plot.caption = element_text(size = PEERJ_ANNOTATION_PT, hjust = 0, colour = "grey30")
  )

n_rows <- length(unique(fig_data$label))
fig_height_mm <- max(90, min(240, 25 + n_rows * 4.2))

fs::dir_create(FIG_DIR)
audit_fig_stem <- file.path(FIG_DIR, "selection_audit_long_record_variants")
ggsave(paste0(audit_fig_stem, ".png"), p_audit, width = PEERJ_FIG_WIDTH_MM, height = fig_height_mm,
       units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
ggsave(paste0(audit_fig_stem, ".pdf"), p_audit, width = PEERJ_FIG_WIDTH_MM, height = fig_height_mm,
       units = "mm", device = grDevices::pdf, family = "Helvetica", bg = "white")
writeLines(c(
  "Selection audit: long-record sites by year, coloured by the strictest rule variant each year passes.",
  "Rows: long-record sites (>=15 years with 12 months of data in site_year_data_presence.csv).",
  "* after the site name marks a site that is one of the GSL 62.",
  "Grey tiles fail every variant tried, including the loosest (iv_gap14: tolerate failing runs up to 14 days).",
  "Strictness order (strictest to loosest; not a formally proven total order -- see methods memo):",
  paste("  ", seq_along(strictness_order), ". ", strict_labels[strictness_order], sep = "")
), paste0(audit_fig_stem, ".legend.txt"))
write_meta(
  paste0(audit_fig_stem, ".png"),
  input_sources = c(file.path(TABLES_DIR, "selection_audit_site_year_screen.csv"),
                     file.path(TABLES_DIR, "selection_audit_population.csv")),
  notes = "Diagnostic figure for the selection audit, not a manuscript figure; uses the shared GSL PeerJ style for consistency only."
)
message("Saved: ", audit_fig_stem, ".png/.pdf/.legend.txt/.meta.json (", n_rows, " long-record site rows)")

message("--- 11_selection_audit.R: done ---")
