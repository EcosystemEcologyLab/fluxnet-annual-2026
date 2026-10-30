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
  SELECT site_id, TIMESTAMP, NEE_VUT_REF_QC, NEE_CUT_REF_QC, GPP_NT_VUT_REF, GPP_NT_CUT_REF
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

## ============================================================================
## Step 8 (2026-10-30, selection-rule-options follow-on task -- see
## logs/prompts/2026-10-30_gsl_selection_rule_options.md and
## docs/selection_rule_options_2026-10-30.md): evaluate a per-site VUT/CUT QC
## fallback, exactly as scripts/04_qc.R already does for the 62-site
## analysis, crossed with four year-passing rules (R0 manuscript baseline,
## R1 = R0 + fallback, R2 = R1 + a flat daily-failure allowance, R3 = R1 +
## ignoring failing days whose gap-filled GPP falls outside a low-GPP band
## near the site's P99). Read-only, additive only -- does NOT alter
## anything written by Steps 1-7 above, and does NOT touch the 62-site GSL
## analysis.
##
## Unchanged from the manuscript throughout this step: rule 1 (IGBP, already
## applied to `population`), the 0.25 daily threshold, NA-as-failing, the
## >=5-year rule 4 threshold, and rule 5's >10% share cutoff.
## ============================================================================

## ---- Per-site QC/GPP column choice, exactly as scripts/04_qc.R: VUT if the
## site has any non-NA VUT QC, else CUT (never mixed within a site). Checked
## above (has_vut) that every population site has at least one of the two --
## there is no "neither" case to handle here.
qc_fallback <- has_vut |>
  mutate(
    qc_col_used  = ifelse(any_vut_qc, "NEE_VUT_REF_QC", "NEE_CUT_REF_QC"),
    gpp_col_used = ifelse(any_vut_qc, "GPP_NT_VUT_REF", "GPP_NT_CUT_REF")
  )
stopifnot(all(qc_fallback$any_vut_qc | qc_fallback$any_cut_qc))
message(sum(!qc_fallback$any_vut_qc), " population site(s) are gated on the CUT fallback ",
        "(no non-NA VUT QC at all); all of them have CUT data, so none is left fully ungated.")

write_csv(qc_fallback, file.path(TABLES_DIR, "selection_audit_rule_options_qc_fallback.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_rule_options_qc_fallback.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_population.csv"),
  notes = "Per-site QC/GPP column choice for the R1-R3 rule options: NEE_VUT_REF_QC/GPP_NT_VUT_REF if the site has any non-NA NEE_VUT_REF_QC, else NEE_CUT_REF_QC/GPP_NT_CUT_REF -- the same per-site (not per-row) priority scripts/04_qc.R already uses for the 62-site analysis."
)

## ---- Full calendar-grid day table under the fallback QC column, for every
## population site-year with >=1 day of data (same 4562 site-years as Step
## 2 -- rule 1 is unchanged, so the population and its site-years are
## unchanged). Kept in memory only (not written as a table -- ~1.6M rows,
## and every number the prompt asks for is an aggregate of it).
pop_daily_fb <- pop_daily |>
  left_join(qc_fallback |> select(site_id, qc_col_used, gpp_col_used), by = "site_id") |>
  mutate(
    qc_fb  = ifelse(qc_col_used == "NEE_VUT_REF_QC", NEE_VUT_REF_QC, NEE_CUT_REF_QC),
    gpp_fb = ifelse(gpp_col_used == "GPP_NT_VUT_REF", GPP_NT_VUT_REF, GPP_NT_CUT_REF)
  )

build_day_fb <- function(sid, yr) {
  d <- pop_daily_fb |> filter(site_id == sid, year == yr) |> select(TIMESTAMP, qc_fb, gpp_fb)
  grid <- tibble(TIMESTAMP = seq(as.Date(sprintf("%d-01-01", yr)), as.Date(sprintf("%d-12-31", yr)), by = "day"))
  g <- grid |> left_join(d, by = "TIMESTAMP")
  pass_raw <- qc_passes(g$qc_fb, RULE_QC_DAY)
  is_fail <- ifelse(is.na(pass_raw), TRUE, !pass_raw) # NA counts as failing, same convention as Step 2
  tibble(site_id = sid, year = yr, date = g$TIMESTAMP, is_fail = is_fail, gpp_value = g$gpp_fb)
}

message("Building the fallback-QC full-calendar day grid for ", nrow(site_years_present),
        " population site-years (R1 basis) -- this takes a minute...")
day_fb <- purrr::pmap_dfr(site_years_present, function(site_id, year) build_day_fb(site_id, year))
message("day_fb: ", nrow(day_fb), " rows (site-year x calendar day).")

## ---- R0 (manuscript baseline, VUT only -- the existing i_manuscript column
## from Step 3) and R1 (R0 + fallback) and R2 (R1 + flat daily allowance).
site_year_screen_fb <- day_fb |>
  group_by(site_id, year) |>
  summarise(
    expected_days = n(),
    n_fail_fb = sum(is_fail),
    longest_fail_run_fb = {
      r <- rle(is_fail); fr <- r$lengths[r$values]
      if (length(fr) == 0) 0L else max(fr)
    },
    .groups = "drop"
  ) |>
  mutate(
    pass_r1     = n_fail_fb == 0L,
    pass_r2_g3  = n_fail_fb <= 3L,
    pass_r2_g7  = n_fail_fb <= 7L,
    pass_r2_g14 = n_fail_fb <= 14L
  )

## ---- P99 per site for R3's band, computed from days that pass the daily
## test (is_fail == FALSE), restricted to years that pass R1; if a site has
## no R1-passing year at all, computed from all its passing days instead
## (state which -- p99_basis column below, per the prompt's instruction).
day_fb_r1 <- day_fb |> left_join(site_year_screen_fb |> select(site_id, year, pass_r1), by = c("site_id", "year"))
sites_with_r1_year <- site_year_screen_fb |> filter(pass_r1) |> distinct(site_id) |> pull(site_id)

safe_p99 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(quantile(x, 0.99, type = 7))
}

p99_from_r1_years <- day_fb_r1 |>
  filter(!is_fail, pass_r1) |>
  group_by(site_id) |>
  summarise(p99_r1 = safe_p99(gpp_value), n_r1 = sum(!is.na(gpp_value)), .groups = "drop")
p99_from_all_passing <- day_fb_r1 |>
  filter(!is_fail) |>
  group_by(site_id) |>
  summarise(p99_all = safe_p99(gpp_value), n_all = sum(!is.na(gpp_value)), .groups = "drop")

p99_tbl <- population |>
  select(site_id) |>
  left_join(p99_from_r1_years, by = "site_id") |>
  left_join(p99_from_all_passing, by = "site_id") |>
  mutate(
    has_r1_year = site_id %in% sites_with_r1_year,
    p99_gpp              = ifelse(has_r1_year, p99_r1, p99_all),
    n_days_used_for_p99  = ifelse(has_r1_year, n_r1, n_all),
    p99_basis = ifelse(has_r1_year, "R1-passing years", "all passing days (no R1-passing year available)")
  ) |>
  select(site_id, p99_gpp, n_days_used_for_p99, p99_basis)

write_csv(p99_tbl, file.path(TABLES_DIR, "selection_audit_rule_options_p99.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_rule_options_p99.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_population.csv"),
  notes = sprintf(
    paste0(
      "Per-site P99 of gap-filled daily GPP (VUT or CUT, per qc_fallback's gpp_col_used), computed ",
      "from days passing the fallback 0.25 daily test, in years passing R1; if a site has no ",
      "R1-passing year, computed from all its passing days instead (p99_basis states which). ",
      "%d of %d population sites have no R1-passing year and fall back to the all-passing-days basis."
    ), sum(!p99_tbl$site_id %in% sites_with_r1_year), nrow(p99_tbl)
  )
)
message("Saved: selection_audit_rule_options_p99.csv -- ", length(sites_with_r1_year),
        " site(s) have >=1 R1-passing year; ", nrow(p99_tbl) - length(sites_with_r1_year),
        " fall back to the all-passing-days P99 basis.")

## ---- R3: a failing day is "inside the band" (consequential, capped at M)
## if its gap-filled GPP lies in [0, 25% of site P99]; "outside the band"
## (always forgiven, unlimited) otherwise. A failing day with NO gpp value
## at all (missing calendar day) is conservatively treated as inside the
## band (counts toward M), consistent with this audit's NA-as-failing
## convention -- we cannot tell whether it would have been near the
## season-boundary-defining threshold range, so it is not given the benefit
## of the doubt.
day_fb_band <- day_fb |>
  left_join(p99_tbl |> select(site_id, p99_gpp), by = "site_id") |>
  mutate(
    in_band_fail  = is_fail & (is.na(gpp_value) | (gpp_value >= 0 & gpp_value <= 0.25 * p99_gpp)),
    out_band_fail = is_fail & !in_band_fail
  )

r3_summary <- day_fb_band |>
  group_by(site_id, year) |>
  summarise(
    n_fail_inside_band  = sum(in_band_fail),
    n_fail_outside_band = sum(out_band_fail),
    .groups = "drop"
  ) |>
  mutate(
    pass_r3_m0 = n_fail_inside_band <= 0L,
    pass_r3_m3 = n_fail_inside_band <= 3L
  )

band_by_month <- day_fb_band |>
  filter(is_fail) |>
  mutate(month = lubridate::month(date)) |>
  group_by(month) |>
  summarise(n_fail_inside_band = sum(in_band_fail), n_fail_outside_band = sum(out_band_fail), .groups = "drop") |>
  arrange(month)

write_csv(band_by_month, file.path(TABLES_DIR, "selection_audit_rule_options_r3_band_by_month.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_rule_options_r3_band_by_month.csv"),
  input_sources = c(file.path(TABLES_DIR, "selection_audit_rule_options_p99.csv")),
  notes = "Among ALL failing days (fallback 0.25 daily test) across the population, counted by calendar month: how many fall inside the R3 band (GPP in [0, 25% of site P99], or GPP missing -- consequential, capped at M) vs. outside it (always forgiven)."
)
message("Saved: selection_audit_rule_options_r3_band_by_month.csv")
print(band_by_month)

## ---- Assemble all 7 rule-option pass/fail columns on one per-site-year
## table: R0 (reuse Step 3's i_manuscript, VUT-only, no fallback), R1, R2 at
## N=3/7/14, R3 at M=0/3.
site_year_rule_options <- site_year_screen |>
  select(site_id, year, pass_r0 = pass_i_manuscript) |>
  left_join(site_year_screen_fb, by = c("site_id", "year")) |>
  left_join(r3_summary, by = c("site_id", "year"))

write_csv(site_year_rule_options, file.path(TABLES_DIR, "selection_audit_rule_options_site_year.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_rule_options_site_year.csv"),
  input_sources = c(file.path(TABLES_DIR, "selection_audit_site_year_screen.csv"),
                     file.path(TABLES_DIR, "selection_audit_rule_options_qc_fallback.csv"),
                     file.path(TABLES_DIR, "selection_audit_rule_options_p99.csv")),
  notes = paste0(
    "One row per population site-year (same 4562 as selection_audit_site_year_screen.csv -- rule 1 ",
    "is unchanged). pass_r0 = the manuscript baseline (VUT only, no fallback; identical to that ",
    "table's pass_i_manuscript). pass_r1 = R0 + per-site VUT/CUT fallback. pass_r2_g3/g7/g14 = R1 ",
    "+ a flat allowance of up to 3/7/14 failing days per year. pass_r3_m0/m3 = R1 + ignoring ",
    "failing days outside the P99 band, allowing up to 0/3 remaining (inside-band) failing days."
  )
)
message("Saved: selection_audit_rule_options_site_year.csv")

## ---- Per-site, per-rule: n passing years and the >=5-year gate (rule 4,
## unchanged), same pattern as Step 3's site_variant.
rule_cols <- c("pass_r0", "pass_r1", "pass_r2_g3", "pass_r2_g7", "pass_r2_g14", "pass_r3_m0", "pass_r3_m3")

site_rule_option <- site_year_rule_options |>
  select(site_id, year, all_of(rule_cols)) |>
  pivot_longer(all_of(rule_cols), names_to = "rule", names_prefix = "pass_", values_to = "pass") |>
  group_by(site_id, rule) |>
  summarise(n_passing_years = sum(pass), .groups = "drop") |>
  mutate(ge5 = n_passing_years >= RULE_MIN_YEARS, ge10_trend = n_passing_years >= 10L) |>
  left_join(population |> select(site_id, is_long_record, in_gsl62), by = "site_id")

write_csv(site_rule_option, file.path(TABLES_DIR, "selection_audit_rule_options_site_rule.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_rule_options_site_rule.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_rule_options_site_year.csv"),
  notes = "One row per population site x rule option (r0/r1/r2_g3/r2_g7/r2_g14/r3_m0/r3_m3): n_passing_years, the >=5-year gate (rule 4), and a >=10-year 'trend site' flag."
)

kept_sites_by_rule <- function(r) site_rule_option$site_id[site_rule_option$rule == r & site_rule_option$ge5]
kept_site_years_by_rule <- function(r) {
  kept <- kept_sites_by_rule(r)
  sum(site_year_rule_options[[paste0("pass_", r)]] & site_year_rule_options$site_id %in% kept)
}
trend_sites_by_rule <- function(r) site_rule_option$site_id[site_rule_option$rule == r & site_rule_option$ge10_trend]

sites_kept_r0 <- kept_sites_by_rule("r0")
stopifnot(setequal(sites_kept_r0, sites_kept_i)) # r0 must reproduce Step 3's manuscript rule exactly

non_gsl62_lr_ids <- long_record_pop$site_id[!long_record_pop$in_gsl62]
stopifnot(length(non_gsl62_lr_ids) == 37L)

rule_option_summary <- purrr::map_dfr(sub("^pass_", "", rule_cols), function(r) {
  kept <- kept_sites_by_rule(r)
  recovered_lr <- intersect(kept, non_gsl62_lr_ids)
  still_lost_lr <- setdiff(non_gsl62_lr_ids, kept)
  tibble(
    rule = r,
    sites_kept = length(kept),
    site_years_kept = kept_site_years_by_rule(r),
    trend_sites_kept = length(trend_sites_by_rule(r)),
    non_gsl62_long_record_recovered = length(recovered_lr),
    non_gsl62_long_record_recovered_list = paste(sort(recovered_lr), collapse = "; "),
    non_gsl62_long_record_still_lost = length(still_lost_lr),
    non_gsl62_long_record_still_lost_list = paste(sort(still_lost_lr), collapse = "; ")
  )
})

write_csv(rule_option_summary, file.path(TABLES_DIR, "selection_audit_rule_options_summary.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_rule_options_summary.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_rule_options_site_rule.csv"),
  notes = "sites_kept/site_years_kept/trend_sites_kept are gated on >=5 / >=10 passing years respectively. non_gsl62_long_record_* is relative to the fixed 37-site population of long-record sites not already in the GSL 62 (selection_audit_population.csv)."
)
message("Saved: selection_audit_rule_options_summary.csv")
print(rule_option_summary |> select(rule, sites_kept, site_years_kept, trend_sites_kept,
                                     non_gsl62_long_record_recovered, non_gsl62_long_record_still_lost))

## ---- Reasons for long-record sites still lost under every rule tried
## (loosest = r3_m3 and r2_g14 -- report both since neither strictly nests
## the other).
still_lost_everywhere <- Reduce(intersect, list(
  setdiff(non_gsl62_lr_ids, kept_sites_by_rule("r2_g14")),
  setdiff(non_gsl62_lr_ids, kept_sites_by_rule("r3_m3"))
))

still_lost_reason <- purrr::map_dfr(still_lost_everywhere, function(sid) {
  n_r1  <- site_rule_option$n_passing_years[site_rule_option$site_id == sid & site_rule_option$rule == "r1"]
  n_g14 <- site_rule_option$n_passing_years[site_rule_option$site_id == sid & site_rule_option$rule == "r2_g14"]
  n_m3  <- site_rule_option$n_passing_years[site_rule_option$site_id == sid & site_rule_option$rule == "r3_m3"]
  tibble(
    site_id = sid, n_passing_years_r1 = n_r1, n_passing_years_r2_g14 = n_g14, n_passing_years_r3_m3 = n_m3,
    reason = sprintf(
      "still < %d passing years even with the VUT/CUT fallback and the loosest tolerance tried (R1: %d yr; R2 gap<=14d: %d yr; R3 M<=3: %d yr)",
      RULE_MIN_YEARS, n_r1, n_g14, n_m3
    )
  )
})

write_csv(still_lost_reason, file.path(TABLES_DIR, "selection_audit_rule_options_still_lost.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_rule_options_still_lost.csv"),
  input_sources = file.path(TABLES_DIR, "selection_audit_rule_options_site_rule.csv"),
  notes = "Non-GSL-62 long-record sites that are NOT recovered (not kept at >=5 passing years) under EITHER of the two loosest rules tried (r2_g14, r3_m3)."
)
message("Saved: selection_audit_rule_options_still_lost.csv -- ", length(still_lost_everywhere),
        " long-record site(s) still lost under every rule tried: ",
        paste(sort(still_lost_everywhere), collapse = ", "))

## ============================================================================
## Step 9: confirm the GSL 62's own 505 site-years still pass every rule
## (each rule must CONTAIN the manuscript's set -- the prompt's explicit
## check, not assumed from the rules' nesting).
## ============================================================================

gsl62_expanded <- GSL_SITES |>
  select(site_id, years) |>
  tidyr::unnest(years) |>
  rename(year = years)
stopifnot(nrow(gsl62_expanded) == 505L)
stopifnot(all(gsl62_expanded$site_id %in% population$site_id))

gsl62_check <- gsl62_expanded |>
  left_join(site_year_rule_options, by = c("site_id", "year"))

gsl62_check_summary <- tibble(
  rule = sub("^pass_", "", rule_cols),
  n_site_years_checked = 505L,
  n_passing = purrr::map_int(rule_cols, function(rc) sum(gsl62_check[[rc]], na.rm = TRUE)),
  n_missing_or_failing = purrr::map_int(rule_cols, function(rc) sum(is.na(gsl62_check[[rc]]) | !gsl62_check[[rc]]))
)

write_csv(gsl62_check_summary, file.path(TABLES_DIR, "selection_audit_rule_options_gsl62_check.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_rule_options_gsl62_check.csv"),
  input_sources = c(file.path(TABLES_DIR, "selection_audit_rule_options_site_year.csv")),
  notes = "Confirms all 505 GSL-62 site-years (docs/framework.md's Sites table, via 00_config.R::GSL_SITES) pass each rule option -- i.e. each rule's kept set contains the manuscript's. n_missing_or_failing should be 0 for every rule; if not, this audit's SESSION_LOG entry says so explicitly."
)
message("GSL-62 containment check (all should be 0 missing/failing): ")
print(gsl62_check_summary)
if (any(gsl62_check_summary$n_missing_or_failing > 0)) {
  bad_rules <- gsl62_check_summary$rule[gsl62_check_summary$n_missing_or_failing > 0]
  warning("GSL-62 containment FAILS for rule(s): ", paste(bad_rules, collapse = ", "),
          " -- see selection_audit_rule_options_gsl62_check.csv and investigate before trusting any recommendation.")
}

## ============================================================================
## Step 10: rule 5 (negative GPP) check for every site newly added (relative
## to the manuscript rule, sites_kept_i/sites_kept_r0) under ANY of the R1-R3
## rules, using that site's own fallback GPP column.
## ============================================================================

newly_added_any_rule <- sort(unique(unlist(lapply(
  sub("^pass_", "", rule_cols)[-1], # all except r0
  function(r) setdiff(kept_sites_by_rule(r), sites_kept_r0)
))))

neggpp_check_ro <- pop_daily |>
  left_join(qc_fallback |> select(site_id, gpp_col_used), by = "site_id") |>
  filter(site_id %in% newly_added_any_rule) |>
  mutate(gpp_val = ifelse(gpp_col_used == "GPP_NT_VUT_REF", GPP_NT_VUT_REF, GPP_NT_CUT_REF)) |>
  group_by(site_id) |>
  summarise(
    gpp_col_used = first(gpp_col_used),
    n_days_gpp_present = sum(!is.na(gpp_val)),
    n_days_gpp_below_neg1 = sum(gpp_val < RULE_NEGGPP_VAL, na.rm = TRUE),
    share_gpp_below_neg1 = ifelse(n_days_gpp_present > 0, n_days_gpp_below_neg1 / n_days_gpp_present, NA_real_),
    fails_rule5 = share_gpp_below_neg1 > RULE_NEGGPP_SHR,
    .groups = "drop"
  )

write_csv(neggpp_check_ro, file.path(TABLES_DIR, "selection_audit_rule_options_rule5_check.csv"))
write_meta(
  file.path(TABLES_DIR, "selection_audit_rule_options_rule5_check.csv"),
  input_sources = "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')",
  notes = paste0(
    "Rule 5 (>10% of daily GPP < -1 gC/m2/d) checked over every available daily row, using each ",
    "site's own fallback GPP column (GPP_NT_VUT_REF or GPP_NT_CUT_REF, per qc_fallback), for every ",
    "site newly kept under any of R1/R2/R3 that was not already kept under the manuscript rule (R0). ",
    if (isTRUE(any(neggpp_check_ro$fails_rule5, na.rm = TRUE)))
      paste0(sum(neggpp_check_ro$fails_rule5, na.rm = TRUE), " site(s) WOULD fail rule 5.")
    else "No newly-added site fails rule 5.",
    if (any(is.na(neggpp_check_ro$fails_rule5)))
      paste0(" ", sum(is.na(neggpp_check_ro$fails_rule5)), " site(s) have no GPP data at all, so rule 5 is undetermined (NA) for them: ",
             paste(neggpp_check_ro$site_id[is.na(neggpp_check_ro$fails_rule5)], collapse = ", "), ".")
    else ""
  )
)
message("Saved: selection_audit_rule_options_rule5_check.csv -- ", length(newly_added_any_rule),
        " newly-added site(s) checked; ", sum(neggpp_check_ro$fails_rule5, na.rm = TRUE), " would fail rule 5.")
print(neggpp_check_ro)

message("--- 11_selection_audit.R: done (incl. 2026-10-30 selection-rule-options extension) ---")
