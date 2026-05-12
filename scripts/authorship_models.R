## scripts/authorship_models.R
##
## Compute invited-author counts for the FLUXNET Annual Paper 2026 using the
## locked authorship rubric. The rubric assigns a per-site author count based
## on (1) years of data contributed and (2) latency from 2026 to the site's
## most recent data year.
##
## Rubric locked: 2026-05-07 (team meeting decision).
## Three latency tiers (>3 yr / ≤3 yr / ≤2 yr) × five data-volume bins
## (≤5 / 6–10 / 11–15 / 16–20 / ≥21 years), fully monotone, author counts 2–8.
##
## Usage:
##   Rscript scripts/authorship_models.R            # Step 1 — schema verification only
##   Rscript scripts/authorship_models.R --compute  # Step 1 + Step 2 — full computation
##
## Step 1 (always runs): reads both input files, reports schema and site
##   coverage, flags decisions requiring human sign-off, then stops.
## Step 2 (requires --compute): computes author counts and writes all outputs
##   to outputs/authorship/. Do NOT pass --compute until Step 1 output has
##   been reviewed and confirmed.
##
## Inputs:
##   data/snapshots/site_year_data_presence.csv              — yearly data presence
##   data/snapshots/site_candidates_full.csv                 — site metadata / network
##   data/snapshots/fluxnet_shuttle_snapshot_20260428T231049.csv — master manifest
##
## Outputs (Step 2 only, in outputs/authorship/):
##   site_authors.csv            — one row per site with n_invited_authors
##   authors_by_network.csv      — one row per submitting network, totals
##   authors_by_network.png      — bar chart of invited authors by network
##   followup_tasks.txt          — logged decisions requiring future action
##   *.meta.json companion for every CSV and PNG
##   (data_coverage_summary.csv omitted: requires month counts — see FOLLOWUP-E2)

# ── Startup ──────────────────────────────────────────────────────────────────

if (file.exists(".env")) {
  if (!requireNamespace("dotenv", quietly = TRUE)) {
    stop("Package 'dotenv' required for local use. Install: install.packages('dotenv')")
  }
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")
check_pipeline_config()

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(fs)

args    <- commandArgs(trailingOnly = TRUE)
COMPUTE <- "--compute" %in% args

# ── Configuration ─────────────────────────────────────────────────────────────

REFERENCE_YEAR  <- 2026L
PRESENCE_FILE   <- "data/snapshots/site_year_data_presence.csv"
CANDIDATES_FILE <- "data/snapshots/site_candidates_full.csv"
SNAPSHOT_FILE   <- "data/snapshots/fluxnet_shuttle_snapshot_20260428T231049.csv"
OUTPUT_DIR      <- "outputs/authorship"
SCRIPT_NAME     <- "authorship_models.R"

# ── Rubric definition ─────────────────────────────────────────────────────────
#
# Locked 2026-05-07 (team meeting). Fully monotone 5 × 3 matrix.
#
# Row index (row_bin) — data-volume bins:
#   1 = ≤5 years   (rubric label "<5 years"; exactly 5 mapped here — DECISION-A)
#   2 = 6–10 years
#   3 = 11–15 years
#   4 = 16–20 years
#   5 = ≥21 years
#
# Column index (latency_col) — latency tiers, thresholds c(3, 2) descending:
#   col 1 = >3 yrs latency   (2026 − max_data_year > 3)
#   col 2 = ≤3 yrs latency
#   col 3 = ≤2 yrs latency

RUBRIC_TABLE <- matrix(
  c(2L, 3L, 4L,
    3L, 4L, 5L,
    4L, 5L, 6L,
    5L, 6L, 7L,
    6L, 7L, 8L),
  nrow = 5L, ncol = 3L, byrow = TRUE
)

# Latency tier thresholds (strictly descending). col = count of thresholds
# that latency_years is ≤ to, plus 1.
LATENCY_THRESHOLDS <- c(3, 2)

#' Map years_of_data to rubric row index (1–5).
#'
#' Bins: ≤5 → 1, 6–10 → 2, 11–15 → 3, 16–20 → 4, ≥21 → 5.
#' Exactly 5 years maps to row 1 (the "<5" tier). See DECISION-A in
#' Step 1 schema output for discussion.
#'
#' @param years_data Integer vector. Distinct years with any data per site.
#' @return Integer vector of row indices (1–5).
row_bin <- function(years_data) {
  dplyr::case_when(
    years_data <= 5L  ~ 1L,
    years_data <= 10L ~ 2L,
    years_data <= 15L ~ 3L,
    years_data <= 20L ~ 4L,
    TRUE              ~ 5L
  )
}

#' Map latency_years to rubric column index.
#'
#' Counts how many entries in LATENCY_THRESHOLDS the value is ≤ to, then
#' adds 1. Yields col 1 (highest latency) through col 3 (most recent).
#' Vectorised via [outer()].
#'
#' @param latency_years Numeric vector. REFERENCE_YEAR minus max data year.
#' @return Integer vector of column indices (1–3).
latency_col <- function(latency_years) {
  as.integer(rowSums(outer(latency_years, LATENCY_THRESHOLDS, `<=`)) + 1L)
}

#' Look up invited-author count from the locked rubric table.
#'
#' Vectorised over `years_data` and `latency_years`. Both vectors must be
#' the same length (or one of length 1 for scalar recycling).
#'
#' @param years_data Integer vector. Distinct years with any data per site.
#' @param latency_years Numeric vector. REFERENCE_YEAR minus max data year.
#' @return Integer vector of invited-author counts (2–8).
apply_rubric <- function(years_data, latency_years) {
  r <- row_bin(years_data)
  c <- latency_col(latency_years)
  RUBRIC_TABLE[cbind(r, c)]
}


# ══════════════════════════════════════════════════════════════════════════════
# STEP 1 — SCHEMA VERIFICATION
# ══════════════════════════════════════════════════════════════════════════════

message("\n=== STEP 1: SCHEMA VERIFICATION ===\n")

# ── Read inputs ───────────────────────────────────────────────────────────────

pres  <- read_csv(PRESENCE_FILE,   show_col_types = FALSE)
cands <- read_csv(CANDIDATES_FILE, show_col_types = FALSE)
snap  <- read_csv(SNAPSHOT_FILE,   show_col_types = FALSE)

# ── site_year_data_presence.csv ───────────────────────────────────────────────

message("── site_year_data_presence.csv ──────────────────────────────────────────")
message(sprintf("   Rows: %d   Cols: %d", nrow(pres), ncol(pres)))
message("   Columns and types:")
for (col in names(pres)) {
  message(sprintf("     %-12s  %s", col, class(pres[[col]])[1]))
}
message(sprintf("   has_data TRUE:  %d rows", sum(pres$has_data, na.rm = TRUE)))
message(sprintf("   has_data FALSE: %d rows", sum(!pres$has_data, na.rm = TRUE)))
message(sprintf("   Unique sites:   %d", length(unique(pres$site_id))))
message(sprintf("   Year range:     %d–%d",
                min(pres$year, na.rm = TRUE), max(pres$year, na.rm = TRUE)))

# Duplicates
dup_siteyr <- pres |>
  group_by(site_id, year) |>
  filter(n() > 1) |>
  ungroup()
n_dup_pairs <- nrow(distinct(dup_siteyr, site_id, year))
n_dup_sites <- n_distinct(dup_siteyr$site_id)
# Are duplicates consistent (same has_data value)?
dup_consistent <- dup_siteyr |>
  group_by(site_id, year) |>
  summarise(n_distinct_vals = n_distinct(has_data), .groups = "drop") |>
  filter(n_distinct_vals > 1)
message(sprintf("\n   Duplicate site-year rows: %d pairs duplicated, %d sites affected",
                n_dup_pairs, n_dup_sites))
if (nrow(dup_consistent) == 0) {
  message("   All duplicates are exact (consistent has_data) — safe to deduplicate.")
} else {
  message(sprintf("   WARNING: %d site-years have CONFLICTING has_data values across duplicate rows.",
                  nrow(dup_consistent)))
}

# Deduplicated per-site summary
pres_dedup <- distinct(pres, site_id, year, has_data)
site_summary <- pres_dedup |>
  filter(has_data) |>
  group_by(site_id) |>
  summarise(
    years_of_data = n_distinct(year),
    max_year      = max(year),
    .groups       = "drop"
  ) |>
  mutate(latency = as.integer(REFERENCE_YEAR) - max_year)

message(sprintf("\n   After deduplication: %d unique site-year rows", nrow(pres_dedup)))
message(sprintf("   Sites with ≥1 data year: %d", nrow(site_summary)))
message("\n   Distribution of years_of_data (presence-file sites with data):")
site_summary |>
  count(years_of_data) |>
  arrange(years_of_data) |>
  mutate(label = sprintf("     %3d yr(s): %d sites", years_of_data, n)) |>
  pull(label) |>
  walk(message)
message("\n   Latency distribution (2026 - max_data_year):")
site_summary |>
  count(latency) |>
  arrange(latency) |>
  mutate(label = sprintf("     latency %2d: %d sites", latency, n)) |>
  pull(label) |>
  walk(message)

# ── site_candidates_full.csv ──────────────────────────────────────────────────

message("\n── site_candidates_full.csv ─────────────────────────────────────────────")
message(sprintf("   Rows: %d   Cols: %d", nrow(cands), ncol(cands)))
message("   All columns:")
for (col in names(cands)) {
  message(sprintf("     %-25s  %s", col, class(cands[[col]])[1]))
}

# Check for expected network column names
expected_net_cols <- c("network", "network_id", "submitting_network",
                       "data_provider", "data_hub", "product_source_network")
found_net_cols <- intersect(expected_net_cols, names(cands))
message("\n   Network-related column search:")
if (length(found_net_cols) == 0) {
  message("   None of the expected network column names found.")
} else {
  for (col in found_net_cols) {
    message(sprintf("     Found: '%s' (%d unique values)", col, n_distinct(cands[[col]])))
  }
}

message(sprintf("\n   'data_hub' unique values (%d):", n_distinct(cands$data_hub)))
cands |>
  count(data_hub, sort = TRUE) |>
  mutate(label = sprintf("     n=%3d  %s", n, data_hub)) |>
  pull(label) |>
  walk(message)

# ── Apr-28 snapshot network fields ────────────────────────────────────────────

message("\n── Apr-28 snapshot — network-related fields ─────────────────────────────")
message(sprintf("   data_hub (3 hub values):"))
snap |>
  count(data_hub, sort = TRUE) |>
  mutate(label = sprintf("     %-12s  n=%d", data_hub, n)) |>
  pull(label) |>
  walk(message)
message("   product_source_network (9 submission-network codes):")
snap |>
  count(product_source_network, sort = TRUE) |>
  mutate(label = sprintf("     %-6s  n=%d", product_source_network, n)) |>
  pull(label) |>
  walk(message)
message("   network: 51 unique semicolon-delimited multi-network strings")
message("     (identical values to site_candidates_full$data_hub for sites in both — confirmed)")

# ── Site coverage across all inputs ───────────────────────────────────────────

snap_ids  <- unique(snap$site_id)
pres_ids  <- unique(pres$site_id)
cands_ids <- unique(cands$site_id)

missing_from_pres  <- setdiff(snap_ids, pres_ids)
missing_from_cands <- setdiff(snap_ids, cands_ids)

message("\n── Site coverage across inputs ──────────────────────────────────────────")
message(sprintf("   Apr-28 snapshot:        %d sites", length(snap_ids)))
message(sprintf("   site_year_data_presence:%d sites  (%d snapshot sites absent)",
                length(pres_ids), length(missing_from_pres)))
message(sprintf("   site_candidates_full:   %d sites  (%d snapshot sites absent)",
                length(cands_ids), length(missing_from_cands)))

message(sprintf("\n   44 sites in snapshot NOT in presence file (all ICOS hub):"))
snap |>
  filter(site_id %in% missing_from_pres) |>
  select(site_id, product_source_network, data_hub) |>
  distinct() |>
  arrange(product_source_network, site_id) |>
  mutate(label = sprintf("     %s  [%s / hub=%s]",
                          site_id, product_source_network, data_hub)) |>
  pull(label) |>
  walk(message)

message(sprintf("\n   %d sites in snapshot NOT in site_candidates_full",
                length(missing_from_cands)))
message("   (All can be joined from the snapshot for product_source_network — see DECISION-D.)")

# ── Encoding summary ──────────────────────────────────────────────────────────

message("\n── Presence encoding summary ────────────────────────────────────────────")
message("   Granularity: ANNUAL — one row per site × year; has_data = TRUE / FALSE")
message("   No monthly breakdown exists in this file.")
message("   Indicator: NEE_VUT_REF presence (YY primary, MM ≥3-month fallback),")
message("   as generated by compute_site_year_presence() in R/utils.R.")
message("   'full year' vs 'partial year' classification is NOT computable from")
message("   this file without monthly-resolution data. See DECISION-E.")


# ══════════════════════════════════════════════════════════════════════════════
# DECISIONS NEEDED BEFORE STEP 2
# ══════════════════════════════════════════════════════════════════════════════

message("\n=== DECISIONS NEEDED BEFORE STEP 2 =====================================\n")

message("DECISION-A  Rubric row for exactly 5 years of data")
message("  The rubric labels '<5 years' and '6–10 years' leave a gap at 5.")
message(sprintf("  There are %d sites with exactly 5 years of data.",
                sum(site_summary$years_of_data == 5L, na.rm = TRUE)))
message("  Current code: 5 years → row 1 ('<5' tier), i.e. the lower author count.")
message("  This was deferred at the 2026-05-07 team meeting — resolve before final lock.\n")

message("DECISION-B  Network field for grouping and bar chart")
message("  site_candidates_full.csv has no column named 'network', 'network_id',")
message("  'data_provider', or 'submitting_network'. The only network-like column")
message("  is 'data_hub', which contains semicolon-delimited multi-network strings")
message("  (48 unique values; e.g. 'AmeriFlux;NEON;Phenocam', 'ChinaFLUX').")
message("  Two cleaner alternatives exist in the Apr-28 snapshot:")
message("    (i)  product_source_network — 9 codes, one per site, no semicolons:")
message("         AMF, EUF, ICOS, JPF, TERN, CNF, KOF, FLX, SAEON")
message("    (ii) data_hub — 3 hub values (AmeriFlux, ICOS, TERN) — too coarse")
message("  Recommendation: use product_source_network (option i) for the bar chart.")
message("  Confirm, or specify whether to use data_hub multi-strings (48 groups)")
message("  or a parsed primary-network column built from them.\n")

message("DECISION-C  44 sites absent from presence file")
message("  All 44 are ICOS-hub sites present in the Apr-28 snapshot but missing")
message("  from site_year_data_presence.csv. Step 2 will log them as UNKNOWN")
message("  and exclude them from author counts.")
message("  Confirm, or specify a fallback (e.g. use snapshot first_year/last_year).\n")

message("DECISION-D  147 sites absent from site_candidates_full")
message("  These sites have presence data but no row in site_candidates_full.")
message("  If Decision-B uses product_source_network from the snapshot, all 147")
message("  can be recovered via a snapshot join (0 sites left unmatched).")
message("  Confirm this approach.\n")

message("DECISION-E  Full-year vs partial-year classification")
message("  You requested tagging each site-year as 'full year' (12 months present)")
message("  or 'partial year' (1–11 months). site_year_data_presence.csv stores only")
message("  a per-year TRUE/FALSE — there is no month count in this file.")
message("  Options:")
message("    (i)  Regenerate the presence file with an n_months_present column")
message("         (requires re-running compute_site_year_presence on MM data)")
message("    (ii) Set years_full_year and years_partial_year to NA in all outputs")
message("    (iii) Proxy: first and last data year per site = partial; all others = full")
message("  Confirm preferred option. Note: the current presence indicator (NEE_VUT_REF)")
message("  may also differ from 'any variable for at least one month' — confirm scope.\n")

# ── Stop unless --compute passed ──────────────────────────────────────────────

if (!COMPUTE) {
  message("── Schema verification complete ──────────────────────────────────────────")
  message("Review the DECISIONS NEEDED above, then re-run with --compute:")
  message("  Rscript scripts/authorship_models.R --compute\n")
  quit(save = "no", status = 0)
}


# ══════════════════════════════════════════════════════════════════════════════
# STEP 2 — COMPUTE AUTHORSHIP
# Rubric locked 2026-05-07. Decisions applied:
#   A: 5 years -> '<5' row (lower tier); deferred label decision noted
#   B: submitting_network = product_source_network from snapshot (9 codes)
#   C: Sites absent from presence file OR with 0 NEE years -> snapshot
#      first_year/last_year fallback; presence_source column distinguishes sources
#   D: Network join from snapshot, not site_candidates_full -> full 716 coverage
#   E.1: has_data renamed nee_year_present internally; NEE-basis noted in metadata
#   E.2: years_full_year/years_partial_year = NA; data_coverage_summary dropped
# ══════════════════════════════════════════════════════════════════════════════

message("\n=== STEP 2: COMPUTING AUTHORSHIP ========================================\n")

if (!dir_exists(OUTPUT_DIR)) dir_create(OUTPUT_DIR, recurse = TRUE)

# ── Snapshot index (all 716 sites; source for network join + fallback) ─────────

snap_index <- snap |>
  select(site_id, product_source_network, first_year, last_year) |>
  distinct()

# ── Presence file: dedup and rename has_data -> nee_year_present (Decision E.1) -

pres_dedup <- distinct(pres, site_id, year, has_data) |>
  rename(nee_year_present = has_data)

# Per-site summary for sites with >=1 TRUE (NEE-present) year
site_from_pres <- pres_dedup |>
  filter(nee_year_present) |>
  group_by(site_id) |>
  summarise(
    years_of_data   = n_distinct(year),
    max_year        = max(year),
    .groups         = "drop"
  ) |>
  mutate(presence_source = "presence_file")

# ── Snapshot fallback for sites with 0 presence-file NEE years (Decision C) ────
#
# Two cases both receive presence_source = "snapshot_fallback":
#   (a) Sites entirely absent from the presence file (44 ICOS sites)
#   (b) Sites in the presence file whose every nee_year_present is FALSE
# years_of_data = last_year - first_year + 1 (span; may overestimate gap years).

site_from_snap <- snap_index |>
  anti_join(site_from_pres, by = "site_id") |>
  filter(!is.na(first_year) & !is.na(last_year)) |>
  mutate(
    years_of_data   = as.integer(last_year - first_year + 1L),
    max_year        = as.integer(last_year),
    presence_source = "snapshot_fallback"
  ) |>
  select(site_id, years_of_data, max_year, presence_source)

# Log any sites where snapshot also lacks first_year/last_year (edge case)
site_no_data <- snap_index |>
  anti_join(site_from_pres, by = "site_id") |>
  filter(is.na(first_year) | is.na(last_year)) |>
  pull(site_id)
if (length(site_no_data) > 0) {
  walk(site_no_data, \(sid) {
    log_unknown(
      record_id = sid,
      reason    = paste0(
        "Absent from presence file and snapshot first_year/last_year is NA; ",
        "cannot compute years_of_data or latency."
      ),
      logged_by = SCRIPT_NAME
    )
  })
  message(sprintf(
    "WARNING: %d site(s) have no usable presence data and are logged as UNKNOWN.",
    length(site_no_data)
  ))
}

n_fallback <- nrow(site_from_snap)
message(sprintf(
  "Presence sources: %d sites from presence file, %d using snapshot fallback.",
  nrow(site_from_pres), n_fallback
))

# ── Combine; compute latency; join network from snapshot (Decisions B + D) ─────

site_presence <- bind_rows(site_from_pres, site_from_snap) |>
  mutate(
    latency_years      = as.integer(REFERENCE_YEAR) - as.integer(max_year),
    years_full_year    = NA_integer_,   # Decision E.2: monthly data unavailable
    years_partial_year = NA_integer_    # Decision E.2: monthly data unavailable
  )

site_data <- snap_index |>
  select(site_id, product_source_network) |>
  inner_join(site_presence, by = "site_id") |>
  rename(submitting_network = product_source_network)

message(sprintf("Total sites entering rubric computation: %d", nrow(site_data)))

# ── Apply rubric ──────────────────────────────────────────────────────────────
# Decision A: exactly 5 years -> row 1 ('<5' tier). Label gap noted; deferred.

site_authors <- site_data |>
  mutate(n_invited_authors = apply_rubric(years_of_data, latency_years))

n_five_yr <- sum(site_authors$years_of_data == 5L, na.rm = TRUE)
if (n_five_yr > 0) {
  message(sprintf(
    "NOTE (Decision-A): %d site(s) with exactly 5 years mapped to '<5' row.",
    n_five_yr
  ))
}

# ── Output: site_authors.csv ──────────────────────────────────────────────────

site_out <- site_authors |>
  select(
    site_id, submitting_network, presence_source,
    years_of_data, years_full_year, years_partial_year,
    latency_years, n_invited_authors
  ) |>
  arrange(site_id)

site_out_path <- file.path(OUTPUT_DIR, "site_authors.csv")
write_csv(site_out, site_out_path)
write_output_metadata(
  site_out_path,
  input_sources = c(PRESENCE_FILE, SNAPSHOT_FILE),
  notes = paste0(
    "Locked authorship rubric (2026-05-07): 3 latency tiers x 5 data-volume bins, ",
    "fully monotone 5x3 matrix, author counts 2-8. Reference year: ", REFERENCE_YEAR, ". ",
    "years_of_data indicator: NEE_VUT_REF presence (nee_year_present), NOT all-variable -- ",
    "see followup task FOLLOWUP-E1. ",
    "Snapshot fallback used for ", n_fallback, " sites: ",
    "years_of_data = last_year - first_year + 1 (span; may overestimate gap years) -- ",
    "see followup task FOLLOWUP-C. ",
    "presence_source: 'presence_file' = >=1 NEE TRUE year; ",
    "'snapshot_fallback' = absent from presence file or 0 NEE TRUE years. ",
    "Exactly 5 years -> '<5' rubric row (lower tier) -- Decision-A, label gap deferred. ",
    "submitting_network = product_source_network from snapshot (9 codes). ",
    "years_full_year and years_partial_year are NA -- monthly data unavailable (FOLLOWUP-E2)."
  )
)
message(sprintf("Written: %s", site_out_path))

# ── Output: authors_by_network.csv ────────────────────────────────────────────

network_out <- site_authors |>
  group_by(submitting_network) |>
  summarise(
    n_sites               = n(),
    n_sites_pres_file     = sum(presence_source == "presence_file"),
    n_sites_snap_fallbk   = sum(presence_source == "snapshot_fallback"),
    total_invited_authors = sum(n_invited_authors, na.rm = TRUE),
    .groups               = "drop"
  ) |>
  arrange(desc(n_sites))

network_out_path <- file.path(OUTPUT_DIR, "authors_by_network.csv")
write_csv(network_out, network_out_path)
write_output_metadata(
  network_out_path,
  input_sources = c(PRESENCE_FILE, SNAPSHOT_FILE),
  notes = paste0(
    "Grouped by product_source_network (9 codes) from Apr-28 snapshot. ",
    "n_sites_snap_fallbk: sites in this network using snapshot years as fallback. ",
    "See site_authors.meta.json for full decision notes."
  )
)
message(sprintf("Written: %s", network_out_path))

# NOTE: data_coverage_summary.csv omitted (Decision E.2) -- requires month counts.

# ── Console summary ───────────────────────────────────────────────────────────

total_authors <- sum(site_authors$n_invited_authors, na.rm = TRUE)

message("\n-- Invited author count -------------------------------------------------")
message(sprintf("   Total:   %d invited authors", total_authors))

message("\n   By network (sorted by site count):")
message(sprintf("     %-6s  %-10s  %-12s  %6s",
                "Net", "n_sites", "fallback", "authors"))
network_out |>
  mutate(label = sprintf(
    "     %-6s  %3d sites    fallback=%2d    %6d",
    submitting_network, n_sites, n_sites_snap_fallbk, total_invited_authors
  )) |>
  pull(label) |>
  walk(message)

# ── Output: authors_by_network.png ────────────────────────────────────────────

plot_data <- network_out |>
  mutate(submitting_network = factor(
    submitting_network,
    levels = network_out$submitting_network[order(network_out$n_sites,
                                                   decreasing = TRUE)]
  ))

p <- ggplot(plot_data,
            aes(x = submitting_network, y = total_invited_authors)) +
  geom_col(fill = "#2171b5", width = 0.7) +
  labs(
    x        = "Submitting network (product_source_network)",
    y        = "Total invited authors",
    title    = "FLUXNET Annual Paper 2026 — Invited authors by network",
    subtitle = "Locked rubric (2026-05-07), sorted by site count descending",
    caption  = sprintf(
      "%d sites from Apr-28 2026 FLUXNET Shuttle snapshot (%d via snapshot fallback). Reference year: %d.",
      nrow(site_authors), n_fallback, REFERENCE_YEAR
    )
  ) +
  fluxnet_theme(base_size = 24) +
  theme(
    axis.text.x  = element_text(angle = 35, hjust = 1),
    plot.caption = element_text(size = 11, hjust = 0)
  )

plot_path <- file.path(OUTPUT_DIR, "authors_by_network.png")
ggsave(plot_path, plot = p, width = 14, height = 7, dpi = 300, bg = "white")
write_output_metadata(
  plot_path,
  input_sources = c(PRESENCE_FILE, SNAPSHOT_FILE),
  notes = paste0(
    "Bar chart: total invited authors by product_source_network (9 codes), ",
    "sorted by site count descending. Locked rubric 2026-05-07. ",
    "base_size=24, 14x7 inches, 300 dpi."
  )
)
message(sprintf("Written: %s", plot_path))

# ── Followup tasks ────────────────────────────────────────────────────────────

followup_tasks <- c(
  paste0(
    "[FOLLOWUP-C]  Regenerate site_year_data_presence.csv for all 716 Shuttle sites. ",
    n_fallback, " sites currently use snapshot first_year/last_year as fallback ",
    "(years_of_data = span, may overestimate). Re-run compute_site_year_presence() ",
    "after all sites are downloaded and extracted."
  ),
  paste0(
    "[FOLLOWUP-E1] Regenerate presence with all-variable indicator. ",
    "Current years_of_data is based on NEE_VUT_REF presence only. ",
    "Intended definition: 'any data of any variable for at least one month'. ",
    "Update compute_site_year_presence() or write a broader any-variable variant."
  ),
  paste0(
    "[FOLLOWUP-E2] Add month counts to presence file. ",
    "years_full_year (12 months) and years_partial_year (1-11 months) are NA. ",
    "Add n_months_present column to compute_site_year_presence() output, ",
    "then re-run this script to populate those columns."
  ),
  paste0(
    "[FOLLOWUP-A]  Resolve rubric row label for exactly 5 years of data. ",
    "Labels '<5 years' and '6-10 years' leave a gap; current code assigns ",
    "5 years -> '<5' row (lower author count). Affects ", n_five_yr, " site(s). ",
    "Decision deferred at 2026-05-07 team meeting."
  )
)

message("\n-- Followup tasks -------------------------------------------------------")
walk(followup_tasks, message)

followup_path <- file.path(OUTPUT_DIR, "followup_tasks.txt")
writeLines(
  c(sprintf("Followup tasks logged by authorship_models.R at %s",
            format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    "",
    followup_tasks),
  followup_path
)
message(sprintf("\nFollowup tasks written to: %s", followup_path))

# ── Completion ────────────────────────────────────────────────────────────────

message(sprintf(
  "\nauthorship_models.R complete: %d sites processed (%d presence file, %d snapshot fallback).",
  nrow(site_authors), nrow(site_from_pres), n_fallback
))
