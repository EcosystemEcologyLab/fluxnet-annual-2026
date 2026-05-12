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
## Presence definition: regenerated 2026-05-12. A site-year is "present" if
## at least one of 12 flux variable columns (NEE VUT+CUT, GPP NT+DT × VUT+CUT,
## RECO NT+DT × VUT+CUT, LE_F_MDS, H_F_MDS) has a non-NA value in at least
## one month of that year. No QC filtering. No snapshot fallback.
##
## Usage:
##   Rscript scripts/authorship_models.R            # Step 1 — schema verification only
##   Rscript scripts/authorship_models.R --compute  # Step 1 + Step 2 — full computation
##
## Inputs:
##   data/snapshots/site_year_data_presence.csv              — yearly data presence
##   data/snapshots/site_candidates_full.csv                 — site metadata / network
##   data/snapshots/fluxnet_shuttle_snapshot_20260428T231049.csv — master manifest
##
## Outputs (Step 2 only, in outputs/authorship/):
##   site_authors.csv                — one row per site with n_invited_authors
##   authors_by_network.csv          — one row per submitting network, totals
##   authors_by_network.png          — bar chart of invited authors by network
##   diagnostics/sites_with_no_presence.csv  — sites with zero presence (auditable)
##   followup_tasks.txt              — logged decisions requiring future action
##   *.meta.json companion for every CSV and PNG

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

LATENCY_THRESHOLDS <- c(3, 2)

#' Map years_of_data to rubric row index (1–5).
#'
#' Bins: ≤5 → 1, 6–10 → 2, 11–15 → 3, 16–20 → 4, ≥21 → 5.
#' Exactly 5 years maps to row 1 (the "<5" tier). See DECISION-A.
#'
#' @param years_data Integer vector. Years with n_months_present > 0 per site.
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
#' @param latency_years Numeric vector. REFERENCE_YEAR minus max data year.
#' @return Integer vector of column indices (1–3).
latency_col <- function(latency_years) {
  as.integer(rowSums(outer(latency_years, LATENCY_THRESHOLDS, `<=`)) + 1L)
}

#' Look up invited-author count from the locked rubric table.
#'
#' @param years_data Integer vector. Years with n_months_present > 0 per site.
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
  message(sprintf("     %-20s  %s", col, class(pres[[col]])[1]))
}
message(sprintf("   n_months_present > 0:  %d rows", sum(pres$n_months_present > 0, na.rm = TRUE)))
message(sprintf("   n_months_present = 0:  %d rows", sum(pres$n_months_present == 0L, na.rm = TRUE)))
message(sprintf("   Unique sites:          %d", n_distinct(pres$site_id)))
message(sprintf("   Year range:            %d–%d",
                min(pres$year, na.rm = TRUE), max(pres$year, na.rm = TRUE)))

# Deduplicated per-site summary for Step 1 reporting
pres_dedup <- distinct(pres, site_id, year, n_months_present)

site_summary <- pres_dedup |>
  group_by(site_id) |>
  summarise(
    years_of_data = sum(n_months_present > 0L),
    max_year      = if (any(n_months_present > 0L))
                      max(year[n_months_present > 0L]) else NA_integer_,
    .groups       = "drop"
  ) |>
  filter(years_of_data > 0L) |>
  mutate(latency = as.integer(REFERENCE_YEAR) - as.integer(max_year))

message(sprintf("\n   Sites with ≥1 data year: %d / %d",
                nrow(site_summary), n_distinct(pres$site_id)))
message("\n   Distribution of years_of_data:")
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

# ── Apr-28 snapshot network fields ────────────────────────────────────────────

message("\n── Apr-28 snapshot — network-related fields ─────────────────────────────")
snap |>
  count(product_source_network, sort = TRUE) |>
  mutate(label = sprintf("     %-6s  n=%d", product_source_network, n)) |>
  pull(label) |>
  walk(message)

# ── Site coverage across all inputs ───────────────────────────────────────────

snap_ids  <- unique(snap$site_id)
pres_ids  <- unique(pres$site_id)

missing_from_pres <- setdiff(snap_ids, pres_ids)

message("\n── Site coverage across inputs ──────────────────────────────────────────")
message(sprintf("   Apr-28 snapshot:         %d sites", length(snap_ids)))
message(sprintf("   site_year_data_presence: %d sites  (%d snapshot sites absent)",
                length(pres_ids), length(missing_from_pres)))
if (length(missing_from_pres) > 0) {
  message("   WARNING: snapshot sites absent from presence file:")
  snap |>
    filter(site_id %in% missing_from_pres) |>
    select(site_id, product_source_network) |>
    distinct() |>
    mutate(label = sprintf("     %s  [%s]", site_id, product_source_network)) |>
    pull(label) |>
    walk(message)
} else {
  message("   All 716 snapshot sites are in the presence file. No fallback needed.")
}

# Sites in presence file with zero data years (n_months_present = 0 for all years)
sites_zero_presence <- pres_dedup |>
  group_by(site_id) |>
  summarise(max_months = max(n_months_present), .groups = "drop") |>
  filter(max_months == 0L) |>
  pull(site_id)
message(sprintf("\n   Sites in presence file with ALL n_months_present = 0: %d",
                length(sites_zero_presence)))
if (length(sites_zero_presence) > 0) {
  message("   These sites will be logged to diagnostics/sites_with_no_presence.csv.")
  walk(sites_zero_presence, \(s) message(sprintf("     %s", s)))
}


# ══════════════════════════════════════════════════════════════════════════════
# DECISIONS NEEDED BEFORE STEP 2
# ══════════════════════════════════════════════════════════════════════════════

message("\n=== DECISIONS NEEDED BEFORE STEP 2 =====================================\n")

message("DECISION-A  [OPEN] Rubric row for exactly 5 years of data")
message("  The rubric labels '<5 years' and '6–10 years' leave a gap at 5.")
message(sprintf("  There are %d sites with exactly 5 years of data.",
                sum(site_summary$years_of_data == 5L, na.rm = TRUE)))
message("  Current code: 5 years → row 1 ('<5' tier), i.e. the lower author count.")
message("  Deferred at 2026-05-07 team meeting — resolve before sending invitations.\n")

message("DECISION-B  [CONFIRMED] Network field: product_source_network from snapshot")
message("  Using product_source_network (9 codes: AMF EUF ICOS JPF TERN CNF KOF FLX SAEON).\n")

message("DECISION-C  [RESOLVED] All 716 snapshot sites are in the presence file.")
message("  Regenerated 2026-05-12 with multi-variable definition. No fallback used.\n")

message("DECISION-D  [RESOLVED] Network join from snapshot covers all 716 sites.\n")

message("DECISION-E  [RESOLVED] n_months_present available in presence file.")
message("  years_full_year (n_months_present = 12) and years_partial_year")
message("  (0 < n_months_present < 12) are now computed from the presence file.\n")

# ── Stop unless --compute passed ──────────────────────────────────────────────

if (!COMPUTE) {
  message("── Schema verification complete ──────────────────────────────────────────")
  message("Review the DECISIONS NEEDED above, then re-run with --compute:")
  message("  Rscript scripts/authorship_models.R --compute\n")
  quit(save = "no", status = 0)
}


# ══════════════════════════════════════════════════════════════════════════════
# STEP 2 — COMPUTE AUTHORSHIP
# Decisions applied (as of 2026-05-12):
#   A: 5 years -> '<5' row (lower tier); label gap deferred
#   B: submitting_network = product_source_network from snapshot (9 codes)
#   C: All 716 sites in presence file — no snapshot fallback
#   D: Network join from snapshot covers all 716 sites
#   E: years_full_year / years_partial_year computed from n_months_present
# ══════════════════════════════════════════════════════════════════════════════

message("\n=== STEP 2: COMPUTING AUTHORSHIP ========================================\n")

if (!dir_exists(OUTPUT_DIR)) dir_create(OUTPUT_DIR, recurse = TRUE)
if (!dir_exists(file.path(OUTPUT_DIR, "diagnostics")))
  dir_create(file.path(OUTPUT_DIR, "diagnostics"), recurse = TRUE)

# ── Snapshot index (all 716 sites; source for network join) ───────────────────

snap_index <- snap |>
  select(site_id, product_source_network, first_year, last_year, igbp) |>
  distinct()

# ── Presence file: deduplicate (Decision E) ───────────────────────────────────

pres_dedup <- distinct(pres, site_id, year, n_months_present)

# ── Per-site summary from presence file ───────────────────────────────────────
#
# years_of_data:      count of years with n_months_present > 0
# years_full_year:    count of years with n_months_present = 12
# years_partial_year: count of years with 0 < n_months_present < 12
# max_year:           most recent year with n_months_present > 0
#
# Sites with zero data years across all months remain in the output with
# years_of_data = 0 and presence_status = "no_presence_data" for audit.

site_summary_full <- pres_dedup |>
  group_by(site_id) |>
  summarise(
    years_of_data      = as.integer(sum(n_months_present > 0L)),
    years_full_year    = as.integer(sum(n_months_present == 12L)),
    years_partial_year = as.integer(sum(n_months_present > 0L & n_months_present < 12L)),
    max_year           = if (any(n_months_present > 0L))
                           max(year[n_months_present > 0L]) else NA_integer_,
    .groups            = "drop"
  ) |>
  mutate(
    presence_status = if_else(years_of_data > 0L, "verified", "no_presence_data")
  )

n_no_presence <- sum(site_summary_full$presence_status == "no_presence_data")
n_verified    <- sum(site_summary_full$presence_status == "verified")
message(sprintf("Presence summary from file: %d verified, %d with no presence data.",
                n_verified, n_no_presence))

# ── Join network from snapshot; compute latency ───────────────────────────────

site_data <- snap_index |>
  select(site_id, product_source_network) |>
  inner_join(site_summary_full, by = "site_id") |>
  rename(submitting_network = product_source_network) |>
  mutate(
    latency_years = if_else(
      !is.na(max_year),
      as.integer(REFERENCE_YEAR) - as.integer(max_year),
      NA_integer_
    )
  )

message(sprintf("Total sites entering rubric computation: %d", nrow(site_data)))

# ── Log and report sites with no presence data ────────────────────────────────

no_presence_sites <- site_data |>
  filter(presence_status == "no_presence_data") |>
  left_join(snap_index |> select(site_id, igbp, first_year, last_year),
            by = "site_id")

no_presence_path <- file.path(OUTPUT_DIR, "diagnostics", "sites_with_no_presence.csv")
write_csv(no_presence_sites, no_presence_path)
write_output_metadata(
  no_presence_path,
  input_sources = c(PRESENCE_FILE, SNAPSHOT_FILE),
  notes = paste0(
    "Sites in the snapshot whose presence file shows n_months_present = 0 for all years. ",
    "These sites have no non-NA values in any of the 12 flux variables across any month. ",
    "They are retained in site_authors.csv with n_invited_authors = NA and ",
    "presence_status = 'no_presence_data' rather than silently excluded. ",
    "Decision on how to treat these sites is required before finalising the author list."
  )
)
if (nrow(no_presence_sites) > 0) {
  message(sprintf("WARNING: %d site(s) have no presence data — logged to %s.",
                  nrow(no_presence_sites), no_presence_path))
  no_presence_sites |>
    mutate(label = sprintf("  %s [%s]  IGBP=%s  years=%s-%s",
                           site_id, submitting_network, igbp, first_year, last_year)) |>
    pull(label) |>
    walk(message)
} else {
  message(sprintf("sites_with_no_presence.csv: 0 rows (all sites have presence data). Written: %s",
                  no_presence_path))
}

# ── Apply rubric (verified sites only) ────────────────────────────────────────
# Decision A: exactly 5 years -> row 1 ('<5' tier). Label gap deferred.

site_authors <- site_data |>
  mutate(
    n_invited_authors = if_else(
      presence_status == "verified",
      apply_rubric(years_of_data, latency_years),
      NA_integer_
    )
  )

n_five_yr <- sum(site_authors$years_of_data == 5L &
                   site_authors$presence_status == "verified", na.rm = TRUE)
if (n_five_yr > 0) {
  message(sprintf(
    "NOTE (Decision-A): %d site(s) with exactly 5 years mapped to '<5' row.",
    n_five_yr
  ))
}

# ── Output: site_authors.csv ──────────────────────────────────────────────────

site_out <- site_authors |>
  select(
    site_id, submitting_network, presence_status,
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
    "Presence definition (2026-05-12): any non-NA value in NEE_VUT_REF, NEE_CUT_REF, ",
    "GPP_NT/DT_VUT/CUT_REF, RECO_NT/DT_VUT/CUT_REF, LE_F_MDS, H_F_MDS per month. ",
    "No QC filtering. No snapshot fallback — all 716 sites have presence-file data. ",
    "years_of_data: count of years with n_months_present > 0. ",
    "years_full_year: years with n_months_present = 12. ",
    "years_partial_year: years with 0 < n_months_present < 12. ",
    "Exactly 5 years -> '<5' rubric row (lower tier) — Decision-A, label gap deferred. ",
    "submitting_network = product_source_network from snapshot (9 codes). ",
    "presence_status: 'verified' = years_of_data > 0; 'no_presence_data' = all months zero."
  )
)
message(sprintf("Written: %s", site_out_path))

# ── Output: authors_by_network.csv ────────────────────────────────────────────

network_out <- site_authors |>
  filter(presence_status == "verified") |>
  group_by(submitting_network) |>
  summarise(
    n_sites               = n(),
    total_invited_authors = sum(n_invited_authors, na.rm = TRUE),
    .groups               = "drop"
  ) |>
  arrange(desc(n_sites))

# Add any networks that only have no_presence sites (so they appear in output)
no_pres_net <- site_authors |>
  filter(presence_status == "no_presence_data") |>
  group_by(submitting_network) |>
  summarise(n_sites_no_pres = n(), .groups = "drop")

if (nrow(no_pres_net) > 0) {
  network_out <- network_out |>
    full_join(no_pres_net, by = "submitting_network") |>
    mutate(
      n_sites               = coalesce(n_sites, 0L) + coalesce(n_sites_no_pres, 0L),
      total_invited_authors = coalesce(total_invited_authors, 0L)
    ) |>
    select(-n_sites_no_pres) |>
    arrange(desc(n_sites))
}

network_out_path <- file.path(OUTPUT_DIR, "authors_by_network.csv")
write_csv(network_out, network_out_path)
write_output_metadata(
  network_out_path,
  input_sources = c(PRESENCE_FILE, SNAPSHOT_FILE),
  notes = paste0(
    "Grouped by product_source_network (9 codes) from Apr-28 snapshot. ",
    "total_invited_authors excludes sites with presence_status = 'no_presence_data'. ",
    "See site_authors.meta.json for full decision notes."
  )
)
message(sprintf("Written: %s", network_out_path))

# ── Console summary ───────────────────────────────────────────────────────────

total_authors <- sum(site_authors$n_invited_authors, na.rm = TRUE)

message("\n-- Invited author count -------------------------------------------------")
message(sprintf("   Total:   %d invited authors (%d verified sites)",
                total_authors, n_verified))
if (n_no_presence > 0) {
  message(sprintf("   Sites with no presence data: %d (n_invited_authors = NA)",
                  n_no_presence))
}

message("\n   By network (sorted by site count):")
message(sprintf("     %-6s  %-10s  %6s", "Net", "n_sites", "authors"))
network_out |>
  mutate(label = sprintf("     %-6s  %3d sites    %6d",
                         submitting_network, n_sites, total_invited_authors)) |>
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
      "%d sites from Apr-28 2026 FLUXNET Shuttle snapshot. Reference year: %d.",
      nrow(site_authors), REFERENCE_YEAR
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
    "Broader presence definition 2026-05-12. ",
    "base_size=24, 14x7 inches, 300 dpi."
  )
)
message(sprintf("Written: %s", plot_path))

# ── Followup tasks ────────────────────────────────────────────────────────────

followup_tasks <- c(
  paste0(
    "[FOLLOWUP-A]  Resolve rubric row label for exactly 5 years of data. ",
    "Labels '<5 years' and '6-10 years' leave a gap; current code assigns ",
    "5 years -> '<5' row (lower author count). Affects ", n_five_yr, " site(s). ",
    "Decision deferred at 2026-05-07 team meeting."
  )
)

if (n_no_presence > 0) {
  followup_tasks <- c(
    followup_tasks,
    paste0(
      "[FOLLOWUP-NO-PRES]  Decide how to handle ", n_no_presence, " site(s) with ",
      "no non-NA values across all 12 flux variables and all months. ",
      "See diagnostics/sites_with_no_presence.csv for full detail. ",
      "Options: (1) exclude and log; (2) use snapshot span for years_of_data; ",
      "(3) investigate per-site extracted CSVs for data completeness."
    )
  )
}

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
  "\nauthorship_models.R complete: %d sites processed (%d verified, %d no presence data).",
  nrow(site_authors), n_verified, n_no_presence
))
