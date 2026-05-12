## scripts/authorship_diagnostics.R
##
## Diagnostic analyses for the authorship outputs.
## Produces three diagnostic CSVs in outputs/authorship/diagnostics/.
## Does NOT modify any main authorship outputs.
##
## Run from repo root:
##   Rscript scripts/authorship_diagnostics.R
##
## Inputs (must exist — run authorship_models.R --compute first):
##   outputs/authorship/site_authors.csv
##   outputs/authorship/authors_by_network.csv
##   data/snapshots/site_year_data_presence.csv
##
## Outputs:
##   diagnostics/sites_no_presence_detail.csv    — per-site detail for sites with no presence data
##   diagnostics/sensitivity_5yr_in_upper_row.csv — totals under alt 5-yr row assignment
##   diagnostics/year_2025_availability_by_network.csv — 2025 data presence by network

if (file.exists(".env")) dotenv::load_dot_env()
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

library(dplyr)
library(readr)
library(purrr)
library(fs)

PRESENCE_FILE  <- "data/snapshots/site_year_data_presence.csv"
SNAPSHOT_FILE  <- "data/snapshots/fluxnet_shuttle_snapshot_20260428T231049.csv"
SITE_OUT_FILE  <- "outputs/authorship/site_authors.csv"
NET_OUT_FILE   <- "outputs/authorship/authors_by_network.csv"
DIAG_DIR       <- "outputs/authorship/diagnostics"
REFERENCE_YEAR <- 2026L
SCRIPT_NAME    <- "authorship_diagnostics.R"

if (!dir_exists(DIAG_DIR)) dir_create(DIAG_DIR, recurse = TRUE)

# ── Rubric (mirror of authorship_models.R) ─────────────────────────────────────
# Needed for the sensitivity analysis. Must stay in sync with the locked rubric.

RUBRIC_TABLE <- matrix(
  c(2L, 3L, 4L,
    3L, 4L, 5L,
    4L, 5L, 6L,
    5L, 6L, 7L,
    6L, 7L, 8L),
  nrow = 5L, ncol = 3L, byrow = TRUE
)
LATENCY_THRESHOLDS <- c(3, 2)

#' @param years_data Integer vector.
#' @param upper_five Logical. If TRUE, 5 years maps to row 2 (6-10 tier).
row_bin <- function(years_data, upper_five = FALSE) {
  if (upper_five) {
    dplyr::case_when(
      years_data <  5L  ~ 1L,
      years_data <= 10L ~ 2L,   # 5 goes into 6-10 row
      years_data <= 15L ~ 3L,
      years_data <= 20L ~ 4L,
      TRUE              ~ 5L
    )
  } else {
    dplyr::case_when(
      years_data <= 5L  ~ 1L,   # default: 5 -> <5 row
      years_data <= 10L ~ 2L,
      years_data <= 15L ~ 3L,
      years_data <= 20L ~ 4L,
      TRUE              ~ 5L
    )
  }
}

latency_col <- function(latency_years) {
  as.integer(rowSums(outer(latency_years, LATENCY_THRESHOLDS, `<=`)) + 1L)
}

apply_rubric <- function(years_data, latency_years, upper_five = FALSE) {
  r <- row_bin(years_data, upper_five = upper_five)
  c <- latency_col(latency_years)
  RUBRIC_TABLE[cbind(r, c)]
}

row_bin_label <- function(years_data, upper_five = FALSE) {
  dplyr::case_when(
    row_bin(years_data, upper_five) == 1L ~ "<5",
    row_bin(years_data, upper_five) == 2L ~ "6-10",
    row_bin(years_data, upper_five) == 3L ~ "11-15",
    row_bin(years_data, upper_five) == 4L ~ "16-20",
    row_bin(years_data, upper_five) == 5L ~ ">=21"
  )
}

# ── Load outputs ───────────────────────────────────────────────────────────────

site_out <- read_csv(SITE_OUT_FILE, show_col_types = FALSE)
net_out  <- read_csv(NET_OUT_FILE,  show_col_types = FALSE)
pres_raw <- read_csv(PRESENCE_FILE, show_col_types = FALSE)

# Deduplicate on site_id + year; keep n_months_present for all diagnostics
pres_dedup <- distinct(pres_raw, site_id, year, n_months_present)

message("Inputs loaded.")
message(sprintf(
  "  site_authors.csv: %d sites | presence file: %d site-year rows (%d sites)",
  nrow(site_out),
  nrow(pres_dedup),
  length(unique(pres_dedup$site_id))
))

# ══════════════════════════════════════════════════════════════════════════════
# DIAGNOSTIC 1 — Sites with no presence data
# ══════════════════════════════════════════════════════════════════════════════
# Under the broader all-variable presence definition (v2, 2026-05-12),
# all 716 sites have presence-file coverage. This diagnostic captures any
# sites that might have no presence data in future runs (e.g. if a site is
# added to the snapshot before its data has been processed).

message("\n=== DIAGNOSTIC 1: Sites with no presence data ===")

no_presence_sites <- site_out |>
  filter(presence_status == "no_presence_data") |>
  mutate(rubric_row = row_bin_label(years_of_data))

message(sprintf("Sites with no presence data: %d", nrow(no_presence_sites)))

no_presence_detail <- no_presence_sites |>
  select(
    site_id, submitting_network, presence_status,
    years_of_data, rubric_row, latency_years, n_invited_authors
  ) |>
  arrange(submitting_network, site_id)

no_pres_path <- file.path(DIAG_DIR, "sites_no_presence_detail.csv")
write_csv(no_presence_detail, no_pres_path)
write_output_metadata(
  no_pres_path,
  input_sources = c(SITE_OUT_FILE),
  notes = paste0(
    "Sites where presence_status = 'no_presence_data'. ",
    "As of 2026-05-12 regeneration (broader all-variable definition): 0 such sites. ",
    "File retained for monitoring future runs."
  )
)
message(sprintf("Written: %s (%d rows)", no_pres_path, nrow(no_presence_detail)))

if (nrow(no_presence_sites) > 0) {
  by_net <- no_presence_sites |>
    count(submitting_network, name = "n_no_presence") |>
    arrange(desc(n_no_presence))
  message("  By network:")
  walk(seq_len(nrow(by_net)), function(i) {
    message(sprintf("    %-6s  %d sites", by_net$submitting_network[i], by_net$n_no_presence[i]))
  })
} else {
  message("  All 716 sites have presence-file coverage. No action required.")
}

# ══════════════════════════════════════════════════════════════════════════════
# DIAGNOSTIC 2 — Sensitivity: 5-year sites in upper (6-10) row
# ══════════════════════════════════════════════════════════════════════════════

message("\n=== DIAGNOSTIC 2: Sensitivity — 5-year sites in upper row ===")

sens <- site_out |>
  mutate(
    alt_authors  = apply_rubric(years_of_data, latency_years, upper_five = TRUE),
    base_authors = n_invited_authors,
    swing        = alt_authors - base_authors
  )

# Only sites with exactly 5 years are affected
affected <- sens |> filter(years_of_data == 5L)
message(sprintf("Sites affected (exactly 5 years): %d", nrow(affected)))
if (nrow(affected) > 0) {
  swings <- unique(affected$swing)
  message(sprintf("  Swing per affected site: %s (uniform across latency tiers)",
                  paste0("+", swings, collapse = ", ")))
}

# Per-network totals under both assignments
sens_by_network <- sens |>
  group_by(submitting_network) |>
  summarise(
    n_sites         = n(),
    n_five_yr_sites = sum(years_of_data == 5L, na.rm = TRUE),
    base_total      = sum(base_authors, na.rm = TRUE),
    alt_total       = sum(alt_authors,  na.rm = TRUE),
    swing           = sum(swing, na.rm = TRUE),
    .groups         = "drop"
  ) |>
  arrange(desc(n_sites))

# Summary row
summary_row <- tibble(
  submitting_network = "TOTAL",
  n_sites            = sum(sens_by_network$n_sites),
  n_five_yr_sites    = sum(sens_by_network$n_five_yr_sites),
  base_total         = sum(sens_by_network$base_total),
  alt_total          = sum(sens_by_network$alt_total),
  swing              = sum(sens_by_network$swing)
)

sens_out <- bind_rows(sens_by_network, summary_row)

sens_path <- file.path(DIAG_DIR, "sensitivity_5yr_in_upper_row.csv")
write_csv(sens_out, sens_path)
write_output_metadata(
  sens_path,
  input_sources = c(SITE_OUT_FILE),
  notes = paste0(
    "Sensitivity: sites with exactly 5 years of data re-assigned from '<5' row to '6-10' row. ",
    "base_total = current (default) totals. ",
    "alt_total = totals under alternative assignment. ",
    "swing = alt_total minus base_total (positive = more authors under alt assignment). ",
    "Last row is TOTAL across all networks."
  )
)
message(sprintf("Written: %s", sens_path))
message(sprintf(
  "Net swing: %+d authors (placing 5-yr sites in '6-10' row vs '<5' row)",
  summary_row$swing
))

# ══════════════════════════════════════════════════════════════════════════════
# DIAGNOSTIC 3 — 2025 data availability by network
# ══════════════════════════════════════════════════════════════════════════════

message("\n=== DIAGNOSTIC 3: 2025 data availability by network ===")

# Which sites have n_months_present > 0 for year 2025?
sites_with_2025 <- pres_dedup |>
  filter(year == 2025L, n_months_present > 0L) |>
  select(site_id) |>
  distinct() |>
  mutate(has_2025_data = TRUE)

# Join to full site list (from site_out) to get network
avail_2025 <- site_out |>
  select(site_id, submitting_network, presence_status, latency_years) |>
  left_join(sites_with_2025, by = "site_id") |>
  mutate(
    has_2025_data = coalesce(has_2025_data, FALSE),
    latency_1_yr  = latency_years == 1L
  )

avail_by_network <- avail_2025 |>
  group_by(submitting_network) |>
  summarise(
    n_sites               = n(),
    n_with_2025_data_pres = sum(has_2025_data),
    n_with_latency_1      = sum(latency_1_yr, na.rm = TRUE),
    n_without_2025        = sum(!has_2025_data),
    pct_with_2025         = round(100 * sum(has_2025_data) / n(), 1),
    .groups               = "drop"
  ) |>
  arrange(desc(n_sites))

avail_path <- file.path(DIAG_DIR, "year_2025_availability_by_network.csv")
write_csv(avail_by_network, avail_path)
write_output_metadata(
  avail_path,
  input_sources = c(PRESENCE_FILE, SITE_OUT_FILE),
  notes = paste0(
    "n_with_2025_data_pres: sites with n_months_present > 0 for year 2025 in presence file ",
    "(any of 12 flux variables: NEE VUT+CUT, GPP/RECO NT+DT x VUT+CUT, LE_F_MDS, H_F_MDS). ",
    "n_with_latency_1: sites where latency_years = 1 (max_data_year = 2025 per presence file). ",
    "All 716 sites now have presence-file coverage; latency is derived from presence file only."
  )
)
message(sprintf("Written: %s", avail_path))
message("\n2025 data availability by network (presence-file evidence, any variable):")
avail_by_network |>
  mutate(label = sprintf(
    "  %-6s  %3d/%3d sites have 2025 data (%4.1f%%)  latency=1: %d",
    submitting_network, n_with_2025_data_pres, n_sites,
    pct_with_2025, n_with_latency_1
  )) |>
  pull(label) |>
  walk(message)

# ── Completion ────────────────────────────────────────────────────────────────

message("\n=== authorship_diagnostics.R complete ===")
message("Outputs written to: ", DIAG_DIR, "/")
message("  sites_no_presence_detail.csv")
message("  sensitivity_5yr_in_upper_row.csv")
message("  year_2025_availability_by_network.csv")
