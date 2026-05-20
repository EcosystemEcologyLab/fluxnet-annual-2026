## scripts/authorship_invitations.R
##
## Build the authorship invitation list for the FLUXNET Annual Paper 2026.
## Long format — one row per contact person per site — with the per-site
## invited-author count (from the locked rubric) repeated for every contact.
##
## The PI for each site will use this file to decide which of their team
## members to invite as co-authors; n_invited_authors_for_site is the
## allocation the PI is working with.
##
## Contact data source priority:
##   1. FLUXNET Shuttle snapshot team_member_name/role/email (semicolon lists)
##      — primary; covers 713 of 716 sites.
##   2. BADM GRP_TEAM / TEAM_MEMBER records — fallback for the 3 sites missing
##      from snapshot contact fields.
##   3. contact_source = "missing" — row with NA contact fields, included so
##      all 716 sites appear at least once.
##
## Usage:
##   Rscript scripts/authorship_invitations.R
##
## Inputs:
##   outputs/authorship/site_authors.csv
##   data/snapshots/fluxnet_shuttle_snapshot_20260428T231049.csv
##   data/processed/badm.rds
##
## Outputs:
##   outputs/authorship/authorship_invitations.csv           + .meta.json
##   outputs/authorship/diagnostics/contact_coverage_comparison.csv  + .meta.json

# ── Startup ───────────────────────────────────────────────────────────────────

if (file.exists(".env")) {
  if (!requireNamespace("dotenv", quietly = TRUE)) {
    stop("Package 'dotenv' required for local use. Install: install.packages('dotenv')")
  }
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(fs)

# ── Configuration ─────────────────────────────────────────────────────────────

SITE_AUTHORS_FILE <- "outputs/authorship/site_authors.csv"
SNAPSHOT_FILE     <- "data/snapshots/fluxnet_shuttle_snapshot_20260428T231049.csv"
BADM_FILE         <- "data/processed/badm.rds"
OUTPUT_DIR        <- "outputs/authorship"
DIAG_DIR          <- file.path(OUTPUT_DIR, "diagnostics")
SCRIPT_NAME       <- "authorship_invitations.R"

# Role priority for within-site sort.
# Lower integer = appears first (PI is most likely co-author candidate).
role_priority <- function(role) {
  case_when(
    role == "PI"                       ~ 1L,
    role == "CO-PI"                    ~ 2L,
    role %in% c("DATA", "DataManager") ~ 3L,
    TRUE                               ~ 4L
  )
}

# ── Read inputs ───────────────────────────────────────────────────────────────

message("Reading inputs...")

site_authors <- read_csv(SITE_AUTHORS_FILE, show_col_types = FALSE)
snap         <- read_csv(SNAPSHOT_FILE,     show_col_types = FALSE)
badm         <- readRDS(BADM_FILE)

message(sprintf("  site_authors : %d sites", nrow(site_authors)))
message(sprintf("  snapshot     : %d rows",  nrow(snap)))
message(sprintf("  badm         : %d rows",  nrow(badm)))

# ── Step 1: Expand snapshot contact fields ─────────────────────────────────────
#
# The snapshot carries one row per site; team_member_name, team_member_role, and
# team_member_email are semicolon-delimited strings. We expand to long format.

snap_raw <- snap |>
  select(site_id, team_member_name, team_member_role, team_member_email) |>
  distinct()

has_snap_contacts <- !is.na(snap_raw$team_member_name) & snap_raw$team_member_name != ""
snap_with         <- snap_raw[has_snap_contacts, ]
snapshot_missing  <- snap_raw$site_id[!has_snap_contacts]

message(sprintf(
  "\nSnapshot contact coverage: %d sites with data, %d missing (%s)",
  sum(has_snap_contacts), length(snapshot_missing),
  paste(snapshot_missing, collapse = ", ")
))

# Expand one site's semicolon strings to a tibble of individual contacts.
expand_one_site <- function(site_id, names_str, roles_str, emails_str) {
  split_safe <- function(s) {
    if (is.na(s) || nchar(trimws(s)) == 0L) return(NA_character_)
    trimws(strsplit(s, ";", fixed = TRUE)[[1]])
  }
  nms  <- split_safe(names_str)
  rols <- split_safe(roles_str)
  emls <- split_safe(emails_str)

  # Number of contacts is driven by the name field length.
  # Pad shorter vectors so all three have the same length.
  n <- max(length(nms), length(rols), length(emls), na.rm = TRUE)
  pad <- function(v) c(v, rep(NA_character_, n - length(v)))
  if (all(is.na(nms)))  nms  <- rep(NA_character_, n)
  if (all(is.na(rols))) rols <- rep(NA_character_, n)
  if (all(is.na(emls))) emls <- rep(NA_character_, n)

  tibble(
    site_id        = site_id,
    contact_name   = pad(nms),
    contact_role   = pad(rols),
    contact_email  = pad(emls),
    contact_source = "snapshot"
  )
}

snap_long <- pmap_dfr(
  list(snap_with$site_id, snap_with$team_member_name,
       snap_with$team_member_role, snap_with$team_member_email),
  expand_one_site
) |>
  mutate(
    contact_email = na_if(contact_email, ""),
    contact_role  = na_if(contact_role,  "")
  )

message(sprintf(
  "Expanded to %d contact rows for %d sites",
  nrow(snap_long), n_distinct(snap_long$site_id)
))

# ── Step 2: BADM fallback for sites missing snapshot contacts ─────────────────
#
# Check GRP_TEAM / TEAM_MEMBER / TEAM_CONTACT records in badm.rds.
# Each (SITE_ID, GROUP_ID) pair represents one person.

badm_team_missing <- badm |>
  filter(
    SITE_ID %in% snapshot_missing,
    VARIABLE_GROUP %in% c("GRP_TEAM", "TEAM_MEMBER", "TEAM_CONTACT"),
    VARIABLE %in% c(
      "TEAM_MEMBER_NAME",  "TEAM_MEMBER_EMAIL",  "TEAM_MEMBER_ROLE",
      "TEAM_CONTACT_NAME", "TEAM_CONTACT_EMAIL"
    )
  )

message(sprintf(
  "\nBADM records for %d snapshot-missing site(s): %d rows",
  length(snapshot_missing), nrow(badm_team_missing)
))

if (nrow(badm_team_missing) > 0) {
  badm_wide <- badm_team_missing |>
    pivot_wider(
      id_cols     = c(SITE_ID, GROUP_ID),
      names_from  = VARIABLE,
      values_from = DATAVALUE,
      values_fn   = first   # guard against rare duplicates
    )

  # Ensure all expected name/email/role columns exist
  for (col in c("TEAM_MEMBER_NAME", "TEAM_MEMBER_EMAIL", "TEAM_MEMBER_ROLE",
                 "TEAM_CONTACT_NAME", "TEAM_CONTACT_EMAIL")) {
    if (!col %in% names(badm_wide)) badm_wide[[col]] <- NA_character_
  }

  badm_contacts <- badm_wide |>
    mutate(
      contact_name   = coalesce(TEAM_MEMBER_NAME,  TEAM_CONTACT_NAME),
      contact_email  = coalesce(TEAM_MEMBER_EMAIL, TEAM_CONTACT_EMAIL),
      contact_role   = TEAM_MEMBER_ROLE,
      contact_source = "badm_fallback"
    ) |>
    filter(!is.na(contact_name)) |>
    select(site_id = SITE_ID, contact_name, contact_email, contact_role, contact_source)
} else {
  badm_contacts <- tibble(
    site_id        = character(),
    contact_name   = character(),
    contact_email  = character(),
    contact_role   = character(),
    contact_source = character()
  )
}

sites_from_badm   <- unique(badm_contacts$site_id)
sites_still_na    <- setdiff(snapshot_missing, sites_from_badm)

message(sprintf("BADM fallback found contacts for: %s",
  if (length(sites_from_badm) == 0) "none" else paste(sites_from_badm, collapse = ", ")))
message(sprintf("Sites with no contacts anywhere (will get NA row): %s",
  if (length(sites_still_na)  == 0) "none" else paste(sites_still_na,  collapse = ", ")))

if (length(sites_from_badm) > 0) {
  message(sprintf("  BADM fallback contact counts:"))
  badm_contacts |>
    count(site_id, name = "n") |>
    mutate(label = sprintf("    %s: %d contacts", site_id, n)) |>
    pull(label) |>
    walk(message)
}

# ── Step 3: Combine all source contacts ───────────────────────────────────────
#
# source_contacts holds every real contact (snapshot + BADM fallback).
# Sites absent here will produce a single NA row after the left_join in Step 4.

source_contacts <- bind_rows(snap_long, badm_contacts)

message(sprintf(
  "\nTotal source contacts: %d rows, %d sites",
  nrow(source_contacts), n_distinct(source_contacts$site_id)
))

# ── Step 4: Join rubric scores and compute n_contacts_for_site ────────────────
#
# Left-join from site_authors ensures all 716 sites appear.
# Sites with no contacts in source_contacts produce one row with NA contact
# fields; contact_source is set to "missing" for these.

# n_contacts_for_site = actual contacts found (0 for "missing" sites)
n_contacts_tbl <- source_contacts |>
  count(site_id, name = "n_contacts_for_site")

invitations <- site_authors |>
  select(site_id, submitting_network, n_invited_authors) |>
  left_join(source_contacts, by = "site_id") |>
  left_join(n_contacts_tbl, by = "site_id") |>
  rename(n_invited_authors_for_site = n_invited_authors) |>
  mutate(
    n_contacts_for_site = coalesce(n_contacts_for_site, 0L),
    contact_source      = if_else(is.na(contact_source), "missing", contact_source)
  )

# ── Step 5: Role-priority sort ────────────────────────────────────────────────
#
# Within each site: PI → CO-PI → DATA/DataManager → all others (alphabetical
# secondary sort on contact_name to make output deterministic).

invitations <- invitations |>
  mutate(.priority = role_priority(contact_role)) |>
  arrange(site_id, .priority, contact_name) |>
  select(-.priority)

# ── Step 6: Final column selection ────────────────────────────────────────────

invitations <- invitations |>
  select(
    site_id,
    submitting_network,
    n_invited_authors_for_site,
    contact_role,
    contact_name,
    contact_email,
    contact_source,
    n_contacts_for_site
  )

# ── Sanity checks ─────────────────────────────────────────────────────────────

message("\n=== SANITY CHECKS ===")

# 1. All 716 sites present
expected_sites <- sort(site_authors$site_id)
found_sites    <- sort(unique(invitations$site_id))
absent         <- setdiff(expected_sites, found_sites)

if (length(absent) == 0L) {
  message(sprintf("PASS  All %d sites appear in output.", length(expected_sites)))
} else {
  message(sprintf("FAIL  %d site(s) missing from output: %s",
                  length(absent), paste(absent, collapse = ", ")))
}

# 2. Row count = sum of n_contacts_for_site (counting "missing" rows as 1)
#    For missing sites the row IS the sentinel, so we count it.
total_rows          <- nrow(invitations)
expected_row_count  <- sum(pmax(invitations |>
  distinct(site_id, n_contacts_for_site) |>
  pull(n_contacts_for_site), 1L))

if (total_rows == expected_row_count) {
  message(sprintf("PASS  Row count %d matches expected (sum of max(contacts,1) per site).",
                  total_rows))
} else {
  message(sprintf("WARN  Row count %d vs expected %d — check for duplicate site rows.",
                  total_rows, expected_row_count))
}

# 3. Contact source breakdown
message("\nContact source breakdown (sites):")
invitations |>
  distinct(site_id, contact_source) |>
  count(contact_source, name = "n_sites") |>
  mutate(label = sprintf("  %-16s %d sites", contact_source, n_sites)) |>
  pull(label) |>
  walk(message)

message(sprintf("\nTotal rows in authorship_invitations.csv: %d", total_rows))

# ── Diagnostic: Snapshot vs BADM contact count comparison ────────────────────

message("\n=== DIAGNOSTIC: CONTACT COVERAGE COMPARISON ===")

# Snapshot counts (all 716 sites)
snap_counts <- snap_raw |>
  mutate(
    n_contacts_snapshot = if_else(
      !is.na(team_member_name) & team_member_name != "",
      as.integer(lengths(strsplit(team_member_name, ";", fixed = TRUE))),
      0L
    )
  ) |>
  select(site_id, n_contacts_snapshot)

# BADM counts (all 716 sites where data exists)
badm_name_records <- badm |>
  filter(
    VARIABLE_GROUP %in% c("GRP_TEAM", "TEAM_MEMBER", "TEAM_CONTACT"),
    VARIABLE       %in% c("TEAM_MEMBER_NAME", "TEAM_CONTACT_NAME")
  )

badm_counts <- badm_name_records |>
  group_by(site_id = SITE_ID) |>
  summarise(n_contacts_badm = n_distinct(GROUP_ID), .groups = "drop")

coverage_comparison <- site_authors |>
  select(site_id, submitting_network) |>
  left_join(snap_counts,  by = "site_id") |>
  left_join(badm_counts,  by = "site_id") |>
  mutate(
    n_contacts_snapshot = coalesce(n_contacts_snapshot, 0L),
    n_contacts_badm     = coalesce(n_contacts_badm,     0L),
    difference          = n_contacts_badm - n_contacts_snapshot,
    badm_has_more       = difference > 0L
  ) |>
  arrange(desc(badm_has_more), desc(difference), site_id)

sites_badm_richer <- coverage_comparison |> filter(badm_has_more)
message(sprintf(
  "Sites where BADM has more contacts than snapshot: %d",
  nrow(sites_badm_richer)
))

if (nrow(sites_badm_richer) > 0) {
  sites_badm_richer |>
    slice_head(n = 20) |>
    mutate(label = sprintf(
      "  %s [%s]  snapshot=%d  badm=%d  diff=%+d",
      site_id, submitting_network, n_contacts_snapshot, n_contacts_badm, difference
    )) |>
    pull(label) |>
    walk(message)
  if (nrow(sites_badm_richer) > 20)
    message(sprintf("  ... and %d more (see diagnostic CSV)", nrow(sites_badm_richer) - 20))
}

# ── Write outputs ─────────────────────────────────────────────────────────────

if (!dir_exists(OUTPUT_DIR)) dir_create(OUTPUT_DIR, recurse = TRUE)
if (!dir_exists(DIAG_DIR))   dir_create(DIAG_DIR,   recurse = TRUE)

inv_path <- file.path(OUTPUT_DIR, "authorship_invitations.csv")
write_csv(invitations, inv_path)
write_output_metadata(
  inv_path,
  input_sources = c(SITE_AUTHORS_FILE, SNAPSHOT_FILE, BADM_FILE),
  notes = paste0(
    "Long-format authorship invitation list: one row per contact person per site. ",
    "Source priority: (1) FLUXNET Shuttle snapshot team_member_name/role/email ",
    "(semicolon-delimited, 713 sites); (2) BADM GRP_TEAM/TEAM_MEMBER fallback for ",
    "sites missing snapshot contacts; (3) NA row with contact_source='missing' for ",
    "sites with no contacts in either source. ",
    "n_invited_authors_for_site from site_authors.csv (locked rubric 2026-05-07, ",
    "reference year 2026). ",
    "Sort order within site: PI, CO-PI, DATA/DataManager, all others (alpha secondary). ",
    "n_contacts_for_site = 0 for 'missing' sites."
  )
)
message(sprintf(
  "\nWritten: %s  (%d rows, %d sites)",
  inv_path, nrow(invitations), n_distinct(invitations$site_id)
))

diag_path <- file.path(DIAG_DIR, "contact_coverage_comparison.csv")
write_csv(coverage_comparison, diag_path)
write_output_metadata(
  diag_path,
  input_sources = c(SNAPSHOT_FILE, BADM_FILE),
  notes = paste0(
    "Per-site comparison of contact record counts from two sources. ",
    "n_contacts_snapshot: semicolon-delimited entries in team_member_name field. ",
    "n_contacts_badm: distinct GROUP_IDs with TEAM_MEMBER_NAME or TEAM_CONTACT_NAME ",
    "in BADM GRP_TEAM/TEAM_MEMBER/TEAM_CONTACT groups. ",
    "badm_has_more=TRUE indicates BADM has additional contacts not in the snapshot; ",
    "these are not automatically included in authorship_invitations.csv (snapshot is ",
    "primary). Sorted: badm_has_more DESC, difference DESC."
  )
)
message(sprintf("Written: %s  (%d rows)", diag_path, nrow(coverage_comparison)))

message(sprintf(
  "\nauthorship_invitations.R complete: %d rows, %d sites, %d total contacts.",
  nrow(invitations),
  n_distinct(invitations$site_id),
  sum(invitations$n_contacts_for_site > 0)   # sites with at least 1 contact
))
