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
## Contact data source values (contact_source column):
##   "snapshot"     — primary contact from the FLUXNET Shuttle snapshot
##                    (semicolon-delimited team_member_* fields); 713 sites.
##   "badm_fallback"— used for sites with no snapshot contact data; those
##                    sites' only known contacts come from BADM (CN-SnB, JP-Api).
##   "badm_extra"   — additional contact known to BADM but absent from the
##                    snapshot for a site that does have snapshot contacts.
##                    Included so PIs can see all known team members.
##   "missing"      — site has no contact data in any source (SD-Dem).
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
# BADM role strings are used as-is (e.g. "DataManager", "CO-PI", "Technician").
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

# ── Step 1: Expand snapshot contact fields ────────────────────────────────────
#
# The snapshot carries one row per site; team_member_name, team_member_role,
# and team_member_email are semicolon-delimited strings. Expand to long format.

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
  badm_wide_fallback <- badm_team_missing |>
    pivot_wider(
      id_cols     = c(SITE_ID, GROUP_ID),
      names_from  = VARIABLE,
      values_from = DATAVALUE,
      values_fn   = first
    )
  for (col in c("TEAM_MEMBER_NAME", "TEAM_MEMBER_EMAIL", "TEAM_MEMBER_ROLE",
                 "TEAM_CONTACT_NAME", "TEAM_CONTACT_EMAIL")) {
    if (!col %in% names(badm_wide_fallback)) badm_wide_fallback[[col]] <- NA_character_
  }
  badm_fallback_contacts <- badm_wide_fallback |>
    mutate(
      contact_name   = coalesce(TEAM_MEMBER_NAME,  TEAM_CONTACT_NAME),
      contact_email  = coalesce(TEAM_MEMBER_EMAIL, TEAM_CONTACT_EMAIL),
      contact_role   = TEAM_MEMBER_ROLE,
      contact_source = "badm_fallback"
    ) |>
    filter(!is.na(contact_name)) |>
    select(site_id = SITE_ID, contact_name, contact_email, contact_role, contact_source)
} else {
  badm_fallback_contacts <- tibble(
    site_id = character(), contact_name = character(),
    contact_email = character(), contact_role = character(),
    contact_source = character()
  )
}

sites_from_badm <- unique(badm_fallback_contacts$site_id)
sites_still_na  <- setdiff(snapshot_missing, sites_from_badm)

message(sprintf("BADM fallback found contacts for: %s",
  if (length(sites_from_badm) == 0) "none" else paste(sites_from_badm, collapse = ", ")))
message(sprintf("Sites with no contacts anywhere (will get NA row): %s",
  if (length(sites_still_na)  == 0) "none" else paste(sites_still_na,  collapse = ", ")))

# ── Step 2b: Identify sites where BADM has contacts not in snapshot ────────────
#
# Pre-compute per-site contact counts from both sources. These counts also feed
# the coverage_comparison diagnostic output (Step 7). We compute them here so
# they are available for the badm_extra extraction below.

snap_counts <- snap_raw |>
  mutate(
    n_contacts_snapshot = if_else(
      !is.na(team_member_name) & team_member_name != "",
      as.integer(lengths(strsplit(team_member_name, ";", fixed = TRUE))),
      0L
    )
  ) |>
  select(site_id, n_contacts_snapshot)

badm_name_records <- badm |>
  filter(
    VARIABLE_GROUP %in% c("GRP_TEAM", "TEAM_MEMBER", "TEAM_CONTACT"),
    VARIABLE       %in% c("TEAM_MEMBER_NAME", "TEAM_CONTACT_NAME")
  )

badm_counts <- badm_name_records |>
  group_by(site_id = SITE_ID) |>
  summarise(n_contacts_badm = n_distinct(GROUP_ID), .groups = "drop")

# Sites where BADM has more contacts AND snapshot already has ≥ 1 contact.
# Sites with snapshot = 0 (CN-SnB, JP-Api) are already captured as
# badm_fallback above — including them here would produce duplicate rows.
badm_extra_site_ids <- site_authors |>
  select(site_id) |>
  left_join(snap_counts, by = "site_id") |>
  left_join(badm_counts, by = "site_id") |>
  mutate(
    n_contacts_snapshot = coalesce(n_contacts_snapshot, 0L),
    n_contacts_badm     = coalesce(n_contacts_badm,     0L)
  ) |>
  filter(n_contacts_badm > n_contacts_snapshot, n_contacts_snapshot > 0L) |>
  pull(site_id)

message(sprintf(
  "\n%d sites where BADM has contacts not in snapshot (snapshot > 0): %s",
  length(badm_extra_site_ids),
  paste(badm_extra_site_ids, collapse = ", ")
))
message(
  "  Note: CN-SnB and JP-Api (snapshot=0) are already captured as",
  "badm_fallback and excluded here to avoid duplicate rows."
)

# ── Step 3: Extract BADM-extra contacts ───────────────────────────────────────
#
# For each badm_extra site, pull all BADM contacts and filter to those
# whose name AND email are absent from the snapshot list for that site.
# Role strings are used as-is from BADM — no normalization applied.

if (length(badm_extra_site_ids) > 0) {
  badm_extra_raw <- badm |>
    filter(
      SITE_ID %in% badm_extra_site_ids,
      VARIABLE_GROUP %in% c("GRP_TEAM", "TEAM_MEMBER", "TEAM_CONTACT"),
      VARIABLE %in% c("TEAM_MEMBER_NAME", "TEAM_MEMBER_EMAIL", "TEAM_MEMBER_ROLE",
                       "TEAM_CONTACT_NAME", "TEAM_CONTACT_EMAIL")
    ) |>
    pivot_wider(
      id_cols     = c(SITE_ID, GROUP_ID),
      names_from  = VARIABLE,
      values_from = DATAVALUE,
      values_fn   = first
    )

  for (col in c("TEAM_MEMBER_NAME", "TEAM_MEMBER_EMAIL", "TEAM_MEMBER_ROLE",
                 "TEAM_CONTACT_NAME", "TEAM_CONTACT_EMAIL")) {
    if (!col %in% names(badm_extra_raw)) badm_extra_raw[[col]] <- NA_character_
  }

  badm_extra_all <- badm_extra_raw |>
    mutate(
      contact_name  = coalesce(TEAM_MEMBER_NAME,  TEAM_CONTACT_NAME),
      contact_email = coalesce(TEAM_MEMBER_EMAIL, TEAM_CONTACT_EMAIL),
      contact_role  = TEAM_MEMBER_ROLE
    ) |>
    filter(!is.na(contact_name)) |>
    select(site_id = SITE_ID, contact_name, contact_email, contact_role)

  # Normalized snapshot name/email lookup (case-insensitive, trimmed) per site
  snap_lookup <- snap_raw |>
    filter(site_id %in% badm_extra_site_ids) |>
    mutate(
      norm_names  = map(team_member_name, ~ {
        if (is.na(.x) || .x == "") character(0L)
        else trimws(tolower(strsplit(.x, ";", fixed = TRUE)[[1]]))
      }),
      norm_emails = map(team_member_email, ~ {
        if (is.na(.x) || .x == "") character(0L)
        else trimws(tolower(strsplit(.x, ";", fixed = TRUE)[[1]]))
      })
    ) |>
    select(site_id, norm_names, norm_emails)

  # A BADM contact is "extra" if their name AND email are both absent from snapshot.
  # Using both fields guards against edge cases where names differ slightly between
  # sources (different encodings, middle initials, etc.) but emails agree.
  badm_extra_contacts <- badm_extra_all |>
    left_join(snap_lookup, by = "site_id") |>
    filter(pmap_lgl(
      list(contact_name, contact_email, norm_names, norm_emails),
      function(nm, em, snap_nms, snap_ems) {
        nm_norm   <- trimws(tolower(nm))
        em_norm   <- trimws(tolower(coalesce(em, "")))
        name_hit  <- nm_norm %in% snap_nms
        email_hit <- em_norm != "" && em_norm %in% snap_ems
        !name_hit && !email_hit
      }
    )) |>
    mutate(contact_source = "badm_extra") |>
    select(site_id, contact_name, contact_email, contact_role, contact_source)

  message(sprintf(
    "BADM-extra contacts identified: %d across %d sites",
    nrow(badm_extra_contacts), n_distinct(badm_extra_contacts$site_id)
  ))
  badm_extra_contacts |>
    mutate(label = sprintf(
      "  %-8s  %-30s  %-35s  %s",
      site_id, contact_name,
      coalesce(contact_email, "(no email)"),
      coalesce(contact_role, "(no role)")
    )) |>
    pull(label) |>
    walk(message)
} else {
  badm_extra_contacts <- tibble(
    site_id = character(), contact_name = character(),
    contact_email = character(), contact_role = character(),
    contact_source = character()
  )
}

# ── Step 4: Combine all source contacts ───────────────────────────────────────
#
# source_contacts holds every real contact across all three sources.
# Sites absent from source_contacts produce a single NA row after the
# left_join in Step 5 (contact_source will be set to "missing").

source_contacts <- bind_rows(snap_long, badm_fallback_contacts, badm_extra_contacts)

message(sprintf(
  "\nTotal source contacts: %d rows, %d sites",
  nrow(source_contacts), n_distinct(source_contacts$site_id)
))
message(sprintf(
  "  By source: snapshot=%d  badm_fallback=%d  badm_extra=%d",
  sum(source_contacts$contact_source == "snapshot"),
  sum(source_contacts$contact_source == "badm_fallback"),
  sum(source_contacts$contact_source == "badm_extra")
))

# ── Step 5: Join rubric scores and compute n_contacts_for_site ────────────────
#
# Left-join from site_authors ensures all 716 sites appear.
# n_contacts_for_site = 0 for "missing" sites (no contacts in any source).

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

# ── Step 6: Role-priority sort ────────────────────────────────────────────────
#
# Within each site: PI → CO-PI → DATA/DataManager → all others.
# Secondary sort on contact_name makes output deterministic.
# BADM-extra contacts slot into this hierarchy by their BADM role string.

invitations <- invitations |>
  mutate(.priority = role_priority(contact_role)) |>
  arrange(site_id, .priority, contact_name) |>
  select(-.priority)

# ── Step 7: Final column selection ────────────────────────────────────────────

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

expected_sites <- sort(site_authors$site_id)
found_sites    <- sort(unique(invitations$site_id))
absent         <- setdiff(expected_sites, found_sites)

if (length(absent) == 0L) {
  message(sprintf("PASS  All %d sites appear in output.", length(expected_sites)))
} else {
  message(sprintf("FAIL  %d site(s) missing: %s",
                  length(absent), paste(absent, collapse = ", ")))
}

total_rows <- nrow(invitations)
message(sprintf("Total rows: %d", total_rows))
message(sprintf(
  "  = %d snapshot + %d badm_fallback + %d badm_extra + %d missing-sentinel",
  sum(invitations$contact_source == "snapshot"),
  sum(invitations$contact_source == "badm_fallback"),
  sum(invitations$contact_source == "badm_extra"),
  sum(invitations$contact_source == "missing")
))
message(
  "  Note: CN-SnB and JP-Api (snapshot=0) are counted as badm_fallback.",
  sprintf(" Expected: 4233 baseline + %d badm_extra = %d",
          nrow(badm_extra_contacts), 4233L + nrow(badm_extra_contacts))
)

message("\nContact source breakdown (sites):")
invitations |>
  distinct(site_id, contact_source) |>
  count(contact_source, name = "n_sites") |>
  mutate(label = sprintf("  %-16s %d sites", contact_source, n_sites)) |>
  pull(label) |>
  walk(message)

# ── Diagnostic: Snapshot vs BADM contact count comparison ────────────────────
#
# Uses the counts computed in Step 2b. The diagnostic is the unchanged raw
# comparison — it is NOT modified by the decision to merge badm_extra contacts.

message("\n=== DIAGNOSTIC: CONTACT COVERAGE COMPARISON ===")

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
  "Sites where BADM has more contacts than snapshot: %d (all merged into output as badm_extra or badm_fallback)",
  nrow(sites_badm_richer)
))

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
    "contact_source values: ",
    "(1) 'snapshot' — FLUXNET Shuttle snapshot team_member_name/role/email, semicolon-delimited (713 sites); ",
    "(2) 'badm_fallback' — BADM GRP_TEAM/TEAM_MEMBER records for sites absent from snapshot (CN-SnB, JP-Api); ",
    "(3) 'badm_extra' — additional contacts known to BADM but absent from snapshot for sites that do ",
    "have snapshot contacts (", nrow(badm_extra_contacts), " contacts across ",
    n_distinct(badm_extra_contacts$site_id), " sites); ",
    "(4) 'missing' — site has no contact data in any source (SD-Dem). ",
    "badm_extra contacts are included so PIs can see all known team members; ",
    "the PI selects which contacts to invite. BADM role strings used as-is (no normalization). ",
    "n_invited_authors_for_site from site_authors.csv (locked rubric 2026-05-07, reference year 2026). ",
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
    "Per-site comparison of contact record counts from two sources (audit trail). ",
    "n_contacts_snapshot: semicolon-delimited entries in team_member_name field. ",
    "n_contacts_badm: distinct GROUP_IDs with TEAM_MEMBER_NAME or TEAM_CONTACT_NAME ",
    "in BADM GRP_TEAM/TEAM_MEMBER/TEAM_CONTACT groups. ",
    "badm_has_more=TRUE indicates BADM has additional contacts; these are now included ",
    "in authorship_invitations.csv as contact_source='badm_extra' (or 'badm_fallback' ",
    "for sites with snapshot=0). This file is the raw audit trail — it is not modified ",
    "by the merge decision. Sorted: badm_has_more DESC, difference DESC."
  )
)
message(sprintf("Written: %s  (%d rows)", diag_path, nrow(coverage_comparison)))

message(sprintf(
  "\nauthorship_invitations.R complete: %d rows, %d sites.",
  nrow(invitations), n_distinct(invitations$site_id)
))
