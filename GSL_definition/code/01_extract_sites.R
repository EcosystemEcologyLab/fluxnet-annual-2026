## 01_extract_sites.R — GSL figure rebuild, framework run-order step 2.
##
## Pulls the 62 sites' daily (DD) FLUXMET rows from the repo-root DuckDB
## store (read-only, never written to), compares product_id across
## snapshots, and reports CUT-only sites among the 62. See
## ../docs/framework.md ("Data required", run-order step 2).

source("GSL_definition/code/00_config.R")

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr)
})

if (!dir.exists(file.path(GSL_ROOT, "data"))) {
  dir.create(file.path(GSL_ROOT, "data"), recursive = TRUE)
}
if (!dir.exists(file.path(GSL_ROOT, "tables"))) {
  dir.create(file.path(GSL_ROOT, "tables"), recursive = TRUE)
}

## ---- Extract: daily FLUXMET rows for the 62 sites -------------------------
## dataset = 'FLUXMET' only -- the daily table also carries dataset = 'ERA5'
## rows (meteorology only, no NEE QC), which must never be mixed in here.
con <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)

site_list_sql <- paste(sprintf("'%s'", GSL_SITE_IDS), collapse = ", ")
daily_extract <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, GPP_NT_VUT_REF, GPP_DT_VUT_REF, NEE_VUT_REF,
         NEE_VUT_REF_QC, NEE_CUT_REF_QC, TA_F, P_F, SWC_F_MDS_1
  FROM daily
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", site_list_sql))

message("Extracted ", nrow(daily_extract), " daily FLUXMET row(s) for ",
        n_distinct(daily_extract$site_id), " of ", length(GSL_SITE_IDS), " sites.")

missing_sites <- setdiff(GSL_SITE_IDS, unique(daily_extract$site_id))
if (length(missing_sites) > 0) {
  message("No daily FLUXMET rows found for: ", paste(missing_sites, collapse = ", "))
  for (sid in missing_sites) {
    log_unknown(
      record_id = sid,
      reason    = "No daily FLUXMET rows found in DuckDB daily table",
      logged_by = "01_extract_sites.R",
      log_path  = GSL_UNKNOWN_LOG
    )
  }
}

saveRDS(daily_extract, file.path(GSL_ROOT, "data", "daily_extract.rds"))
message("Saved: ", file.path(GSL_ROOT, "data", "daily_extract.rds"))

## ---- CUT-only sites, per the 04_qc.R per-site rule -------------------------
## VUT if the site has any non-NA NEE_VUT_REF_QC, else CUT if it has any
## non-NA NEE_CUT_REF_QC, else ungated. Same rule as scripts/04_qc.R /
## R/site_annual_fluxes.R -- not reimplemented differently.
carbon_src <- daily_extract |>
  group_by(site_id) |>
  summarise(
    n_vut = sum(!is.na(NEE_VUT_REF_QC)),
    n_cut = sum(!is.na(NEE_CUT_REF_QC)),
    .groups = "drop"
  ) |>
  mutate(carbon_src = case_when(
    n_vut > 0L ~ "VUT", n_cut > 0L ~ "CUT", TRUE ~ NA_character_
  ))

cut_only_sites <- carbon_src$site_id[carbon_src$carbon_src == "CUT"]
ungated_sites  <- carbon_src$site_id[is.na(carbon_src$carbon_src)]
message(
  "Carbon source among the 62 sites: ", sum(carbon_src$carbon_src == "VUT", na.rm = TRUE),
  " VUT, ", length(cut_only_sites), " CUT-only, ", length(ungated_sites), " ungated."
)
if (length(cut_only_sites) > 0) {
  message("CUT-only site(s): ", paste(cut_only_sites, collapse = ", "))
}
if (length(ungated_sites) > 0) {
  message("Ungated site(s) (no non-NA NEE_VUT_REF_QC or NEE_CUT_REF_QC at all): ",
          paste(ungated_sites, collapse = ", "))
  for (sid in ungated_sites) {
    log_unknown(
      record_id = sid,
      reason    = "No non-NA NEE_VUT_REF_QC or NEE_CUT_REF_QC in any daily FLUXMET row",
      logged_by = "01_extract_sites.R",
      log_path  = GSL_UNKNOWN_LOG
    )
  }
}

dbDisconnect(con, shutdown = TRUE)

## ---- product_id comparison across snapshots --------------------------------
read_snapshot_products <- function(path) {
  s <- read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE)
  s |> filter(site_id %in% GSL_SITE_IDS) |> select(site_id, product_id)
}

sep01 <- read_snapshot_products(GSL_SNAPSHOT_SEP01) |> rename(product_id_sep01 = product_id)
sep20 <- read_snapshot_products(GSL_SNAPSHOT_SEP20) |> rename(product_id_sep20 = product_id)

site_products <- GSL_SITES |>
  select(site_id) |>
  left_join(sep01, by = "site_id") |>
  left_join(sep20, by = "site_id") |>
  mutate(changed_sep01_to_sep20 = product_id_sep01 != product_id_sep20)

n_missing_sep01 <- sum(is.na(site_products$product_id_sep01))
n_missing_sep20 <- sum(is.na(site_products$product_id_sep20))
if (n_missing_sep01 > 0) {
  message(n_missing_sep01, " of the 62 site(s) not found in the 1 September snapshot.")
}
if (n_missing_sep20 > 0) {
  message(n_missing_sep20, " of the 62 site(s) not found in the 20 September snapshot.")
}

tash_available <- file.exists(GSL_TASH_SNAPSHOT)
if (tash_available) {
  tash <- read_snapshot_products(GSL_TASH_SNAPSHOT) |> rename(product_id_tash = product_id)
  site_products <- site_products |>
    left_join(tash, by = "site_id") |>
    mutate(
      changed_tash_to_sep20 = product_id_tash != product_id_sep20,
      flagged = coalesce(changed_tash_to_sep20, TRUE) # no Tash product_id -> cannot confirm match, flag
    )
  message("Compared against Tash's snapshot (", GSL_TASH_SNAPSHOT, ").")
} else {
  site_products$product_id_tash <- NA_character_
  site_products$changed_tash_to_sep20 <- NA
  site_products$flagged <- NA
  message(
    "Tash's snapshot not found at ", GSL_TASH_SNAPSHOT, " -- comparison against it is ",
    "PENDING, not computed. flagged/changed_tash_to_sep20 are NA, not FALSE."
  )
}

changed_sites <- site_products$site_id[site_products$changed_sep01_to_sep20]
message(
  length(changed_sites), " site(s) with a product_id change between our 1 and 20 ",
  "September snapshots: ", paste(changed_sites, collapse = ", ")
)

write_csv(site_products, file.path(GSL_ROOT, "tables", "site_products.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "site_products.csv"),
  input_sources = c(GSL_SNAPSHOT_SEP01, GSL_SNAPSHOT_SEP20,
                     if (tash_available) GSL_TASH_SNAPSHOT else "Tash's 17 Aug snapshot: not yet received"),
  notes = paste0(
    "product_id comparison for the framework's 62 sites across the repo's 1 and 20 ",
    "September 2026 snapshots", if (tash_available) " and Tash's 17 August 2026 snapshot" else
    " (comparison against Tash's 17 August 2026 snapshot is pending -- not yet received)",
    ". carbon_src (VUT/CUT/ungated, per-site 04_qc.R rule): ",
    sum(carbon_src$carbon_src == "VUT", na.rm = TRUE), " VUT, ", length(cut_only_sites),
    " CUT-only (", paste(cut_only_sites, collapse = ", "), "), ", length(ungated_sites),
    " ungated (", paste(ungated_sites, collapse = ", "), ")."
  )
)

message("Saved: ", file.path(GSL_ROOT, "tables", "site_products.csv"))
message("--- 01_extract_sites.R: done ---")
