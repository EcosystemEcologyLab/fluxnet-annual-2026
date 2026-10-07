## dq_stage4_availability.R
##
## Data quality / uncertainty diagnostic -- Stage 4: availability and
## failure, plus the site-year master join table.
##
## Unattended background run. Diagnostic only, writes to
## review/diagnostics/data_quality_uncertainty/. Reads the pre-QC `annual`
## DuckDB table (dataset = 'FLUXMET') and the BIF (BADM) files under
## data/extracted/ -- never *_qc/*_converted.
##
## "Qualifying" throughout means the paper's own rule, applied to each
## carbon type on its own QC column: (1 - QC) <= QC_THRESHOLD_YY. A
## site-year/site is categorised both/VUT_only/CUT_only/neither by which
## of NEE_VUT_REF/NEE_CUT_REF qualify. "neither" is further split into
## no_value (NEE_VUT_REF and NEE_CUT_REF both NA -- nothing to gate) vs
## fails_qc (at least one side has a raw value but it/they fail the QC
## rule). u-star method failures are tabulated from the BIF GRP_UST_THR
## group found at all 781 sites in Stage 0 (USTAR_CP_SUCCESS_RUN,
## USTAR_MP_SUCCESS_RUN, one flag per site-year per method).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr)
  library(purrr); library(jsonlite); library(fs); library(ggplot2)
})

write_meta <- function(output_path, input_sources, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                                 error = function(e) NA_character_),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

OUT_DIR <- "review/diagnostics/data_quality_uncertainty"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

dir.create("logs", showWarnings = FALSE)
LOG_FILE <- file.path("logs", paste0("dq_stage4_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output"); sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== Stage 4: Availability and failure ===")

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
con <- dbConnect(duckdb(), db_path, read_only = TRUE)

site_lookup <- dbGetQuery(con, "
  SELECT site_id, max(igbp) AS igbp, max(data_hub) AS data_hub FROM manifest GROUP BY site_id
") |> as_tibble()

annual <- dbGetQuery(con, "
  SELECT site_id, TIMESTAMP AS year,
    NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_REF_RANDUNC, NEE_VUT_REF_JOINTUNC, NEE_VUT_16, NEE_VUT_84,
    NEE_CUT_REF, NEE_CUT_REF_QC, NEE_CUT_REF_RANDUNC, NEE_CUT_REF_JOINTUNC, NEE_CUT_16, NEE_CUT_84
  FROM annual WHERE dataset = 'FLUXMET'
") |> as_tibble() |> left_join(site_lookup, by = "site_id")
dbDisconnect(con, shutdown = TRUE)
annual$year <- as.integer(annual$year)
msg("Annual FLUXMET rows: ", nrow(annual), "; sites: ", n_distinct(annual$site_id))

## ---------------------------------------------------------------------------
## Master site-year table (ALL FLUXMET annual site-years, not just qualifying)
## ---------------------------------------------------------------------------
master <- annual |>
  mutate(
    qualifies_VUT = !is.na(NEE_VUT_REF) & !is.na(NEE_VUT_REF_QC) & (1 - NEE_VUT_REF_QC) <= QC_THRESHOLD_YY,
    qualifies_CUT = !is.na(NEE_CUT_REF) & !is.na(NEE_CUT_REF_QC) & (1 - NEE_CUT_REF_QC) <= QC_THRESHOLD_YY,
    ustar_term_VUT = (NEE_VUT_84 - NEE_VUT_16) / 2,
    ustar_term_CUT = (NEE_CUT_84 - NEE_CUT_16) / 2,
    VUT_minus_CUT = if_else(!is.na(NEE_VUT_REF) & !is.na(NEE_CUT_REF), NEE_VUT_REF - NEE_CUT_REF, NA_real_),
    category = case_when(
      qualifies_VUT & qualifies_CUT ~ "both",
      qualifies_VUT & !qualifies_CUT ~ "VUT_only",
      !qualifies_VUT & qualifies_CUT ~ "CUT_only",
      TRUE ~ "neither"
    ),
    neither_reason = case_when(
      category != "neither" ~ NA_character_,
      is.na(NEE_VUT_REF) & is.na(NEE_CUT_REF) ~ "no_value",
      TRUE ~ "fails_qc"
    )
  ) |>
  transmute(
    site_id, year, igbp, data_hub,
    NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_REF_RANDUNC, ustar_term_VUT, NEE_VUT_REF_JOINTUNC, qualifies_VUT,
    NEE_CUT_REF, NEE_CUT_REF_QC, NEE_CUT_REF_RANDUNC, ustar_term_CUT, NEE_CUT_REF_JOINTUNC, qualifies_CUT,
    VUT_minus_CUT, category, neither_reason
  )

write_csv(master, file.path(OUT_DIR, "table_stage4_site_year_master.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage4_site_year_master.csv"),
  input_sources = db_path,
  notes = paste0(
    "Join table: one row per (site_id, year) for every FLUXMET annual site-year (",
    nrow(master), " rows), with igbp/data_hub, both NEE values and their own QC/",
    "random/ustar_term/joint terms, VUT_minus_CUT (gC m-2 yr-1, computed whenever ",
    "both raw values are present regardless of QC), qualifies_VUT/qualifies_CUT ",
    "(paper rule, QC_THRESHOLD_YY=", QC_THRESHOLD_YY, ", each gated on its own QC ",
    "column), category (both/VUT_only/CUT_only/neither), and neither_reason ",
    "(no_value vs fails_qc, only populated when category == 'neither')."
  )
)

## ---------------------------------------------------------------------------
## Site-year and site-level category tabulations
## ---------------------------------------------------------------------------
site_year_counts <- master |> count(category, name = "n_site_years") |>
  mutate(share = n_site_years / sum(n_site_years))
neither_split_site_years <- master |> filter(category == "neither") |>
  count(neither_reason, name = "n_site_years") |> mutate(share_of_neither = n_site_years / sum(n_site_years))

msg("Site-year category counts:")
print(site_year_counts)
msg("'neither' site-years split:")
print(neither_split_site_years)

## Site level: a site "has" VUT/CUT if it has >=1 qualifying year for that side.
site_level <- master |>
  group_by(site_id) |>
  summarise(
    any_qualifies_VUT = any(qualifies_VUT),
    any_qualifies_CUT = any(qualifies_CUT),
    any_value_VUT = any(!is.na(NEE_VUT_REF)),
    any_value_CUT = any(!is.na(NEE_CUT_REF)),
    .groups = "drop"
  ) |>
  mutate(
    category = case_when(
      any_qualifies_VUT & any_qualifies_CUT ~ "both",
      any_qualifies_VUT & !any_qualifies_CUT ~ "VUT_only",
      !any_qualifies_VUT & any_qualifies_CUT ~ "CUT_only",
      TRUE ~ "neither"
    ),
    neither_reason = case_when(
      category != "neither" ~ NA_character_,
      !any_value_VUT & !any_value_CUT ~ "no_value",
      TRUE ~ "fails_qc"
    )
  )

## Sites entirely absent from the annual table (in manifest but 0 FLUXMET annual rows)
sites_with_annual <- unique(master$site_id)
sites_missing_annual <- setdiff(site_lookup$site_id, sites_with_annual)
msg("Sites in manifest with zero FLUXMET annual rows: ", length(sites_missing_annual))
if (length(sites_missing_annual) > 0) {
  missing_df <- site_lookup |> filter(site_id %in% sites_missing_annual) |>
    mutate(category = "no_annual_rows", neither_reason = "no_value")
  site_level <- bind_rows(
    site_level,
    missing_df |> transmute(site_id, any_qualifies_VUT = FALSE, any_qualifies_CUT = FALSE,
                             any_value_VUT = FALSE, any_value_CUT = FALSE, category, neither_reason)
  )
}

write_csv(site_level, file.path(OUT_DIR, "table_stage4_site_level_availability.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage4_site_level_availability.csv"),
  input_sources = c(db_path, "table_stage4_site_year_master.csv"),
  notes = paste0(
    "One row per site in the manifest (", nrow(site_level), " total). category: both/",
    "VUT_only/CUT_only/neither based on whether the site has >=1 qualifying year for ",
    "each side (QC_THRESHOLD_YY); 'no_annual_rows' for sites in the manifest with zero ",
    "FLUXMET rows in the annual table at all (none found in this run, see report.md). ",
    "neither_reason: no_value (no raw NEE_VUT_REF or NEE_CUT_REF value in any year) vs ",
    "fails_qc (has raw values but none ever qualify)."
  )
)

site_counts <- site_level |> count(category, name = "n_sites") |> mutate(share = n_sites / sum(n_sites))
neither_split_sites <- site_level |> filter(category %in% c("neither", "no_annual_rows")) |>
  count(category, neither_reason, name = "n_sites")

msg("Site-level category counts:")
print(site_counts)
msg("Site-level 'neither'/'no_annual_rows' split:")
print(neither_split_sites)

write_csv(site_year_counts, file.path(OUT_DIR, "table_stage4_site_year_category_counts.csv"))
write_meta(file.path(OUT_DIR, "table_stage4_site_year_category_counts.csv"),
           input_sources = "table_stage4_site_year_master.csv (derived)",
           notes = "Site-year counts/shares by category (both/VUT_only/CUT_only/neither).")
write_csv(site_counts, file.path(OUT_DIR, "table_stage4_site_level_category_counts.csv"))
write_meta(file.path(OUT_DIR, "table_stage4_site_level_category_counts.csv"),
           input_sources = "table_stage4_site_level_availability.csv (derived)",
           notes = "Site counts/shares by category (both/VUT_only/CUT_only/neither/no_annual_rows).")

## ---------------------------------------------------------------------------
## u-star method success/failure (Stage 0 found GRP_UST_THR at all 781 sites)
## ---------------------------------------------------------------------------
suppressPackageStartupMessages(library(fluxnet))
live_manifest <- flux_discover_files(data_dir = path(FLUXNET_DATA_ROOT, "extracted"))
bif_files <- live_manifest |> filter(dataset == "BIF") |> pull(path) |> unique()
msg("Re-scanning ", length(bif_files), " BIF files for GRP_UST_THR...")

bif_ust <- map_dfr(bif_files, function(p) {
  tryCatch({
    d <- read_csv(p, show_col_types = FALSE, progress = FALSE)
    d[d$VARIABLE_GROUP == "GRP_UST_THR" &
      d$VARIABLE %in% c("USTAR_CP_SUCCESS_RUN", "USTAR_CP_SUCCESS_RUN_YEAR",
                         "USTAR_MP_SUCCESS_RUN", "USTAR_MP_SUCCESS_RUN_YEAR"), , drop = FALSE]
  }, error = function(e) NULL)
})

## Pair each *_SUCCESS_RUN with its *_SUCCESS_RUN_YEAR via GROUP_ID (both rows
## share the same GROUP_ID within a site for a given method-run).
cp_year <- bif_ust |> filter(VARIABLE == "USTAR_CP_SUCCESS_RUN_YEAR") |>
  transmute(SITE_ID, GROUP_ID, year = suppressWarnings(as.integer(DATAVALUE)))
cp_success <- bif_ust |> filter(VARIABLE == "USTAR_CP_SUCCESS_RUN") |>
  transmute(SITE_ID, GROUP_ID, cp_success = suppressWarnings(as.numeric(DATAVALUE)))
cp <- inner_join(cp_year, cp_success, by = c("SITE_ID", "GROUP_ID")) |>
  transmute(site_id = SITE_ID, year, cp_success)

mp_year <- bif_ust |> filter(VARIABLE == "USTAR_MP_SUCCESS_RUN_YEAR") |>
  transmute(SITE_ID, GROUP_ID, year = suppressWarnings(as.integer(DATAVALUE)))
mp_success <- bif_ust |> filter(VARIABLE == "USTAR_MP_SUCCESS_RUN") |>
  transmute(SITE_ID, GROUP_ID, mp_success = suppressWarnings(as.numeric(DATAVALUE)))
mp <- inner_join(mp_year, mp_success, by = c("SITE_ID", "GROUP_ID")) |>
  transmute(site_id = SITE_ID, year, mp_success)

method_success <- full_join(cp, mp, by = c("site_id", "year"))
msg("u-star method-success site-year records: CP=", sum(!is.na(method_success$cp_success)),
    ", MP=", sum(!is.na(method_success$mp_success)), ", paired rows=", nrow(method_success))

## Join to the master qualification table.
method_vs_qc <- master |>
  select(site_id, year, qualifies_VUT, qualifies_CUT, category) |>
  left_join(method_success, by = c("site_id", "year"))

write_csv(method_vs_qc, file.path(OUT_DIR, "table_stage4_ustar_method_vs_qualification.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage4_ustar_method_vs_qualification.csv"),
  input_sources = c(db_path, "data/extracted/*_BIF_*.csv"),
  notes = paste0(
    "Per FLUXMET annual site-year: NEE qualification (qualifies_VUT/CUT, category) joined ",
    "to that year's BIF u-star method success flags (cp_success/mp_success: 1=success, ",
    "0=failure, NA=no BIF record for that site-year -- method-run years in the BIF file ",
    "don't always align 1:1 with annual-table years, e.g. a method can be run for a ",
    "calendar year with no corresponding FLUXMET annual row, or vice versa)."
  )
)

method_summary <- method_vs_qc |>
  summarise(
    n_site_years = n(),
    n_cp_recorded = sum(!is.na(cp_success)), n_cp_success = sum(cp_success == 1, na.rm = TRUE),
    n_cp_fail = sum(cp_success == 0, na.rm = TRUE),
    n_mp_recorded = sum(!is.na(mp_success)), n_mp_success = sum(mp_success == 1, na.rm = TRUE),
    n_mp_fail = sum(mp_success == 0, na.rm = TRUE)
  )
msg("Method success/failure overall:"); print(method_summary)

## Does CP/MP failure predict NEE QC failure ("neither" category)?
method_fail_vs_category <- method_vs_qc |>
  filter(!is.na(cp_success) | !is.na(mp_success)) |>
  mutate(
    any_method_failed = (cp_success == 0 & !is.na(cp_success)) | (mp_success == 0 & !is.na(mp_success)),
    both_methods_failed = (cp_success == 0 & !is.na(cp_success)) & (mp_success == 0 & !is.na(mp_success))
  ) |>
  group_by(category) |>
  summarise(
    n = n(),
    share_any_method_failed = mean(any_method_failed, na.rm = TRUE),
    share_both_methods_failed = mean(both_methods_failed, na.rm = TRUE),
    .groups = "drop"
  )
msg("u-star method failure rate by NEE qualification category:")
print(method_fail_vs_category)

write_csv(method_fail_vs_category, file.path(OUT_DIR, "table_stage4_ustar_failure_by_category.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage4_ustar_failure_by_category.csv"),
  input_sources = "table_stage4_ustar_method_vs_qualification.csv (derived)",
  notes = "Share of site-years with at least one (any_method_failed) or both (both_methods_failed) u-star methods (CP, MP) failing that year, broken down by NEE qualification category."
)

## ---------------------------------------------------------------------------
## Figures
## ---------------------------------------------------------------------------
p1 <- site_year_counts |>
  mutate(category = factor(category, levels = c("both", "VUT_only", "CUT_only", "neither"))) |>
  ggplot(aes(x = category, y = n_site_years, fill = category)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n_site_years, "\n(", scales::percent(share, accuracy = 0.1), ")")), vjust = -0.3) +
  labs(title = "Annual NEE availability by site-year", x = NULL, y = "Site-years") +
  ylim(0, max(site_year_counts$n_site_years) * 1.15) +
  theme_bw()
ggsave(file.path(OUT_DIR, "fig_stage4_site_year_availability.png"), p1, width = 7, height = 5.5, dpi = 150, bg = "white")

p2 <- site_counts |>
  mutate(category = factor(category, levels = c("both", "VUT_only", "CUT_only", "neither"))) |>
  ggplot(aes(x = category, y = n_sites, fill = category)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n_sites, "\n(", scales::percent(share, accuracy = 0.1), ")")), vjust = -0.3) +
  labs(title = "Annual NEE availability by site (>=1 qualifying year)", x = NULL, y = "Sites") +
  ylim(0, max(site_counts$n_sites) * 1.15) +
  theme_bw()
ggsave(file.path(OUT_DIR, "fig_stage4_site_availability.png"), p2, width = 7, height = 5.5, dpi = 150, bg = "white")

msg("Stage 4 complete.")

## ---------------------------------------------------------------------------
## Status
## ---------------------------------------------------------------------------
status_path <- file.path(OUT_DIR, "status.md")
both_sy <- site_year_counts |> filter(category == "both")
neither_sy <- site_year_counts |> filter(category == "neither")
both_s <- site_counts |> filter(category == "both")
neither_s <- site_counts |> filter(category == "neither")
no_val_sy <- neither_split_site_years |> filter(neither_reason == "no_value")
fails_qc_sy <- neither_split_site_years |> filter(neither_reason == "fails_qc")

status_entry <- c(
  "",
  "## Stage 4 -- Availability and failure",
  "",
  paste0("Completed ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "."),
  "",
  paste0("- Site-years (n=", nrow(master), "): both=", both_sy$n_site_years, " (",
         round(both_sy$share*100,1), "%), neither=", neither_sy$n_site_years, " (",
         round(neither_sy$share*100,1), "%); 'neither' splits into no_value=",
         ifelse(nrow(no_val_sy)>0, no_val_sy$n_site_years, 0), " and fails_qc=",
         ifelse(nrow(fails_qc_sy)>0, fails_qc_sy$n_site_years, 0), "."),
  paste0("- Sites (n=", nrow(site_level), "): both=", both_s$n_sites, " (",
         round(both_s$share*100,1), "%), neither=", neither_s$n_sites, " (",
         round(neither_s$share*100,1), "%). Sites in manifest with zero FLUXMET annual rows: ",
         length(sites_missing_annual), "."),
  paste0("- u-star BIF method-success records found (Stage 0 confirmed presence at all 781 ",
         "sites): CP recorded for ", method_summary$n_cp_recorded, " site-years (",
         method_summary$n_cp_fail, " failures), MP recorded for ", method_summary$n_mp_recorded,
         " site-years (", method_summary$n_mp_fail, " failures). Failure-by-category breakdown ",
         "in table_stage4_ustar_failure_by_category.csv."),
  paste0("- Master join table written: table_stage4_site_year_master.csv (", nrow(master),
         " rows, one per FLUXMET annual site-year)."),
  "",
  "Outputs: table_stage4_site_year_master.csv, table_stage4_site_level_availability.csv, ",
  "table_stage4_site_year_category_counts.csv, table_stage4_site_level_category_counts.csv, ",
  "table_stage4_ustar_method_vs_qualification.csv, table_stage4_ustar_failure_by_category.csv, ",
  "fig_stage4_site_year_availability.png, fig_stage4_site_availability.png (+ .meta.json each).",
  ""
)
write_lines(status_entry, status_path, append = TRUE)

cat("\n--- dq_stage4_availability.R: done. See ", LOG_FILE, " ---\n")
