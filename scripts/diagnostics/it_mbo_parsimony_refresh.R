## it_mbo_parsimony_refresh.R
##
## Re-run of review/diagnostics/it_mbo_parsimony/report.md against the store
## refreshed in review/diagnostics/store_refresh_20260920/, for the same
## three sites: IT-MBo (test), US-HB4 (named outlier), FI-Hyy (clean
## control). Read-only with respect to both prior reports and any committed
## figure -- neither is edited here. No download performed by this script;
## if any file is found to belong to a superseded product it is reported and
## the script stops rather than fetching a replacement.
##
## FALSIFIABILITY STATEMENT (written before computing any refreshed number):
## The previous report attributed IT-MBo's ~21.25x DD/MM/YY-vs-HH
## precipitation inconsistency to comparing a June-2026 MM/DD/YY extraction
## against an HH file downloaded fresh the same September morning -- i.e. a
## stale-vintage artifact, not a defect in the distributed product itself.
## That explanation is FALSIFIED if, once every file (HH, DD, MM, YY) is
## confirmed current against today's live flux_listall() product_id (per
## review/diagnostics/store_audit/table_stage1_live_vs_june_all_sites.csv
## and this script's own BIF-file cross-check), IT-MBo's DD/MM/YY-vs-HH
## ratio is STILL ~21x. If, instead, the ratio collapses once all four
## resolutions are drawn from the same current product, stale vintage is
## the correct explanation and the original ~21.25x number is withdrawn.
## The corresponding falsification for the isolated 2013-01/2013-06 IT-MBo
## MM-resolution spike is: if the refreshed MM file's TIMESTAMP 201301
## value still implies ~4,017 mm/month, the spike is a real product defect
## unrelated to vintage; if it now agrees with the HH-summed total for that
## month, vintage explains that too.
##
## New code. Outputs to review/diagnostics/it_mbo_parsimony_refresh/.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(fluxnet)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(fs)
  library(digest)
})

OUTD <- "review/diagnostics/it_mbo_parsimony_refresh"
fs::dir_create(OUTD)

message("=== it_mbo_parsimony_refresh.R : ", Sys.time(), " ===")

SITES <- c("IT-MBo", "US-HB4", "FI-Hyy")
SITE_ROLE <- c("IT-MBo" = "test", "US-HB4" = "named outlier", "FI-Hyy" = "clean control")

PREVIOUS_PID <- c(
  "IT-MBo" = "enS2fTzGG_9PS5-51hqet8iH",
  "US-HB4" = "10.17190/AMF/2571130",
  "FI-Hyy" = "oIJ9cFYf8Q0e9nfMRRS-90vM"
)

# ============================================================================
# 1. Live product identifiers (fresh flux_listall(), not reused from a
#    snapshot on disk) vs. the identifiers quoted in the previous report
# ============================================================================
message("\n================ 1: live product identifiers vs. previous report ================")

live_manifest <- flux_listall()
message("Live manifest: ", nrow(live_manifest), " site-products.")
stopifnot(all(c("site_id", "product_id", "fluxnet_product_name", "first_year", "last_year",
                "oneflux_code_version", "data_hub") %in% names(live_manifest)))

live_ids <- live_manifest |>
  dplyr::filter(site_id %in% SITES) |>
  dplyr::select(site_id, data_hub, product_id, fluxnet_product_name, first_year, last_year, oneflux_code_version)

id_table <- tibble::tibble(site_id = SITES, role = SITE_ROLE[SITES],
                            product_id_previous = PREVIOUS_PID[SITES]) |>
  dplyr::left_join(live_ids, by = "site_id") |>
  dplyr::mutate(product_id_changed = product_id_previous != product_id)

print(as.data.frame(id_table))

out_id <- file.path(OUTD, "table_0_product_identifiers.csv")
readr::write_csv(id_table, out_id)
write_output_metadata(out_id,
  input_sources = "live fluxnet::flux_listall() call (this run) and the product_id values quoted in review/diagnostics/it_mbo_parsimony/report.md",
  notes = "Per-site live product_id/fluxnet_product_name/year-range from a fresh flux_listall() call, beside the product_id quoted in the previous parsimony report, with a changed/unchanged flag. IT-MBo and FI-Hyy's live product_id matches the value already quoted previously (that value was itself obtained from a live flux_listall() check per the earlier report's own PID section) -- both sites were independently confirmed reprocessed relative to the STALE ON-DISK JUNE EXTRACTION by review/diagnostics/store_audit/table_stage1_live_vs_june_all_sites.csv (product_id_june != product_id_live for both), not relative to the previously-reported identifier. US-HB4's product_id is unchanged in all three sources (June extraction, previous report, live today).")

# ============================================================================
# 2. File-level provenance and current-product verification. One directory
#    per site == one downloaded zip == one product; verified via the BIF
#    file's PRODUCT_NAME field (baked into the distributed archive) against
#    the live fluxnet_product_name, plus cross-reference to the independent
#    store_audit live-vs-June comparison and (for IT-MBo/FI-Hyy) the stage3b
#    download-and-verify log. sha256 computed for every raw file read.
# ============================================================================
message("\n================ 2: per-file product verification and provenance ================")

find_dir <- function(site_id) {
  d <- list.files("data/extracted", pattern = paste0("_", site_id, "_FLUXNET_"), full.names = TRUE)
  d <- d[dir.exists(d)]
  if (length(d) != 1L) stop(site_id, ": expected exactly 1 extraction directory, found ", length(d))
  d
}
find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(f) == 0L) return(NA_character_)
  f[[1L]]
}

store_audit_cmp <- readr::read_csv("review/diagnostics/store_audit/table_stage1_live_vs_june_all_sites.csv", show_col_types = FALSE) |>
  dplyr::filter(site_id %in% SITES)

stage3b_log <- if (file.exists("logs/store_refresh_stage3b_progress.csv")) {
  readr::read_csv("logs/store_refresh_stage3b_progress.csv", show_col_types = FALSE)
} else NULL

files_read <- list()
prov_rows <- list()
superseded_found <- FALSE

for (sid in SITES) {
  d <- find_dir(sid)
  bif_path <- find_file(d, "FLUXNET_BIF_.*\\.csv$")
  if (is.na(bif_path)) stop(sid, ": BIF file not found in ", d)
  bif <- readr::read_csv(bif_path, show_col_types = FALSE)
  bif_product_name <- bif |> dplyr::filter(VARIABLE == "PRODUCT_NAME") |> dplyr::pull(DATAVALUE)

  live_row <- dplyr::filter(id_table, site_id == sid)
  name_matches_live <- length(bif_product_name) == 1L && bif_product_name == live_row$fluxnet_product_name

  audit_row <- dplyr::filter(store_audit_cmp, site_id == sid)
  # Currency evidence, distinct from "does the file merely exist":
  #  - BIF's own embedded PRODUCT_NAME must equal today's live product name
  #  - store_audit's independent live-vs-June comparison (any_change) tells
  #    us whether the June-vintage on-disk copy was ever known to be stale
  #  - for sites that store_refresh actually re-downloaded (stage3b log),
  #    that log's own "downloaded_verified" status is direct evidence the
  #    files on disk now are the live-manifest files, not merely same-named
  downloaded_this_refresh <- !is.null(stage3b_log) && sid %in% stage3b_log$site_id &&
    any(stage3b_log$site_id == sid & stage3b_log$status == "downloaded_verified")

  is_current <- name_matches_live && (downloaded_this_refresh || !isTRUE(audit_row$any_change))
  cat(sprintf("\n%s: BIF PRODUCT_NAME='%s' | live fluxnet_product_name='%s' | matches=%s | store_audit any_change=%s | stage3b downloaded_verified=%s | VERDICT=%s\n",
              sid, bif_product_name, live_row$fluxnet_product_name, name_matches_live,
              audit_row$any_change, downloaded_this_refresh, ifelse(is_current, "CURRENT", "SUPERSEDED/UNVERIFIED")))

  if (!is_current) {
    superseded_found <- TRUE
    warning(sid, ": on-disk product could not be confirmed current against the live manifest -- per task instructions, STOPPING without downloading a replacement.")
    next
  }

  paths <- list(
    era_hh = find_file(d, "FLUXNET_ERA5_HH_.*\\.csv$"), era_dd = find_file(d, "FLUXNET_ERA5_DD_.*\\.csv$"),
    era_mm = find_file(d, "FLUXNET_ERA5_MM_.*\\.csv$"), era_yy = find_file(d, "FLUXNET_ERA5_YY_.*\\.csv$"),
    fm_hh  = find_file(d, "FLUXNET_FLUXMET_HH_.*\\.csv$"), fm_dd = find_file(d, "FLUXNET_FLUXMET_DD_.*\\.csv$"),
    fm_mm  = find_file(d, "FLUXNET_FLUXMET_MM_.*\\.csv$"), fm_yy = find_file(d, "FLUXNET_FLUXMET_YY_.*\\.csv$"),
    bif    = bif_path
  )
  missing <- names(paths)[vapply(paths, is.na, logical(1))]
  if (length(missing) > 0L) stop(sid, ": missing file(s) for ", paste(missing, collapse = ", "))
  files_read[[sid]] <- paths

  for (nm in names(paths)) {
    p <- paths[[nm]]
    prov_rows[[paste(sid, nm)]] <- tibble::tibble(
      site_id = sid, file_role = nm, path = p, product_id = live_row$product_id,
      sha256 = digest::digest(file = p, algo = "sha256"),
      byte_size = file.info(p)$size,
      mtime = format(file.info(p)$mtime, "%Y-%m-%dT%H:%M:%S%z"),
      feeds = "table_1_mean_annual_by_resolution.csv (all files); table_2_ts201301_worked_value.csv (fm_mm only, IT-MBo)"
    )
  }
}

if (superseded_found) {
  message("\nOne or more sites' on-disk product could not be confirmed current. Stopping per instructions -- no download attempted.")
}

ref_prov_path <- "review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv"
prov_rows[["ref"]] <- tibble::tibble(
  site_id = "ALL", file_role = "bio12_badm_reference", path = ref_prov_path, product_id = NA_character_,
  sha256 = digest::digest(file = ref_prov_path, algo = "sha256"),
  byte_size = file.info(ref_prov_path)$size,
  mtime = format(file.info(ref_prov_path)$mtime, "%Y-%m-%dT%H:%M:%S%z"),
  feeds = "table_1_mean_annual_by_resolution.csv (BIO12/BADM columns)"
)

prov <- dplyr::bind_rows(prov_rows) |> dplyr::arrange(site_id, file_role)
print(as.data.frame(prov))

out_prov <- file.path(OUTD, "table_4_provenance.csv")
readr::write_csv(prov, out_prov)
write_output_metadata(out_prov,
  input_sources = "every raw file read by this script (see path column)",
  notes = "Path, product_id, sha256, byte size, mtime, and which of this report's tables each file feeds. Only files from sites confirmed CURRENT against a live flux_listall() call (verified via BIF PRODUCT_NAME + store_audit's independent live-vs-June comparison + stage3b download log) are read past the verification step.")

if (superseded_found) stop("Stopping: at least one site's data could not be confirmed current. See console output above for which site and why.")

# ============================================================================
# 3. Table 1, exact previous shape: 6 rows (3 sites x {P_ERA, P_F}), HH/DD/MM/YY
#    mean annual precip (mm/yr) + n_years, plus BIO12/BADM. No ratios, no
#    factor fitting, no tolerance windows.
# ============================================================================
message("\n================ 3: Table 1 (refreshed), exact previous shape ================")

MISSING_MAX <- -9998

raw <- list()
for (sid in SITES) {
  paths <- files_read[[sid]]
  raw[[sid]] <- list(
    era_hh = readr::read_csv(paths$era_hh, show_col_types = FALSE) |>
      dplyr::mutate(date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"), year = lubridate::year(date)) |>
      dplyr::filter(P_ERA > MISSING_MAX),
    era_dd = readr::read_csv(paths$era_dd, show_col_types = FALSE) |>
      dplyr::mutate(date = as.Date(as.character(TIMESTAMP), format = "%Y%m%d"), year = lubridate::year(date)) |>
      dplyr::filter(P_ERA > MISSING_MAX),
    era_mm = readr::read_csv(paths$era_mm, show_col_types = FALSE) |>
      dplyr::mutate(year = TIMESTAMP %/% 100L, month = TIMESTAMP %% 100L) |>
      dplyr::filter(P_ERA > MISSING_MAX),
    era_yy = readr::read_csv(paths$era_yy, show_col_types = FALSE) |>
      dplyr::rename(year = TIMESTAMP) |> dplyr::filter(P_ERA > MISSING_MAX),
    fm_hh = readr::read_csv(paths$fm_hh, show_col_types = FALSE) |>
      dplyr::mutate(date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"), year = lubridate::year(date)) |>
      dplyr::filter(P_F > MISSING_MAX),
    fm_dd = readr::read_csv(paths$fm_dd, show_col_types = FALSE) |>
      dplyr::mutate(date = as.Date(as.character(TIMESTAMP), format = "%Y%m%d"), year = lubridate::year(date)) |>
      dplyr::filter(P_F > MISSING_MAX),
    fm_mm = readr::read_csv(paths$fm_mm, show_col_types = FALSE) |>
      dplyr::mutate(year = TIMESTAMP %/% 100L, month = TIMESTAMP %% 100L) |>
      dplyr::filter(P_F > MISSING_MAX),
    fm_yy = readr::read_csv(paths$fm_yy, show_col_types = FALSE) |>
      dplyr::rename(year = TIMESTAMP) |> dplyr::filter(P_F > MISSING_MAX)
  )
  message(sid, ": loaded refreshed HH/DD/MM/YY for ERA5 and FLUXMET")
}

expected_hh_per_day <- 48L

mean_annual_hh <- function(df, value_col) {
  per_day <- df |> dplyr::count(date, year, name = "n_hh")
  complete_days <- per_day |> dplyr::filter(n_hh == expected_hh_per_day)
  daily_sum <- df |> dplyr::inner_join(dplyr::select(complete_days, date), by = "date") |>
    dplyr::group_by(date, year) |> dplyr::summarise(day_total = sum(.data[[value_col]]), .groups = "drop")
  days_per_year <- daily_sum |> dplyr::count(year, name = "n_days")
  expected_days <- tibble::tibble(year = days_per_year$year,
                                    expected = ifelse(lubridate::leap_year(days_per_year$year), 366L, 365L))
  complete_years <- dplyr::inner_join(days_per_year, expected_days, by = "year") |> dplyr::filter(n_days == expected)
  annual <- daily_sum |> dplyr::inner_join(dplyr::select(complete_years, year), by = "year") |>
    dplyr::group_by(year) |> dplyr::summarise(annual_total = sum(day_total), .groups = "drop")
  list(mean_mm = mean(annual$annual_total), n_years = nrow(annual))
}
mean_annual_dd <- function(df, value_col) {
  per_year <- df |> dplyr::group_by(year) |>
    dplyr::summarise(n_days = dplyr::n(), annual_total = sum(.data[[value_col]]), .groups = "drop") |>
    dplyr::mutate(expected = ifelse(lubridate::leap_year(year), 366L, 365L)) |>
    dplyr::filter(n_days == expected)
  list(mean_mm = mean(per_year$annual_total), n_years = nrow(per_year))
}
mean_annual_mm <- function(df, value_col) {
  per_year <- df |> dplyr::mutate(days = lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", year, month)))) |>
    dplyr::group_by(year) |>
    dplyr::summarise(n_months = dplyr::n(), annual_total = sum(.data[[value_col]] * days), .groups = "drop") |>
    dplyr::filter(n_months == 12L)
  list(mean_mm = mean(per_year$annual_total), n_years = nrow(per_year))
}
mean_annual_yy <- function(df, value_col) list(mean_mm = mean(df[[value_col]]), n_years = nrow(df))

ref <- readr::read_csv(ref_prov_path, show_col_types = FALSE) |>
  dplyr::filter(site_id %in% SITES) |> dplyr::select(site_id, bio12_mm, badm_map_mm)

d1_rows <- list()
for (sid in SITES) {
  for (var_pair in list(c("era", "P_ERA"), c("fm", "P_F"))) {
    src <- var_pair[1]; col <- var_pair[2]
    hh <- mean_annual_hh(raw[[sid]][[paste0(src, "_hh")]], col)
    dd <- mean_annual_dd(raw[[sid]][[paste0(src, "_dd")]], col)
    mm <- mean_annual_mm(raw[[sid]][[paste0(src, "_mm")]], col)
    yy <- mean_annual_yy(raw[[sid]][[paste0(src, "_yy")]], col)
    d1_rows[[paste(sid, col)]] <- tibble::tibble(
      site_id = sid, variable = col,
      mean_hh_mm = hh$mean_mm, n_years_hh = hh$n_years,
      mean_dd_mm = dd$mean_mm, n_years_dd = dd$n_years,
      mean_mm_mm = mm$mean_mm, n_years_mm = mm$n_years,
      mean_yy_mm = yy$mean_mm, n_years_yy = yy$n_years
    )
  }
}
d1_refreshed <- dplyr::bind_rows(d1_rows) |> dplyr::left_join(ref, by = "site_id")
cat("\n-- Refreshed Table 1 --\n"); print(as.data.frame(d1_refreshed))

out_d1 <- file.path(OUTD, "table_1_mean_annual_by_resolution_refreshed.csv")
readr::write_csv(d1_refreshed, out_d1)
write_output_metadata(out_d1,
  input_sources = "data/extracted/*/*_{ERA5,FLUXMET}_{HH,DD,MM,YY}_*.csv (refreshed, confirmed current against live flux_listall(), read directly) and review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv (BIO12/BADM columns, read-only)",
  notes = "Re-run of review/diagnostics/it_mbo_parsimony/table_1_mean_annual_by_resolution.csv against the store refreshed 2026-09-20. Identical methodology (complete-coverage years only, no ratios, no factor fitting). 6 data rows (3 sites x {P_ERA, P_F}).")

# Previous table reproduced verbatim (from the previous report's own CSV) for
# a literal side-by-side, not re-derived.
d1_previous <- readr::read_csv("review/diagnostics/it_mbo_parsimony/table_1_mean_annual_by_resolution.csv", show_col_types = FALSE)
cat("\n-- Previous Table 1 (reproduced, not re-derived) --\n"); print(as.data.frame(d1_previous))

out_d1_prev <- file.path(OUTD, "table_1_mean_annual_by_resolution_previous_reproduced.csv")
readr::write_csv(d1_previous, out_d1_prev)
write_output_metadata(out_d1_prev,
  input_sources = "review/diagnostics/it_mbo_parsimony/table_1_mean_annual_by_resolution.csv (reproduced verbatim, not re-derived)",
  notes = "The previous report's Table 1, copied unmodified so it can sit beside table_1_mean_annual_by_resolution_refreshed.csv for a direct before-and-after. review/diagnostics/it_mbo_parsimony/report.md itself was not edited.")

# ============================================================================
# 4. One value worked in full: IT-MBo MM file, TIMESTAMP 201301
# ============================================================================
message("\n================ 4: IT-MBo TIMESTAMP 201301, worked in full ================")

itmbo_mm_raw <- readr::read_csv(files_read[["IT-MBo"]]$fm_mm, show_col_types = FALSE)
row_201301 <- itmbo_mm_raw |> dplyr::filter(TIMESTAMP == 201301)
if (nrow(row_201301) != 1L) stop("Expected exactly 1 row for TIMESTAMP 201301, found ", nrow(row_201301))

mm_qc_col <- intersect("P_F_QC", names(itmbo_mm_raw))
if (length(mm_qc_col) == 0L) stop("P_F_QC column not found in refreshed IT-MBo FLUXMET_MM file")

p_era_raw   <- row_201301$P_ERA
p_f_raw     <- row_201301$P_F
p_f_qc_raw  <- row_201301[[mm_qc_col]]
days_jan_2013 <- lubridate::days_in_month(as.Date("2013-01-01"))
mm_month_era <- p_era_raw * days_jan_2013
mm_month_f   <- p_f_raw * days_jan_2013

dario_value <- 1.856
agree_p_era <- isTRUE(all.equal(p_era_raw, dario_value, tolerance = 1e-6))
agree_p_f   <- isTRUE(all.equal(p_f_raw, dario_value, tolerance = 1e-6))

cat(sprintf(
  "\nIT-MBo, FLUXMET_MM, TIMESTAMP=201301:\n  P_ERA (raw, mm/d)   = %s\n  P_F   (raw, mm/d)   = %s\n  P_F_QC (0-1 scale)  = %s\n  days in month (Jan 2013) = %d\n  derived P_ERA mm/month = %s x %d = %s\n  derived P_F   mm/month = %s x %d = %s\n  Dario reports 1.856 mm d-1 for this record.\n  Agreement with P_ERA: %s\n  Agreement with P_F:   %s\n",
  p_era_raw, p_f_raw, p_f_qc_raw, days_jan_2013,
  p_era_raw, days_jan_2013, mm_month_era,
  p_f_raw, days_jan_2013, mm_month_f,
  agree_p_era, agree_p_f
))

t2 <- tibble::tibble(
  site_id = "IT-MBo", TIMESTAMP = 201301L,
  P_ERA_raw_mm_d = p_era_raw, P_F_raw_mm_d = p_f_raw, P_F_QC = p_f_qc_raw,
  days_in_month = days_jan_2013,
  derived_mm_month_P_ERA = mm_month_era, derived_mm_month_P_F = mm_month_f,
  dario_reported_mm_d = dario_value,
  agrees_with_P_ERA = agree_p_era, agrees_with_P_F = agree_p_f
)
out_t2 <- file.path(OUTD, "table_2_ts201301_worked_value.csv")
readr::write_csv(t2, out_t2)
write_output_metadata(out_t2,
  input_sources = files_read[["IT-MBo"]]$fm_mm,
  notes = "IT-MBo FLUXMET_MM, TIMESTAMP 201301: raw P_ERA and P_F as they appear in the refreshed file, P_F_QC, days in the month, and both variables' derived mm/month, for a hand-checkable comparison against Dario's reported 1.856 mm d-1.")

message("\n=== it_mbo_parsimony_refresh.R complete ===")
message("See report.md for the falsifiability verdict, PID table, and provenance.")
