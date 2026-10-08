## supp_stage4_bowen_ratio_by_igbp.R
##
## Unattended supplementary run, Stage 4: Bowen ratio (H/LE) by IGBP class.
##
## Reads the pre-QC `annual` FLUXMET rows directly from DuckDB (never
## annual_qc/annual_converted -- same reasoning as
## R/site_annual_fluxes.R::compute_site_annual_fluxes(): a row can have a
## perfectly good H_F_MDS_QC/LE_F_MDS_QC even when its NEE QC column fails,
## and annual_qc drops the whole row on the NEE gate). Each variable is
## gated on its OWN QC column (the paper's rule): a site-year qualifies for
## H when H_F_MDS_QC is non-NA and (1 - H_F_MDS_QC) <= QC_THRESHOLD_YY,
## independently for LE_F_MDS/LE_F_MDS_QC -- identical formula to
## .compute_site_annual_fluxes_core()'s et_qualifies/h_qualifies, not
## reimplemented differently. A site-year is used for Bowen ratio only when
## BOTH qualify.
##
## H_F_MDS and LE_F_MDS are both already native W m-2 MEAN RATES at every
## resolution including YY (R/site_annual_fluxes.R's own header comment:
## "H_F_MDS is already a mean rate at every resolution, not a pre-integrated
## total") -- so H/LE is already a same-units ratio with no conversion step
## needed (fluxnet_convert_units() is for turning a mean rate into a period
## TOTAL, e.g. MJ m-2 yr-1 or mm yr-1; the ratio of two mean rates in the
## same native unit is unaffected by that conversion either way).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")
source("R/plot_constants.R")

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(DBI); library(duckdb); library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Stage 4: Bowen ratio by IGBP class ===")

OUT_DIR <- "review/figures/draft_manuscript_v1/SupTables"
fs::dir_create(OUT_DIR)

CURRENT_SNAPSHOT <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
DB_PATH          <- file.path(FLUXNET_DATA_ROOT, "duckdb", "fluxnet.duckdb")

current_sites <- read_csv(CURRENT_SNAPSHOT, show_col_types = FALSE) |>
  distinct(site_id, .keep_all = TRUE) |>
  select(site_id, igbp)

if (!file.exists(DB_PATH)) stop("DuckDB database not found: ", DB_PATH)
con <- dbConnect(duckdb(), dbdir = DB_PATH, read_only = TRUE)

## ---- Report H_CORR / LE_CORR existence (do not compute with them) --------
annual_fields <- dbListFields(con, "annual")
has_h_corr  <- "H_CORR"  %in% annual_fields
has_le_corr <- "LE_CORR" %in% annual_fields
corr_df <- dbGetQuery(con, sprintf(
  "SELECT site_id, %s AS H_CORR, %s AS LE_CORR FROM annual WHERE dataset = 'FLUXMET'",
  if (has_h_corr) "H_CORR" else "NULL", if (has_le_corr) "LE_CORR" else "NULL"
))
n_sites_h_corr  <- if (has_h_corr)  n_distinct(corr_df$site_id[!is.na(corr_df$H_CORR)])  else 0L
n_sites_le_corr <- if (has_le_corr) n_distinct(corr_df$site_id[!is.na(corr_df$LE_CORR)]) else 0L
msg("H_CORR column present in annual table: ", has_h_corr, " (", n_sites_h_corr, " / ",
    nrow(current_sites), " current-network sites have >=1 non-NA value; NOT used below)")
msg("LE_CORR column present in annual table: ", has_le_corr, " (", n_sites_le_corr, " / ",
    nrow(current_sites), " current-network sites have >=1 non-NA value; NOT used below)")

## ---- Pull H_F_MDS / LE_F_MDS + their own QC columns -----------------------
annual <- dbGetQuery(con, "
  SELECT site_id, TIMESTAMP AS year, H_F_MDS, H_F_MDS_QC, LE_F_MDS, LE_F_MDS_QC
  FROM annual WHERE dataset = 'FLUXMET'
")
dbDisconnect(con, shutdown = TRUE)
annual <- annual |> filter(site_id %in% current_sites$site_id)
msg("Annual FLUXMET rows (current network): ", nrow(annual), " (",
    n_distinct(annual$site_id), " sites)")

d <- annual |>
  mutate(
    h_qualifies  = !is.na(H_F_MDS)  & !is.na(H_F_MDS_QC)  & (1 - H_F_MDS_QC)  <= QC_THRESHOLD_YY,
    le_qualifies = !is.na(LE_F_MDS) & !is.na(LE_F_MDS_QC) & (1 - LE_F_MDS_QC) <= QC_THRESHOLD_YY,
    both_qualify = h_qualifies & le_qualifies
  )
n_both <- sum(d$both_qualify)
msg("Site-years qualifying on BOTH H_F_MDS_QC and LE_F_MDS_QC (QC_THRESHOLD_YY=",
    QC_THRESHOLD_YY, "): ", n_both, " / ", nrow(d))

qualifying <- d |> filter(both_qualify)
n_le_nonpos <- sum(qualifying$LE_F_MDS <= 0)
msg("Dropping ", n_le_nonpos, " site-year(s) with LE_F_MDS <= 0 (Bowen ratio undefined/not ",
    "meaningful with non-positive LE).")
qualifying <- qualifying |> filter(LE_F_MDS > 0)

qualifying <- qualifying |> mutate(bowen = H_F_MDS / LE_F_MDS)
msg("Site-years used for Bowen ratio: ", nrow(qualifying), " (", n_distinct(qualifying$site_id), " sites)")

## ---- Site value = median over its qualifying site-years ------------------
site_values <- qualifying |>
  group_by(site_id) |>
  summarise(n_site_years = n(), bowen_site_median = median(bowen), .groups = "drop") |>
  left_join(current_sites, by = "site_id") |>
  mutate(igbp = if_else(igbp %in% PAPER_IGBP_ORDER, igbp, NA_character_))

n_nonstandard <- sum(is.na(site_values$igbp))
if (n_nonstandard > 0L) {
  msg(n_nonstandard, " site(s) with a non-standard/missing IGBP class excluded from by-class rows ",
      "(still counted in Total).")
}

## ---- Per-class summary -----------------------------------------------------
summarise_class <- function(df, label) {
  tibble::tibble(
    igbp_class    = label,
    n_sites       = nrow(df),
    n_site_years  = sum(df$n_site_years),
    bowen_median  = stats::median(df$bowen_site_median),
    bowen_p25     = stats::quantile(df$bowen_site_median, 0.25, names = FALSE),
    bowen_p75     = stats::quantile(df$bowen_site_median, 0.75, names = FALSE),
    flag_small_n  = nrow(df) < 5L
  )
}

by_class <- lapply(PAPER_IGBP_ORDER, function(cl) {
  summarise_class(site_values |> filter(igbp == cl), cl)
}) |> bind_rows() |> filter(n_sites > 0L)

total_row <- summarise_class(site_values, "Total")

table4 <- bind_rows(by_class, total_row)
print(table4, n = Inf, width = Inf)

out_path <- file.path(OUT_DIR, "tableS4_bowen_ratio_by_igbp.csv")
write_csv(table4, out_path)
write_output_metadata(
  out_path,
  input_sources = c(DB_PATH, CURRENT_SNAPSHOT),
  notes = paste0(
    "Bowen ratio (H_F_MDS / LE_F_MDS, both native W m-2 mean rates, no unit conversion needed) by ",
    "IGBP class (PAPER_IGBP_ORDER) and in total, current (781-site) network. Site-years require ",
    "BOTH H_F_MDS_QC and LE_F_MDS_QC to independently satisfy QC_THRESHOLD_YY (same formula as ",
    "R/site_annual_fluxes.R's h_qualifies/et_qualifies), read from the pre-QC DuckDB `annual` ",
    "table (never annual_qc/annual_converted, which would drop rows on the unrelated NEE QC gate). ",
    n_le_nonpos, " site-year(s) with LE_F_MDS <= 0 dropped (of ", n_both, " site-years qualifying ",
    "on QC alone). Site value = median Bowen ratio over that site's own qualifying site-years; ",
    "bowen_median/bowen_p25/bowen_p75 are the median/25th/75th percentile of SITE values within ",
    "each class (not pooled site-years). flag_small_n = TRUE for classes with <5 sites. ",
    if (n_nonstandard > 0L) paste0(n_nonstandard, " site(s) with non-standard/missing IGBP excluded ",
      "from by-class rows (still in Total). ") else "",
    "H_CORR present in annual table: ", has_h_corr, " (", n_sites_h_corr, " current-network sites ",
    "have >=1 non-NA value). LE_CORR present: ", has_le_corr, " (", n_sites_le_corr, " sites). ",
    "Neither is used in this computation (task instruction: report only, do not compute with them)."
  )
)
msg("Saved: ", out_path)
msg("=== Stage 4 done ===")
