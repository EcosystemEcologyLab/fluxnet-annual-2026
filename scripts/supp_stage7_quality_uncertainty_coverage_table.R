## supp_stage7_quality_uncertainty_coverage_table.R
##
## Supplementary Table S3: quality and uncertainty variables by time step,
## promoted from the interim technical-validation candidate's
## table_tv1_column_coverage_by_site.csv (scripts/technical_validation_interim.R,
## review/technical_validation_interim/) into the paper's own supplementary
## material, per logs/prompts/2026-10-12_renumber_figures.md.
##
## Read-only promotion -- does not recompute coverage, does not touch
## review/technical_validation_interim/. One row per (family, category,
## column); one column per time step (daily/monthly/annual) giving the
## number of sites with at least one non-NA value for that column at that
## resolution.
##
## A column that does not exist at all in the product for a given resolution
## is "NA" in the source table (R's own NA, not a count) -- that is a
## different kind of absence from "0 sites have a value", so it is relabelled
## "not provided" here, per cell, not left blank or coerced to 0. A column
## that exists but happens to have zero non-NA values anywhere would still
## show 0, not "not provided" -- the source table draws that distinction
## already; this script only relabels the NA case.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Supplementary Table S3: quality and uncertainty variables by time step ===")

OUT_DIR <- "review/figures/draft_manuscript_v1/SupTables"
fs::dir_create(OUT_DIR)

SRC_PATH <- "review/technical_validation_interim/tables/table_tv1_column_coverage_by_site.csv"
if (!file.exists(SRC_PATH)) stop("Source table not found: ", SRC_PATH, call. = FALSE)

coverage <- read_csv(SRC_PATH, show_col_types = FALSE)
required_cols <- c("family", "category", "column", "n_sites_daily", "n_sites_monthly", "n_sites_annual")
missing_cols <- setdiff(required_cols, names(coverage))
if (length(missing_cols) > 0L) {
  stop("supp_stage7: source table is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
}
msg("Loaded ", nrow(coverage), " rows from ", SRC_PATH)

not_provided <- function(x) ifelse(is.na(x), "not provided", as.character(x))

table_s3 <- coverage |>
  mutate(
    n_sites_daily   = not_provided(n_sites_daily),
    n_sites_monthly = not_provided(n_sites_monthly),
    n_sites_annual  = not_provided(n_sites_annual)
  )
n_not_provided <- sum(table_s3 == "not provided")
msg("Relabelled ", n_not_provided, " NA cells (column absent at that resolution) as 'not provided'.")

out_path <- file.path(OUT_DIR, "tableS3_quality_uncertainty_coverage.csv")
write_csv(table_s3, out_path)
msg("Saved: ", out_path)

write_output_metadata(
  out_path,
  input_sources = SRC_PATH,
  notes = paste0(
    "Supplementary Table S3: quality and uncertainty variables by time step, promoted from the ",
    "interim technical-validation candidate's table_tv1_column_coverage_by_site.csv (",
    "review/technical_validation_interim/). n_sites_* = count(distinct site_id) with at least ",
    "one non-NA value for that column at that resolution; a variable with no column at all in ",
    "that resolution's table is marked 'not provided' (not blank or zero -- these are two ",
    "different kinds of absence). See logs/prompts/2026-10-12_renumber_figures.md."
  )
)

msg("=== Done ===")
