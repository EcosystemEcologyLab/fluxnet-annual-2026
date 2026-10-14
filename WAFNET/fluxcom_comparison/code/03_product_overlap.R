## 03_product_overlap.R — Stage 1 pre-analysis report, part 4.
##
## Combines the QC-qualifying years/months computed in
## 01_observations_qc_overlap.R with the FLUXCOM product periods established
## by web research (see docs/report_back_<date>.md, section 1) to count, per
## site x variable x product, how many qualifying years/months fall inside
## each product's period. No FLUXCOM data is downloaded; this is a pure
## date-range intersection against periods documented in the literature.
##
## Product periods (comparison data, non-Shuttle -- CLAUDE.md Hard Rule #1):
##   - fluxcom_x_base: FLUXCOM-X-BASE, Nelson et al. (2024) doi:10.5194/bg-21-5079-2024.
##     Headline paper period 2001-2020; the ICOS-archived files extend one
##     year further (2001-2021) -- a discrepancy the source research could
##     not resolve. Both bounds are carried in the table.
##   - fluxcom_carbon_rs: Tramontana et al. (2016) doi:10.5194/bg-13-4291-2016,
##     period 2001-2015.
##   - fluxcom_carbon_rs_meteo: Jung et al. (2020) doi:10.5194/bg-17-1343-2020.
##     Period is forcing-dataset-dependent; "best case" below uses the
##     longest available forcing (CRU JRA, 1950-2017) -- see report_back for
##     the other four forcing variants (GSWP3, WFDEI, ERA5, CERES-GPCP),
##     which all give a SHORTER or differently-positioned period than this.
##   - fluxcom_energy_rs: Tramontana et al. (2016), period 2001-2015 (headline
##     global stats computed over 2001-2013, but the distributed monthly
##     files cover 2001-2015).
##   - fluxcom_energy_rs_meteo: Jung et al. (2019) doi:10.1038/s41597-019-0076-8.
##     Forcing-dependent; "best case" below uses CRUNCEPv8 (1950-2016) -- see
##     report_back for WFDEI/GSWP3/CERES-GPCP variants.
##
## Output (git-tracked):
##   tables/site_variable_product_overlap_yy.csv
##   tables/site_variable_product_overlap_mm.csv

source("WAFNET/fluxcom_comparison/code/00_config.R")

products <- data.frame(
  product = c("fluxcom_x_base", "fluxcom_carbon_rs", "fluxcom_carbon_rs_meteo_best",
              "fluxcom_energy_rs", "fluxcom_energy_rs_meteo_best"),
  start_year = c(2001, 2001, 1950, 2001, 1950),
  end_year   = c(2021, 2015, 2017, 2015, 2016),   # x_base uses the more generous ICOS-archive bound (2021); paper headline is 2020 -- see note above
  stringsAsFactors = FALSE
)

overlap_yy <- utils::read.csv(file.path(FLUXCOM_ROOT, "tables", "site_variable_qc_overlap_yy.csv"), stringsAsFactors = FALSE)
overlap_mm <- utils::read.csv(file.path(FLUXCOM_ROOT, "tables", "site_variable_qc_overlap_mm.csv"), stringsAsFactors = FALSE)

count_overlap_yy <- function(periods_str, start_year, end_year) {
  if (is.na(periods_str) || periods_str == "") return(0L)
  yrs <- as.integer(strsplit(periods_str, ";")[[1]])
  sum(yrs >= start_year & yrs <= end_year)
}
count_overlap_mm <- function(periods_str, start_year, end_year) {
  if (is.na(periods_str) || periods_str == "") return(0L)
  ym <- as.integer(strsplit(periods_str, ";")[[1]])
  yr <- ym %/% 100L
  sum(yr >= start_year & yr <= end_year)
}

build <- function(overlap_df, count_fn) {
  rows <- list()
  for (i in seq_len(nrow(overlap_df))) {
    for (p in seq_len(nrow(products))) {
      n <- count_fn(overlap_df$qualifying_periods[i], products$start_year[p], products$end_year[p])
      rows[[length(rows) + 1]] <- data.frame(
        site_id = overlap_df$site_id[i],
        variable = overlap_df$variable[i],
        product = products$product[p],
        n_qualifying_total = overlap_df$n_qualifying[i],
        n_overlapping_with_product = n
      )
    }
  }
  do.call(rbind, rows)
}

overlap_product_yy <- build(overlap_yy, count_overlap_yy)
overlap_product_mm <- build(overlap_mm, count_overlap_mm)

write.csv(overlap_product_yy, file.path(FLUXCOM_ROOT, "tables", "site_variable_product_overlap_yy.csv"), row.names = FALSE)
write.csv(overlap_product_mm, file.path(FLUXCOM_ROOT, "tables", "site_variable_product_overlap_mm.csv"), row.names = FALSE)

cat("\n[WAFNET/fluxcom_comparison] Annual (YY) overlap, NEE only, all products:\n")
print(overlap_product_yy[overlap_product_yy$variable == "NEE", ])
cat("\n[WAFNET/fluxcom_comparison] BJ-Db1 and SN-Nkr flagged sites, all variables, YY:\n")
print(overlap_product_yy[overlap_product_yy$site_id %in% c("BJ-Db1", "SN-Nkr"), ])
cat("\nWrote tables/site_variable_product_overlap_yy.csv, tables/site_variable_product_overlap_mm.csv\n")
