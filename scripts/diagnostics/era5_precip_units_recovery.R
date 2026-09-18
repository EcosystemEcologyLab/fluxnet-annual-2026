## era5_precip_units_recovery.R
##
## D4 counterfactual for the ERA5 precipitation-units diagnostic (D1-D3 in
## era5_precip_units.R, same directory). Read-only with respect to the
## existing pipeline: sources R/climate_classification.R and R/utils.R
## UNMODIFIED and calls their real, exported functions
## (compute_era5_monthly_climatology(), compute_site_koppen_era5(),
## classify_koppen_geiger()) rather than reimplementing them, to avoid
## introducing a second, independent bug into a counterfactual that is
## supposed to be trustworthy. Does NOT modify R/climate_classification.R,
## step5_compute_koppen_era5.R, any figure, legend, or snapshot CSV.
## Writes only new files under review/diagnostics/era5_precip_units/.
##
## D2 (era5_precip_units.R) found that NEITHER alternative formula (b:
## sum(P_ERA) with no day-weighting; c: mean(P_ERA)*365.25) reconciles the
## 26 excluded sites against WorldClim BIO12 -- variant (a), the pipeline's
## existing sum(P_ERA*days_in_month) formula, is itself the one that
## matches BIO12 for the non-JPF majority of the network (387/781 sites
## within +/-25%). The problem is not a formula/unit-application bug; it is
## that the raw P_ERA VALUES themselves are anomalously large at these
## sites (D3: hub-wide, concentrated in product_source_network == "JPF").
## "The variant D2 identifies as correct" is therefore variant (a) itself
## -- already in use. The one remaining degree of freedom this counter-
## factual can test is the CANDIDATE-YEAR WINDOW: R/climate_classification.R's
## KG_ERA5_PERIOD is hard-coded to 1991-2020 (R/pipeline_config.R:56); this
## script tests whether extending it to each site's own full available
## record (1991 through its last ERA5 year) recovers any of the 26 sites,
## since if the JPF-hub inflation is a persistent data characteristic
## rather than a period-specific glitch, extending the window should not
## help -- stated as a testable prediction, not assumed.

source("R/pipeline_config.R")
source("R/utils.R")
source("R/climate_classification.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(duckdb)
  library(DBI)
  library(fs)
})

SNAP <- "data/snapshots"
EXT  <- "data/external"
OUTD <- "review/diagnostics/era5_precip_units"
fs::dir_create(OUTD)

message("=== era5_precip_units_recovery.R (D4) ===")

KG_ERA5_MIN_YEARS <- 20L  # R/pipeline_config.R:59

excl_df <- readr::read_csv(file.path(OUTD, "table_excluded_sites.csv"), show_col_types = FALSE)
EXCLUDED_SITES <- excl_df$site_id
message("Excluded sites (from era5_precip_units.R output): ", length(EXCLUDED_SITES))

# ============================================================================
# 4a. RECOVERY UNDER EXTENDED CANDIDATE-YEAR WINDOW (variant a, the
#     D2-confirmed-correct formula; only the period changes)
# ============================================================================

message("\n================ D4a: recovery under extended year window ================")

con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
mo_all <- dbGetQuery(con, sprintf(
  "SELECT site_id, TIMESTAMP, TA_ERA, P_ERA FROM monthly WHERE dataset = 'ERA5' AND site_id IN (%s)",
  paste(sprintf("'%s'", EXCLUDED_SITES), collapse = ",")
))
dbDisconnect(con, shutdown = TRUE)

mo_all <- mo_all |> dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP))
last_year_by_site <- mo_all |> dplyr::group_by(site_id) |> dplyr::summarise(last_year = max(year), .groups = "drop")
message("Per-site last available ERA5 year (range): ", paste(range(last_year_by_site$last_year), collapse = "-"))

# ---- Legend (same parse as step5_compute_koppen_era5.R / earlier session's diagnostics) ----
leg_path <- file.path(EXT, "koppen_beck2023", "legend.txt")
leg_lines <- readLines(leg_path)
leg_data  <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]
legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
  m <- regmatches(ln, regexec("^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[", ln, perl = TRUE))[[1]]
  if (length(m) < 4L) return(NULL)
  data.frame(koppen_class_code = as.integer(m[2]), koppen_class = trimws(m[3]),
             koppen_class_name = trimws(m[4]), stringsAsFactors = FALSE)
}))
main_map_lbl <- c(A = "Tropical", B = "Arid", C = "Temperate", D = "Cold", E = "Polar")
legend_df <- legend_df |> dplyr::mutate(koppen_main = substr(koppen_class, 1L, 1L), koppen_main_name = main_map_lbl[koppen_main])

# ---- Run the REAL classification function once per site, period = 1991:its own last year ----
recovery_rows <- lapply(EXCLUDED_SITES, function(sid) {
  site_mo <- mo_all |> dplyr::filter(site_id == sid) |> dplyr::select(site_id, TIMESTAMP, TA_ERA, P_ERA)
  last_yr <- last_year_by_site$last_year[last_year_by_site$site_id == sid]
  res <- tryCatch(
    compute_site_koppen_era5(site_mo, legend = legend_df, period = c(1991L, last_yr)),
    error = function(e) data.frame(site_id = sid, n_years_used = NA_integer_, kg_class = NA_character_)
  )
  res$period_end_used <- last_yr
  res
})
recovery_df <- dplyr::bind_rows(recovery_rows)

n_recovered <- sum(!is.na(recovery_df$kg_class))
cat("\n-- D4a: recoverable under extended 1991-to-end-of-record window (variant a formula, unchanged) --\n")
cat("Recovered:", n_recovered, "/", length(EXCLUDED_SITES), "\n\n")
print(as.data.frame(recovery_df |> dplyr::select(site_id, period_end_used, n_years_used, map_mm, kg_class, koppen_twoletter, koppen_main)))

out_d4a <- file.path(OUTD, "table_d4a_recovery_extended_window.csv")
readr::write_csv(recovery_df, out_d4a)
write_output_metadata(out_d4a, input_sources = c("R/climate_classification.R (unmodified, sourced)", "data/duckdb/fluxnet.duckdb"),
  notes = "D4a: per-excluded-site classification attempt using the REAL, unmodified compute_site_koppen_era5()/compute_era5_monthly_climatology() functions, with period extended from the fixed 1991-2020 to 1991-through-each-site's-own-last-available-ERA5-year. Formula (variant a, sum(P_ERA*days_in_month)) is unchanged -- D2 found no alternative formula reconciles these sites, so only the candidate-year window is varied here.")
message("Saved: ", out_d4a)

# ============================================================================
# 4b. NETWORK FRACTION / JACCARD IMPACT OF ANY RECOVERED SITES
# ============================================================================

message("\n================ D4b: network fraction / Jaccard impact ================")

TL_ORDER <- c("Af","Am","Aw","BS","BW","Cf","Cs","Cw","Df","Ds","Dw","EF","ET")
MAIN_ORDER <- c("A","B","C","D","E")
N_TOTAL <- 781L

compute_j <- function(p, q) { p[is.na(p)]<-0; q[is.na(q)]<-0; sum(pmin(p,q))/sum(pmax(p,q)) }

kg_current <- readr::read_csv(file.path(SNAP, "site_koppen_era5.csv"), show_col_types = FALSE)
global_dist <- readr::read_csv(file.path(SNAP, "koppen_beck2023_global_distribution.csv"), show_col_types = FALSE)
p_tl <- vapply(TL_ORDER, function(tl) sum(global_dist$global_land_fraction[global_dist$koppen_twoletter==tl], na.rm=TRUE), numeric(1))
p_5  <- vapply(MAIN_ORDER, function(m) sum(global_dist$global_land_fraction[global_dist$koppen_main==m], na.rm=TRUE), numeric(1))

q_tl_before <- vapply(TL_ORDER, function(tl) sum(kg_current$koppen_twoletter==tl, na.rm=TRUE), numeric(1)) / N_TOTAL
q_5_before  <- vapply(MAIN_ORDER, function(m) sum(kg_current$koppen_main==m, na.rm=TRUE), numeric(1)) / N_TOTAL
j_tl_before <- compute_j(p_tl, q_tl_before); j_5_before <- compute_j(p_5, q_5_before)

recovered <- recovery_df |> dplyr::filter(!is.na(kg_class))
kg_after <- kg_current |>
  dplyr::rows_update(
    recovered |> dplyr::transmute(site_id, koppen_twoletter = koppen_twoletter, koppen_main = koppen_main),
    by = "site_id"
  )
q_tl_after <- vapply(TL_ORDER, function(tl) sum(kg_after$koppen_twoletter==tl, na.rm=TRUE), numeric(1)) / N_TOTAL
q_5_after  <- vapply(MAIN_ORDER, function(m) sum(kg_after$koppen_main==m, na.rm=TRUE), numeric(1)) / N_TOTAL
j_tl_after <- compute_j(p_tl, q_tl_after); j_5_after <- compute_j(p_5, q_5_after)

cat("\n-- Jaccard before/after recovery (", n_recovered, " sites recovered, of 26) --\n")
cat(sprintf("two-letter: J_before=%.4f  J_after=%.4f  delta=%+.4f\n", j_tl_before, j_tl_after, j_tl_after - j_tl_before))
cat(sprintf("5-class:    J_before=%.4f  J_after=%.4f  delta=%+.4f\n", j_5_before, j_5_after, j_5_after - j_5_before))

out_d4b <- file.path(OUTD, "table_d4b_jaccard_impact.csv")
readr::write_csv(data.frame(
  level = c("twoletter","5class"), n_recovered = n_recovered,
  j_before = c(j_tl_before, j_5_before), j_after = c(j_tl_after, j_5_after),
  delta = c(j_tl_after - j_tl_before, j_5_after - j_5_before)
), out_d4b)
write_output_metadata(out_d4b, input_sources = c(out_d4a, file.path(SNAP, "site_koppen_era5.csv"), file.path(SNAP, "koppen_beck2023_global_distribution.csv")),
  notes = "Counterfactual two-letter/5-class weighted Jaccard for current_781 KG axis if the D4a-recoverable sites were added, network_frac denominator fixed at 781 (matching the pipeline's own dilution convention), against the UNCHANGED Beck 2023 global distribution.")
message("Saved: ", out_d4b)

# ============================================================================
# 4c. RELATIVE SCREEN (factor-of-2 departure from BIO12) VS. CURRENT ABSOLUTE RULE
# ============================================================================

message("\n================ D4c: relative screen vs. absolute rule ================")

site_year <- readr::read_csv(file.path(OUTD, "table_d1_site_year_variants.csv"), show_col_types = FALSE)
d1 <- readr::read_csv(file.path(OUTD, "table_d1_per_site.csv"), show_col_types = FALSE)

sy <- site_year |> dplyr::left_join(dplyr::select(d1, site_id, bio12_mm), by = "site_id") |>
  dplyr::mutate(ratio_to_bio12 = map_a / bio12_mm,
                flag_absolute = map_a > 5000,
                flag_relative = ratio_to_bio12 > 2 | ratio_to_bio12 < 0.5)

cmp <- sy |> dplyr::summarise(
  n_site_years = dplyr::n(),
  n_site_years_absolute = sum(flag_absolute, na.rm=TRUE),
  n_site_years_relative = sum(flag_relative, na.rm=TRUE),
  n_sites_absolute = length(unique(site_id[flag_absolute])),
  n_sites_relative = length(unique(site_id[flag_relative]))
)
cat("\n-- Site-year and site counts flagged: current absolute (>5000mm) vs. relative (>2x or <0.5x BIO12) --\n")
print(as.data.frame(cmp))

# Sites fully excluded under each rule (ALL candidate years flagged -> 0 valid years)
sites_all_years <- sy |> dplyr::group_by(site_id) |> dplyr::summarise(n_total = dplyr::n(), .groups="drop")
fully_out_absolute <- sy |> dplyr::group_by(site_id) |> dplyr::summarise(n_ok = sum(!flag_absolute), .groups="drop") |> dplyr::filter(n_ok < KG_ERA5_MIN_YEARS)
fully_out_relative <- sy |> dplyr::group_by(site_id) |> dplyr::summarise(n_ok = sum(!flag_relative), .groups="drop") |> dplyr::filter(n_ok < KG_ERA5_MIN_YEARS)
cat("\nSites with <", KG_ERA5_MIN_YEARS, "valid years remaining (i.e. would fail to classify), absolute rule:", nrow(fully_out_absolute), "\n")
cat("Sites with <", KG_ERA5_MIN_YEARS, "valid years remaining, relative rule:", nrow(fully_out_relative), "\n")
cat("Overlap:", length(intersect(fully_out_absolute$site_id, fully_out_relative$site_id)), "\n")
cat("Sites the relative rule would newly exclude (not in current absolute-rule 26):\n")
print(setdiff(fully_out_relative$site_id, EXCLUDED_SITES))
cat("Sites the relative rule would newly RECOVER (in current 26, not in relative-rule failure list):\n")
print(setdiff(EXCLUDED_SITES, fully_out_relative$site_id))

out_d4c <- file.path(OUTD, "table_d4c_relative_vs_absolute_screen.csv")
readr::write_csv(sy, out_d4c)
write_output_metadata(out_d4c, input_sources = out_siteyear <- file.path(OUTD, "table_d1_site_year_variants.csv"),
  notes = sprintf("Per-site-year comparison of the current absolute (map_a>5000mm) screen vs. a relative screen (ratio to BIO12 >2x or <0.5x). Site-year counts: absolute=%d, relative=%d (of %d total). Sites failing to reach %d valid years: absolute=%d, relative=%d.",
    cmp$n_site_years_absolute, cmp$n_site_years_relative, cmp$n_site_years, KG_ERA5_MIN_YEARS, nrow(fully_out_absolute), nrow(fully_out_relative)))
message("Saved: ", out_d4c)

message("\n=== era5_precip_units_recovery.R (D4) complete ===")
