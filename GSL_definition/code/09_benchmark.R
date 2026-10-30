## 09_benchmark.R — GSL figure rebuild, Benchmark table.
##
## For each site with >= 10 years, the correlation between annual GSL and
## annual GPP across years, for Z17 (fixed 10% of P99) and "the new method"
## (T95, the fraction-of-annual-GPP-based threshold selection -- framework
## "Derived quantities"/Figure 4), plus the median across sites by
## functional group. Annual GPP sum per site-year is a new, simple
## aggregation (not persisted by 03_derive.R) computed here from
## daily_extract.rds; gsl_z17/gsl_t95 per site-year are reused directly from
## derived_site_year.csv, not recomputed.

source("GSL_definition/code/00_config.R")
source("GSL_definition/code/fig_helpers.R")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr); library(lubridate) })

derived_site <- read_csv(file.path(GSL_ROOT, "tables", "derived_site.csv"), show_col_types = FALSE) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main") |>
  left_join(GSL_SITES |> select(site_id, group), by = "site_id")
derived_site_year <- read_csv(file.path(GSL_ROOT, "tables", "derived_site_year.csv"), show_col_types = FALSE) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main")

daily_extract <- readRDS(file.path(GSL_ROOT, "data", "daily_extract.rds"))
daily_extract$TIMESTAMP <- as.Date(as.character(daily_extract$TIMESTAMP))
daily_extract$year <- lubridate::year(daily_extract$TIMESTAMP)
gpp_col <- GSL_GPP_VARIANTS[["main"]]

trend_sites <- GSL_SITES_GE10YR
stopifnot(length(trend_sites) == 21L)

annual_gpp <- daily_extract |>
  filter(site_id %in% trend_sites) |>
  mutate(gpp = pmax(.data[[gpp_col]], 0)) |>
  group_by(site_id, year) |>
  summarise(annual_gpp_sum = sum(gpp, na.rm = TRUE), .groups = "drop")

benchmark_data <- derived_site_year |>
  filter(site_id %in% trend_sites) |>
  inner_join(annual_gpp, by = c("site_id", "year"))

site_correlations <- benchmark_data |>
  group_by(site_id) |>
  summarise(
    n_years = n(),
    cor_z17 = cor(gsl_z17, annual_gpp_sum, use = "complete.obs"),
    cor_t95 = cor(gsl_t95, annual_gpp_sum, use = "complete.obs"),
    .groups = "drop"
  ) |>
  left_join(derived_site |> select(site_id, group, skewness), by = "site_id") |>
  arrange(skewness)

group_medians <- site_correlations |>
  group_by(group) |>
  summarise(
    n_sites = n(), median_cor_z17 = median(cor_z17, na.rm = TRUE),
    median_cor_t95 = median(cor_t95, na.rm = TRUE), .groups = "drop"
  )
overall_median <- tibble(
  group = "ALL (21 sites)", n_sites = nrow(site_correlations),
  median_cor_z17 = median(site_correlations$cor_z17, na.rm = TRUE),
  median_cor_t95 = median(site_correlations$cor_t95, na.rm = TRUE)
)
group_medians <- bind_rows(group_medians, overall_median)

if (!dir.exists(file.path(GSL_ROOT, "tables"))) dir.create(file.path(GSL_ROOT, "tables"), recursive = TRUE)
write_csv(site_correlations, file.path(GSL_ROOT, "tables", "benchmark_site_correlations.csv"))
write_csv(group_medians, file.path(GSL_ROOT, "tables", "benchmark_group_medians.csv"))

note_text <- paste0(
  "Correlation (Pearson) of annual GSL with annual GPP sum, across each site's own years, for ",
  "Z17 (fixed threshold, 10% of P99) and the new method (T95, the threshold where mean included ",
  "fraction of annual GPP first drops to <=0.95). Both definitions count days above a fixed ",
  "threshold, so neither is favoured by construction (framework.md, Benchmark table). 21 sites ",
  "with >= 10 years after d80 screening."
)
write_meta(
  file.path(GSL_ROOT, "tables", "benchmark_site_correlations.csv"),
  input_sources = c(file.path(GSL_ROOT, "tables", "derived_site_year.csv"),
                     file.path(GSL_ROOT, "data", "daily_extract.rds")),
  notes = note_text
)
write_meta(
  file.path(GSL_ROOT, "tables", "benchmark_group_medians.csv"),
  input_sources = file.path(GSL_ROOT, "tables", "benchmark_site_correlations.csv"),
  notes = note_text
)

message("--- 09_benchmark.R: done ---")
message("Overall median correlation: Z17 = ", round(overall_median$median_cor_z17, 4),
        ", T95 (new method) = ", round(overall_median$median_cor_t95, 4))
print(group_medians)
