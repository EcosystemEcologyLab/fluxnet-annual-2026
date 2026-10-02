## koppen_pi_vs_era5.R
##
## Diagnostic: compares the PI-reported Köppen-Geiger class (BADM
## CLIMATE_KOEPPEN) against the classes actually used by Figure 4
## (scripts/figure4_representativeness.R) -- the ERA5-derived Geo vs Data
## class (site_koppen_era5_fig4.csv) and the Beck 2023 Geo vs Geo class
## (site_koppen_beck2023.csv) -- for the current 781-site network.
##
## Read-only: does not modify any production script, snapshot, or figure.
## Output: review/diagnostics/koppen_pi_vs_era5/ (tables + .meta.json only,
## no figure).
##
## PI source: data/processed/badm.rds, VARIABLE == "CLIMATE_KOEPPEN" -- the
## same processed-BIF-metadata object R/climate_classification.R's
## compute_site_koppen_era5() itself reads (via step5_compute_koppen_era5.R)
## to populate badm_kg_class, re-read fresh here rather than trusted from
## that column, per instruction. Confirms: (a) badm_kg_class in the
## production site_koppen_era5.csv (badm passed in) matches this fresh read,
## (b) badm_kg_class in site_koppen_era5_fig4.csv is all-NA, because
## figure4_representativeness.R's Phase 1 call
## (`compute_site_koppen_era5(monthly_era5, map_max_mm = Inf, legend = NULL)`)
## never passes a `badm` argument.
##
## Case handling: raw BADM CLIMATE_KOEPPEN values are inconsistently cased
## (e.g. "Bsk" and "BSk" both occur). Normalised here by case-insensitive
## matching against the 30 canonical Beck/Köppen class codes (the same set
## `methods_koppen_beck2023.md` documents), not by a fixed-position
## upper/lower rule -- the real convention differs by class (compare "BSk"
## vs "Cfa" vs "ET"), so a positional rule would mis-canonicalise some
## classes. A value that does not match any canonical code (after
## uppercasing) is flagged invalid, not silently dropped or guessed.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(fs)
  library(tibble)
})

OUTD <- "review/diagnostics/koppen_pi_vs_era5"
fs::dir_create(OUTD)

weighted_jaccard <- function(p, q) sum(pmin(p, q)) / sum(pmax(p, q))

msg <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ..., "\n", sep = "")

# ==============================================================================
# Inputs
# ==============================================================================
CURRENT_SNAPSHOT <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
current_sites <- readr::read_csv(CURRENT_SNAPSHOT, show_col_types = FALSE) |>
  dplyr::distinct(site_id, data_hub, location_lat, location_long)
if (nrow(current_sites) != 781L) {
  stop("Expected 781 current-network sites, got ", nrow(current_sites))
}
msg("Current network: ", nrow(current_sites), " sites")

## ---- PI-reported class: fresh BADM CLIMATE_KOEPPEN read -------------------
badm_path <- "data/processed/badm.rds"
if (!file.exists(badm_path)) stop("badm.rds not found at ", badm_path)
badm <- readRDS(badm_path)

climate_koeppen_rows <- badm |>
  dplyr::filter(.data$VARIABLE == "CLIMATE_KOEPPEN", !is.na(.data$DATAVALUE))

## Check for sites with more than one CLIMATE_KOEPPEN value recorded.
dupe_sites <- climate_koeppen_rows |>
  dplyr::group_by(site_id = .data$SITE_ID) |>
  dplyr::summarise(n_rows = dplyr::n(), n_distinct_values = dplyr::n_distinct(.data$DATAVALUE),
                    values = paste(sort(unique(.data$DATAVALUE)), collapse = "; "), .groups = "drop") |>
  dplyr::filter(.data$n_rows > 1L)
if (nrow(dupe_sites) > 0L) {
  msg("NOTE: ", nrow(dupe_sites), " site(s) have >1 CLIMATE_KOEPPEN BIF row -- first value kept, ",
      "all listed in table_0_duplicate_pi_values.csv: ", paste(dupe_sites$site_id, collapse = ", "))
  readr::write_csv(dupe_sites, file.path(OUTD, "table_0_duplicate_pi_values.csv"))
  write_output_metadata(
    file.path(OUTD, "table_0_duplicate_pi_values.csv"),
    input_sources = c(badm_path),
    notes = "Sites with more than one CLIMATE_KOEPPEN BIF row. First row kept for all other tables."
  )
} else {
  msg("No site has more than one CLIMATE_KOEPPEN BIF row.")
}

pi_raw <- climate_koeppen_rows |>
  dplyr::distinct(site_id = .data$SITE_ID, .keep_all = TRUE) |>
  dplyr::transmute(site_id, pi_raw = .data$DATAVALUE)

## Canonical Köppen 30-class codes (same set methods_koppen_beck2023.md
## documents). Case-insensitive lookup: uppercased raw value -> canonical form.
VALID_KG30 <- c(
  "Af", "Am", "Aw",
  "BWh", "BWk", "BSh", "BSk",
  "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc",
  "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd",
  "ET", "EF"
)
canon_lookup <- setNames(VALID_KG30, toupper(VALID_KG30))

pi <- current_sites |>
  dplyr::left_join(pi_raw, by = "site_id") |>
  dplyr::mutate(
    pi_canonical = unname(canon_lookup[toupper(.data$pi_raw)]),
    pi_invalid   = !is.na(.data$pi_raw) & is.na(.data$pi_canonical),
    pi_twoletter = substr(.data$pi_canonical, 1, 2),
    pi_main      = substr(.data$pi_canonical, 1, 1)
  )

invalid_pi_values <- pi |> dplyr::filter(.data$pi_invalid) |>
  dplyr::distinct(.data$pi_raw) |> dplyr::arrange(.data$pi_raw)
if (nrow(invalid_pi_values) > 0L) {
  msg("Invalid PI CLIMATE_KOEPPEN values (not a recognised Köppen class, after case-insensitive ",
      "matching against the 30-class set): ", paste(invalid_pi_values$pi_raw, collapse = ", "))
} else {
  msg("Every non-NA PI CLIMATE_KOEPPEN value matches a recognised Köppen class (case-insensitive).")
}

msg("PI class coverage: ", sum(!is.na(pi$pi_raw)), " / ", nrow(pi), " sites have a raw value; ",
    sum(!is.na(pi$pi_canonical)), " / ", nrow(pi), " have a valid (canonicalisable) class.")

## ---- Cross-check vs. the badm_kg_class column already in production files -
prod_era5 <- readr::read_csv("data/snapshots/site_koppen_era5.csv", show_col_types = FALSE) |>
  dplyr::select(site_id, badm_kg_class_prod = badm_kg_class)
fig4_era5 <- readr::read_csv("data/snapshots/site_koppen_era5_fig4.csv", show_col_types = FALSE) |>
  dplyr::select(site_id, era5_class = koppen_class, era5_twoletter = koppen_twoletter,
                era5_main = koppen_main, excluded_fig4_geo_vs_data, badm_kg_class_fig4 = badm_kg_class)

n_fig4_badm_na <- sum(is.na(fig4_era5$badm_kg_class_fig4))
msg("site_koppen_era5_fig4.csv badm_kg_class: ", n_fig4_badm_na, " / ", nrow(fig4_era5),
    " NA (expected 781/781 -- Phase 1's compute_site_koppen_era5() call passes no badm argument).")
if (n_fig4_badm_na != nrow(fig4_era5)) {
  warning("Expected ALL badm_kg_class values in site_koppen_era5_fig4.csv to be NA; found ",
          nrow(fig4_era5) - n_fig4_badm_na, " non-NA. Investigate before trusting this note.")
}

cross_check <- pi |>
  dplyr::left_join(prod_era5, by = "site_id") |>
  dplyr::mutate(
    matches_prod_badm = dplyr::case_when(
      is.na(.data$pi_raw) & is.na(.data$badm_kg_class_prod) ~ TRUE,
      is.na(.data$pi_raw) | is.na(.data$badm_kg_class_prod) ~ FALSE,
      TRUE ~ toupper(.data$pi_raw) == toupper(.data$badm_kg_class_prod)
    )
  )
n_cross_mismatch <- sum(!cross_check$matches_prod_badm)
msg("Fresh BADM CLIMATE_KOEPPEN read vs. site_koppen_era5.csv's badm_kg_class column: ",
    sum(cross_check$matches_prod_badm), " / ", nrow(cross_check), " sites match (case-insensitive); ",
    n_cross_mismatch, " mismatch.")
if (n_cross_mismatch > 0L) {
  mismatch_tab <- cross_check |> dplyr::filter(!.data$matches_prod_badm) |>
    dplyr::select(site_id, pi_raw, badm_kg_class_prod)
  readr::write_csv(mismatch_tab, file.path(OUTD, "table_0b_badm_cross_check_mismatches.csv"))
  write_output_metadata(
    file.path(OUTD, "table_0b_badm_cross_check_mismatches.csv"),
    input_sources = c(badm_path, "data/snapshots/site_koppen_era5.csv"),
    notes = "Sites where a fresh BADM CLIMATE_KOEPPEN read disagrees with site_koppen_era5.csv's badm_kg_class column."
  )
}

## ---- Beck 2023 Geo vs Geo class --------------------------------------------
beck <- readr::read_csv("data/snapshots/site_koppen_beck2023.csv", show_col_types = FALSE) |>
  dplyr::select(site_id, beck_class = koppen_class, beck_twoletter = koppen_twoletter,
                beck_main = koppen_main)

## ---- Assemble the full per-site comparison table ---------------------------
full <- pi |>
  dplyr::left_join(fig4_era5, by = "site_id") |>
  dplyr::left_join(beck, by = "site_id") |>
  dplyr::mutate(
    agree_pi_era5  = !is.na(.data$pi_canonical) & !is.na(.data$era5_class)  & toupper(.data$pi_canonical) == toupper(.data$era5_class),
    agree_pi_beck  = !is.na(.data$pi_canonical) & !is.na(.data$beck_class)  & toupper(.data$pi_canonical) == toupper(.data$beck_class),
    agree_era5_beck = !is.na(.data$era5_class)  & !is.na(.data$beck_class) & toupper(.data$era5_class)  == toupper(.data$beck_class)
  )

# ==============================================================================
# Report 1: coverage
# ==============================================================================
msg("\n=== Report 1: PI class coverage ===")

coverage_by_hub <- full |>
  dplyr::group_by(data_hub) |>
  dplyr::summarise(
    n_sites = dplyr::n(),
    n_with_pi_raw = sum(!is.na(.data$pi_raw)),
    n_with_valid_pi = sum(!is.na(.data$pi_canonical)),
    pct_with_valid_pi = round(100 * n_with_valid_pi / n_sites, 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(n_sites))
coverage_all <- tibble::tibble(
  data_hub = "ALL", n_sites = nrow(full), n_with_pi_raw = sum(!is.na(full$pi_raw)),
  n_with_valid_pi = sum(!is.na(full$pi_canonical)),
  pct_with_valid_pi = round(100 * sum(!is.na(full$pi_canonical)) / nrow(full), 1)
)
coverage_table <- dplyr::bind_rows(coverage_by_hub, coverage_all)
print(as.data.frame(coverage_table))
readr::write_csv(coverage_table, file.path(OUTD, "table_1_coverage_by_hub.csv"))
write_output_metadata(
  file.path(OUTD, "table_1_coverage_by_hub.csv"),
  input_sources = c(badm_path, CURRENT_SNAPSHOT),
  notes = paste0(
    "PI-reported (BADM CLIMATE_KOEPPEN) class coverage by data_hub (AmeriFlux/ICOS/TERN), for the ",
    "current 781-site network. 'data_hub' used rather than the multi-valued 'network' acknowledgment ",
    "column, per CLAUDE.md Hard Rule 2 (use the manifest's own field, not an ID-prefix inference); ",
    "data_hub is a clean single-value field already in the snapshot. n_with_valid_pi counts only ",
    "raw values that canonicalise to one of the 30 standard Koppen classes (case-insensitive)."
  )
)

n_panel_a_excluded <- sum(full$excluded_fig4_geo_vs_data, na.rm = TRUE)
excluded_with_pi <- full |> dplyr::filter(.data$excluded_fig4_geo_vs_data) |>
  dplyr::summarise(n_excluded = dplyr::n(), n_with_valid_pi = sum(!is.na(.data$pi_canonical)),
                    pct_with_valid_pi = round(100 * n_with_valid_pi / n_excluded, 1))
msg("Of the ", n_panel_a_excluded, " sites excluded from panel a's Geo vs Data side, ",
    excluded_with_pi$n_with_valid_pi, " (", excluded_with_pi$pct_with_valid_pi,
    "%) have a valid PI-reported class.")
readr::write_csv(excluded_with_pi, file.path(OUTD, "table_1b_coverage_among_panel_a_exclusions.csv"))
write_output_metadata(
  file.path(OUTD, "table_1b_coverage_among_panel_a_exclusions.csv"),
  input_sources = c(badm_path, "data/snapshots/site_koppen_era5_fig4.csv"),
  notes = "PI-class coverage among the sites excluded from Figure 4 panel a's Geo vs Data side (excluded_fig4_geo_vs_data == TRUE)."
)

# ==============================================================================
# Report 2: agreement at three class levels, two populations
# ==============================================================================
msg("\n=== Report 2: agreement (PI vs ERA5-derived, PI vs Beck) ===")

agreement_summary <- function(df, population_label) {
  both_era5 <- df |> dplyr::filter(!is.na(.data$pi_canonical), !is.na(.data$era5_class))
  both_beck <- df |> dplyr::filter(!is.na(.data$pi_canonical), !is.na(.data$beck_class))
  one_row <- function(d, comparison, other_class_col, other_two_col, other_main_col) {
    tibble::tibble(
      population = population_label, comparison = comparison, n = nrow(d),
      pct_agree_full_class = round(100 * mean(toupper(d$pi_canonical) == toupper(d[[other_class_col]])), 1),
      pct_agree_two_letter = round(100 * mean(toupper(d$pi_twoletter) == toupper(d[[other_two_col]])), 1),
      pct_agree_main_class = round(100 * mean(toupper(d$pi_main) == toupper(d[[other_main_col]])), 1)
    )
  }
  dplyr::bind_rows(
    one_row(both_era5, "PI vs ERA5-derived", "era5_class", "era5_twoletter", "era5_main"),
    one_row(both_beck, "PI vs Beck 2023", "beck_class", "beck_twoletter", "beck_main")
  )
}

agreement_all <- agreement_summary(full, "all sites with both classes")
panel_a_pool <- full |> dplyr::filter(!.data$excluded_fig4_geo_vs_data %in% TRUE)
agreement_panel_a <- agreement_summary(panel_a_pool, "panel a eligible pool (n=599)")
agreement_table <- dplyr::bind_rows(agreement_all, agreement_panel_a)
print(as.data.frame(agreement_table))
readr::write_csv(agreement_table, file.path(OUTD, "table_2_agreement.csv"))
write_output_metadata(
  file.path(OUTD, "table_2_agreement.csv"),
  input_sources = c(badm_path, "data/snapshots/site_koppen_era5_fig4.csv", "data/snapshots/site_koppen_beck2023.csv"),
  notes = paste0(
    "Agreement between the PI-reported class and (a) the ERA5-derived class actually used by Figure 4 ",
    "panel a's Geo vs Data side, (b) the Beck 2023 class used by panel a's Geo vs Geo side. Full-class = ",
    "the 3-letter code (e.g. BSk); two-letter = the 13-class aggregation Figure 4 itself uses; main-class ",
    "= the 5-class A/B/C/D/E level. 'panel a eligible pool' is the 599 sites not excluded from Figure 4 ",
    "panel a's Geo vs Data side (excluded_fig4_geo_vs_data == FALSE)."
  )
)

# ==============================================================================
# Report 3: 13-class confusion matrix + top disagreements
# ==============================================================================
msg("\n=== Report 3: 13-class confusion matrix (PI vs ERA5-derived) ===")

TL_ORDER <- c("Af", "Am", "Aw", "BS", "BW", "Cf", "Cs", "Cw", "Df", "Ds", "Dw", "EF", "ET")
confusion_pool <- full |> dplyr::filter(!is.na(.data$pi_twoletter), !is.na(.data$era5_twoletter))
confusion <- confusion_pool |>
  dplyr::mutate(pi_twoletter = factor(.data$pi_twoletter, levels = TL_ORDER),
                era5_twoletter = factor(.data$era5_twoletter, levels = TL_ORDER)) |>
  dplyr::count(pi_twoletter, era5_twoletter, name = "n", .drop = FALSE) |>
  tidyr::pivot_wider(names_from = era5_twoletter, values_from = n, values_fill = 0L)
print(as.data.frame(confusion))
readr::write_csv(confusion, file.path(OUTD, "table_3_confusion_13class.csv"))
write_output_metadata(
  file.path(OUTD, "table_3_confusion_13class.csv"),
  input_sources = c(badm_path, "data/snapshots/site_koppen_era5_fig4.csv"),
  notes = paste0(
    "13-class (two-letter) confusion matrix, PI-reported class (rows) against the ERA5-derived class ",
    "Figure 4 panel a's Geo vs Data side uses (columns). Population: ", nrow(confusion_pool),
    " sites with both a valid PI class and a classified ERA5-derived class (not restricted to the ",
    "599-site panel a eligible pool)."
  )
)

top_disagreements <- confusion_pool |>
  dplyr::filter(toupper(.data$pi_twoletter) != toupper(.data$era5_twoletter)) |>
  dplyr::count(pi_twoletter, era5_twoletter, name = "n", sort = TRUE) |>
  head(10)
msg("Ten most common PI-vs-ERA5 disagreements (13-class level):")
print(as.data.frame(top_disagreements))
readr::write_csv(top_disagreements, file.path(OUTD, "table_3b_top10_disagreements.csv"))
write_output_metadata(
  file.path(OUTD, "table_3b_top10_disagreements.csv"),
  input_sources = c(badm_path, "data/snapshots/site_koppen_era5_fig4.csv"),
  notes = "Ten most common PI-vs-ERA5-derived disagreements at the 13-class (two-letter) level, same population as table_3."
)

# ==============================================================================
# Report 4: weighted Jaccard if PI class were used for every site that has one
# ==============================================================================
msg("\n=== Report 4: weighted Jaccard using PI class (vs current ERA5-derived J=0.411, n=599) ===")

kg_global <- readr::read_csv("data/snapshots/koppen_beck2023_global_distribution.csv", show_col_types = FALSE) |>
  dplyr::group_by(koppen_twoletter) |>
  dplyr::summarise(global_land_fraction = sum(global_land_fraction), .groups = "drop") |>
  dplyr::rename(class = koppen_twoletter)

pi_classified <- full |> dplyr::filter(!is.na(.data$pi_twoletter))
n_pi <- nrow(pi_classified)
cnt_pi <- pi_classified |>
  dplyr::count(pi_twoletter, name = "n") |>
  dplyr::rename(class = pi_twoletter) |>
  dplyr::mutate(network_frac = n / n_pi)
merged_pi <- kg_global |>
  dplyr::full_join(cnt_pi, by = "class") |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0),
                global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
j_pi <- weighted_jaccard(merged_pi$global_land_fraction, merged_pi$network_frac)

j_comparison <- tibble::tibble(
  source = c("ERA5-derived (current Figure 4 panel a, Geo vs Data)", "PI-reported (every site with a valid class)"),
  n = c(599L, n_pi),
  weighted_jaccard = c(0.411, round(j_pi, 3))
)
msg("Current (ERA5-derived, panel a Geo vs Data): J = 0.411, n = 599")
msg("If PI class used for every site that has one: J = ", round(j_pi, 3), ", n = ", n_pi)
print(as.data.frame(j_comparison))
readr::write_csv(j_comparison, file.path(OUTD, "table_4_jaccard_comparison.csv"))
write_output_metadata(
  file.path(OUTD, "table_4_jaccard_comparison.csv"),
  input_sources = c(badm_path, "data/snapshots/koppen_beck2023_global_distribution.csv",
                     "data/snapshots/representativeness_metrics_fig4.csv"),
  notes = paste0(
    "Weighted Jaccard (13-class) using the PI-reported class for every one of the ", n_pi,
    " sites with a valid (canonicalisable) CLIMATE_KOEPPEN value -- NOT restricted to the 599-site ",
    "panel a Geo vs Data eligible pool (i.e. this population is not comparable 1:1 with the current ",
    "panel a n; it answers 'what if PI class were substituted network-wide', not 'what if PI class ",
    "were substituted only within the current exclusion rules'). Current panel a figure (0.411, n=599) ",
    "from representativeness_metrics_fig4.csv, reproduced here as a fixed reference value, not recomputed."
  )
)

# ==============================================================================
# Report 5: per-site table
# ==============================================================================
msg("\n=== Report 5: per-site table ===")

per_site <- full |>
  dplyr::transmute(
    site_id, network = data_hub,
    pi_class = pi_canonical, era5_class, beck_class,
    agree_pi_era5, agree_pi_beck, agree_era5_beck,
    excluded_fig4_geo_vs_data = dplyr::coalesce(excluded_fig4_geo_vs_data, FALSE)
  ) |>
  dplyr::arrange(site_id)
readr::write_csv(per_site, file.path(OUTD, "table_5_per_site.csv"))
write_output_metadata(
  file.path(OUTD, "table_5_per_site.csv"),
  input_sources = c(badm_path, "data/snapshots/site_koppen_era5_fig4.csv", "data/snapshots/site_koppen_beck2023.csv", CURRENT_SNAPSHOT),
  notes = paste0(
    "Per-site comparison for all 781 current-network sites. 'network' = data_hub (AmeriFlux/ICOS/TERN). ",
    "pi_class is the canonicalised (case-normalised) PI-reported CLIMATE_KOEPPEN value; NA where no raw ",
    "value exists or it did not match a recognised Koppen class (see table_0 for invalid raw values, ",
    "printed to console). The three agree_* flags are full-class (3-letter) agreement, case-insensitive, ",
    "for each of the three pairwise comparisons among pi_class/era5_class/beck_class. ",
    "excluded_fig4_geo_vs_data mirrors Figure 4 panel a's own Geo vs Data exclusion flag (FALSE for sites ",
    "not evaluated by that rule, e.g. if era5_class itself is NA for an unrelated reason)."
  )
)

msg("\n=== koppen_pi_vs_era5.R: DONE. Outputs in ", OUTD, " ===")
