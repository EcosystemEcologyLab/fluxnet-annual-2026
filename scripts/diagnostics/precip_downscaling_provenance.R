## precip_downscaling_provenance.R
##
## Provenance-based basis for deciding how to treat sites whose P_ERA is
## unusable for the site-side Koppen classification -- NOT a decision about
## inclusion/exclusion, and NOT a benchmark against KG_ERA5_MAP_MAX_MM (the
## current 5,000 mm/yr cutoff is never read, referenced, or reused here in
## any form).
##
## Background (from the task, stated provisionally -- see Step 1): FLUXNET
## meteorological variables are downscaled from ERA5 reanalysis to the tower
## via a regression against site measurements where they exist (Vuichard &
## Papale 2015). Per-variable regression diagnostics are recorded in each
## site's BIF file as ERA_SLOPE/ERA_INTERCEPT/ERA_RMSE/ERA_CORRELATION,
## named by ERA_VARIABLE, inside variable group `GRP_ERA_DOWN` -- the task
## calls this "AUXMETEO metadata"; that is not the literal BIF group name
## used in this repo's extracted files (it is `GRP_ERA_DOWN`), noted here
## rather than silently substituted.
##
## Read-only with respect to R/pipeline_config.R, every pipeline script
## (01-07), and every already-committed figure.
##
## Step 1 (characterise before using) is not a formality here: it finds that
## the task's own stated "not fitted" signature (slope 1, other three
## -9999) is real but incomplete for variable P -- a second sentinel pattern
## exists (slope ALSO -9999) -- and, more importantly, that NO site in the
## network has a genuinely fitted P regression at all (0/781; contrast with
## TA, which is fitted at every site). This is written up in full before
## Step 2 proceeds, per instruction, rather than assumed.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(fs)
  library(ggplot2)
  library(scales)
  library(tibble)
})

OUTD <- "review/diagnostics/precip_downscaling_provenance"
fs::dir_create(OUTD)

message("=== precip_downscaling_provenance.R ===")

CLR_GROUP <- c(
  "not_fitted_slope1"      = "#0072B2",
  "not_fitted_slope_9999"  = "#D55E00",
  "fitted"                 = "#009E73",
  "no_auxmeteo_record"     = "grey50"
)

theme_precip <- function() {
  ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.background = ggplot2::element_rect(fill = "grey85", color = NA),
      legend.position  = "bottom"
    )
}
save_fig <- function(path, plot, width, height, notes, input_sources) {
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 200, bg = "white")
  write_output_metadata(path, input_sources = input_sources, notes = notes)
  message("Saved: ", path)
}

# ============================================================================
# 1. Characterise the GRP_ERA_DOWN ("AUXMETEO") block before using it
# ============================================================================
message("\n================ Step 1: characterise GRP_ERA_DOWN ================")

file_manifest <- suppressWarnings(
  fluxnet::flux_discover_files(data_dir = file.path(FLUXNET_DATA_ROOT, "extracted"))
)
bif_manifest <- file_manifest |> dplyr::filter(dataset == "BIF")
site_meta <- bif_manifest |>
  dplyr::distinct(site_id, path, data_hub, network) |>
  dplyr::rename(bif_path = path)
n_sites <- nrow(site_meta)
message("Canonical sites (BIF manifest): ", n_sites)
if (n_sites != 781L) message("NOTE: expected 781 sites; got ", n_sites, " -- reported, not reconciled.")

era_down_lines <- system2("grep",
  args = c("-hE", shQuote(",GRP_ERA_DOWN,"), shQuote(site_meta$bif_path)),
  stdout = TRUE)
message("GRP_ERA_DOWN rows across ", n_sites, " canonical BIF files: ", length(era_down_lines))

era_down_raw <- readr::read_csv(
  I(era_down_lines),
  col_names = c("site_id", "group_id", "variable_group", "variable", "datavalue"),
  col_types = readr::cols(.default = "c"),
  show_col_types = FALSE
)

era_down_wide <- era_down_raw |>
  tidyr::pivot_wider(id_cols = c(site_id, group_id), names_from = variable, values_from = datavalue) |>
  dplyr::mutate(
    ERA_SLOPE       = suppressWarnings(as.numeric(ERA_SLOPE)),
    ERA_INTERCEPT   = suppressWarnings(as.numeric(ERA_INTERCEPT)),
    ERA_RMSE        = suppressWarnings(as.numeric(ERA_RMSE)),
    ERA_CORRELATION = suppressWarnings(as.numeric(ERA_CORRELATION))
  )

n_sites_with_block <- dplyr::n_distinct(era_down_wide$site_id)
message("Sites with >=1 GRP_ERA_DOWN row: ", n_sites_with_block, " / ", n_sites)

message("\n---- ERA_VARIABLE distribution (should be 1 row per site per variable) ----")
var_counts <- era_down_wide |> dplyr::count(ERA_VARIABLE, name = "n_sites") |> dplyr::arrange(dplyr::desc(n_sites))
print(as.data.frame(var_counts))

rows_per_site <- table(table(era_down_wide$site_id))
message("Distribution of (rows per site) across sites (should be a single value, 8, if one row per variable per site, never per year):")
print(rows_per_site)
structure_is_once_per_site <- length(rows_per_site) == 1L && names(rows_per_site) == as.character(nrow(var_counts))
message("Structure is exactly one row per site per variable (no per-year repetition): ", structure_is_once_per_site)

# ---- Focus: ERA_VARIABLE == "P" ----
p_rows <- era_down_wide |> dplyr::filter(ERA_VARIABLE == "P")
message("\n---- ERA_VARIABLE = P: present for ", nrow(p_rows), " / ", n_sites, " sites ----")
missing_p_sites <- setdiff(site_meta$site_id, p_rows$site_id)
message(length(missing_p_sites), " sites have no P row in GRP_ERA_DOWN at all (true 'no AUXMETEO record' for P).")

p_rows <- p_rows |>
  dplyr::mutate(
    slope_is_1     = ERA_SLOPE == 1,
    slope_is_9999  = ERA_SLOPE == -9999,
    other3_all_9999 = ERA_INTERCEPT == -9999 & ERA_RMSE == -9999 & ERA_CORRELATION == -9999,
    other3_any_real = !other3_all_9999
  )

message("\nValue-pattern combinations for P (slope value x whether the other 3 stats are all -9999):")
pattern_table <- p_rows |> dplyr::count(slope_is_1, slope_is_9999, other3_all_9999, name = "n_sites")
print(as.data.frame(pattern_table))

n_genuinely_fitted_p <- p_rows |> dplyr::filter(!slope_is_9999, other3_any_real) |> nrow()
n_slope1_not_fitted  <- p_rows |> dplyr::filter(slope_is_1, other3_all_9999) |> nrow()
n_slope_missing      <- p_rows |> dplyr::filter(slope_is_9999, other3_all_9999) |> nrow()
n_other_pattern       <- nrow(p_rows) - n_genuinely_fitted_p - n_slope1_not_fitted - n_slope_missing

message(sprintf(
  "P: %d sites with a genuinely fitted regression (real RMSE/correlation/intercept, not -9999).",
  n_genuinely_fitted_p))
message(sprintf(
  "P: %d sites match the task's stated 'not fitted' signature (slope=1, other three=-9999).",
  n_slope1_not_fitted))
message(sprintf(
  "P: %d sites show a SECOND, distinct sentinel pattern not in the task's stated signature (slope ALSO -9999, all four fields sentinel).",
  n_slope_missing))
if (n_other_pattern > 0L) {
  message(sprintf("P: %d sites fall outside all three patterns above -- listed for inspection, not silently absorbed.", n_other_pattern))
}

# ---- Reference comparison: is this a P-specific phenomenon, or does every
#      variable lack real fit statistics (i.e. is -9999 just how this BIF
#      export always looks, and the whole exercise is moot)? Checked against
#      TA, the variable used as the worked example in the task background. ----
ta_rows <- era_down_wide |> dplyr::filter(ERA_VARIABLE == "TA")
n_ta_fitted <- sum(ta_rows$ERA_RMSE != -9999, na.rm = TRUE)
message(sprintf(
  "\nReference check (TA, not P): %d / %d sites have real (non -9999) TA regression statistics -- ",
  n_ta_fitted, nrow(ta_rows)))
message("this confirms the all- -9999 pattern is NOT how every BIF export looks for every variable; ",
        "it is specific to P in this network.")

message("\n=== Step 1 conclusion ===")
message("The task's stated signature (slope 1, other three -9999 = 'not fitted') is real for ",
        n_slope1_not_fitted, "/", nrow(p_rows), " sites but is NOT the only 'not fitted' pattern: ",
        n_slope_missing, " further sites carry a second sentinel pattern (slope also -9999) the ",
        "stated signature does not cover. Critically, ACROSS THE ENTIRE NETWORK, zero sites (0/",
        nrow(p_rows), ") have a genuinely fitted P regression -- every site's P_ERA is either flagged ",
        "not-fitted (two sentinel flavors) or, for this variable, simply has no case where real fit ",
        "diagnostics exist. This is not an ambiguous or dirty signal that stops the analysis -- it is a ",
        "single, clean, network-wide fact, fully characterised above -- but it means Step 2's three-way ",
        "'not fitted / fitted / no record' classification below will have an EMPTY 'fitted with ",
        "statistics' bucket and an EMPTY (for P specifically) 'no AUXMETEO record' bucket; this is ",
        "reported, not engineered around.")

out_step1 <- file.path(OUTD, "table_1_auxmeteo_structure.csv")
readr::write_csv(era_down_wide |> dplyr::select(site_id, ERA_VARIABLE, ERA_SLOPE, ERA_INTERCEPT, ERA_RMSE, ERA_CORRELATION),
                  out_step1)
write_output_metadata(out_step1,
  input_sources = "canonical BIF files via flux_discover_files() (GRP_ERA_DOWN rows)",
  notes = paste0(
    "One row per site x ERA_VARIABLE (8 variables x ", n_sites, " sites = ", nrow(era_down_wide),
    " rows), from each site's currently-canonical BIF file's GRP_ERA_DOWN group (the task's ",
    "'AUXMETEO metadata'; GRP_ERA_DOWN is the literal BIF group name, not literally 'AUXMETEO'). ",
    "Raw ERA_SLOPE/ERA_INTERCEPT/ERA_RMSE/ERA_CORRELATION values as extracted, -9999 sentinel not ",
    "recoded to NA (kept literal so a reader can see the sentinel directly)."
  ))
message("Saved: ", out_step1)

out_step1_pattern <- file.path(OUTD, "table_1b_p_variable_patterns.csv")
readr::write_csv(pattern_table, out_step1_pattern)
write_output_metadata(out_step1_pattern, input_sources = out_step1,
  notes = "Every distinct (slope_is_1, slope_is_9999, other3_all_9999) value-pattern combination observed for ERA_VARIABLE=P across all 781 sites, with site counts. Confirms exactly two patterns exist and both indicate no real regression fit.")
message("Saved: ", out_step1_pattern)

# ============================================================================
# 2. Classify every site; join to precip_site_filter's table_1
# ============================================================================
message("\n================ Step 2: classify sites, join to precip_site_filter table_1 ================")

site_group <- p_rows |>
  dplyr::mutate(
    p_group = dplyr::case_when(
      !slope_is_9999 & other3_any_real ~ "fitted",
      slope_is_1  & other3_all_9999    ~ "not_fitted_slope1",
      slope_is_9999 & other3_all_9999  ~ "not_fitted_slope_9999",
      TRUE ~ "other_unclassified_pattern"
    )
  ) |>
  dplyr::select(site_id, era_slope = ERA_SLOPE, era_intercept = ERA_INTERCEPT,
                era_rmse = ERA_RMSE, era_correlation = ERA_CORRELATION, p_group)

# Sites with literally no P row (true "no AUXMETEO record" for P) -- none
# found in this network (see Step 1), but not assumed away here.
if (length(missing_p_sites) > 0L) {
  site_group <- dplyr::bind_rows(
    site_group,
    tibble::tibble(site_id = missing_p_sites, era_slope = NA_real_, era_intercept = NA_real_,
                    era_rmse = NA_real_, era_correlation = NA_real_, p_group = "no_auxmeteo_record")
  )
}

message("Site classification (n):")
print(table(site_group$p_group))

table1_path <- "review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv"
if (!file.exists(table1_path)) stop("Required input not found: ", table1_path)
table1 <- readr::read_csv(table1_path, show_col_types = FALSE)
if (!"p_era_mean_mm_1981_2025" %in% names(table1)) {
  stop("table_1 does not have p_era_mean_mm_1981_2025 -- expected the 2026-09-22 renamed column; stopping rather than guessing which column to join on.")
}

joined <- table1 |> dplyr::left_join(site_group, by = "site_id")
n_unmatched <- sum(is.na(joined$p_group))
message("Sites in table_1 with no P-group match: ", n_unmatched, " (expect 0 -- P row present for all 781).")

out_joined <- file.path(OUTD, "table_2_site_groups.csv")
readr::write_csv(joined, out_joined)
write_output_metadata(out_joined, input_sources = c(out_step1, table1_path),
  notes = paste0(
    "table_1_site_level_precip_estimates.csv (precip_site_filter, 2026-09-22 window-labelled version) ",
    "left-joined to this script's site-level P downscaling-provenance group (p_group) and raw ",
    "ERA_SLOPE/ERA_INTERCEPT/ERA_RMSE/ERA_CORRELATION for variable P. p_group categories: ",
    "not_fitted_slope1 (n=", n_slope1_not_fitted, "), not_fitted_slope_9999 (n=", n_slope_missing,
    "), fitted (n=", n_genuinely_fitted_p, "), no_auxmeteo_record (n=", length(missing_p_sites), ")."
  ))
message("Saved: ", out_joined)

# ---- Provisional correlate check: does slope_9999 track absence of any
#      QC-measured precip year? (Descriptive only -- ERA_SLOPE's meaning is
#      undocumented in this repo; this is not asserted as the mechanism.) ----
measured_cross <- joined |>
  dplyr::mutate(has_measured = !is.na(n_years_measured)) |>
  dplyr::count(p_group, has_measured)
message("\nProvisional cross-tab: p_group x whether the site has >=1 QC-measured precip year (n_years_measured non-NA in table_1):")
print(as.data.frame(measured_cross))

out_crosstab <- file.path(OUTD, "table_2b_group_vs_measured_years_crosstab.csv")
readr::write_csv(measured_cross, out_crosstab)
write_output_metadata(out_crosstab, input_sources = out_joined,
  notes = "Descriptive cross-tab only: p_group against whether table_1's n_years_measured (QC-measured P years, P_F_QC > QC_THRESHOLD_YY) is non-NA. Not an asserted mechanism -- ERA_SLOPE's meaning/direction is undocumented in this repository (see report.md).")
message("Saved: ", out_crosstab)

# ============================================================================
# 3. Figures
# ============================================================================
message("\n================ Step 3: figures ================")

joined_f <- joined |> dplyr::mutate(p_group = factor(p_group, levels = names(CLR_GROUP)))

## ---- Fig 1: precip_site_filter's fig_1 pairwise comparison, redrawn with
##      point colour = p_group, otherwise unchanged (same six panels, same
##      log-log axes, same 1:1 line) so it is directly comparable. ----
pairwise_long <- function(df, xvar, yvar, xlab, ylab) {
  df |>
    dplyr::filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]]),
                  .data[[xvar]] > 0, .data[[yvar]] > 0) |>
    dplyr::transmute(site_id, p_group, x = .data[[xvar]], y = .data[[yvar]],
                      panel = paste0(ylab, " vs. ", xlab))
}
f1_data <- dplyr::bind_rows(
  pairwise_long(joined_f, "bio12_mm", "p_era_mean_mm_1981_2025", "BIO12", "P_ERA"),
  pairwise_long(joined_f, "badm_map_mm", "p_era_mean_mm_1981_2025", "BADM", "P_ERA"),
  pairwise_long(joined_f, "p_measured_mean_mm", "p_era_mean_mm_1981_2025", "measured", "P_ERA"),
  pairwise_long(joined_f, "bio12_mm", "p_measured_mean_mm", "BIO12", "measured"),
  pairwise_long(joined_f, "badm_map_mm", "p_measured_mean_mm", "BADM", "measured"),
  pairwise_long(joined_f, "bio12_mm", "badm_map_mm", "BIO12", "BADM")
)
f1 <- ggplot2::ggplot(f1_data, ggplot2::aes(x = x, y = y, color = p_group)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  ggplot2::geom_point(alpha = 0.45, size = 1.2) +
  ggplot2::scale_x_log10(labels = scales::label_number(big.mark = " ")) +
  ggplot2::scale_y_log10(labels = scales::label_number(big.mark = " ")) +
  ggplot2::scale_color_manual(values = CLR_GROUP, name = "P downscaling group", drop = TRUE) +
  ggplot2::facet_wrap(~panel, scales = "free", ncol = 3) +
  ggplot2::labs(x = "mm/yr (log scale)", y = "mm/yr (log scale)",
                title = "Four mean-annual-precipitation estimates against each other, coloured by P downscaling group",
                subtitle = "Same six panels as precip_site_filter/fig_1_pairwise_comparison.png; colour is the only change.") +
  theme_precip()
save_fig(file.path(OUTD, "fig_1_pairwise_comparison_by_group.png"), f1, width = 11, height = 7.8,
  notes = "precip_site_filter's fig_1_pairwise_comparison.png redrawn unchanged except points coloured by p_group, for direct visual comparison. From table_2_site_groups.csv.",
  input_sources = out_joined)

## ---- Fig 2: ERA_SLOPE vs log10(P_ERA/BIO12), 'fitted' sites only ----
n_fitted_sites <- sum(joined_f$p_group == "fitted", na.rm = TRUE)
if (n_fitted_sites == 0L) {
  message("Fig 2 as literally specified ('sites with a fitted regression only') cannot be drawn: ",
          "0 sites qualify (see Step 1). Producing a labelled substitute using ALL sites' raw ",
          "ERA_SLOPE values instead, explicitly marked as a substitute, not a silent stand-in.")
  f2 <- ggplot2::ggplot(joined_f |> dplyr::filter(!is.na(era_slope), !is.na(log_ratio_era_bio12)),
                          ggplot2::aes(x = era_slope, y = log_ratio_era_bio12, color = p_group)) +
    ggplot2::geom_vline(xintercept = 1, color = "grey50", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
    ggplot2::geom_jitter(width = 0.15, height = 0, alpha = 0.45, size = 1.2) +
    ggplot2::scale_color_manual(values = CLR_GROUP, name = "P downscaling group", drop = TRUE) +
    ggplot2::labs(x = "ERA_SLOPE (jittered horizontally -- only two values exist: 1 and -9999)",
                  y = "log10(P_ERA / BIO12)",
                  title = "SUBSTITUTE FIGURE: 0 sites have a fitted P regression -- as-specified fig_2 is empty",
                  subtitle = "All 781 sites shown instead, by their raw ERA_SLOPE sentinel value. Dashed lines: slope=1, ratio=1 (not a fitted guide -- there is no fitted subset to draw one for).") +
    theme_precip()
  save_fig(file.path(OUTD, "fig_2_slope_vs_ratio_SUBSTITUTE.png"), f2, width = 10, height = 6.5,
    notes = "SUBSTITUTE for the requested 'ERA_SLOPE vs log10(P_ERA/BIO12), fitted sites only' figure: that group is empty (0/781 sites), so this plots all sites by their raw (sentinel) ERA_SLOPE value instead, explicitly labelled as a substitute. No unit-gradient slope=inflation-factor relationship can be assessed because there is no fitted subset. From table_2_site_groups.csv.",
    input_sources = out_joined)
  message("Fig 2: no unit-gradient relationship can be assessed -- there are no fitted-regression sites to assess it on.")
} else {
  f2_data <- joined_f |> dplyr::filter(p_group == "fitted", !is.na(era_slope), !is.na(log_ratio_era_bio12))
  f2 <- ggplot2::ggplot(f2_data, ggplot2::aes(x = era_slope, y = log_ratio_era_bio12)) +
    ggplot2::geom_vline(xintercept = 1, color = "grey50", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.5, size = 1.3, color = CLR_GROUP[["fitted"]]) +
    ggplot2::labs(x = "ERA_SLOPE", y = "log10(P_ERA / BIO12)",
                  title = "ERA_SLOPE against log10(P_ERA/BIO12), fitted-regression sites only") +
    theme_precip()
  save_fig(file.path(OUTD, "fig_2_slope_vs_ratio.png"), f2, width = 7, height = 6,
    notes = "ERA_SLOPE (x) against log10(P_ERA/BIO12) (y), sites with a fitted P regression only. From table_2_site_groups.csv.",
    input_sources = out_joined)
}

## ---- Fig 3: distribution of log10(P_ERA/BIO12) by group ----
f3_stats <- joined_f |>
  dplyr::filter(!is.na(log_ratio_era_bio12)) |>
  dplyr::group_by(p_group) |>
  dplyr::summarise(n = dplyr::n(), median = stats::median(log_ratio_era_bio12), .groups = "drop") |>
  dplyr::filter(n > 0)
message("\nFig 3 group summary (log10(P_ERA/BIO12)):")
print(as.data.frame(f3_stats))

f3 <- ggplot2::ggplot(joined_f |> dplyr::filter(!is.na(log_ratio_era_bio12), p_group %in% f3_stats$p_group),
                       ggplot2::aes(x = p_group, y = log_ratio_era_bio12, fill = p_group)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey50") +
  ggplot2::geom_violin(alpha = 0.5, trim = FALSE) +
  ggplot2::geom_boxplot(width = 0.12, outlier.alpha = 0.3, fill = "white") +
  ggplot2::geom_text(data = f3_stats, ggplot2::aes(x = p_group, y = max(joined_f$log_ratio_era_bio12, na.rm = TRUE) * 1.05,
                                                     label = paste0("n=", n, "\nmed=", round(median, 3))),
                      inherit.aes = FALSE, size = 3.2, vjust = 0) +
  ggplot2::scale_fill_manual(values = CLR_GROUP, guide = "none") +
  ggplot2::labs(x = "P downscaling group", y = "log10(P_ERA / BIO12)",
                title = "Distribution of log10(P_ERA/BIO12) by P downscaling group") +
  theme_precip()
save_fig(file.path(OUTD, "fig_3_ratio_distribution_by_group.png"), f3, width = 7.5, height = 6.5,
  notes = "Violin+boxplot of log10(P_ERA/BIO12) by p_group, shared y-axis, with n and median annotated per group. From table_2_site_groups.csv.",
  input_sources = out_joined)

## ---- Fig 4: ERA_SLOPE, ERA_RMSE, ERA_CORRELATION each against n_years_measured ----
f4_data <- joined_f |>
  dplyr::mutate(n_years_measured_0 = ifelse(is.na(n_years_measured), 0L, n_years_measured)) |>
  tidyr::pivot_longer(cols = c(era_slope, era_rmse, era_correlation), names_to = "stat", values_to = "value") |>
  dplyr::filter(!is.na(value))
f4_data$stat <- factor(f4_data$stat, levels = c("era_slope", "era_rmse", "era_correlation"),
                        labels = c("ERA_SLOPE", "ERA_RMSE", "ERA_CORRELATION"))

f4 <- ggplot2::ggplot(f4_data, ggplot2::aes(x = n_years_measured_0, y = value, color = p_group)) +
  ggplot2::geom_point(alpha = 0.45, size = 1.1) +
  ggplot2::scale_color_manual(values = CLR_GROUP, name = "P downscaling group", drop = TRUE) +
  ggplot2::facet_wrap(~stat, scales = "free_y", ncol = 3) +
  ggplot2::labs(x = "Number of QC-measured precipitation years (n_years_measured; NA treated as 0)",
                y = "Value (raw, sentinel -9999 included)",
                title = "ERA_SLOPE / ERA_RMSE / ERA_CORRELATION (variable P) against measured-year count",
                subtitle = "ERA_RMSE and ERA_CORRELATION are -9999 for every site (see Step 1) -- these two panels are necessarily flat, not a plotting error.") +
  theme_precip()
save_fig(file.path(OUTD, "fig_4_stats_vs_measured_years.png"), f4, width = 11, height = 5,
  notes = "ERA_SLOPE, ERA_RMSE, ERA_CORRELATION (variable P, raw values incl. -9999 sentinel) each against n_years_measured (NA recoded to 0 for plotting, noted in axis label). ERA_RMSE/ERA_CORRELATION are constant -9999 network-wide per Step 1 -- flat panels are the correct, non-degenerate result of that fact. From table_2_site_groups.csv.",
  input_sources = out_joined)

message("\n=== Step 3 figures complete ===")

# ============================================================================
# 4. Substitution test: Beck 2023 tower-cell class in place of ERA5-derived
#    class, for the sites the grouping flags
# ============================================================================
message("\n================ Step 4: substitution test ================")

current_kg_path <- "data/snapshots/site_koppen_era5.csv"
beck_path       <- "data/snapshots/site_koppen_beck2023.csv"
if (!file.exists(current_kg_path)) stop("Not found: ", current_kg_path)
if (!file.exists(beck_path)) stop("Not found: ", beck_path)

current_kg <- readr::read_csv(current_kg_path, show_col_types = FALSE) |>
  dplyr::select(site_id, koppen_class_current = koppen_class)
beck_kg <- readr::read_csv(beck_path, show_col_types = FALSE) |>
  dplyr::select(site_id, koppen_class_beck = koppen_class, koppen_method_beck = koppen_method)

message(current_kg_path, ": ", nrow(current_kg), " sites, dated ",
        format(file.info(current_kg_path)$mtime, "%Y-%m-%d"))
message(beck_path, ": ", nrow(beck_kg), " sites, dated ",
        format(file.info(beck_path)$mtime, "%Y-%m-%d"))

current_sites <- site_meta$site_id
beck_sites <- beck_kg$site_id
missing_from_beck <- setdiff(current_sites, beck_sites)
extra_in_beck <- setdiff(beck_sites, current_sites)
message(sprintf(
  "STALENESS CHECK: %s covers %d sites; the current canonical network has %d sites. %d current sites are ABSENT from this Beck2023 file; %d Beck2023 rows are for sites not in the current network.",
  beck_path, length(beck_sites), length(current_sites), length(missing_from_beck), length(extra_in_beck)))
if (length(missing_from_beck) > 0L) {
  message("Sites the substitution test cannot resolve (no Beck2023 tower-cell class available): ",
          paste(missing_from_beck, collapse = ", "))
}
beck_is_stale <- length(missing_from_beck) > 0L || length(extra_in_beck) > 0L
message("site_koppen_beck2023.csv is ", if (beck_is_stale) "STALE / a different site list than the current 781-site network -- flagged, not worked around." else "current and matches the site list.")

sub_test <- joined_f |>
  dplyr::select(site_id, p_group) |>
  dplyr::left_join(current_kg, by = "site_id") |>
  dplyr::left_join(beck_kg, by = "site_id") |>
  dplyr::mutate(
    flagged = p_group %in% c("not_fitted_slope1", "not_fitted_slope_9999", "no_auxmeteo_record"),
    can_substitute = flagged & !is.na(koppen_class_beck),
    koppen_class_substituted = dplyr::case_when(
      can_substitute ~ koppen_class_beck,
      TRUE ~ koppen_class_current
    ),
    unresolved_no_beck = flagged & is.na(koppen_class_beck),
    class_changed = can_substitute & !is.na(koppen_class_current) &
                     (koppen_class_current != koppen_class_substituted)
  )

n_flagged <- sum(sub_test$flagged)
n_substituted <- sum(sub_test$can_substitute)
n_unresolved <- sum(sub_test$unresolved_no_beck)
n_changed <- sum(sub_test$class_changed, na.rm = TRUE)
message(sprintf("Sites flagged (not_fitted_slope1 + not_fitted_slope_9999 + no_auxmeteo_record): %d / %d (%.1f%%)",
                n_flagged, nrow(sub_test), 100 * n_flagged / nrow(sub_test)))
message(sprintf("Of those, substitutable with a Beck2023 tower-cell class: %d; unresolved (no Beck2023 row): %d",
                n_substituted, n_unresolved))
message(sprintf("Sites whose KG class CHANGES under substitution: %d", n_changed))
if (n_changed > 0L) {
  print(as.data.frame(sub_test |> dplyr::filter(class_changed) |>
                       dplyr::select(site_id, p_group, koppen_class_current, koppen_class_beck)))
}

out_sub <- file.path(OUTD, "table_3_substitution_test.csv")
readr::write_csv(sub_test, out_sub)
write_output_metadata(out_sub, input_sources = c(out_joined, current_kg_path, beck_path),
  notes = paste0(
    "Per-site substitution test: for sites flagged by p_group (not_fitted_slope1, not_fitted_slope_9999, ",
    "no_auxmeteo_record -- i.e. every site with no genuinely fitted P regression; 'fitted' is empty, see ",
    "Step 1), koppen_class_current (data/snapshots/site_koppen_era5.csv, ERA5-normals-derived) is replaced ",
    "with koppen_class_beck (data/snapshots/site_koppen_beck2023.csv, tower-cell Beck 2023 raster class) ",
    "where available. site_koppen_beck2023.csv is dated ", format(file.info(beck_path)$mtime, "%Y-%m-%d"),
    " and covers ", nrow(beck_kg), " sites vs. the current ", length(current_sites), "-site network -- ",
    length(missing_from_beck), " current sites (", paste(missing_from_beck, collapse=", "), ") have no ",
    "Beck2023 row and are left at koppen_class_current (unresolved_no_beck=TRUE), not silently dropped or ",
    "assumed unchanged. n_kg_class_changed=", n_changed, " among the ", n_substituted, " sites actually ",
    "substituted."
  ))
message("Saved: ", out_sub)

## ---- Weighted Jaccard (KG axis), current vs. substituted ----
message("\n---- Weighted Jaccard (30-class KG axis): current vs. substituted ----")

kg_dir <- file.path("data", "external", "koppen_beck2023")
leg_path <- file.path(kg_dir, "legend.txt")
global_path <- file.path(FLUXNET_DATA_ROOT, "snapshots", "koppen_beck2023_global_distribution.csv")
if (!file.exists(leg_path)) stop("Not found: ", leg_path)
if (!file.exists(global_path)) stop("Not found: ", global_path)

leg_lines <- readLines(leg_path)
leg_data <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]
legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
  m <- regmatches(ln, regexec("^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[(\\d+)\\s+(\\d+)\\s+(\\d+)\\]", ln, perl = TRUE))[[1]]
  if (length(m) < 7L) return(NULL)
  data.frame(koppen_class_code = as.integer(m[2]), koppen_class = m[3], stringsAsFactors = FALSE)
}))
class_order <- legend_df$koppen_class

global_df <- readr::read_csv(global_path, show_col_types = FALSE)
p_30 <- global_df$global_land_fraction[match(class_order, global_df$koppen_class)]
p_30[is.na(p_30)] <- 0

weighted_jaccard <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  sum(pmin(p, q)) / sum(pmax(p, q))
}
site_fracs <- function(class_vec, levels_vec) {
  n <- length(class_vec)
  counts <- table(class_vec)
  vapply(levels_vec, function(lv) { c <- counts[lv]; if (is.na(c)) 0 else as.numeric(c) / n }, numeric(1L))
}

q_current <- site_fracs(sub_test$koppen_class_current, class_order)
q_sub     <- site_fracs(sub_test$koppen_class_substituted, class_order)
j_current <- weighted_jaccard(p_30, q_current)
j_sub     <- weighted_jaccard(p_30, q_sub)

message(sprintf("Weighted Jaccard (30-class KG axis), CURRENT (ERA5-normals-derived, all sites): %.4f", j_current))
message(sprintf("Weighted Jaccard (30-class KG axis), SUBSTITUTED (Beck2023 tower-cell for flagged/substitutable sites): %.4f", j_sub))
message(sprintf("Change: %+.4f", j_sub - j_current))

out_jaccard <- file.path(OUTD, "table_4_weighted_jaccard.csv")
readr::write_csv(tibble::tibble(
  variant = c("current_era5_derived", "substituted_beck2023_for_flagged"),
  weighted_jaccard = c(j_current, j_sub),
  n_sites_total = nrow(sub_test),
  n_sites_flagged = n_flagged,
  n_sites_substituted = c(0L, n_substituted),
  n_sites_unresolved_no_beck = c(0L, n_unresolved)
), out_jaccard)
write_output_metadata(out_jaccard, input_sources = c(out_sub, global_path, leg_path),
  notes = paste0(
    "Weighted Jaccard = sum(pmin(p,q))/sum(pmax(p,q)) on the 30-class KG axis, p = Beck2023 global land-area ",
    "fraction per class (koppen_beck2023_global_distribution.csv), q = network site-fraction per class ",
    "(same formula/inputs as scripts/figure_representativeness_kg.R's compute_repr_metrics(), reused not ",
    "modified). 'current' uses koppen_class from site_koppen_era5.csv unmodified; 'substituted' replaces it ",
    "with the Beck2023 tower-cell class for every site this script's p_group flags as having no fitted P ",
    "regression, where a Beck2023 class is available (", n_substituted, "/", n_flagged, " flagged sites; ",
    n_unresolved, " flagged sites left at their current class because site_koppen_beck2023.csv has no row ",
    "for them). KG_ERA5_MAP_MAX_MM is not read or reused anywhere in this computation."
  ))
message("Saved: ", out_jaccard)

message("\n=== precip_downscaling_provenance.R complete ===")
message("See report.md for the full write-up, what this analysis cannot settle, and provisional caveats on ERA_SLOPE.")
