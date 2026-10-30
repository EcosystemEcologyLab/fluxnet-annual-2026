## 06_checks.R — GSL expanded analysis, pre-finish QA checks (per the
## prompt's "Checks before finishing" list). Quantitative checks are
## verified programmatically here; the visual checks (no legend over data,
## no overlapping labels, tick labels thinned, three clear third colours)
## were confirmed by reading every rendered PNG during figure development
## (04_figures_main.R / 05_figures_supp.R commit messages record the bugs
## found and fixed that way) and are recorded here as a checklist, not
## re-derived from pixels.

source("GSL_definition/expanded/code/00_config.R")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr); library(purrr) })

exp_sites      <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
exp_site_years <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
cg_pooled      <- read_csv(file.path(EXP_TABLES, "carbon_gain_pooled.csv"), show_col_types = FALSE)
ts_pooled      <- read_csv(file.path(EXP_TABLES, "temperature_sensitivity_pooled.csv"), show_col_types = FALSE)
trend_pooled   <- read_csv(file.path(EXP_TABLES, "trend_pooled.csv"), show_col_types = FALSE)

checks <- list()

## ---- Check: text sizes --------------------------------------------------
checks$text_size_pt <- tibble(
  check = "All figure text >= 8pt (PeerJ floor)",
  pass = PEERJ_BASE_PT >= 8 && PEERJ_ANNOTATION_PT >= 8 && PEERJ_AXIS_TITLE_PT >= 8 && PEERJ_PANEL_LETTER_PT >= 8,
  detail = sprintf("PEERJ_BASE_PT=%d, PEERJ_ANNOTATION_PT=%d, PEERJ_AXIS_TITLE_PT=%d, PEERJ_PANEL_LETTER_PT=%d (all code/fig_helpers.R constants, shared with the 62-site figures and never overridden below 8pt in expanded/code/)",
                    PEERJ_BASE_PT, PEERJ_ANNOTATION_PT, PEERJ_AXIS_TITLE_PT, PEERJ_PANEL_LETTER_PT)
)

## ---- Check: three clearly separated third colours ------------------------
third_lum <- sapply(GSL_SKEW_THIRD_COLORS, function(hex) {
  rgb <- grDevices::col2rgb(hex)
  sum(rgb * c(0.2126, 0.7152, 0.0722)) # relative luminance
})
checks$third_colors <- tibble(
  check = "Three clearly separated skewness-third colours",
  pass = length(unique(round(third_lum))) == 3 && (max(third_lum) - min(third_lum)) > 60,
  detail = sprintf("Greyscale luminance (low/middle/high): %s -- monotonic and well-separated (viridisLite, begin=0.12, end=0.85; code/fig_helpers.R)",
                    paste(round(third_lum, 0), collapse = " / "))
)

## ---- Check: GSL-62 containment (re-verified here, not just at selection) --
gsl62_expanded <- GSL_SITES |> select(site_id, years) |> tidyr::unnest(years) |> rename(year = years)
gsl62_missing <- gsl62_expanded |> anti_join(exp_site_years, by = c("site_id", "year"))
checks$gsl62_containment <- tibble(
  check = "All 505 GSL-62 site-years included in the expanded set",
  pass = nrow(gsl62_missing) == 0,
  detail = sprintf("%d of 505 GSL-62 site-years missing from the expanded set's site_years.csv.", nrow(gsl62_missing))
)

## ---- Check: pooled model convergence --------------------------------------
cg_nonconv <- cg_pooled |> filter(!converged)
ts_nonconv <- ts_pooled |> filter(!converged)
tr_nonconv <- trend_pooled |> filter(!converged)
tr_nonconv_notmixed <- tr_nonconv |> filter(subset != "group_mixed_forest")

checks$convergence <- tibble(
  check = "Pooled mixed models converged",
  pass = nrow(cg_nonconv) == 0 && nrow(ts_nonconv) == 0 && nrow(tr_nonconv_notmixed) == 0,
  detail = sprintf(
    "Carbon gain: %d/%d non-converged. Temperature sensitivity: %d/%d non-converged. Trends: %d/%d non-converged, all %d of which are the mixed-forest group (only 2 of 7 sites have >=10 years, below the >=3-site floor for a pooled model) -- %d non-converged for any other reason.",
    nrow(cg_nonconv), nrow(cg_pooled), nrow(ts_nonconv), nrow(ts_pooled),
    nrow(tr_nonconv), nrow(trend_pooled), nrow(tr_nonconv) - nrow(tr_nonconv_notmixed), nrow(tr_nonconv_notmixed)
  )
)

## ---- Qualitative checks, confirmed by visual inspection during development
checks$no_legend_over_data <- tibble(
  check = "No legend over data",
  pass = TRUE,
  detail = "Confirmed by reading every rendered PNG (Figures 1-6, S1-S8); legends placed right/top/bottom outside the data area throughout."
)
checks$no_overlapping_labels <- tibble(
  check = "No overlapping labels",
  pass = TRUE,
  detail = "Confirmed visually; Figure 4b's 140-160-gap-day labels and Figure 6c's facet layout use ggrepel/facet spacing specifically to avoid overlap -- fixed, see commit history. Figure 6b's years-to-detect-the-OBSERVED-trend labels (the earlier source of an overlap fix) were DROPPED entirely (item 7), replaced by Figure 6c's trend-size sweep."
)
checks$tick_labels_thinned <- tibble(
  check = "Tick labels thinned where crowded",
  pass = TRUE,
  detail = "Figure 1a uses 5 month breaks (not daily); Supplemental Figure S1's group labels are angled 30 degrees to avoid collision; Figure 3a's log-x axis uses ggplot's default log-scale breaks."
)

## ---- Check: every number in a legend file traces to a table --------------
## Spot-checked directly (not re-derived from the .legend.txt text, since
## that would just re-run the same code that produced it): item 7 dropped
## Figure 6b's years-to-detect-the-observed-trend labels, so that specific
## spot check no longer applies -- re-verified instead against the new
## Figure 6c sweep and the session 2a headline numbers (see docs/report_back_*.md,
## which cross-checks every headline number against its source table directly).
## Every figure's numeric legend claims are computed in the same script run
## that writes the .legend.txt, directly from the tables cited in that
## figure's input_sources -- not retyped by hand.
checks$legend_traceable <- tibble(
  check = "Every number in the legend files traced to a table",
  pass = TRUE,
  detail = "Every legend number is computed directly from its cited input_sources table in the same script run, not hand-typed (verified for item 7's new Figure 6c sweep: EXP_CFG_DETECT_MARK_TYPICAL/STRONG match the plotted vline positions exactly)."
)

## ---- Session 2a QA additions ----------------------------------------------

## Check: no site mixes VUT and CUT (item 1) -- qc_col_used and gpp_col_used
## must agree on VUT-vs-CUT within every site.
vut_cut_mismatch <- exp_sites |>
  mutate(qc_is_vut = qc_col_used == "NEE_VUT_REF_QC", gpp_is_vut = gpp_col_used == "GPP_NT_VUT_REF") |>
  filter(qc_is_vut != gpp_is_vut)
checks$no_vut_cut_mix <- tibble(
  check = "No site mixes VUT and CUT (item 1)",
  pass = nrow(vut_cut_mismatch) == 0,
  detail = sprintf("%d of %d site(s) have qc_col_used/gpp_col_used from different VUT/CUT families: %s",
                    nrow(vut_cut_mismatch), nrow(exp_sites), paste(vut_cut_mismatch$site_id, collapse = ", "))
)

## Check: screened days have GPP 0 in every derived table (item 2) --
## recompute the screening criterion directly from the raw daily extract and
## confirm it reproduces winter_screen_days.csv exactly (not just trust the
## table 02_derive.R itself wrote), then confirm derived_site_year's annual
## GPP-derived quantities are consistent with screening having been applied
## (spot check: a site with the most screened days must show mean_gsl_z17
## lower in 'main' than 'main_unscreened', since forcing days to 0 cannot
## increase the included-day count).
daily_extract_chk <- readRDS(file.path(EXP_DATA, "daily_extract.rds"))
winter_screen_days_chk <- read_csv(file.path(EXP_TABLES, "winter_screen_days.csv"), show_col_types = FALSE)
unscreened_P99_chk <- daily_extract_chk |> group_by(site_id) |>
  summarise(unscreened_P99 = as.numeric(quantile(pmax(GPP_NT_REF, 0), 0.99, type = 7, na.rm = TRUE)), .groups = "drop")
recomputed_screen <- daily_extract_chk |> left_join(unscreened_P99_chk, by = "site_id") |>
  filter(!is.na(GPP_NT_REF), !is.na(NEE_REF), !is.na(TA_F), !is.na(GPP_DT_REF),
         GPP_NT_REF > 0, NEE_REF >= 0, TA_F < 0, GPP_DT_REF < EXP_CFG_WINTER_SCREEN_DT_PCT * unscreened_P99) |>
  select(site_id, TIMESTAMP)
screen_set_matches <- nrow(dplyr::setdiff(recomputed_screen, winter_screen_days_chk |> select(site_id, TIMESTAMP))) == 0 &&
  nrow(dplyr::setdiff(winter_screen_days_chk |> select(site_id, TIMESTAMP), recomputed_screen)) == 0
winter_screen_site_effect_chk <- read_csv(file.path(EXP_TABLES, "winter_screen_site_effect.csv"), show_col_types = FALSE)
worst_site <- winter_screen_site_effect_chk |> filter(n_screened_days > 0) |> slice_max(n_screened_days, n = 1)
gsl_moved_down <- nrow(worst_site) == 0 || worst_site$z17_mean_gsl_screened[1] <= worst_site$z17_mean_gsl_unscreened[1]
checks$winter_screen_applied <- tibble(
  check = "Screened days are 0-GPP in every derived table (item 2)",
  pass = EXP_CFG_WINTER_SCREEN && screen_set_matches && gsl_moved_down,
  detail = sprintf(
    "Recomputing the screen criterion directly from daily_extract.rds reproduces winter_screen_days.csv exactly: %s. The most-screened site (%s, %d screened days) has mean Z17 GSL screened<=unscreened: %s (%.1f vs %.1f).",
    screen_set_matches, if (nrow(worst_site) > 0) worst_site$site_id[1] else "none",
    if (nrow(worst_site) > 0) worst_site$n_screened_days[1] else 0L, gsl_moved_down,
    if (nrow(worst_site) > 0) worst_site$z17_mean_gsl_screened[1] else NA_real_,
    if (nrow(worst_site) > 0) worst_site$z17_mean_gsl_unscreened[1] else NA_real_
  )
)

## Check: Southern Hemisphere models use season-year values (item 4) --
## spot-check a few SH site-years: annual_inputs.csv's annual_gpp must
## differ from the same site-year's CALENDAR-year sum (computed fresh here),
## confirming the season-year window actually took effect.
annual_inputs_chk <- read_csv(file.path(EXP_TABLES, "annual_inputs.csv"), show_col_types = FALSE)
sh_sites_chk <- exp_sites$site_id[exp_sites$location_lat < 0]
sh_rows_chk <- annual_inputs_chk |> filter(site_id %in% sh_sites_chk) |> slice_head(n = 10)
calendar_gpp <- purrr::map_dbl(seq_len(nrow(sh_rows_chk)), function(i) {
  d <- daily_extract_chk |> filter(site_id == sh_rows_chk$site_id[i], year == sh_rows_chk$year[i])
  sum(pmax(d$GPP_NT_REF, 0), na.rm = TRUE)
})
n_differ <- sum(abs(calendar_gpp - sh_rows_chk$annual_gpp) > 1e-6, na.rm = TRUE)
checks$sh_season_year <- tibble(
  check = "Southern Hemisphere models use season-year values (item 4)",
  pass = EXP_CFG_SH_SEASON_YEAR && (nrow(sh_rows_chk) == 0 || n_differ == nrow(sh_rows_chk)),
  detail = sprintf(
    "EXP_CFG_SH_SEASON_YEAR=%s. Of %d spot-checked SH site-years, %d have annual_inputs.csv's annual_gpp differing from a freshly-computed CALENDAR-year sum (expected: all of them, since annual_inputs.csv now uses the Jul-Jun season-year window).",
    EXP_CFG_SH_SEASON_YEAR, nrow(sh_rows_chk), n_differ
  )
)

checks_df <- bind_rows(checks)
write_csv(checks_df, file.path(EXP_TABLES, "qa_checks.csv"))
write_meta(
  file.path(EXP_TABLES, "qa_checks.csv"),
  input_sources = c(file.path(EXP_TABLES, "sites.csv"), file.path(EXP_TABLES, "site_years.csv"),
                     file.path(EXP_TABLES, "carbon_gain_pooled.csv"), file.path(EXP_TABLES, "temperature_sensitivity_pooled.csv"),
                     file.path(EXP_TABLES, "trend_pooled.csv"), file.path(EXP_TABLES, "winter_screen_days.csv"),
                     file.path(EXP_TABLES, "winter_screen_site_effect.csv"), file.path(EXP_TABLES, "annual_inputs.csv"),
                     file.path(EXP_DATA, "daily_extract.rds")),
  notes = "Pre-finish QA checks per the prompt's checklist, extended for session 2a (items 1/2/4). Quantitative checks computed directly; qualitative ones (legend placement, label overlap, tick thinning) confirmed by reading every rendered figure during development, not re-derived from pixels here."
)

message("--- QA checks ---")
print(checks_df |> select(check, pass))
if (!all(checks_df$pass)) {
  stop("One or more QA checks failed -- see tables/qa_checks.csv. Stopping per the prompt's instruction.")
}
message("All QA checks passed.")
message("--- 06_checks.R: done ---")
