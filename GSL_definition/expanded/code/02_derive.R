## 02_derive.R — GSL expanded analysis, step 2: every quantity in
## docs/framework.md's derived-quantities table (reusing derive_one() etc.
## from code/03_derive.R by sourcing, not copying -- see
## logs/prompts/2026-10-30_gsl_expanded.md), plus the new quantities this
## task asks for: 1-F at Z17/T95, season start/end/gap days, the bounds
## check, and inputs the modelling step (03_models.R) needs (annual GPP/NEP,
## annual mean TA_F). Three GPP/neg-GPP variants: main (GPP_NT_REF, negative
## GPP -> 0), dt (GPP_DT_REF, negative GPP -> 0, for Supplemental S5), and
## negdelivered (GPP_NT_REF, negative GPP as-delivered, one summary line for
## S5). Season start/end/gap/bounds-check use the main variant only.

source("GSL_definition/expanded/code/00_config.R")
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R") # g1_adjusted, l_skewness, kelly_skewness,
                                            # year_threshold_matrices, derive_one,
                                            # FINE_GRID_PCT, TASH_GRID_PCT, WINDOW_STEPS, FRACTION_TARGETS
options(gsl.skip_driver = FALSE)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate); library(purrr)
})

exp_sites      <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
exp_site_years <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
daily_extract  <- readRDS(file.path(EXP_DATA, "daily_extract.rds"))

site_years_list <- split(exp_site_years$year, exp_site_years$site_id)
trend_site_ids  <- names(Filter(function(y) length(y) >= 10L, site_years_list))
message(length(trend_site_ids), " expanded trend site(s) (>= 10 passing years).")

## ============================================================================
## Item 2 (session 2a): winter partitioning-artifact screen. A day is
## screened (GPP_NT_REF -> 0) if GPP_NT_REF>0, NEE_REF>=0, TA_F<0 and
## GPP_DT_REF < EXP_CFG_WINTER_SCREEN_DT_PCT of the site's UNSCREENED P99 --
## all four non-missing (an NA in any of the four leaves the day unscreened).
## Unscreened P99 is each site's P99 of zero-floored GPP_NT_REF, computed
## from the ORIGINAL (unscreened) daily_extract, before screening -- the
## fixed reference the screen is defined against, not a moving target.
## ============================================================================
unscreened_P99_by_site <- daily_extract |>
  group_by(site_id) |>
  summarise(unscreened_P99 = as.numeric(quantile(pmax(GPP_NT_REF, 0), 0.99, type = 7, na.rm = TRUE)), .groups = "drop")

daily_screened <- daily_extract |>
  left_join(unscreened_P99_by_site, by = "site_id") |>
  mutate(
    is_screened_day = !is.na(GPP_NT_REF) & !is.na(NEE_REF) & !is.na(TA_F) & !is.na(GPP_DT_REF) &
      GPP_NT_REF > 0 & NEE_REF >= 0 & TA_F < 0 & GPP_DT_REF < EXP_CFG_WINTER_SCREEN_DT_PCT * unscreened_P99,
    GPP_NT_REF_screened = ifelse(is_screened_day, 0, GPP_NT_REF)
  )
n_screened <- sum(daily_screened$is_screened_day)
message(n_screened, " day(s) screened as winter partitioning artifacts across ",
        n_distinct(daily_screened$site_id[daily_screened$is_screened_day]), " site(s).")

winter_screen_days <- daily_screened |>
  filter(is_screened_day) |>
  select(site_id, TIMESTAMP, year, GPP_NT_REF, GPP_DT_REF, NEE_REF, TA_F, unscreened_P99)
write_csv(winter_screen_days, file.path(EXP_TABLES, "winter_screen_days.csv"))
write_meta(
  file.path(EXP_TABLES, "winter_screen_days.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = paste0(
    "Item 2: every day screened as a winter GPP-partitioning artifact (GPP_NT_REF set to 0 before ",
    "every derived quantity in the 'main' variant below) -- GPP_NT_REF>0, NEE_REF>=0, TA_F<0 and ",
    "GPP_DT_REF < ", EXP_CFG_WINTER_SCREEN_DT_PCT * 100, "% of the site's UNSCREENED P99, all four ",
    "non-missing. ", n_screened, " day(s) total."
  )
)

winter_screen_site_year <- daily_screened |>
  semi_join(exp_site_years, by = c("site_id", "year")) |>
  group_by(site_id, year) |>
  summarise(n_days = n(), n_screened_days = sum(is_screened_day), .groups = "drop") |>
  filter(n_screened_days > 0)
write_csv(winter_screen_site_year, file.path(EXP_TABLES, "winter_screen_site_year.csv"))
write_meta(
  file.path(EXP_TABLES, "winter_screen_site_year.csv"),
  input_sources = file.path(EXP_TABLES, "winter_screen_days.csv"),
  notes = "Count of winter-screened days per passing site-year (rows with zero screened days omitted)."
)

## daily_extract column swapped in-place for the 'main'/'negdelivered'
## variants only; 'dt' (GPP_DT_REF) and 'main_unscreened' (comparison) read
## the ORIGINAL, unmodified GPP_NT_REF column.
daily_extract_screened <- daily_screened |>
  mutate(GPP_NT_REF = GPP_NT_REF_screened) |>
  select(all_of(names(daily_extract)))

## ============================================================================
## Step A: derive_one() per site, for 4 variants (main, dt, negdelivered,
## main_unscreened -- item 2's "keep an unscreened variant alongside"). ------
## ============================================================================
variant_defs <- list(
  main             = list(gpp_col = "GPP_NT_REF", neg_to_zero = TRUE,  screened = TRUE),
  main_unscreened  = list(gpp_col = "GPP_NT_REF", neg_to_zero = TRUE,  screened = FALSE),
  dt               = list(gpp_col = "GPP_DT_REF", neg_to_zero = TRUE,  screened = FALSE),
  negdelivered     = list(gpp_col = "GPP_NT_REF", neg_to_zero = FALSE, screened = TRUE)
)

run_variant <- function(vname) {
  v <- variant_defs[[vname]]
  use_screen <- EXP_CFG_WINTER_SCREEN && v$screened
  de <- if (use_screen) daily_extract_screened else daily_extract
  message("Deriving variant '", vname, "': ", v$gpp_col, ", neg_to_zero=", v$neg_to_zero,
          ", screened=", use_screen)
  out <- purrr::map(exp_sites$site_id, function(sid) {
    derive_one(sid, v$gpp_col, v$neg_to_zero, site_years_list[[sid]], de,
               trend_site_ids = trend_site_ids)
  })
  list(
    site      = bind_rows(purrr::map(out, "site")) |> mutate(variant = vname, .before = 1),
    site_year = bind_rows(purrr::map(out, "site_year")) |> mutate(variant = vname, .before = 1),
    fine_grid = bind_rows(purrr::map(out, "fine_grid")) |> mutate(variant = vname, .before = 1),
    fraction_sweep = bind_rows(purrr::map(out, "fraction_sweep")) |> mutate(variant = vname, .before = 1)
  )
}

res_main   <- run_variant("main")
res_mainu  <- run_variant("main_unscreened")
res_dt     <- run_variant("dt")
res_neg    <- run_variant("negdelivered")

derived_site_year_all <- bind_rows(res_main$site_year, res_mainu$site_year, res_dt$site_year, res_neg$site_year)
fine_grid_main         <- res_main$fine_grid
fine_grid_dt           <- res_dt$fine_grid

## ---- Item 2 report: screen's effect per site (main=screened vs
## main_unscreened), P99/skewness/third/mean-GSL. Thirds computed below
## (recompute_thirds truncates below); joined back in after. ----------------
winter_screen_site_effect <- res_main$site |>
  select(site_id, P99_screened = P99, skewness_screened = skewness,
         z17_mean_gsl_screened = z17_mean_gsl, t95_mean_gsl_screened = t95_mean_gsl) |>
  left_join(
    res_mainu$site |> select(site_id, P99_unscreened = P99, skewness_unscreened = skewness,
                               z17_mean_gsl_unscreened = z17_mean_gsl, t95_mean_gsl_unscreened = t95_mean_gsl),
    by = "site_id"
  ) |>
  mutate(
    delta_P99 = P99_screened - P99_unscreened,
    delta_skewness = skewness_screened - skewness_unscreened,
    delta_z17_mean_gsl = z17_mean_gsl_screened - z17_mean_gsl_unscreened,
    delta_t95_mean_gsl = t95_mean_gsl_screened - t95_mean_gsl_unscreened
  ) |>
  left_join(winter_screen_days |> count(site_id, name = "n_screened_days"), by = "site_id") |>
  mutate(n_screened_days = coalesce(n_screened_days, 0L)) |>
  left_join(exp_sites |> select(site_id, group), by = "site_id")

## ---- Recompute skewness thirds on the EXPANDED set, per variant (not the
## 62-site project's fixed 0.63/0.88 cutoffs) -- terciles of this variant's
## own skewness distribution across the expanded sites. ----
recompute_thirds <- function(site_tbl) {
  brks <- quantile(site_tbl$skewness, c(1/3, 2/3), na.rm = TRUE, type = 7)
  site_tbl$skewness_third <- case_when(
    is.na(site_tbl$skewness) ~ NA_character_,
    site_tbl$skewness <= brks[1] ~ "low",
    site_tbl$skewness <= brks[2] ~ "middle",
    TRUE ~ "high"
  )
  attr(site_tbl, "tercile_breaks") <- as.numeric(brks)
  site_tbl
}
derived_site_main  <- recompute_thirds(res_main$site)
derived_site_mainu <- recompute_thirds(res_mainu$site)
derived_site_dt    <- recompute_thirds(res_dt$site)
derived_site_neg   <- recompute_thirds(res_neg$site)
tercile_breaks_main  <- attr(derived_site_main, "tercile_breaks")
tercile_breaks_mainu <- attr(derived_site_mainu, "tercile_breaks")
tercile_breaks_dt    <- attr(derived_site_dt, "tercile_breaks")
message("Expanded-set skewness tercile breaks (main variant, screened): low<=", round(tercile_breaks_main[1], 3),
        ", middle<=", round(tercile_breaks_main[2], 3), " (n=", nrow(derived_site_main), "); vs 0.68/0.91.")
message("Expanded-set skewness tercile breaks (main_unscreened, a22e893-equivalent): low<=",
        round(tercile_breaks_mainu[1], 3), ", middle<=", round(tercile_breaks_mainu[2], 3), ".")

## ---- Item 2 report, third-membership column: did screening move this site
## across a tercile boundary? (compares the SCREENED third, under the
## screened breaks, to the UNSCREENED third, under the unscreened breaks --
## both are each run's own self-consistent membership, which is what matters
## for "does the screen change which third a site is analysed in".) --------
winter_screen_site_effect <- winter_screen_site_effect |>
  left_join(derived_site_main  |> select(site_id, skewness_third_screened = skewness_third), by = "site_id") |>
  left_join(derived_site_mainu |> select(site_id, skewness_third_unscreened = skewness_third), by = "site_id") |>
  mutate(third_changed = !is.na(skewness_third_screened) & !is.na(skewness_third_unscreened) &
           skewness_third_screened != skewness_third_unscreened) |>
  arrange(desc(n_screened_days))
write_csv(winter_screen_site_effect, file.path(EXP_TABLES, "winter_screen_site_effect.csv"))
write_meta(
  file.path(EXP_TABLES, "winter_screen_site_effect.csv"),
  input_sources = file.path(EXP_TABLES, "winter_screen_days.csv"),
  notes = paste0(
    "Item 2: per-site effect of the winter partitioning-artifact screen -- P99, skewness, skewness ",
    "third (each under its own run's self-consistent tercile breaks) and mean GSL at Z17/T95, ",
    "screened ('main') vs unscreened ('main_unscreened') variant. ",
    sum(winter_screen_site_effect$n_screened_days > 0), " of ", nrow(winter_screen_site_effect),
    " sites have >=1 screened day; ", sum(winter_screen_site_effect$third_changed, na.rm = TRUE),
    " site(s) change skewness third."
  )
)
message(sum(winter_screen_site_effect$n_screened_days > 0), " site(s) affected by the winter screen; ",
        sum(winter_screen_site_effect$third_changed, na.rm = TRUE), " change skewness third.")

derived_site_all <- bind_rows(derived_site_main, derived_site_mainu, derived_site_dt, derived_site_neg)

## ---- Join group/IGBP/network/lat and add 1-F at Z17 and T95 --------------
derived_site_all <- derived_site_all |>
  left_join(exp_sites |> select(site_id, igbp, group, network, location_lat, location_long,
                                  carbon_src, in_gsl62, is_long_record), by = "site_id") |>
  mutate(
    share_outside_season_z17 = 1 - z17_mean_f,
    share_outside_season_t95 = 1 - t95_mean_f
  )

## ---- Item 3: skewness-thirds breaks for the final (screened, main) site
## set, reported against session 1's 0.68/0.91 (which were themselves
## computed before the winter screen and before item 1's GPP-aware fallback
## changed the site list). ---------------------------------------------------
skewness_thirds_breaks <- tibble(
  variant = c("main", "main_unscreened", "dt"),
  low_cut = c(tercile_breaks_main[1], tercile_breaks_mainu[1], tercile_breaks_dt[1]),
  middle_cut = c(tercile_breaks_main[2], tercile_breaks_mainu[2], tercile_breaks_dt[2]),
  session1_low_cut = 0.68, session1_middle_cut = 0.91
) |> mutate(delta_low = low_cut - session1_low_cut, delta_middle = middle_cut - session1_middle_cut)
write_csv(skewness_thirds_breaks, file.path(EXP_TABLES, "skewness_thirds_breaks.csv"))
write_meta(
  file.path(EXP_TABLES, "skewness_thirds_breaks.csv"),
  input_sources = file.path(EXP_TABLES, "derived_site.csv"),
  notes = "Item 3: skewness tercile breaks recomputed on the final (post item-1 fallback, post item-2 screen) site set, per variant, against session 1's checks_report_2026-10-30.md breaks (0.68/0.91, computed on the pre-session-2a 151-site screened-free set)."
)
message("Item 3: main-variant tercile breaks ", round(tercile_breaks_main[1], 4), "/",
        round(tercile_breaks_main[2], 4), " vs session 1's 0.68/0.91.")

write_csv(derived_site_all, file.path(EXP_TABLES, "derived_site.csv"))
write_meta(
  file.path(EXP_TABLES, "derived_site.csv"),
  input_sources = c(file.path(EXP_DATA, "daily_extract.rds"), file.path(EXP_TABLES, "sites.csv")),
  notes = paste0(
    "One row per site x variant (main=GPP_NT_REF, winter-screened + zero-floored; main_unscreened=",
    "GPP_NT_REF zero-floored, NOT winter-screened, kept for comparison; dt=GPP_DT_REF zero-floored; ",
    "negdelivered=GPP_NT_REF winter-screened, as-delivered negative GPP), ", n_distinct(derived_site_all$site_id),
    " sites. derive_one() reused by sourcing code/03_derive.R (gsl.skip_driver=TRUE). Skewness ",
    "thirds are TERCILES OF THIS EXPANDED SET'S OWN skewness distribution, recomputed per ",
    "variant (main breaks: low<=", round(tercile_breaks_main[1], 4), ", middle<=",
    round(tercile_breaks_main[2], 4), "; dt breaks: low<=", round(tercile_breaks_dt[1], 4),
    ", middle<=", round(tercile_breaks_dt[2], 4), ") -- NOT the 62-site project's fixed 0.63/0.88 ",
    "cutoffs. share_outside_season_* = 1 - mean F at that threshold (T95's is ~5% by construction)."
  )
)

write_csv(derived_site_year_all, file.path(EXP_TABLES, "derived_site_year.csv"))
write_meta(
  file.path(EXP_TABLES, "derived_site_year.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = "Annual GSL(y) and F(y) at Z17 (10% of P99) and T95, per site x variant x year."
)

write_csv(res_main$fraction_sweep, file.path(EXP_TABLES, "fraction_sweep_main.csv"))
write_meta(
  file.path(EXP_TABLES, "fraction_sweep_main.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = "Per-site mean GSL at each of 20 target included-GPP fractions (0.80-0.99), main variant -- the threshold needed to include that fraction of annual GPP, for Supplemental Figure S7."
)

## ---- Item 6: recompute local sensitivity with a +/-EXP_CFG_LOCAL_SENS_
## HALFWIDTH_PCT centred difference, directly from mean_gsl on the fine
## grid (0.01% of P99 steps) -- NOT GSL_definition/code/03_derive.R's own
## `local_sensitivity` column (shared with the 62-site reproduction, fixed
## at +/-0.05%, out of scope to edit). Overwrites `local_sensitivity` in
## place so every downstream user (fine_grid_summary, Figure 3, S3, S5)
## picks up the wider window without further code changes. -----------------
FINE_GRID_STEP_PCT <- 0.01 # matches 03_derive.R's FINE_GRID_PCT resolution
sens_steps <- round(EXP_CFG_LOCAL_SENS_HALFWIDTH_PCT / FINE_GRID_STEP_PCT)
recompute_local_sensitivity <- function(fg) {
  fg |>
    arrange(site_id, pct_of_P99) |>
    group_by(site_id) |>
    mutate(local_sensitivity = {
      n <- n()
      out <- rep(NA_real_, n)
      lo <- (sens_steps + 1):(n - sens_steps)
      out[lo] <- (mean_gsl[lo + sens_steps] - mean_gsl[lo - sens_steps]) / (2 * EXP_CFG_LOCAL_SENS_HALFWIDTH_PCT)
      out
    }) |>
    ungroup()
}
fine_grid_main <- recompute_local_sensitivity(fine_grid_main)
fine_grid_dt   <- recompute_local_sensitivity(fine_grid_dt)
message("Item 6: local sensitivity recomputed, +/-", EXP_CFG_LOCAL_SENS_HALFWIDTH_PCT,
        "% of P99 centred difference (was +/-0.05%).")

saveRDS(fine_grid_main, file.path(EXP_DATA, "fine_grid_main.rds"))
saveRDS(fine_grid_dt, file.path(EXP_DATA, "fine_grid_dt.rds"))

fine_grid_summary <- bind_rows(fine_grid_main, fine_grid_dt) |>
  group_by(variant, site_id) |>
  summarise(
    sensitivity_at_5pct  = local_sensitivity[which.min(abs(pct_of_P99 - 5))],
    sensitivity_at_10pct = local_sensitivity[which.min(abs(pct_of_P99 - 10))],
    sensitivity_at_20pct = local_sensitivity[which.min(abs(pct_of_P99 - 20))],
    .groups = "drop"
  )
write_csv(fine_grid_summary, file.path(EXP_TABLES, "fine_grid_summary.csv"))
write_meta(
  file.path(EXP_TABLES, "fine_grid_summary.csv"),
  input_sources = c(file.path(EXP_DATA, "fine_grid_main.rds"), file.path(EXP_DATA, "fine_grid_dt.rds")),
  notes = paste0(
    "Per-site local sensitivity (d(mean GSL)/d(% of P99), centered difference over +/-",
    EXP_CFG_LOCAL_SENS_HALFWIDTH_PCT, "% of P99 -- item 6, widened from a22e893's +/-0.05%) at ",
    "5/10/20% of P99, main and dt variants. Full fine grid (5000 points, 0.01-50% of P99) in ",
    "expanded/data/fine_grid_*.rds (gitignored)."
  )
)

message("--- Step A done: derived_site (", nrow(derived_site_all), " rows), derived_site_year (",
        nrow(derived_site_year_all), " rows) ---")

## ============================================================================
## Step B: season start/end/gap days (main variant only), at 4 threshold
## points per site -- 5%, 10% (Z17), 20% of P99, and T95's own %.
## ============================================================================

## Item 2: season boundaries, the bounds check and annual GPP (Step D) are
## "main variant" quantities, so they use the winter-screened GPP_NT_REF
## when the screen is active (EXP_CFG_WINTER_SCREEN) -- same column swap as
## the 'main' derive_one() variant above.
daily_main <- (if (EXP_CFG_WINTER_SCREEN) daily_extract_screened else daily_extract) |>
  mutate(gpp_use = pmax(GPP_NT_REF, 0)) |>
  select(site_id, TIMESTAMP, year, gpp_use, QC_REF, NEE_REF, TA_F)

P99_by_site  <- setNames(derived_site_main$P99, derived_site_main$site_id)
T95_by_site  <- setNames(derived_site_main$t95_pct, derived_site_main$site_id)
sh_sites     <- exp_sites$site_id[exp_sites$location_lat < 0]

SEASON_PCTS <- c("5pct" = 5, "z17" = 10, "20pct" = 20, "t95" = NA) # t95's % filled in per site

#' Season start/end/gap days for one (site, season_year, threshold).
#' `d` is that site's full daily record (gpp_use, NA-safe); `window` is the
#' Date sequence for this season-year window (calendar year for NH; 1 Jul
#' (season_year) - 30 Jun (season_year+1) for SH, labelled by the starting
#' calendar year). A day with NA gpp_use counts as "below" (consistent with
#' this project's NA-counts-as-failing convention elsewhere).
season_one <- function(d, window_dates, T_abs) {
  g <- d$gpp_use[match(window_dates, d$TIMESTAMP)]
  above <- !is.na(g) & g > T_abs
  above[is.na(above)] <- FALSE
  n <- length(above)
  if (all(above)) {
    return(tibble(start_date = window_dates[1], end_date = window_dates[n],
                   gap_days = 0L, above_threshold_all_year = TRUE, n_days = n))
  }
  r <- rle(above)
  run_end <- cumsum(r$lengths)
  run_start <- run_end - r$lengths + 1L
  qualifying <- which(r$values & r$lengths >= 5L)
  if (length(qualifying) == 0) {
    return(tibble(start_date = as.Date(NA), end_date = as.Date(NA),
                   gap_days = NA_integer_, above_threshold_all_year = FALSE, n_days = n))
  }
  first_run <- qualifying[1]; last_run <- qualifying[length(qualifying)]
  s_idx <- run_start[first_run]; e_idx <- run_end[last_run]
  gap_days <- sum(!above[s_idx:e_idx])
  tibble(start_date = window_dates[s_idx], end_date = window_dates[e_idx],
         gap_days = gap_days, above_threshold_all_year = FALSE, n_days = n)
}

season_for_site <- function(sid) {
  d <- daily_main |> filter(site_id == sid)
  P99 <- P99_by_site[[sid]]
  t95pct <- T95_by_site[[sid]]
  pcts <- SEASON_PCTS; pcts["t95"] <- t95pct
  yrs <- site_years_list[[sid]]
  is_sh <- sid %in% sh_sites

  windows <- if (!is_sh) {
    lapply(yrs, function(y) list(season_year = y,
                                   dates = seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by = "day")))
  } else {
    lapply(yrs, function(y) {
      ## SH season-year y = 1 Jul y - 30 Jun (y+1); requires BOTH calendar
      ## years y and y+1 to be in this site's own passing-year list, else
      ## skipped (insufficient/undefined coverage at the record's edges).
      if (!(y + 1L) %in% yrs) return(NULL)
      list(season_year = y, dates = seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day"))
    })
  }
  windows <- Filter(Negate(is.null), windows)

  out <- purrr::map_dfr(windows, function(w) {
    purrr::map_dfr(names(pcts), function(pn) {
      T_abs <- pcts[[pn]] / 100 * P99
      res <- season_one(d, w$dates, T_abs)
      res$threshold_name <- pn
      res$threshold_pct <- pcts[[pn]]
      res$season_year <- w$season_year
      res
    })
  })
  out$site_id <- sid
  out$is_southern <- is_sh
  out
}

message("Computing season start/end/gap days for ", nrow(exp_sites), " sites x 4 thresholds...")
season_boundaries <- purrr::map_dfr(exp_sites$site_id, season_for_site)
season_boundaries <- season_boundaries |>
  mutate(start_doy = lubridate::yday(start_date), end_doy = lubridate::yday(end_date)) |>
  select(site_id, season_year, is_southern, threshold_name, threshold_pct,
         start_date, end_date, start_doy, end_doy, gap_days, above_threshold_all_year, n_days)

n_all_year <- sum(season_boundaries$above_threshold_all_year)
message(n_all_year, " site-year x threshold combination(s) are above threshold all year.")

write_csv(season_boundaries, file.path(EXP_TABLES, "season_boundaries.csv"))
write_meta(
  file.path(EXP_TABLES, "season_boundaries.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = paste0(
    "Season start = first day of the first run of >=5 consecutive days with gpp_use > threshold; ",
    "end = last day of the last such run; gap_days = days below threshold strictly between start ",
    "and end (inclusive range). NA gpp_use counts as 'below'. Northern-hemisphere sites use the ",
    "calendar year; Southern-hemisphere sites (location_lat<0) use 1 Jul (season_year) - 30 Jun ",
    "(season_year+1), included only when BOTH calendar years are in that site's own passing-year ",
    "list. threshold_name: 5pct/20pct = 5%/20% of P99 (Figure 4a); z17 = 10% of P99; t95 = the ",
    "site's own T95 %. above_threshold_all_year flags a site-year with no day below threshold at ",
    "all (start=day 1, end=last day, gap_days=0 trivially). ", n_all_year, " such combination(s)."
  )
)
message("Saved: season_boundaries.csv (", nrow(season_boundaries), " rows)")

## ============================================================================
## Step C (item 8 redesign): bounds check (main variant) -- only failing
## days (QC < EXP_RULE_QC_DAY or NA, in a passing year) whose gap-filled GPP
## lies within +/-EXP_CFG_BOUNDS_NEAR_PCT of P99 of the threshold are
## "near-threshold" (ambiguous): GSL with ALL of them counted above
## (gsl_upper), and again with ALL of them counted below (gsl_lower). A
## failing day far from the threshold is NOT ambiguous (QC alone doesn't put
## its classification in doubt) and keeps its own reported value in both.
## ============================================================================

daily_main_qc <- daily_main |>
  mutate(pass_raw = qc_passes(QC_REF, EXP_RULE_QC_DAY),
         is_fail = ifelse(is.na(pass_raw), TRUE, !pass_raw))

bounds_one <- function(sid) {
  d <- daily_main_qc |> filter(site_id == sid)
  P99 <- P99_by_site[[sid]]
  t95pct <- T95_by_site[[sid]]
  near_abs <- EXP_CFG_BOUNDS_NEAR_PCT * P99
  yrs <- site_years_list[[sid]]
  purrr::map_dfr(yrs, function(y) {
    dy <- d |> filter(year == y)
    purrr::map_dfr(list(z17 = 10, t95 = t95pct), function(pct) {
      T_abs <- pct / 100 * P99
      is_near <- dy$is_fail & !is.na(dy$gpp_use) & abs(dy$gpp_use - T_abs) <= near_abs
      is_near[is.na(is_near)] <- FALSE # failing day with NA gpp_use: not "near" (no value to compare), keeps its reported (NA->below) classification
      base_above <- !is_near & !is.na(dy$gpp_use) & dy$gpp_use > T_abs
      n_near <- sum(is_near)
      gsl_lower <- sum(base_above)          # all near-threshold failing days counted BELOW
      gsl_upper <- gsl_lower + n_near        # all near-threshold failing days counted ABOVE
      gsl_actual <- sum(!is.na(dy$gpp_use) & dy$gpp_use > T_abs) # as reported elsewhere
      tibble(gsl_lower = gsl_lower, gsl_upper = gsl_upper, gsl_actual = gsl_actual,
             n_fail_days = sum(dy$is_fail), n_near_threshold_days = n_near, spread = gsl_upper - gsl_lower)
    }, .id = "threshold_name") |> mutate(year = y)
  }) |> mutate(site_id = sid)
}

message("Computing the redesigned bounds check for ", nrow(exp_sites), " sites x 2 thresholds...")
bounds_check <- purrr::map_dfr(exp_sites$site_id, bounds_one) |>
  select(site_id, year, threshold_name, gsl_lower, gsl_actual, gsl_upper, n_fail_days, n_near_threshold_days, spread)

stopifnot(all(bounds_check$spread == bounds_check$n_near_threshold_days)) # spread is exactly n_near_threshold_days, by construction
stopifnot(all(bounds_check$gsl_actual >= bounds_check$gsl_lower - 0L & bounds_check$gsl_actual <= bounds_check$gsl_upper))

write_csv(bounds_check, file.path(EXP_TABLES, "bounds_check.csv"))
write_meta(
  file.path(EXP_TABLES, "bounds_check.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = paste0(
    "Item 8 redesign. Per site-year, at Z17 and T95: a failing day (fallback QC < ", EXP_RULE_QC_DAY,
    " or NA, in a passing year) is 'near-threshold' (ambiguous) only if its gap-filled GPP lies ",
    "within +/-", EXP_CFG_BOUNDS_NEAR_PCT * 100, "% of P99 of the threshold -- gsl_lower counts ALL ",
    "near-threshold failing days as BELOW; gsl_upper counts ALL of them as ABOVE; every other day ",
    "(passing QC, or failing but far from the threshold) keeps its own reported value in both. ",
    "gsl_actual = the GSL actually reported elsewhere (every day's own real gap-filled value). ",
    "spread = gsl_upper - gsl_lower = n_near_threshold_days exactly (checked). n_fail_days is kept ",
    "for reference (all QC-failing days, not just the near-threshold ones -- a22e893's definition ",
    "of 'spread', now narrower and more realistic than n_fail_days alone)."
  )
)
message("Saved: bounds_check.csv (", nrow(bounds_check), " rows). Spread range: ",
        min(bounds_check$spread), "-", max(bounds_check$spread),
        " (vs n_fail_days range ", min(bounds_check$n_fail_days), "-", max(bounds_check$n_fail_days), ").")

bounds_check_pooled <- bounds_check |>
  group_by(threshold_name) |>
  summarise(
    n_site_years = n(), total_near_threshold_days = sum(n_near_threshold_days),
    mean_spread = mean(spread), max_spread = max(spread),
    n_site_years_any_near = sum(n_near_threshold_days > 0), .groups = "drop"
  )
write_csv(bounds_check_pooled, file.path(EXP_TABLES, "bounds_check_pooled.csv"))
write_meta(
  file.path(EXP_TABLES, "bounds_check_pooled.csv"),
  input_sources = file.path(EXP_TABLES, "bounds_check.csv"),
  notes = "Item 8: pooled (network-wide) summary of the redesigned bounds check, at Z17 and T95."
)

## ============================================================================
## Step D: annual GPP/NEP and annual mean TA_F per site-year (main variant),
## inputs for 03_models.R's carbon-gain and temperature-sensitivity steps.
## NEP = -NEE (prompt instruction).
## ============================================================================

annual_inputs_calendar <- daily_main |>
  group_by(site_id, year) |>
  summarise(
    annual_gpp = sum(gpp_use, na.rm = TRUE),
    n_days_gpp = sum(!is.na(gpp_use)),
    annual_nep = sum(-NEE_REF, na.rm = TRUE),
    n_days_nee = sum(!is.na(NEE_REF)),
    annual_mean_ta = mean(TA_F, na.rm = TRUE),
    n_days_ta = sum(!is.na(TA_F)),
    .groups = "drop"
  ) |>
  semi_join(exp_site_years, by = c("site_id", "year"))

## ---- Item 4: for Southern-Hemisphere sites, replace the calendar-year sum/
## mean above with the Jul(y)-Jun(y+1) season-year sum/mean -- the same
## window and inclusion rule as Step B's season_for_site() SH branch (both
## calendar years y and y+1 must be in the site's own passing-year list). --
if (EXP_CFG_SH_SEASON_YEAR) {
  season_year_one <- function(sid, y) {
    win <- seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day")
    d <- daily_main |> filter(site_id == sid, TIMESTAMP %in% win)
    tibble(
      site_id = sid, year = y,
      annual_gpp = sum(d$gpp_use, na.rm = TRUE), n_days_gpp = sum(!is.na(d$gpp_use)),
      annual_nep = sum(-d$NEE_REF, na.rm = TRUE), n_days_nee = sum(!is.na(d$NEE_REF)),
      annual_mean_ta = mean(d$TA_F, na.rm = TRUE), n_days_ta = sum(!is.na(d$TA_F))
    )
  }
  annual_inputs_sh <- purrr::map_dfr(sh_sites, function(sid) {
    yrs <- site_years_list[[sid]]
    valid_yrs <- yrs[(yrs + 1L) %in% yrs]
    purrr::map_dfr(valid_yrs, function(y) season_year_one(sid, y))
  })
  n_sh_replaced <- sum(annual_inputs_calendar$site_id %in% sh_sites)
  annual_inputs <- bind_rows(
    annual_inputs_calendar |> filter(!site_id %in% sh_sites),
    annual_inputs_sh
  ) |> semi_join(exp_site_years, by = c("site_id", "year"))
  message("Item 4: replaced ", n_sh_replaced, " calendar-year SH site-year row(s) with ",
          nrow(annual_inputs_sh), " Jul-Jun season-year row(s) (", length(sh_sites), " SH sites; ",
          n_sh_replaced - nrow(annual_inputs_sh), " dropped -- season-year undefined at the record edge).")
} else {
  annual_inputs <- annual_inputs_calendar
}

write_csv(annual_inputs, file.path(EXP_TABLES, "annual_inputs.csv"))
write_meta(
  file.path(EXP_TABLES, "annual_inputs.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = paste0(
    "Annual GPP (gC/m2/yr, zero-floored, main variant), NEP = -NEE_REF, and mean TA_F per passing ",
    "site-year. Sums/means use na.rm=TRUE over available days; n_days_* columns report coverage. ",
    "Item 4: for the ", length(sh_sites), " Southern-Hemisphere sites (location_lat<0), 'year' is the ",
    "Jul(year)-Jun(year+1) SEASON year and the sums/means are taken over that window (not the ",
    "calendar year) -- a22e893 used the calendar year for every site; EXP_CFG_SH_SEASON_YEAR=",
    EXP_CFG_SH_SEASON_YEAR, "."
  )
)
message("Saved: annual_inputs.csv (", nrow(annual_inputs), " rows)")

## ---- New supplementary figure support: annual GPP from the UNSCREENED
## daily extract, calendar-year only (no SH season-year correction needed --
## this table exists purely to compare Figure 5a's carbon-gain result with
## vs without the winter screen; it is not used by any main-run table). ----
annual_gpp_unscreened <- daily_extract |>
  mutate(gpp_use = pmax(GPP_NT_REF, 0)) |>
  group_by(site_id, year) |>
  summarise(annual_gpp_unscreened = sum(gpp_use, na.rm = TRUE), .groups = "drop") |>
  semi_join(exp_site_years, by = c("site_id", "year"))
write_csv(annual_gpp_unscreened, file.path(EXP_TABLES, "annual_gpp_unscreened.csv"))
write_meta(
  file.path(EXP_TABLES, "annual_gpp_unscreened.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = "Annual GPP (gC/m2/yr, zero-floored, calendar year, NOT winter-screened) per passing site-year -- support table for the winter-screen on/off supplementary figure (Figure 2/5a/6 comparison) only, not used by any main-run table."
)

message("--- 02_derive.R: done ---")
