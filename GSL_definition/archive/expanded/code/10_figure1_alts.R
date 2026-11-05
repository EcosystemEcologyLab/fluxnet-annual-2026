## 10_figure1_alts.R — GSL expanded analysis: Figure 1_alt1 two-site variant
## (Dave's prompt, 2026-11-02; revised 2026-11-02 -- see SESSION_LOG.md).
##
## Figure 2_alt2 (the deciduous-forest min/max-skewness pair) was retired in
## the revision and its code path removed -- this script now produces only
## Figure 1_alt1 (AU-How vs US-SRM). Does NOT modify or overwrite the
## existing Figure 1/2 or any other existing script/table/figure -- writes
## only to figures/"Figure 1_alt1".*  and tables/figure1_alts_years.csv.
##
## Reads only the already-committed "main"-variant tables (sites.csv,
## site_years.csv, derived_site.csv, winter_screen_days.csv) and a
## read-only pull from the repo-root DuckDB store (dataset = 'FLUXMET') for
## the two sites used here -- no data/*.rds extract is read. Same daily
## GPP series, winter screen and negative-GPP handling as the main run:
## GPP_NT_REF (per-site VUT/CUT, sites.csv$gpp_col_used), winter-screened
## days (winter_screen_days.csv) forced to 0, then zero-floored
## (neg_to_zero = TRUE, as in 02_derive.R's "main" variant).

source("GSL_definition/expanded/code/00_config.R")
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R") # g1_adjusted() only, reused for this script's own fresh skewness
options(gsl.skip_driver = FALSE)

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(tidyr); library(readr)
  library(lubridate); library(purrr); library(ggplot2); library(patchwork)
})

exp_sites          <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
site_years         <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
derived_site       <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE) |> filter(variant == "main")
winter_screen_days <- read_csv(file.path(EXP_TABLES, "winter_screen_days.csv"), show_col_types = FALSE) |>
  mutate(is_screened = TRUE) |> select(site_id, TIMESTAMP, is_screened)

## ---- Raw daily GPP for AU-How and US-SRM (read-only, dataset =
## 'FLUXMET'), with each site's own main-run VUT/CUT fallback column
## (sites.csv$gpp_col_used) resolved into one GPP_NT_REF column, then the
## committed winter screen applied and negative GPP floored to 0 -- the
## "main" variant's own handling (02_derive.R), recomputed fresh here
## rather than re-reading the gitignored data/daily_extract.rds. ----------
ALT_SITES <- c("US-SRM", "AU-How")
site_meta <- exp_sites |> filter(site_id %in% ALT_SITES) |>
  select(site_id, igbp, group, location_lat, gpp_col_used)
stopifnot(all(ALT_SITES %in% site_meta$site_id))

con <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
site_list_sql <- paste(sprintf("'%s'", ALT_SITES), collapse = ", ")
daily_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, GPP_NT_VUT_REF, GPP_NT_CUT_REF
  FROM daily
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", site_list_sql))
dbDisconnect(con, shutdown = TRUE)
daily_raw$TIMESTAMP <- as.Date(as.character(daily_raw$TIMESTAMP))

daily <- daily_raw |>
  left_join(site_meta |> select(site_id, gpp_col_used), by = "site_id") |>
  mutate(GPP_NT_REF = ifelse(gpp_col_used == "GPP_NT_VUT_REF", GPP_NT_VUT_REF, GPP_NT_CUT_REF)) |>
  left_join(winter_screen_days, by = c("site_id", "TIMESTAMP")) |>
  mutate(gpp_use = ifelse(coalesce(is_screened, FALSE), 0, pmax(GPP_NT_REF, 0))) |>
  select(site_id, TIMESTAMP, gpp_use)
message("Pulled ", nrow(daily_raw), " daily FLUXMET row(s) for ", length(ALT_SITES), " site(s): ",
        paste(ALT_SITES, collapse = ", "))

is_sh <- function(sid) site_meta$location_lat[site_meta$site_id == sid] < 0
raw_years_list <- split(site_years$year, site_years$site_id)

#' A site's own R2/N=14 passing years usable as ONE annual unit in both the
#' histogram and the time-series panel. For a Northern-Hemisphere site this
#' is just its full passing-year list. For a Southern-Hemisphere site
#' (location_lat < 0) a calendar-label year y is only usable when BOTH y
#' and y+1 are themselves passing years -- the Jul(y)-Jun(y+1) window the
#' time-series panel below needs spans parts of both -- same rule as
#' 02_derive.R's SH season-year handling (item 4) and the original Figure
#' 1's panel b/c (docs/methods_memo.md, "session 2b"). Unlike that original
#' figure, THIS script applies the same restriction to the histogram panel
#' too, so that panel's skewness/P99/n are computed from exactly the years
#' the time-series panel below it draws -- the prompt's "use exactly those
#' years in both... panels" requirement.
effective_years <- function(sid) {
  yrs <- sort(raw_years_list[[sid]])
  if (is_sh(sid)) yrs[(yrs + 1L) %in% yrs] else yrs
}

#' One site's annual window for year y: calendar Jan-Dec, or (Southern
#' Hemisphere) Jul(y)-Jun(y+1), day_idx = 1 at the window's first day.
window_dates <- function(sid, y) {
  if (is_sh(sid)) seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day")
  else            seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by = "day")
}
year_series <- function(sid, y) {
  dates <- window_dates(sid, y)
  d <- daily |> filter(site_id == sid)
  g <- tibble(TIMESTAMP = dates) |> left_join(d, by = "TIMESTAMP")
  tibble(site_id = sid, year = y, day_idx = seq_along(dates), gpp_use = g$gpp_use)
}

## ============================================================================
## Builder: 2x2 layout. `lo`/`hi` = the lower-/higher-skewness site (left/
## right columns; panels a/c = lo, b/d = hi). Returns the per-site
## years-used rows for the companion table.
## ============================================================================
build_alt_figure <- function(stem, lo, hi, pair_label) {
  site_levels <- c(lo, hi)

  n_lo <- length(effective_years(lo)); n_hi <- length(effective_years(hi))
  N <- min(n_lo, n_hi)
  years_for <- function(sid) sort(utils::tail(sort(effective_years(sid)), N))
  years_lo <- years_for(lo); years_hi <- years_for(hi)
  message(sprintf("%s: %s has %d usable passing year(s), %s has %d -- N = %d. %s most recent years: %s. %s: %s.",
                   stem, lo, n_lo, hi, n_hi, N, lo, paste(years_lo, collapse = ","), hi, paste(years_hi, collapse = ",")))

  any_sh <- is_sh(lo) || is_sh(hi)

  ## ---- Pooled distribution + fresh skewness/P99/n, computed from exactly
  ## the N selected years' day-windows (not the site's full-record values
  ## in derived_site.csv, which pool a possibly different, larger year
  ## set). ----
  site_series <- function(sid, yrs) bind_rows(lapply(yrs, function(y) year_series(sid, y)))
  series_lo <- site_series(lo, years_lo); series_hi <- site_series(hi, years_hi)
  series_all <- bind_rows(series_lo, series_hi) |> mutate(site_id = factor(site_id, levels = site_levels))

  site_stats <- function(sid, series) {
    pooled <- series$gpp_use[!is.na(series$gpp_use)]
    tibble(site_id = sid, P99 = as.numeric(quantile(pooled, 0.99, type = 7, na.rm = TRUE)),
           skewness = g1_adjusted(pooled), n_years = N)
  }
  stats_tbl <- bind_rows(site_stats(lo, series_lo), site_stats(hi, series_hi)) |>
    mutate(site_id = factor(site_id, levels = site_levels))

  hist_data <- series_all |>
    left_join(stats_tbl |> select(site_id, P99), by = "site_id") |>
    mutate(pct_of_P99 = 100 * gpp_use / P99)

  ## ---- Shared bin width / x-axis range for both histogram panels, chosen
  ## from the pooled data of BOTH sites in this figure only. ----
  BINWIDTH <- 5
  xmax <- max(120, ceiling(max(hist_data$pct_of_P99, na.rm = TRUE) * 1.02 / 10) * 10)
  message(sprintf("%s: shared histogram binwidth = %d%% of P99, x-axis range = 0-%d%%.", stem, BINWIDTH, xmax))

  annot <- stats_tbl |> mutate(label = sprintf("skewness = %.2f\nP99 = %.2f gC m-2 d-1\nn = %d yr", skewness, P99, n_years))
  letter_top <- tibble(site_id = factor(site_levels, levels = site_levels), letter = c("a", "b"))

  p_top <- ggplot(hist_data, aes(x = pct_of_P99)) +
    geom_histogram(binwidth = BINWIDTH, boundary = 0, fill = "grey70", colour = "grey40", linewidth = nature_lwd(0.3)) +
    geom_text(data = annot, aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE,
              hjust = 1.05, vjust = 1.2, size = PEERJ_ANNOTATION_PT / .pt, lineheight = 0.9) +
    geom_text(data = letter_top, aes(x = -Inf, y = Inf, label = letter), inherit.aes = FALSE,
              fontface = "bold", size = PEERJ_PANEL_LETTER_PT / .pt, hjust = -0.4, vjust = 1.3) +
    coord_cartesian(xlim = c(0, xmax)) +
    facet_wrap(~ site_id, ncol = 2, scales = "free_y") +
    labs(x = "Daily GPP (% of site's own P99)", y = "Days") +
    theme_gsl() + theme(strip.text = element_text(face = "bold"))

  ## ---- Bottom row (revised 2026-11-02): per day of year (day of season
  ## year for a Southern-Hemisphere site), the MEDIAN daily GPP across the
  ## N years used, with a shaded band from the min to the max across those
  ## same years -- replaces the earlier one-thin-line-per-year-by-colour
  ## design (no year colour legend now). ----
  band_data <- series_all |>
    group_by(site_id, day_idx) |>
    summarise(
      median_gpp = median(gpp_use, na.rm = TRUE),
      min_gpp = if (all(is.na(gpp_use))) NA_real_ else min(gpp_use, na.rm = TRUE),
      max_gpp = if (all(is.na(gpp_use))) NA_real_ else max(gpp_use, na.rm = TRUE),
      .groups = "drop"
    )

  x_lab <- if (any_sh) "Day of year (day of season year, 1 = 1 Jul, for Southern Hemisphere sites)" else "Day of year"
  letter_bottom <- tibble(site_id = factor(site_levels, levels = site_levels), letter = c("c", "d"))

  p_bottom <- ggplot(band_data, aes(x = day_idx)) +
    geom_ribbon(aes(ymin = min_gpp, ymax = max_gpp), fill = "grey70", alpha = 0.5) +
    geom_line(aes(y = median_gpp), colour = "grey20", linewidth = nature_lwd(PEERJ_LWD_DATA)) +
    geom_text(data = letter_bottom, aes(x = -Inf, y = Inf, label = letter), inherit.aes = FALSE,
              fontface = "bold", size = PEERJ_PANEL_LETTER_PT / .pt, hjust = -0.4, vjust = 1.3) +
    facet_wrap(~ site_id, ncol = 2, scales = "free_y") +
    labs(x = x_lab, y = "Daily GPP (gC m-2 d-1)") +
    theme_gsl() + theme(strip.text = element_text(face = "bold"))

  fig <- p_top / p_bottom + plot_layout(heights = c(1, 1.1))

  legend_lines <- c(
    sprintf("%s. %s (panels a, c; left) vs %s (panels b, d; right); %s.", stem, lo, hi, pair_label),
    sprintf("Both sites use the same number of years, N = %d (the smaller of the two sites' R2/N=14",
            N),
    "passing-year counts; for a Southern-Hemisphere site this counts only years with a valid",
    "Jul-Jun window, i.e. both that calendar year and the next are themselves passing years --",
    "see the script's own comment on effective_years()). Each site's N MOST RECENT such years are",
    "used, identically, in both the histogram above it and the median/range panel below it.",
    sprintf("%s years used: %s.", lo, paste(years_lo, collapse = ", ")),
    sprintf("%s years used: %s.", hi, paste(years_hi, collapse = ", ")),
    "Panels a, b: histogram of daily GPP (main-variant GPP_NT_REF, winter-screened to 0 and",
    "zero-floored, as in the main run) as % of that site's own P99 -- P99, skewness and n computed",
    "fresh from exactly the years used (not derived_site.csv's full-record values). Same bin width",
    sprintf("(%d%% of P99) and x-axis range (0-%d%%) in both panels.", BINWIDTH, xmax),
    "Panels c, d: for each day of year (day of season year for a Southern-Hemisphere site), the",
    "MEDIAN daily GPP (gC m-2 d-1) across the N years used (line), with a shaded band from the",
    "minimum to the maximum across those same years. No threshold lines, no GPP-summed-by-season",
    "panel, no year colour legend.",
    sprintf("Years used per site and this figure's N: tables/figure1_alts_years.csv.")
  )

  save_gsl_figure(
    fig, stem, height_mm = 190,
    legend_lines = legend_lines,
    input_sources = c("data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')",
                        file.path(EXP_TABLES, "sites.csv"), file.path(EXP_TABLES, "site_years.csv"),
                        file.path(EXP_TABLES, "winter_screen_days.csv"), file.path(EXP_TABLES, "derived_site.csv")),
    notes = sprintf("N = %d. %s years: %s. %s years: %s.", N, lo, paste(years_lo, collapse = ";"),
                     hi, paste(years_hi, collapse = ";"))
  )

  bind_rows(
    tibble(figure = stem, role = "lower_skew", site_id = lo, n_years_used = N,
           years_used = paste(years_lo, collapse = ";"), is_southern_hemisphere = is_sh(lo),
           skewness_fresh = stats_tbl$skewness[stats_tbl$site_id == lo],
           P99_fresh = stats_tbl$P99[stats_tbl$site_id == lo]),
    tibble(figure = stem, role = "higher_skew", site_id = hi, n_years_used = N,
           years_used = paste(years_hi, collapse = ";"), is_southern_hemisphere = is_sh(hi),
           skewness_fresh = stats_tbl$skewness[stats_tbl$site_id == hi],
           P99_fresh = stats_tbl$P99[stats_tbl$site_id == hi])
  )
}

## ============================================================================
## Figure 1_alt1 -- US-SRM (higher skewness) vs AU-How (lower skewness),
## both woody-grass mosaic (prompt-specified pair). Figure 2_alt2 (the
## deciduous-forest min/max-skewness pair) was retired 2026-11-02 -- see
## SESSION_LOG.md -- and its code path removed from this script.
## ============================================================================
years_tbl <- build_alt_figure(
  "Figure 1_alt1", lo = "AU-How", hi = "US-SRM",
  pair_label = "both woody-grass mosaic (WSA)"
)

write_csv(years_tbl, file.path(EXP_TABLES, "figure1_alts_years.csv"))
write_meta(
  file.path(EXP_TABLES, "figure1_alts_years.csv"),
  input_sources = c(file.path(EXP_TABLES, "sites.csv"), file.path(EXP_TABLES, "site_years.csv"),
                     file.path(EXP_TABLES, "derived_site.csv"), "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')"),
  notes = paste0(
    "Years used per site for Figure 1_alt1 (AU-How/US-SRM) only -- Figure 2_alt2 (the deciduous-",
    "forest min/max-skewness pair) was retired 2026-11-02 and its rows removed. n_years_used = N = ",
    "the smaller of the two sites' usable-passing-year counts (Southern-Hemisphere counts restricted ",
    "to Jul-Jun-window-valid years -- see 10_figure1_alts.R's effective_years()). skewness_fresh/",
    "P99_fresh are computed fresh from exactly those N years' daily GPP (main-variant handling), not ",
    "derived_site.csv's full-record values."
  )
)
message("Saved: ", file.path(EXP_TABLES, "figure1_alts_years.csv"))

message("--- 10_figure1_alts.R: done --- Figure 1_alt1 (AU-How/US-SRM) written.")
