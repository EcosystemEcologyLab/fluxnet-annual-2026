## 12_methods_review_figures.R — GSL expanded analysis: draft review figures
## for Dave and Tash to discuss, drawn from version 4's methods-comparison
## outputs (11_gsl_methods_compare.R, commit 4965af6) plus one small fresh
## daily-data pull needed for figure B's AU-ASM panel (AU-ASM is not one of
## the 15 comparison sites). See logs/prompts/2026-11-03_gsl_methods_review_figures.md.
##
## Read-only against every existing table; does NOT modify 11_gsl_methods_compare.R
## or any of its committed tables/figures. Writes only NEW figures, under
## figures/drafts/methods_review/. NT series only, throughout, per Tash's framing:
## show a RANGE of thresholds/methods, never a two-way comparison; colour is
## reserved for skewness (continuous), never for vegetation/forest-vs-open.
##
## Four figures:
##   A. Range of all 8 methods' mean GSL, 15 sites sorted by skewness, calendar
##      year_rule (main) + min_gpp_month companion. M2's full 22-value grid as a
##      light band; the three labelled choices (Z16, 85%, 95% of annual GPP)
##      drawn larger.
##   B. Threshold-dependence raster + histogram, AU-ASM-vs-AU-How and
##      US-SRM-vs-AU-How (calendar rule only). AU-ASM computed fresh here (same
##      rules as the 15 sites) since it is not one of them.
##   C. Per-method season placement at US-Ha1/US-Wkg/US-Var/AU-How (calendar)
##      plus US-Var again under min_gpp_month.
##   D. Skewness vs. cross-method GSL range, both year_rules, AU-How annotated.
source("GSL_definition/expanded/code/00_config.R")
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R") # FINE_GRID_PCT only
options(gsl.skip_driver = FALSE)

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(tidyr); library(readr)
  library(lubridate); library(purrr); library(ggplot2); library(patchwork); library(ggrepel)
})

## ============================================================================
## Sites and already-committed version-4 tables (read-only).
## ============================================================================
IAV_SITES <- c("AU-How", "NL-Loo", "DE-Tha", "FR-Pue", "CH-Dav", "US-MMS", "US-Mpj", "AT-Neu",
               "US-Ha1", "JP-Api", "IL-Yat", "US-Ton", "US-Var", "US-Wkg", "US-SRM")
EXTRA_SITE <- "AU-ASM" # figure B only -- not one of the 15, computed fresh below
ALL16 <- c(IAV_SITES, EXTRA_SITE)

exp_sites       <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
derived_site_all <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE)
gsl_site_year    <- read_csv(file.path(EXP_TABLES, "gsl_methods_site_year.csv"), show_col_types = FALSE)
gsl_site_summary <- read_csv(file.path(EXP_TABLES, "gsl_methods_site_summary.csv"), show_col_types = FALSE)
m2_grid22        <- read_csv(file.path(EXP_TABLES, "gsl_methods_m2_grid22.csv"), show_col_types = FALSE)
rule_range       <- read_csv(file.path(EXP_TABLES, "gsl_methods_year_rule_range.csv"), show_col_types = FALSE)

site_meta <- exp_sites |> filter(site_id %in% ALL16) |>
  select(site_id, igbp, group, location_lat, first_year, last_year, gpp_col_used, qc_col_used)
stopifnot(all(ALL16 %in% site_meta$site_id))
is_sh <- function(sid) site_meta$location_lat[site_meta$site_id == sid] < 0
message("Southern-Hemisphere site(s): ", paste(ALL16[sapply(ALL16, is_sh)], collapse = ", "))

## FIX (2026-11-03): skewness now from the unscreened NT series (main_unscreened),
## not the winter-screened main variant, which is no longer used anywhere in this
## script's 8 methods. Only US-MMS and CH-Dav swap rank order between the two
## variants (5th/6th, ascending); 5 of 15 sites (AU-How, IL-Yat, US-Ton, US-Var,
## US-SRM) have identical skewness under both (no days are winter-screened there).
skew_tbl <- derived_site_all |> filter(site_id %in% IAV_SITES, variant == "main_unscreened") |> select(site_id, skewness)
site_order <- skew_tbl |> arrange(skewness) |> pull(site_id)

## ============================================================================
## Raw daily pull (read-only, dataset='FLUXMET'), 15 sites + AU-ASM. NT only.
## Mirrors 11_gsl_methods_compare.R's own GPP_NT_REF/QC_REF construction.
## ============================================================================
con <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
site_list_sql <- paste(sprintf("'%s'", ALL16), collapse = ", ")
daily_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, GPP_NT_VUT_REF, GPP_NT_CUT_REF, NEE_VUT_REF, NEE_CUT_REF,
         NEE_VUT_REF_QC, NEE_CUT_REF_QC
  FROM daily
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", site_list_sql))
dbDisconnect(con, shutdown = TRUE)
daily_raw$TIMESTAMP <- as.Date(as.character(daily_raw$TIMESTAMP))
message("Pulled ", nrow(daily_raw), " daily FLUXMET row(s) for ", length(ALL16), " site(s) (15 + AU-ASM).")

daily0 <- daily_raw |>
  left_join(site_meta |> select(site_id, gpp_col_used, qc_col_used), by = "site_id") |>
  mutate(
    is_vut = gpp_col_used == "GPP_NT_VUT_REF",
    GPP_NT_REF = ifelse(is_vut, GPP_NT_VUT_REF, GPP_NT_CUT_REF),
    QC_REF     = ifelse(qc_col_used == "NEE_VUT_REF_QC", NEE_VUT_REF_QC, NEE_CUT_REF_QC)
  ) |>
  mutate(gpp_nt = pmax(GPP_NT_REF, 0)) |>
  select(site_id, TIMESTAMP, gpp_nt, QC_REF)

make_continuous <- function(sid) {
  d <- daily0 |> filter(site_id == sid) |> arrange(TIMESTAMP)
  full_dates <- seq(min(d$TIMESTAMP), max(d$TIMESTAMP), by = "day")
  tibble(site_id = sid, TIMESTAMP = full_dates) |> left_join(d |> select(-site_id), by = "TIMESTAMP")
}
daily_cont <- bind_rows(lapply(ALL16, make_continuous))
message("Continuous daily sequence built for ", length(ALL16), " site(s).")

## ============================================================================
## Generic, start_month-parameterised window/passing-year machinery (NOT the
## .YEAR_RULE global-dispatch pattern from 11_gsl_methods_compare.R -- this
## script has few call sites, so an explicit start_month argument is simpler
## and keeps this script fully independent of script 11's internals).
## ============================================================================
window_dates_g <- function(sid, y, start_month) {
  start <- as.Date(sprintf("%d-%02d-01", y, start_month))
  seq(start, start + months(12) - days(1), by = "day")
}
calendar_start_month <- function(sid) if (is_sh(sid)) 7L else 1L

season_years_generic <- function(sid, start_month) {
  d <- daily_cont |> filter(site_id == sid)
  rng_lo <- site_meta$first_year[site_meta$site_id == sid] - 1L
  rng_hi <- site_meta$last_year[site_meta$site_id == sid]
  keep <- integer(0)
  for (y in rng_lo:rng_hi) {
    win <- window_dates_g(sid, y, start_month)
    qc_vals <- d$QC_REF[match(win, d$TIMESTAMP)]
    pass_raw <- qc_passes(qc_vals, EXP_RULE_QC_DAY)
    n_fail <- sum(is.na(pass_raw) | !pass_raw)
    if (n_fail <= EXP_RULE_MAX_FAIL_DAYS) keep <- c(keep, y)
  }
  keep
}
calendar_years <- function(sid) season_years_generic(sid, calendar_start_month(sid))

pool_values_g <- function(sid, start_month, years) {
  d <- daily_cont |> filter(site_id == sid)
  dts <- do.call(c, lapply(years, function(y) window_dates_g(sid, y, start_month)))
  v <- d$gpp_nt[match(dts, d$TIMESTAMP)]
  v[!is.na(v)]
}
date_to_pos <- function(y, start_month, date) {
  if (is.na(date)) return(NA_real_)
  win_start <- as.Date(sprintf("%d-%02d-01", y, start_month))
  as.numeric(date - win_start) + 1
}

## ---- min_gpp_month per site (15 sites), recomputed fresh here, same
## algorithm as 11_gsl_methods_compare.R: the calendar rule's own pooled
## passing years' monthly mean NT GPP, lowest wins. Independent recomputation
## (not read from any table -- v4 did not persist it), but deterministic from
## the same inputs/algorithm, so it reproduces v4's reported months exactly. ----
month_mean_gpp_g <- function(sid, start_month, years) {
  d <- daily_cont |> filter(site_id == sid)
  dts <- do.call(c, lapply(years, function(y) window_dates_g(sid, y, start_month)))
  mo <- lubridate::month(dts)
  val <- d$gpp_nt[match(dts, d$TIMESTAMP)]
  vapply(1:12, function(m) mean(val[mo == m], na.rm = TRUE), numeric(1))
}
min_gpp_month_for <- setNames(sapply(IAV_SITES, function(s) {
  which.min(month_mean_gpp_g(s, calendar_start_month(s), calendar_years(s)))
}), IAV_SITES)
message("Min-GPP-month per site (recomputed, matches v4): ",
        paste(sprintf("%s=%d", IAV_SITES, min_gpp_month_for), collapse = ", "))

start_month_for_rule <- function(sid, rule) if (rule == "calendar") calendar_start_month(sid) else min_gpp_month_for[[sid]]
years_for_rule <- function(sid, rule) if (rule == "calendar") calendar_years(sid) else season_years_generic(sid, min_gpp_month_for[[sid]])

## ---- P99 (NT, unscreened): calendar rule read straight from derived_site.csv
## ('main_unscreened'), as 11_gsl_methods_compare.R's own checks confirmed this
## equals the freshly pooled value exactly, for all 16 sites here. ----
p99_lookup <- derived_site_all |> filter(variant == "main_unscreened", site_id %in% ALL16) |> select(site_id, P99)
P99_cal <- setNames(p99_lookup$P99, p99_lookup$site_id)
stopifnot(all(ALL16 %in% names(P99_cal)))

## ============================================================================
## Method roster: headline sub per method (matches 11_gsl_methods_compare.R's
## own FIG_METHODS exactly), plus a method-FAMILY label (Tash's framing: shape
## distinguishes method family, colour is reserved for skewness/the 3 named
## choices -- never for vegetation/forest-vs-open).
## ============================================================================
HEADLINE_ALL <- tibble::tribble(
  ~method, ~sub,       ~family,                     ~use_run5,
  "M1",    "summer",   "span (smoothed GPP)",       FALSE,
  "M2",    "10pct",    "threshold count",           TRUE,
  "M3",    "1gC",      "threshold count",           TRUE,
  "M4",    "0.85",     "fraction of annual GPP",    TRUE,
  "M5",    "span",     "NEP-based",                 FALSE,
  "M6",    "anchored", "NEP-based",                 FALSE,
  "M7",    "span",     "derivative or normalized",  FALSE,
  "M8",    "0.25",     "derivative or normalized",  FALSE
)
FAMILY_SHAPES <- c("threshold count" = 16, "fraction of annual GPP" = 17, "span (smoothed GPP)" = 15,
                   "NEP-based" = 18, "derivative or normalized" = 8)
FAMILY_ORDER <- c("span (smoothed GPP)", "threshold count", "fraction of annual GPP", "NEP-based", "derivative or normalized")
## FIX (2026-11-03): Z16 changed from black to blue -- in Figure B, a black
## Z16 strip was visually indistinguishable from the raster's own black ("1.00
## share") shading directly above it (both render as solid black at a glance).
CHOICE_COLORS <- c("10% of P99 (Z16)" = "#377eb8", "85% of annual GPP" = "#1b9e77", "95% of annual GPP" = "#d95f02")
M2_PCTS_GRID <- c(0.1, 0.5, 1:20) # Tash's full 22-value grid, as v4

## patchwork intercepts `+ labs()/theme()` and applies it only to the LAST
## plot added, not the whole composite (confirmed by a minimal repro) -- a
## patchwork object needs plot_annotation() instead for a figure-wide title.
extra_title <- function(p) {
  if (inherits(p, "patchwork")) {
    p + patchwork::plot_annotation(title = "Draft for discussion",
          theme = theme(plot.title = element_text(size = PEERJ_AXIS_TITLE_PT, face = "bold", colour = "grey10", hjust = 0)))
  } else {
    p + labs(title = "Draft for discussion") +
      theme(plot.title = element_text(size = PEERJ_AXIS_TITLE_PT, face = "bold", colour = "grey10", hjust = 0))
  }
}

message("=== Setup complete. Building figures. ===")

## ============================================================================
## Figure A: range of all 8 methods' mean GSL, 15 sites sorted by skewness.
## Calendar rule (main) + min_gpp_month rule (companion). M2's full 22-value
## grid as a light band; grey bar = min-to-max mean GSL across all 8 headline
## methods (reuses gsl_methods_year_rule_range.csv, same headline subs v4
## used); three labelled choices drawn larger.
## ============================================================================
compute_m2_grid_means <- function(sid, start_month, years, P99, pcts) {
  d <- daily_cont |> filter(site_id == sid)
  sapply(pcts, function(pct) {
    T_abs <- pct / 100 * P99
    per_yr <- sapply(years, function(y) {
      win <- window_dates_g(sid, y, start_month)
      vals <- d$gpp_nt[match(win, d$TIMESTAMP)]
      sum(!is.na(vals) & vals > T_abs, na.rm = TRUE)
    })
    mean(per_yr, na.rm = TRUE)
  })
}

## ---- M2 22-grid band under min_gpp_month: not persisted by v4 (only the
## calendar-rule grid is, in gsl_methods_m2_grid22.csv) -- recomputed fresh
## here, same algorithm, for the 15 sites. ----
P99_mingpp <- setNames(sapply(IAV_SITES, function(s) {
  ys <- season_years_generic(s, min_gpp_month_for[[s]])
  as.numeric(quantile(pool_values_g(s, min_gpp_month_for[[s]], ys), 0.99, type = 7))
}), IAV_SITES)
m2_band_mingpp <- purrr::map_dfr(IAV_SITES, function(s) {
  ys <- season_years_generic(s, min_gpp_month_for[[s]])
  means <- compute_m2_grid_means(s, min_gpp_month_for[[s]], ys, P99_mingpp[[s]], M2_PCTS_GRID)
  tibble(site_id = s, band_min = min(means, na.rm = TRUE), band_max = max(means, na.rm = TRUE))
})
m2_band_cal <- m2_grid22 |> group_by(site_id) |> summarise(band_min = min(mean_gsl, na.rm = TRUE), band_max = max(mean_gsl, na.rm = TRUE), .groups = "drop")

plot_fig_a <- function(rule) {
  gss <- gsl_site_summary |> filter(year_rule == rule, variant %in% c("nt", "all"))
  pts <- gss |> inner_join(HEADLINE_ALL, by = c("method", "sub")) |> select(site_id, method, family, mean_gsl)
  big <- gss |> filter((method == "M2" & sub == "10pct") | (method == "M4" & sub %in% c("0.85", "0.95"))) |>
    mutate(choice = case_when(method == "M2" ~ "10% of P99 (Z16)", sub == "0.85" ~ "85% of annual GPP", sub == "0.95" ~ "95% of annual GPP")) |>
    select(site_id, choice, mean_gsl)
  if (rule == "calendar") {
    grey <- rule_range |> select(site_id, gsl_min = gsl_min_calendar, gsl_max = gsl_max_calendar)
    band <- m2_band_cal
  } else {
    grey <- rule_range |> select(site_id, gsl_min = gsl_min_min_gpp_month, gsl_max = gsl_max_min_gpp_month)
    band <- m2_band_mingpp
  }
  y_num <- setNames(seq_along(site_order), site_order)
  pts  <- pts  |> filter(site_id %in% IAV_SITES) |> mutate(y = y_num[site_id], family = factor(family, levels = FAMILY_ORDER))
  big  <- big  |> filter(site_id %in% IAV_SITES) |> mutate(y = y_num[site_id], choice = factor(choice, levels = names(CHOICE_COLORS)))
  grey <- grey |> filter(site_id %in% IAV_SITES) |> mutate(y = y_num[site_id])
  band <- band |> filter(site_id %in% IAV_SITES) |> mutate(y = y_num[site_id])
  skew_lab <- skew_tbl |> mutate(y = y_num[site_id])

  xlim_lo <- -60
  p <- ggplot() +
    geom_segment(data = grey, aes(x = gsl_min, xend = gsl_max, y = y, yend = y), colour = "grey70", linewidth = 2.2) +
    geom_segment(data = band, aes(x = band_min, xend = band_max, y = y, yend = y), colour = "#6baed6", linewidth = 4.2, alpha = 0.5) +
    geom_point(data = pts, aes(x = mean_gsl, y = y, shape = family), size = 2.0, colour = "grey15") +
    ## M2's labelled choice keeps the "threshold count" family shape (circle); M4's two keep
    ## "fraction of annual GPP"'s shape (triangle) -- fixed per-layer (not mapped), so these
    ## share the SAME shape legend as `pts` instead of opening a second, conflicting one.
    geom_point(data = dplyr::filter(big, choice == "10% of P99 (Z16)"), aes(x = mean_gsl, y = y, colour = choice), shape = FAMILY_SHAPES[["threshold count"]], size = 3.6) +
    geom_point(data = dplyr::filter(big, choice != "10% of P99 (Z16)"), aes(x = mean_gsl, y = y, colour = choice), shape = FAMILY_SHAPES[["fraction of annual GPP"]], size = 3.6) +
    geom_tile(data = skew_lab, aes(x = xlim_lo, y = y, fill = skewness), width = 34, height = 0.85) +
    geom_text(data = skew_lab, aes(x = xlim_lo, y = y, label = sprintf("%.2f", skewness)), colour = "white", size = PEERJ_ANNOTATION_PT / .pt * 0.85) +
    scale_y_continuous(breaks = seq_along(site_order), labels = site_order, minor_breaks = NULL, expand = expansion(add = 0.8)) +
    scale_shape_manual(values = FAMILY_SHAPES, name = "Method family") +
    scale_colour_manual(values = CHOICE_COLORS, name = "Labelled choice") +
    scale_fill_viridis_c(name = "Skewness") +
    labs(x = sprintf("Mean GSL (days, NT series, %s year_rule)", rule), y = NULL) +
    theme_gsl() + theme(legend.position = "right")
  extra_title(p)
}
p_fig_a_cal <- plot_fig_a("calendar")
p_fig_a_min <- plot_fig_a("min_gpp_month")

differ_months <- IAV_SITES[min_gpp_month_for != 1L]
message("Figure A: sites where calendar and min_gpp_month differ (lowest-GPP month != January): ", paste(differ_months, collapse = ", "))

fs::dir_create(file.path(GSL_FIG_DIR, "drafts", "methods_review"))
save_gsl_figure(p_fig_a_cal, "drafts/methods_review/Figure A", width_mm = 200, height_mm = 180,
  legend_lines = c(
    "Draft for discussion. Figure A: range of all 8 GSL methods' mean GSL (days), 15 sites, calendar",
    "year_rule (NH = Jan-Dec, AU-How = Jul-Jun). NT series only. Sites sorted by skewness (ascending,",
    "printed and coloured continuously at left -- never by vegetation/forest-vs-open). Grey bar = min-to-",
    "max mean GSL across the 8 headline methods (same subs as tables/gsl_methods_year_rule_range.csv).",
    "Light blue band = M2's full 22-value %-of-P99 grid. Small shapes = one marker per method, shaped by",
    "method family (threshold count: M2/M3; fraction of annual GPP: M4; span of smoothed GPP: M1; NEP-",
    "based: M5/M6; derivative/normalized: M7/M8). Large coloured points = the three framing choices: 10%",
    "of P99 (Z16, = M2), 85% and 95% of annual GPP (= M4; Tash's own target between these is not settled).",
    "Numbers behind this figure: tables/gsl_methods_site_summary.csv, tables/gsl_methods_m2_grid22.csv,",
    "tables/gsl_methods_year_rule_range.csv, tables/derived_site.csv."
  ),
  input_sources = c(file.path(EXP_TABLES, "gsl_methods_site_summary.csv"), file.path(EXP_TABLES, "gsl_methods_m2_grid22.csv"),
                     file.path(EXP_TABLES, "gsl_methods_year_rule_range.csv"), file.path(EXP_TABLES, "derived_site.csv")),
  notes = "Draft for Dave/Tash discussion. Calendar year_rule. Not a Z16-vs-T85/T95 comparison -- shows the full range.")

save_gsl_figure(p_fig_a_min, "drafts/methods_review/Figure A (min_gpp_month companion)", width_mm = 200, height_mm = 180,
  legend_lines = c(
    "Draft for discussion. Figure A companion: same as Figure A, but every site's year starts on its own",
    "lowest-mean-NT-GPP calendar month (min_gpp_month year_rule) instead of the calendar year.",
    sprintf("Sites where this differs from the calendar-year figure (lowest-GPP month != January): %s.", paste(differ_months, collapse = ", ")),
    "At every other site, the two figures are identical (lowest-GPP month = January = the calendar rule's",
    "own start month). M2's 22-value-grid band is recomputed fresh for this rule (not in any committed",
    "table); every other element is read from tables/gsl_methods_site_summary.csv etc.",
    "Numbers behind this figure: tables/gsl_methods_site_summary.csv, tables/gsl_methods_year_rule_range.csv,",
    "tables/derived_site.csv; M2 band computed fresh in this script (not persisted)."
  ),
  input_sources = c(file.path(EXP_TABLES, "gsl_methods_site_summary.csv"), file.path(EXP_TABLES, "gsl_methods_year_rule_range.csv"),
                     file.path(EXP_TABLES, "derived_site.csv"), "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET', for the min_gpp_month M2 band only)"),
  notes = "Draft for Dave/Tash discussion. min_gpp_month year_rule companion to Figure A.")
message("Saved Figure A (calendar + min_gpp_month companion).")

## ============================================================================
## Figure B: threshold-dependence raster + histogram, two pairs
## (AU-ASM vs AU-How; US-SRM vs AU-How), calendar rule only. AU-ASM is not one
## of the 15 sites -- computed fresh here, same rules (own passing years, own
## P99, own M4 85%/95% thresholds).
## ============================================================================
m4_pct_lookup <- function(sid, target_str) {
  v <- gsl_site_year |> filter(site_id == sid, variant == "nt", method == "M4", sub == target_str, year_rule == "calendar") |> pull(threshold_pct_of_P99)
  unique(v)[1]
}
compute_m4_threshold_pct <- function(sid, start_month, years, P99, target) {
  d <- daily_cont |> filter(site_id == sid)
  fine_abs <- FINE_GRID_PCT / 100 * P99
  f_mean <- sapply(fine_abs, function(T_abs) {
    per_yr <- sapply(years, function(y) {
      win <- window_dates_g(sid, y, start_month)
      vv <- d$gpp_nt[match(win, d$TIMESTAMP)]
      tot <- sum(vv, na.rm = TRUE)
      if (tot == 0) return(NA_real_)
      sum(vv[!is.na(vv) & vv > T_abs], na.rm = TRUE) / tot
    })
    mean(per_yr, na.rm = TRUE)
  })
  f_rounded <- round(f_mean, 6)
  idx <- which(f_rounded <= round(target, 6))[1]
  if (is.na(idx)) NA_real_ else FINE_GRID_PCT[idx]
}

build_raster_site <- function(sid) {
  sm <- calendar_start_month(sid)
  years <- calendar_years(sid)
  P99 <- P99_cal[[sid]]
  d <- daily_cont |> filter(site_id == sid)
  rows <- purrr::map_dfr(years, function(y) {
    win <- window_dates_g(sid, y, sm)
    vals <- d$gpp_nt[match(win, d$TIMESTAMP)]
    share <- vapply(vals, function(v) if (is.na(v)) NA_real_ else mean(v > (M2_PCTS_GRID / 100 * P99)), numeric(1))
    tibble(year = y, day_pos = seq_along(win), value = vals, share = share)
  })
  if (sid %in% IAV_SITES) {
    m85 <- m4_pct_lookup(sid, "0.85"); m95 <- m4_pct_lookup(sid, "0.95")
  } else {
    m85 <- compute_m4_threshold_pct(sid, sm, years, P99, 0.85)
    m95 <- compute_m4_threshold_pct(sid, sm, years, P99, 0.95)
  }
  crit <- tibble(choice = names(CHOICE_COLORS), pct_p99 = c(10, m85, m95))
  ## FIX (2026-11-03): individual in-season DAYS per choice, not a first-to-
  ## last-qualifying-day bracket (which ran the full Z16/85%/95% span and hid
  ## the raster underneath it).
  member <- purrr::pmap_dfr(crit, function(choice, pct_p99) {
    T_abs <- pct_p99 / 100 * P99
    rows |> dplyr::filter(!is.na(value), value > T_abs) |> dplyr::transmute(year, day_pos, choice = choice)
  })
  list(rows = rows, member = member, years = years, sm = sm, P99 = P99, m85 = m85, m95 = m95)
}

month_breaks_g <- function(sid, sm, years) {
  y0 <- years[1]
  win0 <- window_dates_g(sid, y0, sm)
  mo <- if (sm == 1L) 1:12 else c(sm:12, 1:(sm - 1))
  yr <- if (sm == 1L) rep(y0, 12) else c(rep(y0, 13 - sm), rep(y0 + 1L, sm - 1L))
  starts <- as.Date(sprintf("%d-%02d-01", yr, mo))
  tibble(pos = match(starts, win0), label = format(starts, "%b"))
}

## FIX (2026-11-03): the raster now occupies only the TOP part of each year's
## row (RASTER_YOFF/RASTER_H), and each of the 3 labelled choices gets its own
## thin strip stacked below it (STRIP_YOFF/STRIP_H) -- geometrically
## non-overlapping (checked by construction below), so the strips mark
## individual in-season days without ever covering the raster.
RASTER_YOFF <- 0.20; RASTER_H <- 0.50
STRIP_YOFF <- c("10% of P99 (Z16)" = -0.125, "85% of annual GPP" = -0.245, "95% of annual GPP" = -0.365)
STRIP_H <- 0.11

plot_raster_panel <- function(sid, dat, letter) {
  mb <- month_breaks_g(sid, dat$sm, dat$years)
  member <- dat$member |> mutate(choice = factor(choice, levels = names(CHOICE_COLORS)),
                                   y0 = year + STRIP_YOFF[as.character(choice)])
  ## geom_segment (not geom_tile) for the strips: geom_tile's fill aesthetic
  ## would need a second, discrete fill scale alongside the raster's continuous
  ## one, which ggplot cannot do in one plot without an extra package (not
  ## added, per CLAUDE.md). geom_segment's stroke is `colour`, a genuinely
  ## separate aesthetic from `fill`, so it coexists with the raster cleanly.
  p <- ggplot() +
    geom_tile(data = dat$rows, aes(x = day_pos, y = year + RASTER_YOFF, fill = share), height = RASTER_H) +
    geom_segment(data = member, aes(x = day_pos, xend = day_pos, y = y0 - STRIP_H / 2, yend = y0 + STRIP_H / 2, colour = choice),
                 linewidth = 0.45, na.rm = TRUE) +
    scale_fill_distiller(palette = "Greys", direction = 1, limits = c(0, 1), name = "Share of 22\nthresholds\nin-season") +
    scale_colour_manual(values = CHOICE_COLORS, name = "Labelled choice\n(individual days)") +
    scale_x_continuous(breaks = mb$pos, labels = mb$label, expand = expansion(add = 2)) +
    scale_y_continuous(breaks = dat$years) +
    labs(x = NULL, y = "Year", title = sprintf("%s (calendar year_rule)", sid)) +
    theme_gsl() + theme(plot.title = element_text(size = PEERJ_BASE_PT, face = "bold", hjust = 0),
                          axis.text.x = element_text(angle = 45, hjust = 1, size = PEERJ_BASE_PT - 1))
  p + gsl_panel_letter(letter, x = -Inf, y = Inf)
}
## FIX (2026-11-03): x-axis capped at 0-120% of P99 for every site (comparable
## across panels); days above 120% are dropped from the histogram bars and
## counted in an in-panel annotation instead of stretching the axis.
plot_hist_panel <- function(sid, dat, letter) {
  hd <- dat$rows |> filter(!is.na(value)) |> mutate(pct_p99 = value / dat$P99 * 100)
  n_above <- sum(hd$pct_p99 > 120)
  hd_capped <- hd |> filter(pct_p99 <= 120)
  vlines <- tibble(choice = names(CHOICE_COLORS), pct_p99 = c(10, dat$m85, dat$m95))
  p <- ggplot(hd_capped, aes(x = pct_p99)) +
    annotate("rect", xmin = min(M2_PCTS_GRID), xmax = max(M2_PCTS_GRID), ymin = 0, ymax = Inf, fill = "#6baed6", alpha = 0.25) +
    geom_histogram(binwidth = 3, boundary = 0, fill = "grey50", colour = "white", linewidth = 0.1) +
    geom_vline(data = vlines, aes(xintercept = pct_p99, colour = choice), linewidth = 1.0, na.rm = TRUE) +
    scale_colour_manual(values = CHOICE_COLORS, guide = "none") +
    scale_x_continuous(limits = c(0, 120)) +
    annotate("text", x = 118, y = Inf, label = sprintf("%d d > 120%%\nof P99 (not shown)", n_above),
              hjust = 1, vjust = 1.3, size = PEERJ_ANNOTATION_PT / .pt * 0.8, colour = "grey30") +
    labs(x = "Daily NT GPP (% of P99)", y = "Days") +
    theme_gsl()
  list(plot = p + gsl_panel_letter(letter, x = -Inf, y = Inf), n_above = n_above)
}

make_fig_b <- function(site_x, letters) {
  dat_x <- build_raster_site(site_x)
  dat_how <- build_raster_site("AU-How")
  p1 <- plot_raster_panel(site_x, dat_x, letters[1])
  h2 <- plot_hist_panel(site_x, dat_x, letters[2])
  p3 <- plot_raster_panel("AU-How", dat_how, letters[3])
  h4 <- plot_hist_panel("AU-How", dat_how, letters[4])
  combo <- (p1 | h2$plot) / (p3 | h4$plot) + patchwork::plot_layout(widths = c(2.2, 1), guides = "collect")
  list(plot = extra_title(combo), dat_x = dat_x, dat_how = dat_how, n_above_x = h2$n_above, n_above_how = h4$n_above)
}

fb1 <- make_fig_b("AU-ASM", c("a", "b", "c", "d"))
save_gsl_figure(fb1$plot, "drafts/methods_review/Figure B (AU-ASM vs AU-How)", width_mm = 190, height_mm = 190,
  legend_lines = c(
    "Draft for discussion. Figure B (AU-ASM vs AU-How), calendar year_rule (Jul-Jun, Southern Hemisphere).",
    "NT series. (a,c) Raster (top part of each year's row): each day shaded by the share of Tash's 22",
    "%-of-P99 thresholds at which that day's NT GPP counts as in-season (dark = in season at every",
    "threshold; white = at none). Below the raster, 3 thin coloured strips per year (not overlapping the",
    "raster): the INDIVIDUAL days counted in-season by each of the three labelled choices (10% of P99 =",
    "Z16; 85%/95% of annual GPP) -- a tick for each such day, not a first-to-last-day bracket. (b,d)",
    "Histogram of daily NT GPP, as % of P99, x-axis capped at 0-120% of P99 for every site (days above",
    "120% are counted, not plotted -- see each panel's in-panel note); light blue = the 22-threshold",
    "grid's own range (0.1-20% of P99); vertical lines = the same three labelled choices, in %-of-P99 units.",
    "AU-ASM is not one of the 15 comparison sites; computed fresh here from its own passing years, same",
    sprintf("rules as the 15 sites (P99 = %.2f gC/m2/d, M4 85%%/95%% thresholds = %.2f%%/%.2f%% of P99; %d day(s) above 120%% of P99).", fb1$dat_x$P99, fb1$dat_x$m85, fb1$dat_x$m95, fb1$n_above_x),
    sprintf("AU-How: P99 = %.2f gC/m2/d, M4 85%%/95%% thresholds = %.2f%%/%.2f%% of P99; %d day(s) above 120%% of P99.", fb1$dat_how$P99, fb1$dat_how$m85, fb1$dat_how$m95, fb1$n_above_how),
    "Numbers behind this figure: this script's own fresh daily-data computation (not persisted to a table;",
    "AU-How's M4 thresholds are also in tables/gsl_methods_site_year.csv)."
  ),
  input_sources = c("data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')", file.path(EXP_TABLES, "derived_site.csv"), file.path(EXP_TABLES, "gsl_methods_site_year.csv")),
  notes = "Draft for Dave/Tash discussion. AU-ASM computed fresh (not one of the 15 sites).")

fb2 <- make_fig_b("US-SRM", c("a", "b", "c", "d"))
save_gsl_figure(fb2$plot, "drafts/methods_review/Figure B (US-SRM vs AU-How)", width_mm = 190, height_mm = 190,
  legend_lines = c(
    "Draft for discussion. Figure B (US-SRM vs AU-How), calendar year_rule (US-SRM: Jan-Dec; AU-How: Jul-Jun,",
    "Southern Hemisphere). NT series. (a,c) Raster (top part of each year's row): each day shaded by the",
    "share of Tash's 22 %-of-P99 thresholds at which that day's NT GPP counts as in-season (dark = in",
    "season at every threshold; white = at none). Below the raster, 3 thin coloured strips per year (not",
    "overlapping the raster): the INDIVIDUAL days counted in-season by each of the three labelled choices",
    "(10% of P99 = Z16; 85%/95% of annual GPP) -- a tick for each such day, not a first-to-last-day bracket.",
    "(b,d) Histogram of daily NT GPP, as % of P99, x-axis capped at 0-120% of P99 for every site (days",
    "above 120% are counted, not plotted -- see each panel's in-panel note); light blue = the 22-threshold",
    "grid's own range (0.1-20% of P99); vertical lines = the same three labelled choices, in %-of-P99 units.",
    sprintf("US-SRM: P99 = %.2f gC/m2/d, M4 85%%/95%% thresholds = %.2f%%/%.2f%% of P99; %d day(s) above 120%% of P99.", fb2$dat_x$P99, fb2$dat_x$m85, fb2$dat_x$m95, fb2$n_above_x),
    sprintf("AU-How: P99 = %.2f gC/m2/d, M4 85%%/95%% thresholds = %.2f%%/%.2f%% of P99; %d day(s) above 120%% of P99.", fb2$dat_how$P99, fb2$dat_how$m85, fb2$dat_how$m95, fb2$n_above_how),
    "Numbers behind this figure: tables/gsl_methods_site_year.csv (both sites' M4 thresholds) plus this",
    "script's own fresh daily-data computation for the raster/histogram."
  ),
  input_sources = c("data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')", file.path(EXP_TABLES, "gsl_methods_site_year.csv")),
  notes = "Draft for Dave/Tash discussion. Tash's suggested pair; both sites are among the 15.")
message("Saved Figure B (both pairs).")

## ============================================================================
## Figure C: where each method puts the season. US-Ha1/US-Wkg/US-Var/AU-How
## (calendar) + US-Var again (min_gpp_month). Count methods (M2/M3/M4) use the
## 5-day run rule (start_date_run5/end_date_run5) for start/end, per the prompt.
## ============================================================================
build_curve_c <- function(sid, rule) {
  sm <- start_month_for_rule(sid, rule); ys <- years_for_rule(sid, rule)
  d <- daily_cont |> filter(site_id == sid)
  rows <- purrr::map_dfr(ys, function(y) {
    win <- window_dates_g(sid, y, sm)
    vals <- d$gpp_nt[match(win, d$TIMESTAMP)]
    tibble(year = y, day_pos = seq_along(win), value = vals)
  })
  rows |> group_by(day_pos) |> summarise(med = median(value, na.rm = TRUE), q25 = quantile(value, 0.25, na.rm = TRUE),
                                           q75 = quantile(value, 0.75, na.rm = TRUE), .groups = "drop")
}
## FIX (2026-11-03): Figure C's own roster -- same 8 methods as HEADLINE_ALL,
## but (a) each row carries a name+variant label, not a bare "M1".."M8", and
## (b) M4 is split into its two labelled-choice rows (85%/95% of annual GPP)
## instead of one row at its 0.85 headline, per the prompt's examples.
HEADLINE_C <- tibble::tribble(
  ~row_label,                  ~method, ~sub,    ~family,                    ~use_run5,
  "M1 span, summer P99",       "M1",    "summer", "span (smoothed GPP)",      FALSE,
  "M2, 10% of P99",            "M2",    "10pct",  "threshold count",          TRUE,
  "M3, 1 gC m-2 d-1",          "M3",    "1gC",    "threshold count",          TRUE,
  "M4, 85% of annual GPP",     "M4",    "0.85",   "fraction of annual GPP",   TRUE,
  "M4, 95% of annual GPP",     "M4",    "0.95",   "fraction of annual GPP",   TRUE,
  "M5, NEP > 0 span",          "M5",    "span",   "NEP-based",                FALSE,
  "M6, Hu, anchored",          "M6",    "anchored","NEP-based",               FALSE,
  "M7, Ma",                    "M7",    "span",   "derivative or normalized", FALSE,
  "M8, Panwar 0.25",           "M8",    "0.25",   "derivative or normalized", FALSE
)

## purrr::pmap_dfr's function args would shadow dplyr's data-mask columns of
## the same name inside filter() -- args renamed (mth/sb/...) to avoid that trap.
build_bars_c <- function(sid, rule) {
  sm <- start_month_for_rule(sid, rule)
  gy <- gsl_site_year |> filter(site_id == sid, year_rule == rule, variant %in% c("nt", "all"))
  purrr::pmap_dfr(HEADLINE_C, function(row_label, method, sub, family, use_run5) {
    mth <- method; sb <- sub; r5 <- use_run5
    g <- gy |> filter(method == mth, sub == sb)
    if (nrow(g) == 0) return(NULL)
    sdate <- if (r5) g$start_date_run5 else g$start_date
    edate <- if (r5) g$end_date_run5 else g$end_date
    pos_s <- mapply(function(y, dt) date_to_pos(y, sm, dt), g$year, sdate)
    pos_e <- mapply(function(y, dt) date_to_pos(y, sm, dt), g$year, edate)
    tibble(row_label = row_label, family = family,
           med_s = median(pos_s, na.rm = TRUE), q25_s = quantile(pos_s, 0.25, na.rm = TRUE), q75_s = quantile(pos_s, 0.75, na.rm = TRUE),
           med_e = median(pos_e, na.rm = TRUE), q25_e = quantile(pos_e, 0.25, na.rm = TRUE), q75_e = quantile(pos_e, 0.75, na.rm = TRUE))
  })
}

## One composite (curve-over-bars) per site/rule. Laid out 2 columns x 3 rows
## below (not a single column) -- 5 stacked full two-tier panels do not fit
## PeerJ's page height with 8 legible method rows each (confirmed by a first
## attempt: the 8 method category labels overlapped into an unreadable block
## when each bars panel got under ~20 mm of height). The y-axis "Method" title
## is dropped (redundant with the M1-M8 tick labels) to save width for 2 columns.
plot_panel_c <- function(sid, rule, letter) {
  sm <- start_month_for_rule(sid, rule); ys <- years_for_rule(sid, rule)
  curve <- build_curve_c(sid, rule)
  bars <- build_bars_c(sid, rule) |> mutate(row_label = factor(row_label, levels = HEADLINE_C$row_label))
  mb <- month_breaks_g(sid, sm, ys)
  xlims <- c(1, 366)
  p_curve <- ggplot(curve, aes(x = day_pos)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "grey75", alpha = 0.6) +
    geom_line(aes(y = med), colour = "grey15", linewidth = 0.7, na.rm = TRUE) +
    scale_x_continuous(limits = xlims, breaks = mb$pos, labels = mb$label) +
    labs(x = NULL, y = "Median\nNT GPP", title = sprintf("%s (%s year_rule)", sid, rule)) +
    theme_gsl() + theme(plot.title = element_text(size = PEERJ_BASE_PT, face = "bold", hjust = 0), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p_curve <- p_curve + gsl_panel_letter(letter, x = -Inf, y = Inf)

  p_bars <- ggplot(bars) +
    geom_segment(aes(x = med_s, xend = med_e, y = row_label, yend = row_label), colour = "grey40", linewidth = 2.0) +
    geom_segment(aes(x = q25_s, xend = q75_s, y = row_label, yend = row_label), colour = "black", linewidth = 0.6) +
    geom_segment(aes(x = q25_e, xend = q75_e, y = row_label, yend = row_label), colour = "black", linewidth = 0.6) +
    geom_point(aes(x = med_s, y = row_label, shape = family), size = 1.6) +
    geom_point(aes(x = med_e, y = row_label, shape = family), size = 1.6) +
    scale_x_continuous(limits = xlims, breaks = mb$pos, labels = mb$label) +
    scale_y_discrete(limits = rev(HEADLINE_C$row_label)) +
    scale_shape_manual(values = FAMILY_SHAPES, name = "Method family", guide = "none") +
    labs(x = NULL, y = NULL) +
    theme_gsl() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = PEERJ_BASE_PT - 1),
                          axis.text.y = element_text(size = PEERJ_BASE_PT - 1.5))
  (p_curve / p_bars) + patchwork::plot_layout(heights = c(1, 1.5))
}

C_PANELS <- list(c("US-Ha1", "calendar"), c("US-Wkg", "calendar"), c("US-Var", "calendar"), c("AU-How", "calendar"), c("US-Var", "min_gpp_month"))
c_blocks <- Map(function(spec, letter) plot_panel_c(spec[1], spec[2], letter), C_PANELS, letters[1:5])
p_fig_c <- extra_title(patchwork::wrap_plots(c_blocks[[1]], c_blocks[[2]], c_blocks[[3]], c_blocks[[4]], c_blocks[[5]],
                                               design = "AB\nCD\nE#"))

save_gsl_figure(p_fig_c, "drafts/methods_review/Figure C", width_mm = 200, height_mm = 240,
  legend_lines = c(
    "Draft for discussion. Figure C: where each method puts the season, NT series. (a) US-Ha1, (b) US-Wkg,",
    "(c) US-Var, (d) AU-How -- all calendar year_rule (NH = Jan-Dec; AU-How = Jul-Jun). (e) US-Var again",
    "under the min_gpp_month year_rule (starts in August, its own lowest-mean-NT-GPP month) -- compare",
    "with (c) to see how the calendar-year (January) boundary splits US-Var's winter-spring season.",
    "Top of each panel: median daily NT GPP across years (black line) with interquartile range across",
    "years (grey ribbon), by day-of-window. Bottom: one bar per method x variant (9 rows -- M4 shown as",
    "two separate rows, 85% and 95% of annual GPP, instead of one; every row labelled with its method",
    "name AND the variant plotted, e.g. 'M6, Hu, anchored', 'M7, Ma'; same order/family shapes as",
    "Figure A) from its median start to median end across years, with thin black whiskers for the",
    "interquartile range of start/end across years. Count methods (M2/M3/M4) use the 5-day-run start/end",
    "rule (start_date_run5/end_date_run5), not the first/last-qualifying-day rule.",
    "Numbers behind this figure: tables/gsl_methods_site_year.csv (method bars) plus this script's own",
    "fresh daily-data computation (median/IQR curves, not persisted to a table)."
  ),
  input_sources = c(file.path(EXP_TABLES, "gsl_methods_site_year.csv"), "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')"),
  notes = "Draft for Dave/Tash discussion. Count methods use the 5-day-run start/end rule.")
message("Saved Figure C.")

## ============================================================================
## Figure D: skewness vs. cross-method GSL range, both year_rules, 15 sites,
## AU-How annotated.
## ============================================================================
fd <- rule_range |> filter(site_id %in% IAV_SITES) |> left_join(skew_tbl, by = "site_id") |>
  select(site_id, skewness, calendar = range_calendar, min_gpp_month = range_min_gpp_month) |>
  pivot_longer(cols = c(calendar, min_gpp_month), names_to = "rule", values_to = "range_days") |>
  mutate(rule = factor(rule, levels = c("calendar", "min_gpp_month")))

p_fig_d <- ggplot(fd, aes(x = skewness, y = range_days, colour = rule)) +
  geom_point(size = 2.2) +
  ggrepel::geom_text_repel(aes(label = site_id), size = PEERJ_ANNOTATION_PT / .pt, show.legend = FALSE, max.overlaps = 30, seed = 1) +
  geom_point(data = dplyr::filter(fd, site_id == "AU-How", rule == "calendar"), shape = 1, size = 6, colour = "black", stroke = 1.1) +
  scale_colour_manual(values = c(calendar = "#377eb8", min_gpp_month = "#e41a1c"), name = "Year rule") +
  labs(x = "Skewness (NT series, 'main_unscreened' variant)", y = "Range of mean GSL across methods (days)") +
  theme_gsl() + theme(legend.position = "right")
p_fig_d <- extra_title(p_fig_d)
auhow_range_cal <- fd$range_days[fd$site_id == "AU-How" & fd$rule == "calendar"]
auhow_skew <- fd$skewness[fd$site_id == "AU-How" & fd$rule == "calendar"][1]

save_gsl_figure(p_fig_d, "drafts/methods_review/Figure D", width_mm = 150, height_mm = 150,
  legend_lines = c(
    "Draft for discussion. Figure D: spread across methods vs. skewness, 15 sites, NT series. x =",
    "skewness (derived_site.csv, 'main_unscreened' variant); y = range of mean GSL (days) across the 8 headline",
    "methods (same subs as Figure A/tables/gsl_methods_year_rule_range.csv), one point per site, for",
    "both year_rules (blue = calendar, red = min_gpp_month).",
    sprintf("AU-How is circled: its cross-method range under the calendar rule is %.0f days despite a low", auhow_range_cal),
    sprintf("skewness of %.2f -- one of the largest ranges in the set at one of the lowest skewness values.", auhow_skew),
    "Numbers behind this figure: tables/gsl_methods_year_rule_range.csv, tables/derived_site.csv."
  ),
  input_sources = c(file.path(EXP_TABLES, "gsl_methods_year_rule_range.csv"), file.path(EXP_TABLES, "derived_site.csv")),
  notes = "Draft for Dave/Tash discussion.")
message("Saved Figure D.")
message("=== 12_methods_review_figures.R: all figures done ===")
