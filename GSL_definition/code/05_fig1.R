## 05_fig1.R — GSL figure rebuild, Figure 1: mechanism at two representative
## sites. Main run only (GPP_NT_VUT_REF, negative GPP zero-floored, d80
## screening, GSL_STRICT_GT). Reads only Stage 1/2 tables and data;
## recomputes nothing 03_derive.R already produced, except the mean-GSL
## curve beyond 50% of P99 (fine_grid.rds stops at 50%; the Stage 3
## revisions asked for panel b to extend to >=100%).

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
daily_extract$doy <- lubridate::yday(daily_extract$TIMESTAMP)

## ---- Figure 1 sites (Stage 3 revision) --------------------------------------
## Low third: nearest the low-third median skewness, never AU-ASM (unchanged
## from the original rule -- the revision only changes the high-third rule).
## High third: nearest the HIGH-THIRD median skewness (computed over the
## full high third, as before), but restricted to woody-grass mosaic and
## grassland sites only, because the figure's takeaway concerns drylands.
## Never AU-ASM.
pick_low_site <- function() {
  sub <- derived_site |> filter(skewness_third == "low", site_id != "AU-ASM")
  med <- median(sub$skewness)
  sub$site_id[which.min(abs(sub$skewness - med))]
}
pick_high_dryland_site <- function() {
  high_all <- derived_site |> filter(skewness_third == "high", site_id != "AU-ASM")
  med <- median(high_all$skewness) # median of the FULL high third, not just the dryland subset
  dryland <- high_all |> filter(group %in% c("woody-grass mosaic", "grassland"))
  dryland$site_id[which.min(abs(dryland$skewness - med))]
}
site_low <- pick_low_site()
site_high <- pick_high_dryland_site()
fig1_sites <- c(low = site_low, high = site_high)
message("Figure 1 sites: low third = ", site_low, " (skewness ",
        round(derived_site$skewness[derived_site$site_id == site_low], 4), "), high third = ",
        site_high, " (skewness ", round(derived_site$skewness[derived_site$site_id == site_high], 4),
        ", group = ", derived_site$group[derived_site$site_id == site_high], ")")

## ---- Per-site data for panel a (daily GPP, zero-floored main run) --------
gpp_col <- GSL_GPP_VARIANTS[["main"]]
site_daily <- function(sid) {
  years <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
  d <- daily_extract |> filter(site_id == sid, year %in% years)
  d$gpp <- pmax(d[[gpp_col]], 0)
  d
}

## Median-GSL year: the year (among the site's own screened years) whose
## GSL(Z17) is closest to the median of this site's own GSL(Z17) values.
median_gsl_year <- function(sid) {
  sy <- derived_site_year |> filter(site_id == sid)
  med <- median(sy$gsl_z17, na.rm = TRUE)
  sy$year[which.min(abs(sy$gsl_z17 - med))]
}

panel_a_data <- function(sid) {
  d <- site_daily(sid)
  myr <- median_gsl_year(sid)
  d |> mutate(is_median_year = year == myr, median_year = myr)
}

## ---- Panel a: daily GPP by day-of-year, median-GSL year dark, other years
## faint grey. Dashed lines at 5%/10%(Z17)/20% of P99; T95 in a distinct
## (dotted) linetype so it reads as a different kind of reference. Each row
## names its site and skewness (identifying which data is shown -- not an
## explanatory caption, so not covered by R/nature_format.R's "no in-panel
## text" rule, which is about describing what the lines/thresholds mean). ----
make_panel_a <- function(sid, label) {
  d <- panel_a_data(sid)
  P99 <- derived_site$P99[derived_site$site_id == sid]
  t95 <- derived_site$t95_pct[derived_site$site_id == sid]
  skew <- derived_site$skewness[derived_site$site_id == sid]
  fixed_lines <- tibble(gpp = c(5, 10, 20) / 100 * P99)
  t95_line <- tibble(gpp = t95 / 100 * P99)
  ggplot(d, aes(x = doy, y = gpp, group = year)) +
    geom_rect(data = tibble(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.10 * P99),
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "grey90", alpha = 0.5) +
    ## "Other years" darker/thicker than before (grey75/0.25pt), but still
    ## clearly behind the highlighted median-GSL year (PEERJ_LWD_DATA, black).
    geom_line(data = d |> filter(!is_median_year), colour = "grey55", linewidth = nature_lwd(PEERJ_LWD_OTHERYEARS)) +
    geom_line(data = d |> filter(is_median_year), colour = "black", linewidth = nature_lwd(PEERJ_LWD_DATA)) +
    geom_hline(data = fixed_lines, aes(yintercept = gpp), colour = "grey40",
               linewidth = nature_lwd(PEERJ_LWD_REF), linetype = "dashed") +
    geom_hline(data = t95_line, aes(yintercept = gpp), colour = "firebrick",
               linewidth = nature_lwd(PEERJ_LWD_REF), linetype = "dotted") +
    annotate("text", x = 366, y = Inf, label = sprintf("%s (skew = %.2f)", sid, skew),
              hjust = 1, vjust = 1.4, size = PEERJ_ANNOTATION_PT / .pt, family = NATURE_FONT, colour = "black") +
    scale_x_continuous(limits = c(1, 366), breaks = c(1, 91, 182, 274, 366),
                        labels = c("Jan", "Apr", "Jul", "Oct", "Jan")) +
    labs(x = NULL, y = expression("GPP"["NT"]~"(gC m"^-2~"d"^-1*")"),
         title = NULL) +
    theme_gsl() +
    if (nzchar(label)) gsl_panel_letter(label) else NULL
}

p1a <- make_panel_a(site_low, "a")
p1a2 <- make_panel_a(site_high, "")

## ---- Panel b: shared densities of daily GPP (x = % of P99, extended to
## 100%), same thresholds as vertical lines, mean GSL vs threshold on a
## right-hand axis. fine_grid.rds only covers 0.1-50% of P99 (03_derive.R's
## fine grid), so the mean-GSL curve from 50-100% is a new computation here,
## for these 2 sites only, reusing each site's own screened daily data (not
## re-deriving P99/skewness/etc., which are reused from derived_site.csv). --
ext_grid_pct <- seq(0.1, 100, by = 0.5)
site_mean_gsl_curve <- function(sid) {
  years <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
  P99 <- derived_site$P99[derived_site$site_id == sid]
  d <- daily_extract |> filter(site_id == sid, year %in% years)
  d$gpp <- pmax(d[[gpp_col]], 0)
  gpp_by_year <- split(d$gpp, as.character(d$year))
  gsl_mat <- sapply(gpp_by_year, function(gy) {
    gy <- gy[!is.na(gy)]
    if (length(gy) == 0) return(rep(NA_real_, length(ext_grid_pct)))
    T_abs <- ext_grid_pct / 100 * P99
    length(gy) - findInterval(T_abs, sort(gy))
  })
  tibble(site_id = sid, pct_of_P99 = ext_grid_pct, mean_gsl = rowMeans(gsl_mat, na.rm = TRUE))
}
mean_gsl_curves <- bind_rows(lapply(fig1_sites, site_mean_gsl_curve))

hist_data <- bind_rows(lapply(fig1_sites, function(sid) {
  d <- site_daily(sid)
  P99 <- derived_site$P99[derived_site$site_id == sid]
  tibble(site_id = sid, pct_of_P99 = 100 * d$gpp / P99) # uncapped -- the view, not the data, is limited
}))

## Scale mean_gsl onto the density axis for the right-hand axis
hist_max_density <- max(density(hist_data$pct_of_P99[hist_data$site_id == fig1_sites[1]], na.rm = TRUE)$y,
                         density(hist_data$pct_of_P99[hist_data$site_id == fig1_sites[2]], na.rm = TRUE)$y)
gsl_max <- max(mean_gsl_curves$mean_gsl, na.rm = TRUE)
sec_scale <- hist_max_density / gsl_max

## ---- Bracket: change in mean GSL between 5% and 10% of P99 (framework
## spec), drawn on panel b as a small labelled bracket on the right-axis
## curve, in addition to .legend.txt. ----
delta_gsl_5_10 <- sapply(fig1_sites, function(sid) {
  mg <- mean_gsl_curves |> filter(site_id == sid)
  g5 <- mg$mean_gsl[which.min(abs(mg$pct_of_P99 - 5))]
  g10 <- mg$mean_gsl[which.min(abs(mg$pct_of_P99 - 10))]
  g10 - g5
})
message("Change in mean GSL, 5% -> 10% of P99: ", site_low, " = ", round(delta_gsl_5_10[1], 2),
        " days; ", site_high, " = ", round(delta_gsl_5_10[2], 2), " days")

bracket_data <- bind_rows(lapply(fig1_sites, function(sid) {
  mg <- mean_gsl_curves |> filter(site_id == sid)
  g5 <- mg$mean_gsl[which.min(abs(mg$pct_of_P99 - 5))]
  g10 <- mg$mean_gsl[which.min(abs(mg$pct_of_P99 - 10))]
  tibble(site_id = sid, x = 7.5, y0 = g5 * sec_scale, y1 = g10 * sec_scale,
         label = sprintf("%+.0f d", g10 - g5))
}))

## Density/mean-GSL lines labelled and coloured by site name (not "low
## third"/"high third"), using the shared low/high colours from the
## skewness-third palette (fig_helpers.R) -- the same two colours Figure 4
## panel a uses for these same two sites.
site_colours <- setNames(c(GSL_SKEW_THIRD_COLORS[["low"]], GSL_SKEW_THIRD_COLORS[["high"]]), unname(fig1_sites))
p1b <- ggplot() +
  geom_density(data = hist_data, aes(x = pct_of_P99, colour = site_id, linetype = "Daily GPP density"),
               linewidth = nature_lwd(PEERJ_LWD_DATA), fill = NA, key_glyph = "path") +
  geom_line(data = mean_gsl_curves,
            aes(x = pct_of_P99, y = mean_gsl * sec_scale, colour = site_id, linetype = "Mean GSL"),
            linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  geom_vline(xintercept = c(5, 10, 20), colour = "grey40", linewidth = nature_lwd(PEERJ_LWD_REF), linetype = "dashed") +
  geom_segment(data = bracket_data, aes(x = x, xend = x, y = y0, yend = y1), colour = "black",
               linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_text(data = bracket_data, aes(x = x, y = (y0 + y1) / 2, label = label), hjust = -0.15,
            size = PEERJ_ANNOTATION_PT / .pt, family = NATURE_FONT, colour = "black") +
  scale_colour_manual(values = site_colours, breaks = unname(fig1_sites), name = NULL) +
  scale_linetype_manual(values = c("Daily GPP density" = "solid", "Mean GSL" = "dotted"), name = NULL) +
  scale_y_continuous(name = "Density",
                      sec.axis = sec_axis(~ . / sec_scale, name = "Mean GSL (days above threshold)")) +
  scale_x_continuous(name = "Threshold (% of P99)") +
  coord_cartesian(xlim = c(0, 100)) +
  theme_gsl() +
  gsl_panel_letter("b") +
  theme(legend.position = c(0.8, 0.82), legend.key.width = unit(5, "mm"),
        legend.background = element_blank(), legend.box = "vertical")

## ---- Assemble and save ------------------------------------------------------
fig1 <- (p1a / p1a2) | p1b
fig1 <- fig1 + plot_layout(widths = c(1, 1.2))

legend_lines <- c(
  "Figure 1. Mechanism at two representative sites (main run: GPP_NT_VUT_REF, negative GPP",
  "set to 0, year screening d80, GSL counts GPP > T).",
  sprintf("Low-third site: %s, skewness = %.4f.", site_low, derived_site$skewness[derived_site$site_id == site_low]),
  sprintf(
    "High-third site: %s, skewness = %.4f, group = %s. Chosen nearest the high third's own ",
    site_high, derived_site$skewness[derived_site$site_id == site_high], derived_site$group[derived_site$site_id == site_high]
  ),
  "median skewness, restricted to woody-grass mosaic and grassland sites (drylands), per the",
  "Stage 3 revision -- see docs/methods_memo.md. AU-ASM was excluded from consideration throughout.",
  paste0(
    "Panel a: daily GPP_NT_VUT_REF (negative values set to 0) for each site's median-GSL ",
    "year (dark line; GSL = days with GPP > threshold at 10% of P99), other screened years in ",
    "grey, site name and skewness labelled on each row. Dashed lines at 5%, 10% (Z17) and 20% ",
    "of P99; dotted red line at each site's own T95 (a different linetype, since T95 varies by site)."
  ),
  sprintf("%s T95 = %.1f%% of P99.", site_low, derived_site$t95_pct[derived_site$site_id == site_low]),
  sprintf("%s T95 = %.1f%% of P99.", site_high, derived_site$t95_pct[derived_site$site_id == site_high]),
  paste0(
    "Panel b: overlaid daily-GPP density (solid, left axis, % of P99, view limited to 0-100% -- ",
    "the underlying density estimate itself is not truncated) and mean GSL vs. threshold ",
    "(dotted, right axis) for both sites, each line labelled and coloured by site name (not ",
    "'low third'/'high third'); low-third site in the skewness-third palette's low colour, ",
    "high-third site in its high colour (fig_helpers.R::GSL_SKEW_THIRD_COLORS, same two colours ",
    "Figure 4 panel a uses for these sites). Vertical bracket at 7.5% of P99 shows the change in ",
    "mean GSL between 5% and 10% of P99 for each site (also stated below)."
  ),
  sprintf(
    "Change in mean GSL from 5%% to 10%% of P99 (negative = fewer days qualify at the higher threshold): %s = %.2f days, %s = %.2f days.",
    site_low, delta_gsl_5_10[1], site_high, delta_gsl_5_10[2]
  )
)

save_gsl_figure(
  fig1, "Figure 1", width_mm = PEERJ_FIG_WIDTH_MM, height_mm = 170,
  legend_lines = legend_lines,
  input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"),
                     file.path(GSL_ROOT, "tables", "derived_site_year.csv"),
                     file.path(GSL_ROOT, "data", "daily_extract.rds"),
                     file.path(GSL_ROOT, "data", "fine_grid.rds")),
  notes = sprintf(
    "Figure 1 sites: %s (low third, skewness %.4f), %s (high third, skewness %.4f, group %s, dryland-restricted per Stage 3 revision). AU-ASM excluded per framework.",
    site_low, derived_site$skewness[derived_site$site_id == site_low],
    site_high, derived_site$skewness[derived_site$site_id == site_high], derived_site$group[derived_site$site_id == site_high]
  )
)

message("--- 05_fig1.R: done ---")
