## 05_fig1.R — GSL figure rebuild, Figure 1: mechanism at two representative
## sites. Main run only (GPP_NT_VUT_REF, negative GPP zero-floored, d80
## screening, GSL_STRICT_GT). Reads only Stage 1/2 tables and data;
## recomputes nothing 03_derive.R already produced.

source("GSL_definition/code/00_config.R")
source("GSL_definition/code/fig_helpers.R")

suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr); library(lubridate) })

derived_site <- read_csv(file.path(GSL_ROOT, "tables", "derived_site.csv"), show_col_types = FALSE) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main")
derived_site_year <- read_csv(file.path(GSL_ROOT, "tables", "derived_site_year.csv"), show_col_types = FALSE) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main")
daily_extract <- readRDS(file.path(GSL_ROOT, "data", "daily_extract.rds"))
daily_extract$TIMESTAMP <- as.Date(as.character(daily_extract$TIMESTAMP))
daily_extract$year <- lubridate::year(daily_extract$TIMESTAMP)
daily_extract$doy <- lubridate::yday(daily_extract$TIMESTAMP)

## ---- Figure 1 sites: nearest the median skewness of the low and high
## thirds, never AU-ASM (docs/framework.md, Sites / Figure 1). ----
pick_tercile_site <- function(third) {
  sub <- derived_site |> filter(skewness_third == third, site_id != "AU-ASM")
  med <- median(sub$skewness)
  sub$site_id[which.min(abs(sub$skewness - med))]
}
site_low <- pick_tercile_site("low")
site_high <- pick_tercile_site("high")
fig1_sites <- c(low = site_low, high = site_high)
message("Figure 1 sites: low third = ", site_low, " (skewness ",
        round(derived_site$skewness[derived_site$site_id == site_low], 4), "), high third = ",
        site_high, " (skewness ", round(derived_site$skewness[derived_site$site_id == site_high], 4), ")")

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
## faint grey. Horizontal lines at 5%/10%(Z17)/20% of P99 and the site's T95. --
## No in-panel explanatory text (R/nature_format.R: "no caption or
## explanatory text drawn inside the figure itself -- put it in the
## .legend.txt file instead") -- the three fixed thresholds and each site's
## own T95 are dashed reference lines only, explained in the .legend.txt.
make_panel_a <- function(sid, label) {
  d <- panel_a_data(sid)
  P99 <- derived_site$P99[derived_site$site_id == sid]
  t95 <- derived_site$t95_pct[derived_site$site_id == sid]
  hlines <- tibble(gpp = c(5, 10, 20, t95) / 100 * P99)
  ggplot(d, aes(x = doy, y = gpp, group = year)) +
    geom_rect(data = tibble(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.10 * P99),
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "grey90", alpha = 0.5) +
    geom_line(data = d |> filter(!is_median_year), colour = "grey75", linewidth = nature_lwd(0.25)) +
    geom_line(data = d |> filter(is_median_year), colour = "black", linewidth = nature_lwd(0.6)) +
    geom_hline(data = hlines, aes(yintercept = gpp), colour = "grey40",
               linewidth = nature_lwd(0.25), linetype = "dashed") +
    scale_x_continuous(limits = c(1, 366), breaks = c(1, 91, 182, 274, 366),
                        labels = c("Jan", "Apr", "Jul", "Oct", "Jan")) +
    labs(x = NULL, y = expression("GPP"["NT"]~"(gC m"^-2~"d"^-1*")"),
         title = NULL) +
    theme_gsl() +
    if (nzchar(label)) panel_letter(label) else NULL
}

p1a <- make_panel_a(site_low, "a")
p1a2 <- make_panel_a(site_high, "")

## ---- Panel b: shared histograms of daily GPP (x = % of P99), overlaid
## outlines, same thresholds as vertical lines, mean GSL vs threshold on a
## right-hand axis. ----
fine_grid <- readRDS(file.path(GSL_ROOT, "data", "fine_grid.rds")) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main", site_id %in% fig1_sites)

hist_data <- bind_rows(lapply(fig1_sites, function(sid) {
  d <- site_daily(sid)
  P99 <- derived_site$P99[derived_site$site_id == sid]
  tibble(site_id = sid, pct_of_P99 = pmin(100 * d$gpp / P99, 60)) # cap display at 60% for readability
}))
site_role <- setNames(c("low third", "high third"), fig1_sites)
hist_data$role <- site_role[hist_data$site_id]
fine_grid$role <- site_role[fine_grid$site_id]

## Scale mean_gsl onto the histogram's density axis for the right-hand axis
hist_max_density <- max(density(hist_data$pct_of_P99[hist_data$site_id == fig1_sites[1]], na.rm = TRUE)$y,
                         density(hist_data$pct_of_P99[hist_data$site_id == fig1_sites[2]], na.rm = TRUE)$y)
gsl_max <- max(fine_grid$mean_gsl[fine_grid$pct_of_P99 <= 60], na.rm = TRUE)
sec_scale <- hist_max_density / gsl_max

role_colours <- setNames(c("#1b7837", "#762a83"), c("low third", "high third"))
p1b <- ggplot() +
  geom_density(data = hist_data, aes(x = pct_of_P99, colour = role, linetype = "Daily GPP density"),
               linewidth = nature_lwd(0.5), fill = NA, key_glyph = "path") +
  geom_line(data = fine_grid |> filter(pct_of_P99 <= 60),
            aes(x = pct_of_P99, y = mean_gsl * sec_scale, colour = role, linetype = "Mean GSL"),
            linewidth = nature_lwd(0.4)) +
  geom_vline(xintercept = c(5, 10, 20), colour = "grey40", linewidth = nature_lwd(0.25), linetype = "dashed") +
  scale_colour_manual(values = role_colours, name = NULL) +
  scale_linetype_manual(values = c("Daily GPP density" = "solid", "Mean GSL" = "dotted"), name = NULL) +
  scale_y_continuous(name = "Density of daily GPP (% of P99)",
                      sec.axis = sec_axis(~ . / sec_scale, name = "Mean GSL (days above threshold)")) +
  scale_x_continuous(name = "Threshold (% of P99)", limits = c(0, 60)) +
  theme_gsl() +
  panel_letter("b") +
  theme(legend.position = c(0.78, 0.82), legend.key.width = unit(5, "mm"),
        legend.background = element_blank(), legend.box = "vertical")

## ---- Annotation: change in mean GSL between 5% and 10% of P99 ------------
delta_gsl_5_10 <- sapply(fig1_sites, function(sid) {
  fg <- fine_grid |> filter(site_id == sid)
  g5 <- fg$mean_gsl[which.min(abs(fg$pct_of_P99 - 5))]
  g10 <- fg$mean_gsl[which.min(abs(fg$pct_of_P99 - 10))]
  g10 - g5
})
message("Change in mean GSL, 5% -> 10% of P99: ", site_low, " = ", round(delta_gsl_5_10[1], 2),
        " days; ", site_high, " = ", round(delta_gsl_5_10[2], 2), " days")

## ---- Assemble and save ------------------------------------------------------
fig1 <- (p1a / p1a2) | p1b
fig1 <- fig1 + plot_layout(widths = c(1, 1.2))

legend_lines <- c(
  "Figure 1. Mechanism at two representative sites (main run: GPP_NT_VUT_REF, negative GPP",
  "set to 0, year screening d80, GSL counts GPP > T).",
  sprintf("Low-third site: %s, skewness = %.4f.", site_low, derived_site$skewness[derived_site$site_id == site_low]),
  sprintf("High-third site: %s, skewness = %.4f.", site_high, derived_site$skewness[derived_site$site_id == site_high]),
  "AU-ASM was excluded from consideration, per the framework.",
  paste0(
    "Panel a: daily GPP_NT_VUT_REF (negative values set to 0) for each site's median-GSL ",
    "year (dark line; GSL = days with GPP > threshold at 10% of P99), other screened years in ",
    "grey. Dashed lines at 5%, 10% (Z17) and 20% of P99, and at each site's own T95."
  ),
  sprintf("%s T95 = %.1f%% of P99.", site_low, derived_site$t95_pct[derived_site$site_id == site_low]),
  sprintf("%s T95 = %.1f%% of P99.", site_high, derived_site$t95_pct[derived_site$site_id == site_high]),
  paste0(
    "Panel b: overlaid daily-GPP density (solid, left axis, % of P99) and mean GSL vs. threshold ",
    "(dotted, right axis) for both sites, capped at 60% of P99 for display."
  ),
  sprintf(
    "Change in mean GSL from 5%% to 10%% of P99 (negative = fewer days qualify at the higher threshold): %s = %.2f days, %s = %.2f days.",
    site_low, delta_gsl_5_10[1], site_high, delta_gsl_5_10[2]
  )
)

save_gsl_figure(
  fig1, "fig_01_mechanism", width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 150,
  legend_lines = legend_lines,
  input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"),
                     file.path(GSL_ROOT, "tables", "derived_site_year.csv"),
                     file.path(GSL_ROOT, "data", "daily_extract.rds"),
                     file.path(GSL_ROOT, "data", "fine_grid.rds")),
  notes = sprintf(
    "Figure 1 sites: %s (low third, skewness %.4f), %s (high third, skewness %.4f). AU-ASM excluded per framework.",
    site_low, derived_site$skewness[derived_site$site_id == site_low],
    site_high, derived_site$skewness[derived_site$site_id == site_high]
  )
)

message("--- 05_fig1.R: done ---")
