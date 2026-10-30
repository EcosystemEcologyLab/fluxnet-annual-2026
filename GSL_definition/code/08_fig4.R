## 08_fig4.R — GSL figure rebuild, Figure 4: the fraction-of-annual-GPP
## method and its own sensitivity. Main run only.
##
## Panel b (GSL vs. target fraction) and panel c (common-unit sensitivity at
## Z17 and T95 vs. skewness) reuse 03_derive.R's existing outputs directly
## (fraction_sweep.rds, derived_site.csv -- T95 is BY DEFINITION the
## threshold where mean F first drops to <=0.95, so "common-unit sensitivity
## at 0.95" = t95_common_unit_sensitivity, already computed). Panel a needs
## per-year F(y,T)/GSL(y,T) pairs for the two Figure 1 sites, which
## 03_derive.R did not persist (only the pooled mean) -- computed fresh here
## from daily_extract.rds, reusing each site's P99 from derived_site.csv.

source("GSL_definition/code/00_config.R")
source("GSL_definition/code/fig_helpers.R")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr); library(lubridate) })

derived_site <- read_csv(file.path(GSL_ROOT, "tables", "derived_site.csv"), show_col_types = FALSE) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main")
fraction_sweep <- readRDS(file.path(GSL_ROOT, "data", "fraction_sweep.rds")) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main")
daily_extract <- readRDS(file.path(GSL_ROOT, "data", "daily_extract.rds"))
daily_extract$TIMESTAMP <- as.Date(as.character(daily_extract$TIMESTAMP))
daily_extract$year <- lubridate::year(daily_extract$TIMESTAMP)
gpp_col <- GSL_GPP_VARIANTS[["main"]]

## ---- Figure 1 sites (same selection rule as 05_fig1.R -- not persisted
## anywhere, so reapplied here; the skewness VALUES themselves are reused,
## not recomputed). ----
pick_tercile_site <- function(third) {
  sub <- derived_site |> filter(skewness_third == third, site_id != "AU-ASM")
  med <- median(sub$skewness)
  sub$site_id[which.min(abs(sub$skewness - med))]
}
fig1_sites <- c(low = pick_tercile_site("low"), high = pick_tercile_site("high"))

## ---- Panel a: per-year F(y,T) vs. GSL(y,T) for the two Figure 1 sites ----
threshold_grid_pct <- seq(0.1, 60, by = 0.5)

site_f_gsl_curve <- function(sid) {
  years <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
  P99 <- derived_site$P99[derived_site$site_id == sid]
  d <- daily_extract |> filter(site_id == sid, year %in% years)
  d$gpp <- pmax(d[[gpp_col]], 0)
  gpp_by_year <- split(d$gpp, as.character(d$year))

  per_year <- bind_rows(lapply(names(gpp_by_year), function(yr) {
    gy <- gpp_by_year[[yr]]; gy <- gy[!is.na(gy)]
    n <- length(gy); sorted <- sort(gy); csum <- c(0, cumsum(sorted))
    total_sum <- sum(gy)
    T_abs <- threshold_grid_pct / 100 * P99
    idx <- findInterval(T_abs, sorted)
    gsl <- n - idx
    f <- (total_sum - csum[idx + 1]) / total_sum
    tibble(site_id = sid, year = as.integer(yr), pct_of_P99 = threshold_grid_pct, gsl = gsl, f = f)
  }))
  mean_curve <- per_year |> group_by(site_id, pct_of_P99) |>
    summarise(gsl = mean(gsl), f = mean(f), .groups = "drop") |> mutate(year = NA_integer_)
  list(per_year = per_year, mean_curve = mean_curve)
}

curves <- lapply(fig1_sites, site_f_gsl_curve)
per_year_all <- bind_rows(lapply(curves, `[[`, "per_year"))
mean_all <- bind_rows(lapply(curves, `[[`, "mean_curve"))
site_role <- setNames(c("low third", "high third"), fig1_sites)
per_year_all$role <- site_role[per_year_all$site_id]
mean_all$role <- site_role[mean_all$site_id]

z17_marker <- derived_site |> filter(site_id %in% fig1_sites) |>
  select(site_id, gsl = z17_mean_gsl, f = z17_mean_f) |>
  mutate(role = site_role[site_id])

p4a <- ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.90, ymax = 0.99, fill = "grey90", alpha = 0.6) +
  geom_hline(yintercept = 0.95, colour = "grey50", linewidth = nature_lwd(0.25), linetype = "dashed") +
  geom_line(data = per_year_all, aes(x = gsl, y = f, group = interaction(site_id, year), colour = role),
            alpha = 0.3, linewidth = nature_lwd(0.25)) +
  geom_line(data = mean_all, aes(x = gsl, y = f, colour = role), linewidth = nature_lwd(0.6)) +
  geom_point(data = z17_marker, aes(x = gsl, y = f, colour = role), shape = 4, size = 1.5,
             stroke = nature_lwd(0.6)) +
  scale_colour_manual(values = setNames(c("#1b7837", "#762a83"), c("low third", "high third")), name = NULL) +
  labs(x = "Days above threshold", y = "Included fraction of annual GPP") +
  theme_gsl() +
  theme(legend.position = c(0.82, 0.18)) +
  panel_letter("a")

## ---- Panel b: all 62 sites, GSL vs. target fraction, coloured by skewness,
## marker at each site's Z17 implied fraction. ----
fraction_sweep_main <- fraction_sweep |>
  left_join(derived_site |> select(site_id, skewness), by = "site_id")
z17_pts <- derived_site |> select(site_id, skewness, z17_mean_f, z17_mean_gsl)

p4b <- ggplot(fraction_sweep_main, aes(x = target_fraction, y = gsl, group = site_id, colour = skewness)) +
  geom_line(linewidth = nature_lwd(0.25)) +
  geom_point(data = z17_pts, aes(x = z17_mean_f, y = z17_mean_gsl, colour = skewness),
             inherit.aes = FALSE, shape = 4, size = 1, stroke = nature_lwd(0.5)) +
  scale_color_gsl_skew() +
  labs(x = "Target fraction of annual GPP", y = "GSL (days above threshold)") +
  theme_gsl() +
  theme(legend.key.height = unit(3, "mm"), legend.key.width = unit(2.5, "mm")) +
  panel_letter("b")

## ---- Panel c: common-unit sensitivity at Z17 and at T95 (=0.95 by
## definition) vs. skewness. ----
panel_c_data <- derived_site |>
  select(site_id, skewness, z17_common_unit_sensitivity, t95_common_unit_sensitivity) |>
  pivot_longer(c(z17_common_unit_sensitivity, t95_common_unit_sensitivity),
               names_to = "at", values_to = "sensitivity") |>
  mutate(at = recode(at, z17_common_unit_sensitivity = "Z17 (10% of P99)",
                      t95_common_unit_sensitivity = "T95 (F=0.95)"))

median_c <- panel_c_data |> group_by(at) |> summarise(median_sens = median(sensitivity, na.rm = TRUE), .groups = "drop")

p4c <- ggplot(panel_c_data, aes(x = skewness, y = sensitivity, colour = at)) +
  geom_point(size = 0.8) +
  scale_colour_manual(values = c("Z17 (10% of P99)" = "black", "T95 (F=0.95)" = "firebrick"), name = NULL) +
  labs(x = "Skewness", y = "Common-unit sensitivity\n(days per 1% change in F)") +
  theme_gsl() +
  theme(legend.position = c(0.75, 0.9)) +
  panel_letter("c")

## ---- Assemble --------------------------------------------------------------
fig4 <- (p4a | p4b | p4c) + plot_layout(guides = "keep")

legend_lines <- c(
  "Figure 4. The fraction-of-annual-GPP method and its own sensitivity (main run:",
  "GPP_NT_VUT_REF, negative GPP set to 0, year screening d80, GSL counts GPP > T).",
  sprintf("Panel a sites (same as Figure 1): low third = %s, high third = %s.", fig1_sites[1], fig1_sites[2]),
  paste0(
    "Panel a: included fraction of annual GPP vs. days above threshold, each year faint, ",
    "multi-year mean bold, for the two Figure 1 sites. Grey band 0.90-0.99, dashed line at 0.95, ",
    "x marker at each site's Z17 (10% of P99)."
  ),
  paste0(
    "Panel b: all 62 sites, mean GSL vs. target fraction (0.80-0.99), coloured by skewness; x ",
    "marker at each site's own Z17-implied fraction."
  ),
  "Panel c: common-unit sensitivity (days per 1% change in included fraction) at Z17 and at",
  "T95 (the threshold where mean F first drops to <=0.95, i.e. F=0.95 by definition), vs. skewness.",
  sprintf("Median common-unit sensitivity: Z17 = %.3f, T95 = %.3f (days per 1%% change in F).",
          median_c$median_sens[median_c$at == "Z17 (10% of P99)"],
          median_c$median_sens[median_c$at == "T95 (F=0.95)"])
)

save_gsl_figure(
  fig4, "fig_04_fraction_method", width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 90,
  legend_lines = legend_lines,
  input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"),
                     file.path(GSL_ROOT, "data", "fraction_sweep.rds"),
                     file.path(GSL_ROOT, "data", "daily_extract.rds")),
  notes = sprintf("Median common-unit sensitivity: Z17=%.3f, T95=%.3f.",
                   median_c$median_sens[median_c$at == "Z17 (10% of P99)"],
                   median_c$median_sens[median_c$at == "T95 (F=0.95)"])
)

message("--- 08_fig4.R: done ---")
