## 10_climatology.R — mean seasonal cycle (climatology) per site, Stage 2 fixes.
##
## FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1).
## New figure set added in the 2026-11-03 fixes
## (logs/prompts/2026-11-03_wafnet_fluxcom_x_stage2_fixes.md), not part of the
## original Stage 2 request. Uses the SAME monthly values already computed by
## 08_extract_statistics.R (tables/xbase_monthly_values.csv) -- i.e. the same
## QC-passing tower months within 2001-2021 used for the Stage 2 statistics,
## so the tower and X-BASE series cover exactly the same site-months by
## construction (they are two columns of the same filtered row set, not
## independently filtered).
##
## One figure per site (the five agreed sites + SN-Nkr, flagged "pending
## Ossenatou" in its title; BJ-Db1 has zero overlapping site-months with
## X-BASE at all -- see docs/report_back_2026-11-03.md section 5 -- and is
## not included here, same as every other Stage 2 table/figure). One panel
## per flux (NEE, GPP, RECO [X-BASE derived, stated in the panel title], LE).
## GH-Ank has no NT-partitioned GPP/RECO tower data at all (section 4 of the
## report) -- its GPP and RECO panels are left out, with the reason stated in
## the figure's caption, not silently dropped.
##
## Per panel: tower and X-BASE monthly climatological means (shared site
## palette for tower, one fixed XBASE_SERIES_COLOR for X-BASE, both from
## 00_config.R). Months with >= 3 contributing years get a mean +/- 1 SD
## ribbon; months with < 3 years show the individual year values as points
## instead (no ribbon drawn there -- not enough years to characterise a
## spread). The number of contributing years is printed along the bottom of
## every panel.
##
## Output (git-tracked):
##   figures/draft_climatology_<site>.png  (one per site, titled DRAFT)
##   tables/xbase_climatology.csv          (site x flux x month: n, means, SDs)

source("WAFNET/fluxcom_comparison/code/00_config.R")
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

monthly_df <- utils::read.csv(file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_values.csv"), stringsAsFactors = FALSE)

flux_order <- c("NEE", "GPP", "RECO", "LE")
flux_units <- c(NEE = "gC m-2 d-1", GPP = "gC m-2 d-1", RECO = "gC m-2 d-1 (X-BASE derived: GPP + NEE)", LE = "W m-2")
flux_title <- c(NEE = "NEE", GPP = "GPP", RECO = "RECO (derived)", LE = "LE")

sites_to_plot <- c(STAGE2_SITES, "SN-Nkr")  # five agreed + SN-Nkr (pending, has overlap); BJ-Db1 has none

## ---- Monthly climatology summary: n years, mean +/- SD per site x flux x month ----
clim_summary <- monthly_df %>%
  group_by(site_id, site_status, flux, month) %>%
  summarise(
    n_years = dplyr::n(),
    tower_mean = mean(tower_value), tower_sd = if (dplyr::n() >= 3) stats::sd(tower_value) else NA_real_,
    xbase_mean = mean(xbase_value), xbase_sd = if (dplyr::n() >= 3) stats::sd(xbase_value) else NA_real_,
    .groups = "drop"
  )

write.csv(clim_summary, file.path(FLUXCOM_ROOT, "tables", "xbase_climatology.csv"), row.names = FALSE)
write_meta(
  file.path(FLUXCOM_ROOT, "tables", "xbase_climatology.csv"),
  input_sources = file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_values.csv"),
  notes = "Per site x flux x calendar month: n_years = number of QC-passing site-years contributing to that month within 2001-2021 (same site-months as the Stage 2 statistics); tower_mean/xbase_mean = mean across those years; tower_sd/xbase_sd = SD across those years, NA when n_years < 3 (not enough years to characterise a spread -- see figures/draft_climatology_<site>.png, which plots individual years instead of a ribbon for those months). Bias = X-BASE minus tower; NEE negative for uptake; RECO is X-BASE-derived (GPP + NEE)."
)

## Individual-year points for months with < 3 contributing years (shown
## instead of a ribbon at those months).
thin_months <- monthly_df %>%
  dplyr::inner_join(clim_summary %>% dplyr::filter(n_years < 3) %>% dplyr::select(site_id, flux, month),
                     by = c("site_id", "flux", "month"))

## ---- Per-site "years used" / IGBP / training-site title text -------------
site_title_info <- function(site_id) {
  yrs <- sort(unique(monthly_df$year[monthly_df$site_id == site_id]))
  years_str <- if (length(yrs) == 0) "no overlapping years" else if (max(yrs) - min(yrs) + 1 == length(yrs)) {
    paste0(min(yrs), "–", max(yrs))
  } else {
    paste(yrs, collapse = ", ")
  }
  igbp <- site_igbp_for(site_id)
  training <- if (site_id %in% c("GH-Ank", "SN-Dhr")) ", training site" else ""
  pending <- if (site_id %in% STAGE2_PENDING_SITES) " (pending Ossenatou)" else ""
  paste0("DRAFT — ", site_id, pending, " — ", igbp, " — ", years_str, training)
}

## ---- One panel (one flux, one site) ---------------------------------------
one_flux_panel <- function(site_id, fx, site_color) {
  sub <- clim_summary[clim_summary$site_id == site_id & clim_summary$flux == fx, ]
  if (nrow(sub) == 0) return(NULL)
  pts <- thin_months[thin_months$site_id == site_id & thin_months$flux == fx, ]

  y_range <- range(c(sub$tower_mean - ifelse(is.na(sub$tower_sd), 0, sub$tower_sd),
                      sub$tower_mean + ifelse(is.na(sub$tower_sd), 0, sub$tower_sd),
                      sub$xbase_mean - ifelse(is.na(sub$xbase_sd), 0, sub$xbase_sd),
                      sub$xbase_mean + ifelse(is.na(sub$xbase_sd), 0, sub$xbase_sd),
                      pts$tower_value, pts$xbase_value), na.rm = TRUE)
  y_pad <- diff(y_range) * 0.12
  n_label_y <- y_range[1] - y_pad

  ggplot(sub, aes(x = month)) +
    geom_ribbon(aes(ymin = tower_mean - tower_sd, ymax = tower_mean + tower_sd), fill = site_color, alpha = 0.18, na.rm = TRUE) +
    geom_ribbon(aes(ymin = xbase_mean - xbase_sd, ymax = xbase_mean + xbase_sd), fill = XBASE_SERIES_COLOR, alpha = 0.18, na.rm = TRUE) +
    { if (nrow(pts) > 0) geom_point(data = pts, aes(x = month, y = tower_value), color = site_color, shape = 1, size = 1.6, alpha = 0.8) } +
    { if (nrow(pts) > 0) geom_point(data = pts, aes(x = month, y = xbase_value), color = XBASE_SERIES_COLOR, shape = 1, size = 1.6, alpha = 0.8) } +
    geom_line(aes(y = tower_mean, color = "Tower"), linewidth = 0.9) +
    geom_point(aes(y = tower_mean, color = "Tower"), size = 1.6) +
    geom_line(aes(y = xbase_mean, color = "X-BASE"), linewidth = 0.9) +
    geom_point(aes(y = xbase_mean, color = "X-BASE"), size = 1.6) +
    geom_text(aes(y = n_label_y, label = n_years), size = 2.3, color = "grey40") +
    scale_color_manual(values = c("Tower" = site_color, "X-BASE" = XBASE_SERIES_COLOR), name = NULL) +
    scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(1, 12), expand = expansion(mult = 0.03)) +
    coord_cartesian(ylim = c(n_label_y - y_pad * 0.5, y_range[2] + y_pad * 0.3)) +
    labs(title = paste0(flux_title[fx], " [", flux_units[fx], "]"), x = NULL, y = NULL) +
    theme_bw(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
}

## ---- One figure per site ---------------------------------------------------
excluded_note <- function(site_id) {
  if (site_id == "GH-Ank") {
    "GPP and RECO panels omitted for GH-Ank: it has no nighttime-partitioned (GPP_NT/RECO_NT) tower data at all (0 of 48 monthly rows), the partition X-BASE's own GPP training uses (see docs/report_back_2026-11-03.md section 4) -- not a QC exclusion."
  } else {
    NULL
  }
}

for (site_id in sites_to_plot) {
  site_color <- STAGE2_SITE_COLORS[[site_id]]
  panel_list <- stats::setNames(lapply(flux_order, one_flux_panel, site_id = site_id, site_color = site_color), flux_order)
  panel_list <- Filter(Negate(is.null), panel_list)
  panels <- unname(panel_list)
  fluxes_shown <- names(panel_list)
  if (length(panels) == 0) {
    message("[WAFNET/fluxcom_comparison] No X-BASE-overlapping months for ", site_id, " -- skipping climatology figure.")
    next
  }

  note <- excluded_note(site_id)
  caption <- paste0(
    "Shaded band = tower/X-BASE mean ± 1 SD across years (months with ≥ 3 years); ",
    "open circles = individual years (months with < 3 years). Number along the bottom of each panel = years behind that month.",
    if (!is.null(note)) paste0(" ", note) else ""
  )
  caption <- stringr::str_wrap(caption, width = 140)

  p <- (patchwork::wrap_plots(panels, ncol = 2) + patchwork::plot_layout(guides = "collect")) &
    theme(legend.position = "bottom")
  p <- p + patchwork::plot_annotation(
    title = site_title_info(site_id),
    subtitle = "Mean seasonal cycle, tower vs. FLUXCOM-X-BASE, QC-passing site-months within 2001-2021 (same months used in the Stage 2 statistics)",
    caption = caption,
    theme = theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 10), size = 10),
      plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 10))
    )
  )

  out_file <- file.path(FLUXCOM_ROOT, "figures", paste0("draft_climatology_", site_id, ".png"))
  fig_height <- if (length(panels) > 2) 9 else 5
  ggsave(out_file, p, width = 10, height = fig_height, dpi = 200, bg = "white")
  cat("[WAFNET/fluxcom_comparison] Wrote", out_file, "\n")

  site_status <- if (site_id %in% STAGE2_PENDING_SITES) "pending Ossenatou" else "agreed"
  n_by_flux_site <- table(monthly_df$flux[monthly_df$site_id == site_id])
  write_caption(out_file, c(
    paste0(
      "Figure (DRAFT). Mean seasonal cycle at ", site_id, " (", site_igbp_for(site_id), "), tower vs. ",
      "FLUXCOM-X-BASE, one panel per flux (", paste(fluxes_shown, collapse = ", "), "). ",
      if (!is.null(excluded_note(site_id))) excluded_note(site_id) else NULL,
      " X-axis: calendar month (Jan-Dec). Line and points: mean value across the contributing years ",
      "for that month (tower in ", site_id, "'s colour -- fixed palette, consistent across every ",
      "Stage 2 figure; X-BASE in a single fixed colour used for every site). Shaded band: mean +/- 1 ",
      "SD across years, shown only for months with >= 3 contributing years; open circles: the ",
      "individual years' values instead, shown for months with fewer than 3. The number printed ",
      "along the bottom of each panel is the count of contributing years for that month."
    ),
    paste0(
      "Years used: ", paste(sort(unique(monthly_df$year[monthly_df$site_id == site_id])), collapse = ", "),
      ". Site-months per flux: ", paste(paste0(names(n_by_flux_site), "=", as.integer(n_by_flux_site)), collapse = ", "), "."
    ),
    "Units: NEE, GPP, RECO in gC m-2 d-1; LE in W m-2.",
    STAGE2_CAPTION_DATA,
    STAGE2_CAPTION_CONVENTION,
    STAGE2_CAPTION_SITES
  ))
  write_meta(
    out_file,
    input_sources = c(file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_values.csv"),
                       file.path(FLUXCOM_ROOT, "tables", "xbase_climatology.csv")),
    notes = paste0(
      "Site status: ", site_status, ". Fluxes shown: ", paste(fluxes_shown, collapse = ", "), ". ",
      if (!is.null(excluded_note(site_id))) excluded_note(site_id) else "",
      " Ribbon vs. individual-year-points threshold: n_years >= 3 per month (same threshold as the ",
      "annual secondary statistics in 08_extract_statistics.R)."
    )
  )
}

cat("\n[WAFNET/fluxcom_comparison] Wrote tables/xbase_climatology.csv and figures/draft_climatology_<site>.png\n")
