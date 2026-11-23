## 08_v2_check_figure.R -- v2 check figure (step 4): same axes as v1 (annual
## GPP_NT vs annual P_F, calendar years), but now ALL kept sites' calendar
## years (not filtered to measured-precip-only -- v2's whole point is to NOT
## drop points for Russ), coloured by IGBP, shaped by the per-year
## precip_source column (measured / ERA-filled / unknown), labelled DRAFT.

source("RSCOTT_AGU26/code/00_config.R")

PKG_DIR_V2 <- file.path(RSCOTT_ROOT, "package_rscott_2026-11-23_v2")

candidates <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv"), show_col_types = FALSE)
site_years_kept <- readr::read_csv(file.path(PKG_DIR_V2, "site_years.csv"), show_col_types = FALSE)

plot_df <- site_years_kept |>
  dplyr::filter(time_step == "calendar") |>
  dplyr::left_join(dplyr::select(candidates, site_id, igbp), by = "site_id") |>
  dplyr::filter(!is.na(gpp_nt_gC_m2_yr), !is.na(p_f_mm_yr)) |>
  dplyr::mutate(precip_source_label = dplyr::coalesce(precip_source, "unknown"))

message("Check-figure points (all kept sites, calendar years): ", nrow(plot_df))
message("  Sites represented: ", dplyr::n_distinct(plot_df$site_id))
message("  By precip_source_label: "); print(table(plot_df$precip_source_label))

p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = p_f_mm_yr, y = gpp_nt_gC_m2_yr, color = igbp, shape = precip_source_label)) +
  ggplot2::geom_point(size = 2.4, alpha = 0.85, stroke = 1) +
  ggplot2::scale_shape_manual(values = c("measured" = 16, "ERA-filled" = 17, "unknown" = 4)) +
  ggplot2::labs(
    title = "DRAFT -- annual GPP_NT vs. annual precipitation, v2 (widened) selection",
    subtitle = "All 54 kept sites, calendar years (both measured and ERA5-filled precipitation shown)",
    x = "Annual precipitation, P_F (mm yr-1)",
    y = expression("Annual GPP"["NT"]*" (g C "*m^-2*" "*yr^-1*")"),
    color = "IGBP class",
    shape = "Precipitation source (per year)"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.position  = "bottom",
    legend.box = "vertical",
    plot.title = ggplot2::element_text(face = "bold", color = "firebrick")
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 15, size = 3)))

fig_path <- file.path(RSCOTT_ROOT, "figures", "check_gpp_vs_precip_v2_DRAFT.png")
ggplot2::ggsave(fig_path, p, width = 8, height = 6.5, dpi = 200, bg = "white")
message("Saved figure: ", fig_path)

write_caption(fig_path, c(
  "DRAFT check figure (not for publication) -- v2 widened selection. Annual gross primary",
  "productivity partitioned by the nighttime method (GPP_NT, gC m-2 yr-1, VUT where available else",
  "CUT, never mixed within a site) plotted against annual precipitation (P_F, mm yr-1), for every",
  "calendar year of every one of the 54 v2-kept dryland (IGBP OSH/CSH/GRA/SAV/WSA) sites with both",
  "values present (no GPP/ET QC filter and no measured-precipitation filter applied to these points --",
  "v2 shows all data rather than dropping it). Colour = IGBP class. Shape = this year's precipitation",
  "source (filled circle = measured, <=10% ERA-filled; triangle = ERA-filled, mostly or entirely",
  "reanalysis; x = share could not be computed for that row). Produced for internal data-quality",
  "checking only."
))
write_meta(fig_path, input_sources = file.path(PKG_DIR_V2, "site_years.csv"), notes = "DRAFT check figure, v2 widened selection, not for publication; see .legend.txt")

fs::file_copy(fig_path, file.path(PKG_DIR_V2, "check_gpp_vs_precip_v2_DRAFT.png"), overwrite = TRUE)
fs::file_copy(paste0(tools::file_path_sans_ext(fig_path), ".legend.txt"),
              file.path(PKG_DIR_V2, "check_gpp_vs_precip_v2_DRAFT.legend.txt"), overwrite = TRUE)

message("08_v2_check_figure.R complete.")
