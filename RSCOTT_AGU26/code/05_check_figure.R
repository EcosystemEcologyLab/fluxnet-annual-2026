## 05_check_figure.R — step 6's single check figure: annual GPP_NT vs annual
## measured P (calendar years with measured precipitation only), coloured by
## IGBP class, Southern Hemisphere sites marked, labelled DRAFT.

source("RSCOTT_AGU26/code/00_config.R")

PKG_DATE <- "2026-11-23"
PKG_DIR <- file.path(RSCOTT_ROOT, paste0("package_rscott_", PKG_DATE))

candidates <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv"), show_col_types = FALSE)
sites_tbl <- readr::read_csv(file.path(PKG_DIR, "sites.csv"), show_col_types = FALSE)
site_years_kept <- readr::read_csv(file.path(PKG_DIR, "site_years.csv"), show_col_types = FALSE)

plot_df <- site_years_kept |>
  dplyr::filter(time_step == "calendar", pass_precip_measured) |>
  dplyr::left_join(dplyr::select(candidates, site_id, igbp, hemisphere), by = "site_id") |>
  dplyr::filter(!is.na(gpp_nt_gC_m2_yr), !is.na(p_f_mm_yr))

message("Check-figure points (calendar years, measured precip only): ", nrow(plot_df))
message("  Sites represented: ", dplyr::n_distinct(plot_df$site_id))
message("  Southern Hemisphere points: ", sum(plot_df$hemisphere == "S"))

plot_df <- plot_df |> dplyr::mutate(hemisphere_label = ifelse(hemisphere == "S", "Southern", "Northern"))

p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = p_f_mm_yr, y = gpp_nt_gC_m2_yr, color = igbp, shape = hemisphere_label)) +
  ggplot2::geom_point(size = 2.4, alpha = 0.85, stroke = 1) +
  ggplot2::scale_shape_manual(values = c("Northern" = 16, "Southern" = 17)) +
  ggplot2::labs(
    title = "DRAFT -- annual GPP_NT vs. annual measured precipitation",
    subtitle = "Calendar years with measured precipitation only\n(ERA-filled share <= 10%)",
    x = "Annual measured precipitation, P_F (mm yr-1)",
    y = expression("Annual GPP"["NT"]*" (g C "*m^-2*" "*yr^-1*")"),
    color = "IGBP class",
    shape = "Hemisphere"
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

fig_path <- file.path(RSCOTT_ROOT, "figures", "check_gpp_vs_precip_DRAFT.png")
ggplot2::ggsave(fig_path, p, width = 7.5, height = 6.5, dpi = 200, bg = "white")
message("Saved figure: ", fig_path)

write_caption(fig_path, c(
  "DRAFT check figure (not for publication). Annual gross primary productivity partitioned by the",
  "nighttime method (GPP_NT, gC m-2 yr-1, VUT where available else CUT, never mixed within a site)",
  "plotted against annual measured precipitation (P_F, mm yr-1), for calendar years where measured",
  "precipitation passes this project's 10% ERA-filled-share rule (no additional GPP/ET QC filter is",
  "applied to these points), from the 43 kept dryland (IGBP OSH/CSH/GRA/SAV/WSA) sites.",
  "Colour = IGBP class. Triangles = Southern Hemisphere sites (calendar years split the austral",
  "summer wet season there; see README.md). Produced for internal data-quality checking only."
))
write_meta(fig_path, input_sources = file.path(PKG_DIR, "site_years.csv"), notes = "DRAFT check figure, not for publication; see .legend.txt")

## Copy into the package itself (step 6 asks for the figure inside the package).
fs::file_copy(fig_path, file.path(PKG_DIR, "check_gpp_vs_precip_DRAFT.png"), overwrite = TRUE)
fs::file_copy(paste0(tools::file_path_sans_ext(fig_path), ".legend.txt"),
              file.path(PKG_DIR, "check_gpp_vs_precip_DRAFT.legend.txt"), overwrite = TRUE)

message("05_check_figure.R complete.")
