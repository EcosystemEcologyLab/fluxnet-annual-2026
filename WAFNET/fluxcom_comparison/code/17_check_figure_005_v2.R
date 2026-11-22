## 17_check_figure_005_v2.R — Stage 3b step 4: regenerate the one check figure from the v2
## package files (package_ossenatou_<date>_v2/). Same spec as Stage 3's 13_check_figure_005.R:
## monthly X-BASE (0.05deg, centre cell) against tower, one panel each for NEE/GPP/LE, sites by
## colour, training sites marked, 1:1 line, labelled DRAFT -- a sanity check for the package, not
## a paper figure. FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1).
##
## Points explicitly at the v2 package directory (not "most recently modified", since v1 and v2
## package directories now coexist on disk).
##
## Output (into the v2 package folder, so it travels with the data):
##   package_ossenatou_<date>_v2/check_figure_xbase_005_vs_tower_DRAFT.png (+ .legend.txt)

source("WAFNET/fluxcom_comparison/code/00_config.R")
suppressPackageStartupMessages({ library(ggplot2); library(dplyr) })

STAGE3_SITES <- c("GH-Ank", "BJ-Nhu", "BJ-Bfg", "SN-Dhr", "ML-Kem", "SN-Nkr")

PKG_DATE <- format(Sys.Date(), "%Y-%m-%d")
PKG_DIR <- file.path(FLUXCOM_ROOT, paste0("package_ossenatou_", PKG_DATE, "_v2"))
if (!dir.exists(PKG_DIR)) stop("[17_check_figure_005_v2] Package directory not found: ", PKG_DIR, " -- run 16_package_ossenatou_v2.R first.")
message("[17_check_figure_005_v2] Using package: ", PKG_DIR)

tower_df <- utils::read.csv(file.path(PKG_DIR, "tower_monthly.csv"), stringsAsFactors = FALSE)
xbase_df <- utils::read.csv(file.path(PKG_DIR, "xbase_005_monthly.csv"), stringsAsFactors = FALSE)
training_df <- utils::read.csv(file.path(FLUXCOM_ROOT, "tables", "fluxcom_x_base_training_sites_stage2.csv"), stringsAsFactors = FALSE)

m <- merge(tower_df, xbase_df, by = c("site_id", "year", "month"))

rows <- list()
if (any(m$NEE_pass_qc, na.rm = TRUE)) {
  sub <- m[m$NEE_pass_qc, ]
  rows[[length(rows) + 1]] <- data.frame(site_id = sub$site_id, flux = "NEE", tower_value = sub$NEE_VUT_REF, xbase_value = sub$NEE_gC_m2_d1)
}
if (any(m$GPP_NT_pass_qc, na.rm = TRUE)) {
  sub <- m[m$GPP_NT_pass_qc, ]
  rows[[length(rows) + 1]] <- data.frame(site_id = sub$site_id, flux = "GPP", tower_value = sub$GPP_NT_VUT_REF, xbase_value = sub$GPP_gC_m2_d1)
}
if (any(m$LE_pass_qc, na.rm = TRUE)) {
  sub <- m[m$LE_pass_qc, ]
  rows[[length(rows) + 1]] <- data.frame(site_id = sub$site_id, flux = "LE", tower_value = sub$LE_F_MDS, xbase_value = sub$LE_W_m2)
}
plot_df <- do.call(rbind, rows)
plot_df <- plot_df[stats::complete.cases(plot_df), ]
plot_df$flux <- factor(plot_df$flux, levels = c("NEE", "GPP", "LE"))
plot_df$is_training <- training_df$in_fluxcom_x_base_training_set[match(plot_df$site_id, training_df$site_id)]
plot_df$shape_code <- ifelse(plot_df$is_training, 17, 16)

site_colors <- STAGE2_SITE_COLORS[STAGE3_SITES]

p <- ggplot(plot_df, aes(x = tower_value, y = xbase_value, color = site_id, shape = factor(shape_code))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.7, size = 1.8) +
  scale_color_manual(values = site_colors, name = "Site") +
  scale_shape_manual(values = c("16" = 16, "17" = 17),
                      labels = c("16" = "not in training set", "17" = "X-BASE training site"),
                      name = "Training status") +
  facet_wrap(~ flux, scales = "free", labeller = as_labeller(c(NEE = "NEE (gC m-2 d-1)", GPP = "GPP (gC m-2 d-1)", LE = "LE (W m-2)"))) +
  labs(
    title = "DRAFT -- FLUXCOM-X-BASE (0.05deg) vs. tower, monthly, 2001-2021",
    subtitle = "QC-passing months only; GPP uses nighttime-partitioned tower data; six WAF-Net sites (v2, traceable package)",
    x = "Tower (QC-passing monthly value)", y = "FLUXCOM-X-BASE (0.05deg grid cell)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

fig_path <- file.path(PKG_DIR, "check_figure_xbase_005_vs_tower_DRAFT.png")
ggsave(fig_path, p, width = 11, height = 4.5, dpi = 300, bg = "white")
message("[17_check_figure_005_v2] Wrote ", fig_path, " -- open and check before committing.")

write_caption(fig_path, c(
  "DRAFT check figure (v2, traceable package). FLUXCOM-X-BASE (0.05 degree monthly grid, ",
  "2001-2021) plotted against tower monthly values for the six WAF-Net sites, one panel per flux ",
  "(NEE, GPP, LE; gC m-2 d-1 for NEE/GPP, W m-2 for LE), restricted to QC-passing (>= 0.50) tower ",
  "months. GPP uses the tower's nighttime-partitioned series (GPP_NT), matching X-BASE's own ",
  "nighttime-only GPP training target. Dashed line is 1:1. Triangles mark the two sites (GH-Ank, ",
  "SN-Dhr) in FLUXCOM-X-BASE's own training set; circles are the other four sites. This is a ",
  "sanity check for the data package, not a paper figure."
))
