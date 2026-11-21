## 13_check_figure_005.R — Stage 3 step 4: one check figure only.
##
## FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1). Per the Stage 3
## prompt, this is a single sanity-check figure for the 0.05deg package, not a new figure set:
## monthly X-BASE (0.05deg) against tower, one panel each for NEE, GPP, LE (not RECO or ET),
## sites by colour (shared palette, 00_config.R), training sites marked, 1:1 line, titled
## DRAFT. Same marker-shape/palette convention as 09_figures_draft.R's Stage 2 scatter.
##
## Output (into the package folder itself, so it travels with the data; also left here as the
## canonical copy for docs/report_back_<date>.md to reference):
##   package_ossenatou_<date>/check_figure_xbase_005_vs_tower_DRAFT.png

source("WAFNET/fluxcom_comparison/code/00_config.R")
suppressPackageStartupMessages({ library(ggplot2); library(dplyr) })

STAGE3_SITES <- c("GH-Ank", "BJ-Nhu", "BJ-Bfg", "SN-Dhr", "ML-Kem", "SN-Nkr")

pkg_dirs <- list.dirs(FLUXCOM_ROOT, recursive = FALSE)
pkg_dirs <- pkg_dirs[grepl("^package_ossenatou_", basename(pkg_dirs))]
if (length(pkg_dirs) == 0) stop("[13_check_figure_005] No package_ossenatou_<date> folder found -- run 12_package_ossenatou.R first.")
PKG_DIR <- pkg_dirs[order(file.info(pkg_dirs)$mtime, decreasing = TRUE)][1]
message("[13_check_figure_005] Using package: ", PKG_DIR)

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
plot_df$shape_code <- ifelse(plot_df$is_training, 17, 16)   # triangle (training) / circle

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
    subtitle = "QC-passing months only; GPP uses nighttime-partitioned tower data; six WAF-Net sites",
    x = "Tower (QC-passing monthly value)", y = "FLUXCOM-X-BASE (0.05deg grid cell)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

fig_path <- file.path(PKG_DIR, "check_figure_xbase_005_vs_tower_DRAFT.png")
ggsave(fig_path, p, width = 11, height = 4.5, dpi = 300, bg = "white")
message("[13_check_figure_005] Wrote ", fig_path, " -- open and check before committing.")

write_caption(fig_path, c(
  "DRAFT check figure. FLUXCOM-X-BASE (0.05 degree monthly grid, 2001-2021) plotted against ",
  "tower monthly values for the six WAF-Net sites, one panel per flux (NEE, GPP, LE; gC m-2 d-1 ",
  "for NEE/GPP, W m-2 for LE), restricted to QC-passing (>= 0.50) tower months. GPP uses the ",
  "tower's nighttime-partitioned series (GPP_NT), matching X-BASE's own nighttime-only GPP ",
  "training target. Dashed line is 1:1. Triangles mark the two sites (GH-Ank, SN-Dhr) confirmed ",
  "in FLUXCOM-X-BASE's own 294-site training set; circles are the other four sites. This is a ",
  "sanity check for the data package, not a paper figure."
))
