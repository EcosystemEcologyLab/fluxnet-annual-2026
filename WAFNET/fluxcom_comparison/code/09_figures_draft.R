## 09_figures_draft.R — Stage 2 step 7: draft comparison figures.
##
## FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1).
## Every figure is titled "DRAFT" per the Stage 2 prompt. Training sites
## (GH-Ank, SN-Dhr -- confirmed in 07_training_site_check.R) are marked with a
## distinct symbol (triangle) from the other agreed sites (circle); the two
## "pending Ossenatou" sites (BJ-Db1, SN-Nkr) use a square and are called out
## in each caption -- kept visually and categorically separate from the five
## agreed sites, per the Stage 1/Ossenatou decision. Bias = X-BASE minus
## tower in every figure (stated in titles/captions). RECO is labelled
## "derived" (GPP + NEE) in every panel that shows it. H is never shown --
## X-BASE has no sensible-heat variable (see docs/report_back_2026-11-03.md).
##
## Per CLAUDE.md "Figures" convention: bg = "white" on every ggsave()/png().
##
## 2026-11-03 fixes (logs/prompts/2026-11-03_wafnet_fluxcom_x_stage2_fixes.md):
## one shared site colour palette (STAGE2_SITE_COLORS, 00_config.R) used here
## AND in 10_climatology.R; the site map's BJ-Nhu/BJ-Bfg and SN-Dhr/SN-Nkr
## symbols, which sat almost on top of each other, are now nudged apart with a
## leader line back to the true coordinate, and the map's empty margin is
## trimmed to the sites' actual extent; the Taylor diagrams' "tower" label,
## previously clipped by the axis (it sits at y slightly below 0, outside the
## default plot clip region), is now drawn with par(xpd = NA).
##
## Output (git-tracked):
##   figures/draft_monthly_scatter.png
##   figures/draft_site_map_bias.png
##   figures/draft_taylor_diagram_{NEE,GPP,RECO,LE}.png

source("WAFNET/fluxcom_comparison/code/00_config.R")
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

monthly_df <- utils::read.csv(file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_values.csv"), stringsAsFactors = FALSE)

site_coords <- do.call(rbind, lapply(STAGE2_ALL_SITES, function(site_id) {
  co <- site_coords_for(site_id)
  data.frame(site_id = site_id, lat = co[["lat"]], lon = co[["lon"]])
}))

marker_shape <- function(site_id, site_status) {
  ifelse(site_status == "pending Ossenatou", 15,            # square
         ifelse(site_id %in% c("GH-Ank", "SN-Dhr"), 17, 16)) # triangle (training) / circle
}

flux_units <- c(NEE = "gC m-2 d-1", GPP = "gC m-2 d-1", RECO = "gC m-2 d-1 (derived)", LE = "W m-2")
flux_order <- c("NEE", "GPP", "RECO", "LE")

monthly_df$flux <- factor(monthly_df$flux, levels = flux_order)
monthly_df$shape_code <- marker_shape(monthly_df$site_id, monthly_df$site_status)
monthly_df$site_label <- ifelse(monthly_df$site_status == "pending Ossenatou",
                                 paste0(monthly_df$site_id, " (pending Ossenatou)"), monthly_df$site_id)

# Shared site palette (00_config.R), keyed by the "<site> (pending Ossenatou)"
# labels used for the legend here.
site_label_for <- function(site_id) ifelse(site_id %in% STAGE2_PENDING_SITES,
                                            paste0(site_id, " (pending Ossenatou)"), site_id)
site_label_colors <- stats::setNames(STAGE2_SITE_COLORS, site_label_for(names(STAGE2_SITE_COLORS)))

## ---- (a) Monthly scatter: X-BASE vs tower, one panel per flux -------------
p_scatter <- ggplot(monthly_df, aes(x = tower_value, y = xbase_value, color = site_label, shape = factor(shape_code))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.7, size = 1.8) +
  scale_color_manual(values = site_label_colors, name = "Site") +
  scale_shape_manual(
    values = c("15" = 15, "16" = 16, "17" = 17),
    labels = c("15" = "pending Ossenatou", "16" = "agreed", "17" = "agreed, training site"),
    name = "Site status"
  ) +
  facet_wrap(~flux, scales = "free", labeller = labeller(flux = function(f) paste0(f, " [", flux_units[f], "]"))) +
  labs(
    title = "DRAFT — FLUXCOM-X-BASE vs. tower, monthly values",
    subtitle = "1:1 line shown. RECO is derived (GPP + NEE). Bias (X-BASE minus tower) in tables/xbase_monthly_stats.csv.",
    x = "Tower (FLUXNET)", y = "FLUXCOM-X-BASE", color = "Site"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave(file.path(FLUXCOM_ROOT, "figures", "draft_monthly_scatter.png"), p_scatter,
       width = 10, height = 8, dpi = 200, bg = "white")

## ---- (b) Site map, symbols sized/coloured by monthly bias, per flux ------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

bias_df <- monthly_df %>%
  group_by(site_id, site_status, flux) %>%
  summarise(bias = mean(bias_product_minus_tower), .groups = "drop") %>%
  left_join(site_coords, by = "site_id")
# Fill-capable shapes (21 circle / 22 square / 24 triangle) so the BORDER
# colour can carry site identity (shared palette) while the FILL colour
# still carries bias -- two encodings on one symbol, both requested in the
# 2026-11-03 fixes (one shared site palette; bias colour/size per panel).
bias_df$shape_code <- ifelse(bias_df$site_status == "pending Ossenatou", 22,
                              ifelse(bias_df$site_id %in% c("GH-Ank", "SN-Dhr"), 24, 21))
bias_df$site_label <- ifelse(bias_df$site_status == "pending Ossenatou",
                              paste0(bias_df$site_id, " (pending Ossenatou)"), bias_df$site_id)

# BJ-Nhu/BJ-Bfg (0.12 deg apart) and SN-Dhr/SN-Nkr (~1.0-1.4 deg apart, but
# still overlapping at this map's scale) sit close enough that their symbols
# overlapped -- checked visually 2026-11-03. Nudge those four sites' PLOTTED
# position apart by a small fixed offset and draw a thin leader line (site
# colour) back to the true coordinate, which stays in lon_true/lat_true.
nudge <- list("BJ-Nhu" = c(dlon = -0.55, dlat = -0.45), "BJ-Bfg" = c(dlon = 0.55, dlat = 0.45),
              "SN-Dhr" = c(dlon = 0.55, dlat = 0.45), "SN-Nkr" = c(dlon = -0.55, dlat = -0.45))
bias_df$lon_true <- bias_df$lon
bias_df$lat_true <- bias_df$lat
for (s in names(nudge)) {
  rows <- bias_df$site_id == s
  bias_df$lon[rows] <- bias_df$lon[rows] + nudge[[s]]["dlon"]
  bias_df$lat[rows] <- bias_df$lat[rows] + nudge[[s]]["dlat"]
}
nudged_df <- bias_df[bias_df$site_id %in% names(nudge), ]

# Trim the map extent to the (nudged) sites' own footprint + a small margin,
# instead of a fixed West-Africa-wide box that left large empty margins above
# and below the panels -- checked visually and fixed 2026-11-03.
MAP_XLIM <- range(bias_df$lon) + c(-2, 2)
MAP_YLIM <- range(bias_df$lat) + c(-2.5, 2.5)

## Each flux gets its OWN bias colour/size scale (NEE/GPP/RECO are O(1-5)
## gC m-2 d-1 while LE is O(10-40) W m-2 -- a single shared scale across
## panels made the carbon panels' points nearly invisible; checked visually
## and fixed 2026-11-03, see docs/report_back_2026-11-03.md).
one_flux_map <- function(fx) {
  sub <- bias_df[bias_df$flux == fx, ]
  nsub <- nudged_df[nudged_df$flux == fx, ]
  ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.2) +
    geom_segment(data = nsub, aes(x = lon_true, y = lat_true, xend = lon, yend = lat, color = site_id),
                 linewidth = 0.4, linetype = "22", show.legend = FALSE) +
    geom_point(data = sub, aes(x = lon, y = lat, size = abs(bias), fill = bias, shape = factor(shape_code), color = site_id),
               alpha = 0.95, stroke = 1.3) +
    ggrepel::geom_text_repel(data = sub, aes(x = lon, y = lat, label = site_id, color = site_id),
                              size = 2.9, min.segment.length = 0, fontface = "bold", show.legend = FALSE,
                              bg.color = "white", bg.r = 0.12) +
    scale_fill_gradient2(low = "#2166AC", mid = "grey85", high = "#B2182B", midpoint = 0, name = "Bias\n(X-BASE − tower)") +
    scale_color_manual(values = STAGE2_SITE_COLORS, guide = "none") +
    scale_shape_manual(values = c("21" = 21, "22" = 22, "24" = 24), guide = "none") +
    scale_size_continuous(range = c(4, 12), guide = "none") +  # size duplicates fill; one legend (fill) is enough and avoids legend-column overlap
    coord_sf(xlim = MAP_XLIM, ylim = MAP_YLIM, expand = TRUE) +
    labs(title = paste0(fx, " [", flux_units[fx], "]"), x = NULL, y = NULL) +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11))
}

p_map <- patchwork::wrap_plots(lapply(flux_order, one_flux_map), ncol = 2) +
  patchwork::plot_annotation(
    title = "DRAFT — Mean monthly bias by site (FLUXCOM-X-BASE minus tower)",
    subtitle = "Triangle = training site; circle = agreed, non-training; square = pending Ossenatou. Symbol border = site (shared palette); fill = bias. RECO is derived.",
    caption = "Each panel has its own fill/size scale — fluxes are on different magnitude scales (do not compare bubble sizes across panels). BJ-Nhu/BJ-Bfg and SN-Dhr/SN-Nkr are nudged apart with a dashed leader line to their true location.",
    theme = theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 30)),
      plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
    )
  )

ggsave(file.path(FLUXCOM_ROOT, "figures", "draft_site_map_bias.png"), p_map,
       width = 13, height = 12, dpi = 200, bg = "white")

## ---- (c) Taylor diagram per flux, monthly scale, multi-site --------------
## No Taylor-diagram package is in the renv lockfile (openair/plotrix); per
## CLAUDE.md's "no new dependencies without discussion", this is a small
## self-contained base-R implementation (standard formulation: angle =
## acos(correlation), radius = sd(model)/sd(observed), reference point at
## (1, 0)) rather than adding one.
taylor_diagram <- function(tower, xbase, site_labels, shape_codes, colors, title) {
  max_r <- 1.6

  plot(NA, xlim = c(0, max_r), ylim = c(0, max_r), asp = 1, axes = FALSE,
       xlab = "", ylab = "", main = title)
  # SD-ratio circles and correlation-angle spokes (standard Taylor-diagram grid)
  theta_seq <- seq(0, pi / 2, length.out = 200)
  for (rad in seq(0.5, max_r, by = 0.5)) lines(rad * cos(theta_seq), rad * sin(theta_seq), col = "grey85")
  for (r in c(0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99)) {
    theta <- acos(r)
    lines(c(0, max_r * cos(theta)), c(0, max_r * sin(theta)), col = "grey80", lty = 3)
    text(max_r * 1.02 * cos(theta), max_r * 1.02 * sin(theta), labels = r, cex = 0.6, col = "grey50")
  }
  axis(1, at = seq(0, max_r, 0.5)); axis(2, at = seq(0, max_r, 0.5))
  points(1, 0, pch = 8, col = "black", cex = 1.3)  # reference (tower)
  # The "tower" label sits just below the x-axis (y slightly < 0), outside
  # the default plot clip region, which clipped it off -- checked visually
  # and fixed 2026-11-03 by allowing this one annotation to draw outside the
  # plot region (par(xpd = NA)), restored immediately after.
  op <- par(xpd = NA)
  text(1, -0.09, "tower", cex = 0.7)
  par(op)
}

flux_png_names <- c(NEE = "draft_taylor_diagram_NEE.png", GPP = "draft_taylor_diagram_GPP.png",
                     RECO = "draft_taylor_diagram_RECO.png", LE = "draft_taylor_diagram_LE.png")

# Shared site palette (00_config.R) -- same colours as the scatter and
# climatology figures, per the 2026-11-03 fixes.
site_colors <- STAGE2_SITE_COLORS

for (fx in flux_order) {
  sub <- monthly_df[monthly_df$flux == fx, ]
  if (nrow(sub) == 0) next
  sites_here <- unique(sub$site_id)

  png(file.path(FLUXCOM_ROOT, "figures", flux_png_names[fx]), width = 1400, height = 1400, res = 200)
  taylor_diagram(sub$tower_value, sub$xbase_value, sub$site_id, sub$shape_code, site_colors,
                  paste0("DRAFT — Taylor diagram, ", fx, " (monthly, bias = X-BASE − tower)"))
  for (site_id in sites_here) {
    s <- sub[sub$site_id == site_id, ]
    r <- stats::cor(s$tower_value, s$xbase_value)
    sd_ratio <- stats::sd(s$xbase_value) / stats::sd(s$tower_value)
    theta <- acos(max(-1, min(1, r)))
    shp <- unique(s$shape_code)[1]
    points(sd_ratio * cos(theta), sd_ratio * sin(theta), pch = shp, col = site_colors[site_id], cex = 2, lwd = 2)
  }
  legend("topright", legend = sites_here, col = site_colors[sites_here], pch = 16, bty = "n", cex = 0.8)
  legend("bottomright", legend = c("agreed", "agreed, training site", "pending Ossenatou"),
         pch = c(16, 17, 15), bty = "n", cex = 0.7)
  mtext(paste0(fx, " [", flux_units[fx], "]; radius = sd(X-BASE)/sd(tower); angle = correlation; * = tower reference"),
        side = 1, line = 3, cex = 0.7)
  dev.off()
}

cat("\n[WAFNET/fluxcom_comparison] Wrote figures/draft_monthly_scatter.png, draft_site_map_bias.png, draft_taylor_diagram_{NEE,GPP,RECO,LE}.png\n")
