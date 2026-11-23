## 09_site_map.R -- two-panel site map for the RSCOTT_AGU26 v2 package
##
## No change to any selection or data -- reads package_rscott_2026-11-23_v2/sites.csv
## (54 already-kept sites) as-is and only draws a map.
##
## Panel (a): world map (Equal Earth), all 54 sites, no labels.
## Panel (b): zoom on the contiguous western United States (LAEA, centred on
##   that region), where 24 of the 27 US sites cluster (the other 3 -- US-TKs,
##   US-ICt, US-ICh -- are in interior Alaska and are excluded from this panel
##   so the zoom isn't blown out to cover both; they remain visible in panel a).
##
## Shapes: the paper's own `shape_igbp` (R/plot_constants.R) assigns CSH/GRA/
## OSH/SAV/WSA to pch 3/5/7/0/2 -- all hollow, outline-only symbols with no
## fill channel. The user's spec explicitly asks for symbols "filled in the
## paper's IGBP colours", which hollow shapes cannot do. Deviation, stated
## here and in the caption: this figure instead uses 5 solid ggplot fillable
## shapes (21 circle / 22 square / 23 diamond / 24 triangle-up / 25
## triangle-down), chosen for maximum pairwise visual distinctness (all 5
## IGBP_colours hex values are close yellow/olive/gold hues), each filled
## with the paper's exact IGBP_colours hex value and outlined in black for
## visibility against grey land.

source("RSCOTT_AGU26/code/00_config.R")
source("R/figures/fig_maps.R")  # sources R/plot_constants.R + R/nature_format.R itself

suppressPackageStartupMessages({
  library(sf)
  library(ggrepel)
  library(patchwork)
})

.disable_s2()  # avoids spherical-geometry degenerate-edge errors on st_union() of world polygons

PKG_DIR_V2 <- file.path(RSCOTT_ROOT, "package_rscott_2026-11-23_v2")
sites <- readr::read_csv(file.path(PKG_DIR_V2, "sites.csv"), show_col_types = FALSE)

stopifnot(nrow(sites) == 54)
stopifnot(all(sites$igbp %in% c("CSH", "GRA", "OSH", "SAV", "WSA")))
message("[09_site_map] ", nrow(sites), " kept sites read from ", PKG_DIR_V2, "/sites.csv")

# ---- Shape/fill mapping (documented deviation -- see header) ---------------
igbp_shape_fill   <- c(CSH = 24, GRA = 21, OSH = 22, SAV = 23, WSA = 25)
igbp_fill_colours <- IGBP_colours[names(igbp_shape_fill)]
igbp_labels       <- IGBP_names[names(igbp_shape_fill)]

# Fixed factor levels (all 5 classes) so panel (a) and panel (b) build
# IDENTICAL shape/fill scales even though panel (b)'s western-US zoom has no
# CSH site -- combined with drop = FALSE below, this lets patchwork collapse
# both panels' legends into one shared 5-class legend instead of two
# different-length ones.
sites$igbp <- factor(sites$igbp, levels = names(igbp_shape_fill))

sites_sf <- sf::st_as_sf(sites, coords = c("location_long", "location_lat"),
                          crs = 4326, remove = FALSE)

.dense_boundary_sf <- function(lon_min, lon_max, lat_min, lat_max, n = 200) {
  lons <- seq(lon_min, lon_max, length.out = n)
  lats <- seq(lat_min, lat_max, length.out = n)
  ring <- rbind(
    cbind(lons, lat_min), cbind(lon_max, lats),
    cbind(rev(lons), lat_max), cbind(lon_min, rev(lats)),
    cbind(lon_min, lat_min)
  )
  sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
}

# ============================================================================
# Panel (a): world map, Equal Earth, no site labels
# ============================================================================
message("[09_site_map] Building panel (a) world map ...")
geo_world <- .land_and_coast_sf()
land_a    <- sf::st_make_valid(sf::st_union(geo_world$land))  # dissolve -- plain grey, no country borders

panel_a <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = land_a, fill = "grey80", colour = NA) +
  ggplot2::geom_sf(data = geo_world$coast, colour = "grey45", linewidth = nature_lwd(0.4)) +
  ggplot2::geom_sf(data = sites_sf, ggplot2::aes(shape = igbp, fill = igbp),
                    colour = "black", size = 2.3, stroke = 0.3) +
  ggplot2::scale_shape_manual(values = igbp_shape_fill, name = "IGBP class",
                               breaks = names(igbp_shape_fill), labels = igbp_labels, drop = FALSE) +
  ggplot2::scale_fill_manual(values = igbp_fill_colours, name = "IGBP class",
                              breaks = names(igbp_shape_fill), labels = igbp_labels, drop = FALSE) +
  ggplot2::coord_sf(crs = EQUAL_EARTH_CRS, expand = FALSE, datum = NA) +
  ggplot2::theme_void(base_size = 11) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(title = "(a) World")

# ============================================================================
# Panel (b): zoom on the contiguous western United States, LAEA
# ============================================================================
message("[09_site_map] Building panel (b) western-US zoom ...")

WUS <- list(lon_min = -125, lon_max = -93, lat_min = 29, lat_max = 49.5)
clon <- mean(c(WUS$lon_min, WUS$lon_max))
clat <- mean(c(WUS$lat_min, WUS$lat_max))
crs_laea_wus <- sprintf("+proj=laea +lat_0=%f +lon_0=%f +datum=WGS84 +units=m +no_defs", clat, clon)

pad_lon <- (WUS$lon_max - WUS$lon_min) * 0.12
pad_lat <- (WUS$lat_max - WUS$lat_min) * 0.12
geo_b <- .land_and_coast_sf(
  xmin = WUS$lon_min - pad_lon, xmax = WUS$lon_max + pad_lon,
  ymin = WUS$lat_min - pad_lat, ymax = WUS$lat_max + pad_lat
)
land_b <- sf::st_make_valid(sf::st_union(geo_b$land))

in_wus <- sites$location_long >= WUS$lon_min & sites$location_long <= WUS$lon_max &
          sites$location_lat  >= WUS$lat_min & sites$location_lat  <= WUS$lat_max
sites_b <- sites[in_wus, ]
message("[09_site_map] ", nrow(sites_b), "/", nrow(sites),
        " sites fall inside the western-US zoom box (excludes the 3 interior-Alaska sites: ",
        paste(setdiff(sites$site_id[sites$country == "US"], sites_b$site_id), collapse = ", "), ")")
sites_b_sf <- sf::st_as_sf(sites_b, coords = c("location_long", "location_lat"),
                            crs = 4326, remove = FALSE)

boundary_b     <- .dense_boundary_sf(WUS$lon_min, WUS$lon_max, WUS$lat_min, WUS$lat_max)
boundary_b_prj <- sf::st_transform(sf::st_sf(geometry = boundary_b), crs_laea_wus)
bb_b           <- sf::st_bbox(boundary_b_prj)

# ---- Overlap detection for panel (b) labels --------------------------------
# Distance in the PROJECTED (LAEA, metres) space actually plotted, not raw
# lat/long degrees -- needed for the dist-matrix fallback below, and for the
# earlier (superseded) threshold attempts documented in git history for this
# script.
#
# Two automatic km-threshold passes were tried and rejected: a formula from
# the panel's nominal mm scale badly over-flagged (even US-KFS/US-Kon, ~118km
# apart and clearly distinct in the render, fell inside a 2.5x-point-diameter
# ~120km threshold); a tightened 20km threshold then under-flagged -- it
# missed US-Mpj/US-Wjs/US-Aud, which a plain-dot debug render (all 24 sites
# labelled, `.debug_all_labels.png`, not shipped) showed DO sit inside the
# same tight visual cluster as US-Seg/US-Ses/US-SRG/US-SRM/US-SRS at this
# panel's actual print scale, despite being 34-56km from their nearest
# cluster-mate -- farther than the pairs the 20km threshold correctly caught.
# No single distance threshold cleanly separates "visually clustered" from
# "visually separate" here (e.g. US-Mpj/US-Ses at 47.9km visually clusters,
# while US-Kon/US-KFS at 118km does not, but US-Seg/US-Wjs at 77.3km -- same
# cluster as Mpj -- is still larger than some separate-looking pairs
# elsewhere) -- so the final label set below was determined by direct visual
# inspection of the actual rendered panel (b), not a formula, then confirmed
# against the pairwise distances computed below for documentation.
label_sites <- c(
  "US-SRG", "US-SRM", "US-SRS", "US-Whs", "US-Wkg", "US-Aud",  # Santa Rita Experimental Range cluster, AZ (1.5-56km apart)
  "US-Seg", "US-Ses", "US-Mpj", "US-Wjs",                       # Sevilleta/Mountainair/Willard cluster, NM (5-82km apart)
  "US-Jo1", "US-Jo2", "US-xJR",                                 # Jornada cluster, NM (3-22km apart)
  "US-Ton", "US-Var",                                           # N. California pair (2.4km)
  "US-xDC", "US-xWD",                                           # North Dakota pair (10.9km)
  "US-Kon", "US-xKA"                                            # Kansas pair (5.5km)
)
# NOT labelled, confirmed visually and by distance as clearly separate at
# this panel's scale: US-KFS/US-KLS (118/130km+ from US-Kon/US-xKA),
# US-Fwf, US-CdM, US-xAE (each >100km from any other site in this panel).
pts_b_prj <- sf::st_coordinates(sf::st_transform(sites_b_sf, crs_laea_wus))
dmat <- as.matrix(stats::dist(pts_b_prj))
dimnames(dmat) <- list(sites_b$site_id, sites_b$site_id)
diag(dmat) <- Inf
for (sid in label_sites) {
  nearest <- sort(dmat[sid, ])[1]
  message("[09_site_map] ", sid, ": nearest neighbour ", names(nearest), " at ",
          round(nearest / 1000, 1), " km")
}
message("[09_site_map] Sites labelled for visual overlap in panel (b) (manually confirmed, see script comment): ",
        paste(sort(label_sites), collapse = ", "))

sites_b_sf$label <- ifelse(sites_b_sf$site_id %in% label_sites, sites_b_sf$site_id, NA_character_)

panel_b <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = land_b, fill = "grey80", colour = NA) +
  ggplot2::geom_sf(data = geo_b$coast, colour = "grey45", linewidth = nature_lwd(0.4)) +
  ggplot2::geom_sf(data = sites_b_sf, ggplot2::aes(shape = igbp, fill = igbp),
                    colour = "black", size = 2.3, stroke = 0.3) +
  ggrepel::geom_text_repel(
    # IMPORTANT: geom_text_repel does not go through coord_sf()'s automatic
    # lon/lat -> projected reprojection the way geom_sf() does, so it needs
    # already-PROJECTED (LAEA metres) coordinates, not raw lon/lat -- using
    # raw lon/lat here first collapsed every label near one point (small
    # degree-valued x/y read as near-origin metre offsets in this CRS).
    data = sf::st_set_geometry(sites_b_sf, NULL) |> cbind(as.data.frame(pts_b_prj)),
    ggplot2::aes(x = X, y = Y, label = label),
    # box.padding/point.padding/force raised, and max.time/max.iter given more
    # budget, after the default settings produced a leader line (US-SRS ->
    # its point) that visibly crossed through the neighbouring "US-SRG" label
    # text in the dense Santa Rita cluster -- ggrepel's repulsion only avoids
    # label/label and label/point bounding-box overlap, not a segment
    # slicing through an unrelated label, so this had to be fixed by giving
    # it more room and more search budget to find a layout that avoids it,
    # not by changing what it optimises for. Re-verified by cropping the
    # rendered PNG at pixel-exact regions for every labelled cluster after
    # this change (not shipped; see SESSION_LOG.md for the crop coordinates).
    size = 2.4, min.segment.length = 0, segment.size = 0.25, max.overlaps = Inf,
    box.padding = 0.5, point.padding = 0.3, force = 3, max.time = 3, max.iter = 100000,
    na.rm = TRUE, seed = 1
  ) +
  ggplot2::scale_shape_manual(values = igbp_shape_fill, name = "IGBP class",
                               breaks = names(igbp_shape_fill), labels = igbp_labels, drop = FALSE) +
  ggplot2::scale_fill_manual(values = igbp_fill_colours, name = "IGBP class",
                              breaks = names(igbp_shape_fill), labels = igbp_labels, drop = FALSE) +
  ggplot2::coord_sf(crs = crs_laea_wus, xlim = c(bb_b["xmin"], bb_b["xmax"]),
                     ylim = c(bb_b["ymin"], bb_b["ymax"]), expand = FALSE, datum = NA) +
  ggplot2::theme_void(base_size = 11) +
  ggplot2::theme(legend.position = "none") +  # single shared legend comes from panel (a) -- see combine step
  ggplot2::labs(title = "(b) Western United States")

# ============================================================================
# Combine, save
# ============================================================================
# One shared legend only (from panel a, which has all 5 IGBP classes present
# in its own data -- panel b's western-US zoom has no CSH site, so letting
# both panels try to each draw their own legend produced two different-length
# legends that patchwork's guides="collect" did not merge, and overflowed the
# figure width). plot_layout(guides="collect") is kept so patchwork still
# manages the shared legend's placement, but the `&` theme override that
# would otherwise re-enable panel b's own legend is deliberately NOT applied
# here (panel b's own theme(legend.position="none") above would be clobbered
# by a blanket `& theme(legend.position = "bottom")`).
combined <- (panel_a | panel_b) +
  patchwork::plot_layout(guides = "collect", widths = c(1.6, 1)) +
  patchwork::plot_annotation(
    title = "DRAFT -- RSCOTT_AGU26 kept sites by IGBP class",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                            legend.position = "bottom")
  )

fig_dir  <- file.path(RSCOTT_ROOT, "figures")
png_path <- file.path(fig_dir, "site_map_igbp_DRAFT.png")
pdf_path <- file.path(fig_dir, "site_map_igbp_DRAFT.pdf")

ggplot2::ggsave(png_path, combined, width = 12, height = 6, dpi = 300, bg = "white")
ggplot2::ggsave(pdf_path, combined, width = 12, height = 6, bg = "white")
message("[09_site_map] Saved: ", png_path, " and ", pdf_path)

caption <- c(
  "DRAFT -- not for publication.",
  paste0(
    "Map of the 54 FLUXNET sites kept in the RSCOTT_AGU26 v2 extraction for Russ Scott's AGU26 ",
    "poster. Panel (a) shows all 54 sites worldwide; panel (b) zooms on the contiguous western ",
    "United States, where most of the US sites cluster (3 interior-Alaska sites -- US-TKs, ",
    "US-ICt, US-ICh -- fall outside this zoom box and are shown only in panel a)."
  ),
  paste0(
    "Selection rule (see sites.csv/README.md for the full detail): dryland IGBP classes ",
    "(Closed Shrubland, Open Shrubland, Grassland, Savanna, Woody Savanna) with mean annual ",
    "precipitation below 800 mm and at least 5 years (calendar or water year) of passing GPP, ",
    "ET and precipitation data, from the FLUXNET Shuttle snapshot taken 2026-09-20."
  ),
  paste0(
    "Symbol shape and fill colour both encode IGBP class (legend). Note: these 5 filled shapes ",
    "(circle/square/diamond/triangle-up/triangle-down) are not the paper's usual hollow ",
    "shape_igbp symbols for these classes -- hollow shapes have no fill channel, so filled ",
    "shapes were substituted to satisfy the 'filled in the paper's colours' request; the fill ",
    "colours themselves are the paper's exact IGBP_colours hex values (R/plot_constants.R)."
  ),
  paste0(
    "Land is a single dissolved grey polygon with no internal country borders or labels. In ",
    "panel (b), site_id labels are shown only for sites whose symbols are close enough at this ",
    "zoom to visually overlap, confirmed by direct inspection of the rendered panel (see ",
    "09_site_map.R for the specific clusters and the pairwise distances behind them); all ",
    "other sites are unlabelled in both panels."
  )
)
write_caption(png_path, caption)
write_meta(
  png_path,
  input_sources = c(
    file.path(PKG_DIR_V2, "sites.csv"),
    "R/plot_constants.R (IGBP_colours)",
    "R/figures/fig_maps.R (.land_and_coast_sf, EQUAL_EARTH_CRS)",
    "rnaturalearth::ne_countries()/ne_coastline() (scale='medium')"
  ),
  notes = paste0(
    "DRAFT two-panel site map, no change to any selection/data. Panel (b) overlap labels: ",
    paste(sort(label_sites), collapse = ", "), ". Filled-shape deviation from paper's shape_igbp documented in the .legend.txt caption."
  )
)

message("[09_site_map] Done.")
