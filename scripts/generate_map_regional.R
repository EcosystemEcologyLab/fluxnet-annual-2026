## generate_map_regional.R
## Extended Data figure: Equal Earth world overview (panel a, the same map as
## Figure 1a) with the four regional extents outlined, plus four Lambert
## Azimuthal Equal-Area regional close-ups (panels b-e, one per region) in a
## 2x2 grid below it.
##
## Output: review/figures/draft_manuscript_v1/SupFigs/
##   supp_map_regional.png/.pdf/.jpg + .legend.txt

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/figures/fig_maps.R")
source("R/nature_format.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(sf); library(patchwork)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Extended Data: regional map figure ===")

OUT_DIR  <- file.path("review", "figures", "draft_manuscript_v1", "SupFigs")
OUT_STEM <- file.path(OUT_DIR, "supp_map_regional")
fs::dir_create(OUT_DIR)

.disable_s2()

snap_file <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
shuttle_meta <- read_csv(snap_file, show_col_types = FALSE)
sites_clean <- shuttle_meta |>
  filter(!is.na(location_lat), !is.na(location_long),
         between(location_lat, -90, 90), between(location_long, -180, 180)) |>
  distinct(site_id, .keep_all = TRUE)
msg("Loaded ", nrow(sites_clean), " sites (", snap_file, ")")

## ---- Region extents (task 8) -----------------------------------------------
## Starting extents taken verbatim from the task, each adjusted only as far as
## needed so no tower sits on a panel edge -- checked directly against
## sites_clean (epsilon 0.6 deg): only East/Southeast Asia needed a change,
## CN-Erg at 50.2N sits 0.2 deg outside the starting 50N edge; lat_max raised
## to 51.5N (1.3 deg clearance). The other three regions had zero sites within
## 0.6 deg of any starting edge, so are used unchanged.
REGIONS <- list(
  north_america = list(label = "North America",          lat_min = 15,  lat_max = 72,   lon_min = -170, lon_max = -50),
  europe        = list(label = "Europe",                  lat_min = 34,  lat_max = 72,   lon_min = -12,  lon_max = 42),
  asia          = list(label = "East and Southeast Asia", lat_min = -10, lat_max = 51.5, lon_min = 95,   lon_max = 146),
  anz           = list(label = "Australia and New Zealand", lat_min = -48, lat_max = -10, lon_min = 110, lon_max = 180)
)
REGION_LETTERS <- c(north_america = "b", europe = "c", asia = "d", anz = "e")

in_region <- function(df, r) {
  dplyr::between(df$location_lat, r$lat_min, r$lat_max) &
    dplyr::between(df$location_long, r$lon_min, r$lon_max)
}
in_any_region <- Reduce(`|`, lapply(REGIONS, in_region, df = sites_clean))
n_outside <- sum(!in_any_region)
msg("Towers outside all four regional extents (panel a only): ", n_outside, " of ", nrow(sites_clean))

for (nm in names(REGIONS)) {
  r <- REGIONS[[nm]]
  n_in <- sum(in_region(sites_clean, r))
  msg("Region '", r$label, "': lat [", r$lat_min, ", ", r$lat_max, "], lon [",
      r$lon_min, ", ", r$lon_max, "] -- ", n_in, " towers")
}

## ---- Dense-boundary projected bbox helper (reused for region rectangles on
## panel a AND for each regional panel's own LAEA xlim/ylim) ------------------
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

## ---- Panel a: Equal Earth world overview + region outlines -----------------
msg("Building panel a (world overview) ...")
geo_world <- .land_and_coast_sf()
sites_sf_all <- sf::st_as_sf(sites_clean, coords = c("location_long", "location_lat"),
                              crs = 4326, remove = FALSE)
region_outlines <- do.call(c, lapply(REGIONS, function(r) {
  .dense_boundary_sf(r$lon_min, r$lon_max, r$lat_min, r$lat_max)
}))
region_outlines_sf <- sf::st_sf(region = names(REGIONS), geometry = region_outlines)

panel_a <- .map_base_eqearth(geo_world$land, geo_world$coast) +
  ggplot2::geom_sf(data = sites_sf_all, shape = 16, colour = "#0072B2", size = 0.5, alpha = 0.45) +
  ggplot2::geom_sf(data = region_outlines_sf, fill = NA, colour = "#D55E00",
                    linewidth = nature_lwd(0.6)) +
  ggplot2::coord_sf(crs = EQUAL_EARTH_CRS, expand = FALSE, datum = NA) +
  panel_letter("a")
fig01a_h <- equal_earth_height_mm(NATURE_ED_MAX_WIDTH_MM)
msg("Panel a height at ", NATURE_ED_MAX_WIDTH_MM, "mm wide: ", round(fig01a_h, 2), "mm")

## ---- Panels b-e: LAEA regional close-ups ------------------------------------
## Round-length scale bar: pick the "nicest" round km value near span/4 from a
## fixed candidate set, drawn as a plain segment + label in the panel's own
## projected (metre) coordinates -- no mapping package beyond base sf/ggplot2
## is added (ggspatial is not installed; CLAUDE.md asks not to add new
## package dependencies without discussion).
.nice_round_km <- function(span_km) {
  candidates <- c(50, 100, 200, 250, 500, 1000, 1500, 2000, 2500, 3000)
  target <- span_km / 4
  candidates[which.min(abs(candidates - target))]
}

build_region_panel <- function(nm) {
  r <- REGIONS[[nm]]
  clon <- mean(c(r$lon_min, r$lon_max))
  clat <- mean(c(r$lat_min, r$lat_max))
  crs_laea <- sprintf("+proj=laea +lat_0=%f +lon_0=%f +datum=WGS84 +units=m +no_defs", clat, clon)

  ## Basemap cropped to a padded window (wider than the display extent) so no
  ## blank wedge appears at the panel corners after projecting.
  pad_lon <- (r$lon_max - r$lon_min) * 0.15
  pad_lat <- (r$lat_max - r$lat_min) * 0.15
  geo <- .land_and_coast_sf(xmin = max(r$lon_min - pad_lon, -180), xmax = min(r$lon_max + pad_lon, 180),
                             ymin = max(r$lat_min - pad_lat, -60), ymax = min(r$lat_max + pad_lat, 85))

  sites_r <- sites_clean[in_region(sites_clean, r), ]
  sites_r_sf <- sf::st_as_sf(sites_r, coords = c("location_long", "location_lat"), crs = 4326, remove = FALSE)

  boundary <- .dense_boundary_sf(r$lon_min, r$lon_max, r$lat_min, r$lat_max)
  boundary_proj <- sf::st_transform(sf::st_sf(geometry = boundary), crs_laea)
  bb <- sf::st_bbox(boundary_proj)

  span_km <- unname((bb["xmax"] - bb["xmin"]) / 1000)
  bar_km  <- .nice_round_km(span_km)
  bar_m   <- bar_km * 1000
  bar_x0  <- unname(bb["xmin"] + 0.06 * (bb["xmax"] - bb["xmin"]))
  bar_y0  <- unname(bb["ymin"] + 0.08 * (bb["ymax"] - bb["ymin"]))

  p <- .map_base_eqearth(geo$land, geo$coast) +
    ggplot2::geom_sf(data = sites_r_sf, shape = 16, colour = "#0072B2", size = 0.7, alpha = 0.55) +
    ggplot2::annotate("segment", x = bar_x0, xend = bar_x0 + bar_m, y = bar_y0, yend = bar_y0,
                       linewidth = nature_lwd(0.6), colour = "black") +
    ggplot2::annotate("text", x = bar_x0 + bar_m / 2, y = bar_y0, label = paste0(bar_km, " km"),
                       size = NATURE_SMALL_PT / ggplot2::.pt, vjust = -0.6, colour = "black",
                       family = NATURE_FONT) +
    ggplot2::coord_sf(crs = crs_laea, xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]),
                       expand = FALSE, datum = NA) +
    panel_letter(REGION_LETTERS[[nm]])

  list(panel = p, n = nrow(sites_r), span_km = span_km, bar_km = bar_km,
       extent = r, crs = crs_laea)
}

msg("Building regional panels (b-e) ...")
region_results <- lapply(names(REGIONS), build_region_panel)
names(region_results) <- names(REGIONS)
for (nm in names(region_results)) {
  rr <- region_results[[nm]]
  msg("  Panel ", REGION_LETTERS[[nm]], " (", REGIONS[[nm]]$label, "): n = ", rr$n,
      " towers, LAEA, span ~", round(rr$span_km), "km, scale bar ", rr$bar_km, "km")
}

grid_be <- (region_results$north_america$panel | region_results$europe$panel) /
  (region_results$asia$panel | region_results$anz$panel)

combo <- panel_a / grid_be + patchwork::plot_layout(heights = c(fig01a_h, 2 * fig01a_h))

saved <- save_nature_figure(combo, OUT_STEM, width_mm = NATURE_ED_MAX_WIDTH_MM,
                             height_mm = min(fig01a_h + 2 * fig01a_h + 10, NATURE_ED_MAX_HEIGHT_MM),
                             extended_data = TRUE)
msg("Saved: ", saved$png, ", ", saved$pdf, ", ", saved$jpeg)

## ---- Legend ------------------------------------------------------------------
region_line <- function(nm) {
  rr <- region_results[[nm]]
  r  <- REGIONS[[nm]]
  paste0("  ", REGION_LETTERS[[nm]], " ", r$label, " -- lat [", r$lat_min, ", ", r$lat_max,
         "], lon [", r$lon_min, ", ", r$lon_max, "]; n = ", rr$n, " towers; scale bar ", rr$bar_km, " km")
}
legend_lines <- c(
  "FIGURE LEGEND — supp_map_regional.png",
  strrep("=", 60), "",
  "TITLE: Extended Data Figure — Regional distribution of the current FLUXNET network",
  "",
  "DESCRIPTION:",
  "Panel a: the same Equal Earth (EPSG:8857) world map as Figure 1a, all ",
  paste0(nrow(sites_clean), " towers, with the four regional extents used in panels b-e outlined"),
  "in orange. Panels b-e: one Lambert Azimuthal Equal-Area projection per region, centred on",
  "that region's own extent centroid, showing only the towers that fall inside that region's",
  paste0("extent (towers outside all four extents -- ", n_outside, " of ", nrow(sites_clean),
         " -- appear only in panel a, not in any regional panel)."),
  "",
  "PANELS:",
  region_line("north_america"),
  region_line("europe"),
  region_line("asia"),
  region_line("anz"),
  "",
  "PROJECTIONS: panel a Equal Earth (EPSG:8857, equal-area, pseudo-cylindrical, whole-world).",
  "Panels b-e each their own Lambert Azimuthal Equal-Area (+proj=laea), centred on that panel's",
  "own region -- NOT the same projection or centre as panel a or as each other.",
  "",
  "SCALE BARS (panels b-e): round-length, labelled, in the panel's own projected (metre)",
  "coordinates -- chosen per panel as the nearest of {50, 100, 200, 250, 500, 1000, 1500, 2000,",
  "2500, 3000} km to one quarter of that panel's displayed span.",
  "",
  "POINT STYLE: identical in all five panels -- filled, no outline, semi-transparent (shape 16,",
  "colour #0072B2), so overlapping towers read as darker, matching Figure 1a.",
  "",
  "SOURCE: scripts/generate_map_regional.R. Basemap: rnaturalearth::ne_countries()/ne_coastline()",
  "(1:50m), cropped per panel before projecting.",
  paste0("DIMENSIONS: ", NATURE_ED_MAX_WIDTH_MM, " mm wide, 600 dpi PNG + vector PDF + 300 ppi JPEG,"),
  "Helvetica, white background. All text 5-7pt except the bold lower-case panel letters (8pt)."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))
msg("=== Done ===")
