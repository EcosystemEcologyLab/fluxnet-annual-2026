## generate_fig_map_network.R
## Figure stage 2 (logs/figstage_prompt.md): the five-panel regional map
## (Equal Earth world overview with the four regional extents outlined, plus
## four Lambert Azimuthal Equal-Area regional close-ups) promoted from
## Extended Data (SupFigs/supp_map_regional) to the main-text Figure 1, at
## 183mm wide. Builds the same five panels, from the same pinned snapshot, as
## scripts/generate_map_regional.R (now retired -- see figure stage 2 in
## SESSION_LOG.md), with three changes:
##   1. 183mm width (main-text double-column) instead of 180mm (Extended
##      Data), and no JPEG (main-text figures don't get one).
##   2. Row heights for the 2x2 regional grid are computed from each
##      region's OWN rendered aspect ratio (LAEA-projected bbox
##      xspan/yspan) at the actual column width, instead of a fixed
##      "2 x panel-a-height" allocation split evenly between two rows --
##      the fixed allocation let coord_sf's fixed-aspect rendering
##      letterbox (pad with blank space) inside each row's cell whenever a
##      region's true aspect didn't match the allocated cell, which is what
##      produced the wide blank bands between rows in the retired ED
##      version. Matching each row's allocated height to its tallest
##      panel's true rendered height removes that letterboxing.
##   3. Scale bar (panels b-e) and panel-letter (a-e) placement is chosen
##      programmatically: a small set of candidate positions, ordered by
##      distance from the original default (bottom-left for the bar,
##      top-left for the letter) is tested against the panel's own land
##      polygon (reprojected to that panel's own LAEA CRS) via
##      sf::st_intersects(), and the nearest candidate that does not
##      intersect land is used. This is a direct, reproducible fix for two
##      defects found on review of the Extended Data version: the "1000 km"
##      / "1500 km" scale-bar labels in panels c/d printed over coastline
##      and towers, and the panel letter "e" touching an island -- rather
##      than hand-tuned pixel offsets, which would not generalise if the
##      network (and hence site/land layout) changes.
##
## Output: review/figures/draft_manuscript_v1/fig_01_map_network.png/.pdf/.legend.txt
## (renumbered from fig_map_network.* under figure stage 6 numbering, 2026-10-02;
## this script's own name is unchanged -- see docs/figure_inventory.md)

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
msg("=== Main-text Figure: five-panel network map (world + 4 regions) ===")

OUT_DIR  <- file.path("review", "figures", "draft_manuscript_v1")
OUT_STEM <- file.path(OUT_DIR, "fig_01_map_network")
fs::dir_create(OUT_DIR)

.disable_s2()

snap_file <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
shuttle_meta <- read_csv(snap_file, show_col_types = FALSE)
sites_clean <- shuttle_meta |>
  filter(!is.na(location_lat), !is.na(location_long),
         between(location_lat, -90, 90), between(location_long, -180, 180)) |>
  distinct(site_id, .keep_all = TRUE)
msg("Loaded ", nrow(sites_clean), " sites (", snap_file, ")")

## ---- Region extents (unchanged from scripts/generate_map_regional.R) -------
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

## ---- Dense-boundary projected bbox helper -----------------------------------
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
h_a <- equal_earth_height_mm(NATURE_WIDTH_DOUBLE_MM)
msg("Panel a height at ", NATURE_WIDTH_DOUBLE_MM, "mm wide: ", round(h_a, 2), "mm")

## ---- Land-avoidance helpers (task: scale bar / letter placement) -----------
## Systematic grid of candidate anchor fractions (of the panel's own bb),
## sorted by distance from a default anchor -- nearest-to-default first, so
## the search always prefers the smallest possible change from the original
## bottom-left (bar) / top-left (letter) placement, and only moves further
## away when nearer spots are blocked by land. This replaces a hand-picked
## short candidate list that missed real open-water/blank-margin areas for
## some regions (e.g. the mid-Atlantic strip in panel c, the East China/
## Philippine Sea strip in panel d).
.grid_candidates <- function(default_x, default_y, step = 0.04, lo = 0.03, hi = 0.93) {
  xs <- seq(lo, hi, by = step)
  ys <- seq(lo, hi, by = step)
  grid <- expand.grid(x0_frac = xs, y0_frac = ys)
  grid$dist <- sqrt((grid$x0_frac - default_x)^2 + (grid$y0_frac - default_y)^2)
  grid <- grid[order(grid$dist), ]
  lapply(seq_len(nrow(grid)), function(i) {
    list(x0_frac = grid$x0_frac[i], y0_frac = grid$y0_frac[i],
         name = sprintf("x=%.2f,y=%.2f", grid$x0_frac[i], grid$y0_frac[i]))
  })
}

## Generic "clear rectangle" search over the grid above; the first candidate
## whose rectangle does not intersect land_proj is used. Falls back to the
## nearest-to-default candidate, with a warning, if every candidate on the
## grid intersects land -- never silently drops the bar/letter.
.find_clear_rect <- function(land_proj, bb, width, height, default_x, default_y, what) {
  xspan <- unname(bb["xmax"] - bb["xmin"]); yspan <- unname(bb["ymax"] - bb["ymin"])
  candidates <- .grid_candidates(default_x, default_y)
  for (i in seq_along(candidates)) {
    cand <- candidates[[i]]
    if (cand$x0_frac + width / xspan > 0.97 || cand$y0_frac + height / yspan > 0.97) next
    x0 <- unname(bb["xmin"]) + cand$x0_frac * xspan
    y0 <- unname(bb["ymin"]) + cand$y0_frac * yspan
    rect <- sf::st_as_sfc(sf::st_bbox(c(xmin = x0, xmax = x0 + width,
                                          ymin = y0, ymax = y0 + height),
                                        crs = sf::st_crs(land_proj)))
    hit <- suppressMessages(any(sf::st_intersects(rect, land_proj, sparse = FALSE)))
    if (!hit) {
      msg("  ", what, ": candidate (", cand$name, ", dist ", round(sqrt((cand$x0_frac-default_x)^2+(cand$y0_frac-default_y)^2), 2),
          " from default) is clear of land -- used.")
      return(list(x0 = x0, y0 = y0, cand = cand))
    }
  }
  msg("  ", what, ": WARNING -- no grid candidate was clear of land; falling back to default.")
  list(x0 = unname(bb["xmin"]) + default_x * xspan,
       y0 = unname(bb["ymin"]) + default_y * yspan, cand = NULL)
}

## Panel-letter placement: point-buffer test over the same grid, defaulting
## to the top-left corner (keeps the letter visually "in the corner" as for
## every other panel unless land forces it elsewhere).
.place_letter <- function(letter, land_proj, bb) {
  xspan <- unname(bb["xmax"] - bb["xmin"]); yspan <- unname(bb["ymax"] - bb["ymin"])
  buf_r <- 0.045 * min(xspan, yspan)
  candidates <- .grid_candidates(default_x = 0.03, default_y = 0.95)
  chosen <- NULL
  for (i in seq_along(candidates)) {
    cand <- candidates[[i]]
    x <- unname(bb["xmin"]) + cand$x0_frac * xspan
    y <- unname(bb["ymin"]) + cand$y0_frac * yspan
    pt <- sf::st_sfc(sf::st_point(c(x, y)), crs = sf::st_crs(land_proj))
    circ <- sf::st_buffer(pt, buf_r)
    hit <- suppressMessages(any(sf::st_intersects(circ, land_proj, sparse = FALSE)))
    if (!hit) {
      msg("  letter '", letter, "': candidate (", cand$name, ") is clear of land -- used.")
      chosen <- list(x = x, y = y)
      break
    }
  }
  if (is.null(chosen)) {
    msg("  letter '", letter, "': WARNING -- no grid candidate was clear of land; using default top-left.")
    chosen <- list(x = unname(bb["xmin"]) + 0.03 * xspan, y = unname(bb["ymin"]) + 0.95 * yspan)
  }
  ggplot2::annotate("text", x = chosen$x, y = chosen$y, label = tolower(letter),
                     hjust = 0, vjust = 1, fontface = "bold",
                     family = NATURE_FONT, size = NATURE_PANEL_LETTER_PT / ggplot2::.pt,
                     colour = "black")
}

.nice_round_km <- function(span_km) {
  candidates <- c(50, 100, 200, 250, 500, 1000, 1500, 2000, 2500, 3000)
  target <- span_km / 4
  candidates[which.min(abs(candidates - target))]
}

## ---- Panels b-e: LAEA regional close-ups ------------------------------------
build_region_panel <- function(nm) {
  r <- REGIONS[[nm]]
  clon <- mean(c(r$lon_min, r$lon_max))
  clat <- mean(c(r$lat_min, r$lat_max))
  crs_laea <- sprintf("+proj=laea +lat_0=%f +lon_0=%f +datum=WGS84 +units=m +no_defs", clat, clon)

  pad_lon <- (r$lon_max - r$lon_min) * 0.15
  pad_lat <- (r$lat_max - r$lat_min) * 0.15
  geo <- .land_and_coast_sf(xmin = max(r$lon_min - pad_lon, -180), xmax = min(r$lon_max + pad_lon, 180),
                             ymin = max(r$lat_min - pad_lat, -60), ymax = min(r$lat_max + pad_lat, 85))

  sites_r <- sites_clean[in_region(sites_clean, r), ]
  sites_r_sf <- sf::st_as_sf(sites_r, coords = c("location_long", "location_lat"), crs = 4326, remove = FALSE)

  boundary <- .dense_boundary_sf(r$lon_min, r$lon_max, r$lat_min, r$lat_max)
  boundary_proj <- sf::st_transform(sf::st_sf(geometry = boundary), crs_laea)
  bb <- sf::st_bbox(boundary_proj)
  land_proj <- sf::st_transform(geo$land, crs_laea)

  span_km <- unname((bb["xmax"] - bb["xmin"]) / 1000)
  bar_km  <- .nice_round_km(span_km)
  bar_m   <- bar_km * 1000

  ## Scale bar + label clear-rectangle: width spans a touch wider than the
  ## bar itself (the centred label can overhang the bar ends slightly);
  ## height covers the label's text band above the bar line.
  xspan <- unname(bb["xmax"] - bb["xmin"]); yspan <- unname(bb["ymax"] - bb["ymin"])
  rect_w <- bar_m * 1.15
  rect_h <- 0.09 * yspan
  placed <- .find_clear_rect(land_proj, bb, rect_w, rect_h,
                              default_x = 0.06, default_y = 0.08,
                              what = paste0("panel ", REGION_LETTERS[[nm]], " scale bar"))
  bar_x0 <- placed$x0 + 0.075 * rect_w  # small inset so the rect pad isn't flush with the bar
  bar_y0 <- placed$y0

  letter_layer <- .place_letter(REGION_LETTERS[[nm]], land_proj, bb)

  p <- .map_base_eqearth(geo$land, geo$coast) +
    ggplot2::geom_sf(data = sites_r_sf, shape = 16, colour = "#0072B2", size = 0.7, alpha = 0.55) +
    ggplot2::annotate("segment", x = bar_x0, xend = bar_x0 + bar_m, y = bar_y0, yend = bar_y0,
                       linewidth = nature_lwd(0.6), colour = "black") +
    ggplot2::annotate("text", x = bar_x0 + bar_m / 2, y = bar_y0, label = paste0(bar_km, " km"),
                       size = NATURE_SMALL_PT / ggplot2::.pt, vjust = -0.6, colour = "black",
                       family = NATURE_FONT) +
    ggplot2::coord_sf(crs = crs_laea, xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]),
                       expand = FALSE, datum = NA) +
    letter_layer

  list(panel = p, n = nrow(sites_r), span_km = span_km, bar_km = bar_km,
       extent = r, crs = crs_laea, aspect = xspan / yspan)
}

msg("Building regional panels (b-e) ...")
region_results <- lapply(names(REGIONS), build_region_panel)
names(region_results) <- names(REGIONS)
for (nm in names(region_results)) {
  rr <- region_results[[nm]]
  msg("  Panel ", REGION_LETTERS[[nm]], " (", REGIONS[[nm]]$label, "): n = ", rr$n,
      " towers, LAEA, span ~", round(rr$span_km), "km, scale bar ", rr$bar_km,
      "km, aspect ", round(rr$aspect, 3))
}

## ---- Row heights/widths from each region's OWN rendered aspect ------------
## (fixes the blank vertical bands in the retired ED version, which allocated
## a fixed "2 x panel-a-height" split evenly between rows AND evenly between
## the two columns, regardless of the regions' true LAEA aspect ratios --
## letting coord_sf's fixed-aspect rendering pad blank space into whichever
## dimension didn't match). Each row is packed edge-to-edge at exactly
## NATURE_WIDTH_DOUBLE_MM by giving it the one height, H, at which its two
## panels' aspect-correct widths sum to the full figure width: H = W /
## (aspect_left + aspect_right); each panel then gets width = H * its own
## aspect. This removes letterboxing in BOTH directions at once, not just
## vertically -- a 50/50 column split (tried first) still letterboxed
## internally since the regions' aspects differ enough (0.83 to 1.72) that
## neither panel exactly filled a 91.5mm column at the row's required height.
.row_height <- function(aspect_left, aspect_right, total_width_mm) {
  total_width_mm / (aspect_left + aspect_right)
}
h_row1 <- .row_height(region_results$north_america$aspect, region_results$europe$aspect,
                       NATURE_WIDTH_DOUBLE_MM)
h_row2 <- .row_height(region_results$asia$aspect, region_results$anz$aspect,
                       NATURE_WIDTH_DOUBLE_MM)
w_na <- h_row1 * region_results$north_america$aspect
w_eu <- h_row1 * region_results$europe$aspect
w_as <- h_row2 * region_results$asia$aspect
w_anz <- h_row2 * region_results$anz$aspect
msg("Row 1 (b|c) height: ", round(h_row1, 2), "mm (b ", round(w_na, 1), "mm + c ", round(w_eu, 1), "mm)")
msg("Row 2 (d|e) height: ", round(h_row2, 2), "mm (d ", round(w_as, 1), "mm + e ", round(w_anz, 1), "mm)")

grid_row1 <- (region_results$north_america$panel | region_results$europe$panel) +
  patchwork::plot_layout(widths = c(w_na, w_eu))
grid_row2 <- (region_results$asia$panel | region_results$anz$panel) +
  patchwork::plot_layout(widths = c(w_as, w_anz))

combo <- panel_a / grid_row1 / grid_row2 +
  patchwork::plot_layout(heights = c(h_a, h_row1, h_row2))
total_h <- h_a + h_row1 + h_row2
msg("Combined height: ", round(total_h, 2), "mm (target <=200mm, hard limit ",
    NATURE_MAX_HEIGHT_MM, "mm)")

saved <- save_nature_figure(combo, OUT_STEM, width_mm = NATURE_WIDTH_DOUBLE_MM,
                             height_mm = total_h)
msg("Saved: ", saved$png, ", ", saved$pdf)

## ---- Legend ------------------------------------------------------------------
region_line <- function(nm) {
  rr <- region_results[[nm]]
  r  <- REGIONS[[nm]]
  paste0("  ", REGION_LETTERS[[nm]], " ", r$label, " -- lat [", r$lat_min, ", ", r$lat_max,
         "], lon [", r$lon_min, ", ", r$lon_max, "]; n = ", rr$n, " towers; scale bar ", rr$bar_km, " km")
}
legend_lines <- c(
  "FIGURE LEGEND — fig_01_map_network.png",
  strrep("=", 60), "",
  "TITLE: Figure 1. Global distribution of the current FLUXNET network",
  "",
  "DESCRIPTION:",
  "Panel a: Equal Earth (EPSG:8857) world map, all ",
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
  "2500, 3000} km to one quarter of that panel's displayed span. Position is chosen",
  "programmatically per panel (nearest candidate to the default bottom-left corner whose",
  "rectangle -- bar plus label band -- does not intersect that panel's own land polygon); panel",
  "letters are placed the same way, scanning near the top-left corner. See",
  "scripts/generate_fig_map_network.R for the candidate search.",
  "",
  "POINT STYLE: identical in all five panels -- filled, no outline, semi-transparent (shape 16,",
  "colour #0072B2), so overlapping towers read as darker, matching the retired Extended Data",
  "version this figure replaces (SupFigs/supp_map_regional, now in SupFigs/deprecated/).",
  "",
  "SOURCE: scripts/generate_fig_map_network.R. Basemap: rnaturalearth::ne_countries()/",
  "ne_coastline() (1:50m), cropped per panel before projecting.",
  paste0("DIMENSIONS: ", NATURE_WIDTH_DOUBLE_MM, " x ", round(total_h, 1),
         " mm (panel a ", round(h_a, 1), "mm + row b/c ", round(h_row1, 1),
         "mm + row d/e ", round(h_row2, 1), "mm), 600 dpi PNG + vector PDF,"),
  "Helvetica, white background. All text 5-7pt except the bold lower-case panel letters (8pt)."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))
msg("=== Done ===")
