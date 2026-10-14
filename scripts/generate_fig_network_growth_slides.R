## generate_fig_network_growth_slides.R
##
## New presentation-only "network growth" talk slides (2026-10-13 v2 task):
## one slide per historical collection plus the current snapshot, all drawn
## on the identical Equal Earth world extent/basemap/point style used for
## Figure 1a, so the four slides flip cleanly as a build. This script has no
## manuscript counterpart -- it writes only into
## review/figures/presentation_figures/slides/ and never touches
## review/figures/draft_manuscript_v1/.
##
## Site counts are gated against Table 1 (scripts/collection_comparison_table.R,
## data/snapshots/collection_sites_siteyears.csv): Marconi 35, La Thuile 252,
## FLUXNET2015 212, Current 781.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/figures/fig_maps.R")
source("R/nature_format.R")
source("R/presentation_format.R")
source("R/historical_datasets.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(sf)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Presentation-only: network growth slides (Marconi / La Thuile / FLUXNET2015 / current) ===")

SLIDE_DIR <- file.path("review", "figures", "presentation_figures", "slides")
fs::dir_create(SLIDE_DIR)

.disable_s2()

## Same pinned snapshot Figure 1 uses.
SNAP_FILE <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
shuttle_meta <- read_csv(SNAP_FILE, show_col_types = FALSE)
sites_current <- shuttle_meta |>
  filter(!is.na(location_lat), !is.na(location_long),
         between(location_lat, -90, 90), between(location_long, -180, 180)) |>
  distinct(site_id, .keep_all = TRUE)
msg("Current snapshot: ", nrow(sites_current), " sites (", SNAP_FILE, ")")

hist_lists <- load_historical_site_lists(shuttle_snapshot = SNAP_FILE)

## ---- Table 1 gate: must match data/snapshots/collection_sites_siteyears.csv
EXPECTED_N <- c(marconi = 35L, la_thuile = 252L, fluxnet2015 = 212L, current = 781L)
actual_n <- c(
  marconi     = sum(!is.na(hist_lists$marconi$location_lat)     & !is.na(hist_lists$marconi$location_long)),
  la_thuile   = sum(!is.na(hist_lists$la_thuile$location_lat)   & !is.na(hist_lists$la_thuile$location_long)),
  fluxnet2015 = sum(!is.na(hist_lists$fluxnet2015$location_lat) & !is.na(hist_lists$fluxnet2015$location_long)),
  current     = nrow(sites_current)
)
for (nm in names(EXPECTED_N)) {
  msg("Table 1 gate -- ", nm, ": expected ", EXPECTED_N[[nm]], ", actual ", actual_n[[nm]])
  if (actual_n[[nm]] != EXPECTED_N[[nm]]) {
    stop("generate_fig_network_growth_slides.R: '", nm, "' site count (", actual_n[[nm]],
         ") does not match Table 1 (", EXPECTED_N[[nm]], "). Not writing slides.")
  }
}

## ---- Shared basemap/extent -- identical across all 4 slides (Fig 1a style).
geo_world <- .land_and_coast_sf()
base_map  <- .map_base_eqearth(geo_world$land, geo_world$coast) +
  ggplot2::coord_sf(crs = EQUAL_EARTH_CRS, expand = FALSE, datum = NA) +
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA)
  )

## One symbol/colour throughout, matching Figure 1a's point style.
POINT_COLOUR <- "#0072B2"
POINT_SIZE   <- 3
POINT_ALPHA  <- 0.5
LABEL_PT     <- 20   # bold corner label (collection + n); matches the 2.5x-scaled
                      # NATURE_PANEL_LETTER_PT (8pt) used for panel letters elsewhere
                      # in the presentation slide set -- see R/presentation_format.R.

## Small white-boxed corner label, same mechanism as generate_fig_map_network.R's
## .place_letter() (top-left, opaque background so it reads over land or water).
.corner_label <- function(label) {
  ggplot2::annotate("label", x = -Inf, y = Inf, label = label,
                     hjust = 0, vjust = 1, fontface = "bold",
                     family = PRESENTATION_FONT, size = LABEL_PT / ggplot2::.pt,
                     colour = "black", fill = "white", alpha = 0.85,
                     linewidth = 0, label.padding = grid::unit(0.5, "lines"))
}

ASPECT_W <- NATURE_WIDTH_DOUBLE_MM
ASPECT_H <- equal_earth_height_mm(NATURE_WIDTH_DOUBLE_MM)

build_slide <- function(df, label, out_stem) {
  sf_pts <- sf::st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326, remove = FALSE)
  p <- base_map +
    ggplot2::geom_sf(data = sf_pts, shape = 16, colour = POINT_COLOUR,
                      size = POINT_SIZE, alpha = POINT_ALPHA) +
    .corner_label(label)
  save_presentation_figure(p, out_stem, aspect_w = ASPECT_W, aspect_h = ASPECT_H)
}

slides <- list(
  list(df = hist_lists$marconi,     label = sprintf("Marconi 2000, n = %d", actual_n[["marconi"]]),
       stem = "growth_1_marconi"),
  list(df = hist_lists$la_thuile,   label = sprintf("La Thuile 2007, n = %d", actual_n[["la_thuile"]]),
       stem = "growth_2_lathuile"),
  list(df = hist_lists$fluxnet2015, label = sprintf("FLUXNET2015, n = %d", actual_n[["fluxnet2015"]]),
       stem = "growth_3_fluxnet2015"),
  list(df = sites_current,          label = sprintf("Current FLUXNET Shuttle snapshot, n = %d", actual_n[["current"]]),
       stem = "growth_4_current")
)

for (s in slides) {
  out_path <- build_slide(s$df, s$label, file.path(SLIDE_DIR, s$stem))
  msg("Saved: ", out_path, " (", s$label, ")")
}

## ---- Legend/caption companion (this figure has no manuscript .legend.txt to
## draw from, since it has no manuscript counterpart -- written here instead,
## for the deck manifest to pull caption text from).
legend_lines <- c(
  "FIGURE LEGEND — network growth (presentation-only talk slides)",
  strrep("=", 60), "",
  "TITLE: Network growth: historical collections and the current FLUXNET Shuttle network",
  "",
  "DESCRIPTION:",
  "Four slides, one per collection, on the identical Equal Earth (EPSG:8857) world extent and",
  "basemap used for Figure 1a, so the slides flip cleanly as a build. Each slide shows only that",
  "collection's own sites, one symbol and colour throughout (filled circle, #0072B2), with a",
  "small top-left corner label giving the collection name and site count. Sites are matched to",
  "current FLUXNET Shuttle site IDs as for Table 1 (scripts/collection_comparison_table.R).",
  "",
  "STEPS:",
  sprintf("  1. Marconi 2000 -- n = %d", actual_n[["marconi"]]),
  sprintf("  2. La Thuile 2007 -- n = %d", actual_n[["la_thuile"]]),
  sprintf("  3. FLUXNET2015 -- n = %d", actual_n[["fluxnet2015"]]),
  sprintf("  4. Current FLUXNET Shuttle snapshot -- n = %d", actual_n[["current"]]),
  "",
  "Site counts confirmed against Table 1 (data/snapshots/collection_sites_siteyears.csv):",
  sprintf("  Marconi 35, La Thuile 252, FLUXNET2015 212, Current 781 -- all matched (gate passed)."),
  "",
  paste0("SNAPSHOT: ", SNAP_FILE),
  "SOURCE: scripts/generate_fig_network_growth_slides.R (presentation-only; no manuscript figure)."
)
writeLines(legend_lines, file.path(SLIDE_DIR, "growth.legend.txt"))
msg("Saved: ", file.path(SLIDE_DIR, "growth.legend.txt"))

msg("=== Done: network growth slides ===")
