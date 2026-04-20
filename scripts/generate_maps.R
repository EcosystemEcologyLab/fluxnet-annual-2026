## scripts/generate_maps.R
## Generates all 9 canonical UN subregion choropleth map figures.
## Run from repo root: Rscript scripts/generate_maps.R
##
## Outputs (review/figures/maps/):
##   fig_map01_ShuttleFull.png           — FLUXNET Shuttle 2025 (full network)
##   fig_map02_Marconi.png               — Marconi 2000
##   fig_map03_LaThuile.png              — La Thuile 2007
##   fig_map04_FLUXNET2015.png           — FLUXNET2015
##   fig_map05_ShuttleSnapshot2000.png   — Shuttle snapshot 2000
##   fig_map06_ShuttleSnapshot2007.png   — Shuttle snapshot 2007
##   fig_map07_ShuttleSnapshot2015.png   — Shuttle snapshot 2015
##   fig_map08_HistoricalStack.png       — Map02 / Map03 / Map04 stacked
##   fig_map09_ShuttleSnapshotsStack.png — Map05 / Map06 / Map07 stacked
##
## NOTE on data sources: Map02-04 use non-Shuttle historical site lists for
## development/comparison purposes only — clearly labelled per CLAUDE.md §1.
##
## Architecture mirrors scripts/generate_whittaker.R:
##   - Shared scale_breaks computed once from all 7 distinct datasets
##   - Single core function fig_map_historical() for all panels
##   - Stack figures assembled via .make_map_stack()

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/figures/fig_maps.R")

library(dplyr)
library(readr)
library(patchwork)
library(ggplot2)
library(sf)
library(countrycode)

check_pipeline_config()

out_dir <- file.path("review", "figures", "maps")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Load data ---------------------------------------------------------------

snap_file <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)[[1]]
message("Using snapshot: ", snap_file)
shuttle_meta <- readr::read_csv(snap_file, show_col_types = FALSE)

# NOTE: Historical site lists are comparison-only figures — non-Shuttle data.
# See CLAUDE.md §1 for data source policy.
sites_marconi     <- readr::read_csv("data/snapshots/sites_marconi_clean.csv",
                                     show_col_types = FALSE)
sites_la_thuile   <- readr::read_csv("data/snapshots/sites_la_thuile_clean.csv",
                                     show_col_types = FALSE)
sites_fluxnet2015 <- readr::read_csv("data/snapshots/sites_fluxnet2015_clean.csv",
                                     show_col_types = FALSE)

# ---- Compute shared colour scale breaks (A3) ---------------------------------
# Pool per-subregion site counts from all 7 distinct datasets (Map01-07).
# Map08 and Map09 are stacks that reuse the same data — excluded to avoid
# double-counting. Take the 95th percentile to clip the upper end and prevent
# highly instrumented subregions (e.g. Northern America) from dominating.

message("\nComputing shared scale breaks from all 7 distinct datasets...")

sf::sf_use_s2(FALSE)

# Build subregion polygons once for break computation
.countries_brk <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf"
) |> sf::st_make_valid()

.countries_brk$subregion <- countrycode::countrycode(
  .countries_brk$name_long, "country.name", "un.regionsub.name", warn = FALSE
)

.subregions_brk <- .countries_brk |>
  dplyr::filter(!is.na(.data$subregion)) |>
  dplyr::group_by(.data$subregion) |>
  dplyr::summarise(
    geometry = suppressWarnings(sf::st_union(.data$geometry)),
    .groups  = "drop"
  ) |>
  sf::st_make_valid()

# Helper: return per-subregion site counts for a given site list
.count_subregion <- function(meta, yr_cut = NULL) {
  if (!is.null(yr_cut) && "first_year" %in% names(meta)) {
    meta <- dplyr::filter(meta,
                          as.integer(.data$first_year) <= as.integer(yr_cut))
  }
  sites <- meta |>
    dplyr::filter(!is.na(.data$location_lat), !is.na(.data$location_long),
                  dplyr::between(.data$location_lat,   -90,  90),
                  dplyr::between(.data$location_long, -180, 180)) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE)
  if (nrow(sites) == 0L) return(integer(0L))
  sites_sf <- sf::st_as_sf(sites,
                            coords = c("location_long", "location_lat"),
                            crs = 4326, remove = FALSE)
  joined <- sf::st_join(sites_sf, .subregions_brk["subregion"], left = TRUE)
  na_idx <- which(is.na(joined$subregion))
  if (length(na_idx) > 0L) {
    nearest <- sf::st_nearest_feature(joined[na_idx, ], .subregions_brk)
    joined$subregion[na_idx] <- .subregions_brk$subregion[nearest]
  }
  tbl <- sf::st_drop_geometry(joined)
  dplyr::count(tbl, .data$subregion, name = "n") |> dplyr::pull(.data$n)
}

all_counts <- c(
  .count_subregion(shuttle_meta,      NULL),   # Map01: Shuttle full
  .count_subregion(sites_marconi,     NULL),   # Map02: Marconi
  .count_subregion(sites_la_thuile,   NULL),   # Map03: La Thuile
  .count_subregion(sites_fluxnet2015, NULL),   # Map04: FLUXNET2015
  .count_subregion(shuttle_meta,      2000L),  # Map05: Shuttle snapshot 2000
  .count_subregion(shuttle_meta,      2007L),  # Map06: Shuttle snapshot 2007
  .count_subregion(shuttle_meta,      2015L)   # Map07: Shuttle snapshot 2015
)

p95          <- quantile(all_counts, 0.95, na.rm = TRUE)
scale_breaks <- unique(sort(c(0L, 5L, 15L, 30L, round(p95 / 2), round(p95))))

message("Shared scale breaks: ", paste(scale_breaks, collapse = ", "))

# ---- Helper: save a single map panel -----------------------------------------
save_map <- function(p, name, style = MAP_STYLE) {
  path <- file.path(out_dir, paste0(name, ".png"))
  ggplot2::ggsave(path, plot = p,
                  width  = style$width_in,
                  height = style$height_in,
                  units  = "in", dpi = 150, bg = "white")
  message("Saved: ", path)
  invisible(path)
}

# ---- Stack helper ------------------------------------------------------------
# Assembles three map panels into a stacked figure.
# Layout rules:
#   - Legend on bottom panel only — suppressed on top and mid
#   - Inset text on each panel (already set by fig_map_historical())
#   - Zero vertical margins between panels so they touch
.make_map_stack <- function(p_top, p_mid, p_bot, style = MAP_STYLE) {
  no_legend <- ggplot2::theme(
    legend.position = "none",
    plot.margin     = ggplot2::margin(0, 5, 0, 5)
  )
  bot_theme <- ggplot2::theme(
    plot.margin = ggplot2::margin(0, 5, 0, 5)
  )

  patchwork::wrap_plots(
    p_top + no_legend,
    p_mid + no_legend,
    p_bot + bot_theme,
    ncol = 1
  )
}

# ============================================================
# Map01 — FLUXNET Shuttle 2025 (full network)
# ============================================================
message("\n── Map01: FLUXNET Shuttle 2025 ──")
map01 <- fig_map_historical(
  site_meta    = shuttle_meta,
  detail_label = "FLUXNET Shuttle 2025",
  year_cutoff  = NULL,
  scale_breaks = scale_breaks
)
save_map(map01, "fig_map01_ShuttleFull")

# ============================================================
# Map02 — Marconi 2000
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
message("\n── Map02: Marconi 2000 ──")
map02 <- fig_map_historical(
  site_meta    = sites_marconi,
  detail_label = "Marconi 2000",
  year_cutoff  = NULL,
  scale_breaks = scale_breaks
)
save_map(map02, "fig_map02_Marconi")

# ============================================================
# Map03 — La Thuile 2007
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
message("\n── Map03: La Thuile 2007 ──")
map03 <- fig_map_historical(
  site_meta    = sites_la_thuile,
  detail_label = "La Thuile 2007",
  year_cutoff  = NULL,
  scale_breaks = scale_breaks
)
save_map(map03, "fig_map03_LaThuile")

# ============================================================
# Map04 — FLUXNET2015
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
message("\n── Map04: FLUXNET2015 ──")
map04 <- fig_map_historical(
  site_meta    = sites_fluxnet2015,
  detail_label = "FLUXNET2015",
  year_cutoff  = NULL,
  scale_breaks = scale_breaks
)
save_map(map04, "fig_map04_FLUXNET2015")

# ============================================================
# Map05 — Shuttle snapshot 2000
# ============================================================
message("\n── Map05: Shuttle snapshot 2000 ──")
map05 <- fig_map_historical(
  site_meta    = shuttle_meta,
  detail_label = "Shuttle snapshot 2000",
  year_cutoff  = 2000L,
  scale_breaks = scale_breaks
)
save_map(map05, "fig_map05_ShuttleSnapshot2000")

# ============================================================
# Map06 — Shuttle snapshot 2007
# ============================================================
message("\n── Map06: Shuttle snapshot 2007 ──")
map06 <- fig_map_historical(
  site_meta    = shuttle_meta,
  detail_label = "Shuttle snapshot 2007",
  year_cutoff  = 2007L,
  scale_breaks = scale_breaks
)
save_map(map06, "fig_map06_ShuttleSnapshot2007")

# ============================================================
# Map07 — Shuttle snapshot 2015
# ============================================================
message("\n── Map07: Shuttle snapshot 2015 ──")
map07 <- fig_map_historical(
  site_meta    = shuttle_meta,
  detail_label = "Shuttle snapshot 2015",
  year_cutoff  = 2015L,
  scale_breaks = scale_breaks
)
save_map(map07, "fig_map07_ShuttleSnapshot2015")

# ============================================================
# Map08 — historical datasets stack (Map02 / Map03 / Map04)
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
message("\n── Map08: historical datasets stack (Map02/03/04) ──")
map08 <- .make_map_stack(map02, map03, map04)
path08 <- file.path(out_dir, "fig_map08_HistoricalStack.png")
ggplot2::ggsave(
  path08,
  plot   = map08,
  width  = MAP_STYLE$width_in,
  height = MAP_STYLE$height_in * 3,
  units  = "in", dpi = 150, bg = "white"
)
message("Saved: ", path08)

# ============================================================
# Map09 — Shuttle snapshots stack (Map05 / Map06 / Map07)
# ============================================================
message("\n── Map09: Shuttle snapshots stack (Map05/06/07) ──")
map09 <- .make_map_stack(map05, map06, map07)
path09 <- file.path(out_dir, "fig_map09_ShuttleSnapshotsStack.png")
ggplot2::ggsave(
  path09,
  plot   = map09,
  width  = MAP_STYLE$width_in,
  height = MAP_STYLE$height_in * 3,
  units  = "in", dpi = 150, bg = "white"
)
message("Saved: ", path09)

message("\nDone. All 9 map figures generated: Map01-09.")
