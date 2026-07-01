## scripts/generate_point_maps.R
## Generate point-based network maps for fig_03 and fig_04 candidate figures.
## Produces four PNGs: white-background and aridity-backdrop variants of each.
##
## Outputs (review/figures/maps_point/):
##   fig_03_map_current.png           — current 759-site network, white backdrop
##   fig_03_map_current_aridity.png   — current 759-site network, aridity backdrop
##   fig_04_map_snapshots.png         — 4-panel historical datasets, white backdrop
##   fig_04_map_snapshots_aridity.png — 4-panel historical datasets, aridity backdrop
##
## Candidate files updated in review/figures/candidates/:
##   fig_03_map_current.png           (replaces fig_03_choropleth_current.png)
##   fig_03_map_current_aridity.png   (new)
##   fig_04_map_snapshots.png         (replaces fig_04_choropleth_snapshots.png)
##   fig_04_map_snapshots_aridity.png (new)
##
## NOTE: Marconi, La Thuile, and FLUXNET2015 panels are comparison figures
## using non-Shuttle data — clearly labelled per CLAUDE.md §1.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()
source("R/plot_constants.R")
source("R/figures/fig_maps.R")

library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(sf)

out_dir  <- file.path("review", "figures", "maps_point")
cand_dir <- file.path("review", "figures", "candidates")
fs::dir_create(out_dir)

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

message("Sites loaded: Shuttle=", nrow(shuttle_meta),
        " Marconi=", nrow(sites_marconi),
        " LaThuile=", nrow(sites_la_thuile),
        " FLUXNET2015=", nrow(sites_fluxnet2015))

# ---- Load aridity raster once (shared by both aridity variants) --------------
message("\nLoading and aggregating aridity raster (0.2 degree target) ...")
aridity_df <- .aridity_raster_df(target_res = 0.2)
message("  Aridity data: ", format(nrow(aridity_df), big.mark = ","),
        " cells after aggregation")

# ---- fig_03: current 759-site network ----------------------------------------
message("\n── fig_03: current network (white) ──")
p03_white <- fig_map_point_network(
  metadata   = shuttle_meta,
  backdrop   = "white",
  pt_size    = 1.0,     # ~half the original 2.0 (draft-manuscript style)
  pt_alpha   = 0.65,    # semi-transparent so overlap density is visible (Europe, N. America)
  title      = NULL
) +
  ggplot2::labs(subtitle = NULL)   # draft-manuscript style: no title/subtitle (n reported in legend)
path03w <- file.path(out_dir, "fig_03_map_current.png")
ggplot2::ggsave(path03w, plot = p03_white, width = 3.5, height = 3.5,
                units = "in", dpi = 300, bg = "white")
message("  Saved: ", path03w)

message("── fig_03: current network (aridity) ──")
p03_arid <- fig_map_point_network(
  metadata   = shuttle_meta,
  backdrop   = "aridity",
  aridity_df = aridity_df,
  pt_size    = 2.0,
  title      = "FLUXNET Shuttle 2025 — current network"
)
path03a <- file.path(out_dir, "fig_03_map_current_aridity.png")
ggplot2::ggsave(path03a, plot = p03_arid, width = 12, height = 7,
                units = "in", dpi = 150, bg = "white")
message("  Saved: ", path03a)

# ---- fig_04: four-panel historical datasets ----------------------------------
message("\n── fig_04: historical snapshots (white) ──")
p04_white <- fig_map_point_snapshots(
  snap_meta         = shuttle_meta,
  sites_marconi     = sites_marconi,
  sites_la_thuile   = sites_la_thuile,
  sites_fluxnet2015 = sites_fluxnet2015,
  backdrop          = "white",
  pt_size           = 1.8,
  ncol              = 1L
)
path04w <- file.path(out_dir, "fig_04_map_snapshots.png")
ggplot2::ggsave(path04w, plot = p04_white, width = 12, height = 22,
                units = "in", dpi = 150, bg = "white")
message("  Saved: ", path04w)

message("── fig_04: historical snapshots (aridity) ──")
p04_arid <- fig_map_point_snapshots(
  snap_meta         = shuttle_meta,
  sites_marconi     = sites_marconi,
  sites_la_thuile   = sites_la_thuile,
  sites_fluxnet2015 = sites_fluxnet2015,
  backdrop          = "aridity",
  aridity_df        = aridity_df,
  pt_size           = 1.8,
  ncol              = 1L
)
path04a <- file.path(out_dir, "fig_04_map_snapshots_aridity.png")
ggplot2::ggsave(path04a, plot = p04_arid, width = 12, height = 22,
                units = "in", dpi = 150, bg = "white")
message("  Saved: ", path04a)

# ---- Copy to candidates ------------------------------------------------------
message("\nCopying to review/figures/candidates/ ...")

# White variants replace the existing choropleth candidates
file.copy(path03w, file.path(cand_dir, "fig_03_map_current.png"),           overwrite = TRUE)
file.copy(path03a, file.path(cand_dir, "fig_03_map_current_aridity.png"),   overwrite = TRUE)
file.copy(path04w, file.path(cand_dir, "fig_04_map_snapshots.png"),         overwrite = TRUE)
file.copy(path04a, file.path(cand_dir, "fig_04_map_snapshots_aridity.png"), overwrite = TRUE)

# Remove superseded choropleth candidates
if (file.exists(file.path(cand_dir, "fig_03_choropleth_current.png")))
  file.remove(file.path(cand_dir, "fig_03_choropleth_current.png"))
if (file.exists(file.path(cand_dir, "fig_04_choropleth_snapshots.png")))
  file.remove(file.path(cand_dir, "fig_04_choropleth_snapshots.png"))

message("Done. Four candidate figures written:")
message("  ", file.path(cand_dir, "fig_03_map_current.png"))
message("  ", file.path(cand_dir, "fig_03_map_current_aridity.png"))
message("  ", file.path(cand_dir, "fig_04_map_snapshots.png"))
message("  ", file.path(cand_dir, "fig_04_map_snapshots_aridity.png"))
