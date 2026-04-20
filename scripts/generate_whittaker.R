## scripts/generate_whittaker.R
## Generates all 9 canonical Whittaker biome figures.
## Run from repo root: Rscript scripts/generate_whittaker.R
##
## Outputs (review/figures/whittaker/):
##   fig_whit01.png  — FLUXNET Shuttle 2025 (full network)
##   fig_whit02.png  — Marconi 2000
##   fig_whit03.png  — La Thuile 2007
##   fig_whit04.png  — FLUXNET2015
##   fig_whit05.png  — Shuttle snapshot 2000
##   fig_whit06.png  — Shuttle snapshot 2007
##   fig_whit07.png  — Shuttle snapshot 2015
##   fig_whit08_historical_stack.png   — Whit02 / Whit03 / Whit04 stacked
##   fig_whit09_shuttle_snapshots_stack.png — Whit05 / Whit06 / Whit07 stacked
##
## Replaces: scripts/generate_whittaker_figures.R
##           scripts/generate_worldclim_figures.R
## (those scripts moved to scripts/legacy/)

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/figures/fig_climate.R")

library(dplyr)
library(readr)
library(patchwork)
library(ggplot2)

check_pipeline_config()

out_dir <- file.path("review", "figures", "whittaker")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Load data ---------------------------------------------------------------
processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")
data_yy <- readRDS(file.path(processed_dir, "flux_data_converted_yy.rds"))

# Latest Shuttle snapshot for full-network site_meta
snap_file <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)[[1]]
shuttle_meta <- readr::read_csv(snap_file, show_col_types = FALSE)

# Historical site lists
sites_marconi    <- readr::read_csv("data/snapshots/sites_marconi_clean.csv",
                                    show_col_types = FALSE)
sites_la_thuile  <- readr::read_csv("data/snapshots/sites_la_thuile_clean.csv",
                                    show_col_types = FALSE)
sites_fluxnet2015 <- readr::read_csv("data/snapshots/sites_fluxnet2015_clean.csv",
                                     show_col_types = FALSE)

# ---- Compute shared NEE limits from full Shuttle data once ------------------
# All panels use the same scale so figures are directly comparable.
nee_q   <- quantile(data_yy$NEE_VUT_REF, probs = c(0.05, 0.95), na.rm = TRUE)
nee_max <- max(abs(nee_q))
shared_style <- WHITTAKER_STYLE
shared_style$nee_lims <- c(-nee_max, nee_max)

message("Shared NEE colour limits: [",
        round(shared_style$nee_lims[1], 1), ", ",
        round(shared_style$nee_lims[2], 1), "] gC m\u207b\u00b2 yr\u207b\u00b9")

# ---- Helper: save a single Whittaker panel ----------------------------------
save_whit <- function(p, name, style = shared_style) {
  path <- file.path(out_dir, paste0(name, ".png"))
  ggplot2::ggsave(path, plot = p,
                  width = style$width_in, height = style$height_in,
                  units = "in", dpi = 150, bg = "white")
  message("Saved: ", path)
  invisible(path)
}

# ============================================================
# Whit01 — FLUXNET Shuttle 2025 (full network, all years)
# ============================================================
message("\n── Whit01: FLUXNET Shuttle 2025 ──")
whit01 <- fig_whittaker_worldclim(
  data_yy      = data_yy,
  site_meta    = shuttle_meta,
  detail_label = "FLUXNET Shuttle 2025",
  style        = shared_style
)
save_whit(whit01, "fig_whit01")

# ============================================================
# Whit02 — Marconi 2000
# ============================================================
message("\n── Whit02: Marconi 2000 ──")
whit02 <- fig_whittaker_worldclim(
  data_yy      = data_yy,
  site_meta    = sites_marconi,
  detail_label = "Marconi 2000",
  style        = shared_style
)
save_whit(whit02, "fig_whit02")

# ============================================================
# Whit03 — La Thuile 2007
# ============================================================
message("\n── Whit03: La Thuile 2007 ──")
whit03 <- fig_whittaker_worldclim(
  data_yy      = data_yy,
  site_meta    = sites_la_thuile,
  detail_label = "La Thuile 2007",
  style        = shared_style
)
save_whit(whit03, "fig_whit03")

# ============================================================
# Whit04 — FLUXNET2015
# ============================================================
message("\n── Whit04: FLUXNET2015 ──")
whit04 <- fig_whittaker_worldclim(
  data_yy      = data_yy,
  site_meta    = sites_fluxnet2015,
  detail_label = "FLUXNET2015",
  style        = shared_style
)
save_whit(whit04, "fig_whit04")

# ============================================================
# Whit05 — Shuttle snapshot 2000
# ============================================================
message("\n── Whit05: Shuttle snapshot 2000 ──")
whit05 <- fig_whittaker_worldclim(
  data_yy      = data_yy,
  site_meta    = shuttle_meta,
  year_cutoff  = 2000L,
  detail_label = "Shuttle snapshot 2000",
  style        = shared_style
)
save_whit(whit05, "fig_whit05")

# ============================================================
# Whit06 — Shuttle snapshot 2007
# ============================================================
message("\n── Whit06: Shuttle snapshot 2007 ──")
whit06 <- fig_whittaker_worldclim(
  data_yy      = data_yy,
  site_meta    = shuttle_meta,
  year_cutoff  = 2007L,
  detail_label = "Shuttle snapshot 2007",
  style        = shared_style
)
save_whit(whit06, "fig_whit06")

# ============================================================
# Whit07 — Shuttle snapshot 2015
# ============================================================
message("\n── Whit07: Shuttle snapshot 2015 ──")
whit07 <- fig_whittaker_worldclim(
  data_yy      = data_yy,
  site_meta    = shuttle_meta,
  year_cutoff  = 2015L,
  detail_label = "Shuttle snapshot 2015",
  style        = shared_style
)
save_whit(whit07, "fig_whit07")

# ============================================================
# Stacked figure helpers
# Layout rules for both Whit08 and Whit09:
#   - Legend on top panel only; suppressed on panels 2 and 3
#   - No space between panels
#   - Single x-axis label at bottom; single y-axis label centred on left
# ============================================================

.make_stack <- function(p_top, p_mid, p_bot) {
  # Top panel: keep legend at upper-left (from shared_style$legend_pos)
  p1 <- p_top

  # Middle and bottom panels: no legend, no x-axis title (collected by patchwork)
  p2 <- p_mid + ggplot2::theme(legend.position = "none")
  p3 <- p_bot + ggplot2::theme(legend.position = "none")

  patchwork::wrap_plots(p1, p2, p3, ncol = 1) +
    patchwork::plot_layout(axes = "collect") &
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 0, 5))
}

# ============================================================
# Whit08 — historical stack (Marconi / La Thuile / FLUXNET2015)
# ============================================================
message("\n── Whit08: historical stack (Whit02 / Whit03 / Whit04) ──")
whit08 <- .make_stack(whit02, whit03, whit04)

path08 <- file.path(out_dir, "fig_whit08_historical_stack.png")
ggplot2::ggsave(path08, plot = whit08,
                width  = shared_style$width_in,
                height = shared_style$height_in * 3,
                units  = "in", dpi = 150, bg = "white")
message("Saved: ", path08)

# ============================================================
# Whit09 — Shuttle snapshot stack (2000 / 2007 / 2015)
# ============================================================
message("\n── Whit09: Shuttle snapshot stack (Whit05 / Whit06 / Whit07) ──")
whit09 <- .make_stack(whit05, whit06, whit07)

path09 <- file.path(out_dir, "fig_whit09_shuttle_snapshots_stack.png")
ggplot2::ggsave(path09, plot = whit09,
                width  = shared_style$width_in,
                height = shared_style$height_in * 3,
                units  = "in", dpi = 150, bg = "white")
message("Saved: ", path09)

# ============================================================
# Copy canonical outputs to review/figures/candidates/
# fig_05 → fig_whit01 (Shuttle current)
# fig_06 → fig_whit09 (Shuttle snapshot stack)
# ============================================================
cand_dir <- file.path("review", "figures", "candidates")
if (!dir.exists(cand_dir)) dir.create(cand_dir, recursive = TRUE)

file.copy(file.path(out_dir, "fig_whit01.png"),
          file.path(cand_dir, "fig_05_whittaker_current.png"),
          overwrite = TRUE)
message("Copied fig_whit01 → candidates/fig_05_whittaker_current.png")

file.copy(path09,
          file.path(cand_dir, "fig_06_whittaker_snapshots.png"),
          overwrite = TRUE)
message("Copied fig_whit09 → candidates/fig_06_whittaker_snapshots.png")

message("\nAll 9 Whittaker figures done.")
