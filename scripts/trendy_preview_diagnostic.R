## scripts/trendy_preview_diagnostic.R
##
## Quick-look diagnostic: TRENDY v14 partial ensemble (8 models complete).
##
## Reads completed intermediate TIFs from data/external/trendy/derived/intermediate/
## Writes preview ensemble TIFs to   data/external/trendy/derived/preview/
## Writes figures to                  review/figures/representativeness/trendy_preview/
## Does NOT touch any outputs used by the running compute job (PID 2066).
##
## Run in the foreground. Expected runtime: 15-30 min.

source("renv/activate.R")
suppressPackageStartupMessages({
  library(terra)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(sf)
  library(rnaturalearth)
  library(scales)
})

cat("=== TRENDY v14 diagnostic preview (8-model partial ensemble) ===\n\n")

# ---- Configuration -----------------------------------------------------------
INTER_DIR   <- "data/external/trendy/derived/intermediate"
PREV_DIR    <- "data/external/trendy/derived/preview"
KG_PATH     <- "data/external/koppen_beck2023/1991_2020/koppen_geiger_0p5.tif"
SITE_CSV    <- "data/snapshots/site_biomass_cci_v7.csv"
METRICS_CSV <- "data/snapshots/representativeness_metrics.csv"
FIG_DIR     <- "review/figures/representativeness/trendy_preview"

NBP_LOW_CUT <- 5.0
ET_LOW_CUT  <- 5.0

MODELS_OK <- c("CABLE-POP", "CLASSIC", "CLM", "DLEM",
               "ED", "ELM", "ELM-FATES", "IBIS")
n_models  <- length(MODELS_OK)

dir.create(PREV_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR,  recursive = TRUE, showWarnings = FALSE)

# ---- Stat helpers (identical to compute script) ------------------------------
compute_detrended_sd <- function(r) {
  n <- nlyr(r); vals <- values(r); yr_idx <- seq_len(n)
  X <- cbind(1, yr_idx)
  P <- diag(n) - X %*% tcrossprod(solve(crossprod(X)), X)
  complete  <- rowSums(is.na(vals)) == 0L
  out_vals  <- rep(NA_real_, nrow(vals))
  if (any(complete)) {
    resids <- vals[complete, , drop = FALSE] %*% t(P)
    out_vals[complete] <- sqrt(rowSums(resids^2) / (n - 2L))
  }
  r_out <- r[[1L]]; values(r_out) <- out_vals; names(r_out) <- "detrended_sd"; r_out
}

compute_mean_abs <- function(r) {
  vals <- values(r); complete <- rowSums(is.na(vals)) == 0L
  out_vals <- rep(NA_real_, nrow(vals))
  if (any(complete))
    out_vals[complete] <- rowMeans(abs(vals[complete, , drop = FALSE]))
  r_out <- r[[1L]]; values(r_out) <- out_vals; names(r_out) <- "mean_abs"; r_out
}

compute_mean <- function(r) {
  vals <- values(r); complete <- rowSums(is.na(vals)) == 0L
  out_vals <- rep(NA_real_, nrow(vals))
  if (any(complete))
    out_vals[complete] <- rowMeans(vals[complete, , drop = FALSE])
  r_out <- r[[1L]]; values(r_out) <- out_vals; names(r_out) <- "mean"; r_out
}

# ---- Step 1b: per-model means for grid figures ------------------------------
cat("Loading intermediate TIFs and computing 34-yr means...\n")
nbp_means <- vector("list", n_models)
et_means  <- vector("list", n_models)
for (i in seq_along(MODELS_OK)) {
  mdl <- MODELS_OK[[i]]
  r_nbp <- rast(file.path(INTER_DIR, paste0(mdl, "_nbp_regridded.tif")))
  r_et  <- rast(file.path(INTER_DIR, paste0(mdl, "_evapotrans_regridded.tif")))
  # Use na.rm=TRUE here (grid visualisation only — ELM is missing 2023)
  nbp_means[[i]] <- app(r_nbp, fun = mean, na.rm = TRUE)
  et_means[[i]]  <- app(r_et,  fun = mean, na.rm = TRUE)
  names(nbp_means[[i]]) <- mdl
  names(et_means[[i]])  <- mdl
  cat("  ", mdl, "\n")
  rm(r_nbp, r_et); gc(verbose = FALSE)
}

# ---- Step 2: partial ensemble median maps -----------------------------------
cat("\nComputing partial ensemble maps...\n")

compute_preview_ensemble <- function(stat_key, var, stat_fn) {
  out_path <- file.path(PREV_DIR, paste0("preview_", stat_key, ".tif"))
  if (file.exists(out_path)) {
    cat("  SKIP (exists):", basename(out_path), "\n"); return(rast(out_path))
  }
  cat(" ", stat_key, "...\n")
  model_stats <- lapply(MODELS_OK, function(mdl) {
    r <- rast(file.path(INTER_DIR, paste0(mdl, "_", var, "_regridded.tif")))
    s <- stat_fn(r); names(s) <- mdl; rm(r); gc(verbose = FALSE); s
  })
  stk <- rast(model_stats)
  ens <- app(stk, fun = function(v) median(v, na.rm = TRUE))
  names(ens) <- stat_key
  writeRaster(ens, out_path, gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
  cat("  Saved:", basename(out_path), "\n"); ens
}

r_nee_iav    <- compute_preview_ensemble("nee_iav",    "nbp",        compute_detrended_sd)
r_et_iav     <- compute_preview_ensemble("et_iav",     "evapotrans", compute_detrended_sd)
r_nee_median <- compute_preview_ensemble("nee_median", "nbp",        compute_mean_abs)
r_et_median  <- compute_preview_ensemble("et_median",  "evapotrans", compute_mean)

# ---- Infrastructure for metrics and figures ---------------------------------
cat("\nLoading KG mask and site data...\n")
kg_05 <- rast(KG_PATH)
sites <- read_csv(SITE_CSV, show_col_types = FALSE)
n_sites <- nrow(sites)
cat(" ", n_sites, "sites\n")

cat("Computing cell areas...\n")
cell_areas_05 <- cellSize(kg_05, mask = TRUE, unit = "km")

world <- ne_countries(scale = "small", returnclass = "sf")

# Shared map theme
theme_map <- function() {
  theme_void(base_size = 9) +
    theme(
      panel.background  = element_rect(fill = "#d0e8f5", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      plot.title        = element_text(size = 9, face = "bold", margin = margin(b = 4)),
      plot.subtitle     = element_text(size = 7, colour = "grey40"),
      legend.position   = "bottom",
      legend.title      = element_text(size = 7),
      legend.text       = element_text(size = 6),
      legend.key.width  = unit(1.5, "cm"),
      legend.key.height = unit(0.35, "cm"),
      strip.text        = element_text(size = 7, face = "bold"),
      panel.border      = element_rect(fill = NA, colour = "grey60", linewidth = 0.3)
    )
}

# raster → tile-ready data frame
rast_to_df <- function(r, val_name = "value") {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- val_name
  df
}

# Single global map
make_single_map <- function(r, title, subtitle = NULL, fill_label,
                            fill_scale, limits = NULL) {
  df <- rast_to_df(mask(r, kg_05))
  ggplot() +
    geom_tile(data = df, aes(x = x, y = y, fill = value),
              width = 0.5, height = 0.5) +
    geom_sf(data = world, fill = NA, colour = "grey20",
            linewidth = 0.15, inherit.aes = FALSE) +
    fill_scale +
    coord_sf(xlim = c(-180, 180), ylim = c(-58, 83), expand = FALSE,
             default_crs = st_crs(4326)) +
    labs(title = title, subtitle = subtitle) +
    guides(fill = guide_colorbar(title = fill_label,
                                 barwidth = 10, barheight = 0.4,
                                 title.position = "top", title.hjust = 0.5)) +
    theme_map()
}

# ---- Figures 1–4: ensemble output maps --------------------------------------
cat("\nGenerating ensemble maps...\n")

tag <- paste0("(", n_models, "-model partial ensemble, 1990–2023)")

p1 <- make_single_map(
  r_nee_iav,
  title    = "NEE inter-annual variability",
  subtitle = paste("Detrended SD of annual NBP", tag),
  fill_label = "gC m⁻² yr⁻¹",
  fill_scale = scale_fill_viridis_c(option = "plasma", na.value = NA,
                                     limits = c(0, quantile(values(r_nee_iav), 0.98, na.rm=TRUE)),
                                     oob = squish)
)
ggsave(file.path(FIG_DIR, "preview_nee_iav.png"), p1,
       width = 10, height = 5.2, dpi = 150, bg = "white")
cat("  preview_nee_iav.png\n")

p2 <- make_single_map(
  r_et_iav,
  title    = "ET inter-annual variability",
  subtitle = paste("Detrended SD of annual ET", tag),
  fill_label = "mm yr⁻¹",
  fill_scale = scale_fill_viridis_c(option = "viridis", na.value = NA,
                                     limits = c(0, quantile(values(r_et_iav), 0.98, na.rm=TRUE)),
                                     oob = squish)
)
ggsave(file.path(FIG_DIR, "preview_et_iav.png"), p2,
       width = 10, height = 5.2, dpi = 150, bg = "white")
cat("  preview_et_iav.png\n")

p3 <- make_single_map(
  r_nee_median,
  title    = "NEE magnitude",
  subtitle = paste("Ensemble-median mean |NBP|", tag),
  fill_label = "gC m⁻² yr⁻¹",
  fill_scale = scale_fill_viridis_c(option = "mako", direction = -1, na.value = NA,
                                     limits = c(0, quantile(values(r_nee_median), 0.98, na.rm=TRUE)),
                                     oob = squish)
)
ggsave(file.path(FIG_DIR, "preview_nee_median.png"), p3,
       width = 10, height = 5.2, dpi = 150, bg = "white")
cat("  preview_nee_median.png\n")

p4 <- make_single_map(
  r_et_median,
  title    = "ET magnitude",
  subtitle = paste("Ensemble-median mean ET", tag),
  fill_label = "mm yr⁻¹",
  fill_scale = scale_fill_viridis_c(option = "turbo", na.value = NA,
                                     limits = c(0, quantile(values(r_et_median), 0.98, na.rm=TRUE)),
                                     oob = squish)
)
ggsave(file.path(FIG_DIR, "preview_et_median.png"), p4,
       width = 10, height = 5.2, dpi = 150, bg = "white")
cat("  preview_et_median.png\n")

# ---- Figure 5: per-model NBP mean grid ---------------------------------------
cat("Generating per-model NBP grid...\n")
nbp_df <- bind_rows(lapply(seq_along(MODELS_OK), function(i) {
  df <- rast_to_df(nbp_means[[i]]); df$model <- MODELS_OK[[i]]; df
}))
lim_nbp <- quantile(nbp_df$value, c(0.02, 0.98), na.rm = TRUE)

p5 <- ggplot() +
  geom_tile(data = nbp_df, aes(x = x, y = y, fill = value),
            width = 0.5, height = 0.5) +
  geom_sf(data = world, fill = NA, colour = "grey25",
          linewidth = 0.1, inherit.aes = FALSE) +
  scale_fill_gradient2(
    name = "gC m⁻² yr⁻¹",
    low = "#2166ac", mid = "white", high = "#b2182b",
    midpoint = 0, limits = lim_nbp, oob = squish,
    guide = guide_colorbar(barwidth = 8, barheight = 0.35,
                           title.position = "top", title.hjust = 0.5)
  ) +
  facet_wrap(~model, ncol = 4) +
  coord_sf(xlim = c(-180, 180), ylim = c(-58, 83), expand = FALSE,
           default_crs = st_crs(4326)) +
  labs(title = "Per-model NBP 34-yr mean (1990–2023)",
       subtitle = "8 models with complete intermediate TIFs  |  gC m⁻² yr⁻¹  |  colour limits = 2nd–98th pctile") +
  theme_map()

ggsave(file.path(FIG_DIR, "preview_model_nbp_grid.png"), p5,
       width = 16, height = 8, dpi = 150, bg = "white")
cat("  preview_model_nbp_grid.png\n")

# ---- Figure 6: per-model ET mean grid ----------------------------------------
cat("Generating per-model ET grid...\n")
et_df <- bind_rows(lapply(seq_along(MODELS_OK), function(i) {
  df <- rast_to_df(et_means[[i]]); df$model <- MODELS_OK[[i]]; df
}))
lim_et_hi <- quantile(et_df$value[et_df$value >= 0], 0.98, na.rm = TRUE)

p6 <- ggplot() +
  geom_tile(data = et_df[et_df$value >= 0, ],
            aes(x = x, y = y, fill = value),
            width = 0.5, height = 0.5) +
  geom_sf(data = world, fill = NA, colour = "grey25",
          linewidth = 0.1, inherit.aes = FALSE) +
  scale_fill_viridis_c(
    option = "viridis", na.value = NA,
    limits = c(0, lim_et_hi), oob = squish,
    guide = guide_colorbar(barwidth = 8, barheight = 0.35,
                           title = "mm yr⁻¹",
                           title.position = "top", title.hjust = 0.5)
  ) +
  facet_wrap(~model, ncol = 4) +
  coord_sf(xlim = c(-180, 180), ylim = c(-58, 83), expand = FALSE,
           default_crs = st_crs(4326)) +
  labs(title = "Per-model ET 34-yr mean (1990–2023)",
       subtitle = "8 models with complete intermediate TIFs  |  mm yr⁻¹  |  colour limits = 0–98th pctile") +
  theme_map()

ggsave(file.path(FIG_DIR, "preview_model_et_grid.png"), p6,
       width = 16, height = 8, dpi = 150, bg = "white")
cat("  preview_model_et_grid.png\n")

# ---- Steps 3-4: representativeness metrics ----------------------------------
cat("\n=== Computing representativeness metrics ===\n")

process_axis_preview <- function(r_map, axis_key, low_cut, unit_str) {
  # Simplified version: pixel-count quantiles (not area-weighted).
  # Fine for a diagnostic preview; J/H scores will be approximate.
  cat("\n--- Axis:", axis_key, "---\n")
  r_land <- mask(r_map, kg_05)

  v <- as.vector(values(r_land))
  v <- v[!is.na(v)]
  v[v < 0] <- 0
  n_total <- length(v)

  low_frac <- sum(v < low_cut) / n_total
  cat("  Near-zero (<", low_cut, unit_str, "):", round(low_frac * 100, 1), "% of pixels\n")
  if (low_frac < 0.01) low_cut <- low_cut * 0.5
  if (low_frac > 0.70) low_cut <- low_cut * 2.0

  v_veg    <- v[v >= low_cut]
  q_breaks <- round(quantile(v_veg, probs = seq(1, 5) / 6, na.rm = TRUE,
                              names = FALSE), 2)
  cat("  Q-breaks (", unit_str, "):", paste(q_breaks, collapse = ", "), "\n")

  BIN_BREAKS <- c(-Inf, low_cut, q_breaks, Inf)
  assign_bin <- function(x) {
    b <- findInterval(x, BIN_BREAKS[-length(BIN_BREAKS)])
    b[b < 1L] <- 1L; b[b > 7L] <- 7L; b
  }
  p_global <- tabulate(assign_bin(v), nbins = 7) / n_total

  coords_mat <- as.matrix(sites[, c("location_long", "location_lat")])
  ext_df     <- terra::extract(r_land, coords_mat, method = "simple")
  # extract() with a matrix returns no ID column — values are in column 1
  site_vals  <- ext_df[, 1]
  site_vals[is.na(site_vals) | site_vals < 0] <- 0
  q_net <- tabulate(assign_bin(site_vals), nbins = 7) / n_sites

  J <- sum(pmin(p_global, q_net)) / sum(pmax(p_global, q_net))
  H <- (1 / sqrt(2)) * sqrt(sum((sqrt(p_global) - sqrt(q_net))^2))
  cat("  J =", round(J, 4), "  H =", round(H, 4), "\n")
  list(J = J, H = H)
}

res_nee_iav    <- process_axis_preview(r_nee_iav,    "nee_iav",    NBP_LOW_CUT, "gC m-2 yr-1")
res_et_iav     <- process_axis_preview(r_et_iav,     "et_iav",     ET_LOW_CUT,  "mm yr-1")
res_nee_median <- process_axis_preview(r_nee_median, "nee_median", NBP_LOW_CUT, "gC m-2 yr-1")
res_et_median  <- process_axis_preview(r_et_median,  "et_median",  ET_LOW_CUT,  "mm yr-1")

# ---- Summary -----------------------------------------------------------------
cat("\n\n========================================================\n")
cat("METRIC SUMMARY —", n_models, "model partial ensemble\n")
cat("========================================================\n")
cat(sprintf("  %-22s  J = %.4f   H = %.4f\n", "NEE IAV",    res_nee_iav$J,    res_nee_iav$H))
cat(sprintf("  %-22s  J = %.4f   H = %.4f\n", "ET IAV",     res_et_iav$J,     res_et_iav$H))
cat(sprintf("  %-22s  J = %.4f   H = %.4f\n", "NEE median", res_nee_median$J, res_nee_median$H))
cat(sprintf("  %-22s  J = %.4f   H = %.4f\n", "ET median",  res_et_median$J,  res_et_median$H))

cat("\n--- Existing axes (for comparison) ---\n")
existing <- read_csv(METRICS_CSV, show_col_types = FALSE)
for (i in seq_len(nrow(existing))) {
  cat(sprintf("  %-48s  J = %.4f   H = %.4f\n",
    paste0(existing$axis[i], " (", existing$aggregation_level[i], ")"),
    existing$weighted_jaccard[i], existing$hellinger_distance[i]))
}

cat("\nFigures written to:", FIG_DIR, "\n")
cat("Done.\n")
