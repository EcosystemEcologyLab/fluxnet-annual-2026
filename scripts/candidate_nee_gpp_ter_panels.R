## candidate_nee_gpp_ter_panels.R
##
## Task 3 candidate mock-ups: renders the production NEE representativeness
## axis (scripts/figure_representativeness_nee_signed.R, already committed to
## data/snapshots/) as standalone Fig-4-style panels and a Fig-5 trajectory
## line, and builds matching (not yet production) GPP/TER mock-up panels.
## Writes only to review/figures/candidates/ -- never overwrites committed
## figures in review/figures/representativeness/.
##
## NEE (reused from data/snapshots/, no recomputation):
##   Geo  = S3 ensemble-median flux-based NEE (ra+rh-gpp), 1991-2020 mean
##   Data = Step 3 tower annual NEE, VUT->CUT fallback
##   5-bin signed scheme (near-zero +/-h, 3 sink quantile bins, 1 source bin)
##
## GPP/TER (computed here, mock-up only -- not written to data/snapshots/):
##   Geo  = model gpp (GPP) / ra+rh (TER), same 17-model 1991-2020 ensemble
##          median as the NEE axis, reusing the SAME cached per-model native
##          annual stacks (data/external/trendy/derived/intermediate/) --
##          no netCDF re-processing.
##   Data = tower GPP_{NT,DT}_{VUT,CUT}_REF / RECO_{NT,DT}_{VUT,CUT}_REF,
##          Step 3 annual method, SAME per-site VUT/CUT choice as NEE, plus
##          an INDEPENDENT per-site NT->DT fallback (pattern from
##          scripts/diagnostics/flux_tower_model_distributions.R).
##   5 equal-area quantile bins of the global field, no near-zero bin (GPP
##   and TER are always positive at the 1991-2020 ensemble-median scale, so
##   there is no sink/source split to make).
##
## Panel style: single-panel version of make_panel_single() in
## figure_representativeness_summary.R (log2 sampling-ratio bars). Trajectory
## style: single-line-added version of make_traj_no_bars() (Fig 007 style).
## Both reproduced here rather than sourced, since figure_representativeness_
## summary.R has side effects (regenerates the committed Figs 001-010) if run.

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(duckdb)
  library(lubridate)
  library(ggplot2)
  library(fs)
})

source("R/pipeline_config.R")
check_pipeline_config()

SNAP_DIR    <- "data/snapshots"
DERIVED_DIR <- "data/external/trendy/derived"
INTER_DIR   <- file.path(DERIVED_DIR, "intermediate")
KG_PATH     <- "data/external/koppen_beck2023/1991_2020/koppen_geiger_0p5.tif"
OUT_DIR     <- "review/figures/candidates"
fs::dir_create(OUT_DIR)
SITE_CSV    <- file.path(SNAP_DIR, "site_biomass_cci_v7.csv")
METRICS_CSV <- file.path(SNAP_DIR, "representativeness_metrics.csv")
QC_THRESH_MM <- 0.80
WIN_START <- 1991L; WIN_END <- 2020L

MODELS_TARGET <- c("CABLE-POP", "CLASSIC", "CLM", "DLEM", "ED", "ELM", "ELM-FATES",
                    "IBIS", "ISAM", "JULES-ES", "LPJ-GUESS", "LPJml", "LPJwsl",
                    "LPX-Bern", "ORCHIDEE", "TEM", "VISIT-UT")  # 17 models, matches
                    # scripts/diagnostics/nee_corrected_axis.R's MODELS_TARGET

TARGET <- rast(nrows = 360L, ncols = 720L, xmin = -180, xmax = 180,
                ymin = -90, ymax = 90, crs = "EPSG:4326")

LOG_START <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE  <- file.path("logs", paste0("candidate_nee_gpp_ter_panels_", LOG_START, ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output")
sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== candidate_nee_gpp_ter_panels.R ===")
msg("Log: ", LOG_FILE)

current_sites <- read_csv(SITE_CSV, show_col_types = FALSE) |>
  select(site_id, location_lat, location_long) |> distinct(site_id, .keep_all = TRUE)
n_sites <- nrow(current_sites)

## Write a short candidates/-convention .txt note alongside a .png, matching
## the existing review/figures/candidates/<name>.txt precedent.
write_note <- function(png_path, lines) {
  txt_path <- sub("\\.png$", ".txt", png_path)
  writeLines(lines, txt_path)
  msg("  Note: ", txt_path)
}

# ============================================================================
# SHARED PANEL / TRAJECTORY PLOTTING (reproduced from figure_representativeness_
# summary.R's make_panel_single() / make_traj_no_bars(); see header note)
# ============================================================================

LOG2_MAX    <- log2(5)
LOG2_BREAKS <- c(-LOG2_MAX, -1, 0, 1, LOG2_MAX)
LOG2_LABELS <- c("1/5×","1/2×","1×","2×","5×")
LOG2_XLIM   <- c(-LOG2_MAX - 0.25, LOG2_MAX + 0.25)

base_theme <- theme_minimal(base_size = 9) +
  theme(
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.ticks        = element_line(colour = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.background = element_rect(fill = "white", colour = NA)
  )

make_candidate_panel <- function(df, j_val, title_str, bin_colors) {
  df <- df |>
    mutate(
      sampling_ratio = if_else(global_land_fraction > 0 & site_frac > 0,
                                site_frac / global_land_fraction, NA_real_),
      log2_sr = if_else(!is.na(sampling_ratio), log2(sampling_ratio), NA_real_),
      log2_sr_clip = pmax(pmin(coalesce(log2_sr, 0), LOG2_MAX), -LOG2_MAX),
      truncated = !is.na(log2_sr) & abs(log2_sr) > LOG2_MAX,
      annot_label = case_when(
        truncated & log2_sr > 0 ~ paste0(sprintf("%.1f", sampling_ratio), "×"),
        truncated & log2_sr < 0 ~ paste0("1/", sprintf("%.1f", 1 / sampling_ratio), "×"),
        TRUE ~ NA_character_
      ),
      annot_x = case_when(
        truncated & log2_sr > 0 ~  LOG2_MAX - 0.08,
        truncated & log2_sr < 0 ~ -LOG2_MAX + 0.08,
        TRUE ~ NA_real_
      ),
      annot_hjust = case_when(
        truncated & log2_sr > 0 ~ 1, truncated & log2_sr < 0 ~ 0, TRUE ~ 0.5
      ),
      bin_label = factor(bin_label, levels = bin_label)
    )

  p <- ggplot(df, aes(x = log2_sr_clip, y = bin_label, fill = bin_label)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_col(width = 0.72, na.rm = TRUE, show.legend = FALSE, colour = "black", linewidth = 0.25) +
    scale_fill_manual(values = bin_colors) +
    scale_x_continuous(limits = LOG2_XLIM, breaks = LOG2_BREAKS, labels = LOG2_LABELS,
                        expand = expansion(mult = 0), name = "Sampling ratio",
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    scale_y_discrete(name = NULL) +
    labs(title = title_str) +
    base_theme +
    theme(axis.text.y = element_text(size = 7.5, margin = margin(r = 5)),
          axis.text.x = element_text(size = 7, margin = margin(t = 5)),
          axis.ticks.x = element_line(), axis.title.x = element_text(size = 8),
          plot.title = element_text(size = 9.5, face = "bold"))

  ann_df <- filter(df, !is.na(annot_label))
  if (nrow(ann_df) > 0) {
    p <- p + geom_text(data = ann_df, aes(x = annot_x, y = bin_label, label = annot_label, hjust = annot_hjust),
                        inherit.aes = FALSE, size = 2.4, colour = "grey15")
  }
  if (!is.na(j_val)) {
    p <- p + annotate("text", x = Inf, y = Inf, label = sprintf("J = %.3f", j_val),
                       hjust = 1.1, vjust = 1.8, size = 3, colour = "grey20")
  }
  p
}

# ============================================================================
# PART 1: NEE panels (Fig 4, both variants) + Fig 5 trajectory line
# reuses data/snapshots/ outputs from figure_representativeness_nee_signed.R
# ============================================================================
msg("\n=== PART 1: NEE candidate panels ===")

nee_global <- read_csv(file.path(SNAP_DIR, "trendy_nee_signed5_global_distribution.csv"), show_col_types = FALSE)
nee_occ    <- read_csv(file.path(SNAP_DIR, "nee_signed5_occupancy_jaccard.csv"), show_col_types = FALSE)

NEE5_COLORS <- setNames(
  c("#67001f", "#b2182b", "#ef8a62", "#f7f7f7", "#2166ac"),  # sink(dark->light) / near-zero / source
  nee_global$bin_label[match(1:5, nee_global$bin)]
)

for (variant_i in list(list(v = "geo_vs_geo", label = "Geo vs Geo", file = "fig4_nee_geo_vs_geo.png"),
                        list(v = "geo_vs_data", label = "Geo vs Data", file = "fig4_nee_geo_vs_data.png"))) {
  occ_v <- nee_occ |> filter(variant == variant_i$v, network == "current_781") |> arrange(bin)
  j_val <- occ_v$weighted_jaccard[1]
  p <- make_candidate_panel(
    occ_v |> select(bin_label, site_frac, global_frac) |> rename(global_land_fraction = global_frac),
    j_val,
    paste0("NEE (signed, 5-bin) — ", variant_i$label, " — current network (n=", n_sites, ")"),
    NEE5_COLORS
  )
  out_path <- file.path(OUT_DIR, variant_i$file)
  ggsave(out_path, p, width = 6, height = 3.2, dpi = 300, bg = "white")
  msg("  Saved: ", out_path)
  write_note(out_path, c(
    paste0("NEE (signed, 5-bin) representativeness panel -- Fig 4 (current network), ", variant_i$label, " variant."),
    paste0("Bin edges (gC m-2 yr-1): see data/snapshots/trendy_nee_signed5_global_distribution.meta.json"),
    paste0("  ", paste(occ_v$bin_label, collapse = " | ")),
    paste0("Site count: n_total=", occ_v$n_total[1], "  n_classified=", occ_v$n_classified[1]),
    paste0("Weighted Jaccard J = ", round(j_val, 4)),
    "Source: data/snapshots/site_trendy_nee_signed5_{geo,data}_current_781.csv, nee_signed5_occupancy_jaccard.csv",
    "(scripts/figure_representativeness_nee_signed.R, production, committed 2026-09-28)."
  ))
}

## Fig 5: add the NEE (Data) line to the 6-default-axis Jaccard trajectory
msg("\n--- Fig 5: NEE line on Jaccard trajectory ---")
metrics_df <- read_csv(METRICS_CSV, show_col_types = FALSE)
get_j <- function(axis, agg, net) {
  v <- metrics_df |> filter(.data$axis == !!axis, aggregation_level == agg, network == net) |> pull(weighted_jaccard)
  if (length(v) == 0L) NA_real_ else v[[1L]]
}
NET_ORDER  <- c("marconi", "la_thuile", "fluxnet2015", "current_781")
NET_X      <- setNames(1:4, NET_ORDER)
NET_NSITES <- c(marconi = 35L, la_thuile = 252L, fluxnet2015 = 212L, current_781 = 781L)
NET_XLABELS_N <- c("Marconi\n(n=35)", "La Thuile\n(n=252)", "FLUXNET2015\n(n=212)", "Current\n(n=781)")

ax_specs_6 <- list(
  "KG (13-class)"     = list(axis = "koppen_beck2023", agg = "13class_twoletter"),
  "LULC (10-class)"   = list(axis = "landcover_cci",    agg = "high_level"),
  "Aridity (7-class)" = list(axis = "aridity_unep7",    agg = "unep7"),
  "Biomass (18-bin)"  = list(axis = "biomass_cci_v7",   agg = "18bin_hybrid"),
  "TRENDY NEE-IAV"    = list(axis = "trendy_nee_iav",   agg = "18bin_hybrid"),
  "TRENDY ET-median"  = list(axis = "trendy_et_median", agg = "18bin_hybrid"),
  "NEE (signed, 5-bin)" = list(axis = "nee_signed5_data", agg = "5bin_signed")
)
TRAJ_COLORS <- c(
  "KG (13-class)" = "#D55E00", "LULC (10-class)" = "#CC79A7", "Aridity (7-class)" = "#E69F00",
  "Biomass (18-bin)" = "#009E73", "TRENDY NEE-IAV" = "#0072B2", "TRENDY ET-median" = "#56B4E9",
  "NEE (signed, 5-bin)" = "#000000"
)

traj_df <- do.call(bind_rows, lapply(names(ax_specs_6), function(label) {
  spec <- ax_specs_6[[label]]
  do.call(bind_rows, lapply(NET_ORDER, function(net) {
    data.frame(axis_label = label, network = net, net_x = NET_X[[net]],
               jaccard = get_j(spec$axis, spec$agg, net), stringsAsFactors = FALSE)
  }))
}))
traj_df$axis_label <- factor(traj_df$axis_label, levels = names(TRAJ_COLORS))

traj_theme <- theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "black"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = margin(t = 5)), axis.text.y = element_text(margin = margin(r = 5)),
    legend.position = c(0.05, 0.95), legend.justification = c(0, 1),
    legend.background = element_rect(fill = grDevices::adjustcolor("white", alpha.f = 0.8), colour = "black", linewidth = 0.2),
    legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 7), legend.spacing.y = unit(0.05, "cm")
  )

p_traj <- ggplot(traj_df, aes(x = net_x, y = jaccard, colour = axis_label, group = axis_label)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 2.5, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = TRAJ_COLORS) +
  scale_x_continuous(breaks = 1:4, labels = NET_XLABELS_N, limits = c(0.6, 4.4), expand = expansion(mult = 0)) +
  scale_y_continuous(name = "Weighted Jaccard", limits = c(0, 1), breaks = seq(0, 1, 0.25),
                      expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Jaccard representativeness trajectory — 6 default axes + NEE (candidate)", x = NULL) +
  traj_theme

fig5_path <- file.path(OUT_DIR, "fig5_jaccard_trajectory_with_nee.png")
ggsave(fig5_path, p_traj, width = 7, height = 5, dpi = 300, bg = "white")
msg("  Saved: ", fig5_path)
write_note(fig5_path, c(
  "Jaccard trajectory (Fig 5 style) -- the existing 6 default axes plus a new NEE (signed, 5-bin, Data variant) line.",
  paste0("NEE line J by network: ", paste(sprintf("%s=%.3f", NET_ORDER,
         vapply(NET_ORDER, function(n) get_j("nee_signed5_data", "5bin_signed", n), numeric(1))), collapse = "  ")),
  "Historical-network NEE values are current-release tower data for each network's sites still active",
  "in the current release (Hard Rule 1) -- see data/snapshots/nee_signed5_occupancy_jaccard.csv.",
  "Source: data/snapshots/representativeness_metrics.csv."
))

msg("=== PART 1 complete ===")

# ============================================================================
# PART 2: GPP / TER mock-up computation (not written to data/snapshots/ --
# candidate only, unlike the production NEE axis in Part 1)
# ============================================================================
msg("\n=== PART 2: GPP/TER model + tower computation ===")

msg("Loading KG land mask (0.5 deg) ...")
kg_05 <- rast(KG_PATH)
cell_areas_05 <- cellSize(kg_05, mask = TRUE, unit = "km")

## Complete-case temporal mean (all layers must be non-NA at a pixel) --
## reused verbatim from scripts/diagnostics/nee_corrected_axis.R.
complete_mean <- function(r) {
  vals <- values(r)
  complete <- rowSums(is.na(vals)) == 0L
  out <- rep(NA_real_, nrow(vals))
  if (any(complete)) out[complete] <- rowMeans(vals[complete, , drop = FALSE])
  r1 <- r[[1L]]; values(r1) <- out; names(r1) <- "mean"
  r1
}
regrid_mean_to_target <- function(r_mean_native, kg_mask) {
  if (!isTRUE(all.equal(res(r_mean_native), c(0.5, 0.5)))) {
    r_mean_native <- resample(r_mean_native, TARGET, method = "bilinear", threads = TRUE)
  }
  mask(r_mean_native, kg_mask)
}

gpp_layers <- list(); ter_layers <- list()
models_ok <- character(0)
for (mdl in MODELS_TARGET) {
  gpp_cache <- file.path(INTER_DIR, paste0(mdl, "_gpp_annual_native_", WIN_START, "_", WIN_END, ".tif"))
  ra_cache  <- file.path(INTER_DIR, paste0(mdl, "_ra_annual_native_",  WIN_START, "_", WIN_END, ".tif"))
  rh_cache  <- file.path(INTER_DIR, paste0(mdl, "_rh_annual_native_",  WIN_START, "_", WIN_END, ".tif"))
  if (!all(file.exists(gpp_cache, ra_cache, rh_cache))) {
    msg("  ", mdl, ": missing cached native stack(s) -- skipping")
    next
  }
  r_gpp <- complete_mean(rast(gpp_cache))
  r_ra  <- complete_mean(rast(ra_cache))
  r_rh  <- complete_mean(rast(rh_cache))
  r_ter <- r_ra + r_rh
  gpp_layers[[mdl]] <- regrid_mean_to_target(r_gpp, kg_05)
  ter_layers[[mdl]] <- regrid_mean_to_target(r_ter, kg_05)
  models_ok <- c(models_ok, mdl)
}
msg("Models with usable gpp/ra/rh (", length(models_ok), "/", length(MODELS_TARGET), "): ",
    paste(models_ok, collapse = ", "))

gpp_stack <- rast(gpp_layers); names(gpp_stack) <- models_ok
ter_stack <- rast(ter_layers); names(ter_stack) <- models_ok
r_gpp_model <- app(gpp_stack, fun = function(v) median(v, na.rm = TRUE))
r_ter_model <- app(ter_stack, fun = function(v) median(v, na.rm = TRUE))

writeRaster(r_gpp_model, file.path(DERIVED_DIR, "candidate_gpp_median.tif"), gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
writeRaster(r_ter_model, file.path(DERIVED_DIR, "candidate_ter_median.tif"), gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
msg("Saved ensemble-median rasters: candidate_gpp_median.tif, candidate_ter_median.tif (", length(models_ok), " models, ", WIN_START, "-", WIN_END, " mean)")

## ---- 5 equal-area quantile bins of the global field, no near-zero bin -----
build_global_hist_unsigned <- function(r_map, kg_mask, cell_areas, step = 1, hist_max = 6000) {
  r_land <- mask(r_map, kg_mask)
  lo <- seq(0, hist_max - step, by = step)
  hi <- lo + step
  ids <- seq_along(lo)
  catch_id <- max(ids) + 1L
  rcl <- rbind(cbind(lo, hi, as.numeric(ids)), c(hist_max, 1e9, as.numeric(catch_id)))
  r_hist <- classify(ifel(r_land < 0, 0, r_land), rcl, right = FALSE, include.lowest = TRUE)
  areas <- zonal(cell_areas, r_hist, fun = "sum", na.rm = TRUE)
  names(areas) <- c("bin_id", "area_km2")
  areas <- areas[!is.na(areas$bin_id), ]
  bin_lo_vec <- c(lo, hist_max)
  areas$value <- bin_lo_vec[areas$bin_id]
  areas[order(areas$value), c("value", "area_km2")]
}

make_quantile_bins_5 <- function(hist_df) {
  ordered <- hist_df[order(hist_df$value), ]
  cum <- cumsum(ordered$area_km2)
  total <- sum(ordered$area_km2)
  sort(vapply(c(1, 2, 3, 4) / 5, function(f) {
    idx <- which(cum >= f * total)[1L]
    ordered$value[idx]
  }, numeric(1)))
}

classify_5 <- function(x, breaks) {
  b <- findInterval(x, breaks[-length(breaks)], left.open = FALSE)
  b[b < 1L] <- 1L
  b[b > 5L] <- 5L
  as.integer(b)
}

geo_coords <- as.matrix(current_sites[, c("location_long", "location_lat")])

process_flux_axis <- function(r_model, flux_name, unit_str = "gC m-2 yr-1") {
  hist_df <- build_global_hist_unsigned(r_model, kg_05, cell_areas_05)
  q_breaks <- make_quantile_bins_5(hist_df)
  breaks <- c(-Inf, q_breaks, Inf)
  msg("  ", flux_name, " quantile breaks (", unit_str, "): ", paste(round(q_breaks, 1), collapse = ", "))

  total_area <- sum(hist_df$area_km2)
  hist_df$bin <- classify_5(hist_df$value, breaks)
  bin_labels <- c(
    sprintf("0–%.0f", q_breaks[1]),
    sprintf("%.0f–%.0f", q_breaks[1], q_breaks[2]),
    sprintf("%.0f–%.0f", q_breaks[2], q_breaks[3]),
    sprintf("%.0f–%.0f", q_breaks[3], q_breaks[4]),
    sprintf(">%.0f", q_breaks[4])
  )
  global_frac <- hist_df |> group_by(bin) |> summarise(area_km2 = sum(area_km2), .groups = "drop") |>
    mutate(global_land_fraction = area_km2 / total_area, bin_label = bin_labels[bin]) |> arrange(bin)

  model_vals <- terra::extract(r_model, geo_coords, method = "bilinear")[, 1]
  geo_df <- current_sites |> mutate(model_value = model_vals) |> filter(!is.na(model_value)) |>
    mutate(bin = classify_5(model_value, breaks))

  list(breaks = breaks, bin_labels = bin_labels, global_frac = global_frac, geo_df = geo_df)
}

gpp_axis <- process_flux_axis(r_gpp_model, "GPP")
ter_axis <- process_flux_axis(r_ter_model, "TER")

## ---- Tower side: same VUT/CUT choice as NEE, + independent NT->DT fallback
## The VUT/CUT *decision* is the same rule NEE uses (per-site NEE_VUT_REF_QC
## presence -> VUT, else NEE_CUT_REF_QC presence -> CUT), applied to every
## site with a decision (not just the subset that happened to also clear
## NEE's own 12-month qualification -- GPP/TER qualify independently).
msg("\n--- Tower GPP/RECO: same VUT/CUT choice as NEE, + NT->DT fallback ---")

site_ids_sql <- paste(sprintf("'%s'", current_sites$site_id), collapse = ", ")
con <- dbConnect(duckdb::duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
monthly_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP,
         NEE_VUT_REF_QC, NEE_CUT_REF_QC,
         GPP_NT_VUT_REF, GPP_DT_VUT_REF, GPP_NT_CUT_REF, GPP_DT_CUT_REF,
         RECO_NT_VUT_REF, RECO_DT_VUT_REF, RECO_NT_CUT_REF, RECO_DT_CUT_REF
  FROM monthly_converted
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", site_ids_sql))
dbDisconnect(con, shutdown = TRUE)

site_carbon_src <- monthly_raw |>
  group_by(site_id) |>
  summarise(any_vut_qc = any(!is.na(NEE_VUT_REF_QC)), any_cut_qc = any(!is.na(NEE_CUT_REF_QC)), .groups = "drop") |>
  mutate(tower_carbon_src = case_when(any_vut_qc ~ "VUT", any_cut_qc ~ "CUT", TRUE ~ NA_character_))
msg("Per-site VUT/CUT choice (same rule as NEE): VUT=", sum(site_carbon_src$tower_carbon_src == "VUT", na.rm = TRUE),
    "  CUT (fallback)=", sum(site_carbon_src$tower_carbon_src == "CUT", na.rm = TRUE),
    "  neither=", sum(is.na(site_carbon_src$tower_carbon_src)))

monthly_raw <- monthly_raw |>
  mutate(TIMESTAMP = as.Date(TIMESTAMP), year = year(TIMESTAMP), month = month(TIMESTAMP)) |>
  left_join(site_carbon_src |> select(site_id, tower_carbon_src), by = "site_id") |>
  mutate(
    gpp_nt = if_else(tower_carbon_src == "VUT", GPP_NT_VUT_REF, GPP_NT_CUT_REF),
    gpp_dt = if_else(tower_carbon_src == "VUT", GPP_DT_VUT_REF, GPP_DT_CUT_REF),
    ter_nt = if_else(tower_carbon_src == "VUT", RECO_NT_VUT_REF, RECO_NT_CUT_REF),
    ter_dt = if_else(tower_carbon_src == "VUT", RECO_DT_VUT_REF, RECO_DT_CUT_REF)
  )
## monthly_converted's carbon columns are already gC m-2 month-1 totals
## (05_units.R's daily-rate x days-in-month conversion) -- use directly.

## Per-site independent NT->DT fallback (pattern from flux_tower_model_distributions.R)
site_partition <- monthly_raw |>
  group_by(site_id) |>
  summarise(n_gpp_nt = sum(!is.na(gpp_nt)), n_gpp_dt = sum(!is.na(gpp_dt)),
            n_ter_nt = sum(!is.na(ter_nt)), n_ter_dt = sum(!is.na(ter_dt)), .groups = "drop") |>
  mutate(gpp_partition = case_when(n_gpp_nt > 0L ~ "NT", n_gpp_dt > 0L ~ "DT", TRUE ~ NA_character_),
         ter_partition = case_when(n_ter_nt > 0L ~ "NT", n_ter_dt > 0L ~ "DT", TRUE ~ NA_character_)) |>
  select(site_id, gpp_partition, ter_partition)
msg("GPP partitioning: NT=", sum(site_partition$gpp_partition == "NT", na.rm = TRUE),
    "  DT (fallback)=", sum(site_partition$gpp_partition == "DT", na.rm = TRUE))
msg("TER partitioning: NT=", sum(site_partition$ter_partition == "NT", na.rm = TRUE),
    "  DT (fallback)=", sum(site_partition$ter_partition == "DT", na.rm = TRUE))

monthly_raw <- monthly_raw |> left_join(site_partition, by = "site_id") |>
  mutate(
    gpp_gC = case_when(gpp_partition == "NT" ~ gpp_nt, gpp_partition == "DT" ~ gpp_dt, TRUE ~ NA_real_),
    ter_gC = case_when(ter_partition == "NT" ~ ter_nt, ter_partition == "DT" ~ ter_dt, TRUE ~ NA_real_)
  )

build_annual <- function(df, value_col) {
  cyc <- df |> filter(!is.na(.data[[value_col]])) |> group_by(site_id, month) |>
    summarise(mean_month = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  all12 <- cyc |> group_by(site_id) |> summarise(n_months = n(), .groups = "drop") |>
    filter(n_months == 12L) |> pull(site_id)
  cyc |> filter(site_id %in% all12) |> group_by(site_id) |>
    summarise(tower_value = sum(mean_month), .groups = "drop")
}

gpp_tower <- build_annual(monthly_raw, "gpp_gC") |> mutate(bin = classify_5(tower_value, gpp_axis$breaks))
ter_tower <- build_annual(monthly_raw, "ter_gC") |> mutate(bin = classify_5(tower_value, ter_axis$breaks))
msg("Tower annual GPP: ", nrow(gpp_tower), " sites.  Tower annual TER: ", nrow(ter_tower), " sites.")

## ---- Occupancy / Jaccard + render panels -----------------------------------
site_fracs <- function(bins, n_total) as.numeric(table(factor(bins, levels = 1:5))) / n_total
weighted_jaccard <- function(p, q) sum(pmin(p, q)) / sum(pmax(p, q))

FLUX_QUANTILE_COLORS <- setNames(
  c("#f7fcf5", "#c7e9c0", "#74c476", "#238b45", "#00441b"),  # light -> dark green, low -> high flux
  NULL
)

render_flux_panel <- function(axis_info, site_bins, n_total, n_classified, flux_label, variant_label, file_name) {
  fr <- site_fracs(site_bins, n_total)
  gf <- axis_info$global_frac$global_land_fraction[match(1:5, axis_info$global_frac$bin)]
  j  <- weighted_jaccard(gf, fr)
  df <- data.frame(bin_label = axis_info$bin_labels, site_frac = fr, global_land_fraction = gf)
  colors <- setNames(FLUX_QUANTILE_COLORS, axis_info$bin_labels)
  p <- make_candidate_panel(df, j, paste0(flux_label, " — ", variant_label, " — current network (n=", n_total, ")"), colors)
  out_path <- file.path(OUT_DIR, file_name)
  ggsave(out_path, p, width = 6, height = 3.2, dpi = 300, bg = "white")
  msg("  Saved: ", out_path)
  write_note(out_path, c(
    paste0(flux_label, " (5 equal-area quantile bins, no near-zero bin) -- ", variant_label, " -- current network."),
    paste0("Bin edges (gC m-2 yr-1): ", paste(round(axis_info$breaks[is.finite(axis_info$breaks)], 1), collapse = ", ")),
    paste0("  ", paste(axis_info$bin_labels, collapse = " | ")),
    paste0("Site count: n_total=", n_total, "  n_classified=", n_classified),
    paste0("Weighted Jaccard J = ", round(j, 4)),
    if (grepl("Data", variant_label)) {
      "Tower: Step 3 annual method, same per-site VUT/CUT choice as the NEE axis, plus an independent per-site NT->DT fallback."
    } else {
      "Model: S3 ensemble-median TRENDY v14, 1991-2020 mean, bilinear-extracted at site coordinates."
    },
    "Candidate mock-up -- not written to data/snapshots/ (unlike the production NEE axis)."
  ))
  invisible(j)
}

render_flux_panel(gpp_axis, gpp_axis$geo_df$bin, n_sites, nrow(gpp_axis$geo_df), "GPP", "Geo", "fig4_gpp_geo.png")
render_flux_panel(gpp_axis, gpp_tower$bin,       n_sites, nrow(gpp_tower),       "GPP", "Data", "fig4_gpp_data.png")
render_flux_panel(ter_axis, ter_axis$geo_df$bin, n_sites, nrow(ter_axis$geo_df), "TER", "Geo", "fig4_ter_geo.png")
render_flux_panel(ter_axis, ter_tower$bin,       n_sites, nrow(ter_tower),       "TER", "Data", "fig4_ter_data.png")

msg("\n=== candidate_nee_gpp_ter_panels.R complete ===")
