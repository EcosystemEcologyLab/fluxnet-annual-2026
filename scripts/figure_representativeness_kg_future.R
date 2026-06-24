## figure_representativeness_kg_future.R
## Future Köppen-Geiger representativeness axis.
## Scenario: Beck 2023 SSP2-4.5 projection, 2041-2070, 1 km resolution.
##
## Asymmetric design (documented in methods):
##   Earth bar  = global land area fractions under PROJECTED 2041-2070 climate
##   Network bar = current 767-site FLUXNET network with PRESENT-DAY KG
##                 assignments (from site_koppen_beck2023.csv)
##
## The question: does the network placed under today's biogeography sample
## tomorrow's climate distribution?
##
## Source raster: data/external/koppen_beck2023/2041_2070/ssp245/
##                koppen_geiger_0p00833333.tif
## Beck et al. (2023) Sci Data 10:724. doi:10.1038/s41597-023-02549-6
## Figshare v2: doi:10.6084/m9.figshare.21789074.v2

if (file.exists(".env")) dotenv::load_dot_env()
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

library(terra)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---- Paths ------------------------------------------------------------------
kg_dir      <- file.path("data", "external", "koppen_beck2023")
leg_path    <- file.path(kg_dir, "legend.txt")
rast_fut    <- file.path(kg_dir, "2041_2070", "ssp245",
                         "koppen_geiger_0p00833333.tif")
rast_pres   <- file.path(kg_dir, "1991_2020", "koppen_geiger_0p00833333.tif")
snap_path   <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "fluxnet_shuttle_snapshot_20260624T095651.csv")
site_pres   <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "site_koppen_beck2023.csv")
pres_glob   <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "koppen_beck2023_global_distribution.csv")

scenario    <- "ssp245_2041_2070"
site_out    <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         paste0("site_koppen_beck2023_", scenario, ".csv"))
glob_out    <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         paste0("koppen_beck2023_", scenario,
                                "_global_distribution.csv"))
metrics_out <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "representativeness_metrics.csv")
out_dir     <- file.path("review", "figures", "representativeness")
methods_out <- file.path(out_dir,
                         "methods_koppen_beck2023_future.md")

for (p in c(rast_fut, leg_path, snap_path, site_pres)) {
  if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tag <- "SSP2-4.5, 2041–70"   # used in figure labels

# ---- Parse legend (class codes, names, RGB colors) --------------------------
leg_lines <- readLines(leg_path)
leg_data  <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]

legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
  m <- regmatches(ln, regexec(
    "^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[(\\d+)\\s+(\\d+)\\s+(\\d+)\\]",
    ln, perl = TRUE))[[1]]
  if (length(m) < 7L) return(NULL)
  data.frame(koppen_class_code = as.integer(m[2]), koppen_class = m[3],
             koppen_class_name = trimws(m[4]),
             r = as.integer(m[5]), g = as.integer(m[6]), b = as.integer(m[7]),
             stringsAsFactors = FALSE)
}))

main_map <- c(A = "Tropical", B = "Arid", C = "Temperate", D = "Cold", E = "Polar")
legend_df <- legend_df |>
  dplyr::mutate(
    color_hex        = grDevices::rgb(r, g, b, maxColorValue = 255),
    koppen_main      = substr(koppen_class, 1L, 1L),
    koppen_main_name = main_map[koppen_main],
    koppen_twoletter = substr(koppen_class, 1L, 2L)
  )

class_order <- legend_df$koppen_class
color_30    <- setNames(legend_df$color_hex, legend_df$koppen_class)
main_order  <- c("A", "B", "C", "D", "E")
main_reps   <- c(A = "Aw", B = "BSh", C = "Cfa", D = "Dfb", E = "ET")
color_5     <- setNames(
  legend_df$color_hex[match(main_reps, legend_df$koppen_class)], names(main_reps))
tl_order    <- c("Af","Am","Aw","BS","BW","Cf","Cs","Cw",
                 "Df","Ds","Dw","EF","ET")
color_tl    <- setNames(
  vapply(tl_order, function(tl) {
    m <- legend_df[substr(legend_df$koppen_class, 1L, 2L) == tl, ]
    grDevices::rgb(mean(m$r), mean(m$g), mean(m$b), maxColorValue = 255)
  }, character(1L)),
  tl_order)

# ---- Load future raster and snapshot ----------------------------------------
message("Loading future KG raster (", tag, "): ", rast_fut)
kg_fut <- terra::rast(rast_fut)
message("  Dims: ", nrow(kg_fut), " × ", ncol(kg_fut))

snapshot    <- readr::read_csv(snap_path, show_col_types = FALSE)
site_coords <- snapshot |>
  dplyr::distinct(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))
N <- nrow(site_coords)
message("Sites: ", N)

# ---- Step 1: Per-site future KG extraction ----------------------------------
message("\nExtracting future KG at site coordinates ...")
pts <- terra::vect(
  data.frame(x = site_coords$location_long, y = site_coords$location_lat),
  geom = c("x", "y"), crs = "EPSG:4326"
)

kg_raw <- terra::extract(kg_fut, pts, ID = FALSE)
names(kg_raw)[1] <- "koppen_class_code"
site_coords$koppen_class_code <- kg_raw$koppen_class_code
site_coords$koppen_method     <- "exact"

na_idx <- which(is.na(site_coords$koppen_class_code))
message("NA after exact extraction: ", length(na_idx), " site(s)")

if (length(na_idx) > 0L) {
  int_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(NA_integer_)
    as.integer(names(sort(table(x), decreasing = TRUE))[[1L]])
  }
  buffer_degs   <- c(0.01, 0.05, 0.1, 0.25, 0.5)
  still_na      <- na_idx
  recovered_any <- logical(length(still_na))

  for (j in seq_along(still_na)) {
    pt_j <- pts[still_na[j], ]
    for (buf in buffer_degs) {
      buf_vals <- terra::extract(kg_fut, pt_j, buffer = buf, ID = TRUE)
      mode_val <- int_mode(buf_vals[[2L]])
      if (!is.na(mode_val)) {
        site_coords$koppen_class_code[still_na[j]] <- mode_val
        site_coords$koppen_method[still_na[j]] <-
          paste0("buffer_", round(buf * 111, 0), "km")
        message("  ", site_coords$site_id[still_na[j]],
                " recovered at buffer ", buf, "°: code ", mode_val)
        recovered_any[j] <- TRUE
        break
      }
    }
  }
  still_na <- still_na[!recovered_any]

  if (length(still_na) > 0L) {
    message(length(still_na), " still NA — nearest-land search ...")
    for (j in seq_along(still_na)) {
      idx  <- still_na[j]
      lng  <- site_coords$location_long[idx]
      lat  <- site_coords$location_lat[idx]
      ext  <- terra::ext(lng - 3, lng + 3, lat - 3, lat + 3)
      lcr  <- terra::crop(kg_fut, ext)
      lpts <- terra::as.points(lcr, na.rm = TRUE)
      if (terra::nrow(lpts) == 0L) next
      dsts <- terra::distance(pts[idx, ], lpts)
      nidx <- which.min(dsts)
      nval <- as.integer(terra::values(lpts)[[1L]][nidx])
      nkm  <- round(min(dsts) / 1000, 1)
      site_coords$koppen_class_code[idx] <- nval
      site_coords$koppen_method[idx]     <- paste0("nearest_land_", nkm, "km")
      message("  ", site_coords$site_id[idx], " → code ", nval, " (", nkm, " km)")
    }
  }
}

out_fut <- site_coords |>
  dplyr::left_join(
    dplyr::select(legend_df, koppen_class_code, koppen_class, koppen_class_name,
                  koppen_main, koppen_main_name),
    by = "koppen_class_code"
  ) |>
  dplyr::select(site_id, location_lat, location_long,
                koppen_class_code, koppen_class, koppen_class_name,
                koppen_main, koppen_main_name, koppen_method)

n_sites <- nrow(out_fut)

cat("\n=== FUTURE KG SITE DISTRIBUTION (", n_sites, " sites,", tag, ") ===\n")
fut_cls_tbl <- out_fut |>
  dplyr::count(koppen_main, koppen_main_name, name = "n") |>
  dplyr::arrange(koppen_main) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1))
print(as.data.frame(fut_cls_tbl))

# Sites that change class under future climate vs. present-day
site_pres_df <- readr::read_csv(site_pres, show_col_types = FALSE)
changed <- dplyr::left_join(
  dplyr::select(out_fut, site_id, future_class = koppen_class),
  dplyr::select(site_pres_df, site_id, present_class = koppen_class),
  by = "site_id"
) |>
  dplyr::filter(!is.na(future_class), !is.na(present_class),
                future_class != present_class)
message(sprintf("Sites changing class under %s: %d / %d (%.1f%%)",
                tag, nrow(changed), n_sites, 100 * nrow(changed) / n_sites))
if (nrow(changed) > 0L) {
  cat("\n--- Sites with class change ---\n")
  print(as.data.frame(changed), row.names = FALSE)
}

readr::write_csv(out_fut, site_out)
message("Saved: ", site_out)
write_output_metadata(
  site_out,
  input_sources = c(snap_path, rast_fut),
  notes = paste0(
    "Future KG site extraction for 767 sites (snapshot 20260624T095651). ",
    "Scenario: SSP2-4.5, 2041-2070. Same terra::extract() + nearest-land ",
    "fallback method as present-day (scripts/step4_extract_koppen_beck2023.R). ",
    "This CSV records PROJECTED future class at each site's PRESENT location. ",
    "For figures, the network bar uses present-day assignments from ",
    "site_koppen_beck2023.csv, NOT this file."
  )
)

# ---- Step 2: Global area-weighted future distribution -----------------------
message("\nComputing global KG distribution under ", tag, " ...")
cell_areas <- terra::cellSize(kg_fut, mask = TRUE, unit = "km")
t0         <- proc.time()
zone_areas <- terra::zonal(cell_areas, kg_fut, fun = "sum", na.rm = TRUE)
elapsed    <- round((proc.time() - t0)[["elapsed"]], 1)
names(zone_areas) <- c("koppen_class_code", "global_land_area_km2")
zone_areas <- zone_areas |>
  dplyr::filter(koppen_class_code >= 1L, koppen_class_code <= 30L)

total_land_km2 <- sum(zone_areas$global_land_area_km2)
message("  Total land area: ", format(round(total_land_km2), big.mark = ","), " km²",
        "  (present-day: 147,322,862 km²)")
message("  Elapsed (zonal): ", elapsed, " s")

dist_fut <- legend_df |>
  dplyr::left_join(zone_areas, by = "koppen_class_code") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_land_km2
  ) |>
  dplyr::select(koppen_class_code, koppen_class, koppen_class_name,
                koppen_twoletter, koppen_main, koppen_main_name,
                global_land_area_km2, global_land_fraction) |>
  dplyr::arrange(koppen_class_code)

readr::write_csv(dist_fut, glob_out)
message("Saved: ", glob_out)
write_output_metadata(
  glob_out,
  input_sources = c(rast_fut,
    paste0("Beck et al. (2023) KG SSP2-4.5 2041-2070 1 km raster — ",
           "figshare doi:10.6084/m9.figshare.21789074.v2")),
  notes = paste0(
    "Global land area per KG class under SSP2-4.5, 2041-2070 projection. ",
    "Method: terra::cellSize(mask=TRUE, unit='km') + terra::zonal(fun='sum'). ",
    "Total land: ", format(round(total_land_km2), big.mark = ","), " km². ",
    "Elapsed (zonal): ", elapsed, " s."
  )
)

# ---- Present-day vs future global shifts (top movers) ----------------------
dist_pres <- readr::read_csv(pres_glob, show_col_types = FALSE)
shifts <- dplyr::inner_join(
  dplyr::select(dist_fut,  koppen_class, future_frac  = global_land_fraction),
  dplyr::select(dist_pres, koppen_class, present_frac = global_land_fraction),
  by = "koppen_class"
) |>
  dplyr::mutate(delta = future_frac - present_frac) |>
  dplyr::arrange(dplyr::desc(abs(delta)))

cat("\n=== TOP 10 GLOBAL CLASS SHIFTS (present-day → ", tag, ") ===\n")
print(as.data.frame(dplyr::mutate(head(shifts, 10),
  pres_pct   = round(present_frac * 100, 2),
  fut_pct    = round(future_frac  * 100, 2),
  delta_pp   = round(delta        * 100, 2)
)[, c("koppen_class", "pres_pct", "fut_pct", "delta_pp")]), row.names = FALSE)

# Main-class shifts
shifts_main <- dist_fut |>
  dplyr::group_by(koppen_main) |>
  dplyr::summarise(future_frac = sum(global_land_fraction), .groups = "drop") |>
  dplyr::inner_join(
    dist_pres |>
      dplyr::group_by(koppen_main) |>
      dplyr::summarise(present_frac = sum(global_land_fraction), .groups = "drop"),
    by = "koppen_main"
  ) |>
  dplyr::mutate(delta = future_frac - present_frac)
cat("\n=== 5-CLASS SHIFTS ===\n")
print(as.data.frame(dplyr::mutate(shifts_main,
  p = round(present_frac*100,1), f = round(future_frac*100,1),
  d = round(delta*100,2))[, c("koppen_main","p","f","d")]), row.names = FALSE)

# ---- Step 3: Metrics --------------------------------------------------------
compute_repr_metrics <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

# Network q: PRESENT-DAY site assignments (the asymmetric design)
n_pres <- nrow(site_pres_df)

site_fracs <- function(col_name, levels_vec, df) {
  counts <- table(df[[col_name]])
  vapply(levels_vec, function(lv) {
    n <- counts[lv]
    if (is.na(n)) 0 else as.numeric(n) / n_pres
  }, numeric(1L))
}

# 30-class
p30 <- dist_fut$global_land_fraction[match(class_order, dist_fut$koppen_class)]
p30[is.na(p30)] <- 0
q30 <- site_fracs("koppen_class", class_order, site_pres_df)
m30 <- compute_repr_metrics(p30, q30)

# 5-class
p5 <- vapply(main_order, function(m)
  sum(dist_fut$global_land_fraction[dist_fut$koppen_main == m], na.rm = TRUE), numeric(1L))
q5 <- site_fracs("koppen_main", main_order, site_pres_df)
m5 <- compute_repr_metrics(p5, q5)

# 13-class two-letter
fut_tl <- dist_fut |>
  dplyr::group_by(koppen_twoletter) |>
  dplyr::summarise(global_land_fraction = sum(global_land_fraction), .groups = "drop")
p_tl <- vapply(tl_order, function(tl) {
  v <- fut_tl$global_land_fraction[fut_tl$koppen_twoletter == tl]
  if (length(v) == 0) 0 else v
}, numeric(1L))
q_tl <- site_fracs("koppen_twoletter", tl_order, site_pres_df)
m_tl <- compute_repr_metrics(p_tl, q_tl)

message(sprintf("\nFuture metrics — 30-class: J = %.4f, H = %.4f",
                m30$weighted_jaccard, m30$hellinger_distance))
message(sprintf("Future metrics —  5-class: J = %.4f, H = %.4f",
                m5$weighted_jaccard,  m5$hellinger_distance))
message(sprintf("Future metrics — two-letter: J = %.4f, H = %.4f",
                m_tl$weighted_jaccard, m_tl$hellinger_distance))

# Append to metrics CSV
old_met <- if (file.exists(metrics_out)) {
  readr::read_csv(metrics_out, show_col_types = FALSE) |>
    dplyr::filter(axis != paste0("koppen_beck2023_future_", scenario))
} else {
  data.frame(axis = character(), aggregation_level = character(),
             n_classes = integer(), weighted_jaccard = numeric(),
             hellinger_distance = numeric())
}

axis_name <- paste0("koppen_beck2023_future_", scenario)
metrics_df <- dplyr::bind_rows(
  old_met,
  data.frame(axis = axis_name, aggregation_level = "5class",            n_classes = 5L,
             weighted_jaccard = m5$weighted_jaccard,
             hellinger_distance = m5$hellinger_distance),
  data.frame(axis = axis_name, aggregation_level = "13class_twoletter", n_classes = 13L,
             weighted_jaccard = m_tl$weighted_jaccard,
             hellinger_distance = m_tl$hellinger_distance),
  data.frame(axis = axis_name, aggregation_level = "30class",           n_classes = 30L,
             weighted_jaccard = m30$weighted_jaccard,
             hellinger_distance = m30$hellinger_distance)
)
readr::write_csv(metrics_df, metrics_out)
message("Saved: ", metrics_out)

cat("\n=== FULL METRICS TABLE ===\n")
print(as.data.frame(dplyr::mutate(metrics_df,
  J = round(weighted_jaccard, 3), H = round(hellinger_distance, 3)
)[, c("axis", "aggregation_level", "J", "H")]), row.names = FALSE)

# ---- Step 4: Figures --------------------------------------------------------
bar_labels <- c(
  "global"  = paste0("Global land\n(", tag, ")"),
  "network" = "FLUXNET\n(767 sites)"
)

base_theme <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "right",
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA)
  )

make_long <- function(global_vec, network_vec, class_levels, bar_label_fn) {
  data.frame(
    class_id = factor(class_levels, levels = class_levels),
    global   = global_vec,
    network  = network_vec,
    stringsAsFactors = FALSE
  ) |>
    tidyr::pivot_longer(c(global, network), names_to = "bar", values_to = "fraction") |>
    dplyr::mutate(
      bar   = factor(bar, levels = c("global", "network"),
                     labels = c(bar_labels["global"], bar_labels["network"])),
      label = bar_label_fn(as.character(class_id), fraction)
    )
}

label_30 <- function(cls, frac)
  dplyr::if_else(frac >= 0.015, cls, NA_character_)

label_5  <- function(cls, frac) {
  dplyr::case_when(
    frac >= 0.08  ~ sprintf("%s\n%.1f%%", cls, frac * 100),
    frac >= 0.03  ~ sprintf("%s  %.1f%%", cls, frac * 100),
    TRUE          ~ cls
  )
}

label_tl <- function(cls, frac) {
  dplyr::case_when(
    frac >= 0.07  ~ sprintf("%s\n%.1f%%", cls, frac * 100),
    frac >= 0.025 ~ sprintf("%s %.1f%%",  cls, frac * 100),
    frac >  0     ~ cls,
    TRUE          ~ NA_character_
  )
}

make_ratio_df <- function(global_vec, network_vec, class_levels) {
  data.frame(
    class_id       = factor(class_levels, levels = class_levels),
    global_frac    = global_vec,
    network_frac   = network_vec,
    sampling_ratio = network_vec / global_vec,
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(label = sprintf("%.2f×", sampling_ratio))
}

make_bar_panel <- function(plot_df, color_map, class_col = "class_id",
                           guide_ncol = 1, metrics_list, legend_labels = NULL) {
  lv <- levels(plot_df$class_id)
  if (is.null(legend_labels)) legend_labels <- setNames(lv, lv)
  ggplot(plot_df, aes(x = bar, y = fraction, fill = class_id)) +
    geom_bar(stat = "identity", width = 0.6, colour = "white", linewidth = 0.15) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),
              size = 2.3, family = "sans", colour = "black", na.rm = TRUE) +
    scale_fill_manual(values = color_map, breaks = lv, labels = legend_labels,
                      name = NULL,
                      guide = guide_legend(ncol = guide_ncol, reverse = FALSE,
                                           override.aes = list(colour = NA))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01)),
                       labels = scales::percent_format(accuracy = 1),
                       name = "Fraction of total") +
    scale_x_discrete(name = NULL) +
    annotate("text", x = Inf, y = Inf,
             label = sprintf("J = %.2f\nH = %.2f",
                             metrics_list$weighted_jaccard,
                             metrics_list$hellinger_distance),
             hjust = 1.08, vjust = 1.5,
             size = 2.9, family = "sans", colour = "grey25", lineheight = 1.2) +
    base_theme +
    theme(legend.key.size = unit(0.35, "cm"),
          legend.text     = element_text(size = 7, family = "sans"))
}

make_ratio_panel <- function(ratio_df, color_map, axis_text_size = 8,
                             ratio_limits = c(0.05, 15)) {
  lv <- levels(ratio_df$class_id)
  ggplot(ratio_df, aes(x = class_id, y = sampling_ratio, colour = class_id)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    geom_segment(aes(xend = class_id, yend = 1),
                 colour = "grey75", linewidth = 0.5) +
    geom_point(size = 3) +
    geom_text(aes(label = label), vjust = -0.65, size = 2.4,
              family = "sans", colour = "black") +
    scale_colour_manual(values = color_map[lv], guide = "none") +
    scale_y_continuous(
      name   = "Sampling ratio\n(network / global)",
      trans  = "log2",
      breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
      labels = c("0.125×", "0.25×", "0.5×", "1×", "2×", "4×", "8×"),
      limits = ratio_limits
    ) +
    scale_x_discrete(name = NULL) +
    base_theme +
    theme(
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(size = axis_text_size),
      axis.title.y       = element_text(size = 8),
      legend.position    = "none"
    )
}

# --- Figure 1: 30-class ------------------------------------------------------
pd30 <- make_long(p30, q30, class_order, label_30)
rd30 <- make_ratio_df(p30, q30, class_order)
p30_bars  <- make_bar_panel(pd30, color_30, guide_ncol = 2, metrics_list = m30)
p30_ratio <- make_ratio_panel(rd30, color_30,
                              axis_text_size = 0,  # too crowded, suppress
                              ratio_limits = c(0.05, 16))
fig_30 <- (p30_bars / p30_ratio +
  plot_layout(heights = c(4.5, 1.5), guides = "keep")) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

out30 <- file.path(out_dir,
  paste0("fig_representativeness_kg_future_", scenario, "_30class.png"))
ggsave(out30, plot = fig_30, width = 6.5, height = 8, dpi = 200, bg = "white")
message("Saved: ", out30)

# --- Figure 2: 5-class -------------------------------------------------------
pd5 <- make_long(p5, q5, main_order, label_5)
rd5 <- make_ratio_df(p5, q5, main_order)
p5_bars  <- make_bar_panel(pd5, color_5, guide_ncol = 1, metrics_list = m5,
                           legend_labels = c(
                             A = "A — Tropical", B = "B — Arid",
                             C = "C — Temperate", D = "D — Cold", E = "E — Polar"))
p5_ratio <- make_ratio_panel(rd5, color_5, ratio_limits = c(0.07, 10))
fig_5 <- (p5_bars / p5_ratio +
  plot_layout(heights = c(3, 1.8), guides = "keep")) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

out5 <- file.path(out_dir,
  paste0("fig_representativeness_kg_future_", scenario, "_5class.png"))
ggsave(out5, plot = fig_5, width = 7.5, height = 5, dpi = 200, bg = "white")
message("Saved: ", out5)

# --- Figure 3: 13-class two-letter -------------------------------------------
all_tl_df <- data.frame(koppen_twoletter = tl_order) |>
  dplyr::left_join(
    dist_fut |>
      dplyr::group_by(koppen_twoletter) |>
      dplyr::summarise(global_frac = sum(global_land_fraction), .groups = "drop"),
    by = "koppen_twoletter"
  ) |>
  dplyr::mutate(global_frac = dplyr::coalesce(global_frac, 0))

pd_tl <- make_long(p_tl, q_tl, tl_order, label_tl)
rd_tl <- make_ratio_df(p_tl, q_tl, tl_order)

pTL_bars  <- make_bar_panel(pd_tl, color_tl, guide_ncol = 2, metrics_list = m_tl)
pTL_ratio <- make_ratio_panel(rd_tl, color_tl, axis_text_size = 8,
                              ratio_limits = c(0.05, 12))
fig_tl <- (pTL_bars / pTL_ratio +
  plot_layout(heights = c(3, 1.8), guides = "keep")) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

out_tl <- file.path(out_dir,
  paste0("fig_representativeness_kg_future_", scenario, "_twoletter.png"))
ggsave(out_tl, plot = fig_tl, width = 6.5, height = 6.5, dpi = 200, bg = "white")
message("Saved: ", out_tl)

# ---- Comparison table (present-day vs future) -------------------------------
pres_met <- readr::read_csv(metrics_out, show_col_types = FALSE) |>
  dplyr::filter(axis == "koppen_beck2023")
fut_met  <- readr::read_csv(metrics_out, show_col_types = FALSE) |>
  dplyr::filter(axis == axis_name)
cmp <- dplyr::inner_join(
  dplyr::select(pres_met, aggregation_level,
                J_pres = weighted_jaccard, H_pres = hellinger_distance),
  dplyr::select(fut_met,  aggregation_level,
                J_fut  = weighted_jaccard, H_fut  = hellinger_distance),
  by = "aggregation_level"
) |>
  dplyr::mutate(
    dJ = round(J_fut  - J_pres,  3),
    dH = round(H_fut  - H_pres,  3),
    J_pres = round(J_pres, 3), J_fut = round(J_fut, 3),
    H_pres = round(H_pres, 3), H_fut = round(H_fut, 3)
  )
cat("\n=== PRESENT vs FUTURE METRICS COMPARISON ===\n")
print(as.data.frame(cmp[, c("aggregation_level","J_pres","J_fut","dJ",
                             "H_pres","H_fut","dH")]), row.names = FALSE)

# ---- Step 5: Methods text ---------------------------------------------------
methods_lines <- c(
  "Future-climate representativeness was assessed using the Beck et al. (2023)",
  "SSP2-4.5 projected Köppen-Geiger map for 2041-2070. SSP2-4.5 (\"Middle of the",
  "Road\") represents a moderate forcing scenario in which emissions peak around",
  "mid-century before declining; it is one of the more widely used scenarios for",
  "near-term climate assessment. The source raster is at 1 km resolution and uses",
  "the same 30-class KG scheme and legend as the present-day 1991-2020 map. Full",
  "citation and data source are identical to the present-day analysis",
  "(see methods_koppen_beck2023.md); the SSP2-4.5 2041-2070 raster is distributed",
  "within the same figshare archive (doi:10.6084/m9.figshare.21789074.v2).",
  "",
  "The core design choice for this axis is asymmetric: the Earth bar in each",
  "figure shows the PROJECTED global land area distribution under 2041-2070",
  "SSP2-4.5, while the FLUXNET network bar shows PRESENT-DAY KG assignments",
  "from site_koppen_beck2023.csv. This asymmetry is intentional. The FLUXNET",
  "network occupies fixed physical locations; sites do not migrate as their local",
  "climate shifts. The question this axis asks is therefore: does the current",
  "network, classified by its present-day biogeography, sample the climate",
  "distribution that is projected to cover the globe in 2041-2070? A network",
  "that already samples a future-like distribution would require less supplementation",
  "under climate change; one that does not would need new sites in projected",
  "growth zones (primarily arid B and expanding semi-arid classes) to remain",
  "representative.",
  "",
  "As a secondary output, per-site future KG assignments were also extracted",
  sprintf("(site_koppen_beck2023_%s.csv). These show which sites are", scenario),
  "projected to cross a class boundary under SSP2-4.5 by 2041-2070. This CSV is",
  "for exploratory use; it is NOT the basis for the network bar in any figure.",
  "",
  "Global area computation and per-site extraction methods are identical to the",
  "present-day analysis described in methods_koppen_beck2023.md. The same",
  "terra::cellSize(mask=TRUE, unit='km') + terra::zonal(fun='sum') approach was",
  sprintf("used. Total land area under the future projection: %s km²",
          format(round(total_land_km2), big.mark = ",")),
  sprintf("(present-day: 147,322,862 km²; difference reflects raster land mask)."),
  "",
  "Representativeness metrics (weighted Jaccard J and Hellinger distance H) are",
  "computed as described in methods_koppen_beck2023.md, with p = future global",
  "land fraction per class and q = present-day network fraction per class.",
  "",
  "Comparison with present-day metrics:",
  sprintf("  5-class:    present J=%.3f H=%.3f | future J=%.3f H=%.3f | dJ=%+.3f dH=%+.3f",
          cmp$J_pres[cmp$aggregation_level=="5class"],
          cmp$H_pres[cmp$aggregation_level=="5class"],
          cmp$J_fut[cmp$aggregation_level=="5class"],
          cmp$H_fut[cmp$aggregation_level=="5class"],
          cmp$dJ[cmp$aggregation_level=="5class"],
          cmp$dH[cmp$aggregation_level=="5class"]),
  sprintf("  two-letter: present J=%.3f H=%.3f | future J=%.3f H=%.3f | dJ=%+.3f dH=%+.3f",
          cmp$J_pres[cmp$aggregation_level=="13class_twoletter"],
          cmp$H_pres[cmp$aggregation_level=="13class_twoletter"],
          cmp$J_fut[cmp$aggregation_level=="13class_twoletter"],
          cmp$H_fut[cmp$aggregation_level=="13class_twoletter"],
          cmp$dJ[cmp$aggregation_level=="13class_twoletter"],
          cmp$dH[cmp$aggregation_level=="13class_twoletter"]),
  sprintf("  30-class:   present J=%.3f H=%.3f | future J=%.3f H=%.3f | dJ=%+.3f dH=%+.3f",
          cmp$J_pres[cmp$aggregation_level=="30class"],
          cmp$H_pres[cmp$aggregation_level=="30class"],
          cmp$J_fut[cmp$aggregation_level=="30class"],
          cmp$H_fut[cmp$aggregation_level=="30class"],
          cmp$dJ[cmp$aggregation_level=="30class"],
          cmp$dH[cmp$aggregation_level=="30class"])
)
writeLines(methods_lines, methods_out)
message("Saved: ", methods_out)
message("\nAll outputs complete.")
