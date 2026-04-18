# scripts/generate_kg_availability_heatmaps.R
#
# Step 3 of the KG analysis workflow:
#   1. Normalise CLIMATE_KOEPPEN capitalisation (Bsk -> BSk) in badm.rds
#   2. Join KG classification from badm.rds to
#      data/snapshots/long_record_site_candidates_gez.csv
#   3. Save enriched table to
#      data/snapshots/long_record_site_candidates_gez_kg.csv
#   4. Generate three KG availability heatmaps at main / second / third level
#      saved to review/figures/Anomalies_KG/
#
# Heatmap layout:
#   rows    = UN subregion
#   columns = KG class (at specified level)
#   fill    = number of sites with >= 8 valid NEE years
#   facets  = IGBP class

source("R/pipeline_config.R")
source("R/plot_constants.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(fs)
})

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

check_pipeline_config()

# ---- Constants ---------------------------------------------------------------

MIN_NEE_YEARS <- 8L
OUT_DIR       <- file.path("review", "figures", "Anomalies_KG")
SNAPSHOTS_DIR <- file.path(FLUXNET_DATA_ROOT, "snapshots")
PROCESSED_DIR <- file.path(FLUXNET_DATA_ROOT, "processed")
FIGURE_WIDTH  <- 18   # inches
FIGURE_HEIGHT <- 12   # inches
FIGURE_DPI    <- 150

fs::dir_create(OUT_DIR)

# ---- 1. Load and normalise BADM ----------------------------------------------

badm_path <- file.path(PROCESSED_DIR, "badm.rds")
message("Loading BADM: ", badm_path)
badm <- readRDS(badm_path)

# Normalise capitalisation: Bsk -> BSk (same climate class, different case)
n_bsk <- sum(badm$VARIABLE == "CLIMATE_KOEPPEN" & badm$DATAVALUE == "Bsk",
             na.rm = TRUE)
if (n_bsk > 0L) {
  message(sprintf("  Normalising %d 'Bsk' -> 'BSk' entries in BADM", n_bsk))
  badm$DATAVALUE[badm$VARIABLE == "CLIMATE_KOEPPEN" &
                   badm$DATAVALUE == "Bsk"] <- "BSk"
  saveRDS(badm, badm_path)
  message("  Saved normalised badm.rds")
} else {
  message("  No 'Bsk' entries found — BADM already normalised")
}

# Extract one KG code per site (take the first value; no site has duplicates)
kg_per_site <- badm |>
  dplyr::filter(.data$VARIABLE == "CLIMATE_KOEPPEN", !is.na(.data$DATAVALUE)) |>
  dplyr::distinct(.data$SITE_ID, .keep_all = TRUE) |>
  dplyr::select(site_id = "SITE_ID", kg_class = "DATAVALUE") |>
  dplyr::mutate(
    kg_second = substr(.data$kg_class, 1L, 2L),
    kg_main   = substr(.data$kg_class, 1L, 1L)
  )

cat(sprintf("\nKG codes available for %d sites\n", nrow(kg_per_site)))

# ---- 2. Join KG to GEZ candidates and save -----------------------------------

candidates_path <- file.path(SNAPSHOTS_DIR, "long_record_site_candidates_gez.csv")
message("Loading GEZ candidates: ", candidates_path)
candidates <- readr::read_csv(candidates_path, show_col_types = FALSE)

candidates_kg <- candidates |>
  dplyr::left_join(kg_per_site, by = "site_id")

n_with_kg    <- sum(!is.na(candidates_kg$kg_class))
n_without_kg <- sum(is.na(candidates_kg$kg_class))
cat(sprintf(
  "Sites with KG:    %d\nSites without KG: %d (will appear as NA in heatmaps)\n\n",
  n_with_kg, n_without_kg
))

out_csv <- file.path(SNAPSHOTS_DIR, "long_record_site_candidates_gez_kg.csv")
readr::write_csv(candidates_kg, out_csv)
message("Saved enriched table: ", out_csv)

# ---- 3. Build heatmap data ---------------------------------------------------

# Qualifying sites: >= 8 valid NEE years, non-NA subregion and KG
qual_sites <- candidates_kg |>
  dplyr::filter(
    .data$n_years_valid_nee >= MIN_NEE_YEARS,
    !is.na(.data$un_subregion),
    !is.na(.data$kg_class)
  )

cat(sprintf("Qualifying sites (>= %d NEE yr, non-NA subregion+KG): %d\n\n",
            MIN_NEE_YEARS, nrow(qual_sites)))

# Helper: build count grid for one KG level
build_heatmap_data <- function(data, kg_col) {
  data |>
    dplyr::group_by(
      .data$igbp,
      .data$un_subregion,
      kg = .data[[kg_col]]
    ) |>
    dplyr::summarise(n_sites = dplyr::n(), .groups = "drop")
}

heat_main   <- build_heatmap_data(qual_sites, "kg_main")
heat_second <- build_heatmap_data(qual_sites, "kg_second")
heat_third  <- build_heatmap_data(qual_sites, "kg_class")

# ---- 4. Ordered axis levels --------------------------------------------------

# KG main: A B C D E
kg_main_order   <- c("A", "B", "C", "D", "E")

# KG second: standard climatological order
kg_second_order <- c(
  "Af", "Am", "Aw",
  "BS", "BW",
  "Cs", "Cw", "Cf",
  "Ds", "Dw", "Df",
  "ET", "EF"
)

# KG third: all codes present, sorted alphabetically within main group
kg_third_order <- sort(unique(heat_third$kg))

# UN subregions: sort alphabetically (consistent across panels)
subregion_order <- sort(unique(qual_sites$un_subregion))

# IGBP: sort alphabetically
igbp_order <- sort(unique(qual_sites$igbp))

# ---- 5. Plot helper ----------------------------------------------------------

make_kg_heatmap <- function(heat_data, kg_order, title_suffix) {
  # Filter to KG codes that actually appear in the data
  kg_order_present <- kg_order[kg_order %in% heat_data$kg]

  heat_data <- heat_data |>
    dplyr::mutate(
      kg          = factor(.data$kg, levels = kg_order_present),
      un_subregion = factor(.data$un_subregion, levels = rev(subregion_order)),
      igbp        = factor(.data$igbp, levels = igbp_order)
    )

  # Complete grid so empty cells are shown as 0
  heat_complete <- heat_data |>
    tidyr::complete(
      igbp, un_subregion, kg,
      fill = list(n_sites = 0L)
    ) |>
    dplyr::filter(.data$n_sites > 0L | TRUE)  # keep all for full grid

  ggplot2::ggplot(
    heat_complete,
    ggplot2::aes(x = kg, y = un_subregion, fill = n_sites)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(.data$n_sites > 0L,
                               as.character(.data$n_sites), "")
      ),
      size = 3, color = "black"
    ) +
    ggplot2::facet_wrap(~ igbp, ncol = 4) +
    ggplot2::scale_fill_gradient(
      low  = "#EFF3FF",
      high = "#084594",
      na.value = "grey95",
      name = "Sites\n(\u22658 NEE yr)"
    ) +
    ggplot2::labs(
      title = paste("KG availability heatmap —", title_suffix,
                    "(sites with \u22658 valid NEE years)"),
      x = NULL,
      y = NULL
    ) +
    fluxnet_theme() +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1,
                                               vjust = 1, size = 9),
      axis.text.y      = ggplot2::element_text(size = 8),
      strip.text       = ggplot2::element_text(face = "bold", size = 9),
      legend.position  = "right",
      panel.spacing    = ggplot2::unit(0.5, "lines"),
      plot.title       = ggplot2::element_text(face = "bold", size = 12,
                                               hjust = 0.5)
    )
}

# ---- 6. Generate and save heatmaps -------------------------------------------

heatmaps <- list(
  list(
    data   = heat_main,
    order  = kg_main_order,
    suffix = "main group (A/B/C/D/E)",
    file   = "fig_kg_availability_main_8yr.png"
  ),
  list(
    data   = heat_second,
    order  = kg_second_order,
    suffix = "second level (Af/Am/Cf/Df …)",
    file   = "fig_kg_availability_second_8yr.png"
  ),
  list(
    data   = heat_third,
    order  = kg_third_order,
    suffix = "third level (full code)",
    file   = "fig_kg_availability_third_8yr.png"
  )
)

for (h in heatmaps) {
  message("Generating: ", h$file)
  p        <- make_kg_heatmap(h$data, h$order, h$suffix)
  fig_path <- file.path(OUT_DIR, h$file)
  ggplot2::ggsave(
    fig_path, plot = p,
    width = FIGURE_WIDTH, height = FIGURE_HEIGHT,
    units = "in", dpi = FIGURE_DPI,
    bg = "white"
  )
  message("  Saved: ", fig_path)
}

cat("\n================================================================\n")
cat("  DONE\n")
cat("================================================================\n")
cat(sprintf("  Output directory: %s\n", OUT_DIR))
cat(sprintf("  Enriched CSV:     %s\n\n", out_csv))
