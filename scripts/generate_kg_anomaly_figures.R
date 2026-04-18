# scripts/generate_kg_anomaly_figures.R
#
# Generates anomaly context figures for all qualifying
# IGBP × UN subregion × KG class combinations, at two levels:
#
#   Level 1 (kg_main)   — single character: A / B / C / D / E
#   Level 2 (kg_second) — two characters:   e.g. Cf, Df, Bs, Aw
#
# Qualifying criteria (per level):
#   - At least MIN_SITES sites with >= MIN_NEE_YEARS valid NEE years
#
# Output:
#   review/figures/Anomalies_KG/level1/fig_anomaly_{igbp}_{subregion}_{kg}.png
#   review/figures/Anomalies_KG/level2/fig_anomaly_{igbp}_{subregion}_{kg}.png
#
# Run after 05_units.R and generate_kg_availability_heatmaps.R

source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")
source("R/figures/fig_anomaly_context.R")

suppressPackageStartupMessages({
  library(dplyr)
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

MIN_SITES     <- 4L
MIN_NEE_YEARS <- 8L
RECENT_YEARS  <- 2019:2024
FLUX_VARS     <- c("NEE_VUT_REF", "LE_F_MDS", "H_F_MDS")
FIGURE_WIDTH  <- 10   # inches
FIGURE_HEIGHT <- 12   # inches
FIGURE_DPI    <- 150

# ---- Load data ---------------------------------------------------------------

processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")
snapshots_dir <- file.path(FLUXNET_DATA_ROOT, "snapshots")

# Annual flux data (converted units preferred)
yy_path <- if (fs::file_exists(file.path(processed_dir, "flux_data_converted_yy.rds"))) {
  file.path(processed_dir, "flux_data_converted_yy.rds")
} else if (fs::file_exists(file.path(processed_dir, "flux_data_qc_yy.rds"))) {
  file.path(processed_dir, "flux_data_qc_yy.rds")
} else {
  stop("No annual processed data found in ", processed_dir,
       ". Run 05_units.R first.", call. = FALSE)
}
message("Loading annual flux data: ", yy_path)
data_yy <- readRDS(yy_path)

# Latest shuttle snapshot for site metadata
snapshot_csv <- sort(
  fs::dir_ls(snapshots_dir, glob = "*fluxnet_shuttle_snapshot*.csv"),
  decreasing = TRUE
)
if (length(snapshot_csv) == 0L) {
  stop("No shuttle snapshot CSV found in ", snapshots_dir, call. = FALSE)
}
message("Using snapshot: ", snapshot_csv[[1]])
snapshot_meta <- readr::read_csv(
  snapshot_csv[[1]],
  show_col_types = FALSE
) |>
  dplyr::select(site_id, data_hub, igbp, location_lat, location_long,
                dplyr::any_of(c("first_year", "last_year")))

# KG-enriched candidates (source of kg_main / kg_second / kg_class per site)
kg_candidates_path <- file.path(snapshots_dir, "long_record_site_candidates_gez_kg.csv")
if (!fs::file_exists(kg_candidates_path)) {
  stop(
    "long_record_site_candidates_gez_kg.csv not found at: ", kg_candidates_path,
    ". Run generate_kg_availability_heatmaps.R first.",
    call. = FALSE
  )
}
message("Loading KG candidates: ", kg_candidates_path)
kg_candidates <- readr::read_csv(kg_candidates_path, show_col_types = FALSE)

# Build a slim per-site KG lookup (site_id + all three KG columns)
kg_lookup <- kg_candidates |>
  dplyr::select(site_id, kg_main, kg_second, kg_class) |>
  dplyr::distinct() |>
  dplyr::filter(!is.na(.data$kg_class))

# ---- Helper: sanitise a string for use in a file name -----------------------

clean_name <- function(x) {
  x |>
    tolower() |>
    gsub(pattern = "[^a-z0-9 ]", replacement = "", perl = TRUE) |>
    trimws() |>
    gsub(pattern = "\\s+", replacement = "_", perl = TRUE)
}

# ---- Helper: determine qualifying combos for one KG level -------------------

qualifying_combos <- function(kg_col_name) {
  qual_sites <- kg_candidates |>
    dplyr::filter(
      .data$n_years_valid_nee >= MIN_NEE_YEARS,
      !is.na(.data$un_subregion),
      !is.na(.data[[kg_col_name]])
    )

  qual_sites |>
    dplyr::group_by(
      .data$igbp,
      .data$un_subregion,
      kg_class = .data[[kg_col_name]]
    ) |>
    dplyr::summarise(
      n_sites_qualifying = dplyr::n(),
      .groups            = "drop"
    ) |>
    dplyr::filter(.data$n_sites_qualifying >= MIN_SITES) |>
    dplyr::arrange(.data$igbp, .data$un_subregion, .data$kg_class)
}

combo_l1 <- qualifying_combos("kg_main")
combo_l2 <- qualifying_combos("kg_second")

# ---- Print qualifying combination tables ------------------------------------

print_combo_table <- function(combos, level_label) {
  cat("\n")
  cat("================================================================\n")
  cat(sprintf(
    "  QUALIFYING COMBINATIONS — %s (>= %d sites, >= %d NEE yr)\n",
    level_label, MIN_SITES, MIN_NEE_YEARS
  ))
  cat("================================================================\n\n")

  if (nrow(combos) == 0L) {
    cat("  No qualifying combinations found.\n\n")
    return(invisible(NULL))
  }

  cat(sprintf("  %-6s  %-30s  %-12s  %s\n",
              "igbp", "un_subregion", "kg_class", "n_sites_qualifying"))
  cat("  ", strrep("-", 64), "\n", sep = "")
  for (i in seq_len(nrow(combos))) {
    r <- combos[i, ]
    cat(sprintf("  %-6s  %-30s  %-12s  %d\n",
                r$igbp, r$un_subregion, r$kg_class, r$n_sites_qualifying))
  }
  cat("\n")
  cat(sprintf("  Figures to generate: %d\n\n", nrow(combos)))
}

print_combo_table(combo_l1, "Level 1 (kg_main — A/B/C/D/E)")
print_combo_table(combo_l2, "Level 2 (kg_second — Af/Cf/Df…)")

# ---- Helper: generate figures for one level ----------------------------------

generate_level <- function(combos, kg_col_name, out_subdir) {
  if (nrow(combos) == 0L) {
    message("No qualifying combinations for ", kg_col_name, " — skipping.")
    return(invisible(NULL))
  }

  out_dir <- file.path("review", "figures", "Anomalies_KG", out_subdir)
  fs::dir_create(out_dir)

  failures <- list()
  n_ok     <- 0L

  for (i in seq_len(nrow(combos))) {
    r         <- combos[i, ]
    igbp_i    <- r$igbp
    subreg_i  <- r$un_subregion
    kg_i      <- r$kg_class
    n_sites_i <- r$n_sites_qualifying

    fig_name <- sprintf(
      "fig_anomaly_%s_%s_%s.png",
      clean_name(igbp_i),
      clean_name(subreg_i),
      clean_name(kg_i)
    )
    fig_path <- file.path(out_dir, fig_name)

    message(sprintf(
      "Generating [%d/%d]: %s \u00d7 %s \u00d7 %s=%s (n=%d sites)",
      i, nrow(combos), igbp_i, subreg_i, kg_col_name, kg_i, n_sites_i
    ))

    p <- tryCatch(
      fig_anomaly_context_kg(
        data_yy       = data_yy,
        metadata      = snapshot_meta,
        kg_lookup     = kg_lookup,
        igbp          = igbp_i,
        kg_filter     = kg_i,
        kg_col        = kg_col_name,
        subregion     = subreg_i,
        recent_years  = RECENT_YEARS,
        min_sites     = MIN_SITES,
        min_nee_years = MIN_NEE_YEARS,
        flux_vars     = FLUX_VARS
      ),
      error = function(e) {
        failures[[fig_name]] <<- conditionMessage(e)
        NULL
      }
    )

    if (is.null(p)) next

    save_ok <- tryCatch({
      ggplot2::ggsave(
        fig_path, plot = p,
        width = FIGURE_WIDTH, height = FIGURE_HEIGHT,
        units = "in", dpi = FIGURE_DPI,
        bg    = "white"
      )
      TRUE
    }, error = function(e) {
      failures[[fig_name]] <<- paste("ggsave failed:", conditionMessage(e))
      FALSE
    })

    if (save_ok) {
      message("  Saved: ", fig_path)
      n_ok <- n_ok + 1L
    }
  }

  cat("\n")
  cat(sprintf("  %s: %d / %d succeeded", out_subdir, n_ok, nrow(combos)))
  n_fail <- length(failures)
  if (n_fail > 0L) {
    cat(sprintf(", %d failed\n", n_fail))
    for (nm in names(failures)) {
      cat(sprintf("    %s\n      Reason: %s\n", nm, failures[[nm]]))
    }
  } else {
    cat(", 0 failed\n")
  }

  invisible(n_ok)
}

# ---- Generate both levels ----------------------------------------------------

cat("================================================================\n")
cat("  GENERATING FIGURES\n")
cat("================================================================\n")

generate_level(combo_l1, "kg_main",   "level1")
generate_level(combo_l2, "kg_second", "level2")

cat("\n")
cat("================================================================\n")
cat("  DONE\n")
cat(sprintf("  Output root: review/figures/Anomalies_KG/\n"))
cat("================================================================\n\n")
