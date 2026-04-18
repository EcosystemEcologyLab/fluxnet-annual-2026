# scripts/generate_gez_anomaly_figures.R
#
# Generates anomaly context figures for all qualifying
# IGBP × UN subregion × GEZ combinations.
#
# Qualifying criteria:
#   - ALL IGBP classes (no forest-only restriction)
#   - At least 4 sites with >= 8 valid NEE years in the combination
#
# Output: review/figures/Anomalies_GEZ/fig_anomaly_{igbp}_{subregion_clean}_{gez_clean}.png
#
# Run after 05_units.R has produced flux_data_converted_yy.rds.

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

# GEZ lookup
gez_lookup_path <- file.path(snapshots_dir, "site_gez_lookup.csv")
if (!fs::file_exists(gez_lookup_path)) {
  stop("site_gez_lookup.csv not found at: ", gez_lookup_path, call. = FALSE)
}
message("Loading GEZ lookup: ", gez_lookup_path)
gez_lookup <- readr::read_csv(gez_lookup_path, show_col_types = FALSE)

# GEZ-enriched candidates (source of n_years_valid_nee per site)
candidates_path <- file.path(snapshots_dir, "long_record_site_candidates_gez.csv")
if (!fs::file_exists(candidates_path)) {
  stop("long_record_site_candidates_gez.csv not found at: ", candidates_path,
       call. = FALSE)
}
message("Loading GEZ candidates: ", candidates_path)
candidates <- readr::read_csv(candidates_path, show_col_types = FALSE)

# ---- Determine qualifying combinations ---------------------------------------

qualifying_sites <- candidates |>
  dplyr::filter(
    .data$n_years_valid_nee >= MIN_NEE_YEARS,
    !is.na(.data$un_subregion),
    !is.na(.data$gez_name)
  )

combo_counts <- qualifying_sites |>
  dplyr::group_by(.data$igbp, .data$un_subregion, .data$gez_name) |>
  dplyr::summarise(
    n_sites_qualifying = dplyr::n(),
    .groups            = "drop"
  ) |>
  dplyr::filter(.data$n_sites_qualifying >= MIN_SITES) |>
  dplyr::arrange(.data$igbp, .data$un_subregion, .data$gez_name)

# ---- Print summary table before generating -----------------------------------

cat("\n")
cat("================================================================\n")
cat(sprintf(
  "  QUALIFYING COMBINATIONS (all IGBP, >= %d sites, >= %d NEE yr)\n",
  MIN_SITES, MIN_NEE_YEARS
))
cat("================================================================\n\n")

if (nrow(combo_counts) == 0L) {
  cat("  No qualifying combinations found.\n\n")
  message("No qualifying combinations found — no figures generated.")
  quit(save = "no", status = 0)
}

cat(sprintf("  %-6s  %-30s  %-38s  %s\n",
            "igbp", "un_subregion", "gez_name", "n_sites_qualifying"))
cat("  ", strrep("-", 86), "\n", sep = "")
for (i in seq_len(nrow(combo_counts))) {
  r <- combo_counts[i, ]
  cat(sprintf("  %-6s  %-30s  %-38s  %d\n",
              r$igbp, r$un_subregion, r$gez_name, r$n_sites_qualifying))
}
cat("\n")
cat(sprintf("  Figures to generate: %d\n\n", nrow(combo_counts)))

# ---- Set up output directory -------------------------------------------------

out_dir <- file.path("review", "figures", "Anomalies_GEZ")
fs::dir_create(out_dir)

# ---- Helper: sanitise a string for use in a file name -----------------------

clean_name <- function(x) {
  x |>
    tolower() |>
    gsub(pattern = "[^a-z0-9 ]", replacement = "", perl = TRUE) |>
    trimws() |>
    gsub(pattern = "\\s+", replacement = "_", perl = TRUE)
}

# ---- Generate one figure per qualifying combination -------------------------

failures <- list()   # named list: fig_name -> reason string
n_ok     <- 0L

for (i in seq_len(nrow(combo_counts))) {
  r         <- combo_counts[i, ]
  igbp_i    <- r$igbp
  subreg_i  <- r$un_subregion
  gez_i     <- r$gez_name
  n_sites_i <- r$n_sites_qualifying

  fig_name <- sprintf(
    "fig_anomaly_%s_%s_%s.png",
    clean_name(igbp_i),
    clean_name(subreg_i),
    clean_name(gez_i)
  )
  fig_path <- file.path(out_dir, fig_name)

  message(sprintf(
    "Generating [%d/%d]: %s \u00d7 %s \u00d7 %s (n=%d sites)",
    i, nrow(combo_counts), igbp_i, subreg_i, gez_i, n_sites_i
  ))

  p <- tryCatch(
    fig_anomaly_context(
      data_yy       = data_yy,
      metadata      = snapshot_meta,
      gez_lookup    = gez_lookup,
      igbp          = igbp_i,
      gez_filter    = gez_i,
      subregion     = subreg_i,
      recent_years  = RECENT_YEARS,
      min_sites     = MIN_SITES,
      min_nee_years = MIN_NEE_YEARS
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

# ---- Final summary -----------------------------------------------------------

n_fail <- length(failures)

cat("\n")
cat("================================================================\n")
cat("  DONE\n")
cat("================================================================\n")
cat(sprintf("  Successfully generated: %d / %d\n", n_ok, nrow(combo_counts)))

if (n_fail > 0L) {
  cat(sprintf("  Failed:                 %d\n\n", n_fail))
  cat("  Failed combinations:\n")
  for (nm in names(failures)) {
    cat(sprintf("    %s\n      Reason: %s\n", nm, failures[[nm]]))
  }
} else {
  cat("  Failed:                 0\n")
}

cat(sprintf("  Output directory:       %s\n\n", out_dir))
