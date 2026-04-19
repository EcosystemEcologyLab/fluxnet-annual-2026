# R/historical_datasets.R
# Unified site metadata for four historical FLUXNET datasets.
#
# Datasets (chronological):
#   marconi     — Marconi 2000 workshop dataset (35 sites)
#   la_thuile   — La Thuile 2007 release       (252 sites)
#   fluxnet2015 — FLUXNET2015 release           (212 sites)
#   shuttle     — FLUXNET Shuttle / modern 2025 (672 sites)
#
# Functions:
#   load_historical_site_lists()  — returns a named list of four enriched data frames

# ---- Constants ---------------------------------------------------------------

#' Publication year for each historical FLUXNET dataset
DATASET_PUB_YEARS <- c(
  marconi     = 2000L,
  la_thuile   = 2007L,
  fluxnet2015 = 2015L,
  shuttle     = 2025L
)

#' Display label for each dataset (used in figure titles and panel annotations)
DATASET_DISPLAY_LABELS <- c(
  marconi     = "Marconi 2000",
  la_thuile   = "La Thuile 2007",
  fluxnet2015 = "FLUXNET2015",
  shuttle     = "Shuttle 2025"
)

# ---- Internal helper ---------------------------------------------------------

#' Assign UN M.49 subregion from FLUXNET site_id prefix
#'
#' Recodes non-standard "UK" prefix to ISO "GB" before lookup.
#'
#' @param site_ids Character vector of FLUXNET site IDs.
#' @return Character vector of UN subregion names (NA for unresolved prefixes).
#' @noRd
.subregion_from_site_id <- function(site_ids) {
  iso2 <- dplyr::if_else(
    substr(site_ids, 1L, 2L) == "UK", "GB",
    substr(site_ids, 1L, 2L)
  )
  countrycode::countrycode(iso2, "iso2c", "un.regionsub.name", warn = FALSE)
}

# ---- Exported function -------------------------------------------------------

#' Load and enrich site metadata for all four historical FLUXNET datasets
#'
#' Returns a named list of four data frames (marconi, la_thuile, fluxnet2015,
#' shuttle). Each is enriched with FLUXNET Shuttle snapshot metadata (coordinates,
#' IGBP, first/last year, NEE record length) using a priority hierarchy:
#'
#' 1. **Shuttle snapshot**: use Shuttle coordinates, IGBP, first_year, last_year
#'    when the site is present.
#' 2. **Historical source**: fall back to the dataset's own coordinates and IGBP
#'    when the site is absent from the Shuttle.
#'
#' Record length (`record_length`) is:
#' - Shuttle sites with `n_years_valid_nee`: use that value directly.
#' - Shuttle sites without it: `last_year - first_year`.
#' - Non-Shuttle sites: `last_year - first_year` (Marconi only, from "Years in
#'   Marconi" field); `NA` for La Thuile / FLUXNET2015 sites not in Shuttle
#'   (their source files lack first/last year data).
#'
#' @param shuttle_snapshot Character or `NULL`. Path to the Shuttle snapshot
#'   CSV. If `NULL` (default), the most recent snapshot in `data/snapshots/`
#'   is used automatically.
#' @param marconi_path Character. Path to the Marconi Excel mapping table
#'   (default `"data/lists/Marconi_to_Modern_SiteIDs.xlsx"`).
#' @param la_thuile_path Character. Path to the La Thuile clean CSV
#'   (default `"data/snapshots/sites_la_thuile_clean.csv"`).
#' @param fluxnet2015_path Character. Path to the FLUXNET2015 clean CSV
#'   (default `"data/snapshots/sites_fluxnet2015_clean.csv"`).
#' @param long_record_path Character. Path to `long_record_site_candidates_gez_kg.csv`
#'   (default `"data/snapshots/long_record_site_candidates_gez_kg.csv"`).
#'
#' @return Named list with elements `marconi`, `la_thuile`, `fluxnet2015`,
#'   `shuttle`. Each data frame contains: `site_id`, `location_lat`,
#'   `location_long`, `igbp`, `first_year` (integer), `last_year` (integer),
#'   `record_length` (numeric, may be NA), `in_shuttle` (logical),
#'   `un_subregion` (character), `dataset` (character label).
#'
#' @examples
#' \dontrun{
#' site_lists <- load_historical_site_lists()
#' lapply(site_lists, nrow)
#' # $marconi [1] 35
#' # $la_thuile [1] 252
#' # $fluxnet2015 [1] 212
#' # $shuttle [1] 672
#' }
load_historical_site_lists <- function(
  shuttle_snapshot = NULL,
  marconi_path     = "data/lists/Marconi_to_Modern_SiteIDs.xlsx",
  la_thuile_path   = "data/snapshots/sites_la_thuile_clean.csv",
  fluxnet2015_path = "data/snapshots/sites_fluxnet2015_clean.csv",
  long_record_path = "data/snapshots/long_record_site_candidates_gez_kg.csv"
) {
  for (pkg in c("countrycode", "readxl", "dplyr", "readr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('",
           pkg, "')", call. = FALSE)
    }
  }

  # --- Resolve shuttle snapshot path ------------------------------------------
  if (is.null(shuttle_snapshot)) {
    snap_files <- sort(
      list.files("data/snapshots",
                 pattern = "fluxnet_shuttle_snapshot_.*\\.csv",
                 full.names = TRUE),
      decreasing = TRUE
    )
    if (length(snap_files) == 0L) {
      stop("No shuttle snapshot found in data/snapshots/. ",
           "Run 01_download.R first.", call. = FALSE)
    }
    shuttle_snapshot <- snap_files[[1L]]
    message("load_historical_site_lists: using snapshot: ",
            basename(shuttle_snapshot))
  }

  # --- Load Shuttle snapshot --------------------------------------------------
  shuttle_snap <- readr::read_csv(shuttle_snapshot, show_col_types = FALSE) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::select("site_id", "location_lat", "location_long",
                  "igbp", "first_year", "last_year") |>
    dplyr::mutate(
      first_year = as.integer(.data$first_year),
      last_year  = as.integer(.data$last_year)
    )

  shuttle_ids <- shuttle_snap$site_id

  # --- Load n_years_valid_nee from long-record candidates --------------------
  long_record <- readr::read_csv(long_record_path, show_col_types = FALSE) |>
    dplyr::select("site_id", "n_years_valid_nee")

  # Shuttle enrichment reference: snapshot + n_years_valid_nee
  shuttle_ref <- dplyr::left_join(shuttle_snap, long_record, by = "site_id") |>
    # Rename for clean join semantics (avoids suffix collisions)
    dplyr::rename(
      sh_lat   = "location_lat",
      sh_long  = "location_long",
      sh_igbp  = "igbp",
      sh_first = "first_year",
      sh_last  = "last_year",
      sh_nee   = "n_years_valid_nee"
    )

  # --- Internal enrichment function ------------------------------------------
  # raw: data frame with at least site_id (and optionally location_lat/long,
  #      igbp, first_year, last_year). Missing columns filled with NA before join.
  .enrich <- function(raw, dataset_label) {
    if (!"location_lat"  %in% names(raw)) raw$location_lat  <- NA_real_
    if (!"location_long" %in% names(raw)) raw$location_long <- NA_real_
    if (!"igbp"          %in% names(raw)) raw$igbp          <- NA_character_
    if (!"first_year"    %in% names(raw)) raw$first_year    <- NA_integer_
    if (!"last_year"     %in% names(raw)) raw$last_year     <- NA_integer_

    raw |>
      dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
      dplyr::left_join(shuttle_ref, by = "site_id") |>
      dplyr::mutate(
        in_shuttle    = .data$site_id %in% shuttle_ids,
        # Shuttle coordinates take priority (more precise / current)
        location_lat  = dplyr::coalesce(
          as.numeric(.data$sh_lat),   as.numeric(.data$location_lat)
        ),
        location_long = dplyr::coalesce(
          as.numeric(.data$sh_long),  as.numeric(.data$location_long)
        ),
        igbp          = dplyr::coalesce(
          as.character(.data$sh_igbp), as.character(.data$igbp)
        ),
        first_year    = dplyr::coalesce(
          as.integer(.data$sh_first),  as.integer(.data$first_year)
        ),
        last_year     = dplyr::coalesce(
          as.integer(.data$sh_last),   as.integer(.data$last_year)
        ),
        record_length = dplyr::case_when(
          .data$in_shuttle & !is.na(.data$sh_nee) ~
            as.numeric(.data$sh_nee),
          !is.na(.data$first_year) & !is.na(.data$last_year) ~
            as.numeric(.data$last_year - .data$first_year),
          TRUE ~ NA_real_
        ),
        un_subregion  = .subregion_from_site_id(.data$site_id),
        dataset       = dataset_label
      ) |>
      dplyr::select(
        "site_id", "location_lat", "location_long", "igbp",
        "first_year", "last_year", "record_length",
        "in_shuttle", "un_subregion", "dataset"
      )
  }

  # --- Marconi ----------------------------------------------------------------
  marconi_raw <- readxl::read_excel(marconi_path) |>
    dplyr::rename(
      site_id          = `Modern Site ID`,
      location_lat     = Latitude,
      location_long    = Longitude,
      igbp             = IGBP,
      years_in_marconi = `Years in Marconi`
    ) |>
    dplyr::mutate(
      # Parse "1997-1998" → first 1997, last 1998; "1994" → first=last=1994
      first_year = as.integer(
        sub("^(\\d{4}).*", "\\1", .data$years_in_marconi)
      ),
      last_year  = suppressWarnings(
        as.integer(sub("^\\d{4}-(\\d{4})$", "\\1", .data$years_in_marconi))
      ),
      last_year  = dplyr::if_else(
        is.na(.data$last_year), .data$first_year, .data$last_year
      )
    )

  marconi <- .enrich(marconi_raw, DATASET_DISPLAY_LABELS[["marconi"]])

  # --- La Thuile --------------------------------------------------------------
  la_thuile_raw <- readr::read_csv(la_thuile_path, show_col_types = FALSE)
  la_thuile     <- .enrich(la_thuile_raw, DATASET_DISPLAY_LABELS[["la_thuile"]])

  # --- FLUXNET2015 ------------------------------------------------------------
  fluxnet2015_raw <- readr::read_csv(fluxnet2015_path, show_col_types = FALSE)
  fluxnet2015     <- .enrich(fluxnet2015_raw, DATASET_DISPLAY_LABELS[["fluxnet2015"]])

  # --- Shuttle (full) ---------------------------------------------------------
  shuttle_full <- shuttle_ref |>
    dplyr::rename(
      location_lat  = "sh_lat",
      location_long = "sh_long",
      igbp          = "sh_igbp",
      first_year    = "sh_first",
      last_year     = "sh_last"
    ) |>
    dplyr::mutate(
      in_shuttle    = TRUE,
      record_length = dplyr::if_else(
        !is.na(.data$sh_nee),
        as.numeric(.data$sh_nee),
        as.numeric(.data$last_year - .data$first_year)
      ),
      un_subregion  = .subregion_from_site_id(.data$site_id),
      dataset       = DATASET_DISPLAY_LABELS[["shuttle"]]
    ) |>
    dplyr::select(
      "site_id", "location_lat", "location_long", "igbp",
      "first_year", "last_year", "record_length",
      "in_shuttle", "un_subregion", "dataset"
    )

  site_lists <- list(
    marconi     = marconi,
    la_thuile   = la_thuile,
    fluxnet2015 = fluxnet2015,
    shuttle     = shuttle_full
  )

  # --- Print summary table ----------------------------------------------------
  cat("\n--- Historical dataset summary ---\n")
  cat(sprintf("  %-16s %6s %12s %12s\n",
              "Dataset", "Total", "In Shuttle", "Fallback meta"))
  cat(strrep("-", 52), "\n")
  for (nm in names(site_lists)) {
    df         <- site_lists[[nm]]
    n_total    <- nrow(df)
    n_in       <- sum(df$in_shuttle, na.rm = TRUE)
    n_fallback <- n_total - n_in
    cat(sprintf("  %-16s %6d %12d %12d\n",
                nm, n_total, n_in, n_fallback))
  }
  cat("\n")

  site_lists
}
