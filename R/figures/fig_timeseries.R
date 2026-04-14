# R/figures/fig_timeseries.R
# Long-record annual time series figures for the FLUXNET Annual Paper 2026.
#
# Functions:
#   save_long_record_sites()       — select top sites per continent by record
#                                    span, write data/snapshots/long_record_sites.csv
#   fig_long_record_timeseries()   — annual time series panels per continent

library(ggplot2)
library(ggtext)
library(dplyr)

# ---- Internal helpers -------------------------------------------------------

#' Map flux variable name to axis label (time series context)
#'
#' @param flux_var Character. Column name.
#' @return Character label (HTML safe for element_markdown).
#' @noRd
.flux_ts_label <- function(flux_var) {
  switch(
    sub("_.*", "", flux_var),
    NEE  = lab_nee_annual,
    GPP  = lab_gpp_annual,
    RECO = lab_reco_annual,
    LE   = "LE (MJ m<sup>-2</sup> yr<sup>-1</sup>)",
    H    = "H (MJ m<sup>-2</sup> yr<sup>-1</sup>)",
    flux_var
  )
}

#' Map site ISO-2 prefix to UN Geoscheme continent
#'
#' Applies `countrycode::countrycode()` with a pre-substitution for FLUXNET
#' site ID conventions that differ from ISO 3166-1 alpha-2 (notably `UK` → `GB`
#' for United Kingdom sites).  Returns `NA` for unrecognised codes.
#'
#' @param iso2 Character vector of two-letter country prefixes.
#' @return Character vector of continent names (same length as `iso2`).
#' @noRd
.iso2_to_continent <- function(iso2) {
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop(
      "Package 'countrycode' is required for continent assignment. ",
      "Install with: install.packages('countrycode')",
      call. = FALSE
    )
  }
  # FLUXNET convention: "UK" is used for United Kingdom sites; ISO 3166-1 is "GB"
  iso2_norm <- dplyr::case_when(
    iso2 == "UK" ~ "GB",
    TRUE         ~ iso2
  )
  countrycode::countrycode(iso2_norm, origin = "iso2c", destination = "continent",
                           warn = FALSE)
}

# ---- Exported functions -----------------------------------------------------

#' Select and save top long-record sites per continent
#'
#' Reads the most recent FLUXNET Shuttle snapshot CSV from `snapshot_dir`,
#' assigns continents using the UN Geoscheme 6-continent system via
#' `countrycode::countrycode()`, selects the top `n_sites` sites per continent
#' by record span (`last_year - first_year`), writes the selection to
#' `data/snapshots/long_record_sites.csv`, and prints the table to the
#' console for review.
#'
#' @param n_sites Integer. Number of top sites to select per continent
#'   (default `5`).
#' @param snapshot_dir Character. Directory to search for snapshot CSVs
#'   (default `"data/snapshots"`).
#'
#' @return A data frame with columns `site_id`, `continent`, `first_year`,
#'   `last_year`, `span`, and `igbp`, invisibly.  Side effect: writes
#'   `data/snapshots/long_record_sites.csv` and prints the table.
#'
#' @export
save_long_record_sites <- function(n_sites = 5L,
                                   snapshot_dir = "data/snapshots") {
  # Find the most recent snapshot CSV (lexicographic sort — timestamps sort
  # correctly as YYYYMMDDTHHMMSS)
  csvs <- sort(
    list.files(snapshot_dir,
               pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
               full.names = TRUE),
    decreasing = TRUE
  )
  if (length(csvs) == 0L) {
    stop("No snapshot CSVs found in: ", snapshot_dir, call. = FALSE)
  }
  snap_path <- csvs[[1L]]
  message("Reading snapshot: ", snap_path)

  snap <- readr::read_csv(snap_path, show_col_types = FALSE)

  required_cols <- c("site_id", "igbp", "first_year", "last_year")
  missing_cols  <- setdiff(required_cols, names(snap))
  if (length(missing_cols) > 0L) {
    stop(
      "Snapshot CSV is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Continent via countrycode (UN Geoscheme 6-continent system)
  iso2 <- substr(snap$site_id, 1L, 2L)
  continent <- .iso2_to_continent(iso2)

  site_tbl <- snap |>
    dplyr::select("site_id", "igbp", "first_year", "last_year") |>
    dplyr::mutate(
      continent  = continent,
      first_year = as.integer(.data$first_year),
      last_year  = as.integer(.data$last_year),
      span       = .data$last_year - .data$first_year
    ) |>
    dplyr::filter(!is.na(.data$continent)) |>
    dplyr::group_by(.data$continent) |>
    dplyr::slice_max(order_by = .data$span, n = as.integer(n_sites),
                     with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$continent, dplyr::desc(.data$span)) |>
    dplyr::select("site_id", "continent", "first_year", "last_year", "span", "igbp")

  out_path <- file.path(snapshot_dir, "long_record_sites.csv")
  readr::write_csv(site_tbl, out_path)
  message("Long-record site list written: ", out_path)

  cat("\n--- Top ", n_sites, " sites per continent by record span ---\n", sep = "")
  print(as.data.frame(site_tbl), row.names = FALSE)
  cat("\n")

  invisible(site_tbl)
}

#' Long-record annual time series by continent
#'
#' Selects the top `n_sites` sites per continent (by record span derived from
#' `metadata`) and plots annual time series of each `flux_var` as coloured
#' lines, one panel per variable assembled with \pkg{patchwork}.  Site lines
#' are coloured by IGBP class using [scale_color_igbp()].
#'
#' Continent assignment uses the UN Geoscheme 6-continent system
#' (Africa, Americas, Asia, Europe, Oceania, Antarctica) via
#' `countrycode::countrycode(iso2, "iso2c", "continent")`, where `iso2` is
#' extracted from the first two characters of `site_id`.
#'
#' Only continents represented in `data_yy` appear in the returned list.
#' Flux variables absent from `data_yy` are skipped with a warning.
#'
#' **For complete results ensure `data_yy` contains the full site record.
#' Consider adding top sites per continent to `FLUXNET_SITE_FILTER` for
#' permanent retention.**
#'
#' @param data_yy Annual FLUXNET data frame.  Must contain `site_id`, a year
#'   column (`TIMESTAMP` or `YEAR`), and the columns named in `flux_vars`.
#' @param metadata Site metadata data frame.  Must contain `site_id`,
#'   `first_year`, `last_year`, and `igbp`.
#' @param flux_vars Character vector of flux variable column names to plot
#'   (default: NEE, GPP, RECO, LE, H).
#' @param n_sites Integer.  Number of top-span sites to select per continent
#'   (default `5`).
#' @param geo_level Character.  Aggregation level; currently only
#'   `"continent"` is supported.
#'
#' @return A named list of \pkg{patchwork} objects, one per continent present
#'   in `data_yy`.  Each patchwork has one panel per available `flux_var`.
#'
#' @examples
#' \dontrun{
#' plots <- fig_long_record_timeseries(data_yy, metadata = snapshot_meta)
#' plots[["Europe"]]
#' }
#'
#' @export
fig_long_record_timeseries <- function(
    data_yy,
    metadata,
    flux_vars = c("NEE_VUT_REF", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF",
                  "LE_F_MDS", "H_F_MDS"),
    n_sites   = 5L,
    geo_level = "continent"
) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' is required. ",
      "Install with: install.packages('patchwork')",
      call. = FALSE
    )
  }

  if (!identical(geo_level, "continent")) {
    stop(
      "geo_level = '", geo_level, "' is not supported. ",
      "Only geo_level = 'continent' is currently implemented.",
      call. = FALSE
    )
  }

  # ---- Column checks ----------------------------------------------------------
  if (!"site_id" %in% names(data_yy)) {
    stop("data_yy must contain a 'site_id' column.", call. = FALSE)
  }

  # Accept TIMESTAMP (normalised by pipeline scripts) or YEAR (raw)
  year_col <- if ("TIMESTAMP" %in% names(data_yy)) {
    "TIMESTAMP"
  } else if ("YEAR" %in% names(data_yy)) {
    "YEAR"
  } else {
    stop(
      "data_yy must contain a 'TIMESTAMP' or 'YEAR' column for the x-axis.",
      call. = FALSE
    )
  }

  meta_required <- c("site_id", "first_year", "last_year", "igbp")
  missing_meta  <- setdiff(meta_required, names(metadata))
  if (length(missing_meta) > 0L) {
    stop(
      "metadata is missing required column(s): ",
      paste(missing_meta, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- Continent assignment ---------------------------------------------------
  iso2       <- substr(metadata$site_id, 1L, 2L)
  continent  <- .iso2_to_continent(iso2)

  site_meta <- metadata |>
    dplyr::select("site_id", "first_year", "last_year", "igbp") |>
    dplyr::mutate(
      continent  = continent,
      first_year = as.integer(.data$first_year),
      last_year  = as.integer(.data$last_year),
      span       = .data$last_year - .data$first_year
    ) |>
    dplyr::filter(!is.na(.data$continent))

  # ---- Site selection: top n_sites per continent by span --------------------
  selected_sites <- site_meta |>
    dplyr::group_by(.data$continent) |>
    dplyr::slice_max(order_by = .data$span, n = as.integer(n_sites),
                     with_ties = FALSE) |>
    dplyr::ungroup()

  if (nrow(selected_sites) == 0L) {
    warning(
      "No sites could be assigned to a continent. ",
      "Check that site_id prefixes are valid ISO 3166-1 alpha-2 codes.",
      call. = FALSE
    )
    return(list())
  }

  # ---- Filter flux vars to those present in data_yy -------------------------
  avail_vars <- intersect(flux_vars, names(data_yy))
  skipped    <- setdiff(flux_vars, names(data_yy))
  if (length(skipped) > 0L) {
    warning(
      "flux_var(s) not found in data_yy \u2014 skipped: ",
      paste(skipped, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(avail_vars) == 0L) {
    warning(
      "None of the requested flux_vars are present in data_yy. ",
      "Returning empty list.",
      call. = FALSE
    )
    return(list())
  }

  # ---- Join IGBP and continent onto data_yy ----------------------------------
  data_join <- data_yy |>
    dplyr::filter(.data$site_id %in% selected_sites$site_id) |>
    dplyr::left_join(
      dplyr::select(selected_sites, "site_id", "continent", "igbp", "span"),
      by = "site_id"
    ) |>
    dplyr::mutate(
      IGBP = factor(.data$igbp, levels = IGBP_order),
      year = as.integer(.data[[year_col]])
    )

  continents_in_data <- sort(unique(na.omit(data_join$continent)))

  if (length(continents_in_data) == 0L) {
    warning(
      "No selected sites found in data_yy after filtering. ",
      "Returning empty list.",
      call. = FALSE
    )
    return(list())
  }

  # ---- Build one patchwork per continent -------------------------------------
  out <- list()

  for (cont in continents_in_data) {
    df_cont <- dplyr::filter(data_join, .data$continent == cont)
    sites_present <- unique(df_cont$site_id)

    panels <- list()

    for (fv in avail_vars) {
      df_fv <- dplyr::filter(df_cont, !is.na(.data[[fv]]))
      if (nrow(df_fv) == 0L) next

      y_label    <- .flux_ts_label(fv)
      igbp_lvls  <- intersect(IGBP_order, unique(na.omit(df_fv$IGBP)))
      n_site_lbl <- length(unique(df_fv$site_id))

      p <- ggplot2::ggplot(
        df_fv,
        ggplot2::aes(
          x     = .data$year,
          y     = .data[[fv]],
          color = .data$IGBP,
          group = .data$site_id
        )
      ) +
        ggplot2::geom_line(linewidth = poster_linewidth * 0.9, alpha = 0.85) +
        ggplot2::geom_point(size = 2.0, alpha = 0.70) +
        scale_color_igbp(
          limits = igbp_lvls,
          guide  = ggplot2::guide_legend(ncol = 2, title = "IGBP")
        ) +
        ggplot2::labs(
          y        = y_label,
          x        = "Year",
          subtitle = paste0(cont, " \u2014 top ", n_site_lbl,
                            " site(s) by record span")
        ) +
        fluxnet_theme() +
        ggplot2::theme(
          axis.title.y    = ggtext::element_markdown(),
          legend.position = "right"
        )

      panels[[fv]] <- p
    }

    if (length(panels) == 0L) next

    pw <- patchwork::wrap_plots(panels, ncol = 1) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "right")

    out[[cont]] <- pw
  }

  if (length(out) == 0L) {
    warning(
      "fig_long_record_timeseries(): no plots generated — ",
      "check that data_yy contains flux_vars for sites in selected continents.",
      call. = FALSE
    )
  }

  out
}
