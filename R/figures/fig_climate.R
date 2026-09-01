# R/figures/fig_climate.R
# Climate-flux scatter figures for the FLUXNET Annual Paper 2026.
#
# Ported from legacy/demo_fluxnet_plots.R and legacy/fcn_plot_FLUXNET.R.
# Reference only — do not edit legacy/ originals.
#
# Functions:
#   fig_whittaker_worldclim()  — Whittaker hexbin (WorldClim climate, all site lists)
#   fig_climate_scatter()      — Precipitation vs NEE + Temperature vs GPP
#   fig_xy_annual()            — General XY scatter with IGBP shapes
#
# Deprecated (moved to R/figures/fig_climate_legacy.R):
#   fig_whittaker_hexbin()           — use fig_whittaker_worldclim() instead
#   fig_whittaker_hexbin_era5()      — use fig_whittaker_worldclim() instead
#   fig_whittaker_hexbin_worldclim() — use fig_whittaker_worldclim() instead

library(ggplot2)
library(dplyr)
library(colorspace)

# ---- Shared Whittaker style constants ----------------------------------------

#' Shared visual parameters for all Whittaker biome figures
#'
#' A named list used as the default \code{style} argument to
#' \code{\link{fig_whittaker_worldclim}}.  Override individual elements by
#' passing a modified copy to the function.
#'
#' @format A named list with elements:
#' \describe{
#'   \item{xlim}{MAT axis limits in °C.}
#'   \item{ylim}{MAP axis limits in mm yr⁻¹.}
#'   \item{width_in, height_in}{Default ggsave dimensions in inches.}
#'   \item{legend_pos, legend_just}{Legend position and justification (NDC).}
#'   \item{detail_x, detail_y}{Inset detail-text anchor (NDC fractions).}
#'   \item{nee_lims}{Colour-scale limits; computed at runtime when \code{NULL}.}
#' }
#' @note Axis and legend labels are \code{expression()} objects constructed at
#'   plot-build time in \code{\link{fig_whittaker_worldclim}} — they are not
#'   stored here, because R expressions cannot be stored in a plain list and
#'   retrieved as expressions.
#' @export
WHITTAKER_STYLE <- list(
  xlim        = c(-15, 35),
  ylim        = c(0, 4000),
  width_in    = 14,
  height_in   = 7,
  legend_pos  = c(0.02, 0.88),
  legend_just = c(0, 1),
  detail_x    = 0.02,
  detail_y    = 0.98,
  nee_lims    = NULL,
  # Absolute point sizes below are calibrated for the 14x7in poster canvas.
  # Callers rendering at a different physical size (e.g. a single-column
  # square draft-manuscript figure) should pass a modified copy with smaller
  # values — see generate_whittaker.R's whit01_style override.
  axis_text_size    = 22,
  axis_title_size   = 24,
  legend_text_size  = 20,
  legend_title_size = 22,
  detail_text_size  = 6.5,
  colorbar_width    = 15,
  colorbar_height   = 0.8
)

# ---- fig_whittaker_worldclim ------------------------------------------------

#' Whittaker biome hexbin — WorldClim climate, any site list
#'
#' Plots sites on a MAT × MAP climate space using hexagonal binning with each
#' hex coloured by the median site-mean NEE from \code{data_yy}.  Sites present
#' in \code{site_meta} but absent from the Shuttle flux data render as grey
#' hexbins (NA bins are not drawn).
#'
#' Climate source priority per site:
#' \enumerate{
#'   \item \code{worldclim_csv} — pre-computed table (fast, Codespace-safe).
#'   \item \code{terra::extract()} from \code{worldclim_dir} GeoTIFFs — used
#'     only for sites not already in the CSV.
#' }
#'
#' NEE colour-scale limits are derived from the 5th–95th percentile of the
#' \emph{full} \code{data_yy} distribution (before any site or year filtering),
#' ensuring a shared scale across all panels in a composite figure.  Pass a
#' pre-computed \code{style$nee_lims} to override.
#'
#' @param data_yy Annual FLUXNET data frame (Shuttle \code{flux_data_converted_yy.rds}).
#'   Must contain \code{site_id}, \code{NEE_VUT_REF}, and \code{YEAR}.
#' @param site_meta Data frame of sites to display.  Must contain \code{site_id},
#'   \code{location_lat}, and \code{location_long}.  For \code{year_cutoff}
#'   filtering, also needs \code{first_year}.
#' @param worldclim_dir Character. Directory of WorldClim 2.1 2.5m bio GeoTIFFs
#'   (default \code{"data/external/worldclim/climate/wc2.1_2.5m/"}).
#' @param worldclim_csv Character. Path to pre-computed per-site WorldClim table
#'   (default \code{"data/snapshots/site_worldclim.csv"}).
#' @param year_cutoff Integer or \code{NULL}.  When set, \code{site_meta} is
#'   filtered to \code{first_year <= year_cutoff} and \code{data_yy} records to
#'   \code{YEAR <= year_cutoff}.
#' @param detail_label Character or \code{NULL}.  Dataset label shown in the
#'   inset text (e.g. \code{"FLUXNET Shuttle 2025"}).
#' @param style Named list of visual parameters.  Defaults to
#'   \code{\link{WHITTAKER_STYLE}}.
#' @param hex_regular Logical (default \code{FALSE}, preserving existing
#'   behaviour for all current callers). When \code{TRUE}, hexagon
#'   \code{binwidth} is computed explicitly from \code{style$xlim}/
#'   \code{style$ylim} (instead of ggplot2's default, which sizes bins from
#'   the full unclipped data range) and \code{\link[ggplot2]{coord_fixed}} is
#'   used in place of \code{\link[ggplot2]{coord_cartesian}}, with
#'   \code{ratio} set so that one MAT unit and one MAP unit render at a fixed
#'   physical-length ratio. Without this, hexagons drawn via
#'   \code{ggplot2::stat_summary_hex()} are only visually regular by
#'   coincidence -- their shape depends on the rendered panel's incidental
#'   physical aspect ratio (legend/axis-label margins, requested figure
#'   width/height), not on the MAT/MAP data ranges, and stretch whenever that
#'   incidental aspect ratio doesn't match \code{diff(style$ylim)/diff(style$xlim)}.
#' @param hex_bins Integer, number of hexagon bins spanning \code{style$xlim}
#'   (and, when \code{hex_regular = TRUE}, \code{style$ylim}). Default 15,
#'   matching the previous hardcoded value.
#' @param points_in_front Logical (default \code{FALSE}, preserving existing
#'   behaviour). When \code{FALSE}, per-site points are drawn behind the
#'   hexagons (visible only where they poke out). When \code{TRUE}, hexagons
#'   are drawn first and points are drawn on top.
#' @param point_size Numeric, per-site point size (default 1.4, matching the
#'   previous hardcoded value).
#' @param point_colour Character, per-site point colour (default
#'   \code{"grey30"}, matching the previous hardcoded value).
#' @param point_alpha Numeric, per-site point alpha/opacity (default
#'   \code{0.50}, matching the previous hardcoded value).
#' @param nee_mid_colour Character or \code{NULL} (default \code{NULL},
#'   preserving existing behaviour: the stock
#'   \code{colorspace::scale_fill_continuous_diverging(palette = "Blue-Red 3")}
#'   scale, whose centre is near-white, \code{"#F6F6F6"}). When a hex colour
#'   is supplied, the fill scale is rebuilt as
#'   \code{ggplot2::scale_fill_gradient2()} with \code{low}/\code{high}
#'   fixed to \code{colorspace::diverging_hcl(2, palette = "Blue-Red 3")}'s
#'   exact endpoint colours (\code{"#002F70"}/\code{"#5F1415"}) -- identical
#'   blue/red ends, identical \code{midpoint = 0} zero-centring, identical
#'   \code{limits}/\code{oob} -- and \code{mid} set to \code{nee_mid_colour}.
#'   (A same-endpoints custom HCL two-half reconstruction, matching
#'   \code{diverging_hcl}'s own polar-coordinate interpolation exactly, was
#'   tried first and rejected: linearly sweeping hue from blue (H=255) to an
#'   off-hue tan centre (H~90) passes THROUGH green/teal hues partway, which
#'   is not hidden the way it is in the stock palette, where chroma drops to
#'   0 at the achromatic white centre before hue would matter.
#'   \code{gradient2}'s Lab-space interpolation has no such hue-wraparound
#'   artifact.)
#' @param detail_lines Character vector or \code{NULL} (default \code{NULL},
#'   preserving existing behaviour: a single auto-built
#'   \code{"N = <n_sites> sites | <n_site_years> site-years"} line). When
#'   supplied, these lines (each rendered on its own line, after
#'   \code{detail_label} if also given) replace that auto-built line
#'   verbatim -- e.g. to report additional counts (sites with vs. without
#'   qualifying NEE) without hardcoding wording into this shared function.
#' @param detail_hjust Numeric (default \code{-0.07}, matching the previous
#'   hardcoded value). \code{hjust} for the inset \code{annotate("text")}
#'   call. Grid's multi-line text justification applies a non-zero
#'   \code{hjust} proportionally to each line's own string width, so with
#'   \code{detail_lines} of similar length but different width (e.g. several
#'   short count lines), the default \code{-0.07} visibly mis-aligns lines
#'   left/right of each other by a few pixels; pass \code{0} (true per-line
#'   left-justification) for clean multi-line alignment.
#' @param detail_x_offset Numeric, in MAT data units (default \code{0},
#'   preserving existing behaviour: \code{x = -Inf}, i.e. the text starts
#'   exactly at the panel's left edge -- with \code{detail_hjust = 0} this
#'   sits flush against the axis, no margin). When non-zero, \code{x} is set
#'   to \code{style$xlim[1] + detail_x_offset} instead, shifting the whole
#'   inset text block right by that many data units to clear the axis with a
#'   margin. Does not affect the legend/colorbar's own position
#'   (\code{style$legend_pos}) -- shift that separately to move the whole
#'   inset block (text + colorbar) together.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' snapshot_meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_latest.csv")
#' data_yy       <- readRDS("data/processed/flux_data_converted_yy.rds")
#' p <- fig_whittaker_worldclim(data_yy, snapshot_meta,
#'                              detail_label = "FLUXNET Shuttle 2025")
#' }
#'
#' @export
fig_whittaker_worldclim <- function(
  data_yy,
  site_meta,
  worldclim_dir   = "data/external/worldclim/climate/wc2.1_2.5m/",
  worldclim_csv   = "data/snapshots/site_worldclim.csv",
  year_cutoff     = NULL,
  detail_label    = NULL,
  style           = WHITTAKER_STYLE,
  hex_regular     = FALSE,
  hex_bins        = 15,
  points_in_front = FALSE,
  point_size      = 1.4,
  point_colour    = "grey30",
  point_alpha     = 0.50,
  nee_mid_colour  = NULL,
  detail_lines    = NULL,
  detail_hjust    = -0.07,
  detail_x_offset = 0
) {

  for (pkg in c("hexbin", "colorspace")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for fig_whittaker_worldclim().",
           call. = FALSE)
    }
  }

  .check_cols_climate(data_yy,   c("site_id", "YEAR"))
  .check_cols_climate(site_meta, c("site_id", "location_lat", "location_long"))
  if (!any(c("NEE_VUT_REF", "NEE_CUT_REF") %in% names(data_yy))) {
    stop("data_yy must contain NEE_VUT_REF and/or NEE_CUT_REF.", call. = FALSE)
  }

  # --- year_cutoff: filter site_meta to sites established by cutoff year ------
  if (!is.null(year_cutoff)) {
    year_cutoff <- as.integer(year_cutoff)
    if ("first_year" %in% names(site_meta)) {
      site_meta <- dplyr::filter(site_meta,
                                 as.integer(.data$first_year) <= year_cutoff)
    }
  }

  # --- WorldClim climate lookup -----------------------------------------------
  wc_known <- if (file.exists(worldclim_csv)) {
    readr::read_csv(worldclim_csv, show_col_types = FALSE)
  } else {
    data.frame(site_id       = character(0),
               mat_worldclim = numeric(0),
               map_worldclim = numeric(0))
  }

  need_extract <- site_meta |>
    dplyr::filter(
      !.data$site_id %in% wc_known$site_id,
      !is.na(.data$location_lat),
      !is.na(.data$location_long)
    ) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE)

  wc_extracted <- NULL
  if (nrow(need_extract) > 0L) {
    if (requireNamespace("terra", quietly = TRUE) && dir.exists(worldclim_dir)) {
      bio_files  <- list.files(worldclim_dir, pattern = "\\.tif$",
                               full.names = TRUE)
      bio1_file  <- bio_files[grepl("_bio_1\\.tif$",  bio_files,
                                    ignore.case = TRUE)]
      bio12_file <- bio_files[grepl("_bio_12\\.tif$", bio_files,
                                    ignore.case = TRUE)]

      if (length(bio1_file) == 1L && length(bio12_file) == 1L) {
        wc_rast <- terra::rast(c(bio1_file, bio12_file))
        pts     <- terra::vect(
          data.frame(x = need_extract$location_long,
                     y = need_extract$location_lat),
          geom = c("x", "y"), crs = "EPSG:4326"
        )
        vals    <- as.data.frame(terra::extract(wc_rast, pts, ID = FALSE))
        mat_raw <- vals[[1L]]
        mat_c   <- if (max(abs(mat_raw), na.rm = TRUE) > 70) mat_raw / 10
                   else mat_raw
        wc_extracted <- need_extract |>
          dplyr::mutate(mat_worldclim = mat_c,
                        map_worldclim = vals[[2L]])
        message("fig_whittaker_worldclim: extracted WorldClim for ",
                nrow(wc_extracted), " sites not in worldclim_csv.")
      } else {
        message("fig_whittaker_worldclim: bio_1.tif or bio_12.tif not found in ",
                worldclim_dir, ". Those sites will lack climate data.")
      }
    } else {
      message("fig_whittaker_worldclim: terra unavailable or worldclim_dir ",
              "missing. ", nrow(need_extract), " sites will lack climate data.")
    }
  }

  wc_all <- dplyr::bind_rows(
    wc_known,
    if (!is.null(wc_extracted))
      dplyr::select(wc_extracted, "site_id", "mat_worldclim", "map_worldclim")
    else NULL
  )

  # --- NEE colour limits from FULL data_yy (not filtered) ---------------------
  # Coalesce VUT and CUT so CUT-only sites contribute to the scale.
  if (is.null(style$nee_lims)) {
    nee_vut_all <- if ("NEE_VUT_REF" %in% names(data_yy)) data_yy[["NEE_VUT_REF"]] else rep(NA_real_, nrow(data_yy))
    nee_cut_all <- if ("NEE_CUT_REF" %in% names(data_yy)) data_yy[["NEE_CUT_REF"]] else rep(NA_real_, nrow(data_yy))
    nee_q   <- quantile(dplyr::coalesce(nee_vut_all, nee_cut_all),
                        probs = c(0.05, 0.95), na.rm = TRUE)
    nee_max <- max(abs(nee_q))
    style$nee_lims <- c(-nee_max, nee_max)
  }

  # --- filter data_yy to sites and year_cutoff --------------------------------
  site_ids  <- unique(site_meta$site_id)
  data_filt <- dplyr::filter(data_yy, .data$site_id %in% site_ids)
  if (!is.null(year_cutoff)) {
    data_filt <- dplyr::filter(data_filt,
                               as.integer(.data$YEAR) <= year_cutoff)
  }

  # --- per-site NEE median and site-year count --------------------------------
  # Coalesce VUT and CUT: recovers ~36 CUT-only sites with no NEE_VUT_REF.
  nee_vut_f <- if ("NEE_VUT_REF" %in% names(data_filt)) data_filt[["NEE_VUT_REF"]] else rep(NA_real_, nrow(data_filt))
  nee_cut_f <- if ("NEE_CUT_REF" %in% names(data_filt)) data_filt[["NEE_CUT_REF"]] else rep(NA_real_, nrow(data_filt))
  data_filt <- dplyr::mutate(data_filt, NEE_ref = dplyr::coalesce(nee_vut_f, nee_cut_f))
  site_nee <- data_filt |>
    dplyr::filter(!is.na(.data$NEE_ref)) |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(
      median_nee  = median(.data$NEE_ref, na.rm = TRUE),
      n_nee_years = dplyr::n_distinct(.data$YEAR),
      .groups     = "drop"
    )

  n_sites      <- length(site_ids)
  n_site_years <- sum(site_nee$n_nee_years)

  # --- assemble plot data (climate + NEE) -------------------------------------
  plot_data <- site_meta |>
    dplyr::select("site_id", "location_lat", "location_long") |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::left_join(
      dplyr::select(wc_all, "site_id", "mat_worldclim", "map_worldclim"),
      by = "site_id"
    ) |>
    dplyr::left_join(
      dplyr::select(site_nee, "site_id", "median_nee"),
      by = "site_id"
    ) |>
    dplyr::filter(!is.na(.data$mat_worldclim), !is.na(.data$map_worldclim))

  if (nrow(plot_data) == 0L) {
    warning("fig_whittaker_worldclim: no sites with climate data after filtering.",
            call. = FALSE)
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No data") +
             fluxnet_theme())
  }

  # --- inset detail text (top-left) -------------------------------------------
  # detail_lines = NULL (default): unchanged auto-built "N = ... sites |
  # ... site-years" line. Supplying detail_lines overrides just that last
  # line (detail_label, if given, is still prepended) -- lets a caller report
  # different/additional counts without hardcoding wording into this shared
  # function; see e.g. scripts/generate_whittaker_alt_fig02_update.R.
  detail_str <- paste0(
    if (!is.null(detail_label)) paste0(detail_label, "\n") else "",
    if (is.null(detail_lines)) {
      paste0("N = ", n_sites, " sites | ", n_site_years, " site-years")
    } else {
      paste(detail_lines, collapse = "\n")
    }
  )

  # --- build plot -------------------------------------------------------------
  # Hexagon regularity: ggplot2::stat_summary_hex()'s default binwidth (one
  # scalar `bins` split evenly across the FULL, unclipped x and y data range)
  # only renders as a visually regular hexagon when the rendered panel's
  # incidental physical aspect ratio happens to match diff(y range)/diff(x
  # range) -- true by coincidence, not by construction. When hex_regular =
  # TRUE, binwidth is computed explicitly from style$xlim/style$ylim, and
  # coord_fixed(ratio = diff(xlim)/diff(ylim)) -- ggplot2's `ratio` is
  # expressed as physical-y-per-physical-x, i.e. the panel's height/width =
  # (diff(ylim)/diff(xlim)) * ratio, so ratio = diff(xlim)/diff(ylim) is what
  # makes height/width == 1 (a square panel) -- pins the physical length of
  # one MAT unit relative to one MAP unit, so hex width:height is regular by
  # construction regardless of legend/axis-label margins or requested figure
  # size (with hex_bins equal in both directions, this also always yields a
  # square panel, since the same ratio value satisfies both conditions).
  hex_binwidth <- if (isTRUE(hex_regular)) {
    c(diff(range(style$xlim)) / hex_bins, diff(range(style$ylim)) / hex_bins)
  } else {
    NULL   # ggplot2 default: hex_binwidth(bins, scales) from the full data range
  }
  hex_ratio <- diff(range(style$xlim)) / diff(range(style$ylim))

  point_layer <- ggplot2::geom_point(
    ggplot2::aes(x = .data$mat_worldclim, y = .data$map_worldclim),
    size        = point_size,
    colour      = point_colour,
    alpha       = point_alpha,
    inherit.aes = FALSE
  )
  hex_layer <- ggplot2::stat_summary_hex(
    fun      = function(x) if (all(is.na(x))) NA_real_
                           else median(x, na.rm = TRUE),
    bins     = hex_bins,
    binwidth = hex_binwidth,
    alpha    = 0.85
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$mat_worldclim, y = .data$map_worldclim,
                 z = .data$median_nee)
  )
  # Layer order controls what's drawn on top. Default (points_in_front =
  # FALSE): points first/behind, hexagons second/in front — site-level
  # MAT/MAP values are visible only where they poke out from under the
  # density summary. points_in_front = TRUE reverses this.
  p <- if (isTRUE(points_in_front)) {
    p + hex_layer + point_layer
  } else {
    p + point_layer + hex_layer
  }

  # nee_mid_colour = NULL (default): unchanged stock colorspace diverging
  # scale (near-white centre). Non-NULL: rebuilt as scale_fill_gradient2()
  # with low/high pinned to diverging_hcl(2, "Blue-Red 3")'s exact endpoint
  # colours -- identical blue/red ends, identical midpoint = 0, identical
  # limits/oob -- and mid recoloured. See @param nee_mid_colour above for why
  # a same-endpoints custom HCL two-half reconstruction (matching
  # diverging_hcl's own interpolation exactly) was tried and rejected.
  nee_fill_scale <- if (is.null(nee_mid_colour)) {
    colorspace::scale_fill_continuous_diverging(
      palette  = "Blue-Red 3",
      mid      = 0,
      limits   = style$nee_lims,
      oob      = scales::squish,
      na.value = NA
      # guide set separately below via guides() so expression() title works
    )
  } else {
    ggplot2::scale_fill_gradient2(
      low      = "#002F70",   # diverging_hcl(2, "Blue-Red 3")[1] -- unchanged
      high     = "#5F1415",   # diverging_hcl(2, "Blue-Red 3")[2] -- unchanged
      mid      = nee_mid_colour,
      midpoint = 0,
      limits   = style$nee_lims,
      oob      = scales::squish,
      na.value = NA
    )
  }

  p <- p +
    nee_fill_scale +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title          = expression("NEE (g C m"^{-2}*" yr"^{-1}*")"),
        title.position = "top",
        barwidth       = style$colorbar_width,
        barheight      = style$colorbar_height,
        direction      = "horizontal"
      )
    ) +
    ggplot2::annotate(
      "text",
      # detail_x_offset == 0 (default): x = -Inf, unchanged from prior
      # behaviour (panel's exact left edge). Non-zero: a finite x in data
      # units, shifting the block right to clear the axis with a margin.
      x          = if (detail_x_offset == 0) -Inf else style$xlim[1] + detail_x_offset,
      y = Inf,
      label      = detail_str,
      hjust      = detail_hjust, vjust = 1.3,
      size       = style$detail_text_size,
      # style$detail_lineheight absent -> 1.2, ggplot2::GeomText's own
      # default lineheight aes, i.e. identical to the prior unset behaviour.
      lineheight = if (!is.null(style$detail_lineheight)) style$detail_lineheight else 1.2
    ) +
    (if (isTRUE(hex_regular)) {
      ggplot2::coord_fixed(ratio = hex_ratio, xlim = style$xlim, ylim = style$ylim)
    } else {
      ggplot2::coord_cartesian(xlim = style$xlim, ylim = style$ylim)
    }) +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::labs(
      x = expression("Mean Annual Temperature (" * degree * "C)"),
      y = expression(atop("Mean Annual Precipitation", "(mm yr"^{-1}*")"))
    ) +
    .whittaker_theme(style)

  p
}


# ---- Global (WorldClim + CCI) ice-free land background figures --------------
# Added for talk-slide backgrounds that sit under the network-distribution
# Whittaker panels (fig_whit01_ShuttleFull.png / fig_02_whittaker_current.png).
# These represent global ice-free land area, not FLUXNET sites, and carry no
# NEE information -- fig_whittaker_worldclim() above is untouched.

#' Build the global ice-free-land MAT/MAP pixel table
#'
#' Reads the WorldClim v2.1 2.5 arc-minute BIO1 (MAT) and BIO12 (MAP) rasters,
#' builds an ice-free land mask from the ESA CCI land-cover product (excluding
#' water bodies, permanent snow/ice, and a latitude backstop for Antarctica),
#' and returns one row per WorldClim pixel that survives the mask, with an
#' area weight proportional to \code{cos(latitude)} (WorldClim is an
#' equal-angle grid, so raw pixel counts over-weight high latitudes).
#'
#' @param bio1_path Character. Path to the WorldClim BIO1 (MAT, degC) GeoTIFF.
#' @param bio12_path Character. Path to the WorldClim BIO12 (MAP, mm/yr) GeoTIFF.
#' @param landcover_path Character. Path to the ESA CCI land-cover GeoTIFF
#'   used to build the ice/water mask. Must use CCI class codes (0 = no data,
#'   210 = water bodies, 220 = permanent snow/ice; all other codes = land).
#' @param antarctica_lat_cutoff Numeric. Latitude (deg) south of which pixels
#'   are excluded regardless of land-cover class, as a backstop for any
#'   coastline/no-data gaps in the CCI product over Antarctica.
#' @param land_frac_threshold Numeric in (0, 1). Minimum fraction of
#'   ice-free-land sub-pixels (from the aggregated CCI mask) required to keep
#'   a WorldClim pixel.
#'
#' @return A data frame with columns \code{mat} (degC), \code{map} (mm/yr),
#'   \code{lat} (deg), and \code{weight} (\code{cos(lat * pi / 180)}).
#'
#' @export
build_global_landclimate <- function(
  bio1_path      = "data/external/worldclim/climate/wc2.1_2.5m/wc2.1_2.5m_bio_1.tif",
  bio12_path     = "data/external/worldclim/climate/wc2.1_2.5m/wc2.1_2.5m_bio_12.tif",
  landcover_path = "data/external/cci_landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif",
  antarctica_lat_cutoff = -60,
  land_frac_threshold    = 0.5
) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required for build_global_landclimate().", call. = FALSE)
  }
  if (!file.exists(bio1_path))  stop("WorldClim BIO1 raster not found: ",  bio1_path,  call. = FALSE)
  if (!file.exists(bio12_path)) stop("WorldClim BIO12 raster not found: ", bio12_path, call. = FALSE)
  if (!file.exists(landcover_path)) {
    stop("Land-cover raster not found: ", landcover_path, call. = FALSE)
  }

  r_bio1  <- terra::rast(bio1_path)
  r_bio12 <- terra::rast(bio12_path)
  r_cci   <- terra::rast(landcover_path)

  # Ice-free land indicator at CCI native resolution: NA = no data,
  # 0 = water/ice (excluded), 1 = ice-free land.
  r_land01 <- terra::classify(
    r_cci,
    rcl    = matrix(c(0, NA, 210, 0, 220, 0), ncol = 2, byrow = TRUE),
    others = 1
  )

  agg_fact <- round(terra::res(r_bio1)[1] / terra::res(r_cci)[1])
  r_landfrac <- terra::aggregate(r_land01, fact = agg_fact, fun = "mean", na.rm = TRUE)
  if (!terra::compareGeom(r_landfrac, r_bio1, stopOnError = FALSE)) {
    stop("Aggregated land-cover grid does not align with the WorldClim grid ",
         "(expected an exact integer aggregation factor).", call. = FALSE)
  }

  r_mask <- r_landfrac >= land_frac_threshold
  r_lat  <- terra::init(r_bio1, "y")
  r_mask[r_lat < antarctica_lat_cutoff] <- FALSE

  stack <- c(r_bio1, r_bio12, r_mask)
  names(stack) <- c("mat", "map", "is_land")

  df <- as.data.frame(stack, xy = TRUE, na.rm = FALSE)
  df <- df[!is.na(df$is_land) & df$is_land == 1 & !is.na(df$mat) & !is.na(df$map), ]

  data.frame(
    mat    = df$mat,
    map    = df$map,
    lat    = df$y,
    weight = cos(df$y * pi / 180)
  )
}

#' Global ice-free land — Whittaker frequency hexbin (WorldClim + CCI)
#'
#' Bins global ice-free land pixels into the same MAT/MAP hexagonal grid used
#' by \code{\link{fig_whittaker_worldclim}}, shaded by area-weighted pixel
#' frequency on a log10 scale. Intended as a neutral background layer (no NEE
#' information) for overlaying the network distribution on talk slides.
#'
#' Pixel data are clipped to \code{style$xlim}/\code{style$ylim} before
#' hexbinning so the hex-cell geometry matches
#' \code{\link{fig_whittaker_worldclim}} exactly (identical bin domain and
#' \code{bins = 15}), which is required for the two figures to register when
#' layered.
#'
#' @param land_climate Data frame from \code{\link{build_global_landclimate}}
#'   (columns \code{mat}, \code{map}, \code{weight}).
#' @param style Named list of visual parameters, defaults to
#'   \code{\link{WHITTAKER_STYLE}}.
#' @param source_label Character. Data-source line shown in the inset text.
#'
#' @return A list with elements \code{plot} (the ggplot object) and
#'   \code{frac_kept} (fraction of total global land-area weight that falls
#'   within \code{style$xlim}/\code{style$ylim}, i.e. was not clipped).
#'
#' @export
fig_whittaker_global_frequency <- function(
  land_climate,
  style        = WHITTAKER_STYLE,
  source_label = "WorldClim v2.1 x ESA CCI LC 2015"
) {
  for (pkg in c("hexbin", "colorspace")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for fig_whittaker_global_frequency().",
           call. = FALSE)
    }
  }
  .check_cols_climate(land_climate, c("mat", "map", "weight"))

  clipped <- dplyr::filter(
    land_climate,
    .data$mat >= style$xlim[1], .data$mat <= style$xlim[2],
    .data$map >= style$ylim[1], .data$map <= style$ylim[2]
  )
  frac_kept <- sum(clipped$weight) / sum(land_climate$weight)

  detail_str <- paste0(
    source_label, "\n",
    "all ice-free land, area-weighted\n",
    sprintf("%.1f%% of global land area shown", 100 * frac_kept)
  )

  p <- ggplot2::ggplot(
    clipped,
    ggplot2::aes(x = .data$mat, y = .data$map, z = .data$weight)
  ) +
    ggplot2::stat_summary_hex(
      fun   = function(w) if (all(is.na(w))) NA_real_ else sum(w, na.rm = TRUE),
      bins  = 15,
      alpha = 0.9
    ) +
    colorspace::scale_fill_continuous_sequential(
      palette  = "Blues 3",
      trans    = "log10",
      na.value = NA
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title          = "Area-weighted land\nfrequency (log10)",
        title.position = "top",
        barwidth       = style$colorbar_width,
        barheight      = style$colorbar_height,
        direction      = "horizontal"
      )
    ) +
    ggplot2::annotate(
      "text",
      x     = -Inf, y = Inf,
      label = detail_str,
      hjust = -0.05, vjust = 1.3,
      size  = style$detail_text_size
    ) +
    ggplot2::coord_cartesian(
      xlim = style$xlim,
      ylim = style$ylim
    ) +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::labs(
      x = expression("Mean Annual Temperature (" * degree * "C)"),
      y = expression(atop("Mean Annual Precipitation", "(mm yr"^{-1}*")"))
    ) +
    .whittaker_theme(style)

  list(plot = p, frac_kept = frac_kept)
}

#' Global ice-free land — Whittaker density-contour envelope (WorldClim + CCI)
#'
#' Draws highest-density-region contour lines enclosing \code{probs} (default
#' 95% and 99%) of global ice-free land area in MAT/MAP climate space, on a
#' plain white background with no fill. Intended as a clean envelope layer
#' under network points on talk slides.
#'
#' The kernel density estimate is computed from the \emph{full, unclipped}
#' global pixel distribution (so the 95%/99% figures are honest to "global
#' ice-free land area" rather than to whatever falls inside the display
#' window), using area (cosine-of-latitude) weights. The estimate uses
#' weighted linear binning onto a regular grid followed by separable Gaussian
#' smoothing (bandwidth from a weighted Scott's-rule reference, effective
#' sample size accounting for the pixel weights); this avoids adding a new
#' package dependency (no \code{ks}/\code{spatstat}). The same
#' \code{style$xlim}/\code{style$ylim} axis window as
#' \code{\link{fig_whittaker_worldclim}} is then applied for display via
#' \code{coord_cartesian()}; any part of a contour outside that window is
#' clipped from view (not recomputed).
#'
#' @param land_climate Data frame from \code{\link{build_global_landclimate}}
#'   (columns \code{mat}, \code{map}, \code{weight}). Ignored (may be
#'   \code{NULL}) when \code{density_grid} is supplied directly.
#' @param style Named list of visual parameters, defaults to
#'   \code{\link{WHITTAKER_STYLE}}.
#' @param probs Numeric vector of coverage probabilities for the contour
#'   lines (default \code{c(0.95, 0.99)}).
#' @param gridsize Integer length-2 vector, KDE evaluation grid size. Ignored
#'   when \code{density_grid} is supplied.
#' @param density_grid Optional pre-computed density grid (a list with
#'   \code{xbin}, \code{ybin}, \code{density}, as returned by
#'   \code{\link{build_global_landclimate}} + \code{.weighted_density_grid()}).
#'   Pass this to reuse an already-computed density surface (e.g. shared with
#'   \code{\link{fig_whittaker_global_density}} and coverage-statistic code)
#'   instead of recomputing it here. When \code{NULL} (default), the density
#'   grid is computed from \code{land_climate}, preserving prior behaviour.
#'
#' @return A list with elements \code{plot} (the ggplot object) and
#'   \code{contour_df} (the underlying contour-line coordinates).
#'
#' @export
fig_whittaker_global_contour <- function(
  land_climate = NULL,
  style        = WHITTAKER_STYLE,
  probs        = c(0.95, 0.99),
  gridsize     = c(201, 201),
  density_grid = NULL
) {
  if (is.null(density_grid)) {
    .check_cols_climate(land_climate, c("mat", "map", "weight"))
    density_grid <- .weighted_density_grid(land_climate$mat, land_climate$map,
                                           land_climate$weight, gridsize = gridsize)
  }
  dg     <- density_grid
  levels <- .hdr_levels(dg$density, probs = probs)

  contour_df <- do.call(rbind, lapply(seq_along(probs), function(i) {
    cl <- grDevices::contourLines(x = dg$xbin, y = dg$ybin, z = dg$density,
                                  levels = levels[i])
    if (length(cl) == 0L) return(NULL)
    do.call(rbind, lapply(seq_along(cl), function(j) {
      data.frame(x = cl[[j]]$x, y = cl[[j]]$y, piece = j, prob = probs[i])
    }))
  }))
  if (is.null(contour_df) || nrow(contour_df) == 0L) {
    stop("fig_whittaker_global_contour: no contour lines found at levels ",
         paste(round(levels, 6), collapse = ", "), call. = FALSE)
  }
  contour_df$prob_label <- factor(paste0(contour_df$prob * 100, "%"),
                                  levels = paste0(sort(probs, decreasing = TRUE) * 100, "%"))
  # Smallest prob (innermost, most-certain core) drawn solid; larger probs
  # (outer, wider envelope) drawn with progressively broken lines.
  probs_asc <- sort(unique(probs))
  linetypes <- stats::setNames(
    rep(c("solid", "dashed", "dotted"), length.out = length(probs_asc)),
    paste0(probs_asc * 100, "%")
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = contour_df,
      ggplot2::aes(x = .data$x, y = .data$y,
                  group = interaction(.data$prob, .data$piece),
                  linetype = .data$prob_label),
      colour    = "grey20",
      linewidth = 0.6
    ) +
    ggplot2::scale_linetype_manual(
      values = linetypes,
      name   = "Global ice-free\nland area"
    ) +
    ggplot2::coord_cartesian(
      xlim = style$xlim,
      ylim = style$ylim
    ) +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::labs(
      x = expression("Mean Annual Temperature (" * degree * "C)"),
      y = expression(atop("Mean Annual Precipitation", "(mm yr"^{-1}*")"))
    ) +
    .whittaker_theme(style) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position   = "inside",
      legend.position.inside = style$legend_pos,
      legend.justification   = style$legend_just
    )

  list(plot = p, contour_df = contour_df)
}

#' Global ice-free land — Whittaker continuous density shading (WorldClim + CCI)
#'
#' Draws the same weighted 2D kernel density surface used by
#' \code{\link{fig_whittaker_global_contour}} as continuous shading (a
#' smoothly interpolated raster) rather than discrete hexagons, on a neutral
#' sequential colour scale. Supersedes the discrete hexbin produced by
#' \code{\link{fig_whittaker_global_frequency}} for talk-slide backgrounds,
#' since a continuous field reads more cleanly under overlaid points/contours.
#'
#' Unlike \code{\link{fig_whittaker_global_frequency}}, no data pre-clipping
#' is needed before plotting: \code{density_grid} is already a fixed regular
#' grid, and \code{coord_cartesian()} windows the display to
#' \code{style$xlim}/\code{style$ylim} exactly as the other Whittaker figures
#' do.
#'
#' @param density_grid A density grid as returned by
#'   \code{.weighted_density_grid()} (list with \code{xbin}, \code{ybin},
#'   \code{density}) -- typically the same object passed to
#'   \code{\link{fig_whittaker_global_contour}}, so the shading and the
#'   contour lines are guaranteed consistent.
#' @param style Named list of visual parameters, defaults to
#'   \code{\link{WHITTAKER_STYLE}}.
#' @param source_label Character. Data-source line shown in the inset text.
#'
#' @return A list with elements \code{plot} (the ggplot object) and
#'   \code{frac_kept} (fraction of total grid mass that falls within
#'   \code{style$xlim}/\code{style$ylim}, i.e. is visible in the displayed
#'   window).
#'
#' @export
fig_whittaker_global_density <- function(
  density_grid,
  style        = WHITTAKER_STYLE,
  source_label = "WorldClim v2.1 x ESA CCI LC 2015"
) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required for fig_whittaker_global_density().",
         call. = FALSE)
  }

  grid_df <- expand.grid(mat = density_grid$xbin, map = density_grid$ybin)
  grid_df$density <- as.vector(density_grid$density)

  in_window <- grid_df$mat >= style$xlim[1] & grid_df$mat <= style$xlim[2] &
               grid_df$map >= style$ylim[1] & grid_df$map <= style$ylim[2]
  frac_kept <- sum(grid_df$density[in_window]) / sum(grid_df$density)

  # Plot only the grid cells inside the display window: the KDE's padded
  # domain extends far beyond style$xlim/ylim (down to near-zero density in
  # the tails), and colour-scale limits computed over that full padded grid
  # would compress every visible cell into a sliver of the scale. Restricting
  # both the plotted data AND the colour domain to what is actually shown
  # keeps the shading legible and matches the visible window exactly.
  grid_df <- grid_df[in_window, ]

  # Even within the visible window, a Gaussian-kernel density estimate over a
  # skewed, heavy-tailed global distribution can span dozens of orders of
  # magnitude between its peak and its faintest visible tail (real feature of
  # the KDE, not a bug) -- e.g. a rare cold+wet MAT/MAP corner sits many
  # bandwidths from the bulk of the data. A log10 scale spanning that full
  # range makes for an uninformative colourbar (e.g. "1e-56, 1e-40, ...").
  # Floor the colour domain 6 orders of magnitude below the visible peak and
  # squish anything below that into the lightest shade: differences beyond
  # that point are visually and scientifically indistinguishable from zero.
  dens_max  <- max(grid_df$density)
  dens_floor <- dens_max * 1e-6
  fill_limits <- c(dens_floor, dens_max)

  detail_str <- paste0(
    source_label, "\n",
    "all ice-free land, area-weighted\n",
    sprintf("%.1f%% of global land area shown", 100 * frac_kept)
  )

  p <- ggplot2::ggplot(
    grid_df,
    ggplot2::aes(x = .data$mat, y = .data$map, fill = .data$density)
  ) +
    ggplot2::geom_raster(interpolate = TRUE) +
    colorspace::scale_fill_continuous_sequential(
      palette  = "Blues 3",
      trans    = "log10",
      limits   = fill_limits,
      oob      = scales::squish,
      na.value = NA
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title          = "Area-weighted land\ndensity (log10)",
        title.position = "top",
        barwidth       = style$colorbar_width,
        barheight      = style$colorbar_height,
        direction      = "horizontal"
      )
    ) +
    ggplot2::annotate(
      "text",
      x     = -Inf, y = Inf,
      label = detail_str,
      hjust = -0.05, vjust = 1.3,
      size  = style$detail_text_size
    ) +
    ggplot2::coord_cartesian(
      xlim = style$xlim,
      ylim = style$ylim
    ) +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::labs(
      x = expression("Mean Annual Temperature (" * degree * "C)"),
      y = expression(atop("Mean Annual Precipitation", "(mm yr"^{-1}*")"))
    ) +
    .whittaker_theme(style)

  list(plot = p, frac_kept = frac_kept)
}

#' FLUXNET network coverage of global ice-free-land HDR climate-space envelopes
#'
#' Computes how much of the global ice-free-land climate-space footprint the
#' FLUXNET network reaches, relative to the same highest-density-region (HDR)
#' envelopes drawn by \code{\link{fig_whittaker_global_contour}}. For each
#' target probability in \code{probs}, a site's MAT/MAP position is snapped
#' to the nearest node of \code{density_grid} (the same discretisation the
#' HDR envelope is defined on), and four numbers are returned:
#' \enumerate{
#'   \item \code{cell_coverage} — of the grid cells inside the HDR region
#'     (\code{density >= level}), the fraction that contain at least one site.
#'   \item \code{area_weighted_coverage} — of the global ice-free land-area
#'     weight inside the HDR region, the fraction that falls in
#'     site-containing cells.
#' }
#' both computed once per \code{probs} entry (so, with the default
#' \code{probs = c(0.95, 0.99)}, this yields the four numbers a-d described
#' in the coverage-statistics task).
#'
#' @param density_grid A density grid as returned by
#'   \code{.weighted_density_grid()} (list with \code{xbin}, \code{ybin},
#'   \code{density}) — the same object used for the contour/shading figures,
#'   so the coverage statistics are defined against the identical envelopes.
#' @param site_mat,site_map Numeric vectors of FLUXNET site MAT (degC) and
#'   MAP (mm/yr) positions, one row per site (should come from the same
#'   WorldClim source as \code{density_grid} so coordinates are directly
#'   comparable).
#' @param probs Numeric vector of target HDR coverage probabilities (default
#'   \code{c(0.95, 0.99)}).
#'
#' @return A data frame with one row per \code{probs} entry and columns
#'   \code{prob}, \code{hdr_level}, \code{n_cells_region}, \code{n_sites},
#'   \code{cell_coverage}, \code{area_weighted_coverage}.
#'
#' @export
whittaker_hdr_coverage <- function(density_grid, site_mat, site_map, probs = c(0.95, 0.99)) {
  nx <- length(density_grid$xbin)
  ny <- length(density_grid$ybin)
  dx <- density_grid$xbin[2] - density_grid$xbin[1]
  dy <- density_grid$ybin[2] - density_grid$ybin[1]

  keep <- !is.na(site_mat) & !is.na(site_map)
  site_mat <- site_mat[keep]
  site_map <- site_map[keep]

  ix <- pmin(nx, pmax(1L, round((site_mat - density_grid$xbin[1]) / dx) + 1L))
  iy <- pmin(ny, pmax(1L, round((site_map - density_grid$ybin[1]) / dy) + 1L))
  site_cell <- (iy - 1L) * nx + ix

  has_site <- logical(nx * ny)
  has_site[unique(site_cell)] <- TRUE

  dens_vec <- as.vector(density_grid$density)
  levels   <- .hdr_levels(density_grid$density, probs = probs)

  do.call(rbind, lapply(seq_along(probs), function(i) {
    in_region <- dens_vec >= levels[i]
    data.frame(
      prob                   = probs[i],
      hdr_level              = levels[i],
      n_cells_region         = sum(in_region),
      n_sites                = length(unique(site_cell)),
      cell_coverage          = sum(has_site[in_region]) / sum(in_region),
      area_weighted_coverage = sum(dens_vec[in_region][has_site[in_region]]) / sum(dens_vec[in_region])
    )
  }))
}

#' FLUXNET network coverage of global ice-free land, Mahalanobis-distance definition
#'
#' An area-weighted coverage metric that replaces the cell-count coverage in
#' \code{\link{whittaker_hdr_coverage}} with a grid-independent one: a land
#' pixel counts as "sampled" if its exact (MAT, MAP) position lies within
#' Mahalanobis distance \code{d} of at least one FLUXNET site's exact (MAT,
#' MAP) position -- sites are never snapped to a grid cell, only land pixels
#' (already gridded at native WorldClim resolution) are aggregated.
#'
#' The Mahalanobis metric uses the covariance of the \strong{area-weighted
#' global ice-free-land climate distribution} (\code{land_climate}), not the
#' site/tower distribution: \code{d(x, s) = sqrt((x - s)' Sigma^-1 (x - s))},
#' where \code{Sigma} is the weighted covariance of \code{(mat, map)} across
#' all land pixels. Distances are computed once, for every land pixel, as the
#' minimum over all sites (a loop over the small site list with a vectorised
#' distance computation over the large pixel vector -- paying the full
#' pixel-by-site cost exactly once), then reused to tabulate coverage at
#' every threshold and region cheaply.
#'
#' Optional region restriction (e.g. the 95%/99% highest-density-region
#' envelopes) reuses \code{\link{.hdr_levels}} on a supplied
#' \code{density_grid}, with land pixels (not sites) snapped to their nearest
#' grid node purely to classify region membership.
#'
#' @param land_climate Data frame from \code{\link{build_global_landclimate}}
#'   (columns \code{mat}, \code{map}, \code{weight}).
#' @param site_mat,site_map Numeric vectors of FLUXNET site MAT (degC) and
#'   MAP (mm/yr) exact positions (not snapped to any grid).
#' @param thresholds Numeric vector of Mahalanobis-distance thresholds
#'   (default \code{c(0.25, 0.5, 1.0)}).
#' @param density_grid Optional density grid (as from
#'   \code{.weighted_density_grid()}) used to additionally restrict coverage
#'   to the highest-density-region envelopes named in \code{hdr_probs}. When
#'   \code{NULL}, only the unrestricted ("all") region is reported.
#' @param hdr_probs Numeric vector of HDR coverage probabilities (default
#'   \code{c(0.95, 0.99)}), used only when \code{density_grid} is supplied.
#'
#' @return A list with elements \code{table} (a data frame with columns
#'   \code{region}, \code{threshold}, \code{area_weighted_coverage}) and
#'   \code{covariance} (the 2x2 weighted covariance matrix of the land
#'   climate distribution used to define the metric, with row/col names
#'   \code{mat}/\code{map}) and \code{mean} (the weighted mean MAT/MAP used
#'   to centre the distance calculation).
#'
#' @export
whittaker_mahalanobis_coverage <- function(
  land_climate,
  site_mat,
  site_map,
  thresholds   = c(0.25, 0.5, 1.0),
  density_grid = NULL,
  hdr_probs    = c(0.95, 0.99)
) {
  .check_cols_climate(land_climate, c("mat", "map", "weight"))
  keep     <- !is.na(site_mat) & !is.na(site_map)
  site_mat <- site_mat[keep]
  site_map <- site_map[keep]

  mat <- land_climate$mat
  map <- land_climate$map
  w   <- land_climate$weight
  wsum <- sum(w)

  # --- area-weighted covariance of the global land-climate distribution ------
  mx <- sum(w * mat) / wsum
  my <- sum(w * map) / wsum
  sxx <- sum(w * (mat - mx)^2) / wsum
  syy <- sum(w * (map - my)^2) / wsum
  sxy <- sum(w * (mat - mx) * (map - my)) / wsum
  Sigma <- matrix(c(sxx, sxy, sxy, syy), nrow = 2,
                  dimnames = list(c("mat", "map"), c("mat", "map")))

  # --- whitening transform: Sigma^-1 = V diag(e) V', so ------------------------
  # --- (x-s)' Sigma^-1 (x-s) = || diag(sqrt(e)) V' (x-s) ||^2 -----------------
  eig  <- eigen(solve(Sigma), symmetric = TRUE)
  Tmat <- diag(sqrt(eig$values), nrow = 2) %*% t(eig$vectors)

  px <- Tmat[1, 1] * (mat - mx) + Tmat[1, 2] * (map - my)
  py <- Tmat[2, 1] * (mat - mx) + Tmat[2, 2] * (map - my)
  sx <- Tmat[1, 1] * (site_mat - mx) + Tmat[1, 2] * (site_map - my)
  sy <- Tmat[2, 1] * (site_mat - mx) + Tmat[2, 2] * (site_map - my)

  # --- per-pixel minimum Mahalanobis distance to any site (computed once) ----
  min_dist <- rep(Inf, length(px))
  for (j in seq_along(sx)) {
    min_dist <- pmin(min_dist, sqrt((px - sx[j])^2 + (py - sy[j])^2))
  }

  region_defs <- list(all = rep(TRUE, length(mat)))
  if (!is.null(density_grid)) {
    nx <- length(density_grid$xbin)
    ny <- length(density_grid$ybin)
    dx <- density_grid$xbin[2] - density_grid$xbin[1]
    dy <- density_grid$ybin[2] - density_grid$ybin[1]
    ix <- pmin(nx, pmax(1L, round((mat - density_grid$xbin[1]) / dx) + 1L))
    iy <- pmin(ny, pmax(1L, round((map - density_grid$ybin[1]) / dy) + 1L))
    pix_density <- density_grid$density[cbind(ix, iy)]
    levels <- .hdr_levels(density_grid$density, probs = hdr_probs)
    for (i in seq_along(hdr_probs)) {
      region_defs[[paste0("hdr_", hdr_probs[i] * 100)]] <- pix_density >= levels[i]
    }
  }

  tbl <- do.call(rbind, lapply(names(region_defs), function(region_name) {
    in_region     <- region_defs[[region_name]]
    region_w      <- w[in_region]
    region_dist   <- min_dist[in_region]
    region_wsum   <- sum(region_w)
    do.call(rbind, lapply(thresholds, function(d) {
      data.frame(
        region                 = region_name,
        threshold              = d,
        area_weighted_coverage = sum(region_w[region_dist <= d]) / region_wsum
      )
    }))
  }))

  list(table = tbl, covariance = Sigma, mean = c(mat = mx, map = my))
}


# ---- Internal helpers -------------------------------------------------------

#' Weighted 2D kernel density estimate via linear binning + Gaussian smoothing
#'
#' A dependency-free (no \code{ks}/\code{spatstat}) weighted 2D KDE: each
#' point's weight is distributed onto the four nearest nodes of a regular
#' grid (linear binning, mass-preserving), then the grid is smoothed with a
#' separable Gaussian kernel implemented as two row-stochastic matrix
#' multiplications and finally renormalised so the discretised grid mass
#' exactly equals \code{sum(w)}. Bandwidth follows a weighted Scott's-rule
#' reference (\code{sigma * n_eff^(-1/6)}), where \code{n_eff} is the
#' effective sample size accounting for unequal weights.
#'
#' @param x,y Numeric vectors of equal length.
#' @param w Numeric vector of positive weights, same length as \code{x}/\code{y}.
#' @param gridsize Integer length-2 vector, number of grid nodes in x and y.
#' @param pad Numeric. Fractional padding added beyond \code{range(x)}/\code{range(y)}
#'   so the smoothed density is not truncated at the data extent.
#'
#' @return A list with \code{xbin}, \code{ybin} (grid node coordinates) and
#'   \code{density} (an \code{nx} by \code{ny} matrix of grid mass, summing
#'   to \code{sum(w)}).
#' @noRd
.weighted_density_grid <- function(x, y, w, gridsize = c(201, 201), pad = 0.02) {
  nx <- gridsize[1]; ny <- gridsize[2]

  rx <- range(x); ry <- range(y)
  padx <- diff(rx) * pad; pady <- diff(ry) * pad
  xbin <- seq(rx[1] - padx, rx[2] + padx, length.out = nx)
  ybin <- seq(ry[1] - pady, ry[2] + pady, length.out = ny)

  wsum <- sum(w)
  mx   <- sum(w * x) / wsum
  my   <- sum(w * y) / wsum
  sx   <- sqrt(sum(w * (x - mx)^2) / wsum)
  sy   <- sqrt(sum(w * (y - my)^2) / wsum)
  neff <- wsum^2 / sum(w^2)
  bw   <- c(sx, sy) * neff^(-1/6)

  # --- linear binning: distribute each point's weight over its 4 neighbours ---
  ix <- findInterval(x, xbin, all.inside = TRUE)
  iy <- findInterval(y, ybin, all.inside = TRUE)
  fx <- (x - xbin[ix]) / (xbin[ix + 1L] - xbin[ix])
  fy <- (y - ybin[iy]) / (ybin[iy + 1L] - ybin[iy])

  acc <- numeric(nx * ny)
  add_corner <- function(lin, wt) {
    s <- rowsum(wt, lin, reorder = FALSE)
    idx <- as.integer(rownames(s))
    acc[idx] <<- acc[idx] + s[, 1]
  }
  add_corner((iy - 1L) * nx + ix,        w * (1 - fx) * (1 - fy))
  add_corner((iy - 1L) * nx + (ix + 1L), w * fx       * (1 - fy))
  add_corner(iy * nx + ix,               w * (1 - fx) * fy)
  add_corner(iy * nx + (ix + 1L),        w * fx       * fy)
  grid <- matrix(acc, nrow = nx, ncol = ny)

  # --- separable Gaussian smoothing (row-stochastic kernel matrices) ---------
  gauss_mat <- function(centers, bandwidth) {
    k <- stats::dnorm(outer(centers, centers, "-"), sd = bandwidth)
    k / rowSums(k)
  }
  gx   <- gauss_mat(xbin, bw[1])
  gy   <- gauss_mat(ybin, bw[2])
  dens <- gx %*% grid %*% t(gy)
  dens <- dens * (sum(grid) / sum(dens))  # exact mass preservation

  list(xbin = xbin, ybin = ybin, density = dens)
}

#' Highest-density-region thresholds from a density grid
#'
#' Sorts grid cell masses in descending order and finds, for each target
#' coverage probability, the density value such that all cells at or above it
#' contain at least that fraction of the total grid mass. Passing the
#' returned levels to \code{\link[grDevices]{contourLines}} draws the
#' corresponding highest-density-region contour.
#'
#' @param density_grid Numeric matrix of grid cell masses (need not be
#'   normalised).
#' @param probs Numeric vector of target coverage probabilities in (0, 1).
#'
#' @return Numeric vector of density thresholds, same length as \code{probs}.
#' @noRd
.hdr_levels <- function(density_grid, probs) {
  v   <- sort(as.vector(density_grid), decreasing = TRUE)
  cum <- cumsum(v) / sum(v)
  vapply(probs, function(p) v[which(cum >= p)[1]], numeric(1))
}

#' Build the ggplot2 theme for Whittaker figures
#'
#' Uses \code{element_text()} (NOT \code{element_markdown()}) for axis titles so
#' that \code{expression()} plotmath labels render correctly.  \code{fluxnet_theme()}
#' sets \code{element_markdown()} globally, which coerces expressions to raw
#' strings; this function bypasses that by building from \code{theme_classic()}.
#'
#' @param style Named list — the \code{WHITTAKER_STYLE} passed to the parent call.
#' @return A ggplot2 theme object.
#' @noRd
.whittaker_theme <- function(style) {
  th <- ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(
      panel.border           = ggplot2::element_rect(color = "black", fill = NA,
                                                     linewidth = 0.8),
      panel.background       = ggplot2::element_blank(),
      axis.text              = ggplot2::element_text(color = "black",
                                                      size = style$axis_text_size),
      axis.ticks             = ggplot2::element_line(color = "black"),
      axis.ticks.length      = grid::unit(-4, "pt"),
      axis.ticks.length.x    = grid::unit(-4, "pt"),
      axis.ticks.length.y    = grid::unit(-4, "pt"),
      # element_text (NOT element_markdown) — required for plotmath expressions
      axis.title             = ggplot2::element_text(size = style$axis_title_size),
      legend.text            = ggplot2::element_text(size = style$legend_text_size),
      legend.title           = ggplot2::element_text(size = style$legend_title_size),
      legend.position        = "inside",
      legend.position.inside = style$legend_pos,
      legend.justification   = style$legend_just,
      legend.background      = ggplot2::element_rect(fill = "white", color = NA)
    )
  # style$legend_margin / style$legend_title_margin / style$legend_box_margin:
  # absent in WHITTAKER_STYLE and every existing style override, so this is a
  # no-op (ggplot2 default margins apply) unless a caller adds these fields
  # explicitly -- e.g. to tighten the padding around an inside-positioned
  # legend/colorbar. See scripts/generate_whittaker_alt_fig02_update.R.
  if (!is.null(style$legend_margin)) {
    th <- th + ggplot2::theme(legend.margin = style$legend_margin)
  }
  if (!is.null(style$legend_title_margin)) {
    th <- th + ggplot2::theme(
      legend.title = ggplot2::element_text(size = style$legend_title_size,
                                            margin = style$legend_title_margin)
    )
  }
  if (!is.null(style$legend_box_margin)) {
    th <- th + ggplot2::theme(legend.box.margin = style$legend_box_margin)
  }
  th
}

#' Check that required columns exist in a data frame
#'
#' @param data A data frame.
#' @param cols Character vector of required column names.
#' @param data_name Character. Label used in the error message.
#' @return Invisibly TRUE; stops with an informative message if any col missing.
#' @noRd
.check_cols_climate <- function(data, cols, data_name = "data") {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    stop(
      data_name, " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Map flux variable name to axis label
#'
#' @param flux_var Character. Variable name.
#' @return Character label string (HTML safe for element_markdown).
#' @noRd
.flux_climate_label <- function(flux_var) {
  switch(
    sub("_.*", "", flux_var),
    NEE  = lab_nee_annual,
    GPP  = lab_gpp_annual,
    RECO = lab_reco_annual,
    flux_var
  )
}

#' Climate scatter plots: precipitation vs NEE and temperature vs GPP
#'
#' Produces two scatter plots coloured by IGBP: (a) annual precipitation vs a
#' user-specified flux, and (b) annual temperature vs a second flux.  Both
#' plots use `fluxnet_theme()` and `ggtext::element_markdown()` axis labels.
#'
#' The column name convention for climate variables in the converted annual
#' data is `P_F` (precipitation, mm yr⁻¹) and `TA_F` (temperature, °C).
#' The function checks for these columns and stops with an informative error if
#' they are absent.
#'
#' @param data_yy Annual FLUXNET data frame. Must contain `site_id`, `IGBP`,
#'   `P_F`, `TA_F`, and the flux columns named in `flux_var_y` and
#'   `flux_var_y2`.
#' @param flux_var_y Character. Flux variable for the precipitation plot
#'   y-axis (default `"NEE_VUT_REF"`).
#' @param flux_var_y2 Character. Flux variable for the temperature plot
#'   y-axis (default `"GPP_NT_VUT_REF"`).
#' @param metadata Optional data frame with `site_id` and `igbp` columns used
#'   to supply IGBP when absent from `data_yy`.
#'
#' @return A named list:
#'   \describe{
#'     \item{`precip_vs_flux`}{ggplot of P_F vs `flux_var_y`.}
#'     \item{`temp_vs_flux`}{ggplot of TA_F vs `flux_var_y2`.}
#'   }
#'
#' @examples
#' \dontrun{
#' plots <- fig_climate_scatter(data_yy, metadata = snapshot_meta)
#' plots$precip_vs_flux
#' plots$temp_vs_flux
#' }
#'
#' @export
fig_climate_scatter <- function(data_yy,
                                flux_var_y  = "NEE_VUT_REF",
                                flux_var_y2 = "GPP_NT_VUT_REF",
                                metadata    = NULL) {

  # --- column checks ----------------------------------------------------------
  .check_cols_climate(data_yy, c("site_id", "P_F", "TA_F", flux_var_y, flux_var_y2))

  # --- IGBP -------------------------------------------------------------------
  if (!"IGBP" %in% names(data_yy) && !is.null(metadata)) {
    if (all(c("site_id", "igbp") %in% names(metadata))) {
      data_yy <- dplyr::left_join(
        data_yy,
        dplyr::select(metadata, site_id, IGBP = igbp),
        by = "site_id"
      )
    }
  }

  igbp_present <- "IGBP" %in% names(data_yy) && any(!is.na(data_yy$IGBP))

  if (!igbp_present) {
    warning("IGBP not available — plots will not be colour-coded by IGBP.",
            call. = FALSE)
    color_scale <- ggplot2::scale_color_discrete()
    aes_col <- NULL
  } else {
    data_yy     <- dplyr::mutate(data_yy,
                                 IGBP = factor(IGBP, levels = IGBP_order))
    color_scale <- scale_color_igbp(
      guide = ggplot2::guide_legend(ncol = 3, title = "IGBP")
    )
    aes_col <- ggplot2::aes(color = IGBP)
  }

  # --- labels -----------------------------------------------------------------
  y_lab1 <- .flux_climate_label(flux_var_y)
  y_lab2 <- .flux_climate_label(flux_var_y2)

  base_aes1 <- ggplot2::aes(x = P_F, y = .data[[flux_var_y]])
  base_aes2 <- ggplot2::aes(x = TA_F, y = .data[[flux_var_y2]])

  if (igbp_present) {
    base_aes1 <- utils::modifyList(base_aes1, aes_col)
    base_aes2 <- utils::modifyList(base_aes2, aes_col)
  }

  # --- precipitation vs flux --------------------------------------------------
  p_precip <- ggplot2::ggplot(
    dplyr::filter(data_yy, !is.na(P_F), !is.na(.data[[flux_var_y]])),
    base_aes1
  ) +
    ggplot2::geom_point(alpha = 0.55, size = 2.0) +
    color_scale +
    ggplot2::labs(
      x = lab_precip_annual,
      y = y_lab1
    ) +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown()
    )

  # --- temperature vs flux ----------------------------------------------------
  p_temp <- ggplot2::ggplot(
    dplyr::filter(data_yy, !is.na(TA_F), !is.na(.data[[flux_var_y2]])),
    base_aes2
  ) +
    ggplot2::geom_point(alpha = 0.55, size = 2.0) +
    color_scale +
    ggplot2::labs(
      x = lab_temp_annual,
      y = y_lab2
    ) +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.y = ggtext::element_markdown()
    )

  list(
    precip_vs_flux = p_precip,
    temp_vs_flux   = p_temp
  )
}

#' General XY scatter of any two annual variables, shaped by IGBP
#'
#' A flexible scatter plot for annual FLUXNET data with IGBP encoded as shape.
#' Axis labels default to the column names but can be overridden.
#'
#' @param data_yy Annual FLUXNET data frame. Must contain `site_id`, `x_var`,
#'   and `y_var`.
#' @param x_var Character. Column name for the x-axis variable.
#' @param y_var Character. Column name for the y-axis variable.
#' @param x_lab Character or `NULL`. Custom x-axis label (default: `x_var`).
#'   Supports HTML via `ggtext::element_markdown()`.
#' @param y_lab Character or `NULL`. Custom y-axis label (default: `y_var`).
#'   Supports HTML via `ggtext::element_markdown()`.
#' @param metadata Optional data frame with `site_id` and `igbp` columns for
#'   IGBP join when absent from `data_yy`.
#'
#' @return A single ggplot object.
#'
#' @examples
#' \dontrun{
#' fig_xy_annual(data_yy, x_var = "GPP_NT_VUT_REF", y_var = "LE_F_MDS",
#'               y_lab = "LE (W m<sup>-2</sup>)", metadata = snapshot_meta)
#' }
#'
#' @export
fig_xy_annual <- function(data_yy,
                          x_var,
                          y_var,
                          x_lab    = NULL,
                          y_lab    = NULL,
                          metadata = NULL) {

  .check_cols_climate(data_yy, c("site_id", x_var, y_var))

  # --- IGBP -------------------------------------------------------------------
  if (!"IGBP" %in% names(data_yy) && !is.null(metadata)) {
    if (all(c("site_id", "igbp") %in% names(metadata))) {
      data_yy <- dplyr::left_join(
        data_yy,
        dplyr::select(metadata, site_id, IGBP = igbp),
        by = "site_id"
      )
    }
  }

  igbp_present <- "IGBP" %in% names(data_yy) && any(!is.na(data_yy$IGBP))

  if (igbp_present) {
    igbp_lvls <- intersect(IGBP_order, unique(na.omit(data_yy$IGBP)))
    shp       <- stats::setNames(
      shape_igbp[seq_along(igbp_lvls)],
      igbp_lvls
    )
    data_yy <- dplyr::mutate(data_yy,
                             IGBP = factor(IGBP, levels = IGBP_order))
  } else {
    warning("IGBP not available — shapes not differentiated.", call. = FALSE)
  }

  plot_data <- dplyr::filter(
    data_yy,
    !is.na(.data[[x_var]]),
    !is.na(.data[[y_var]])
  )

  x_label <- if (!is.null(x_lab)) x_lab else x_var
  y_label <- if (!is.null(y_lab)) y_lab else y_var

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
  ) +
    ggplot2::geom_point(size = 2.5, stroke = 0.7, color = "black",
                        alpha = 0.75) +
    ggplot2::labs(x = x_label, y = y_label, shape = "IGBP") +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown(),
      legend.position = "right"
    )

  if (igbp_present) {
    p <- p +
      ggplot2::aes(shape = IGBP) +
      ggplot2::scale_shape_manual(values = shp)
  }

  p
}
