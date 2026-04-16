## 00_candidate_figures.R — Paper-candidate figures HTML report
##
## Produces outputs/candidate_figures.html from data in data/processed/.
## Contains figures under development as candidates for the annual paper.
## YY and MM data are loaded at startup; DD data is loaded lazily.
##
## Sections:
##   1 — Annual time series (YY)
##   2 — Monthly climatology (MM)
##   3 — Daily climatology (DD) — lazy load, freed after use
##   4 — Whittaker biome snapshots (ERA5 climate, Codespace-safe)
##   5 — Latitudinal multi-variable ribbon
##   6 — Environmental response curves (binned flux vs climate)
##   7 — Long-record annual time series by continent
##   8 — Network growth cumulative (metadata only)
##   9 — Network growth annual new sites (metadata only)
##  10 — UN subregion choropleth at 2010/2015/2020/2025 — count + density
##  11 — Deployment duration profile at 2010/2015/2020/2025 (draft)
##  12 — Network active proportion over time (draft)
##  13 — Subregion overview: map + total sites + latency (3-panel)
##
## Package requirements (all in renv.lock): ggplot2, dplyr, tidyr, readr,
## lubridate, scales, grDevices, jsonlite, plotly.
## Time-series and climatology sections use plotly (interactive).
## Daily climatology section uses ggplot2 rendered to inline PNG (static).

source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")
source("R/figures/fig_climate.R")
source("R/figures/fig_latitudinal.R")
source("R/figures/fig_environmental_response.R")
source("R/figures/fig_timeseries.R")
source("R/figures/fig_network_growth.R")
source("R/figures/fig_maps.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(readr)
library(lubridate)
library(scales)
library(plotly)
library(jsonlite)

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

check_pipeline_config()

if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)

# ---- Run metadata ----
RUN_DATETIME <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
GIT_HASH     <- tryCatch(
  system("git rev-parse --short HEAD", intern = TRUE),
  error = function(e) "unknown"
)

# ============================================================
# Data loading — YY and MM eager; DD lazy (loaded in build_s3)
# ============================================================
processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")
snapshots_dir <- file.path(FLUXNET_DATA_ROOT, "snapshots")

# Latest snapshot for site metadata (lat/lon, IGBP, hub)
snapshot_csv <- sort(
  list.files(snapshots_dir,
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)
snapshot_meta <- if (length(snapshot_csv) > 0) {
  readr::read_csv(snapshot_csv[[1]], show_col_types = FALSE) |>
    dplyr::select(site_id, data_hub, igbp,
                  location_lat, location_long,
                  first_year, last_year)
} else {
  NULL
}

# Full snapshot with all columns — used by fig_network_growth() and
# fig_map_subregion_sites() which need location_lat/long, first_year, last_year
snapshot_meta_full <- if (length(snapshot_csv) > 0) {
  readr::read_csv(snapshot_csv[[1]], show_col_types = FALSE)
} else {
  NULL
}

# Prefer converted > qc > raw for each resolution
load_best <- function(suffix) {
  for (stage in c("converted", "qc", "raw")) {
    p <- file.path(processed_dir,
                   paste0("flux_data_", stage, "_", suffix, ".rds"))
    if (file.exists(p)) {
      return(list(data = readRDS(p), stage = stage))
    }
  }
  NULL
}

# Normalise timestamps: add integer TIMESTAMP column matching the format
# expected by ts_year() / ts_month() (chars 1-4 = year, 5-6 = month):
#   YY  → TIMESTAMP = YYYY         (from YEAR column)
#   MM  → TIMESTAMP = YYYYMM       (from DATE Date object)
#   DD  → TIMESTAMP = YYYYMMDD     (from DATE Date object)
normalise_ts <- function(res, x) {
  df <- x$data
  if (!"TIMESTAMP" %in% names(df)) {
    if (res == "yy" && "YEAR" %in% names(df)) {
      df <- dplyr::mutate(df, TIMESTAMP = as.integer(.data$YEAR))
    } else if (res == "mm" && "DATE" %in% names(df)) {
      df <- dplyr::mutate(df,
        TIMESTAMP = as.integer(format(.data$DATE, "%Y%m")))
    } else if (res == "dd" && "DATE" %in% names(df)) {
      df <- dplyr::mutate(df,
        TIMESTAMP = as.integer(format(.data$DATE, "%Y%m%d")))
    }
  }
  x$data <- df
  x
}

# Eager load: YY and MM only
avail_eager <- Filter(
  Negate(is.null),
  setNames(lapply(c("yy", "mm"), load_best), c("yy", "mm"))
)

site_data <- setNames(
  lapply(names(avail_eager),
         function(res) normalise_ts(res, avail_eager[[res]])),
  names(avail_eager)
)

all_sites <- unique(unlist(
  lapply(site_data, function(x) unique(x$data$site_id))
))

if (length(all_sites) == 0) {
  message(
    "No processed data found in ", processed_dir, ".\n",
    "Run tests/fixtures/make_test_data.R or the full pipeline first."
  )
}

selected_sites <- all_sites

# ---- IGBP colour palette (from plot_constants.R) ----
IGBP_PAL <- setNames(poster_pal(length(IGBP_order)), IGBP_order)

site_igbp_lookup <- if (!is.null(snapshot_meta) && "igbp" %in% names(snapshot_meta)) {
  setNames(
    as.character(snapshot_meta$igbp[match(selected_sites, snapshot_meta$site_id)]),
    selected_sites
  )
} else {
  setNames(rep(NA_character_, length(selected_sites)), selected_sites)
}

# ============================================================
# Helper functions
# ============================================================

# Render a ggplot to an inline base64 PNG img tag (used for static DD plots).
# Resolution is 150 dpi — appropriate for typical screen viewing.
plot_to_png <- function(p, width = 8, height = 4.5) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = width, height = height, units = "in",
                 res = 150, bg = "white")
  print(p)
  grDevices::dev.off()
  b64 <- jsonlite::base64_enc(readBin(tmp, "raw", file.info(tmp)$size))
  paste0('<img src="data:image/png;base64,', b64,
         '" style="width:100%;max-width:', round(width * 150),
         'px;height:auto;display:block">')
}

# Convert a ggplot (or plotly figure) to an interactive plotly HTML div.
# plotly.js must be included in the page head (see html_head below).
.plt_counter <- local({ n <- 0L; function() { n <<- n + 1L; paste0("plt_", n) } })

plotly_div <- function(p, height = "380px") {
  div_id  <- .plt_counter()
  fig     <- if (inherits(p, "ggplot")) plotly::ggplotly(p) else p
  if (inherits(p, "ggplot")) {
    fig <- plotly::layout(fig,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
      margin = list(b = 80, l = 50, r = 20, t = 40)
    )
  }
  fig_json <- plotly::plotly_json(fig, jsonedit = FALSE)
  paste0(
    '<div id="', div_id, '" class="plot-wrap plotly-chart"',
    ' style="height:', height, ';min-height:', height, '"></div>\n',
    '<script>(function(){var f=', fig_json, ';',
    'Plotly.react("', div_id, '",f.data,f.layout,{responsive:true});})();</script>'
  )
}

section <- function(n, title, body) {
  paste0('\n<section id="s', n, '">',
         '<h2>', n, ". ", title, "</h2>", body, "</section>\n")
}

no_data <- function(msg = "No data available for this section.") {
  paste0('<p class="note">', msg, "</p>")
}

# Extract year from a TIMESTAMP column (handles YY=YYYY, MM=YYYYMM, DD=YYYYMMDD)
ts_year  <- function(ts) as.integer(substr(as.character(ts), 1, 4))
ts_month <- function(ts) as.integer(substr(as.character(ts), 5, 6))

# Join IGBP class onto any data frame with a site_id column.
join_igbp <- function(df) {
  if (is.null(snapshot_meta) || !"igbp" %in% names(snapshot_meta)) return(df)
  dplyr::left_join(
    df,
    dplyr::distinct(snapshot_meta, .data$site_id, .data$igbp),
    by = "site_id"
  )
}

# Shared IGBP colour scale used across all sections
igbp_colour_scale <- scale_colour_manual(values = IGBP_PAL, name = "IGBP",
                                         na.value = "#888888")

# ============================================================
# HTML document header and CSS
# ============================================================
html_head <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>FLUXNET Annual 2026 — Candidate Figures</title>
<script src="https://cdn.plot.ly/plotly-2.32.0.min.js" crossorigin="anonymous"></script>
<style>
  *, *::before, *::after { box-sizing: border-box; }
  body { font-family: system-ui, -apple-system, sans-serif; font-size: 14px;
         color: #1a1a2e; background: #fafafa; margin: 0; padding: 0; }
  header { background: #1a1a2e; color: #fff; padding: 1.5rem 2rem; }
  header h1 { margin: 0; font-size: 1.4rem; }
  header p  { margin: 0.3rem 0 0; font-size: 0.85rem; opacity: 0.75; }
  main { max-width: 1100px; margin: 0 auto; padding: 2rem 1.5rem; }
  section { margin-bottom: 2.5rem; }
  h2 { font-size: 1.1rem; border-bottom: 2px solid #1a1a2e; padding-bottom: 0.3rem;
       margin-top: 0; }
  h3 { font-size: 0.95rem; color: #444; margin: 1rem 0 0.4rem; }
  svg { width: 100%; height: auto; display: block; }
  .plotly-chart svg { width: auto; height: auto; }
  .plot-wrap img { width: 100%; height: auto; display: block; }
  .plot-wrap { background: #fff; border: 1px solid #e0e0e0; border-radius: 4px;
               padding: 0.5rem; margin-bottom: 1rem; }
  .note { font-style: italic; color: #666; background: #f9f9e8;
          padding: 0.5rem 0.75rem; border-left: 3px solid #ccc;
          border-radius: 2px; font-size: 12px; }
  footer { background: #f0f0f0; border-top: 1px solid #ddd;
           padding: 1rem 2rem; font-size: 11px; color: #555; }
  footer dl { display: grid; grid-template-columns: 160px 1fr;
              gap: 0.2rem 0.5rem; margin: 0; }
  footer dt { font-weight: 600; }
</style>
</head>
<body>
<header>
  <h1>FLUXNET Annual 2026 — Candidate Figures</h1>
  <p>Paper-candidate figures under development &mdash; generated by 00_candidate_figures.R &mdash;
     interactive charts: plotly &bull; static charts: ggplot2/PNG</p>
</header>
<main>
'

# ============================================================
# Section 1 — Annual time series (YY)
# ============================================================
build_s1 <- function() {
  if (is.null(site_data[["yy"]])) return(no_data("No YY data available."))
  df <- site_data[["yy"]]$data |>
    dplyr::mutate(yr = ts_year(.data$TIMESTAMP)) |>
    join_igbp()

  has_col <- function(col) col %in% names(df)

  plots <- list()

  # NEE — filter to years where NEE_VUT_REF is not NA to avoid ERA5-only rows
  # creating spurious line segments in the plotly time series.
  if (has_col("NEE_VUT_REF")) {
    p <- ggplot(dplyr::filter(df, !is.na(.data$NEE_VUT_REF)),
                aes(x = .data$yr, y = .data$NEE_VUT_REF,
                        colour = .data$igbp, group = .data$site_id)) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      igbp_colour_scale +
      labs(title = "Annual NEE \u2014 all sites (coloured by IGBP)",
           x = "Year", y = "NEE (gC m\u207b\u00b2 yr\u207b\u00b9)") +
      fluxnet_theme()
    plots[["nee"]] <- plotly_div(p)
  }

  # GPP — NT solid, DT dashed; drop NA rows so ERA5-only years do not appear.
  if (has_col("GPP_NT_VUT_REF") || has_col("GPP_DT_VUT_REF")) {
    gpp_long <- dplyr::bind_rows(
      if (has_col("GPP_NT_VUT_REF"))
        dplyr::transmute(df, site_id, igbp = .data$igbp, yr,
                         gpp = .data$GPP_NT_VUT_REF, type = "NT"),
      if (has_col("GPP_DT_VUT_REF"))
        dplyr::transmute(df, site_id, igbp = .data$igbp, yr,
                         gpp = .data$GPP_DT_VUT_REF, type = "DT")
    ) |> dplyr::filter(!is.na(.data$gpp))
    p <- ggplot(gpp_long,
                aes(x = .data$yr, y = .data$gpp,
                    colour = .data$igbp,
                    linetype = .data$type,
                    group = interaction(.data$site_id, .data$type))) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      igbp_colour_scale +
      scale_linetype_manual(values = c(NT = "solid", DT = "dashed"),
                            name = "Partitioning") +
      labs(title = "Annual GPP (NT solid, DT dashed) \u2014 all sites (coloured by IGBP)",
           x = "Year", y = "GPP (gC m\u207b\u00b2 yr\u207b\u00b9)") +
      fluxnet_theme()
    plots[["gpp"]] <- plotly_div(p)
  }

  # ET from LE — filter to non-NA years for the same ERA5 row reason.
  if (has_col("LE_F_MDS")) {
    p <- ggplot(dplyr::filter(df, !is.na(.data$LE_F_MDS)),
                aes(x = .data$yr, y = .data$LE_F_MDS,
                        colour = .data$igbp, group = .data$site_id)) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      igbp_colour_scale +
      labs(title = "Annual ET (from LE_F_MDS) \u2014 all sites (coloured by IGBP)",
           x = "Year", y = "ET (mm yr\u207b\u00b9)") +
      fluxnet_theme()
    plots[["et"]] <- plotly_div(p)
  }

  if (length(plots) == 0) return(no_data("Required columns not found in YY data."))
  paste(unlist(plots), collapse = "\n")
}

# ============================================================
# Section 2 — Monthly climatology (MM)
# ============================================================
build_s2 <- function() {
  if (is.null(site_data[["mm"]])) return(no_data("No MM data available."))
  df <- site_data[["mm"]]$data |>
    dplyr::mutate(month = ts_month(.data$TIMESTAMP)) |>
    join_igbp()

  # Mean over all years per site × month
  clim <- df |>
    dplyr::group_by(.data$site_id, .data$igbp, .data$month) |>
    dplyr::summarise(
      NEE    = mean(.data$NEE_VUT_REF,  na.rm = TRUE),
      GPP_NT = if ("GPP_NT_VUT_REF" %in% names(df))
                 mean(.data$GPP_NT_VUT_REF, na.rm = TRUE) else NA_real_,
      GPP_DT = if ("GPP_DT_VUT_REF" %in% names(df))
                 mean(.data$GPP_DT_VUT_REF, na.rm = TRUE) else NA_real_,
      ET     = if ("LE_F_MDS" %in% names(df))
                 mean(.data$LE_F_MDS, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )

  mo_labs <- c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")

  make_panel <- function(aes_y, title, ylab) {
    p <- ggplot(clim, aes(x = .data$month, y = {{ aes_y }},
                          colour = .data$igbp, group = .data$site_id)) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      igbp_colour_scale +
      scale_x_continuous(breaks = 1:12, labels = mo_labs) +
      labs(title = title, x = "Month", y = ylab) +
      fluxnet_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plotly_div(p)
  }

  plots <- list()
  plots[["nee"]] <- make_panel(.data$NEE,
                               "Monthly climatology \u2014 NEE (all sites, coloured by IGBP)",
                               "NEE (gC m\u207b\u00b2 month\u207b\u00b9)")

  if (!all(is.na(clim$GPP_NT)) || !all(is.na(clim$GPP_DT))) {
    gpp_long <- dplyr::bind_rows(
      dplyr::transmute(clim, site_id, igbp = .data$igbp, month,
                       gpp = .data$GPP_NT, type = "NT"),
      dplyr::transmute(clim, site_id, igbp = .data$igbp, month,
                       gpp = .data$GPP_DT, type = "DT")
    ) |> dplyr::filter(!is.na(.data$gpp))

    p_gpp <- ggplot(gpp_long,
                    aes(x = .data$month, y = .data$gpp,
                        colour = .data$igbp,
                        linetype = .data$type,
                        group = interaction(.data$site_id, .data$type))) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      igbp_colour_scale +
      scale_linetype_manual(values = c(NT = "solid", DT = "dashed"),
                            name = "Partitioning") +
      scale_x_continuous(breaks = 1:12, labels = mo_labs) +
      labs(title = "Monthly climatology \u2014 GPP (NT solid, DT dashed) \u2014 all sites (IGBP colour)",
           x = "Month", y = "GPP (gC m\u207b\u00b2 month\u207b\u00b9)") +
      fluxnet_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots[["gpp"]] <- plotly_div(p_gpp)
  }

  if (!all(is.na(clim$ET))) {
    plots[["et"]] <- make_panel(.data$ET,
                                "Monthly climatology \u2014 ET (all sites, coloured by IGBP)",
                                "ET (mm month\u207b\u00b9)")
  }

  paste(unlist(plots), collapse = "\n")
}

# ============================================================
# Section 3 — Daily climatology (DD) — lazy load
# DD data is large (~1.9 GB, 576 cols); load immediately before use and
# free immediately after to avoid holding it in memory.
# ============================================================
build_s3 <- function() {
  message("  Loading DD data (lazy) ...")
  dd_raw <- load_best("dd")
  if (is.null(dd_raw)) return(no_data("No DD data available."))

  dd_loaded <- normalise_ts("dd", dd_raw)
  rm(dd_raw)

  # Select only flux columns needed — avoids manipulating ~576-col frame
  flux_cols <- intersect(
    c("site_id", "TIMESTAMP", "DOY",
      "NEE_VUT_REF", "GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "LE_F_MDS"),
    names(dd_loaded$data)
  )
  df <- dd_loaded$data |>
    dplyr::select(dplyr::all_of(flux_cols)) |>
    join_igbp()

  rm(dd_loaded)
  gc()

  if (!"DOY" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        DOY = as.integer(format(as.Date(as.character(.data$TIMESTAMP),
                                        format = "%Y%m%d"), "%j"))
      )
  }

  # Pre-aggregate to mean DOY climatology per site (across all years)
  make_panel_dd <- function(y_col, title, ylab) {
    if (!y_col %in% names(df)) return(NULL)
    clim_dd <- dplyr::select(df, site_id, igbp = .data$igbp, DOY,
                              y = dplyr::all_of(y_col)) |>
      dplyr::filter(!is.na(.data$y)) |>
      dplyr::group_by(.data$site_id, .data$igbp, .data$DOY) |>
      dplyr::summarise(y = mean(.data$y, na.rm = TRUE), .groups = "drop")
    p <- ggplot(clim_dd, aes(x = .data$DOY, y = .data$y,
                             colour = .data$igbp, group = .data$site_id)) +
      geom_line(linewidth = 0.5, alpha = 0.6) +
      igbp_colour_scale +
      labs(title = title, x = "Day of year", y = ylab) +
      fluxnet_theme(base_size = 11)
    paste0('<div class="plot-wrap">', plot_to_png(p, 9, 4.5), "</div>")
  }

  plots <- Filter(Negate(is.null), list(
    nee = make_panel_dd("NEE_VUT_REF",
                        "Daily climatology \u2014 NEE, all sites \u2014 mean by DOY (IGBP colour)",
                        "NEE (gC m\u207b\u00b2 day\u207b\u00b9)"),
    gpp = {
      if (!any(c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF") %in% names(df))) {
        NULL
      } else {
        agg_type <- function(col, lbl) {
          if (!col %in% names(df)) return(NULL)
          dplyr::transmute(df, site_id, igbp = .data$igbp, DOY,
                           gpp = .data[[col]], type = lbl) |>
            dplyr::filter(!is.na(.data$gpp)) |>
            dplyr::group_by(.data$site_id, .data$igbp, .data$DOY, .data$type) |>
            dplyr::summarise(gpp = mean(.data$gpp, na.rm = TRUE), .groups = "drop")
        }
        gpp_dd <- dplyr::bind_rows(
          agg_type("GPP_NT_VUT_REF", "NT"),
          agg_type("GPP_DT_VUT_REF", "DT")
        )
        p <- ggplot(gpp_dd,
                    aes(x = .data$DOY, y = .data$gpp,
                        colour = .data$igbp,
                        linetype = .data$type,
                        group = interaction(.data$site_id, .data$type))) +
          geom_line(linewidth = 0.5, alpha = 0.6) +
          igbp_colour_scale +
          scale_linetype_manual(values = c(NT = "solid", DT = "dashed"),
                                name = "Partitioning") +
          labs(title = "Daily climatology \u2014 GPP, all sites \u2014 mean by DOY (IGBP colour, NT solid, DT dashed)",
               x = "Day of year", y = "GPP (gC m\u207b\u00b2 day\u207b\u00b9)") +
          fluxnet_theme(base_size = 11)
        paste0('<div class="plot-wrap">', plot_to_png(p, 9, 4.5), "</div>")
      }
    },
    et = make_panel_dd("LE_F_MDS",
                       "Daily climatology \u2014 ET, all sites \u2014 mean by DOY (IGBP colour)",
                       "ET (mm day\u207b\u00b9)")
  ))

  if (length(plots) == 0) return(no_data("Required columns not found in DD data."))
  paste(unlist(plots), collapse = "\n")
}

# ============================================================
# Section 5 — Latitudinal multi-variable ribbon
# Calls fig_latitudinal_multi() for NEE, LE, and H; saves a review PNG.
# ============================================================
build_latitudinal_multi <- function() {
  if (is.null(site_data[["yy"]])) return(no_data("No YY data available."))
  if (is.null(snapshot_meta))     return(no_data("No snapshot metadata available."))

  data_yy <- site_data[["yy"]]$data

  p <- tryCatch(
    fig_latitudinal_multi(data_yy, metadata = snapshot_meta),
    error = function(e) {
      warning("fig_latitudinal_multi() failed: ", conditionMessage(e),
              call. = FALSE)
      NULL
    }
  )

  if (is.null(p)) return(no_data("Latitudinal multi figure could not be generated."))

  # Save review PNG
  review_dir <- file.path("review", "figures")
  if (!dir.exists(review_dir)) dir.create(review_dir, recursive = TRUE)
  review_path <- file.path(review_dir, "fig_latitudinal_multi.png")
  ggplot2::ggsave(
    review_path,
    plot   = p,
    width  = 10,
    height = 14,
    units  = "in",
    dpi    = 150
  )
  message("Review figure saved: ", review_path)

  paste0('<div class="plot-wrap">',
         plot_to_png(p, width = 10, height = 14),
         "</div>")
}

# ============================================================
# Section 4 — Whittaker biome snapshots
# ERA5 version: four panels at year_cutoff = 2010 / 2015 / 2020 / 2025,
# assembled 2×2.  Uses TA_ERA / P_ERA from the processed YY data — no
# external WorldClim data required; runs in the Codespace.
# WorldClim version (local Mac only) is available via source = "worldclim".
# ============================================================
build_whittaker_snapshots <- function() {
  if (is.null(site_data[["yy"]])) return(no_data("No YY data available."))

  tryCatch({
    data_yy <- site_data[["yy"]]$data
    cutoffs <- c(2010L, 2015L, 2020L, 2025L)

    panels <- lapply(cutoffs, function(yr) {
      fig_whittaker_hexbin(
        data_yy     = data_yy,
        flux_var    = "NEE_VUT_REF",
        year_cutoff = yr,
        source      = "era5"
      ) + fluxnet_theme(base_size = 11)
    })

    pw <- (panels[[1]] | panels[[2]]) /
          (panels[[3]] | panels[[4]]) +
      patchwork::plot_layout(guides = "collect")

    # Save review PNG
    review_dir  <- file.path("review", "figures")
    if (!dir.exists(review_dir)) dir.create(review_dir, recursive = TRUE)
    review_path <- file.path(review_dir, "fig_whittaker_hexbin_era5.png")
    ggplot2::ggsave(
      review_path,
      plot   = pw,
      width  = 14,
      height = 10,
      units  = "in",
      dpi    = 150
    )
    message("Review figure saved: ", review_path)

    paste0('<div class="plot-wrap">',
           plot_to_png(pw, width = 14, height = 10),
           "</div>")
  }, error = function(e) {
    no_data(paste0(
      "Whittaker ERA5 snapshots unavailable: ", conditionMessage(e)
    ))
  })
}

# ============================================================
# Section 6 — Environmental response curves
# Calls fig_environmental_response() with TA_F and P_F only (no WorldClim or
# aridity data required in the Codespace).  aridity_index is excluded so the
# section runs without external data downloads.
# ============================================================
build_environmental_response <- function() {
  if (is.null(site_data[["yy"]])) return(no_data("No YY data available."))

  data_yy <- site_data[["yy"]]$data |> join_igbp()

  tryCatch({
    plots <- fig_environmental_response(
      data_yy  = data_yy,
      env_vars = c("TA_F", "P_F")   # aridity_index excluded — requires external data
    )

    if (length(plots) == 0L) {
      return(no_data(
        "No valid env_var \u00d7 flux_var combinations found. ",
        "Columns TA_F and P_F may not be present in the processed YY data."
      ))
    }

    divs <- lapply(names(plots), function(nm) {
      paste0(
        '<h3>', nm, '</h3>',
        '<div class="plot-wrap">',
        plot_to_png(plots[[nm]], width = 7, height = 5),
        '</div>'
      )
    })
    paste(unlist(divs), collapse = "\n")
  }, error = function(e) {
    no_data(paste0(
      "Environmental response curves unavailable: ", conditionMessage(e),
      " &mdash; aridity_index requires CGIAR data (see R/external_data.R)."
    ))
  })
}

# ============================================================
# Section 7 — Long-record annual time series by continent
# Calls save_long_record_sites() to write data/snapshots/long_record_sites.csv
# and print the table, then fig_long_record_timeseries() for all flux vars.
# One patchwork per continent; review PNGs saved to review/figures/.
# ============================================================
build_long_record_timeseries <- function() {
  if (is.null(site_data[["yy"]])) return(no_data("No YY data available."))
  if (is.null(snapshot_meta))     return(no_data("No snapshot metadata available."))

  data_yy <- site_data[["yy"]]$data

  # Write long_record_sites.csv and print the table to the terminal
  tryCatch(
    save_long_record_sites(n_sites = 5L),
    error = function(e) {
      warning("save_long_record_sites() failed: ", conditionMessage(e),
              call. = FALSE)
    }
  )

  plots <- tryCatch(
    fig_long_record_timeseries(data_yy, metadata = snapshot_meta),
    error = function(e) {
      warning("fig_long_record_timeseries() failed: ", conditionMessage(e),
              call. = FALSE)
      NULL
    }
  )

  if (is.null(plots) || length(plots) == 0L) {
    return(no_data(
      "No long-record time series generated. Data may be too sparse or continent mapping failed."
    ))
  }

  review_dir <- file.path("review", "figures")
  if (!dir.exists(review_dir)) dir.create(review_dir, recursive = TRUE)

  divs <- lapply(names(plots), function(cont) {
    p <- plots[[cont]]

    review_path <- file.path(
      review_dir,
      paste0("fig_timeseries_", tolower(gsub("[^A-Za-z0-9]", "_", cont)), ".png")
    )
    tryCatch(
      ggplot2::ggsave(review_path, plot = p, width = 10, height = 12,
                      units = "in", dpi = 150),
      error = function(e) {
        warning("Could not save review PNG for ", cont, ": ",
                conditionMessage(e), call. = FALSE)
      }
    )
    message("Review figure saved: ", review_path)

    paste0(
      '<h3>', cont, '</h3>',
      '<div class="plot-wrap">',
      plot_to_png(p, width = 10, height = 12),
      '</div>'
    )
  })

  paste(unlist(divs), collapse = "\n")
}

# ============================================================
# Section 8 — Network growth (cumulative)
# fig_network_growth(): cumulative sites by IGBP; metadata only.
# ============================================================
build_network_growth <- function() {
  if (is.null(snapshot_meta_full)) return(no_data("No snapshot metadata available."))
  tryCatch({
    p <- fig_network_growth(snapshot_meta_full)
    review_dir <- file.path("review", "figures")
    if (!dir.exists(review_dir)) dir.create(review_dir, recursive = TRUE)
    review_path <- file.path(review_dir, "fig_network_growth.png")
    ggplot2::ggsave(review_path, plot = p, width = 10, height = 6,
                    units = "in", dpi = 150)
    message("Review figure saved: ", review_path)
    paste0('<div class="plot-wrap">', plot_to_png(p, width = 10, height = 6),
           "</div>")
  }, error = function(e) {
    no_data(paste0("Network growth (cumulative) unavailable: ", conditionMessage(e)))
  })
}

# ============================================================
# Section 9 — Network growth (annual new sites)
# fig_network_growth_annual(): new sites per year; metadata only.
# ============================================================
build_network_growth_annual <- function() {
  if (is.null(snapshot_meta_full)) return(no_data("No snapshot metadata available."))
  tryCatch({
    p <- fig_network_growth_annual(snapshot_meta_full)
    review_dir <- file.path("review", "figures")
    if (!dir.exists(review_dir)) dir.create(review_dir, recursive = TRUE)
    review_path <- file.path(review_dir, "fig_network_growth_annual.png")
    ggplot2::ggsave(review_path, plot = p, width = 10, height = 6,
                    units = "in", dpi = 150)
    message("Review figure saved: ", review_path)
    paste0('<div class="plot-wrap">', plot_to_png(p, width = 10, height = 6),
           "</div>")
  }, error = function(e) {
    no_data(paste0("Network growth (annual) unavailable: ", conditionMessage(e)))
  })
}

# ============================================================
# Section 11 — Deployment duration profile (draft)
# fig_network_duration_profile(): histograms of record length at four snapshot
# years, coloured by active/inactive status. Requires data_yy + metadata.
# ============================================================
build_duration_profile <- function() {
  if (is.null(snapshot_meta_full)) return(no_data("No snapshot metadata available."))
  tryCatch({
    figs <- fig_network_duration_profile(metadata = snapshot_meta_full)
    review_dir <- file.path("review", "figures")
    if (!dir.exists(review_dir)) dir.create(review_dir, recursive = TRUE)

    path_vA <- file.path(review_dir, "fig_network_duration_profile_vA.png")
    path_vB <- file.path(review_dir, "fig_network_duration_profile_vB.png")
    ggplot2::ggsave(path_vA, plot = figs$vA, width = 8,  height = 14,
                    units = "in", dpi = 150)
    ggplot2::ggsave(path_vB, plot = figs$vB, width = 18, height = 5,
                    units = "in", dpi = 150)
    message("Review figures saved: ", path_vA, " | ", path_vB)

    paste0(
      "<h3>Version A \u2014 stacked vertically</h3>",
      '<div class="plot-wrap">', plot_to_png(figs$vA, width = 8,  height = 14), "</div>",
      "<h3>Version B \u2014 side by side</h3>",
      '<div class="plot-wrap">', plot_to_png(figs$vB, width = 18, height = 5),  "</div>"
    )
  }, error = function(e) {
    no_data(paste0("Deployment duration profile unavailable: ", conditionMessage(e)))
  })
}

# ============================================================
# Section 12 — Network active proportion over time (draft)
# fig_network_active_proportion(): cumulative sites + % functionally active.
# Draft candidate for integration into network growth figure.
# ============================================================
build_active_proportion <- function() {
  if (is.null(snapshot_meta_full)) return(no_data("No snapshot metadata available."))
  if (is.null(site_data[["yy"]]))  return(no_data("No YY data available."))
  tryCatch({
    p <- fig_network_active_proportion(
      metadata = snapshot_meta_full,
      data_yy  = site_data[["yy"]]$data
    )
    review_dir  <- file.path("review", "figures")
    if (!dir.exists(review_dir)) dir.create(review_dir, recursive = TRUE)
    review_path <- file.path(review_dir, "fig_network_active_proportion.png")
    ggplot2::ggsave(review_path, plot = p, width = 10, height = 8,
                    units = "in", dpi = 150)
    message("Review figure saved: ", review_path)
    paste0('<div class="plot-wrap">', plot_to_png(p, width = 10, height = 8),
           "</div>")
  }, error = function(e) {
    no_data(paste0("Network active proportion unavailable: ", conditionMessage(e)))
  })
}

# ============================================================
# Section 10 — UN subregion choropleth
# fig_map_subregion_sites(): count and density at 2010/2015/2020/2025.
# Two separate figures (count + density) saved as review PNGs.
# ============================================================
build_country_map <- function() {
  if (is.null(snapshot_meta_full)) return(no_data("No snapshot metadata available."))

  review_dir <- file.path("review", "figures")
  if (!dir.exists(review_dir)) dir.create(review_dir, recursive = TRUE)

  cutoffs <- c(2010L, 2015L, 2020L, 2025L)

  make_panel <- function(metric_arg) {
    tryCatch({
      p <- fig_map_subregion_sites(
        snapshot_meta_full,
        year_cutoffs = cutoffs,
        metric       = metric_arg,
        add_dots     = TRUE
      )
      review_path <- file.path(
        review_dir,
        paste0("fig_map_subregion_sites_", metric_arg, ".png")
      )
      ggplot2::ggsave(review_path, plot = p, width = 10, height = 18,
                      units = "in", dpi = 150)
      message("Review figure saved: ", review_path)
      list(html = plot_to_png(p, width = 10, height = 18), ok = TRUE)
    }, error = function(e) {
      list(html = no_data(paste0("Subregion choropleth (", metric_arg, ") unavailable: ",
                                 conditionMessage(e))),
           ok = FALSE)
    })
  }

  r_count   <- make_panel("count")
  r_density <- make_panel("density")

  paste0(
    "<h3>Site count per UN subregion</h3>",
    '<div class="plot-wrap">', r_count$html,   "</div>",
    "<h3>Site density per UN subregion (sites per 10&#x2076; km&#x00B2;)</h3>",
    '<div class="plot-wrap">', r_density$html, "</div>"
  )
}

# ============================================================
# Assemble the report
# ============================================================
message("Building Section 1 — Annual time series (YY) ...")
s1 <- section(1, "Annual time series (YY data)",  build_s1())
message("Building Section 2 — Monthly climatology (MM) ...")
s2 <- section(2, "Monthly climatology (MM data)", build_s2())
message("Building Section 3 — Daily climatology (DD) ...")
s3 <- section(3, "Daily climatology (DD data)",   build_s3())
message("Building Section 8 — Network growth (cumulative) ...")
s8 <- section(8, "Network growth \u2014 cumulative sites by IGBP",
              build_network_growth())
message("Building Section 9 — Network growth (annual new sites) ...")
s9 <- section(9, "Network growth \u2014 new sites per year by IGBP",
              build_network_growth_annual())
message("Building Section 11 — Deployment duration profile ...")
s11 <- section(11, "Deployment duration profile \u2014 record length at snapshot years (draft)",
               build_duration_profile())
message("Building Section 12 — Network active proportion ...")
s12 <- section(12, "Network size and data currency over time (draft)",
               build_active_proportion())
message("Building Section 13 — Subregion overview ...")
s13 <- section(13, "Subregion overview \u2014 site counts and latency by UN subregion (2025)",
               tryCatch({
                 p <- fig_network_subregion_overview(
                   metadata = snapshot_meta_full
                 )
                 review_dir <- file.path("review", "figures")
                 if (!dir.exists(review_dir)) dir.create(review_dir, recursive = TRUE)
                 review_path <- file.path(review_dir, "fig_network_subregion_overview.png")
                 ggplot2::ggsave(review_path, plot = p, width = 12, height = 10,
                                 units = "in", dpi = 150)
                 message("Review figure saved: ", review_path)
                 paste0('<div class="plot-wrap">',
                        plot_to_png(p, width = 12, height = 10), "</div>")
               }, error = function(e) {
                 no_data(paste0("Subregion overview unavailable: ", conditionMessage(e)))
               }))
message("Building Section 10 — UN subregion choropleth ...")
s10 <- section(10, "UN subregion choropleth \u2014 2010\u20132015\u20132020\u20132025",
               build_country_map())
message("Building Section 4 — Whittaker biome snapshots (ERA5) ...")
s4 <- section(4, "Whittaker biome snapshots \u2014 ERA5 climate (Codespace-safe)",
              build_whittaker_snapshots())
message("Building Section 5 — Latitudinal multi-variable ribbon ...")
s5 <- section(5, "Latitudinal multi-variable ribbon", build_latitudinal_multi())
message("Building Section 6 — Environmental response curves ...")
s6 <- section(6, "Environmental response curves (binned flux vs climate)",
              build_environmental_response())
message("Building Section 7 — Long-record annual time series by continent ...")
s7 <- section(7, "Long-record annual time series by continent",
              build_long_record_timeseries())

html_footer <- paste0(
  '</main>\n<footer>\n<dl>\n',
  "<dt>Run datetime UTC</dt><dd>",   RUN_DATETIME, "</dd>\n",
  "<dt>Git hash</dt><dd>",           GIT_HASH,     "</dd>\n",
  "<dt>Sites included</dt><dd>",     length(selected_sites), " sites", "</dd>\n",
  "<dt>Site IDs</dt><dd>",
  if (length(selected_sites) > 0) paste(sort(selected_sites), collapse = ", ")
  else "(none)",
  "</dd>\n",
  "<dt>Resolutions loaded</dt><dd>",
  if (length(site_data) > 0) paste(toupper(names(site_data)), collapse = ", ")
  else "(none)",
  "</dd>\n",
  "<dt>FLUXNET_EXTRACT_RESOLUTIONS</dt><dd>",
  paste(FLUXNET_EXTRACT_RESOLUTIONS, collapse = " "), "</dd>\n",
  "</dl>\n</footer>\n</body>\n</html>"
)

out_html <- paste0(html_head, s1, s2, s3, s8, s9, s11, s12, s13, s10, s4, s5, s6, s7, html_footer)

out_path <- file.path("outputs", "candidate_figures.html")
writeLines(out_html, out_path, useBytes = FALSE)
message("Report written: ", out_path,
        " (", round(file.size(out_path) / 1024), " KB)")

# Write companion metadata per CLAUDE.md output spec
write_output_metadata(
  out_path,
  input_sources = c(
    if (length(snapshot_csv) > 0) snapshot_csv[[1]] else character(0),
    unlist(lapply(names(avail_eager), function(s) {
      file.path(processed_dir,
                paste0("flux_data_", avail_eager[[s]]$stage, "_", s, ".rds"))
    }))
  ),
  notes = paste0("Candidate figures report. All sites included (n=", length(selected_sites),
                 "): ", paste(sort(selected_sites), collapse = ", "), ".")
)

# Session info
writeLines(capture.output(sessionInfo()),
           file.path("outputs", "session_info.txt"))
message("Done.")
