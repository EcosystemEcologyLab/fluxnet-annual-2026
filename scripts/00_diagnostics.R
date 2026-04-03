## 00_diagnostics.R — Self-contained HTML diagnostic report
##
## Produces outputs/diagnostics.html from data in data/processed/.
## Displays ALL sites present in the processed data.
## Time-series and climatology plots colour sites by IGBP class using the
## shared palette from R/plot_constants.R.
##
## Package requirements (all in renv.lock): ggplot2, dplyr, tidyr, readr,
## lubridate, scales, grDevices, jsonlite, plotly.
## Time-series, climatology, and site map sections use plotly (interactive).
## QC / sensitivity sections use ggplot2 rendered to inline PNG (static).

source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")

library(dplyr)
library(tidyr)
library(ggplot2)
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
# Data loading
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

avail <- Filter(
  Negate(is.null),
  setNames(lapply(c("yy", "mm", "dd"), load_best), c("yy", "mm", "dd"))
)

all_sites <- unique(unlist(
  lapply(avail, function(x) unique(x$data$site_id))
))

if (length(all_sites) == 0) {
  message(
    "No processed data found in ", processed_dir, ".\n",
    "Run tests/fixtures/make_test_data.R or the full pipeline first."
  )
}

# Use all sites — no sampling
selected_sites <- all_sites

# No filtering; use all processed data
site_data <- avail

# Normalise timestamps: add integer TIMESTAMP column matching the format
# expected by ts_year() / ts_month() (chars 1-4 = year, 5-6 = month):
#   YY  → TIMESTAMP = YYYY         (from YEAR column)
#   MM  → TIMESTAMP = YYYYMM       (from DATE Date object)
#   DD  → TIMESTAMP = YYYYMMDD     (from DATE Date object)
site_data <- setNames(lapply(names(site_data), function(res) {
  x  <- site_data[[res]]
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
}), names(site_data))

# ---- IGBP colour palette (from plot_constants.R) -------------------------
# Sites of the same IGBP class share a colour family.
# IGBP_order and poster_pal() are sourced from R/plot_constants.R above.
IGBP_PAL <- setNames(poster_pal(length(IGBP_order)), IGBP_order)

# Map each site to its IGBP class (for use in section builders)
site_igbp_lookup <- if (!is.null(snapshot_meta) && "igbp" %in% names(snapshot_meta)) {
  setNames(
    as.character(snapshot_meta$igbp[match(selected_sites, snapshot_meta$site_id)]),
    selected_sites
  )
} else {
  setNames(rep(NA_character_, length(selected_sites)), selected_sites)
}

# SITE_PAL: per-site colour derived from IGBP colour; grey for unknown IGBP
SITE_PAL <- setNames(
  ifelse(!is.na(site_igbp_lookup) & site_igbp_lookup %in% names(IGBP_PAL),
         IGBP_PAL[site_igbp_lookup],
         "#888888"),
  selected_sites
)

# ============================================================
# Helper functions
# ============================================================

# Render a ggplot to an inline base64 PNG img tag (used for static QC/sensitivity plots).
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
  # For ggplot-converted charts, move legend below plot with enough margin
  # so it does not overlap the x-axis label.
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

# Data frame → HTML table string.
df_to_html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return('<p class="note">No records.</p>')
  }
  fmt <- as.data.frame(lapply(df, function(x) {
    if (is.numeric(x)) formatC(x, format = "g", digits = 4) else as.character(x)
  }), stringsAsFactors = FALSE)
  hdr  <- paste0("<th>", names(fmt), "</th>", collapse = "")
  rows <- apply(fmt, 1, function(r) {
    paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>")
  })
  paste0('<div class="tbl-wrap"><table class="dt">',
         "<thead><tr>", hdr, "</tr></thead><tbody>",
         paste(rows, collapse = ""),
         "</tbody></table></div>")
}

section <- function(n, title, body) {
  paste0('\n<section id="s', n, '">',
         '<h2>', n, ". ", title, "</h2>", body, "</section>\n")
}

no_data <- function(msg = "No data available for this section.") {
  paste0('<p class="note">', msg, "</p>")
}

diag_theme <- theme_bw(base_size = 11) +
  theme(
    strip.background   = element_rect(fill = "#f4f4f4", colour = NA),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank()
  )

# Extract year from a TIMESTAMP column (handles YY=YYYY, MM=YYYYMM, DD=YYYYMMDD)
ts_year <- function(ts) as.integer(substr(as.character(ts), 1, 4))
ts_month <- function(ts) as.integer(substr(as.character(ts), 5, 6))

# Join IGBP class onto any data frame with a site_id column.
# Returns df unchanged if snapshot_meta has no igbp column.
join_igbp <- function(df) {
  if (is.null(snapshot_meta) || !"igbp" %in% names(snapshot_meta)) return(df)
  dplyr::left_join(
    df,
    dplyr::distinct(snapshot_meta, .data$site_id, .data$igbp),
    by = "site_id"
  )
}

# ============================================================
# HTML document header and CSS
# ============================================================
html_head <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>FLUXNET Annual 2026 — Diagnostic Report</title>
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
  .tbl-wrap { overflow-x: auto; margin-bottom: 1rem; }
  table.dt { border-collapse: collapse; font-size: 12px; width: 100%; }
  table.dt th { background: #1a1a2e; color: #fff; padding: 5px 10px;
                text-align: left; white-space: nowrap; }
  table.dt td { padding: 4px 10px; border-bottom: 1px solid #e8e8e8; }
  table.dt tr:nth-child(even) td { background: #f7f7f7; }
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
  <h1>FLUXNET Annual 2026 — Diagnostic Report</h1>
  <p>Interactive report generated by 00_diagnostics.R &mdash;
     time-series/climatology/map charts: plotly &bull;
     QC/sensitivity charts: ggplot2/PNG</p>
</header>
<main>
'

# ============================================================
# Section 1 — Site inventory
# ============================================================
build_s1 <- function() {
  if (length(selected_sites) == 0) return(no_data())

  # Merge snapshot metadata onto selected sites
  # Use data from processed files to get temporal resolution
  res_info <- dplyr::bind_rows(lapply(names(site_data), function(res) {
    df <- site_data[[res]]$data
    if (nrow(df) == 0) return(NULL)
    yrs <- df |>
      dplyr::mutate(yr = ts_year(.data$TIMESTAMP)) |>
      dplyr::group_by(.data$site_id) |>
      dplyr::summarise(
        data_first_year = min(.data$yr, na.rm = TRUE),
        data_last_year  = max(.data$yr, na.rm = TRUE),
        n_records       = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(resolution = toupper(res))
    yrs
  }))

  # Site table
  if (!is.null(snapshot_meta)) {
    site_tbl <- snapshot_meta |>
      dplyr::filter(.data$site_id %in% selected_sites)
  } else {
    site_tbl <- data.frame(site_id = selected_sites)
  }

  # Add record counts from data
  if (nrow(res_info) > 0) {
    per_site <- res_info |>
      dplyr::group_by(.data$site_id) |>
      dplyr::summarise(
        resolutions = paste(sort(unique(.data$resolution)), collapse = "/"),
        data_years  = paste(min(.data$data_first_year), max(.data$data_last_year), sep = "–"),
        .groups = "drop"
      )
    site_tbl <- dplyr::left_join(site_tbl, per_site, by = "site_id")
  }

  tbl_html <- df_to_html_table(site_tbl)

  # Site map — plotly geo scatter coloured by IGBP (no external tile dependency)
  map_html <- if (!is.null(snapshot_meta) &&
                  "location_lat" %in% names(snapshot_meta)) {
    map_df <- dplyr::filter(snapshot_meta, .data$site_id %in% selected_sites)
    if (nrow(map_df) > 0) {
      hover_text <- paste0(
        "<b>", map_df$site_id, "</b>",
        if ("igbp"     %in% names(map_df)) paste0("<br>IGBP: ",     map_df$igbp)     else "",
        if ("data_hub" %in% names(map_df)) paste0("<br>Hub: ",      map_df$data_hub) else "",
        if ("first_year" %in% names(map_df) && "last_year" %in% names(map_df))
          paste0("<br>Years: ", map_df$first_year, "\u2013", map_df$last_year)
        else ""
      )
      # Assign IGBP colours to each point
      pt_colors <- ifelse(
        !is.na(map_df$igbp) & map_df$igbp %in% names(IGBP_PAL),
        IGBP_PAL[map_df$igbp],
        "#888888"
      )
      fig <- plotly::plot_geo(map_df,
                              lat = ~location_lat,
                              lon = ~location_long) |>
        plotly::add_markers(
          text       = hover_text,
          hoverinfo  = "text",
          marker     = list(size = 8, color = pt_colors,
                            line = list(color = "#1a1a2e", width = 0.8))
        ) |>
        plotly::layout(
          geo    = list(
            showland      = TRUE,  landcolor  = "#f0f0f0",
            showocean     = TRUE,  oceancolor = "#d0e8f0",
            showcountries = TRUE,  countrycolor = "#aaaaaa",
            showframe     = FALSE,
            projection    = list(type = "natural earth")
          ),
          margin = list(l = 0, r = 0, t = 0, b = 0)
        )
      plotly_div(fig, height = "420px")
    } else {
      no_data("No site coordinates available for selected sites.")
    }
  } else {
    no_data("Snapshot metadata not available — map skipped.")
  }

  paste0(map_html, tbl_html)
}

# ============================================================
# Section 2 — Data completeness
# ============================================================
build_s2 <- function() {
  parts <- list()

  for (res in c("yy", "mm", "dd")) {
    if (is.null(site_data[[res]])) next
    df <- site_data[[res]]$data
    qc_col <- "NEE_VUT_REF_QC"
    if (!qc_col %in% names(df)) next

    ts_col  <- "TIMESTAMP"
    res_up  <- toupper(res)

    # --- Stacked bar: fraction of records by QC quality bin ---
    bar_df <- df |>
      dplyr::select(site_id, qc = dplyr::all_of(qc_col)) |>
      dplyr::mutate(
        qc_bin = dplyr::case_when(
          .data$qc >= 0.75 ~ "≥0.75 (good)",
          .data$qc >= 0.50 ~ "0.50–0.75 (medium)",
          TRUE             ~ "<0.50 (low)"
        )
      ) |>
      dplyr::count(.data$site_id, .data$qc_bin) |>
      dplyr::group_by(.data$site_id) |>
      dplyr::mutate(frac = .data$n / sum(.data$n)) |>
      dplyr::ungroup()

    bar_df$qc_bin <- factor(
      bar_df$qc_bin,
      levels = c("≥0.75 (good)", "0.50–0.75 (medium)", "<0.50 (low)")
    )

    p_bar <- ggplot(bar_df,
                    aes(x = .data$site_id, y = .data$frac,
                        fill = .data$qc_bin)) +
      geom_col() +
      scale_fill_manual(
        values = c("≥0.75 (good)"        = "#2196F3",
                   "0.50–0.75 (medium)"  = "#FFC107",
                   "<0.50 (low)"         = "#F44336"),
        name = "NEE_VUT_REF_QC"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0(res_up, ": NEE_VUT_REF_QC distribution"),
           x = NULL, y = "Fraction of records") +
      diag_theme

    parts[[paste0(res, "_bar")]] <- paste0(
      '<h3>', res_up, ' — QC distribution</h3>',
      '<div class="plot-wrap">', plot_to_png(p_bar, 7, 3.5), "</div>"
    )

    # --- Heatmap timeline: resolution-aware ---
    if (res == "yy") {
      heat_df <- df |>
        dplyr::mutate(x_val = ts_year(.data[[ts_col]])) |>
        dplyr::group_by(.data$site_id, .data$x_val) |>
        dplyr::summarise(mean_qc = mean(.data[[qc_col]], na.rm = TRUE),
                         .groups = "drop")
      heat_xlab  <- "Year"
      heat_title <- paste0(res_up, ": NEE_VUT_REF_QC by site \u00d7 year")
    } else if (res == "mm") {
      heat_df <- df |>
        dplyr::mutate(x_val = ts_month(.data[[ts_col]])) |>
        dplyr::group_by(.data$site_id, .data$x_val) |>
        dplyr::summarise(mean_qc = mean(.data[[qc_col]], na.rm = TRUE),
                         .groups = "drop")
      heat_xlab  <- "Month (averaged across all years)"
      heat_title <- paste0(res_up, ": NEE_VUT_REF_QC by site \u00d7 month")
    } else {
      # DD: bin DOY into 14-day periods
      heat_df <- df |>
        dplyr::mutate(
          doy   = as.integer(format(
            as.Date(as.character(.data[[ts_col]]), format = "%Y%m%d"), "%j")),
          x_val = ((.data$doy - 1L) %/% 14L) * 14L + 1L
        ) |>
        dplyr::group_by(.data$site_id, .data$x_val) |>
        dplyr::summarise(mean_qc = mean(.data[[qc_col]], na.rm = TRUE),
                         .groups = "drop")
      heat_xlab  <- "Day of year (14-day bins, averaged across all years)"
      heat_title <- paste0(res_up, ": NEE_VUT_REF_QC by site \u00d7 DOY")
    }

    p_heat <- ggplot(heat_df,
                     aes(x = .data$x_val, y = .data$site_id,
                         fill = .data$mean_qc)) +
      geom_tile(colour = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low = "#F44336", mid = "#FFC107", high = "#2196F3",
        midpoint = 0.625, limits = c(0, 1),
        name = "Mean NEE_QC"
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(title = heat_title, x = heat_xlab, y = NULL) +
      diag_theme +
      theme(legend.position = "right")

    parts[[paste0(res, "_heat")]] <- paste0(
      '<h3>', res_up, ' — QC timeline</h3>',
      '<div class="plot-wrap">', plot_to_png(p_heat, 8, 2.5), "</div>"
    )
  }

  if (length(parts) == 0) return(no_data())
  paste(unlist(parts), collapse = "\n")
}

# ============================================================
# Section 3 — QC threshold sensitivity
# ============================================================
build_s3 <- function() {
  thresholds <- seq(0.5, 0.95, by = 0.05)
  plots <- list()

  for (res in c("yy", "mm", "dd")) {
    if (is.null(site_data[[res]])) next
    df    <- site_data[[res]]$data
    qcol  <- "NEE_VUT_REF_QC"
    if (!qcol %in% names(df)) next

    res_up <- toupper(res)

    # Count site-years surviving each threshold
    df2 <- df |>
      dplyr::mutate(yr = ts_year(.data$TIMESTAMP)) |>
      dplyr::select(site_id, yr, qc = dplyr::all_of(qcol))

    sens <- lapply(thresholds, function(thr) {
      surviving <- df2 |>
        dplyr::group_by(.data$site_id, .data$yr) |>
        dplyr::summarise(pass = all(.data$qc > thr, na.rm = TRUE), .groups = "drop") |>
        dplyr::filter(.data$pass) |>
        nrow()
      data.frame(threshold = thr, site_years = surviving)
    })
    sens_df <- do.call(rbind, sens)

    p <- ggplot(sens_df, aes(x = .data$threshold, y = .data$site_years)) +
      geom_line(colour = "#1a1a2e", linewidth = 1) +
      geom_point(colour = "#1a1a2e", size = 2) +
      geom_vline(xintercept = 0.75, linetype = "dashed",
                 colour = "#E91E63", linewidth = 0.8) +
      annotate("text", x = 0.76, y = max(sens_df$site_years) * 0.95,
               label = "default\n(0.75)", colour = "#E91E63",
               hjust = 0, size = 3) +
      scale_x_continuous(breaks = thresholds) +
      labs(title = paste0(res_up, ": site-years surviving QC threshold"),
           x = "NEE_VUT_REF_QC threshold", y = "Site-years") +
      diag_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    plots[[res]] <- paste0(
      '<h3>', res_up, "</h3>",
      '<div class="plot-wrap">', plot_to_png(p, 7, 3.5), "</div>"
    )
  }

  if (length(plots) == 0) return(no_data())
  paste(unlist(plots), collapse = "\n")
}

# ============================================================
# Section 4 — Annual time series (YY)
# ============================================================
build_s4 <- function() {
  if (is.null(site_data[["yy"]])) return(no_data("No YY data available."))
  df <- site_data[["yy"]]$data |>
    dplyr::mutate(yr = ts_year(.data$TIMESTAMP)) |>
    join_igbp()

  has_col <- function(col) col %in% names(df)

  # Colour by IGBP; group by site so each site draws its own line
  igbp_colour_scale <- scale_colour_manual(values = IGBP_PAL, name = "IGBP",
                                           na.value = "#888888")

  plots <- list()

  # NEE
  if (has_col("NEE_VUT_REF")) {
    p <- ggplot(df, aes(x = .data$yr, y = .data$NEE_VUT_REF,
                        colour = .data$igbp, group = .data$site_id)) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      igbp_colour_scale +
      labs(title = "Annual NEE — all sites (coloured by IGBP)",
           x = "Year", y = "NEE (gC m\u207b\u00b2 yr\u207b\u00b9)") +
      diag_theme
    plots[["nee"]] <- plotly_div(p)
  }

  # GPP — NT solid, DT dashed
  if (has_col("GPP_NT_VUT_REF") || has_col("GPP_DT_VUT_REF")) {
    gpp_long <- dplyr::bind_rows(
      if (has_col("GPP_NT_VUT_REF"))
        dplyr::transmute(df, site_id, igbp = .data$igbp, yr,
                         gpp = .data$GPP_NT_VUT_REF, type = "NT"),
      if (has_col("GPP_DT_VUT_REF"))
        dplyr::transmute(df, site_id, igbp = .data$igbp, yr,
                         gpp = .data$GPP_DT_VUT_REF, type = "DT")
    )
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
      labs(title = "Annual GPP (NT solid, DT dashed) — all sites (coloured by IGBP)",
           x = "Year", y = "GPP (gC m\u207b\u00b2 yr\u207b\u00b9)") +
      diag_theme
    plots[["gpp"]] <- plotly_div(p)
  }

  # ET from LE
  if (has_col("LE_F_MDS")) {
    p <- ggplot(df, aes(x = .data$yr, y = .data$LE_F_MDS,
                        colour = .data$igbp, group = .data$site_id)) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      igbp_colour_scale +
      labs(title = "Annual ET (from LE_F_MDS) — all sites (coloured by IGBP)",
           x = "Year", y = "ET (mm yr\u207b\u00b9)") +
      diag_theme
    plots[["et"]] <- plotly_div(p)
  }

  if (length(plots) == 0) return(no_data("Required columns not found in YY data."))
  paste(unlist(plots), collapse = "\n")
}

# ============================================================
# Section 5 — Annual climatology (MM)
# ============================================================
build_s5 <- function() {
  if (is.null(site_data[["mm"]])) return(no_data("No MM data available."))
  df <- site_data[["mm"]]$data |>
    dplyr::mutate(month = ts_month(.data$TIMESTAMP)) |>
    join_igbp()

  # Mean over all years per site × month, carry igbp through
  clim <- df |>
    dplyr::group_by(.data$site_id, .data$igbp, .data$month) |>
    dplyr::summarise(
      NEE  = mean(.data$NEE_VUT_REF,  na.rm = TRUE),
      GPP_NT = if ("GPP_NT_VUT_REF" %in% names(df))
                 mean(.data$GPP_NT_VUT_REF, na.rm = TRUE) else NA_real_,
      GPP_DT = if ("GPP_DT_VUT_REF" %in% names(df))
                 mean(.data$GPP_DT_VUT_REF, na.rm = TRUE) else NA_real_,
      ET   = if ("LE_F_MDS" %in% names(df))
               mean(.data$LE_F_MDS, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )

  mo_labs <- c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")

  igbp_colour_scale <- scale_colour_manual(values = IGBP_PAL, name = "IGBP",
                                           na.value = "#888888")

  make_panel <- function(aes_y, title, ylab) {
    p <- ggplot(clim, aes(x = .data$month, y = {{ aes_y }},
                          colour = .data$igbp, group = .data$site_id)) +
      geom_line(linewidth = 0.6, alpha = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      igbp_colour_scale +
      scale_x_continuous(breaks = 1:12, labels = mo_labs) +
      labs(title = title, x = "Month", y = ylab) +
      diag_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plotly_div(p)
  }

  plots <- list()
  plots[["nee"]] <- make_panel(.data$NEE,
                               "Monthly climatology — NEE (all sites, coloured by IGBP)",
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
      labs(title = "Monthly climatology — GPP (NT solid, DT dashed) — all sites (IGBP colour)",
           x = "Month", y = "GPP (gC m\u207b\u00b2 month\u207b\u00b9)") +
      diag_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots[["gpp"]] <- plotly_div(p_gpp)
  }

  if (!all(is.na(clim$ET))) {
    plots[["et"]] <- make_panel(.data$ET,
                                "Monthly climatology — ET (all sites, coloured by IGBP)",
                                "ET (mm month\u207b\u00b9)")
  }

  paste(unlist(plots), collapse = "\n")
}

# ============================================================
# Section 6 — Daily climatology (DD)
# ============================================================
build_s6 <- function() {
  if (is.null(site_data[["dd"]])) return(no_data("No DD data available."))

  # Select only columns needed for climatology plots before join — the full DD
  # data frame is ~1.9 GB (576 cols) and manipulating it whole exhausts memory.
  flux_cols <- intersect(
    c("site_id", "TIMESTAMP", "DOY",
      "NEE_VUT_REF", "GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "LE_F_MDS"),
    names(site_data[["dd"]]$data)
  )
  df <- site_data[["dd"]]$data |>
    dplyr::select(dplyr::all_of(flux_cols)) |>
    join_igbp()

  # Use DOY column if present, otherwise derive from TIMESTAMP
  if (!"DOY" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        DOY = as.integer(format(as.Date(as.character(.data$TIMESTAMP),
                                        format = "%Y%m%d"), "%j"))
      )
  }

  igbp_colour_scale <- scale_colour_manual(values = IGBP_PAL, name = "IGBP",
                                           na.value = "#888888")

  # Pre-aggregate to mean DOY climatology per site (across all years).
  # This avoids passing millions of raw daily points to plotly/loess.
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
      diag_theme
    paste0('<div class="plot-wrap">', plot_to_png(p, 9, 4.5), "</div>")
  }

  plots <- Filter(Negate(is.null), list(
    nee = make_panel_dd("NEE_VUT_REF",
                        "Daily climatology — NEE, all sites — mean by DOY (IGBP colour)",
                        "NEE (gC m\u207b\u00b2 day\u207b\u00b9)"),
    gpp = {
      # NT and DT on same panel; pre-aggregate each separately then bind
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
          labs(title = "Daily climatology — GPP, all sites — mean by DOY (IGBP colour, NT solid, DT dashed)",
               x = "Day of year", y = "GPP (gC m\u207b\u00b2 day\u207b\u00b9)") +
          diag_theme
        paste0('<div class="plot-wrap">', plot_to_png(p, 9, 4.5), "</div>")
      }
    },
    et  = make_panel_dd("LE_F_MDS",
                        "Daily climatology — ET, all sites — mean by DOY (IGBP colour)",
                        "ET (mm day\u207b\u00b9)")
  ))

  if (length(plots) == 0) return(no_data("Required columns not found in DD data."))
  paste(unlist(plots), collapse = "\n")
}

# ============================================================
# Section 7 — Unit conversion check
# ============================================================
build_s7 <- function() {
  if (is.null(site_data[["yy"]])) return(no_data("No YY data available."))
  df <- site_data[["yy"]]$data

  native_col    <- "NEE_VUT_REF_native"
  converted_col <- "NEE_VUT_REF"

  if (!converted_col %in% names(df)) return(no_data("NEE_VUT_REF not found."))

  tbl <- df |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(
      n_years                        = dplyr::n(),
      mean_NEE_native_umol_m2_s1     =
        if (native_col %in% names(df))
          round(mean(.data[[native_col]], na.rm = TRUE), 4)
        else NA_real_,
      mean_NEE_converted_gC_m2_yr1   =
        round(mean(.data[[converted_col]], na.rm = TRUE), 1),
      .groups = "drop"
    )

  note <- '<p class="note">Expected range for vegetated sites:
  roughly &minus;200 to +50 gC m\u207b\u00b2 yr\u207b\u00b9.
  Native column present only when 05_units.R has been run.</p>'

  paste0(df_to_html_table(tbl), note)
}

# ============================================================
# Section 8 — Exclusion log summary
# ============================================================
build_s8 <- function() {
  excl_path    <- file.path("outputs", "exclusion_log.csv")
  unknown_path <- file.path("outputs", "unknown_log.csv")
  parts <- list()

  if (file.exists(excl_path)) {
    excl <- readr::read_csv(excl_path, show_col_types = FALSE)
    if (nrow(excl) > 0) {
      summary_tbl <- excl |>
        dplyr::count(.data$reason, .data$excluded_by, name = "n_records") |>
        dplyr::arrange(dplyr::desc(.data$n_records))
      parts[["excl"]] <- paste0(
        "<h3>Exclusion log (", nrow(excl), " total records excluded)</h3>",
        df_to_html_table(summary_tbl)
      )
    } else {
      parts[["excl"]] <- '<p class="note">exclusion_log.csv is empty — no records excluded.</p>'
    }
  } else {
    parts[["excl"]] <- no_data("outputs/exclusion_log.csv not found. Run 04_qc.R first.")
  }

  if (file.exists(unknown_path)) {
    n_unk <- nrow(readr::read_csv(unknown_path, show_col_types = FALSE))
    parts[["unk"]] <- paste0(
      '<p class="note">Unknown log: ', n_unk, ' entry/entries in outputs/unknown_log.csv.</p>'
    )
  } else {
    parts[["unk"]] <- '<p class="note">outputs/unknown_log.csv not found.</p>'
  }

  paste(unlist(parts), collapse = "\n")
}

# ============================================================
# Assemble the report
# ============================================================
message("Building Section 1 — Site inventory ...")
s1 <- section(1, "Site inventory",                  build_s1())
message("Building Section 2 — Data completeness ...")
s2 <- section(2, "Data completeness",               build_s2())
message("Building Section 3 — QC threshold sensitivity ...")
s3 <- section(3, "QC threshold sensitivity",        build_s3())
message("Building Section 4 — Annual time series (YY) ...")
s4 <- section(4, "Annual time series (YY data)",    build_s4())
message("Building Section 5 — Annual climatology (MM) ...")
s5 <- section(5, "Annual climatology (MM data)",    build_s5())
message("Building Section 6 — Daily climatology (DD) ...")
s6 <- section(6, "Daily climatology (DD data)",     build_s6())
message("Building Section 7 — Unit conversion check ...")
s7 <- section(7, "Unit conversion check",           build_s7())
message("Building Section 8 — Exclusion log summary ...")
s8 <- section(8, "Exclusion log summary",           build_s8())

html_footer <- paste0(
  '</main>\n<footer>\n<dl>\n',
  "<dt>Run datetime UTC</dt><dd>", RUN_DATETIME, "</dd>\n",
  "<dt>Git hash</dt><dd>",         GIT_HASH,     "</dd>\n",
  "<dt>Sites included</dt><dd>",   length(selected_sites), " sites", "</dd>\n",
  "<dt>Site IDs</dt><dd>",
  if (length(selected_sites) > 0) paste(sort(selected_sites), collapse = ", ")
  else "(none)",
  "</dd>\n",
  "<dt>FLUXNET_EXTRACT_RESOLUTIONS</dt><dd>",
  paste(FLUXNET_EXTRACT_RESOLUTIONS, collapse = " "), "</dd>\n",
  "<dt>Resolutions in report</dt><dd>",
  if (length(avail) > 0) paste(toupper(names(avail)), collapse = ", ")
  else "(none)",
  "</dd>\n",
  "</dl>\n</footer>\n</body>\n</html>"
)

out_html <- paste0(
  html_head,
  s1, s2, s3, s4, s5, s6, s7, s8,
  html_footer
)

out_path <- file.path("docs", "diagnostics.html")
writeLines(out_html, out_path, useBytes = FALSE)
message("Report written: ", out_path,
        " (", round(file.size(out_path) / 1024), " KB)")

# Write companion metadata per CLAUDE.md output spec
write_output_metadata(
  out_path,
  input_sources = c(
    if (length(snapshot_csv) > 0) snapshot_csv[[1]] else character(0),
    unlist(lapply(names(avail),
                  function(s) file.path(processed_dir,
                                        paste0("flux_data_", avail[[s]]$stage,
                                               "_", s, ".rds"))))
  ),
  notes = paste0("Diagnostic report. All sites included (n=", length(selected_sites),
                 "): ", paste(sort(selected_sites), collapse = ", "), ".")
)

# Session info
writeLines(capture.output(sessionInfo()),
           file.path("outputs", "session_info.txt"))
message("Done.")
