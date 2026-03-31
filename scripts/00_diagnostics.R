## 00_diagnostics.R — Self-contained HTML diagnostic report
##
## Produces outputs/diagnostics.html from data in data/processed/.
## Selects up to 3 sites at random; seed is recorded in the report footer.
##
## Package requirements (all in renv.lock): ggplot2, dplyr, tidyr, readr,
## lubridate, scales, grDevices, jsonlite.
## Does NOT require rmarkdown, plotly, or leaflet — all plots are inline SVG.

source("R/pipeline_config.R")
source("R/utils.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)
library(scales)

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

check_pipeline_config()

if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)

# ---- Reproducibility ----
DIAG_SEED    <- as.integer(Sys.time()) %% 99991L
set.seed(DIAG_SEED)
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

selected_sites <- if (length(all_sites) > 3) sample(all_sites, 3) else all_sites

# Filter all data to selected sites
site_data <- lapply(avail, function(x) {
  x$data <- dplyr::filter(x$data, .data$site_id %in% selected_sites)
  x
})

# Site colour palette — defined once, reused in every plot
SITE_PAL <- setNames(
  scales::hue_pal()(max(length(selected_sites), 1)),
  selected_sites
)

# ============================================================
# Helper functions
# ============================================================

# Render a ggplot to an inline SVG string.
plot_to_svg <- function(p, width = 8, height = 4.5) {
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::svg(tmp, width = width, height = height, bg = "white")
  print(p)
  grDevices::dev.off()
  # Strip the XML declaration so the SVG nests cleanly inside HTML5
  gsub("^<\\?xml[^\n]*\n?", "",
       paste(readLines(tmp, warn = FALSE), collapse = "\n"))
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

# ============================================================
# HTML document header and CSS
# ============================================================
html_head <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>FLUXNET Annual 2026 — Diagnostic Report</title>
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
  <p>Static report generated by 00_diagnostics.R &mdash; all plots are ggplot2/SVG
     (rmarkdown/plotly/leaflet not in renv.lock)</p>
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

  # Map: lat/lon scatter coloured by IGBP
  map_plot <- NULL
  if (!is.null(snapshot_meta)) {
    map_df <- dplyr::filter(snapshot_meta, .data$site_id %in% selected_sites)
    if (nrow(map_df) > 0 && "location_lat" %in% names(map_df)) {
      map_df$igbp <- if ("igbp" %in% names(map_df)) map_df$igbp else "Unknown"
      map_plot <- ggplot(map_df,
                         aes(x = .data$location_long,
                             y = .data$location_lat,
                             colour = .data$igbp,
                             label  = .data$site_id)) +
        geom_point(size = 4) +
        ggplot2::geom_text(hjust = -0.2, size = 3.5, show.legend = FALSE) +
        labs(x = "Longitude", y = "Latitude", colour = "IGBP",
             title = "Site locations (lat/lon; no background map — leaflet not in renv.lock)") +
        coord_fixed(1.3, xlim = range(map_df$location_long) + c(-15, 15),
                    ylim = range(map_df$location_lat)  + c(-10, 10)) +
        diag_theme
    }
  }

  map_html <- if (!is.null(map_plot)) {
    paste0('<div class="plot-wrap">', plot_to_svg(map_plot, 8, 4), "</div>")
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
      '<div class="plot-wrap">', plot_to_svg(p_bar, 7, 3.5), "</div>"
    )

    # --- Heatmap timeline: year × site, colour = mean QC ---
    heat_df <- df |>
      dplyr::mutate(yr = ts_year(.data[[ts_col]])) |>
      dplyr::group_by(.data$site_id, .data$yr) |>
      dplyr::summarise(mean_qc = mean(.data[[qc_col]], na.rm = TRUE),
                       .groups = "drop")

    p_heat <- ggplot(heat_df,
                     aes(x = .data$yr, y = .data$site_id,
                         fill = .data$mean_qc)) +
      geom_tile(colour = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low = "#F44336", mid = "#FFC107", high = "#2196F3",
        midpoint = 0.625, limits = c(0, 1),
        name = "Mean NEE_QC"
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(title = paste0(res_up, ": NEE_VUT_REF_QC by site-year"),
           x = "Year", y = NULL) +
      diag_theme +
      theme(legend.position = "right")

    parts[[paste0(res, "_heat")]] <- paste0(
      '<h3>', res_up, ' — QC timeline</h3>',
      '<div class="plot-wrap">', plot_to_svg(p_heat, 8, 2.5), "</div>"
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
      '<div class="plot-wrap">', plot_to_svg(p, 7, 3.5), "</div>"
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
  df <- site_data[["yy"]]$data

  # Year column
  df <- dplyr::mutate(df, yr = ts_year(.data$TIMESTAMP))

  has_col <- function(col) col %in% names(df)

  plots <- list()

  # NEE
  if (has_col("NEE_VUT_REF")) {
    p <- ggplot(df, aes(x = .data$yr, y = .data$NEE_VUT_REF,
                        colour = .data$site_id, group = .data$site_id)) +
      geom_line(linewidth = 1) + geom_point(size = 2.5) +
      scale_colour_manual(values = SITE_PAL, name = NULL) +
      labs(title = "Annual NEE", x = "Year",
           y = "NEE (gC m\u207b\u00b2 yr\u207b\u00b9)") +
      diag_theme
    plots[["nee"]] <- paste0('<div class="plot-wrap">',
                             plot_to_svg(p, 8, 3.5), "</div>")
  }

  # GPP — NT solid, DT dashed
  if (has_col("GPP_NT_VUT_REF") || has_col("GPP_DT_VUT_REF")) {
    gpp_long <- dplyr::bind_rows(
      if (has_col("GPP_NT_VUT_REF"))
        dplyr::transmute(df, site_id, yr,
                         gpp = .data$GPP_NT_VUT_REF, type = "NT"),
      if (has_col("GPP_DT_VUT_REF"))
        dplyr::transmute(df, site_id, yr,
                         gpp = .data$GPP_DT_VUT_REF, type = "DT")
    )
    p <- ggplot(gpp_long,
                aes(x = .data$yr, y = .data$gpp,
                    colour = .data$site_id,
                    linetype = .data$type,
                    group = interaction(.data$site_id, .data$type))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_colour_manual(values = SITE_PAL, name = NULL) +
      scale_linetype_manual(values = c(NT = "solid", DT = "dashed"),
                            name = "Partitioning") +
      labs(title = "Annual GPP (NT solid, DT dashed)",
           x = "Year", y = "GPP (gC m\u207b\u00b2 yr\u207b\u00b9)") +
      diag_theme
    plots[["gpp"]] <- paste0('<div class="plot-wrap">',
                             plot_to_svg(p, 8, 3.5), "</div>")
  }

  # ET from LE
  if (has_col("LE_F_MDS")) {
    p <- ggplot(df, aes(x = .data$yr, y = .data$LE_F_MDS,
                        colour = .data$site_id, group = .data$site_id)) +
      geom_line(linewidth = 1) + geom_point(size = 2.5) +
      scale_colour_manual(values = SITE_PAL, name = NULL) +
      labs(title = "Annual ET (from LE_F_MDS)",
           x = "Year", y = "ET (mm yr\u207b\u00b9)") +
      diag_theme
    plots[["et"]] <- paste0('<div class="plot-wrap">',
                            plot_to_svg(p, 8, 3.5), "</div>")
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
    dplyr::mutate(month = ts_month(.data$TIMESTAMP))

  # Mean over all years per site × month
  clim <- df |>
    dplyr::group_by(.data$site_id, .data$month) |>
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

  make_panel <- function(aes_y, title, ylab) {
    p <- ggplot(clim, aes(x = .data$month, y = {{ aes_y }},
                          colour = .data$site_id, group = .data$site_id)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_colour_manual(values = SITE_PAL, name = NULL) +
      scale_x_continuous(breaks = 1:12, labels = mo_labs) +
      labs(title = title, x = "Month", y = ylab) +
      diag_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    paste0('<div class="plot-wrap">', plot_to_svg(p, 8, 3.5), "</div>")
  }

  plots <- list()
  plots[["nee"]] <- make_panel(.data$NEE, "Monthly climatology — NEE",
                               "NEE (gC m\u207b\u00b2 month\u207b\u00b9)")

  if (!all(is.na(clim$GPP_NT)) || !all(is.na(clim$GPP_DT))) {
    gpp_long <- dplyr::bind_rows(
      dplyr::transmute(clim, site_id, month,
                       gpp = .data$GPP_NT, type = "NT"),
      dplyr::transmute(clim, site_id, month,
                       gpp = .data$GPP_DT, type = "DT")
    ) |> dplyr::filter(!is.na(.data$gpp))

    p_gpp <- ggplot(gpp_long,
                    aes(x = .data$month, y = .data$gpp,
                        colour = .data$site_id,
                        linetype = .data$type,
                        group = interaction(.data$site_id, .data$type))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_colour_manual(values = SITE_PAL, name = NULL) +
      scale_linetype_manual(values = c(NT = "solid", DT = "dashed"),
                            name = "Partitioning") +
      scale_x_continuous(breaks = 1:12, labels = mo_labs) +
      labs(title = "Monthly climatology — GPP (NT solid, DT dashed)",
           x = "Month", y = "GPP (gC m\u207b\u00b2 month\u207b\u00b9)") +
      diag_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots[["gpp"]] <- paste0('<div class="plot-wrap">',
                             plot_to_svg(p_gpp, 8, 3.5), "</div>")
  }

  if (!all(is.na(clim$ET))) {
    plots[["et"]] <- make_panel(.data$ET, "Monthly climatology — ET",
                                "ET (mm month\u207b\u00b9)")
  }

  paste(unlist(plots), collapse = "\n")
}

# ============================================================
# Section 6 — Daily climatology (DD)
# ============================================================
build_s6 <- function() {
  if (is.null(site_data[["dd"]])) return(no_data("No DD data available."))
  df <- site_data[["dd"]]$data

  # Use DOY column if present, otherwise derive from TIMESTAMP
  if (!"DOY" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        DOY = as.integer(format(as.Date(as.character(.data$TIMESTAMP),
                                        format = "%Y%m%d"), "%j"))
      )
  }

  make_panel_dd <- function(y_col, title, ylab) {
    if (!y_col %in% names(df)) return(NULL)
    sub <- dplyr::select(df, site_id, DOY, y = dplyr::all_of(y_col)) |>
      dplyr::filter(!is.na(.data$y))
    p <- ggplot(sub, aes(x = .data$DOY, y = .data$y, colour = .data$site_id)) +
      geom_point(size = 0.5, alpha = 0.4) +
      geom_smooth(aes(group = .data$site_id), method = "loess", span = 0.3,
                  se = FALSE, linewidth = 1) +
      scale_colour_manual(values = SITE_PAL, name = NULL) +
      labs(title = title, x = "Day of year", y = ylab) +
      diag_theme
    paste0('<div class="plot-wrap">', plot_to_svg(p, 8, 4), "</div>")
  }

  plots <- Filter(Negate(is.null), list(
    nee = make_panel_dd("NEE_VUT_REF",
                        "Daily climatology — NEE (raw points + loess span=0.3)",
                        "NEE (gC m\u207b\u00b2 day\u207b\u00b9)"),
    gpp = {
      # NT and DT on same panel
      if (!any(c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF") %in% names(df))) {
        NULL
      } else {
        gpp_dd <- dplyr::bind_rows(
          if ("GPP_NT_VUT_REF" %in% names(df))
            dplyr::transmute(df, site_id, DOY,
                             gpp = .data$GPP_NT_VUT_REF, type = "NT"),
          if ("GPP_DT_VUT_REF" %in% names(df))
            dplyr::transmute(df, site_id, DOY,
                             gpp = .data$GPP_DT_VUT_REF, type = "DT")
        ) |> dplyr::filter(!is.na(.data$gpp))
        p <- ggplot(gpp_dd,
                    aes(x = .data$DOY, y = .data$gpp,
                        colour = .data$site_id)) +
          geom_point(aes(shape = .data$type), size = 0.5, alpha = 0.3) +
          geom_smooth(aes(group = interaction(.data$site_id, .data$type),
                          linetype = .data$type),
                      method = "loess", span = 0.3, se = FALSE, linewidth = 1) +
          scale_colour_manual(values = SITE_PAL, name = NULL) +
          scale_linetype_manual(values = c(NT = "solid", DT = "dashed"),
                                name = "Partitioning") +
          labs(title = "Daily climatology — GPP (NT solid, DT dashed, loess span=0.3)",
               x = "Day of year", y = "GPP (gC m\u207b\u00b2 day\u207b\u00b9)") +
          diag_theme
        paste0('<div class="plot-wrap">', plot_to_svg(p, 8, 4), "</div>")
      }
    },
    et  = make_panel_dd("LE_F_MDS",
                        "Daily climatology — ET (loess span=0.3)",
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
  "<dt>Sites included</dt><dd>",
  if (length(selected_sites) > 0) paste(selected_sites, collapse = ", ")
  else "(none)",
  "</dd>\n",
  "<dt>Random seed</dt><dd>",      DIAG_SEED,    "</dd>\n",
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

out_path <- file.path("outputs", "diagnostics.html")
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
  notes = paste0("Diagnostic report. Random seed: ", DIAG_SEED,
                 ". Sites: ", paste(selected_sites, collapse = ", "), ".")
)

# Session info
writeLines(capture.output(sessionInfo()),
           file.path("outputs", "session_info.txt"))
message("Done.")
