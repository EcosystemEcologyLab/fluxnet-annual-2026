## generate_fig02alt_cumulative_siteyears_record_length_inset.R
##
## CANDIDATE figure, not a replacement for Figure 2 or Supplementary Figure
## S6 -- see logs/prompts/2026-10-12_fig02_inset.md. Combines the current
## Figure 2 (fig_cumulative_siteyears_igbp(), R/figures/fig_network_growth.R)
## as the main panel with the S6 record-length histogram
## (fig_record_length_histogram(), same file -- extracted 2026-10-12 from
## scripts/supp_stage2_record_length_collections_figure.R) as an inset placed
## in the empty upper-left region of the main panel (left of the 2000 dashed
## reference line, above the Marconi/La Thuile historical labels -- the only
## region that touches no stacked area, historical line, historical label, or
## dashed reference line). One shared IGBP legend (both panels use the same
## PAPER_IGBP_ORDER/PAPER_IGBP_COLOURS palette) sits to the inset's right,
## still inside the main panel.
##
## Inputs: the latest snapshot, fluxnet_shuttle_snapshot_20260920T102211.csv
## (not scripts/generate_fig_cumulative_siteyears.R's or supp_stage2's own
## pinned, older snapshots -- see docs/decisions_pending.md 2026-10-12: the
## latest snapshot is the default for new work unless a specific, documented
## reason pins an older one).
##
## Output: review/figures/candidates/fig_02alt_cumulative_siteyears_record_length_inset*
## at 89x89mm and 120x120mm (PNG 600dpi + PDF), each with .legend.txt and
## .meta.json. Gates (stop, do not save, on any mismatch): main panel total
## 6,200 site-years; inset 781 sites; threshold counts 483 / 225 / 63.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")
source("R/figures/fig_network_growth.R")
source("R/nature_format.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(readxl); library(tidyr)
  library(patchwork); library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Candidate: Figure 2 + record-length inset ===")

OUT_DIR  <- "review/figures/candidates"
OUT_STEM <- file.path(OUT_DIR, "fig_02alt_cumulative_siteyears_record_length_inset")
fs::dir_create(OUT_DIR)

SNAP_FILE <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
if (!file.exists(SNAP_FILE)) stop("Latest snapshot not found: ", SNAP_FILE, call. = FALSE)
msg("Using snapshot (latest): ", SNAP_FILE)
shuttle_meta <- read_csv(SNAP_FILE, show_col_types = FALSE)
if (dplyr::n_distinct(shuttle_meta$site_id) != 781L) {
  stop("Expected 781 sites in ", SNAP_FILE, ", got ", dplyr::n_distinct(shuttle_meta$site_id), call. = FALSE)
}

sites_marconi     <- read_csv("data/snapshots/sites_marconi_clean.csv", show_col_types = FALSE) |>
  left_join(read_csv("data/snapshots/years_marconi.csv", show_col_types = FALSE), by = "site_id")
sites_la_thuile   <- read_csv("data/snapshots/sites_la_thuile_clean.csv", show_col_types = FALSE) |>
  left_join(read_csv("data/snapshots/years_la_thuile.csv", show_col_types = FALSE), by = "site_id")
sites_fluxnet2015 <- read_csv("data/snapshots/sites_fluxnet2015_clean.csv", show_col_types = FALSE) |>
  left_join(read_csv("data/snapshots/years_fluxnet2015.csv", show_col_types = FALSE), by = "site_id")

presence_df <- read_csv(file.path(FLUXNET_DATA_ROOT, "snapshots", "site_year_data_presence.csv"),
                         show_col_types = FALSE) |>
  mutate(year = as.integer(.data$year), has_data = as.logical(.data$has_data))
msg("Loaded presence_df: ", nrow(presence_df), " rows")

## ---- La Thuile actual year-indicator presence (not first-to-last span) -----
## Identical to scripts/generate_fig_cumulative_siteyears.R -- see its own
## comment for why (correct total 965, not the span-based 1008).
la_thuile_xlsx        <- read_excel("data/lists/LaThuileList.xlsx")
la_thuile_yr_cols     <- names(la_thuile_xlsx)[grepl("^[0-9]{4}$", names(la_thuile_xlsx))]
la_thuile_year_matrix <- la_thuile_xlsx |>
  dplyr::select(site_id = "SITE", dplyr::all_of(la_thuile_yr_cols)) |>
  tidyr::pivot_longer(cols = dplyr::all_of(la_thuile_yr_cols), names_to = "year", values_to = "flag") |>
  dplyr::mutate(year = as.integer(.data$year), present = !is.na(.data$flag) & .data$flag == 1)
msg("Loaded La Thuile year-indicator matrix: ",
    sum(la_thuile_year_matrix$present), " site-years (expect 965)")

## =============================================================================
## Gate checks -- stop, do not save, on any mismatch
## =============================================================================
window <- data_year_window(presence_df)
siteyears_plotted <- presence_df |>
  dplyr::filter(has_data, year >= window$first_year, year <= window$last_year) |>
  dplyr::left_join(
    shuttle_meta |> dplyr::distinct(site_id, .keep_all = TRUE) |> dplyr::select(site_id, igbp),
    by = "site_id"
  ) |>
  dplyr::filter(!is.na(igbp), igbp %in% PAPER_IGBP_ORDER)
main_total_siteyears <- nrow(siteyears_plotted)
msg("Main panel total site-years (", window$first_year, "-", window$last_year, "): ", main_total_siteyears)

record_length <- fig_record_length_histogram(
  site_meta           = shuttle_meta,
  presence_df         = presence_df,
  thresholds          = c(5L, 10L, 20L),
  base_size           = NATURE_SMALL_PT,
  show_legend         = FALSE,
  show_thresh_labels  = FALSE,
  x_label             = NULL,
  y_label             = "Sites"
)
msg("Inset sites: ", record_length$n_sites,
    "; threshold counts (>=5,>=10,>=20): ", paste(record_length$thresh_counts, collapse = ", "))

gate_issues <- character(0)
if (main_total_siteyears != 6200L) {
  gate_issues <- c(gate_issues, paste0("main panel total site-years = ", main_total_siteyears, ", expected 6200"))
}
if (record_length$n_sites != 781L) {
  gate_issues <- c(gate_issues, paste0("inset n_sites = ", record_length$n_sites, ", expected 781"))
}
if (!identical(as.integer(record_length$thresh_counts), c(483L, 225L, 63L))) {
  gate_issues <- c(gate_issues, paste0("inset threshold counts = ",
                                        paste(record_length$thresh_counts, collapse = ", "),
                                        ", expected 483, 225, 63"))
}
if (length(gate_issues) > 0L) {
  stop("Gate check FAILED -- not saving any output:\n  - ", paste(gate_issues, collapse = "\n  - "), call. = FALSE)
}
msg("Gate checks PASSED: 6200 main-panel site-years; 781 inset sites; thresholds 483/225/63.")

## =============================================================================
## Build panels
## =============================================================================

## ---- Main panel -------------------------------------------------------------
## Same call as scripts/generate_fig_cumulative_siteyears.R, but its own
## internal "inside" legend is repositioned (not removed) to sit to the right
## of the inset -- one shared legend for both panels, since both use the same
## PAPER_IGBP_ORDER/PAPER_IGBP_COLOURS palette (fig1b_igbp_colours inside
## fig_cumulative_siteyears_igbp() IS PAPER_IGBP_COLOURS; fig_record_length_
## histogram()'s scale_fill_paper_igbp() draws from the same constant).
##
## Layout constants (npc, align_to = "panel" i.e. relative to the plot panel
## area only, not axis text/titles): the 2000 dashed reference line sits at
## npc x ~= 0.269 given the data-driven year window below (1991-2025, 1%
## scale expansion each side) -- the inset and legend must stay clear of it
## (and of 2007/2015, further right) so no dashed line, stacked area,
## historical line, or historical label is covered. INSET_RIGHT is kept
## below that line; the legend, which is text/colour chips with far more
## negative space than the inset's opaque panel background, is allowed to
## extend a little past it, the same way the existing "FLUXNET2015" label
## already crosses the 2015 line in the unmodified Figure 2.
INSET_LEFT   <- 0.02
INSET_BOTTOM <- 0.50
INSET_RIGHT  <- 0.23
INSET_TOP    <- 0.97
LEGEND_X     <- 0.30
LEGEND_Y     <- 0.97

main_panel <- fig_cumulative_siteyears_igbp(
  presence_df           = presence_df,
  shuttle_meta          = shuttle_meta,
  sites_marconi         = sites_marconi,
  sites_la_thuile       = sites_la_thuile,
  sites_fluxnet2015     = sites_fluxnet2015,
  base_size             = 9L,
  la_thuile_year_matrix = la_thuile_year_matrix
) +
  nature_theme(base_size = NATURE_BASE_PT) +
  ggplot2::theme(
    legend.position.inside = c(LEGEND_X, LEGEND_Y),
    legend.justification   = c(0, 1),
    legend.key.size        = grid::unit(1.8, "mm"),
    legend.text            = ggplot2::element_text(size = NATURE_SMALL_PT, family = NATURE_FONT, colour = "grey10"),
    legend.title           = ggplot2::element_text(size = NATURE_SMALL_PT, family = NATURE_FONT, colour = "grey10"),
    legend.spacing.y       = grid::unit(0, "pt"),
    plot.margin             = ggplot2::margin(t = 5.5, r = 14, b = 5.5, l = 5.5, unit = "pt")
  )

## ---- Inset: record-length histogram (no panel letter, no own legend) -------
## Built per output size, not once: the inset's npc footprint (INSET_LEFT/
## RIGHT/BOTTOM/TOP) is a fixed fraction of the main panel, so its physical
## size scales up with the figure -- at 120mm there is enough room to try the
## main panel's own 7pt (NATURE_BASE_PT) instead of the 5pt floor
## (NATURE_SMALL_PT) needed at 89mm. Checked by rendering and inspecting both
## (see run report) -- never go below the 5pt floor either way.
build_inset_panel <- function(base_size) {
  rl <- fig_record_length_histogram(
    site_meta           = shuttle_meta,
    presence_df         = presence_df,
    thresholds          = c(5L, 10L, 20L),
    base_size           = base_size,
    show_legend         = FALSE,
    show_thresh_labels  = FALSE,
    x_label             = NULL,
    y_label             = "Sites"
  )
  rl$plot +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", colour = "grey40",
                                                 linewidth = nature_lwd(NATURE_LINEWIDTH_MIN)),
      plot.margin      = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "pt"),
      axis.title       = ggplot2::element_text(size = base_size),
      axis.text        = ggplot2::element_text(size = base_size)
    )
}

build_combined <- function(inset_base_size) {
  inset_panel <- build_inset_panel(inset_base_size)
  main_panel + patchwork::inset_element(
    inset_panel,
    left = INSET_LEFT, bottom = INSET_BOTTOM, right = INSET_RIGHT, top = INSET_TOP,
    align_to = "panel"
  )
}

## ---- Local save helper (not save_nature_figure(): that function asserts
## main-text widths of exactly 89 or 183mm unless extended_data = TRUE, which
## also forces a JPEG this candidate does not need. Same PNG/PDF mechanics --
## ragg::agg_png at 600dpi, base grDevices::pdf with Helvetica, white bg.) ---
save_candidate <- function(plot, path_stem, width_mm, height_mm) {
  png_path <- paste0(path_stem, ".png")
  pdf_path <- paste0(path_stem, ".pdf")
  ggplot2::ggsave(png_path, plot, width = width_mm, height = height_mm, units = "mm",
                   dpi = 600, device = ragg::agg_png, bg = "white")
  ggplot2::ggsave(pdf_path, plot, width = width_mm, height = height_mm, units = "mm",
                   device = grDevices::pdf, family = "Helvetica", bg = "white")
  invisible(list(png = png_path, pdf = pdf_path))
}

SIZES <- list(
  sq89  = list(mm = 89,  stem = paste0(OUT_STEM, "_89mm"),  inset_base_size = NATURE_SMALL_PT),
  sq120 = list(mm = 120, stem = paste0(OUT_STEM, "_120mm"), inset_base_size = NATURE_BASE_PT)
)

legend_text_common <- paste0(
  "Candidate figure -- not a replacement for Figure 2 (fig_02_cumulative_siteyears_igbp) or\n",
  "Supplementary Figure S6 (figS6_record_length). Combines both as a single panel: the current\n",
  "Figure 2 cumulative site-years chart as the main panel, with the S6 record-length histogram\n",
  "as an inset in the main panel's empty upper-left region (left of the 2000 historical-release\n",
  "reference line, above the Marconi/La Thuile labels).\n\n",
  "MAIN PANEL: cumulative site-years of the current network through time, stacked by IGBP class\n",
  "(PAPER_IGBP_COLOURS, R/plot_constants.R), with Marconi/La Thuile/FLUXNET2015 cumulative\n",
  "site-year lines overlaid for historical context (comparison data only, not primary data --\n",
  "see CLAUDE.md Hard Rule 1). Identical to fig_02_cumulative_siteyears_igbp.png except for the\n",
  "inset and the repositioned legend. Total site-years plotted (", window$first_year, "-",
  window$last_year, "): ", main_total_siteyears, ".\n\n",
  "INSET: distribution of years with data per site in the snapshot (781 sites), stacked by IGBP\n",
  "class, same measure and palette as the main panel. \"Record length\" = years with any of the\n",
  "flux variables checked by compute_site_year_presence() (R/utils.R) present in at least one\n",
  "month of that year -- not a published site-table listing (the current network has no such\n",
  "table). Dashed vertical lines mark 5, 10 and 20 years with data, each labelled with the\n",
  "number of sites at or above that threshold: n=", record_length$thresh_counts[1], " (>=5yr), n=",
  record_length$thresh_counts[2], " (>=10yr), n=", record_length$thresh_counts[3], " (>=20yr).\n",
  "Same panel-building function as Supplementary Figure S6 panel a\n",
  "(fig_record_length_histogram(), R/figures/fig_network_growth.R), called here on the latest\n",
  "snapshot (S6 itself remains pinned to its own documented snapshot -- see\n",
  "docs/decisions_pending.md 2026-10-12).\n\n",
  "LEGEND: one shared IGBP legend for both panels (both use PAPER_IGBP_ORDER/PAPER_IGBP_COLOURS),\n",
  "placed to the right of the inset, inside the main panel. No panel letters.\n\n",
  "DATA: data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv (781 sites, the latest\n",
  "snapshot at the time this candidate was built -- see docs/decisions_pending.md 2026-10-12).\n\n",
  "SOURCE: scripts/generate_fig02alt_cumulative_siteyears_record_length_inset.R, calling\n",
  "R/figures/fig_network_growth.R::fig_cumulative_siteyears_igbp() and\n",
  "::fig_record_length_histogram(). Gate checks (stop, no save, on mismatch): main panel 6,200\n",
  "site-years; inset 781 sites; threshold counts 483/225/63 -- all passed.\n\n"
)

for (sz in SIZES) {
  combined <- build_combined(sz$inset_base_size)
  saved <- save_candidate(combined, sz$stem, width_mm = sz$mm, height_mm = sz$mm)
  msg("Saved: ", saved$png, ", ", saved$pdf, " (inset text ", sz$inset_base_size, "pt)")

  inset_size_note <- if (sz$inset_base_size <= NATURE_SMALL_PT) {
    paste0("inset text ", sz$inset_base_size,
           "pt -- the 5pt floor; larger text did not fit legibly in the inset's ",
           "available width at this figure size (see run report).")
  } else {
    paste0("inset text ", sz$inset_base_size,
           "pt -- the same size as the main panel's own text (the ceiling: inset text is never ",
           "drawn larger than the main panel's); confirmed to fit at this figure size (see run report).")
  }

  legend_lines <- paste0(
    "FIGURE LEGEND -- ", basename(sz$stem), ".png\n",
    strrep("=", 60), "\n\n",
    "TITLE: Figure 2 (candidate) -- Cumulative site-years of the FLUXNET Shuttle network through\n",
    "time, by IGBP land-cover class, with a record-length inset\n\n",
    legend_text_common,
    "DIMENSIONS: ", sz$mm, " x ", sz$mm, " mm, 600 dpi PNG + vector PDF, Helvetica, white\n",
    "background. Main-panel text 7pt; ", inset_size_note, "\n"
  )
  writeLines(legend_lines, paste0(sz$stem, ".legend.txt"))
  msg("Saved: ", paste0(sz$stem, ".legend.txt"))

  write_output_metadata(
    saved$png,
    input_sources = c(SNAP_FILE,
                       "data/snapshots/site_year_data_presence.csv",
                       "data/snapshots/sites_marconi_clean.csv", "data/snapshots/years_marconi.csv",
                       "data/snapshots/sites_la_thuile_clean.csv", "data/snapshots/years_la_thuile.csv",
                       "data/lists/LaThuileList.xlsx",
                       "data/snapshots/sites_fluxnet2015_clean.csv", "data/snapshots/years_fluxnet2015.csv"),
    notes = paste0("Candidate figure (not a replacement for Figure 2 or S6): Figure 2 main panel ",
                    "+ S6 record-length histogram as an inset. Snapshot: ", SNAP_FILE, ". ",
                    "Gate checks passed: 6200 main-panel site-years, 781 inset sites, thresholds ",
                    "483/225/63. See .legend.txt.")
  )
}

msg("=== Done ===")
