## supp_stage2_record_length_collections_figure.R
##
## Unattended supplementary run, Stage 2: record length per collection
## (Marconi, La Thuile, FLUXNET2015, current network) -- Supplementary
## Figure S1.
##
## Renumbered 2026-10-12 (logs/prompts/2026-10-12_renumber_figures.md): this
## script used to build a two-panel figS6_record_length (panel a, the
## current-network record-length histogram; panel b, the cross-collection
## share-with->=n-years step lines). Panel a's content now lives directly in
## the rebuilt Figure 2's inset (scripts/generate_fig_cumulative_siteyears.R,
## calling the same fig_record_length_histogram()), so it is no longer
## duplicated here -- this script now builds ONLY the former panel b, alone,
## as the new single-panel Supplementary Figure S1
## (figS1_record_length_collections).
##
## Per-site year counts, reusing scripts/collection_comparison_table.R's own
## list-reading code verbatim (not reimplemented): Marconi "Years in
## Marconi" ranges (span only, no year-by-year record); La Thuile year-
## indicator columns (1991-2007) of data/lists/LaThuileList.xlsx, NOT
## data/snapshots/years_la_thuile.csv's span sum (1,008 vs. the correct
## 965); FLUXNET2015.xlsx's two-digit year columns, after dropping its
## second ("Site ID" text) header row, where a non-NA cell (numeric "1",
## "+", or "Tier 2") all count as a year with data; current network via
## compute_site_year_presence() (same measure as Stage 1).
##
## Validates against data/snapshots/collection_sites_siteyears.csv before
## producing any output -- if the per-collection totals this script computes
## do not match 96 / 965 / 1,532 / that file's own Current total, the stage
## stops without writing the figure (see review/supp_run_status.md). Still
## pinned to its own older snapshot (see docs/decisions_pending.md
## 2026-10-12) -- unaffected by this renumbering.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")
source("R/nature_format.R")
source("R/plot_constants.R")
source("R/figures/fig_network_growth.R")

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(readxl); library(tidyr)
  library(ggplot2); library(patchwork); library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Stage 2: record length per collection -- Figure S1 ===")

OUT_DIR     <- "review/figures/draft_manuscript_v1/SupTables"
SUPFIGS_DIR <- "review/figures/draft_manuscript_v1/SupFigs"
fs::dir_create(OUT_DIR); fs::dir_create(SUPFIGS_DIR)

CURRENT_SNAPSHOT  <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
PRESENCE_PATH     <- "data/snapshots/site_year_data_presence.csv"
SITEYEARS_CHECK   <- "data/snapshots/collection_sites_siteyears.csv"

## ---- Marconi: span only (no year-by-year record) --------------------------
marconi_xlsx <- read_excel("data/lists/Marconi_to_Modern_SiteIDs.xlsx")
marconi_fy   <- as.integer(sub("^(\\d{4}).*", "\\1", marconi_xlsx$`Years in Marconi`))
marconi_ly   <- suppressWarnings(as.integer(sub("^\\d{4}-(\\d{4})$", "\\1", marconi_xlsx$`Years in Marconi`)))
marconi_ly   <- dplyr::if_else(is.na(marconi_ly), marconi_fy, marconi_ly)
marconi_per_site <- tibble::tibble(
  site_id = marconi_xlsx$`Modern Site ID`,
  n_years = as.integer(marconi_ly - marconi_fy + 1L)
) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_years = sum(n_years, na.rm = TRUE), .groups = "drop")
marconi_total <- sum(marconi_per_site$n_years)
msg("Marconi: ", nrow(marconi_per_site), " sites, ", marconi_total, " site-years (span sum)")

## ---- La Thuile: year-indicator columns of LaThuileList.xlsx ---------------
la_thuile_xlsx   <- read_excel("data/lists/LaThuileList.xlsx")
la_thuile_yrcols <- names(la_thuile_xlsx)[grepl("^[0-9]{4}$", names(la_thuile_xlsx))]
la_thuile_per_site <- tibble::tibble(
  site_id = la_thuile_xlsx$SITE,
  n_years = as.integer(rowSums(as.matrix(la_thuile_xlsx[, la_thuile_yrcols]), na.rm = TRUE))
)
la_thuile_total <- sum(la_thuile_per_site$n_years)
msg("La Thuile: ", nrow(la_thuile_per_site), " sites, ", la_thuile_total, " site-years (indicator-column count)")

## ---- FLUXNET2015: drop second header row, two-digit year columns ---------
fluxnet2015_xlsx <- read_excel("data/lists/FLUXNET2015.xlsx")
## trimws() alone leaves a trailing U+00A0 (non-breaking space) the xlsx cell
## actually contains -- strip it explicitly before comparing.
if (!identical(trimws(gsub(" ", "", fluxnet2015_xlsx[[1]][1])), "Site ID")) {
  stop("FLUXNET2015.xlsx: expected row 1, column 1 to be the literal second-header-row text ",
       "'Site ID' -- got '", fluxnet2015_xlsx[[1]][1], "'. File layout may have changed; ",
       "re-check the row-drop logic before proceeding.")
}
fluxnet2015_xlsx   <- fluxnet2015_xlsx[-1, ]
fluxnet2015_yrcols <- names(fluxnet2015_xlsx)[grepl("^[0-9]{2}", names(fluxnet2015_xlsx))]
fluxnet2015_per_site <- tibble::tibble(
  site_id = fluxnet2015_xlsx[[1]],
  n_years = as.integer(rowSums(!is.na(as.matrix(fluxnet2015_xlsx[, fluxnet2015_yrcols]))))
)
fluxnet2015_total <- sum(fluxnet2015_per_site$n_years)
msg("FLUXNET2015: ", nrow(fluxnet2015_per_site), " sites, ", fluxnet2015_total,
    " site-years (non-NA two-digit-year-column count; '+' and 'Tier 2' both count)")

## ---- Current network: compute_site_year_presence() (same as Stage 1) -----
current_sites <- read_csv(CURRENT_SNAPSHOT, show_col_types = FALSE) |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::select(site_id, igbp)
presence <- read_csv(PRESENCE_PATH, show_col_types = FALSE) |>
  dplyr::mutate(year = as.integer(year), has_data = as.logical(has_data))
current_per_site <- presence |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_years = sum(has_data), .groups = "drop") |>
  dplyr::filter(site_id %in% current_sites$site_id)
current_total <- sum(current_per_site$n_years)
msg("Current: ", nrow(current_per_site), " sites, ", current_total, " site-years (has_data count)")

## ---- Validation gate: stop (no figure) if totals disagree -----------------
expected <- read_csv(SITEYEARS_CHECK, show_col_types = FALSE)
expected_current <- expected$site_years[expected$collection == "Current"]

checks <- tibble::tibble(
  collection = c("Marconi", "La Thuile", "FLUXNET2015", "Current"),
  computed   = c(marconi_total, la_thuile_total, fluxnet2015_total, current_total),
  expected   = c(96L, 965L, 1532L, expected_current)
) |> dplyr::mutate(ok = computed == expected)
print(checks)

if (!all(checks$ok)) {
  bad <- checks |> dplyr::filter(!ok)
  stop("Stage 2 validation FAILED -- computed site-year totals do not match the expected ",
       "values (96 / 965 / 1,532 / collection_sites_siteyears.csv's Current total):\n",
       paste(capture.output(print(bad)), collapse = "\n"))
}
msg("Validation PASSED: all four collection totals match collection_sites_siteyears.csv / the task's stated checks.")

## =============================================================================
## Figure S1: single panel -- share of sites with >= n years, four collections
## =============================================================================
msg("--- Building Figure S1 ---")

## ---- Share of sites with >= n years, four collections ---------------------
share_curve <- function(n_years_vec, label) {
  n_total <- length(n_years_vec)
  max_n <- max(n_years_vec, na.rm = TRUE)
  tibble::tibble(n = 0:max_n) |>
    dplyr::mutate(
      share      = vapply(n, function(x) sum(n_years_vec >= x) / n_total, numeric(1L)),
      collection = label, n_sites = n_total
    )
}
step_df <- dplyr::bind_rows(
  share_curve(marconi_per_site$n_years,     "Marconi"),
  share_curve(la_thuile_per_site$n_years,   "La Thuile"),
  share_curve(fluxnet2015_per_site$n_years, "FLUXNET2015"),
  share_curve(current_per_site$n_years,     "Current")
)
coll_n <- step_df |> dplyr::distinct(collection, n_sites)
## Display label only -- coll_n$collection itself (used as the lookup key
## below, and for the validation join against SITEYEARS_CHECK's own
## "collection" column) stays the literal data value "Current"; only what's
## actually drawn in the legend key is renamed to "the snapshot".
coll_display <- ifelse(coll_n$collection == "Current", "the snapshot", coll_n$collection)
coll_labels <- setNames(
  paste0(coll_display, " (n=", coll_n$n_sites, ")"),
  coll_n$collection
)
step_df$collection_label <- coll_labels[step_df$collection]

## Collection colours as in the rebuilt Figure 2 (Marconi is now dark grey
## there, not green -- see scripts/generate_fig_cumulative_siteyears.R,
## marconi_colour = "#404040"), plus true black at a heavier line weight for
## the snapshot so it reads as distinct from Marconi's similar dark grey.
COLLECTION_COLOURS <- c(
  "Marconi"     = "#404040",
  "La Thuile"   = "#E74C3C",
  "FLUXNET2015" = "#3498DB",
  "Current"     = "black"
)
COLLECTION_LWD <- c(
  "Marconi"     = nature_lwd(0.75),
  "La Thuile"   = nature_lwd(0.75),
  "FLUXNET2015" = nature_lwd(0.75),
  "Current"     = nature_lwd(1.0)   # heavier -- distinct from Marconi's similar grey; NATURE_LINEWIDTH_MAX
)
names(COLLECTION_COLOURS) <- coll_labels[names(COLLECTION_COLOURS)]
names(COLLECTION_LWD)     <- coll_labels[names(COLLECTION_LWD)]
step_df$collection_label <- factor(step_df$collection_label, levels = coll_labels[c("Marconi", "La Thuile", "FLUXNET2015", "Current")])

## check_figure_format.R measures the RENDERED stroke, including legend key
## glyphs. The in-plot step lines render at exactly their COLLECTION_LWD
## values (confirmed by direct SVG inspection), but the colour legend's own
## key glyphs do not inherit linewidth from scale_linewidth_manual() once
## its own guide is suppressed (guide = "none", needed so only one legend --
## colour -- is drawn, not two) -- they fall back to a ggplot2 default that
## measured 1.07pt, just over the 1.00pt (+-0.05pt tolerance) ceiling.
## override.aes on the colour legend's own guide fixes the key glyphs to a
## single safe width, independent of each line's actual in-plot width.
panel_s1 <- ggplot(step_df, aes(x = n, y = share, colour = collection_label, linewidth = collection_label)) +
  geom_step() +
  scale_colour_manual(name = NULL, values = COLLECTION_COLOURS,
                       guide = guide_legend(override.aes = list(linewidth = nature_lwd(0.75)))) +
  scale_linewidth_manual(values = COLLECTION_LWD, guide = "none") +
  scale_x_continuous(name = "At least n years with data", breaks = scales::breaks_pretty()) +
  scale_y_continuous(name = "Share of sites", labels = scales::label_percent(), limits = c(0, 1)) +
  nature_theme() +
  theme(legend.position = "right", legend.key.size = unit(2.2, "mm"))

## Single panel now (panel b alone) -- same per-panel size it had inside the
## old two-panel 180x90mm composite (each panel was ~90mm wide x 90mm tall).
FIG_WIDTH_MM  <- 90
FIG_HEIGHT_MM <- 90

fig_stem <- file.path(SUPFIGS_DIR, "figS1_record_length_collections")
saved <- save_nature_figure(panel_s1, fig_stem, width_mm = FIG_WIDTH_MM, height_mm = FIG_HEIGHT_MM,
                             extended_data = TRUE)
png_path <- saved$png
msg("Saved: ", saved$png, ", ", saved$pdf, ", ", saved$jpeg)

## ---- Legend file -----------------------------------------------------------
legend_text <- paste0(
"FIGURE LEGEND -- figS1_record_length_collections.png\n",
"============================================================\n\n",
"TITLE: Supplementary Figure S1 — Record length across FLUXNET collections and the snapshot\n\n",
"PUBLICATION LEGEND:\n",
"Supplementary Figure S1. For each of four FLUXNET network generations (Marconi, La Thuile,\n",
"FLUXNET2015, the snapshot), the share of that collection's sites with at least n years of\n",
"data, as a step line. Data used in the analyses are described in Section 2.5.\n\n",
"DESCRIPTION:\n",
"Share of each collection's sites with at least n years of data, as a step line. Site counts\n",
"shown in the legend key. Collection colours as in Figure 2: Marconi dark grey (#404040),\n",
"La Thuile red (#E74C3C), FLUXNET2015 blue (#3498DB); the snapshot is drawn true black, at a\n",
"heavier line weight than the other three, so it reads as distinct from Marconi's similarly\n",
"dark grey. (Figure 2's own record-length histogram by IGBP class -- formerly this figure's\n",
"panel a -- now lives directly in Figure 2's inset; it is not duplicated here.)\n\n",
"IMPORTANT -- two different meanings of 'a year', not interchangeable:\n",
"  - Marconi, La Thuile, FLUXNET2015 (the three historical collections): a year counts if that\n",
"    site is LISTED as having data for that year in the collection's own published site table\n",
"    (La Thuile: 1991-2007 year-indicator columns of data/lists/LaThuileList.xlsx; FLUXNET2015:\n",
"    two-digit year columns of data/lists/FLUXNET2015.xlsx, where a plus sign or the text\n",
"    'Tier 2' both count as a year with data, same as a plain year marker).\n",
"  - The snapshot: a year counts if ANY flux value (of the 12 broad flux variables checked by\n",
"    compute_site_year_presence(), R/utils.R) is present in AT LEAST ONE MONTH of that year --\n",
"    not a published site-table listing, since the current network has no such table.\n",
"  - Marconi values are SPANS ONLY: Marconi_to_Modern_SiteIDs.xlsx records only a first-last year\n",
"    range per site (e.g. '1997-1998'), not a year-by-year record, so Marconi's per-site year\n",
"    count is that range's length, not a count of individually-confirmed data years. La Thuile and\n",
"    FLUXNET2015, by contrast, are genuine year-by-year indicator counts.\n\n",
"Because of this, this figure's four lines are not a strictly like-for-like comparison --\n",
"stated here so the figure is not read as claiming otherwise.\n\n",
"COLLECTION TOTALS (sites, site-years): Marconi ", nrow(marconi_per_site), ", ", marconi_total,
"; La Thuile ", nrow(la_thuile_per_site), ", ", la_thuile_total,
"; FLUXNET2015 ", nrow(fluxnet2015_per_site), ", ", fluxnet2015_total,
"; the snapshot ", nrow(current_per_site), ", ", current_total, ".\n\n",
"Final artwork size: 90 mm wide x 90 mm tall, Helvetica throughout. Supplementary Figure (target\n",
"journal Scientific Data has no Extended Data concept) -- see docs/figure_inventory.md.\n\n",
"SOURCE: scripts/supp_stage2_record_length_collections_figure.R.\n",
"Validated against data/snapshots/collection_sites_siteyears.csv (96 / 965 / 1,532 / ",
expected_current, " site-years).\n",
"Vector PDF alongside this PNG; 300 ppi JPEG alongside both.\n"
)
legend_path <- paste0(fig_stem, ".legend.txt")
writeLines(legend_text, legend_path)
msg("Saved: ", legend_path)

write_output_metadata(
  png_path,
  input_sources = c("data/lists/Marconi_to_Modern_SiteIDs.xlsx", "data/lists/LaThuileList.xlsx",
                     "data/lists/FLUXNET2015.xlsx", CURRENT_SNAPSHOT, PRESENCE_PATH, SITEYEARS_CHECK),
  notes = "Figure S1: cross-collection share-with->=n-years step lines (formerly panel b of figS6_record_length; former panel a now lives in Figure 2's inset). See .legend.txt for the two-meanings-of-a-year caveat and Marconi-is-a-span caveat."
)

msg("=== Stage 2 done ===")
