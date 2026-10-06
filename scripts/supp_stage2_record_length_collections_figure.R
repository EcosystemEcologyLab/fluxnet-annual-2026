## supp_stage2_record_length_collections_figure.R
##
## Unattended supplementary run, Stage 2: record length per collection
## (Marconi, La Thuile, FLUXNET2015, current network), and Supplementary
## Figure S7.
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
## stops without writing the figure (see review/supp_run_status.md).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")
source("R/nature_format.R")
source("R/plot_constants.R")

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(readxl); library(tidyr)
  library(ggplot2); library(patchwork); library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Stage 2: record length per collection + Figure S7 ===")

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
## Figure S7: two panels
## =============================================================================
msg("--- Building Figure S7 ---")

THRESHOLDS <- c(5L, 10L, 20L)

## ---- Panel a: current-network histogram, stacked by IGBP ------------------
current_hist_df <- current_sites |>
  dplyr::left_join(current_per_site, by = "site_id") |>
  dplyr::mutate(igbp = dplyr::if_else(igbp %in% PAPER_IGBP_ORDER, igbp, NA_character_)) |>
  ## Stack order must follow PAPER_IGBP_ORDER's factor levels (matches the
  ## legend key and Figure 2's own stacking, R/figures/fig_network_growth.R::
  ## fig_cumulative_siteyears_igbp()) -- a plain character column stacks in
  ## whatever order ggplot2 encounters the values, not the palette order.
  dplyr::mutate(igbp = factor(igbp, levels = PAPER_IGBP_ORDER))

thresh_counts <- vapply(THRESHOLDS, function(t) sum(current_hist_df$n_years >= t), integer(1L))
msg("Current network sites at/above thresholds: ",
    paste(paste0(">=", THRESHOLDS, "yr: ", thresh_counts), collapse = "; "))

panel_a <- ggplot(current_hist_df, aes(x = n_years, fill = igbp)) +
  ## alpha = 0.8: same transparency as Figure 2's IGBP-stacked area
  ## (fig_cumulative_siteyears_igbp()'s geom_area, alpha = 0.8); fill
  ## colours come from the same PAPER_IGBP_COLOURS palette via
  ## scale_fill_paper_igbp().
  geom_histogram(binwidth = 1, boundary = 0.5, colour = NA, alpha = 0.8) +
  scale_fill_paper_igbp(name = "IGBP", na.value = "grey70") +
  geom_vline(xintercept = THRESHOLDS - 0.5, linetype = "dashed",
             linewidth = nature_lwd(0.5), colour = "grey20") +
  ## Labels shifted one full bin to the right of their dashed line (was
  ## centred exactly on it, so the line visually crossed the text) --
  ## placed just inside the ">= threshold" side, clear of the line.
  annotate("text", x = THRESHOLDS + 0.5, y = Inf,
           label = paste0("n=", thresh_counts), angle = 90, vjust = 1.2, hjust = 1.1,
           size = NATURE_SMALL_PT / .pt, family = NATURE_FONT, colour = "grey20") +
  scale_x_continuous(name = "Years with data (current network)", breaks = scales::breaks_pretty()) +
  scale_y_continuous(name = "Number of sites", expand = expansion(mult = c(0, 0.08))) +
  nature_theme() +
  theme(legend.key.size = unit(2.2, "mm")) +
  panel_letter("a")

## ---- Panel b: share of sites with >= n years, four collections -----------
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
coll_labels <- setNames(
  paste0(coll_n$collection, " (n=", coll_n$n_sites, ")"),
  coll_n$collection
)
step_df$collection_label <- coll_labels[step_df$collection]

## Figure 2's own collection colours (R/figures/fig_network_growth.R), plus
## dark grey for the current network per task instruction.
COLLECTION_COLOURS <- c(
  "Marconi"     = "#2ECC71",
  "La Thuile"   = "#E74C3C",
  "FLUXNET2015" = "#3498DB",
  "Current"     = "grey20"
)
names(COLLECTION_COLOURS) <- coll_labels[names(COLLECTION_COLOURS)]
step_df$collection_label <- factor(step_df$collection_label, levels = coll_labels[c("Marconi", "La Thuile", "FLUXNET2015", "Current")])

panel_b <- ggplot(step_df, aes(x = n, y = share, colour = collection_label)) +
  geom_step(linewidth = nature_lwd(0.75)) +
  scale_colour_manual(name = NULL, values = COLLECTION_COLOURS) +
  scale_x_continuous(name = "At least n years with data", breaks = scales::breaks_pretty()) +
  scale_y_continuous(name = "Share of sites", labels = scales::label_percent(), limits = c(0, 1)) +
  nature_theme() +
  theme(legend.position = "right", legend.key.size = unit(2.2, "mm")) +
  panel_letter("b")

composite <- panel_a + panel_b + patchwork::plot_layout(ncol = 2, widths = c(1, 1))

## SupFig (Extended-Data-style) width limit: 180 mm, not the 183 mm main-text
## double-column width -- scripts/check_figure_format.R enforces <=180mm for
## every figure under SupFigs/ (confirmed: every existing figS1-figS6 is
## <=179.9mm wide; the repo's only 183mm-wide panels, e.g.
## fig_05_representativeness, live in draft_manuscript_v1/ as main-text
## figures, not SupFigs/). Using 180mm here (not 183mm) so this figure is
## consistent with that established convention and passes the format check.
FIG_WIDTH_MM  <- 180
FIG_HEIGHT_MM <- 90

fig_stem <- file.path(SUPFIGS_DIR, "figS7_record_length")
saved <- save_nature_figure(composite, fig_stem, width_mm = FIG_WIDTH_MM, height_mm = FIG_HEIGHT_MM,
                             extended_data = TRUE)
png_path <- saved$png
msg("Saved: ", saved$png, ", ", saved$pdf, ", ", saved$jpeg)

## ---- Legend file -----------------------------------------------------------
legend_text <- paste0(
"FIGURE LEGEND -- figS7_record_length.png\n",
"============================================================\n\n",
"TITLE: Supplementary Figure S7 -- Record length of the current network and across FLUXNET collections\n\n",
"DESCRIPTION:\n",
"Panel a: distribution of years with data per site for the current (781-site) FLUXNET Shuttle\n",
"network, stacked by IGBP class (PAPER_IGBP_ORDER palette, R/plot_constants.R). Dashed vertical\n",
"lines mark 5, 10 and 20 years, each labelled with the number of sites at or above that\n",
"threshold (n=", thresh_counts[1], ", n=", thresh_counts[2], ", n=", thresh_counts[3], ").\n\n",
"Panel b: for each of four FLUXNET network generations (Marconi, La Thuile, FLUXNET2015, current),\n",
"the share of that collection's sites with at least n years of data, as a step line. Site counts\n",
"shown in the legend key. Current-network line coloured dark grey; the three historical\n",
"collections use the same colours as their lines in Figure 2 (Marconi #2ECC71, La Thuile #E74C3C,\n",
"FLUXNET2015 #3498DB).\n\n",
"IMPORTANT -- two different meanings of 'a year', not interchangeable:\n",
"  - Marconi, La Thuile, FLUXNET2015 (the three historical collections): a year counts if that\n",
"    site is LISTED as having data for that year in the collection's own published site table\n",
"    (La Thuile: 1991-2007 year-indicator columns of data/lists/LaThuileList.xlsx; FLUXNET2015:\n",
"    two-digit year columns of data/lists/FLUXNET2015.xlsx, where a plus sign or the text\n",
"    'Tier 2' both count as a year with data, same as a plain year marker).\n",
"  - Current network: a year counts if ANY flux value (of the 12 broad flux variables checked by\n",
"    compute_site_year_presence(), R/utils.R) is present in AT LEAST ONE MONTH of that year --\n",
"    not a published site-table listing, since the current network has no such table.\n",
"  - Marconi values are SPANS ONLY: Marconi_to_Modern_SiteIDs.xlsx records only a first-last year\n",
"    range per site (e.g. '1997-1998'), not a year-by-year record, so Marconi's per-site year\n",
"    count is that range's length, not a count of individually-confirmed data years. La Thuile and\n",
"    FLUXNET2015, by contrast, are genuine year-by-year indicator counts.\n\n",
"Because of this, panel b's four lines are not a strictly like-for-like comparison -- stated here\n",
"so the figure is not read as claiming otherwise.\n\n",
"COLLECTION TOTALS (sites, site-years): Marconi ", nrow(marconi_per_site), ", ", marconi_total,
"; La Thuile ", nrow(la_thuile_per_site), ", ", la_thuile_total,
"; FLUXNET2015 ", nrow(fluxnet2015_per_site), ", ", fluxnet2015_total,
"; Current ", nrow(current_per_site), ", ", current_total, ".\n\n",
"Final artwork size: 180 mm wide x 90 mm tall, Helvetica throughout. Supplementary Figure (target\n",
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
  notes = "Figure S7: current-network record-length histogram by IGBP (panel a) and cross-collection share-with->=n-years step lines (panel b). See .legend.txt for the two-meanings-of-a-year caveat and Marconi-is-a-span caveat."
)

msg("=== Stage 2 done ===")
