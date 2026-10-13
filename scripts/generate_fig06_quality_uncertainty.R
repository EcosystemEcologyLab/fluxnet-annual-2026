## generate_fig06_quality_uncertainty.R
##
## Main Figure 6: quality and uncertainty of annual fluxes, for Results 3.4.
## Per logs/prompts/2026-10-13_fig06_quality_uncertainty.md.
##
## Source data: the interim technical-validation outputs
## (scripts/technical_validation_interim.R and
## review/diagnostics/data_quality_uncertainty/), READ-ONLY -- this script
## does NOT re-run any dq_stage*.R diagnostic. Panel (a) is one exception,
## matching technical_validation_interim.R's own documented exception for
## its Figure 1a: a DIRECT DUCKDB READ of the annual table's per-row QC
## values (dataset = 'FLUXMET'), because the existing summary tables store
## only quantiles, not the full per-row distribution an ECDF needs. This is
## the annual panel removed from Supplementary Figure S5
## (scripts/generate_figS5_quality_flags.R, which now keeps only the
## daily/monthly sub-panels).
##
## Panels:
##   a. ECDF of the annual QC flag fraction, NEE_VUT/NEE_CUT/LE/H.
##   b. Uncertainty terms for usable annual NEE (random, u*-threshold,
##      joint), VUT vs CUT -- as interim Figure 2a (TV2a).
##   c. Histogram of NEE_VUT - NEE_CUT, site-years where both are usable.
##
## Output: review/figures/draft_manuscript_v1/fig_06_quality_uncertainty.{png,pdf}
## (+.legend.txt, +.meta.json).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")
source("R/plot_constants.R")
source("R/nature_format.R")

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr); library(tidyr)
  library(ggplot2); library(patchwork); library(fs); library(scales)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Main Figure 6: quality and uncertainty of annual fluxes ===")

DIAG_DIR <- "review/diagnostics/data_quality_uncertainty"
OUT_DIR  <- "review/figures/draft_manuscript_v1"
fs::dir_create(OUT_DIR)
OUT_STEM <- file.path(OUT_DIR, "fig_06_quality_uncertainty")
db_path  <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
SNAPSHOT_OF_RECORD <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"

## Same local theme as the other technical-validation-derived figures
## (scripts/technical_validation_interim.R, scripts/generate_figS5_quality_flags.R):
## theme_classic() direct, not R/plot_constants.R::fluxnet_theme() (its
## axis.title.x/.y = ggtext::element_markdown() cannot merge with a later
## element_text() override).
tv_theme <- function() {
  ggplot2::theme_classic(base_size = 7) +
    ggplot2::theme(
      panel.border      = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.8),
      panel.background  = ggplot2::element_blank(),
      axis.text         = ggplot2::element_text(colour = "black"),
      axis.ticks        = ggplot2::element_line(colour = "black"),
      axis.ticks.length = grid::unit(-3, "pt")
    ) +
    nature_theme()
}
tv_tag_theme <- function() {
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 8, family = NATURE_FONT))
}
log_labels <- function() nature_minus_labels(drop0trailing = TRUE, big.mark = "")

## Same two palettes as Supplementary Figures S5/S6 (and
## scripts/technical_validation_interim.R, which originated them).
VUT_CUT_COLOURS  <- c(VUT = "#2C7BB6", CUT = "#D7301F")
FOUR_VAR_COLOURS <- c(NEE_VUT = "#2C7BB6", NEE_CUT = "#D7301F", LE = "#1B9E77", H = "#E6AB02")
## Display labels for the four QC variables -- same order/colours as
## FOUR_VAR_COLOURS, matched by name so scale_*_manual(labels=) resolves
## correctly regardless of break order. Matches Supplementary Figure S5.
VAR_LABELS <- c(NEE_VUT = "NEE (VUT)", NEE_CUT = "NEE (CUT)", LE = "LE", H = "H")

## ==============================================================================
## Gates -- stop and report on any mismatch, before building or saving anything
## ==============================================================================
msg("--- Gates ---")
master  <- read_csv(file.path(DIAG_DIR, "table_stage4_site_year_master.csv"), show_col_types = FALSE)
stage2  <- read_csv(file.path(DIAG_DIR, "table_stage2_site_year_nee_uncertainty.csv"), show_col_types = FALSE)
stage3  <- read_csv(file.path(DIAG_DIR, "table_stage3_site_year_vut_vs_cut.csv"), show_col_types = FALSE)

gates <- tibble::tribble(
  ~item,                                      ~expected, ~actual,
  "annual rows (table_stage4 master)",         6336L,     nrow(master),
  "site-years with both VUT and CUT usable",   3960L,     sum(master$category == "both"),
  "usable VUT site-years",                     4017L,     sum(stage2$carbon_type == "VUT"),
  "usable CUT site-years",                     4320L,     sum(stage2$carbon_type == "CUT"),
  "site-years missing the joint term on >=1 side", 20L,   sum(is.na(stage3$smaller_than_joint))
) |> dplyr::mutate(ok = expected == actual)
print(gates)

if (!all(gates$ok)) {
  bad <- gates |> dplyr::filter(!ok)
  stop("Figure 6 gate check FAILED -- stopping, not saving:\n",
       paste(capture.output(print(bad)), collapse = "\n"), call. = FALSE)
}
msg("All gates passed.")

## Also confirm stage3 (used for panel c) is already restricted to exactly
## the "both usable" site-years -- it is table_stage3_site_year_vut_vs_cut.csv's
## own scope, not re-derived here.
if (nrow(stage3) != 3960L) {
  stop("table_stage3_site_year_vut_vs_cut.csv has ", nrow(stage3), " rows, expected 3,960 ",
       "(both-usable site-years) -- stopping.", call. = FALSE)
}

## ==============================================================================
## Panel a: ECDF of the annual QC flag fraction, NEE_VUT/NEE_CUT/LE/H
## ==============================================================================
msg("--- Panel a: annual QC ECDF (direct DuckDB read) ---")
con <- dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
qc_cols <- c(NEE_VUT = "NEE_VUT_REF_QC", NEE_CUT = "NEE_CUT_REF_QC",
             LE = "LE_F_MDS_QC", H = "H_F_MDS_QC")
annual_qc <- dplyr::bind_rows(lapply(names(qc_cols), function(var_name) {
  col <- qc_cols[[var_name]]
  d <- dbGetQuery(con, sprintf(
    "SELECT %s AS qc FROM annual WHERE dataset = 'FLUXMET' AND %s IS NOT NULL", col, col))
  if (nrow(d) == 0L) return(NULL)
  tibble::tibble(variable = var_name, qc = d$qc)
}))
dbDisconnect(con, shutdown = TRUE)
msg("Panel a: ", nrow(annual_qc), " annual QC values pulled (4 variables).")

## Fixed order NEE_VUT, NEE_CUT, LE, H (matches Supplementary Figure S5's
## own panel a, fixed there 2026-10-12 for the same reason: a character
## `variable` column legends alphabetically otherwise).
annual_qc <- annual_qc |>
  dplyr::mutate(variable = factor(variable, levels = c("NEE_VUT", "NEE_CUT", "LE", "H")))

## Sites below the panel's now-restricted x-axis range (0.5-1.0, see below) --
## reported here (msg() + legend.txt caption sentence), not drawn in-panel.
below_half <- annual_qc |>
  dplyr::group_by(variable) |>
  dplyr::summarise(n_below_0.5 = sum(qc < 0.5), n_total = dplyr::n(), .groups = "drop")
msg("Panel a: annual QC < 0.5 (below the plotted x-axis range), by variable: ",
    paste(sprintf("%s=%s/%s", below_half$variable, below_half$n_below_0.5, below_half$n_total),
          collapse = ", "))

## coord_cartesian(xlim=), not scale_x_continuous(limits=): a continuous
## scale's own `limits` drops out-of-range rows BEFORE stat_ecdf() computes
## the curve, which would renormalize the ECDF to the 0.5-1.0 subset alone
## (curve starting at 0 right at x=0.5) instead of showing the true
## cumulative share at x=0.5 (a small nonzero value -- the below_half counts
## above). coord_cartesian() only zooms the already-computed curve, so the
## displayed curve still reflects the full [0,1] data.
panel_a <- ggplot(annual_qc, aes(x = qc, colour = variable)) +
  stat_ecdf(geom = "step", linewidth = nature_lwd(1.0)) +
  scale_colour_manual(values = FOUR_VAR_COLOURS, name = "Variable", labels = VAR_LABELS) +
  scale_x_continuous(breaks = c(0.5, 0.75, 1)) +
  coord_cartesian(xlim = c(0.5, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  labs(x = "Annual QC flag (fraction measured or\ngood-quality gap-filled)", y = "Cumulative share") +
  tv_theme() +
  theme(legend.position = c(0.04, 0.97), legend.justification = c(0, 1),
        legend.background = ggplot2::element_rect(fill = "white", colour = NA),
        legend.key.size = grid::unit(6, "pt")) +
  guides(colour = guide_legend(ncol = 1))

## ==============================================================================
## Panel b: uncertainty terms for usable annual NEE, VUT vs CUT (as TV2a)
## ==============================================================================
msg("--- Panel b: uncertainty terms (random, u*-threshold, joint), VUT vs CUT ---")
stage2 <- stage2 |> dplyr::mutate(carbon_type = factor(carbon_type, levels = c("VUT", "CUT")))

box_long <- stage2 |>
  dplyr::select(carbon_type, random, ustar_term, joint) |>
  tidyr::pivot_longer(cols = c(random, ustar_term, joint), names_to = "term", values_to = "value") |>
  dplyr::mutate(term = factor(term, levels = c("random", "ustar_term", "joint"),
                               labels = c("random", "u*-threshold", "joint")))

panel_b <- ggplot(box_long, aes(x = term, y = value, fill = carbon_type)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.3, linewidth = nature_lwd(0.4), width = 0.6) +
  scale_fill_manual(values = VUT_CUT_COLOURS, name = NULL) +
  scale_y_log10(labels = log_labels()) +
  labs(x = "Uncertainty term", y = expression("Uncertainty (g C "*m^{-2}*" "*yr^{-1}*", log"[10]*")")) +
  tv_theme() +
  theme(legend.position = c(0.04, 0.97), legend.justification = c(0, 1),
        legend.background = ggplot2::element_rect(fill = "white", colour = NA),
        legend.key.size = grid::unit(6, "pt"))

## ==============================================================================
## Panel c: histogram of NEE_VUT - NEE_CUT, site-years where both are usable
## ==============================================================================
msg("--- Panel c: VUT - CUT histogram ---")
n_exceeds   <- sum(stage3$smaller_than_joint == FALSE, na.rm = TRUE)
n_within    <- sum(stage3$smaller_than_joint == TRUE,  na.rm = TRUE)
n_no_joint  <- sum(is.na(stage3$smaller_than_joint))
CLIP <- 150
n_beyond_100 <- sum(abs(stage3$diff) > 100, na.rm = TRUE)
n_beyond_clip <- sum(abs(stage3$diff) > CLIP, na.rm = TRUE)
msg("Panel c: ", n_exceeds, " of ", nrow(stage3), " exceed combined uncertainty; ",
    n_no_joint, " lack the joint term; ", n_beyond_100, " beyond +/-100; ",
    n_beyond_clip, " beyond +/-", CLIP, " (clipped from view).")

## No in-panel text (2026-10-13 revision) -- the three counts previously
## annotated here (exceeds combined uncertainty, lacks the joint term,
## outside the plotted range) are instead caption sentences in .legend.txt.
## y-axis limit fixed at 1,490 (no longer needs the extra top expansion a
## multi-line in-panel label once required).
panel_c <- ggplot(stage3 |> dplyr::filter(abs(diff) <= CLIP), aes(x = diff)) +
  geom_histogram(binwidth = 5, fill = "grey60", colour = "white", linewidth = nature_lwd(0.25)) +
  scale_x_continuous(limits = c(-CLIP, CLIP), labels = nature_minus_labels()) +
  scale_y_continuous(limits = c(0, 1490), expand = expansion(mult = c(0, 0))) +
  labs(x = expression("NEE"["VUT"]*" - NEE"["CUT"]*" (g C "*m^{-2}*" "*yr^{-1}*")"), y = "Site-years") +
  tv_theme()

## ==============================================================================
## Compose: one row of three panels, 183 mm wide
## ==============================================================================
msg("--- Composing figure ---")
fig6 <- (panel_a | panel_b | panel_c) +
  plot_annotation(tag_levels = "a") & tv_tag_theme()

saved <- save_nature_figure(fig6, OUT_STEM, width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 78)
msg("Saved: ", saved$png, ", ", saved$pdf)

## ==============================================================================
## Legend
## ==============================================================================
legend_lines <- c(
  "FIGURE LEGEND -- fig_06_quality_uncertainty.png",
  strrep("=", 60), "",
  "TITLE: Figure 6. Quality and uncertainty of annual fluxes", "",
  "PUBLICATION LEGEND:",
  "Figure 6. Quality and uncertainty of annual fluxes. (a) Completeness of the annual record by",
  "variable. (b) Magnitude of each uncertainty term contributing to usable annual NEE, VUT and",
  "CUT compared. (c) Effect of the VUT/CUT processing choice on annual NEE sums, for site-years",
  "where both are usable.", "",
  "DESCRIPTION:",
  "(a) Empirical cumulative distribution functions (ECDFs) of the annual quality-control (QC)",
  "flag fraction for NEE_VUT, NEE_CUT, LE and H, across all 781-site FLUXMET annual rows",
  sprintf("(n = %s). The QC flag fraction runs 1 (every underlying half-hourly/hourly record", format(nrow(master), big.mark=",")),
  "for that site-year was measured or good-quality gap-filled) down to 0 (every record is",
  "medium- or poor-quality gap-filled, or missing) -- higher is better. The x-axis is shown from",
  "0.5 to 1.0 only, the 0.5 rule applied throughout this synthesis (QC_THRESHOLD_YY,",
  "R/pipeline_config.R). The variable legend",
  "is placed inside the panel's upper-left area, which the ECDF curves do not reach. Annual QC",
  "values below 0.50 (off the plotted range) are not shown in the curve but are counted here:",
  sprintf("%s of %s NEE_VUT, %s of %s NEE_CUT, %s of %s LE, and %s of %s H annual QC values fall",
          format(below_half$n_below_0.5[below_half$variable == "NEE_VUT"], big.mark = ","),
          format(below_half$n_total[below_half$variable == "NEE_VUT"], big.mark = ","),
          format(below_half$n_below_0.5[below_half$variable == "NEE_CUT"], big.mark = ","),
          format(below_half$n_total[below_half$variable == "NEE_CUT"], big.mark = ","),
          format(below_half$n_below_0.5[below_half$variable == "LE"], big.mark = ","),
          format(below_half$n_total[below_half$variable == "LE"], big.mark = ","),
          format(below_half$n_below_0.5[below_half$variable == "H"], big.mark = ","),
          format(below_half$n_total[below_half$variable == "H"], big.mark = ",")),
  "below 0.50.",
  "This is the annual panel removed from Supplementary Figure S5, which now keeps only the",
  "daily/monthly panels.",
  "(b) Boxplots (median, IQR box, 1.5xIQR whiskers, outlier points) of three uncertainty terms",
  "for usable annual NEE: the random uncertainty term (NEE_REF_RANDUNC), the u*-threshold term",
  "((P84-P16)/2 of the u*-percentile ensemble), and the joint term (NEE_REF_JOINTUNC), VUT and",
  "CUT side by side, log10 y-axis (the u*-threshold term has a long right tail). LE and H have",
  "no u*-threshold term in the FLUXNET Shuttle product and are not shown in this panel. The",
  "VUT/CUT legend is placed inside the panel's upper-left area.",
  "(c) Histogram of NEE_VUT - NEE_CUT for site-years where both are usable (one bar per 5 g C",
  sprintf("m^-2 yr^-1 bin), n = %s. No notes are drawn in the panel; the three counts below are", format(nrow(stage3), big.mark=",")),
  "reported here instead. Of the",
  sprintf("%s site-years, %s exceed the propagated combined uncertainty of the two estimates,",
          format(nrow(stage3), big.mark = ","), format(n_exceeds, big.mark = ",")),
  "sqrt(JOINTUNC_VUT^2 + JOINTUNC_CUT^2);",
  sprintf("%s site-years have the joint-uncertainty term itself unavailable on at least one side", format(n_no_joint, big.mark = ",")),
  "even though both REF values qualify; and",
  sprintf("%s site-years fall outside the plotted range (+/-%d g C m^-2 yr^-1) and are not shown", format(n_beyond_clip, big.mark = ","), CLIP),
  "in the histogram -- clipping is a display choice and does not affect either count above. The",
  "y-axis is fixed at 1,490 site-years.", "", "PATTERNS (described neutrally, no cause offered):",
  "(a) NEE_VUT and NEE_CUT reach similar annual QC fractions at every percentile; LE and H",
  "reach a given QC fraction at a lower percentile than the two NEE variables.",
  "(b) The joint term is larger than either the random or u*-threshold term alone for both VUT",
  "and CUT, consistent with it combining the two; the u*-threshold term's distribution has a",
  "longer right tail than the random term's.",
  sprintf("(c) Differences cluster near zero; %s of %s site-years (%.1f%%) exceed the combined", n_exceeds, nrow(stage3), 100*n_exceeds/nrow(stage3)),
  sprintf("uncertainty, and %s lack a joint-uncertainty value on at least one side.", n_no_joint), "",
  "QUALIFICATION RULE (panels b, c): 'usable' = (1 - QC) <= QC_THRESHOLD_YY (0.50), applied",
  "separately to VUT (own NEE_VUT_REF_QC) and CUT (own NEE_CUT_REF_QC) -- not the single",
  "per-site VUT/CUT fallback scripts/04_qc.R uses for row exclusion elsewhere in the paper.",
  sprintf("n = %s usable VUT site-years, %s usable CUT site-years, %s site-years where both are",
          format(sum(stage2$carbon_type == "VUT"), big.mark=","),
          format(sum(stage2$carbon_type == "CUT"), big.mark=","),
          format(nrow(stage3), big.mark=",")),
  "usable (panel c and panel b's paired comparison).", "",
  "COLOUR CODING:",
  "(a) NEE_VUT/NEE_CUT/LE/H, the same 4-colour palette as Supplementary Figure S5",
  "(FOUR_VAR_COLOURS), displayed in the legend as 'NEE (VUT)', 'NEE (CUT)', 'LE', 'H' (same",
  "order and colours as the underlying column names).",
  "(b) VUT/CUT, the same 2-colour palette as Supplementary Figures S5 and S6",
  "(VUT_CUT_COLOURS).", "",
  "DATA SOURCE:",
  "(a) Direct read of the annual DuckDB table (dataset = 'FLUXMET'), read-only -- the existing",
  "table_stage1_qc_distribution.csv only stores summary quantiles, not the full per-site-year",
  "distribution an ECDF needs (same documented exception as",
  "scripts/technical_validation_interim.R's own Figure 1a).",
  "(b) review/diagnostics/data_quality_uncertainty/table_stage2_site_year_nee_uncertainty.csv,",
  "unchanged.",
  "(c) review/diagnostics/data_quality_uncertainty/table_stage3_site_year_vut_vs_cut.csv,",
  "unchanged. No dq_stage*.R diagnostic was re-run to build this figure.", "",
  sprintf("SNAPSHOT: %s (781 sites).", basename(SNAPSHOT_OF_RECORD)), "",
  "SOURCE: scripts/generate_fig06_quality_uncertainty.R.",
  paste0("DIMENSIONS: ", NATURE_WIDTH_DOUBLE_MM, " x 78 mm, 600 dpi PNG + vector PDF, Helvetica, white background.")
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))

write_output_metadata(
  saved$png,
  input_sources = c(
    db_path,
    file.path(DIAG_DIR, "table_stage4_site_year_master.csv"),
    file.path(DIAG_DIR, "table_stage2_site_year_nee_uncertainty.csv"),
    file.path(DIAG_DIR, "table_stage3_site_year_vut_vs_cut.csv"),
    SNAPSHOT_OF_RECORD
  ),
  notes = paste0(
    "Main Figure 6: quality and uncertainty of annual fluxes (Results 3.4). Panel a is a direct, ",
    "read-only DuckDB query (annual QC fractions) -- the annual panel removed from Supplementary ",
    "Figure S5; panels b/c reuse review/diagnostics/data_quality_uncertainty/ table_stage2/",
    "table_stage3 unchanged, no dq_stage*.R diagnostic re-run. Gates passed: 6,336 annual rows; ",
    "3,960 both usable; 4,017 VUT usable; 4,320 CUT usable; 20 missing the joint term. 2026-10-13 ",
    "revision: ECDF lines in panel a thickened to 1.0pt (same change in Supplementary Figure S5a); ",
    "panel a x-axis restricted to 0.5-1.0 with the variable legend moved inside the panel; panel b's ",
    "legend moved inside the panel; panel c's in-panel text removed (counts now in .legend.txt only) ",
    "and its y-axis fixed at 1,490. See .legend.txt."
  )
)

msg("=== Done ===")
