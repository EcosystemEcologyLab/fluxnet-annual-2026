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

panel_a <- ggplot(annual_qc, aes(x = qc, colour = variable)) +
  stat_ecdf(geom = "step", linewidth = nature_lwd(0.5)) +
  geom_vline(xintercept = 0.50, linetype = "dashed", colour = "grey50", linewidth = nature_lwd(0.3)) +
  scale_colour_manual(values = FOUR_VAR_COLOURS, name = "Variable") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  labs(x = "Annual QC flag (fraction measured or\ngood-quality gap-filled)", y = "Cumulative share") +
  tv_theme() +
  theme(legend.position = "bottom", legend.key.size = grid::unit(6, "pt")) +
  guides(colour = guide_legend(ncol = 2))

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
  theme(legend.position = "bottom", legend.key.size = grid::unit(6, "pt"))

## ==============================================================================
## Panel c: histogram of NEE_VUT - NEE_CUT, site-years where both are usable
## ==============================================================================
msg("--- Panel c: VUT - CUT histogram ---")
n_exceeds   <- sum(stage3$smaller_than_joint == FALSE, na.rm = TRUE)
n_within    <- sum(stage3$smaller_than_joint == TRUE,  na.rm = TRUE)
n_no_joint  <- sum(is.na(stage3$smaller_than_joint))
CLIP <- 150
n_beyond_clip <- sum(abs(stage3$diff) > CLIP, na.rm = TRUE)
msg("Panel c: ", n_exceeds, " of ", nrow(stage3), " exceed combined uncertainty; ",
    n_no_joint, " lack the joint term; ", n_beyond_clip, " beyond +/-", CLIP, " (clipped from view).")

panel_c_label <- paste0(
  n_exceeds, " of ", nrow(stage3), " exceed combined uncertainty\n",
  n_no_joint, " lack the joint term\n",
  n_beyond_clip, " beyond ±", CLIP, " (clipped)"
)
panel_c <- ggplot(stage3 |> dplyr::filter(abs(diff) <= CLIP), aes(x = diff)) +
  geom_histogram(binwidth = 5, fill = "grey60", colour = "white", linewidth = nature_lwd(0.25)) +
  scale_x_continuous(limits = c(-CLIP, CLIP), labels = nature_minus_labels()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(x = expression("NEE"["VUT"]*" - NEE"["CUT"]*" (g C "*m^{-2}*" "*yr^{-1}*")"), y = "Site-years") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.03, vjust = 1.2, size = 5 / .pt, family = "Helvetica",
           label = panel_c_label) +
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
  "medium- or poor-quality gap-filled, or missing) -- higher is better. The dashed vertical",
  "line marks QC = 0.50, the 0.5 rule applied throughout this synthesis (QC_THRESHOLD_YY,",
  "R/pipeline_config.R). This is the annual panel removed from Supplementary Figure S5, which",
  "now keeps only the daily/monthly panels.",
  "(b) Boxplots (median, IQR box, 1.5xIQR whiskers, outlier points) of three uncertainty terms",
  "for usable annual NEE: the random uncertainty term (NEE_REF_RANDUNC), the u*-threshold term",
  "((P84-P16)/2 of the u*-percentile ensemble), and the joint term (NEE_REF_JOINTUNC), VUT and",
  "CUT side by side, log10 y-axis (the u*-threshold term has a long right tail). LE and H have",
  "no u*-threshold term in the FLUXNET Shuttle product and are not shown in this panel.",
  "(c) Histogram of NEE_VUT - NEE_CUT for site-years where both are usable (one bar per 5 g C",
  sprintf("m^-2 yr^-1 bin), n = %s. The panel states how many of these differences exceed the", format(nrow(stage3), big.mark=",")),
  "propagated combined uncertainty of the two estimates, sqrt(JOINTUNC_VUT^2 + JOINTUNC_CUT^2),",
  "and how many site-years have the joint-uncertainty term itself unavailable on at least one",
  "side even though both REF values qualify. The x-axis is clipped at +/-150 g C m^-2 yr^-1;",
  "the number of site-years lying beyond that clip is stated in the panel separately from the",
  "combined-uncertainty count -- clipping is a display choice and does not affect either count.",
  "", "PATTERNS (described neutrally, no cause offered):",
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
  "(FOUR_VAR_COLOURS).",
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
    "3,960 both usable; 4,017 VUT usable; 4,320 CUT usable; 20 missing the joint term. See .legend.txt."
  )
)

msg("=== Done ===")
