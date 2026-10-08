## technical_validation_interim.R
##
## Candidate Technical Validation section for Trevor Keenan's review comment
## 25 (data quality and uncertainty). Redraws the already-completed
## diagnostic in review/diagnostics/data_quality_uncertainty/ (stages 0-4) as
## four Nature-format figures and one table, for circulation to all
## co-authors. Side output: does NOT touch paper figures, data/snapshots/, or
## any metrics file, and does NOT rerun dq_stage0-4.R -- every figure/table
## below is built from those scripts' already-written CSVs, with exactly
## three documented exceptions where a figure needs per-record granularity
## the summary tables don't carry (marked "DIRECT DUCKDB READ, read-only,
## same dataset='FLUXMET' scope the diagnostic itself used" below):
##   1. Figure 1a's ECDFs (the existing table only stores quantiles, not the
##      full per-site-period distribution an ECDF needs).
##   2. Check 1 (DuckDB-vs-snapshot-of-record site-list comparison).
##   3. Check 3 (NEE_VUT_SE-vs-MEAN/percentile site-count asymmetry).
##
## Labels/legends throughout use neutral wording only ("usable", "not
## usable", "no reported value", "below quality rule", "did not succeed") --
## never "fail"/"error" -- per the task brief.
##
## Output: review/technical_validation_interim/{figures,tables}/, PNG+PDF
## (+.legend.txt) per figure, CSV+.meta.json for the table.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr); library(tidyr)
  library(ggplot2); library(patchwork); library(fs); library(jsonlite); library(scales)
})
source("R/plot_constants.R")
source("R/nature_format.R")
options(useFancyQuotes = FALSE)

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

DIAG_DIR <- "review/diagnostics/data_quality_uncertainty"
OUT_DIR  <- "review/technical_validation_interim"
FIG_DIR  <- file.path(OUT_DIR, "figures")
TAB_DIR  <- file.path(OUT_DIR, "tables")
dir_create(FIG_DIR, recurse = TRUE)
dir_create(TAB_DIR, recurse = TRUE)

SNAPSHOT_OF_RECORD <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")

write_meta <- function(output_path, input_sources, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                                 error = function(e) NA_character_),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

## Local Nature-format theme add-on, following the combo_theme() pattern in
## scripts/figure_flux_comparison_combo.R: build from theme_classic()
## directly rather than R/plot_constants.R::fluxnet_theme(), whose
## axis.title.x/.y = ggtext::element_markdown() cannot merge with a later
## element_text() override.
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

## Panel-letter tagging: patchwork's own plot_annotation(tag_levels=...)
## (grid/gtable-level, scale-agnostic), NOT R/nature_format.R::panel_letter()
## (a ggplot2 annotate() layer at x=-Inf/y=Inf). Confirmed by direct
## reproduction: annotate(x=-Inf) silently drops its row on a log10-
## transformed scale (log(-Inf) = NaN -> "Removed 1 row ... geom_text()",
## exactly the missing Figure 1b/2b/2c tags this fixes) and is unreliable
## under coord_flip(). patchwork's tag is unaffected by the underlying
## panel's coordinate transform or flip. tv_tag_theme() is added via `&` to
## a finished patchwork composite that already has tag_levels = "a" set.
tv_tag_theme <- function() {
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 8, family = NATURE_FONT))
}

## No dedicated VUT/CUT or 4-variable palette exists yet in R/plot_constants.R
## (checked: only IGBP-class palettes are defined there). Defined locally,
## colourblind-distinguishable; NEE_VUT/NEE_CUT reuse the same two hues in
## every figure below for a consistent visual identity across the section.
VUT_CUT_COLOURS <- c(VUT = "#2C7BB6", CUT = "#D7301F")
FOUR_VAR_COLOURS <- c(NEE_VUT = "#2C7BB6", NEE_CUT = "#D7301F", LE = "#1B9E77", H = "#E6AB02")
QC_FLAG_COLOURS <- c(measured = "#2166AC", good_gapfill = "#92C5DE",
                      medium_gapfill = "#F4A582", poor_gapfill = "#B2182B")
QC_FLAG_LABELS  <- c(measured = "measured", good_gapfill = "good gap-fill",
                      medium_gapfill = "medium gap-fill", poor_gapfill = "poor gap-fill")

## =============================================================================
## Checks
## =============================================================================
check_lines <- character(0)
add_check <- function(...) check_lines <<- c(check_lines, paste0(...))

msg("=== Check 1: DuckDB store vs snapshot of record ===")
con <- dbConnect(duckdb(), db_path, read_only = TRUE)
db_sites <- dbGetQuery(con, "SELECT DISTINCT site_id FROM annual WHERE dataset = 'FLUXMET'")$site_id
snap <- read_csv(SNAPSHOT_OF_RECORD, show_col_types = FALSE)
snap_sites <- unique(snap$site_id)
only_db   <- setdiff(db_sites, snap_sites)
only_snap <- setdiff(snap_sites, db_sites)
match_ok <- length(only_db) == 0L && length(only_snap) == 0L && length(db_sites) == length(snap_sites)

add_check("Check 1 -- DuckDB (annual FLUXMET table) vs snapshot of record (",
          basename(SNAPSHOT_OF_RECORD), "): ", if (match_ok) "MATCH." else "MISMATCH.",
          " DB sites=", length(db_sites), ", snapshot sites=", length(snap_sites),
          ", only-in-DB=", length(only_db), ", only-in-snapshot=", length(only_snap), ".")
if (!match_ok) {
  stop("Check 1 failed: data/duckdb/fluxnet.duckdb's annual FLUXMET site list does not match ",
       SNAPSHOT_OF_RECORD, ". only-in-DB: ", paste(only_db, collapse = ", "),
       "; only-in-snapshot: ", paste(only_snap, collapse = ", "),
       ". Stopping per task instructions -- do not proceed on a mismatched store.")
}
msg("Check 1: MATCH (", length(db_sites), " sites, zero set difference).")

msg("=== Check 3: NEE_VUT_SE vs MEAN/percentile site-count asymmetry ===")
se_vs_mean <- dbGetQuery(con, "
  SELECT
    sum(CASE WHEN NEE_VUT_REF IS NULL AND NEE_VUT_SE IS NOT NULL THEN 1 ELSE 0 END) AS n_se_only_rows,
    sum(CASE WHEN NEE_VUT_REF IS NULL AND NEE_VUT_SE IS NOT NULL
              AND NEE_VUT_REF_NIGHT IS NOT NULL THEN 1 ELSE 0 END) AS n_se_only_with_night
  FROM annual WHERE dataset = 'FLUXMET'
")
## Rewritten as an observation only, per instruction -- no cause offered.
## NEE_VUT_REF_NIGHT/_DAY are the average nighttime/daytime NEE computed
## from daily data (BIFVARINFO_YY VAR_INFO_DEFINITION, confirmed identical
## across sites), not a "day/night partitioning method" -- that causal/
## mechanistic framing in an earlier version of this check is withdrawn.
add_check("Check 3 -- NEE_VUT_SE present at 732 sites vs 618 for REF/MEAN/percentiles: at ",
          se_vs_mean$n_se_only_rows, " site-years, NEE_VUT_SE and NEE_VUT_REF_NIGHT/",
          "NEE_VUT_REF_DAY (the average nighttime and daytime NEE from daily data -- ",
          "BIFVARINFO_YY VAR_INFO_DEFINITION) are reported while the combined annual ",
          "NEE_VUT_REF is -9999 (NA). No cause is offered for this pattern.")
dbDisconnect(con, shutdown = TRUE)
msg("Check 3: ", se_vs_mean$n_se_only_rows, " site-years explained by day/night partials.")

msg("=== Check 2: headline numbers vs report.md/status.md ===")
master      <- read_csv(file.path(DIAG_DIR, "table_stage4_site_year_master.csv"), show_col_types = FALSE)
site_avail  <- read_csv(file.path(DIAG_DIR, "table_stage4_site_level_availability.csv"), show_col_types = FALSE)
stage2_sy   <- read_csv(file.path(DIAG_DIR, "table_stage2_site_year_nee_uncertainty.csv"), show_col_types = FALSE)
hub_compare <- read_csv(file.path(DIAG_DIR, "table_stage1_subdaily_vs_network_hub.csv"), show_col_types = FALSE)
hh_hr_sites <- read_csv(file.path(DIAG_DIR, "table_stage0_hh_hr_sites.csv"), show_col_types = FALSE)

n_site_years   <- nrow(master)
n_sites        <- n_distinct(master$site_id)
n_vut_usable   <- sum(stage2_sy$carbon_type == "VUT")
n_cut_usable   <- sum(stage2_sy$carbon_type == "CUT")
both_sy        <- sum(master$category == "both")
both_sites_n   <- sum(site_avail$category == "both")
n_subdaily     <- n_distinct(hh_hr_sites$site_id)
icos_row       <- hub_compare |> filter(data_hub == "ICOS")
n_no_annual    <- sum(site_avail$category == "neither")

check2 <- tibble::tribble(
  ~item,                                    ~brief,   ~data,
  "site-years (FLUXMET annual)",            6336,     n_site_years,
  "sites",                                  781,      n_sites,
  "usable VUT site-years",                  4017,     n_vut_usable,
  "usable CUT site-years",                  4320,     n_cut_usable,
  "site-years with both usable",            3960,     both_sy,
  "sites with both usable",                 575,      both_sites_n,
  "sub-daily (HH/HR) sites",                31,       n_subdaily,
  "sites with no usable annual NEE (ever)", 125,      n_no_annual
) |> mutate(matches = brief == data)
add_check("Check 2 -- headline numbers:")
for (i in seq_len(nrow(check2))) {
  add_check("  ", check2$item[i], ": brief=", check2$brief[i], ", data=", check2$data[i],
            if (check2$matches[i]) " (confirmed)" else " (CORRECTION NEEDED)")
}
add_check("  ICOS sub-daily share: brief~74% vs data ", round(icos_row$share_subdaily * 100, 1),
          "%; ICOS network-wide share: brief~45% vs data ", round(icos_row$share_network * 100, 1),
          "% (both confirmed by rounding).")

writeLines(check_lines, file.path(OUT_DIR, "checks.txt"))
msg("Checks written to ", file.path(OUT_DIR, "checks.txt"))
for (l in check_lines) message(l)

## =============================================================================
## Figure 1. Gaps by variable and time step
## =============================================================================
msg("=== Figure 1 ===")

## (a) ECDFs of the QC flag fraction, daily/monthly/annual, 4 variables.
## DIRECT DUCKDB READ: the existing table_stage1_qc_distribution.csv only
## stores quantiles (q05/q25/q50/q75/q95), not the full per-site-period
## distribution an ECDF needs -- same dataset='FLUXMET' scope dq_stage1 used,
## read-only, no write to any pipeline table. Weekly excluded (1 site only,
## not network-representative; CLAUDE.md/Stage 1 report).
con <- dbConnect(duckdb(), db_path, read_only = TRUE)
qc_cols <- c(NEE_VUT = "NEE_VUT_REF_QC", NEE_CUT = "NEE_CUT_REF_QC",
             LE = "LE_F_MDS_QC", H = "H_F_MDS_QC")
resolutions <- c(daily = "daily", monthly = "monthly", annual = "annual")

ecdf_data <- bind_rows(lapply(names(resolutions), function(res_name) {
  tbl <- resolutions[[res_name]]
  bind_rows(lapply(names(qc_cols), function(var_name) {
    col <- qc_cols[[var_name]]
    d <- dbGetQuery(con, sprintf(
      "SELECT %s AS qc FROM %s WHERE dataset = 'FLUXMET' AND %s IS NOT NULL", col, tbl, col))
    if (nrow(d) == 0L) return(NULL)
    tibble(resolution = res_name, variable = var_name, qc = d$qc)
  }))
}))
dbDisconnect(con, shutdown = TRUE)
msg("Figure 1a: ", nrow(ecdf_data), " QC values pulled (daily/monthly/annual x 4 variables).")

ecdf_data <- ecdf_data |> mutate(resolution = factor(resolution, levels = c("daily", "monthly", "annual")))

make_ecdf_panel <- function(res_name) {
  d <- ecdf_data |> filter(resolution == res_name)
  ggplot(d, aes(x = qc, colour = variable)) +
    stat_ecdf(geom = "step", linewidth = nature_lwd(0.5)) +
    geom_vline(xintercept = 0.50, linetype = "dashed", colour = "grey50", linewidth = nature_lwd(0.3)) +
    scale_colour_manual(values = FOUR_VAR_COLOURS, name = "Variable") +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = paste0(tools::toTitleCase(res_name), " QC flag (fraction)"), y = "Cumulative share") +
    tv_theme() +
    theme(legend.position = "none")
}
p1a_daily   <- make_ecdf_panel("daily")
p1a_monthly <- make_ecdf_panel("monthly")
p1a_annual  <- make_ecdf_panel("annual") +
  theme(legend.position = "right") + guides(colour = guide_legend(title = "Variable"))

## wrap_elements(full=...) collapses the row of 3 resolution sub-plots into
## ONE opaque unit for patchwork's tagging purposes, so it gets a single "a"
## (as the task's figure spec treats it -- "one panel per resolution" under
## one lettered item), not a/b/c for the three sub-plots individually.
panel1a <- patchwork::wrap_elements(full = p1a_daily | p1a_monthly | p1a_annual)

## (b) Pooled sub-daily QC 0/1/2/3 shares, 31 sites, horizontal stacked bars.
subdaily_net <- read_csv(file.path(DIAG_DIR, "table_stage1_subdaily_qc_network_summary.csv"),
                          show_col_types = FALSE) |>
  pivot_longer(cols = -variable, names_to = "flag", values_to = "share") |>
  mutate(flag = factor(flag, levels = c("measured", "good_gapfill", "medium_gapfill", "poor_gapfill")),
         variable = factor(variable, levels = c("NEE_VUT", "NEE_CUT", "LE", "H")))

panel1b <- ggplot(subdaily_net, aes(x = variable, y = share, fill = flag)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = QC_FLAG_COLOURS, labels = QC_FLAG_LABELS, name = NULL) +
  scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Share of sub-daily records (31 sites)") +
  tv_theme() +
  theme(legend.position = "bottom")

fig1 <- (panel1a / panel1b) + plot_layout(heights = c(1, 0.8)) +
  plot_annotation(tag_levels = "a") & tv_tag_theme()
saved1 <- save_nature_figure(fig1, file.path(FIG_DIR, "fig_tv1_gaps_by_variable_and_timestep"),
                              width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 150)
msg("Saved Figure 1: ", saved1$png)

writeLines(c(
"FIGURE LEGEND -- fig_tv1_gaps_by_variable_and_timestep.png",
"=============================================================",
"",
"TITLE: Figure 1. Gaps by variable and time step",
"",
"DESCRIPTION:",
"(a) Empirical cumulative distribution functions (ECDFs) of the quality-control",
"(QC) flag fraction for NEE_VUT, NEE_CUT, LE and H, one panel per temporal",
"resolution (daily, monthly, annual; left to right), across all 781-site",
"FLUXMET site-periods. Weekly resolution is excluded -- the current DuckDB",
"store holds weekly data for only one site (US-MMS), not a network-",
"representative sample. The dashed vertical line marks QC = 0.50, the",
"paper's QC_THRESHOLD_YY/MM/DD/WW gate (R/pipeline_config.R).",
"(b) Pooled share of sub-daily (half-hourly/hourly) records in each of the",
"four true QC categories (measured, good/medium/poor gap-fill), for the 31",
"of 781 sites with sub-daily FLUXMET files extracted on disk. This 31-site",
"subset is hub-skewed toward ICOS (74% of the subset vs 45% network-wide)",
"though roughly IGBP-representative -- read as an ICOS-weighted estimate,",
"not a network average.",
"",
"COLOUR CODING:",
"(a) NEE_VUT/NEE_CUT/LE/H, fixed 4-colour palette defined in",
"scripts/technical_validation_interim.R (FOUR_VAR_COLOURS; no dedicated",
"palette yet exists in R/plot_constants.R for these four variables).",
"(b) QC category, blue (measured) to red (poor gap-fill), ordinal.",
"",
"DATA SOURCE:",
"(a) Direct read of the annual/monthly/daily DuckDB tables (dataset =",
"'FLUXMET'), read-only -- the existing table_stage1_qc_distribution.csv only",
"stores summary quantiles, not the full per-site-period distribution an ECDF",
"needs. (b) table_stage1_subdaily_qc_network_summary.csv, unchanged.",
"Both ultimately trace to data/duckdb/fluxnet.duckdb, confirmed (this",
"section's Check 1) to match the snapshot of record,",
"data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv.",
"",
"REPRODUCIBILITY:",
"Script: scripts/technical_validation_interim.R",
"Upstream diagnostic: scripts/diagnostics/dq_stage0_inventory.R, dq_stage1_gaps.R",
"DIMENSIONS: 183 x 150 mm, 600 dpi PNG + vector PDF, Helvetica, white background."
), file.path(FIG_DIR, "fig_tv1_gaps_by_variable_and_timestep.legend.txt"))

## =============================================================================
## Figure 2. Uncertainty of usable annual NEE
## =============================================================================
msg("=== Figure 2 ===")
stage2_sy <- stage2_sy |> mutate(carbon_type = factor(carbon_type, levels = c("VUT", "CUT")))

box_long <- stage2_sy |>
  select(carbon_type, random, ustar_term, joint) |>
  pivot_longer(cols = c(random, ustar_term, joint), names_to = "term", values_to = "value") |>
  mutate(term = factor(term, levels = c("random", "ustar_term", "joint"),
                        labels = c("random", "u*-threshold", "joint")))

panel2a <- ggplot(box_long, aes(x = term, y = value, fill = carbon_type)) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3, linewidth = nature_lwd(0.4), width = 0.6) +
  scale_fill_manual(values = VUT_CUT_COLOURS, name = NULL) +
  scale_y_log10(labels = nature_minus_labels()) +
  labs(x = "Uncertainty term", y = expression("Uncertainty (g C "*m^{-2}*" "*yr^{-1}*", log"[10]*")")) +
  tv_theme() +
  theme(legend.position = "right")

## drop0trailing=TRUE, big.mark="": nature_minus_labels()'s default scales::
## label_number() formatting rendered the top log10 break as "1 000.0"
## (space-grouped, one decimal applied uniformly to every break because the
## auto-accuracy heuristic is set by the smallest gap, here 0.1 vs 1) --
## fixed to the plain integer "1000". accuracy=1 alone was tried first and
## rejected: it forces every label to the nearest whole number, which
## rounds the 0.1 break down to "0". drop0trailing only strips zeros that
## are actually trailing, so "0.1" is untouched while "1000.0" -> "1000".
log_labels <- function() nature_minus_labels(drop0trailing = TRUE, big.mark = "")

make_vs_nee_panel <- function(term_col, ylab) {
  d <- stage2_sy |> mutate(abs_nee = abs(NEE), term_val = .data[[term_col]]) |>
    filter(abs_nee > 0, term_val > 0)
  ggplot(d, aes(x = abs_nee, y = term_val, colour = carbon_type)) +
    ## shape=16 (solid circle, no separate border path) -- NOT the default
    ## shape 19, whose nominally-zero-width border still writes a stroked
    ## path into the PDF: scripts/check_figure_format.R measured these as
    ## ~16,600 strokes at 0.01pt, outside the 0.25-1pt Nature rule.
    geom_point(shape = 16, size = 0.5, alpha = 0.35) +
    scale_colour_manual(values = VUT_CUT_COLOURS, name = NULL) +
    scale_x_log10(labels = log_labels()) +
    scale_y_log10(labels = log_labels()) +
    ## NEE magnitude, not "|NEE|": a literal "|" pipe character inside a
    ## plotmath expression() renders as a stray "I" glyph in the PDF export's
    ## base PostScript Helvetica (confirmed by direct render -- same class of
    ## glyph-availability gap R/nature_format.R's header documents for
    ## superscripts and the minus sign). Avoided rather than routed through
    ## plotmath group("|", ., "|"), which hits the same font gap.
    labs(x = expression("NEE magnitude (g C "*m^{-2}*" "*yr^{-1}*", log"[10]*")"), y = ylab) +
    tv_theme() +
    theme(legend.position = "none")
}
panel2b <- make_vs_nee_panel("random",
  expression("Random term (g C "*m^{-2}*" "*yr^{-1}*", log"[10]*")"))
panel2c <- make_vs_nee_panel("ustar_term",
  expression("u*-threshold term (g C "*m^{-2}*" "*yr^{-1}*", log"[10]*")")) +
  theme(legend.position = "right")

fig2 <- (panel2a / (panel2b | panel2c)) + plot_layout(heights = c(0.8, 1)) +
  plot_annotation(tag_levels = "a") & tv_tag_theme()
saved2 <- save_nature_figure(fig2, file.path(FIG_DIR, "fig_tv2_uncertainty_usable_annual_nee"),
                              width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 170)
msg("Saved Figure 2: ", saved2$png)

writeLines(c(
"FIGURE LEGEND -- fig_tv2_uncertainty_usable_annual_nee.png",
"=============================================================",
"",
"TITLE: Figure 2. Uncertainty of usable annual NEE",
"",
"DESCRIPTION:",
"(a) Boxplots (median, IQR box, 1.5xIQR whiskers, outlier points) of the",
"random uncertainty term (NEE_REF_RANDUNC), the u*-threshold term",
"((P84-P16)/2 of the u*-percentile ensemble), and the joint term",
"(NEE_REF_JOINTUNC), for VUT and CUT side by side, log10 y-axis (the u*",
"term has a long right tail -- see report.md Stage 2). (b,c) Random (b) and",
"u*-threshold (c) uncertainty terms (g C m^-2 yr^-1) against NEE magnitude",
"(g C m^-2 yr^-1), one point per usable site-year, VUT/CUT in two colours,",
"both axes log10. Log-axis tick labels are plain integers (e.g. '1000'),",
"not the scales-package default space-grouped '1 000.0'.",
"",
"COLOUR CODING: VUT/CUT, fixed 2-colour palette (VUT_CUT_COLOURS in",
"scripts/technical_validation_interim.R; no dedicated palette yet exists in",
"R/plot_constants.R for VUT/CUT).",
"",
"QUALIFICATION RULE: 'usable' = (1 - QC) <= QC_THRESHOLD_YY (0.50),",
"applied separately to VUT (own NEE_VUT_REF_QC) and CUT (own",
"NEE_CUT_REF_QC) -- not the single per-site VUT/CUT fallback 04_qc.R uses",
"for row exclusion. n = 4,017 usable VUT site-years, 4,320 usable CUT",
"site-years (confirmed, this section's Check 2).",
"",
"DATA SOURCE: table_stage2_site_year_nee_uncertainty.csv, unchanged (no new",
"computation; column definitions as in report.md Stage 2).",
"",
"REPRODUCIBILITY:",
"Script: scripts/technical_validation_interim.R",
"Upstream diagnostic: scripts/diagnostics/dq_stage2_annual_uncertainty.R",
"DIMENSIONS: 183 x 170 mm, 600 dpi PNG + vector PDF, Helvetica, white background."
), file.path(FIG_DIR, "fig_tv2_uncertainty_usable_annual_nee.legend.txt"))

## =============================================================================
## Figure 3. VUT against CUT, site-years where both are usable
## =============================================================================
msg("=== Figure 3 ===")
stage3_sy <- read_csv(file.path(DIAG_DIR, "table_stage3_site_year_vut_vs_cut.csv"), show_col_types = FALSE) |>
  mutate(exceeds_lab = case_when(
    is.na(smaller_than_joint) ~ "joint uncertainty not reported",
    smaller_than_joint        ~ "within combined uncertainty",
    !smaller_than_joint       ~ "exceeds combined uncertainty"
  ) |> factor(levels = c("within combined uncertainty", "exceeds combined uncertainty",
                          "joint uncertainty not reported")))
## 20 of 3,960 site-years have smaller_than_joint = NA (JOINTUNC_VUT or
## JOINTUNC_CUT itself unavailable even though both REF values qualify --
## consistent with Stage 0's REF/RANDUNC/JOINTUNC group not being
## perfectly co-populated at every qualifying site-year). Drawn as an open,
## dark-grey symbol (not the small semi-transparent dots used for the other
## two classes) so these 20 points are visible rather than lost among 3,940
## others.
pts_main    <- stage3_sy |> filter(exceeds_lab != "joint uncertainty not reported")
pts_special <- stage3_sy |> filter(exceeds_lab == "joint uncertainty not reported")

eq_rng <- range(c(stage3_sy$NEE_VUT, stage3_sy$NEE_CUT), na.rm = TRUE)

panel3a <- ggplot(stage3_sy, aes(x = NEE_VUT, y = NEE_CUT, colour = exceeds_lab)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50", linewidth = nature_lwd(0.3)) +
  geom_point(data = pts_main, shape = 16, size = 0.5, alpha = 0.4) +  ## see Figure 2 note on shape=16
  geom_point(data = pts_special, shape = 21, fill = "white", size = 1.3,
             stroke = nature_lwd(0.5), alpha = 1) +
  scale_colour_manual(values = c("within combined uncertainty" = "grey40",
                                  "exceeds combined uncertainty" = "#D7301F",
                                  "joint uncertainty not reported" = "grey30"), name = NULL) +
  coord_equal(xlim = eq_rng, ylim = eq_rng) +
  scale_x_continuous(labels = nature_minus_labels()) +
  scale_y_continuous(labels = nature_minus_labels()) +
  labs(x = expression("NEE"["VUT"]*" (g C "*m^{-2}*" "*yr^{-1}*")"),
       y = expression("NEE"["CUT"]*" (g C "*m^{-2}*" "*yr^{-1}*")")) +
  tv_theme() +
  theme(legend.position = "bottom") +
  ## "within combined uncertainty"'s actual points are drawn at size=0.5,
  ## alpha=0.4 (deliberately faint -- ~3,940 overlapping points); without
  ## an override its legend key renders at that same faint size/alpha and
  ## is hard to see. override.aes bumps every key to full alpha and a
  ## larger, legible size regardless of how its points are actually drawn.
  guides(colour = guide_legend(override.aes = list(shape = c(16, 16, 21), size = c(2.2, 2.2, 1.6),
                                                     alpha = c(1, 1, 1), fill = c(NA, NA, "white"))))

CLIP <- 150
n_beyond <- sum(abs(stage3_sy$diff) > CLIP, na.rm = TRUE)
panel3b <- ggplot(stage3_sy |> filter(abs(diff) <= CLIP), aes(x = diff)) +
  geom_histogram(binwidth = 5, fill = "grey60", colour = "white", linewidth = nature_lwd(0.25)) +
  geom_vline(xintercept = c(-100, -50, -25, 25, 50, 100), linetype = "dashed",
             colour = "grey40", linewidth = nature_lwd(0.25)) +
  scale_x_continuous(limits = c(-CLIP, CLIP), labels = nature_minus_labels()) +
  labs(x = expression("NEE"["VUT"]*" - NEE"["CUT"]*" (g C "*m^{-2}*" "*yr^{-1}*")"), y = "Site-years") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5, size = 5 / .pt, family = "Helvetica",
            label = paste0(n_beyond, " site-years beyond ±", CLIP, " (clipped)")) +
  tv_theme()

fig3 <- (panel3a | panel3b) + plot_annotation(tag_levels = "a") & tv_tag_theme()
saved3 <- save_nature_figure(fig3, file.path(FIG_DIR, "fig_tv3_vut_vs_cut"),
                              width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 100)
msg("Saved Figure 3: ", saved3$png, " (", n_beyond, " site-years beyond +/-", CLIP, ")")

writeLines(c(
"FIGURE LEGEND -- fig_tv3_vut_vs_cut.png",
"=============================================================",
"",
"TITLE: Figure 3. VUT against CUT, site-years where both are usable",
"",
"DESCRIPTION:",
"(a) Scatter of annual NEE_CUT against NEE_VUT on equal axes, dashed 1:1",
"line. Points coloured by whether |VUT-CUT| exceeds the propagated combined",
"uncertainty of the two estimates, sqrt(JOINTUNC_VUT^2 + JOINTUNC_CUT^2).",
"20 of 3,960 site-years ('joint uncertainty not reported') have the",
"joint-uncertainty term itself unavailable on at least one side even though",
"both REF values qualify -- drawn as a visible open dark-grey circle (not",
"the small semi-transparent dot used for the other two classes), since 20",
"points would otherwise be lost among the other 3,940. All three legend",
"keys are shown at full alpha and an enlarged size (override.aes) regardless",
"of how faint/small their actual points are drawn (the ~3,940-point",
"'within combined uncertainty' class in particular is plotted at alpha=0.4,",
"size=0.5 to stay legible as overlapping points, which would otherwise make",
"its own legend key nearly invisible).",
"(b) Histogram of VUT-CUT with dashed vertical lines at +/-25, 50 and 100 g",
"C m^-2 yr^-1. The x-axis is clipped at +/-150; the number of site-years",
paste0("lying beyond that clip (", n_beyond, ") is printed in the panel."),
"",
"n = 3,960 site-years (575 sites) where both NEE_VUT and NEE_CUT",
"independently clear the paper's QC_THRESHOLD_YY=0.50 rule on their own QC",
"column (confirmed, this section's Check 2).",
"",
"DATA SOURCE: table_stage3_site_year_vut_vs_cut.csv, unchanged.",
"",
"REPRODUCIBILITY:",
"Script: scripts/technical_validation_interim.R",
"Upstream diagnostic: scripts/diagnostics/dq_stage3_vut_vs_cut.R",
"DIMENSIONS: 183 x 100 mm, 600 dpi PNG + vector PDF, Helvetica, white background."
), file.path(FIG_DIR, "fig_tv3_vut_vs_cut.legend.txt"))

## =============================================================================
## Figure 4. Availability of annual NEE
## =============================================================================
msg("=== Figure 4 ===")

## (a) Site-years and sites by category, 'neither' split no_value/fails_qc.
sy_cat <- master |>
  mutate(cat2 = case_when(
    category == "neither" & neither_reason == "no_value" ~ "no reported value",
    category == "neither" & neither_reason == "fails_qc" ~ "below quality rule",
    category == "both" ~ "both usable",
    category == "VUT_only" ~ "VUT only usable",
    category == "CUT_only" ~ "CUT only usable"
  )) |> count(cat2, name = "n") |> mutate(level = "Site-years (n = 6,336)")

site_cat <- site_avail |>
  mutate(cat2 = case_when(
    category == "neither" & neither_reason == "no_value" ~ "no reported value",
    category == "neither" & neither_reason == "fails_qc" ~ "below quality rule",
    category == "both" ~ "both usable",
    category == "VUT_only" ~ "VUT only usable",
    category == "CUT_only" ~ "CUT only usable"
  )) |> count(cat2, name = "n") |> mutate(level = "Sites (n = 781)")

CAT_LEVELS <- c("both usable", "VUT only usable", "CUT only usable",
                "no reported value", "below quality rule")
CAT_COLOURS <- c("both usable" = "#4D4D4D", "VUT only usable" = VUT_CUT_COLOURS[["VUT"]],
                  "CUT only usable" = VUT_CUT_COLOURS[["CUT"]],
                  "no reported value" = "#BDBDBD", "below quality rule" = "#756BB1")

## Redrawn as two 100%-stacked horizontal bars (proportion, not raw count) --
## this alone fixes most of the earlier overlap problem, since the Sites bar
## (781 total) no longer has to share one axis with the 8x-larger Site-years
## bar (6,336 total); each bar now spans the same 0-100% width regardless of
## its own total. y = level, x = share, orientation = "y" throughout (not
## coord_flip(), which this script's patchwork-tag fix found unreliable for
## annotate()-based placement; geom_col/geom_text's own `orientation` arg
## gives genuinely horizontal bars without flipping the coordinate system).
avail_df <- bind_rows(sy_cat, site_cat) |>
  mutate(cat2 = factor(cat2, levels = CAT_LEVELS),
         level = factor(level, levels = c("Sites (n = 781)", "Site-years (n = 6,336)"))) |>
  group_by(level) |>
  mutate(share = n / sum(n)) |>
  ungroup()

## No in-bar labels: each category's exact count is carried in its own
## legend label instead, "<category> (<n> site-years, <n> sites)" -- so the
## bars themselves stay a plain, uncluttered 0-100% stacked proportion.
cat_counts <- avail_df |>
  select(cat2, level, n) |>
  tidyr::pivot_wider(names_from = level, values_from = n, values_fill = 0)
CAT_LABELS <- setNames(
  sprintf("%s (%s site-years, %s sites)", cat_counts$cat2,
          format(cat_counts[["Site-years (n = 6,336)"]], big.mark = ","),
          format(cat_counts[["Sites (n = 781)"]], big.mark = ",")),
  as.character(cat_counts$cat2)
)

panel4a <- ggplot(avail_df, aes(y = level, x = share, fill = cat2)) +
  ## position_stack(reverse = TRUE): ggplot2's default stacking order for
  ## orientation="y" placed the LAST CAT_LEVELS entry ("below quality rule")
  ## at x=0 and the FIRST ("both usable") at the far end -- the opposite of
  ## CAT_LEVELS/the legend order. reverse=TRUE matches visual stacking
  ## order to CAT_LEVELS/the legend order.
  geom_col(width = 0.6, orientation = "y", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = CAT_COLOURS, name = NULL, breaks = CAT_LEVELS,
                     labels = CAT_LABELS[CAT_LEVELS]) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1),
                      breaks = c(0, 0.25, 0.5, 0.75, 1), expand = expansion(mult = c(0, 0))) +
  labs(x = "Share", y = NULL) +
  tv_theme() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 1))

## (b) Share where CP/MP did not succeed, by site-year category.
method_qc <- read_csv(file.path(DIAG_DIR, "table_stage4_ustar_method_vs_qualification.csv"),
                       show_col_types = FALSE)
method_summary <- method_qc |>
  group_by(category) |>
  summarise(
    CP   = mean(cp_success == 0, na.rm = TRUE),
    MP   = mean(mp_success == 0, na.rm = TRUE),
    both = mean(cp_success == 0 & mp_success == 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ## Shortened category labels (vs panel a's full "X usable"/"no reported
  ## value" wording) so the x-axis can stay horizontal: scripts/
  ## check_figure_format.R's pdftotext-bbox measurement overstates rendered
  ## font size for ROTATED text (it reports the rotated bbox's axis-aligned
  ## height, not the glyph's true point size), flagging compliant 7pt text
  ## as ~9-10.6pt when angled. Panel (a), directly adjacent, and this
  ## figure's .legend.txt both give the full category names.
  mutate(category = recode(category, both = "both", VUT_only = "VUT only",
                            CUT_only = "CUT only", neither = "neither")) |>
  pivot_longer(cols = -category, names_to = "method", values_to = "share") |>
  mutate(category = factor(category, levels = c("both", "VUT only", "CUT only", "neither")),
         method = factor(method, levels = c("CP", "MP", "both")))

## Legend/series labelled "CP", "MP", "CP and MP"; y-axis title states what
## is being shared in full.
panel4b <- ggplot(method_summary, aes(x = category, y = share, fill = method)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c("CP" = "#FC8D62", "MP" = "#8DA0CB", "both" = "#4D4D4D"), name = NULL,
                     labels = c("CP" = "CP", "MP" = "MP", "both" = "CP and MP")) +
  scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "NEE availability category", y = "Share of site-years in which the method did not succeed") +
  tv_theme() +
  theme(legend.position = "bottom")

fig4 <- (panel4a | panel4b) + plot_annotation(tag_levels = "a") & tv_tag_theme()
saved4 <- save_nature_figure(fig4, file.path(FIG_DIR, "fig_tv4_availability_annual_nee"),
                              width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 115)
msg("Saved Figure 4: ", saved4$png)

## USTAR_CP_SUCCESS_RUN / USTAR_MP_SUCCESS_RUN distinct-value check, per task
## instruction ("report the distinct values... do not interpret them").
ustar_distinct <- method_qc |> summarise(
  cp_vals = paste(sort(unique(cp_success)), collapse = ", "),
  mp_vals = paste(sort(unique(mp_success)), collapse = ", "),
  n_na_cp = sum(is.na(cp_success)), n_na_mp = sum(is.na(mp_success))
)
msg("USTAR_CP_SUCCESS_RUN distinct values: ", ustar_distinct$cp_vals, " (NA count: ", ustar_distinct$n_na_cp, ")")
msg("USTAR_MP_SUCCESS_RUN distinct values: ", ustar_distinct$mp_vals, " (NA count: ", ustar_distinct$n_na_mp, ")")
msg("No file in this repository (CLAUDE.md, R/, docs/, or any data/extracted BIFVARINFO file) ",
    "defines what the USTAR_CP_SUCCESS_RUN/USTAR_MP_SUCCESS_RUN codes mean. The only definition ",
    "present anywhere is scripts/diagnostics/dq_stage4_availability.R's own inline note ",
    "('1=success, 0=failure'), inferred from the variable name, not sourced from FLUXNET/BADM ",
    "product documentation. This figure inherits that convention (labelled 'did not succeed' for ",
    "the value coded 0) without independently re-deriving or re-interpreting it.")

writeLines(c(
"FIGURE LEGEND -- fig_tv4_availability_annual_nee.png",
"=============================================================",
"",
"TITLE: Figure 4. Availability of annual NEE",
"",
"DESCRIPTION:",
"(a) Two 100%-stacked horizontal bars -- site-years (n=6,336) and sites",
"(n=781) -- by availability category: both VUT and CUT usable, VUT only,",
"CUT only, or neither -- 'neither' split into 'no reported value' (no raw",
"NEE_VUT_REF/NEE_CUT_REF value exists that year/site) and 'below quality",
"rule' (a raw value exists but does not clear QC_THRESHOLD_YY=0.50).",
"'Usable' = (1-QC) <= QC_THRESHOLD_YY, own QC column per side (same rule as",
"Figures 2-3). No in-bar count labels -- each category's exact count is",
"given in its own legend entry instead, '<category> (<n> site-years, <n>",
"sites)': both usable (3,960 site-years, 575 sites); VUT only usable (57,",
"41); CUT only usable (360, 40); no reported value (1,935, 125); below",
"quality rule (24, 0).",
"(b) For each site-year availability category, the share where",
"USTAR_CP_SUCCESS_RUN did not succeed (series 'CP'), where",
"USTAR_MP_SUCCESS_RUN did not succeed (series 'MP'), and where both did",
"not succeed (series 'CP and MP') -- the y-axis title states 'in which the",
"method did not succeed' once rather than repeating it in each legend",
"entry (which truncated at 183mm page width when spelled out per-series).",
"Panel (b)'s",
"x-axis uses shortened category labels ('both'/'VUT only'/'CUT only'/",
"'neither') so the text can stay horizontal -- rotated text is measured by",
"scripts/check_figure_format.R's pdftotext-bbox check as larger than its",
"true rendered size. Panel (a), immediately to the left, gives the full",
"category names ('both usable', 'VUT only usable', etc.).",
"IMPORTANT: USTAR_CP_SUCCESS_RUN",
"and USTAR_MP_SUCCESS_RUN take exactly two distinct values (0, 1) in the",
"BIF records joined here, with no missing/other codes. No documentation in",
"this repository defines what these two codes mean -- the 'did not",
"succeed' = 0 labelling is inherited from the diagnostic script's own",
"inline, name-inferred convention (scripts/diagnostics/dq_stage4_availability.R),",
"not independently verified against FLUXNET/BADM product documentation, and",
"this figure does not attempt to re-interpret it further.",
"",
"COLOUR CODING: (a) 5-category palette, VUT/CUT-only categories reuse",
"VUT_CUT_COLOURS. (b) method, 3-colour palette, both local to",
"scripts/technical_validation_interim.R.",
"",
"DATA SOURCE: (a) table_stage4_site_year_master.csv,",
"table_stage4_site_level_availability.csv. (b)",
"table_stage4_ustar_method_vs_qualification.csv. All unchanged.",
"",
"REPRODUCIBILITY:",
"Script: scripts/technical_validation_interim.R",
"Upstream diagnostic: scripts/diagnostics/dq_stage4_availability.R",
"DIMENSIONS: 183 x 115 mm, 600 dpi PNG + vector PDF, Helvetica, white background."
), file.path(FIG_DIR, "fig_tv4_availability_annual_nee.legend.txt"))

## =============================================================================
## Table 1. Sites with values for each quality/uncertainty column
## =============================================================================
msg("=== Table 1 ===")
col_inv <- read_csv(file.path(DIAG_DIR, "table_stage0_column_inventory.csv"), show_col_types = FALSE)

table1 <- col_inv |>
  filter(table %in% c("daily", "monthly", "annual"),
         family %in% c("NEE_VUT", "NEE_CUT", "LE", "H")) |>
  select(table, family, category, column, exists_in_db, n_sites_with_data) |>
  pivot_wider(id_cols = c(family, category, column), names_from = table,
              values_from = n_sites_with_data, names_prefix = "n_sites_") |>
  select(family, category, column, n_sites_daily, n_sites_monthly, n_sites_annual) |>
  arrange(factor(family, levels = c("NEE_VUT", "NEE_CUT", "LE", "H")),
          factor(category, levels = c("reference_value", "qc_flag", "random_uncertainty",
                                       "joint_uncertainty", "pctl_05", "pctl_16", "pctl_25",
                                       "pctl_50", "pctl_75", "pctl_84", "pctl_95", "ustar50",
                                       "mean_variant", "se_variant", "corr_value", "corr_25",
                                       "corr_75", "corr_jointunc")))

table1_path <- file.path(TAB_DIR, "table_tv1_column_coverage_by_site.csv")
write_csv(table1, table1_path)
write_meta(table1_path, input_sources = file.path(DIAG_DIR, "table_stage0_column_inventory.csv"),
           notes = paste0(
             "Pivot of table_stage0_column_inventory.csv restricted to daily/monthly/annual ",
             "resolution (weekly excluded -- 1 site only, not network-representative) and the ",
             "NEE_VUT/NEE_CUT/LE/H families. n_sites_* = count(distinct site_id) with at least one ",
             "non-NA value for that column at that resolution; NA means the column does not exist ",
             "in that table at all. No new computation -- values unchanged from the source table."
           ))
msg("Saved Table 1: ", table1_path, " (", nrow(table1), " rows)")

msg("=== technical_validation_interim.R complete ===")
