## figure_flux_comparison_combo_alt_common_siteyears.R
## ALTERNATIVE ("ALT") version of the NEP/ET/H combo comparison figure
## (fig_03_flux_comparison_combo_nep_et_h.png), restricting each IGBP class's
## FLUXNET2015-vs-Shuttle comparison to the SAME sites *and* the same
## qualifying calendar years on both axes, matched independently per flux.
##
## Candidate/supplemental figure only -- not part of the numbered pipeline.
## Non-destructive: reads the same already-extracted raw YY files the two
## assess_flux_data_by_igbp_*.R scripts use, and writes new outputs alongside
## them. Does not touch scripts/figure_flux_comparison_combo.R,
## scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R, or any of their
## existing outputs (site_flux_medians_*.csv, flux_comparison_fluxnet2015_vs_
## shuttle.csv, fig_flux_comparison_*.png, fig_03_flux_comparison_combo_*.png).
##
## Rationale (see review/figures/methods_flux_medians.md, "Processing-version
## confound"): the primary combo figure computes each axis's per-class median
## independently, over whatever site-years qualify in *that* dataset alone --
## so a class's FLUXNET2015 n and Shuttle n can differ a lot (e.g. GRA: n=34
## FLUXNET2015 vs n=120 Shuttle), meaning any shift between axes reflects BOTH
## network-composition change (different sites/years) AND ONEFlux
## processing-version differences, and the primary figure cannot separate the
## two. This ALT figure isolates the processing-only effect: same site, same
## calendar year, contributes to both axes -- only whether that site-year was
## processed through the FLUXNET2015 (2020) release or the current Shuttle
## reprocessing differs.
##
## Method (identical row-level QC/fallback policy to
## assess_flux_data_by_igbp_{shuttle,fluxnet2015}.R, vectorised here instead
## of row-looped -- same QC_THRESH, same VUT/CUT and LE/H gating):
##  - NEP: per site-year, NEE_VUT_REF preferred / NEE_CUT_REF fallback, each
##    gated on its own QC >= QC_THRESH. NEP = -NEE. (FLUXNET2015 FULLSET YY
##    has no NEE_CUT_REF column at all, so FLUXNET2015 NEP is VUT-only by
##    construction -- same behaviour as the upstream assess script.)
##  - ET: LE_F_MDS (QC >= QC_THRESH) -> mm/yr via LAMBDA/SECS_YR. Same ad hoc
##    conversion already used by both upstream assess scripts (NOT routed
##    through fluxnet_convert_units() -- kept identical to those scripts for
##    direct comparability, not introduced fresh here).
##  - H: H_F_MDS (QC >= QC_THRESH), W/m2, no conversion.
##  - A site-year qualifies for a given flux only if BOTH datasets have a
##    QC-passing value for that flux in that exact calendar year. Matching is
##    independent per flux -- a site's matched years for NEP need not equal
##    its matched years for ET or H.
##  - Per-site median is computed only over the matched years, on BOTH axes.
##  - Class-level stats (median of site medians, sd, n) computed the same way
##    as class_summary() in figure_flux_comparison_fluxnet2015_vs_shuttle.R.
##    Because sites are matched, fluxnet2015_n_sites == shuttle_n_sites per
##    class by construction here.
##  - Same n>=5 reliability threshold for exclusion, recomputed against the
##    new (generally smaller) matched n -- so the excluded-class set can
##    differ from the primary figure's {CVM, CSH}.
##  - IGBP class label: current Shuttle classification only (one label per
##    site, used for both axes), from data/snapshots/site_flux_medians_
##    shuttle.csv -- avoids a site landing in different class rows on the two
##    axes if it was reclassified between releases.
##
## Outputs:
##   data/snapshots/flux_comparison_fluxnet2015_vs_shuttle_common_siteyears.csv
##     + .meta.json
##   review/figures/candidates/ALT_fig_03_flux_comparison_combo_nep_et_h.png
##     (same panel aesthetics/layout as the primary combo figure)
##   review/figures/candidates/ALT_fig_03_flux_comparison_combo_nep_et_h.txt
##     (plain-text caption, matching the candidates/ folder convention)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(jsonlite)
})

source("R/plot_constants.R")

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

# ---- Constants (identical to assess_flux_data_by_igbp_{shuttle,fluxnet2015}.R) --
QC_THRESH   <- 0.80
LAMBDA      <- 2.45e6
SECS_YR     <- 365.25 * 86400
NA_FLAG     <- -9999

F15_DIR     <- "data/fluxnet2015_comparison"  # relocated out of data/extracted 2026-09-01 (fluxnet2015's non-Shuttle filenames broke flux_discover_files() scans of data/extracted; see SESSION_LOG.md)
SHUTTLE_MEDIANS_CSV <- "data/snapshots/site_flux_medians_shuttle.csv"
OUT_CSV     <- "data/snapshots/flux_comparison_fluxnet2015_vs_shuttle_common_siteyears.csv"
OUT_FIG     <- "review/figures/candidates/ALT_fig_03_flux_comparison_combo_nep_et_h.png"
OUT_TXT     <- "review/figures/candidates/ALT_fig_03_flux_comparison_combo_nep_et_h.txt"
FIG_WIDTH   <- 3.5
FIG_HEIGHT  <- 9.5

STANDARD_IGBP    <- c("EBF","MF","DBF","ENF","CSH","OSH",
                       "WSA","SAV","GRA","WET","CRO","CVM")
RELIABILITY_MIN  <- 5L   # same n>=5 reliability threshold as the primary figure

FLUXES <- c(nep = "NEP", et = "ET", h = "H")
UNITS  <- c(nep = "gC m⁻² yr⁻¹", et = "mm yr⁻¹", h = "W m⁻²")

msg("=== ALT combo figure: FLUXNET2015 vs Shuttle, common sites AND years ===")

# ---- Needed raw columns --------------------------------------------------------
NEEDED_COLS <- c("TIMESTAMP", "NEE_VUT_REF", "NEE_VUT_REF_QC",
                  "NEE_CUT_REF", "NEE_CUT_REF_QC",
                  "LE_F_MDS", "LE_F_MDS_QC", "H_F_MDS", "H_F_MDS_QC")

na_to_na <- function(x) ifelse(is.na(x) | x == NA_FLAG, NA_real_, as.numeric(x))

#' Read one YY file and derive per-year NEP/ET/H, applying the same
#' QC-gated VUT-preferred/CUT-fallback (NEP) and QC-gated LE/H policy used by
#' assess_flux_data_by_igbp_{shuttle,fluxnet2015}.R, vectorised.
read_year_values <- function(path) {
  yy <- tryCatch(read_csv(path, show_col_types = FALSE, na = as.character(NA_FLAG)),
                 error = function(e) NULL)
  if (is.null(yy) || nrow(yy) == 0L || !"TIMESTAMP" %in% names(yy)) return(NULL)

  missing_cols <- setdiff(NEEDED_COLS, names(yy))
  for (col in missing_cols) yy[[col]] <- NA_real_
  for (col in setdiff(NEEDED_COLS, "TIMESTAMP")) yy[[col]] <- na_to_na(yy[[col]])

  yy |>
    transmute(
      year  = TIMESTAMP,
      vut_ok = !is.na(NEE_VUT_REF) & !is.na(NEE_VUT_REF_QC) & NEE_VUT_REF_QC >= QC_THRESH,
      cut_ok = !is.na(NEE_CUT_REF) & !is.na(NEE_CUT_REF_QC) & NEE_CUT_REF_QC >= QC_THRESH,
      nee   = case_when(vut_ok ~ NEE_VUT_REF, cut_ok ~ NEE_CUT_REF, TRUE ~ NA_real_),
      nep   = ifelse(is.na(nee), NA_real_, -nee),
      le_ok = !is.na(LE_F_MDS) & !is.na(LE_F_MDS_QC) & LE_F_MDS_QC >= QC_THRESH,
      et    = ifelse(le_ok, LE_F_MDS * SECS_YR / LAMBDA, NA_real_),
      h_ok  = !is.na(H_F_MDS) & !is.na(H_F_MDS_QC) & H_F_MDS_QC >= QC_THRESH,
      h     = ifelse(h_ok, H_F_MDS, NA_real_)
    ) |>
    select(year, nep, et, h)
}

# ---- Step 0: discover raw files, both datasets ---------------------------------
msg("Discovering FLUXNET2015 YY files under ", F15_DIR)
f15_files <- list.files(F15_DIR, pattern = "_FLUXNET2015_FULLSET_YY_.*\\.csv$",
                         recursive = TRUE, full.names = TRUE)
f15_lookup <- data.frame(
  site_id = basename(dirname(f15_files)),
  path    = f15_files,
  stringsAsFactors = FALSE
) |> distinct(site_id, .keep_all = TRUE)
msg("  FLUXNET2015 sites with YY file: ", nrow(f15_lookup))

msg("Discovering Shuttle YY files under data/extracted (excluding fluxnet2015/)")
sh_files <- list.files("data/extracted", pattern = "FLUXMET_YY.*\\.csv$",
                        recursive = TRUE, full.names = TRUE)
extract_site_id <- function(path) {
  bn <- basename(path)
  m <- regmatches(bn, regexpr("[A-Z]{2,4}-[A-Za-z0-9]+(?=_FLUXNET)", bn, perl = TRUE))
  if (length(m) == 0L) NA_character_ else m[[1L]]
}
sh_lookup <- data.frame(
  path    = sh_files,
  site_id = vapply(sh_files, extract_site_id, character(1L)),
  stringsAsFactors = FALSE
) |>
  filter(!is.na(site_id)) |>
  mutate(fsize = file.size(path)) |>
  group_by(site_id) |>
  slice_max(fsize, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  select(site_id, path)
msg("  Shuttle sites with YY file: ", nrow(sh_lookup))

common_sites <- intersect(f15_lookup$site_id, sh_lookup$site_id)
msg("  Sites present in BOTH datasets: ", length(common_sites))

# ---- IGBP class: current Shuttle classification, single label per site --------
sh_class <- read_csv(SHUTTLE_MEDIANS_CSV, show_col_types = FALSE) |>
  select(site_id, igbp_class) |>
  distinct(site_id, .keep_all = TRUE)

# ---- Step 1: per-site, per-flux medians over MATCHED years only ---------------
msg("\n=== STEP 1: matching years per site per flux ===")

site_rows <- vector("list", length(common_sites))
for (i in seq_along(common_sites)) {
  sid <- common_sites[i]
  if (i %% 50 == 0L) msg("  Processing site ", i, " / ", length(common_sites))

  f15_yv <- read_year_values(f15_lookup$path[f15_lookup$site_id == sid])
  sh_yv  <- read_year_values(sh_lookup$path[sh_lookup$site_id == sid])
  if (is.null(f15_yv) || is.null(sh_yv)) next

  out <- lapply(names(FLUXES), function(fx) {
    f15_ok_years <- f15_yv$year[!is.na(f15_yv[[fx]])]
    sh_ok_years  <- sh_yv$year[!is.na(sh_yv[[fx]])]
    matched <- intersect(f15_ok_years, sh_ok_years)
    if (length(matched) == 0L) {
      return(data.frame(site_id = sid, flux = toupper(fx),
                         fluxnet2015_val = NA_real_, shuttle_val = NA_real_,
                         n_matched_years = 0L, stringsAsFactors = FALSE))
    }
    data.frame(
      site_id = sid, flux = toupper(fx),
      fluxnet2015_val = median(f15_yv[[fx]][f15_yv$year %in% matched], na.rm = TRUE),
      shuttle_val     = median(sh_yv[[fx]][sh_yv$year %in% matched],  na.rm = TRUE),
      n_matched_years = length(matched),
      stringsAsFactors = FALSE
    )
  })
  site_rows[[i]] <- bind_rows(out)
}
site_flux <- bind_rows(site_rows) |>
  left_join(sh_class, by = "site_id") |>
  filter(n_matched_years > 0L)

msg("  Site-flux rows with >=1 matched year: ", nrow(site_flux))

# ---- Step 2: class-level comparison table (same shape as the primary CSV) -----
msg("\n=== STEP 2: class-level stats ===")

comparison_table <- site_flux |>
  filter(igbp_class %in% STANDARD_IGBP) |>
  group_by(flux, igbp_class) |>
  summarise(
    fluxnet2015_median = median(fluxnet2015_val, na.rm = TRUE),
    fluxnet2015_sd      = if (n() < 2L) NA_real_ else sd(fluxnet2015_val, na.rm = TRUE),
    fluxnet2015_n_sites = n(),
    shuttle_median      = median(shuttle_val, na.rm = TRUE),
    shuttle_sd          = if (n() < 2L) NA_real_ else sd(shuttle_val, na.rm = TRUE),
    shuttle_n_sites     = n(),
    .groups = "drop"
  ) |>
  mutate(
    diff     = shuttle_median - fluxnet2015_median,
    pct_diff = 100 * diff / abs(fluxnet2015_median),
    excluded = fluxnet2015_n_sites < RELIABILITY_MIN,
    notes    = ifelse(excluded,
                       paste0("n=", fluxnet2015_n_sites,
                              " sites with >=1 common site-year, below n>=",
                              RELIABILITY_MIN, " reliability threshold"), "")
  ) |>
  arrange(flux, igbp_class)

# Sites present in the intersection but not classified in STANDARD_IGBP, or
# STANDARD_IGBP classes with zero common sites (e.g. CVM, structurally absent
# from FLUXNET2015), still get a row so the exclusion is explicit -- mirrors
# the primary comparison table's "flagged, not dropped" convention.
full_grid <- expand.grid(flux = toupper(names(FLUXES)), igbp_class = STANDARD_IGBP,
                          stringsAsFactors = FALSE)
comparison_table <- full_grid |>
  left_join(comparison_table, by = c("flux", "igbp_class")) |>
  mutate(
    fluxnet2015_n_sites = ifelse(is.na(fluxnet2015_n_sites), 0L, fluxnet2015_n_sites),
    shuttle_n_sites      = ifelse(is.na(shuttle_n_sites), 0L, shuttle_n_sites),
    excluded = ifelse(is.na(excluded), TRUE, excluded),
    notes    = ifelse(is.na(notes) | notes == "",
                       ifelse(fluxnet2015_n_sites == 0L,
                              "0 sites with any common site-year for this class/flux",
                              notes),
                       notes)
  ) |>
  arrange(flux, igbp_class)

for (fx in unname(FLUXES)) {
  sub <- comparison_table |> filter(flux == fx, !excluded)
  msg(sprintf("  %s: %d classes plotted (n_sites range %d-%d)", fx, nrow(sub),
              if (nrow(sub) > 0L) min(sub$fluxnet2015_n_sites) else NA,
              if (nrow(sub) > 0L) max(sub$fluxnet2015_n_sites) else NA))
  excl <- comparison_table |> filter(flux == fx, excluded, fluxnet2015_n_sites > 0L)
  if (nrow(excl) > 0L) {
    msg("    Excluded (n>0 but below threshold): ",
        paste(sprintf("%s(n=%d)", excl$igbp_class, excl$fluxnet2015_n_sites), collapse = ", "))
  }
}

write_csv(comparison_table, OUT_CSV)
msg("Saved: ", OUT_CSV)

meta <- list(
  run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  pipeline_version = system("git rev-parse --short HEAD", intern = TRUE),
  data_sources = list(
    fluxnet2015_raw = F15_DIR,
    shuttle_raw      = "data/extracted (FLUXMET_YY files)",
    shuttle_igbp_class_source = SHUTTLE_MEDIANS_CSV
  ),
  qc_threshold = QC_THRESH,
  method = paste0(
    "Alternative to flux_comparison_fluxnet2015_vs_shuttle.csv: restricts ",
    "each site's FLUXNET2015 and Shuttle per-flux median to ONLY the ",
    "calendar years where BOTH datasets have a QC-passing (>=", QC_THRESH,
    ") value for that flux, matched independently per flux (NEP/ET/H). ",
    "fluxnet2015_n_sites == shuttle_n_sites by construction (same site set ",
    "contributes to both axes). Isolates ONEFlux processing-version ",
    "differences from network-composition/coverage differences -- see ",
    "review/figures/methods_flux_medians.md, 'Processing-version confound'."),
  reliability_threshold = RELIABILITY_MIN,
  igbp_class_source = "current Shuttle classification only (site_flux_medians_shuttle.csv), one label per site used for both axes",
  notes = "Candidate/supplemental analysis; not part of the numbered pipeline. Does not modify or depend on figure_flux_comparison_combo.R or its outputs."
)
jsonlite::write_json(meta, paste0(OUT_CSV, ".meta.json"), pretty = TRUE, auto_unbox = TRUE)
msg("Saved: ", paste0(OUT_CSV, ".meta.json"))

# ---- Step 3: build the 3-panel combo figure ------------------------------------
msg("\n=== STEP 3: building ALT combo figure ===")

combo_theme <- function() {
  fluxnet_theme(base_size = 8) +
    theme(
      legend.position    = "none",
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_rect(fill = "white", colour = NA),
      axis.title         = element_text(size = 7.5),
      axis.text          = element_text(size = 6.5)
    )
}

make_panel <- function(flux_code, unit_str, tag) {
  df <- comparison_table |> filter(flux == flux_code, !excluded)

  all_vals <- c(df$fluxnet2015_median - df$fluxnet2015_sd,
                df$fluxnet2015_median + df$fluxnet2015_sd,
                df$shuttle_median - df$shuttle_sd,
                df$shuttle_median + df$shuttle_sd,
                df$fluxnet2015_median, df$shuttle_median)
  all_vals <- all_vals[is.finite(all_vals)]
  rng  <- range(all_vals)
  pad  <- diff(rng) * 0.10
  lims <- c(rng[1] - pad, rng[2] + pad)

  ggplot(df, aes(x = fluxnet2015_median, y = shuttle_median)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey70", linewidth = 0.4) +
    geom_errorbar(aes(xmin = fluxnet2015_median - fluxnet2015_sd,
                       xmax = fluxnet2015_median + fluxnet2015_sd),
                   orientation = "y", width = 0, colour = "black", linewidth = 0.25) +
    geom_errorbar(aes(ymin = shuttle_median - shuttle_sd,
                       ymax = shuttle_median + shuttle_sd),
                   width = 0, colour = "black", linewidth = 0.25) +
    geom_point(aes(fill = igbp_class), shape = 21, size = 2, colour = "black",
               stroke = 0.3) +
    ggrepel::geom_text_repel(aes(label = igbp_class), size = 2.2, colour = "black",
              seed = 42, min.segment.length = 0.3, segment.size = 0.2,
              segment.colour = "grey50", box.padding = 0.3, point.padding = 0.2) +
    scale_fill_igbp() +
    scale_x_continuous(limits = lims, expand = expansion(mult = 0),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    scale_y_continuous(limits = lims, expand = expansion(mult = 0),
                        sec.axis = dup_axis(name = NULL, labels = NULL)) +
    annotate("text", x = -Inf, y = Inf, label = tag, hjust = -0.5, vjust = 1.6,
             fontface = "bold", size = 3.2, colour = "black") +
    labs(
      x = paste0("FLUXNET2015 median ", flux_code, " ± SD (", unit_str, ")"),
      y = paste0("FLUXNET Shuttle median ", flux_code, " ± SD (", unit_str, ")")
    ) +
    combo_theme()
}

CAPTION <- paste0(
  "ALT version: FLUXNET2015 and Shuttle medians computed over identical ",
  "sites AND identical calendar years on both axes (matched per flux). ",
  "Excluded classes have fewer than ", RELIABILITY_MIN, " common sites for ",
  "that flux -- see companion CSV for per-class n and per-flux exclusions ",
  "(differs from the primary combo figure's fixed {CVM, CSH} exclusion set)."
)

panel_plots <- list(
  make_panel("NEP", UNITS[["nep"]], "A"),
  make_panel("ET",  UNITS[["et"]],  "B"),
  make_panel("H",   UNITS[["h"]],   "C")
)

combo <- (panel_plots[[1]] / panel_plots[[2]] / panel_plots[[3]]) +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(
    caption = paste(strwrap(CAPTION, width = 60), collapse = "\n")
  ) &
  theme(
    plot.caption = element_text(size = 6, colour = "grey30", hjust = 0,
                                 face = "italic", margin = margin(t = 8))
  )

dir.create(dirname(OUT_FIG), showWarnings = FALSE, recursive = TRUE)
ggsave(OUT_FIG, combo, width = FIG_WIDTH, height = FIG_HEIGHT, dpi = 300, bg = "white")
msg("Saved: ", OUT_FIG, " (", FIG_WIDTH, " x ", FIG_HEIGHT, " in, 300 dpi)")

writeLines(c(
  "ALT Figure 3 -- FLUXNET2015 vs Shuttle: NEP/ET/H, common sites AND years",
  "",
  "Alternative to fig_03_flux_comparison_combo_nep_et_h.png. Same panel",
  "layout/aesthetics (A=NEP, B=ET, C=H), but each site's FLUXNET2015 and",
  "Shuttle medians are computed over ONLY the calendar years where both",
  "datasets have a QC-passing value for that flux (matched independently per",
  "flux) -- so fluxnet2015_n_sites == shuttle_n_sites per class by",
  "construction, and any shift between axes isolates ONEFlux",
  "processing-version differences from network-composition/coverage change.",
  "",
  "Excluded classes: n < 5 common sites for that flux (recomputed per flux;",
  "differs from the primary figure's fixed {CVM, CSH} set -- see the",
  "companion CSV for exact per-class/per-flux n and exclusion reasons).",
  "",
  "Data source: data/extracted/fluxnet2015/ (FLUXNET2015 FULLSET YY) and",
  "data/extracted/ (Shuttle FLUXMET YY), QC threshold >= 0.80 on both sides,",
  "IGBP class from data/snapshots/site_flux_medians_shuttle.csv.",
  "",
  "Companion table: data/snapshots/flux_comparison_fluxnet2015_vs_shuttle_common_siteyears.csv",
  "Script: scripts/figure_flux_comparison_combo_alt_common_siteyears.R"
), OUT_TXT)
msg("Saved: ", OUT_TXT)

msg("\n=== ALT combo figure complete ===")
