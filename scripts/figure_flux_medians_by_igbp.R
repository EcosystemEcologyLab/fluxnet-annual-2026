## figure_flux_medians_by_igbp.R
## Proportional-height scaffold figures and companion tables for five fluxes
## by IGBP class (NEP, GPP, TER, ET, H).
##
## Layout: 6 columns × 2 rows grid of black rectangles.
##   Top row:    EBF, MF, DBF, ENF, CSH, OSH
##   Bottom row: WSA, SAV, GRA, WET, CRO, CVM
## Height = median / max(median) within each flux.
##
## Outputs:
##   review/figures/flux_medians/fig_flux_{nep,gpp,ter,et,h}_by_igbp.png  (300 dpi)
##   data/snapshots/flux_medians_by_igbp_{nep,gpp,ter,et,h}.csv
##   data/snapshots/flux_medians_by_igbp_{nep,gpp,ter,et,h}.meta.json
##     (via write_output_metadata(), R/utils.R)

if (file.exists(".env")) dotenv::load_dot_env()
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

# ---- Constants ---------------------------------------------------------------
SNAPSHOT_CSV <- "data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv"
MEDIANS_CSV  <- "data/snapshots/site_flux_medians_shuttle.csv"
OUT_FIG_DIR  <- "review/figures/flux_medians"
OUT_CSV_DIR  <- "data/snapshots"

dir.create(OUT_FIG_DIR, showWarnings = FALSE, recursive = TRUE)

IGBP_ORDER <- c("EBF","MF","DBF","ENF","CSH","OSH",
                 "WSA","SAV","GRA","WET","CRO","CVM")

IGBP_NAMES <- c(
  EBF = "Evergreen broadleaf forest",
  MF  = "Mixed forest",
  DBF = "Deciduous broadleaf forest",
  ENF = "Evergreen needleleaf forest",
  CSH = "Closed shrubland",
  OSH = "Open shrubland",
  WSA = "Woody savanna",
  SAV = "Savanna",
  GRA = "Grassland",
  WET = "Wetland",
  CRO = "Cropland",
  CVM = "Cropland/natural vegetation mosaic"
)

FLUXES <- list(
  nep = list(
    val      = "nep_median",
    nyrs     = "n_years_nee",
    unit     = "gC m⁻² yr⁻¹",
    title    = "Net Ecosystem Production (NEP) by IGBP class",
    fig_file = "fig_flux_nep_by_igbp.png",
    csv_file = "flux_medians_by_igbp_nep.csv",
    source_priority = paste0(
      "NEP = -NEE (sign flip; positive = net carbon uptake). NEE_VUT_REF ",
      "preferred; NEE_CUT_REF fallback when VUT is unavailable or fails QC. ",
      "VUT/CUT decision made per site-year and applied jointly to NEE, GPP, ",
      "and TER for that year."
    )
  ),
  gpp = list(
    val      = "gpp_median",
    nyrs     = "n_years_gpp",
    unit     = "gC m⁻² yr⁻¹",
    title    = "Gross Primary Productivity (GPP) by IGBP class",
    fig_file = "fig_flux_gpp_by_igbp.png",
    csv_file = "flux_medians_by_igbp_gpp.csv",
    source_priority = paste0(
      "GPP_NT_VUT_REF (or GPP_NT_CUT_REF under the CUT fallback) preferred. ",
      "GPP_DT_VUT_REF (or GPP_DT_CUT_REF) used as a site-level fallback only ",
      "when NT partitioning yields zero qualifying years for a site. NT/DT ",
      "choice is made per site, not per year, to avoid mixing partitioning ",
      "methods within a site's median; recorded per site in gpp_partition ",
      "('NT'/'DT') in the source table."
    )
  ),
  ter = list(
    val      = "ter_median",
    nyrs     = "n_years_ter",
    unit     = "gC m⁻² yr⁻¹",
    title    = "Total Ecosystem Respiration (TER) by IGBP class",
    fig_file = "fig_flux_ter_by_igbp.png",
    csv_file = "flux_medians_by_igbp_ter.csv",
    source_priority = paste0(
      "RECO_NT_VUT_REF (or RECO_NT_CUT_REF) preferred; RECO_DT_VUT_REF (or ",
      "RECO_DT_CUT_REF) fallback under the same per-site NT/DT policy as GPP; ",
      "recorded per site in ter_partition ('NT'/'DT') in the source table."
    )
  ),
  et = list(
    val      = "et_median",
    nyrs     = "n_years_le",
    unit     = "mm yr⁻¹",
    title    = "Evapotranspiration (ET) by IGBP class",
    fig_file = "fig_flux_et_by_igbp.png",
    csv_file = "flux_medians_by_igbp_et.csv",
    source_priority = paste0(
      "Derived from LE_F_MDS [W m⁻²] x 31,557,600 [s yr⁻¹] / lambda [J kg⁻¹], ",
      "lambda = 2.45e6 J/kg (latent heat of vaporisation); 1 kg m⁻² = 1 mm ",
      "water depth. No NT/DT or VUT/CUT distinction applies to LE."
    )
  ),
  h = list(
    val      = "h_median",
    nyrs     = "n_years_h",
    unit     = "W m⁻²",
    title    = "Sensible heat flux (H) by IGBP class",
    fig_file = "fig_flux_h_by_igbp.png",
    csv_file = "flux_medians_by_igbp_h.csv",
    source_priority = paste0(
      "H_F_MDS [W m⁻²], annual mean of gap-filled half-hourly/hourly records; ",
      "no unit conversion applied. No NT/DT or VUT/CUT distinction applies to H."
    )
  )
)

msg("=== Proportional-height IGBP flux scaffold figures ===")
msg("Loading: ", MEDIANS_CSV)
sites <- read_csv(MEDIANS_CSV, show_col_types = FALSE)
msg("  ", nrow(sites), " sites loaded")

# ---- Helper: per-class summary -----------------------------------------------
make_class_table <- function(df, val_col, nyrs_col) {
  df |>
    filter(igbp_class %in% IGBP_ORDER) |>
    group_by(igbp_class) |>
    summarise(
      median_val        = median(.data[[val_col]], na.rm = TRUE),
      n_sites           = sum(!is.na(.data[[val_col]])),
      total_site_years  = sum(.data[[nyrs_col]][!is.na(.data[[val_col]])],
                              na.rm = TRUE),
      std_dev           = sd(.data[[val_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      normalized_height = median_val / max(median_val, na.rm = TRUE),
      igbp_name         = IGBP_NAMES[igbp_class],
      igbp_class        = factor(igbp_class, levels = IGBP_ORDER)
    ) |>
    arrange(igbp_class) |>
    select(igbp_class, igbp_name,
           median = median_val, n_sites, total_site_years, std_dev,
           normalized_height)
}

# ---- Helper: scaffold figure -------------------------------------------------
make_figure <- function(tbl, flux_title, fig_file) {
  df <- tbl |>
    mutate(
      igbp_class = factor(igbp_class, levels = IGBP_ORDER),
      row_panel  = if_else(as.integer(igbp_class) <= 6L, "top", "bottom"),
      row_panel  = factor(row_panel, levels = c("top", "bottom"))
    )

  p <- ggplot(df, aes(x = igbp_class, y = normalized_height)) +
    geom_col(fill = "black", width = 0.72) +
    # Ground line at baseline of rectangles, per panel
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "black") +
    facet_wrap(~ row_panel, nrow = 2, scales = "free_x") +
    scale_y_continuous(
      limits = c(-0.04, 1.05),
      expand = expansion(mult = c(0, 0.01))
    ) +
    labs(title = flux_title) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.title       = element_text(size = 9.5, hjust = 0.5,
                                      margin = margin(b = 10)),
      plot.margin      = margin(t = 14, r = 10, b = 6, l = 10),
      # Show class labels on x-axis below each rectangle
      axis.text.x      = element_text(size = 8, colour = "grey10",
                                       margin = margin(t = 4)),
      # Suppress facet strips (they carry no information)
      strip.text       = element_blank(),
      strip.background = element_blank(),
      # Vertical spacing between the two rows
      panel.spacing    = unit(1.0, "lines")
    )

  out_path <- file.path(OUT_FIG_DIR, fig_file)
  ggsave(out_path, p, width = 7, height = 4.5, dpi = 300, bg = "white")
  msg("  Figure: ", out_path)
  invisible(p)
}

# ---- Helper: write CSV + meta.json -------------------------------------------
write_table <- function(tbl, flux_key, unit_str, csv_file, source_priority) {
  csv_path <- file.path(OUT_CSV_DIR, csv_file)

  out <- tbl |>
    mutate(
      igbp_class        = as.character(igbp_class),
      median            = round(median, 4),
      std_dev           = round(std_dev, 4),
      normalized_height = round(normalized_height, 6)
    )

  write_csv(out, csv_path)
  msg("  Table:  ", csv_path)

  notes <- paste0(
    "Source: FLUXNET Shuttle snapshot ", basename(SNAPSHOT_CSV),
    " (767 sites). Data product: FLUXMET YY v1.3_r1. ",
    "Variable: ", toupper(flux_key), ". Source priority: ", source_priority, " ",
    "QC threshold: 0.80 (governing QC flag must be >= 0.80 for a site-year to ",
    "qualify; NEE_VUT_REF_QC/NEE_CUT_REF_QC gates NEP/GPP/TER, LE_F_MDS_QC ",
    "gates ET, H_F_MDS_QC gates H). ",
    "Per-site aggregation: median across qualifying years, computed per site. ",
    "Class scheme: 12 standard FLUXNET IGBP classes (",
    paste(IGBP_ORDER, collapse = ", "),
    "); non-standard classes (BSV, DNF, SNO) excluded from this summary. ",
    "Cross-class aggregation: each class value is the median of site medians ",
    "(not a site-year pooled median). ",
    "Units: ", unit_str, ". ",
    "normalized_height = median / max(median) within this flux table; ",
    "heights are not comparable across flux tables. ",
    "Generated by scripts/figure_flux_medians_by_igbp.R from ", MEDIANS_CSV, "."
  )

  write_output_metadata(
    csv_path,
    input_sources = c(SNAPSHOT_CSV, MEDIANS_CSV),
    notes = notes
  )
  invisible(csv_path)
}

# ---- Process each flux -------------------------------------------------------
for (flux_key in names(FLUXES)) {
  fd  <- FLUXES[[flux_key]]
  msg("\n--- ", toupper(flux_key), " ---")

  tbl <- make_class_table(sites, fd$val, fd$nyrs)

  # Diagnostic report
  max_cls <- as.character(tbl$igbp_class[which.max(tbl$normalized_height)])
  msg(sprintf("  Max class: %s (norm height = 1.000, median = %.2f %s)",
              max_cls, max(tbl$median, na.rm = TRUE), fd$unit))

  msg("  Normalized heights per class:")
  for (i in seq_len(nrow(tbl))) {
    low_flag <- if (tbl$normalized_height[i] < 0.10) " *** LOW" else ""
    neg_flag <- if (!is.na(tbl$median[i]) && tbl$median[i] < 0) " *** NEGATIVE" else ""
    msg(sprintf("    %-4s  h=%.3f  median=%7.2f  n=%d%s%s",
                as.character(tbl$igbp_class[i]),
                tbl$normalized_height[i],
                tbl$median[i],
                tbl$n_sites[i],
                low_flag, neg_flag))
  }

  # NEP sign check
  neg_classes <- tbl |> filter(median < 0)
  if (nrow(neg_classes) > 0L) {
    msg("  NOTE: ", nrow(neg_classes),
        " IGBP class(es) have NEGATIVE median — normalization needs review:")
    for (j in seq_len(nrow(neg_classes))) {
      msg("    ", as.character(neg_classes$igbp_class[j]),
          " median = ", round(neg_classes$median[j], 2))
    }
  } else {
    msg("  Sign check OK: all class medians non-negative for ", flux_key)
  }

  write_table(tbl, flux_key, fd$unit, fd$csv_file, fd$source_priority)
  make_figure(tbl, fd$title, fd$fig_file)
}

msg("\n=== All five scaffold figures and companion tables complete ===")
