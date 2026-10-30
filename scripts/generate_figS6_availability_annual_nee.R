## generate_figS6_availability_annual_nee.R
##
## Supplementary Figure S6: availability of annual NEE, promoted from the
## interim technical-validation candidate (scripts/technical_validation_interim.R,
## review/technical_validation_interim/) into the paper's own supplementary
## material, per logs/prompts/2026-10-12_renumber_figures.md.
##
## Reuses that script's Figure 4 ("Availability of annual NEE") panel a data
## and panel-building logic, WITHOUT panel b (the u*-threshold method-success
## panel, CP/MP) -- single panel here, no panel letter needed. Does NOT
## re-run any dq_stage*.R diagnostic -- reads only its already-written table
## CSVs, read-only.
##
## "No annual NEE in the product" = no annual NEE value at all in the product
## (neither VUT nor CUT has a raw value for that site-year) -- distinct from
## "below quality rule" (a raw value exists but does not clear QC_THRESHOLD_YY).
##
## Site-year count used here (6,336, all FLUXMET annual rows regardless of
## quality) differs from Figure 2's 6,200: Figure 2 counts site-years via
## compute_site_year_presence() (any of several flux variables present in at
## least one month, filtered to its own data-driven year window and to sites
## with a recognised IGBP class), not "has an annual NEE row in FLUXMET" --
## two different measures, not a discrepancy to be reconciled here.

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
  library(dplyr); library(readr); library(tidyr); library(ggplot2); library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Supplementary Figure S6: availability of annual NEE ===")

DIAG_DIR <- "review/diagnostics/data_quality_uncertainty"
OUT_DIR  <- "review/figures/draft_manuscript_v1/SupFigs"
fs::dir_create(OUT_DIR)
OUT_STEM <- file.path(OUT_DIR, "figS6_availability_annual_nee")

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
VUT_CUT_COLOURS <- c(VUT = "#2C7BB6", CUT = "#D7301F")

master     <- read_csv(file.path(DIAG_DIR, "table_stage4_site_year_master.csv"), show_col_types = FALSE)
site_avail <- read_csv(file.path(DIAG_DIR, "table_stage4_site_level_availability.csv"), show_col_types = FALSE)
n_site_years <- nrow(master)
n_sites      <- n_distinct(master$site_id)
msg("Site-years (FLUXMET annual rows): ", n_site_years, "; sites: ", n_sites)

## ---- Panel: site-years and sites by availability category -----------------
sy_cat <- master |>
  mutate(cat2 = case_when(
    category == "neither" & neither_reason == "no_value" ~ "no annual NEE in the product",
    category == "neither" & neither_reason == "fails_qc" ~ "below quality rule",
    category == "both" ~ "both usable",
    category == "VUT_only" ~ "VUT only usable",
    category == "CUT_only" ~ "CUT only usable"
  )) |> count(cat2, name = "n") |> mutate(level = paste0("Site-years (n = ", format(n_site_years, big.mark = ","), ")"))

site_cat <- site_avail |>
  mutate(cat2 = case_when(
    category == "neither" & neither_reason == "no_value" ~ "no annual NEE in the product",
    category == "neither" & neither_reason == "fails_qc" ~ "below quality rule",
    category == "both" ~ "both usable",
    category == "VUT_only" ~ "VUT only usable",
    category == "CUT_only" ~ "CUT only usable"
  )) |> count(cat2, name = "n") |> mutate(level = paste0("Sites (n = ", n_sites, ")"))

CAT_LEVELS <- c("both usable", "VUT only usable", "CUT only usable",
                "no annual NEE in the product", "below quality rule")
CAT_COLOURS <- c("both usable" = "#4D4D4D", "VUT only usable" = VUT_CUT_COLOURS[["VUT"]],
                  "CUT only usable" = VUT_CUT_COLOURS[["CUT"]],
                  "no annual NEE in the product" = "#BDBDBD", "below quality rule" = "#756BB1")

sy_level_label   <- unique(sy_cat$level)
site_level_label <- unique(site_cat$level)

avail_df <- bind_rows(sy_cat, site_cat) |>
  mutate(cat2 = factor(cat2, levels = CAT_LEVELS),
         level = factor(level, levels = c(site_level_label, sy_level_label))) |>
  group_by(level) |>
  mutate(share = n / sum(n)) |>
  ungroup()

cat_counts <- avail_df |>
  select(cat2, level, n) |>
  tidyr::pivot_wider(names_from = level, values_from = n, values_fill = 0)
## Two lines per key (category name, then counts) -- the longest category
## name ("no annual NEE in the product") plus its count suffix on one line
## clipped the right page edge (check_figure_format.R) at this figure's width.
CAT_LABELS <- setNames(
  sprintf("%s\n(%s site-years, %s sites)", cat_counts$cat2,
          format(cat_counts[[sy_level_label]], big.mark = ",", trim = TRUE),
          format(cat_counts[[site_level_label]], big.mark = ",", trim = TRUE)),
  as.character(cat_counts$cat2)
)

panel <- ggplot(avail_df, aes(y = level, x = share, fill = cat2)) +
  geom_col(width = 0.6, orientation = "y", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = CAT_COLOURS, name = NULL, breaks = CAT_LEVELS,
                     labels = CAT_LABELS[CAT_LEVELS]) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1),
                      breaks = c(0, 0.25, 0.5, 0.75, 1), expand = expansion(mult = c(0, 0))) +
  labs(x = "Share", y = NULL) +
  tv_theme() +
  theme(legend.position = "bottom",
        legend.key.size = grid::unit(7, "pt"),
        plot.margin = ggplot2::margin(t = 5.5, r = 12, b = 5.5, l = 5.5, unit = "pt")) +
  guides(fill = guide_legend(ncol = 1))

saved <- save_nature_figure(panel, OUT_STEM, width_mm = NATURE_WIDTH_SINGLE_MM, height_mm = 80,
                             extended_data = TRUE)
msg("Saved: ", saved$png, ", ", saved$pdf)

## ---- Legend ----------------------------------------------------------------
cc <- cat_counts |> tibble::column_to_rownames("cat2")
fmt_counts <- function(cat) sprintf("%s (%s site-years, %s sites)", cat,
                                      format(cc[cat, sy_level_label], big.mark = ","),
                                      format(cc[cat, site_level_label], big.mark = ","))

legend_lines <- c(
  "FIGURE LEGEND -- figS6_availability_annual_nee.png",
  strrep("=", 60), "",
  "TITLE: Supplementary Figure S6 -- Availability of annual NEE", "",
  "PUBLICATION LEGEND:",
  "Supplementary Figure S6. Availability of annual NEE across the snapshot, by usability",
  "category.", "",
  "DESCRIPTION:",
  sprintf("Two 100%%-stacked horizontal bars -- site-years (n=%s) and sites (n=%s) -- by",
          format(n_site_years, big.mark = ","), n_sites),
  "availability category: both VUT and CUT usable, VUT only, CUT only, or neither --",
  "'neither' split into 'no annual NEE in the product' (neither NEE_VUT_REF nor NEE_CUT_REF",
  "has a raw value for that site-year) and 'below quality rule' (a raw value exists but does",
  "not clear QC_THRESHOLD_YY=0.50). 'Usable' = QC >=",
  "QC_THRESHOLD_YY, own QC column per side. No in-bar count labels -- each category's exact",
  "count is given in its own legend entry instead, '<category> (<n> site-years, <n> sites)':",
  paste0("  ", fmt_counts("both usable")), paste0("  ", fmt_counts("VUT only usable")),
  paste0("  ", fmt_counts("CUT only usable")), paste0("  ", fmt_counts("no annual NEE in the product")),
  paste0("  ", fmt_counts("below quality rule")), "",
  "SITE-YEAR BAR vs. SITE BAR: at site-year resolution, 'both usable' means that single",
  "site-year's row has both VUT and CUT usable (necessarily the same year, since a site-year",
  "row is one site in one year). At site resolution, the two sides are judged independently",
  "across a site's own years: a site counts as 'both usable' if it has at least one year",
  "usable under VUT and at least one year usable under CUT, not necessarily the same year --",
  "likewise 'VUT only usable' / 'CUT only usable' mean only that side ever clears the rule",
  "across the site's years. 'Below quality rule' has 0 sites at site resolution: no site in",
  "the snapshot fails the 0.50 rule in every year it reports an annual NEE value -- any site",
  "with multiple reporting years has at least one usable year, or else has no reported value",
  "in a given year at all (landing that year in 'no annual NEE in the product' instead).", "",
  sprintf("SITE-YEAR COUNT: this figure uses %s site-years -- every row in the FLUXMET annual",
          format(n_site_years, big.mark = ",")),
  "table (dataset = 'FLUXMET'), regardless of quality. This differs from the 6,200 site-years",
  "in Figure 2, which counts site-years via compute_site_year_presence() -- any of several",
  "flux variables present in at least one month, filtered to Figure 2's own data-driven year",
  "window and to sites with a recognised IGBP class. The two figures use different site-year",
  "measures by design; this is not a discrepancy to be reconciled.", "",
  "NOTE: the u*-threshold method-success panel (CP/MP, panel b of the interim candidate's",
  "fig_tv4) is not included here.", "",
  "COLOUR CODING: 5-category palette; VUT/CUT-only categories reuse VUT_CUT_COLOURS",
  "(this script's own, as in Supplementary Figure S5).", "",
  "DATA SOURCE: table_stage4_site_year_master.csv, table_stage4_site_level_availability.csv",
  "(review/diagnostics/data_quality_uncertainty/), unchanged.", "",
  "PROVENANCE: promoted from the interim technical-validation candidate",
  "(scripts/technical_validation_interim.R, review/technical_validation_interim/fig_tv4_*),",
  "panel a only.", "",
  "SOURCE: scripts/generate_figS6_availability_annual_nee.R.",
  paste0("DIMENSIONS: ", NATURE_WIDTH_SINGLE_MM, " x 80 mm, 600 dpi PNG + vector PDF, Helvetica, white background.")
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))

write_output_metadata(
  saved$png,
  input_sources = c(file.path(DIAG_DIR, "table_stage4_site_year_master.csv"),
                     file.path(DIAG_DIR, "table_stage4_site_level_availability.csv")),
  notes = paste0("Supplementary Figure S6: availability of annual NEE, promoted from the interim ",
                  "technical-validation candidate's fig_tv4 panel a (u*-method-success panel b not ",
                  "included). Uses ", n_site_years, " annual FLUXMET rows, distinct from Figure 2's ",
                  "6,200 site-years (different measures -- see .legend.txt).")
)

msg("=== Done ===")
