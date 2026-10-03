## generate_whittaker_ed_three_flux.R
## Extended Data figure: Whittaker climate-space hexbins for three fluxes
## (NEE, GPP, TER) side by side -- same hexagons, points, and global
## ice-free-land contour overlay as Figure 2 (NEE panel reuses
## fig_whittaker_worldclim() directly, in fill_mode = "stepped", the same
## call Figure 2 makes). GPP and TER are not NEE-specific in
## fig_whittaker_worldclim(), so their panels are built directly here from
## compute_site_annual_fluxes()'s own per-site median values, reusing only
## the flux-agnostic pieces of the Whittaker machinery (WorldClim climate
## join, hex_regular equal-aspect binning, fig_whittaker_global_contour()).
##
## Output: review/figures/draft_manuscript_v1/SupFigs/
##   supp_whittaker_nee_gpp_ter.png/.pdf/.jpg + .legend.txt

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/figures/fig_climate.R")
source("R/units.R")
source("R/site_annual_fluxes.R")
source("R/nature_format.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(colorspace); library(duckdb); library(readr)
  library(patchwork)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Extended Data: Whittaker NEE/GPP/TER three-flux figure ===")

OUT_DIR  <- file.path("review", "figures", "draft_manuscript_v1", "SupFigs")
OUT_STEM <- file.path(OUT_DIR, "supp_whittaker_nee_gpp_ter")
fs::dir_create(OUT_DIR)

# ---- Step: load site-level annual fluxes (shared function) -------------------
db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
con <- dbConnect(duckdb(), db_path, read_only = TRUE)
site_fluxes <- compute_site_annual_fluxes(con, site_ids = NULL)
dbDisconnect(con, shutdown = TRUE)
site_summary <- site_fluxes$site_summary
msg("compute_site_annual_fluxes(): ", nrow(site_summary), " sites; qualifying NEE/GPP/RECO = ",
    sum(!is.na(site_summary$nee_median)), "/", sum(!is.na(site_summary$gpp_median)), "/",
    sum(!is.na(site_summary$reco_median)))

snap_file <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
shuttle_meta <- read_csv(snap_file, show_col_types = FALSE)

# ---- Step: cached global ice-free-land density grid (shared with Figure 2) ---
densitygrid_rds <- "data/processed/whittaker_global_density_grid.rds"
if (!file.exists(densitygrid_rds)) stop("Missing cache: ", densitygrid_rds, call. = FALSE)
density_grid <- readRDS(densitygrid_rds)

contour_result <- fig_whittaker_global_contour(style = WHITTAKER_STYLE, probs = c(0.95, 0.99),
                                                density_grid = density_grid)
contour_df <- contour_result$contour_df
contour_df$prob_label <- droplevels(contour_df$prob_label)
line_map <- c("95%" = "solid", "99%" = "dashed")
contour_layer <- function() {
  list(
    ggplot2::geom_path(
      data = contour_df,
      ggplot2::aes(x = .data$x, y = .data$y, group = interaction(.data$prob, .data$piece),
                   linetype = .data$prob_label),
      colour = "black", linewidth = nature_lwd(0.4), inherit.aes = FALSE
    ),
    ggplot2::scale_linetype_manual(values = line_map[levels(contour_df$prob_label)], guide = "none")
  )
}

POINT_COLOUR <- "#2B2B2B"
POINT_SIZE   <- 0.35
POINT_ALPHA  <- 0.35

# ---- Panel a: NEE -- reuses Figure 2's own stepped scale, via fig_whittaker_worldclim() ----
NEE_STEP_WIDTH  <- 100
NEE_SINK_END    <- -400
NEE_SOURCE_END  <- 200
nee_step_breaks  <- seq(NEE_SINK_END, NEE_SOURCE_END, by = NEE_STEP_WIDTH)
nee_step_colours <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0",
                       "#FDDBC7", "#F4A582", "#D6604D")

data_yy_nee <- site_fluxes$site_year |>
  dplyr::filter(!is.na(NEE)) |>
  dplyr::transmute(site_id, YEAR = as.integer(year), NEE_VUT_REF = NEE, NEE_CUT_REF = NA_real_)

style_ed <- utils::modifyList(WHITTAKER_STYLE, list(
  width_in = NATURE_ED_MAX_WIDTH_MM / 25.4, height_in = 76 / 25.4,
  axis_text_size = NATURE_BASE_PT, axis_title_size = NATURE_BASE_PT,
  legend_text_size = NATURE_SMALL_PT, legend_title_size = NATURE_BASE_PT,
  detail_text_size = NATURE_SMALL_PT / ggplot2::.pt
))

panel_nee <- fig_whittaker_worldclim(
  data_yy = data_yy_nee, site_meta = shuttle_meta, detail_label = NULL,
  style = style_ed, hex_regular = TRUE, points_in_front = TRUE,
  point_size = POINT_SIZE, point_colour = POINT_COLOUR, point_alpha = POINT_ALPHA,
  fill_mode = "stepped", step_breaks = nee_step_breaks, step_colours = nee_step_colours,
  detail_lines = character(0), detail_hjust = 0, detail_x_offset = 0.6
) + contour_layer() + nature_theme() + panel_letter("a") +
  ggplot2::theme(legend.key.size = grid::unit(5, "pt"), legend.position = "none")
n_nee <- sum(!is.na(data_yy_nee$site_id[!duplicated(data_yy_nee$site_id)]))
n_nee <- dplyr::n_distinct(data_yy_nee$site_id)
msg("Panel a (NEE): n = ", n_nee, " sites")

# ---- Panels b/c: GPP, TER -- built directly from site_summary, shared viridis scale ----
wc <- read_csv("data/snapshots/site_worldclim.csv", show_col_types = FALSE)

build_flux_climate_df <- function(value_col) {
  shuttle_meta |>
    dplyr::distinct(site_id, .keep_all = TRUE) |>
    dplyr::select(site_id, location_lat, location_long) |>
    dplyr::left_join(dplyr::select(wc, site_id, mat_worldclim, map_worldclim), by = "site_id") |>
    dplyr::left_join(dplyr::select(site_summary, site_id, value = dplyr::all_of(value_col)), by = "site_id") |>
    dplyr::filter(!is.na(mat_worldclim), !is.na(map_worldclim))
}

gpp_df <- build_flux_climate_df("gpp_median")
ter_df <- build_flux_climate_df("reco_median")
n_gpp  <- sum(!is.na(gpp_df$value))
n_ter  <- sum(!is.na(ter_df$value))
msg("Panel b (GPP): n = ", n_gpp, " sites; panel c (TER): n = ", n_ter, " sites")

hex_bins     <- 15
hex_binwidth <- c(diff(range(WHITTAKER_STYLE$xlim)) / hex_bins, diff(range(WHITTAKER_STYLE$ylim)) / hex_bins)
hex_ratio    <- diff(range(WHITTAKER_STYLE$xlim)) / diff(range(WHITTAKER_STYLE$ylim))

## Shared stepped viridis scale for GPP and TER (task 5, 2026-10-02): GPP and
## TER are non-negative g C m-2 yr-1 totals, so -- unlike Figure 2's NEE scale,
## which needs a "below" sink bin -- this one starts at zero. Named constants,
## one shared key for both panels (not a per-panel colourbar).
GPP_TER_STEP_WIDTH  <- 500
GPP_TER_MAX         <- 3000
gpp_ter_step_breaks <- seq(0, GPP_TER_MAX, by = GPP_TER_STEP_WIDTH)
gpp_ter_step_labels <- c(
  paste0(utils::head(gpp_ter_step_breaks, -1), " to ", gpp_ter_step_breaks[-1]),
  paste0("above ", GPP_TER_MAX)
)
gpp_ter_step_colours <- setNames(viridisLite::viridis(length(gpp_ter_step_labels)), gpp_ter_step_labels)
gpp_ter_unit_expr <- expression("GPP & TER (g C m"^{-2}*" yr"^{-1}*")")

flux_step_scale <- function() {
  ggplot2::scale_fill_manual(
    name = gpp_ter_unit_expr, values = gpp_ter_step_colours, breaks = gpp_ter_step_labels,
    na.translate = FALSE, drop = FALSE,
    guide = ggplot2::guide_legend(
      title.position = "top", nrow = 2,
      override.aes = list(colour = "black", linewidth = nature_lwd(NATURE_LINEWIDTH_MIN))
    )
  )
}

base_flux_panel <- function(df, letter) {
  ggplot2::ggplot(df, ggplot2::aes(x = mat_worldclim, y = map_worldclim, z = value)) +
    ggplot2::stat_summary_hex(
      mapping = ggplot2::aes(fill = ggplot2::after_stat(
        cut(value, breaks = c(gpp_ter_step_breaks, Inf), labels = gpp_ter_step_labels, right = FALSE)
      )),
      fun = function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE),
      bins = hex_bins, binwidth = hex_binwidth, alpha = 0.85
    ) +
    ## Points drawn in front of the hexagons, same style and order as panel a
    ## (fig_whittaker_worldclim(points_in_front = TRUE)) -- task 5.
    ggplot2::geom_point(ggplot2::aes(x = mat_worldclim, y = map_worldclim), inherit.aes = FALSE,
                         size = POINT_SIZE, colour = POINT_COLOUR, alpha = POINT_ALPHA) +
    contour_layer() +
    panel_letter(letter, hjust = -0.6, vjust = 1.8) +
    flux_step_scale() +
    ggplot2::coord_fixed(ratio = hex_ratio, xlim = WHITTAKER_STYLE$xlim, ylim = WHITTAKER_STYLE$ylim) +
    ## expand = c(0, 0): see R/figures/fig_climate.R's fig_whittaker_worldclim()
    ## for why coord_fixed(xlim=,ylim=) alone leaves a blank band (task 4).
    ggplot2::scale_x_continuous(labels = nature_minus_labels(), expand = c(0, 0),
                                 sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::scale_y_continuous(labels = nature_minus_labels(), expand = c(0, 0),
                                 sec.axis = ggplot2::dup_axis(name = NULL, labels = NULL)) +
    ggplot2::labs(
      x = expression("Mean Annual Temperature (" * degree * "C)"),
      y = NULL
    ) +
    ggplot2::theme_classic(base_size = NATURE_BASE_PT) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = nature_lwd(NATURE_LINEWIDTH_MAX)),
      panel.background = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.ticks = ggplot2::element_line(colour = "black"),
      axis.ticks.length = grid::unit(-4, "pt"),
      legend.position = "none"
    ) +
    nature_theme()
}

panel_gpp <- base_flux_panel(gpp_df, "b")
panel_ter <- base_flux_panel(ter_df, "c")

## Hexagons-per-step report (required session report item) -- classify each
## panel's own drawn hexagon medians with the same breaks/labels used above.
hex_layer_idx <- function(p) which(vapply(p$layers, function(l) inherits(l$stat, "StatSummaryHex"), logical(1)))[1]
.report_hex_steps <- function(p, label) {
  vals <- ggplot2::ggplot_build(p)$data[[hex_layer_idx(p)]]$value
  classed <- cut(vals, breaks = c(gpp_ter_step_breaks, Inf), labels = gpp_ter_step_labels, right = FALSE)
  counts <- table(classed)
  msg(label, " hexagons per step: ", paste(names(counts), "=", as.integer(counts), collapse = "; "))
}
.report_hex_steps(panel_gpp, "GPP")
.report_hex_steps(panel_ter, "TER")

## One shared legend for panels b/c, in its OWN dedicated row via
## patchwork::guide_area() -- task 5. The simpler `guides = "collect"` +
## theme(legend.position = "bottom") (no guide_area()) was tried first and
## rejected: confirmed directly that patchwork sized the plot panels'
## shared column width to leave room for the collected legend BESIDE them
## rather than below them, crushing all three panels into a sliver and
## leaving the wide multi-swatch legend stretched across most of the
## canvas. An explicit guide_area() row avoids that: the legend renders in
## its own reserved band, sized independently of the panel row's heights.
panel_ter <- panel_ter + ggplot2::theme(legend.position = "bottom")

# ---- Assemble 3-panel row + dedicated legend row -------------------------------
## Only panel_ter carries a visible ("bottom") legend.position -- panel_nee and
## panel_gpp are both "none" -- so guides = "collect" has exactly one guide to
## hoist into guide_area(), with no risk of re-enabling the other two panels'
## suppressed legends (unlike the `&` patchwork operator applied with theme(),
## which would overwrite all three panels' legend.position, undoing the other
## two).
combo <- (panel_nee | panel_gpp | panel_ter) / patchwork::guide_area() +
  patchwork::plot_layout(heights = c(1, 0.22), guides = "collect")

saved <- save_nature_figure(combo, OUT_STEM, width_mm = NATURE_ED_MAX_WIDTH_MM, height_mm = 95,
                             extended_data = TRUE)
msg("Saved: ", saved$png, ", ", saved$pdf, ", ", saved$jpeg)

# ---- Legend --------------------------------------------------------------------
legend_lines <- c(
  "FIGURE LEGEND — supp_whittaker_nee_gpp_ter.png",
  strrep("=", 60), "",
  "TITLE: Extended Data Figure — Whittaker climate-space distribution of the current",
  "FLUXNET network for three fluxes: net ecosystem exchange, gross primary productivity,",
  "and total ecosystem respiration", "",
  "DESCRIPTION:",
  "Three panels in a row (a NEE, b GPP, c TER), each the same hexagonal-binned Whittaker",
  "climate-space plot as Figure 2 (mean annual temperature, WorldClim v2.1 BIO1, by mean",
  "annual precipitation, BIO12), with the same per-site points overlaid (dark charcoal,",
  "alpha 0.35, drawn in front of the hexagons) and the same global ice-free-land 95%",
  "(solid) / 99% (dashed) highest-density-region contour overlay as Figure 2 -- identical",
  "geometry and registration, since both reuse the same cached density grid",
  "(data/processed/whittaker_global_density_grid.rds).",
  "",
  "Values come from R/site_annual_fluxes.R::compute_site_annual_fluxes(): each tower's",
  "value is the median of its annual values passing QC_THRESHOLD_YY, per-site VUT/CUT, the",
  "same rule as Figure 4 and Figure 2. A hexagon's colour is the median, across the towers",
  "falling in that climate-space bin, of each tower's own median value (median of site",
  "medians) -- a hexagon with no towers is white (not drawn).",
  "",
  "PANELS:",
  paste0("  a NEE — same stepped ColorBrewer RdBu scale as Figure 2 (8 classes, no middle"),
  paste0("    class, 100 g C m⁻² yr⁻¹ steps, endpoints −400/200; see Figure 2's legend for"),
  "    the full definition). Own legend omitted here (identical to Figure 2's) --",
  paste0("    n = ", n_nee, " sites."),
  paste0("  b GPP — stepped viridis scale, shared with panel c (see COLOUR SCALE, below) --"),
  paste0("    n = ", n_gpp, " sites."),
  paste0("  c TER — same shared stepped viridis scale as panel b — n = ", n_ter, " sites."),
  "",
  "COLOUR SCALE (panels b, c):",
  "Stepped viridis scale, ONE shared key for both GPP and TER (not duplicated per panel,",
  paste0("not independently rescaled): ", GPP_TER_STEP_WIDTH, " g C m⁻² yr⁻¹ steps from 0 to ",
         GPP_TER_MAX, ", plus a final \"above ", GPP_TER_MAX, "\" bin -- ",
         length(gpp_ter_step_labels), " classes total (named constants GPP_TER_STEP_WIDTH,",
         " GPP_TER_MAX in the script)."),
  "Panel a (NEE) uses its own, unrelated stepped scale -- GPP/TER and NEE values are never",
  "compared on the same colour scale.",
  "",
  "AXES:",
  "  X (all panels) — Mean Annual Temperature (°C), fixed range -15 to 35",
  "  Y (panel a only; panels b/c share panel a's y axis visually but do not repeat the",
  "    axis title or text) — Mean Annual Precipitation (mm yr⁻¹), fixed range 0 to 4000",
  "Four-sided black tick marks (inward) via a duplicated secondary axis, no gridlines,",
  "solid black panel border, identical MAT:MAP physical-length ratio in all three panels",
  "(coord_fixed(), same construction as Figure 2's hex_regular = TRUE).",
  "",
  "SOURCE: scripts/generate_whittaker_ed_three_flux.R. Panel a: fig_whittaker_worldclim()",
  "(R/figures/fig_climate.R), fill_mode = \"stepped\" (same call as Figure 2). Panels b/c:",
  "built directly in the script from compute_site_annual_fluxes()'s site-level medians --",
  "not NEE-specific, so not routed through fig_whittaker_worldclim(). Contour overlay:",
  "fig_whittaker_global_contour(), shared with Figure 2.",
  paste0("DIMENSIONS: ", NATURE_ED_MAX_WIDTH_MM, " x 95 mm, 600 dpi PNG + vector PDF + 300 ppi JPEG,"),
  "Helvetica, white background. All text 5-7pt except the bold lower-case panel letters (8pt)."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))
msg("=== Done ===")
