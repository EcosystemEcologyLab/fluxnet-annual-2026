# AMFOct25_poster.R
# Poster prep pipeline: graphics standards, data load, RData save, QA/QC

suppressPackageStartupMessages({
  # Core Tidyverse packages
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(stringr)
  library(purrr)
  library(tidyr)
  
  # File management
  library(here)
  
  # Plotting Enhancements
  library(ggtext)      # For markdown in plot labels
  library(colorspace)  # For color palettes/manipulation (e.g., qualitative_hcl)
  library(patchwork)   # For combining multiple plots
  
  # Spatial/Mapping (required for section 5)
  library(sf)
  library(rnaturalearth)
  library(cols4all)
  
  # Saving Plots (SVG format)
  library(svglite)     # For saving plots as SVG
})

# ---- Your utilities ----
# Provides: discover_AMF_files(), load_fluxnet_data(), load_fluxnet_metadata(),
#           flag_bad_gapfilled(), year_from_df(), etc.
source("R/fcn_utility_FLUXNET.R")
source("R/poster_constants.R")



# ================================
# 1) GRAPHICS STANDARDS (poster)
# ================================
poster_base_size <- 18
poster_pt_size   <- 3.5
poster_line_size <- 0.9
poster_stroke    <- 0.7

poster_theme <- function(base_size = poster_base_size) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      axis.title.x  = element_markdown(margin = margin(t = 6)),
      axis.title.y  = element_markdown(margin = margin(r = 6)),
      axis.text     = element_text(size = base_size * 0.9),
      axis.ticks    = element_line(linewidth = poster_line_size),
      axis.line     = element_line(linewidth = poster_line_size),
      legend.title  = element_text(size = base_size * 0.95),
      legend.text   = element_text(size = base_size * 0.9),
      strip.text    = element_text(size = base_size * 0.95),
      plot.title    = element_text(size = base_size * 1.05, face = "bold"),
      plot.subtitle = element_text(size = base_size * 0.95, margin = margin(b = 6))
    )
}
theme_set(poster_theme())

save_plot_draft <- function(p, filename, width = 8, height = 10, dpi = 300) {
  dir.create(here("plots"), showWarnings = FALSE, recursive = TRUE)
  ggsave(here("plots", filename), p, width = width, height = height,
         units = "in", dpi = dpi, limitsize = FALSE)
}

save_plot_final <- function(p, filename_stub, width = 8, height = 10, dpi = 600) {
  dir.create(here("AGU25_FLUXNET"), showWarnings = FALSE, recursive = TRUE)
  ggsave(here("AGU25", paste0(filename_stub, ".png")), p,
         width = width, height = height, units = "in", dpi = dpi, limitsize = FALSE)
  ggsave(here("AGU25", paste0(filename_stub, ".pdf")), p,
         width = width, height = height, units = "in", device = cairo_pdf, limitsize = FALSE)
}



# ============================
# 2) LOAD DATA FROM MANIFEST / CACHE
# ============================
# For now: use cached data
 load(here::here("data/FLUXNET", "AMFOct25_daily.RData"))
# load(here::here("data/FLUXNET", "AMFOct25_annual.RData"))


## Load data
metadata <- load_fluxnet_metadata()
manifest <-
  discover_AMF_files(data_dir = here("data/FLUXNET/AMF")) %>%
  filter(dataset == "FULLSET", time_integral == "DD") %>%
  # Deduplicate by keeping just one file per site—the one with the later end_year
  group_by(site) %>%
  filter(end_year == max(end_year)) %>% 
  ungroup()

# Feel free to replace this with whatever read function you're using.  I'd just
# recommend selecting only the site, date, and GPP columns to free up memory
# though.
flux_vars <- "GPP_NT_VUT_MEAN" # This could be a vetor

# apparently not all files have a GPP_NT_VUT_MEAN column, so uses any_of()
daily <- map2(manifest$path, manifest$site, \(path, site) {
  read_csv(
    path,
    col_select = c(TIMESTAMP, any_of(flux_vars)),
    show_col_types = FALSE
  ) %>%
    mutate(site = unique(site), .before = 1)
}) %>%
  list_rbind() %>%
  mutate(
    across(any_of(flux_vars), \(x) na_if(x, -9999)),
    date = ymd(TIMESTAMP),
    .after = site
  ) %>%
  select(-TIMESTAMP) %>%
  left_join(metadata %>% select(site, LOCATION_LAT), by = join_by(site))


# ============================
# 3) QA/QC GATE (% GAP-FILLED)
# ============================
lab_precip_annual <- "Precipitation (mm y<sup>-1</sup>)"
lab_gpp_daily     <- "GPP (g C m<sup>-2</sup> d<sup>-1</sup>)"
lab_nee_daily     <- "NEE (g C m<sup>-2</sup> d<sup>-1</sup>)"
lab_reco_daily    <- "RECO (g C m<sup>-2</sup> d<sup>-1</sup>)"
lab_gpp_annual    <- "GPP (g C m<sup>-2</sup> yr<sup>-1</sup>)"
lab_nee_annual    <- "NEE (g C m<sup>-2</sup> yr<sup>-1</sup>)"
lab_reco_annual   <- "RECO (g C m<sup>-2</sup> yr<sup>-1</sup>)"

# User-tunable knobs
qc_gate_vars        <- c("NEE_VUT_REF")
max_gapfilled_bad   <- 0.75
drop_if_qc_missing  <- TRUE

annual_flagged <- annual %>%
  flag_bad_gapfilled(
    gate_vars       = qc_gate_vars,
    max_gapfilled   = max_gapfilled_bad,
    drop_if_missing = drop_if_qc_missing
  )

annual_clean <- annual_flagged %>% filter(!is_bad)

annual_excluded <- annual_flagged %>%
  filter(is_bad) %>%
  select(
    site, year, pct_gapfilled,
    dplyr::all_of(paste0(qc_gate_vars, "_QC")),
    dplyr::any_of(c(qc_gate_vars, paste0(qc_gate_vars, "_JOINTUNC")))
  )

message(sprintf("QA/QC: kept %s of %s annual rows (%.1f%%).",
                scales::comma(nrow(annual_clean)),
                scales::comma(nrow(annual_flagged)),
                100 * nrow(annual_clean) / nrow(annual_flagged)))

# Quick precip sanity check (draft only)
p_qaqc_precip <- ggplot(annual_clean, aes(P_F)) +
  geom_histogram(linewidth = poster_stroke) +
  labs(x = lab_precip_annual, y = "Count", title = "Annual precipitation after QA/QC")
save_plot_draft(p_qaqc_precip, "qaqc_precip_hist.png", width = 8, height = 6, dpi = 300)

save(annual_clean, annual_excluded,
     file = here("data/FLUXNET", "AMFOct25_annual_clean.RData"))

# ----- DAILY QA/QC -----
daily_flagged <- daily %>%
  flag_bad_gapfilled(
    gate_vars       = qc_gate_vars,
    max_gapfilled   = max_gapfilled_bad,
    drop_if_missing = drop_if_qc_missing
  )

daily_clean <- daily_flagged %>% filter(!is_bad)

daily_excluded <- daily_flagged %>%
  filter(is_bad) %>%
  select(
    site, date, pct_gapfilled,
    dplyr::all_of(paste0(qc_gate_vars, "_QC")),
    dplyr::any_of(c(qc_gate_vars, paste0(qc_gate_vars, "_JOINTUNC")))
  )

message(sprintf("QA/QC (daily): kept %s of %s rows (%.1f%%).",
                scales::comma(nrow(daily_clean)),
                scales::comma(nrow(daily_flagged)),
                100 * nrow(daily_clean) / nrow(daily_flagged)))

p_qaqc_daily_nee <- ggplot(daily_clean, aes(NEE_VUT_REF)) +
  geom_histogram(linewidth = poster_stroke, bins = 60) +
  labs(x = lab_nee_daily, y = "Count", title = "Daily NEE after QA/QC")
save_plot_draft(p_qaqc_daily_nee, "qaqc_daily_nee_hist.png", width = 8, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# Limit datasets to North America by latitude (approx. 7–83°N)
# -----------------------------------------------------------------------------
lat_min <- 7
lat_max <- 83

annual <- annual %>%
  dplyr::filter(LOCATION_LAT >= lat_min, LOCATION_LAT <= lat_max)

daily <- daily %>%
  dplyr::filter(LOCATION_LAT >= lat_min, LOCATION_LAT <= lat_max)

message(sprintf("Retained %d annual and %d daily records within %.0f–%.0f°N.",
                nrow(annual), nrow(daily), lat_min, lat_max))

source("R/fcn_plot_FLUXNET.R")

# ============================
# 4) PLOTS – Annual IGBP summary
# ============================
flux_var <- "NEE_VUT_REF"  # <— define once and reuse

cat("Generating flux summary by IGBP...\n")
plots_igbp <- plot_flux_by_igbp(annual_clean, flux_var = flux_var)
print(plots_igbp$composite_plot)

fs::dir_create("AMFOct25_Poster")

png_file <- sprintf("AMFOct25_Poster/flux_by_igbp_%s_poster.png", flux_var)
pdf_file <- sprintf("AMFOct25_Poster/flux_by_igbp_%s_poster.pdf", flux_var)

ggplot2::ggsave(png_file, plot = plots_igbp$composite_plot, width = 8, height = 10, dpi = 600)
ggplot2::ggsave(pdf_file, plot = plots_igbp$composite_plot, width = 8, height = 10, device = cairo_pdf)

message("Poster figure saved: ", png_file, " and ", pdf_file)

# Caption
if (grepl("^NEE", flux_var)) {
  var_long  <- "net ecosystem exchange (NEE)"
  var_units <- "g C m^-2 yr^-1"
} else if (grepl("^GPP", flux_var)) {
  var_long  <- "gross primary production (GPP)"
  var_units <- "g C m^-2 yr^-1"
} else if (grepl("^RECO", flux_var)) {
  var_long  <- "ecosystem respiration (Reco)"
  var_units <- "g C m^-2 yr^-1"
} else if (grepl("^LE", flux_var)) {
  var_long  <- "latent heat flux (LE)"
  var_units <- "W m^-2"
} else {
  var_long  <- flux_var
  var_units <- ""
}

nsites     <- annual_clean |> dplyr::distinct(site) |> nrow()
nsiteyears <- nrow(annual_clean)
gap_rule   <- if (exists("max_gapfilled_bad")) max_gapfilled_bad else 0.75
gate_vars  <- if (exists("qc_gate_vars")) paste(qc_gate_vars, collapse = ", ") else "NEE_VUT_REF, PA_F"

caption <- paste0(
  "Figure. Distribution of annual ", var_long,
  if (nzchar(var_units)) paste0(" (", var_units, ")") else "",
  " across IGBP classes for North American AmeriFlux/FLUXNET sites (",
  nsites, " sites; ", nsiteyears, " site-years). ",
  "Top: boxplots (IQR) with individual site-years (gray points) shown behind. ",
  "Middle: median by IGBP. Bottom: number of contributing site-years. ",
  "Data use FLUXNET FULLSET (YY; and ICOS L2 where present), restricted to ",
  lat_min, "–", lat_max, "°N and QA/QC’d to exclude records with >",
  round(100 * gap_rule), "% gap-filled in the gating variables (", gate_vars, "). ",
  "Years vary by site; points represent site-years."
)

cap_file <- sprintf("AMFOct25_Poster/caption_flux_by_igbp_%s.txt", flux_var)
writeLines(caption, cap_file)
message("Wrote caption to: ", cap_file)


# ============================
# 4b) Seasonal weekly cycles by IGBP
# ============================

# poster_geom_point / poster_geom_line can be defined here if not in your util file:
# poster_geom_point <- function(...) ggplot2::geom_point(size = 3.5, stroke = 0.7, alpha = 0.8, ...)
# poster_geom_line  <- function(...) ggplot2::geom_line(linewidth = 0.9, alpha = 0.9, ...)

min_sites <- 10  # only keep IGBPs with at least this many sites

igbp_site_counts <- daily_clean %>%
  dplyr::distinct(site, IGBP) %>%
  dplyr::count(IGBP, name = "n_sites")

eligible_igbp <- igbp_site_counts %>%
  dplyr::filter(n_sites >= min_sites) %>%
  dplyr::arrange(IGBP) %>%
  dplyr::pull(IGBP)

print(igbp_site_counts)
cat("Including IGBP:", paste(eligible_igbp, collapse = ", "), "\n")

flux_weekly <- daily_clean %>%
  dplyr::filter(IGBP %in% eligible_igbp) %>%
  dplyr::mutate(week = lubridate::isoweek(date), .after = doy) %>%
  dplyr::group_by(IGBP, week) %>%
  dplyr::summarize(
    GPP_NT_VUT_REF  = median(GPP_NT_VUT_REF,  na.rm = TRUE),
    RECO_NT_VUT_REF = median(RECO_NT_VUT_REF, na.rm = TRUE),
    NEE_VUT_REF     = median(NEE_VUT_REF,     na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  dplyr::mutate(fake_date = lubridate::ymd("2020-01-01") + lubridate::weeks(week))

# Stable colors/shapes (printer-friendly), ordered by eligible IGBPs
igbp_levels <- sort(unique(flux_weekly$IGBP))
pal <- colorspace::qualitative_hcl(length(igbp_levels), palette = "Dark 3")
names(pal) <- igbp_levels
shape_vec <- c(16,17,15,3,7,8,18,0,1,2,4,5,6,9,10,11)
shp <- setNames(shape_vec[seq_along(igbp_levels)], igbp_levels)

flux_weekly <- flux_weekly %>%
  dplyr::mutate(IGBP = factor(IGBP, levels = igbp_levels))
# Helper for month ticks – define this BEFORE add_top_right_axes is used
month_breaks <- scales::breaks_width("2 months")

y_axis_parallel <- theme(
  axis.title.y = ggtext::element_markdown(angle = 90, vjust = 0.5, hjust = 0.5, size = 16)
)

# GPP
p_seasonal_gpp <- ggplot2::ggplot(
  flux_weekly, ggplot2::aes(fake_date, GPP_NT_VUT_REF, color = IGBP, shape = IGBP)
) +
  poster_geom_point() + poster_geom_line() +
  ggplot2::scale_color_manual(values = pal, guide = ggplot2::guide_legend(ncol = 2)) +
  ggplot2::scale_shape_manual(values = shp) +
  ggplot2::labs(x = "", y = lab_gpp_daily, color = "IGBP", shape = "IGBP") +
  ggplot2::theme(axis.title.y = ggtext::element_markdown())
p_seasonal_gpp <- add_top_right_axes(p_seasonal_gpp)

# NEE
p_seasonal_nee <- ggplot2::ggplot(
  flux_weekly, ggplot2::aes(fake_date, NEE_VUT_REF, color = IGBP, shape = IGBP)
) +
  poster_geom_point() + poster_geom_line() +
  ggplot2::scale_color_manual(values = pal, guide = ggplot2::guide_legend(ncol = 2)) +
  ggplot2::scale_shape_manual(values = shp) +
  ggplot2::labs(x = "", y = lab_nee_daily, color = "IGBP", shape = "IGBP") +
  ggplot2::theme(axis.title.y = ggtext::element_markdown())
p_seasonal_nee <- add_top_right_axes(p_seasonal_nee)

# RECO
p_seasonal_reco <- ggplot2::ggplot(
  flux_weekly, ggplot2::aes(fake_date, RECO_NT_VUT_REF, color = IGBP, shape = IGBP)
) +
  poster_geom_point() + poster_geom_line() +
  ggplot2::scale_color_manual(values = pal, guide = ggplot2::guide_legend(ncol = 2)) +
  ggplot2::scale_shape_manual(values = shp) +
  ggplot2::labs(x = "", y = lab_reco_daily, color = "IGBP", shape = "IGBP") +
  ggplot2::theme(axis.title.y = ggtext::element_markdown())
p_seasonal_reco <- add_top_right_axes(p_seasonal_reco)

# Apply parallel y-axis styling
p_seasonal_gpp  <- p_seasonal_gpp  + y_axis_parallel
p_seasonal_nee  <- p_seasonal_nee  + y_axis_parallel
p_seasonal_reco <- p_seasonal_reco + y_axis_parallel

# View interactively
p_seasonal_gpp
p_seasonal_nee
p_seasonal_reco


library(patchwork)

tight_top <- ggplot2::theme(
  axis.text.x   = ggplot2::element_blank(),
  axis.title.x  = ggplot2::element_blank(),
  axis.ticks.x  = ggplot2::element_blank(),
  plot.margin   = ggplot2::margin(b = 2, t = 2, l = 4, r = 4)
)
tight_mid <- ggplot2::theme(
  axis.text.x   = ggplot2::element_blank(),
  axis.title.x  = ggplot2::element_blank(),
  axis.ticks.x  = ggplot2::element_blank(),
  plot.margin   = ggplot2::margin(b = 2, t = 2, l = 4, r = 4)
)
tight_bottom <- ggplot2::theme(
  plot.margin   = ggplot2::margin(t = 2, b = 2, l = 4, r = 4)
)


p_seasonal_vertical <-
  (p_seasonal_nee  + tight_top    + y_axis_parallel) /
  (p_seasonal_gpp  + tight_mid    + y_axis_parallel) /
  (p_seasonal_reco + tight_bottom + y_axis_parallel) +
  patchwork::plot_layout(heights = c(1, 1, 1), guides = "collect") &
  ggplot2::theme(
    legend.position = "bottom",
    plot.margin = ggplot2::margin(2, 4, 2, 4)
  )

p_seasonal_vertical

# Optional: make sure svglite is available
# install.packages("svglite")
library(svglite)
# Dimensions (inches)
w <- 8
h_single <- 3.3
h_stack  <- 10

# Helper to save both formats
save_plot <- function(plot, name, height) {
  ggsave(paste0("AMFOct25_Poster/", name, ".png"),
         plot = plot, width = w, height = height, dpi = 600)
  ggsave(paste0("AMFOct25_Poster/", name, ".svg"),
         plot = plot, width = w, height = height, device = svglite::svglite)
}

# Individual plots
save_plot(p_seasonal_gpp,  "seasonal_gpp_poster",  h_single)
save_plot(p_seasonal_nee,  "seasonal_nee_poster",  h_single)
save_plot(p_seasonal_reco, "seasonal_reco_poster", h_single)

# Stacked composite (~8×10 in total)
save_plot(p_seasonal_vertical, "seasonal_stack_poster", h_stack)

message("Saved high-resolution PNG + editable SVG plots to AMFOct25_Poster/")

# ── Create and save caption for seasonal stacked poster ─────────────────────

# Reuse existing objects for metadata
nsites      <- daily_clean |> dplyr::distinct(site) |> nrow()
nsiteyears  <- daily_clean |> nrow()
lat_min     <- 7   # match your current domain filter
lat_max     <- 83
gap_rule    <- if (exists("max_gapfilled_bad")) max_gapfilled_bad else 0.75
gate_vars   <- if (exists("qc_gate_vars")) paste(qc_gate_vars, collapse = ", ") else "NEE_VUT_REF, PA_F"
min_sites   <- if (exists("min_sites")) min_sites else 10

caption <- paste0(
  "Figure. Seasonal cycles of daily median gross primary production (GPP), ",
  "ecosystem respiration (Reco), and net ecosystem exchange (NEE) for North American ",
  "AmeriFlux/FLUXNET sites (", nsites, " sites; ", nsiteyears, " site-days). ",
  "Weekly medians were computed within IGBP vegetation classes, limited to those ",
  "with at least ", min_sites, " sites. Lines and points indicate median fluxes across sites; ",
  "shading not shown for clarity. Axes include top/right ticks for poster readability. ",
  "Data are restricted to ", lat_min, "–", lat_max, "°N and filtered using QA/QC thresholds ",
  "excluding records with >", round(100*gap_rule), "% gap-filled data (", gate_vars, ")."
)

cap_file <- "AMFOct25_Poster/caption_seasonal_stack.txt"
writeLines(caption, cap_file)
message("Wrote caption to: ", cap_file)



# ============================
# 5) CURRENT vs HISTORICAL MAPS
# ============================# ============================================================
# NEE maps (explicit vars): historical mean vs (recent_k − historical)
# Excluding known outliers: US-DS3, US-Dmg, US-Bi2
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(cols4all)
  library(patchwork)
  library(ggtext)
})

# -----------------------
# Adjustable parameters
# -----------------------
recent_k      <- 5
palette_div   <- "scico.berlin"   # blue ↔ white ↔ red (high contrast)
fade_fraction <- 0.10
alpha_faded   <- 0.25
alpha_full    <- 1
xlim_na       <- c(-170, -50)
ylim_na       <- c(   5,   85)

# Sites to exclude
exclude_sites <- c("US-DS3", "US-Dmg", "US-Bi2")

# -----------------------
# Ensure columns exist
# -----------------------
required_cols <- c("GPP_NT_VUT_REF","RECO_NT_VUT_REF","NEE_VUT_REF")
missing <- setdiff(required_cols, names(annual_clean))
if (length(missing))
  stop("Missing required columns: ", paste(missing, collapse = ", "))

# -----------------------
# Summaries per site
# -----------------------
nee_recent_hist <- annual_clean %>%
  filter(!site %in% exclude_sites) %>%
  select(site, year, LOCATION_LAT, LOCATION_LONG, NEE_VUT_REF) %>%
  group_by(site) %>%
  mutate(
    yrs_finite = list(sort(unique(year[is.finite(NEE_VUT_REF)]))),
    recent_set = list(tail(yrs_finite[[1]], recent_k)),
    is_recent  = year %in% recent_set[[1]]
  ) %>%
  ungroup()

nee_site_summ <- nee_recent_hist %>%
  group_by(site) %>%
  summarize(
    n_recent      = sum(is_recent & is.finite(NEE_VUT_REF)),
    n_hist        = sum(!is_recent & is.finite(NEE_VUT_REF)),
    mean_recent   = if (n_recent > 0) mean(NEE_VUT_REF[is_recent],  na.rm = TRUE) else NA_real_,
    mean_hist     = if (n_hist   > 0) mean(NEE_VUT_REF[!is_recent], na.rm = TRUE) else NA_real_,
    LOCATION_LAT  = first(LOCATION_LAT),
    LOCATION_LONG = first(LOCATION_LONG),
    .groups = "drop"
  ) %>%
  mutate(diff_nee = mean_recent - mean_hist) %>%
  filter(is.finite(LOCATION_LONG), is.finite(LOCATION_LAT)) %>%
  filter(is.finite(mean_hist) & is.finite(mean_recent))



# -----------------------
# Basemap and limits
# -----------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica")

nee_site_summ <- nee_site_summ %>%
  mutate(
    alpha_hist = 0.8,
    alpha_diff = 0.8
  )
nee_pts <- st_as_sf(nee_site_summ, coords = c("LOCATION_LONG","LOCATION_LAT"), crs = 4326)

robust_limits <- function(x, mult = 1.2, probs = c(0.01, 0.99)) {
  q <- quantile(x[is.finite(x)], probs = probs, na.rm = TRUE)
  lim <- max(abs(q))
  c(-lim, lim) * mult
}
hist_limit <- robust_limits(nee_site_summ$mean_hist)
diff_limit <- robust_limits(nee_site_summ$diff_nee)

# -----------------------
# Historical mean map
# -----------------------
p_hist <- ggplot() +
  geom_sf(data = world, fill = "gray95", linewidth = 0.6) +
  geom_sf(
    data = nee_pts,
    aes(color = mean_hist, alpha = alpha_hist),
    size = 2, na.rm = TRUE
  ) +
  scale_alpha_identity() +
  scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0, limits = hist_limit) +
  coord_sf(xlim = xlim_na, ylim = ylim_na, expand = FALSE) +
  labs(title = "Historical mean NEE",
       color = "NEE (g C m⁻² y⁻¹)") +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.title = element_markdown(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# -----------------------
# Recent minus historical map
# -----------------------
p_diff <- ggplot() +
  geom_sf(data = world, fill = "gray95", linewidth = 0.6) +
  geom_sf(
    data = nee_pts,
    aes(color = diff_nee, alpha = alpha_diff),
    size = 2, na.rm = TRUE
  ) +
  scale_alpha_identity() +
  scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0, limits = hist_limit)+
  coord_sf(xlim = xlim_na, ylim = ylim_na, expand = FALSE) +
  labs(title = paste0("ΔNEE (Last ", recent_k, " yrs − Long-term mean)"),
       color = "ΔNEE (g C m⁻² y⁻¹)") +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.title = element_markdown(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# -----------------------
# Side-by-side composite
# -----------------------
p_side <- p_hist | p_diff +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_side


# ============================================================
# Base (>=10 yrs) + Diff (2020–2023 subset) with diagnostics
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(sf); library(rnaturalearth); library(patchwork); library(ggtext)
})

# Parameters
exclude_sites <- c("US-DS3", "US-Dmg", "US-Bi2")
xlim_na <- c(-170, -50); ylim_na <- c(5, 85)
alpha_pt <- 0.8
recent_years <- 2020:2023

# Base dataframe
df <- annual_clean %>%
  filter(!site %in% exclude_sites) %>%
  select(site, year, LOCATION_LAT, LOCATION_LONG, NEE_VUT_REF)

# ---------- Consolidated per-site stats ----------
stats <- df %>%
  group_by(site) %>%
  summarise(
    n_years   = sum(is.finite(NEE_VUT_REF)),
    have_recent = all(recent_years %in% year[is.finite(NEE_VUT_REF)]),
    mean_hist_full        = mean(NEE_VUT_REF, na.rm = TRUE),
    mean_recent_2020_2023 = mean(NEE_VUT_REF[year %in% recent_years], na.rm = TRUE),
    mean_hist_pre2020     = mean(NEE_VUT_REF[year < 2020],           na.rm = TRUE),
    LOCATION_LAT  = first(LOCATION_LAT),
    LOCATION_LONG = first(LOCATION_LONG),
    .groups = "drop"
  )

# Base map: sites with >=10 years (full-record mean)
base_sites <- stats %>%
  filter(
    n_years >= 10,
    is.finite(mean_hist_full),
    is.finite(LOCATION_LAT), is.finite(LOCATION_LONG)
  ) %>%
  st_as_sf(coords = c("LOCATION_LONG","LOCATION_LAT"), crs = 4326)

# Diff map: subset of base (>=10 yrs) that ALSO have all recent years 2020–2023
diff_sites <- stats %>%
  filter(
    n_years >= 10, have_recent,
    is.finite(mean_recent_2020_2023),
    is.finite(mean_hist_pre2020),
    is.finite(LOCATION_LAT), is.finite(LOCATION_LONG)
  ) %>%
  mutate(diff_nee_fixed = mean_recent_2020_2023 - mean_hist_pre2020) %>%
  st_as_sf(coords = c("LOCATION_LONG","LOCATION_LAT"), crs = 4326)

# ---------- Diagnostics ----------
base_ids <- base_sites$site
diff_ids <- diff_sites$site
common_ids <- intersect(base_ids, diff_ids)

cat("=====================================================\n")
cat("Diagnostic: Site Counts\n")
cat("-----------------------------------------------------\n")
cat("Total sites in BASE (>=10 yrs):", length(base_ids), "\n")
cat("Total sites in DIFF (>=10 yrs & 2020–2023 complete):", length(diff_ids), "\n")
cat("Sites in BOTH maps:", length(common_ids), "\n")
cat("Sites ONLY in BASE:", length(setdiff(base_ids, diff_ids)), "\n")
cat("Sites ONLY in DIFF:", length(setdiff(diff_ids, base_ids)), "\n")

if (length(setdiff(diff_ids, base_ids)) > 0) {
  cat("\nThese sites are in DIFF but not in BASE:\n")
  print(setdiff(diff_ids, base_ids))
}
cat("=====================================================\n\n")

# ---------- Basemap ----------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(continent != "Antarctica")

# ---------- Limits ----------
robust_limits <- function(x, mult = 1.2, probs = c(0.01, 0.99)) {
  q <- stats::quantile(x[is.finite(x)], probs = probs, na.rm = TRUE)
  lim <- max(abs(q))
  c(-lim, lim) * mult
}
base_limit <- robust_limits(base_sites$mean_hist_full)
diff_limit <- robust_limits(diff_sites$diff_nee_fixed)

# ---------- Plots ----------
p_base10 <- ggplot() +
  geom_sf(data = world, fill = "gray95", linewidth = 0.6) +
  geom_sf(data = base_sites, aes(color = mean_hist_full), alpha = alpha_pt, size = 2) +
  scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0, limits = base_limit) +
  coord_sf(xlim = xlim_na, ylim = ylim_na, expand = FALSE) +
  labs(title = "Historical mean NEE (sites with ≥10 years)", color = "NEE (g C m⁻² y⁻¹)") +
  theme_classic(base_size = 18) +
  theme(legend.position = "bottom", legend.title = element_markdown(size = 16),
        axis.title.x = element_blank(), axis.title.y = element_blank())

p_diff_fixed <- ggplot() +
  geom_sf(data = world, fill = "gray95", linewidth = 0.6) +
  geom_sf(data = diff_sites, aes(color = diff_nee_fixed), alpha = alpha_pt, size = 2) +
  scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0, limits = diff_limit) +
  coord_sf(xlim = xlim_na, ylim = ylim_na, expand = FALSE) +
  labs(title = "ΔNEE: mean(2020–2023) − mean(<2020)", color = "ΔNEE (g C m⁻² y⁻¹)") +
  theme_classic(base_size = 18) +
  theme(legend.position = "bottom", legend.title = element_markdown(size = 16),
        axis.title.x = element_blank(), axis.title.y = element_blank())

# ---------- Side-by-side ----------
p_side_fixed_nodots <- p_base10 | p_diff_fixed +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_side_fixed_nodots





########
# ============================================================
# Revised maps: black dots ON TOP of colored symbols
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(sf); library(rnaturalearth); library(patchwork); library(ggtext)
})

# --- sizes and transparency ---
size_colored <- 3.2
size_bg      <- 1.2
if (!exists("alpha_pt")) alpha_pt <- 0.8
if (!exists("xlim_na"))  xlim_na  <- c(-170, -50)
if (!exists("ylim_na"))  ylim_na  <- c(   5,   85)
if (!exists("exclude_sites")) exclude_sites <- c("US-DS3","US-Dmg","US-Bi2")

# --- background: all site locations ---
all_sites <- annual_clean %>%
  dplyr::filter(!site %in% exclude_sites) %>%
  dplyr::distinct(site, LOCATION_LAT, LOCATION_LONG) %>%
  dplyr::filter(is.finite(LOCATION_LAT), is.finite(LOCATION_LONG)) %>%
  sf::st_as_sf(coords = c("LOCATION_LONG","LOCATION_LAT"), crs = 4326)

# --- basemap ---
if (!exists("world")) {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::filter(continent != "Antarctica")
}

# --- color limits (fallback if not defined) ---
robust_limits <- function(x, mult = 1.2, probs = c(0.01, 0.99)) {
  q <- stats::quantile(x[is.finite(x)], probs = probs, na.rm = TRUE)
  lim <- max(abs(q))
  c(-lim, lim) * mult
}
if (!exists("base_limit")) base_limit <- robust_limits(base_sites$mean_hist_full)
if (!exists("diff_limit")) diff_limit <- robust_limits(diff_sites$diff_nee_fixed)

# ---------- BASE map ----------
p_base10 <- ggplot() +
  geom_sf(data = world, fill = "gray95", linewidth = 0.6) +
  # foreground: colored points (drawn first)
  geom_sf(data = base_sites, aes(color = mean_hist_full),
          alpha = alpha_pt, size = size_colored) +
  # overlay: small black dots for all sites
  geom_sf(data = all_sites, color = "black", size = size_bg, alpha = 0.6,
          shape = 16, inherit.aes = FALSE) +
  scale_color_gradient2(low = "blue", mid = "gray85", high = "red",
                        midpoint = 0, limits = base_limit) +
  coord_sf(xlim = xlim_na, ylim = ylim_na, expand = FALSE) +
  labs(title = "Historical mean NEE (sites with ≥10 years)",
       color = "NEE (g C m⁻² y⁻¹)") +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.title = ggtext::element_markdown(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# ---------- DIFF map ----------
p_diff_fixed <- ggplot() +
  geom_sf(data = world, fill = "gray95", linewidth = 0.6) +
  # foreground: colored diff points (drawn first)
  geom_sf(data = diff_sites, aes(color = diff_nee_fixed),
          alpha = alpha_pt, size = size_colored) +
  # overlay: small black dots for all sites
  geom_sf(data = all_sites, color = "black", size = size_bg, alpha = 0.6,
          shape = 16, inherit.aes = FALSE) +
  scale_color_gradient2(low = "blue", mid = "gray85", high = "red",
                        midpoint = 0, limits = diff_limit) +
  coord_sf(xlim = xlim_na, ylim = ylim_na, expand = FALSE) +
  labs(title = "ΔNEE: mean(2020–2023) − mean(<2020)",
       color = "ΔNEE (g C m⁻² y⁻¹)") +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.title = ggtext::element_markdown(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# ---------- Side-by-side ----------
p_side_fixed <- p_base10 | p_diff_fixed +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_side_fixed





p_diff_fixed <- ggplot() +
  geom_sf(data = world, fill = "gray95", linewidth = 0.6) +
  geom_sf(
    data = diff_sites, aes(color = diff_nee_fixed),
    alpha = alpha_pt, size = size_colored
  ) +
  geom_sf(
    data = all_sites, color = "black", size = size_bg, alpha = 0.6,
    shape = 16, inherit.aes = FALSE
  ) +
  scale_color_gradient2(
    low = "blue", mid = "gray85", high = "red",
    midpoint = 0, limits = diff_limit
  ) +
  coord_sf(xlim = xlim_na, ylim = ylim_na, expand = FALSE) +
  labs(
    title = "ΔNEE: mean(2020–2023) - mean(<2020)",  # use simple hyphen here
    color = "ΔNEE (g C m<sup>-2</sup> y<sup>-1</sup>)"  # HTML superscripts
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.title    = ggtext::element_markdown(size = 16),
    legend.text     = ggtext::element_markdown(size = 12),
    axis.title.x    = element_blank(),
    axis.title.y    = element_blank()
  )

