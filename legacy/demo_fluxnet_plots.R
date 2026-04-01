# demo_fluxnet_plots.R
# ------------------------
# Demonstration script for FLUXNET plotting functions
# Requires: fcn_utility_FLUXNET.R and fcn_plot_FLUXNET.R
# ------------------------

library(ggplot2)
library(ggtext)
library(colorspace)
library(scales)      # for squish()
library(sf)
library(tidyr)
library(purrr)
library(readxl)
library(readr)
library(dplyr)
# force dplyr’s select to be used even if masked
select  <- dplyr::select
filter  <- dplyr::filter
rename  <- dplyr::rename
mutate  <- dplyr::mutate
arrange <- dplyr::arrange
# ---- Load source scripts (assumes they are in working directory) ----
# Project utilities and plotting helpers
source(file = "R/fcn_utility_FLUXNET.R")
source(file = "R/fcn_plot_FLUXNET.R")
source("R/poster_constants.R")

# -----------------------------------------------------------------------------
# 1) Discover and load site metadata
#    - Pulls AmeriFlux + ICOS site info; harmonizes fields (IGBP, LAT/LON, etc.)
# -----------------------------------------------------------------------------
metadata <- load_fluxnet_metadata()
## Warning message about "UK" stems from countrycode ambiguity — OK to note.

library(dplyr)
library(tidyr)
library(readxl)
library(readr)

# Existing (AmeriFlux-based) metadata
metadata_tbl <- tibble::as_tibble(metadata)

# ============================================================
# 1) Read FLUXNET2015 BIF Excel (long metadata for all sites)
# ============================================================


bif_path <- here::here(
  "data",
  "FLUXNET",
  "FLX_AA-Flx_BIF_YY_20200501.xlsx"
)

# Check sheet names once (run this interactively the first time)
# readxl::excel_sheets(bif_path)

# In most distributions the main sheet is the first one:
flx_bif_long <- readxl::read_excel(bif_path, sheet = 1)

# Should have columns like:
# SITE_ID, GROUP_ID, VARIABLE_GROUP, VARIABLE, DATAVALUE
# str(flx_bif_long)

# ============================================================
# 2) Filter to variables we care about and pivot to wide
# ============================================================

flx_meta_filtered <- flx_bif_long %>%
  dplyr::filter(VARIABLE_GROUP %in% c(
    "GRP_IGBP",
    "GRP_LOCATION",
    "GRP_CLIM_AVG",
    "GRP_COUNTRY",
    "GRP_HEADER"
  )) %>%
  dplyr::select(SITE_ID, VARIABLE, DATAVALUE)

flx_meta_wide <- flx_meta_filtered %>%
  dplyr::distinct(SITE_ID, VARIABLE, .keep_all = TRUE) %>%
  tidyr::pivot_wider(
    names_from  = VARIABLE,
    values_from = DATAVALUE
  )

# ============================================================
# 3) Harmonize to match your metadata schema
# ============================================================

flx_meta_clean <- flx_meta_wide %>%
  dplyr::transmute(
    SITE_ID         = SITE_ID,
    SITE_NAME       = SITE_NAME,                 # from GRP_HEADER
    COUNTRY         = COUNTRY,                   # from GRP_COUNTRY
    STATE           = NA_character_,             # not in BIF → NA
    IGBP            = IGBP,                      # from GRP_IGBP
    LOCATION_LAT    = readr::parse_number(LOCATION_LAT),
    LOCATION_LONG   = readr::parse_number(LOCATION_LONG),
    LOCATION_ELEV   = NA_real_,                  # not using it here
    CLIMATE_KOEPPEN = NA_character_,             # not in this BIF
    MAT             = NA_real_,                  # not in this BIF
    MAP             = readr::parse_number(MAP),  # from GRP_CLIM_AVG
    DATA_SOURCE     = "FLUXNET2015",
    site            = SITE_ID,
    SITEID          = SITE_ID
  )



# ============================================================
# 3b) Legacy European site metadata (recovers 32 missing sites)
# ============================================================

legacy_path <- here::here(
  "data",
  "FLUXNET",
  "Legacy_EuropeanSitesListMetadata.csv"
)

legacy_eu_raw <- readr::read_csv(legacy_path, show_col_types = FALSE)

# Columns in the CSV (after clean_names):
# site_code, site_name, site_responsible, site_latitude, site_longitude,
# utc_offset, igbp_code, projects, fluxes, mean_annual_temperature,
# mean_annual_precpitation, ...

legacy_eu_meta <- legacy_eu_raw %>%
  janitor::clean_names() %>%
  dplyr::transmute(
    SITE_ID         = site_code,
    SITE_NAME       = site_name,
    COUNTRY         = NA_character_,          # will be filled later by assign_country_from_site()
    STATE           = NA_character_,
    IGBP            = igbp_code,
    LOCATION_LAT    = site_latitude,
    LOCATION_LONG   = site_longitude,
    LOCATION_ELEV   = NA_real_,
    CLIMATE_KOEPPEN = NA_character_,
    MAT             = NA_real_,               # not essential here
    MAP             = NA_real_,
    DATA_SOURCE     = "LegacyEurope",
    site            = SITE_ID,
    SITEID          = SITE_ID
  )

# ============================================================
# 4) Combine AmeriFlux metadata, FLUXNET2015 BIF, and Legacy EU metadata
#     - order matters: first rows win in distinct(site, .keep_all = TRUE)
# ============================================================

metadata_full <- dplyr::bind_rows(
  metadata_tbl,       # AmeriFlux / ICOS discovery
  flx_meta_manifest,  # FLUXNET2015 BIF for sites in manifest
  legacy_eu_meta      # Legacy European sites (fills the 32 missing ones)
) %>%
  dplyr::distinct(site, .keep_all = TRUE)

# ---- Patch missing IGBP from FLUXNET2015 BIF into metadata_full ----

bif_igbp_lookup <- flx_meta_clean %>%
  dplyr::select(site = SITE_ID, IGBP_BIF = IGBP) %>%
  dplyr::distinct(site, .keep_all = TRUE)

metadata_full <- metadata_full %>%
  dplyr::left_join(bif_igbp_lookup, by = "site") %>%
  dplyr::mutate(
    IGBP = dplyr::if_else(
      is.na(IGBP) & !is.na(IGBP_BIF),
      IGBP_BIF,
      IGBP
    )
  ) %>%
  dplyr::select(-IGBP_BIF)

# Quick sanity check
metadata_full %>% dplyr::count(DATA_SOURCE)



# -----------------------------------------------------------------------------
# 2) Discover locally available AMF/ICOS files and build a file manifest
#    - Manifest encodes site, dataset type (FULLSET, L2), time integral (YY, etc.)
# -----------------------------------------------------------------------------
amf_files  <- discover_AMF_files(data_dir = here::here("data/FLUXNET/AMF"))
icos_files <- discover_ICOS_files(data_dir = here::here("data/FLUXNET/ICOS"))

# Combine and de-duplicate rows that differ only by internal path bookkeeping
manifest <- bind_rows(amf_files, icos_files) %>%
  distinct(
    site, data_product, dataset, time_integral, start_year, end_year,
    .keep_all = TRUE
  )


# If some of these columns don't exist in your BIF, they'll just be NA;
# you can drop them from transmute() if R complains.

# Optionally restrict to FLX sites actually in the manifest
flx_sites_in_manifest <- manifest %>%
  dplyr::filter(data_center == "FLX", data_product == "FLUXNET2015") %>%
  dplyr::distinct(site) %>%
  dplyr::pull(site)

flx_meta_manifest <- flx_meta_clean %>%
  dplyr::filter(SITE_ID %in% flx_sites_in_manifest)

# -----------------------------------------------------------------------------
# 3) Load annual (YY) FULLSET data using the manifest
#    - Replaces -9999 sentinels with NA
#    - Adds integer year column
#    - Joins site metadata (IGBP, LAT/LON, etc.) to the annual records
# -----------------------------------------------------------------------------
# annual <- manifest %>%
#   dplyr::filter(time_integral == "YY", dataset == "FULLSET") %>%
#   load_fluxnet_data() %>%
#   dplyr::mutate(across(where(is.numeric), \(x) na_if(x, -9999))) %>%
#   dplyr::mutate(year = as.integer(TIMESTAMP), .before = TIMESTAMP) %>%
#   dplyr::left_join(
#     metadata_full %>% dplyr::select(-SITEID, -SITE_ID),
#     by = dplyr::join_by(site)
#   ) %>%
#   assign_country_from_site(site_col = "site")

annual <- manifest %>%
  dplyr::filter(
    # Keep what you already had: YY FULLSET (AMF + legacy FLX)
    (time_integral == "YY" & dataset == "FULLSET") |
      # NEW: ICOS L2 annual files
      (time_integral == "YY" & dataset == "L2" & data_center == "ICOSETC")
  ) %>%
  load_fluxnet_data() %>%
  dplyr::mutate(across(where(is.numeric), \(x) na_if(x, -9999))) %>%
  dplyr::mutate(year = as.integer(TIMESTAMP), .before = TIMESTAMP) %>%
  dplyr::left_join(
    metadata_full %>% dplyr::select(-SITEID, -SITE_ID),
    by = dplyr::join_by(site)
  ) %>%
  assign_country_from_site(site_col = "site")




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
qc_gate_vars        <- c("NEE_VUT_REF", "PA_F")
max_gapfilled_bad   <- 0.75
drop_if_qc_missing  <- TRUE

annual_flagged <- annual %>%
  flag_bad_gapfilled(
    gate_vars       = qc_gate_vars,
    max_gapfilled   = max_gapfilled_bad,
    drop_if_missing = drop_if_qc_missing
  )

annual_clean <- annual_flagged %>% filter(!is_bad)

# deduplication) Within each (site, year, data_center, data_product, dataset),
#    keep the row from the file with the largest end_year.
#    Treat NA end_year as "oldest" (i.e., lowest priority).
annual_versioned <- annual_clean %>%
  group_by(site, year, data_center, data_product, dataset) %>%
  arrange(
    dplyr::desc(replace_na(end_year, -9999L)),  # NA end_year -> -9999
    .by_group = TRUE
  ) %>%
  slice(1L) %>%
  ungroup()


annual_dedup <- annual_versioned %>%
  mutate(
    # Source priority across product families
    # Assumption: ICOS L2 > AMF FULLSET > FLUXNET2015 FULLSET
    source_priority = case_when(
      data_center == "ICOSETC" & dataset == "L2"                     ~ 1L,
      data_center == "AMF"     & data_product == "FLUXNET"    &
        dataset == "FULLSET"                                        ~ 2L,
      data_center == "FLX"     & data_product == "FLUXNET2015" &
        dataset == "FULLSET"                                        ~ 3L,
      TRUE                                                          ~ 99L
    )
  ) %>%
  group_by(site, year) %>%
  arrange(source_priority, .by_group = TRUE) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-source_priority)

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

##### site US-HB4 has weirdly high precip values and QAQC does not remove it
#### Manually removing it here
annual <- annual_dedup %>%
  filter(site != "US-HB4") %>%
  filter(!is.na(IGBP))

cat("Generating country-level site and site-year summaries...\n")
plots_country <- plot_sites_by_country(annual)
#print(plots_country$new_sites)
print(plots_country$site_years)
#for no legends
# plots_country$new_sites  + theme(legend.position = "none")
# plots_country$site_years + theme(legend.position = "none")


# ---- Plot 1a: IGBP Aggregated Flux Summary (Boxplot, Median, Count) ----
cat("Generating flux summary by IGBP...\n")
plots_igbp <- plot_flux_by_igbp(annual, flux_var = "GPP_NT_VUT_REF")
print(plots_igbp$composite_plot)
plots_igbp$summary_table


forest_shrub <- c("EBF","MF","DBF","ENF","CSH","OSH")
plots_igbp_forest_shrub <- plot_flux_by_igbp(annual, "GPP_NT_VUT_REF", igbp_order = forest_shrub)
FS <- plots_igbp_forest_shrub$flux_plot

Sav_crop <- c("WSA","SAV","GRA", "WET", "CRO", "CVM")
plots_igbp_sav_crop <- plot_flux_by_igbp(annual, "GPP_NT_VUT_REF", igbp_order = Sav_crop)
SC <- plots_igbp_sav_crop$flux_plot

FS <- plots_igbp_forest_shrub$flux_plot +
  coord_cartesian(ylim = c(0, 5000))

SC <- plots_igbp_sav_crop$flux_plot +
  coord_cartesian(ylim = c(0, 5000))


p_T <- SC & theme(
  plot.background  = element_rect(fill = "transparent", color = NA),
  panel.background = element_rect(fill = "transparent", color = NA)
)

ggsave(
  "GPP_BottomPanel.png",
  plot       = p_T,
  width      = 13.27,
  height     = 5.95,
  dpi        = 300,
  device     = ragg::agg_png,
  background = "transparent"
)



# ---- Plot 1b: IGBP Aggregated Flux Summary (Boxplot, Median, Count) ----
cat("Generating flux summary by IGBP...\n")
plots_igbp <- plot_flux_by_igbp(annual, flux_var = "NEE_VUT_REF")
print(plots_igbp$composite_plot)

# ---- Plot 1c: IGBP Aggregated Flux Summary (Boxplot, Median, Count) ----
cat("Generating flux summary by IGBP...\n")
plots_igbp <- plot_flux_by_igbp(annual, flux_var = "RECO_NT_VUT_REF")
print(plots_igbp$composite_plot)

# # ---- Plot 1d: IGBP Aggregated Flux Summary (Boxplot, Median, Count) ----
# cat("Generating flux summary by IGBP...\n")
# plots_igbp <- plot_flux_by_igbp(annual, flux_var = "WUE")
# print(plots_igbp$composite_plot)


# ---- Plot 2: Flux by IGBP and Time Slice (5-year bins) ----
cat("Generating time-sliced flux comparison...\n")
plots_timeslice <- plot_flux_by_igbp_timeslice_grouped(annual, flux_var = "NEE_VUT_REF")
print(plots_timeslice$flux_plot)

# ---- Plot 3: Interannual Boxplots Grouped by IGBP Type ----
cat("Generating group-wise annual flux boxplots...\n")
boxplots <- plot_flux_box_by_group(annual, flux_var = "GPP_NT_VUT_REF", y_mode = "squish")
print(boxplots$Forest)
print(boxplots$ShrubOpens)
print(boxplots$GrassCropsWet)



# ---- Plot 4a: Timeseries of Median Fluxes by IGBP ----
cat("Generating timeseries of median NEE by IGBP...\n")
ts_plot <- plot_flux_timeseries_by_igbp(annual, flux_var = "NEE_VUT_REF")
print(ts_plot)

# ---- Plot 4b: Timeseries of Median Fluxes by IGBP ----
cat("Generating timeseries of median GPP by IGBP...\n")
ts_plot <- plot_flux_timeseries_by_igbp(annual, flux_var = "GPP_NT_VUT_REF")
print(ts_plot)

# ---- Plot 4c: Timeseries of Median Fluxes by IGBP ----
cat("Generating timeseries of median RECO by IGBP...\n")
ts_plot <- plot_flux_timeseries_by_igbp(annual, flux_var = "RECO_NT_VUT_REF")
print(ts_plot)

# ---- Plot 4d: Timeseries of Median Fluxes by IGBP ----
cat("Generating timeseries of median WUE by IGBP...\n")
ts_plot <- plot_flux_timeseries_by_igbp(annual, flux_var = "WUE")
print(ts_plot)

# ---- Plot 5: Latitudinal Summary (Ribbon + Points) ----
#5a 
cat("Generating latitudinal flux summary...\n")
lat_plot <- plot_latitudinal_flux(annual, metadata, flux_var = "NEE_VUT_REF")
print(lat_plot)

#5b
cat("Generating latitudinal flux summary...\n")
lat_plot <- plot_latitudinal_flux(annual, metadata, flux_var = "GPP_NT_VUT_REF")
print(lat_plot)

#5c
cat("Generating latitudinal flux summary...\n")
lat_plot <- plot_latitudinal_flux(annual, metadata, flux_var = "RECO_NT_VUT_REF")
print(lat_plot)


# # ---- Plot 6: Seasonal Climatology by IGBP Group ----
# cat("Generating daily seasonal cycle plots...\n")
# seasonal_cycle <- plot_seasonal_cycle(daily_data, flux_var = "GPP_NT_VUT_REF", y_mode = "full")
# print(seasonal_cycle$Forest)

# ---- Plot 7: Scatterplots of Climate vs Flux ----
cat("Generating annual precipitation vs NEE and temperature vs GPP...\n")
climate_plots <- plot_annual_fluxnet_data(annual)
print(climate_plots$precip_vs_nee)
print(climate_plots$temp_vs_gpp)


# ---- Plot 8: General XY Scatter (GPP vs LE) ----
cat("Generating general scatterplot of GPP vs LE...\n")
scatter <- PlotXY_annual(annual, x_var = "GPP_NT_VUT_REF", y_var = "LE_F_MDS")
print(scatter)

scatter <- PlotXY_annual(annual, x_var = "NEE_VUT_REF", y_var = "LE_F_MDS")
print(scatter)

# ---- Optional: Save plots to folder ----
# Uncomment to save any plot list, e.g.:
# save_plot_list(plots_igbp, prefix = "flux_by_IGBP")
# save_plot_list(boxplots, prefix = "grouped_boxplot")

cat("All plots generated successfully.\n")



# -----------------------------------------------------------------------------
# 4) Compute site-level climate & flux summaries from tower data
#    - Mean P_F (mm/yr), TA_F (°C), GPP, NEE per site
#    - Derive precipitation in cm/yr for Whittaker axes
#    - Tag sink/source by sign of mean NEE
#    - Re-attach site metadata to the site summaries
# -----------------------------------------------------------------------------
climate_summary <- annual %>%
  group_by(site) %>%
  summarize(
    mean_precip = mean(P_ERA, na.rm = TRUE),       # mm yr-1 (tower)
    mean_temp   = mean(TA_F, na.rm = TRUE),      # °C (tower)
    mean_GPP    = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    mean_NEE    = mean(NEE_VUT_REF, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_precip_cm = mean_precip / 10,  # mm → cm
    NEE_sign = ifelse(mean_NEE < 0, "Sink (NEE < 0)", "Source (NEE ≥ 0)")
  ) %>%
  left_join(metadata, by = c("site" = "SITE_ID"))

# -----------------------------------------------------------------------------
# 5) Extract WorldClim climate at tower locations
#    - Uses WorldClim v2.1 bioclim: bio1 (MAT*10), bio12 (MAP mm)
#    - Converts MAP mm → cm for Whittaker y-axis
# -----------------------------------------------------------------------------
wc <- readRDS("data/wc_worldclim_30s.rds")

# If this RDS was saved from raster::stack or something else, coerce it:
if (!inherits(wc, "SpatRaster")) {
  wc <- terra::rast(wc)
}

# Build point geometry from site coords (as you had it)
site_pts <- terra::vect(
  climate_summary,
  geom = c("LOCATION_LONG", "LOCATION_LAT"),
  crs  = "EPSG:4326"
)

# Option A: subset first
wc_extract <- terra::extract(wc[[c(1, 12)]], site_pts)

# Option B: use lyrs argument (also works)
# wc_extract <- terra::extract(wc, site_pts, lyrs = c(1, 12))

# Attach WorldClim fields (note: MAT_WorldClim is bio1; scale as needed)
climate_summary$MAT_WorldClim      <- wc_extract[["wc2.1_30s_bio_1"]]   # MAT * 10 (°C*10)
climate_summary$MAP_WorldClim      <- wc_extract[["wc2.1_30s_bio_12"]]  # MAP (mm)
climate_summary$MAP_WorldClim_cm   <- climate_summary$MAP_WorldClim / 10# mm → cm

# -----------------------------------------------------------------------------
# 6) Simple tower-vs-WorldClim climate checks
#    - Scatter against 1:1 line for MAT and MAP
# -----------------------------------------------------------------------------
ggplot(climate_summary, aes(x = MAT_WorldClim, y = mean_temp)) +
  geom_point(color = "#1f78b4", size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "WorldClim MAT (°C*10 or °C depending on scale)",  # adjust label if you rescale
       y = "Observed Site MAT (°C)") +
  theme_classic()

ggplot(climate_summary, aes(x = MAP_WorldClim, y = mean_precip)) +
  geom_point(color = "#33a02c", size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "WorldClim MAP (mm)", y = "Observed Site MAP (mm)") +
  theme_classic()

# -----------------------------------------------------------------------------
# 7) Whittaker biome overlay + site points
#    - Base polygons from plotbiomes::Whittaker_biomes (temp °C, precip cm)
#    - Overlay tower sites colored by IGBP
# -----------------------------------------------------------------------------

library(plotbiomes)
library(measurements)
library(colorspace)
library(ggtext)
library(sf)
library(hexbin)
# Convert prcip to cm and filter out large values
plot_df <- annual %>% 
  mutate(precip_cm = conv_unit(P_F, "mm", "cm")) %>% 
  filter(precip_cm < 500) %>% 
  select(precip_cm, temp_c = TA_F, NEE = NEE_VUT_REF)

# Convert adjacent polygons with overlapping borders to polygons with borders
# that don't quite touch for nicer plotting

# Scale coords so buffering works equally on both dimensions
whit_scaled <- Whittaker_biomes %>% mutate(across(c(temp_c, precp_cm), scale))
# create sf object of boundaries
borders <- whit_scaled %>%
  st_as_sf(coords = c("temp_c", "precp_cm")) %>%
  group_by(biome) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_buffer(dist = -0.01) %>% # <- might need to tweak this number
  st_boundary()

# Convert back to data frame and unscale
temp_center <- attr(whit_scaled$temp_c, "scaled:center")
temp_scale <- attr(whit_scaled$temp_c, "scaled:scale")
precip_center <- attr(whit_scaled$precp_cm, "scaled:center")
precip_scale <- attr(whit_scaled$precp_cm, "scaled:scale")

borders_df <- borders %>%
  as_tibble() %>%
  group_by(biome) %>%
  reframe(
    temp_c = st_coordinates(geometry)[, 1],
    precip_cm = st_coordinates(geometry)[, 2]
  ) %>%
  #unscale
  mutate(
    temp_c = (temp_c * temp_scale) + temp_center,
    precip_cm = (precip_cm * precip_scale) + precip_center
  )

ggplot() +
  geom_hex(
    data = plot_df,
    aes(x = temp_c, y = precip_cm, z = NEE),
    stat = "summary_hex",
    bins = 40,
    fun = median,
    na.rm = TRUE
  ) +
  scale_fill_continuous_diverging(
    name = "NEE (g C m<sup>-2</sup> y<sup>-1</sup>)"
  ) +
  geom_polygon(
    data = borders_df,
    aes(x = temp_c, y = precip_cm, color = biome),
    linewidth = 1.5, # <- might need to tweak this number
    fill = NA
  ) +
  scale_color_manual(
    name = "Whittaker biomes",
    breaks = names(Ricklefs_colors),
    labels = names(Ricklefs_colors),
    values = Ricklefs_colors,
  ) +
  labs(x = "Temperature (C)", y = "Precipitation (cm y<sup>-1</sup>)") +
  theme_minimal() +
  theme(legend.title = element_markdown(), axis.title.y = element_markdown())


source("R/poster_constants.R")

library(ggplot2)
library(colorspace)
library(ggtext)
library(scales)   # for squish()

# ============================================================
# 1) Robust color limits for NEE
#    - Controls how much the extremes influence the color scale
# ============================================================

# probs = c(0.10, 0.90) keeps the central 80% of values.
# Make these closer to c(0.02, 0.98) for lighter clipping,
# or c(0.20, 0.80) for more aggressive clipping.
nee_q <- quantile(plot_df$NEE, probs = c(0.10, 0.90), na.rm = TRUE)

# Enforce symmetry around 0 so sinks/sources are treated equally
nee_max  <- max(abs(nee_q))
nee_lims <- c(-nee_max, nee_max)   # <-- COLOR LIMITS USED BELOW

nee_lims  # <-- Run to inspect and sanity-check the range

# ============================================================
# 2) Hexbin plot: Temperature vs Precipitation vs NEE (median)
# ============================================================

p_temp_precip_nee <- ggplot(
  data = plot_df,
  aes(x = temp_c, y = precip_cm, z = NEE)
) +
  # -------------------- HEXBIN LAYER ------------------------
# bins: resolution of the hex grid (more bins → finer structure)
geom_hex(
  stat = "summary_hex",
  bins = 40,      # <-- CHANGE RESOLUTION HERE
  fun  = median,  # <-- median NEE per hex; could use mean, etc.
  na.rm = TRUE
) +
  
  # -------------------- COLOR SCALE -------------------------
# - lab_nee_annual comes from poster_constants.R
# - limits = nee_lims clips to central range
# - oob = squish pins values outside limits to the colorbar ends
scale_fill_continuous_diverging(
  name   = lab_nee_annual,  # "NEE (g C m<sup>-2</sup> yr<sup>-1</sup>)"
  limits = nee_lims,        # <-- CLIP TO CENTRAL RANGE
  oob    = squish           # <-- values outside limits get squished to ends
  # You can also experiment with:
  # oob = squish_mid or oob = rescale_none
) +
  
  # -------------------- AXIS LABELS -------------------------
labs(
  x = lab_temp_annual,                     # from poster_constants: "Temperature (°C)"
  y = "Precipitation (cm y<sup>-1</sup>)"  # still in cm (you can edit text here)
) +
  
  # -------------------- POSTER THEME BASE -------------------
# Controls base font size for axes, titles, etc.
poster_theme(base_size = 18) +            # <-- GLOBAL SIZE KNOB
  
  # -------------------- LOCAL THEME TWEAKS -----------------
theme(
  # Legend title and text (colorbar)
  legend.title = ggtext::element_markdown(size = 16),  # <-- legend title size
  legend.text  = ggtext::element_markdown(size = 14),  # <-- legend tick label size
  
  # Axis titles
  axis.title.x = ggtext::element_markdown(
    size = 18,    # <-- X-axis label size
    angle = 0
  ),
  axis.title.y = ggtext::element_markdown(
    size = 18,    # <-- Y-axis label size
    angle = 90,   # <-- rotate label (0 = horizontal, 90 = vertical)
    vjust = 0.5
  ),
  
  # Axis tick labels
  axis.text.x = element_text(
    size = 14,    # <-- X tick label size
    angle = 0     # <-- tilt ticks (e.g., 45 for diagonal labels)
  ),
  axis.text.y = element_text(
    size = 14     # <-- Y tick label size
  ),
  
  # Tick mark lengths (poster_theme already uses inward ticks;
  # override here if you want different behavior)
  axis.ticks.length = grid::unit(-4, "pt"),  # negative = inward ticks
  
  # Panel border thickness override (if you want it heavier/lighter)
  panel.border = element_rect(
    color = "black",
    fill  = NA,
    linewidth = 1.0  # <-- frame thickness
  )
)

# Preview
p_temp_precip_nee


