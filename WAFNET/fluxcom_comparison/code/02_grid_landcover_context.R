## 02_grid_landcover_context.R — Stage 1 pre-analysis report, part 5.
##
## READ-ONLY. Pulls together land-cover/IGBP context already computed in the
## repo for these six sites -- does NOT extract anything new from
## data/external/ rasters, and does NOT attempt to aggregate land cover to
## any FLUXCOM product's native grid cell (none of the committed snapshots
## are pre-aggregated to a FLUXCOM-specific resolution; see report_back for
## what that gap means for Stage 2).
##
## Sources read (repo-root, committed, read-only):
##   data/snapshots/site_igbp_fig4.csv           -- PI IGBP + MCD12C1 MODIS
##                                                    IGBP (0.05 deg / ~5.6 km
##                                                    grid cell) + agreement flag
##   data/snapshots/site_landcover_cci.csv        -- ESA CCI LC v2.0.7, 2015
##                                                    (300 m, exact pixel, NOT
##                                                    a cell-aggregate)
##   data/snapshots/site_landcover_cci_current_767.csv -- ESA CCI LC v2.1.1,
##                                                    2022 (300 m, exact pixel)
##
## Output (git-tracked):
##   tables/site_landcover_context.csv

source("WAFNET/fluxcom_comparison/code/00_config.R")

read_snap <- function(name) {
  path <- file.path("data", "snapshots", name)
  d <- utils::read.csv(path, stringsAsFactors = FALSE)
  d[d$site_id %in% WAFNET_SITES, , drop = FALSE]
}

igbp   <- read_snap("site_igbp_fig4.csv")
cci_v207 <- read_snap("site_landcover_cci.csv")              # 2015 vintage
cci_v211 <- read_snap("site_landcover_cci_current_767.csv")   # 2022 vintage

merged <- merge(igbp, cci_v207[, c("site_id", "cci_native_class_name", "cci_high_level_class_name")],
                 by = "site_id", suffixes = c("", "_cci2015"))
names(merged)[names(merged) == "cci_native_class_name"] <- "cci_v2.0.7_2015_native_class"
names(merged)[names(merged) == "cci_high_level_class_name"] <- "cci_v2.0.7_2015_highlevel_class"

merged <- merge(merged, cci_v211[, c("site_id", "cci_native_class_name", "cci_high_level_class_name")],
                 by = "site_id")
names(merged)[names(merged) == "cci_native_class_name"] <- "cci_v2.1.1_2022_native_class"
names(merged)[names(merged) == "cci_high_level_class_name"] <- "cci_v2.1.1_2022_highlevel_class"

merged <- merged[match(WAFNET_SITES, merged$site_id), ]
merged$note <- paste0(
  "MCD12C1 IGBP is a 0.05 deg (~5.6 km) MODIS-derived grid-cell value, closer in scale to FLUXCOM-X-BASE's ",
  "~0.0833 deg grid than to original FLUXCOM's 0.5 deg grid; CCI classes above are the EXACT 300 m pixel at the ",
  "tower coordinate, not a dominant-class aggregate over any FLUXCOM grid cell -- no land-cover product has been ",
  "aggregated to a FLUXCOM-native grid cell for these sites in this repo (see report_back, section 5/6)."
)

write.csv(merged, file.path(FLUXCOM_ROOT, "tables", "site_landcover_context.csv"), row.names = FALSE)
cat("\n[WAFNET/fluxcom_comparison] Land-cover context per site:\n")
print(merged[, c("site_id", "igbp_pi", "igbp_modis_class", "agree",
                  "cci_v2.0.7_2015_native_class", "cci_v2.1.1_2022_native_class")])
cat("\nWrote tables/site_landcover_context.csv\n")
