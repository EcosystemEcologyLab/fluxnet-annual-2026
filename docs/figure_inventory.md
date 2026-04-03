# Figure Inventory — FLUXNET Annual Paper 2026

All figure functions are defined in `R/figures/`. Export filenames are as used
in `scripts/07_figures.R` via `save_fig()` (a thin wrapper around
`ggplot2::ggsave()`). Functions that are not yet wired into `07_figures.R` show
`—` in the export column.

| Function | Export filename(s) | Description | Source |
|---|---|---|---|
| `fig_flux_by_igbp` | `igbp_nee_composite.png`<br>`igbp_gpp_composite.png` | IGBP boxplot + median bar + site-year count patchwork composite for a chosen flux variable | `R/figures/fig_igbp.R:95` |
| `fig_flux_by_igbp_timeslice` | `igbp_nee_timeslice.png` | Annual flux boxplots stratified by IGBP class and equal-width time bins | `R/figures/fig_igbp.R:200` |
| `fig_flux_by_biome_group` | `biome_nee_forest.png`<br>`biome_nee_shrubopens.png`<br>`biome_nee_grasscropswet.png`<br>`biome_nee_other.png` | Annual flux boxplots by broad biome group (Forest / Shrub+Opens / Grass+Crops+Wet / Other) | `R/figures/fig_igbp.R:292` |
| `fig_flux_timeseries_by_igbp` | `igbp_nee_timeseries.png` | Median annual flux time series with 95 % CI ribbon, faceted by IGBP class | `R/figures/fig_igbp.R:390` |
| `fig_seasonal_cycle` | `seasonal_gpp_<group>.png`<br>`seasonal_nee_<group>.png` | Day-of-year seasonal cycle by broad biome group with 95 % CI ribbons per IGBP class | `R/figures/fig_seasonal.R:113` |
| `fig_seasonal_weekly` | — | Weekly median seasonal cycle by IGBP class with ISO-week aggregation and mirror axes | `R/figures/fig_seasonal.R:220` |
| `fig_seasonal_triplet` | — | Weekly NEE seasonal cycle for a named set of sites with IQR ribbons for direct site comparison | `R/figures/fig_seasonal.R:318` |
| `fig_map_global` | `map_global_hub.png`<br>`map_global_igbp.png` | Global outline map of all sites coloured by data hub or IGBP class | `R/figures/fig_maps.R:145` |
| `fig_map_nee_mean` | `map_nee_mean.png` | Global map of sites coloured by long-term mean NEE (blue–grey–red diverging scale, centred on zero) | `R/figures/fig_maps.R:248` |
| `fig_map_nee_delta` | `map_nee_delta.png` | Global map of sites coloured by ΔNEE (recent period minus historical, blue–grey–red scale) | `R/figures/fig_maps.R:348` |
| `fig_whittaker_hexbin` | — | Whittaker biome hexbin of temperature × precipitation coloured by flux (requires WorldClim data; currently stops with informative error) | `R/figures/fig_climate.R:79` |
| `fig_climate_scatter` | `climate_precip_vs_nee.png`<br>`climate_temp_vs_gpp.png` | Two scatter plots coloured by IGBP: annual precipitation vs NEE and mean temperature vs GPP | `R/figures/fig_climate.R:125` |
| `fig_xy_annual` | — | General-purpose XY scatter of any two annual variables with points shaped by IGBP | `R/figures/fig_climate.R:240` |
| `fig_latitudinal_flux` | `latitudinal_nee.png`<br>`latitudinal_gpp.png` | Latitudinal ribbon plot of flux range by 5° latitude band with individual site-mean points shaped by IGBP | `R/figures/fig_latitudinal.R:61` |
| `fig_growing_season_nee` | `growing_season_count.png`<br>`growing_season_span.png` | Growing season length (uptake-day count and calendar span) vs annual NEE scatter, coloured by IGBP | `R/figures/fig_growing_season.R:201` |
