# legacy/

These scripts are **reference only** — do not source or run them directly.

They are the source material for the figure functions in `R/figures/` and
`R/plot_constants.R`. They were developed for prior analyses using legacy
FLUXNET data (FLUXNET2015, AmeriFlux BASE, and ICOS L2 products accessed
directly) and will not run without modification in the current pipeline
environment.

---

## What each file contains

| File | Contents |
|---|---|
| `poster_constants.R` | Theme (`poster_theme()`), IGBP order/names/colours, axis label constants, country colour palette, geom wrappers |
| `fcn_utility_FLUXNET.R` | Data loading, file discovery, metadata harmonisation, QC flagging (`flag_bad_gapfilled()`), caching utilities |
| `fcn_plot_FLUXNET.R` | Eight plotting functions: IGBP boxplots, time series, latitudinal ribbon, seasonal cycle, scatter plots |
| `demo_fluxnet_plots.R` | Whittaker biome hexbin, climate-flux scatter, metadata harmonisation workflow |
| `MappingFluxnet.R` | Global site maps, network overlay maps, aridity background maps |
| `AGUSlop.R` | Weekly seasonal cycles by IGBP, site triplet seasonal plots, growing season vs annual NEE |
| `AMFOct25_poster.R` | Poster pipeline: annual IGBP summaries, historical NEE maps, ΔNEE maps, QC diagnostics |

---

## Why these are kept here

- **Traceability** — per `SCIENCE_PRINCIPLES_PIPELINES.md`, every result must
  be traceable to its source. These scripts are the documented origin of the
  figure logic now implemented in `R/figures/`.
- **Reference** — the original figure styles, colour choices, and analytical
  approaches are preserved here for comparison.
- **Recovery** — if a ported function behaves unexpectedly, the original
  implementation is available for direct comparison.

---

## What was ported and where

| Legacy function / figure | Ported to |
|---|---|
| `poster_theme()`, IGBP constants, axis labels | `R/plot_constants.R` |
| `plot_flux_by_igbp()` | `R/figures/fig_igbp.R` |
| `plot_flux_by_igbp_timeslice_grouped()` | `R/figures/fig_igbp.R` |
| `plot_flux_box_by_group()` | `R/figures/fig_igbp.R` |
| `plot_flux_timeseries_by_igbp()` | `R/figures/fig_igbp.R` |
| `plot_seasonal_cycle()` | `R/figures/fig_seasonal.R` |
| Weekly seasonal cycle (AGUSlop) | `R/figures/fig_seasonal.R` |
| Site triplet seasonal (AGUSlop) | `R/figures/fig_seasonal.R` |
| Global site maps, network overlay | `R/figures/fig_maps.R` |
| Historical NEE map, ΔNEE map | `R/figures/fig_maps.R` |
| Whittaker biome hexbin | `R/figures/fig_climate.R` |
| `plot_annual_fluxnet_data()` | `R/figures/fig_climate.R` |
| `PlotXY_annual()` | `R/figures/fig_climate.R` |
| `plot_latitudinal_flux()` | `R/figures/fig_latitudinal.R` |
| Growing season vs annual NEE | `R/figures/fig_growing_season.R` |
| Aridity maps | `R/figures/fig_maps.R` (placeholder — requires external data) |

---

## External data dependencies in these scripts

Two figures require external datasets not included in the repository.
See `R/external_data.R` for download instructions.

| Dataset | Used in | Download |
|---|---|---|
| WorldClim bioclim rasters | `demo_fluxnet_plots.R` (Whittaker hexbin) | https://worldclim.org/data/worldclim21.html |
| CGIAR Global Aridity Index | `MappingFluxnet.R` (aridity maps) | https://figshare.com/articles/dataset/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v2/7504448 |
