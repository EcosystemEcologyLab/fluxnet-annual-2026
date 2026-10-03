# Figure Inventory — FLUXNET Annual Paper 2026

All figure functions are defined in `R/figures/`. Export filenames are as used
in `scripts/07_figures.R` via `save_fig()` (a thin wrapper around
`ggplot2::ggsave()`). Functions that are not yet wired into `07_figures.R` show
`—` in the export column.

External data dependencies:
- **None** — uses only FLUXNET Shuttle data and snapshot metadata
- **WorldClim** — requires WorldClim 30 s bioclimatic rasters; see `R/external_data.R`
- **CGIAR** — requires CGIAR global aridity index raster; see `R/external_data.R`
- **MODIS** — requires MODIS land-cover or NDVI products; see `R/external_data.R`

| Function | Export filename(s) | Description | Source | Resolution | External data |
|---|---|---|---|---|---|
| `fig_flux_by_igbp` | `igbp_nee_composite.png`<br>`igbp_gpp_composite.png` | IGBP boxplot + median bar + site-year count patchwork composite for a chosen flux variable | `R/figures/fig_igbp.R:95` | YY | None |
| `fig_flux_by_igbp_timeslice` | `igbp_nee_timeslice.png` | Annual flux boxplots stratified by IGBP class and equal-width time bins | `R/figures/fig_igbp.R:200` | YY | None |
| `fig_flux_by_biome_group` | `biome_nee_forest.png`<br>`biome_nee_shrubopens.png`<br>`biome_nee_grasscropswet.png`<br>`biome_nee_other.png` | Annual flux boxplots by broad biome group (Forest / Shrub+Opens / Grass+Crops+Wet / Other) | `R/figures/fig_igbp.R:292` | YY | None |
| `fig_flux_timeseries_by_igbp` | `igbp_nee_timeseries.png` | Median annual flux time series with 95 % CI ribbon, faceted by IGBP class | `R/figures/fig_igbp.R:390` | YY | None |
| `fig_seasonal_cycle` | `seasonal_gpp_<group>.png`<br>`seasonal_nee_<group>.png` | Day-of-year seasonal cycle by broad biome group with 95 % CI ribbons per IGBP class | `R/figures/fig_seasonal.R:113` | DD | None |
| `fig_seasonal_weekly` | — | Weekly median seasonal cycle by IGBP class with ISO-week aggregation and mirror axes | `R/figures/fig_seasonal.R:220` | DD | None |
| `fig_seasonal_triplet` | — | Weekly NEE seasonal cycle for a named set of sites with IQR ribbons for direct site comparison | `R/figures/fig_seasonal.R:318` | DD | None |
| `fig_network_growth` | — | Cumulative stacked area chart of active FLUXNET sites by year, stratified by IGBP class; uses `first_year` from snapshot metadata | `R/figures/fig_network_growth.R:72` | Metadata only | None |
| `fig_network_growth_annual` | — | Stacked bar chart of new FLUXNET sites added per year by IGBP class; complements `fig_network_growth()` by showing expansion rate | `R/figures/fig_network_growth.R:156` | Metadata only | None |
| `fig_map_global` | `map_global_hub.png`<br>`map_global_igbp.png` | Global outline map of all sites coloured by data hub or IGBP class | `R/figures/fig_maps.R:145` | Metadata only | None |
| `fig_map_country_sites` | — | Country choropleth of active FLUXNET site counts per ISO country at user-specified year cutoffs (default 2015/2020/2025); patchwork of three panels with shared colour scale | `R/figures/fig_maps.R:465` | Metadata only | None |
| `fig_map_nee_mean` | `map_nee_mean.png` | Global map of sites coloured by long-term mean NEE (blue–grey–red diverging scale, centred on zero) | `R/figures/fig_maps.R:239` | YY | None |
| `fig_map_nee_delta` | `map_nee_delta.png` | Global map of sites coloured by ΔNEE (recent period minus historical, blue–grey–red scale) | `R/figures/fig_maps.R:339` | YY | None |
| `fig_whittaker_hexbin` | — | Whittaker biome hexbin of WorldClim MAT × MAP coloured by median site-mean flux; optional `year_cutoff` for snapshot panels. Requires WorldClim data — designed for local Mac execution | `R/figures/fig_climate.R:96` | YY | WorldClim |
| `fig_climate_scatter` | `climate_precip_vs_nee.png`<br>`climate_temp_vs_gpp.png` | Two scatter plots coloured by IGBP: annual precipitation vs NEE and mean temperature vs GPP | `R/figures/fig_climate.R:325` | YY | None |
| `fig_xy_annual` | — | General-purpose XY scatter of any two annual variables with points shaped by IGBP | `R/figures/fig_climate.R:436` | YY | None |
| `fig_latitudinal_flux` | `latitudinal_nee.png`<br>`latitudinal_gpp.png` | Latitudinal ribbon plot of flux range by 5° latitude band with individual site-mean points shaped by IGBP | `R/figures/fig_latitudinal.R:62` | YY | None |
| `fig_latitudinal_multi` | — | Multi-variable wrapper: calls `fig_latitudinal_flux()` for each variable in `flux_vars` and assembles a vertical patchwork with shared latitude axis | `R/figures/fig_latitudinal.R:227` | YY | None |
| `fig_environmental_response` | — | Binned flux vs environment response curves: bins each env_var into equal-frequency quantile bins, plots median ± IQR ribbon (or IGBP-coloured lines). WorldClim / CGIAR aridity required for WorldClim and aridity env_vars | `R/figures/fig_environmental_response.R:107` | YY | WorldClim / CGIAR |
| `fig_long_record_timeseries` | — | Annual time series of flux variables for top-`n` longest-record sites per continent (UN Geoscheme); lines coloured by IGBP with `scale_color_igbp()` | `R/figures/fig_timeseries.R:178` | YY | None |
| `fig_growing_season_nee` | `growing_season_count.png`<br>`growing_season_span.png` | Growing season length (uptake-day count and calendar span) vs annual NEE scatter, coloured by IGBP | `R/figures/fig_growing_season.R:201` | YY + DD | None |

## Figure 1 (merged)

`scripts/generate_fig01_merged.R` builds `draft_manuscript_v1/fig_01.png`/`.pdf`/`.legend.txt`
(added 2026-10-02, task 9): panel a (the Equal Earth network map) above panel b (cumulative
site-years by IGBP), 89 mm wide, ONE file -- calling the same two panel-building functions and
pinned inputs (`fig_map_point_network()`, `fig_cumulative_siteyears_igbp()`) as
`scripts/generate_point_maps.R` and `scripts/generate_duration_histograms.R` below, which
continue to stage the separate `fig_01a_map_current_network.png`/`fig_01b_cumulative_siteyears_
igbp.png` files this merged file does not replace. Figure 1a's map is Equal Earth (EPSG:8857,
`R/figures/fig_maps.R::fig_map_point_network()`, revised 2026-10-02, task 7): canvas height fit
to the map (not a fixed square, via `equal_earth_height_mm()`), filled no-outline semi-transparent
points, separate thinner/lighter country-border vs. coastline line layers
(`.map_base_eqearth()`), Antarctica/high Arctic excluded by cropping the basemap and points to
latitude [-56, 85] before projecting.

## Shared IGBP palette (task 2, 2026-10-02)

Every paper figure that colours by IGBP class (Figures 1b, 3, 4, and the matched/six-panel
Extended Data figures) uses `R/plot_constants.R::PAPER_IGBP_ORDER`/`PAPER_IGBP_COLOURS`
(`scale_fill_paper_igbp()`/`scale_color_paper_igbp()`) -- Figure 4's own MODIS/061/MCD12Q1 GEE
palette (15 classes, CVM/BSV/SNO included), promoted to a shared constant. This is DIFFERENT
from the older `IGBP_order`/`IGBP_colours` (still used by other, non-paper figures) -- see
`docs/known_issues.md` §11.

## Manuscript representativeness figures (Figure 4 and supplement)

Not part of `07_figures.R` / `R/figures/` — these are standalone production scripts, each producing a
PNG + vector PDF pair plus a `.meta.json` and `.legend.txt`, copied into
`review/figures/draft_manuscript_v1/`.

| Script | Output (main location) | Draft-manuscript copy | Description | External data | Status |
|---|---|---|---|---|---|
| `scripts/figure4_representativeness.R` | `review/figures/representativeness/fig_04_representativeness.png`/`.pdf` | `fig_04_representativeness.png`/`.pdf` | **Figure 4.** Six-panel log2 sampling-ratio figure (Köppen-Geiger, IGBP land cover, aridity, biomass, NEE, ET) vs. current 781-site network, Geo vs Data | Beck 2023 KG, MODIS MCD12C1, CGIAR Aridity v3.1, ESA CCI Biomass v7, TRENDY v14, WorldClim BIO12, ERA5 | Current |
| `scripts/figure4_representativeness.R` | `review/figures/representativeness/supp_representativeness_geo_vs_geo.png`/`.pdf` | `SupFigs/supp_representativeness_geo_vs_geo.png`/`.pdf` | **Extended Data figure** (revised 2026-10-02: moved from `draft_manuscript_v1/` into `draft_manuscript_v1/SupFigs/` and re-rendered at 180 mm wide — Nature takes no Supplementary Information figures, so this is Extended Data, not a "supplemental figure"; `draft_manuscript_v1/` itself keeps only main-text figures). Same six panels, Geo vs Geo (gridded product's own value at each tower, not the site's own measurement) | same as above | Current |
| `scripts/figure_representativeness_summary.R` | `review/figures/representativeness/fig_rep001_current.png` | — (superseded, see below) | Prior "Figure 4": six-panel sampling ratio figure (Köppen-Geiger, ESA CCI Land Cover, CGIAR Aridity, ESA CCI Biomass, TRENDY NEE-IAV, TRENDY ET-median) vs. 767-site network | Beck 2023 KG, ESA CCI LC v2.1.1, CGIAR Aridity v3.1, ESA CCI Biomass v7, TRENDY v14 | **Superseded** 2026-10-02 by `fig_04_representativeness.png` above |
| `scripts/figure_representativeness_summary.R` | `review/figures/representativeness/fig_rep002_marconi.png` … `fig_rep008_jaccard_trajectory_with_counts.png` | — (see note below) | Historical-network representativeness comparisons (Marconi, La Thuile, FLUXNET2015) and the Jaccard-overlap trajectory over network history | same as fig_rep001 | Current (not touched by the Figure 4 work); `fig_rep008`'s draft-manuscript copy retired 2026-10-02 (see note below) |

**Superseded Figure 4.** `review/figures/draft_manuscript_v1/fig_04_current_network_sampling_ratios.png`
(and its `.legend.txt`), the `fig_rep001_current.png`-sourced draft-manuscript copy of the prior Figure 4,
were moved to `review/figures/draft_manuscript_v1/deprecated/` on 2026-10-02 and are no longer produced by
`scripts/build_draft_manuscript_v1.R`. `fig_rep001_current.png` itself (the source file in
`review/figures/representativeness/`) is untouched and still produced by
`scripts/figure_representativeness_summary.R`.

**Retired Figure 5.** `fig_05_jaccard_trajectory_with_counts.png`/`.legend.txt` (sourced from
`fig_rep008_jaccard_trajectory_with_counts.png`) were taken out of the draft manuscript on 2026-10-02:
moved to `review/figures/draft_manuscript_v1/deprecated/` and removed from
`scripts/build_draft_manuscript_v1.R`'s copy maps. `fig_rep008_jaccard_trajectory_with_counts.png` itself
is untouched and still produced by `scripts/figure_representativeness_summary.R`.

## Extended Data figures (`draft_manuscript_v1/SupFigs/`)

Added 2026-10-02. Nature takes no Supplementary Information figures, so every figure in this
folder is an Extended Data figure: ≤180 mm wide, ≤240 mm tall, PNG (600 dpi) + vector PDF +
300 p.p.i. JPEG, Helvetica, all text 5–7 pt except 8 pt bold lower-case panel letters. None of
these scripts are wired into `scripts/build_draft_manuscript_v1.R` — each writes directly to
`SupFigs/`.

| Script | Output | Description |
|---|---|---|
| `scripts/figure4_representativeness.R` | `supp_representativeness_geo_vs_geo.png`/`.pdf` | See the main table above. |
| `scripts/generate_whittaker_ed_three_flux.R` | `supp_whittaker_nee_gpp_ter.png`/`.pdf`/`.jpg` | Three-panel Whittaker climate-space hexbin (a NEE, stepped RdBu scale shared with Figure 2; b GPP, c TER, ONE shared STEPPED viridis scale, 500 g C m⁻² yr⁻¹ steps from 0 to 3000 plus an "above 3000" bin, one shared key in its own row below the panels) — same hexagons, points (drawn in front, same order in all three panels) and global ice-free-land contour overlay as Figure 2. Values from `compute_site_annual_fluxes()`. Stepped scale and shared-key layout revised 2026-10-02, task 5. |
| `scripts/figure_flux_comparison_combo_alt_common_siteyears.R` | `supp_flux_comparison_matched_siteyears.png`/`.pdf`/`.jpg` | FLUXNET2015-vs-Shuttle NEP/ET/H comparison restricted to matched site-years (same site **and** calendar year required on both axes) — isolates ONEFlux processing-version differences from network-composition change. Rebuilt 2026-10-02 on `compute_site_annual_fluxes()`/`compute_site_annual_fluxes_from_df()`; see `docs/known_issues.md` §10. |
| `scripts/figure_flux_comparison_six_panel.R` | `supp_flux_comparison_six_panel.png`/`.pdf`/`.jpg` | Six-panel re-plot (no new computation) of the two tables above side by side: rows NEP/ET/H, left column = Figure 3's "all qualifying site-years" data, right column = the matched-site-years data, identical axis limits within each row. |
| `scripts/generate_map_regional.R` | `supp_map_regional.png`/`.pdf`/`.jpg` | Regional network distribution (added 2026-10-02, task 8): panel a the same Equal Earth world map as Figure 1a with the four regional extents outlined; panels b–e, one Lambert Azimuthal Equal-Area projection per region (North America, Europe, East/Southeast Asia, Australia/New Zealand), each with a labelled round-length scale bar, showing only the towers inside that region's extent. |
