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

## Main-text figures (`draft_manuscript_v1/`, figure stage 6 numbering, 2026-10-02)

Renumbered under figure stage 6 (`logs/figstage_prompt.md`) to close the gap left when Figure 1
(map + cumulative site-years) and the map's Extended-Data predecessor were reworked in figure
stages 1-2. Scripts, functions and underlying data files keep their own names (per figure stage 6
rule 3) — only the `draft_manuscript_v1/` copy filenames below changed. `fig_01.*`,
`fig_01a_map_current_network.*`, `fig_01b_cumulative_siteyears_igbp.*` (the earlier,
now-superseded Figure 1 attempts) are retired in `draft_manuscript_v1/deprecated/` — see figure
stage 2 in `SESSION_LOG.md`, 2026-10-02.

| Manuscript file | Built by | Description |
|---|---|---|
| `fig_01_map_network.png`/`.pdf` | `scripts/generate_fig_map_network.R` (writes directly, no copy step) | Figure 1. Five-panel global network map: Equal Earth world overview (panel a) + four Lambert Azimuthal Equal-Area regional close-ups (b-e: North America, Europe, East/Southeast Asia, Australia/New Zealand), 183 mm wide. Promoted from the retired Extended Data `supp_map_regional` in figure stage 2. |
| `fig_02_cumulative_siteyears_igbp.png`/`.pdf` | `scripts/generate_fig_cumulative_siteyears.R` (writes directly) | Figure 2. Cumulative site-years of the current network through time by IGBP class, with Marconi/La Thuile/FLUXNET2015 historical overlay lines, 89 mm wide, no panel letter. |
| `fig_03_whittaker_current.png`/`.pdf` | `scripts/build_draft_manuscript_v1.R`, copying `review/figures/candidates/ALT_fig_02_whittaker_current.png` (source keeps its own name) | Figure 3. Whittaker climate-space hexbin of the current network, coloured by median annual NEE, with global ice-free-land contour overlay. |
| `fig_04_flux_comparison_combo_nep_et_h.png`/`.pdf` | `scripts/build_draft_manuscript_v1.R`, copying `review/figures/flux_medians/fig_flux_comparison_combo_nep_et_h.png` (source keeps its own name) | Figure 4. Per-IGBP-class median NEP/ET/H comparison, FLUXNET2015 (comparison data) vs. current FLUXNET Shuttle network. |
| `fig_05_representativeness.png`/`.pdf` | `scripts/figure4_representativeness.R` (writes directly; FIG_DIR source keeps its own name, `fig_04_representativeness.*`) | Figure 5. Six-panel representativeness figure, Geo vs Data — see the dedicated table below. |

Figure 1a's map is Equal Earth (EPSG:8857, `R/figures/fig_maps.R::.map_base_eqearth()`): canvas
height fit to the map (not a fixed square), filled no-outline semi-transparent points, separate
thinner/lighter country-border vs. coastline line layers, Antarctica/high Arctic excluded by
cropping the basemap and points to latitude [-56, 85] before projecting. Panel-letter and
scale-bar placement in panels b-e is chosen programmatically to avoid land (figure stage 2,
`scripts/generate_fig_map_network.R::.find_clear_rect()`/`.place_letter()`).

## Shared IGBP palette (task 2, 2026-10-02)

Every paper figure that colours by IGBP class (Figures 2, 4, 5, Supplementary Figure S2, and
`review/figures/presentation_figures/flux_comparison_six_panel.png`, not a numbered supplementary
figure since the 2026-10-08 restructure -- see "Supplementary Figures" below) uses
`R/plot_constants.R::PAPER_IGBP_ORDER`/`PAPER_IGBP_COLOURS`
(`scale_fill_paper_igbp()`/`scale_color_paper_igbp()`) -- Figure 4's own MODIS/061/MCD12Q1 GEE
palette (15 classes, CVM/BSV/SNO included), promoted to a shared constant. This is DIFFERENT
from the older `IGBP_order`/`IGBP_colours` (still used by other, non-paper figures) -- see
`docs/known_issues.md` §11.

## Manuscript representativeness figures (Figure 5 and supplement)

Not part of `07_figures.R` / `R/figures/` — these are standalone production scripts, each producing a
PNG + vector PDF pair plus a `.meta.json` and `.legend.txt`, copied into
`review/figures/draft_manuscript_v1/`. Renumbered Figure 4 -> Figure 5 under figure stage 6
(2026-10-02) when Figure 1 (map) and Figure 2 (cumulative site-years) were split out as their own
main-text figures (see "Main-text figures" above); `scripts/figure4_representativeness.R`'s own
name and its FIG_DIR source basenames (`fig_04_representativeness.*`,
`supp_representativeness_geo_vs_geo.*`) are unchanged — only the `draft_manuscript_v1`/`SupFigs`
copy filenames were renumbered.

| Script | Output (main location, unchanged name) | Draft-manuscript copy (renumbered) | Description | External data | Status |
|---|---|---|---|---|---|
| `scripts/figure4_representativeness.R` | `review/figures/representativeness/fig_04_representativeness.png`/`.pdf` | `fig_05_representativeness.png`/`.pdf` | **Figure 5.** Six-panel log2 sampling-ratio figure (Köppen-Geiger, IGBP land cover, aridity, biomass, NEE, ET) vs. current 781-site network, Geo vs Data | Beck 2023 KG, MODIS MCD12C1, CGIAR Aridity v3.1, ESA CCI Biomass v7, TRENDY v14, WorldClim BIO12, ERA5 | Current |
| `scripts/figure4_representativeness.R` | `review/figures/representativeness/supp_representativeness_geo_vs_geo.png`/`.pdf` | `SupFigs/figS3_sampling_gridded_at_tower.png`/`.pdf` (was `figS4_representativeness_geo_vs_geo.*` before the 2026-10-08 supplementary material restructure, SESSION_LOG.md) | **Supplementary Figure S3** (target journal Scientific Data has no Extended Data concept; `draft_manuscript_v1/` itself keeps only main-text figures). Same six panels, using the gridded product's own value at each tower (not the site's own measurement) -- what Figure 5's own legend still calls "Geo vs Geo" | same as above | Current |
| `scripts/figure_representativeness_summary.R` | `review/figures/representativeness/deprecated/fig_rep001_current.png` | — (superseded) | Prior "Figure 4": six-panel sampling ratio figure (Köppen-Geiger, ESA CCI Land Cover, CGIAR Aridity, ESA CCI Biomass, TRENDY NEE-IAV, TRENDY ET-median) vs. 767-site network | Beck 2023 KG, ESA CCI LC v2.1.1, CGIAR Aridity v3.1, ESA CCI Biomass v7, TRENDY v14 | **Superseded** 2026-10-02 by `fig_04_representativeness.png`/`fig_05_representativeness.png` above |
| `scripts/figure_representativeness_summary.R` | `review/figures/representativeness/deprecated/fig_rep002_marconi.png` … `fig_rep018_jaccard_et_median_aggregation.png`, `fig_representativeness_*.png` | — (superseded) | Historical-network representativeness comparisons (Marconi, La Thuile, FLUXNET2015), the Jaccard-overlap trajectory over network history, and per-axis diagnostic aggregation figures (Köppen/land-cover/aridity/biomass/TRENDY NEE/TRENDY ET at various class resolutions) | same as fig_rep001 | **Superseded** 2026-10-02 (see note below) |

**Superseded Figure 4.** `review/figures/draft_manuscript_v1/fig_04_current_network_sampling_ratios.png`
(and its `.legend.txt`), the `fig_rep001_current.png`-sourced draft-manuscript copy of the prior Figure 4,
were moved to `review/figures/draft_manuscript_v1/deprecated/` on 2026-10-02 and are no longer produced by
`scripts/build_draft_manuscript_v1.R`. `fig_rep001_current.png` itself (the source file in
`review/figures/representativeness/`) is untouched and still produced by
`scripts/figure_representativeness_summary.R`.

**Retired Figure 5 (original, pre-renumbering).** `fig_05_jaccard_trajectory_with_counts.png`/`.legend.txt`
(sourced from `fig_rep008_jaccard_trajectory_with_counts.png`) were taken out of the draft manuscript on
2026-10-02: moved to `review/figures/draft_manuscript_v1/deprecated/` and removed from
`scripts/build_draft_manuscript_v1.R`'s copy maps. `fig_rep008_jaccard_trajectory_with_counts.png` itself
is untouched and still produced by `scripts/figure_representativeness_summary.R`. (This "Figure 5" name
slot was later reused by figure stage 6, 2026-10-02, for the unrelated, renumbered
`fig_05_representativeness.png` above — the two are not the same figure.)

**Superseded: `scripts/figure_representativeness_summary.R`'s remaining outputs.** Figure stage 5
(`SESSION_LOG.md`, 2026-10-02) flagged their retirement as conditional on figure stage 3 (the
representativeness trajectory supplement, now Supplementary Figure S5) being marked DONE in
`review/figstage_status.md`. Stage 3 was closed out in the close-out session (2026-10-02): its
current-network Geo-vs-Geo values were reconfirmed to reproduce
`representativeness_metrics_fig4.csv`'s six rows exactly, and its figure staged as
`SupFigs/figS5_sampling_collections.*` (was `figS6_representativeness_trajectory.*` before the
2026-10-08 supplementary material restructure). With stage 3 DONE, all 46 remaining
`scripts/figure_representativeness_summary.R` outputs (`fig_rep001`-`fig_rep018` and
`fig_representativeness_*`, `.png` + `.legend.txt` companions) were `git mv`'d into
`review/figures/representativeness/deprecated/` in the same close-out session. The script itself is
unchanged and still runnable — it would simply write its outputs to that same `deprecated/`-sibling
location again on a fresh run; it is not deleted or renamed, per figure-stage rule 3.

## Supplementary Figures (`draft_manuscript_v1/SupFigs/`)

**Renumbered S1–S6 in the supplementary material restructure (2026-10-08, SESSION_LOG.md)** --
old-stem -> new-stem, with `figS3_flux_comparison_six_panel` dropped from the numbering entirely
(moved to `review/figures/presentation_figures/`, see below):

| Old # (added 2026-10-02) | Old stem | New # | New stem |
|---|---|---|---|
| S1 | `figS1_whittaker_nee_gpp_ter` | S1 | `figS1_whittaker_nee_gpp_reco` |
| S2 | `figS2_flux_comparison_matched_siteyears` | S2 | `figS2_flux_comparison_matched_siteyears` (unchanged) |
| S3 | `figS3_flux_comparison_six_panel` | -- | moved to `presentation_figures/flux_comparison_six_panel` (no longer numbered) |
| S4 | `figS4_representativeness_geo_vs_geo` | S3 | `figS3_sampling_gridded_at_tower` |
| S5 | `figS5_flux_representativeness` | S4 | `figS4_sampling_flux_axes` |
| S6 | `figS6_representativeness_trajectory` | S5 | `figS5_sampling_collections` |
| S7 | `figS7_record_length` | S6 | `figS6_record_length` |

Wording changes applied throughout the plotting code, `.legend.txt` files and the compiled PDF for
the figures listed: "Representativeness" -> "Network sampling" (S3, S4, S5 titles); "FLUXNET
Shuttle"/"Current"/"current FLUXNET network" -> "the snapshot" (S1, S2, S5, S6, and the two figures
moved to `presentation_figures/`); "TER" -> "RECO" (S1, S4); "Koppen" -> "Köppen" (S5 key); "Geo vs
Geo"/"Geo vs Data" -> "gridded value at the tower"/"the site's own value" (S3/S4 titles and panel
headers). Figure 5 (main text) is unchanged in both content and wording -- only its cross-references
to its companion figure's number were updated (S4 -> S3).

Old-S1-S6 PNG/PDF/JPEG/`.legend.txt` (and `.meta.json` where present) moved to
`SupFigs/deprecated/`, not deleted, per CLAUDE.md. The compiled PDF
(`SupFigs/supplementary_figures.pdf`) was rebuilt and renamed
`SupFigs/supplementary_information.pdf` (also now includes Tables S1–S4 as formatted table pages
after the six figure pages, and a publication-facing legend -- see below); the old
`supplementary_figures.pdf` moved to `SupFigs/deprecated/` alongside the superseded figures.

Target journal Scientific Data has no Extended Data concept — every figure in this folder is a
Supplementary Figure: ≤180 mm wide, ≤240 mm tall, PNG (600 dpi) + vector PDF + 300 p.p.i. JPEG,
Helvetica, all text 5–7 pt except 8 pt bold lower-case panel letters
(`scripts/check_figure_format.R` enforces the size rules; its own internal
`extended_data`/`ED_*`/`NATURE_ED_*` names are unchanged code identifiers, not user-facing
terminology). None of these scripts are wired into `scripts/build_draft_manuscript_v1.R` — each
writes directly to `SupFigs/`. Scripts, functions and underlying data/table files keep their own
names (figure stage 6 rule 3, still honoured by the 2026-10-08 restructure) — only the `SupFigs/`
copy filenames changed.

| # | Script | FIG_DIR / canonical source name | `SupFigs/` copy | Description |
|---|---|---|---|---|
| S1 | `scripts/generate_whittaker_ed_three_flux.R` | (writes directly to `SupFigs/`, no separate FIG_DIR source) | `figS1_whittaker_nee_gpp_reco.png`/`.pdf`/`.jpg` | Three-panel Whittaker climate-space hexbin (a NEE, stepped RdBu scale shared with Figure 3; b GPP, c RECO, ONE shared STEPPED viridis scale, 500 g C m⁻² yr⁻¹ steps from 0 to 3000 plus an "above 3000" bin, one shared key in its own row below the panels) — same hexagons, points (drawn in front, same order in all three panels) and global ice-free-land contour overlay as Figure 3. Values from `compute_site_annual_fluxes()`. Stepped scale and shared-key layout revised 2026-10-02, task 5. |
| S2 | `scripts/figure_flux_comparison_combo_alt_common_siteyears.R` | (writes directly to `SupFigs/`) | `figS2_flux_comparison_matched_siteyears.png`/`.pdf`/`.jpg` | FLUXNET2015-vs-snapshot NEP/ET/H comparison restricted to matched site-years (same site **and** calendar year required on both axes) — isolates ONEFlux processing-version differences from network-composition change. Rebuilt 2026-10-02 on `compute_site_annual_fluxes()`/`compute_site_annual_fluxes_from_df()`; see `docs/known_issues.md` §10. |
| S3 | `scripts/figure4_representativeness.R` | `review/figures/representativeness/supp_representativeness_geo_vs_geo.png`/`.pdf`/`.jpg` | `figS3_sampling_gridded_at_tower.png`/`.pdf`/`.jpg` | See the main representativeness table above. Companion to Figure 5 (what Figure 5's own legend calls Geo vs Geo; this figure's own wording: "gridded value at the tower"). |
| S4 | `scripts/figure_flux_representativeness_supp.R` | `review/figures/representativeness/supp_flux_representativeness.png`/`.pdf`/`.jpg` | `figS4_sampling_flux_axes.png`/`.pdf`/`.jpg` | Eight panels: four fluxes (NEE, GPP, RECO, ET) x two comparisons (gridded value at the tower, the site's own value). NEE/ET panels (a/b/g/h) are Figure 5's own panels e/f, confirmed programmatically identical (n/J) before rendering. GPP/RECO (c/d/e/f) are new: TRENDY v14 S3 17-model ensemble-median (1991–2020 mean, RECO = ra+rh) vs. tower medians from `compute_site_annual_fluxes()`. Table: `data/snapshots/representativeness_metrics_flux_supp.csv`. Added figure stage 4, 2026-10-02. |
| S5 | `scripts/figure_representativeness_trajectory.R` | `review/figures/representativeness/supp_representativeness_trajectory.png`/`.pdf`/`.jpg` | `figS5_sampling_collections.png`/`.pdf`/`.jpg` | Gridded-value-at-the-tower weighted Jaccard similarity (J), Figure 5's own six axes, tracked across four FLUXNET network generations (Marconi, La Thuile, FLUXNET2015, the snapshot, 781-site). Classes/bin edges/land grids/J-definition taken as-is from `scripts/figure4_representativeness.R`'s own committed tables, not redefined. New raster extractions for the three historical networks: MODIS MCD12C1 IGBP (nearest cell) and TRENDY model NEE/GPP/ET (bilinear). Snapshot values confirmed to reproduce `representativeness_metrics_fig4.csv`'s six Geo-vs-Geo rows exactly. Table: `data/snapshots/representativeness_metrics_trajectory.csv`. Figure stage 3 (2026-10-02); closed out (committed, numbered) in the close-out session, 2026-10-02 — see `review/figstage_status.md` Stage 3 entry. |
| S6 | `scripts/supp_stage2_record_length_collections_figure.R` | (writes directly to `SupFigs/`) | `figS6_record_length.png`/`.pdf`/`.jpg` | Record length of the snapshot and across three prior FLUXNET collections (Marconi, La Thuile, FLUXNET2015) — panel a: distribution of years-with-data per site in the snapshot, by IGBP class; panel b: share of each collection's sites with at least n years of data. |

**Retired.** `scripts/generate_map_regional.R` → `supp_map_regional.png`/`.pdf`/`.jpg`: regional
network distribution (panel a the Equal Earth world map with the four regional extents outlined;
panels b–e, one Lambert Azimuthal Equal-Area projection per region). Retired to
`SupFigs/deprecated/` in figure stage 2 (2026-10-02) when its five panels were promoted to the
main-text `fig_01_map_network` (see "Main-text figures" above) — not part of the figS1–S6
numbering.

## Presentation figures (`review/figures/presentation_figures/`)

Added 2026-10-08 (supplementary material restructure, SESSION_LOG.md). A pool of talk/presentation
companion figures, beside `draft_manuscript_v1/`, not journal-submission content; neither is a
numbered Supplementary Figure, but both are still validated by `scripts/check_figure_format.R`
(run explicitly against this directory) using the same Extended-Data-style size/font/text/line/edge
rules as everything in `SupFigs/`.

| Script | `presentation_figures/` output | Description |
|---|---|---|
| `scripts/figure_flux_comparison_six_panel.R` | `flux_comparison_six_panel.png`/`.pdf`/`.jpg` | Six-panel re-plot (no new computation) of the Figure 4 and Supplementary Figure S2 comparison tables side by side: rows NEP/ET/H, left column = Figure 4's "all qualifying site-years" data, right column = the matched-site-years data (Supplementary Figure S2), identical axis limits within each row. Was `SupFigs/figS3_flux_comparison_six_panel.*` before this restructure; the "figS3_" stem prefix was dropped (not a hand rename -- the script itself now writes under the new stem) since it is no longer part of the S-numbering. |
| `scripts/generate_fig02_historical_only.R` | `fig_02_historical_only.png`/`.pdf`/`.jpg` | A presentation "reveal" companion to Figure 2: identical x/y axes, dashed reference lines, and historical-collection lines as Figure 2, with the snapshot's IGBP-stacked area and its "IGBP" legend omitted — the y axis is still driven by the (undrawn) snapshot totals via `fig_cumulative_siteyears_igbp(show_current_network = FALSE)`, so it is a strict visual subset of Figure 2 on the same scale, meant to be shown immediately before it in a talk. Was `SupFigs/fig_02_historical_only.*` (added 2026-10-06) before this restructure; stem unchanged, only the directory moved. |

## Supplementary Tables (`draft_manuscript_v1/SupTables/`)

Added/renamed in the supplementary material restructure (2026-10-08, SESSION_LOG.md).

| Table | Script | Description |
|---|---|---|
| `tableS1_regional_networks.csv` | `scripts/supp_stage5_regional_networks_table.R` (new) | One row per regional network, from the snapshot of record (`fluxnet_shuttle_snapshot_20260920T102211.csv`): network code, name, processing hub (snapshot's own `data_hub` field), number of sites, number of site-years (the paper's own definition, same as Figure 2). Checked against sites=781, hub totals AmeriFlux=381/ICOS=348/TERN=52, site-years=6,200; the script stops rather than writing the table if any check fails or a product code is not in its known list. |
| `tableS2_record_length_by_igbp.csv` | `scripts/supp_stage1_record_length_by_igbp.R` | Was `tableS_record_length_by_igbp.csv`; regenerated under the new name, unchanged content. |
| `tableS3_sampling_ratios_by_axis.csv` | `scripts/supp_stage3_sampling_ratios.R` | Was `tableS_sampling_ratios_by_axis.csv`; regenerated under the new name, unchanged content. |
| `tableS4_bowen_ratio_by_igbp.csv` | `scripts/supp_stage4_bowen_ratio_by_igbp.R` | Was `tableS_bowen_ratio_by_igbp.csv`; regenerated under the new name, unchanged content. |
| `supplementary_data_1_sites.csv` | `scripts/supp_stage6_sites_data_table.R` (new) | One row per site from the snapshot of record: site ID, product name, product version (parsed from the product name, e.g. `v1.3_r1`), network code, DOI or handle (`product_id`). |

`tableS_sampling_ratio_extremes.csv` and `tableS_sampling_ratio_jaccard_check.csv` (both from
`scripts/supp_stage3_sampling_ratios.R`) moved out of `SupTables/` to
`review/diagnostics/sampling_ratio_checks/` — validation/derived-check outputs, not supplementary
submission tables; names unchanged, only the location moved. Old copies of all five renamed/moved
tables are in `SupTables/deprecated/`, not deleted, per CLAUDE.md.

