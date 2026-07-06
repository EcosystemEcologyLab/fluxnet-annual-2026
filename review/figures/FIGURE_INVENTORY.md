# Figure Inventory — review/figures/

A findability guide to every figure under `review/figures/`, written so co-authors can locate the pool of alternatives to whatever is in the current draft. It describes categories, not individual figures. Each immediate subdirectory of `review/figures/` is one category. Documentation files (`.md`, `.txt`, `.legend.txt`) are not counted as figures.

Scope counted: 199 image files (`.png` only in practice; no `.pdf/.svg/.jpg/.eps/.tif` were found). Of these, 183 sit in 17 category subdirectories and 16 are loose images directly under `review/figures/` (the unfiled group). Descriptions are drawn from each category's sibling `.legend.txt` where one exists, and from the writing script otherwise.

## What is in the draft, and where its alternatives live

The current draft holds six figures in `draft_manuscript_v1/`. Each was copied from a source category by `scripts/build_draft_manuscript_v1.R`. For each: the source, and the sibling category to browse for a replacement.

Figure 1A (current network map): from `candidates/fig_03_map_current.png`. Alternatives: `maps_point/` (same map plus an aridity-coloured variant) and `maps/` (network maps across snapshots and historical releases).

Figure 1B (cumulative site-years by IGBP): from `network/fig_dur11_CumulativeSiteYears_IGBP.png`. Alternatives: elsewhere in `network/` (per-year and network-growth variants).

Figure 2 (Whittaker climate space with global-land contours): from `whittaker/fig_whit_fig2_with_both_contours.png`. This is the both-contours version, not the plain no-contour output. Alternatives: `whittaker/` (single-contour and no-contour variants).

Figure 3 (FLUXNET2015 vs Shuttle flux medians, NEP/ET/H combo): from `flux_medians/fig_flux_comparison_combo_nep_et_h.png`. Alternatives: `flux_medians/` (per-flux comparison panels and single-network by-IGBP panels).

Figure 4 (current-network sampling ratios): from `representativeness/fig_rep001_current.png`. Alternatives: `representativeness/` (other networks and aggregation-sensitivity variants).

Figure 5 (Jaccard representativeness trajectory with counts): from `representativeness/fig_rep008_jaccard_trajectory_with_counts.png`. Alternatives: `representativeness/` (the same trajectory without count bars, and finer-resolution variants).

## Draft mapping (verbatim source paths from the build script)

`scripts/build_draft_manuscript_v1.R` copies each draft PNG from these sources (paths quoted exactly as they appear in the script's `figs` copy map):

Figure 1A `fig_01a_map_current_network.png` from `review/figures/candidates/fig_03_map_current.png` — category candidates.

Figure 1B `fig_01b_cumulative_siteyears_igbp.png` from `review/figures/network/fig_dur11_CumulativeSiteYears_IGBP.png` — category network.

Figure 2 `fig_02_whittaker_current.png` from `review/figures/whittaker/fig_whit_fig2_with_both_contours.png` — category whittaker.

Figure 3 `fig_03_flux_comparison_combo_nep_et_h.png` from `review/figures/flux_medians/fig_flux_comparison_combo_nep_et_h.png` — category flux_medians.

Figure 4 `fig_04_current_network_sampling_ratios.png` from `review/figures/representativeness/fig_rep001_current.png` — category representativeness.

Figure 5 `fig_05_jaccard_trajectory_with_counts.png` from `review/figures/representativeness/fig_rep008_jaccard_trajectory_with_counts.png` — category representativeness.

Figure 2 check: the build script points to `fig_whit_fig2_with_both_contours.png`. This is the intended both-contours version. The mapping is not stale; it does not point at the plain no-contour Whittaker output (`fig_whit01_ShuttleFull.png`).

## Categories

### candidates
Path `review/figures/candidates/`, 12 figures. The curated set of paper-candidate figures under one descriptive-name scheme (network growth, duration profile, current and snapshot maps, Whittaker, latitudinal, environmental response, a BADM management supplement). This is a mixed staging pool rather than one axis. Feeds the draft: `fig_03_map_current.png` is the Figure 1A source. The rest are alternatives, several of which are the single-panel originals behind other draft figures.

### network
Path `review/figures/network/`, 16 figures. Network growth and record-duration over time: per-dataset duration profiles (Shuttle, Marconi, La Thuile, FLUXNET2015, and Shuttle snapshots), site-year accumulation by year and by IGBP class, data latency by subregion, and active-proportion. Feeds the draft: `fig_dur11_CumulativeSiteYears_IGBP.png` is the Figure 1B source. Most likely drop-in alternative to Figure 1B: `fig_dur10_SiteYearsByYear_IGBP.png` (annual rather than cumulative, same IGBP partition) or `fig_network_growth.png`.

### whittaker
Path `review/figures/whittaker/`, 15 figures. The network placed in mean-annual-temperature by mean-annual-precipitation climate space (Whittaker), as a hexbin coloured by median NEE, with optional global ice-free-land density contours overlaid. Includes per-dataset and per-snapshot panels plus global land-density surfaces. Feeds the draft: `fig_whit_fig2_with_both_contours.png` is the Figure 2 source. Most likely drop-in alternatives to Figure 2: `fig_whit_fig2_with_95contour.png` or `fig_whit_fig2_with_99contour.png` (single-contour versions of the same base hexbin); `fig_whit01_ShuttleFull.png` is the no-contour base.

### flux_medians
Path `review/figures/flux_medians/`, 11 figures. Per-IGBP-class median flux values (NEP, GPP, TER, ET, H): both a FLUXNET2015-versus-Shuttle comparison family (a combined NEP/ET/H panel plus one figure per flux) and a single-network by-IGBP family. Feeds the draft: `fig_flux_comparison_combo_nep_et_h.png` is the Figure 3 source. Most likely drop-in alternatives to Figure 3: the individual comparison panels `fig_flux_comparison_nep.png`, `fig_flux_comparison_et.png`, `fig_flux_comparison_h.png` (the three combined into the draft), or the GPP/TER comparison panels if the flux set changes.

### representativeness
Path `review/figures/representativeness/`, 44 figures (including a `trendy_preview/` subdirectory of 6). How well the network samples global land across classification axes (Köppen-Geiger, land cover, aridity, biomass, TRENDY-derived NEE/ET), reported as per-class sampling ratios and as weighted-Jaccard trajectories across network generations. This category holds many near-identical variants: 4 single-network sampling-ratio grids plus 2 network-comparison panels (`fig_rep001`–`006`); 4 Jaccard-trajectory variants (`fig_rep007`–`010`); 8 aggregation- or bin-sensitivity trajectories (`fig_rep011`–`018`); and roughly 20 per-axis representativeness panels (`fig_representativeness_*`, including present-day and future-scenario Köppen variants); the 6 `trendy_preview/` items are model-ensemble previews. Feeds the draft: `fig_rep001_current.png` (Figure 4) and `fig_rep008_jaccard_trajectory_with_counts.png` (Figure 5). Most likely drop-in alternatives: for Figure 5, `fig_rep007_jaccard_trajectory.png` (the identical trajectory without the count-bar overlay); for Figure 4, `fig_rep005_fluxnet2015_vs_current.png` (overlays the prior release against the current network on the same sampling-ratio axes).

### maps
Path `review/figures/maps/`, 9 figures. Global network location maps rendered per dataset and per snapshot (Shuttle full, Marconi, La Thuile, FLUXNET2015, Shuttle snapshots 2000/2007/2015) plus two stacked composites. None feed the draft directly; the whole category is an alternatives pool for the Figure 1A map (a fuller styling and a per-snapshot series).

### maps_point
Path `review/figures/maps_point/`, 4 figures. Point-style location maps for the current network and for snapshots, in a plain variant and an aridity-coloured variant. This is the generator's output directory for the Figure 1A map; `fig_03_map_current.png` here is byte-identical to the `candidates/` copy the draft uses. None are wired into the draft directly, so all four are alternatives; the aridity-coloured `fig_03_map_current_aridity.png` is the most likely drop-in if a co-author wants the map to carry a climate overlay.

### historical
Path `review/figures/historical/`, 7 figures. Side-by-side comparisons of the current network against prior static releases: a multi-dataset choropleth and per-year (2000/2007/2015) duration and Whittaker comparison panels. None feed the draft; all are alternatives, chiefly for framing network evolution.

### timeseries
Path `review/figures/timeseries/`, 10 figures. Regional flux time series, one figure per UN subregion. None feed the draft; the category is an alternatives pool for a regional-breakdown figure. No legends; described from the writing script `scripts/00_candidate_figures.R`.

### envresponse
Path `review/figures/envresponse/`, 10 figures. Environmental-response scatter of fluxes against climate drivers: two predictors (precipitation `P_F`, air temperature `TA_F`) crossed with five flux responses (NEE, GPP, RECO, LE, H). None feed the draft; all are alternatives for an environmental-response figure. No legends; described from `scripts/00_candidate_figures.R`.

### climate
Path `review/figures/climate/`, 2 figures. Composite environmental-response figures, an ERA5-driven variant and a WorldClim-driven variant. None feed the draft; both are alternatives. No legends; described from `scripts/generate_env_response_era5.R`. Note: `fig_environmental_response_era5.png` is modified in the working tree from a prior session (a pre-existing change, unrelated to this inventory).

### anomalies
Path `review/figures/anomalies/`, 2 figures. Data-availability heatmaps (UN subregion by GEZ, faceted by IGBP) showing counts of sites with at least 5 and at least 10 valid NEE years. None feed the draft; both are alternatives. No legends; described from `scripts/generate_availability_heatmap.R`.

### Anomalies_GEZ
Path `review/figures/Anomalies_GEZ/`, 8 figures. Flux-anomaly context panels for qualifying IGBP by UN-subregion by FAO Global Ecological Zone combinations (one figure per combination). None feed the draft; all are alternatives for a stratified anomaly figure. No legends; described from `scripts/generate_gez_anomaly_figures.R`.

### Anomalies_KG
Path `review/figures/Anomalies_KG/`, 23 figures. The Köppen-Geiger counterpart of Anomalies_GEZ: flux-anomaly context panels for IGBP by UN-subregion by Köppen class, provided at two aggregation levels (`level1/` and `level2/`, 10 each), plus 3 Köppen availability heatmaps at the category root. Many near-identical stratified variants; none feed the draft. No legends; described from `scripts/generate_kg_anomaly_figures.R` and `scripts/generate_kg_availability_heatmaps.R`.

### diagnostic
Path `review/figures/diagnostic/`, 3 figures. Network diagnostic maps by hub, by IGBP, and by network membership (`fig_diag_map_hub.png`, `fig_diag_map_igbp.png`, `fig_diag_map_network.png`). None feed the draft. Provenance note: no current writing script and no legend were found for these files, so the description is inferred from the filenames only; a reader wanting exact axes/data source should regenerate or locate the original diagnostic script.

### latitudinal
Path `review/figures/latitudinal/`, 1 figure. A single latitudinal ribbon plot of fluxes against latitude. Does not feed the draft; it is an alternative for a latitudinal-gradient figure. No legend; described from `R/figures/fig_latitudinal.R`. A near-identical loose copy sits in the unfiled group (see below).

### draft_manuscript_v1
Path `review/figures/draft_manuscript_v1/`, 6 figures. This is the assembled current draft (the destination of the build script), not an alternatives pool. Its six figures are the ones mapped above; each has a sibling `.legend.txt` here.

## Unfiled loose images (directly under review/figures/)

16 loose `.png` files sit directly in `review/figures/` with no category subdirectory: resolution-labelled flux panels (`fig_dd_*`, `fig_mm_*`, `fig_yy_*` for NEE/GPP/LE/ET), five per-continent time series (`fig_timeseries_africa/americas/asia/europe/oceania`), `fig_latitudinal_multi.png`, and `fig_network_growth_annual.png`. These are older or one-off outputs that were written to the figures root rather than into a category; they overlap in intent with the `timeseries/`, `latitudinal/`, and `network/` categories. None feed the draft. Two loose documentation files (`methods_badm_management.md`, `methods_flux_medians.md`) also sit here and are documentation, not figures.

## Notes

No category was empty and every category directory opened successfully. The only category whose provenance could not be traced to a current script is `diagnostic/` (noted above). Counts in this document are recursive within each category (so `representativeness` includes its `trendy_preview/` subdirectory, and `Anomalies_KG` includes its `level1/` and `level2/` subdirectories).
