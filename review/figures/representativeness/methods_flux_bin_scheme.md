**This note supports:** Figure 4 panels E (NEE) and F (ET), both Geo vs Geo and Geo vs Data sides.
Figure 4 does not use the TRENDY NEE-IAV/ET-IAV/NEE-median/ET-median axes described in
`methods_trendy_iav.md` — that note still applies to `fig_rep001–008` panels E/F.

Panels E (NEE) and F (ET) of the new Figure 4 use the bin scheme first built and validated in
`scripts/diagnostics/flux_bin_breaks.R` (a diagnostic, run earlier the same session), ported — not
sourced — into `scripts/figure4_representativeness.R` as production panels E and F.

**Land mask.** The Beck et al. (2023) Köppen-Geiger mask at 0.5°, matching the resolution of the TRENDY
v14 ensemble rasters. Land total is the TRENDY ensemble's own valid-data footprint under that mask
(163,331,649 km²) — a small number of KG-classified land cells (islands, ice-sheet margins) have no
TRENDY model data and are excluded from this total, consistently across both panels.

**Bar 1 (fixed near-zero bin).** GPP < 5 gC m⁻² yr⁻¹ (model TRENDY ensemble-median GPP at that cell),
used as a shared vegetation mask for both NEE and ET. For ET the cut is on ET's own value; for NEE the
cut is on model GPP specifically, since NEE is signed and cannot be cut on its own magnitude near zero. A
site's bar-1 membership is decided by the model value at its cell (never the tower-measured value), so
membership is identical between the Geo vs Data and Geo vs Geo comparisons for a given site.

**Bars 2–7.** Rounded sextiles (5 edges) of the 50/50 mixture of the land CDF and the tower CDF, computed
outside bar 1 only. NEE edges rounded to the nearest 25 gC m⁻² yr⁻¹; ET to the nearest 50 mm yr⁻¹. Outer
bars stay open-ended regardless of rounding.

**ET raster.** The dedicated 1991–2020, 17-model TRENDY ensemble-median raster built for this work
(`flux_bin_breaks_et_median_1991_2020.tif`), not the older committed `trendy_et_median.tif` (a
1990–2023-window, 16-model product — a different window and ensemble than the other TRENDY-derived axes
in this figure; see the 2026-10-01 draft-Fig-4-audit SESSION_LOG entry for why that mismatch mattered).

**Geo vs Data** = tower-measured annual value via `R/site_annual_fluxes.R::compute_site_annual_fluxes()`
(revised 2026-10-02; this shared function also serves Figures 2/3) — a tower's value is the **median of
its `QC_THRESHOLD_YY`-qualifying annual values** (`R/pipeline_config.R`, currently 0.50; at least one
qualifying year required), reading the pre-QC `annual` table directly rather than `annual_qc`/
`annual_converted` (which drop a whole row whenever that site's NEE QC fails, which would wrongly
discard ET years with a perfectly good `LE_F_MDS_QC`). This replaces the mean-monthly-cycle,
QC≥0.80-qualifying-months method used until this revision — that threshold (0.80) was copied from
`scripts/assess_flux_data_by_igbp_shuttle.R` and was never the paper's actual QC gate; see
`docs/known_issues.md` §10. The QC-gated source variable differs by panel: **NEE** (panel E) uses the
per-site VUT→CUT fallback chosen by the same rule `scripts/04_qc.R` applies (`NEE_VUT_REF`/
`NEE_VUT_REF_QC` where the site has any non-NA VUT QC, else `NEE_CUT_REF`/`NEE_CUT_REF_QC`; 616 VUT, 40
CUT, 125 neither, for the current 781-site network's `annual` table); **ET** (panel F) always uses
`LE_F_MDS` gated on its own `LE_F_MDS_QC` ≥ `QC_THRESHOLD_YY`, independent of the NEE gate and with no
VUT/CUT distinction — that fallback is specific to the carbon-flux variables and does not apply to ET.
**Geo vs Geo** = the model's own value at the tower cell (bilinear extraction), for both panels —
unaffected by this revision (it was never tower-QC-gated).

**Geo vs Data site count (revised 2026-10-02).** A site is counted in a panel's Geo vs Data side only
if it has a qualifying tower value for *that panel's own flux* — including bar 1: previously, a site
whose model mask value (model GPP for NEE; model ET for ET) fell below the bar-1 cut was counted as
bar 1 regardless of whether it had any tower value at all, so four towers with no qualifying NEE
(`CA-Mtk`, `GL-ZaH`, `GL-ZaF`, `SJ-Adv`) were counted as "unvegetated" in panel E with no actual NEE
value behind them. `classify_flux_sites(..., require_own=TRUE)` now excludes a site from Geo vs Data
entirely (not just from bar 1) when its own tower value is missing; Geo vs Geo is unaffected (its own
value is the model's, never missing where the mask value is present).

**Colours.** NEE uses a diverging ramp (sink bars darken with sink strength, a near-neutral −25-to-0 bin,
a contrasting warm hue for the source bar >0) — fixed from an earlier sequential-green version that made
the source bar read as the strongest sink. ET uses a sequential blue ramp. Both share bar 1's bare/ice
colour from the biomass axis's own palette.

**Bin edges (rounded sextiles of the 50/50 land/tower mixture, recomputed 2026-10-02 against the new
QC_THRESHOLD_YY-gated tower values): unchanged after rounding** — NEE −250/−100/−50/−25/0 gC m⁻² yr⁻¹;
ET 200/350/450/600/850 mm/yr, identical to this session's `flux_bin_breaks.R` validation run and to the
edges under the retired QC≥0.80/mean-monthly-cycle method.

**n and J, before (QC≥0.80, mean monthly cycle) vs after (QC_THRESHOLD_YY=0.50, median of qualifying
years) this revision:** NEE Geo vs Data n=601→656/781, J=0.162→0.165; ET Geo vs Data n=634→665/781,
J=0.456→0.479. Geo vs Geo is unchanged for both (NEE n=781/781, J=0.530; ET n=781/781, J=0.456), as
expected since it was never tower-QC-gated. 69 towers gained and 14 lost for NEE; 39 gained and 8 lost
for ET (full site lists: `SESSION_LOG.md`, 2026-10-02 entry for this revision).

Outputs: `data/snapshots/site_nee_fig4.csv`, `site_et_fig4.csv`, `nee_et_fig4_global_distribution.csv`.
