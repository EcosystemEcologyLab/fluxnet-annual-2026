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

**Geo vs Data** = tower-measured annual value: the Step-3 annual method (mean monthly cycle across all
QC≥0.80-qualifying years, all 12 calendar months required, then summed), with the per-site VUT→CUT
fallback (731 VUT, 49 CUT, 1 neither, for the current 781-site network). **Geo vs Geo** = the model's own
value at the tower cell (bilinear extraction).

**Colours.** NEE uses a diverging ramp (sink bars darken with sink strength, a near-neutral −25-to-0 bin,
a contrasting warm hue for the source bar >0) — fixed from an earlier sequential-green version that made
the source bar read as the strongest sink. ET uses a sequential blue ramp. Both share bar 1's bare/ice
colour from the biomass axis's own palette.

**Results** match this session's `flux_bin_breaks.R` validation run exactly (same edges: NEE
−250/−100/−50/−25/0 gC m⁻² yr⁻¹; ET 200/350/450/600/850 mm/yr), confirming the port is faithful: NEE Geo
vs Data n=601/781, J=0.162; NEE Geo vs Geo n=781/781, J=0.530; ET Geo vs Data n=634/781, J=0.456; ET Geo
vs Geo n=781/781, J=0.456.

Outputs: `data/snapshots/site_nee_fig4.csv`, `site_et_fig4.csv`, `nee_et_fig4_global_distribution.csv`.
