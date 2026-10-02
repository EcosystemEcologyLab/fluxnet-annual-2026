**This note supports:** `docs/methods_requirements.md` §5.2/§5.4 (site-level Köppen
classification used throughout the main pipeline, `site_koppen_era5.csv`) and Figure 4
panel A's Geo vs Data side (`site_koppen_era5_fig4.csv`; see "Two output files" below
and `methods_precip_exclusions.md`). Not used by fig_rep001–008 (`figure_representativeness_summary.R`
reads the same `site_koppen_era5.csv` production file as the main pipeline, not a
Figure-4-specific one).

Per-site Köppen-Geiger (KG) classes for the current 781-site FLUXNET Shuttle
network are computed locally from each site's own ERA5 monthly reanalysis data,
rather than extracted from an external map (contrast `methods_koppen_beck2023.md`,
which remains the method for the global land-area backdrop, the future-scenario
figures, and historical-network comparisons — none of which are Shuttle sites with
bundled ERA5 monthly data). This follows the method used by ICOS's
`KG_classification` script: the Beck et al. classification rule cascade applied to
a 30-year monthly temperature/precipitation normal, rather than a self-reported
metadata field or a raster sample. Implemented in `R/climate_classification.R`
(`classify_koppen_geiger()`, `compute_era5_monthly_climatology()`,
`compute_site_koppen_era5()`), run by `scripts/step5_compute_koppen_era5.R`,
output `data/snapshots/site_koppen_era5.csv` (781 rows as of the 2026-09-01
snapshot pin; `step5_compute_koppen_era5.R`'s own header comment still says
"current 767-site" — stale text, not a code bug, since the snapshot pin itself
is current — flagged here rather than edited, per instruction to leave code as-is).

**Why this replaces the previous two sources for the current network.** Before
this change, the `Anomalies_KG` figures read the `CLIMATE_KOEPPEN` BADM metadata
field (a self-reported value of undocumented provenance, NA for sites without a
BADM entry), while the `representativeness` figures extracted from the Beck 2023
raster at each site's coordinates. These two sources disagreed for some sites and
neither is derived from the FLUXNET Shuttle data itself. This single ERA5-derived
source is now used by both figure families for the current network, so the same
site shows the same KG class everywhere.

**Data source.** FLUXNET Shuttle bundles a standalone `*_FLUXNET_ERA5_MM_*.csv`
per site containing a full 1981–2025 monthly ERA5 reanalysis record
(`TA_ERA`, `P_ERA`, and other variables), independent of the tower's own
operational years. This is already ingested into the pipeline's DuckDB `monthly`
table as `dataset = 'ERA5'` rows (raw, pre-QC — the QC gating in `04_qc.R` is
keyed on flux-variable QC flags that these climate-only rows carry as NA, so the
classification script reads the raw `monthly` table directly, not
`monthly_qc`/`monthly_converted`).

**Units gotcha.** `P_ERA` is a **daily-mean** value (mm/day), not a monthly
total — confirmed directly against on-disk `*_FLUXNET_ERA5_MM_*.csv` files
(values in the 0.2–2.5 mm/day range, implausible as monthly totals for any
climate). `compute_era5_monthly_climatology()` multiplies by the number of days
in each month before summing, matching the ICOS reference implementation. Missing
this step would silently corrupt every aridity/seasonality threshold in the
classification.

**Climatology period.** 1991–2020, matching Beck et al. (2023)'s own "present-day"
reference window, so the retained Beck-raster comparison column stays meaningful.
Sites with fewer than `KG_ERA5_MIN_YEARS` (20 of the 30 candidate years,
`R/pipeline_config.R`) valid years after screening are left unclassified (`NA`)
and logged to `outputs/unknown_log.csv` rather than classified from insufficient
data.

**Precipitation outlier screening.** Site-years with a computed annual `P_ERA`
total above `KG_ERA5_MAP_MAX_MM` (5000 mm/yr, `R/pipeline_config.R`) are dropped
before averaging and logged to `outputs/exclusion_log.csv`. This threshold matches
the known ERA5 spatial-averaging artifact documented in `docs/known_issues.md`
§9a (3.7% of FLUXMET site-years affected, concentrated at coastal/high-relief
sites), applied here to the climatology computation rather than left unscreened.

**Classification algorithm.** `classify_koppen_geiger()` is a direct, faithful
port of the boolean rule cascade in ICOS's `KG_classificator_data()` (itself
based on Beck's original MATLAB implementation) — the same Pdry/Psdry/Pswet/
Pwdry/Pwwet/Pthresh construction and A/B/C/D/E branch logic, translated
Python-to-R one-for-one with no reinterpretation. The "summer" half-year is
determined dynamically as whichever of April–September or October–March is
warmer at each site (not by a hemisphere flag) — this is what makes the
algorithm correct for both hemispheres without any special-casing.

**Comparison columns.** `site_koppen_era5.csv` retains `badm_kg_class` (BADM
`CLIMATE_KOEPPEN`) and `beck2023_kg_class` (Beck 2023 raster extraction) with
`agree_badm`/`agree_beck2023` flags, mirroring the map-vs-data comparison the
ICOS reference script itself performs. The ERA5 result is authoritative for all
current-network figures; the comparison columns are for QA and methods
reporting, not for classification. Agreement percentages from the most recent
run are printed in the script's console output and recorded in `SESSION_LOG.md`.

**Two output files (added 2026-10-02 for Figure 4).** This method now produces two
site-level CSVs that differ only in whether the precipitation outlier screen above
is applied:

- `data/snapshots/site_koppen_era5.csv` — the original, screened file described
  throughout this note (`KG_ERA5_MAP_MAX_MM` applied, site-years above 5000 mm/yr
  dropped before averaging). This remains the classification source for the main
  pipeline (`docs/methods_requirements.md` §5.2/§5.4) and for `fig_rep001–008`
  (`scripts/figure_representativeness_summary.R` panel A) — both still depend on
  it directly, confirmed by a repository-wide search for the filename (also
  referenced by several `review/diagnostics/*` reports and
  `scripts/generate_kg_availability_heatmaps.R`). **The screened file is still
  live and depended upon; it was not superseded by the Figure 4 work.**
- `data/snapshots/site_koppen_era5_fig4.csv` — written by
  `scripts/figure4_representativeness.R` Phase 1, same
  `compute_site_koppen_era5()` call but with `map_max_mm = Inf` (the
  `KG_ERA5_MAP_MAX_MM` screen NOT applied as an exclusion). This ERA5-local
  classification now serves ONLY as panel A's **fallback** source (see "PI-
  reported class used first" below) and, for fallback sites, is still subject to
  the three precipitation-dependent rules in `methods_precip_exclusions.md`
  (GRP_ERA_DOWN, `P_ERA_MAX_RATIO`, and `P_ERA_MIN_RATIO`, added 2026-10-02),
  additive to each other and unrelated to the `KG_ERA5_MAP_MAX_MM` threshold.
  `figure4_representativeness.R` also reads the screened `site_koppen_era5.csv`
  once, read-only, purely as a QA comparison (how many of its 26 previously-
  unclassified sites fall inside vs outside the new 172-site GRP_ERA_DOWN group)
  — not as a classification input for Figure 4.

**PI-reported class used first (revised 2026-10-02).** Figure 4 panel A's Geo vs
Data side (main figure only; the Geo vs Geo supplemental panel still uses the
Beck 2023 raster and is unaffected) no longer classifies every site from this
ERA5-local source. `review/diagnostics/koppen_pi_vs_era5/` (run before this
revision; `scripts/diagnostics/koppen_pi_vs_era5.R`) found that the ERA5-derived
class disagrees with the PI-reported class (BADM `CLIMATE_KOEPPEN`) more often
than it agrees (59.5% full-class agreement, n=603 comparable sites), while the PI
class agrees noticeably better with the independent Beck 2023 raster (69.2%) —
i.e. the ERA5-local classification is the less reliable of the two Geo-vs-Data
sources available for this panel. Panel A now uses the PI-reported class (case-
normalised against the 30 canonical Koppen codes, same lookup as
`scripts/diagnostics/koppen_pi_vs_era5.R`) for every site that has one (603/781
as of the 2026-09-01 snapshot pin — TERN's 0% `CLIMATE_KOEPPEN` BADM coverage
accounts for most of the shortfall vs. 781), and falls back to the ERA5-local
class above only for the remaining sites. Because the three precipitation-
dependent exclusion rules screen the ERA5 climatology itself, they apply ONLY to
fallback sites — a site with a PI-reported class is NEVER excluded from this
panel, even if its own ERA5 climatology would fail one of the rules.
`site_koppen_era5_fig4.csv`'s `pi_raw`/`pi_canonical`/`pi_twoletter`/
`panel_a_source`/`panel_a_class_used`/`panel_a_eligible` columns record this.
See `SESSION_LOG.md` for the before/after n and J.

**Scope.** This method applies only to the current 781-site Shuttle network.
Historical-network comparisons (FLUXNET2015, La Thuile, MARCONI) and the global
land-area backdrop distribution remain on the Beck 2023 raster
(`methods_koppen_beck2023.md`), since those aren't Shuttle sites with bundled
ERA5 monthly data.
