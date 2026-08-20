Per-site Köppen-Geiger (KG) classes for the current 767-site FLUXNET Shuttle
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
output `data/snapshots/site_koppen_era5.csv`.

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

**Scope.** This method applies only to the current 767-site Shuttle network.
Historical-network comparisons (FLUXNET2015, La Thuile, MARCONI) and the global
land-area backdrop distribution remain on the Beck 2023 raster
(`methods_koppen_beck2023.md`), since those aren't Shuttle sites with bundled
ERA5 monthly data.
