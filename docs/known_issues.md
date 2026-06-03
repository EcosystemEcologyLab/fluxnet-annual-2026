# Known Issues

This file tracks bugs and data quality issues encountered during pipeline development.
Issues are reported to the relevant maintainers. Last updated: 2026-04-30.

**ONEFlux contact:** Gilberto Pastorello (LBL) has been flagged by Dario Papale as the
contact for ONEFlux processing documentation — relevant for methods section writing.

---

## Section 1 — fluxnet R package issues

Repository: [EcosystemEcologyLab/fluxnet](https://github.com/EcosystemEcologyLab/fluxnet)
Maintainer: Eric Scott ([@Aariq](https://github.com/Aariq))

| Issue | Description | Impact | Workaround | Action |
|---|---|---|---|---|
| `flux_badm()` calls `quit()` | Function terminates the R session rather than throwing a catchable error when called from within a script | `03_read.R` exits after ~24 seconds without processing BADM data | **Implemented:** `03_read.R` reads BIF CSV files directly via `readr::read_csv()` + `dplyr::bind_rows()`, bypassing `flux_badm()` entirely — same data, no API call | **RESOLVED** — workaround in place; GitHub issue filed on EcosystemEcologyLab/fluxnet |

---

## Section 2 — Extreme NEE values

Eleven annual records in `flux_data_converted_yy.rds` (716-site dataset, April 2026)
have `abs(NEE_VUT_REF) > 2000 gC m⁻² yr⁻¹`. Values are **not excluded** by the pipeline;
they are passed through unchanged from the FLUXNET Shuttle source files. Two sites account
for all 11 records:

| site_id | IGBP | Years | NEE range (gC m⁻² yr⁻¹) | data_hub | Priority |
|---|---|---|---|---|---|
| IT-Lav | ENF | 2009–2020 (9 years) | −2,018 to −2,419 | ICOS-ETC | **High** |
| US-Bi2 | CRO | 2023–2024 (2 years) | +2,267 to +2,317 | AmeriFlux | Lower |

**IT-Lav** — annual `NEE_VUT_REF` values −1,617 to −2,419 gC m⁻² yr⁻¹ across 2003–2020
are 5–10× the typical literature range for temperate spruce forest. Values verified to
come directly from the FLUXNET Shuttle (snapshot 2026-04-14), not from pipeline processing.
Flagged with Dario Papale (ICOS-ETC) for clarification on 2026-04-30. Pending response —
interpret figure outputs at this site with caution.

**US-Bi2** — annual `NEE_VUT_REF` values +992 to +2,317 gC m⁻² yr⁻¹ across 2018–2024.
Directionally plausible (harvested bioenergy crop is a net carbon source) but at the high
end of literature. Source values verified. Lower priority than IT-Lav but worth noting.
Flag for co-author review before final analysis — check whether CRO sites should use a
separate threshold or flag-rather-than-exclude approach.

**Percentile impact (YY, all 716 sites):** Excluding both sites shifts p5 from −798 to
−777 gC m⁻² yr⁻¹ and p95 from 225 to 222 gC m⁻² yr⁻¹ — modest effect on the bulk
distribution. Min/max are materially affected (−2,420 → −1,903; +2,317 → +1,211).

---

## Section 3 — Sites missing NEE_VUT_REF

Of 672 sites in the current dataset, 142 have no valid `NEE_VUT_REF` values in the
annual (YY) processed data. These sites pass QC (the `NEE_VUT_REF_QC` gate is only
triggered when the column is present) but carry `NA` throughout for the primary
analysis variable.

**Breakdown (as of 2026-04-14):**

| Category | Count |
|---|---|
| Sites with 0 valid `NEE_VUT_REF` years | 142 |
| — of which have valid `NEE_CUT_REF` (Constant U* Threshold alternative) | 36 |
| — of which have neither `NEE_VUT_REF` nor `NEE_CUT_REF` | 106 |

Full site lists:
- `outputs/sites_no_nee_vut.csv` — all 142 zero-NEE-VUT sites (gitignored, regenerated each run)
- `outputs/sites_nee_cut_only.csv` — the 36 sites with valid `NEE_CUT_REF` but not `NEE_VUT_REF`

**Root cause for the 106 with neither (confirmed by Dario Papale, 2026-04-16):** ONEFlux
does not calculate annual values when gaps exceed 15 consecutive days for all years in the
record. This is expected behaviour — annual NEE estimates are not meaningful when data
continuity is insufficient. These sites are correctly excluded from annual flux analysis.

Verified by manually reading FLUXMET YY CSVs for a sample of 8 sites (RU-NeF, US-TLR,
US-CS6, CN-SnB, US-Lin, US-Sag, CA-PB1, US-YK1) — every case confirmed all-`-9999`.

**Root cause for the 36 NEE_CUT only (confirmed by Dario Papale, 2026-04-16):** VUT cannot
always be calculated at sites where determining a u* threshold is statistically difficult.
`NEE_CUT_REF` is the appropriate fallback for these sites.

**Action:** Report to support@fluxnet.org for routing to data contributors. See
`docs/shuttle_team_report_20260414.md` for the full site list and draft report text.
See `docs/decisions_pending.md` for the open decision on whether to fall back to
`NEE_CUT_REF` for the 36 sites where that alternative is available.

---

## Section 4 — US-PF* sites: sub-annual campaign towers (CHEESEHEAD 2019)

The 16 US-PF* sites (US-PFb through US-PFt) are temporary research towers deployed as
part of the CHEESEHEAD 2019 campaign (Chequamegon Heterogeneous Ecosystem Energy-balance
Study, Wisconsin). All measurement records span approximately June–October 2019 only
(~4 months per site). They are not year-round permanent towers.

**BIF investigation (2026-04-16):** No `TOWER_SUNSET` or "seasonal operation" BADM
variable exists in the AmeriFlux BIF schema. All US-PF* sites record
`FLUX_MEASUREMENTS_OPERATIONS = "Continuous operation"`, which refers to their ~4-month
deployment window, not to year-round continuous operation. The sub-annual deployment
is documented in `FLUX_MEASUREMENTS_DATE_START` / `FLUX_MEASUREMENTS_DATE_END` (e.g.,
`20190624` – `20191018` for most sites).

**Connection to missing NEE_VUT_REF:** The sub-annual deployment explains why these sites
appear in the 106 all-missing group. A ~4-month record leaves >15 consecutive days of
gaps in the calendar year; ONEFlux correctly withholds annual NEE estimates in this case
(see Section 3). This is expected, not a data error.

**Action:** No pipeline change needed. Methods section should note that campaign/temporary
towers with sub-annual deployments are excluded from annual flux analysis.

---

## Section 5 — FLUXNET Shuttle issues

Repository: [github.com/fluxnet/shuttle](https://github.com/fluxnet/shuttle)
Contacts: Danielle Christianson, Dario Papale

| Issue | Description | Impact | Workaround | Action |
|---|---|---|---|---|
| `httr2_failure` on batch download | HTTP request failures during download of batches 12 and 13 — `resp` is not an HTTP response object | Some sites may not download on first attempt | Re-run `batch_download.R` — resumable design handles retries | Report to support@fluxnet.org |
| AU-Dry BIF column order | `TERN_AU-Dry_FLUXNET_BIF_2009-2025_v1.3_r1.csv` has columns in unexpected order causing read failure | Site excluded from read stage | Fixed in `03_read.R` with column reordering | **RESOLVED** — fixed in `03_read.R`; reported to TERN data contributor |
| Windows file paths in `VARIABLE_GROUP` | 3 BIF files contain Windows-style file paths appearing as `VARIABLE_GROUP` values | Inflates group count and adds processing overhead | Filtered in `03_read.R` with regex guard | Report to support@fluxnet.org with affected site IDs |
| TERN hub silently dropped on 2026-05-25 | `flux_listall()` dropped all 52 TERN (AU/NZ OzFlux) sites from the manifest (716→668) when TERN's upstream returned HTTP 404; no R-level warning was raised | Paper site set changes silently — violates spirit of Hard Rule #5 | Hub-presence assertion added to `scripts/01_download.R` (2026-06-02): `stop()` if any of AmeriFlux/ICOS/TERN absent from live manifest | **RESOLVED** — assertion added; report HTTP 404 to TERN and shuttle maintainers |

**Note on hub-drop behaviour:** `flux_listall()` delegates to the fluxnet-shuttle CLI, which fetches each hub independently. When a hub's upstream returns an error, the shuttle logs `get_all_sites: N results, 1 errors` and silently drops that hub — the R function does not propagate the error count. The TERN 404 is at `https://dap.tern.org.au/thredds/fileServer/ecosystem_process/fluxnet/BIF_all_sites.csv`; it is present across shuttle versions 0.3.7, 0.3.8, and HEAD — upgrading does not fix it. The hub-presence assertion in `01_download.R` now makes this failure loud rather than silent.

**`flux_download()` version-pinning gap — empirically confirmed 2026-06-02:** `check_pipeline_config()` reports the version installed in the pinned `fluxnet_annual_2026` venv (0.3.7, confirmed from venv `METADATA`). However, `flux_download()` emits `Installed N packages in Nms` at each batch — a `uv` ephemeral-environment message — indicating it bootstraps a separate `uv` environment at call time rather than using the pinned venv. The actual shuttle commit used at download time is not captured in the batch logs or in any run metadata. This means the version check passes (0.3.7 venv) but the download may have run on a different shuttle commit. Confirmed across 14 batches of the 2026-06-01 full download run (`logs/dl_local_full_20260601.log`). Tracked as deferred in `docs/decisions_pending.md` — action required before final dataset lock.

---

## Section 6 — MM data structure: dual ERA5/FLUXMET rows; only 11.4% of rows carry valid NEE

**Discovered:** 2026-04-19 during `compute_site_year_presence()` implementation.
**Partially resolved:** 2026-05-12 — presence file regenerated with all-variable definition (see below).

### What was found

The monthly (MM) processed data (`flux_data_converted_mm.rds`) contains **two `dataset`
values per site per month**: `ERA5` and `FLUXMET`. Each calendar month therefore appears
**twice** for most sites — once for ERA5 climate variables (TA, SW\_IN, VPD, P, etc.)
and once for FLUXMET flux variables (NEE, GPP, LE, H, etc.).

Consequence: only 11.4% of MM rows have a non-NA `NEE_VUT_REF`:

```
Total MM rows:         422,631
NEE_VUT_REF non-NA:     48,066  (11.4%)
NEE_VUT_REF NA:        374,565  (88.6%)
```

**Example — US-Ha1 (948 rows, 540 unique dates):**

| dataset | rows | date range | NEE non-NA |
|---|---|---|---|
| ERA5    | 540  | 1981-01 – 2025-12 | 0   |
| FLUXMET | 408  | 1991-01 – 2025-12 | 396 |

ERA5 rows span the full ERA5 record (back to 1981); FLUXMET rows begin only when the
tower was first commissioned. ERA5 rows carry `NA` for all flux variables — they exist
solely to provide climate forcing.

### Resolution — 2026-05-12

The presence file was regenerated with a 12-column all-variable union (NEE VUT/CUT, GPP/RECO
NT/DT × VUT/CUT, LE\_F\_MDS, H\_F\_MDS), replacing the NEE\_VUT\_REF-only indicator. This
resolved the three open questions:

1. **The 44 zero-NEE MM sites explained.** These sites have valid LE\_F\_MDS and/or GPP/RECO
   data in FLUXMET rows. The NEE absence reflects CUT-only processing (VUT statistically
   infeasible) or downstream processing failure, not missing flux tower data. This is the same
   root cause documented for the 36 NEE\_CUT-only sites in Section 3. US-WCr (24 years data),
   US-NR1 (28 years), and US-Ho2 (27 years) were among the most significant recovered sites.

2. **ERA5 rows do not affect the multi-variable indicator.** ERA5 rows carry `NA` for all 12
   flux variables, confirming that counting any non-NA across the union is ERA5-safe without
   requiring an explicit `dataset == "FLUXMET"` filter. The filter is still recommended as a
   defensive guard for future schema changes — see remaining action item below.

3. **Site coverage: 716/716.** The old NEE-only file covered 672 sites (with 88 of 716
   snapshot sites on a span-based fallback). The new file covers all 716 sites; no fallback
   is used in the authorship script.

**US-WCr investigation resolved.** US-WCr had a 27-year snapshot span but zero annual
`NEE_VUT_REF` records, previously listed as an uninvestigated anomaly. Confirmed: 24 years
of FLUXMET presence under the all-variable definition. NEE absence is downstream processing;
the site is correctly allocated 8 invited authors (≥21 yr, ≤2 yr latency).

### Remaining action

- [ ] Add `dataset == "FLUXMET"` filter to `compute_site_year_presence()` as a defensive guard
      against future pipeline stages that might populate ERA5 rows with non-NA flux values.
      Lower priority now that all-variable union is confirmed ERA5-safe for the current schema.

---

## Section 6 — DD (daily) read OOM on local Mac mini (open)

`03_read.R` crashed during DD resolution processing at site 150 of 759 with
`Error: vector memory limit of 16.0 Gb reached`. YY and MM completed successfully.

**What is on disk:**
- `data/processed/flux_data_raw_yy.rds` — complete (759 sites, 10 MB)
- `data/processed/flux_data_raw_mm.rds` — complete (759 sites, 123 MB)
- `data/processed/flux_data_raw_dd_partial.rds` — 150 sites, 411 MB (resumable)
- `data/processed/flux_data_raw_dd_done_sites.rds` — list of 150 completed site IDs

**Root cause:** DD data at 759 sites × ~390 columns × daily resolution accumulates
to a frame that exceeds the 16 GB R vector memory ceiling on the local Mac mini.
The resumable partial design in `03_read.R` wrote a checkpoint at site 150.

**Figures affected:** `fig_seasonal_cycle`, `fig_seasonal_weekly`,
`fig_seasonal_triplet`, `fig_growing_season_nee`. All YY-based figures (maps,
IGBP boxplots, Whittaker, latitudinal, climate scatter, anomaly) are unaffected.

**Next steps (deferred):** Run DD read on the Codespace (more RAM), or split
into batches of ~100 sites per chunk and merge. The partial file at site 150
can serve as the starting point — `03_read.R` will resume from where it left off.
`07_figures.R` has been updated (2026-06-02) to degrade gracefully when
`flux_data_converted_dd.rds` is absent, so the rest of the pipeline can proceed.

---

## Section 7 — site_candidates_full.csv stale after 759-site re-extraction (open)

`data/snapshots/site_candidates_full.csv` was rebuilt by `step2_extract_aridity.R`
on 2026-06-02 but contains only 569 rows, not 759. It left-joins from
`data/snapshots/long_record_site_candidates_gez_kg.csv`, which was built against the
716-site April 2026 dataset and has not been updated. The 43 new sites added in the
Jun 1 snapshot are absent from `site_candidates_full.csv`.

**Impact:** Any figure or analysis that filters on `currently_selected` or uses
candidate status (anomaly figures, long-record site selection) will not include the
43 new sites.

**Action:** After `03_read.R` runs on the 759-site dataset and produces updated NEE
presence data, rebuild `long_record_site_candidates_gez_kg.csv` and then re-run
`step2_extract_aridity.R` to regenerate `site_candidates_full.csv`. This step falls
between the pipeline rerun (scripts 03–07) and the final figure generation pass.

---

## Section 8 — CUT QC not filtered at pipeline level (open)

`04_qc.R` gates row exclusion exclusively on `NEE_VUT_REF_QC >= QC_THRESHOLD_YY` (by
design). For the ~36 CUT-only sites (`NEE_CUT_REF` present, `NEE_VUT_REF` absent),
`NEE_VUT_REF_QC` is either absent or all-NA, so those rows pass the QC stage without
any quality filtering on the CUT variable. As of 2026-06-02, figure functions that
use `coalesce(NEE_VUT_REF, NEE_CUT_REF)` (fig_map_nee_mean, fig_map_nee_delta,
fig_whittaker_worldclim, 00_candidate_figures Section 1) will therefore plot CUT
values that are unfiltered on `NEE_CUT_REF_QC`.

**Impact:** CUT-only sites are a small fraction of the network (~36 of 759). Their
annual NEE data comes from sites where VUT processing was statistically infeasible
(insufficient u* threshold data); the CUT values are scientifically valid but may
include years with high gap-fill fractions.

**Action:** Before final analysis, add `NEE_CUT_REF_QC >= QC_THRESHOLD_YY` gating
to `04_qc.R` for rows where `NEE_VUT_REF_QC` is NA but `NEE_CUT_REF_QC` is present.
This should be symmetric with the VUT threshold. Alternatively, document the asymmetry
in the methods section and apply a post-hoc filter in the analysis scripts.

---

## Section 8 — Pipeline analysis bugs (resolved)

| Issue | Description | Impact | Fix | Resolved |
|---|---|---|---|---|
| QC gating applied to all `_QC` variables | Row exclusion in `04_qc.R` evaluated `_QC` thresholds across all columns (NEE, GPP, RECO, LE, H), causing 347/672 sites (52%) to lose all annual records when any secondary variable had high gap-fill | Over-exclusion of valid sites | Fixed: row exclusion now gated on `NEE_VUT_REF_QC` only; secondary variable QC columns retained for per-variable downstream filtering | **RESOLVED 2026-04-20** — implemented in `R/qc.R` and `scripts/04_qc.R` |
| Unit conversion applied to pre-integrated annual/monthly data | `fluxnet_convert_units()` applied the µmol CO₂ m⁻² s⁻¹ → gC m⁻² per-period factor to YY and MM carbon flux variables that are already expressed in gC m⁻² per period, producing an ~800× overcorrection | All annual and monthly NEE/GPP/RECO values inflated by ~800× in outputs and figures | Fixed in commit 31e653b: DD/WW/MM/YY carbon flux variables passed through unchanged; conversion applied only at HH and HR resolutions | **RESOLVED 2026-04-20** — all pre-fix figure outputs must be regenerated |
| `build_review_flags()` crash on zero-flag subsets | `sapply()` on an empty `flags_named` list returns `list()` rather than `logical(0)`, causing an `invalid subscript type 'list'` error when the predicate `grepl("No author block", ...)` is applied. Only triggered when `generate_fluxnet_citations()` is called with a small subset of sites that happens to have no review flags at all (the full 718-site set always has some). | `generate_fluxnet_citations()` crashes at the `writeLines(build_review_flags(...))` call for small site subsets | Replaced `sapply(flags_named, function(f) grepl(...), ...)` with `vapply(..., logical(1L))` in both predicate calls inside `build_review_flags()` — `vapply` always returns a typed vector, even over an empty list | **RESOLVED 2026-05-28** — fixed in `scripts/generate_fluxnet_citations.R` |

**Context — coarse-resolution carbon data are pre-integrated:** At DD, WW, MM, and YY resolutions, FLUXNET Shuttle carbon flux variables (NEE, GPP, RECO) are expressed as period-integrated totals (gC m⁻² period⁻¹), not as instantaneous rates (µmol CO₂ m⁻² s⁻¹). The pre-fix code applied the HH/HR rate-to-integral conversion factor (~1800–3600 s × molar mass of C) to the already-integrated YY and MM values, producing ~800× overcorrected outputs. Fixed in commit 31e653b (2026-04-14): DD/WW/MM/YY carbon data now passes through unchanged; conversion is applied only at HH and HR resolutions. All pipeline outputs and figures generated before that commit must be regenerated. Verified in `R/units.R`: `.infer_source_unit()` returns `"gC m-2 d-1"` for DD carbon variables, so no µmol conversion is triggered at any coarse resolution.

---

## Section 8 — Technical debt: `read_bifvarinfo_units()` long-format parser

`read_bifvarinfo_units()` in `R/units.R` does not parse the long-format BIFVARINFO
files used by 731 site-files in the dataset. These files store unit information as row
values in a `VARIABLE`/`DATAVALUE` column structure (e.g., `VARIABLE_GROUP=GRP_VAR_INFO`,
`VARIABLE=VAR_INFO_UNIT`, `DATAVALUE=gC m-2 y-1`) rather than as dedicated column
headers (`VAR_INFO_VARNAME`, `VAR_INFO_UNIT`) expected by the current parser.

The hardcoded fallback is correct for all current variable classes — annual carbon is
assumed pre-integrated (pass-through), energy variables assumed W m⁻², temperatures °C
(converted to K) — so **pipeline outputs are unaffected**. However, the parser should
be fixed before any new variable class is added with non-default native units, as the
fallback would silently apply incorrect assumptions for an unrecognised class.

Track as technical debt. Fix: extend `read_bifvarinfo_units()` to detect the long-format
schema by checking for `VARIABLE` and `DATAVALUE` column names and pivot accordingly
before extracting unit assignments.

---

## Section 9 — Precipitation anomalies: ERA5 P_ERA and tower-observed P_F (open)

**Flagged:** 2026-06-03. ERA5 observation added 2026-06-03 (commit 5e868b4).

Two related but distinct precipitation anomaly issues have been identified. They have
different sources, different likely causes, and require separate investigation passes.

---

### 9a — ERA5 reanalysis precipitation (P_ERA): quantified

**Observation:** 225 of 6,108 FLUXMET site-years (3.7%) in `annual_converted` have
`P_ERA > 5000 mm yr⁻¹`. Surfaced during the `generate_env_response_era5.R` DuckDB
port (commit 5e868b4, 2026-06-03).

**Source:** Reanalysis — ERA5 annual precipitation interpolated to site locations by
the FLUXNET Shuttle. Not derived from tower instruments.

**Likely cause:** Spatial-averaging artifacts at the ERA5 grid scale (~31 km), amplified
at topographically complex sites (e.g., coastal, high-relief terrain) where the ERA5
grid cell may include ocean or orographic precipitation enhancement.

**Current pipeline handling:** `fig_environmental_response_era5()` applies its own
outlier filter before plotting (removes `P_ERA > 5000`, `VPD_ERA > 5 kPa`,
`TA_ERA_C` outside [−30, 40] °C). The 225 outlier site-years are silently dropped
from fig_08 panels. No flag is written to the exclusion log.

**Action required:**
1. Log the 225 removed site-years to the exclusion log (currently silent drop in the
   figure function — should call `log_exclusion()` or equivalent).
2. Investigate the site-year distribution: are the > 5000 mm cases concentrated at
   specific sites, specific years, or specific geographic regions?
3. Consider whether a `P_ERA_QC` flag or outlier annotation should be added to
   `annual_converted` so downstream scripts don't each re-implement the filter.

---

### 9b — Tower-observed precipitation (P_F): unquantified

**Observation:** Some FLUXNET sites carry staggeringly wrong tower-measured `P_F`
values. Specific sites and magnitudes are not yet characterised — this was flagged
during visual review and has not been subjected to a systematic identification pass.

**Source:** Tower-observed precipitation (gap-filled where missing). Distinct from ERA5
reanalysis — though ERA5 is commonly used to gap-fill `P_F`, so the two phenomena
can co-occur at sites with heavy gap-fill fractions.

**Likely cause:** Instrument failure, unit-of-measure errors (e.g., mm s⁻¹ mistakenly
reported as mm per timestep), or gap-fill artifacts (ERA5 substitution producing values
inconsistent with the site's local climate).

**Current pipeline handling:** No range check, outlier detection, or QC flag exists for
`P_F` in the current pipeline. Anomalies pass through unchanged to `annual_converted`.

**Action required before any analysis or figure using tower P:**
1. Systematic identification pass — flag site-years where annual `P_F` falls outside a
   plausible range (e.g., < 0 or > 5000 mm yr⁻¹, or > 3σ from WorldClim MAP for that
   site).
2. Characterise the anomaly pattern (unit error, instrument failure, gap-fill artifact).
3. Decide: flag-and-exclude the anomalous site-years, or correct where correction is
   defensible (e.g., unit rescaling with documented justification).
4. Document the decision in `docs/decisions_pending.md` before finalising any figure
   that uses tower precipitation.

---

**Current candidate figure exposure:** fig_05 and fig_06 (Whittaker) use WorldClim MAP
(bio12) — unaffected by either anomaly. fig_08 (environmental response) uses ERA5 `P_ERA`
with its own outlier filter — affected by 9a, handled in-function. No current candidate
figure uses tower `P_F` as a primary axis. If future figures add a tower-P axis, 9b
becomes active.

Cross-reference: see `docs/methods_requirements.md` §5.3 — methods text must address
how both precipitation anomaly types are handled in any candidate figure that uses
precipitation as a primary axis or predictor.

---

## Future enhancements

### FAO GEZ shapefile

FAO GEZ shapefile stored at `data/external/gez/` — download from
https://data.apps.fao.org/catalog/dataset/2fb209d0-fd34-4e5e-a3d8-a13c241eb61b
if not present (file: `gez2010.zip`, extract to `data/external/gez/gez_2010_wgs84.shp`).

The site-level GEZ lookup is pre-computed in `data/snapshots/site_gez_lookup.csv`
(columns: `site_id`, `gez_name`, `gez_code`, `gez_method`) and committed to the repository.
Regenerate by running `scripts/step3_extract_gez.R`. Direct download URL for the shapefile:
`https://storage.googleapis.com/fao-maps-catalog-data/uuid/2fb209d0-fd34-4e5e-a3d8-a13c241eb61b/resources/gez2010.zip`
(61.6 MB; extract to `data/external/gez/gez_2010_wgs84.shp`).
Three sites require nearest-feature fallback (outside all polygons): AR-TF1, CA-RBM, CN-SnB.
