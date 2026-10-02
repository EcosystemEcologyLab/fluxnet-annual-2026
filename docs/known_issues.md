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

---

### 9c — ERA5 defects found via Figure 4's precipitation-dependent panels (quantified, 2026-10-02)

**Flagged:** 2026-10-02, during development of `scripts/figure4_representativeness.R` panels A
(Köppen) and C (aridity), both of which depend on each site's own 1991–2020 mean annual P_ERA.
Four distinct defects were found and are handled as site-level exclusions, additive to each
other (not alternatives) — see `review/figures/representativeness/methods_precip_exclusions.md`
and `methods_aridity_era5.md` for the full method and `docs/methods_requirements.md` §5.8 for
how they affect each panel's n.

1. **GRP_ERA_DOWN — 172 sites.** No usable P_ERA-vs-measured-precipitation regression slope
   (`ERA_SLOPE` recorded as sentinel −9999, not merely unfitted). Source:
   `review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv`
   (`not_fitted_slope_9999` group). Affects panels A and C identically, except `DE-Zrk` (one of
   the 172) is counted once under the separate invalid-input screen below for panel C, so panel
   C's distinct GRP_ERA_DOWN-only count is 171, not 172.

2. **P_ERA_MAX_RATIO — 10 further sites.** 1991–2020 mean annual P_ERA exceeds 3× (`R/
   pipeline_config.R`, `P_ERA_MAX_RATIO`) **every** reference available for the site — PI-reported
   BADM `MAP` (where present/non-zero) **and** WorldClim v2.1 BIO12 at the tower coordinate. Four
   of the ten (`CA-CF2`, `IT-Niv`, `NO-And`, `US-HB4`) are large enough to be clear ERA5
   spatial-averaging artifacts (`US-HB4`: ~460–487× its references); the other six are more
   moderate (3.1–11×). Full site list and ratios in `SESSION_LOG.md` (2026-10-02 entries).

3. **Invalid raw ERA5 meteorological inputs — 4 sites (panel C / aridity only).** `CD-Ygb`,
   `DE-Zrk`, `FR-LBr`, `US-Sne` have at least one calendar month with a physically impossible raw
   ERA5 value feeding the FAO-56 PET calculation — e.g. `LW_IN_ERA` up to ~30,000 W/m² (physical
   maximum ~1000 W/m²) or `VPD_ERA` up to ~1,660 hPa (physical maximum ~100 hPa). Caught by a
   physical-plausibility screen (LW/SW <0 or >1000 W/m²; VPD <0 or >100 hPa; WS ≤0 or >50 m/s; PA
   outside [50,110] kPa; TA outside [−90,60]°C) applied only within panel C's PET calculation —
   this is a data-quality issue in the bundled ERA5 extraction, not a modelling choice. Two further
   sites (`DE-SbM`, PET=0 mm/yr; `KE-Aq2`, PET=122 mm/yr) have implausible *outputs* from
   valid-looking raw inputs — a known limitation of the net-radiation approximation documented in
   `methods_aridity_era5.md`, flagged but not screened (both already excluded by rules 1–2 anyway).

4. **P_ERA_MIN_RATIO — a further 22 sites (added 2026-10-02).** The low-side mirror of rule 2:
   1991–2020 mean annual P_ERA falls below 1/3 (`R/pipeline_config.R`, `P_ERA_MIN_RATIO = 1/3`)
   **every** reference available for the site, same dual-reference AND logic. Several of these are
   extreme — e.g. `CA-TP2`'s P_ERA is ~0 mm/yr against BADM MAP 1036 mm/yr and BIO12 970 mm/yr — the
   same class of ERA5 spatial-averaging/extraction artifact as the high-side cases, in the opposite
   direction. Sensitivity: 33 sites at ratio < 1/2, 22 at < 1/3 (the adopted threshold), 16 at < 1/4.
   Full site list and ratios in `SESSION_LOG.md` (2026-10-02 entry for this change).

**The finding that prompted panel A's 2026-10-02 redesign.** `review/diagnostics/
koppen_pi_vs_era5/` (`scripts/diagnostics/koppen_pi_vs_era5.R`) compared the ERA5-derived class
these rules gate against the PI-reported class (BADM `CLIMATE_KOEPPEN`) and found the ERA5-derived
class disagrees with the PI-reported class *more often than it agrees* (59.5% full-class agreement,
n=603 comparable sites), while the PI class agrees much better with the independent Beck 2023 raster
(69.2%) — i.e. on top of the four P_ERA defects above, the ERA5-local classification itself is the
less reliable of the two Geo-vs-Data sources available for panel A. This is not itself a P_ERA
numeric defect (it is about classification reliability, not an implausible raw value), but it is the
direct motivation for panel A's PI-first redesign (`methods_koppen_era5.md`, "PI-reported class used
first"): the four exclusion rules above now apply only to the minority of sites that fall back to the
ERA5-local class (no PI-reported `CLIMATE_KOEPPEN` value); a PI-sourced site is never excluded by
them. Panel C (aridity) has no PI-reported analogue and still applies all four rules to every site.

**Current pipeline handling:** all four are site-level exclusions removed from both the numerator
and denominator of the affected panel (not merely left unclassified), logged via
`log_exclusion()`/traced explicitly in `figure4_representativeness.R`'s console output. These are
in addition to, and distinct from, the existing §9a `KG_ERA5_MAP_MAX_MM` outlier screen used by the
main pipeline's `site_koppen_era5.csv` — Figure 4's `site_koppen_era5_fig4.csv` does NOT apply that
screen, relying on rules 1, 2 and 4 above instead (see `methods_koppen_era5.md`, "Two output files").

**Action required:** none at this time — the exclusion rules are the accepted, documented handling
for Figure 4. If a future panel or analysis depends on site-level P_ERA or ERA5 meteorology beyond
Figure 4's scope, revisit whether these same three screens should be applied there too, or whether
the underlying Shuttle ERA5 extraction itself should be reported upstream.

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

## Section 10 — QC_THRESH=0.80 hardcoded outside the paper's QC gate (open, 2026-10-02)

**Context:** `scripts/figure4_representativeness.R` panels E (NEE) and F (ET) used a hardcoded
`QC>=0.80` annual-QC gate, copied from `scripts/assess_flux_data_by_igbp_shuttle.R`. The paper's
actual QC gate is `QC_THRESHOLD_YY` (`R/pipeline_config.R`, currently `0.50`) — the same constant
`scripts/04_qc.R` uses for the rest of the pipeline. Fixed 2026-10-02: panels E/F now take tower
NEE/GPP/RECO/ET/H from the new shared `compute_site_annual_fluxes()`
(`R/site_annual_fluxes.R`), which reads the pre-QC `annual` table directly and gates NEE/GPP/RECO
on the per-site VUT/CUT-chosen NEE QC column and ET/H on their own QC columns, all against
`QC_THRESHOLD_YY`. See `docs/methods_requirements.md` §5.8 (rows E/F) and
`review/figures/representativeness/methods_flux_bin_scheme.md`.

**Not fixed by this change — still hardcode a literal 0.80, independent of
`compute_site_annual_fluxes()`/`QC_THRESHOLD_YY`.** Left as-is; not rerun as part of this fix (none
of their outputs feed the current Figure 4). A future cleanup should point these at the shared
function instead of maintaining a second, divergent QC threshold.

| Script | QC constant | Outputs already on disk under QC>=0.80 |
|---|---|---|
| `scripts/assess_flux_data_by_igbp_shuttle.R` | `QC_THRESH <- 0.80` | `data/snapshots/site_flux_medians_shuttle.csv`, `igbp_class_flux_distributions_shuttle.csv` |
| `scripts/assess_flux_data_by_igbp_fluxnet2015.R` | `QC_THRESH <- 0.80` | `data/snapshots/site_flux_medians_fluxnet2015.csv`, `igbp_class_flux_distributions_fluxnet2015.csv` |
| `scripts/figure_flux_comparison_combo_alt_common_siteyears.R` | `QC_THRESH <- 0.80` | `data/snapshots/flux_comparison_fluxnet2015_vs_shuttle_common_siteyears.csv`, `review/figures/candidates/ALT_fig_03_flux_comparison_combo_nep_et_h.png` |
| `scripts/figure_representativeness_nee_signed.R` | `QC_THRESH_MM <- 0.80` | `data/snapshots/site_trendy_nee_signed5_geo_current_781.csv`, `site_trendy_nee_signed5_data_{current_781,marconi,la_thuile,fluxnet2015}.csv`, `nee_signed5_occupancy_jaccard.csv`, `trendy_nee_signed5_global_distribution.csv` |
| `scripts/candidate_nee_gpp_ter_panels.R` | `QC_THRESH_MM <- 0.80` | `review/figures/candidates/` (per-panel PNGs, `fig5_jaccard_trajectory_with_nee.png`) |
| `scripts/diagnostics/flux_tower_model_distributions.R` | `QC_THRESH_MM <- 0.80` | `review/diagnostics/nee_corrected_axis/` (`table_step3_tower_annual_nee.csv`, `table_dist_tower_vs_model.csv`, `fig_dist_histograms.png`, `fig_dist_latitude.png`, `fig_dist_scatter_1to1.png`) |
| `scripts/diagnostics/nee_corrected_axis.R` | `QC_THRESH_MM <- 0.80` | `review/diagnostics/nee_corrected_axis/` (`table_step1_residual_and_positivity.csv` through `table_step7_paired_check.csv`, `fig_step5_global_distribution.png`, `fig_step6_bin_fractions.png`, `fig_step7_paired_scatter.png`, plus the three `fig_dist_*.png` it shares with `flux_tower_model_distributions.R` above) |
| `scripts/diagnostics/flux_bin_breaks.R` | `QC_THRESH_MM <- 0.80` | `review/diagnostics/flux_bin_breaks/` (`table_edges.csv`, per-panel and composite PNGs) — the diagnostic Figure 4 panels E/F's *binning scheme* (not its tower values) was ported from; Figure 4 itself no longer uses this script's threshold |
| `scripts/diagnostics/nee_et_site_vs_trendy_raster.R` | literal `0.80` in SQL (no named constant) | `review/diagnostics/nee_et_site_vs_trendy/table_paired_measured_vs_trendy.csv` |

**Consume those scripts' 0.80-threshold medians rather than defining their own threshold** (so a
fix only needs to happen upstream, at the scripts above):

| Script | Outputs |
|---|---|
| `scripts/figure_flux_medians_by_igbp.R` | `review/figures/flux_medians/fig_flux_{nep,gpp,ter,et,h}_by_igbp.png`, `data/snapshots/flux_medians_by_igbp_{nep,gpp,ter,et,h}.csv` |
| `scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R` | `review/figures/flux_medians/fig_flux_comparison_{nep,gpp,ter,et,h}.png`, `data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv` |
| `scripts/figure_representativeness_supp_sitelevel.R` | `review/diagnostics/.../Supp_sampling_ratio_siteKG_IGBP_NEE_ET.png`, `Supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.png` |

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
