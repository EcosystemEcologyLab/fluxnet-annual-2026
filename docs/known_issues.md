# Known Issues

This file tracks bugs and data quality issues encountered during pipeline development.
Issues are reported to the relevant maintainers. Last updated: 2026-04-16.

**ONEFlux contact:** Gilberto Pastorello (LBL) has been flagged by Dario Papale as the
contact for ONEFlux processing documentation — relevant for methods section writing.

---

## Section 1 — fluxnet R package issues

Repository: [EcosystemEcologyLab/fluxnet](https://github.com/EcosystemEcologyLab/fluxnet)
Maintainer: Eric Scott ([@Aariq](https://github.com/Aariq))

| Issue | Description | Impact | Workaround | Action |
|---|---|---|---|---|
| `flux_badm()` calls `quit()` | Function terminates the R session rather than throwing a catchable error when called from within a script | `03_read.R` exits after ~24 seconds without processing BADM data | **Implemented:** `03_read.R` reads BIF CSV files directly via `readr::read_csv()` + `dplyr::bind_rows()`, bypassing `flux_badm()` entirely — same data, no API call | Open GitHub issue on EcosystemEcologyLab/fluxnet |

---

## Section 2 — Extreme NEE values

Two annual records in `flux_data_converted_yy.rds` have `abs(NEE_VUT_REF) > 2000 gC m⁻² yr⁻¹`.
Values in this range are physically plausible for high-productivity ecosystems and are **not excluded** by the pipeline.
Full record list saved to `outputs/nee_extreme_values.csv` (regenerated on each pipeline run).

| site_id | IGBP | Year | NEE_VUT_REF (gC m⁻² yr⁻¹) | data_hub |
|---|---|---|---|---|
| US-Bi2 | CRO | 2023 | 2267.34 | AmeriFlux |
| US-Bi2 | CRO | 2024 | 2316.51 | AmeriFlux |

US-Bi2 is a California rice paddy (CRO). Extreme positive NEE indicates strong net carbon source; may reflect harvest/residue practices. Flag for co-author review before final analysis — check whether CRO sites should use a separate threshold or flag-rather-than-exclude approach.

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
| AU-Dry BIF column order | `TERN_AU-Dry_FLUXNET_BIF_2009-2025_v1.3_r1.csv` has columns in unexpected order causing read failure | Site excluded from read stage | Fixed in `03_read.R` with column reordering | Report to TERN data contributor |
| Windows file paths in `VARIABLE_GROUP` | 3 BIF files contain Windows-style file paths appearing as `VARIABLE_GROUP` values | Inflates group count and adds processing overhead | Filtered in `03_read.R` with regex guard | Report to support@fluxnet.org with affected site IDs |

---

## Future enhancements

### FAO GEZ shapefile

FAO GEZ shapefile stored at `data/external/gez/` — download from
https://data.apps.fao.org/catalog/dataset/2fb209d0-fd34-4e5e-a3d8-a13c241eb61b
if not present (file: `gez2010.zip`, extract to `data/external/gez/gez_2010_wgs84.shp`).

The site-level GEZ lookup is pre-computed in `data/snapshots/site_gez_lookup.csv`
(columns: `site_id`, `gez_name`, `gez_code`) and committed to the repository.
Regenerate by re-running the GEZ join block in `scripts/07_figures.R` or via
the inline code used in `R/figures/fig_anomaly_context.R`.
