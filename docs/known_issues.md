# Known Issues

This file tracks bugs and data quality issues encountered during pipeline development.
Issues are reported to the relevant maintainers. Last updated: 2026-04-20.

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
| AU-Dry BIF column order | `TERN_AU-Dry_FLUXNET_BIF_2009-2025_v1.3_r1.csv` has columns in unexpected order causing read failure | Site excluded from read stage | Fixed in `03_read.R` with column reordering | **RESOLVED** — fixed in `03_read.R`; reported to TERN data contributor |
| Windows file paths in `VARIABLE_GROUP` | 3 BIF files contain Windows-style file paths appearing as `VARIABLE_GROUP` values | Inflates group count and adds processing overhead | Filtered in `03_read.R` with regex guard | Report to support@fluxnet.org with affected site IDs |

---

## Section 6 — MM data structure: dual ERA5/FLUXMET rows; only 11.4% of rows carry valid NEE

**Discovered:** 2026-04-19 during `compute_site_year_presence()` implementation.

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

### Why this matters for `compute_site_year_presence()`

`compute_site_year_presence()` (added 2026-04-19 in `R/utils.R`) groups MM data by
`site_id × year` and counts rows with non-NA `NEE_VUT_REF`. Because ERA5 rows are all-NA
for NEE, they do not inflate the valid-month count. Spot-check of US-Ha1 confirmed the
function returns correct per-year counts (396 valid months, active in all years 1991–2025).

However, the following questions have **not been fully verified** before the presence CSV
can be treated as authoritative input to `is_functionally_active()`:

1. **Are the 44 MM zero-NEE sites the same as the 142 YY zero-NEE sites (Section 3)?**
   It is not confirmed whether MM and YY gap patterns are consistent across sites.

2. **Do all 672 sites have FLUXMET rows, or only ERA5 rows for some?**
   44 sites have zero non-NA NEE across all MM rows. It is not confirmed whether these
   are sites with no FLUXMET download vs sites where `NEE_VUT_REF` is genuinely absent
   (sub-annual campaign towers, 15-day gap rule, etc.).

3. **Is the implicit ERA5-NA assumption safe to rely on?**
   The current implementation does not filter to `dataset == "FLUXMET"`. It is correct
   now because ERA5 rows are all-NA for NEE, but fragile: if any future pipeline stage
   adds non-NA values to ERA5 rows, those months would be incorrectly counted as valid.

### Current state

`data/snapshots/site_year_data_presence.csv` was committed on 2026-04-19 (commit c6d186b).
It yields **373 functionally active sites for 2025** vs 431 from the old `last_year >= 2021`
definition. The 58-site difference has not been audited to confirm it reflects genuine data
currency gaps rather than ERA5/FLUXMET row-structure artefacts.

### Action needed

- [ ] Cross-check the 44 zero-NEE MM sites against the 142 zero-NEE YY sites (Section 3)
      to confirm MM and YY gaps are consistent.
- [ ] Verify whether each of the 44 sites has FLUXMET rows or only ERA5 rows in MM data.
- [ ] Add `dataset == "FLUXMET"` filter to `compute_site_year_presence()` to make intent
      explicit and guard against future ERA5 column changes.
- [ ] Recompute and recommit `site_year_data_presence.csv` after the filter is added.
- [ ] Audit a sample of the 58 sites that lost active status under the new definition to
      confirm these are genuine data-currency differences, not filtering artefacts.

---

## Section 7 — Pipeline analysis bugs (resolved)

| Issue | Description | Impact | Fix | Resolved |
|---|---|---|---|---|
| QC gating applied to all `_QC` variables | Row exclusion in `04_qc.R` evaluated `_QC` thresholds across all columns (NEE, GPP, RECO, LE, H), causing 347/672 sites (52%) to lose all annual records when any secondary variable had high gap-fill | Over-exclusion of valid sites | Fixed: row exclusion now gated on `NEE_VUT_REF_QC` only; secondary variable QC columns retained for per-variable downstream filtering | **RESOLVED 2026-04-20** — implemented in `R/qc.R` and `scripts/04_qc.R` |
| Unit conversion applied to pre-integrated annual/monthly data | `fluxnet_convert_units()` applied the µmol CO₂ m⁻² s⁻¹ → gC m⁻² per-period factor to YY and MM carbon flux variables that are already expressed in gC m⁻² per period, producing an ~800× overcorrection | All annual and monthly NEE/GPP/RECO values inflated by ~800× in outputs and figures | Fixed in commit 31e653b: YY and MM carbon flux variables passed through unchanged; conversion applied only at HH/HR/DD resolutions | **RESOLVED 2026-04-20** — all pre-fix figure outputs must be regenerated |

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
