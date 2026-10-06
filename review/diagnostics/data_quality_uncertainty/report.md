# Data quality and uncertainty across the network (781 sites)

*Diagnostic only. Reads the pre-QC DuckDB tables (`dataset = 'FLUXMET'`) — never
`*_qc` or `*_converted`. Does not touch paper figures, snapshots, or metrics files.*

## Summary

*(Placeholder — this section is rewritten once all stages are complete, per the
task's closing instruction. See `status.md` for the current run state.)*

---

## Stage 0 — Inventory

**Scope:** annual, monthly, weekly, daily DuckDB tables, `dataset = 'FLUXMET'`, 781 sites.

**Outputs:** `table_stage0_column_inventory.csv`, `table_stage0_hh_hr_sites.csv`,
`table_stage0_bif_ustar_summary.csv`, `table_stage0_bif_ustar_site_summary.csv`.

### Column inventory

For NEE (VUT, CUT), LE and H, checked: reference value, QC flag, random uncertainty
(`_RANDUNC`), joint uncertainty (`_JOINTUNC`), the u-star percentile columns
(`_05`…`_95`), `_USTAR50`, `_MEAN`, `_SE`, and — for LE/H — the energy-balance-corrected
`_CORR` value with its own spread columns (`_CORR_25`, `_CORR_75`, `_CORR_JOINTUNC`).
Full combination-by-combination result (table × family × category) is in
`table_stage0_column_inventory.csv`; `n_sites_with_data` is `count(distinct site_id)`
with at least one non-NA value for that column, at that resolution.

**Present everywhere (all four resolutions), for both NEE_VUT and NEE_CUT:**
reference value, QC flag, random uncertainty, joint uncertainty, all seven u-star
percentile columns, USTAR50, MEAN, SE. These form the full FLUXNET u-star-threshold
perturbation ensemble (the percentile/MEAN/SE/USTAR50 family) plus the two named
single-value uncertainty terms (`_RANDUNC`, `_JOINTUNC`) on the REF value. At the
annual step, counts range ~615–618 sites for the REF/QC/RANDUNC/JOINTUNC group and
~617–732 for the ensemble-statistic columns (MEAN/SE/percentiles) — i.e. **more sites
carry the u-star ensemble statistics than carry a usable REF value** (732 vs 616 sites
for `NEE_VUT_SE` vs `NEE_VUT_REF` at the annual step). This is a real asymmetry, not a
join artefact: `NEE_VUT_REF` is the single percentile selected by ONEFlux's combined
CP/MP method, which can fail (see Stage 4) even when the broader percentile ensemble
still has values.

**Present everywhere, for LE and H:** reference value (`LE_F_MDS`/`H_F_MDS`), QC flag,
random uncertainty (`LE_RANDUNC`/`H_RANDUNC`), and the energy-balance-corrected value
itself (`LE_CORR`/`H_CORR`, ~436–437 sites at the annual step — substantially fewer
than the ~665–669 sites holding the uncorrected `LE_F_MDS`/`H_F_MDS`, since the
correction needs a successful energy-balance closure fit).

**Absent everywhere (genuinely absent from the FLUXNET product, not dropped at
ingest — see below), for LE and H at every resolution:** `_JOINTUNC` (uncorrected),
all seven percentile columns, `_USTAR50`, `_MEAN`, `_SE`. LE/H carry no u-star-threshold
perturbation ensemble and no named joint-uncertainty term on the uncorrected value —
only `_RANDUNC`. This matches the physical picture: the u-star ensemble exists because
u-star filtering is a NEE/GPP/RECO-specific correction; LE and H are not u-star-filtered
in the same way.

**Resolution-dependent — energy-balance-corrected spread columns:** `LE_CORR_25`,
`LE_CORR_75`, `LE_CORR_JOINTUNC` (and the H equivalents) exist **only in the daily
table**; absent from annual, monthly and weekly. So a per-period uncertainty estimate
on the corrected LE/H value is only available at daily resolution — Stage 2's "whatever
uncertainty columns exist" for LE/H is consequently a daily-only exercise for the
corrected variables, annual/monthly/weekly-only for the uncorrected `_RANDUNC`.

**Ingest check:** two sample extracted annual CSVs (`AR-Bal`, CUT-only, 198 columns;
`US-MMS`, VUT+CUT+ensemble, 317 columns) were diffed against the DB's column set —
every column in both files is present in the DB, and DuckDB ingest uses
`union_by_name` across all files at a resolution, so the DB's column set is already
the union of every site's own header. An absent-from-DB column therefore cannot be
present in any individual extracted file either: the LE/H absences above are real
absences in the FLUXNET product, not an ingest drop.

### Sub-daily (HH/HR) file extraction

**31 of 781 sites** have sub-daily FLUXMET files extracted on disk (30 at HH
half-hourly, 1 at HR hourly — `US-MMS`), per a live `flux_discover_files()` scan of
`data/extracted` (`table_stage0_hh_hr_sites.csv`). This is expected: `CLAUDE.md`'s
pipeline default is `FLUXNET_EXTRACT_RESOLUTIONS="y m d"` (no `h`), so the vast
majority of sites were never extracted at sub-daily resolution; these 31 are evidently
leftover from earlier ad hoc/test extractions. The DuckDB store's own `manifest` table
is stale on this point — it records **zero** HH/HR FLUXMET files — because it reflects
whatever was on disk when `03b_create_database.R` last ran, not current disk state.
Stage 1's sub-daily QC comparison is therefore based on these same 31 sites, read
directly from the extracted CSVs (not from the DB `hourly` table, which itself holds a
full ingested copy of only one of the 31 — `US-MMS`).

### BIF (BADM) u-star threshold / method records

All **781/781** sites have a `VARIABLE_GROUP = 'GRP_UST_THR'` group in their BIF file.
Variables found (`table_stage0_bif_ustar_summary.csv`): `USTAR_CP_SUCCESS_RUN` and
`USTAR_MP_SUCCESS_RUN` (one flag per year, CP = change-point method, MP = moving-point
method — 6336 site-year records each, matching the 6336 FLUXMET annual site-years
inventoried in Stage 0's column check), their `_YEAR` companions, `USTAR_PERCENTILE`,
`USTAR_PERCENTILE_YEAR`, `USTAR_THRESHOLD` and `USTAR_VERSION` (291797 records each —
one row per percentile/threshold estimate, i.e. the per-year, per-percentile detail
behind the annual ensemble columns above). A compact per-site success/failure count
for CP and MP is in `table_stage0_bif_ustar_site_summary.csv`; Stage 4 uses this to
tabulate u-star method failures.

### Hub-grouping decision

`CLAUDE.md` Hard Rule 2 prohibits inferring hub membership from site-ID prefixes and
says to use the manifest/snapshot "network" field instead. The manifest/snapshot
actually carries two distinct fields: `network` (a semicolon-separated list of every
community network a site has ever belonged to, e.g. `"AmeriFlux;NEON;Phenocam"` — not
usable as a single grouping key) and `data_hub` (single-valued: `AmeriFlux`/`ICOS`/
`TERN`/etc — the actual distributing hub, as used by `flux_discover_files()` and
`01_download.R`). Existing diagnostics in this repo
(`scripts/diagnostics/koppen_pi_vs_era5.R`, `era5_precip_units.R`) already
`group_by(data_hub)` for "by hub" breakdowns. **All "by hub" tabulations in Stages 1–4
of this diagnostic use `data_hub`**, matching that precedent — it is manifest-derived
from the download source, not inferred from site-ID prefixes, so it satisfies Hard
Rule 2's intent despite not being the literally-named `network` column.
