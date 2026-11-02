# GPP and RECO uncertainty inventory

*Read-only inventory. Reads the pre-QC DuckDB tables (`dataset = 'FLUXMET'`) and raw extracted
CSVs; writes nothing outside this directory. No analysis performed and nothing in `R/`,
`scripts/`, or any existing output touched. Informs a planned (not-yet-written)
`R/gpp_uncertainty.R` that would treat GPP as an ensemble across partitioning method (NT/DT), u*
threshold (REF and percentiles), and VUT/CUT.*

## Summary

GPP and RECO carry a full u*-threshold ensemble — `REF`, `MEAN`, `SE`, `USTAR50`, and the 7-point
percentile set (05/16/25/50/75/84/95) — for **both** partitioning methods (NT, DT) and **both**
u*-variance treatments (VUT, CUT), **at every resolution including daily**, confirmed against raw
FULLSET (`_FLUXMET_`) file headers for 3 sites with zero ingest drops found. The GPP/RECO
percentile ensemble is explicitly documented (BIFVARINFO) as coming from the **same up-to-40
u*-threshold NEE realizations** that produce NEE's own ensemble — each `GPP_NT_VUT_<pctl>` is "from
corresponding NEE_VUT version," so a GPP ensemble built from these columns is really resampling
the same underlying 40 NEE_VUT/NEE_CUT draws, not an independent uncertainty source.

**What's missing, and where it bites:** GPP/RECO have **no `RANDUNC`/`JOINTUNC` columns at all** —
unlike NEE, their random (turbulence) uncertainty is never separately quantified; only the
u*-threshold spread is exposed. **DT partitioning is not universal**: 30 of 781 network sites (5 of
the 153 `GSL_definition/expanded/tables/sites.csv` sites) have no DT-partitioned GPP/RECO at any
resolution — NT-only. **VUT is not universal either**: 49 of 781 sites (12 of 153) are CUT-only, no
`NEE_VUT_REF`/`GPP_*_VUT_*` at all. Coverage of the *full* ensemble (`REF`/`MEAN`/`SE`/percentiles)
is also consistently lower than the single-point `USTAR50` estimate for the same family (e.g.
daily `GPP_NT_VUT_REF`/`MEAN`/`SE`/`50`: 666 of 781 sites, 85.3%; `GPP_NT_VUT_USTAR50`: 701, 89.8%)
— the multi-realization summary requires more of the up-to-40 fits to have succeeded than a single
USTAR50-threshold run does. **Sub-daily (HH/HR) is the real limiter**: the DuckDB `hourly` table
currently holds only **1 of 781 sites** (US-MMS, HR-native — it happens to be in the 153-site GSL
set), even though raw HH/HR FLUXMET files are extracted on disk for 31 sites. Any
sub-daily-resolution use of the GPP/RECO/NEE ensemble for the other 30 would have to read those
raw CSVs directly; it cannot currently be queried from the DuckDB store the rest of this repo's
pipeline relies on.

## 1. Column inventory (`table1_column_inventory.csv`)

Checked all 16 DuckDB tables (5 base resolutions — `annual`, `daily`, `hourly`, `monthly`,
`weekly` — plus each one's `_qc` and `_converted` variant). 146 columns per table match the
GPP/RECO/NEE-uncertainty-family patterns at `annual`/`daily`/`monthly`/`weekly`; `hourly` has 126
(it lacks the day/night-split sub-columns the coarser resolutions carry, but adds
`NEE_*_RANDUNC_METHOD`/`NEE_*_RANDUNC_N`, which the coarser tables don't have). `_qc` tables carry
exactly the same 146/126 columns as their base table (0 dropped by QC filtering). `_converted`
tables carry the same set **plus** an unconverted `_native` backup of each one (not a new ensemble
member — DD/MM/WW carbon fluxes need no unit conversion per `R/units.R`, so `_converted` and
`_native` are numerically identical; this was not re-verified value-by-value here, only that no
column was dropped).

Per base resolution, the matched columns break down as:

| Family | NT/DT | VUT/CUT | Members present |
|---|---|---|---|
| `GPP_*` | NT, DT | VUT, CUT | `REF`, `USTAR50`, `MEAN`, `SE`, `05/16/25/50/75/84/95` (7 of each) |
| `RECO_*` | NT, DT | VUT, CUT | same 11-member set as GPP |
| `NEE_VUT_*` / `NEE_CUT_*` | — | VUT, CUT | `REF` + `REF_RANDUNC`/`REF_JOINTUNC` (+ `REF_DAY`/`REF_NIGHT` variants at DD/MM/WW/YY only), `USTAR50` (+ its own `RANDUNC`/`JOINTUNC`/`QC`/`SD`, + DAY/NIGHT at DD/MM/WW/YY, + `RANDUNC_METHOD`/`RANDUNC_N` at HH/HR only), `MEAN`, `SE`, `05/16/25/50/75/84/95` |

GPP/RECO have **no** `RANDUNC` or `JOINTUNC` member anywhere, at any resolution — confirmed by
pattern search across all 16 tables (0 matches for `^GPP_.*RANDUNC`, `^GPP_.*JOINTUNC`, same for
`RECO_`). Their only representation of uncertainty is the u*-threshold ensemble.

## 2. Raw vs. DuckDB header check (`table2_raw_vs_duckdb_check.csv`)

Compared raw `_FLUXMET_` (= FULLSET) file headers against DuckDB for 3 sites: **US-MMS**
(VUT+CUT, HR-native), **FI-Hyy** (VUT+CUT, HH), **JP-Nkm** (CUT-only, NT-only, HH) — at DD and at
whichever sub-daily resolution each site has extracted.

**Zero ingest drops** in all 6 site×resolution checks (US-MMS DD/HR, FI-Hyy DD/HH, JP-Nkm DD/HH):
every GPP/RECO/NEE-uncertainty-family column present in a site's raw header is present in the
corresponding DuckDB table, with no exceptions.

The only "extra" columns are on the DuckDB side for JP-Nkm (73 at DD, 63 at HH) — and these are
not an ingest problem: JP-Nkm's raw file simply has no `NEE_VUT_*` columns at all (CUT-only site)
and no `GPP_DT_*`/`RECO_DT_*` columns at all (no DT partitioning run for this site). DuckDB's
columns are a network-wide union, so these show up as NULL-for-this-site columns rather than
absent columns — a genuine per-site gap in the underlying FLUXNET product, not something the
ingest pipeline dropped.

## 3. Coverage (`table3_coverage.csv`)

Full per-column coverage (site count/share, row count/share — both network-wide, 781 sites, and
restricted to the 153 `GSL_definition/expanded/tables/sites.csv` sites) computed for `annual`,
`daily`, and `hourly`. Monthly/weekly were not separately computed: Section 1 already confirms
they carry the identical column set to daily/annual (same ONEFlux variables, different temporal
binning), so a separate coverage pass would only restate the same availability pattern at a
different aggregation window, not reveal a new ensemble member.

**-9999 handling**: FLUXNET's missing-value sentinel is already converted to SQL `NULL` by the
ingest step (`flux_read()`, `scripts/03_read.R`) before the DuckDB tables are built — confirmed
directly (no column in this family holds any value ≤ -9000 anywhere in the 2.3M-row daily table).
"Non-missing" below is therefore just `IS NOT NULL`.

Representative figures, daily resolution:

| Column | Network (781 sites) | GSL-153 sites |
|---|---|---|
| `GPP_NT_VUT_REF`/`MEAN`/`SE`/`50` | 666 (85.3%) | 139 (90.8%) |
| `GPP_NT_VUT_USTAR50` | 701 (89.8%) | 141 (92.2%) |
| `GPP_DT_VUT_REF` | 703 (90.0%) | 136 (88.9%) |
| `GPP_NT_CUT_REF` | 605 (77.5%) | 151 (98.7%) |
| `GPP_DT_CUT_REF` | 630 (80.7%) | 148 (96.7%) |
| `RECO_NT_VUT_REF` | 666 (85.3%) | 139 (90.8%) |
| `RECO_DT_VUT_REF` | 703 (90.0%) | 136 (88.9%) |
| `NEE_VUT_REF_RANDUNC`/`JOINTUNC` | 732 (93.7%) | 141 (92.2%) |

The GSL-153 subset is noticeably more CUT-complete than the network average (CUT coverage
≈97–99% vs. ≈78–81% network-wide) because that subset was itself selected on GPP/QC
availability (`GSL_definition/expanded/code/01_select.R`'s per-site VUT/CUT fallback).

VUT/CUT/DT availability by site (daily table, network-wide / GSL-153):

| | Network (781) | GSL-153 |
|---|---|---|
| VUT-only (no CUT) | 121 | 0 |
| CUT-only (no VUT) | 49 | 12 |
| Both VUT and CUT | 611 | 141 |
| No DT at all (NT-only) | 30 | 5 |

## 4. Definitions (`table4_bifvarinfo_definitions.csv`)

Pulled from `AMF_US-MMS_FLUXNET_BIFVARINFO_DD_1999-2023_v1.3_r1.csv`'s `GRP_VAR_INFO` group
(`VAR_INFO_VARNAME`/`VAR_INFO_DEFINITION` pairs) — ONEFlux's own per-variable glossary, generated
per site but using an identical template across the network (spot-checked). All 146 daily-table
matching columns have a definition; **0 are undocumented**. The `fluxnet` R package exposes an
equivalent reader, `flux_varinfo()`, over the same BIFVARINFO source; reading the CSV directly
was equivalent for this one-off lookup.

Representative definitions (verbatim):

- **`GPP_NT_VUT_REF`**: "Gross Primary Production, from Nighttime partitioning method, most
  representative realization across up to 40 RECO_VUT versions, calculated from half-hourly data,
  using model efficiency approach (MEF)."
- **`GPP_NT_VUT_MEAN`**: "...average from up to 40 daily GPP_NT_VUT versions, each from
  corresponding NEE_VUT version"
- **`GPP_NT_VUT_SE`**: "...calculated from up to 40 daily GPP_NT_VUT versions as (SD(GPP_NT_VUT) /
  SQRT(40))"
- **`GPP_NT_VUT_05`..`95`**: "XXth percentile from up to 40 daily Gross Primary Production
  versions, from Nighttime partitioning method (GPP_NT_VUT) with XX = 05, 16, 25, 50, 75, 84, 95"
- **`GPP_NT_VUT_USTAR50`**: "...based on NEE_VUT_USTAR50, calculated from half-hourly data" (the
  single-realization point estimate at the 50th-percentile u* threshold, not a summary of the
  ensemble)
- **`NEE_VUT_REF_RANDUNC`**: "Random uncertainty for NEE_VUT_REF, from random uncertainty of
  individual half-hours (rand(i)) = [SQRT(SUM(rand(i)^2)) / n]..."
- **`NEE_VUT_REF_JOINTUNC`**: "Joint uncertainty estimation for NEE_VUT_REF, including random
  uncertainty and USTAR filtering uncertainty. [SQRT(NEE_VUT_REF_RANDUNC^2 + ((NEE_VUT_84 -
  NEE_VUT_16) / 2)^2)]"

**Yes — confirmed, GPP's percentile ensemble comes from the same up-to-40 u* versions as NEE.**
`GPP_NT_VUT_MEAN`'s and the percentile columns' own definitions say so explicitly ("each from
corresponding NEE_VUT version"); there is no independent GPP-specific resampling. One upstream
wording oddity, quoted as-is and not corrected: `NEE_VUT_USTAR50`'s own definition text says
"using Variable Ustar Threshold (**CUT**)" where context (the `VUT` column name, and the parallel
`NEE_CUT_USTAR50` definition that correctly says CUT) makes clear it should say VUT — an
ONEFlux-side documentation typo, not a data error.

## 5. What this means for an ensemble-based GPP uncertainty utility

**Exists at daily resolution, for both NT and DT, both VUT and CUT:** `REF`, `USTAR50`, `MEAN`,
`SE`, and the 7-point percentile set (05/16/25/50/75/84/95) — a 10-member ensemble per
method×variant combination, 40 ensemble combinations total (2 methods × 2 variants × 10 members),
all drawing on the same underlying up-to-40 u*-threshold NEE realizations. Identical at annual,
monthly, and weekly resolution too (same columns, coarser aggregation).

**Exists only sub-daily (and only for 1 of 781 sites via the DuckDB store):** nothing additional —
the HH/HR `hourly` table carries the *same* column set as daily (actually 20 fewer, since it
drops the DAY/NIGHT split), so sub-daily resolution adds no new ensemble member type, only finer
time binning. The practical limit is access, not content: `R/gpp_uncertainty.R`, if it is to use
the DuckDB store as this repository's other pipeline code does, can currently reach sub-daily GPP
only for US-MMS.

**Missing outright, for every site and every resolution:** a GPP/RECO-specific random-uncertainty
term (`RANDUNC`/`JOINTUNC`) analogous to NEE's. Any propagated GPP uncertainty built purely from
`SE`/percentile spread across u* thresholds will therefore capture u*-threshold uncertainty only,
not turbulence/random-flux uncertainty — that would have to be approximated by carrying NEE's own
`RANDUNC`/`JOINTUNC` through the partitioning step, or left out and flagged.

**What limits an ensemble-based GPP uncertainty in practice:**

1. **DT is not available everywhere** — 30 of 781 sites (5 of the GSL-153 subset) have no
   DT-partitioned GPP at all; an ensemble spanning "method" must either drop those sites or fall
   back to NT-only for them.
2. **VUT is not available everywhere** — 49 of 781 sites (12 of the GSL-153 subset) are CUT-only.
3. **The full ensemble (`REF`/`MEAN`/`SE`/percentiles) has measurably lower site coverage than the
   single `USTAR50` point estimate** for the same method×variant (e.g. daily `GPP_NT_VUT`:
   85.3% vs. 89.8% of sites network-wide) — some sites have enough data for a single u*-threshold
   run but not for the full up-to-40-realization summary.
4. **GPP/RECO have no independent random-uncertainty term** (point above).
5. **Sub-daily is effectively a single-site resource via the DuckDB store** (point above) — not a
   blocker for a daily-resolution utility, but relevant if finer-than-daily ensemble behaviour is
   ever wanted.
6. **The GSL-153 subset is not representative of VUT/CUT/DT availability network-wide** (it was
   itself selected partly on GPP/QC availability) — any ensemble utility validated only against
   that subset should not assume its coverage numbers generalize to the full 781-site network.

None of these are reasons an ensemble GPP-uncertainty utility can't be built — they are the
concrete scope boundaries it needs to document: which method/variant combinations exist per site,
that the ensemble is a resampling of NEE's own u*-threshold spread rather than an independent GPP
uncertainty, and that no random/turbulence term exists for GPP itself.
