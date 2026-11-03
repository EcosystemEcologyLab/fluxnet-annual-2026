# WAFNET x FLUXCOM comparison — methods memo

Side analysis, not part of the FLUXNET Annual Paper 2026 (see `../README.md`).
Every filtering decision and assumption made in `code/` is recorded here as
it is made, same convention as `WAFNET/energy_partitioning/docs/methods_memo.md`.
Stage 1 (pre-analysis only, no download, no statistics) is fully recorded in
`report_back_20261014.md`; this file starts at Stage 2, where the first
real filtering/derivation decisions were made.

## Sites

Two site sets, kept separate everywhere (`code/00_config.R`):

- `STAGE2_SITES` (five, agreed): `GH-Ank`, `BJ-Nhu`, `BJ-Bfg`, `SN-Dhr`,
  `ML-Kem` (added 2026-11-03 at Ossénatou Mamadou's request — Kelma, Mali,
  WSA, 2007–2009).
- `STAGE2_PENDING_SITES` (two, flagged `pending Ossénatou`): `BJ-Db1`,
  `SN-Nkr` — excluded from the agreed set in Stage 1 only for lack of
  temporal overlap with the originally-chosen product, re-checked against
  FLUXCOM-X-BASE's longer period in Stage 2 (`BJ-Db1`: still zero overlap;
  `SN-Nkr`: now has 46/80 overlapping months, 3/6 overlapping years).

## Product choice (2026-11-03)

The original FLUXCOM product (RS+METEO) Ossénatou's 16 October reply chose
is currently unobtainable (MPI-BGC portal link 404, no longer listed).
**Decision (Dave):** proceed with FLUXCOM-X-BASE instead, keep the original
as an option if it resurfaces. A documentation-only backup copy of the
original ensemble (gpp/reco/le/sh) was pulled from the ILAMB reference
collection and is explicitly **not used** in any comparison — see
`report_back_2026-11-03.md` section 2 for its own metadata.

## Partition match (2026-11-03) — a correction from Stage 1's default

Stage 1 used the tower's **DT** partition (`GPP_DT`/`RECO_DT`) by default,
per the WAF-Net paper's general preference (documented in `../README.md`).
FLUXCOM-X-BASE's own ICOS metadata states its GPP is trained on the
**nighttime (NT)** partition only (`report_back_2026-11-03.md` section 3,
high confidence, read directly from the product description, not inferred).
**All X-BASE GPP/RECO comparisons therefore use `GPP_NT`/`RECO_NT`**, not
`GPP_DT`/`RECO_DT`. The DT rows already written to
`tables/site_variable_qc_overlap_{yy,mm}.csv` in Stage 1 are **kept, not
removed** — `code/06_tower_data_update.R` adds NT rows alongside them,
superset not replacement, in case a future DT-partitioned product (e.g. the
original FLUXCOM carbon line, if it becomes available again) is compared.

**`GH-Ank` has no NT-partitioned GPP/RECO at all** in its FLUXMET extract
(`GPP_NT_VUT_REF`/`RECO_NT_VUT_REF` are 100% `NA` across all 48 MM rows,
checked directly; `GPP_DT_VUT_REF`/`RECO_DT_VUT_REF` are present for 34 of
48). This is a genuine data-availability gap, not a QC exclusion — `GH-Ank`
is skipped for GPP/RECO only (NEE and LE are unaffected, since neither
depends on the NT/DT partition).

## NEE sign convention (2026-11-03) — checked, not assumed

X-BASE's own netCDF/ICOS metadata carries no explicit sign-convention
attribute. Checked empirically against `GH-Ank` 2001 (a known strong
tropical carbon sink): X-BASE GPP is positive year-round, X-BASE NEE is
negative year-round — the same atmospheric convention
(negative = ecosystem uptake, positive = release) as the tower's
`NEE_VUT_REF`/`NEE_CUT_REF`. **No sign flip applied.**

## RECO derivation (2026-11-03)

X-BASE distributes no respiration variable. `RECO_derived = GPP + NEE`
(same X-BASE series, same sign convention as above). Labelled `derived`
(a `TRUE`/`FALSE` column) in every table and figure that shows it. Not a
measured-against-measured comparison — this is explicit in
`report_back_2026-11-03.md` and should be stated wherever these numbers are
reused.

## ET -> LE conversion (2026-11-03)

X-BASE's `ET`/`ET_T` are **mean rates in `mm hr-1`**, confirmed by reading
the downloaded netCDF unit attribute directly (not assumed from the paper,
which reports only aggregated km³ totals). Converted using the same
λ = 2.45×10⁶ J/kg as `R/units.R`'s own LE conversion:
`LE[W/m2] = ET[mm/hr] * λ / 3600` (1 mm of water over 1 m² is exactly 1 kg
at ρ_water = 1000 kg/m³, the identity underlying "mm of water" itself — no
separate density factor is needed beyond that). `H` has no X-BASE
equivalent at all and is never compared.

## Carbon unit match (2026-11-03) — no conversion applied

X-BASE's GPP/NEE at monthly resolution are `gC m-2 d-1` mean daily rates —
the **same unit** the tower's native MM FLUXMET extract already uses
(confirmed independently twice: Stage 1's BIFVARINFO read, and this stage's
direct netCDF units-attribute read). Compared as distributed; **not**
multiplied by days-in-month (that `R/units.R` conversion turns a mean rate
into a period *total*, which this comparison does not need).

## Statistics convention (carried from Stage 1 / Ossénatou's reply)

- Monthly statistics are the primary result.
- Annual statistics are a secondary look, computed **only** for site × flux
  combinations with ≥ 3 overlapping annual values, and only from calendar
  years with all 12 qualifying months present (`code/08_extract_statistics.R`).
- **Bias = product (X-BASE) minus tower**, stated in every output.
- Training sites (`GH-Ank`, `SN-Dhr`, confirmed section 3) marked with a
  distinct symbol (triangle) in every figure; pending sites (`BJ-Db1`,
  `SN-Nkr`) with another (square) and a `site_status` column in every table.

## Figure fix (2026-11-03)

`code/09_figures_draft.R`'s site-bias map initially used one shared
colour/size scale across all four flux panels — carbon fluxes (O(1–5)
gC m⁻² d⁻¹) and LE (O(10–40) W m⁻²) are on different magnitude scales, so
the carbon panels' bubbles were nearly invisible next to LE's. Fixed by
giving each flux panel its own independent colour/size scale
(`patchwork::wrap_plots()` of four separately-scaled `ggplot`s, not
`facet_wrap()` with a single shared scale) before committing.

## Open items

- `BJ-Db1` cannot currently be compared against any FLUXCOM release (zero
  overlap confirmed against both the original product, Stage 1, and X-BASE,
  Stage 2) — its tower record (2022-05 onward) postdates every available
  product period.
- `SN-Nkr`'s annual secondary numbers (n=3 years) should be read as
  low-confidence per Stage 1's original caveat — a 3-year annual sample.
- `BJ-Nhu`'s LE bias has opposite sign between the full monthly sample and
  the annual (3-year) secondary look — different underlying years, not a
  contradiction, but worth knowing before either number is quoted alone
  (`report_back_2026-11-03.md` section 6).
- The original FLUXCOM product's own availability should be rechecked
  periodically; if it resurfaces, a like-for-like Reco/H/DT comparison
  becomes possible again and this memo's "no Reco/H, NT-only" constraints
  would need revisiting, not necessarily discarding (X-BASE may still be
  worth reporting alongside it for its longer period).
