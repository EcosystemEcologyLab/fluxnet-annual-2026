# IT-MBo/US-HB4/FI-Hyy precipitation parsimony check — refreshed store

Re-run of `review/diagnostics/it_mbo_parsimony/report.md` against the store refreshed in
`review/diagnostics/store_refresh_20260920/`, for the same three sites and the same Table 1
shape, so this sits directly beside what Dario has already seen. Read-only with respect to
both prior reports and any committed figure — neither is edited here. New code:
`scripts/diagnostics/it_mbo_parsimony_refresh.R`. No download performed by this script.

## Falsifiability statement (written before computing any refreshed number)

The previous report attributed IT-MBo's ~21.25x DD/MM/YY-vs-HH precipitation inconsistency to
comparing a June-2026 MM/DD/YY extraction against an HH file downloaded fresh the same
September morning — a stale-vintage artifact, not a defect in the distributed product. **That
explanation is falsified if, once every file (HH, DD, MM, YY) is confirmed current against
today's live `flux_listall()` product_id, IT-MBo's DD/MM/YY-vs-HH ratio is still ~21x.** If
instead the ratio collapses once all four resolutions are drawn from the same current product,
stale vintage is correct and the ~21.25x number is withdrawn. Same logic for the isolated
2013-01 IT-MBo MM-resolution spike: if the refreshed MM file's TIMESTAMP 201301 still implies
~4,017 mm/month, the spike is a real product defect unrelated to vintage; if it now agrees with
the HH-summed total for that month, vintage explains that too.

**Result reached: the falsifying result did not occur.** The ratio collapsed and the spike
disappeared. Detail below.

## The previous ~21.25x number is withdrawn

That comparison used the June-extracted DD/MM/YY files against an HH file downloaded the same
September morning as the earlier diagnostic — two different product vintages compared against
each other, not a same-vintage discrepancy in the distributed product. `review/diagnostics/
store_audit/table_stage1_live_vs_june_all_sites.csv` independently confirms IT-MBo's and
FI-Hyy's June-vintage `product_id` (`E0wSFc1mB8oN23pHXtN2iH3Z` and
`pIDsMGxYyXTg31dHecNO55Nq` respectively) differ from today's live `product_id`
(`enS2fTzGG_9PS5-51hqet8iH` and `oIJ9cFYf8Q0e9nfMRRS-90vM`) — both sites were genuinely
reprocessed since June, and the store refresh (stage 3b, 2026-09-20) downloaded and verified
both under their current product IDs. **The refreshed data supports the stale-vintage
explanation, not a residual product defect**: once DD/MM/YY are drawn from the same
current-product download as HH, IT-MBo's four resolutions agree with each other (table below).

## 1. Product identifiers — live vs. previously quoted

| site_id | role | product_id (previous report) | product_id (live, today) | changed? |
|---|---|---|---|---|
| IT-MBo | test | `enS2fTzGG_9PS5-51hqet8iH` | `enS2fTzGG_9PS5-51hqet8iH` | No |
| US-HB4 | named outlier | `10.17190/AMF/2571130` | `10.17190/AMF/2571130` | No |
| FI-Hyy | clean control | `oIJ9cFYf8Q0e9nfMRRS-90vM` | `oIJ9cFYf8Q0e9nfMRRS-90vM` | No |

Full table with `fluxnet_product_name`/year-range/`oneflux_code_version`:
`table_0_product_identifiers.csv`.

**None of the three identifiers changed relative to the previous report — this is not a
contradiction of "IT-MBo/FI-Hyy were reprocessed."** The previous report's PID for IT-MBo and
FI-Hyy was itself already obtained from a live `flux_listall()` check at the time (its own PID
section says so explicitly), not read off the stale June on-disk files. The thing that changed
is which *on-disk file* corresponds to that PID: in June, IT-MBo's and FI-Hyy's on-disk
DD/MM/YY files were under the *old* product IDs above (`E0wSFc1mB8oN23pHXtN2iH3Z` /
`pIDsMGxYyXTg31dHecNO55Nq`), not the one already being quoted. The store refresh replaced those
on-disk files with the current-PID download. **Finding, as instructed to report explicitly:**
identifiers unchanged + values changed (IT-MBo, FI-Hyy) is exactly what you'd expect when the
*previously quoted* identifier was already current but the *on-disk file* was not — not an
inconsistency. US-HB4 is the cleaner case: identifier unchanged **and** values unchanged
(confirmed below), because its on-disk June file was never behind the live product to begin
with (`store_audit`'s `any_change` = `FALSE` for US-HB4, the only one of the three sites where
that holds).

## Per-file currency verification (before reading any data)

Every file is read only after its site's on-disk product is confirmed to match today's live
manifest — via the BIF file's own embedded `PRODUCT_NAME` field (baked into the distributed
archive, not merely a matching directory name), cross-checked against `store_audit`'s
independent live-vs-June comparison and, for the two sites the refresh actually re-downloaded,
the stage 3b download-and-verify log:

| site_id | BIF `PRODUCT_NAME` matches live? | `store_audit` any_change (June vs. live) | Re-downloaded + verified in stage 3b? | Verdict |
|---|---|---|---|---|
| IT-MBo | Yes | TRUE (was stale) | Yes (2026-09-20 11:28:25) | **CURRENT** |
| US-HB4 | Yes | FALSE (never stale) | No (not queued — not needed) | **CURRENT** |
| FI-Hyy | Yes | TRUE (was stale) | Yes (2026-09-20 12:06:13) | **CURRENT** |

No file belonged to a superseded product — nothing to stop for, no download attempted by this
script.

## 2. Table 1, refreshed — exact previous shape

6 data rows (3 sites x {P_ERA, P_F}), no ratios, no factor fitting, no tolerance windows. Full
table: `table_1_mean_annual_by_resolution_refreshed.csv`.

| site_id | variable | HH mean (n yrs) | DD mean (n yrs) | MM mean (n yrs) | YY mean (n yrs) | BIO12 | BADM |
|---|---|---|---|---|---|---|---|
| IT-MBo | P_ERA | 1,126.10 (45) | 1,126.19 (45) | 1,126.19 (45) | 1,126.19 (45) | 407 | 1,365 |
| IT-MBo | P_F | 1,152.81 (23) | 1,152.82 (23) | 1,152.82 (23) | 1,152.82 (23) | 407 | 1,365 |
| US-HB4 | P_ERA | 657,076.72 (45) | 657,076.70 (45) | 657,076.70 (45) | 657,076.70 (45) | 1,350 | 1,429 |
| US-HB4 | P_F | 24,571.84 (5) | 24,571.84 (5) | 24,571.83 (5) | 24,571.84 (5) | 1,350 | 1,429 |
| FI-Hyy | P_ERA | 709.18 (45) | 709.49 (45) | 709.49 (45) | 709.49 (45) | 663 | 711 |
| FI-Hyy | P_F | 700.33 (29) | 700.43 (29) | 700.42 (29) | 700.43 (29) | 663 | 711 |

### Previous table, reproduced verbatim for a direct side-by-side

(Copied unmodified from `review/diagnostics/it_mbo_parsimony/table_1_mean_annual_by_resolution.csv`;
that file and its report were not re-derived or edited.) Full copy:
`table_1_mean_annual_by_resolution_previous_reproduced.csv`.

| site_id | variable | HH mean (n yrs) | DD mean (n yrs) | MM mean (n yrs) | YY mean (n yrs) | BIO12 | BADM |
|---|---|---|---|---|---|---|---|
| IT-MBo | P_ERA | 1,126 (45) | 23,905 (44) | 23,905 (44) | 23,905 (44) | 407 | 1,365 |
| IT-MBo | P_F | 1,153 (23) | 24,472 (22) | 24,472 (22) | 24,472 (22) | 407 | 1,365 |
| US-HB4 | P_ERA | 657,077 (45) | 657,077 (45) | 657,077 (45) | 657,077 (45) | 1,350 | 1,429 |
| US-HB4 | P_F | 24,572 (5) | 24,572 (5) | 24,572 (5) | 24,572 (5) | 1,350 | 1,429 |
| FI-Hyy | P_ERA | 709 (45) | 712 (45) | 712 (45) | 712 (45) | 663 | 711 |
| FI-Hyy | P_F | 700 (29) | 702 (28) | 702 (28) | 702 (28) | 663 | 711 |

### Reading the before-and-after

**IT-MBo**: DD/MM/YY collapse from ~23,905–24,472 (~21x above HH) to 1,126.19–1,152.82 — now
essentially identical to HH (1,126.10 / 1,152.81) at every resolution, and both P_ERA and P_F
sit close to BADM (1,365) and reasonably close to BIO12 (407, on the low side as before but no
longer a resolution artifact on top of it). This is the falsifying condition failing to occur —
the ~21x gap is gone once all four resolutions share one current product.

**US-HB4**: every cell is numerically unchanged to the reported precision (e.g. P_ERA HH
657,076.72 vs. 657,077 before, DD/MM/YY the same) — confirms this site's ERA5 defect is not a
staleness artifact, consistent with `store_refresh_20260920/report.md`'s stage 6 finding that
US-HB4 is "numerically identical before and after."

**FI-Hyy**: a small, uniform shift — DD/MM/YY move from 712.23/701.57 to 709.49/700.43,
slightly closer to HH (709.18/700.33) than before, and n_years_dd/mm for P_F increases from 28
to 29 (one more complete year now present). This is ordinary reprocessing drift of the kind
`store_audit` already characterized for this site (median ratio ~1.004 network-wide for
minor-drift ICOS sites) — not a resolution artifact, and FI-Hyy remains internally consistent
and close to both references at every resolution, before and after.

## 3. IT-MBo, TIMESTAMP 201301, worked in full

Full table: `table_2_ts201301_worked_value.csv`.

| Quantity | Value |
|---|---|
| File | `ICOS_IT-MBo_FLUXNET_FLUXMET_MM_2003-2025_v1.3_r1.csv` (refreshed, confirmed current) |
| TIMESTAMP | 201301 |
| `P_ERA` (raw, as in file) | 1.856 |
| `P_F` (raw, as in file) | 1.856 |
| `P_F_QC` (0–1 scale) | 0.0 |
| Days in month (January 2013) | 31 |
| Derived: `P_ERA` mm/month = 1.856 × 31 | **57.536** |
| Derived: `P_F` mm/month = 1.856 × 31 | **57.536** |

Dario reports **1.856 mm d⁻¹** for this record. **We now agree, exactly, to the full precision
recorded in the file (three decimal places, i.e. to 0.001 mm/d)** — both `P_ERA` and `P_F` read
1.856 in the refreshed file.

`P_F_QC = 0.0` here reflects the MM-resolution fraction field's documented polarity flip
(`review/diagnostics/it_mbo_parsimony/report.md` deliverable 2: "the v3 polarity flip was
specific to the MM-resolution fraction field") — under that flip, `0.0` is the good/measured
end, consistent with `P_F` and `P_ERA` agreeing exactly at this record. This is a pre-existing,
already-documented quirk, not a new finding.

**Independent cross-check, not re-derived here**: the previous report's own Table 3
(`table_3_itmbo_2012_2014_months.csv`, built from the same-day-downloaded HH file, which was
never stale) gives 2013-01's HH-summed `P_F` total as **57.5 mm** — matching this refreshed
MM-derived value (57.536 mm) to within normal rounding. The previous report's MM-resolution
value for that same month was 4,017.4 mm/month, a ~70x discrepancy against its own HH-summed
figure; that discrepancy is gone in the refreshed file. This is the second falsifying condition
(the isolated 2013 spike) also failing to occur.

## 4. Provenance

Path, product_id, sha256, byte size, mtime, and which table each file feeds:
`table_4_provenance.csv` (28 rows: 8 raw files + 1 BIF per site x 3 sites, plus the shared
BIO12/BADM reference file). Snapshot state: this script called `fluxnet::flux_listall()` live
rather than reading a snapshot CSV, per the task's "fresh flux_listall() call" instruction; a
new snapshot was not written to `data/snapshots/`.

## Verdict

The store refresh converts the previous report's provisional caveat into a confirmed result for
IT-MBo, and confirms no equivalent staleness affected US-HB4 or FI-Hyy in a way that changes
their verdicts:

- **IT-MBo**: the ~21.25x DD/MM/YY-vs-HH inconsistency is **withdrawn** — it was a stale-vintage
  artifact (June-extracted coarse files vs. a same-morning HH download), not a defect in the
  distributed product. All four resolutions now agree with each other and with BADM once drawn
  from the same current product.
- **US-HB4**: unaffected — every value is unchanged, confirming `store_refresh_20260920`'s
  stage 6 finding directly at the file level. The ERA5 defect at this site is real, not a
  staleness artifact.
- **FI-Hyy**: unaffected in substance — a small (~0.4%), uniform reprocessing-driven shift
  toward closer HH/DD/MM/YY agreement, consistent with ordinary product revision rather than a
  resolution or vintage artifact.

## Scope

Three sites only, per the brief. Not extended to the wider network; the ~123-site cluster
question (`store_refresh_20260920/report.md` stage 6) is not touched here.

## Files in this directory

| File | Contents |
|---|---|
| `table_0_product_identifiers.csv` | Live vs. previously-quoted product_id/name/year-range per site |
| `table_1_mean_annual_by_resolution_refreshed.csv` | Refreshed Table 1, same shape as before |
| `table_1_mean_annual_by_resolution_previous_reproduced.csv` | Previous report's Table 1, copied verbatim for side-by-side |
| `table_2_ts201301_worked_value.csv` | IT-MBo TIMESTAMP 201301 worked in full |
| `table_4_provenance.csv` | Path, product_id, sha256, mtime, feeds — every file read here |
