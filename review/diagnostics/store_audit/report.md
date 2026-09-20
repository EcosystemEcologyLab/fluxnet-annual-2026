# Extracted-data store audit

> Prompted by the 2026-09-20 finding (`review/diagnostics/it_mbo_file_check/report.md`) that
> IT-MBo's on-disk June 2026 extraction differs substantially from the currently-distributed
> FLUXNET archive under the same PID. This audit asks: is IT-MBo isolated, or does the whole
> store need a refresh?

## Decision rule (fixed before looking at Stage 1/2 evidence)

**If any site other than IT-MBo differs from the archive in a variable this project uses, the
whole store gets refreshed.** This is a detection question, not an estimation question — the
rule does not soften if only a few sites, or only small differences, are found.

**Verdict: the rule fires.** Stage 1's live-manifest comparison (zero downloads required) found
**61 of 759 on-disk sites (8.0%)** whose `product_id` and/or `fluxnet_product_name` and/or
year-range differ from what a live `flux_listall()` reports today — every one of them a
`data_hub == "ICOS"` site (`product_source_network` sub-labels: ICOS 55, EUF 3, JPF 2, FLX 1).
Stage 2 (below) confirms this is not purely a metadata artifact: real, non-zero content
differences exist beyond IT-MBo, in flux variables as well as precipitation. Refresh is called
for. Its scope, cost, and how it differs from "IT-MBo was a local anomaly" are in the closing
section.

---

## Stage 1 — inventory and refresh-mechanism audit (no downloads)

### Extraction vintage distribution

786 directories on disk, 781 unique sites (5 sites have two directories — see below).

| Extraction date | n dirs |
|---|---|
| 2026-06-01 | 281 |
| 2026-06-02 | 453 |
| 2026-09-01 | 21 |
| 2026-09-20 (today) | 30 |
| 2026-05-25 | 1 |

**734 of 786 directories (93%) date from the original June 1-2, 2026 bulk download and its
June 2 corrupt-zip redownload.** Everything this project has built — every ERA5/precipitation
diagnostic, the DuckDB database, every figure — rests overwhelmingly on that one snapshot in
time. Full inventory: `table_stage1_extraction_inventory.csv`.

### Sites with two extracted directories

| site_id | old dir (mtime) | new dir (mtime) | why |
|---|---|---|---|
| IT-MBo | 2026-06-01 (MM/DD/YY) | 2026-09-20 (HH only) | `it_mbo_bug_hunt.R` targeted re-download |
| FI-Hyy | 2026-06-01 (MM/DD/YY) | 2026-09-20 (HH only) | same, used as control site |
| DE-Hzd | 2026-06-01 (MM/DD/YY) | 2026-09-20 (HH only) | `cluster_resolution_sample` control site |
| GF-Guy | 2026-06-01 (MM/DD/YY) | 2026-09-20 (HH only) | `cluster_resolution_sample` control site |
| IE-Cra | 2026-06-01 (MM/DD/YY) | 2026-09-20 (HH only) | `cluster_resolution_sample` control site |

**All five are also in the 61-site changed-metadata list below** — i.e. every site that today's
earlier diagnostics happened to re-download for HH data turns out to already have been flagged
by this audit as reprocessed upstream. This is coincidence (these sites were chosen as
"unaffected controls," not because anyone knew they were reprocessed), not selection.

### Refresh/re-fetch logic in the pipeline — does an already-downloaded site ever get re-fetched?

**Yes, by design, correctly, for all hubs — but the mechanism was never actually re-run after
the original June download**, which is the real root cause of the staleness, not a logic bug.

Quoting `scripts/01_download.R:110-123`:
```r
# Extend download queue with any sites absent from data/extracted/.
...
new_to_queue <- setdiff(
  setdiff(sites_to_check, already_extracted),
  download_manifest$site_id
)
```
This only adds sites **entirely absent** from `data/extracted/` — it never re-queues a site
that already has *some* extraction on disk, no matter how stale.

The actual re-fetch trigger for already-downloaded sites is `sites_to_download()`
(`R/sync.R:150-163`), called from `01_download.R:64`, which unions three categories from
`compare_snapshots()` (`R/sync.R:96-160`): `new_sites`, `extended_data` (year range grew), and
`reprocessed` (`product_id` or `fluxnet_product_name` changed). Per `R/sync.R`'s own docstring:

> "Reprocessing detection confirmed for all hubs by Dario Papale (2026-03-30): product_id is a
> checksum-based PID that changes whenever a file is updated — reliable reprocessing signal for
> ALL hubs."

**This logic is correctly hub-agnostic and would have caught IT-MBo's June-to-August
reprocessing (see below) had it ever been exercised against a same-era snapshot pair.** But
`scripts/01_download.R:56-61`'s console message still reads:
```r
"reprocessed AmeriFlux: ",  nrow(comparison$reprocessed_ameriflux), "; ",
"reprocessed ICOS/TERN: pending Gilberto team response (support@fluxnet.org)"
```
`comparison$reprocessed_ameriflux` does not exist in the list `compare_snapshots()` returns
(it returns a single all-hub `reprocessed` field, per commit `7d87396`, which superseded the
ICOS/TERN-pending framing from `f638fa8`). This message is **stale and misleading** — it
understates coverage the actual code (`sites_to_download()`, called two lines later at
`01_download.R:64`) already has. It should be fixed, but is not the cause of the staleness:
it is cosmetic, not functional (confirmed: `sites_to_download()` uses the correct all-hub
`comparison$reprocessed` field, not the stale-named one only the message references).

**The real cause**: the compare-and-download cycle simply never ran again after the original
June bulk download. The two subsequent snapshot-related activities were narrower:
- `logs/pull_snapshot_20260827.log`: a bare `write_snapshot()` call — records a new manifest,
  performs **no comparison, no download**.
- `logs/gap_download_delta_20260901.log` / `gap_check_listall_discover_...`: a "gap fill" for
  22 sites **absent from `data/extracted/`** (new sites), not a reprocessing check on existing
  sites — see `Download manifest rows: 22` and the hard-coded site list in that log, none of
  which overlap the 61 changed sites found below.

**Neither run ever called `sites_to_download()`'s `reprocessed`/`extended_data` categories
against the June baseline.** The detection logic works; it was never invoked end to end since
June.

### Live manifest vs. June snapshot — the actual count

Comparing the June 1 snapshot (`fluxnet_shuttle_snapshot_20260601T224043.csv`, 759 on-disk
sites) against a **live `flux_listall()` run today** (781 sites, zero downloads required):

**61 / 759 on-disk sites (8.0%) now differ** in `product_id` and/or `fluxnet_product_name`
and/or year range. All 61 are `data_hub == "ICOS"`. Full list:
`table_stage1_changed_sites.csv`; full per-site comparison (all 759):
`table_stage1_live_vs_june_all_sites.csv`.

IT-MBo's own change, confirmed directly in the snapshot history
(`data/snapshots/fluxnet_shuttle_snapshot_*.csv`, 27 files spanning 2026-03-28 to 2026-09-01):

| snapshot date | product_id | last_year | fluxnet_product_name |
|---|---|---|---|
| 2026-03-28 through 2026-06-24 (24 snapshots) | `E0wSFc1mB8oN23pHXtN2iH3Z` | 2024 | `..._2003-2024_v1.3_r1.zip` |
| 2026-08-27, 2026-09-01 | `enS2fTzGG_9PS5-51hqet8iH` | 2025 | `..._2003-2025_v1.3_r1.zip` |

**IT-MBo was reprocessed by ICOS between 2026-06-24 and 2026-08-27** — a real, detectable
product_id change. **This corrects a specific claim in `it_mbo_bug_hunt/report.md:150-155`**:
that report states "IT-MBo's `product_id` and `oneflux_code_version` are confirmed unchanged"
and "there is no newer/corrected version of this product upstream," based on comparing a live
`flux_listall()` call against *the fresh HH-only directory `it_mbo_bug_hunt.R` had itself just
downloaded* (both `enS2fTzGG_9PS5-51hqet8iH`) — not against the actual old on-disk MM/DD/YY
directory it was reading from for the rest of that report's D2/D3 tests, which was built under
the older `product_id` `E0wSFc1mB8oN23pHXtN2iH3Z` (confirmed directly in the snapshot history
above). The comparison that report actually needed to make — old directory's PID vs. live PID
— was never made; it compared today's fresh download against itself. The product *was*
reprocessed between the old directory's creation and today.

**This lower-bound caveat matters**: metadata-only screening is necessary but not sufficient.
Stage 2 (below) directly demonstrates a counter-example: `FI-Hyy`'s `product_id` also changed,
yet its precipitation and flux variables in the overlapping historical period are effectively
unchanged. A `product_id` diff proves *something* in the file changed; it does not by itself
say whether the variables this project uses were affected, or by how much — that is exactly
what Stage 2's content comparison is for.

**No-download coverage note**: because this comparison uses `flux_listall()` directly rather
than any downloaded file, it covers all 759 on-disk sites' metadata at zero cost — a
materially cheaper and more complete first pass than downloading a sample.

### Known prior failures/retries (compiled from `logs/`, feeds Stage 2's exhaustive list)

| site_id | issue | resolution |
|---|---|---|
| US-ARM, US-Aud, US-Bar, US-Bi1, US-Bi2, US-BZB | Jun 1 truncated ZIP | Redownloaded + verified `unzip -t`, Jun 2 (`dl_redownload_failed_20260602.R`) |
| RU-Ege, MY-LHP, AU-Ya1 | Truncated on first attempt | Retried + verified same day (`cluster_resolution_sample_retry.R`, 2026-09-20) |

No other `fail`/`error`/`corrupt`/`retry` hits in `dl_local_full_20260601.log`,
`dl_gap_20260601.log`, or the `gap_*_20260901` logs. Two unrelated data-completeness issues
surfaced while reading these logs (not staleness, noted for completeness):
`gap_verify_cleanup_20260901.log` flags `FI-Si2` and `JP-Nkm` as `WARN — missing
NEE_VUT_REF` in their extracted files.

**Hard Rule 5 compliance note**: every script run in this audit printed
`check_pipeline_config()`'s warning that the installed `fluxnet-shuttle` version (`0.3.8`)
does not match `FLUXNET_SHUTTLE_VERSION` (`0.3.7.post0+dirty`) — left visible in every log
in `logs/store_audit_*`, not suppressed, per the Hard Rule. Not investigated further here
(out of this audit's scope), but worth noting alongside the reprocessing-detection findings
above since a shuttle version drift is a second, independent channel through which the local
environment can silently diverge from what the archive currently serves.

---

## Stage 2 — one download pass, content comparison

**Site list** (`table_stage2_site_list.csv`, written before any download, seed `20260920`):
45 sites — 9 exhaustive failed/retried, 20 stratified from the 61 changed-metadata sites
(all 6 non-ICOS-sublabel sites + 14 of the 51 remaining ICOS-sublabel sites), 16 controls
(`US-HB4` + 5 AmeriFlux + 5 TERN + 5 ICOS, all metadata-unchanged). Plus **5 sites already
free-checked with zero new downloads** (`table_stage2_free_checked_sites.csv`: IT-MBo, FI-Hyy,
GF-Guy, IE-Cra, DE-Hzd) — their fresh zips already existed on disk from earlier same-day
diagnostics (`data/raw/*_hh_check/`, `data/raw/ICOS_*.zip`, still present because those runs
did not delete zips), so their MM files were extracted and compared directly at zero
additional cost before this pass launched.

**Download**: PID `27927`, log `logs/store_audit_stage2_download_20260920.log`. Launched via
`nohup ... & disown`. Verifies each zip with `unzip -t` before extracting; retries a truncated
site once, deleting the corrupt zip first; restart-safe (`already_done()` skips any site
already extracted into the scratch dir). Scratch location: `data/raw/store_audit_scratch/`
(covered by the existing `data/raw/` `.gitignore` pattern).

### Content comparison — all sites checked so far, met + flux variables

**Classification note**: an earlier version of the comparison script's severity threshold had
two real bugs, both caught and fixed before finalizing this report (not left in a scratch
file — the fixes are in the committed `store_audit_stage2_compare.R`): (1) it flagged *any*
site where every month differed by even a trivial amount as "uniform factor," conflating
IT-MBo's genuine ~21x defect with FI-Hyy's trivial 1.004x reprocessing rounding; (2) its
replacement used a bare >2x/<0.5x ratio threshold, which is unreliable for `NEE_VUT_REF` and
`TA_ERA`/`TA_F` (both cross zero — a swing from -0.05 to +0.05 gC/m2 is a physically trivial
difference but an undefined or wildly "extreme" ratio). The committed script instead requires
an **absolute** difference beyond a fixed, stated, per-variable floor (2 mm/day for
precipitation/temperature-scale variables, 10 units for flux variables) before a month counts
as "severe" — chosen as round numbers well above ordinary reprocessing noise (FI-Hyy's largest
drift across all 8 variables was ~1 unit) and well below IT-MBo's actual defect (>37 mm/day).

**Site-level result — the full planned 50-site sample, download complete (45/45 OK, 0
failed; PID `27927` exited cleanly, see download status below)**:

| status | n sites | sites |
|---|---|---|
| IDENTICAL | 25 | all 9 exhaustive failed/retried sites, all 15 `control_unchanged` sites, and `US-HB4` — **US-HB4's own known defect is present identically in both the old and a fresh download, confirming it is a genuine, still-current archive defect, not a staleness artifact** (consistent with `it_mbo_file_check`'s earlier finding) |
| DIFFERS_UNIFORM (severe, IT-MBo-like) | 1 | IT-MBo only |
| DIFFERS_SCATTERED (a handful of severe months) | 4 | DE-HoH (`P_F`, 1/27 differing months severe, max abs diff 3.85 mm/day), GF-Guy (`P_ERA`, 13/96 months severe, median ratio 0.894, max abs diff 3.14 mm/day), **IT-BCi (`LE_F_MDS`, a flux variable — 4/13 differing months severe, max abs diff 34.8 W/m2)**, **SE-Svb (`H_F_MDS`, another flux variable — 1/41 months severe, max abs diff 10.1 W/m2)** — all four real but an order of magnitude smaller than IT-MBo's pattern, not a repeat of it |
| DIFFERS_MINOR (ordinary reprocessing drift, no severe months) | 20 | remaining changed-metadata sites |

Full per-variable table: `table_stage2_variable_comparison.csv`; per-site summary (incl.
sha256/byte-size/year-range for every compared file): `table_stage2_site_summary.csv`.

**Restricting to sites that were actually flagged as metadata-changed** (the population the
decision rule is about): **25 of the 61 changed-metadata sites (41%) have now been checked —
1 severe (IT-MBo, 4%), 4 modestly scattered (DE-HoH, GF-Guy, IT-BCi, SE-Svb, 16%), 20
minor-only (80%).** This is a real, bug-fixed, absolute-magnitude-calibrated sample covering
over 40% of the flagged population: **IT-MBo is the clear severity outlier, not a preview of a
network-wide repeat — but 4 of 25 (16%) show a real, if smaller, defect, two of them
(IT-BCi, SE-Svb) in flux variables rather than precipitation, which is enough on its own to
keep the decision rule's "any site other than IT-MBo" trigger firing.**

### Download status — complete

Per CLAUDE.md's guidance for long-running scripts ("report the PID and log path... then
monitor periodically rather than blocking"), this report was drafted against a partial sample
while the download continued in the background; it has since **completed: 45/45 sites
downloaded, verified, and extracted, 0 failures** (PID `27927`, log
`logs/store_audit_stage2_download_20260920.log`, total runtime ~54 minutes — several ICOS
sites are large, long-record towers, e.g. FR-Pue alone took ~8.5 minutes). The comparison
above reflects the full, completed 50-site sample. `already_done()` (fixed during this audit
— see the note in `store_audit_stage2_download.R`) was never actually exercised on a restart
in this run, since the single continuous pass completed on its own.


---

## Stage 3 — independent re-derivation (no repository helpers)

`scripts/diagnostics/store_audit_stage3_rederivation.R`: mean annual precipitation (`P_ERA`,
`P_F`) and mean annual `NEE_VUT_REF` for 5 sites (`IT-Lav`, `AU-Ctr`, `BE-Lcr`, `US-KS2`,
`AU-DaP` — all metadata-unchanged, so any divergence found would implicate pipeline code, not
file staleness), computed directly from the raw `*_FLUXNET_FLUXMET_YY_*.csv` files using a
from-scratch base-R CSV reader (no `readr`, no `R/units.R`, no `R/utils.R`, no pipeline
script), compared against the DuckDB `annual_converted` table.

**Result: exact agreement (differences ~1e-12, floating-point noise) for both P_ERA and
NEE_VUT_REF at all 5 sites.** `table_stage3_independent_rederivation.csv`.

```
site_id    mean_P_ERA_independent  mean_P_ERA_pipeline  diff   mean_NEE_independent  mean_NEE_pipeline  diff
AU-Ctr     4401.5898                4401.5898            0      118.6833               118.6833          0
AU-DaP     1363.1802                1363.1802            0     -231.5310              -231.5310          0
BE-Lcr      728.2275                 728.2275            0     -302.8738              -302.8738          0
IT-Lav     1115.4301                1115.4301            0    -1974.1233             -1974.1233          0
US-KS2     1144.0355                1144.0355            0     -335.0337              -335.0337          0
```

**A real pitfall was found and fixed in this script itself, worth documenting because it will
recur for anyone else querying this table**: the DuckDB `annual_converted` table stores **two
rows per year-slot per site** — `dataset='ERA5'` (from the standalone `ERA5_YY` file, full
1981-2025 reanalysis record) and `dataset='FLUXMET'` (from the `FLUXMET_YY` file's own
embedded `P_ERA` column, tower-operational years only). An unfiltered `SELECT ... FROM
annual_converted WHERE site_id = ...` blends both, producing a spurious 2-10% "divergence"
(e.g. IT-Lav: independent 1115.43 vs. unfiltered-pipeline 1090.40) that has nothing to do with
pipeline correctness — it is purely a consequence of not filtering by `dataset`. Filtering to
`dataset='FLUXMET'` (matching the source this script's independent reader uses) gives exact
agreement. `era5_precip_units_v2`'s report independently found and documented this same
`WHERE dataset='...'` requirement — this is a known, load-bearing trap for this table, now
confirmed a second, independent way.

**Conclusion: the pipeline's arithmetic and unit-handling logic is verified correct by an
independent, from-scratch implementation. Nothing in Stage 3 implicates pipeline code as a
source of the IT-MBo or wider staleness problem — the problem is entirely upstream of the
pipeline, in which files sit in `data/extracted/`.**

---

## Stage 4 — manifest proposal (not implemented)

**Good news: most of the needed infrastructure already exists.** DuckDB already has a
`manifest` table (`scripts/duckdb_setup.R:58-73`, populated from `fluxnet::flux_discover_files()`)
carrying `product_id`, `download_time`, `first_year`, `last_year`, `oneflux_code_version`,
`release_version`, and `path` per file — everything needed to detect staleness **except a
content hash**. And `scripts/duckdb_update.R:21-41` already implements exactly the right
incremental-refresh logic: it re-scans `data/extracted/` fresh, `anti_join`s against the
stored manifest on `(site_id, dataset, time_resolution, first_year, last_year,
oneflux_code_version, release_version, product_id)`, and `INSERT OR REPLACE`s
(`duckdb_update.R:87-91` etc.) into the data tables keyed on `(site_id, dataset, TIMESTAMP)` —
this correctly overwrites stale rows rather than duplicating them, and does **not** require
deleting and rebuilding the 14 GB database from scratch.

**What's missing is verification, not storage.** Proposed manifest addition:

1. **Produce**: add a `sha256` column to the existing DuckDB `manifest` table, computed at
   extraction time in `scripts/02_extract.R` (the natural point — files are already being
   read off disk there) using a fast hash (`digest::digest(file = ..., algo = "sha256")` --
   already an installed, lockfile-resolved dependency in this project, confirmed during this
   audit -- or shell out to `shasum -a 256` to avoid even that). Store alongside the existing
   manifest columns.
2. **Consume**: add a check, run at the start of `01_download.R` (after `flux_listall()`,
   before deciding what to download) that compares the live manifest's `product_id` per site
   against the stored `manifest` table's `product_id` — this is exactly the `reprocessed`
   category `compare_snapshots()`/`sites_to_download()` already compute correctly; the gap is
   that nothing currently *forces* this comparison to run on a schedule independent of someone
   remembering to invoke it. Per Hard Rule 5's existing pattern (`check_pipeline_config()`
   warns but does not error on a version mismatch), a mismatch here should **warn loudly and
   list the affected sites**, matching how this project already treats a shuttle-version
   mismatch — not silently proceed.
3. A sha256-level check (comparing the manifest's stored hash against a fresh hash of the
   on-disk file) would catch **local damage** (truncation, this session's own corrupted-zip
   incidents) that a `product_id`-only check cannot, since local corruption doesn't change
   what the *archive* reports — only what's actually on disk. This is the complementary half
   of the check the `product_id` comparison above doesn't cover.

Not implemented here, per scope.

---

## Close

**Does the fixed decision rule call for a full store refresh? Yes.** 61/759 on-disk sites
(all ICOS) have a stale `product_id`/year-range relative to the live archive. Of those 61,
**25 (41%) were content-checked**, the full planned sample: 20 (80%) show only ordinary
reprocessing-scale drift, but **4 (DE-HoH, GF-Guy, IT-BCi, SE-Svb — 16%) show a real, if
smaller-than-IT-MBo's, defect** (a handful of months off by several units, not the whole
record), which on its own satisfies "any site other than IT-MBo differs... in a variable this
project uses." IT-MBo remains the clear severity outlier (the only `DIFFERS_UNIFORM` site of
50 checked), not the typical case, but the rule as fixed in advance does not require
typicality — only detection — and it detects real problems at 5 of 25 checked changed-metadata
sites (IT-MBo + DE-HoH + GF-Guy + IT-BCi + SE-Svb), not just 1. `US-HB4` (a known,
already-confirmed defect, unaffected by staleness) came back `IDENTICAL` between old and
fresh, as expected — a useful negative control confirming the classification script isn't
simply flagging every site.

**Scope and cost.** Re-downloading and re-extracting 61 ICOS sites (MM/DD/YY only, per this
project's `FLUXNET_EXTRACT_RESOLUTIONS=y m d` default) is small relative to the full 781-site
network — `gap_download_delta_20260901.log` shows 22 sites downloaded+extracted in 9.38 min,
so 61 sites is roughly 25-30 min of download time, order-of-magnitude (this audit's own
Stage 2 download of 45 sites, a comparable batch, ran for approximately 35-45 min in practice
including per-site zip verification — see PID `27927`'s log timestamps — so treat the
9.38-min figure as an optimistic floor, not a realistic estimate). **The DuckDB rebuild does
not need to be a full 14 GB rebuild-from-scratch**: `scripts/duckdb_update.R`'s existing
incremental upsert (see Stage 4) is designed for exactly this case and should absorb a refresh
of 61 sites in minutes, not hours — the Sep 1 incremental update of 22 sites took ~5.5 min for
the monthly+daily tables alone (`logs/rebuild_03b_duckdb_20260901.log`); scaling to 61 sites
and all five resolutions is still order-of-magnitude tens of minutes, not a multi-hour full
rebuild. **One operational risk to flag before running it**: 5 sites (IT-MBo, FI-Hyy, DE-Hzd,
GF-Guy, IE-Cra) currently have **two** `data/extracted/` directories each (stale MM/DD/YY +
fresh HH-only, both from today's earlier diagnostics) — `flux_discover_files()`'s behavior
when a site has two conflicting-metadata directories has not been verified in this audit, and
should be checked (or the stale directories removed) before re-running
`03b_create_database.R`, to avoid the same directory-ambiguity failure mode
`era5_share_for_coordination.R`'s `site_dir_lookup[["IT-MBo"]]` hit this morning. `04_qc.R`
and `05_units.R` are SQL-only per their own header comments and should be comparably fast.
`07_figures.R` and every diagnostic built on the DuckDB tables would need re-running after the
refresh to reflect corrected values — not costed here.

**Is any flux variable involved? Yes, and not only at the ordinary-reprocessing scale.**
**`IT-BCi` (`LE_F_MDS`) and `SE-Svb` (`H_F_MDS`) are both `DIFFERS_SCATTERED` on flux
variables**, not precipitation: IT-BCi has 4/13 differing months over the severity floor (max
abs diff 34.8 W/m2), SE-Svb 1/41 (max abs diff 10.1 W/m2) — smaller than IT-MBo's pattern but
real, scattered defects in flux variables, confirming the store issue is not scoped to
precipitation alone, as the brief for this audit anticipated. Beyond that, ordinary
`DIFFERS_MINOR`-level (non-zero but sub-severity) differences also appear in `NEE_VUT_REF` and
`GPP_NT_VUT_REF` at several sites (GF-Guy, DE-Hzd — see `table_stage2_variable_comparison.csv`),
and IE-Cra's `NEE_VUT_REF` column is absent entirely from the old file (added by the
reprocessing, not merely changed).

**Did Stage 3 find any divergence between the pipeline and an independent derivation? No** —
exact agreement once compared against the correct `dataset='FLUXMET'` subset. The pipeline's
own arithmetic is independently verified correct; every problem found in this audit and in
`it_mbo_file_check`/`it_mbo_bug_hunt`/`it_mbo_parsimony` traces to what files are sitting in
`data/extracted/`, never to how the pipeline processes them once there.
