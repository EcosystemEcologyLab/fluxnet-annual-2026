# FLUXNET Shuttle Gap Download — 2026-09-01

**Author:** Claude Code, on behalf of David J.P. Moore
**Scope:** Step 1 of 2 — download and record only. No figures, no analysis, no
change to the frozen paper snapshot or any figure/analysis script.

---

## Summary

The paper is pinned to `data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv`
(767 sites) — **untouched by this session**. This report reconciles that frozen
reference against today's live Shuttle listing and the actual contents of
`data/extracted/`, and downloads the sites found missing on disk.

| Quantity | Count |
|---|---|
| Frozen snapshot (20260624), for reference only | 767 sites |
| Sites in today's `flux_listall()` (2026-09-01) | 781 sites |
| Sites with an extracted YY FLUXMET file on disk (before this session) | 759 sites |
| **Delta downloaded this session** (in live listing, no on-disk YY file) | **22 sites** |
| — of which already known at freeze but never downloaded | 8 sites |
| — of which genuinely new to the Shuttle since 20260624 | 14 sites |
| Sites removed from the Shuttle since 20260624 (in frozen, absent from live) | 0 sites |
| Sites on disk whose product version differs from the current live listing | 5 sites |

New snapshot written: `data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv`
(781 rows, full 18-column Shuttle schema).

---

## Step 1 — Current listing

`flux_listall()` called 2026-09-01, returned **781 sites** across the expected
hubs (AmeriFlux, ICOS, TERN — none missing). Persisted immediately via
`write_snapshot()` to:

```
data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv
```

---

## Step 2 — On-disk inventory

`data/extracted/` contains 760 top-level entries: 759 Shuttle-product directories
(`NETWORK_SITE_FLUXNET_YYYY-YYYY_vX.X_rN` naming) plus one unrelated directory,
`data/extracted/fluxnet2015/`, which holds a FLUXNET2015 comparison dataset used
only for validation analyses per CLAUDE.md Hard Rule #1 — **not Shuttle data**,
and excluded from the inventory scan below (its file-naming convention is
incompatible with `flux_discover_files()` and made the scan fail with a
"too-short" parse error until excluded; noted here as a housekeeping oddity, not
acted on further).

`flux_discover_files()` was run against the 759 legitimate Shuttle directories
(via a read-only file-level symlink farm in the scratch dir — nothing in
`data/extracted/` itself was moved or modified) and joined against today's live
listing. Result: **759 unique sites have an on-disk YY FLUXMET file**, matching
the 22-site gap reported below (781 live − 759 on disk = 22).

---

## Step 3 — Delta, removed sites, and version differences

### Delta to download (22 sites)

Sites present in today's `flux_listall()` with no extracted YY file on disk.
`in_frozen` = TRUE means the site was already listed in the 20260624 frozen
snapshot but was never downloaded (a pre-existing gap, not a new addition to
the Shuttle); FALSE means it is genuinely new to the Shuttle since the freeze.

| site_id | network (source) | hub | IGBP | first_year | last_year | product | in_frozen_20260624 |
|---|---|---|---|---|---|---|---|
| DK-Eng | CarboEuropeIP;European Fluxes Database | ICOS | GRA | 2005 | 2008 | EUF_DK-Eng_FLUXNET_2005-2008_v1.3_r1 | TRUE |
| DK-Fou | CarboEuropeIP;European Fluxes Database | ICOS | CRO | 2005 | 2005 | EUF_DK-Fou_FLUXNET_2005-2005_v1.3_r1 | TRUE |
| ES-LgS | European Fluxes Database;GHG-Europe | ICOS | OSH | 2007 | 2009 | EUF_ES-LgS_FLUXNET_2007-2009_v1.3_r1 | FALSE |
| ES-Ln2 | European Fluxes Database;GHG-Europe | ICOS | OSH | 2009 | 2009 | EUF_ES-Ln2_FLUXNET_2009-2009_v1.3_r1 | FALSE |
| ES-Pdu | European Fluxes Database;GHG-Europe;InGOS | ICOS | WET | 2014 | 2017 | EUF_ES-Pdu_FLUXNET_2014-2017_v1.3_r1 | TRUE |
| FI-Si2 | European Fluxes Database | ICOS | WET | 2012 | 2016 | EUF_FI-Si2_FLUXNET_2012-2016_v1.3_r1 | FALSE |
| HK-MPM | Unaffiliated | ICOS | EBF | 2016 | 2018 | FLX_HK-MPM_FLUXNET_2016-2018_v1.3_r1 | FALSE |
| IT-Cpz | CarboEuroFlux;CarboEuropeIP;CarboItaly;EuroFlux;European Fluxes Database;GHG-Europe | ICOS | EBF | 2001 | 2008 | EUF_IT-Cpz_FLUXNET_2001-2008_v1.3_r1 | FALSE |
| IT-MtM | European Fluxes Database | ICOS | GRA | 2014 | 2019 | EUF_IT-MtM_FLUXNET_2014-2019_v1.3_r1 | TRUE |
| IT-MtP | European Fluxes Database | ICOS | GRA | 2014 | 2019 | EUF_IT-MtP_FLUXNET_2014-2019_v1.3_r1 | FALSE |
| IT-PT1 | CarboEuroFlux;CarboEuropeIP;European Fluxes Database | ICOS | DBF | 2002 | 2004 | EUF_IT-PT1_FLUXNET_2002-2004_v1.3_r1 | TRUE |
| IT-Ro1 | CarboEuroFlux;CarboEuropeIP;CarboItaly;European Fluxes Database;GHG-Europe;IMECC | ICOS | DBF | 2000 | 2008 | EUF_IT-Ro1_FLUXNET_2000-2008_v1.3_r1 | FALSE |
| IT-Ro2 | CarboEuroFlux;CarboEuropeIP;CarboItaly;European Fluxes Database;GHG-Europe | ICOS | DBF | 2002 | 2012 | EUF_IT-Ro2_FLUXNET_2002-2012_v1.3_r2 | FALSE |
| JP-Nkm | AsiaFlux;JapanFlux | ICOS | ENF | 2018 | 2024 | JPF_JP-Nkm_FLUXNET_2018-2024_v1.3_r1 | TRUE |
| JP-Tgf | AsiaFlux | ICOS | GRA | 2002 | 2004 | JPF_JP-Tgf_FLUXNET_2002-2004_v1.3_r1 | TRUE |
| SJ-Adv | European Fluxes Database;PAGE21 | ICOS | WET | 2012 | 2014 | EUF_SJ-Adv_FLUXNET_2012-2014_v1.3_r1 | TRUE |
| US-KLS | AmeriFlux | AmeriFlux | GRA | 2012 | 2019 | AMF_US-KLS_FLUXNET_2012-2019_v1.3_r1 | FALSE |
| US-LS2 | AmeriFlux | AmeriFlux | SAV | 2003 | 2007 | AMF_US-LS2_FLUXNET_2003-2007_v1.3_r1 | FALSE |
| US-ZF1 | AmeriFlux | AmeriFlux | CRO | 2024 | 2025 | AMF_US-ZF1_FLUXNET_2024-2025_v1.3_r1 | FALSE |
| US-xHA | AmeriFlux;NEON;Phenocam | AmeriFlux | DBF | 2019 | 2024 | AMF_US-xHA_FLUXNET_2019-2024_v1.3_r1 | FALSE |
| US-xKA | AmeriFlux;NEON;Phenocam | AmeriFlux | GRA | 2019 | 2024 | AMF_US-xKA_FLUXNET_2019-2024_v1.3_r1 | FALSE |
| US-xTA | AmeriFlux;NEON;Phenocam | AmeriFlux | ENF | 2019 | 2024 | AMF_US-xTA_FLUXNET_2019-2024_v1.3_r1 | FALSE |

Note: site ID prefixes above (`US-`, `DE-`, `JP-`, …) are country codes, not
hub identifiers — the `hub` and `network` columns above come from the
manifest's `data_hub`/`network` fields, not inferred from the prefix, per
CLAUDE.md Hard Rule #2.

### Removed since 20260624

**None.** Every one of the 767 sites in the frozen snapshot is still present
in today's live listing.

### Product version differences (on-disk file vs. today's live listing)

Restricted to sites that already have an on-disk YY FLUXMET file (i.e.
excluded from the delta above). Comparing the version tag embedded in the
on-disk filename against the version tag `flux_listall()` reports today for
that site:

| site_id | on-disk version | current live version | note |
|---|---|---|---|
| DE-Hai | v1.3_r2 | v1.3_r1 | live listing shows an *older* release than what's on disk |
| DE-Hte | v1.3_r1 | v1.3_r2 | live listing shows a newer release |
| FR-CLt | v1.3_r2 | v1.3_r1 | live listing shows an *older* release than what's on disk |
| FR-EM2 | v1.3_r2 | v1.3_r1 | live listing shows an *older* release than what's on disk |
| JP-Api | v1.3_r1 | v1.3_r2 | live listing shows a newer release |

Three of the five (DE-Hai, FR-CLt, FR-EM2) show the live Shuttle listing at a
**lower** `_r` release number than what is currently on disk, which is
unusual for a monotonic release counter and worth flagging to the Shuttle
team rather than assuming it is a simple reprocessing bump. Not acted on in
this session.

Separately, one on-disk site — **IT-SR2** — changed `data_hub`/`product_source_network`
between the frozen snapshot (`FLX`, Unaffiliated/FLUXNET-CH4 hub) and today's
live listing (`ICOS`). Because `flux_discover_files()` joins on
`(product_source_network, site_id)`, this hub change means the on-disk IT-SR2
file no longer joins to a current metadata row at all — its version could not
be compared automatically and is called out here rather than silently dropped.
Not acted on in this session (IT-SR2 already has an on-disk YY file, so it is
not part of the download delta).

Additionally, 42 of the 767 frozen-snapshot sites show `last_year` advancing
from 2024 to 2025 in today's live listing (routine extended coverage, not a
version/reprocessing change) — not tabulated individually here since it
reflects normal data extension rather than a reprocessing event, but visible
in the full comparison CSV retained with this session's working files if
needed.

---

## Step 4 — Download

Connectivity and AmeriFlux credentials confirmed with a single-site test
(`US-KLS`, 92 MB, valid zip) before the bulk run.

`flux_download()` was called with `file_list_df` subset to the 22 delta site
rows (its default `overwrite = FALSE` only skips zips already present in
`data/raw/`, which is otherwise emptied by the pipeline's zip-cleanup step —
so the subset, not the built-in skip logic, is what prevented re-fetching the
other 759 sites). Downloaded zips were extracted into `data/extracted/` with
`flux_extract(site_ids = <delta>, resolutions = "y m d")`, matching
`02_extract.R` and the `FLUXNET_EXTRACT_RESOLUTIONS` pipeline default.

Launched in background: PID 83225, log `logs/gap_download_delta_20260901.log`.

**21 of 22 zips downloaded and extracted cleanly on the first pass.**
**IT-Cpz failed initially**: the downloaded zip (32.9 MB) was truncated —
`unzip -l` reported "End-of-central-directory signature not found" — and
`flux_extract()` silently skipped it while the other 21 sites extracted
successfully (logged, not inferred: see `logs/gap_download_delta_20260901.log`).
Retried with `flux_download(..., overwrite = TRUE)` for IT-Cpz alone
(log: `logs/gap_retry_itcpz_20260901.log`); the re-downloaded zip was 85.0 MB
and extracted without error. All 22 sites are now present in `data/extracted/`.

A separate script bug (not a data issue) surfaced during the first run: the
post-download verification step called `flux_discover_files("data/extracted")`
directly, which hit the same `data/extracted/fluxnet2015/` naming-convention
error described in Step 2 and halted before zip cleanup ran. Re-run against
the filtered (fluxnet2015-excluded) file list — see Step 5 — completed cleanly.

After verification (below) confirmed every zip's extraction, all 22
`data/raw/` zips (929 MB total) were deleted per `FLUXNET_DELETE_ZIPS=TRUE`,
matching `02_extract.R`'s cleanup convention. `data/raw/` is empty again.

---

## Step 5 — Verification

Each of the 22 delta sites' YY FLUXMET CSV was located via `flux_discover_files()`
(run again after extraction, on the fluxnet2015-excluded file list) and checked for:
existence, parseability (`readr::read_csv`), and presence of a core variable set
(`TIMESTAMP`, `NEE_VUT_REF`, `GPP_NT_VUT_REF`, `RECO_NT_VUT_REF`, `LE_F_MDS`, `TA_F`, `P_F`).

| site_id | status | detail |
|---|---|---|
| DK-Eng | OK | 4 rows, all expected variables present |
| DK-Fou | OK | 1 row, all expected variables present |
| ES-LgS | OK | 3 rows, all expected variables present |
| ES-Ln2 | OK | 1 row, all expected variables present |
| ES-Pdu | OK | 4 rows, all expected variables present |
| FI-Si2 | WARN | 5 rows; no `NEE_VUT_REF` column — see note below |
| HK-MPM | OK | 3 rows, all expected variables present |
| IT-Cpz | OK | 8 rows, all expected variables present (after retry) |
| IT-MtM | OK | 6 rows, all expected variables present |
| IT-MtP | OK | 6 rows, all expected variables present |
| IT-PT1 | OK | 3 rows, all expected variables present |
| IT-Ro1 | OK | 9 rows, all expected variables present |
| IT-Ro2 | OK | 11 rows, all expected variables present |
| JP-Nkm | WARN | 7 rows; no `NEE_VUT_REF` column — see note below |
| JP-Tgf | OK | 3 rows, all expected variables present |
| SJ-Adv | OK | 3 rows, all expected variables present |
| US-KLS | OK | 8 rows, all expected variables present |
| US-LS2 | OK | 5 rows, all expected variables present |
| US-ZF1 | OK | 2 rows, all expected variables present |
| US-xHA | OK | 6 rows, all expected variables present |
| US-xKA | OK | 6 rows, all expected variables present |
| US-xTA | OK | 6 rows, all expected variables present |

**Result: 20 OK, 2 WARN, 0 FAIL.** No site failed to download, extract, or
parse.

**FI-Si2 and JP-Nkm ("WARN")** are not failures: both files have no
`NEE_VUT_REF` / `NEE_VUT_REF_QC` column at all, only `NEE_CUT_*` columns —
i.e. they are CUT-only sites, the documented fallback case in CLAUDE.md's
QC Flag Reference (§"Default QC thresholds": *"CUT-only sites (where
`NEE_VUT_REF_QC` is entirely NA across all of that site's rows): gated on
`NEE_CUT_REF_QC`"*). `04_qc.R`'s per-site VUT/CUT fallback already handles
this pattern; flagged here only so it isn't mistaken for a download problem.

---

## Confirmation: frozen snapshot and figure scripts unchanged

`git status` before and after this session's work is recorded below.
`data/extracted/` and `data/raw/` are gitignored, so the only committed
changes from this session are: the new snapshot CSV, this report, and the
`SESSION_LOG.md` entry.

**Before this session** (`git status` at session start): `outputs/session_info.txt`
and `renv/activate.R` were already modified locally (pre-existing, unrelated to
this task — not touched here), plus a large number of pre-existing untracked
files from prior sessions (logs, snapshot CSVs, figure-candidate outputs, etc.).

**After this session**: `git status --short data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv`
returns empty (frozen snapshot byte-for-byte unchanged) and
`git status --short scripts/ R/ review/` shows no tracked-file changes
(only a pre-existing untracked `review/figures/maps_point/` directory from an
earlier session). `outputs/session_info.txt` and `renv/activate.R` remain in
their pre-session modified state — untouched and left for a separate commit
decision, per CLAUDE.md's multi-machine sync guidance. The new files added by
this session are exactly: `data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv`,
this report, and the `SESSION_LOG.md` entry — the three staged and committed
below.
