# Shuttle listing check — 2026-11-24

Diagnostic only. Prompt: `logs/prompts/2026-11-24_shuttle_listing_check.md`. Script:
`scripts/diagnostics/shuttle_listing_check_2026-11-24.R`. Run log:
`logs/shuttle_listing_check_2026-11-24.log`.

No data was downloaded. The snapshot of record
(`data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv`), `FLUXNET_SNAPSHOT_MODE`, and
the pipeline config were not changed.

## 1. Pipeline config and live listing

- `check_pipeline_config()` ran with no warnings. Installed `fluxnet-shuttle` version: **0.3.7**
  (matches `FLUXNET_SHUTTLE_VERSION=0.3.7` in `.env`; `.env` pins `0.3.7`, not CLAUDE.md's
  documented default `0.3.7.post0+dirty` — see repo convention in `R/pipeline_config.R`).
- `flux_listall()` was called once, live, at 2026-11-24 12:58:33–12:58:45 (script timestamps).
- Hub-drop guard (`scripts/01_download.R` lines 21–33, `data_hub` field), rows returned per hub:

  | `data_hub` | rows |
  |---|---|
  | AmeriFlux | 381 |
  | ICOS | 352 |
  | TERN | 51 |

  All three expected hubs present, all with >0 rows. Guard passed; no stop, no retry.

## Step 2 — glob/newest-file check and save location

Grep for code that globs or picks the newest file in `data/snapshots/` (full results in the
session's tool output; commands: `grep -rn "data/snapshots" ...` and
`grep -rn "list.files.*snapshots\|dir(.*snapshots\|find_previous_snapshot\|find_latest_snapshot\|most recent snapshot" --include="*.R" .`):

- `R/sync.R::find_previous_snapshot()` — `fs::dir_ls(snapshots_dir, regexp = "fluxnet_shuttle_snapshot_\\d{8}T\\d{6}\\.csv$")`, used by `scripts/01_download.R` and `scripts/diagnostics/store_refresh_stage3_download.R`.
- `scripts/generate_fluxnet_citations.R::find_latest_snapshot()` — `list.files(snapshot_dir, pattern = "fluxnet_shuttle_snapshot.*\\.csv$", full.names = TRUE)`.
- Ad hoc `list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"), ...)` calls (same pattern style) in: `R/historical_datasets.R`, `legacy/07_figures_rds.R`, `scripts/generate_whittaker_alt_fig02.R`, `scripts/generate_whittaker_overlays.R`, `scripts/generate_maps.R`, `scripts/00_diagnostics.R`, `scripts/step2_extract_aridity.R`, `scripts/generate_historical_comparison_figures.R`, `scripts/00_candidate_figures.R`, `scripts/compute_site_record_length.R`, `scripts/generate_whittaker.R`, `scripts/legacy/generate_worldclim_figures.R`, `scripts/legacy/generate_whittaker_figures.R`, `scripts/generate_env_response_era5.R`.
- `scripts/diagnostics/store_refresh_stage5_reconcile.R` — `sort(list.files("data/snapshots", pattern = "^fluxnet_shuttle_snapshot_20260920.*\\.csv$", full.names = TRUE))` (date-pinned, not a "newest" glob).

Every one of these calls `fs::dir_ls()`/`list.files()` against `data/snapshots` (or
`file.path(FLUXNET_DATA_ROOT, "snapshots")`) with no `recurse`/`recursive = TRUE` argument, so
each defaults to non-recursive and only sees files directly inside `data/snapshots/`, not inside
a subfolder. None of them reach `data/snapshots/check_only/`.

Today's listing was saved to:

```
data/snapshots/check_only/fluxnet_shuttle_snapshot_20261124T125845.csv
```

## 3. Counts from today's listing

| Metric | Value |
|---|---|
| Rows | 784 |
| Distinct `site_id` | 784 |
| Distinct `product_id` | 784 |
| `site_id` values listed more than once | 0 |

Sites by hub (`product_source_network`):

| `product_source_network` | n sites |
|---|---|
| AMF | 381 |
| CNF | 32 |
| EUF | 140 |
| FLX | 18 |
| ICOS | 82 |
| JPF | 54 |
| KOF | 21 |
| SAEON | 5 |
| TERN | 51 |
| **Total** | **784** |

No `site_id` is listed more than once today, so there is no "listed more than once" table or
reason to give.

## 4. Comparison with the 20 September 2026 snapshot

Matched on `site_id` (the FLUXNET site ID column in both files), the same key
`find_previous_snapshot()`/`compare_snapshots()` (`R/sync.R`) use for snapshot-to-snapshot
comparison.

| Metric | 2026-09-20 snapshot | 2026-11-24 live listing |
|---|---|---|
| Rows | 781 | 784 |
| Distinct `site_id` | 781 | 784 |
| Distinct `product_id` | 781 | 784 |
| `site_id` listed more than once | 0 | 0 |

Sites by hub (`product_source_network`), side by side:

| `product_source_network` | 2026-09-20 | 2026-11-24 |
|---|---|---|
| AMF | 381 | 381 |
| CNF | 32 | 32 |
| EUF | 138 | 140 |
| FLX | 18 | 18 |
| ICOS | 80 | 82 |
| JPF | 54 | 54 |
| KOF | 21 | 21 |
| SAEON | 5 | 5 |
| TERN | 52 | 51 |
| **Total** | **781** | **784** |

### Sites added (in 2026-11-24, not in 2026-09-20) — 4

| `site_id` | `data_hub` | `product_source_network` |
|---|---|---|
| FI-Lom | ICOS | ICOS |
| SE-Lnn | ICOS | EUF |
| SE-Myc | ICOS | ICOS |
| SJ-Blv | ICOS | EUF |

### Sites removed (in 2026-09-20, not in 2026-11-24) — 1

| `site_id` | `data_hub` | `product_source_network` |
|---|---|---|
| AU-GWW | TERN | TERN |

### Sites with a changed `product_id` or `oneflux_code_version` — 58

All 58 are sites present in both snapshots with an unchanged `site_id`; `oneflux_code_version`
is `v1.3` on both sides for every one of the 58 — the only field that changed for any of them is
`product_id`. Full table (also written to
`scripts/diagnostics/shuttle_listing_check_2026-11-24_product_changes.csv`):

| `site_id` | `product_id` (old) | `oneflux_code_version` (old) | `product_id` (new) | `oneflux_code_version` (new) |
|---|---|---|---|---|
| AT-Mmg | SR8Y7XJE2CuwVkPh9WO7iXKF | v1.3 | vHyGo8dhsRM5RdQFzYEkutaB | v1.3 |
| BE-Bra | LFEir6YGDlkpjoTnCuB1qWeS | v1.3 | jqFMpuyScNG1HdiOILFit4Dh | v1.3 |
| BE-Maa | DwEiFNFPCZypbHKn5zK7Phsl | v1.3 | msdWDk98FR3UQUqS9OM2N32V | v1.3 |
| BE-Vie | mYZAS5TMuYruvEj80RAUa5OO | v1.3 | PZh-nxtZjfrymcZJu9U4uLBJ | v1.3 |
| CH-Dav | Pr9SZHBdwu6YbWLoOGhzjSJQ | v1.3 | CXf4ux17dTFu2JrV9Tu_bSk_ | v1.3 |
| CZ-BK1 | HH11awh469XrSeL0fO6WxYA4 | v1.3 | jFai8tDYa4LFIrBMKJ_Kx3I1 | v1.3 |
| CZ-Lnz | qtH0gyWH-yg0lP-HmC2lXv24 | v1.3 | nyTyuAYYMFqBPZDS9y0543dx | v1.3 |
| DE-Akm | P8ryFkG8dw0RkoDJmqv06lnk | v1.3 | IMzfKbRgPQZDJNaSakSqdbQN | v1.3 |
| DE-Etn | timq13anOgu_cC9YrhwCaa1K | v1.3 | b0tz6VWoOuy5q8fPPS-ivEKc | v1.3 |
| DE-Geb | jFkfMT_3ZahqoODz-R82VeUG | v1.3 | WoO8JdfzAUBObbdbra782Pap | v1.3 |
| DE-HoH | tfW10G3Do0hp0cimfI4XrgSd | v1.3 | 2gBr7fYbxeL6oMsRhXYEBWyx | v1.3 |
| DE-Kli | ViYMPdxbY25optpqiQ7OVgXN | v1.3 | mlrCmtpUu7ZzEbkvYEK1DpMg | v1.3 |
| DE-Lnf | Iy6uj-QptZzaR9BFnqiv4hF6 | v1.3 | TjZoWxONuNgLAOvNM_ZOZTxm | v1.3 |
| DE-Obe | mk-YBqXT0YwbluI9V726yD2d | v1.3 | gV9mm29h9oS7GXxosxQAmbsv | v1.3 |
| DE-Rns | ZOfRvdcakFBnrRQoiDMUEyds | v1.3 | Y3MXtNJifcBRKyaDHsMZ-AaD | v1.3 |
| DE-RuC | MKtmPmXNZnCQc-p38RBQGATH | v1.3 | 6wyhw6OzYJx9CTweBWw2nqVG | v1.3 |
| DE-RuW | mlrCmtpUu7ZzEbkvYEK1DpMg | v1.3 | MF9WjbuAZ50-LUQgTsloGxSD | v1.3 |
| DE-Tha | 3LQRnWuwu9ngPJQCQu2r2DTy | v1.3 | uRbHIsWzYaaZ1ry37NruE8_J | v1.3 |
| EE-Pts | j8jcyOzfXuBoZFKfMcbX5HQb | v1.3 | tRcwY_kTpM1ZZS6b9oRWAvI9 | v1.3 |
| EE-Rng | bokdUcMa7H-mSuOKeMR3mYiK | v1.3 | pzhWzdUB9mug5_LG8O59e44w | v1.3 |
| EE-Stg | C4Y455t1rGorFhBAkA5CiOXw | v1.3 | sFfCVhQN_VAihmQaOYe60Kv9 | v1.3 |
| ES-LM1 | 9c9e--DRFTTyhtl6c0lcA3O6 | v1.3 | F7-nBn7xl13T_i2qkSJvi2uV | v1.3 |
| ES-LM2 | CgWe1iCQaO3oYYxgSMEe1BSq | v1.3 | X7T1q_aY6Igw6FssAiIUCblx | v1.3 |
| ES-Pzb | 7xwSGufRWY7Duyrf11N282pK | v1.3 | MCo93azRjtwVI6d9esdIeMck | v1.3 |
| FI-Hyy | oIJ9cFYf8Q0e9nfMRRS-90vM | v1.3 | ZcRUMOt7xGz-h-IHMeooad6z | v1.3 |
| FI-Jok | 8PzFwQMtdQ_ly0Tahg8oE1Sz | v1.3 | cgiNAijXEPsdarV0jDSFhEnK | v1.3 |
| FI-Sii | mkbfyrZKqg3Djz0tW1rRap1c | v1.3 | yWlraPr2iQkRA0WKxpHF66uo | v1.3 |
| FI-Sod | TNkSnk2tWf7rkMpSQ2Dtf8FI | v1.3 | WKoKt_ZroOMtbWFLV05N5VPT | v1.3 |
| FR-Bil | OHrM-TP6a33pLElt40MWm6rD | v1.3 | 7T6UyUOXgB0j4LrWbPhTMu2P | v1.3 |
| FR-FBn | dw5JqZoWiNVw0JQS-QXykPY6 | v1.3 | XBQ9Wp2zgNpR1XryvQv_76kg | v1.3 |
| FR-Fon | vgMluaOupOEt-hzl3RWXI99X | v1.3 | BDD6tNB_kw3VPrSdMRYYvY94 | v1.3 |
| FR-Gri | gsAnVUkxq-6VBt2qu1hewpGD | v1.3 | XSI0ETkKyVVydpJuL7ro3_rQ | v1.3 |
| FR-Hes | Di7uhs3hUdH1SvS0f0Wv5vjH | v1.3 | PiV4aacfudwGs03sWmiN7Inf | v1.3 |
| FR-Lqu | FT09cB-tv2MKDSSw1R_JXBJx | v1.3 | FYx805VSU6_eRFBtYYMFqfhv | v1.3 |
| FR-Lus | S_NN3abNF1zutV6hnPOuCupl | v1.3 | sR44Fqgq6Ps64sv2PGGW-CCI | v1.3 |
| FR-Nzn | NySpPG4qsETA19sRa2e_ONBG | v1.3 | RQ_jc_Wrew30CVD3ZzWJjS3b | v1.3 |
| FR-Pue | HaC9deo1SSLTZxUtYYJcdLAd | v1.3 | zX4eolqNe8xbcMScyT2IQ9XV | v1.3 |
| GL-ZaF | Lf3j25yaK_SzajN-sxvn-Mfj | v1.3 | g7DBBlgwdTCe3nl9u9jmFnfx | v1.3 |
| IT-BCi | 2LNryh4qBQqqtGj-L8ONRyxA | v1.3 | QcwmmUDyX5MXPIYAuDq5vepJ | v1.3 |
| IT-Cp2 | Oz2rJQyIT8nYjOwvl4nCxlsl | v1.3 | Xb6gBcoSkdkQMor4lpYhvGey | v1.3 |
| IT-MBo | enS2fTzGG_9PS5-51hqet8iH | v1.3 | PPBHqoo4w7tey0vQFJMxOg4g | v1.3 |
| IT-Ren | pNTPZfSPE9VmxdZ-g9ijYCSc | v1.3 | 525_tM0uR8c7nAB-2FjGiYH0 | v1.3 |
| NL-Loo | fnq1hhvNLYGhtdiC16YIM0YQ | v1.3 | YvbuYlKu2dPVWeO1x7ZVIB62 | v1.3 |
| NO-Hur | CDVyAitBD2Nv07IGEndbf0mk | v1.3 | QaTyVBkjr4Xn0AzPsq3LdYHj | v1.3 |
| RU-Fy2 | Vh2pWjfw5HPNZJDA6XfQSvcP | v1.3 | -FEeYNcv7UkMTJ4l72w8CLL9 | v1.3 |
| RU-Fyo | GW1i5FQInU-IW_0iisGVDXW3 | v1.3 | WDPOlLyjxJ7H43yO9QgAoOfj | v1.3 |
| SE-Deg | IiyJGdmPkDhdItxg1_8lgLFp | v1.3 | KEU6reqLfaC-xwEaOooI-OdJ | v1.3 |
| SE-Htm | 8hMY0lY7kERIfokXkEpev-03 | v1.3 | ijcFWAyxq647kIP7fgVh7KkC | v1.3 |
| SE-Nor | TQJ13O19n72qJ3MoGHnXCdRB | v1.3 | f6wXlakGyjFukPbYht4gE6fD | v1.3 |
| SE-Sto | gROMoDY5jMLhMTRejjKXtC-s | v1.3 | GQrDp6EHJ5tDnk1lrE_74knW | v1.3 |
| SE-Svb | 6APPPkAwbCF69Dx4gIGfRo8j | v1.3 | aOE0EYzWDWCQYR2pK8kcNORZ | v1.3 |
| UK-Ags | 84y7MrbQ3ZSBKr4Kv-0_fdiY | v1.3 | 1ZUP9SKUkeaLUfCFH8xeYoi4 | v1.3 |
| UK-BnB | QBlY2INal0mc2mRsgN9Wr_CV | v1.3 | JGdYqMj6WfcCoQJZQbTGxt9n | v1.3 |
| UK-CCr | LrO8fItItMas7E62Hm_A032I | v1.3 | B7iOKKJXFrHDoZf6w2ZLth_y | v1.3 |
| UK-GaB | 3ur3ylHM5TMBuyVRXci8ohT_ | v1.3 | _sXbYddwkHiI1X9uPGVzmewo | v1.3 |
| UK-Gnn | rr5GFzsDarjbHdARWXryBopk | v1.3 | sYEZMyw8YWYYbIGSrtbzTys1 | v1.3 |
| UK-Gwr | kdw13mqVIM_fuCSUoFjwLTBX | v1.3 | Zwvqtl-S3Cv2vQ969t-syBkz | v1.3 |
| UK-HpF | 9XNveiBJ_2A4uYC0hHFDy3RS | v1.3 | RGCl92O_6QSyV-KQgiCPlsXv | v1.3 |

No site was excluded from this comparison for being duplicated within either snapshot (both have
0 duplicate `site_id` values, confirmed in step 3 and the matching left-hand column of the step-4
table above).

## Anything unexpected

- `.env` sets `FLUXNET_SHUTTLE_VERSION=0.3.7`; CLAUDE.md's documented default in
  `R/pipeline_config.R` is `0.3.7.post0+dirty`. This is pre-existing (see memory:
  "fluxnet-shuttle pinned to 0.3.7 tag") and not something this run changed — noted here only
  because `check_pipeline_config()` compares against whichever of the two is set.
- All 58 `product_id`/`oneflux_code_version` changes have `oneflux_code_version` unchanged
  (`v1.3` → `v1.3`); only `product_id` changed, for every one of the 58.
- The 4 added sites and the 1 removed site are all under `data_hub = "ICOS"` (added) or
  `data_hub = "TERN"` (removed) — none under `data_hub = "AmeriFlux"`.
- No duplicate `site_id` values in either snapshot.

Supporting CSVs written alongside this report in `scripts/diagnostics/`:
`shuttle_listing_check_2026-11-24_hub_counts.csv` (data_hub), `..._sites_by_hub.csv`
(product_source_network), `..._sites_added.csv`, `..._sites_removed.csv`,
`..._product_changes.csv`. (No `..._duplicate_sites.csv` was written — there were no duplicates
to report.) The live listing itself is at
`data/snapshots/check_only/fluxnet_shuttle_snapshot_20261124T125845.csv`.
