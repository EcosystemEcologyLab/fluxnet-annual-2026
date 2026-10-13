# TowerTango — card game built on site information

**This is not part of the FLUXNET Annual Paper 2026.** It is a side project: a card game where
each card is one FLUXNET Shuttle site, with gameplay-relevant stats (carbon sink strength,
evapotranspiration, tower height, climate, a short site description, etc.) drawn from the
network's own data. Requested by David Moore, 2026-10-12.

Everything for this project — code, docs, and the card tables — lives under this directory
(`TowerTango/`). **Nothing in this project writes outside `TowerTango/`, except
`SESSION_LOG.md` at the repo root.** `data/snapshots/`, the local flux store
(`data/duckdb/fluxnet.duckdb`), and the BADM files (`data/extracted/*/*_BIF_*.csv`) are read
**read-only** — this project never downloads, re-extracts, or modifies any of them, and never
calls `R/utils.R::write_output_metadata()` (which always appends to the repo-root
`outputs/session_info.txt`) for exactly that reason; it uses its own local `.meta.json` writer
instead (see `code/update_cards.R`).

## Layout

- `code/update_cards.R` — "Update Cards". The one script in this project. Re-runnable whenever
  a new Shuttle snapshot is downloaded (see below); regenerates the card table and its report
  from whatever is the latest snapshot and local flux store content at run time.
- `docs/update_cards_report.md` — coverage, SITE_DESC word-count distribution, tower-height and
  canopy-height spot-checks, and an out-of-range value listing, from the most recent run.
- `tables/towertango_cards_<snapshot timestamp>.csv` (+ `.meta.json`) — one row per site, the
  full snapshot plus the card columns. Re-running the script against a newer snapshot writes a
  new, separately-timestamped file; old ones are not deleted or overwritten.

## Snapshot selection

`update_cards.R` always uses whatever Shuttle snapshot is already on disk under
`data/snapshots/` — it never downloads one. It selects the *latest* snapshot by parsing the
`YYYYMMDDTHHMMSS` timestamp embedded in each `fluxnet_shuttle_snapshot_*.csv` filename and
taking the maximum of that parsed timestamp — **not** by filesystem modification time or
directory-listing order ("newest file in the folder"), which is a real, previously-hit failure
mode in this repository: a prior figure script once picked up an ad hoc 775-site development
snapshot that happened to look newest by that naive method (see
`docs/figure_rebuild_781_20260901.md`, Step 1 caveat). The file actually used is printed at the
top of every run. See `docs/decisions_pending.md`, "Latest snapshot is the default for new
analyses and figures (2026-10-12)".

## Card columns

Appended, in order, after every column of the snapshot CSV unchanged:

| Column | Source |
|---|---|
| `card_site_name`, `card_regional_network`, `card_site_id` | Snapshot `site_name`, `network`, `site_id`, verbatim |
| `card_year_established` | First year with data, via `compute_site_year_presence()` (`R/utils.R`) |
| `card_sink_strength_nep_gC_m2_yr`, `card_nep_n_years` | Median usable annual NEP (= -NEE), via `compute_site_annual_fluxes()` (`R/site_annual_fluxes.R`) — VUT with CUT fallback, the paper's QC rule |
| `card_et_mm_yr`, `card_et_mm_d`, `card_et_n_years` | Median usable annual ET from LE, same conversion as the paper's `flux_medians_by_igbp_et` output (`compute_site_annual_fluxes()`'s own `fluxnet_convert_units()` call); mm/d = mm/yr ÷ 365.25 |
| `card_tower_height_m`, `card_tower_height_date`, `card_tower_height_variable`, `card_instrument_model`, `card_tower_height_source` | BADM EC instrument measurement height (not canopy height) — see "Tower height" below |
| `card_canopy_height_max_m`, `card_canopy_height_date`, `card_canopy_height_n_records` | BADM canopy height (`HEIGHTC`, distinct from `card_tower_height_m`): the maximum `HEIGHTC` value across ALL of a site's dated `GRP_HEIGHTC`/`HEIGHTC` records (plus JP-Shn's own non-standard `VARIABLE_GROUP` value, a leaked Windows file path — see the 2026-10-08 BADM search report), used exactly as recorded with no statistic filter, the raw date string of that record, and the total number of records found (numeric or not) — see "Canopy height" below |
| `card_height_check` | Blank, or one or more of: canopy height above 60 m; canopy height of 0 m; canopy height greater than `card_tower_height_m`; instrument height changed over the record (with the range) — flags a site for a by-hand check, never excludes or alters a value — see "Height flags" below |
| `card_map_mm_yr`, `card_mat_degC` | WorldClim 2.1 bio12 (MAP), bio1 (MAT), from `data/snapshots/site_worldclim.csv`, with a local-raster fallback for any site missing from it (never downloaded) |
| `card_site_description`, `card_site_description_words` | BADM `SITE_DESC`, same sources as the 2026-10-08 BADM search (`data/extracted/*/*_BIF_*.csv`, resolved one file per site via `flux_discover_files()`); several entries joined with `" | "` in ascending `GROUP_ID` ("BADM order") |

**Tower height.** Read from the structured `VAR_INFO_HEIGHT` field (`GRP_VAR_INFO` group) in
each site's YY-resolution BIFVARINFO file (`data/extracted/*/*_FLUXNET_BIFVARINFO_YY_*.csv`,
resolved one file per site via `flux_discover_files()`, same mechanism as the BIF file) — not
the BIF file itself, which has no structured EC-instrument-height field. For `NEE_VUT_REF`, else
`NEE_CUT_REF`, else `USTAR`, taking the group with the latest `VAR_INFO_DATE` (a site's
instrument height can change over its record — `card_tower_height_date` records which date won,
`card_tower_height_variable` which of the three variables was used, and `card_instrument_model`
the corresponding `VAR_INFO_MODEL`). `card_tower_height_source` is always `"VAR_INFO_HEIGHT"`.
This replaces an earlier (2026-10-12) `FLUX_MEASUREMENTS_COMMENT` free-text regex guess, whose
coverage was effectively zero. An initial 2026-10-13 investigation wrongly concluded
`VAR_INFO_HEIGHT` does not exist at YY resolution at all — a false negative caused by this
shell's wrapped `grep` silently matching nothing; see `SESSION_LOG.md`'s 2026-10-13 correction
entry. Coverage (`docs/update_cards_report.md`) is 777 of 781 sites.

**Canopy height.** `card_canopy_height_max_m` is the maximum `HEIGHTC` value across all of a
site's dated records, used exactly as recorded in BADM — no `HEIGHTC_STATISTIC` filter (a filter
on `HEIGHTC_STATISTIC` was added, then removed again, both on 2026-10-13; see `SESSION_LOG.md`).

**Heights are taken from BADM as reported, with no filtering or alteration.** Both height
columns can include unusual-looking values (a dispersion statistic reported as `HEIGHTC`, a
canopy height greater than the EC instrument height, etc.) because nothing is excluded. Sites
worth checking by hand before use are flagged in `card_height_check` (see the column table
above) — see "Height flags" in `docs/update_cards_report.md` for the full list and per-flag
counts.
