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
- `docs/update_cards_report.md` — coverage, SITE_DESC word-count distribution, a three-site
  hand-computed spot-check, and an out-of-range value listing, from the most recent run.
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
| `card_tower_height_m`, `card_tower_height_source` | BADM EC instrument measurement height (not canopy height) — see caveat below |
| `card_map_mm_yr`, `card_mat_degC` | WorldClim 2.1 bio12 (MAP), bio1 (MAT), from `data/snapshots/site_worldclim.csv`, with a local-raster fallback for any site missing from it (never downloaded) |
| `card_site_description`, `card_site_description_words` | BADM `SITE_DESC`, same sources as the 2026-10-08 BADM search (`data/extracted/*/*_BIF_*.csv`, resolved one file per site via `flux_discover_files()`); several entries joined with `" | "` in ascending `GROUP_ID` ("BADM order") |

**Tower height caveat:** the FLUXNET Shuttle's BIF export is a reduced, ONEFlux-processing-time
BADM export (see `scripts/investigate_badm_management.R`'s own header comment) — it has no
structured numeric field for EC instrument/flux-measurement height anywhere (confirmed by a
network-wide scan of every `VARIABLE` name across all extracted BIF files for this run; only
canopy height, `HEIGHTC`, is structured). `card_tower_height_m` is therefore a best-effort
regex extraction from the free-text `FLUX_MEASUREMENTS_COMMENT` field, only where a
number-of-metres value appears within 60 characters of the word "height"; `card_tower_height_source`
records that this is `FLUX_MEASUREMENTS_COMMENT` (not a structured field) whenever a value is
found. Coverage is expected to be very low — see `docs/update_cards_report.md` for the actual
count.
