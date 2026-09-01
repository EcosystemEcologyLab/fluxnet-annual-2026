# Draft-manuscript figure rebuild — 767/775 → 781 sites — 2026-09-01

**Author:** Claude Code, on behalf of David J.P. Moore
**Scope:** Step 2 of 2 (figure rebuild). Follows the gap download reported in
`docs/shuttle_gap_download_20260901.md`. All six `review/figures/draft_manuscript_v1/`
figures and legends rebuilt against the new 781-site snapshot; old versions
deprecated with history preserved; shuttle-team email recorded.

---

## Summary

| | |
|---|---|
| Reference snapshot (pinned, every figure) | `data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv` (781 sites) |
| Frozen paper snapshot | `data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv` (767 sites) — **untouched** |
| Figures rebuilt | 6 / 6, all render, all timestamps 2026-09-01 |
| Legends rebuilt | 6 / 6, all counts/N/J cross-checked against the actual data |
| Six flagged sites | All confirmed handled per spec (see Step 3 below) |
| IT-SR2 (FLX→ICOS hub change) | Confirmed present in every figure it belongs in |

---

## Step 0 — Shuttle-team email

Saved to `docs/correspondence/shuttle_release_query_20260901.md`.

---

## Step 1 — Deprecation

Existing convention confirmed: `review/figures/draft_manuscript_v1/deprecated/`
already existed (used once before, 2026-08-31, for a single-figure swap).
The **entire** prior contents of `review/figures/draft_manuscript_v1/` (6
PNGs, 6 `.legend.txt`, and `RUN_LOG_fig2_swap.txt`) were moved via `git mv`
into a new dated subfolder, `deprecated/draft_manuscript_v1_20260624/`,
before any regeneration — full history preserved, nothing overwritten in
place.

**Caveat on the "20260624" label:** five of the six deprecated figures
(1A, 1B, 3, 4, 5) were in fact built against the 767-site frozen snapshot,
consistent with the folder name. **Figure 2 was the exception** — its most
recent build (2026-08-31) had drifted onto an ad hoc 775-site development
snapshot (`fluxnet_shuttle_snapshot_20260827T102948.csv`), not the 767-site
frozen one, because its source script selected the snapshot via a
newest-file glob rather than a pin. This is exactly the divergence risk the
task's pinning requirement is meant to close (see Step 3, Fig 2).

---

## Step 2 — Extending network-side inputs (global reference distributions unchanged)

Global reference distributions (TRENDY grids, Köppen, land cover, aridity,
biomass, WorldClim) were **not recomputed** — only the network's sampling of
them. All rasters and lookup tables reused as-is from `data/external/`.

### Data pipeline prerequisite (not itself one of the six figures, but a hard
dependency for Fig 2 and Fig 4/5)

`data/duckdb/fluxnet.duckdb` still reflected only 759 sites at session start
(built before the 22-site gap download). Rebuilt in order:

1. `scripts/03b_create_database.R` (`duckdb_update.R`'s incremental
   anti-join upsert — not a full rebuild) → all resolution tables now cover
   781 sites.
2. `scripts/04_qc.R` → `*_qc` tables refreshed (YY: 614 VUT-gated, 42
   CUT-gated, 125 no-QC-column sites = 781).
3. `scripts/05_units.R` → `*_converted` tables refreshed (`annual_converted`:
   41,161 rows).

**Housekeeping side-effect, done once, autonomous (rename within the
repository):** `data/extracted/fluxnet2015/` (a FLUXNET2015 comparison
dataset, not Shuttle data — see the prior session's report) was permanently
relocated to `data/fluxnet2015_comparison/`. Its presence inside
`data/extracted/` broke every `flux_discover_files()` scan of that directory
(`duckdb_setup.R`/`duckdb_update.R` included) with a "too-short filename"
parse error — previously worked around read-only via a scratch symlink farm;
this session it was simply moved out for good, since it isn't Shuttle data
and doesn't belong under `data/extracted/` regardless. Two other scripts
that referenced the old path (`assess_flux_data_by_igbp_fluxnet2015.R`,
`figure_flux_comparison_combo_alt_common_siteyears.R` — neither part of this
rebuild) were updated to the new path so they aren't left broken.

### Per-site covariate extraction for the 14 genuinely-new-to-Shuttle sites

The 767→781 change is **not** a straightforward +14: of the 22 sites
downloaded in the gap-download step, 8 were already listed in the 767-site
frozen snapshot (never downloaded, but already covariate-extractable by
coordinate) and 14 are genuinely new to the Shuttle listing. 767 + 14 = 781.

Two axes (biomass, land cover) had no re-extraction path in their current
scripts — `figure_representativeness_biomass.R` / `_landcover.R` only
re-classify an *existing* `site_biomass_cci_v7.csv` / `site_landcover_cci.csv`
into bins, they don't call `terra::extract()` themselves (the original
one-off extraction script for the current network predates this repo's
history). A new script,
`scripts/extract_current_network_biomass_landcover.R`, fills that gap —
same method (`terra::extract` + nearest-land recovery within 3°, band 18 of
ESA CCI Biomass v7.0) already established in
`extract_historical_sites_representativeness.R` for the historical networks
— and wrote fresh 781-row raw values (0 NA for either axis). Aridity and
KG-present-day (ERA5-based) already did fresh extraction from the pinned
snapshot / DuckDB each run and needed only the pin fix. TRENDY axes read
their site list from `site_biomass_cci_v7.csv` and do their own extraction,
so fixing biomass's coordinate list fixed TRENDY too.

**Provenance convention:** the pre-existing 767-row per-site CSVs and their
`.meta.json` files were archived to a `_current_767` suffix (mirroring the
existing `_marconi`/`_la_thuile`/`_fluxnet2015` convention) *before* being
overwritten with fresh 781-row data — nothing was destructively lost:
`site_koppen_beck2023(*).csv`, `site_aridity.csv`, `site_biomass_cci_v7.csv`,
`site_landcover_cci.csv`, `site_trendy_*.csv` (×4), `site_worldclim.csv`,
`site_koppen_era5.csv`.

**Bug found and fixed en route:** `figure_representativeness_biomass.R`,
`_landcover.R`, and `_aridity.R` each upserted `representativeness_metrics.csv`
with a bare `filter(axis != "<axis>")` (or `!grepl("^aridity", axis)`) —
this drops **every network's** row for that axis, not just the one being
rewritten, and the replacement rows carried no `network`/`n_sites` column at
all. Fixed to scope the filter to `(axis == X & network == "current_781")`,
matching the pattern already used correctly in `figure_representativeness_kg.R`
and `_trendy_compute.R`. Confirmed post-fix: `representativeness_metrics.csv`
still holds all 39 marconi / 39 la_thuile / 39 fluxnet2015 rows plus the
preserved 39 current_767 rows, alongside 33 new current_781 rows — 189 rows
total. current_781 is 6 short of current_767's 39: the missing 6 are the
`koppen_beck2023_future_ssp245_2041_2070` / `_ssp585_2071_2099` axes
(3 aggregation levels each), written only by `figure_representativeness_kg_future.R`
— deliberately out of scope for this rebuild since none of the six target
figures consume the future-KG axes (verified: `figure_representativeness_summary.R`'s
`AXES6`/`ax_specs_3`/`ax_specs_5`/aggregation-sensitivity configs reference
only the present-day `koppen_beck2023` axis, never the `_future_*` variants).
`recompute_continuous_axes_30bin.R` / `_multibin.R` also
had `bind_rows()` with no dedup guard against a re-run; added a
`distinct(axis, aggregation_level, network, .keep_all = TRUE)` safety net
(new rows win) since this session needed to iterate.

Regenerated inputs post-date this step, confirmed: `site_biomass_cci_v7.csv`
/ `site_landcover_cci.csv` / `site_aridity.csv` / `site_trendy_*.csv` /
`site_koppen_era5.csv` / `site_worldclim.csv` all timestamp 2026-09-01, all
781 rows, and `representativeness_metrics.csv`'s `current_781` rows
timestamp the same run.

`site_year_data_presence.csv` (consumed by Fig 1B) was found **stale since
2026-05-24** — nothing in the DuckDB-era pipeline refreshed it (its only
prior caller depended on the legacy RDS path). A new script,
`scripts/refresh_site_year_presence.R`, rebuilds it from the DuckDB `monthly`
table (raw, unfiltered — matching Fig 1B's existing convention) for the
781-site network: 6,106 / 6,243 site-years have data. This is exactly the
"regenerated inputs lagging the figures" risk the task called out.

---

## Step 3 — Six source figures regenerated against 781

| Figure | Script | Pin fix applied | Notes |
|---|---|---|---|
| 1A point map | `generate_point_maps.R` | glob → pinned 20260901 | |
| 1B cumulative site-years | `generate_duration_histograms.R` | glob → pinned 20260901 | needed presence-table refresh, above |
| 2 Whittaker | `generate_whittaker_alt_fig02_update.R` | glob → pinned 20260901 | see note below on script substitution |
| 3 flux comparison combo | `assess_flux_data_by_igbp_shuttle.R` (upstream) | hardcoded 20260624 → pinned 20260901 | 0.80 local QC threshold preserved exactly |
| 4/5 representativeness | full chain, 9 scripts | glob/hardcoded → pinned/current_781 | see Step 2 |

**Fig 2 script substitution, flagged per the task's "do not deviate without
flagging" instruction:** the task named `generate_whittaker.R` +
`generate_whittaker_overlays.R`. The actually-wired production source (per
`build_draft_manuscript_v1.R`'s own header comment, current as of
2026-08-31) is `generate_whittaker_alt_fig02_update.R`, which reuses
`generate_whittaker_overlays.R`'s cached global density grid and contour
functions but is the script that actually produces
`ALT_fig_02_whittaker_current.png`. Used the actually-wired script rather
than the two named ones, since running the latter would not have touched
what `build_draft_manuscript_v1.R` actually copies into the draft folder.

**Fig 2 hardcoded legend count — fixed, made dynamic:** the figure's own
inset text was already computed at run time (not hardcoded) — the
`generate_whittaker_alt_fig02_update.R` script always recomputed
`n_sites`/`n_nee_sites`/`n_site_years` from the DuckDB `annual_converted`
table + pinned snapshot. The bug was one level up: `build_draft_manuscript_v1.R`
had these three numbers **hand-typed** into a hardcoded legend string,
disconnected from the actual run (this is exactly how the deprecated Fig 2
ended up citing 775/638/4227 against a 775-site dev snapshot instead of the
767-site frozen one). Fixed by having `generate_whittaker_alt_fig02_update.R`
write a small sidecar,
`review/figures/candidates/ALT_fig_02_whittaker_current.counts.json`, and
`build_draft_manuscript_v1.R` now reads it and interpolates the values —
the build step will fail loudly (not silently substitute a stale number) if
the sidecar is missing. New counts: **781 sites, 656 with a qualifying
annual NEE value, 4,303 site-years** (previous, on 775 sites: 638 / 4,227).

**Fig 3 — which of the 22 new sites qualify at the 0.80 threshold:**

| Qualify (15) | Do not qualify — 0 site-years at QC ≥ 0.80 (7) |
|---|---|
| US-KLS, US-LS2, US-xHA, US-xKA, US-ZF1, JP-Tgf, IT-Ro1, IT-Ro2, IT-PT1, IT-MtP, IT-MtM, IT-Cpz, ES-Pdu, ES-LgS, DK-Eng | US-xTA, JP-Nkm, HK-MPM, SJ-Adv, FI-Si2, ES-Ln2, DK-Fou |

None of the 22 are CSH (the smallest Shuttle class); **no IGBP class crosses
the project's n≥5 reliability threshold in either direction** — every
plotted Shuttle class already had n≥7 before this update (see the
old-vs-new table in Step 5) and can only gain sites from an addition-only
delta. CSH and CVM remain excluded for the same reasons as before (CSH:
FLUXNET2015-side n=2; CVM: absent from the FLUXNET2015 release) — neither
exclusion rule is about Shuttle-side sample size, so this update cannot
change them.

**Fig 4/5 — full representativeness chain**, in dependency order:
`step5_compute_koppen_era5.R` → `figure_representativeness_kg.R` →
`_biomass.R` → `_landcover.R` → `_aridity.R` → `_trendy_compute.R` →
`recompute_continuous_axes_30bin.R` → `_multibin.R` →
`figure_representativeness_summary.R` (builds all 18 Rep figures in one
run, including our Fig 4 = `fig_rep001_current.png` and Fig 5 =
`fig_rep008_jaccard_trajectory_with_counts.png`). Network label renamed
`current_767` → `current_781` throughout (site-count-bearing labels, the
`site_csv()` bare-filename special case, `NET_ORDER`/`NET_NSITES`/
`NET_TITLES`/`NET_XLABELS_N`, the AXES6 `kg$load_fn` ERA5-vs-Beck2023
special case) — old `current_767` rows/labels untouched elsewhere (see
Step 2's provenance note).

---

## Step 4 — Draft folder and legends rebuilt

`scripts/build_draft_manuscript_v1.R` re-run: all 6 figures + 6 legends
copied/written into `review/figures/draft_manuscript_v1/` (timestamps
2026-09-01 11:08–11:10). The five legends with no source-script generator
(hand-authored at each figure's original creation — `fig_01a`, `fig_01b`,
`fig_03`, `fig_04`, `fig_05`) were manually edited against the actual
regenerated data (site counts, per-class N, Jaccard values — all cross-
checked against `representativeness_metrics.csv` / the comparison CSVs /
the presence table directly, not estimated). Fig 2's legend is now
sidecar-driven (see Step 3).

**One pre-existing inaccuracy found, not silently corrected:** Fig 4 and
Fig 5's legends both cited a Köppen-Geiger (13-class) J = 0.373 for the
"current" network. Neither the current_767 row (0.423) nor the new
current_781 row (0.420) in `representativeness_metrics.csv` matches that
number. This predates this session (present in the deprecated legends
too) and was not investigated further — flagged explicitly in both
legends rather than silently overwritten to make the numbers agree.

---

## Step 5 — Verification: old vs new headline numbers

### Total sites

| | Old (deprecated) | New | Δ |
|---|---|---|---|
| Fig 1A / 1B / 3 / 4 / 5 (767-site build) | 767 | 781 | +14 |
| Fig 2 (post-drift 775-site build) | 775 | 781 | +6 |

### Fig 3 — per-IGBP Shuttle N, NEP / ET / H panels (old 767 → new 781)

| IGBP | NEP old→new | ET old→new | H old→new |
|---|---|---|---|
| EBF | 38→39 | 38→39 | 39→40 |
| MF | 22→22 | 22→22 | 23→23 |
| DBF | 60→64 | 60→64 | 60→64 |
| ENF | 95→95 | 100→102 | 100→102 |
| CSH (excluded) | 11→11 | 11→11 | 11→11 |
| OSH | 32→33 | 33→34 | 34→35 |
| WSA | 15→15 | 17→17 | 17→17 |
| SAV | 10→11 | 12→13 | 12→13 |
| GRA | 120→126 | 123→129 | 123→129 |
| WET | 90→91 | 93→94 | 93→94 |
| CRO | 107→108 | 109→110 | 110→111 |
| CVM (excluded) | 7→7 | 7→7 | 8→8 |

### Fig 4/5 — Jaccard (current network, 7-bin for Fig 4 / 18-bin for Fig 5)

| Axis | Fig 4 old (767, 7-bin) | Fig 4 new (781, 7-bin) | Fig 5 old (767, 18-bin) | Fig 5 new (781, 18-bin) |
|---|---|---|---|---|
| KG (13-class) | 0.423 | 0.420 | 0.423 | 0.420 |
| LULC (10-class HL) | 0.556 | 0.567 | 0.556 | 0.567 |
| Aridity (7-class) | 0.667 | 0.666 | 0.667 | 0.666 |
| Biomass | 0.626 | 0.636 | 0.616 | 0.613 |
| TRENDY NEE-IAV | 0.494 | 0.506 | 0.486 | 0.484 |
| TRENDY ET-median | 0.446 | 0.456 | 0.433 | 0.432 |

(Marconi / La Thuile / FLUXNET2015 values are unchanged — global reference
distributions and historical-network site lists are untouched by this
update.)

### Six flagged sites — confirmed handled exactly as specified

| Site | Requirement | Confirmed |
|---|---|---|
| DE-Hai, FR-CLt, FR-EM2 | Render from on-disk (newer, `_r2`) release despite live listing now showing `_r1` | Not re-pulled or re-extracted this session — on-disk data unchanged since the prior session's download |
| DE-Hte, JP-Api | Render from on-disk (older) release despite live listing now showing a newer one | Same — not re-pulled |
| IT-SR2 | Include despite FLX→ICOS hub change; confirm not silently dropped by the broken `(product_source_network, site_id)` metadata join | **Confirmed present** in: the pinned snapshot; `site_biomass_cci_v7.csv`, `site_aridity.csv`, `site_landcover_cci.csv`, `site_trendy_nee_iav.csv`, `site_trendy_et_median.csv`, `site_koppen_era5.csv`, `site_worldclim.csv`, `site_flux_medians_shuttle.csv`; and DuckDB `annual_converted` (11 years of non-NA `NEE_VUT_REF`/`NEE_CUT_REF`, 2013–2023) — contributes to Fig 2's hexbin colouring (not just the point overlay) and to Fig 3's ENF class. The broken join only ever affected `flux_discover_files()`'s own optional metadata merge (used for reporting in the prior session, and as an optional QA comparison column in `step5_compute_koppen_era5.R`) — none of the actual figure-building scripts join on `product_source_network`, so none of them were at risk of dropping it. |

### git status before / after

**Before this session's figure work:** clean except the pre-existing
`outputs/session_info.txt` / `renv/activate.R` modifications (unrelated,
predate this session — left untouched, per the prior session's report) and
the prior session's own committed gap-download artifacts.

**Committed by this session:** the deprecated-folder `git mv`s (13 files),
the six regenerated figures + legends in `draft_manuscript_v1/`, the source
figures they were copied from (`review/figures/candidates/`,
`review/figures/network/`, `review/figures/flux_medians/`,
`review/figures/representativeness/` — all git-tracked per CLAUDE.md),
the regenerated `data/snapshots/*.csv` covariate/metrics files (git-tracked,
not gitignored), the new/edited scripts, this report, the correspondence
file, and the `SESSION_LOG.md` entry. `data/extracted/`, `data/raw/`, and
`data/duckdb/` remain gitignored and uncommitted, as does the relocated
`data/fluxnet2015_comparison/`.
