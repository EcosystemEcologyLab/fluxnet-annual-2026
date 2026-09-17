# Köppen–Geiger source-consistency investigation

**Type:** Read-and-report investigation + counterfactual calculation.
**Scope:** No existing script, figure, legend, snapshot CSV, or
`representativeness_metrics.csv` was modified. New code:
`scripts/diagnostics/kg_source_consistency.R`. New outputs: this report and
the CSVs listed in [Task 4](#task-4--three-way-counterfactual), all under
`review/diagnostics/kg_source_consistency/`.
**Run log:** `logs/kg_source_consistency_20260917.log`.

---

## Verdict

**Yes, a source mismatch exists, and Köppen–Geiger (KG) is the only
representativeness axis where it does.** For the current 781-site network,
the site-level classification (*q*) is computed locally from each site's own
ERA5 monthly reanalysis normals (`data/snapshots/site_koppen_era5.csv`,
`scripts/step5_compute_koppen_era5.R`), while the global land-area reference
distribution (*p*) is still the Beck et al. (2023) 1 km raster
(`data/snapshots/koppen_beck2023_global_distribution.csv`), never
recomputed under ERA5. The other four axes used in draft-manuscript Fig 4/5
(LULC, Aridity, Biomass, TRENDY NEE-IAV, TRENDY ET-median) draw *p* and *q*
from the identical raster/derived-product file — see the
[provenance table](#task-1--provenance-of-p-and-q) — so they are not
confounded this way.

The counterfactual in [Task 4](#task-4--three-way-counterfactual) isolates
the size of the effect at `current_781`. At the **two-letter (13-class)**
and **5-class** aggregation levels, switching the current network's KG
source from Beck-raster to ERA5-local raises weighted Jaccard (J) by
**+0.048 and +0.058** respectively, and **≈92% of that rise is attributable
to the classification-method change itself**, not to the 26 sites the ERA5
method leaves unclassified (dropping those 26 sites alone, holding the Beck
classification fixed, moves J by only +0.004 and +0.004). At the **30-class**
level the same source switch changes J by essentially nothing (**+0.0013**,
an order of magnitude smaller): 30-class agreement between the two sources
is only 69.0% (755 comparable sites), but the disagreements are spread
across many classes rather than concentrated in a few over/under-sampled
two-letter buckets, and net out at fine resolution. The task's own
before/after numbers (two-letter 0.373→0.423, 5-class 0.401→0.458) are
independently confirmed in `SESSION_LOG.md`'s 2026-08-20 entry (line 958),
computed at the then-current 767-site network; the same source-switch effect
persists at `current_781`, of comparable size (+0.048/+0.058 vs. the
767-network's +0.050/+0.057).

---

## Task 1 — Provenance of *p* and *q*

For every axis in `scripts/figure_representativeness_summary.R`'s `AXES6`
(lines 318–434, the config used to build draft Fig 4 =
`fig_rep001_current.png` and draft Fig 5 =
`fig_rep008_jaccard_trajectory_with_counts.png`):

| Axis | *q* (site, current_781) | *p* (global) | Same source? |
|---|---|---|---|
| **KG (13-class)** | `site_koppen_era5.csv`, ERA5-local classification (`AXES6$kg$load_fn`, `figure_representativeness_summary.R:322-332`; standalone KG figure script `figure_representativeness_kg.R:43`) | `koppen_beck2023_global_distribution.csv`, Beck et al. (2023) 1 km raster (`figure_representativeness_summary.R:223-228`; `figure_representativeness_kg.R:41`) | **No.** Different classification methods. Documented as a deliberate, scoped decision in `figure_representativeness_kg.R:6-14` and `SESSION_LOG.md:1009-1017` (2026-08-20) — not an oversight, but a real source mismatch nonetheless. |
| **LULC (10-class HL)** | `site_landcover_cci.csv`, point-extracted from `data/external/cci_landcover/v2.1.1/cci_lc_2022_kg_aligned_native.tif` (`scripts/extract_current_network_biomass_landcover.R:137-139`) | Same cached raster file, read or (if absent) rebuilt via `terra::resample(lc_rast, kg_rast, method="near")` from the identical source NetCDF and written back to that exact path (`scripts/figure_representativeness_landcover.R:68-97, 287-309`); global zonal sums run on `terra::mask(lc_aligned, kg_rast)` (`:349-368`) | **Yes.** One script writes the shared cache, the other reads it. |
| **Aridity (7-class UNEP)** | `terra::extract(ai_rast, pts, ...)` (`scripts/figure_representativeness_aridity.R:122`, with fallback at `:146`) on `ai_rast <- terra::rast("data/external/aridity/Global-AI_ET0__annual_v3_1/ai_v31_yr.tif")` (`:47,104`) | `terra::classify(ai_rast, rcl5/rcl7, ...)` (`:~268,~301`) on the **same in-memory raster object**, single script run | **Yes.** Identical object, single run — no separate load path to drift. |
| **Biomass (7-bin hybrid)** | Point-extracted at native resolution from `data/external/cci_biomass/ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif`, band 18 (2024) (`scripts/extract_current_network_biomass_landcover.R:117-119`) | Same file, same `BAND_YEAR <- 18L` (`scripts/figure_representativeness_biomass.R:32,56,64,69`), resampled (bilinear) onto the Beck-KG 0.00833° grid for zonal integration (comment at `:275`) | **Yes, same product/version/band.** Note (per the task's "same raster version and grid" check): the *processing* differs — site side is point-extracted at native ~1 km resolution, global side is bilinear-resampled onto the ~0.00833° KG grid for area-weighting — a standard aggregation step on the same source data, not a different source. |
| **TRENDY NEE-IAV / ET-median** | `terra::extract(r_map, coords_mat, ...)` (`scripts/figure_representativeness_trendy_compute.R:396`) on ensemble rasters built once per axis (`:379-382`) and cached to `data/external/trendy/derived/*.tif` (`:336-339`) | Global distribution built from the same cached `.tif` (`input_sources` at `:567,594`), same script run | **Yes.** Same derived raster, same run. |

**Confirms the task's premise: KG is the only axis with a p/q source
mismatch.** All four other axes were checked for raster-version/grid
consistency between the site-extraction and global-distribution steps
(the task's specific instruction for continuous axes); none showed a
mismatch. Biomass is the only axis with a processing difference (native
point extraction vs. resampled zonal aggregation), not a source difference,
and is flagged above for completeness.

**Not found:** no separate script was found that recomputes
`koppen_beck2023_global_distribution.csv` from ERA5 — searched
`scripts/` for any `*era5*global*` or `*koppen*global*` name beyond
`scripts/compute_koppen_beck2023_global.R` (Beck-raster only; see
[Task 5](#task-5--era5-based-global-distribution-feasibility-scoping-only)).

---

## Task 2 — What's actually in draft Fig 4 and Fig 5, and the historical-network question

**Fig 4** (`review/figures/draft_manuscript_v1/fig_04_current_network_sampling_ratios.png`)
is `review/figures/representativeness/fig_rep001_current.png`, built by
`make_grid_fig("current_781", mode="single")`
(`figure_representativeness_summary.R:970-978`) — a single-network 2×3
sampling-ratio grid at **7-bin hybrid** for the continuous axes (per its own
legend, `fig_04_current_network_sampling_ratios.legend.txt:24-25`, which
flags that this is legacy relative to Fig 5/Rep007-008's 18-bin). KG panel
uses the **13-class two-letter** aggregation (`m_agg = "13class_twoletter"`,
`:320`).

**Fig 5** (`review/figures/draft_manuscript_v1/fig_05_jaccard_trajectory_with_counts.png`)
is `review/figures/representativeness/fig_rep008_jaccard_trajectory_with_counts.png`,
built from `ax_specs_6` (`figure_representativeness_summary.R:1037-1044`),
also using **13-class two-letter** for KG and **18-bin hybrid** for the
continuous axes, across all four network generations (marconi, la_thuile,
fluxnet2015, current_781).

**Historical-network KG points in Fig 5 are still Beck-raster, confirmed.**
`AXES6$kg$load_fn` (`figure_representativeness_summary.R:322-332`):
`kg_base <- if (net == "current_781") "koppen_era5" else "koppen_beck2023"`.
For marconi/la_thuile/fluxnet2015 this resolves (via `site_csv()`,
`:274-277`) to `site_koppen_beck2023_marconi.csv` (35 rows),
`site_koppen_beck2023_la_thuile.csv` (252 rows), and
`site_koppen_beck2023_fluxnet2015.csv` (212 rows) — all present on disk with
exactly those row counts, all Beck2023-raster-derived. Only `current_781`
switches to `site_koppen_era5.csv`.

**Yes, the FLUXNET2015 → Current step in Fig 5's KG line mixes two
classification methods within one trajectory.** Three points (Marconi, La
Thuile, FLUXNET2015) are Beck-raster; the fourth (Current) is ERA5-local;
all four are plotted as a single continuous line with no visual or legend
cue that the method changes at the last segment. This is by explicit,
documented design (`SESSION_LOG.md:1010-1017`, 2026-08-20: "Scope
deliberately excludes those historical-network comparisons... per decision")
— not an oversight — but the figure itself does not disclose it, and per
[Task 4](#task-4--three-way-counterfactual) roughly half of the total
Marconi→Current rise in KG two-letter J (0.223→0.420, ΔJ=+0.197) occurs in
that last, source-mixed step (FLUXNET2015 0.365 → Current 0.420, ΔJ=+0.055,
of which the counterfactual below attributes +0.045 to the source switch
alone).

---

## Task 3 — The denominator

Per `SESSION_LOG.md:986-1001` (2026-08-20, at the then-767-site network) and
the 2026-09-01 rerun log (`logs/repr_chain_step5_compute_koppen_era5_20260901T105744.log`),
the classifiable-site accounting differs between the two network sizes
because the underlying reason for exclusion differs:

| | 767-site network (2026-08-20 run) | 781-site network (2026-09-01 run, current) |
|---|---|---|
| Sites with **zero** ERA5 monthly rows in DuckDB | 8 (DK-Eng, DK-Fou, ES-Pdu, IT-MtM, IT-PT1, JP-Nkm, JP-Tgf, SJ-Adv) | **0** — all 781 sites now have ≥1 ERA5 monthly row (log: "Raw ERA5 monthly rows: 419328 (781 sites)") |
| Sites with ERA5 rows but **every candidate year fails** the 5000 mm/yr P-screen (`KG_ERA5_MAP_MAX_MM`) | 25 | 26 (all with `n_years_used = 0`, confirmed in the run log's unclassified-sites table) |
| **Total unclassified** | 33 (767 − 734 classified) | **26** (781 − 755 classified) |
| Row count in `site_koppen_era5.csv` | **759** (not 767 — see mechanism below) | **781** (all sites present) |

**Why the row count differs (759 vs. 781) even though both runs use the
same code** (`R/climate_classification.R`, only 2 commits touch this file
per `git log`; no version drift between runs): `compute_era5_monthly_climatology()`
builds its site list from `df`, the ERA5 rows already filtered to the
1991–2020 period with non-NA `TA_ERA`/`p_tot` (`:245-255`), then does
`all_sites <- dplyr::distinct(df, .data$site_id)` (`:354`) to "ensure every
site in the input appears, even if unclassifiable" (`:353`). A site with
**zero** ERA5 rows in that filtered input never enters `df` at all, so it is
silently absent from the output CSV entirely (not even an NA row) — this is
what happened to the 767-network's 8 no-ERA5-data sites. A site **with**
ERA5 rows whose years all fail the P-screen **does** get an output row, with
`koppen_class = NA` — this is the 25/26-site P-screen-fail category in both
runs. At 781, because 0 sites lack ERA5 rows entirely, the two categories
collapse into one 26-site NA-only outcome and the CSV row count equals the
full network size.

**How the 26 unclassified sites are handled in *q*: dropped from the
numerator, but retained in the denominator (diluted, not NA-filled, not
given a fallback class).** In `count_sites()`
(`figure_representativeness_summary.R:279-286`), `n_total <- nrow(df)` is
computed **before** the `!is.na(class_col)` filter, so `network_frac = n /
n_total` uses the full 781 as denominator while unclassified sites
contribute 0 to every class's numerator. The standalone
`figure_representativeness_kg.R` does the same thing via `n_sites <-
nrow(sites_df)` (`:91`, = 781) and `site_fracs()`'s `table()` call
(`:107-113`, which silently drops `NA` levels from the count but not from
`n_sites`). **The result: network fractions for the KG axis sum to
755/781 ≈ 0.967, not 1.0** — every class's sampling ratio is uniformly
diluted by that ~3.3% factor relative to what it would be if the
denominator were the 755 actually-classified sites.

**`n_sites` in `representativeness_metrics.csv` vs. the figure legend:**
both show **781**, and both are consistent with each other —
`representativeness_metrics.csv`'s `koppen_beck2023`/`current_781` rows
carry `n_sites = 781` (`figure_representativeness_kg.R:427`, `n_sites <-
nrow(sites_df)`), matching the "Current (n=781)"/"FLUXNET\\n(781 sites)"
labels in `fig_rep001_current.png`/Fig 4's legend
(`NET_TITLES`/`NET_NSITES`, `figure_representativeness_summary.R:719-724,839`)
and Fig 4's own legend text (`fig_04_..._sampling_ratios.legend.txt:82-83`,
"NETWORKS: Current FLUXNET (n = 781 sites...)"). **The number is
consistent but potentially misleading**: it states the network size, not
the number of sites that actually contributed a KG classification (755) —
the 26-site gap is not disclosed anywhere in the figure, legend, or
`representativeness_metrics.csv` row.

---

## Task 4 — Three-way counterfactual

Computed by `scripts/diagnostics/kg_source_consistency.R`; full outputs in
`kg_counterfactual_metrics.csv`, `kg_sampling_ratios.csv`,
`kg_confusion_twoletter.csv`, `kg_class_shift_drivers.csv` (this directory).
`site_koppen_beck2023.csv` (767 rows) was extended to 781 rows for this
task only, by extracting the same Beck 2023 raster at the 14 sites added
between the 767- and 781-site snapshots (confirmed a strict superset — 0
removed, 14 added — by the script's own check); written to
`site_koppen_beck2023_current_781.csv` here, **not** to `data/snapshots/`.
All three variants use a fixed denominator of 781 (matching the pipeline's
own dilution convention from Task 3, so that (a) vs (c) isolates the
classification-source effect only, and (b) vs (c) isolates the
site-coverage effect only, at identical denominator).

| Level | (a) as built<br>ERA5 vs Beck-global | (b) consistent raster<br>Beck(781) vs Beck-global | (c) same sample<br>Beck(755) vs Beck-global | Δ (a−b)<br>*source + coverage* | Δ (a−c)<br>*source only* | Δ (c−b)<br>*coverage only* |
|---|---|---|---|---|---|---|
| Two-letter (13-class) | J=0.4198, H=0.3826 | J=0.3716, H=0.4112 | J=0.3753, H=0.4075 | **+0.0482** | **+0.0445** | +0.0037 |
| 5-class | J=0.4560, H=0.2990 | J=0.3984, H=0.3309 | J=0.4028, H=0.3269 | **+0.0576** | **+0.0532** | +0.0044 |
| 30-class | J=0.3511, H=0.4426 | J=0.3498, H=0.4410 | J=0.3495, H=0.4381 | +0.0013 | +0.0016 | −0.0003 |

(Identity check: Δ(a−b) = Δ(a−c) + Δ(c−b) at every level, by construction.)

(a)'s two-letter/5-class/30-class values reproduce
`representativeness_metrics.csv`'s `koppen_beck2023`/`current_781` rows
exactly (e.g. J=0.4198469054828605 two-letter), confirming this script's
methodology matches the actual pipeline.

**Reading the decomposition:** at two-letter and 5-class, **~92% of the
total (a−b) rise is the classification-source effect (a−c), and only ~8% is
the site-coverage effect (c−b)** — dropping the 26 unclassifiable sites
barely moves J on its own. At 30-class, all three effects are within
±0.002 of each other — noise-scale relative to the two-letter/5-class
effects, an order of magnitude smaller.

### Site-level confusion (two-letter, 755 sites classified by both methods)

Full-code (30-class) agreement: 69.0% (521/755). Two-letter agreement:
**77.5%** (585/755) — not previously reported in `SESSION_LOG.md`, which
only gives full-code (69.2–69.4%) and main-group/5-class (84.5–84.6%)
figures; this fills the gap at the two-letter level draft Fig 4/5 actually
use. Main-group (5-class) agreement: 84.5% (638/755). (These are computed
on the complete 755-site comparable set, 14 more than the 741-site figure
in the 2026-08-20/2026-09-01 run logs, because `site_koppen_beck2023.csv`
on disk still lacked the 14 sites added in the 767→781 update — those logs'
741-site comparison predates this task's re-extraction.)

**Classes ranked by |Δ network fraction|, (a) ERA5 → (b) Beck, two-letter:**

| Rank | Class | q (ERA5) | q (Beck) | Δ sites (of 781) | Direction |
|---|---|---|---|---|---|
| 1 | **BW** (arid, desert) | 0.0653 (51 sites) | 0.0307 (24 sites) | **−27** | ERA5 calls far more sites BW than Beck |
| 2 | **Df** (cold, no dry season) | 0.3137 (245) | 0.3444 (269) | **+24** | Beck calls more sites Df |
| 3 | BS (arid, steppe) | 0.1024 (80) | 0.0807 (63) | −17 | ERA5 calls more sites BS |
| 4 | Cf (temperate, no dry season) | 0.2510 (196) | 0.2676 (209) | +13 | Beck calls more sites Cf |
| 5 | Ds (cold, dry summer) | 0.0218 (17) | 0.0359 (28) | +11 | |
| 6 | Cs (temperate, dry summer) | 0.0883 (69) | 0.0999 (78) | +9 | |
| — | Am, Cw, Af, Aw, Dw, EF, ET | — | — | ≤4 each | minor |

**Sites driving the top two shifts** (from `kg_class_shift_drivers.csv`):

- **BW → mostly BS under Beck** (36 sites move out of ERA5's BW class under
  Beck raster; 20 of those specifically become Beck-BS): e.g. `AU-Cpr,
  AU-Lon, AU-Lox, CA-TP2, CN-GuT, ES-HeB, ES-Hen, KE-Mkt, US-A37, US-A39,
  US-Akn, US-CF1..CF4` and 24 more. Only 9 sites move the other way (Beck
  calls BW where ERA5 didn't): `AU-GWW, CN-Dmn, CN-Zha, ES-Amo, ML-Kem,
  US-ASH, US-ASM, US-PSH, US-UTJ`.
- **Df gains 30 sites under Beck** that ERA5 called something else (mostly
  from BS, per the confusion table: `BS→Df` = 12 sites) — e.g. `AU-APL,
  AU-Sno, CA-EM2, CA-TP2, CA-TVC, CH-Rh1, CH-Rh2, DE-Lnf, ES-VDA, KR-ScC,
  NO-Fns, SE-Htm, SE-St1, US-BZB, US-BZF` and 15 more; 15 sites move the
  other way (`CA-CF1, CA-CF3, CN-Mxn, CN-Qng, JP-Fjy, KR-GmP/GmR/GmS,
  KR-PcD, KR-UAO, RU-Ch2, RU-Che, US-KPL, US-xDC, US-xWD`).

Net pattern: ERA5 systematically classifies more sites into the two arid
classes (BW, BS combined: 131 sites under ERA5 vs. 87 under Beck, in the
755-site comparable set) than the finer-resolution Beck raster does for the
same coordinates, with most of the difference reallocating to the cold/
temperate no-dry-season classes (Df, Cf). This is consistent with — though
not proof of — the coarse (~0.25–0.5° native) spatial averaging in ERA5
reanalysis blurring local aridity gradients that Beck's 1 km raster
resolves; `docs/known_issues.md` §9a already documents a related ERA5
precipitation spatial-averaging artifact for the P-screen exclusions.

---

## Task 5 — ERA5-based global distribution: feasibility (scoping only, not computed)

**On disk:** no gridded ERA5 data exists anywhere under `data/external/`
(searched for `*era5*` — zero hits besides the per-site
`site_koppen_era5.csv` outputs already discussed). The only ERA5 data in
this repo is per-site monthly reanalysis bundled inside each FLUXNET
Shuttle download (`*_FLUXNET_ERA5_MM_*.csv`), ingested into DuckDB's
`monthly` table (`dataset = 'ERA5'`) — point data at site locations, not a
spatial grid. A global distribution requires an entirely new gridded-ERA5
download; nothing on disk gets you partway there.

**What would need replicating:** `scripts/compute_koppen_beck2023_global.R`
loads the already-classified Beck raster
(`data/external/koppen_beck2023/1991_2020/koppen_geiger_0p00833333.tif`, 1
km) and simply area-weights it (`terra::cellSize(mask=TRUE)` →
`terra::zonal(sum)`). An ERA5 equivalent must additionally *run the
classification itself* — Beck's raster is pre-classified; a gridded ERA5
product is not. That means downloading 30 years (1991–2020) of gridded
monthly-mean 2 m temperature and total precipitation. ERA5 reanalysis-proper
is native ~0.25° (~31 km); ERA5-Land is ~0.1° (~9 km) — both far coarser
than Beck's 1 km, so any "consistent" ERA5-based global product would itself
be a lower-resolution product, introducing a new p/q resolution question
even if the classification source were unified.

**Classification cascade is not the bottleneck:**
`R/climate_classification.R:54-175`'s `classify_koppen_geiger()` operates
on a single site's 12-month T/P normal vector as a pure boolean cascade —
directly vectorizable per-gridcell (`terra::app()` or a row-wise matrix
apply) once a gridded 30-year monthly normal exists. A global 0.25° grid is
~1M land cells; applying the cascade is a minutes-scale operation.

**Rough cost:** the download is the real cost — a few GB (0.25°) to tens of
GB (0.1°/ERA5-Land) of compressed NetCDF from the Copernicus Climate Data
Store, which requires a CDS API account/credentials not currently
configured anywhere in this repo (no CDS/ERA5-download variables appear in
`R/pipeline_config.R`'s environment-variable table; `.env.example` was not
exhaustively checked for this — flagged as unverified). Resolving the
resolution mismatch against Beck's 1 km raster (regridding one or the
other, and choosing a consistent land mask) is a design decision with no
existing partial implementation in this repo, and is the more open-ended
part of the work — not a large compute cost, but not a small decision
either.

---

## Options for the manuscript (no recommendation)

1. **Revert `current_781`'s KG axis to Beck-raster classification** (use
   `site_koppen_beck2023_current_781.csv`-equivalent, i.e. re-run
   `step4_extract_koppen_beck2023.R` against the 781-site snapshot instead
   of its hardcoded 767-site one). *Changes:* Fig 4 panel A, Fig 5's KG
   line's Current point, and the `koppen_beck2023`/`current_781` rows in
   `representativeness_metrics.csv` (J two-letter 0.420→0.372-ish,
   5-class 0.456→0.398-ish, 30-class ≈unchanged, per this report's variant
   (b)). Restores full p/q consistency for KG, at the cost of losing the
   ERA5-based single-source unification with the `Anomalies_KG` figures
   that motivated the 2026-08-20 switch in the first place.
2. **Build an ERA5-based global distribution** (per Task 5 scoping above)
   so both p and q are ERA5-derived for `current_781`. *Changes:* a new
   `koppen_era5_global_distribution.csv`, Fig 4 panel A, Fig 5's KG line's
   Current point, `representativeness_metrics.csv`'s KG rows for
   `current_781` only (historical networks would still need Beck-raster p,
   since they're not ERA5-classified — see Task 2). Requires a new external
   download (CDS credentials) and a resolution-mismatch decision; the most
   labor to implement, but the most internally consistent end state for
   `current_781` specifically.
3. **Keep the current setup, disclose it in Methods.** *Changes:* no figure
   or data file changes; add explicit Methods/caption language noting that
   `current_781`'s KG site classification is ERA5-local while the global
   reference and all three historical-network points remain Beck-raster,
   and (per Task 3) that only 755 of 781 sites contribute a KG
   classification. Lowest effort; leaves the p/q mismatch and the Fig
   5 method-mixing in place, now disclosed rather than silent.

---

## Not-found log

- No script recomputing `koppen_beck2023_global_distribution.csv` from
  ERA5 — searched `scripts/` for `*era5*global*`/`*koppen*global*` beyond
  `compute_koppen_beck2023_global.R`.
- `scripts/figure_representativeness_trendy_wrap.R` was not opened (only
  `_compute.R` performs the extraction/global-distribution work relevant to
  Task 1; `_wrap.R` is out of scope for provenance).
- `.env.example` was not exhaustively checked for CDS/ERA5-download
  credential variables (Task 5); no such variables appear in
  `R/pipeline_config.R`'s documented environment-variable table.
- `outputs/exclusion_log.csv` / `outputs/unknown_log.csv` (gitignored,
  regenerated per run) were not read directly for this report; the
  26-site P-screen-failure accounting instead comes from
  `logs/repr_chain_step5_compute_koppen_era5_20260901T105744.log`, the
  captured console output of the run that produced the current
  `site_koppen_era5.csv`.

## Where code and docs disagree (code treated as authoritative)

- `docs/figure_rebuild_781_20260901.md` and both Fig 4/5
  `.legend.txt` files note a **prior, unexplained** KG two-letter J=0.373
  for "current" that matches neither `current_767` (0.423) nor
  `current_781` (0.420) in `representativeness_metrics.csv`, and state it
  predates the 2026-09-01 rebuild and "was not investigated further." This
  report does not resolve that specific number either — it is a separate,
  older discrepancy from the one this task investigates (which is about
  0.373→0.423, the documented 767-network before/after in
  `SESSION_LOG.md:1034-1039`, not the 0.373 in the old legend text itself).
  Flagged here per the task's instruction to report code/doc disagreements
  rather than silently reconcile them.
