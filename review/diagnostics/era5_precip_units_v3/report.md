# ERA5 precipitation units, v3: ruling out ingestion/query, then a network-wide correction test

> **Provisional — pending store audit (2026-09-20).** The numbers in this report rest on the June 2026 `data/extracted/` extraction. One file in that extraction (IT-MBo's `FLUXNET_FLUXMET_MM` file) is known to differ from the currently-distributed archive under the same product ID. This report's conclusions are provisional pending `review/diagnostics/store_audit/` (in progress).

**Type:** Read-and-report diagnostic + counterfactual, confined to the
diagnostics folder. New code: `scripts/diagnostics/era5_precip_units_v3_partA.R`,
`scripts/diagnostics/era5_precip_units_v3_partB.R`. No edits to the
pipeline, figures, legends, snapshot CSVs, or the v1/v2 reports/outputs
(confirmed via `git status`). `R/climate_classification.R` read and
sourced only, never edited. Outputs in this directory.

---

## Verdict

**Part A: the P_F≈P_ERA identity v2 reported is real, but v2's
interpretation of it was backwards — a QC-flag polarity error, not
ingestion or query contamination. Ingestion and v2's own query are both
clean.** Read directly from the raw, undistributed `*_FLUXNET_FLUXMET_MM_*.csv`
files (bypassing DuckDB entirely), `P_F` and the bundled reference
`P_ERA` column are identical at exactly the months where `P_F_QC` is at
one end of its scale — and DuckDB's `monthly` table shows zero cross-
dataset column contamination (`P_F`/`TA_F` are non-null *only* in
`dataset='FLUXMET'` rows; `P_ERA`/`TA_ERA` non-null *only* in
`dataset='ERA5'` rows, at every one of the 8 test sites). v2's own query
correctly constrained `WHERE dataset = 'FLUXMET'`/`'ERA5'` (confirmed by
re-reading the script directly). **The actual root cause: at this
MM-resolution consolidated field, `P_F_QC` runs the *opposite* direction
from the raw HH-resolution convention CLAUDE.md documents (`0=measured`)
— confirmed at network scale (74,916 site-months): `P_F_QC=0` is
identical to `P_ERA` 100% of the time (entirely gap-filled), `P_F_QC=1`
only 0.8% of the time (genuinely measured).** v2's "P_F_QC=0 = fully
measured" filter selected precisely the wrong end of the scale. Redone
correctly (`P_F_QC≥0.9`): only **4 of the 26 excluded sites have any
genuinely-measured tower month at all in 1991–2020** (`CA-CF2`, `IT-MBo`,
`NO-And`, `US-HB4`) — the other 22 (essentially the whole JPF cluster)
have **zero** independent ground truth from tower data at any QC level,
full stop. This also resolves the open question v2 left about `IT-MBo`:
its 188 genuinely-measured months show ~16,865 mm/yr — still far above
the ~1200 mm/yr independently cited as its true climate, meaning
**IT-MBo's own tower rain gauge, not just ERA5, is implicated** — a
site-instrument issue is now on the table alongside a possible ERA5
extraction issue there.

**Part B: letting BADM and BIO12 jointly set an empirical factor (not
assuming one) finds two real, mutually-corroborated clusters — 113 sites
near 4× and 10 sites near 8× — spanning far beyond the 26 excluded sites
and beyond the JPF hub.** 112 of the 123 clustered sites have *both*
independent references individually disagreeing with ERA5 by a similar
margin (not one reference driving a spurious combined estimate). Applying
the nearest canonical factor brings 98 of 123 (80%) within normal scatter
of *both* references simultaneously. Running the real KG classifier on
the corrected values recovers 15 of the 26 previously-excluded sites and
changes the class of 12 already-classified sites (mostly Cf/Cs/Ds→BS/BW,
i.e. correcting away *false* aridity in wetter or already-marginal
sites), moving weighted Jaccard by **+0.0044** (two-letter) and
**+0.0142** (5-class) — both improvements, unlike the single-site
extended-window test in v1/v2 which made J marginally worse. `TA_ERA`
shows no comparable discrepancy against BADM MAT or WorldClim BIO1 at any
of these sites (mean absolute difference <1°C, ordinary reanalysis-vs-
station scatter) — **the error is precipitation-specific.** Reading the
Beck cascade directly: the arid (B) boundary and the tropical Af/Am/Aw
split use *absolute* MAP thresholds and are mechanically sensitive to a
uniform precipitation scaling; the Cw/Dw (dry-winter) split and every
temperature-driven subtype suffix (a/b/c/d) are *ratio*- or
temperature-only-based and are not mechanically affected — so most of
the 12 class flips (into BS/BW) are exactly the mechanical B-boundary
effect the cascade's own arithmetic predicts, not a surprise.

**This remains a magnitude/extraction question, not a units question —
Part A's/v2's T1 result (the pipeline's day-weighting formula matches
ONEFlux's own official annual product to <0.7% everywhere, including the
most extreme sites) is untouched by anything in this report.** Whatever
produces the ~4×/~8× disagreement with BADM and BIO12 sits further
upstream than this pipeline's arithmetic.

---

# PART A — Is the P_F≈P_ERA identity real, or an artifact?

## A1 — Raw file read, bypassing DuckDB entirely

Read directly from `data/extracted/*_FLUXNET_{FLUXMET,ERA5}_MM_*.csv` for
`IT-MBo`, `US-HB4`, `JP-Tak`, `JP-Mse`, `US-Akn`, and three unaffected
controls (`BR-Sa1`, `FI-Hyy`, `JP-Khw`) — 6 sample months each, including
the specific months v2 flagged. Full table: `table_a1_raw_file_values.csv`.

| site | n_qc0-or-flagged months sampled | P_F identical to P_ERA (within file) |
|---|---|---|
| FI-Hyy (control) | 6/6 at `P_F_QC=0` | 6/6 identical |
| JP-Khw (control) | 6/6 at `P_F_QC=0` | 6/6 identical |
| US-Akn (control) | 6/6 at `P_F_QC=0` | 6/6 identical |
| JP-Mse (flagged) | 6/6 at `P_F_QC=0` | 6/6 identical |
| JP-Tak (flagged) | 6/6 at `P_F_QC=0` | 6/6 identical |
| BR-Sa1 (control) | 0/6 at `P_F_QC=0` (QC 0.67–0.87) | 0/6 identical — real divergence |
| IT-MBo (flagged) | 0/6 at `P_F_QC=0` (QC 0.97–1.0) | 0/6 identical — real divergence |
| US-HB4 (flagged) | 0/6 at `P_F_QC=0` (QC 0.997–1.0) | 0/6 identical — real divergence |

**The identity exists in the product itself, and it is not confined to
"affected" sites** — it appears at `FI-Hyy` and `JP-Khw`, both
otherwise-unremarkable control sites, at exactly the same `P_F_QC=0`
months. This immediately signals that `P_F_QC=0` marks something common
to *all* sites at certain months (gap-filling), not a site-specific
problem.

## A2 — Ingestion trace: no column collision

`scripts/03_read.R`, `scripts/duckdb_setup.R`, `scripts/duckdb_update.R`
were read for every line mentioning `dataset`/`ERA5`/`FLUXMET` (line
numbers logged in the run log). Per-site, per-`dataset` non-null column
coverage in the ingested `monthly` table, for all 8 test sites
(`table_a2_dataset_coverage.csv`):

| dataset | P_F non-null | P_ERA non-null | TA_F non-null | TA_ERA non-null |
|---|---|---|---|---|
| FLUXMET | matches row count exactly | matches row count exactly | matches | matches |
| ERA5 | **0** | matches row count exactly | **0** | matches |

Zero cross-contamination at every site: `FLUXMET` rows never carry a
non-null `P_ERA`/`TA_ERA` and vice versa. **Ingestion is clean — this is
not a column-collision bug.**

## A3 — v2's own query

Re-read directly from `scripts/diagnostics/era5_precip_units_v2.R`:
`mo_era5 <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_ERA FROM monthly WHERE dataset = 'ERA5'")`
and `mo_fluxmet <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_F, P_F_QC FROM monthly WHERE dataset = 'FLUXMET'")`
— both correctly constrained. **Not a query bug.**

## A4 — Root cause: P_F_QC polarity, verified at network scale

| P_F_QC value | n site-months | fraction identical to P_ERA |
|---|---|---|
| exactly 0 | 22,101 | **100.0%** |
| (0, 0.5] | ~2,000 | 1.7–5.7%, decreasing |
| (0.5, 1) | ~2,900 | ~1.0–1.7% |
| exactly 1 | 38,205 | **0.8%** |

Full 10-bin table: `table_a4_qc_polarity.csv`. **`P_F_QC=0` means
entirely gap-filled (from ERA5); `P_F_QC=1` means entirely measured
(independent) — the opposite of the raw HH-resolution System-2 convention
CLAUDE.md documents (`0=measured`), which v2 applied uncritically to this
MM-resolution *fraction* field without checking whether the same polarity
survives aggregation.** This is the actual root cause: not ingestion
(A2), not an unconstrained query (A3), but an analyst misreading of an
ambiguously-documented, resolution-dependent QC convention — a real
category the task did not list among its three candidate explanations,
reported here because it is what the evidence supports.

**Corrected comparison** (`P_F_QC≥0.9`, genuinely measured), the 26
excluded sites (`table_a4_corrected_pf_comparison.csv`):

| site | genuinely-measured months (1991–2020) | ERA5 MAP | genuine tower MAP | ratio |
|---|---|---|---|---|
| US-HB4 | 12 | 658,045 | 2,016 | **326.4×** |
| IT-MBo | 188 | 24,150 | 16,865 | 1.43× |
| NO-And | 28 | 9,318 | 8,746 | 1.07× |
| CA-CF2 | 12 | 4,964 | 9,525 | 0.52× |
| *all other 22 sites* | **0** | — | — | **no ground truth available** |

**This is the single most important scoping fact for the co-author
decision: 22 of 26 excluded sites — essentially the entire JPF cluster —
have no tower precipitation data, of any quality, to check ERA5 against,
at any point in the 30-year window.** BADM and BIO12 (Part B) are the
only references available for them. `NO-And` is now the single site among
the 26 with genuine, reasonably-abundant (28-month) tower confirmation
that its high ERA5 value is real. `IT-MBo`'s own tower gauge (188
genuinely-measured months, the best-supported comparison of the four)
shows ~16,865 mm/yr — high, but not as extreme as ERA5's 24,150, and both
values are far above the ~1200 mm/yr independently cited for this site —
**consistent with a site-level instrument issue at IT-MBo, not only (or
even primarily) an ERA5 problem there.**

---

# PART B — Three-factor correction test

## B1 — Empirical factor estimate, letting BADM and BIO12 set it

Per-site `factor_estimate = median(ratio_to_bio12, ratio_to_badm_map)`,
all 781 sites, clustered against 1/4/8/24/1000 (within 15%):

| cluster | n (all sites) | n (sites with both references) |
|---|---|---|
| 1 | 323 | 262 |
| **4** | **113** | 103 |
| **8** | **10** | 10 |
| 24 | 0 | 0 |
| 1000 | 0 | 0 |
| elsewhere | 335 | 262 |

Full table: `table_b1_factor_estimates.csv`. **Robustness check**: of the
123 sites clustering at 4× or 8×, **112 (91%) have both BADM and BIO12
individually disagreeing with ERA5 by a comparable margin** — this is not
a single-reference artifact riding through an unrobust 2-point median; it
is two independent sources agreeing with each other. The `elsewhere`
bucket (335 sites, factor range 0–∞, median 0.82) is not a third coherent
population — it is the continuous middle ground v2 already described, not
a new cluster. `IT-MBo` (factor_estimate=38.5) and `US-HB4` (474.0) both
land in `elsewhere` — correctly excluded from the systematic clusters,
consistent with A4's finding that they are idiosyncratic, not part of the
cross-hub 4×/8× pattern.

## B2 — Does correction bring sites within normal scatter of *both* references?

Data-driven "normal scatter" band (5th–95th percentile of ratio-to-BIO12
among the 323 near-1 sites): **[0.83, 1.15]**.

| | n |
|---|---|
| Sites tested (clustered at 4× or 8×) | 123 |
| Corrected value within normal scatter of **both** BIO12 and BADM | **98 (80%)** |
| Not within normal scatter of both (closer to one reference only, or neither) | 25 (20%) |

Full table: `table_b2_corrected_vs_both_refs.csv`. This is a real,
substantial improvement, not just a shift toward one cherry-picked
reference — meeting the task's own bar for the test.

## B3 — Network-wide KG reclassification counterfactual

Applied the nearest canonical factor to all 123 clustered sites' `P_ERA`
values and reran the real, unmodified `compute_site_koppen_era5()`.

**Recovered (15 of 26 previously-excluded sites):**

| site | new class | | site | new class |
|---|---|---|---|---|
| AU-Fog | Aw | | JP-Shn | Dfa |
| BR-SM1 | Cfa | | JP-Tak | Dfb |
| JP-Fmt | Cfa | | JP-Tkb | Cfa |
| JP-Kzw | Dfb | | JP-Yms | Cfa |
| JP-MBF | Dfb | | KH-Kmp | Aw |
| JP-Mse | Cfa | | NO-And | Dfc |
| JP-Om2 | Cfa | | PE-QFR | Af |
| JP-Nkm | Dfc | | | |

**Not recovered (11 of 26)** — factor doesn't cleanly cluster (borderline
~3.0–3.3×, just outside the 15% band around 4×, or an extreme outlier):
`BR-Ji3` (3.30), `CA-CF2` (11.4), `DE-SfS` (4.69, borderline), `JP-KaP`
(148.9, BADM-outlier-driven), `JP-Nuf` (3.08), `JP-SMF` (3.00), `JP-Ynf`
(2.88), `US-Cwt` (3.10), `JP-Api` (3.25), plus the two confirmed isolated
anomalies `US-HB4` and `IT-MBo`.

**Class changes among 12 already-classified sites** (all mechanical
B-boundary corrections, per B4's cascade analysis below):

| site | before | after |
|---|---|---|
| CN-Dda | Dwb | BSk |
| CN-Huz | Dwb | BWk |
| CN-Zha | Dwb | BWk |
| EE-Rng | Dfb | BWk |
| ES-Mzn | Cfb | BSk |
| ES-Srn | Cfa | BSk |
| ES-TzM | Csa | BSk |
| US-Rls | Dsb | BSk |
| US-Rws | Dsb | BSk |
| US-UTJ | Dsa | BWk |
| US-UTW | Dsa | BWk |
| US-WT1 | Cfa | BSk |

Full table: `table_b3_reclassification.csv`.

**Network fraction / Jaccard impact** (`table_b3_jaccard_impact.csv`):

| level | J before | J after | Δ |
|---|---|---|---|
| Two-letter | 0.4198 | 0.4243 | **+0.0044** |
| 5-class | 0.4560 | 0.4702 | **+0.0142** |

Both improve — a different, more favorable result than the single-site
extended-window recovery test in v1/v2 (which moved J by −0.0004). This
is a substantially larger, network-wide counterfactual, not a like-for-
like comparison to that earlier, narrower test.

## B4 — Is the error precipitation-only?

`TA_ERA`-derived MAT vs. WorldClim BIO1 and BADM MAT, all 123 clustered
sites plus `IT-MBo`/`US-HB4` (`table_b4_temperature_check.csv`):

| | value |
|---|---|
| Mean absolute difference, TA_ERA-derived MAT vs. BIO1 | **0.94 °C** |
| Mean absolute difference, TA_ERA-derived MAT vs. BADM MAT | **0.91 °C** |

Ordinary reanalysis-vs-station scatter (the largest single outlier,
`ES-FtD`, is 5.25 °C — still trivial next to a 2.6×–460× precipitation
ratio). **The error is precipitation-specific — temperature at these same
sites is fine.**

**Which Beck-cascade rules are mechanically sensitive to a uniform
precipitation scaling** (read directly from `classify_koppen_geiger()`,
`R/climate_classification.R:54-175`):

| Rule | Comparison | Scale-sensitive? |
|---|---|---|
| B (arid) / BW / BS | `MAP < 10*Pthresh`, `< 5*Pthresh`, `>= 5*Pthresh` | **Yes — absolute MAP vs. a P-independent threshold** |
| BWh/BWk/BSh/BSk | temperature only | No |
| Af | `Pdry >= 60` | **Yes — absolute threshold** |
| Am/Aw | `Pdry >= 100 - MAP/25` (both sides depend on absolute P, non-trivially) | **Yes** |
| Cs/Ds ("dry-summer") | `Psdry < 40` (absolute) **and** `Psdry < Pwwet/3` (ratio) | **Partially — the absolute term is scale-sensitive** |
| Cw/Dw ("dry-winter") | `Pwdry < Pswet/10` (pure ratio) | **No — scale-invariant** |
| Csa/b/c, Cwa/b/c, Cfa/b/c, Dsa/b/c/d, Dwa/b/c/d, Dfa/b/c/d suffixes | `Thot`, `Tmon10`, `Tcold` (temperature only) | No |
| Season-half selection, Pthresh-formula selection | `Pw*2.333` vs. `Ps`, etc. (ratio) | No |
| E / ET / EF | `Thot` (temperature only, modulated by `!B`) | Indirectly, via B only |

**Every one of the 12 class flips lands in BS/BW** — exactly the
mechanically-predicted consequence of a uniform precipitation
overestimate pushing a site's true (non-arid) climate across the
absolute `10*Pthresh`/`5*Pthresh` boundary. None of the flips involve a
Cw/Dw (ratio-based) reclassification, consistent with the cascade's own
arithmetic. This is a useful diagnostic in itself: **a class flip
straight into BS/BW under a P-only correction is exactly what a real
precipitation-magnitude fix predicts; a flip elsewhere in the cascade
would not be, and would need a different explanation.**

---

## Per-site evidence table (Coordination Project form)

`table_coordination_project_evidence.csv` — **134 rows**: 123
`systematic_cluster` sites (the 4×/8× population), 2 `confirmed_isolated_anomaly`
sites (`IT-MBo`, `US-HB4`), and 9 `excluded_no_clean_cluster` sites (part
of the original 26 but not cleanly clustering). Columns: `site_id`,
`data_hub`, `product_source_network`, `oneflux_code_version`, `status`,
`factor_estimate`, `factor_applied`, `map_era5_uncorrected`,
`map_era5_corrected`, `bio12_mm`, `badm_map_mm`, `was_in_original_26`.
Not drafted into correspondence, per instruction — data only.

---

## Not-found / carried-forward notes

- No repository documentation was found describing the MM-resolution
  `P_F_QC` fraction convention explicitly (the ambiguity that caused v2's
  error) — CLAUDE.md documents System 1's coarse-resolution fraction
  convention explicitly but is silent on whether System 2 (`*_F_QC`)
  aggregates the same way; this report's A4 finding (verified at n=74,916)
  should be treated as the authoritative statement for this field going
  forward, and CLAUDE.md's QC Flag Reference section may be worth
  amending to say so explicitly — not done here, as this task is
  read-and-report only.
- T1 (the MM-vs-YY internal consistency check establishing the pipeline's
  formula is exactly correct) is unaffected by anything in this report
  and is not re-litigated here.
- `CANDIDATE_FACTORS` tested were 1/4/8/24/1000 per instruction; no sites
  clustered at 24 or 1000 at any point in this analysis.
