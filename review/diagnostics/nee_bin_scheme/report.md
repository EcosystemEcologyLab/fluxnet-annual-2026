# A common seven-bin NEE scheme for Geo-vs-Data and Geo-vs-Geo (Figs 4/5)

**Type:** Read-and-report diagnostic + new diagnostic code. Read-only with
respect to the pipeline, `R/pipeline_config.R`, and every committed figure.
New code: `scripts/diagnostics/nee_bin_scheme.R`. No existing script, figure,
legend, snapshot CSV, or `representativeness_metrics.csv` was modified.
Outputs and run log (`logs/nee_bin_scheme_*.log`) in this directory.

**Geo vs Data** = site-measured NEE (`nep_median` from
`site_flux_medians_{shuttle,fluxnet2015}.csv`, sign-converted) vs. the global
TRENDY area distribution — the axis built by
`scripts/figure_representativeness_supp_sitelevel.R`. Covers current_781 and
FLUXNET2015 only; La Thuile and Marconi have no downloaded flux time series
in this repo.

**Geo vs Geo** = TRENDY sampled at tower coordinates vs. the same global
TRENDY area distribution — the axis built by
`scripts/figure_representativeness_trendy_compute.R` /
`scripts/extract_historical_sites_representativeness.R`. Covers all four
networks (Marconi, La Thuile, FLUXNET2015, current_781).

---

## Verdict

**None of the three schemes is satisfactory as a shared replacement for the
existing absolute (Scheme 1) convention.** Signing the axis does not fix Geo
vs Data — it makes the Jaccard *worse* (0.233 -> 0.11-0.15) because it adds a
second, independent source of mismatch (measured towers are predominantly
net sinks; the TRENDY grid-cell signed mean collapses toward carbon balance
almost everywhere) on top of the already-documented magnitude mismatch. And
it does not give Geo vs Geo a *meaningful* improvement — the apparent rise in
Jaccard (0.493 -> 0.59-0.95, increasing with half-width) is a **saturation
artifact**: the global near-zero bin swallows 63-96% of land area as
half-width grows, and at the largest half-width tested (50 gC m-2 yr-1) the
two outermost bins hold **0% of TRENDY-at-site values for every network** —
six of seven bins carry essentially no information. Scheme 3 (no
zero-anchor) has the same underlying problem in a different guise: 22.5% of
global land area falls inside a single 0.2 gC m-2 yr-1-wide bin at the
scheme's own quantile-derived breakpoints, because the signed global
distribution is sharply peaked at zero. **Recommendation: keep Scheme 1 (the
existing absolute convention) for both Fig 4/5 versions, with the caveat
already on record from the 2026-09-17 diagnostic** (`review/diagnostics/nee_et_site_vs_trendy/report.md`,
Option 3) — do not adopt a signed scheme for this purpose.

---

## Section 1 — Reproduction check (required before proceeding)

Both cited values reproduced exactly under Scheme 1 before any new
computation was trusted (`table_reproduction_check.csv`):

| Version | Network | Computed J | Target J | Target source | Reproduced |
|---|---|---|---|---|---|
| Geo vs Geo | current_781 | 0.4931868270307604 | 0.4931868270307604 | `data/snapshots/representativeness_metrics.csv:189` (row `trendy_nee_median,7bin_hybrid,7,...,current_781,781`; also `logs/trendy_analysis_complete.marker`, "trendy_nee_median J=0.4932") | **Yes**, exact |
| Geo vs Data | current_781 | 0.232669 | 0.233 | `SESSION_LOG.md:1337` | **Yes**, to stated precision |

The Geo vs Geo reproduction additionally cross-checked the freshly computed
bin assignment against the already-stored `trendy_nee_median_bin` column in
`data/snapshots/site_trendy_nee_median.csv`: **0 mismatches across 781
sites**. The Geo vs Data reproduction independently re-derives the current_781
"nee" panel of `scripts/figure_representativeness_supp_sitelevel.R` (its
`load_measured_axis()`/`count_sites()`/`compute_repr_metrics()`, lines
206-301, 332-333) using `abs(nep_median)` against the stored
`trendy_nee_median_global_distribution.csv` 7-bin edges — the same
methodology `review/diagnostics/nee_et_site_vs_trendy/` used to first surface
this number.

## Section 2 — NBP/NEE definition and sign convention

Read directly from `scripts/figure_representativeness_trendy_compute.R`
(unmodified):

- The global `nee_median` axis uses TRENDY variable **`nbp`**, converted to
  gC m-2 yr-1 by unit conversion only — no sign flip (`load_annual()`,
  lines 138-196).
- The stored `trendy_nee_median.tif` is the **ensemble-median-across-16-models
  of (mean of `|nbp|` across the 34 annual layers 1990-2023, per model)** —
  i.e. `mean(|NBP_annual|)`, not `|mean(NBP_annual)|`. Function
  `compute_mean_abs` (lines 227-238), invoked at line 381
  (`compute_ensemble("nee_median", "nbp", compute_mean_abs)`). This confirms
  the "mean-of-absolute" convention already documented in
  `review/diagnostics/nee_et_site_vs_trendy/report.md` (T1/T3).
- TRENDY's own sign convention: **`nbp` positive = net land carbon
  sink/uptake, negative = net source/release** — confirmed by the code's own
  comment at line 543 ("nbp can be negative (source)").

Standard eddy-covariance NEE convention is the **opposite** sign sense:
positive NEE = net release to atmosphere (source), negative NEE = net
uptake (sink); by definition NEE = -NEP. This diagnostic therefore defines,
for all signed work below:

```
NEE_signed_global = -1 x (ensemble-median per-cell mean-annual nbp)
NEE_signed_site   = -1 x nep_median
```

`nep_median`'s units are confirmed gC m-2 yr-1 by
`data/snapshots/site_flux_medians_shuttle.csv.meta.json`:
`"unit_nep_gpp_ter": "gC m-2 yr-1 (pre-integrated YY product; NEP = -NEE)"` —
so `NEE_signed_site` is directly comparable, same units and same sign sense
(positive = source), to `NEE_signed_global`.

The stored `nee_median` raster is absolute-valued, so the **signed**
equivalent was computed fresh (not by inverting the stored raster, which has
already lost sign information). `scripts/diagnostics/nee_bin_scheme.R`
reuses the already-cached per-model regridded intermediates at
`data/external/trendy/derived/intermediate/<MODEL>_nbp_regridded.tif`
(0.5 deg, 720x360, 34 layers, gC m-2 yr-1, already KG-land-masked — **Step 1
regridding was not rerun**) for the same 16-model ensemble that produced
`trendy_nee_median.tif` (per `logs/trendy_analysis_complete.marker`:
CABLE-POP, CLASSIC, CLM, DLEM, ED, ELM-FATES, IBIS, ISAM, JULES-ES,
LPJ-GUESS, LPJml, LPJwsl, LPX-Bern, ORCHIDEE, TEM, VISIT-UT), applying
`rowMeans()` without `abs()` (the same `compute_mean()` already used for
`et_median`, lines 241-252) instead of `compute_mean_abs()`, then taking the
ensemble median across models and flipping sign. Result:
`trendy_nee_signed_mean.tif` (this directory), global land range
**[-224.98, 480.49] gC m-2 yr-1**, total land area 163,331,649 km2 — this
matches the *existing* `trendy_nee_median_global_distribution.csv`'s own
total exactly (163,331,648.67 km2), confirming the same land mask and
per-model "complete years" criterion were reproduced faithfully.

## Section 3 — The three schemes

All bin edges are set from the global area-weighted distribution only, as
instructed; site classification never influences bin edges.

**Scheme 1 — existing absolute convention.** Unchanged stored edges from
`trendy_nee_median_global_distribution.csv`: a `[0, 5)` near-zero bin, then
six equal-land-area quantile bins of `|NEE|` magnitude up to an open-ended
top bin at `>70.2 gC m-2 yr-1`.

**Scheme 2 — signed, sign-anchored.** One near-zero bin `[-h, h]` (tested
`h` = 10, 25, 50 gC m-2 yr-1), then three equal-area quantile bins on the
sink side (`< -h`) and three on the source side (`> h`), outer bins
open-ended. Quantile construction mirrors `make_bins()` in
`figure_representativeness_trendy_compute.R:436-449` (equal cumulative-area
targets), applied separately to each side of the signed area histogram.
Resulting breaks (gC m-2 yr-1):

| h | Sink-side breaks | Source-side breaks |
|---|---|---|
| 10 | -27.4, -16.8, (-10) | (10), 20.6, 44.8 |
| 25 | -42.2, -31.2, (-25) | (25), 40.1, 63.1 |
| 50 | -65.4, -56.2, (-50) | (50), 63.1, 88.0 |

**Scheme 3 — signed equal-area septiles, no zero anchor.** Seven equal-area
bins across the full signed distribution: breaks at -22.4, -11.4, -4.9,
-0.9, -0.1, 0.1 gC m-2 yr-1. Note bin 6, `[-0.1, 0.1]` — a 0.2 gC m-2 yr-1-wide
sliver — alone holds 22.5% of global land area (see Section 4), an
unavoidable consequence of quantiles derived from a distribution sharply
peaked at zero, not a construction bug (verified by rerunning with a
0.1 gC m-2 yr-1 histogram step, 10x finer than the original script's own
histogram granularity).

## Section 4 — Results

### 4a. Global area fraction per bin

| Bin | Scheme 1 (abs) | Scheme 2, h=10 | Scheme 2, h=25 | Scheme 2, h=50 | Scheme 3 |
|---|---|---|---|---|---|
| 1 | 0.240 | 0.103 | 0.040 | 0.008 | 0.143 |
| 2 | 0.126 | 0.103 | 0.040 | 0.008 | 0.142 |
| 3 | 0.126 | 0.104 | 0.041 | 0.008 | 0.143 |
| 4 | 0.127 | **0.625** | **0.842** | **0.957** | 0.140 |
| 5 | 0.128 | 0.022 | 0.012 | 0.006 | 0.062 |
| 6 | 0.126 | 0.022 | 0.013 | 0.006 | **0.225** |
| 7 | 0.127 | 0.022 | 0.013 | 0.006 | 0.144 |

(Full precision in `table_scheme{1,2,3}_global.csv`.) Scheme 2's near-zero
bin (row 4) dominates increasingly as half-width grows — a direct
consequence of the signed ensemble-median field being close to carbon
balance almost everywhere (consistent with the 2026-09-17 diagnostic's T3
finding that `mean(|annual|)` is inflated ~3-4x relative to
`|mean(annual)|` by interannual sign-cancellation). Scheme 3's bin 6 shows
the same concentration in a different form.

### 4b. Weighted Jaccard and outer-two-bin share (site fraction in bins 1+7)

| Version | Network | Scheme 1 | Scheme 2, h=10 | Scheme 2, h=25 | Scheme 2, h=50 | Scheme 3 |
|---|---|---|---|---|---|---|
| Geo vs Geo | marconi | 0.381 | 0.516 | 0.759 | 0.918 | 0.457 |
| Geo vs Geo | la_thuile | 0.407 | 0.523 | 0.752 | 0.948 | 0.452 |
| Geo vs Geo | fluxnet2015 | 0.495 | 0.574 | 0.812 | 0.927 | 0.529 |
| Geo vs Geo | current_781 | **0.493** | 0.592 | 0.802 | **0.954** | 0.565 |
| Geo vs Data | fluxnet2015 | 0.211 | 0.148 | 0.112 | 0.107 | 0.215 |
| Geo vs Data | current_781 | **0.233** | 0.139 | 0.121 | 0.125 | 0.222 |

Outer-two-bin share (bins 1+7 combined, share of *sites*, current_781):

| Version | Scheme 1 | Scheme 2, h=10 | Scheme 2, h=25 | Scheme 2, h=50 | Scheme 3 |
|---|---|---|---|---|---|
| Geo vs Geo | 10.1% | 21.6% | 7.7% | **0.0%** | 36.2% |
| Geo vs Data | 60.8% | 71.6% | 64.8% | 59.2% | 77.2% |

(Full per-bin, per-network detail in `table_scheme{1,2,3}_sites.csv` and
`table_scheme{1,2,3}_jaccard.csv`.)

**Reading these together:** Geo vs Geo's Jaccard rises monotonically with
Scheme 2's half-width, peaking at 0.95 for h=50 — but that network's own
outer-two-bin share simultaneously falls to **exactly zero**, meaning bins 1
and 7 (the two "extreme flux regime" bins the scheme was built to
distinguish) are empty of sites for *every* network at h=50. The high
Jaccard reflects near-total collapse of both the global and site
distributions into a single central bin, not genuine agreement across a
meaningful range of flux regimes. Geo vs Data moves the opposite direction:
every signed variant is *worse* than Scheme 1, and the outer-two-bin share
for measured sites *rises* to 71-77% under Schemes 2/3 (vs. 61% under
Scheme 1) — measured towers, which are predominantly net carbon sinks
(negative NEE by the sign convention here), pile into the sink-side extreme
bin at a much higher rate than TRENDY-at-site's near-zero-dominated signed
distribution ever does, so signing *adds* a distributional-shape mismatch on
top of the pre-existing magnitude mismatch rather than resolving it.

## Section 5 — Figures

- `fig_scheme1_bin_fractions.png` — global area fraction vs. current_781 site
  fraction per bin, Geo vs Geo and Geo vs Data side by side.
- `fig_scheme2_bin_fractions.png` — as above, faceted additionally by
  half-width (10/25/50).
- `fig_scheme3_bin_fractions.png` — as above.

All three make the same point visually: for Geo vs Geo, the "global area"
bars and "site fraction" bars converge toward being dominated by the same
single central bin as signed schemes are used; for Geo vs Data, the site
bars remain concentrated at an outer bin regardless of scheme, while the
global bars are near-uniform (Scheme 1) or spike at center (Schemes 2/3) —
the two distributions never actually take the same shape.

## Recommendation

**Keep Scheme 1** (the existing absolute-magnitude, mean-of-`|flux|`
convention, unchanged stored edges) for both Fig 4/5 versions. Neither
signed alternative is an improvement:

- Scheme 2 (signed, zero-anchored) only *appears* better for Geo vs Geo, and
  that appearance is a saturation artifact of the near-zero bin absorbing
  more and more global area (and, correspondingly, more and more TRENDY-at-site
  values) as half-width grows — at the widest half-width tested, two of the
  scheme's seven bins carry no site information at all, for any network.
  For Geo vs Data it is strictly worse (J drops from 0.233 to 0.11-0.15 at
  current_781) at every tested half-width.
- Scheme 3 (signed septiles, no zero anchor) avoids the half-width
  sensitivity but inherits the same underlying pathology in a fixed form:
  22.5% of global land area collapses into a single narrow bin at the
  scheme's own zero-crossing quantile, and it is also worse than Scheme 1
  for Geo vs Data (0.222 vs. 0.233) while offering only a modest, likely
  non-robust improvement for Geo vs Geo (0.565 vs. 0.493) bought at the cost
  of that degenerate central bin.

The root cause is physical, not a binning-construction choice: the
underlying **signed** ensemble-median mean-annual NBP field is close to
carbon balance at most 0.5 deg grid cells (the same interannual/cross-model
cancellation effect the 2026-09-17 diagnostic already quantified at ~3-4x
for the *unsigned* statistic), so any bin scheme built on it — sign-anchored
or not — will concentrate most of the globe into a narrow band near zero.
Measured tower NEE, by contrast, is predominantly and legitimately signed
toward net uptake. No seven-bin signed scheme derived purely from the
global TRENDY area distribution can represent both of those facts at once
without either saturating (Scheme 2) or producing a degenerate central bin
(Scheme 3). This is the same conclusion the 2026-09-17 diagnostic reached
about the *unsigned* axis for a different reason (bin saturation from a
scale/statistic mismatch) — signing the axis does not sidestep that
conclusion, it reproduces it in a new form.

If a genuinely comparable Geo-vs-Data axis is wanted in the future, the
2026-09-17 report's Option 2 (drop the binned/Jaccard framing for NEE/ET
entirely in favor of a paired measured-vs-TRENDY-at-site comparison, as its
own T7 figure already does) remains the more promising path — it sidesteps
the global-distribution-shape problem this diagnostic confirms is
fundamental, rather than trying to re-bin around it.

---

## Outputs in this directory

- `table_reproduction_check.csv` — Section 1.
- `trendy_nee_signed_mean.tif` — new signed ensemble-median mean-annual-NBP
  raster (Section 2).
- `table_scheme{1,2,3}_global.csv` — global area fraction per bin, per scheme.
- `table_scheme{1,2,3}_sites.csv` — site count/fraction per bin, per network,
  per version (geo_vs_geo / geo_vs_data), per scheme.
- `table_scheme{1,2,3}_jaccard.csv` — weighted Jaccard and outer-two-bin
  share, per network, per version, per scheme.
- `fig_scheme{1,2,3}_bin_fractions.png` — figures described in Section 5.

Every CSV has a companion `.meta.json`. Full run log:
`logs/nee_bin_scheme_20260924_150838.log`.
