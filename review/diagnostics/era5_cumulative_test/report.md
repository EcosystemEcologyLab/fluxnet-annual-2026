# ERA5 within-year cumulative-total hypothesis: read-only test

Read-only test, run on the mini (`data/extracted/` complete, 781 sites; hostname
`setanta.local`). Reads the raw `*_FLUXNET_ERA5_MM_*.csv` files directly — **not
DuckDB** — so ingestion (`03_read.R`/`duckdb_setup.R`/`duckdb_update.R`) plays no part.
No edits to anything: confirmed via `git status` that only new files under
`review/diagnostics/era5_cumulative_test/` were written.

Script: `scripts/diagnostics/era5_cumulative_test.R`.

## Hypothesis

The monthly `P_ERA` series at the affected sites (the 4x/8x cluster from
`review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv`) may be a
within-year cumulative total rather than a monthly value, which would inflate the
annual sum by a rainfall-timing-dependent factor (~6.5 for evenly spread rain, ~4-5 for
June-September rain, lower for late-season rain).

## Bottom line

**The within-year cumulative-total pattern is absent.** It is absent at all six named
affected sites and absent network-wide in the 4x/8x cluster as a group. Every direct
test — the raw printed values, the figure, and the Dec/Jan-ratio/rho statistics —
points the same way. The one calculation that superficially lands near the observed
ratios (step 3) does so despite violating its own core assumption in the data, for
reasons given below, and should not be read as support.

## Step 1 — raw monthly values (see `table_step1_raw_monthly.csv` and the figure)

Printed month-by-month for the most recent run of 3 consecutive complete calendar
years at each of the 9 named sites (values are `P_ERA` exactly as stored in the raw
file, mm/day). At every one of the 6 affected sites, the within-year series **rises
and then falls** — a seasonal hump (or, at `PE-QFR`/`BR-Ji3`, a trough) — not a
monotonic ramp. Example, `JP-Tak`, 2023: `13.1, 16.4, 19.2, 27.7, 36.8, 40.1, 47.0,
16.3, 19.9, 18.6, 24.3, 19.4` — up through July, then a sharp drop at August, then
fluctuating. A genuine within-year cumulative total of a non-negative quantity cannot
decrease month to month; every affected site's printed series decreases repeatedly
within every printed year. The small-multiples figure
(`fig_monthly_p_era_small_multiples.png`) shows this directly: the 6 affected panels
each show a clear seasonal hump/trough shape, not a staircase.

## Step 2 — Dec/Jan ratio and Spearman rho(value, month), network-wide, by cluster

Computed per complete calendar year, every site with a readable raw ERA5 MM file
(781/781 sites read successfully). A genuinely cumulative series resetting each January
would show a **large** December/January ratio and rho **near 1**; a monthly series
shows neither.

| Group | n site-years | n sites | Dec/Jan ratio (median, IQR) | rho (median, IQR) |
|---|---|---|---|---|
| In 4x/8x cluster | 5,507 | 123 | 1.128 [0.706, 1.871] | 0.119 [-0.077, 0.315] |
| Not in cluster | 29,203 | 657 | 1.096 [0.630, 1.900] | 0.077 [-0.154, 0.287] |

Neither group shows the predicted signature. Both medians are close to the null
values (ratio ≈ 1, rho ≈ 0), and the cluster group is not meaningfully different from
the rest of the network on either statistic. This is the cleanest test in this set and
it refutes the hypothesis directly, network-wide.

## Step 3 — quantitative prediction: differencing (see `table_step3_site_summary.csv`, `table_step3_siteyear.csv`)

Per instruction, for each affected site and complete year: `raw_mm = P_ERA *
days_in_month` (the pipeline's own day-weighting); `implied_mm` = the positive part of
the month-to-month difference (Jan keeps its own raw value); `implied_annual_mm =
sum(implied_mm)`; `predicted_factor = sum(raw_mm) / implied_annual_mm`.

| Site | n years | predicted_factor (mean) | observed ratio_to_bio12 | difference |
|---|---|---|---|---|
| BR-Ji3 | 45 | 3.114 | 3.303 | -0.19 |
| JP-Mse | 45 | 3.557 | 3.969 | -0.41 |
| JP-Tak | 45 | 3.789 | 4.880 | -1.09 |
| JP-Yms | 45 | 3.263 | 3.271 | -0.01 |
| KH-Kmp | 45 | 4.241 | 3.872 | +0.37 |
| PE-QFR | 45 | 5.066 | 3.805 | +1.26 |

**These do not agree closely across sites.** Two of six sites differ from their
observed ratio by more than 1 (`JP-Tak`, `PE-QFR`); the differences range from -1.09 to
+1.26 with no consistent sign. Where they do land close (`JP-Yms`, `BR-Ji3`), that
should be read in light of the following:

**The differencing method's premise does not hold in this data.** A true cumulative
series has at most a handful of negative month-to-month differences per year (rounding
noise). Here, every one of the 45 years at every one of the 6 affected sites has
**3 to 8 negative differences out of the 11 possible** (Feb-Dec vs. the prior month) —
roughly half the year, every year, at every site (`n_negative_diffs` column,
`table_step3_siteyear.csv`). That is exactly what step 1's seasonal hump/trough shape
predicts, and it is inconsistent with the reconstruction's own assumption. Because the
method keeps every positive step in full but clips every negative step to zero, it
mechanically produces a `predicted_factor` > 1 for *any* seasonally-varying monthly
series, cumulative or not — it does not discriminate between the two. The rough
magnitude overlap with `ratio_to_bio12` for some sites is consistent with that generic
upward bias, not with genuine cumulation.

## Step 4 — implied annual total vs. BIO12/BADM (see `table_step4_implied_vs_references.csv`)

Near-1x normal-scatter band (5th-95th percentile among `table_b1_factor_estimates.csv`
sites with `nearest_cluster == "1"`, n = 323): ratio_to_bio12 `[0.831, 1.149]`;
ratio_to_badm_map `[0.856, 1.133]`.

| Site | implied/BIO12 | within band? | implied/BADM | within band? |
|---|---|---|---|---|
| BR-Ji3 | 1.061 | yes | — (no BADM) | — |
| JP-Mse | 1.140 | yes | 1.242 | no |
| JP-Tak | 1.355 | **no** | 1.075 | yes |
| JP-Yms | 1.037 | yes | 1.423 | no |
| KH-Kmp | 0.965 | yes | 1.103 | yes |
| PE-QFR | 0.779 | **no** | — (no BADM) | — |

Mixed: within band for BIO12 at 4/6 sites, within band for BADM at 2/4 sites that have
a BADM value. Given step 3's premise is already shown not to hold, this partial
agreement should not be read as corroboration — the implied-annual-total values it is
built from inherit the same flawed premise.

## Step 5 — same test on TA_ERA, 9 named sites (see `table_step5_siteyear_stats_TA_ERA.csv`)

| Group | n site-years | n sites | Dec/Jan ratio (median) | rho (median) |
|---|---|---|---|---|
| Affected | 270 | 6 | 1.010 | 0.357 |
| Control | 135 | 3 | 0.793 | 0.203 |

TA_ERA shows no large Dec/Jan ratio and no rho near 1 at either group — no within-year
ramp, as expected for an averaged variable. This is consistent with, but does not add
independent weight to, the step-2 finding: `P_ERA` did not show the ramp signature
either, so there is no P-vs-TA contrast to point to here — both variables are equally
free of it.

## Plain statement

The within-year cumulative-total pattern is **absent** — at all six affected sites and
at the 4x/8x cluster as a whole. The raw values (step 1, and the figure) show ordinary
rising-and-falling seasonal cycles, which a true cumulative total cannot produce. The
most direct statistical test (step 2) shows no difference between the cluster and the
rest of the network on either the Dec/Jan ratio or rho(value, month), and neither is
near the value a cumulative series would produce. Step 3's numerical overlap with
`ratio_to_bio12` is not read as support, because the calculation's own diagnostic
(`n_negative_diffs`) shows its premise is violated in every affected site-year. Nothing
here extends to why the 4x/8x factor exists — this test only speaks to whether it is
explained by within-year cumulation, and it is not.

## Files

| File | Contents |
|---|---|
| `table_step1_raw_monthly.csv` | Raw monthly P_ERA, 9 named sites, 3 consecutive complete years each |
| `fig_monthly_p_era_small_multiples.png` | Small multiples of the above |
| `table_step2_siteyear_stats_P_ERA.csv` | Per site-year Dec/Jan ratio and rho, all 781 sites, tagged by cluster |
| `table_step2_summary_P_ERA.csv` | Step 2 summary by cluster membership |
| `table_step3_siteyear.csv` | Per site-year differencing reconstruction, 6 affected sites |
| `table_step3_site_summary.csv` | Step 3 per-site means vs. observed ratio_to_bio12 |
| `table_step4_implied_vs_references.csv` | Implied annual total vs. BIO12/BADM and the near-1x band |
| `table_step5_siteyear_stats_TA_ERA.csv` | Per site-year Dec/Jan ratio and rho for TA_ERA, 9 named sites |

All CSV/PNG outputs have a companion `.meta.json` per repository convention.
