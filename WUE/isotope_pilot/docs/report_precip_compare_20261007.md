# ERA5 vs. gauge daily precipitation comparison (2026-10-07)

Side analysis, not the FLUXNET Annual Paper 2026, and not stage 2 of the WUE isotope pilot
itself -- a read-and-report comparison requested before deciding whether to launch the stage 2
full run. Describes what was measured; asserts no cause; proposes, recommends, and applies no
threshold; does not change the rain screen (MY DECISION 4, any P_ERA above zero, is unchanged);
does not recompute WUE. Revises the 2026-10-07 run of this same script
(`docs/report_precip_compare_20261007.md`), which stopped entirely at an exact-match identity
gate -- the user's own instruction, found too strict, now relaxed as described below.

## Falsifiability statement (written before computing anything)

**What would show P_ERA can stand in for the gauge, under the stage 2 rain rule's "any
precipitation above zero" definition (MY DECISION 4):** on fully measured days, P_ERA and the
gauge classify most of the same days as wet vs. dry at the >0 mm level (few gauge-dry/P_ERA-wet
or gauge-wet/P_ERA-dry days in the agreement table); P_ERA is rarely, and only slightly, positive
on days the gauge recorded as fully dry (a low share_era_positive in era_on_gauge_dry.csv, and
small median/p90/p99 amounts when it is); and the rain-rule day-removal counts in
rain_rule_consequence.csv are similar whether driven by the gauge or by P_ERA.

**What would show it cannot:** a large share of gauge-dry fully measured days carry non-trivial
positive P_ERA (so the >0 rule would exclude many days the gauge alone would have kept); the two
series disagree substantially on wet/dry classification at the >0 level; or the rain-rule
day-removal counts diverge materially between the two sources in rain_rule_consequence.csv.

This script does not apply either verdict -- it only reports the numbers above would need for
someone else (or a later session) to decide.

## 1. Identity check

Per site: share of `P_F_QC==2` records where `P_F` equals `P_ERA` (expected 100%), share of
`P_F_QC==0` records with non-zero `P_F` where `P_F` equals `P_ERA` (expected <= 1%, not exactly
0% -- the 2026-10-07 run's exact-0% requirement was too strict), and the 5 most frequent matching
values where `P_F_QC==0` non-zero matches occur. **A site passes when the `P_F_QC==2` side is
exactly 100% AND the `P_F_QC==0` non-zero side is <= 1%; a failing site is excluded from items 1-9
and the figures below (not a global stop) and named here.**

| site_id | n_qc2 | pct_qc2_eq_era | n_qc0_nonzero | pct_qc0_nonzero_eq_era | top5_matching_values | passes_identity_gate |
|---|---|---|---|---|---|---|
| US-Ha1 | 124344 | 100 | 17112 | 0.0701 | 0.3:8, 0.6:1, 0.8:1, 1.4:1, 2.1:1 | TRUE |
| US-Ho2 | 43442 | 100 | 32639 | 0.095 | 0.1:14, 0.3:7, 0.2:4, 0.4:4, 0.6:1 | TRUE |
| US-MMS | 3811 | 100 | 17648 | 0.1133 | 0.1:12, 0.3:2, 0.4:2, 0.2:1, 0.6:1 | TRUE |
| US-SP1 | 152259 | 100 | 8636 | 0 |  | TRUE |
| US-Bar | 6389 | 100 | 23138 | 0.0475 | 0.254:6, 0.508:4, 1.016:1 | TRUE |
| US-Slt | 140337 | 100 | 9782 | 0.0613 | 0.508:3, 0.254:2, 1.016:1 | TRUE |
| US-Dk2 | 0 | NA | 8761 | 0.0685 | 0.1:1, 0.151:1, 0.182:1, 0.2:1, 0.3:1 | TRUE |
| US-Fuf | 24784 | 100 | 3455 | 0.0579 | 0.04:1, 0.08:1 | TRUE |
| DE-Tha | 31592 | 100 | 49895 | 0.1623 | 0.1:43, 0.2:19, 0.3:9, 0.4:4, 0.5:4 | TRUE |
| BE-Vie | 37014 | 100 | 73905 | 0.2666 | 0.05:56, 0.1:33, 0.01:23, 0.03:12, 0.15:12 | TRUE |
| NL-Loo | 382780 | 100 | 39942 | 0.1227 | 0.2:23, 0.4:8, 0.02:7, 0.01:3, 0.04:3 | TRUE |
| FI-Hyy | 151634 | 100 | 56722 | 0.4319 | 0.01:101, 0.02:29, 0.05:20, 0.03:13, 0.04:12 | TRUE |

**Result: every site passes.**

## Missing P_ERA timesteps

Daily `P_ERA` was previously summed with `na.rm = TRUE`, so a day with no `P_ERA` at all silently
read as a 0 mm (dry) day. Revised: a day's `P_ERA` is now `NA` unless `P_ERA` is present at every
expected timestep, and that is now also required for "fully measured" (in addition to the
existing `P_F_QC == 0` requirement). Full counts by site and year (2026 excluded):
`tables/precip_compare/era_missing_timesteps.csv` (0 missing timestep(s) total across passing sites).

No site-year kept by `years_dropped.csv` has a missing P_ERA timestep.



## Per-site summary, all months

One row per site passing the identity gate. `fully_measured_days` pools all years;
`mean_days_removed_*` covers only site-years with >=350 fully measured days (`rain_rule_consequence.csv`).

| site_id | fully_measured_days | share_gauge_wet | share_era_wet | share_era_positive_on_gauge_dry | median_era_on_gauge_dry_mm | p90_era_on_gauge_dry_mm | freq_matching_amount_mm | ratio_era_to_gauge | mean_days_removed_gauge | mean_days_removed_era |
|---|---|---|---|---|---|---|---|---|---|---|
| US-Ha1 | 7603 | 0.4079 | 0.7556 | 0.6128 | 0.09 | 1.3264 | 0.549 | 1.0001 | 239.25 | 311.2 |
| US-Ho2 | 8869 | 0.398 | 0.7298 | 0.5808 | 0.13 | 2.616 | 0.454 | 0.999 | 220.67 | 299.27 |
| US-MMS | 8888 | 0.3769 | 0.7005 | 0.5386 | 0.137 | 2.0578 | 0.6614 | 0.9998 | 212.68 | 290.37 |
| US-SP1 | 4353 | 0.3375 | 0.753 | 0.6387 | 0.433 | 7.2568 | 2.2263 | 1.0199 | 154 | 291.75 |
| US-Bar | 7168 | 0.38 | 0.7868 | 0.6667 | 0.162 | 2.8944 | 0.9108 | 0.9999 | 224.05 | 313.16 |
| US-Slt | 3639 | 0.3509 | 0.6568 | 0.492 | 0.116 | 2.0818 | 0.714 | 0.9997 | 210.8 | 288.2 |
| US-Dk2 | 2922 | 0.3607 | 0.6215 | 0.4374 | 0.222 | 2.41 | 0.664 | 0.9998 | 197.12 | 273.5 |
| US-Fuf | 1619 | 0.3125 | 0.4688 | 0.2848 | 0.059 | 1.2618 | 0.1296 | 0.9741 | 156 | 195 |
| DE-Tha | 10492 | 0.5532 | 0.8228 | 0.6442 | 0.146 | 1.1042 | 0.394 | 1.0002 | 267 | 318.04 |
| BE-Vie | 10366 | 0.5883 | 0.851 | 0.6809 | 0.208 | 1.68 | 0.432 | 0.9992 | 266.91 | 326.5 |
| NL-Loo | 1231 | 0.6409 | 0.8383 | 0.6222 | 0.136 | 1.3296 | 0.2086 | 1.1256 | 278 | 322.5 |
| FI-Hyy | 7429 | 0.5955 | 0.8686 | 0.6885 | 0.144 | 0.9264 | 0.2912 | 1 | 288.83 | 335.33 |

## Per-site summary, May to September

Same columns, restricted to fully measured days with month in 5:9. `mean_days_removed_*` is an
all-year quantity (item 9 was not computed by period) and is omitted here.

| site_id | fully_measured_days | share_gauge_wet | share_era_wet | share_era_positive_on_gauge_dry | median_era_on_gauge_dry_mm | p90_era_on_gauge_dry_mm | freq_matching_amount_mm | ratio_era_to_gauge |
|---|---|---|---|---|---|---|---|---|
| US-Ha1 | 3213 | 0.4279 | 0.7336 | 0.5773 | 0.133 | 1.689 | 0.4524 | 0.885 |
| US-Ho2 | 3764 | 0.4509 | 0.7242 | 0.5201 | 0.096 | 1.1616 | 0.2949 | 0.8959 |
| US-MMS | 3713 | 0.3725 | 0.7145 | 0.5571 | 0.1715 | 2.419 | 0.7551 | 0.8752 |
| US-SP1 | 1774 | 0.4346 | 0.9087 | 0.8465 | 1.223 | 10.6564 | 3.9623 | 0.8485 |
| US-Bar | 3060 | 0.416 | 0.7621 | 0.5971 | 0.142 | 1.8212 | 0.804 | 0.8772 |
| US-Slt | 1523 | 0.3657 | 0.719 | 0.5745 | 0.158 | 2.564 | 0.8842 | 0.9646 |
| US-Dk2 | 1224 | 0.3824 | 0.7435 | 0.6019 | 0.276 | 2.7304 | 0.9643 | 1.0172 |
| US-Fuf | 735 | 0.4082 | 0.6272 | 0.4161 | 0.046 | 0.966 | 0.1528 | 0.7753 |
| DE-Tha | 4596 | 0.5194 | 0.8192 | 0.6668 | 0.21 | 1.3772 | 0.515 | 0.993 |
| BE-Vie | 4385 | 0.5341 | 0.834 | 0.6721 | 0.252 | 1.678 | 0.6811 | 1.0316 |
| NL-Loo | 557 | 0.526 | 0.7864 | 0.5909 | 0.219 | 1.577 | 0.6074 | 1.1842 |
| FI-Hyy | 3185 | 0.4889 | 0.832 | 0.6757 | 0.238 | 1.3702 | 0.653 | 1.0214 |

## Figures

- `figures/precip_compare/fig_wet_freq_amount.png` -- (a) wet-day frequency vs. amount, both
  series, one panel per site (fully measured days, all months).
- `figures/precip_compare/fig_days_removed_by_source.png` -- (b) days removed per year by the
  rain rule under each source, by site (site-years with >=350 fully measured days).
- `figures/precip_compare/fig_era_on_gauge_dry.png` -- (c) daily P_ERA on gauge-dry days, log
  x-axis, one panel per site.

## What I could not do

Nothing -- all nine COMPUTE items, both per-period summaries, and all three figures were produced for all 12 sites.
