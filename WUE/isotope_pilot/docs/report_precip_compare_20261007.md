# ERA5 vs. gauge daily precipitation comparison (2026-10-07)

Side analysis, not the FLUXNET Annual Paper 2026, and not stage 2 of the WUE isotope pilot
itself -- a read-and-report comparison requested before deciding whether to launch the stage 2
full run. Describes what was measured; asserts no cause; proposes, recommends, and applies no
threshold; does not change the rain screen; does not recompute WUE.

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

Per site: share of `P_F_QC==2` records where `P_F` equals `P_ERA` (expected 100%), and share of
`P_F_QC==0` records with non-zero `P_F` where `P_F` equals `P_ERA` (expected 0%) -- confirming the
2026-09-20 SESSION_LOG identity at these 12 sites specifically (that entry checked 3 different
sites: IT-MBo, US-HB4, FI-Hyy).

| site_id | n_qc2 | pct_qc2_eq_era | n_qc0_nonzero | pct_qc0_nonzero_eq_era | departs_from_expected |
|---|---|---|---|---|---|
| US-Ha1 | 124344 | 100 | 17112 | 0.0701 | TRUE |
| US-Ho2 | 43442 | 100 | 32639 | 0.095 | TRUE |
| US-MMS | 3811 | 100 | 17648 | 0.1133 | TRUE |
| US-SP1 | 152259 | 100 | 8636 | 0 | FALSE |
| US-Bar | 6389 | 100 | 23138 | 0.0475 | TRUE |
| US-Slt | 140337 | 100 | 9782 | 0.0613 | TRUE |
| US-Dk2 | 0 | NA | 8761 | 0.0685 | TRUE |
| US-Fuf | 24784 | 100 | 3455 | 0.0579 | TRUE |
| DE-Tha | 31592 | 100 | 49895 | 0.1623 | TRUE |
| BE-Vie | 37014 | 100 | 73905 | 0.2666 | TRUE |
| NL-Loo | 382780 | 100 | 39942 | 0.1227 | TRUE |
| FI-Hyy | 151634 | 100 | 56722 | 0.4319 | TRUE |

**Result: 11 of 12 site(s) depart from the expected pattern** -- not at the `P_F_QC==2` side (100% everywhere `n_qc2 > 0`; `US-Dk2` has zero `P_F_QC==2` records, hence `NA`), but at the `P_F_QC==0` non-zero side, where a small share (0.05-0.43% of measured non-zero gauge records across the affected sites, `US-SP1` the only exact 0%) happen to equal `P_ERA` exactly.

Departing sites: US-Ha1, US-Ho2, US-MMS, US-Bar, US-Slt, US-Dk2, US-Fuf, DE-Tha, BE-Vie, NL-Loo, FI-Hyy.

Per instructions, this stops the analysis here: items 1-9 and the figures were not computed.
No cause is asserted for the small non-zero match rate (it could be coincidental rounding --
both series can land on the same value by chance when the gauge's own resolution is coarse --
or something else; that question is not investigated here).

## What I could not do

- Items 2-9 (totals, gauge resolution, wet-day frequency, agreement, P_ERA on gauge-dry days,
  frequency-matching amount, duration, rain-rule consequence) and figures a-c: not computed.
  The identity check (item 0, confirming the prerequisite 2026-09-20 identity) did not pass for
  11 of 12 sites, and per instructions ("Stop and tell me if a site departs from 100% and 0%")
  this analysis stops there rather than proceeding on an unconfirmed foundation.
