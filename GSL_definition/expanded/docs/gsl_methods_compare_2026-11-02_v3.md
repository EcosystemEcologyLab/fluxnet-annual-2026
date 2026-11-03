# GSL methods comparison — version 3

**Script:** `GSL_definition/expanded/code/11_gsl_methods_compare.R`
**Prompt:** `logs/prompts/2026-11-02_gsl_methods_compare_v3.md`
**Supersedes:** version 2 (committed `e0348a5`; `gsl_methods_compare_2026-11-02_v2.md`), which in turn
superseded version 1 (`15dc51f`).

**Purpose, per Tash**: this comparison exists to show that published GSL definitions span a wide
range of low thresholds and rules — no single definition, Z16 (the network's working method)
included, is "the" reference. Accordingly, nothing below sets up a Z16-vs-T85/T95 framing: M2's and
M4's thresholds are reported as part of an 8-method, multi-threshold landscape, not as two poles of
a comparison.

Eight methods are compared, at the same 15 sites as v1/v2, on two independent GPP series (**NT** =
zero-floored `GPP_NT_REF`, no winter screen; **DT** = zero-floored `GPP_DT_REF`), never mixed:

| Method | Source | What's new in v3 |
|---|---|---|
| M1 | Zscheischler et al. (2016), crossing-span | unchanged |
| M2 | % of P99 (count) | full 22-value grid (0.1, 0.5, 1–20%); kept 5/10/20% in tables |
| M3 | fixed absolute threshold (count) | unchanged |
| M4 | fraction-of-annual-GPP (count, fine grid) | targets 0.85/0.90/0.95/0.99 (adds 0.85) |
| M5 | NEP > 0 span/count | unchanged |
| M6 | Hu et al. (2010) NEP rate-of-change | now reported in anchored AND unanchored forms |
| M7 | **new**: Ma et al. (2013) amplitude/derivative | |
| M8 | **new**: Panwar et al. (2023) normalized threshold | |

AU-How (the one Southern-Hemisphere site) uses Jul–Jun season years in every method, unchanged from
v2.

## Checks

As v2, both passed exactly: NH-site NT-series M2@10%/M4@0.95 == `derived_site_year.csv`'s
`main_unscreened` `gsl_z16`/`gsl_t95`/`f_z16`/`f_t95` (276 rows); every one of 11,058
`gsl_methods_site_year.csv` rows has a GSL value or an explanatory flag.

**New check (M4@0.85 vs. the existing fraction sweep): not applicable.** `tables/fraction_sweep_main.csv`'s
only `variant` value is `"main"` — the winter-*screened* primary series from `GSL_definition/code`
— not `"main_unscreened"`/NT. No NT-series 0.85 fraction sweep exists anywhere else in the
repository to check against. This is exactly the conditional the prompt anticipated ("if that table
holds it"): it doesn't, so the check is reported as not applicable rather than silently skipped.

## Item 1 — thresholds, and M2 as a range

M4 now targets 0.85 (Tash's current working threshold), 0.90, 0.95, 0.99, all via the same fine-grid
mechanism as v1/v2. M2 was run on Tash's full 22-value grid (0.1, 0.5, 1–20% of P99); 5/10/20% are
kept in the main tables (`gsl_methods_site_year.csv`, `gsl_methods_site_summary.csv`), and the full
grid (mean GSL per site × percentage, NT series, 330 rows) is in `tables/gsl_methods_m2_grid22.csv`.
The dot plot shows M2 as its full min–max range across that grid — not a point-and-SD like the other
seven methods (see the dot plot below).

## Items 2, 3 — M7, M8, and three start/end rules for M2–M4

**M7** (Ma et al. 2013): on 15-day-smoothed GPP, start = halfway (by day index) between the
pre-season minimum and the day of fastest green-up (largest positive first difference); end =
halfway between the day of fastest brown-down (largest negative first difference) and the
post-season minimum. Flagged if the peak falls at a window edge, or no valid pre-/post-season
minimum exists.

**M8** (Panwar et al. 2023): smoothed GPP rescaled to [0, 1] by that site-year's own min/max. GSL =
the single contiguous run of days above the threshold (0.10, 0.25, 0.50) that *contains* the day of
the window's maximum — the crossing closest to the seasonal peak.

**Three start/end rules for M2–M4** (NT series), reported side by side in `gsl_methods_site_year.csv`:

- **(a)** first/last qualifying day (`start_date`/`end_date`, unchanged from v1/v2)
- **(b)** first/last run of ≥5 consecutive qualifying days — **our own choice, not from the
  literature** (`start_date_run5`/`end_date_run5`)
- **(c)** Panwar's rule: the smoothed-GPP threshold crossing closest to the peak
  (`start_date_smpeak`/`end_date_smpeak`)

`tables/gsl_methods_rule_shifts.csv` reports the median absolute shift between rules, by site
(NT series, M2–M4 pooled). Rule (a)-vs-(b) shifts are mostly small (median 0–40 d), but rule
(a)-vs-(c) shifts are large and highly site-dependent:

| Site | median shift, start (a vs c) | median shift, end (a vs c) |
|---|---|---|
| US-Wkg | 114 d | 8 d |
| US-SRM | 88.5 d | 19 d |
| US-Ton | 4.5 d | 108 d |
| US-Var | 3 d | 216.5 d |
| IL-Yat | 0 d | 193.5 d |

Inspecting individual rows explains this: at the semi-arid/episodic sites (US-Wkg, US-SRM), rule (a)
is frequently extended to late December by an isolated post-monsoon rain-pulse day that clears a low
(5–20%-of-P99) threshold, while rule (c) anchors to the actual smoothed-GPP peak and its immediate
surroundings and ends months earlier (e.g. US-Wkg 2020: rule (a) end = 2020-12-31, rule (c) end =
2020-09-23). This is a real methodological difference, not a bug: **rule (a) is vulnerable to
isolated rain-pulse blips at water-limited sites; rule (c) is anchored to the growing season's actual
peak.** Mean (not median) shifts are even larger at IL-Yat/US-Var (up to ~200 d) — kept in the table
for transparency, but the median is the headline number here since a handful of extreme site-years
dominate any mean.

## Item 4 — M6, anchored and unanchored

M6 is now reported in two forms (`sub = "anchored"` / `"unanchored"`). "Anchored" is v2's form
(search ±30 d of the M5-span's own start/end). "Unanchored" is v1's own window/search logic, ported
verbatim from `15dc51f` (single global peak of smoothed NEP as pivot; two-stage steepest-then-
shallowest-negative-slope search after the peak; v1's alternate-end is not reproduced, consistent
with v2's decision to drop it).

| | mean GSL | 
|---|---|
| anchored (v2 form) | 220.8 d |
| unanchored (v1 form) | 116.7 d |

**Figures use the anchored form only.** Its Spearman agreement with M5 (0.89 for GSL, 0.80 for
start date, 0.89 for end date) is **partly built in by construction** — the anchored search windows
are centred on M5's own span boundaries — and this is stated in the agreement figure's caption, not
presented as independent confirmation.

## Item 5 — method agreement: Spearman r and median absolute date difference

`tables/gsl_methods_agreement.csv` (280 rows) holds both metrics for every method pair. Spearman r
(site-year anomalies for GSL; day-of-year anomalies for start/end — **not** raw pooled dates, which
would be spuriously ≈1.0 for every pair since absolute calendar dates are dominated by which site/
year a row belongs to, not by genuine method agreement) is shown in the agreement figure. Median
absolute start/end-date differences (days), not shown on the tile plot, are reported here:

**Smallest (most agreeing) start-date pairs**: M2–M3 (0 d), M1–M2 (4 d), M1–M3 (5 d), M1–M4 (9 d),
M4–M5 (9 d) — the count methods and M1 agree closely on when the season starts.

**Largest (least agreeing) start-date pairs**: M3–M8 (60 d), M3–M7 (58 d), M2–M8 (56 d), M2–M7
(55 d), M1–M7 (41 d) — M7 and M8, the two new smoothed-derivative/normalized methods, disagree
substantially with the raw-threshold count methods on start timing.

**End-date pairs** show the same pattern: smallest M2–M3 (0 d), M1–M2 (4 d), M1–M3 (5 d), M1–M5
(11 d), M5–M6 (11 d, the anchored-agreement artifact above); largest M3–M7 (69 d), M2–M7 (65 d),
M3–M8 (57 d), M2–M8 (55 d), M1–M7 (44 d).

## Item 6 — NT vs. DT: signed start/end shifts, by site and vegetation group

`tables/gsl_methods_dt_vs_nt_by_group.csv` / `..._by_site_group.csv`. Positive start shift = DT
starts *later* than NT, i.e. NT's season begins *earlier* — the direction Keenan et al. (2019)
predict for forests, from NT's early-spring overestimate of GPP (an artefact of light inhibition of
daytime respiration not being corrected for in the NT partitioning).

| Vegetation group | n | mean start shift (d) | mean end shift (d) |
|---|---|---|---|
| Deciduous forest | 1,003 | **+9.6** | −6.3 |
| Grassland | 1,054 | **+10.6** | −12.6 |
| Woody-grass mosaic | 1,189 | +0.7 | −2.1 |
| Evergreen forest | 1,654 | +0.7 | +0.2 |

**The data partially agree with Keenan et al. (2019), and the partial pattern is itself informative.**
Deciduous forest shows the predicted positive start shift, but it is driven almost entirely by
US-Ha1 (+16.3 d) and US-MMS (+13.7 d); the third deciduous site, JP-Api, shows a small *negative*
shift (−1.4 d). Evergreen forest — the other forest group, and nominally also subject to the same
NT-partitioning artefact — shows essentially **no** shift (+0.7 d, pooled across CH-Dav, DE-Tha,
FR-Pue, IL-Yat, NL-Loo, all individually small and mixed in sign). This is consistent with Keenan et
al.'s mechanism being specifically about *early-spring leaf-out*: deciduous canopies have an abrupt,
strongly light-responsive green-up that the NT bias would most affect, while evergreen canopies have
no comparable leaf-out transition for the bias to act on. Grassland shows the largest shift of any
group (+10.6 d, not predicted by Keenan et al., who studied forests) — AT-Neu (+19.7 d) and US-Wkg
(+11.7 d) drive this; US-Var is much smaller (+2.6 d). **Conclusion: the forest-specific prediction
is only partly confirmed (deciduous yes, evergreen no), and an unpredicted, comparably large shift
appears in grasslands — the NT/DT asymmetry is broader than a pure forest-phenology artefact.**

## Item 7 — management/disturbance table

`tables/site_management_15.csv` (18 rows; multiple rows at sites with more than one BADM category).
BADM has **no dated management-event records anywhere in the FLUXNET Shuttle network**
(`review/figures/methods_badm_management.md`) — only a coarse `DOM_DIST_MGMT` tag and `SITE_DESC`
keyword mining — so "dates where known" is empty except where the literature supplies them.

| Site | Type | Source |
|---|---|---|
| AT-Neu | mown + other (manure) | Hammerle et al. (2008); BADM `mgmt_harvest=TRUE` |
| AU-How | unknown | BADM: no record |
| CH-Dav | other | BADM `mgmt_tillage=TRUE` (unverified, plausible historical mention at this ENF site) |
| DE-Tha, FR-Pue, JP-Api, NL-Loo | unknown | BADM: no record |
| IL-Yat | grazed + burned + other (irrigation) | BADM (`mgmt_grazing`, `mgmt_burning`, `mgmt_irrigation` all TRUE) |
| US-Ha1 | other | BADM `DOM_DIST_MGMT = "Undisturbed"` — an explicit no-management tag, distinct from "unknown" |
| US-MMS | other | BADM `DOM_DIST_MGMT = "Forestry"` (unspecified silviculture) |
| US-Mpj | other | BADM `DOM_DIST_MGMT = "Drought; Pests and disease"` (natural disturbance, not management) |
| US-SRM, US-Ton, US-Wkg | grazed | BADM `mgmt_grazing=TRUE` |
| US-Var | grazed | BADM `mgmt_grazing=TRUE`; FLUXNET site page (brush removal for cattle) |

**US-SRM and US-Wkg grazing is BADM-confirmed** (`DOM_DIST_MGMT` includes "Grazing", `mgmt_grazing
=TRUE`), so both are reported as confirmed, not "unconfirmed" — BADM does say otherwise, per the
prompt's conditional.

## Item 8 — AT-Neu mowing test

4 of AT-Neu's site-years are flagged two-season (2010, 2018, 2019, 2020). For each, the days between
the TIMESAT secondary smoothed-GPP peak and the end of AT-Neu's 3 recurring cut windows (cuts end
16 Jun, 12 Aug, 28 Oct; Hammerle et al. 2008):

| Year | Closest cut | Days after that cut's end | Consistent (14–28 d)? |
|---|---|---|---|
| 2010 | cut1 (Jun) | −19 | No |
| 2018 | cut1 (Jun) | +39 | No |
| 2019 | cut1 (Jun) | −26 | No |
| 2020 | cut2 (Jul–Aug) | **+2** | No |

**None of AT-Neu's 4 two-season years fall in the 2–4-week post-cut window tested.** 2018 is close
but 11 days too late; 2020's secondary peak begins just 2 days after cut2 ends — far faster than a
2–4-week regrowth lag, suggesting that *if* mowing drives these flags at all, regrowth response is
faster than assumed, not that it is absent. **Rewritten attribution**: v2 described AT-Neu's
two-season flag generically as consistent with "a physically plausible bimodal growing season." That
claim is not supported by this direct test — the secondary-peak timing does not line up with the
2–4-week mowing-regrowth window as tested. What the data *do* support: two-season flags are
**strongly concentrated at actively managed sites**. Of the 9 total two-season site-years network-
wide, 8 are at sites confirmed as mown/grazed/burned (AT-Neu: 4, US-SRM: 2, US-Var: 1, US-Wkg: 1);
only 1 (US-Mpj, a natural drought/pest disturbance site, not active management) is at an unmanaged
site. So management (of some kind, at some timing not captured by this specific 2–4-week test) is
still the dominant correlate of the two-season flag — the AT-Neu-specific mowing-calendar mechanism
proposed is just not confirmed at the tested lag.

## Item 9 — AU-How

**M7 vs. Ma et al. (2013)**: M7 (NT series) gives a mean GSL of **171.3 d** across AU-How's 15
season-years (range 141–199 d) — well *below* Ma et al.'s reported 244–275 d at Howard Springs. This
is the largest disagreement of any method at AU-How with the literature value for this specific site
(see the cross-method spread below); M7's amplitude/derivative definition, calibrated on a forest
seasonal cycle, evidently detects a much shorter "core" season at this site than the threshold/span
methods report for the same site-years.

**Wet-season GPP and amplitude** (15 Oct–15 Apr, Moore et al. 2017), per season-year:
mean wet-season GPP = 1410.4 gC m⁻², **SD = 114.4** gC m⁻² (8.1% CV); mean seasonal amplitude
(smoothed NT GPP, max − min within the season-year) = 8.17 gC m⁻² d⁻¹, **SD = 1.25** gC m⁻² d⁻¹
(15.3% CV). Both show real, modest interannual variability — AU-How's *productivity* does vary
year to year.

**AU-How is a site where threshold-based GSL carries no interannual signal.** Across M1–M8's
headline sub-threshold (NT/NEP-only series), the **cross-method SD of mean GSL is 73.6 d** (means
range from 171 d, M7, to 365 d, M1) — a spread comparable to the entire GSL range seen across other
sites in this comparison, entirely a function of *which method* is used, not of any real seasonal
signal. Within most individual methods, the correlation between GSL and annual GPP/annual NEP
(detrended) is weak and non-significant: M1 r_gpp=−0.17 (p=0.56), M5 r_gpp=0.21 (p=0.45), M6
r_gpp=−0.27 (p=0.33), M7 r_gpp=0.01 (p=0.97). The two exceptions are M4 (r_gpp=0.93, p<0.01) and, to
a lesser extent, M2/M3 (r_gpp≈0.56–0.60, p≈0.02–0.03) — but these methods are also the ones whose
GSL is most mechanically close to 365 d at this site (mean GSL 359–365 d; AU-How's GPP essentially
never drops below even a strict percentage-of-P99 threshold), so their "signal" is largely an
artefact of near-ceiling values tracking whichever years have slightly more low-GPP days, not a
clean phenological read. **Net conclusion: at AU-How, GSL as a threshold/span construct is not a
useful year-to-year metric** — the site's real interannual variability (wet-season GPP, seasonal
amplitude, both with ~10–15% interannual CV) is better captured directly than through any of these
8 GSL definitions.

## Three to five largest differences between methods (updated for v3)

Per-site range across all 8 methods' headline sub-threshold (NT/NEP-only series), mean GSL:

| Site | Range (d) | Smallest | Largest |
|---|---|---|---|
| US-Var | 255.8 | M8 (109 d) | M1-summer (364.8 d) |
| IL-Yat | 246.2 | M8 (119 d) | M1-summer (365.2 d) |
| US-Ton | 212.6 | M8 (151.7 d) | M1-summer (364.3 d) |
| FR-Pue | 197.9 | M8 (162.6 d) | M1-summer (360.5 d) |
| AU-How | 193.9 | M7 (171.3 d) | M1-summer (365.3 d) |

M1-summer's near-year-round "season" (the Mediterranean/summer-dry climate overestimate already
noted in v1/v2) is the largest value at 4 of these 5 sites; M8 (the new normalized-threshold method,
at its 0.25 crossing) is consistently the shortest. This is the clearest illustration in the whole
comparison of the prompt's underlying point: **picking a different published method moves GSL at
these sites by 194–256 days** — close to the entire length of a calendar year — with no principled
way, from this analysis alone, to call one of the 8 "correct."

## Outputs

- `tables/gsl_methods_site_year.csv` (11,058 rows), `tables/gsl_methods_site_summary.csv` (570 rows) — regenerated in place, now 8 methods × 2 series × 3 start/end rules (M2–M4) × 2 M6 forms.
- `tables/gsl_methods_m2_grid22.csv` — M2's full 22-value grid, NT, site-level means (330 rows).
- `tables/gsl_methods_rule_shifts.csv` — item 3, M2–M4 rule-pair shifts by site.
- `tables/gsl_methods_agreement.csv` — item 5, Spearman r + median abs. date diff, every method pair.
- `tables/gsl_methods_dt_vs_nt.csv`, `..._by_site_group.csv`, `..._by_group.csv` — item 1 + item 6.
- `tables/site_management_15.csv` — item 7.
- `tables/atneu_mowing_test.csv` — item 8.
- `tables/auhow_wet_season_v3.csv` — item 9.
- Three draft figures in `figures/drafts/`, regenerated in place: dotplot (M2 as range), agreement (Spearman only), raster (US-Ha1/US-Wkg/US-SRM, 7 method rows).

## Not reproduced / follow-ups

As in v1/v2, the light-response-curve method (requiring sub-daily PAR/NEE) is not reproduced here —
only US-MMS has sub-daily data in DuckDB (per Task B's earlier finding). M7/M8's derivative-based
definitions are sensitive to the 15-day smoothing half-width, which was not swept here (fixed at the
project-wide 15-day window used throughout this whole comparison); a smoothing-sensitivity analysis
for M7/M8 specifically (paralleling M6's k=3–9 sweep) would be a natural next step if these two
methods are carried forward.
