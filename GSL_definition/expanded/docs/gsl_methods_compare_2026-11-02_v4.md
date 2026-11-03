# GSL methods comparison — version 4

**Script:** `GSL_definition/expanded/code/11_gsl_methods_compare.R`
**Prompt:** `logs/prompts/2026-11-02_gsl_methods_compare_v4.md`
**Supersedes:** version 3 (committed `cd7e547`; `gsl_methods_compare_2026-11-02_v3.md`).

Same 15 sites, NT/DT series, and purpose as v3 (no Z16-vs-T85/T95 framing). Checks are as v3, run
under the calendar `year_rule` only (all passed exactly — see "Checks", below).

## 1. Analysis year, run both ways (item 1, reported first per the prompt)

Every method now carries a `year_rule` column: **calendar** (v1–v3's rule — NH = Jan–Dec, AU-How =
Jul–Jun) and **min_gpp_month** (each site's year starts on the 1st of its own lowest-mean-NT-GPP
calendar month, determined once from the calendar rule's own pooled passing years; a `<=14`-failing-
day QC test applies to year-windows under either rule). The chosen start month per site:

| Site | Start month | Site | Start month | Site | Start month |
|---|---|---|---|---|---|
| AU-How | Aug | CH-Dav | Jan | US-Ha1 | Jan |
| NL-Loo | Jan | US-MMS | Jan | JP-Api | Jan |
| DE-Tha | Jan | US-Mpj | Jan | **IL-Yat** | **Aug** |
| **FR-Pue** | **Dec** | **AT-Neu** | **Feb** | **US-Ton** | **Oct** |
| | | | | **US-Var** | **Aug** |
| | | | | US-Wkg | Jan |
| | | | | US-SRM | Jan |

Most NH sites' lowest-GPP month is January, confirming the standard calendar split is fine for them.
**The 4 sites the prompt flagged as "probably" winter-growing (IL-Yat, US-Var, US-Ton, FR-Pue) all
have a non-January minimum** (Aug, Aug, Oct, Dec respectively), and AT-Neu's is February — exactly
the group where a Jan–Dec split risks cutting a real growing season in two.

**Years per site** (`tables/gsl_methods_year_rule_years.csv`): mostly unchanged (10 of 15 sites
identical) or off by one (AT-Neu 18→16, FR-Pue 19→21, US-Ton 20→21, US-Var 23→22) — the different
start month mostly just slides which 12-month block is tested, not how many pass.

**GSL, start, end**: the published `gsl_methods_year_rule_compare.csv` reports shifts in mean
start/end *day-of-year*, but day-of-year is not a safe metric to difference across two rules with
very different start months — the same physical date can be ~0 or ~+/-300 apart in raw `yday()`
depending on which month the window itself starts in (a wraparound artifact, not a real shift; this
shows up as the implausible −213 to −333 d "end_doy_shift" rows for AT-Neu/US-Var M2–M4 in that
table). **A cleaner, wraparound-free check** was run directly from `gsl_methods_site_year.csv`:
start/end expressed as days *since that row's own window start* (reproducible: `start_date -
window_start_date`, `window_start_date` = calendar Jan 1/Jul 1 or the site's own min-GPP-month 1st).
For M2/M3/M4 (NT, 10%/1 gC/0.95 pooled):

| Site | start (into window), calendar → min_gpp_month | end (into window), calendar → min_gpp_month |
|---|---|---|
| **US-Var** | 3.3 d → 90.3 d | **362.0 d → 301.8 d** |
| **IL-Yat** | 0.8 d → 24.8 d | **357.2 d → 326.1 d** |
| US-Ton | 1.4 d → 4.9 d | 362.3 d → 361.5 d |
| FR-Pue | 1.1 d → 1.4 d | 362.2 d → 363.5 d |
| AT-Neu | 25.8 d → 25.1 d | 344.0 d → 346.9 d |
| AU-How | 1.1 d → 2.5 d | 363.4 d → 361.8 d |
| other 9 sites | unchanged (identical windows under both rules for these subs) | unchanged |

**This directly confirms the expectation for US-Var and IL-Yat, and shows it does NOT hold for
FR-Pue/US-Ton/AT-Neu.** Under calendar years, US-Var's and IL-Yat's count-method "season" ends
essentially at the last day of the window (362/357 d in) — the "whole-year" artifact already flagged
in v2/v3 as driven by isolated late-year rain-pulse days. Under min_gpp_month (starting in their own
driest month, August), the season now ends ~60 d (US-Var) and ~31 d (IL-Yat) earlier **and** starts
later — a real, compact growing season instead of a near-year-round one. FR-Pue, US-Ton and AT-Neu,
despite having non-January minimum months too, show almost no change — their count-method seasons
were never hitting the window boundary under calendar years in the first place.

**Rule (a)-vs-(c) start/end shifts (v3 item 3)** change accordingly:

| Site | median start shift (a-c), calendar → min_gpp_month | median end shift (a-c), calendar → min_gpp_month |
|---|---|---|
| US-Var | 3 d → 54.5 d | **216.5 d → 2 d** |
| IL-Yat | 0 d → 75 d | **193.5 d → 15 d** |
| US-Ton | 4.5 d → 85 d | 108 d → 9.5 d |
| AT-Neu | 52.5 d → 20 d | 33 d → 65.5 d |

US-Var and IL-Yat's enormous end-date disagreement between rules (a) and (c) under calendar years
(193–217 d, reported in v3) **collapses to 2–15 d under min_gpp_month** — strong confirmation that
most of that disagreement was itself the calendar-split artifact (rule (a) chasing a rain-pulse day
near the window boundary; rule (c) anchored to the real peak) rather than a genuine rule difference.
US-Ton's end-shift also collapses (108→9.5 d) but its *start*-shift grows (4.5→85 d) — the split
moves which boundary is awkward, not whether one is.

**M8's GSL** barely changes for most sites (`gsl_methods_year_rule_compare.csv`, M8 sub `0.25`):
IL-Yat (119.1→127.0 d, +7.9), FR-Pue (162.6→168.8 d, +6.1), US-Ton (151.7→154.2 d, +2.5) all move
*up* slightly (a less-truncated, slightly longer detected season) — consistent with, but much smaller
than, the count-method story; US-Var moves negligibly (109.0→108.0 d) because M8's own
closest-to-peak-crossing logic was already largely immune to the boundary artifact that hit rules (a)
and M1-summer.

**Largest cross-method differences** (`gsl_methods_year_rule_range.csv`): US-Var's range drops from
255.8 d (calendar) to **133.5 d** (min_gpp_month) — less than half — almost entirely because
**M1-summer's near-year-round reading collapses from 364.8 d to 241.5 d** once the window starts in
August instead of January (`gsl_methods_year_rule_compare.csv`, M1/summer). IL-Yat similarly drops
206.2→217.9 d range (M1-summer 365.2→344.9 d). AU-How's range also drops (139.2→122.5 d) even though
AU-How's own min-GPP month (August) differs from its calendar-rule Jul-start by only one month.

**Conclusion: the expectation holds, strongly, for US-Var and IL-Yat (where M1-summer and the
count-method "last qualifying day" rule were both chasing the Dec 31 window boundary), and does
not hold for FR-Pue or US-Ton**, whose count-method seasons were not boundary-truncated under
calendar years despite also having non-January minimum months. This is a genuine, site-specific
finding, not a uniform "calendar years are wrong everywhere" result — exactly the kind of number
Dave and Tash need for the year-rule decision.

## 2. M7 resmoothing (Ma et al. 2013)

`Rssa` is **not installed** (`requireNamespace("Rssa", quietly = TRUE)` returns `FALSE`), and
CLAUDE.md requires discussion before adding a new package dependency or touching `renv.lock` —
neither is available in this autonomous run. Per the prompt's own fallback, M7 now uses a
**per-site-year harmonic regression on 16-day GPP composites**, interpolated to daily values,
replacing the 15-day rolling mean. K was chosen by mean R² across all 291 calendar-rule NT
site-years: **K=2 → R²=0.863; K=3 → R²=0.919 → K=3 used** (a meaningful, not marginal, improvement).
`m7_one()`'s amplitude/derivative algorithm itself is byte-for-byte unchanged from v3 ("'Halfway' is
in time, as implemented").

**AU-How recheck (Jul–Jun, calendar rule)**: mean M7 GSL rises from v3's 171.3 d (15-day rolling
mean) to **226.1 d** (harmonic fit) — a 55-day jump from smoothing alone, and now **only 17.9 d short
of Ma et al.'s 244 d lower bound** (vs. 73–104 d short in v3). Per-year range: 201–271 d, overlapping
Ma et al.'s 244–275 d range in several years (2010: 271 d; 2022: 258 d; 2011: 246 d). **This gap is
much smaller than v3's, but still real** — AU-How's mean M7 GSL remains below Ma et al.'s own range,
plausibly reflecting remaining smoothing-method differences (SSA vs. a fixed-order harmonic fit) or
genuine inter-annual/site differences from the original Howard Springs study years. Reported as a
real, if now much smaller, difference, not closed by this fix.

## 3. AT-Neu cuts

**2010 mowing test rerun with the actual published dates** (Hortnagl & Wohlfahrt 2014: cuts 5 Jun,
31 Jul, 20 Sep; manure 18–22 Oct) against AT-Neu 2010's TIMESAT secondary peak (28 May):

| Cut | Days after cut | Consistent (14–28 d)? |
|---|---|---|
| 5 Jun | −8 | No |
| 31 Jul | −64 | No |
| 20 Sep | −115 | No |

**The secondary peak (28 May) precedes all three of 2010's actual cuts** — the earliest by just 8
days. This firmly **rules out mowing as the driver of AT-Neu's 2010 two-season flag**: whatever
produces the secondary peak, it happens *before* the growing season has even been cut once that year.

**Cut detector**, built on unsmoothed daily NT GPP (≥50% drop from a 3-day baseline within 1–3 days,
followed by ≥30% regrowth 14–28 days after the trough; adjacent candidates within 5 days clustered
into one event — as implemented, not from the literature): **291 events detected over AT-Neu's full
record** (~15.4/year, one roughly every 24 days). Validated against the 6 published 2010/2011 dates
(±10-day tolerance): **5 of 6 hits** (mean timing error on hits = 5.4 d), **1 miss** (the 5 Jun 2010
cut — nothing was detected nearby), and **24 false alarms** in just those 2 years.

**This false-alarm rate is a serious limitation, quantified**: at ~15.4 events/year, a Poisson
back-of-envelope gives roughly a **59% chance that any random day falls within ±10 days of some
detected event purely by the detector's own base rate**. That makes the trough-vs-cut coincidence
test below only weakly informative — a "hit" is not far from what chance alone would produce.

**Trough-vs-detected-cut test**, every AT-Neu two-season year: 3 of 4 (2018, 2019, 2020) have a
between-peaks trough within 10 days of a detected event; 2010 does not (nearest detected event is 55
days away). Given the ~59% chance base rate above, **3/4 is barely above what chance alone predicts**
(expected ≈2.4/4 by chance) and should not be read as confirmation that cutting drives these flags —
only that it is not ruled out either.

**v3's claim that "management is the main correlate of two-season years" is removed.** Two-season
years are now reported in two separate groups, with no causal claim attached to either:

| Group | Site-years | Sites |
|---|---|---|
| AT-Neu (mowing) | 4 | AT-Neu: 2010, 2018, 2019, 2020 |
| US Southwest dryland (winter/monsoon) | 5 | US-SRM (2), US-Var (1), US-Wkg (1), US-Mpj (1) |

AT-Neu's flags are plausibly management-related in some way (it is the only hay meadow among these
15 sites, cut 3–4×/year), but the direct 2010 test and the detector's weak discriminating power mean
this is **not confirmed at any specific cut**. The US Southwest dryland sites' two-season pattern
(US-SRM, US-Var, US-Wkg, US-Mpj) more plausibly reflects winter-rain/summer-monsoon bimodal
precipitation, a climate feature of that region unrelated to any single management event — grazing
at these sites is continuous, not an event like mowing, so it has no obvious signature to test for in
the GPP curve the way a cut does.

## 4. Management table — full replacement

`tables/site_management_15.csv` is now entirely literature-sourced (BADM `DOM_DIST_MGMT`/`SITE_DESC`
keyword mining, used in v3, is dropped — it produced false positives, e.g. CH-Dav flagged "tillage"
at a never-tilled research forest). All 15 sites, one row each (AT-Neu's entry folds in the item-3
cut dates):

| Site | Type | Summary |
|---|---|---|
| AU-How | burned | Controlled burns every 1–3 years, early dry season (TERN) |
| NL-Loo | other | Scots pine planted c. 1909; little management since (ICOS) |
| DE-Tha | other | Spruce sown 1887; thinned ~30% Apr 2002; 2 ha windthrow Jan 2007, 300 m from tower (Bernhofer et al. 2011) |
| FR-Pue | other | Holm oak coppice, last clear-cut 1942; throughfall-exclusion/thinning experiments since 2003 (DEIMS) |
| CH-Dav | other | Research plots never thinned; harvest in surrounding (non-footprint) forest; no tillage (WSL) |
| US-MMS | other | Selective logging, since ended, year not given (AmeriFlux/OSTI) |
| US-Mpj | other | Drought + bark-beetle pinon die-off 2013–2015; no grazing reported (AmeriFlux/OSTI) |
| AT-Neu | mown | Hay meadow, cut 3–4×/yr + manured; 2010/2011 dates published, 2018–2020 not (Hortnagl & Wohlfahrt 2014) |
| US-Ha1 | other | Cleared for farming 1600s–1700s; overstorey uprooted by the 1938 hurricane; no recent management |
| JP-Api | unknown | ~70-year-old beech forest; none reported (AsiaFlux) |
| IL-Yat | grazed | Aleppo pine planted 1964; fire-protected; sheep/goat grazing late spring; irrigation experiment plot in south, footprint overlap unknown (ICOS) |
| US-Ton | grazed | Cattle grazing, periodic brush removal; season/intensity not found |
| US-Var | grazed | Cattle grazing, periodic brush removal; season/intensity not found |
| US-Wkg | grazed | Light-to-moderate grazing throughout the record (Scott et al. 2010) |
| US-SRM | grazed | Cattle grazing year-round 1957–2005; minimal 2006; none from 2007 (Scott et al. 2009) |

## 5. NT vs. DT — rewritten conclusion (item 6, v3)

**v3's reading was incomplete.** Keenan et al. (2019) *did* include grasslands — one of 11 ecosystem
types in their Fig. 3, not excluded. Apparent light inhibition of daytime respiration (the NT-GPP
overestimate mechanism) was **highest in open shrublands (22.9%), savannas, woody savannas and
wetlands, and lowest in evergreen broadleaf forest (5.1%)** — tracking each ecosystem's own fAPAR
seasonal cycle, i.e. **the bias is largest wherever green-up is fastest**, not specifically "forests
vs. non-forests." That predicts an earlier NT season start at **both** AT-Neu and US-Wkg (fast-green-
up herbaceous systems) — not only at deciduous forest, as v3 argued.

The data agree with this corrected reading. Grassland shows the single largest mean start shift of
any group (+10.6 to +11.3 d across versions), driven by AT-Neu (+19.7 d) and US-Wkg (+11.7 d) — both
exactly the kind of fast-green-up system Keenan et al.'s inhibition-vs-fAPAR relationship predicts.
Deciduous forest's shift (+9.6–9.7 d) is almost entirely US-Ha1 (+16.3 d) and US-MMS (+13.7 d), with
JP-Api essentially flat (−1.4 d) — also broadly consistent, since deciduous canopies have their own
fast spring leaf-out. Evergreen forest shows ~no shift (+0.7 d) — consistent with Keenan et al.'s
5.1% EBF-lowest inhibition estimate and the absence of an abrupt leaf-out transition. Woody-grass
mosaic is also near zero (+0.7–0.8 d), which is more surprising given Keenan et al. report savannas
and woody savannas among the *highest*-inhibition ecosystem types — our mosaic sites (US-Mpj, US-SRM,
US-Ton, AU-How) do not show the correspondingly large shift their category would predict, a genuine
open question this analysis does not resolve.

**Caveats worth keeping attached to this reading**: Keenan et al.'s respiration-inhibition estimate
is itself derived from flux data (the same kind of data whose partitioning it is meant to explain),
checked against an independent method (isotopes) at only one site (Harvard Forest), and the authors
themselves describe their estimate as **conservative** (likely an underestimate of the true bias).
**Revised conclusion: the NT/DT asymmetry tracks green-up speed across ecosystem types broadly, as
Keenan et al. (2019) predict, confirmed at both a grassland (AT-Neu) and deciduous forest (US-Ha1/
US-MMS) — but the woody-grass mosaic group does not show the shift their category would predict, and
the underlying bias estimate itself carries real, acknowledged uncertainty.**

## Checks

As v3, run under the calendar `year_rule`: NH-site NT-series M2@10%/M4@0.95 == `derived_site_year.csv`
main_unscreened values (276 rows, exact); every one of 22,116 rows (both year_rules) has a value or
an explanatory flag; M4@0.85-vs-fraction-sweep remains not applicable (as v3).

## Outputs

New/changed tables: `gsl_methods_year_rule_years.csv`, `gsl_methods_year_rule_compare.csv`,
`gsl_methods_year_rule_range.csv`, `gsl_methods_rule_shifts_min_gpp_month.csv` (item 1);
`atneu_2010_mowing_test_real_dates.csv`, `atneu_cut_detector_events.csv`,
`atneu_cut_detector_validation.csv`, `atneu_trough_vs_cut_test.csv`, `two_season_years_by_group.csv`
(item 3); `site_management_15.csv` (full replacement, item 4). `gsl_methods_site_year.csv`/
`gsl_methods_site_summary.csv` now carry `year_rule` and are roughly double v3's row counts. The 3
draft figures are calendar-rule only, regenerated in place; M7's raster/dotplot entries visibly
smoother than v3's (harmonic-fit input curve).

## Not reproduced / follow-ups

The window-relative start/end recheck in item 1 above (days-since-window-start, rather than raw
day-of-year) was computed directly from `gsl_methods_site_year.csv` for this doc and is reproducible
from that table, but is not itself persisted as a table — a natural follow-up if this becomes a
recurring comparison. The AT-Neu cut detector's high false-alarm rate (24 in 2 validation years, ~59%
chance-level coincidence) means it should not be used as confirmatory evidence on its own; a version
using smoothed GPP or a steeper/shorter drop criterion might discriminate better, but was not
attempted here. 2018–2020 AT-Neu cut dates remain unavailable without contacting Georg Wohlfahrt;
Tonzi/Vaira's (US-Ton/US-Var) water-year start month is still unverified against a primary source —
both noted by Dave as open, not attempted in this revision.
