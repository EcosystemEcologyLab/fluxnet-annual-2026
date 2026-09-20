# ERA5-derived annual precipitation vs. independent references: a network-wide check

> **Provisional — pending store audit (2026-09-20).** The numbers in this package rest on the June 2026 `data/extracted/` extraction. One file in that extraction (IT-MBo's `FLUXNET_FLUXMET_MM` file) is known to differ from the currently-distributed archive under the same product ID. This package's conclusions are provisional pending `review/diagnostics/store_audit/` (in progress).

This package summarises a check of ERA5-derived mean annual precipitation (MAP) against
independent references, across the current FLUXNET network. It describes what was
measured. It does not assert a cause.

## Contents

| File | Contents |
|---|---|
| `fig1_hb4_mbo_era5_vs_measured.png` | Monthly ERA5 vs. tower-measured precipitation at the two most extreme sites |
| `fig2_ratio_histograms.png` | Distribution, across the network, of ERA5 annual precipitation relative to two references |
| `fig3_seasonal_cycle_small_multiples.png` | Monthly ERA5 precipitation shape at five flagged sites and two unaffected sites, one shared scale |
| `site_list.csv` | The 123 flagged sites plus 2 extreme individual outliers, with values and ratios |
| `data_dictionary.txt` | Column-by-column description of `site_list.csv` |
| `README.md` | This file |

## Figure captions

**Figure 1.** At `US-HB4` and `IT-MBo`, the two individual sites with the largest
ERA5-to-reference ratios in the network, monthly ERA5 precipitation runs roughly one to
three orders of magnitude higher than the tower's own measured precipitation for the
same months, consistently across the full multi-year record available at each site. The
y-axis is on a log scale because the two series differ by up to a factor of several
thousand at points; without it, the smaller series would be indistinguishable from zero.
Gaps in the tower-measured line are months that did not meet the measured-data-quality
threshold described below and so are not shown.

**Figure 2.** Across the 781 sites in the current network, the ratio of ERA5 annual
precipitation to two independent references — WorldClim BIO12 and each site's own
BADM PI-reported mean annual precipitation — separates into two groups rather than one:
most sites cluster near a ratio of 1 (matching the reference within ordinary scatter),
and a smaller group clusters near a ratio of 4, with comparatively few sites in between.
Against BIO12 (n=780): 535 sites fall between 0.5x-1.5x, only 35 between 1.5x-3x, and
157 between 3x-6x. Against BADM (n=631): 436 sites fall between 0.5x-1.5x, only 17
between 1.5x-3x, and 129 between 3x-6x. 17 sites in each panel fall outside the plotted
0.2x-10x range and are not shown.

**Figure 3.** At five sites with an elevated ERA5-to-reference ratio (rows 1-2) and two
sites without one (row 3), the monthly ERA5 precipitation cycle rises and falls in the
same calendar months, in a season-appropriate shape, at every site — the shapes are not
distorted. What differs is the absolute scale: the five flagged sites' annual totals
(roughly 4,000-12,000 mm/yr across the three years shown) are five to fifteen times the
two unaffected sites' totals (roughly 600-1,500 mm/yr), plotted here on one shared
y-axis so the difference in scale, not just shape, is visible directly. Three
consecutive years are shown per site to indicate that this is a stable, repeating
pattern rather than a single unusual year.

## Methods, in brief

**ERA5 annual precipitation.** For each site, ERA5 annual precipitation is computed
from the monthly ERA5 reanalysis file distributed with that site's data product: each
month's reported precipitation rate is multiplied by the number of days in that
calendar month, the twelve resulting monthly totals are summed to a calendar-year
total, and the calendar-year totals are averaged across all complete years available
between 1991 and 2020. No spatial or temporal adjustment beyond that day-weighting is
applied.

**WorldClim BIO12 and BADM MAP, and their limits as references.** WorldClim BIO12 is a
gridded climatology (on the order of a few kilometres per pixel) built primarily by
interpolating long-term weather-station records across the landscape; it is independent
of ERA5, but, like any interpolated product, can be less reliable in complex terrain or
in regions with few nearby stations, and it averages over a different multi-decade
period than any individual tower's own record. BADM MAP is the mean annual
precipitation the site's own PI reported in that site's metadata; it is independent of
both ERA5 and BIO12, but the basis for that number — which years it covers, what
instrument produced it, and whether it is itself a direct tower measurement or a
regional estimate — is not standardised across sites and is not always documented.

**Measured annual totals, and why most flagged sites don't have one.** Where used, the
measured annual total is built directly from the tower's own monthly precipitation
gauge record. A calendar year is used only if every one of its 12 months individually
meets a measured-data-quality threshold (a data-quality-fraction of at least 0.9, on the
scale the FLUXNET product itself reports); the measured annual total is the average of
such fully-qualifying years only. No partial year is scaled up to a full year, and no
gap-filled month is used as if it were measured. Most flagged sites have no such year at
all: eddy-covariance precipitation gauges are frequently or entirely gap-filled at many
sites — a known general limitation of tower-based precipitation measurement — so this
same-standard requirement leaves too few, or zero, qualifying months to build even one
complete year at most flagged sites.

**The flagging rule.** A site is included in `site_list.csv` as flagged if its empirical
scale factor — the median of (ERA5/BIO12) and (ERA5/BADM) for that site — falls within
15% of 4 (fourfold) or 8 (eightfold). This is a descriptive grouping rule based on where
a site's ratio to two independent references happens to land; it is not a statement
about which value, if either, is in error. Two additional sites, `US-HB4` and `IT-MBo`,
are included individually because their ratios are extreme outliers that do not fall
into either the 4x or 8x grouping.

## What we ruled out

Two mechanical explanations were tested directly and are not the reason for the pattern
shown here.

- **A unit or aggregation error in the day-weighting step.** Summing (each month's
  reported ERA5 rate x days in that month) reproduces the annual precipitation total in
  the independently-produced annual-resolution file that accompanies the same product,
  to within 0.7% (median ratio 1.0000; range 0.9947-1.0068), across 2,955 site-years at
  the 66 sites where that independent annual file is available for direct comparison.
- **A within-year running total mislabelled as a monthly value.** If a site's monthly
  file reported a year-to-date cumulative total rather than a monthly value, its
  December value divided by its January value would be large, and each month's value
  would rank almost perfectly with its position in the calendar year (a rank correlation
  near 1), resetting every January. Neither signature is present, and neither statistic
  differs meaningfully between the flagged sites and the rest of the network: the
  December/January ratio and the rank correlation between monthly value and month
  number are, respectively, 1.13 and 0.12 (median, flagged group, n=5,507 site-years)
  versus 1.10 and 0.08 (median, rest of the network, n=29,203 site-years).
