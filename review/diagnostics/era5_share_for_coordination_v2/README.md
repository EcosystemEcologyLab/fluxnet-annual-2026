# ERA5-derived annual precipitation: flagged sites, for hub coordination (v2)

**Supersedes `review/diagnostics/era5_share_for_coordination/` (18 September 2026).** That
package used its own ad hoc empirical clustering (sites whose ERA5-to-reference ratio happened
to sit near 4x or 8x). This package replaces that with the four exclusion flags that
`scripts/figure4_representativeness.R` — the production script for main-text Figure 5
("Representativeness of the current FLUXNET network") — actually applies to its
precipitation-dependent Geo-vs-Data panels, read directly from the already-committed
`data/snapshots/site_aridity_era5_fig4.csv`. No new flag logic is introduced here.

**Corrected since the 18 September package: `IT-MBo`.** The previous package's `IT-MBo` row
(`era5_annual_precip_mm` = 24,150.38 mm/yr, ratio to BADM = 17.69, ratio to BIO12 = 59.3) was
built from a June-2026 local `data/extracted/` file that was stale relative to the currently
distributed product — a vintage mismatch, not a property of the distributed data. This was
diagnosed and the number withdrawn in
[`review/diagnostics/it_mbo_parsimony_refresh/report.md`](../it_mbo_parsimony_refresh/report.md):
once every resolution is drawn from the same current product, `IT-MBo`'s ERA5-derived mean
annual precipitation collapses to ~1,126–1,153 mm/yr depending on resolution, essentially
unchanged from its half-hourly value — the ~21x inflation was entirely a stale-file artifact.
**`IT-MBo` carries none of the four flags in this package.** Its current values, read from this
package's own source table (`data/snapshots/site_aridity_era5_fig4.csv`): ERA5 mean annual
precipitation (1991–2020) = 1,136.86 mm/yr; WorldClim BIO12 = 407 mm/yr (ratio 2.79); BADM
PI-reported MAP = 1,365 mm/yr (ratio 0.83). `US-HB4`, this package's other named reference
point, is numerically unchanged from the 18 September package (ERA5 = 658,044.6 mm/yr either
way) and remains flagged here (`above_3x_every_reference`) — its ERA5 defect was never a
staleness artifact.

This package describes what was measured. It uses the word **flagged** throughout and does not
use the word "erroneous" — a flag here is a description of where a site's ratio to its
references lands, not a claim about which value, if either, is in error, or why.

## Contents

| File | Contents |
|---|---|
| `site_list_ICOS.csv` | Every ICOS-hub site carrying at least one flag, with its tier and shared-value group |
| `site_list_AmeriFlux.csv` | Every AmeriFlux-hub site carrying at least one flag, with its tier and shared-value group |
| `site_list_TERN.csv` | Every TERN-hub site carrying at least one flag, with its tier and shared-value group |
| `summary_by_hub.csv` | Count of each flag, by hub, plus each hub's total flagged-site count |
| `summary_icos_by_source_network.csv` | ICOS sites only, flagged and total counts per contributing regional network |
| `summary_by_source_network.csv` | Flagged and total counts per `product_source_network`, with hub, across all three hubs |
| `summary_no_slope_group_by_hub.csv` | For the `no_slope` group only: median ratio to BIO12 and the share between 3x and 6x, per hub |
| `summary_tiers_by_hub.csv` | Count of each of the 5 severity tiers, by hub |
| `summary_shared_values_by_hub.csv` | Per hub: how many flagged sites share their ERA5 annual value with another flagged site, and in how many groups |
| `table_invalid_inputs.csv` | For each of the 4 `invalid_input` sites: which raw ERA5 variable fails, in how many months, and the range of the offending values |
| `table_koppen_panel_dropped.csv` | Every site dropped from Figure 5's Köppen (panel A) Geo-vs-Data panel, with hub, source network, and the rule that dropped it |
| `fig1_ratio_by_slope_group_and_hub.png` | Distribution of the ERA5-to-BIO12 ratio, log axis, for the two `ERA_SLOPE` metadata groups, coloured by hub |
| `fig2_map_flagged_sites_by_tier.png` | World map of all 207 flagged sites, coloured by tier, shaped by hub |
| `era5_cumulative_test_rerun/` | Unmodified re-run of `scripts/diagnostics/era5_cumulative_test.R` against the current store (see "Cumulative-total re-test" below) |
| `data_dictionary.txt` | Column-by-column description of `site_list_<hub>.csv` |
| `README.md` | This file |

## What was measured

For each of the 781 sites in the current FLUXNET network, `figure4_representativeness.R`
computes 1991–2020 mean annual precipitation from the site's own ERA5 reanalysis file (ERA5
MAP) and compares it against two independent references — WorldClim v2.1 BIO12 (a gridded
climatology at the tower coordinate) and, where reported, the site PI's own BADM mean annual
precipitation (BADM MAP) — to decide which sites are usable in Figure 5's precipitation-
dependent Köppen and aridity panels. A site is excluded from those panels, i.e. flagged here,
under exactly one of four rules. **Every flagged site in this package carries exactly one of the
four flags** (verified by this package's own script; no site carries more than one).

## The four flags

**`no_slope`** — the site's own BIF downscaling metadata (`GRP_ERA_DOWN`, `ERA_VARIABLE = P`)
records `ERA_SLOPE` as the sentinel value `-9999`, a distinct pattern from the standard
"not fitted" sentinel (`ERA_SLOPE = 1`) that the other 609 of 781 sites carry. **No site in the
current network has a genuinely fitted precipitation downscaling slope** — the distinction is
between two sentinel groups, not between fitted and unfitted
(`review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv`,
`not_fitted_slope_9999` group, 172 sites network-wide, 171 of which reach this flag — the 172nd,
`DE-Zrk`, is separately caught by the `invalid_input` rule below, which is evaluated for that
site first).

**`above_3x_every_reference`** — quoting `R/pipeline_config.R` directly:

> `P_ERA_MAX_RATIO <- 3` — a site is excluded from precipitation-dependent Geo-vs-Data panels if
> its 1991–2020 mean annual P_ERA exceeds this many times EVERY reference available for it
> (PI-reported BADM MAP where present and non-zero, AND WorldClim BIO12 at the tower — revised
> 2026-10-02 to dual-reference AND logic; where only one reference exists, that one decides
> alone).

**`below_one_third`** — the mirror rule, quoting `R/pipeline_config.R` directly:

> `P_ERA_MIN_RATIO <- 1 / 3` — mirrors `P_ERA_MAX_RATIO` on the low side: a site is excluded from
> precipitation-dependent Geo-vs-Data panels if its 1991-2020 mean annual P_ERA is below this
> fraction of EVERY reference available for it (same dual-reference AND logic as
> `P_ERA_MAX_RATIO`).

**`invalid_input`** — not a precipitation-specific rule. A site is flagged if at least one
calendar month of its raw ERA5 meteorological input (used in the Figure 5 aridity panel's FAO-56
potential-evapotranspiration calculation, not P_ERA itself) is outside a generous physical
plausibility screen (`LW_IN`/`SW_IN` <0 or >1000 W/m²; `VPD` <0 or >100 hPa; `WS` ≤0 or >50 m/s;
`PA` outside [50,110] kPa; `TA` outside [−90,60]°C). Because this rule concerns a different raw
ERA5 variable, not precipitation, affected sites have no ERA5 MAP, BIO12 ratio, or BADM ratio in
`site_list_<hub>.csv` — those columns are blank for them, matching
`data/snapshots/site_aridity_era5_fig4.csv` itself.

### Invalid-input sites, individually

`table_invalid_inputs.csv` reads each of the 4 `invalid_input` sites' own already-extracted raw
monthly ERA5 file (`data/extracted/*/*_ERA5_MM_*.csv`, the one raster/re-extraction exception in
this package — a read of an already-extracted file, not a new extraction) and applies
`figure4_representativeness.R`'s own physical-plausibility screen to every individual calendar
month in the 1991–2020 window (up to 360 site-months), not to the script's own 12-point
climatological-mean series — so these month counts can run higher than 12. **Each site fails for
exactly one variable; no site fails more than one.** This resolves a gap in the 18
September-replacement v2 package, which could only report `US-Sne` (`LW_IN_ERA`) and `CD-Ygb`
(`VPD_ERA`) individually and left `DE-Zrk`/`FR-LBr` as "same pattern, undetermined" — the
committed record alone did not pin down which variable:

| site_id | hub | variable | invalid months | of | offending range |
|---|---|---|---:|---:|---|
| `US-Sne` | AmeriFlux | `LW_IN_ERA` | 356 | 360 | −9,999 to 53,620 W/m² (physical ceiling 1,000 W/m²; includes an explicit −9999 missing-data sentinel in some months alongside the inflated values) |
| `CD-Ygb` | ICOS | `VPD_ERA` | 360 | 360 | 1,403.6 to 1,928.1 hPa (physical ceiling 100 hPa) |
| `DE-Zrk` | ICOS | `VPD_ERA` | 261 | 360 | 100.2 to 921.7 hPa (physical ceiling 100 hPa; its full record's range, including non-invalid months, is 22.0–921.7 hPa, i.e. even its non-invalid months run high) |
| `FR-LBr` | ICOS | `LW_IN_ERA` | 360 | 360 | 1,633.6 to 2,106.6 W/m² (physical ceiling 1,000 W/m²) |

Three of the four sites fail in every one of their 360 available months; `DE-Zrk` fails in 261 of
360. No site's raw file shows a second variable also failing the screen.

## Tiers

Each flagged site also carries a severity tier, 1 (most severe) through 5, built from the same
flags and ratios above — no new exclusion logic, just an ordering of what's already there. Site
lists are sorted by tier, then by departure from parity (`abs(log10(ratio to BIO12))`, largest
first — this ranks departure symmetrically whether the ratio is far above 1 or far below it,
which a plain descending sort on the ratio itself would not do within tier 2, where both
directions occur).

| Tier | Definition |
|---|---|
| 1 | `invalid_input` |
| 2 | `above_3x_every_reference`, or `below_one_third` |
| 3 | `no_slope`, both references present, ratio above 3x **both** BADM and BIO12 |
| 4 | `no_slope`, above 3x the only reference available (BADM absent), **or** above 3x one reference and not the other |
| 5 | `no_slope`, below 3x every reference available |

Tiers 3–5 exist because `no_slope` sites never reach the direct ratio rule (it is skipped for
them — see "The four flags" above) even though their underlying ratios are still present in
`data/snapshots/site_aridity_era5_fig4.csv` and vary considerably (2.2x–8.6x against BIO12 among
`no_slope` sites). Tiers 3–5 read those existing ratios directly; they do not change which sites
are flagged or why.

## Counts

From `summary_by_hub.csv`:

| hub | no_slope | above_3x_every_reference | below_one_third | invalid_input | total flagged |
|---|---:|---:|---:|---:|---:|
| AmeriFlux | 75 | 7 | 21 | 1 | 104 |
| ICOS | 92 | 3 | 1 | 3 | 99 |
| TERN | 4 | 0 | 0 | 0 | 4 |

207 sites flagged network-wide (of 781).

From `summary_icos_by_source_network.csv` — ICOS-hub sites, by contributing regional network:

| source network | flagged | total |
|---|---:|---:|
| CNF | 13 | 32 |
| EUF | 31 | 138 |
| FLX | 8 | 18 |
| ICOS | 6 | 80 |
| JPF | 36 | 54 |
| KOF | 5 | 21 |
| SAEON | 0 | 5 |

From `summary_no_slope_group_by_hub.csv` — the `no_slope` group only:

| hub | n | median ratio to BIO12 | n between 3x and 6x | share between 3x and 6x |
|---|---:|---:|---:|---:|
| AmeriFlux | 75 | 3.83 | 68 | 90.7% |
| ICOS | 92 | 4.17 | 82 | 89.1% |
| TERN | 4 | 2.59 | 1 | 25.0% |

From `summary_tiers_by_hub.csv`:

| hub | tier 1 | tier 2 | tier 3 | tier 4 | tier 5 | total flagged |
|---|---:|---:|---:|---:|---:|---:|
| AmeriFlux | 1 | 28 | 64 | 11 | 0 | 104 |
| ICOS | 3 | 4 | 70 | 19 | 3 | 99 |
| TERN | 0 | 0 | 0 | 1 | 3 | 4 |

Tier totals (1+2+3+4+5) reproduce each hub's total flagged count exactly — 104 / 99 / 4, matching
`summary_by_hub.csv` above.

From `summary_by_source_network.csv` — every hub, by contributing regional network (the ICOS
rows reproduce `summary_icos_by_source_network.csv` above; AmeriFlux and TERN each map 1:1 to
their own hub's network):

| hub | source network | flagged | total |
|---|---|---:|---:|
| AmeriFlux | AMF | 104 | 381 |
| ICOS | CNF | 13 | 32 |
| ICOS | EUF | 31 | 138 |
| ICOS | FLX | 8 | 18 |
| ICOS | ICOS | 6 | 80 |
| ICOS | JPF | 36 | 54 |
| ICOS | KOF | 5 | 21 |
| ICOS | SAEON | 0 | 5 |
| TERN | TERN | 4 | 52 |

From `summary_shared_values_by_hub.csv` — flagged sites whose ERA5 1991–2020 mean annual
precipitation is numerically identical to another flagged site in the same hub (all three
`no_slope`-only; this scope was checked against computing the grouping within-hub on the full
flagged list, within-hub on the `no_slope` subset alone, and network-wide, and all three agree
exactly — **no difference found against the independent hand tally**: 31 of 92 ICOS `no_slope`
sites sharing a value in 14 groups, 38 of 75 AmeriFlux `no_slope` sites in 9 groups):

| hub | sites sharing a value | groups |
|---|---:|---:|
| AmeriFlux | 38 | 9 |
| ICOS | 31 | 14 |
| TERN | 0 | 0 |

A shared ERA5 value across sites is consistent with ERA5's own grid resolution — multiple towers
can fall in the same reanalysis grid cell and so draw an identical gridded value — and is
reported here descriptively, with no cause asserted.

### Köppen panel (Figure 5, panel A), dropped sites

`table_koppen_panel_dropped.csv` is a different, smaller list from the one above: Figure 5's
Köppen panel is PI-class-first — a site with its own PI-reported Köppen class is never dropped,
even if its own ERA5 climatology would otherwise fail one of the same rules used above. Only
sites **without** a PI-reported class and failing `no_slope` or `below_one_third` are dropped
from that panel (`above_3x_every_reference` drops no site here, because every site that rule
would otherwise catch has a PI-reported class instead). 31 sites dropped network-wide:

| hub | `no_slope` | `below_one_third` | total |
|---|---:|---:|---:|
| AmeriFlux | 4 | 2 | 6 |
| ICOS | 20 | 1 | 21 |
| TERN | 4 | 0 | 4 |

This 31-site list is not a subset or superset of this package's own 207-site flagged list in any
simple way — most of the 207 are PI-sourced sites that the Köppen panel never drops, while the
Köppen panel also drops some sites this package's aridity-panel-based flags do not touch
identically (both panels apply the same precipitation rules, but panel A's PI-first design
changes which sites those rules actually reach).

### Cumulative-total re-test

`scripts/diagnostics/era5_cumulative_test.R` — the read-only test of whether the affected sites'
monthly `P_ERA` is actually a within-year cumulative total rather than a true monthly value — was
re-run **unmodified** against the current store. Its output was copied to
`era5_cumulative_test_rerun/` in this package rather than overwriting the committed 18 September
outputs in `review/diagnostics/era5_cumulative_test/`: the script's own `OUTD` constant always
writes to that original folder, so the original folder's committed state was restored via
`git checkout` immediately after copying the fresh run's output out, leaving it unchanged (`git
status` confirms a clean working tree for that folder afterward). No file in
`review/diagnostics/era5_cumulative_test/` or the script itself was edited.

**The verdict holds: the within-year cumulative-total pattern remains absent.** The re-run's key
statistics reproduce the original report's closely, with the small differences expected from
ordinary reprocessing drift between the store snapshots each run reads — not a change in
conclusion:

| statistic | original report | re-run |
|---|---|---|
| Dec/Jan ratio, median (4x/8x cluster) | 1.13 | 1.128 |
| Dec/Jan ratio, median (rest of network) | 1.10 | 1.096 |
| rho(value, month), median (cluster) | 0.12 | 0.119 |
| rho(value, month), median (rest) | 0.08 | 0.077 |
| site-years (cluster / rest) | 5,507 / 29,203 | 5,509 / 29,207 |

`fig1_ratio_by_slope_group_and_hub.png` shows the same pattern visually across all 781 sites:
most sites, regardless of hub, cluster near a ratio of 1 in the `ERA_SLOPE = 1` panel; the
`ERA_SLOPE = -9999` (`no_slope`) group clusters instead near a ratio of 4, at both AmeriFlux and
ICOS. TERN contributes only 4 `no_slope` sites and does not show a comparably clean pattern at
that count. TERN's `ratio_era5_to_badm` column is blank throughout `site_list_TERN.csv` because
none of TERN's 52 current-network sites report a BADM MAP in this snapshot — TERN's flags are
therefore decided by the WorldClim BIO12 reference alone.

## Methods notes

- **Identifier links.** `identifier_link` is constructed per hub from `product_id` in
  `data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv`: a `doi.org` link for
  AmeriFlux (`product_id` is a bare DOI suffix), an `hdl.handle.net/11676/` link for ICOS
  (verified against every ICOS site's own `product_citation` string in this snapshot), and
  `product_id` itself for TERN, which is already a resolvable URL.
- **Sources.** `data/snapshots/site_aridity_era5_fig4.csv` (flags, ERA5 MAP, BIO12, BADM MAP,
  ratios); `review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv`
  (years of measured precipitation and mean measured precipitation);
  `review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv` (metadata slope and
  its group); `data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv` (hub, source network,
  product name, product identifier, site coordinates for the map);
  `data/snapshots/site_koppen_era5_fig4.csv` (the Köppen-panel-dropped list, `panel_a_eligible`/
  `panel_a_source` columns). All five are already-committed tables. The one exception —
  `table_invalid_inputs.csv` — reads each of the 4 `invalid_input` sites' own already-extracted
  raw monthly ERA5 file directly (`data/extracted/*/*_ERA5_MM_*.csv`), the same raw-file read
  `era5_share_for_coordination.R` and `era5_cumulative_test.R` already use elsewhere in this
  repo; nothing is re-extracted. `fig2_map_flagged_sites_by_tier.png`'s land outline comes from
  the already-installed `rnaturalearthdata` package (medium scale), the same source
  `R/figures/fig_maps.R` uses for main-text figures — no new package dependency introduced.
- **Reproduction checks against an independent hand tally.** Every count in this README that was
  checked against an independent hand tally of the source tables — `summary_by_hub.csv`,
  `summary_icos_by_source_network.csv`, the no-slope group medians and 3x–6x shares, the tier
  totals (99/104/4), and the shared-value counts — matched exactly. **No difference was found
  anywhere**, including the ICOS no-slope median ratio (4.17, rounding to the hand tally's 4.2)
  and its 82-of-92 share between 3x and 6x, and the shared-value groups (31 of 92 ICOS `no_slope`
  sites in 14 groups; 38 of 75 AmeriFlux `no_slope` sites in 9 groups).
