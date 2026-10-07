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
| `site_list_ICOS.csv` | Every ICOS-hub site carrying at least one flag |
| `site_list_AmeriFlux.csv` | Every AmeriFlux-hub site carrying at least one flag |
| `site_list_TERN.csv` | Every TERN-hub site carrying at least one flag |
| `summary_by_hub.csv` | Count of each flag, by hub, plus each hub's total flagged-site count |
| `summary_icos_by_source_network.csv` | ICOS sites only, flagged and total counts per contributing regional network |
| `summary_no_slope_group_by_hub.csv` | For the `no_slope` group only: median ratio to BIO12 and the share between 3x and 6x, per hub |
| `fig1_ratio_by_slope_group_and_hub.png` | Distribution of the ERA5-to-BIO12 ratio, log axis, for the two `ERA_SLOPE` metadata groups, coloured by hub |
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

Four sites carry `invalid_input`, network-wide:

| site_id | hub | invalid raw ERA5 input |
|---|---|---|
| `US-Sne` | AmeriFlux | `LW_IN_ERA` — reaches ~30,000–32,000 W/m² in winter months (physical ceiling ~1,000 W/m²) |
| `CD-Ygb` | ICOS | `VPD_ERA` — reaches ~1,620–1,660 hPa (physical ceiling ~100 hPa; true VPD never exceeds ~12 hPa even in the driest deserts) |
| `DE-Zrk` | ICOS | Documented (`docs/known_issues.md` §9c, `SESSION_LOG.md`) as showing "the same pattern" as `US-Sne`/`CD-Ygb`, without a site-specific variable recorded in the committed record. Not re-derived here — this package's inputs are limited to already-committed tables, none of which carries a per-site variable attribution for this site. |
| `FR-LBr` | ICOS | Same as `DE-Zrk`: documented as the same pattern, no site-specific variable attribution in the committed record. |

This table is reported as-is rather than guessed at for the two undetermined sites.

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
  (years of measured precipitation); `review/diagnostics/precip_downscaling_provenance/
  table_2_site_groups.csv` (metadata slope and its group);
  `data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv` (hub, source network, product
  name, product identifier). All four are already-committed tables — no raster, no
  re-extraction, nothing read from `data/extracted/` or `data/raw/`.
- **Reproduction check against an independent hand tally.** Every count in this README
  (`summary_by_hub.csv`, `summary_icos_by_source_network.csv`, the no-slope group medians and
  3x–6x shares) was checked against an independent hand tally of the same three source tables
  before this package was finalised. **No difference was found** — every count matches exactly,
  including the ICOS no-slope median ratio (4.17, rounding to the hand tally's 4.2) and its
  82-of-92 share between 3x and 6x.
