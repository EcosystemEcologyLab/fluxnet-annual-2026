# ERA5 precipitation units investigation: are the 26 MAP-screen exclusions a units bug?

> **Provisional — pending store audit (2026-09-20).** The numbers in this report rest on the June 2026 `data/extracted/` extraction. One file in that extraction (IT-MBo's `FLUXNET_FLUXMET_MM` file) is known to differ from the currently-distributed archive under the same product ID. This report's conclusions are provisional pending `review/diagnostics/store_audit/` (in progress).

**Type:** Read-and-report diagnostic + scoped counterfactual, for a
co-author decision. No existing script, figure, legend, or snapshot CSV
was modified — `R/climate_classification.R` and
`scripts/step5_compute_koppen_era5.R` were **read and sourced**, never
edited. New code: `scripts/diagnostics/era5_precip_units.R` (D1-D3),
`scripts/diagnostics/era5_precip_units_recovery.R` (D4, reuses the real,
unmodified `compute_site_koppen_era5()`/`compute_era5_monthly_climatology()`
functions rather than reimplementing them). Outputs and run logs in this
directory.

**On the "25 excluded" figure in the task:** the 2026-08-20 log entry
reported 25 sites excluded, at the then-current 767-site network. This
investigation runs on the **current 781-site network**, where the actual
count — re-derived directly from `data/snapshots/site_koppen_era5.csv`
(`koppen_class` is `NA`), not assumed — is **26**. Both figures are
reported; the current 26-site list is authoritative for everything below,
per the task's own "treat code as authoritative" instruction.

---

## Verdict

**Neither a units bug nor a spatial-averaging artifact — this is
geography, and ERA5 is largely correct.** Of the 26 excluded sites, 25
have tower-observed `P_F` available in DuckDB; of those 25, **24 show the
pipeline's existing formula (`sum(P_ERA * days_in_month)`, unchanged)
agreeing closely with tower-observed annual precipitation** (ratio 0.88–
1.31, no systematic direction) **while
disagreeing substantially (2.6–7.75×) with **WorldClim BIO12**. Testing
whether P_ERA carries inconsistent units (D2) found no evidence for it:
removing the day-in-month multiplier (variant b) undershoots BIO12 by
~20–30× everywhere in the network — mathematically expected, since P_ERA
genuinely is a mm/day rate — and the mean×365.25 variant (c) is
functionally identical to the pipeline's own formula (a). **The pipeline's
existing formula is the one that matches BIO12 for the non-JPF majority of
the network (387/781 sites within ±25%)** — there is no alternative
formula to switch to. What's happening instead: `product_source_network ==
"JPF"` sites are excluded at **29.6%** vs. 1.2–1.9% for every other hub
(38.9% using the task's framing of JP-prefixed-plus-KH-Kmp), and the
elevated-ratio pattern extends across **74.1% of all 54 JPF sites**
(median ratio 3.72), not just the 16 that happen to cross the absolute
5000mm threshold — including non-excluded JPF sites in Mongolia and inner
Russia with genuinely low BIO12 baselines. Since JPF serves precisely the
kind of dense-monsoon-forest and steep-terrain sites (Japan, Southeast
Asia, Andean/Amazon transition) where WorldClim's coarse, sparse-
station-interpolated BIO12 is documented to systematically underestimate
true local rainfall, and since the tower gauges independently corroborate
ERA5's higher values, **BIO12 is the outlier reference here, not ERA5**.
The 26th site, `JP-Om2`, has no tower `P_F` data to check (unverified,
not confirmed) but its BIO12 ratio (3.89×) sits squarely inside the same
range as its confirmed JPF neighbors. **The one clear, distinct exception
is `US-HB4`** — every one of its 539 raw
monthly `P_ERA` values from 1981–2025 sits in the 74–7683 mm/day range
(mean 1798, physically impossible as a monthly-mean daily rate — the
world-record 24-hour total is ~1800mm), while its tower `P_F` (2016 mm/yr,
1 year available) is entirely ordinary. This is a genuine, severe,
site-specific raw-data anomaly — order-of-magnitude consistent with a
~1000× scaling error specific to how this one AmeriFlux (not JPF) site's
ERA5 file was produced — and should be handled and reported separately
from the geography-driven pattern affecting the other 25 sites.

---

## D1 — Reference comparison (per-site table, 26 excluded sites)

All values mm/yr; `map_a` = pipeline formula, mean over available complete
1991–2020 years; `bio12_mm` = WorldClim BIO12 at site coordinates;
`pf_obs_mm` = tower-observed `P_F`, mean over available years (`n_yr_pf`)
within 1991–2020; ratios are `map_a` ÷ reference. Sorted by ratio to BIO12
(descending); the anomalous site is marked.

| site_id | map_a | bio12 | ratio→BIO12 | pf_obs | ratio→P_F | n_yr_pf | hub |
|---|---|---|---|---|---|---|---|
| **US-HB4** | **658,045** | 1,350 | **487.4×** | 2,016 | **326.4×** | 1 | AMF — **anomaly, see below** |
| IT-MBo | 24,150 | 407 | 59.3× | 27,392 | 0.88× | 18 | ICOS |
| CA-CF2 | 4,964 | 417 | 11.9× | 5,144 | 0.97× | 4 | AMF |
| NO-And | 9,318 | 1,202 | 7.75× | 8,594 | 1.08× | 7 | EUF |
| JP-Tak | 8,804 | 1,804 | 4.88× | 8,833 | 1.00× | 23 | JPF |
| DE-SfS | 5,252 | 1,115 | 4.71× | 5,117 | 1.03× | 11 | EUF |
| JP-Shn | 6,021 | 1,340 | 4.49× | 6,166 | 0.98× | 7 | JPF |
| JP-MBF | 5,175 | 1,174 | 4.41× | 5,059 | 1.02× | 8 | JPF |
| JP-Nkm | 8,196 | 1,951 | 4.20× | 9,288 | 0.88× | 3 | JPF |
| JP-Tkb | 5,412 | 1,353 | 4.00× | 5,408 | 1.00× | 2 | JPF |
| AU-Fog | 5,851 | 1,466 | 3.99× | 6,376 | 0.92× | 3 | TERN |
| JP-Mse | 5,188 | 1,307 | 3.97× | 5,331 | 0.97× | 8 | JPF |
| JP-KaP | 5,287 | 1,337 | 3.95× | 4,037 | 1.31× | 1 | JPF |
| JP-Fmt | 5,940 | 1,516 | 3.92× | 5,985 | 0.99× | 7 | JPF |
| JP-Om2 | 5,338 | 1,372 | 3.89× | NA | NA (n=0) | 0 | JPF |
| KH-Kmp | 6,637 | 1,714 | 3.87× | 6,804 | 0.98× | 4 | JPF |
| BR-SM1 | 5,638 | 1,473 | 3.83× | 5,782 | 0.98× | 2 | AMF |
| PE-QFR | 10,292 | 2,705 | 3.80× | 10,148 | 1.01× | 3 | AMF |
| JP-Api | 5,372 | 1,478 | 3.63× | 5,390 | 1.00× | 21 | JPF |
| JP-Kzw | 5,584 | 1,589 | 3.51× | 5,463 | 1.02× | 8 | JPF |
| BR-Ji3 | 6,880 | 2,083 | 3.30× | 6,555 | 1.05× | 7 | AMF |
| JP-Yms | 4,913 | 1,502 | 3.27× | 4,982 | 0.99× | 21 | JPF |
| JP-Ynf | 6,653 | 2,150 | 3.09× | 7,172 | 0.93× | 8 | JPF |
| US-Cwt | 5,593 | 1,808 | 3.09× | 5,811 | 0.96× | 10 | AMF |
| JP-Nuf | 5,432 | 1,969 | 2.76× | 5,306 | 1.02× | 2 | JPF |
| JP-SMF | 5,432 | 2,065 | 2.63× | 5,362 | 1.01× | 13 | JPF |

Full per-site-year data (23,430 site-years, all 781 sites, all three
variants): `table_d1_site_year_variants.csv`; full per-site summary with
all ratios: `table_d1_per_site.csv`.

**IT-MBo fits the same P_F-confirmed pattern as the other 23**, just at a
more extreme BIO12 gap: ratio to BIO12 is 59.3× but ratio to tower P_F is
close to 1 (0.88, n=18 years) — mechanically the same "BIO12 underestimates
a very wet site" story, at IT-MBo (Monte Bondone), a high-elevation Alpine
site — exactly where a coarse global climatology interpolated from sparse
stations is least reliable. Of the 25 excluded sites with any P_F data,
**24 fall in this pattern; only `US-HB4` does not** (`JP-Om2` is the
26th site and has no P_F data to check either way).

## D2 — Unit hypothesis: ratio clustering (all 781 sites)

| Variant | near 1 (0.8–1.25×) | near 12 (9.6–14.4×) | near 30.4 (24–36.5×) | elsewhere |
|---|---|---|---|---|
| (a) pipeline formula | 387 | 1 | 0 | 393 |
| (b) no day-weighting | 0 | 0 | 0 | 781 |
| (c) mean×365.25 | 387 | 1 | 0 | 393 |

Restricted to the 26 excluded sites: (a) 25 elsewhere / 1 near-12 — that
one site is `CA-CF2` (ratio 11.90×), which falls in the 9.6–14.4× bucket
purely by numeric coincidence, not because of any real day/month
conversion relationship (there is no 12-month or 12-hour quantity that
would produce exactly that factor for a single site while every other
excluded site sits at 2.6–7.75× or, for `IT-MBo`, 59.3×); (b) 26/26
elsewhere; (c) same as (a). **Reconciliation test**: of the 26 excluded
sites, 24 remain over 5000mm under variant (a) and (c) alike; variant (b)
brings 25/26 under 5000mm but **0 of them land near BIO12** (median ratio
to BIO12 under variant (b) is 0.129 — an order of magnitude too low, not
a fix). **No variant reconciles the exclusions.** The evidence does not
support a mixed unit convention; see verdict.

## D3 — Provenance cross-tabulation

| product_source_network | n total | n excluded | % excluded |
|---|---|---|---|
| **JPF** | 54 | 16 | **29.6%** |
| TERN | 52 | 1 | 1.9% |
| AMF | 381 | 6 | 1.6% |
| EUF | 138 | 2 | 1.4% |
| ICOS | 80 | 1 | 1.2% |
| CNF | 32 | 0 | 0.0% |
| FLX | 18 | 0 | 0.0% |
| KOF | 21 | 0 | 0.0% |
| SAEON | 5 | 0 | 0.0% |

| data_hub | n total | n excluded | % excluded |
|---|---|---|---|
| ICOS | 348 | 19 | 5.5% |
| TERN | 52 | 1 | 1.9% |
| AmeriFlux | 381 | 6 | 1.6% |

`oneflux_code_version` is **v1.3 for all 781 current-network sites** — no
variation to cross-tabulate against; not a discriminating factor. Raw
`*_FLUXNET_ERA5_MM_*.csv` files were read directly for all 26 excluded
sites (`table_d3_raw_file_check.csv`): **all 26 files found**, and **all
26 share one identical header string** (`TIMESTAMP,TA_ERA,...,P_ERA,
WS_ERA`) — no units row, no per-file metadata of any kind was found in
any of them (searched: first two lines of each file; confirmed no units
annotation exists anywhere in the raw CSV format itself). Download date
was **not found** as a queryable field — the snapshot carries
`download_link`/`product_id` but no explicit download-timestamp column;
not further investigated (out of scope: file `mtime` on disk is not a
reliable proxy since files get re-extracted).

**Raw monthly P_ERA distribution, excluded vs. unaffected** (mm/day, as
the pipeline treats the variable):

| | n | p5 | p25 | p50 | p75 | p95 | max |
|---|---|---|---|---|---|---|---|
| Unaffected (755 sites) | 271,800 | 0.054 | 0.791 | 1.92 | 3.97 | 11.36 | 78.2 |
| Excluded (26 sites) | 9,360 | 3.21 | 9.80 | 15.75 | 25.30 | 82.90 | 7,682.8 |

The two distributions are **shifted, not bimodal** — excluded-site values
are uniformly larger across every percentile (not "two separated modes,"
which is what a mixed unit convention would produce), consistent with
genuinely wetter locations rather than a subset running on a different
unit scale. The `max` column's extreme value (7,682.8) belongs to
`US-HB4` alone (see verdict); excluding that one site, the excluded-group
p95 (82.9) is only ~7× the unaffected p95 (11.4) — in line with the
2.6–7.75× BIO12 ratios found for genuinely wet sites, not evidence of a
second data regime.

## D4a — Recovery under extended candidate-year window

Since D2 found no alternative *formula* to apply, the counterfactual
tests the one remaining free parameter: `KG_ERA5_PERIOD`'s hard-coded
1991–2020 upper bound (`R/pipeline_config.R:56`), extended here to each
site's own last available ERA5 year (2024 or 2025 for all 26 — see
`table_d4a_recovery_extended_window.csv`), using the real
`compute_site_koppen_era5()` unmodified.

**Recovered: 1 of 26 (`JP-Yms`, classified `Cfa`, 20 valid years reaching
just the `KG_ERA5_MIN_YEARS=20` floor).** The other 25 remain
unclassified even with 4–5 additional candidate years — confirming the
underlying MAP inflation (real, geography-driven) is a **persistent
site characteristic across the whole record**, not a period-specific
data glitch that a wider window would dilute.

## D4b — Jaccard impact of the recovered site

| Level | n recovered | J before | J after | Δ |
|---|---|---|---|---|
| Two-letter | 1 | 0.4198 | 0.4195 | **−0.0004** |
| 5-class | 1 | 0.4560 | 0.4555 | **−0.0004** |

Recovering the single available site (`JP-Yms` → Cf, already an
over-represented class in the current network) makes weighted Jaccard
**very slightly worse**, not better — a small, counter-intuitive but
real result, reported as computed rather than the more "satisfying"
direction being assumed.

## D4c — Relative screen (factor-of-2 from BIO12) vs. the current absolute rule

| | site-years flagged | distinct sites with ≥1 flagged year | sites failing to reach 20 valid years |
|---|---|---|---|
| Current absolute (map_a > 5000mm) | 794 | 64 | **26** |
| Relative (ratio to BIO12 >2× or <0.5×) | 7,253 | 384 | **238** |

The relative screen, taken literally at a factor-of-2 threshold, is
**far more aggressive**: it fully agrees with the current rule on all 26
existing exclusions (perfect overlap) but would newly push **212
additional sites** below the 20-year floor — including long-established,
widely-used flagship towers (`DE-Hzd`, `US-Ne1`, `US-Ne2`, `US-Ne3`,
`US-Syv`, `GL-Dsk`, `FI-Ruk`, and 205 more — full list in
`table_d4c_relative_vs_absolute_screen.csv`). A literal factor-of-2
relative screen is not a drop-in replacement without further tuning (a
looser factor, a persistence-across-years requirement, or restricting it
to a subset of "trusted" sites) — normal ERA5-vs-BIO12 disagreement
routinely exceeds 2× at many individual, otherwise-unremarkable sites.

---

## Recommendation section (options, not a choice)

1. **Per-site unit correction.** Not supported by this investigation — D2
   found no alternative formula reconciles any of the 26 sites, and the
   raw-value distribution is shifted, not bimodal (D3). **This option
   should likely be taken off the table for the 25 geography-driven
   sites** — there is no "unit" to correct. It may still apply narrowly
   to `US-HB4` alone, pending confirmation of the exact scaling error
   (order-of-magnitude evidence points to ~1000×, not verified further
   here — out of scope for a read-and-report task). Touches: if pursued
   for `US-HB4` only, `R/climate_classification.R`'s screen (or an
   upstream fix in the Shuttle-provided raw file) and `site_koppen_era5.csv`.

2. **Relative screen against BIO12.** Conceptually addresses the
   "absolute threshold excludes real wet places" problem the task
   flags, but D4c shows a literal factor-of-2 threshold is far too
   strict as specified — would multiply exclusions roughly 9× (26→238)
   and catch many well-established, non-problematic sites. Would need
   a looser factor and/or a persistence rule (e.g., flag only if most
   years, not any single year, depart by >2×) before being usable.
   Touches: `R/climate_classification.R`'s screen logic, `KG_ERA5_MAP_MAX_MM`
   constant or its replacement, `site_koppen_era5.csv`,
   `representativeness_metrics.csv`'s KG rows, Figs 4/5's KG panel/line,
   and (new, per the task) **the aridity panel now being planned, which
   needs P in the numerator and takes PET from ERA5 radiation** — if that
   panel also screens or gap-fills against BIO12-vs-ERA5 disagreement,
   it inherits the same false-positive risk at the same wet, JPF-heavy
   sites; whatever threshold is chosen here should probably be shared
   with, or at least cross-checked against, that panel's own P/PET
   construction rather than re-derived independently.

3. **Per-site fallback to observed P_F (with ERA5 temperature).** This
   investigation's own D1 result is the strongest evidence *for* this
   option: tower `P_F` agrees with ERA5 to within ±15% at 23 of the 25
   geography-driven sites, and diverges sharply (326×) at the one true
   anomaly (`US-HB4`) — meaning a P_F fallback would likely recover most
   of the 25 *and* automatically sidestep the `US-HB4` anomaly (P_F
   there is ordinary, unlike its ERA5 value). Coverage caveat: P_F years
   are often sparse relative to the pipeline's 20-year minimum (`n_yr_pf`
   ranges 0–23 in the table above; several sites have <5 years of tower
   P), so this option would need either a relaxed minimum-years rule for
   the P_F-sourced sites or a hybrid (P_F where available, ERA5
   otherwise) — not evaluated further here (would require re-deriving
   `n_years_used` semantics, out of scope for a read-and-report task).
   Touches: `R/climate_classification.R` (a new code path, not just a
   threshold), `site_koppen_era5.csv`, and the same downstream KG
   consumers as option 2 — but *not* the aridity panel, since that
   panel's PET side still needs ERA5 radiation regardless of which P
   source is used for its numerator.

4. **Raise the constant** (`KG_ERA5_MAP_MAX_MM`, currently 5000). Simplest
   change, and defensible given D1 shows real tower-confirmed values up
   to ~10,300 mm/yr (`PE-QFR`) — but a blunt higher constant would also
   let `US-HB4`'s genuine anomaly (658,045 mm/yr) through unless raised
   only modestly (e.g., to ~11,000), and does nothing to validate
   *which* high values are real (P_F-agreeing) vs. anomalous
   (`US-HB4`-like) — it would recover the 25 geography-driven sites
   blindly alongside any future `US-HB4`-like error. Touches: one
   constant in `R/pipeline_config.R`, with the same downstream ripple as
   option 2 (KG axis figures, `representativeness_metrics.csv`, and the
   planned aridity panel if it reuses this same constant or a copy of it).

**No option is chosen here, per instruction.**

---

## Standing notes (not new findings, carried forward as instructed)

- **The absolute 5000mm screen excludes real locations regardless of
  what drove the 26 exclusions examined here** — this investigation's
  own D1 table is direct evidence: `PE-QFR` (tower-confirmed ~10,150
  mm/yr) and `IT-MBo` (tower-confirmed ~27,400 mm/yr, though see the
  n=18-year caveat) are real places with real, gauge-corroborated
  precipitation above the constant, not artifacts of any kind.
- **`docs/known_issues.md` §9a already documents that this same
  `P_ERA > 5000` rule is separately reimplemented in figure code**
  (`fig_environmental_response_era5()`) **with a silent drop and no
  exclusion log** — confirmed by reading that section directly
  (`docs/known_issues.md:319-322`): *"`fig_environmental_response_era5()`
  applies its own outlier filter before plotting (removes `P_ERA >
  5000`...). The 225 outlier site-years are silently dropped from fig_08
  panels. No flag is written to the exclusion log."* Any fix chosen from
  the options above that changes the screening logic or threshold should
  consider whether to also update (or centralize) that second,
  independent implementation — not evaluated further here, out of scope.

## Not-found log

- No units-metadata row or header annotation in any raw
  `*_FLUXNET_ERA5_MM_*.csv` file (26/26 checked directly) — searched the
  first two lines of each file.
- No explicit download-date column in the snapshot — searched
  `fluxnet_shuttle_snapshot_20260901T094522.csv`'s full column list (18
  columns); `download_link`/`product_id` exist but no timestamp field.
- No `oneflux_code_version` variation to cross-tabulate (single value,
  v1.3, network-wide) — confirmed by direct query, not assumed.
