# GSL site-selection audit (2026-10-30)

Full prompt: `logs/prompts/2026-10-30_gsl_selection_audit.md`. Script:
`GSL_definition/code/11_selection_audit.R` (read-only on the repo-root DuckDB store,
`dataset = 'FLUXMET'` rows only). Tables: `GSL_definition/tables/selection_audit_*.csv`.
Figure: `GSL_definition/figures/selection_audit_long_record_variants.png/.pdf`. **This audit does
not change the 62-site GSL analysis, its code, or any of its existing outputs** — it is a
separate, read-only diagnostic.

Question: which long-running FLUXNET sites does the manuscript's selection rule exclude, and
would a rule that drops individual days or years, rather than the whole site, keep them?

## The manuscript rule, as stated in the prompt

1. Drop IGBP classes CRO, CVM, WET, SNO, BSV.
2. A day passes if `NEE_VUT_REF_QC >= 0.25`.
3. A year passes only if every day passes.
4. A site needs >= 5 passing years.
5. Drop sites with > 10% of daily `GPP_NT_VUT_REF` below -1 gC m⁻² d⁻¹.

Rule 2 is implemented **literally** as `NEE_VUT_REF_QC` only, with no CUT fallback — unlike the
per-site VUT/CUT fallback `04_qc.R` and the 62-site GSL analysis itself use. This choice matters a
great deal for the result (see "Long-record sites still excluded under every variant tried — and
why" in section 3) and is not a bug.

"NA counts as failing" (prompt instruction 2) applies both to a day whose `NEE_VUT_REF_QC` is `NA`
and to a calendar day with no row at all — every site-year is screened on the full Jan 1–Dec 31
calendar grid (365/366 expected days), not just the days actually present in the extract, so a
site-year with missing calendar days cannot look better than one with the same count of low-QC
days.

## 1. Population

**507 sites** from the 20 September snapshot (781 total) pass the IGBP rule and have at least one
daily FLUXMET row in DuckDB; this is the population for every step below.

- **92 sites network-wide** are "long record" (>= 15 years with 12 months of data present, from
  `data/snapshots/site_year_data_presence.csv`); **74 of these 92** are in the population (the
  other 18 were dropped by the IGBP rule or have no daily FLUXMET rows).
- Of the 74 long-record population sites, **37 are already in the GSL 62** and **37 are not**.

Table: `selection_audit_population.csv` (one row per population site: IGBP, network, lat/long,
record span, `any_vut_qc`/`any_cut_qc`, `is_long_record`, `in_gsl62`).

## 2. Per site-year screening

4,562 population site-years (>= 1 day of data) were screened on the full calendar grid. Per
site-year: `share_pass_025`/`share_pass_050`, `n_fail_025`/`n_fail_050`, `longest_fail_run_025`/
`longest_fail_run_050`, and failing-day (0.25) counts by calendar month.

Tables: `selection_audit_site_year_screen.csv` (4,562 rows), `selection_audit_site_year_month_fail.csv`
(19,970 rows, long format, months with zero failing days omitted).

## 3. Rule variants

| Variant | Sites kept (>= 5 yr) | Site-years kept | Sites added vs. manuscript | Long-record sites added |
|---|---:|---:|---:|---:|
| i. manuscript (0.25, every day) | 68 | 549 | 0 | 0 |
| ii. 0.25, >= 95% | 158 | 1,655 | 90 | 22 |
| ii. 0.25, >= 90% | 187 | 2,060 | 119 | 23 |
| ii. 0.25, >= 80% | 225 | 2,513 | 157 | 24 |
| iii. 0.50, >= 95% | 140 | 1,420 | 72 | 19 |
| iii. 0.50, >= 90% | 169 | 1,863 | 101 | 22 |
| iii. 0.50, >= 80% | 219 | 2,412 | 151 | 24 |
| iv. gaps <= 3 d | 114 | 1,082 | 46 | 18 |
| iv. gaps <= 7 d | 136 | 1,406 | 68 | 22 |
| iv. gaps <= 14 d | 162 | 1,757 | 94 | 23 |

"Sites kept" is gated on the >= 5-year threshold (rule 4) under that variant's own per-year rule —
a site with 1–4 qualifying years is not counted as kept. Full per-site, per-variant counts:
`selection_audit_site_variant.csv`; variant-level summary with the exact added-site lists:
`selection_audit_variant_summary.csv`.

### Long-record sites recovered

Of the 37 long-record sites not in the GSL 62: 2 (GF-Guy, US-Aud) already pass the manuscript rule
on our data despite not being in the GSL 62 (see section 6), **24 are recovered by at least one
looser variant** (pass >= 5 years under some looser rule, though not under the manuscript rule),
and 11 are still excluded under every variant tried (next subsection). The 24 recovered:

BE-Bra, BE-Vie, BR-Ma2, CA-Cbo, CA-TP1, CH-Fru, CZ-BK1, CZ-Stn, DE-Obe, FR-FBn, IT-Ren, JP-Api,
JP-Tak, JP-Tef, JP-Yms, RU-Fyo, US-Bar, US-GLE, **US-Ha1**, US-ICh, US-Kon, US-NC2, US-SP1, US-Vcm.

This includes 4 of the 6 sites the prompt named as expected (US-Ha1, BE-Vie, CA-Cbo, RU-Fyo); the
other two (US-NR1, US-Ho2) are not recovered — see below, they turn out to be excluded for a
different reason than QC burstiness. The easiest variant to pass, `ii_025_ge80` (>= 80% of days at
the 0.25 threshold), alone recovers 24 of the 24 (every recovered site clears the loosest
share-based bar); the strictest, `iv_gap3`, recovers 18 of them.

Full detail (which variant(s) recovered each site): `selection_audit_long_record_recovery.csv`.

### Long-record sites still excluded under every variant tried — and why

**11 long-record sites** fail every variant, including the loosest (`iv_gap14`): AU-Tum, CA-Ca3,
CA-LP1, DE-Lnf, FR-Hes, US-Ho2, US-NR1, US-Syv, US-Uaf, US-WCr, US-Whs.

**All 11, with no exception, have zero non-NA `NEE_VUT_REF_QC` values anywhere in our FLUXMET
daily extract** — this set is *exactly* the set of long-record population sites with
`any_vut_qc == FALSE`. None of the day/year-tolerance variants (ii, iii, iv) can ever help here,
because every one of them is still built on `NEE_VUT_REF_QC` only: with every day `NA`, every
variant's per-day pass count is 0 regardless of threshold or gap tolerance. This is **not** the
bursty/sparse-QC pattern the variants were designed to test — it is a data-coverage gap in the
VUT variable specifically. All 11 do have substantial `NEE_CUT_REF_QC` data instead (6,739–9,586
non-NA daily values each — see `selection_audit_long_record_recovery.csv`'s `any_cut_qc` column),
so a rule that added the project's own standard per-site VUT/CUT fallback (as `04_qc.R` and the
62-site GSL analysis already do, but which the manuscript's literal rule 2 and this audit's
variants (ii)–(iv) do not) might recover some or all of them — **that fallback was not tested
here**, since it was not one of the variants the prompt specified, but it is the most direct
explanation available for why these particular long-running, well-known sites (including US-NR1,
Niwot Ridge, and US-Ho2, Howland Forest) don't show up as recoverable above even though the prompt
flagged them as plausible candidates.

## 4. Who the manuscript rule removes

Kept (68 sites, pass rule i) vs. lost (439 sites, fail rule i), numbers only:

| | n sites | mean record span (yr) | median record span (yr) | mean MAT (°C, WorldClim) | median MAT (°C) | mean \|latitude\| |
|---|---:|---:|---:|---:|---:|---:|
| Lost | 439 | 7.7 | 6 | 10.43 | 10.12 | 40.0 |
| Kept | 68 | 17.6 | 18 | 12.06 | 10.09 | 39.4 |

Median MAT and mean absolute latitude are nearly identical between kept and lost; mean MAT differs
more than the median (12.1 vs. 10.4 °C), consistent with a right-skewed MAT distribution among the
smaller kept group rather than a systematic cold/warm split. Record length differs sharply, as
expected given rule 4 requires >= 5 complete years outright.

IGBP, kept vs. lost (site counts):

| IGBP | Lost | Kept |
|---|---:|---:|
| CSH | 11 | 1 |
| DBF | 69 | 12 |
| DNF | 12 | 1 |
| EBF | 37 | 7 |
| ENF | 98 | 16 |
| GRA | 128 | 18 |
| MF | 23 | 1 |
| OSH | 37 | 4 |
| SAV | 12 | 2 |
| WSA | 12 | 6 |

Full table: `selection_audit_kept_vs_lost.csv` (per-site), `selection_audit_kept_vs_lost_summary.csv`,
`selection_audit_kept_vs_lost_igbp.csv`.

### Failing-day concentration in local winter at cold vs. warm sites

"Local winter" = Dec/Jan/Feb for Northern-hemisphere sites, Jun/Jul/Aug for Southern; cold/warm
split at the population median `mat_worldclim` (10.12 °C):

| Temperature group | Failing days (0.25), total | ... in local winter | Share in local winter |
|---|---:|---:|---:|
| Cold (below median MAT) | 237,899 | 71,252 | 0.300 |
| Warm (at/above median MAT) | 178,954 | 47,398 | 0.265 |

A uniform (non-seasonal) distribution of failures across the 3 winter months of 12 would give a
25% share. Both groups exceed 25%; the cold group's share (30.0%) is somewhat higher than the warm
group's (26.5%). Numbers only, no further interpretation. Table:
`selection_audit_winter_concentration.csv`.

## 5. Rule 5 (negative GPP) check for every recovered site

None of the 24 long-record sites recovered by any variant would instead fail rule 5 (> 10% of
daily `GPP_NT_VUT_REF` below -1 gC m⁻² d⁻¹), checked over each site's entire available daily
record. Shares range from 0.02% (JP-Tak) to 3.05% (US-GLE) among the 23 sites with GPP data — all
far below the 10% cutoff. **BR-Ma2 has zero `GPP_NT_VUT_REF` data at all** (0 of 0 days; it was
recovered via `GPP`-independent QC screening, but rule 5 is undetermined — `NA`, not
`FALSE` — for this site, not evaluated as passing).

Table: `selection_audit_recovered_rule5_check.csv`.

## 6. Sites passing the manuscript rule on our data but not in the GSL 62

6 population sites pass the manuscript rule (i) on our data (>= 5 years, every day passing 0.25)
but are not among the GSL 62 — listed only, not resolved, per the prompt:

| Site | IGBP | Network | Latitude | Record span (yr) |
|---|---|---|---:|---:|
| AU-Cow | EBF | OzFlux | -16.24 | 17 |
| AU-Rob | EBF | OzFlux | -17.12 | 13 |
| GF-Guy | EBF | European Fluxes Database;ICOS | 5.28 | 22 |
| ID-PaB | OSH | AsiaFlux;JapanFlux | -2.34 | 13 |
| IT-BFt | DBF | European Fluxes Database;ICOS | 45.20 | 7 |
| US-Aud | GRA | AmeriFlux | 31.59 | 23 |

This matches the 5 sites Stage 1's independent Tash-rule reproduction (`tash_rule_reproduction_diff.csv`,
2026-10-29, a differently-scoped network-wide check with no IGBP pre-filter and an explicit
US-Aud exclusion) already flagged as "added": AU-Cow, AU-Rob, GF-Guy, ID-PaB, IT-BFt — plus US-Aud
itself, which that earlier check deliberately excluded by name rather than finding it not to
qualify. Table: `selection_audit_passes_rule_not_gsl62.csv`.

## 7. Figure

`figures/selection_audit_long_record_variants.png/.pdf` — one row per long-record site (74 rows,
GSL-62 sites marked with `*` and listed first), years on the x-axis, each year-tile coloured by the
strictest rule variant it passes (grey = fails every variant tried). Companion `.legend.txt` lists
the strictness order used for colouring.

**Caveat on "strictest variant passed":** the colour order (manuscript → iii-95 → ii-95 → gap-3 →
iii-90 → ii-90 → gap-7 → iii-80 → ii-80 → gap-14) is a reasonable-strictness heuristic, not a
formally proven total order. Within each threshold level, 0.50 is stricter than 0.25 (a day
passing 0.50 always passes 0.25, so `share_pass_050 <= share_pass_025` for every site-year) and a
smaller %/gap is stricter than a larger one — those orderings are exact. But the gap-based (iv) and
share-based (ii/iii) families are not strictly nested against each other: a site-year with one
short, deep QC gap can pass a gap variant while failing a share variant at a nominally similar
tolerance, or vice versa. The figure's colour choice does not affect any of the counts in sections
3–6 above, which are all computed directly from each variant's own pass/fail definition.

## Summary

- Population: 507 sites after the IGBP rule; 74 are long-record, 37 of those already in the GSL 62.
- Manuscript rule (i) keeps 68 of the 507 population sites (549 site-years).
- Every tested looser variant recovers more sites, long-record sites, and site-years than the
  manuscript rule, monotonically from `iv_gap3` (+46 sites, +18 long-record) to `ii_025_ge80`
  (+157 sites, +24 long-record) — see the table in section 3.
- Of 37 non-GSL-62 long-record sites: 2 already pass the manuscript rule, 24 more are recoverable
  by at least one looser variant (none of them at risk of instead failing rule 5), and 11 are
  excluded under every variant tried.
- The 11 long-record sites no variant recovers are excluded by a data-coverage gap in
  `NEE_VUT_REF_QC` specifically (all 11 have substantial `NEE_CUT_REF_QC` data instead), not by
  bursty or sparse QC that a day/year-tolerance rule could address — a VUT/CUT fallback, not
  tested here, is the more direct lever for this group.
- 6 population sites pass the manuscript rule on our data but are not in the GSL 62 (listed, not
  resolved).
- Kept vs. lost sites under the manuscript rule differ sharply in record length but not clearly in
  median temperature or latitude; failing days are somewhat more concentrated in local winter at
  colder sites than at warmer ones (30.0% vs. 26.5% of failing days, vs. a 25% uniform baseline).
