# ERA5 precipitation units, v2: closing the circularity gap in the v1 verdict

> **Provisional — pending store audit (2026-09-20).** The numbers in this report rest on the June 2026 `data/extracted/` extraction. One file in that extraction (IT-MBo's `FLUXNET_FLUXMET_MM` file) is known to differ from the currently-distributed archive under the same product ID. This report's conclusions are provisional pending `review/diagnostics/store_audit/` (in progress).

**Type:** Read-and-report diagnostic + counterfactual, confined to the
diagnostics folder, for a co-author decision. Supersedes the verdict in
`review/diagnostics/era5_precip_units/report.md` (v1). No edits to the
pipeline, figures, legends, snapshot CSVs, or the v1 report/outputs — all
new work is in `scripts/diagnostics/era5_precip_units_v2.R` and this
directory. `R/climate_classification.R` was read only, never edited.

---

## Verdict

**No units problem exists, anywhere, for any of the 26 sites — that part
of v1 is confirmed and now rests on decisive, non-circular evidence. But
v1's further claim that the 24 non-`US-HB4` sites are "genuinely wet,
real places" does not survive scrutiny: its own supporting evidence (tower
`P_F`) is not independent of ERA5, even restricted to nominally
fully-measured months, and a genuinely independent source (BADM
PI-reported MAP) disagrees with ERA5 by the same magnitude as WorldClim
BIO12 at nearly every one of those sites. The honest result is: zero sites
show a units problem; two sites (`US-HB4`, `IT-MBo`) are confirmed genuine
data errors by convergent, non-circular evidence; the remaining 24 sites'
true climate cannot be determined from data available in this repository
— the two theoretically-independent checks point in opposite directions,
and one of them turns out not to be independent.**

- **Definition (T1) — settled, decisively, non-circularly.** Every
  FLUXNET Shuttle site download bundles an annual-resolution
  `*_FLUXNET_ERA5_YY_*.csv` file, produced entirely independently by
  ONEFlux/AmeriFlux's own official processing. Summing
  `P_ERA_month × days_in_month` from the monthly file and comparing
  against that official annual file's own `P_ERA` value gives a ratio of
  **1.0000 (median), range 0.9947–1.0068, across 2,955 site-years at 66
  sites** (all 26 excluded sites plus a random sample of 40 unaffected
  ones) — including sites with the most extreme values, `US-HB4`
  (ratio 1.0000 at the site-year level) and `IT-MBo` (1.0000). **P_ERA at
  MM resolution is a mean daily rate, and the pipeline's day-weighting
  formula is exactly correct — for every site, including the anomalous
  ones.** This directly contradicts the framing in the task and the
  2026-08-20 log that the ICOS reference script's non-multiplication
  might reflect a legitimate alternative convention: `methods_koppen_era5.md`
  itself is self-contradictory on this point (see "Documentation
  discrepancy" below), but the ONEFlux product's own internal arithmetic
  settles it independently of what any reference script does.
- **The circularity concern is real (T2).** `P_F` (tower-observed) at MM
  resolution is *also* a mean daily rate — confirmed directly: for
  `US-Akn`'s nominally fully-measured (`P_F_QC=0`) 2011 months, `P_F`
  equals `P_ERA` to three decimal places (0.080, 0.135, 0.148 …). Beyond
  precipitation, `TA_ERA`/`VPD_ERA` are **numerically identical** to
  `TA_F`/`VPD_F` at `P_F_QC`-equivalent fully-measured months at every
  site checked, **including `IT-MBo` and `US-HB4`** (mean difference
  0.000, correlation 1.000). A "measured" meteorological flag does not
  mean independent of ERA5 in this product — v1's central evidentiary
  claim (P_F confirms ERA5) cannot be trusted even after restricting to
  `P_F_QC=0`, because that flag does not reliably mark ERA5-independent
  values for these consolidated meteorological variables.
- **A genuinely independent check disagrees with ERA5 (T2/T3).** BADM's
  PI-reported `MAP` field exists for 20 of the 26 excluded sites and
  disagrees with ERA5 by 2.7×–460× (median ≈4×) at **every one of the 20**
  — the same order of magnitude as the BIO12 disagreement v1 already
  reported, not close to 1 for any of them. BADM MAP is self-reported and
  has known reliability problems in this project (the same 2026-08-20 log
  entry that documents the KG classification also reports only 56.0%
  agreement between BADM's own `CLIMATE_KOEPPEN` field and the ERA5-based
  classification), so this is suggestive, not conclusive — but it is
  evidence v1 did not have, and it points away from v1's conclusion, not
  toward it.
- **Two sites are confirmed genuine errors, independent of the above
  uncertainty.** `US-HB4`: physically impossible raw values (mean 1798
  mm/day across its full 1981–2025 monthly record — see v1 report),
  present identically in ONEFlux's own official YY product (T1), with
  essentially zero fully-measured tower months to check against and a
  460× BADM disagreement. `IT-MBo`: only 8 fully-measured tower months
  exist in the entire 1991–2020 window (below the 12-month floor used
  here to call a comparison reliable), a 17.7× BADM disagreement, and — as
  the task states directly — an independently known true value on the
  order of 1200 mm/yr against an ERA5-derived value of 15,000–27,000+
  mm/yr, which exceeds any recorded annual total anywhere on Earth.
  **Both series v1 cited for IT-MBo (ERA5-derived MAP and tower P_F) are
  wrong there, exactly as the task states** — this report does not
  attempt to defend that number.

---

## Claims from v1 being overturned or corrected (quoted directly)

> v1: *"BIO12 is the outlier reference here, not ERA5"* — **overturned as
> a general claim.** It may still be true for some subset of the 24
> remaining sites, but the evidence used to support it (P_F agreement) is
> not valid, and BADM (an independent check v1 did not use) disagrees
> with ERA5 at the same 20 sites BIO12 disagrees with.

> v1: *"24 of 26 excluded sites... show the pipeline's existing formula...
> agreeing closely with tower-observed annual precipitation... while
> disagreeing substantially... with WorldClim BIO12"* — **the P_F-based
> half of this comparison is not independent evidence**, per T2's direct
> demonstration that `TA_ERA`/`VPD_ERA` match "measured" tower values
> exactly at the same sites. The comparison itself (numbers) still holds;
> its interpretation as *confirmation* does not.

> v1: *"IT-MBo... ratio to tower P_F is close to 1 (0.88, n=18 years)"* —
> **the n=18 came from the annual-resolution DuckDB table without checking
> fill status.** Restricting to genuinely fully-measured months (T2, this
> report) drops that to **n=8 months**, below this report's own
> reliability floor, and the task's independent information that IT-MBo's
> true MAP is ~1200 mm/yr confirms that whatever those 8 months show is
> not a reliable confirmation.

> v1: *"there is no alternative formula to switch to"* — **confirmed, now
> with authoritative (not inferential) evidence** (T1).

---

## T1 — MM-vs-YY internal consistency (the decisive test)

| | value |
|---|---|
| Site-years tested | 2,955 |
| Sites tested | 66 (all 26 excluded + 40 random unaffected) |
| Ratio (official YY ÷ computed-from-MM), median | **1.0000** |
| Ratio range | 0.9947 – 1.0068 |
| Sites deviating from 1.0 by >1% | **0** |

Full table: `table_t1_mm_yy_consistency.csv`. This is the single most
important result in this follow-up: it is the one test in either report
that involves **no tower data at all** — both files it compares come from
the same official, externally-produced ONEFlux/AmeriFlux product,
entirely outside this repository's code.

**Documentation discrepancy, code treated as authoritative per
instruction:** a targeted external search for FLUXNET/ECMWF documentation
of `P_ERA`'s specific units at MM resolution found no FLUXNET-specific
statement (searched fluxnet.org, ameriflux.lbl.gov, and the ONEFlux/AmeriFlux
FLUXNET-product pages directly — not found). Generic ECMWF Copernicus
documentation for the standard "ERA5 monthly averaged" product does
confirm the same convention this pipeline assumes ("monthly means...
scaled to have units... 'per day'"; official conversion recipe: *"total
(mm per month) = 1000 × total precipitation × number of days/month"* —
i.e., exactly `× days_in_month`), consistent with, but not itself proof
for, the FLUXNET-bundled product specifically. `review/figures/representativeness/methods_koppen_era5.md`
and `SESSION_LOG.md`'s 2026-08-20 entry **contradict each other** on
whether the ICOS reference script multiplies by `days_in_month`: the
methods doc says `compute_era5_monthly_climatology()` "multiplies by the
number of days in each month... **matching** the ICOS reference
implementation," while the session log says skipping that step is
"**as the ICOS script does**" (i.e., claims ICOS does *not* multiply).
Both are quoted here as found; T1's internal-consistency result makes
resolving which description of ICOS's script is accurate unnecessary for
this pipeline's own correctness, since it is verified directly against
the bundled product rather than against ICOS's script.

## T2 — Ratios: BIO12, tower P_F (all months vs. P_F_QC=0 only), BADM MAP

Per-site table (26 excluded sites), sorted by ratio to BIO12:

| site | n qualifying (QC=0) months | ratio→BIO12 | ratio→P_F(QC=0 only) | ratio→BADM MAP | BADM present |
|---|---|---|---|---|---|
| US-HB4 | 0 (**too few**) | 487.4× | n/a | 460.5× | yes |
| IT-MBo | 8 (**too few**) | 59.3× | 0.94× | 17.7× | yes |
| CA-CF2 | 28 | 11.9× | 1.47× | 11.0× | yes |
| NO-And | 55 | 7.75× | 1.08× | 8.79× | yes |
| JP-Tak | 276 | 4.88× | 1.00× | 3.87× | yes |
| DE-SfS | 132 | 4.71× | 1.03× | 4.66× | yes |
| JP-Shn | 84 | 4.49× | 0.98× | 3.75× | yes |
| JP-MBF | 96 | 4.41× | 1.02× | — | no |
| JP-Nkm | 36 | 4.20× | 0.88× | — | no |
| JP-Tkb | 24 | 4.00× | 1.00× | 3.59× | yes |
| AU-Fog | 36 | 3.99× | 0.92× | — | no |
| JP-Mse | 96 | 3.97× | 0.97× | 4.32× | yes |
| JP-KaP | 12 | 3.95× | 1.31× | **293.7×** | yes |
| JP-Fmt | 84 | 3.92× | 0.99× | 3.62× | yes |
| JP-Om2 | 0 (**too few**) | 3.89× | n/a | 4.42× | yes |
| KH-Kmp | 48 | 3.87× | 0.98× | 4.42× | yes |
| BR-SM1 | 24 | 3.83× | 0.98× | 3.32× | yes |
| PE-QFR | 36 | 3.80× | 1.01× | — | no |
| JP-Api | 252 | 3.63× | 1.00× | 2.87× | yes |
| JP-Kzw | 96 | 3.51× | 1.02× | 4.66× | yes |
| BR-Ji3 | 84 | 3.30× | 1.05× | — | no |
| JP-Yms | 252 | 3.27× | 0.99× | 4.49× | yes |
| JP-Ynf | 96 | 3.09× | 0.93× | 2.66× | yes |
| US-Cwt | 120 | 3.09× | 0.96× | 3.11× | yes |
| JP-Nuf | 24 | 2.76× | 1.02× | 3.41× | yes |
| JP-SMF | 156 | 2.63× | 1.01× | 3.36× | yes |

Full table with all fields (including `frac_filled`, the tower's
gap-filled month fraction): `table_t2_ratios.csv`. **Three sites
(`US-HB4`, `IT-MBo`, `JP-Om2`) have too few (<12) fully-measured months
in 1991–2020 to test against P_F at all** — flagged explicitly, not
silently skipped. `implausible_raw` (an ERA5-derived MAP >15,000 mm/yr,
i.e. beyond any credible sustained multi-year mean) flags only `US-HB4`.

**Why the P_F(QC=0)-only column looks like confirmation but isn't fully
trustworthy:** 23 of the 23 testable sites show ratio 0.88–1.47 — on its
face, strong agreement. But `TA_ERA`/`VPD_ERA` were checked directly
against `TA_F`/`VPD_F` at the same QC=0 months for `IT-MBo`, `US-HB4`,
and `JP-Tak`: **exact numerical equality in every case** (mean
difference 0.000, correlation 1.000). A meteorological variable flagged
"fully measured" that is bit-identical to the ERA5 reanalysis value is
not measuring independence — whether by direct copy-through in this
product's own gap-filling/consolidation logic, or because the "measured"
flag does not mean what it appears to for these particular consolidated
fields. Either way, the P_F(QC=0) comparison for precipitation cannot be
relied on as an independent check, and this report does not treat it as
one.

## T3 — What groups the offsets

**Clustering near candidate factors** (within 15%), all 781 sites:

| factor | n within 15% of factor, ratio→BIO12 | n within 15%, ratio→P_F(QC0) |
|---|---|---|
| 1 | 307 | 287 |
| 4 | 105 | 2 |
| 8 | 7 | 1 |
| 12 | 1 | 1 |
| 24 | 0 | 0 |
| 30.4 | 0 | 0 |
| 1000 | 0 | 0 |

No network-wide clustering near 12, 24, 30.4, or 1000 — reinforcing T1's
conclusion that there is no unit-convention population anywhere in the
network. The ratio-to-BIO12 distribution instead shows two real
populations: ~307 sites near 1× and ~105 near 4× (full histogram in
`table_t2_ratios.csv`) — a genuine bimodal split, but by **geography/hub**,
not by units.

**Provenance cross-tabulation** (unchanged from v1, reconfirmed here):
`product_source_network == "JPF"` median ratio-to-BIO12 = 3.72 vs. 0.97–1.18
for every other hub; `oneflux_code_version` is v1.3 network-wide (no
variation to explain anything); `fluxnet_product_name`/`product_id` are
effectively unique per site (not usable as a grouping variable — shown in
`table_t3_provenance_crosstab.csv` for completeness only).

**Regression: log(ratio-to-BIO12) ~ fraction of tower-P_F months
gap-filled.** Significant negative slope network-wide (−1.41, p<10⁻⁸⁰)
and **within JPF alone** (−1.45, p<10⁻²⁶) and **within non-JPF alone**
(−1.36, p<10⁻⁶¹) — i.e. sites with *more* tower gap-filling tend to show
*smaller* ERA5-vs-BIO12 disagreement, and this holds inside both
subgroups, not just because JPF sites happen to have different fill
rates. This is a real, robust statistical pattern, but it is a
**description of a covariate**, not a resolution of batch-vs-mixture: it
does not itself involve P_F's value (only its fill-status), so it cannot
distinguish "wetter real climate" from "a JPF-specific extraction issue"
on its own — reported factually, not over-interpreted. JPF's elevated
median (3.63×) holds regardless of fill fraction (`table_t3_regression.csv`),
which at minimum rules out "this is purely a tower-gap-filling artifact
of the P_F comparison, unrelated to the site's actual hub/location."

## T4 — Recovery (carried forward from v1, unaffected by the circularity correction)

D4a-c are mechanical tests of `compute_site_koppen_era5()` (extended
candidate-year window; relative-vs-absolute screen comparison) that never
touch `P_F` or BADM — they are not affected by this report's correction
and are not re-run. Carried forward verbatim:
`table_d4a_recovery_extended_window_carried_from_v1.csv`,
`table_d4b_jaccard_impact_carried_from_v1.csv`,
`table_d4c_relative_vs_absolute_screen_carried_from_v1.csv`.

- Extending the candidate window from 1991–2020 to each site's full record
  recovers **1 of 26** (`JP-Yms` → `Cf`), moving weighted Jaccard by
  **−0.0004** at both two-letter and 5-class levels (very slightly worse).
- A literal factor-of-2 relative screen against BIO12 would push **238**
  sites (vs. 26 currently) below the classification floor.
- With no factor to "apply" (T1: none exists), there is nothing new to
  recompute here beyond what v1 already reported — the open question this
  report adds is not "what factor recovers these sites" but "which of the
  26 should be recovered at all," which T2/T3 leave genuinely unresolved
  for 24 of them.

## T5 — Reach: does this affect other variables and consumers?

**Other ERA5 variables, tested at `US-HB4`, `IT-MBo`, `JP-Tak`
(fully-measured months only):** `TA_ERA` vs. `TA_F` and `VPD_ERA` vs.
`VPD_F` are **exactly identical** at all three sites (already reported
above as the key T2 caveat) — meaning the entanglement between "measured"
tower meteorology and ERA5 in this product is not precipitation-specific;
it applies to temperature and VPD too. This was not previously documented
anywhere found in this repo.

**Every script/function reading `dataset = 'ERA5'`** (grepped directly,
not assumed): `R/climate_classification.R` (the KG screen itself),
`R/figures/fig_environmental_response.R` (draft-candidate fig_08, per
`docs/known_issues.md` §9a — already flagged there as silently dropping
`P_ERA > 5000` site-years with no exclusion log), `R/figures/fig_climate_legacy.R`
(**a third, independent reimplementation** of essentially the same
`P_ERA` threshold screen, explicitly commented *"excluded (likely ERA5
unit-conversion artifacts)"* at that file's own line 66-67 — the same
unverified "units" framing this report now closes out, found in yet a
third piece of code; this function is marked `Deprecated` in its own
title, so likely not live, but the assumption it encodes was never
correct either way), `scripts/generate_env_response_era5.R` (the driver
for fig_08), and `scripts/step5_compute_koppen_era5.R` (the KG driver).
`scripts/03_read.R`, `scripts/duckdb_setup.R`, `scripts/duckdb_update.R`
are pipeline ingestion infrastructure, not analytical consumers.

**The planned aridity panel** (P in the numerator, PET from ERA5
radiation, per the task's own description): no repository documentation
of this planned panel was found — searched `docs/decisions_pending.md`,
`docs/methods_requirements.md`, `docs/known_issues.md` directly for
"PET"/"potential evapotranspiration" (no matches). It is described only
in this conversation. Given T2/T5's findings, that panel should be
designed with two things in mind: (1) whatever P source it uses inherits
the exact same JPF-cluster uncertainty documented here if it reads
`P_ERA`; (2) if it plans to validate its PET/radiation inputs against
tower `SW_IN_F`/other consolidated meteorological fields at "measured"
QC months, this report's finding that such fields can be bit-identical to
ERA5 at nominally-measured months means that validation would not be
independent either, at least not without the same direct raw-value check
performed here.

## T6 — Upstream table (Coordination Project / affected hubs)

| site | hub (`data_hub` / `product_source_network`) | product version | inferred cause | evidence |
|---|---|---|---|---|
| US-HB4 | AmeriFlux / AMF | v1.3 | **Site-specific extraction error** (not a version or hub-wide issue — no other AMF site shows this) | Every one of 539 raw monthly values 1981–2025 in the 74–7683 mm/day range (physically impossible); identical in ONEFlux's own official YY product; 460× BADM disagreement; near-zero measured tower months to check |
| IT-MBo | ICOS / ICOS | v1.3 | **Site-specific extraction error** | ERA5-derived MAP 15,000–27,000+ mm/yr vs. independently known true value ~1200 mm/yr (per task); only 8 fully-measured tower months in 30 years; 17.7× BADM disagreement |
| JP-Tak, DE-SfS, JP-Shn, JP-MBF, JP-Nkm, JP-Tkb, AU-Fog, JP-Mse, JP-KaP, JP-Om2, KH-Kmp, BR-SM1, PE-QFR, JP-Api, JP-Kzw, BR-Ji3, JP-Yms, JP-Ynf, US-Cwt, JP-Nuf, JP-SMF | mostly ICOS/JPF (16 of these 21), remainder EUF/AMF/TERN | v1.3 (all) | **Undetermined — not a units/version issue (T1 rules that out definitively); genuinely wet climate vs. a JPF-hub-correlated ERA5 extraction issue cannot be distinguished with data in this repository** | JPF-hub concentration (29.6% exclusion rate vs. 1.2–1.9% elsewhere, hub-wide not just excluded subset); BADM disagrees 2.7–4.7× at every site with an entry; tower P_F "confirmation" is not independent (T2) |
| CA-CF2, NO-And | AmeriFlux/AMF, ICOS/EUF | v1.3 | **Undetermined**, same caveats as above, but with somewhat weaker P_F(QC0) agreement (1.47×, 1.08×) than the JPF-pattern sites | 11.0× and 8.79× BADM disagreement, the two largest among sites with reasonable qualifying-month coverage |

**This is not a version-level definition change**: `oneflux_code_version`
is v1.3 uniformly across all 781 current-network sites (T3) — there is no
version heterogeneity in the current network to attribute this to, and
T1's exact internal consistency holds regardless of hub or magnitude. If
there is a cause beyond genuine climate, it is a **site- or hub-specific
extraction/interpolation issue** (e.g., in how ONEFlux/AmeriFlux resolves
site coordinates to an ERA5 grid cell for JPF-network sites specifically),
not a product-version or units-convention problem.

---

## Recommendation section (unchanged options from v1, now re-scoped)

The four options v1 listed (per-site unit correction, relative screen,
P_F fallback, raise the constant) all still apply to the *screening*
question, but **v1's option 3 (per-site fallback to observed P_F) should
be reconsidered**: this report shows tower P_F is not a reliable
independent fallback at these specific sites, since it is itself
entangled with ERA5 in this product. A defensible path forward should
treat `US-HB4` and `IT-MBo` as confirmed data-quality issues (candidates
for site-specific flagging or correction upstream, not a KG-algorithm
question), and treat the remaining 24 as **unresolved** rather than
recovered — keeping the current absolute screen's exclusion for them is
defensible precisely because their true climate cannot currently be
confirmed either way. No option is chosen here, per instruction.

## Not-found log

- No FLUXNET/AmeriFlux-specific documentation of `P_ERA`'s MM-resolution
  units was found (searched fluxnet.org, ameriflux.lbl.gov FLUXNET-product
  pages) — T1's internal YY-file consistency check substitutes for this
  and is more authoritative than a generic documentation statement would
  have been.
- No repository documentation of the planned P/PET aridity panel was
  found (searched `docs/decisions_pending.md`, `docs/methods_requirements.md`,
  `docs/known_issues.md` for "PET"/"potential evapotranspiration").
- `BIFVARINFO_MM` files (present per-site alongside the raw FLUXNET
  archive) were checked for ERA5 unit metadata and contain none — only
  ONEFlux processing metadata (`PRODUCT_FIRST_YEAR`, etc.), not per-variable
  units.
