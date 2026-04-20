> Technical bugs and data quality issues with external dependencies (fluxnet R package,
> FLUXNET Shuttle) are tracked in [docs/known_issues.md](known_issues.md), not here.

## QC gating — RESOLVED 2026-04-20

~~US-Ha1 (Harvard Forest) excluded entirely at QC_THRESHOLD_YY = 0.75 — all 35 FLUXMET YY
records exceed gap-fill threshold.~~

**Decision (2026-04-20):** Row exclusion gated on `NEE_VUT_REF_QC` only, threshold 0.50.
Gating on all `_QC` columns caused 347 of 672 sites (52%) to lose every annual record
because a single secondary variable (e.g., `RECO_NT_VUT_REF` with high winter gap-fill)
contaminated all years even when NEE itself was well-observed. Secondary variable QC
columns (GPP, RECO, LE, H) are retained in the output for per-variable filtering by
downstream figure functions but do not drive row exclusion. Implemented in `R/qc.R`
and `scripts/04_qc.R`.

---

## Unit conversion — RESOLVED 2026-04-20

**Decision (2026-04-20):** Pre-integrated annual and monthly carbon flux data (NEE, GPP,
RECO at YY and MM resolutions) passed through unchanged. These variables are already
expressed in gC m⁻² per period in the FLUXNET published data — no conversion factor is
applied. An 800× overcorrection bug (applying the µmol → gC per-second factor to already-
integrated annual totals) was identified and fixed in commit 31e653b. All figure outputs
generated before that fix must be regenerated. Inline unit conversion at sub-daily/daily
resolution uses `fluxnet_convert_units()` as normal.

---

## Functionally active site definition — RESOLVED 2026-04-20

**Decision (2026-04-20):** A site is considered functionally active if it has ≥3 months
with valid `NEE_VUT_REF` in at least one year within the last 4 years (2022–2025). Active
status is derived from `data/snapshots/site_year_data_presence.csv`, which records
per-site per-year valid month counts computed from MM FLUXMET data. Implemented in
`R/utils.R` (`is_functionally_active()`).

Note: the MM dataset has a dual ERA5/FLUXMET row structure that requires care in
`compute_site_year_presence()` — see `known_issues.md` Section 6 for full details and
outstanding verification steps.

---

## NEE_CUT_REF fallback for 36 sites — RESOLVED 2026-04-20

**Decision (2026-04-20):** No fallback implemented. The 36 sites with valid `NEE_CUT_REF`
but no `NEE_VUT_REF` are documented in `outputs/sites_nee_cut_only.csv` and noted for the
methods section. VUT is the standard ONEFlux product for the Annual Paper; mixing
processing variants without co-author consensus is deferred. Revisit if co-authors request
CUT fallback before submission.

---

## Snapshot years for historical context figures — RESOLVED 2026-04-20

**Decision (2026-04-20):** Four reference years used for network growth and historical
context figures:

| Year | Event | Site count |
|------|-------|------------|
| 2000 | Marconi Conference | 35 sites |
| 2007 | La Thuile synthesis | 252 sites |
| 2015 | FLUXNET2015 release | 212 sites |
| 2025 | Current Shuttle snapshot | 672 sites |

Site lists stored in `data/snapshots/years_marconi.csv`, `years_la_thuile.csv`,
`years_fluxnet2015.csv`. Marconi site crosswalk from
`data/lists/Marconi_to_Modern_SiteIDs.xlsx`.

---

## Methods section — sites excluded due to data gaps

Dario Papale confirmed (2026-04-16) that sites with all-missing `NEE_VUT_REF` have gaps
exceeding 15 consecutive days throughout their record — ONEFlux correctly withholds annual
estimates in these cases. This needs a paragraph in the paper methods section explaining:

1. The 15-day gap threshold in ONEFlux and why annual estimates are not computed when this
   threshold is exceeded for all years in a site's record
2. The number of sites excluded on this basis (106)
3. The use of `NEE_CUT_REF` as fallback for 36 additional sites where VUT could not be
   calculated due to insufficient u* threshold statistics
4. The final site count used in analysis (530 VUT + up to 36 CUT fallback, pending
   co-author decision on NEE_CUT_REF fallback — see resolved entry above)

Contact Gilberto Pastorello (LBL) for ONEFlux processing documentation to support
methods writing (flagged by Dario Papale, 2026-04-16).

---

## DD data download — OPEN

Daily (DD) resolution data has not been downloaded via the Shuttle. Required for:
- Seasonal cycle figures
- Growing season length figures

**Action:** Add `"d"` to `FLUXNET_EXTRACT_RESOLUTIONS` and re-run `01_download.R` /
`02_extract.R`. Coordinate with co-authors on which seasonal/growing season figures are
in scope before downloading (significant additional storage).

---

## Environmental response figure (Fig 08) — OPEN

Fig 08 draft exists but axis labels, font sizes, and panel layout have not yet been
reviewed against the canonical Whittaker/Map/Dur style standards established during the
2026-04-20 session.

**Action:** Review against `MAP_STYLE`, `DUR_STYLE`, and `WHITTAKER_STYLE` constants in
`R/figures/`. Update to match before submission. Assign to the same style pass as Fig 07.

---

## Fig 07 latitudinal gradient — OPEN

Fig 07 latitudinal gradient figure is built but font size and style have not been
confirmed to match the canonical figure series (Whit01–09, Map01–09, Dur01–09).

**Action:** Apply same font size and style review as the Whittaker/Map/Dur series.
Regenerate once confirmed. See `review/figures/latitudinal/` for current output.

---

## Fig 01 network growth — OPEN

Fig 01 network growth figure includes a second Y axis showing the number of functionally
active sites. This line was computed using the old `last_year >= 2021` definition prior
to the `presence_df` refactor.

**Action:** Recompute the functionally active line using `is_functionally_active()` (≥3
months valid NEE in at least one year within last 4 years). Regenerate Fig 01 after
`site_year_data_presence.csv` is confirmed finalized — see outstanding verification
steps in `known_issues.md` Section 6 before treating that CSV as authoritative input.
