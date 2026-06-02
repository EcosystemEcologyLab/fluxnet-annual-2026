> Technical bugs and data quality issues with external dependencies (fluxnet R package,
> FLUXNET Shuttle) are tracked in [docs/known_issues.md](known_issues.md), not here.

## Authorship rubric — 5-year data-volume boundary — RESOLVED 2026-05-20

**Decision (2026-05-20):** Sites with exactly 5 years of data are placed in the **<5 row (lower
author count)** of the locked authorship rubric. This was the implementation at the 2026-05-07
team meeting and is formally confirmed as final policy. The rubric label is understood as "≤5
years". The alternative (moving 5-year sites to the "6–10" row, +50 authors) was considered and
rejected. No code or data change is required; `scripts/authorship_models.R` already implements
`years_data <= 5L ~ row 1` and `outputs/authorship/site_authors.csv` reflects this. Tracked as
FOLLOWUP-A in the authorship report; all authorship followup tasks are now resolved.

---

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

## flux_download() version-pinning — DEFERRED BY DESIGN

`flux_download()` bootstraps an ephemeral `uv` environment at call time rather than using the
pinned `fluxnet_annual_2026` venv. The actual shuttle commit used at download time is not
captured in run logs. This is a known reproducibility gap documented in `known_issues.md`.

**Decision:** Not pinning now — deliberately. Pinning will be applied at paper-lock time, when
the full download is frozen and the commit used needs to be traceable. Doing it earlier adds
maintenance overhead without benefit: the dataset is still being assembled and shuttle updates
between now and lock are expected.

**At paper-lock time, resolve the following:**
1. Confirm which shuttle commit `flux_download()` actually used (inspect the `uv` cache or
   add `--version` capture to the download log).
2. Decide the env-var design: whether `FLUXNET_SHUTTLE_VERSION` covers both the install
   reference (for the venv) and the version-check target, or whether a separate variable is
   needed for each. The two formats (`0.3.7` vs `0.3.7.post0+dirty`) are already divergent
   and the check in `check_pipeline_config()` needs to handle this cleanly.
3. Record the locked shuttle commit in the paper's Methods section alongside the snapshot PID.

Note: fluxnet-package#43 ("route flux_download() through the shuttle Python API") was resolved
by PR #60 (merged 2026-04-28). The download routing itself is no longer the issue; the
version-capture gap remains.

---

## Representativeness analysis climate axis — DEFERRED

The network representativeness analysis is not yet defined in scope (no assigned figure number
or methods specification as of 2026-05-26). When it is eventually defined, **use Köppen-Geiger**
for the climate-variability axis, not CRU. Köppen-Geiger classifications are already extracted
at all sites (`data/snapshots/` — see `long_record_site_candidates_gez_kg.csv`) and are on-disk
and reproducible via the existing pipeline. CRU adds a data dependency without a clear advantage
for this comparison axis.

**Decision:** Climate axis = Köppen-Geiger. Recording now so the decision is not lost when the
analysis is eventually scoped.

---

## DuckDB schema-translation patches in 07_figures.R — MAINTENANCE REFACTOR (deferred)

`scripts/07_figures.R` (adopted as canonical in commit below) contains two inline
schema-translation patches that paper over differences between the RDS and DuckDB
column schemas:

1. `YEAR = as.integer(TIMESTAMP)` — added post-collect on the annual frame because
   DuckDB's `annual_converted` uses `TIMESTAMP` (integer year) where the RDS pipeline's
   `flux_read()` used `YEAR`. Figure functions that expect a `YEAR` column work via
   this alias.

2. `DOY = lubridate::yday(TIMESTAMP)` — added post-collect on the daily frame because
   `daily_converted` stores `TIMESTAMP` as a DuckDB DATE (no pre-computed DOY column).
   `fig_seasonal_cycle()` and `fig_seasonal_weekly()` need a `DOY` column.

**Action (deferred):** Move these upstream — either as post-conversion column additions
in `05_units.R` (add `YEAR` to annual_converted and `DOY` to daily_converted as computed
columns during the compute() step), or as a helper function sourced by both 05 and 07.
Doing this in 05_units.R would make the schema consistent with what figure functions
expect, removing the need for translation patches in 07_figures.R.

This is a maintenance refactor, not a correctness issue — the current patches work.
Schedule for the next pipeline maintenance pass, not before final paper analysis.

---

## DD data OOM blocker — RESOLVED 2026-06-02

~~Daily (DD) resolution data OOM blocker.~~ The 16 GB vector memory ceiling on the local
Mac mini caused `03_read.R` to crash at site 150/759 during DD read. The DuckDB pipeline
(`03b_create_database.R`) resolves this structurally — DuckDB reads all 14M daily rows
directly from CSVs without R materialisation. DD figures (seasonal cycle, seasonal weekly,
growing-season NEE) are now in the canonical figure set and promoted to candidates.

`known_issues.md` Section 6 can be marked resolved.

---

## Next actions — paper-critical, ordered by priority (updated 2026-06-02)

### Priority 1 — Fig 01 is_functionally_active() recomputation (OPEN)

Fig 01 network growth figure includes a second Y axis showing functionally active sites.
This line was computed using the old `last_year >= 2021` definition prior to the
`presence_df` refactor. `is_functionally_active()` and its docstring were corrected today
(commits ee84552, R/utils.R); the figure itself was not regenerated.

**Action:** Recompute the functionally active line using `is_functionally_active()` (≥3
months valid flux data in at least one year within last 4 years, from `site_year_data_presence.csv`).
Regenerate Fig 01. The presence CSV was produced by `03_read.R` on the 759-site dataset;
treat it as authoritative — the any-flux definition was confirmed correct in known_issues.md §6.

### Priority 2 — Fig 07 / Fig 08 style harmonisation (OPEN)

Fig 07 (latitudinal gradient) font size and style have not been confirmed to match the
canonical figure series (Whit01–09, Map01–09, Dur01–09). Fig 08 (environmental response)
axis labels, font sizes, and panel layout have not been reviewed against `MAP_STYLE`,
`DUR_STYLE`, and `WHITTAKER_STYLE` constants in `R/figures/`.

**Action:** Apply the style review pass to Fig 07 and Fig 08 together. Regenerate once
confirmed. Current outputs in `review/figures/latitudinal/` and
`review/figures/envresponse/`.

### Priority 3 — Rebuild site_candidates_full.csv at 759 sites (OPEN)

`data/snapshots/site_candidates_full.csv` has 569 rows (from the 716-site April lineage).
`long_record_site_candidates_gez_kg.csv` is also stale. Any figure that joins on candidate
status (`currently_selected`, long-record site selection, anomaly stratification) will use
the wrong site set until this is rebuilt.

**Action:** After `03_read.R` has produced updated NEE presence data for 759 sites, rebuild
`long_record_site_candidates_gez_kg.csv` and re-run `step2_extract_aridity.R` to regenerate
`site_candidates_full.csv`. Tracked in known_issues.md §7. This is a prerequisite for
anomaly figures and long-record time series.

### Priority 4 — Manuscript drafting

Figures are stable and candidates are promoted. The 759-site, DuckDB-canonical, TERN-inclusive
pipeline has run end-to-end. Figures can now be referenced with confidence in the manuscript.
CUT QC fallback and VUT/CUT coalesce in map and Whittaker figures are implemented and validated.

---

## Deferred items — do not action without revisiting

### Schema-translation refactor (low priority, next maintenance pass)
Move `YEAR = as.integer(TIMESTAMP)` (annual) and `DOY = lubridate::yday(TIMESTAMP)` (daily)
upstream into `05_units.R` as post-compute column additions, or into a shared helper.
Currently working as inline patches in `07_figures.R`; no correctness issue.
See: DuckDB schema-translation patches entry above.

### flux_download() version pinning (deferred-by-design, paper-lock prerequisite)
Not pinning now; will be applied deliberately at paper-lock time. The env-var format conflict
(`0.3.7` install tag vs `0.3.7.post0+dirty` self-report) is part of the same decision.
See: decisions_pending.md flux_download() entry; docs/known_issues.md §5.

### Representativeness analysis climate axis (scope undefined)
When eventually scoped, use Köppen-Geiger (already extracted at all sites). CRU is not needed.

### fluxnet package DuckDB rewrite (out of our control)
If/when the package rewrites flux_read/flux_qc/flux_units with DuckDB internals, much of
today's plumbing (03b_create_database.R, DuckDB 04 and 05) will be superseded by package
function calls. The methodological decisions (QC rule, CUT fallback, figure logic) are
package-rewrite-proof and will remain valid.

---

## Environmental response figure (Fig 08) — OPEN

Fig 08 draft exists but axis labels, font sizes, and panel layout have not yet been
reviewed against the canonical Whittaker/Map/Dur style standards established during the
2026-04-20 session.

**Action:** Review against `MAP_STYLE`, `DUR_STYLE`, and `WHITTAKER_STYLE` constants in
`R/figures/`. Update to match before submission. Assign to the same style pass as Fig 07.
See Priority 2 above.

---

## Fig 07 latitudinal gradient — OPEN

Fig 07 latitudinal gradient figure is built but font size and style have not been
confirmed to match the canonical figure series (Whit01–09, Map01–09, Dur01–09).

**Action:** Apply same font size and style review as the Whittaker/Map/Dur series.
Regenerate once confirmed. See `review/figures/latitudinal/` for current output.
See Priority 2 above.

---

## Fig 01 network growth — OPEN

Fig 01 network growth figure includes a second Y axis showing the number of functionally
active sites. This line was computed using the old `last_year >= 2021` definition prior
to the `presence_df` refactor.

**Action:** See Priority 1 above.
