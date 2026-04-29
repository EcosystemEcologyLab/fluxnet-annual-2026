# Architecture Review — fluxnet-annual-2026
**Date:** 2026-04-28  
**Reviewer:** Claude (claude-sonnet-4-6)  
**Scope:** Full repo read of CLAUDE.md, SCIENCE_PRINCIPLES.md, SCIENCE_PRINCIPLES_PIPELINES.md, docs/methods_requirements.md, docs/session_summary_20260420.md, docs/decisions_pending.md, all scripts/ and R/ files.  
**Status:** Read-only review. No code modified.

---

## 1. Pipeline Coherence

### The numbered pipeline (01–07)

Scripts 01–05 are coherent and do what CLAUDE.md describes:

| Script | Status | Notes |
|--------|--------|-------|
| `01_download.R` | ✓ Coherent | Calls `flux_listall()`, `flux_download()`, snapshot diffing |
| `02_extract.R` | ✓ Coherent | `flux_extract()`, ZIP cleanup, `file_inventory.rds` output |
| `03_read.R` | ✓ Coherent | Custom per-site reader (bypasses `flux_read()` for memory reasons); BADM from BIF CSVs |
| `04_qc.R` | ✓ Coherent | Two-stage QC per resolution; per-site chunked to avoid OOM |
| `05_units.R` | ✓ Coherent | `fluxnet_convert_units()` called; meta.json written |
| `06_analysis.R` | **✗ Broken** | Reads `flux_data_converted.rds` (line 19) — this file does not exist. `05_units.R` writes `flux_data_converted_yy.rds`, `flux_data_converted_mm.rds`, `flux_data_converted_dd.rds`. The script will crash on the first `readRDS()` call. Beyond the broken path, this is an empty placeholder: the actual analysis is `# TODO`. |
| `07_figures.R` | ⚠ Thin | Correctly reads `flux_data_converted_yy.rds` / `_mm` / `_dd`. Sources six figure modules and produces ~15 PNGs. Does not write any `.meta.json`. No `bg = "white"` on `ggsave()` calls (see §2). |

### How generate_whittaker.R, generate_maps.R, generate_duration_histograms.R fit

These three scripts are listed by name in `docs/methods_requirements.md` §5.7 as part of the reproducibility workflow, and they are the canonical source of the paper's network-characterisation figures (Whit01–09, Map01–09, Dur01–11). They are **not called from 07_figures.R** — a researcher following "run scripts 01–07 in order" would never execute them. They are standalone scripts that must be run separately.

There is also a growing cluster of unnumbered batch-figure scripts with no documented position in the pipeline:
- `scripts/generate_gez_anomaly_figures.R`
- `scripts/generate_kg_anomaly_figures.R`
- `scripts/generate_availability_heatmap.R`
- `scripts/generate_kg_availability_heatmaps.R`
- `scripts/generate_historical_comparison_figures.R`
- `scripts/generate_env_response_era5.R`
- `scripts/analysis_long_record_candidates.R`
- `scripts/step1_extract_worldclim.R`
- `scripts/step2_extract_aridity.R`

None of these appear in CLAUDE.md's pipeline execution order table. Some produce outputs that downstream scripts depend on (e.g., `site_candidates_full.csv` → anomaly figures), but the dependency chain is implicit and not documented.

### Is 00_candidate_figures.R deprecated or active?

It is **active and not deprecated**. It was committed to in April 2026 and generates the primary review report (`review/candidate_figures.html`). However:

- It carries **a runtime crash bug** (see §2 below).
- Three of its `build_*` functions (`build_whittaker_snapshots`, `build_country_map`, `build_duration_profile`) are defined in full but their calls are commented out as "DEPRECATED". These function bodies are dead code.
- The "00" prefix is misleading: the script requires processed data from steps 03–05.

---

## 2. Code–Documentation Drift

### Hard Rule #1 — Data source (Shuttle only)

- All numbered pipeline scripts (01–07) use Shuttle-derived data exclusively. ✓
- `generate_maps.R`, `generate_duration_histograms.R`, `generate_historical_comparison_figures.R` use historical site lists (Marconi/La Thuile/FLUXNET2015) for comparison panels. Each carries a comment citing CLAUDE.md §1 and labelling these as "development/comparison purposes only". ✓ Compliant in intent; must ensure outputs are labelled in the final paper.

### Hard Rule #2 — Never infer hub/region from site ID prefix

- The snapshot CSV uses the column `data_hub`. Live figure code (`R/figures/fig_network_growth.R`, `07_figures.R`, `00_candidate_figures.R`) colour-codes by `data_hub`. CLAUDE.md §2 says to use the `network` field from the manifest or snapshot. Whether `data_hub` and `network` are the same field under different names requires checking the snapshot schema — if they differ this is a Hard Rule violation.
- No code found that maps site ID prefixes (`US-`, `DE-`, etc.) to geography in the live pipeline scripts. ✓
- The top-level `legacy/` directory contains `MappingFluxnet.R` which does geographic inference from prefixes, but this file is not sourced from any live script. ✓

### Hard Rule #3 — No hardcoded credentials

No credentials in any committed file. ✓

### Hard Rule #4 — Never commit data files

Only `data/snapshots/` is committed. ✓ However, see §4 for snapshot CSV clutter.

### Hard Rule #5 — check_pipeline_config() at script top

**All seven numbered pipeline scripts call it.** The three canonical figure-generation scripts (generate_whittaker.R, generate_maps.R, generate_duration_histograms.R) and all five batch anomaly/availability scripts call it. Missing from:
- `scripts/analysis_long_record_candidates.R` — no call, only sources `pipeline_config.R`
- `scripts/step1_extract_worldclim.R` — sources `pipeline_config.R`, no call
- `scripts/step2_extract_aridity.R` — not verified but same pattern
- `scripts/generate_env_response_era5.R` — not in grep results
- `scripts/generate_historical_comparison_figures.R` — not in grep results

These are auxiliary scripts, but some produce committed snapshot outputs.

### QC threshold — active mismatch between CLAUDE.md and code

**CLAUDE.md** (`QC Flag Reference`) declares:
```r
QC_THRESHOLD_DD <- 0.75
QC_THRESHOLD_WW <- 0.75
QC_THRESHOLD_MM <- 0.75
QC_THRESHOLD_YY <- 0.75
```
with the note "The stricter default of > 0.75 is intentional for this synthesis paper."

**`R/pipeline_config.R`** (lines 32–45) sets all thresholds to `0.50` with the inline comment:
> "Lowered from 0.75 to 0.50 to match FLUXNET published convention — to be revisited with co-authors"

**`docs/decisions_pending.md`** marks this as RESOLVED on 2026-04-20, and `docs/methods_requirements.md` §5.3 confirms "QC threshold: NEE_VUT_REF_QC ≥ 0.50 (annual and monthly)".

**The decision is correct and documented in decisions_pending and methods_requirements.** However, CLAUDE.md still shows 0.75 everywhere and describes 0.75 as "intentional". Any contributor reading CLAUDE.md will think the code is wrong. CLAUDE.md needs updating.

### Unit conversion — CLAUDE.md table vs actual practice

CLAUDE.md unit conversion table lists `NEE, GPP, RECO | µmol CO₂ m⁻² s⁻¹ → gC m⁻² per period`. This is correct for HH/HR resolution. At coarse resolutions (YY/MM), the data is already pre-integrated as gC m⁻² per period by ONEFlux and is passed through unchanged. This was the critical bug fix in commit `31e653b`. CLAUDE.md does not note the coarse-resolution pass-through exception; `05_units.R` and `R/units.R` handle it correctly, but the table in CLAUDE.md implies conversion is always applied.

### output metadata — write_output_metadata() compliance

CLAUDE.md and SCIENCE_PRINCIPLES_PIPELINES.md both require a `.meta.json` companion for every output file. `write_output_metadata()` is called in:
- ✓ `scripts/05_units.R` — for each `flux_data_converted_*.rds`
- ✓ `scripts/00_candidate_figures.R` — for `review/candidate_figures.html`
- ✓ `scripts/00_diagnostics.R` — for `docs/diagnostics.html`

Not called in:
- `scripts/02_extract.R` — `data/processed/file_inventory.rds` has no companion
- `scripts/03_read.R` — `flux_data_raw_*.rds` have no companion
- `scripts/04_qc.R` — `flux_data_qc_*.rds` have no companion
- `scripts/06_analysis.R` — will produce analysis outputs with no companion (when populated)
- `scripts/07_figures.R` — each PNG in `figures/` has no companion
- All `generate_*.R` scripts — no companion files for any figure outputs

### Exclusion logging — log_exclusion() / log_unknown() coverage

`04_qc.R` properly initialises both logs (empty CSV with headers if absent) and calls `log_exclusion()` for every flagged row. ✓

Gaps:
- `03_read.R` drops BIF files whose required columns are missing (lines 44–61). It issues a `warning()` but does not call `log_unknown()`. These sites lose their BADM metadata silently from the log's perspective.
- The 106 sites excluded for all-missing NEE (ONEFlux 15-day gap rule) are identified implicitly in figure code but never logged via `log_exclusion()` — they simply have no rows in the QC'd data.
- The 36 NEE_CUT-only sites are documented in `docs/decisions_pending.md` and referenced in `outputs/sites_nee_cut_only.csv`, but this CSV is not produced by any call to `log_unknown()` — it is an ad-hoc output from an undocumented step.

### bg = "white" — ggsave compliance

CLAUDE.md: "All review figures saved to `review/figures/` must use a **white (opaque) background** — pass `bg = "white"` to every `ggsave()` call."

- `00_candidate_figures.R`: Sections 1 (YY time series), 2 (MM climatology), 3 (DD climatology), 6 (environmental response), 13b, 13c pass `bg = "white"`. ✓
- Sections 7 (long-record timeseries), 8 (network growth), 9 (network growth annual), 12 (active proportion), 13 (subregion overview) do **not** pass `bg = "white"`. ✗
- `07_figures.R`: `save_fig()` helper (line 34) calls `ggsave()` without `bg = "white"`. ✗ (outputs go to `figures/`, which the convention as written applies to, even though it says `review/figures/`).
- `generate_*.R` canonical figure scripts: `generate_whittaker.R` passes `bg = "white"` ✓; need to verify `generate_maps.R` and `generate_duration_histograms.R` individually.

---

## 3. Reproducibility Surface

### .meta.json companion file inventory

| Output | Script | .meta.json? |
|--------|--------|-------------|
| `data/processed/file_inventory.rds` | `02_extract.R` | ✗ |
| `data/processed/flux_data_raw_*.rds` | `03_read.R` | ✗ |
| `data/processed/flux_data_qc_*.rds` | `04_qc.R` | ✗ |
| `data/processed/flux_data_converted_*.rds` | `05_units.R` | ✓ |
| `review/candidate_figures.html` | `00_candidate_figures.R` | ✓ |
| `docs/diagnostics.html` | `00_diagnostics.R` | ✓ |
| `figures/*.png` (all) | `07_figures.R` | ✗ |
| `review/figures/**/*.png` (all) | `generate_*.R`, `00_candidate_figures.R` | ✗ |
| `data/snapshots/site_candidates_full.csv` | (unknown script) | ✗ |
| `data/snapshots/long_record_sites.csv` | `analysis_long_record_candidates.R` | ✗ |

The most critical gap is that the intermediate data files (`flux_data_raw_*.rds`, `flux_data_qc_*.rds`) carry no provenance. If a reviewer asks "what version of code produced your QC'd data?", the answer is not traceable without manual git archaeology.

### exclusion_log.csv and unknown_log.csv

Only `04_qc.R` writes to these logs. Scripts that perform implicit exclusions — `03_read.R` (BIF drops), figure code that filters to `dataset == "FLUXMET"` — do not touch the logs.

A zero-row `unknown_log.csv` from a run of the full pipeline is misleading: 106 sites with all-missing NEE are simply absent from the QC output without a log entry explaining why.

---

## 4. Dead Code and Clutter

### legacy/ (top-level)

Contains seven R scripts and a README: `AMFOct25_poster.R`, `AGUSlop.R`, `fcn_plot_FLUXNET.R`, `demo_fluxnet_plots.R`, `fcn_utility_FLUXNET.R`, `poster_constants.R`, `MappingFluxnet.R`. None are `source()`-d from any live script (confirmed by grep). These predate the pipeline and contain site-ID-prefix-based geographic inference (`MappingFluxnet.R`) that violates Hard Rule #2, but only in dead code. Not a risk unless accidentally re-activated.

### scripts/legacy/

Contains `generate_whittaker_figures.R` and `generate_worldclim_figures.R`. Referenced only in a comment in `generate_whittaker.R` ("those scripts moved to scripts/legacy/"). Not sourced by live code. ✓

### Snapshot CSV clutter (data/snapshots/)

21 snapshot CSVs are committed, 18 of them from within a 2-hour window on 2026-04-14. These are artifacts of the batch-download iteration that day (each failed batch triggered a new snapshot). The authoritative full-network snapshot is the latest one (`fluxnet_shuttle_snapshot_20260414T154430.csv`). Earlier ones are not stale in the sense that they accurately recorded the state at that moment, but they add noise and the `find_previous_snapshot()` comparison logic in `01_download.R` will pick the most recent one automatically. The earlier ones are not referenced by any script. Three pre-April-14 snapshots (from 2026-03-28 and 2026-04-02 and 2026-04-12) are superseded development snapshots.

### R/ files sourced from nowhere

- `R/figures/fig_climate_legacy.R` — based on the session summary (2026-04-20), this was created for "shared legacy climate-space figure functions extracted for reuse". Not confirmed whether it is currently sourced by any live script.
- `R/sync.R` — sourced from `01_download.R` ✓
- All other `R/` files confirmed sourced by at least one live script.

### 00_candidate_figures.R dead function bodies

Three function definitions inside `00_candidate_figures.R` — `build_whittaker_snapshots()` (lines 590–652), `build_country_map()` (lines 980–1021), `build_duration_profile()` (lines 877–902) — are explicitly commented out at the assembly point (lines 1038–1076) with "DEPRECATED" notices. The function bodies remain but are never called. This is dead code inside an active file.

### testthat/ directory absent

CLAUDE.md: "Every function in `R/` must have at least one test in `tests/testthat/`". The `tests/testthat/` directory does not exist. The only file in `tests/` is `tests/fixtures/make_test_data.R`, a fixture generator, not a test suite. None of the 21 R/ files have corresponding tests.

---

## 5. Risks and Gaps for the Paper

### A. Core analysis missing (scripts/06_analysis.R)

`06_analysis.R` reads a file that doesn't exist (`flux_data_converted.rds`; see §1) and otherwise contains only a `# TODO` comment. The paper's quantitative analyses — trend estimation, anomaly context, inter-variable comparisons — have not been implemented. This is the largest gap between current repo state and a submittable paper. The methods requirements document (§5.4, §5.6) has placeholders for these analyses.

### B. Review report crash (00_candidate_figures.R line 1107)

The final assembly line:
```r
out_html <- paste0(html_head, s1, s2, s3, s8, s9, s11, s12, s13, s13b, s13c, s10, s4, s5, s6, s7, s14, html_footer)
```
references `s11`, `s10`, and `s4` which are commented out and never assigned. Running this script will fail with an "object 's11' not found" error before writing the HTML. The review report is currently unrunnable.

### C. QC threshold inconsistency — CLAUDE.md vs code vs methods doc

A co-author who reads CLAUDE.md will understand the threshold to be > 0.75. The actual code uses ≥ 0.50. The methods section (§5.3) records ≥ 0.50 as the intended value. This three-way inconsistency (CLAUDE.md = 0.75, code = 0.50, methods doc = 0.50) means that without reading `docs/decisions_pending.md`, the discrepancy looks like a bug rather than a resolved decision. A reviewer or co-author auditing the code would flag this immediately.

### D. No test suite

No `tests/testthat/` directory exists. Given the complexity of the QC logic, unit conversion pass-through, temporal resolution handling, and the history of a critical 800× overcorrection bug, the absence of automated tests is a significant fragility risk. Any future refactor of `R/qc.R`, `R/units.R`, or `R/utils.R` has no safety net. SCIENCE_PRINCIPLES.md §4 ("Reproducibility as a first-class output") and CLAUDE.md both require tests.

### E. External data pipeline not integrated

`step1_extract_worldclim.R` and `step2_extract_aridity.R` produce `site_worldclim.csv` and `site_aridity.csv` that are committed to `data/snapshots/`. These files are inputs to the Whittaker and environmental response figures. However:
- These scripts are not in the numbered pipeline.
- They require large external files (WorldClim rasters, ~666 MB; CGIAR AI, ~1.3 GB) that are gitignored and have no automated download step.
- `session_summary_20260420.md` says "Re-download instructions are in `known_issues.md`" — a reviewer reproducing the pipeline would need to find these instructions manually.
- Neither script calls `check_pipeline_config()` or writes a `.meta.json` for its output.

### F. DD data not yet downloaded

`docs/decisions_pending.md` flags daily (DD) data download as OPEN. `07_figures.R` loads `flux_data_converted_dd.rds` (line 54) and `00_candidate_figures.R` has a lazy-load DD section — both will degrade silently if DD data is absent. Seasonal cycle figures (Section 3) and growing season figures (Section 6 of 07_figures.R) require DD data. These are likely paper figures.

### G. Fig 01 (network growth) uses stale functionally-active computation

`docs/decisions_pending.md` flags this explicitly: the functionally active line in Fig 01 was computed using the old `last_year >= 2021` definition before the `presence_df` refactor. It needs regeneration against `site_year_data_presence.csv` via `is_functionally_active()`. `site_year_data_presence.csv` itself has outstanding verification steps (see `known_issues.md` §6 — MM dual-ERA5/FLUXMET row structure).

### H. README.md absent

`docs/methods_requirements.md` §5.7 explicitly lists `README.md (to be created)` as a primary code file for the reproducibility methods section. No README.md exists. A co-author or new contributor has no entry point to the repo.

### I. generate_* scripts not part of the pipeline

The paper will cite the repository as containing all code needed to reproduce figures. A reader who runs scripts 01–07 will not have the network-characterisation figures (the canonical Whit/Map/Dur series) because those are only produced by separate `generate_*.R` scripts that are not invoked by the numbered pipeline. If these are paper figures, the pipeline is incomplete as documented.

---

## Top 5 Issues — Prioritised

| Rank | Issue | File | Justification |
|------|-------|------|--------------|
| 1 | **`06_analysis.R` reads non-existent file and is an empty placeholder** | `scripts/06_analysis.R:19` | The entire paper-specific quantitative analysis step (§5.4, §5.6 of methods) is absent. This is the critical path blocker for submission. |
| 2 | **`00_candidate_figures.R` crashes on line 1107** (`s4`, `s10`, `s11` undefined) | `scripts/00_candidate_figures.R:1107` | The primary review report is unrunnable. The bug is trivial to introduce confusion and not trivial to notice: the variables are defined-but-commented elsewhere in a 1130-line file. |
| 3 | **CLAUDE.md QC threshold (0.75) contradicts active code (0.50) and methods doc** | `CLAUDE.md` QC section; `R/pipeline_config.R:36–45` | The decision is correct and documented in `decisions_pending.md`, but CLAUDE.md is the most-read file in the repo. A co-author, reviewer, or new contributor will see an apparent 50% discrepancy in the filter stringency between the documented and actual pipeline. |
| 4 | **No test suite despite policy and known bug history** | `tests/` directory | The 800× unit conversion overcorrection was caught by inspection. The QC gating bug (52% site loss) was caught by output inspection. A test suite would have caught both at the unit level. Without tests, future changes to `R/qc.R`, `R/units.R`, or `R/utils.R` carry no safety net for a paper that will be cited. |
| 5 | **Output metadata (.meta.json) missing from 04 intermediate outputs** | `scripts/03_read.R`, `04_qc.R`, `07_figures.R` | `flux_data_raw_*.rds` and `flux_data_qc_*.rds` carry no provenance. If an intermediate file is corrupted or accidentally overwritten, there is no record of the code version or run date that produced it. SCIENCE_PRINCIPLES_PIPELINES.md makes this a hard requirement for every output file. |
