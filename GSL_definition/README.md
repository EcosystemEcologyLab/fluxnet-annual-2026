# GSL figure rebuild — side analysis

**This is not part of the FLUXNET Annual Paper 2026.** It is a standalone rebuild of the growing
season length (GSL) figure for the MacBean and Moore manuscript, requested via David Moore,
2026-10-29.

Everything for this analysis — code, data, figures, tables, and docs — lives under this
directory (`GSL_definition/`). Nothing here reads from or writes to the repo-root `R/`,
`scripts/`, `outputs/`, `review/`, or `docs/` directories used by the Annual Paper, with one
deliberate exception: `R/utils.R::log_exclusion()`/`log_unknown()` gained an optional `log_path`
argument (defaulting to the existing repo-root `outputs/` path, so every existing caller is
unaffected) so this side project can pass its own `GSL_definition/logs/` paths instead — see
`logs/prompts/2026-10-29_gsl_stage1.md` and `SESSION_LOG.md`. `.meta.json` companions use a
local `write_meta()` (`code/00_config.R`), not `R/utils.R::write_output_metadata()`, for the same
reason — that function always appends to the repo-root `outputs/session_info.txt`.

`code/` reads (never writes) the repo-root DuckDB store (`data/duckdb/fluxnet.duckdb`),
read-only, filtered to `dataset = 'FLUXMET'` (the pre-QC daily table also carries `dataset =
'ERA5'` rows, meteorology only — every query here must filter to FLUXMET; see
`docs/methods_memo.md`).

**On 5 November 2026, Dave and Tash agreed to write two papers from this work** — a short one on
GSL methods that are sensitive to skewness, and a longer one on the wider methods work — but
which figures and tables go in which paper had not been decided as of that date. This README,
and the directory layout below, is **not** organized by that eventual paper split.

## The two trees, in plain English

There are two independent, self-contained analyses living side by side in this directory. The
expanded tree does not replace the first — both are kept, and the 62-site tree's own figures are
never touched when the expanded tree is rebuilt.

### `GSL_definition/` (the 62-site tree, Tash's original site list)

- **Sites:** 62 sites, Tash's original selection — the manuscript's own site list (Table S1:
  grassland 17, evergreen forest 20, deciduous forest 12, woody-grass mosaic 12, mixed forest 1).
- **Selection rule:** reproduces the manuscript's rule as published — no relaxation.
- **GPP series:** main run uses `GPP_NT_VUT_REF` with negative GPP zero-floored; the 4
  Supplemental Figures (S1–S4) swap in `GPP_DT_VUT_REF` and/or as-delivered (non-zero-floored)
  negative GPP as sensitivity variants.
- **Year convention:** calendar year throughout (no Southern-Hemisphere season-year join — that
  complication is specific to the expanded tree's extra sites).
- **What it's for:** reproducing the manuscript's own Figures 1–4 methodology end to end, plus two
  independent, read-only side questions (a site-selection audit and a selection-rule-options
  recommendation) that do not change the 62-site figures themselves.

### `GSL_definition/expanded/` (the 153-site tree)

- **Sites:** 153 sites — the 62-site list's rule relaxed (a per-site VUT/CUT QC fallback crossed
  with a GPP-availability-aware CUT fallback) to recover long-running network sites the original
  rule excluded, while still containing every one of the original 505 GSL-62 site-years. See
  `docs/selection_rule_options_2026-10-30.md` (in the 62-site tree's own `docs/`) for how that
  rule was chosen.
- **Selection rule:** the relaxed rule above, applied once; not revisited since 2026-10-30 session 2a's
  CUT-fallback refinement (151 → 153 sites).
- **GPP series:** same convention as the 62-site main run (`GPP_NT_VUT_REF`, negative GPP
  zero-floored), with every derived quantity (P99, skewness with expanded-set terciles, Z16/T95,
  season start/end/gap days, carbon-gain and temperature-sensitivity slopes, trend detection)
  recomputed on the expanded population rather than copied from the 62-site run.
- **Year convention:** calendar year, **except** Southern-Hemisphere carbon-gain/temperature
  models and wet-season figures, which use a Jul–Jun season-year join (session 2a onward) — mixing
  the two without the explicit join is a documented source of error; see `docs/methods_memo.md`.
- **What it's for:** the main expanded-population analysis (6 main figures + 8 supplementary,
  currently mid-rerun — see below) plus a separate, later methods-comparison/methods-review track
  (8 GSL-detection methods compared against each other, `11_gsl_methods_compare.R` /
  `12_methods_review_figures.R`) that is its own line of work, not a rebuild of the 6 main figures.

**Where things stand right now:** the expanded tree's 6 main figures and 8 supplementary figures
were built with a winter-screening method that has since been dropped; those old outputs are
retired (see `archive/README.md`) and `expanded/figures/main/` is **empty**, waiting on the
planned rerun with the new method. Nothing below lists a current Figure 1–6 or S1–S8 for the
expanded tree for that reason.

## Current figures

One PNG + PDF per figure (PNG only for drafts that were never promoted), each with a companion
`.legend.txt` and `.meta.json`. "Status" follows the categories used throughout this project:
**reproduction check** (reproduces the manuscript's own analysis), **draft for discussion** (not
a numbered manuscript figure, shown to Dave/Tash for feedback), or **internal** (a QA/audit
figure, not intended for the manuscript at all).

### 62-site tree (`figures/`)

| Path | Description | Script | Status |
|---|---|---|---|
| `figures/reproduction/Figure 1.*` | Mechanism at two representative sites (JP-Fjy, US-Ton) | `code/05_fig1.R` | Reproduction check |
| `figures/reproduction/Figure 2.*` | Threshold sensitivity across all 62 sites (main run) | `code/06_fig2.R` | Reproduction check |
| `figures/reproduction/Figure 3.*` | GSL trends vs. sampling uncertainty (21 trend sites) | `code/07_fig3.R` | Reproduction check |
| `figures/reproduction/Figure 4.*` | The fraction-of-annual-GPP method and its own sensitivity | `code/08_fig4.R` | Reproduction check |
| `figures/reproduction/Supplemental Figure S1.*` | Figure 2, `GPP_DT_VUT_REF` variant | `code/10_supplementary.R` | Reproduction check |
| `figures/reproduction/Supplemental Figure S2.*` | Figure 2, negative-GPP-as-delivered variant | `code/10_supplementary.R` | Reproduction check |
| `figures/reproduction/Supplemental Figure S3.*` | Figure 3, `GPP_DT_VUT_REF` variant | `code/10_supplementary.R` | Reproduction check |
| `figures/reproduction/Supplemental Figure S4.*` | Figure 3, negative-GPP-as-delivered variant | `code/10_supplementary.R` | Reproduction check |
| `figures/selection_audit/selection_audit_long_record_variants.*` | Per-site screen results for the 37 long-record sites not already in the GSL-62, across rule variants | `code/11_selection_audit.R` | Internal (selection audit) |
| `figures/proposal/Figure P1a (duration curve examples).*` | GPP-duration curve mechanism at two sites | `code/12_duration_curve_and_target_sweep.R` | Draft for discussion |
| `figures/proposal/Figure P1c (duration curve exemplars).*` | Five named exemplar duration curves (replaces the retired Figure P1b) | `code/12_duration_curve_and_target_sweep.R` | Draft for discussion |
| `figures/proposal/Figure P1d (sensitivity and Z16 capture vs skewness).*` | Local sensitivity and Z16 capture fraction vs. skewness, all 62 sites | `code/12_duration_curve_and_target_sweep.R` | Draft for discussion |
| `figures/proposal/Figure P2 (target share sweep).*` | GSL threshold choice as a function of target included-GPP share | `code/12_duration_curve_and_target_sweep.R` | Draft for discussion |

### Expanded tree (`expanded/figures/`)

| Path | Description | Script | Status |
|---|---|---|---|
| `expanded/figures/main/` | **Empty.** Will hold the rebuilt Figures 1–6 and Supplemental S1–S8 once the planned rerun (replacing the dropped winter-screen method) is done. | `expanded/code/04_figures_main.R`, `expanded/code/05_figures_supp.R` (both already point here) | Pending rerun |
| `expanded/figures/methods_review/Figure A.*` | Range of all 8 GSL-detection methods' mean GSL, 15 sites sorted by skewness, calendar year rule | `expanded/code/12_methods_review_figures.R` | Draft for discussion |
| `expanded/figures/methods_review/Figure A (min_gpp_month companion).*` | Same as Figure A, `min_gpp_month` year rule | `expanded/code/12_methods_review_figures.R` | Draft for discussion |
| `expanded/figures/methods_review/Figure B (AU-ASM vs AU-How).*` | Threshold-dependence raster + histogram, AU-ASM vs. AU-How | `expanded/code/12_methods_review_figures.R` | Draft for discussion |
| `expanded/figures/methods_review/Figure B (US-SRM vs AU-How).*` | Threshold-dependence raster + histogram, US-SRM vs. AU-How | `expanded/code/12_methods_review_figures.R` | Draft for discussion |
| `expanded/figures/methods_review/Figure C.*` | Per-method season placement at US-Ha1/US-Wkg/US-Var/AU-How | `expanded/code/12_methods_review_figures.R` | Draft for discussion |
| `expanded/figures/methods_review/Figure D.*` | Skewness vs. cross-method GSL range, both year rules | `expanded/code/12_methods_review_figures.R` | Draft for discussion |

Retired figures (old Figure P1b; the pre-rerun expanded Figures 1–6/S1–S8/1_alt1; four early
methods-comparison drafts) are listed with reasons in `archive/README.md`, not here.

## Current reports (`docs/`)

| Path | Description | Status |
|---|---|---|
| `docs/framework.md` | Tash's original GSL analysis framework document (verbatim) | Internal (reference) |
| `docs/corrections_20261029.md` | Corrections to the framework checked against the repo before work started | Internal |
| `docs/methods_memo.md` | Full methods record across every stage and session, both trees | Internal (reference) |
| `docs/reproduction_report.md` | Stage 2 match checks against the manuscript's published values | Reproduction check |
| `docs/report_back_2026-10-29.md` | Stage 1 results: scaffold, site extraction, both screenings, Tash-rule reproduction | Reproduction check |
| `docs/report_back_2026-10-29_stage3_revisions.md` | Stage 3 + revisions: Figures 1–4, benchmark table, 4 supplementary figures | Reproduction check |
| `docs/report_back_2026-10-30_peerj_restyle.md` | PeerJ restyle pass: sizing/style, discrete skewness-tercile palette, Figure 2 fixes | Reproduction check |
| `docs/data_and_code_availability.md` | Supplemental Data groupings, sizes, and DOI/citation notes for submission | Internal |
| `docs/selection_audit_2026-10-30.md` | Read-only audit: can excluded long-running sites be recovered by a less strict rule? | Internal (selection audit) |
| `docs/selection_rule_options_2026-10-30.md` | Follow-on: recommends the specific recoverable-site selection rule applied to build the expanded tree | Internal |
| `docs/duration_curve_and_target_sweep_2026-11-03.md` | Build/redraw record for Figures P1a–P2 and the target-share sweep | Draft for discussion |
| `docs/figure_gsl_iav_2026-10-31.md` | Design note for the expanded tree's "Figure GSL-IAV" draft (figure itself now retired — see `archive/README.md`) | Internal (figure retired) |

| Path (expanded tree) | Description | Status |
|---|---|---|
| `expanded/docs/report_back_2026-10-30.md` | Expanded-site analysis build results: 151 sites, 6 main + 7 supplementary figures | Reproduction check |
| `expanded/docs/checks_report_2026-10-30.md` | Session-1 QA checks on that build, leading to the session 2a revision pass | Internal |
| `expanded/docs/report_back_2026-10-30-session2a.md` | Session 2a methods/figure revision pass (153 sites, winter screen, season-year joins, trend-detectability sweep) | Reproduction check |
| `expanded/docs/gsl_methods_compare_2026-11-02_v4.md` | Current methods-comparison write-up (8 GSL-detection methods, version 4) | Internal |
| `expanded/docs/methods_review_figures_2026-11-03.md` | Build record for the methods-review Figures A–D | Draft for discussion |

Earlier drafts of the methods-compare write-up (v1–v3) are retired — see `archive/README.md`.

## Directory layout

- `code/` — numbered scripts, each sourcing `code/00_config.R`; `fig_helpers.R` holds the shared
  PeerJ figure style (text sizes, line weights, colour scales) and `save_gsl_figure()`
- `data/` — site extracts and large derived tables (fine grid, fraction sweep) pulled from the
  repo-root DuckDB store (gitignored)
- `tables/` — analysis output tables (git-tracked); `tables/selection_audit/` holds the
  site-selection-audit tables; candidate Supplemental Data groupings and sizes are in
  `docs/data_and_code_availability.md`
- `figures/` — `reproduction/` (Figures 1–4, S1–S4), `proposal/` (Figures P1a/P1c/P1d/P2),
  `selection_audit/` (the long-record-variants audit figure) — see the table above
- `docs/` — see the table above
- `logs/` — `exclusion_log.csv`/`unknown_log.csv` for this side project (gitignored, same
  convention as repo-root `outputs/`)
- `archive/` — retired figures, tables, docs and one retired script, moved here (never deleted)
  on 2026-11-05; see `archive/README.md` for what moved and why
- `expanded/` — the 153-site tree, with its own `code/`, `data/`, `tables/`, `figures/`
  (`main/` pending rerun, `methods_review/` as above), `docs/`, `logs/`
