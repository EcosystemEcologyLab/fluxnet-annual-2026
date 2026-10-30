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

## Status

**Stages 1, 2 and 3 done, plus a PeerJ restyle pass (2026-10-30).** Stage 1: scaffold, logging
fix, site extraction, both screenings, year-screening option counts, Tash-rule reproduction
across the whole network. Stage 2: every derived quantity (P99, skewness, GSL, T95, etc.) for the
main run and sensitivity variants, and the match checks against the manuscript's published
values — `docs/reproduction_report.md`. Stage 3 (+ revisions): Figures 1-4, the benchmark table,
and 4 supplementary figures (Figures 2/3 with `GPP_DT_VUT_REF` and with as-delivered negative
GPP) — full results in `docs/report_back_2026-10-29_stage3_revisions.md`. PeerJ restyle: all
figures restyled to PeerJ's author instructions (170mm width, >=8pt text, one PNG+PDF per
figure), a discrete colourblind-safe skewness-third colour palette, and several Figure 2 fixes —
full results in `docs/report_back_2026-10-30_peerj_restyle.md`; full methods record in
`docs/methods_memo.md`'s "PeerJ restyle" section. The current manuscript's own Figures 1-4,
rebuilt for direct comparison, remain **deferred** (not enough detail in the framework to
reproduce without the manuscript itself).

A separate, read-only **site-selection audit** (2026-10-30) asks whether long-running network
sites excluded by the manuscript's selection rule could be recovered by a rule that drops days or
years rather than the whole site — `docs/selection_audit_2026-10-30.md`
(`code/11_selection_audit.R`, `tables/selection_audit_*.csv`,
`figures/selection_audit_long_record_variants.png`). It does not change the 62-site analysis
above.

The Stage 2 open decisions (year screening, negative-GPP treatment, `>` vs. `>=`, GPP variant)
were made by Dave without Tash's input, recorded in `docs/framework.md`'s "Open decisions
(resolved)" section and as named switches in `code/00_config.R` — subject to revision if Tash's
own materials later contradict them.

## Figures (PeerJ naming)

One PNG + PDF per figure, named for direct submission (`figures/`), each with a companion
`.legend.txt` and `.meta.json`. Supplemental Figure S-numbers are assigned in citation order —
the order `code/10_supplementary.R` builds them in:

| File | Content |
|---|---|
| `Figure 1` | Mechanism at two representative sites (JP-Fjy, US-Ton) |
| `Figure 2` | Threshold sensitivity across all 62 sites (main run) |
| `Figure 3` | Consequences for GSL trends vs. sampling uncertainty (21 trend sites) |
| `Figure 4` | The fraction-of-annual-GPP method and its own sensitivity |
| `Supplemental Figure S1` | Figure 2, `GPP_DT_VUT_REF` variant |
| `Supplemental Figure S2` | Figure 2, negative-GPP-as-delivered variant |
| `Supplemental Figure S3` | Figure 3, `GPP_DT_VUT_REF` variant |
| `Supplemental Figure S4` | Figure 3, negative-GPP-as-delivered variant |

## Directory layout

- `code/` — numbered scripts, each sourcing `code/00_config.R`; `fig_helpers.R` holds the shared
  PeerJ figure style (text sizes, line weights, colour scales) and `save_gsl_figure()`
- `data/` — site extracts and large derived tables (fine grid, fraction sweep) pulled from the
  repo-root DuckDB store (gitignored)
- `tables/` — analysis output tables (git-tracked); candidate Supplemental Data groupings and
  sizes are in `docs/data_and_code_availability.md`
- `figures/` — Figures 1-4 and Supplemental Figures S1-S4 (PNG + PDF + `.legend.txt` +
  `.meta.json` each)
- `docs/` — `framework.md` (verbatim), `methods_memo.md` (every stage's record),
  `reproduction_report.md` (Stage 2's match checks), `report_back_*.md` (per-pass results),
  `data_and_code_availability.md` (snapshot/DOI/Supplemental Data notes for submission),
  `selection_audit_*.md` (site-selection audit reports, independent of the 62-site analysis)
- `logs/` — `exclusion_log.csv`/`unknown_log.csv` for this side project (gitignored, same
  convention as repo-root `outputs/`)
