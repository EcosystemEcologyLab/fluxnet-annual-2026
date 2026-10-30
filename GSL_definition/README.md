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

**Stage 1 and 2 done.** Stage 1: scaffold, logging fix, site extraction, both screenings,
year-screening option counts, Tash-rule reproduction across the whole network. Stage 2: every
derived quantity (P99, skewness, GSL, T95, etc.) for the main run and sensitivity variants, and
the match checks against the manuscript's published values — see
`docs/reproduction_report.md`. **Stopped for PI sign-off before any figure** (Stage 3).

The Stage 2 open decisions (year screening, negative-GPP treatment, `>` vs. `>=`, GPP variant)
were made by Dave without Tash's input, recorded in `docs/framework.md`'s "Open decisions
(resolved)" section and as named switches in `code/00_config.R` — subject to revision if Tash's
own materials later contradict them.

## Directory layout

- `code/` — numbered scripts, each sourcing `code/00_config.R`
- `data/` — site extracts and large derived tables (fine grid, fraction sweep) pulled from the
  repo-root DuckDB store (gitignored)
- `tables/` — analysis output tables (git-tracked)
- `figures/` — not used before Stage 3
- `docs/` — `framework.md` (verbatim), `methods_memo.md` (every stage's record),
  `reproduction_report.md` (Stage 2's match checks)
- `logs/` — `exclusion_log.csv`/`unknown_log.csv` for this side project (gitignored, same
  convention as repo-root `outputs/`)
