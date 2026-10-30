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
`logs/prompts/2026-10-29_gsl_stage1.md` and `SESSION_LOG.md`.

`code/` reads (never writes) the repo-root DuckDB store (`data/duckdb/fluxnet.duckdb`),
read-only, filtered to `dataset = 'FLUXMET'` (the pre-QC daily table also carries `dataset =
'ERA5'` rows, meteorology only — every query here must filter to FLUXMET; see
`docs/methods_memo.md`).

## Status

**Stage 1 only** (scaffold, logging fix, site extraction, both screenings, year-screening option
counts, Tash-rule reproduction across the whole network). No derived quantities (P99, skewness,
GSL, thresholds) and no figures yet — those are Stage 2 (after Tash's files arrive) and Stage 3
(after Dave signs off the reproduction report).

`docs/framework.md` (the "GSL Figure Rebuild: Analysis Framework" document, containing the
Sites table — 62 sites, groups, years — that most of Stage 1 depends on) **has not been received
yet** — only a "corrections to the framework" memo was. See `SESSION_LOG.md` for what is blocked
pending that document.

## Directory layout

- `code/` — numbered scripts, each sourcing `code/00_config.R`
- `data/` — site extracts pulled from the repo-root DuckDB store (gitignored)
- `tables/` — analysis output tables (git-tracked)
- `figures/` — not used in Stage 1
- `docs/` — `framework.md` (verbatim, pending), `methods_memo.md` (this stage's record)
- `logs/` — `exclusion_log.csv`/`unknown_log.csv` for this side project (gitignored, same
  convention as repo-root `outputs/`)
