# Stage 1 baseline — what was captured, what was missing

Captured (copied into this directory), all from `data/snapshots/`:
- `fluxnet_shuttle_snapshot_20260901T094522.csv` (most recent snapshot on disk; its `.meta.json` companion does not exist on disk, so not copied)
- `representativeness_metrics.csv`
- `site_koppen_era5.csv`
- `flux_medians_by_igbp_{et,gpp,h,nep,ter}.csv` (5 files — the flux-median tables)

Not found (searched, listed as missing, non-blocking per instruction):
- "Per-site counts behind draft figures" — searched `review/figures/*.csv` and `outputs/*.csv` for a matching file. Found only `outputs/exclusion_log.csv`, `outputs/unknown_log.csv`, and `outputs/authorship/*` (unrelated, authorship-invitation diagnostics). No file matching this description exists under a discoverable name. Not copied.

DuckDB row counts per resolution table (`data/duckdb/fluxnet.duckdb`, read-only connection): see `duckdb_row_counts_before.csv` in this directory — all 16 tables (5 resolutions x {raw, `_qc`, `_converted`} plus `manifest`; `annual`/`weekly`/`hourly`/`daily`/`monthly` are the raw ingested tables).
