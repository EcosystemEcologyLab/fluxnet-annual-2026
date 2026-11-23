# RSCOTT_AGU26 -- dryland GPP vs. precipitation extraction for Russ Scott (AGU26 poster)

Side extraction for Russ Scott (USDA-ARS, Tucson), from the FLUXNET Annual Paper 2026 pipeline's
already-extracted data (read-only; no new downloads), for his AGU26 poster on productivity
against water availability across dryland sites. This project only extracts and packages data;
Russ does the GPP-precipitation analysis and interpretation himself.

## Packages

- **`package_rscott_2026-11-23_v2/` -- current.** Widened selection (54 kept sites, up from 43),
  with extra filtering columns (precipitation source, mean air temperature, Koppen class,
  aridity index, negative-GPP counts) so Russ can apply his own cuts rather than having sites
  dropped for him. See its own `README.md` for the full column reference, and
  `docs/report_back_2026-11-23_v2.md` for what changed and why.
- **`package_rscott_2026-11-23/` -- superseded, left unchanged.** The original, stricter
  extraction (43 kept sites, measured-precipitation-only keep rule). Still valid and usable on
  its own; kept in place rather than deleted in case Russ already started from it. The v2
  `README.md` explains exactly how to reproduce this selection from the v2 package's columns, if
  preferred.

Both packages share the same underlying data, conventions, units, QC rules and water-year
conventions -- see `code/`, `tables/`, and `docs/` for the full build. `docs/report_back_2026-11-23.md`
(v1) documents the P_F_QC polarity confirmation and the repo-wide MM-resolution `P_F` unit bug
discovered during this work; both still apply to v2 unchanged.
