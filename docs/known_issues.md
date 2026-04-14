# Known Issues

This file tracks bugs and data quality issues encountered during pipeline development.
Issues are reported to the relevant maintainers. Last updated: 2026-04-14.

---

## Section 1 — fluxnet R package issues

Repository: [EcosystemEcologyLab/fluxnet](https://github.com/EcosystemEcologyLab/fluxnet)
Maintainer: Eric Scott ([@Aariq](https://github.com/Aariq))

| Issue | Description | Impact | Workaround | Action |
|---|---|---|---|---|
| `flux_badm()` calls `quit()` | Function terminates the R session rather than throwing a catchable error when called from within a script | `03_read.R` exits after ~24 seconds without processing BADM data | Call in a subprocess or read BIF files directly | Open GitHub issue on EcosystemEcologyLab/fluxnet |

---

## Section 2 — FLUXNET Shuttle issues

Repository: [github.com/fluxnet/shuttle](https://github.com/fluxnet/shuttle)
Contacts: Danielle Christianson, Dario Papale

| Issue | Description | Impact | Workaround | Action |
|---|---|---|---|---|
| `httr2_failure` on batch download | HTTP request failures during download of batches 12 and 13 — `resp` is not an HTTP response object | Some sites may not download on first attempt | Re-run `batch_download.R` — resumable design handles retries | Report to support@fluxnet.org |
| AU-Dry BIF column order | `TERN_AU-Dry_FLUXNET_BIF_2009-2025_v1.3_r1.csv` has columns in unexpected order causing read failure | Site excluded from read stage | Fixed in `03_read.R` with column reordering | Report to TERN data contributor |
| Windows file paths in `VARIABLE_GROUP` | 3 BIF files contain Windows-style file paths appearing as `VARIABLE_GROUP` values | Inflates group count and adds processing overhead | Filtered in `03_read.R` with regex guard | Report to support@fluxnet.org with affected site IDs |
