# CLAUDE.md — fluxnet-annual-2026

## Project Context

This repository contains the complete analysis pipeline for the FLUXNET
Annual Paper 2026, a global synthesis of eddy covariance flux tower data
coordinated by the FLUXNET Coordination Project (PI: Trevor Keenan; Co-PIs:
David Moore, University of Arizona; Kim Novick, Indiana University).

The repository is maintained by David J.P. Moore (davidjpmoore,
EcosystemEcologyLab, University of Arizona).

All analysis code in this repository is R. There is no Python code written
directly in this repository — Python is used only internally by the `fluxnet`
R package via `reticulate` to run the FLUXNET Shuttle.

---

## Lab Principles Source

- Repository: EcosystemEcologyLab/lab-principles
- Commit: 5cb862187f5c77befc5397c2c50eba178b59c382
- Copied: 2026-03-27
- SCIENCE_PRINCIPLES.md v1.0
- SCIENCE_PRINCIPLES_PIPELINES.md v1.0

---

## Hard Rules — Read These First

### 1. Data source for the Annual Paper
The FLUXNET Annual Paper 2026 must use **only** data sourced from the FLUXNET
Shuttle via `flux_listall()` and `flux_download()`. No data from FLUXNET2015,
LaThuile, AmeriFlux-FLUXNET product, ICOS-FLUXNET product, or any other prior
static release may be used as **primary data** for this paper.

Other datasets (FLUXNET2015, AmeriFlux BASE, ICOS L2, community products such
as FLUXCOM or FLUXNET-CH4) may be used **only** for:
- Figure mocking or illustration during development
- Supplementary or comparison analyses explicitly labelled as such
- Validation of Shuttle-derived results

Any use of non-Shuttle data must be clearly commented in the code and labelled
in any output it produces.

### 2. Never infer hub or region from site ID prefix
Site ID prefixes (e.g. `US-`, `DE-`, `AU-`) encode country codes, not hub
membership. Do not write code that maps site ID prefixes to hubs or assumes
geographic region from hub membership. Use the `network` field from the
manifest or snapshot CSV instead.

### 3. Never hard-code credentials
AmeriFlux credentials and all configuration values must be read from
environment variables. Never write credential values into R scripts,
.Rmd/.qmd files, or any committed file. See `.env.example` for the
full list of required environment variables.

### 4. Never commit data files
The `data/raw/`, `data/extracted/`, and `data/processed/` directories are
gitignored and must never be committed. The `data/snapshots/` directory
is the only part of `data/` that is committed. Figures and outputs are
also gitignored.

### 5. Never silently ignore version mismatches
`check_pipeline_config()` must be called at the start of every pipeline
script. It checks shuttle version, credentials, and snapshot mode. If it
warns, the warning must be visible in the script output — do not suppress it.

---

## Autonomy and Permissions

Check the environment before deciding how much autonomy to apply:

**In a GitHub Codespace** (`CODESPACE_NAME` environment variable is set):
This is a sandboxed environment. Proceed autonomously without asking for
confirmation on:
- Bash commands (running scripts, checking logs, disk usage, process status)
- Reading any file in the repository
- Writing or editing files in the repository
- Git operations (add, commit, push)
- Installing R packages via renv
- Running pipeline scripts (01 through 07, batch_download, diagnostics,
  candidate figures)

**On a local machine or HPC** (`CODESPACE_NAME` is not set):
Apply standard caution — ask before running bash commands that modify files,
install packages, or push to git. This protects production data on HPC and
local research files on personal machines.

**Always ask regardless of environment:**
- Deleting files or directories outside of `data/downloads/` or `data/raw/`
  (Renames within the repository are autonomous; renames that move files out of
  the repository follow the "outside the repository directory" rule.)
- Force pushing to git
- Any action outside the repository directory
- Making changes to `.devcontainer/devcontainer.json`
- Deleting or moving files in `data/snapshots/` (these are authoritative
  manifests and the only committable part of `data/`)
- Modifying `renv.lock` directly, or running `renv::snapshot()` /
  `renv::restore()` (these change the package environment and can leave the
  Codespace in a broken state)

**Long-running pipeline scripts:**
When launching long-running pipeline scripts (reprocessing scripts 02–05,
batch downloads, full figure regeneration), use the established
`nohup` + `disown` pattern and log to a file in `logs/`. Do not run these
scripts in the foreground; Codespace terminal idle timeouts can interrupt them.
Report the PID and log path after launching, then monitor periodically rather
than blocking.

---

## Environment Variables

All configuration is via environment variables. In a Codespace these are set
as GitHub Codespace Secrets. For local use, copy `.env.example` to `.env`.

| Variable                 | Purpose                                              | Default                        |
|--------------------------|------------------------------------------------------|-------------------------------|
| `AMERIFLUX_USER_NAME`    | Registered AmeriFlux account name                    | — (required)                  |
| `AMERIFLUX_USER_EMAIL`   | Registered AmeriFlux account email                   | — (required)                  |
| `AMERIFLUX_INTENDED_USE` | AmeriFlux intended use code (1–6)                    | `1` (Synthesis)               |
| `FLUXNET_SHUTTLE_VERSION`| Expected fluxnet-shuttle version                     | `0.3.7.post0+dirty`           |
| `FLUXNET_SNAPSHOT_MODE`  | `"development"` or `"locked"`                        | `"development"`               |
| `FLUXNET_SNAPSHOT_FILE`  | Path to locked snapshot CSV                          | — (required if locked)        |
| `FLUXNET_DATA_ROOT`      | Root directory for all pipeline data I/O             | `"data"`                      |
| `FLUXNET_EXTRACT_RESOLUTIONS` | Space-separated `flux_extract()` resolution codes | `"y m d"`                |

AmeriFlux intended use codes:
- 1 = Synthesis / network synthesis analysis (default for this project)
- 2 = Model
- 3 = Remote sensing
- 4 = Other research
- 5 = Education
- 6 = Other

---

## Installation and Startup

On Codespace startup or fresh local install, run:

```r
# Install R package dependencies
install.packages("pak")
pak::pak("EcosystemEcologyLab/fluxnet-package")
pak::pak("dotenv")

# Load environment variables (local use only — Codespace uses Secrets)
dotenv::load_dot_env()

# Validate pipeline configuration
source("R/pipeline_config.R")
check_pipeline_config()
```

`check_pipeline_config()` will:
- Check that `AMERIFLUX_USER_NAME` and `AMERIFLUX_USER_EMAIL` are set
- Check that installed `fluxnet-shuttle` version matches `FLUXNET_SHUTTLE_VERSION`
  and warn (not error) if not
- Check that snapshot mode is valid
- If `AMERIFLUX_INTENDED_USE` is not set, print the default value
  (`1 = network synthesis analysis`) and invite the user to change it

The first call to `flux_listall()` will automatically install `fluxnet-shuttle`
into a Python virtualenv named `"fluxnet"` managed by `reticulate`. Python must
be available in the environment.

To force reinstall of fluxnet-shuttle:
```r
reticulate::virtualenv_remove("fluxnet")
# Then re-run flux_listall() to rebuild
```

---

## Session Start — Multi-Machine Sync

This repository is worked on from both the local mini and the Codespace; either machine may
have pushed commits since the last session. At the start of every session, before doing
anything else:

1. **Check working tree status:** `git status`
2. **If the working tree is clean** (no modified tracked files, or only gitignored/untracked
   files): run `git pull` to bring the branch up to date with origin.
3. **If there are uncommitted modifications to tracked files:** do not pull blindly. Report
   the modified files and their likely authority (e.g., locally-regenerated snapshot outputs
   vs. drafting changes). Stash, commit, or explicitly handle the conflict first, then pull.

The snapshot files (`data/snapshots/*.csv`, `*.meta.json`), `outputs/session_info.txt`, and
`renv/profile` are frequently modified locally and represent separate commit decisions — flag
them rather than overwriting with a pull.

---

## renv Profile Selection

This project uses renv profiles to maintain separate lockfiles for
different environments.

At the start of every session, detect the environment and activate
the correct profile before doing anything else:

- If `CODESPACE_NAME` is set: activate the codespace profile
  `renv::activate(profile = 'codespace')`

- If `CODESPACE_NAME` is not set (local machine): activate the macos profile
  `renv::activate(profile = 'macos')`

Never run `renv::snapshot()` without first confirming which profile is
active. Never commit a lockfile change without noting which profile
it applies to in the commit message.

### Updating the codespace lockfile from a local machine

The codespace profile library (`renv/profiles/codespace/renv/`) is empty on the
local mini — packages are only installed inside the GitHub Codespace. This means
`renv::activate(profile = "codespace"); renv::snapshot()` will produce an empty
or incorrect lockfile when run locally.

**Correct procedure for adding new packages to both lockfiles:**

1. Install the package locally and confirm it works.
2. Update the **macos lockfile** normally:
   ```r
   renv::activate(profile = "macos")
   renv::snapshot(prompt = FALSE)
   ```
3. Copy the new package entry/entries into the **codespace lockfile** using
   Python's `json` module (which preserves the verbose-DESCRIPTION format that
   the codespace lockfile uses):
   ```python
   import json
   with open("renv/profiles/macos/renv.lock") as f:
       mac = json.load(f)
   with open("renv/profiles/codespace/renv.lock") as f:
       cs = json.load(f)
   for pkg in ["new_pkg", "its_dep"]:
       cs["Packages"][pkg] = mac["Packages"][pkg]
   cs["Packages"] = dict(sorted(cs["Packages"].items()))
   with open("renv/profiles/codespace/renv.lock", "w") as f:
       json.dump(cs, f, indent=2, ensure_ascii=False)
       f.write("\n")
   ```
4. Verify: `git diff --stat renv/profiles/codespace/renv.lock` should show
   only insertions (no deletions to existing entries).
5. Commit both lockfiles together, noting both profiles in the commit message.

---

## Pipeline Execution Order

Scripts in `scripts/` are numbered and must be run in order:

```
01_download.R          → flux_listall() + flux_download()
02_extract.R           → flux_extract() + flux_discover_files()
03_read.R              → flux_read() + flux_varinfo() + flux_badm()
03b_create_database.R  → creates data/duckdb/fluxnet.duckdb by reading all
                         extracted CSVs directly into DuckDB tables (annual,
                         monthly, weekly, daily, hourly). Avoids the in-memory
                         accumulation that caused a 16 GB OOM on DD resolution.
                         Run after 03_read.R; prerequisite for the DuckDB-based
                         04_qc.R. (sources duckdb_setup.R + duckdb_update.R)
04_qc.R                → reads DuckDB tables, applies QC threshold (>= 0.50)
                         per site, writes filtered copies (annual_qc, monthly_qc,
                         weekly_qc, daily_qc, hourly_qc) back into the database.
                         Row exclusion uses NEE_VUT_REF_QC for VUT sites and
                         NEE_CUT_REF_QC for CUT-only sites (per-site fallback,
                         not per-row). See commit ad7464f.
05_units.R             → fluxnet_convert_units()
06_analysis.R          → paper-specific analyses
07_figures.R           → figure generation
```

Do not skip steps or run them out of order. Each script sources
`R/pipeline_config.R` and calls `check_pipeline_config()` at the top.

---

## Coding Conventions

### Language and style
- All code is R
- Follow the tidyverse style guide: https://style.tidyverse.org/
- Use the base R pipe `|>` (not `%>%`)
- Function names use `snake_case`
- File names use `snake_case`

### Package preferences
- Data manipulation: `dplyr`, `tidyr`, `purrr`
- File system: `fs`
- Date/time: `lubridate`
- Reading data: `readr`
- Plotting: `ggplot2`
- Do not introduce new package dependencies without discussion

### Figures
- All review figures saved to `review/figures/` must use a **white (opaque)
  background** — pass `bg = "white"` to every `ggsave()` call. Transparent
  backgrounds are only acceptable when a figure is explicitly requested for
  overlay or compositing use.

### Functions
- Every function in `R/` must have a roxygen2 documentation header
- Every function must have at least one test in `tests/testthat/`
- Functions that combine sites must check for temporal resolution mismatch
  and stop with a clear error if HH and HR data are mixed without explicit
  aggregation

### R 4.1+ backslash-escape gotcha

In R 4.1+, `"\\_"` is a recognised escape sequence (soft hyphen / non-breaking
underscore), **not** a backslash followed by an underscore. This means:

```r
# WRONG — "\\\\_" in R 4.1+ is parsed as \\ (one backslash) + \_ (soft hyphen)
gsub("\\\\_", "_", title, fixed = TRUE)   # does NOT match a literal \_

# CORRECT — use a regex that matches one literal backslash then any character
gsub("\\\\(.)", "\\1", title, perl = TRUE)   # strips any LaTeX backslash escape
```

This affects any code that tries to unescape LaTeX strings read from `.bib`
files or other LaTeX sources. The regex approach above strips all `\x` sequences
(e.g. `\_` → `_`, `\-` → `-`, `\&` → `&`) and is the canonical fix in this repo.
Discovered in `R/parse_bib_to_apa.R` (2026-05-28); applies anywhere LaTeX
escape sequences appear in character data.

---

## Temporal Resolution

- `flux_read()` reads files at native resolution — no resampling at ingest
- The manifest carries a `temporal_resolution` field per site (HH or HR)
- HH = half-hourly (30-minute intervals); HR = hourly (60-minute intervals)
- Aggregation from HH to HR is an explicit user step, never automatic
- Aggregation rules are variable-type-aware:
  - Instantaneous variables (TA, SW_IN, NEE flux rates, etc.): **average**
  - Accumulated variables (P precipitation): **sum**
  - QC flags during aggregation: carry forward **minimum** flag value
- Any function combining sites across resolutions must `stop()` with a
  clear message if resolution mismatch is detected

---

## QC Flag Reference

**Two QC flag systems are used. Do not confuse them.**

### System 1: `*_QC` flags for gap-filled variables

At HH/HR resolution (integer):
- `0` = measured
- `1` = good quality gap-fill (MDS)
- `2` = medium quality gap-fill
- `3` = poor quality gap-fill

At DD/WW/MM/YY resolution (fraction 0–1):
- Fraction of underlying HH/HR records that were measured or good gap-fill
- **This is NOT the same as the integer scale** — a common source of errors

### System 2: `*_F_QC` flags for consolidated meteorological variables
- `0` = measured
- `1` = MDS gap-filled (from nearby measurements — reliable)
- `2` = ERA-Interim gap-filled (from reanalysis — less reliable)

### Default QC thresholds
All DD/WW/MM/YY thresholds are set as named constants in `R/pipeline_config.R`.
Change them there to adjust pipeline-wide filtering.

```r
QC_THRESHOLD_DD <- 0.50   # daily
QC_THRESHOLD_WW <- 0.50   # weekly
QC_THRESHOLD_MM <- 0.50   # monthly
QC_THRESHOLD_YY <- 0.50   # annual
```

- HH/HR analysis: keep records where `_QC <= 1`
- DD/WW/MM/YY: keep records where `_QC > QC_THRESHOLD_*`
- ERA-Interim fills: flag but do not drop by default (`_F_QC == 2` is marked)
- USTAR filtering: already applied in the distributed data — do not re-apply

The FLUXNET published convention for coarse resolutions is `> 0.5`. The default
for this synthesis paper is also `0.50`. Row exclusion is per-site:
- Sites with any non-NA `NEE_VUT_REF_QC` values: gated on `NEE_VUT_REF_QC >= 0.50`
- CUT-only sites (where `NEE_VUT_REF_QC` is entirely NA across all of that site's
  rows): gated on `NEE_CUT_REF_QC >= 0.50` as a fallback

The fallback is **per-site, not per-row** — VUT and CUT QC are not mixed within
a single site. Exclusion logs record which QC column drove each exclusion
(`NEE_VUT_REF_QC` or `NEE_CUT_REF_QC`). Secondary variable QC columns (GPP, RECO,
LE, H) are retained in output tables but do not drive row exclusion. Implementation
in `scripts/04_qc.R` (commit ad7464f). The decision to use 0.50 rather than 0.75
is documented in `docs/decisions_pending.md` (resolved 2026-04-20) and confirmed in
`docs/methods_requirements.md` §5.3. To reproduce FLUXNET2015 published figures,
no threshold change is needed; to apply a stricter filter, raise all thresholds
to `0.75` in `R/pipeline_config.R`.

---

## Unit Conversion Reference

Use `fluxnet_convert_units()` in `R/units.R` for all unit conversions.
Never implement ad-hoc unit conversions inline in analysis scripts.

| Variable       | Native FLUXNET unit      | Analysis unit         | Notes                              |
|----------------|--------------------------|-----------------------|------------------------------------|
| NEE, GPP, RECO | µmol CO₂ m⁻² s⁻¹        | gC m⁻² per period     | Molar mass of C = 12 g/mol, NOT CO₂|
| LE             | W m⁻²                   | mm H₂O per period     |                                    |
| H              | W m⁻²                   | MJ m⁻² per period     |                                    |
| TA             | °C                       | K                     | Add 273.15                         |
| P              | mm per timestep          | mm per period         | Sum, not average                   |
| VPD            | hPa                      | kPa                   | Divide by 10                       |
| SW_IN          | W m⁻²                   | MJ m⁻² per period     |                                    |

All conversions are timestep-aware — `fluxnet_convert_units()` reads
`temporal_resolution` from the manifest and applies the correct factor
(1800s for HH, 3600s for HR). It will `stop()` if resolution is missing.

---

## Output Metadata

Every output file (CSV, RDS, figure) must be accompanied by a companion
`.meta.json` file with the same base name. Use `write_output_metadata()`
in `R/utils.R` to generate this file — never write metadata manually.

Required fields:

| Field | Content | How to populate |
|---|---|---|
| `run_datetime_utc` | ISO 8601 timestamp of pipeline run | `format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")` |
| `pipeline_version` | Git commit hash at run time | `system("git rev-parse --short HEAD", intern = TRUE)` |
| `input_sources` | Snapshot CSV path + per-site DOIs from snapshot | from `data/snapshots/` and snapshot `data_citation` field |
| `r_session_info` | Output of `sessionInfo()` | saved to `outputs/session_info.txt` at end of every run |
| `notes` | Manual decisions, overrides, or deviations from defaults | free text; empty string if none — field must always be present |

The companion file for `outputs/nee_annual.csv` is `outputs/nee_annual.meta.json`.
`r_session_info` is always written to `outputs/session_info.txt` — not embedded
in individual companion files.

---

## Exclusion Logging

Every record excluded from analysis must be logged. Exclusions and unknowns
are tracked in two separate files, both in `outputs/` (gitignored —
regenerated each run). Use `log_exclusion()` and `log_unknown()` in
`R/utils.R` — never drop records without calling these functions.

### outputs/exclusion_log.csv

| Column | Content |
|---|---|
| `site_id` | FLUXNET site ID |
| `variable` | Variable name or `ALL` if whole record is excluded |
| `timestamp` | Record timestamp or `ALL` if whole site-year is excluded |
| `reason` | Human-readable reason (e.g. `QC_THRESHOLD_YY=0.75 not met`) |
| `threshold` | The threshold or rule applied |
| `excluded_by` | Script name that performed the exclusion |

### outputs/unknown_log.csv

| Column | Content |
|---|---|
| `record_id` | Site ID or record identifier |
| `reason` | Why the record could not be assessed |
| `logged_by` | Script name |

Both files must be written even if empty (zero-row CSV with headers).
A summary of exclusion and unknown counts must be printed to the console
at the end of each QC script run.

**Exclusion vs Unknown:**
- Failed QC threshold → exclusion log
- Missing data, failed download, or unassessable record → unknown log

---

## Locking the Dataset for Final Paper Analysis

When ready to lock the dataset for the final paper analysis:

1. Set `FLUXNET_SNAPSHOT_MODE="development"` and run `01_download.R` one
   final time to generate the definitive snapshot
2. Commit the snapshot CSV to `data/snapshots/` with a clear commit message:
   `"Lock dataset snapshot for final paper analysis"`
3. Set `FLUXNET_SNAPSHOT_MODE="locked"` and
   `FLUXNET_SNAPSHOT_FILE="data/snapshots/fluxnet_shuttle_snapshot_YYYYMMDDTHHMMSS.csv"`
   in Codespace Secrets and `.env`
4. Run the full pipeline once in locked mode to confirm clean reproduction
5. Record the snapshot filename and the `fluxnet-shuttle` version in the
   paper's Methods section

---

## Data Use and Citation

All Shuttle data is CC-BY-4.0. Per-site DOIs are available in the snapshot
CSV (`data_citation` field) and must be included in the paper.

Cite:
- Pastorello et al. (2020) Sci. Data 7:225 for the ONEFlux pipeline
- The `fluxnet` R package: doi:10.5281/zenodo.19210221
- Each site's individual DOI from the snapshot CSV

---

## Known Pending Items

The following items are in progress. Use the stopgap functions in `R/` until
the package is updated. Do not implement workarounds.

| Item | Tracked in |
|------|-----------|
| Pin internal fluxnet-shuttle install to v0.2.0 tag | EcosystemEcologyLab/fluxnet-package (issue posted) |
| Extend `flux_qc()` for HH flag filtering and per-resolution thresholds | EcosystemEcologyLab/fluxnet-package (issue posted) |
| Add unit conversion functions to fluxnet package | EcosystemEcologyLab/fluxnet-package (issue posted) |

# Shuttle version monitoring: check https://github.com/fluxnet/shuttle/releases
# for new releases. Current pin: tag 0.3.7 (commit 3f3bd767), which self-reports
# as "0.3.7.post0+dirty" — this string is embedded in the package metadata at that
# tag, not a local build artefact. pipeline_config.R defaults FLUXNET_SHUTTLE_VERSION
# to "0.3.7.post0+dirty" to match the self-reported string; the env vars table above
# reflects this. The install reference (the tag name) and the version-check string
# differ; resolving this cleanly is part of the paper-lock task in decisions_pending.md.
# Run flux_listall() after upgrading to confirm snapshot format is unchanged.