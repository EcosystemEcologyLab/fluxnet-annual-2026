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

## Environment Variables

All configuration is via environment variables. In a Codespace these are set
as GitHub Codespace Secrets. For local use, copy `.env.example` to `.env`.

| Variable                 | Purpose                                              | Default                        |
|--------------------------|------------------------------------------------------|-------------------------------|
| `AMERIFLUX_USER_NAME`    | Registered AmeriFlux account name                    | — (required)                  |
| `AMERIFLUX_USER_EMAIL`   | Registered AmeriFlux account email                   | — (required)                  |
| `AMERIFLUX_INTENDED_USE` | AmeriFlux intended use code (1–6)                    | `1` (Synthesis)               |
| `FLUXNET_SHUTTLE_VERSION`| Expected fluxnet-shuttle version                     | `0.2.0`                       |
| `FLUXNET_SNAPSHOT_MODE`  | `"development"` or `"locked"`                        | `"development"`               |
| `FLUXNET_SNAPSHOT_FILE`  | Path to locked snapshot CSV                          | — (required if locked)        |

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

## Pipeline Execution Order

Scripts in `scripts/` are numbered and must be run in order:

```
01_download.R   → flux_listall() + flux_download()
02_extract.R    → flux_extract() + flux_discover_files()
03_read.R       → flux_read() + flux_varinfo() + flux_badm()
04_qc.R         → flux_qc() + fluxnet_qc_hh()
05_units.R      → fluxnet_convert_units()
06_analysis.R   → paper-specific analyses
07_figures.R    → figure generation
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

### Functions
- Every function in `R/` must have a roxygen2 documentation header
- Every function must have at least one test in `tests/testthat/`
- Functions that combine sites must check for temporal resolution mismatch
  and stop with a clear error if HH and HR data are mixed without explicit
  aggregation

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
QC_THRESHOLD_DD <- 0.75   # daily
QC_THRESHOLD_WW <- 0.75   # weekly
QC_THRESHOLD_MM <- 0.75   # monthly
QC_THRESHOLD_YY <- 0.75   # annual
```

- HH/HR analysis: keep records where `_QC <= 1`
- DD/WW/MM/YY: keep records where `_QC > QC_THRESHOLD_*`
- ERA-Interim fills: flag but do not drop by default (`_F_QC == 2` is marked)
- USTAR filtering: already applied in the distributed data — do not re-apply

The FLUXNET published convention for coarse resolutions is `> 0.5`.
The stricter default of `> 0.75` is intentional for this synthesis paper.
To reproduce FLUXNET2015 published figures, set all thresholds to `0.5`.

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
| Route `flux_download()` through shuttle Python API for reliable bulk downloads and credential passing | EcosystemEcologyLab/fluxnet-package#43 |
| Pin internal fluxnet-shuttle install to v0.2.0 tag | EcosystemEcologyLab/fluxnet-package (issue posted) |
| Extend `flux_qc()` for HH flag filtering and per-resolution thresholds | EcosystemEcologyLab/fluxnet-package (issue posted) |
| Add unit conversion functions to fluxnet package | EcosystemEcologyLab/fluxnet-package (issue posted) |
| Response pending from Eric Scott re: #43 download rearchitecture | EcosystemEcologyLab/fluxnet-package#43 |