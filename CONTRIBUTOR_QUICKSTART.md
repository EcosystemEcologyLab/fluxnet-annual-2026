# FLUXNET Annual Paper 2026 — Contributor Quickstart

**For:** Eric Scott and other collaborators reviewing or running the pipeline  
**Maintained by:** David Moore (davidjpmoore)  
**Repository:** https://github.com/EcosystemEcologyLab/fluxnet-annual-2026

---

## What this pipeline does

Downloads FLUXNET data from the Shuttle, applies QC filtering, converts units,
and produces a set of figures for the FLUXNET Annual Paper 2026. The pipeline
runs in R and uses the `fluxnet` package (which you maintain) to access the
Shuttle.

---

## Quick setup — local Mac or RStudio

### 1. Clone the repository

```bash
git clone https://github.com/EcosystemEcologyLab/fluxnet-annual-2026.git
cd fluxnet-annual-2026
```

### 2. Restore R packages

Open the project in RStudio, then in the R console:

```r
renv::restore()
```

This installs all locked package versions. Takes ~10–15 minutes the first time.

> **Note for macOS arm64 (M1/M2/M3):** If `Matrix` fails to compile from
> source, install the binary version first:
> ```r
> install.packages("Matrix", type = "binary")
> renv::restore()
> ```

### 3. Set up credentials

```bash
cp .env.example .env
```

Open `.env` and fill in your AmeriFlux credentials:

```
AMERIFLUX_USER_NAME=your_ameriflux_username
AMERIFLUX_USER_EMAIL=your_ameriflux_email
FLUXNET_DATA_ROOT=data
FLUXNET_EXTRACT_RESOLUTIONS=y m
FLUXNET_PROCESS_RESOLUTIONS=y m
FLUXNET_DELETE_ZIPS=TRUE
```

> `.env` is gitignored — never commit it.

### 4. Install the Python shuttle

The pipeline uses `reticulate` to call the FLUXNET Python shuttle library.
Install it once:

```r
reticulate::py_install("git+https://github.com/fluxnet/shuttle.git", pip = TRUE)
```

Verify:

```r
reticulate::py_run_string(
  "import importlib.metadata; print(importlib.metadata.version('fluxnet-shuttle'))"
)
# Should print: 0.3.7 (or higher)
```

---

## Running the pipeline

Load your credentials first, then run scripts in order:

```r
dotenv::load_dot_env(".env")

source("scripts/01_download.R")   # Download from Shuttle
source("scripts/02_extract.R")    # Extract CSVs from ZIPs
source("scripts/03_read.R")       # Read CSVs into RDS
source("scripts/04_qc.R")         # Apply QC filters
source("scripts/05_units.R")      # Convert units to gC m⁻² yr⁻¹
```

### Download a small test set first

To test with just a few sites before downloading everything, set
`FLUXNET_SITE_FILTER` in your `.env`:

```
FLUXNET_SITE_FILTER=DE-Tha US-Ha1 AU-Wom FI-Hyy
```

Remove or leave blank for all sites.

### Full download (~672 sites)

Use the batch download script — it downloads in batches of 50, deletes ZIPs
after extraction to save disk space, and is resumable if interrupted:

```r
dotenv::load_dot_env(".env")
source("scripts/batch_download.R")
```

Expected time: 60–120 minutes. Expected disk usage: ~300MB extracted CSVs
(ZIPs are deleted after extraction).

---

## Reviewing figures

After running scripts 01–05, generate the candidate figures report:

```r
dotenv::load_dot_env(".env")
source("scripts/00_candidate_figures.R")
```

Opens `outputs/candidate_figures.html` — all paper-candidate figures.

For pipeline QC diagnostics:

```r
source("scripts/00_diagnostics.R")
# Opens outputs/diagnostics.html
```

---

## Repository structure

```
scripts/
  00_diagnostics.R        # Pipeline QC report
  00_candidate_figures.R  # Paper-candidate figures (main review target)
  01_download.R           # Download from FLUXNET Shuttle
  02_extract.R            # Extract CSVs from ZIPs
  03_read.R               # Read CSVs into RDS
  04_qc.R                 # QC filtering
  05_units.R              # Unit conversion
  06_analysis.R           # Analysis (placeholder)
  07_figures.R            # Final paper figures (placeholder)
  batch_download.R        # Full download in batches of 50

R/
  pipeline_config.R       # QC thresholds, env vars, config checks
  credentials.R           # AmeriFlux credential handling
  qc.R                    # QC functions
  units.R                 # Unit conversion functions
  utils.R                 # Output metadata, exclusion logging
  sync.R                  # Snapshot comparison, update detection
  snapshot.R              # Snapshot management
  plot_constants.R        # Shared visual theme, IGBP palette
  external_data.R         # WorldClim and aridity index loaders
  figures/
    fig_igbp.R            # IGBP boxplots and time series
    fig_seasonal.R        # Seasonal cycles
    fig_maps.R            # Site maps, NEE maps
    fig_climate.R         # Whittaker hexbin, climate scatter
    fig_latitudinal.R     # Latitudinal ribbon plots
    fig_growing_season.R  # Growing season vs NEE
    fig_network_growth.R  # Network growth over time
    fig_timeseries.R      # Long-record time series by continent
    fig_environmental_response.R  # Flux vs climate response curves

data/
  snapshots/              # Shuttle snapshots (git-tracked)
  downloads/              # Raw ZIPs (gitignored, deleted after extraction)
  extracted/              # Extracted CSVs (gitignored)
  processed/              # RDS files (gitignored)
  external/               # WorldClim, aridity index (gitignored, HPC only)

legacy/                   # Reference scripts from prior analyses
                          # Do not source — reference only
review/figures/           # Review PNGs (git-tracked for offline review)
docs/
  figure_inventory.md     # Complete figure table with data requirements
  decisions_pending.md    # Open scientific decisions for co-author review
```

---

## Key design decisions

**Data source:** FLUXNET Shuttle only (CC-BY-4.0). No direct AmeriFlux or
ICOS downloads. See `CLAUDE.md` Hard Rule #1.

**QC thresholds:** Currently `QC > 0.50` for YY/MM/DD (lowered from 0.75 to
match FLUXNET published convention). See `R/pipeline_config.R` and
`docs/decisions_pending.md` for open discussion.

**Units:** YY/MM/DD FLUXNET data is pre-integrated and arrives in native units
(`gC m⁻² yr⁻¹` for flux variables). The pipeline does NOT apply a µmol → gC
conversion — it passes flux variables through unchanged and only converts
energy variables (LE, H) where needed.

**IGBP colours:** Fixed named palette in `R/plot_constants.R` — stable across
all figures. Use `scale_fill_igbp()` and `scale_color_igbp()` in all figure
functions.

---

## Open questions for co-author review

See `docs/decisions_pending.md` for items requiring scientific decisions,
including:

- QC threshold: 0.50 vs 0.75 vs site-level overrides
- US-Ha1 (Harvard Forest): all records excluded at 0.75 threshold
- Growing season figure: IGBP filter scope

---

## Questions?

Contact David Moore (davidjpmoore@arizona.edu) or open a GitHub issue at
https://github.com/EcosystemEcologyLab/fluxnet-annual-2026/issues
