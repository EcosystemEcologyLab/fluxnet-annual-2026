# Methods Requirements — FLUXNET Annual Paper 2026

This document defines what each methods section must cover and identifies
the primary code files from which accurate methods text will be derived.
It is not a draft — it is a requirements specification.

**Status:** Requirements definition phase. Methods text will be drafted
when analyses are finalised. Do not draft prose from this document yet.

**Repository:** https://github.com/EcosystemEcologyLab/fluxnet-annual-2026

---

## 5.1 Dataset assembly and annual snapshot definition

**Purpose:** Describe how the annual dataset was assembled from regional
hubs and frozen as a reproducible snapshot. Highlight use of
content-addressable persistent identifiers (PIDs) for reproducibility.

**Must cover:**
- FLUXNET Shuttle as the data source (version, date of download)
- Which regional hubs were included (AmeriFlux, ICOS, TERN, others)
- How the snapshot was defined and frozen (product_id checksum-based PID)
- Number of sites downloaded, number passing initial extraction
- Temporal scope of the dataset
- Data license (CC-BY-4.0)

**Primary code files:**
- `scripts/01_download.R` — download logic and hub selection
- `scripts/batch_download.R` — batch download implementation
- `scripts/02_extract.R` — extraction and ZIP cleanup
- `R/snapshot.R` — snapshot detection and PID tracking
- `R/credentials.R` — hub authentication
- `data/snapshots/download_progress.csv` — download audit trail
- `R/pipeline_config.R` — configuration constants

**Key facts to include (update when finalised):**
- Shuttle version: 0.3.7
- Download date: 2026-04-14
- Sites downloaded: 672
- Hubs: AmeriFlux (340), ICOS (280), TERN (52)

---

## 5.2 Site selection and metadata handling

**Purpose:** Define site inclusion criteria, time windows, biome
assignment, regional grouping, and metadata fields used in the analysis.

**Must cover:**
- Site inclusion criteria (CC-BY-4.0 only, sites with valid NEE_VUT_REF)
- How sites with all-missing NEE were identified and excluded
  (ONEFlux 15-day gap rule — confirmed by Dario Papale 2026-04-16)
- NEE_CUT fallback for 36 sites where NEE_VUT_REF unavailable
- IGBP classification source (BADM/BIF files)
- UN subregional grouping (countrycode package, iso2c to un.regionsub.name)
- FAO Global Ecological Zone assignment (GEZ 2010 shapefile, spatial join)
- Koppen-Geiger climate classification (from BADM CLIMATE_KOEPPEN variable)
- Functionally active site definition (≥3 months valid NEE in last 4 years)
- Historical dataset site lists (Marconi, La Thuile, FLUXNET2015)

**Primary code files:**
- `scripts/03_read.R` — site reading and BADM extraction
- `R/utils.R` — `compute_site_year_presence()`, `is_functionally_active()`
- `R/external_data.R` — GEZ, WorldClim, aridity index loading
- `R/historical_datasets.R` — historical site list loading
- `data/snapshots/site_candidates_full.csv` — master site metadata table
- `data/snapshots/site_gez_lookup.csv` — GEZ assignments
- `data/snapshots/site_year_data_presence.csv` — monthly data presence
- `docs/known_issues.md` — documented exclusions and their reasons

**Key facts to include (update when finalised):**
- Sites with valid NEE_VUT_REF: 530 of 672
- Sites excluded (all-missing NEE): 106 (ONEFlux gap rule)
- Sites with NEE_CUT only: 36
- Functionally active threshold: 4 years, ≥3 months valid NEE

---

## 5.3 Harmonisation and flux processing

**Purpose:** Describe QA/QC, variable harmonisation, gap-filling,
partitioning, and uncertainty handling. Cross-hub validation if applicable.

**Must cover:**
- ONEFlux processing pipeline (all hubs use same pipeline)
- QC gating: NEE_VUT_REF_QC threshold = 0.50, primary variable only
  (not all QC columns — see pipeline decision log)
- Variable naming conventions (FLUXNET standard)
- Unit handling: pre-integrated annual/monthly data passed through
  unchanged; energy variables converted (LE→mm, H→MJ)
- ERA5 climate variable integration (already in FLUXNET files)
- Known data quality issues (anomalous ERA5 precipitation at 4 sites)

**Primary code files:**
- `scripts/04_qc.R` — QC gating implementation
- `scripts/05_units.R` — unit conversion
- `R/qc.R` — QC helper functions
- `R/units.R` — unit conversion functions
- `R/pipeline_config.R` — QC threshold constants
- `docs/decisions_pending.md` — QC threshold decision rationale
- `docs/known_issues.md` — ERA5 anomalies, ONEFlux gap rule

**Key facts to include (update when finalised):**
- QC threshold: NEE_VUT_REF_QC ≥ 0.50 (annual and monthly)
- Unit convention: gC m⁻² yr⁻¹ for NEE/GPP/RECO, W m⁻² for LE/H
- ERA5 variables: TA_ERA (K→°C), P_ERA (mm), VPD_ERA (kPa)

---

## 5.4 Derived metrics and benchmark construction

**Purpose:** Explain how annual flux metrics, anomalies, and aggregated
summaries were calculated.

**Must cover:**
- Annual NEE, GPP, RECO, LE, H — source variables and any aggregation
- Anomaly calculation: deviation from long-term median per site
- Percentile position within long-term distribution (anomaly context figures)
- Recent period definition (2019–2024)
- Long-term baseline definition (all years before 2019)
- Minimum data requirements for anomaly analysis
  (≥8 valid NEE years, ≥4 sites per stratum)
- GEZ × IGBP × subregion stratification for anomaly figures
- Koppen-Geiger stratification (Level 1 and Level 2)

**Primary code files:**
- `R/figures/fig_anomaly_context.R` — anomaly calculation and plotting
- `R/figures/fig_network_growth.R` — site-year counting functions
- `data/snapshots/long_record_site_candidates_gez_kg.csv` — stratified
  site candidates
- `data/snapshots/site_year_data_presence.csv` — valid data years

**Key facts to include (update when finalised):**
- Recent anomaly period: 2019–2024
- Minimum record length for anomaly analysis: 8 valid NEE years
- Minimum sites per stratum: 4

---

## 5.5 External comparison datasets

**Purpose:** Document historical FLUXNET datasets and any remote sensing
or model outputs used for comparison.

**Must cover:**
- Marconi dataset (Falge et al. 2001): 35 sites, 97 site-years, 1992–2000
  Note: 3 of 38 original sites could not be matched to modern IDs
- La Thuile dataset (2007): 252 sites, 965 site-years, 1991–2007
- FLUXNET2015 (Pastorello et al. 2020): 212 sites, 1532 site-years,
  1991–2014
- How historical site lists were obtained and standardised
- How historical sites not in Shuttle were handled (fallback metadata)
- WorldClim v2.1 bioclimatic variables (Fick & Hijmans 2017):
  bio1 (MAT), bio12 (MAP), 2.5 arc-minute resolution, 1970–2000 baseline
- CGIAR Global Aridity Index v3.1 (Zomer et al. 2022):
  30 arc-second resolution, divide by 10000 for true AI values
- FAO Global Ecological Zones 2010 shapefile

**Primary code files:**
- `R/historical_datasets.R` — historical site list loading
- `R/external_data.R` — WorldClim, aridity index, GEZ loading
- `data/snapshots/sites_marconi_clean.csv`
- `data/snapshots/sites_la_thuile_clean.csv`
- `data/snapshots/sites_fluxnet2015_clean.csv`
- `data/snapshots/years_marconi.csv`
- `data/snapshots/years_la_thuile.csv`
- `data/snapshots/years_fluxnet2015.csv`
- `data/raw/Marconi_to_Modern_SiteIDs.xlsx` — site ID crosswalk

**Citations required:**
- Falge et al. (2001a,b) Agricultural and Forest Meteorology 107
- Pastorello et al. (2020) Scientific Data 7:225
- Fick & Hijmans (2017) International Journal of Climatology 37:4302
- Zomer et al. (2022) Scientific Data — doi:10.6084/m9.figshare.7504448

---

## 5.6 Statistical analysis and uncertainty estimation

**Purpose:** Document trend estimation, sensitivity analysis, and other
statistical methods.

**Must cover:** (to be defined when analyses are finalised)
- Trend estimation methods if used
- Uncertainty propagation
- Sensitivity to QC threshold choices
- Bootstrap or permutation methods if used

**Status:** Not yet implemented — placeholder for future analyses.

**Primary code files:** TBD

---

## 5.7 Reproducibility workflow

**Purpose:** Describe code organisation, versioning, and repository
structure for reproducing all figures and tables.

**Must cover:**
- Repository structure overview
- How to reproduce the full pipeline (scripts 01–07 in order)
- Dedicated figure generation scripts and their outputs
- renv for R package version locking
- Environment variables required (FLUXNET_DATA_ROOT, credentials)
- Codespace vs HPC workflow differences
- Data not included in repository (downloaded separately via Shuttle)
- Pipeline configuration constants in `R/pipeline_config.R`

**Primary code files:**
- `CLAUDE.md` — project context and workflow
- `README.md` (to be created)
- `R/pipeline_config.R` — all configurable constants
- `renv.lock` — locked R package versions
- `scripts/01_download.R` through `scripts/07_figures.R`
- `scripts/generate_whittaker.R`
- `scripts/generate_maps.R`
- `scripts/generate_duration_histograms.R`
- `.env.example` — required environment variables
- `docs/CODESPACE_SETUP.md` — Codespace setup instructions

---

## 6. Data availability statement

**Template (fill in DATE and PID when snapshot is archived):**

FLUXNET data products used in this work were downloaded from the FLUXNET
Shuttle on [DATE]. The persistent identifier for the snapshot used is
[PID]. All data are shared under the CC-BY-4.0 license. Attribution
requirements vary by contributing network; complete citation information
for each site, including persistent identifiers, is provided in the
DATA_POLICY_LICENSE_AND_INSTRUCTIONS file included with each site's data
download and is available through network-specific portals (AmeriFlux:
https://ameriflux.lbl.gov; ICOS Carbon Portal: https://data.icos-cp.eu;
OzFlux/TERN: https://data.ozflux.org.au). Users of the dataset described
in this paper are required to follow the attribution guidelines at
data.fluxnet.org and to cite each site individually.
https://data.fluxnet.org/data-policy-license-and-instructions-for-attribution/

---

## 7. Code availability statement

**Template (fill in DOI when repository is archived):**

All code used to produce the figures and analyses in this paper is
available at https://github.com/EcosystemEcologyLab/fluxnet-annual-2026
(DOI: [ZENODO DOI]). The repository includes all R scripts, figure
generation code, and pipeline configuration. Raw FLUXNET data are not
included but can be reproduced by running scripts/01_download.R with
valid FLUXNET Shuttle credentials against the snapshot PID specified
in the data availability statement above.

---

## Instructions for Claude Code — drafting methods text

When asked to draft methods text for a specific section:
1. Read all primary code files listed for that section
2. Extract actual parameter values, thresholds, and counts from the code
   and data snapshots — do not invent or approximate values
3. Cross-check key facts against `docs/known_issues.md` and
   `docs/decisions_pending.md`
4. Draft prose in past tense, third person, suitable for a Nature-family
   methods section
5. Flag any values marked "update when finalised" with [TBD] in the draft
6. Do not draft methods text unless explicitly asked — this document is
   a requirements spec, not a drafting prompt
