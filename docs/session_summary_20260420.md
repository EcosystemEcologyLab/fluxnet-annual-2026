# Session Summary — 2026-04-20

## Canonical figure series built

Three complete figure series (9 variants each, 27 figures total) were built and committed
to `review/figures/`:

| Series | Subfolder | Variants | Description |
|--------|-----------|----------|-------------|
| Whit01–09 | `review/figures/whittaker/` | 9 | Whittaker biome diagram with site distribution |
| Map01–09 | `review/figures/maps/` | 9 | Global site map with IGBP colour coding |
| Dur01–09 | `review/figures/duration/` | 9 | Site-year duration histogram (672 sites) |

Priority candidates (8 figures with `.txt` description files) are in
`review/figures/candidates/`.

---

## New scripts

| Script | Purpose |
|--------|---------|
| `scripts/generate_whittaker.R` | Generates Whit01–09 via shared `WHITTAKER_STYLE` constants |
| `scripts/generate_maps.R` | Generates Map01–09 via shared `MAP_STYLE` constants |
| `scripts/generate_duration_histograms.R` | Generates Dur01–09 via shared `DUR_STYLE` constants |

All three scripts use a single core figure function per series with style constants
controlling font sizes, colours, legend placement, and axis formatting to ensure
cross-series consistency.

---

## New data files (committed to `data/snapshots/`)

| File | Description |
|------|-------------|
| `site_year_data_presence.csv` | Per-site per-year valid NEE month counts (from MM FLUXMET data); drives `is_functionally_active()` |
| `years_marconi.csv` | 35 Marconi Conference sites (2000), crosswalked to modern FLUXNET IDs |
| `years_la_thuile.csv` | 252 La Thuile synthesis sites (2007) |
| `years_fluxnet2015.csv` | 212 FLUXNET2015 sites (2015) |
| `site_worldclim.csv` | Per-site mean annual temperature and precipitation from WorldClim v2.1 2.5 min |
| `site_aridity.csv` | Per-site aridity index from CGIAR Aridity Index v3.1 |
| `site_gez_lookup.csv` | Per-site FAO Global Ecological Zone assignment (columns: `site_id`, `gez_name`, `gez_code`) |
| `site_candidates_full.csv` | Full candidate site table with IGBP, Koppen-Geiger, GEZ, climate, and active-status columns |

---

## Historical datasets integrated

| Dataset | Sites | Reference year | Source file |
|---------|-------|---------------|-------------|
| Marconi Conference | 35 | 2000 | `data/lists/Marconi_to_Modern_SiteIDs.xlsx` |
| La Thuile | 252 | 2007 | committed site list |
| FLUXNET2015 | 212 | 2015 | committed site list |
| Shuttle (current) | 672 | 2025 | live Shuttle snapshot |

---

## Key architectural decisions

- **Single core functions with shared style constants.** Each figure series has one
  generation function parameterised by a style list (`MAP_STYLE`, `DUR_STYLE`,
  `WHITTAKER_STYLE`). Variant differences (colour scheme, annotation level, year subset)
  are arguments, not separate functions.

- **Unicode and plotmath for superscripts.** Carbon flux axis labels use Unicode
  superscripts (e.g., `gC m⁻² yr⁻¹`) rather than `expression()` / `bquote()` to
  simplify label composition and ensure consistent rendering across output formats.

- **Descriptive filenames.** Figures use descriptive base names (e.g.,
  `whittaker_shuttle_igbp_labelled.png`) in addition to the canonical series names
  (Whit01–09) to make the candidates folder self-documenting.

- **Dedicated generation scripts.** Figure generation is split into three standalone
  scripts rather than consolidated in `07_figures.R`, keeping each script fast to
  re-run during review iteration.

---

## External data downloaded

| Dataset | Version | Size | Path |
|---------|---------|------|------|
| WorldClim v2.1 mean annual temperature and precipitation | 2.5 min resolution | 666 MB | `data/external/worldclim/` |
| CGIAR Aridity Index | v3.1 | 1.3 GB | `data/external/aridity/` |
| FAO Global Ecological Zones shapefile | GEZ 2010 | — | `data/external/gez/gez_2010_wgs84.shp` |

These files are gitignored. Re-download instructions are in `known_issues.md`
(Future enhancements — FAO GEZ shapefile) and inline in `scripts/07_figures.R`.

---

## Known issues flagged to FLUXNET team

| Issue | Count | Action |
|-------|-------|--------|
| Zero-NEE sites — ONEFlux 15-day gap rule | 106 sites | Reported to support@fluxnet.org; confirmed by Dario Papale (2026-04-16) as expected ONEFlux behaviour |
| NEE_CUT-only sites (no NEE_VUT_REF) | 36 sites | Documented in `outputs/sites_nee_cut_only.csv`; no fallback implemented pending co-author decision |
| `httr2` batch download errors | Batches 12–13 | Reported to support@fluxnet.org; resumable batch_download.R handles retries |

Full report text in `docs/shuttle_team_report_20260414.md`.

---

## Retrospective: 2026-04-14 — Pipeline infrastructure and critical bug fixes

This was a full pipeline reliability day. The 672-site full-network download was
completed, and several critical bugs were fixed before any figures were regenerated.

### Critical bug fixes

| Bug | Commit | Effect |
|-----|--------|--------|
| Unit conversion overcorrection | `31e653b` | Carbon at DD/MM/WW/YY is delivered pre-integrated by ONEFlux as gC m⁻² period⁻¹; previous code applied a ~800× µmol→gC conversion on top, producing physically impossible values (e.g. −194,325 gC m⁻² yr⁻¹). Fixed by reading native units from BIFVARINFO metadata and passing pre-integrated variables through unchanged. |
| QC gating too restrictive | `833ca41` | Row exclusion gated on all `_QC` columns, causing 347/672 sites (52%) to lose every annual record because a single poorly-covered secondary variable (RECO, GPP, LE, H) contaminated all years. Fixed to gate on `NEE_VUT_REF_QC` only. Valid sites rose from 325 → 530. |
| `03_read.R` memory and timeout | multiple | Rewrote to read one site at a time; BADM/varinfo cached; pre-save flush added; resumable (skips completed resolutions); BIF CSVs read directly, bypassing `flux_badm()`. |
| `04_qc.R` OOM on MM resolution | `5bd006a` | Switched to disk-based chunk accumulation; per-site processing to prevent memory spike on large resolutions. |

### New scripts and files created (2026-04-14)

| File | Description |
|------|-------------|
| `scripts/batch_download.R` | Resumable batch download with ZIP cleanup; `FLUXNET_DELETE_ZIPS` env option |
| `docs/known_issues.md` | Persistent issue tracker for upstream reporting to fluxnet-package and shuttle teams |
| `docs/shuttle_team_report_20260414.md` | Formal report to support@fluxnet.org on 106 zero-NEE sites and batch download failures |
| `data/snapshots/download_progress.csv` | Per-site download audit trail |
| `data/snapshots/fluxnet_shuttle_snapshot_*.csv` | Multiple snapshot CSVs from full-network download run (2026-04-14) |

### Configuration updates

- `FLUXNET_SHUTTLE_VERSION` updated to `0.3.7` in `R/pipeline_config.R` and `.env.example`
- `CLAUDE.md` updated with environment-aware autonomy rules (Codespace vs local/HPC)
- Contributor quickstart guide added (`docs/CODESPACE_SETUP.md`)

---

## Retrospective: 2026-04-15 — Stale figure cleanup

Removed stale PNGs from test runs predating 2026-04-14. Retained only the
full-network (672-site) review figures committed on 2026-04-14.

---

## Retrospective: 2026-04-16 — Figure development on corrected 530-site dataset

All review figures were regenerated on the QC-corrected 530-site dataset.
Several new figure types were introduced.

### New figures built

| Figure | Description |
|--------|-------------|
| ERA5 Whittaker hexbin | Runs in Codespace without WorldClim; ERA5 TA/P as climate axes |
| Choropleth map | UN subregions, 4 time snapshots (binned viridis colour scale); geometry fixed |
| Duration profile | Record length per site; `snapshot_year − first_year` calculation; two layout variants |
| Latency by UN subregion | Functionally active sites; traffic-light colour scheme; x-axis break at 75 years |
| Three-panel subregion overview | Map (full width) + site count bar + latency bar |
| Diagnostic timeseries by UN subregion | Top 5 sites per subregion; NEE/LE/H panels |

### Anomaly figure infrastructure (first pass)

- `R/figures/fig_anomaly_context.R` created with core anomaly function
- FAO GEZ lookup implemented; `data/snapshots/site_gez_lookup.csv` committed
- ENF and DBF Temperate North America figures generated as proof of concept
- `data/snapshots/long_record_site_candidates_gez.csv` committed

### Key decisions (2026-04-16)

- **ONEFlux 15-day gap rule confirmed by Dario Papale** (email 2026-04-16): the
  106 all-missing-NEE sites are expected behaviour, not a data delivery error.
  Documented in `docs/known_issues.md` and `docs/decisions_pending.md`.
- **White-background rule added to `CLAUDE.md`**: all `ggsave()` calls must pass
  `bg = "white"` unless explicitly producing overlay/compositing figures.
- **Review figures reorganised** into subfolders by type
  (anomalies, maps, duration, latitudinal, network, whittaker).

---

## Retrospective: 2026-04-17 — Anomaly and availability figure expansion

### Anomaly context figures

Generated for all qualifying GEZ × UN subregion × IGBP combinations meeting:
- ≥ 4 sites (tightened from ≥ 3 on 2026-04-18)
- ≥ 8 valid NEE years per site
- Data covering 2019–2024 (recent period)

Figures organised into `review/figures/anomalies/` with
`Anomalies_GEZ/` and `Anomalies_KG/` subfolders.

### Availability heatmaps

| Figure | Description |
|--------|-------------|
| Forest availability heatmap | UN subregion × GEZ; all IGBP classes with > 10 sites |
| KG availability heatmaps | Koppen-Geiger Level 1, 2, and 3 |
| KG anomaly figures | Level 1 and Level 2; all qualifying IGBP × subregion × KG combinations |

### New files created (2026-04-17)

| File | Description |
|------|-------------|
| `scripts/analysis_long_record_candidates.R` | Identifies qualifying stratified site sets |
| `scripts/generate_availability_heatmap.R` | Forest/IGBP availability heatmaps |
| `scripts/generate_gez_anomaly_figures.R` | Batch GEZ anomaly figure generation |
| `scripts/generate_kg_availability_heatmaps.R` | KG availability heatmaps |
| `scripts/generate_kg_anomaly_figures.R` | Batch KG anomaly figure generation |
| `data/snapshots/long_record_site_candidates_gez_kg.csv` | Stratified site candidates with GEZ and KG |

---

## Retrospective: 2026-04-18 — External data integration and candidate figure polish

### External datasets integrated

| Dataset | Script | Output snapshot |
|---------|--------|-----------------|
| WorldClim v2.1 (bio1 MAT, bio12 MAP; 2.5 arc-min) | `scripts/step1_extract_worldclim.R` | `data/snapshots/site_worldclim.csv` |
| CGIAR Aridity Index v3.1 (÷ 10000 for true AI) | `scripts/step2_extract_aridity.R` | `data/snapshots/site_aridity.csv` |
| FAO Global Ecological Zones (already joined) | `R/external_data.R` | `data/snapshots/site_gez_lookup.csv` |

`R/external_data.R` created with `load_worldclim()`, `load_aridity_index()`,
and GEZ loading functions.

`data/snapshots/site_candidates_full.csv` created — master site metadata table
joining IGBP, Koppen-Geiger, GEZ, WorldClim climate, aridity index, and
functionally-active status for all 672 sites.

### Historical dataset site lists committed

| File | Description |
|------|-------------|
| `data/snapshots/sites_la_thuile.csv` / `sites_la_thuile_clean.csv` | La Thuile (2007): 252 sites |
| `data/snapshots/sites_fluxnet2015.csv` / `sites_fluxnet2015_clean.csv` | FLUXNET2015: 212 sites |

### Candidate figure refinements (numbered figures for paper)

| Figure | Change |
|--------|--------|
| Fig 1 | Functionally active % added as second Y axis |
| Fig 4 | Shared colour scale and single legend across snapshot panels |
| Fig 5, 6 | Replaced with WorldClim versions; legend units and position fixed |
| Fig 7 | Panel titles removed; LE and H axis units corrected |
| Fig 8 | ERA5 environmental response; Kelvin → Celsius fix; outlier filtering; site-year observations used (not site means) |

### Anomaly figure redesign

Redesigned to use gradient ribbon (10th–90th percentile shading), individual
site-year points coloured by percentile position within long-term distribution,
and a median reference line. Consistent sites enforced across GEZ zones;
single shared legend at figure bottom.

### New scripts created (2026-04-18)

| Script | Description |
|--------|-------------|
| `scripts/step1_extract_worldclim.R` | WorldClim raster extraction at site coordinates |
| `scripts/step2_extract_aridity.R` | CGIAR aridity raster extraction |
| `scripts/generate_worldclim_figures.R` | Whittaker and environmental response with WorldClim axes |
| `scripts/generate_env_response_era5.R` | ERA5-based environmental response figures |

`review/figures/candidates/` established: 9 priority paper figures with
standardised names and `.txt` description files.

---

## Retrospective: 2026-04-19 — Historical comparison figures and functionally active definition

### Historical comparison figures

`R/historical_datasets.R` and `R/figures/fig_historical_comparison.R` created.
Figures compare Marconi (2000), La Thuile (2007), FLUXNET2015 (2015), and the
current Shuttle snapshot (2025) across choropleth, duration histogram, and
Whittaker panels. A Marconi-era (2000) snapshot panel added to all 4-panel
comparison figures.

### Marconi site crosswalk

`data/snapshots/sites_marconi_clean.csv` committed. 35 of 38 Marconi Conference
sites matched to modern FLUXNET IDs; 3 could not be matched and are excluded
from overlays.

### Functionally active definition updated

Changed from year-count threshold to: **≥ 3 months valid NEE in the last
4 years** (any of the 4 most recent years with ≥ 3 months of valid NEE data).
`data/snapshots/site_year_data_presence.csv` committed (per-site, per-year
valid NEE month counts derived from MM FLUXMET data).

`R/utils.R` updated: `is_functionally_active()` uses YY resolution as primary
source and MM as fallback for the ≥ 3-month check.

### Known issue documented

MM resolution data contains dual-dataset rows (both ERA5-only and full runs
for some sites). Documented in `docs/known_issues.md` Section 6; YY-primary
logic in `is_functionally_active()` avoids the ambiguity for most sites.

---

## Additions: 2026-04-20 (not in original session summary above)

In addition to the canonical figure series (Whit01–09, Map01–09, Dur01–09)
documented above, the following were built on 2026-04-20:

| Figure / File | Description |
|---------------|-------------|
| `fig_siteyears_by_year_igbp` | Annual stacked bar of site-years by IGBP class; overlays showing historical dataset coverage by year |
| `fig_cumulative_siteyears_igbp` | Cumulative stacked site-years by IGBP; historical dataset overlay lines |
| `R/figures/fig_climate_legacy.R` | Shared legacy climate-space figure functions extracted for reuse |
| `scripts/generate_historical_comparison_figures.R` | Standalone script for Whittaker + duration comparison across historical snapshots |

**Candidate update:** `dur08` (1-year-bin duration histogram with historical overlays)
replaced `fig_02` in `review/figures/candidates/`; `dur11` (site-years by year,
IGBP stacked) added as `fig_02b`.

**Legacy script cleanup:** deprecated generation scripts moved to `scripts/legacy/`;
canonical series scripts (`generate_whittaker.R`, `generate_maps.R`,
`generate_duration_histograms.R`) retained at `scripts/`.

---

## Session: 2026-04-21

| File | Description |
|------|-------------|
| `docs/methods_requirements.md` | Methods section requirements specification for the paper. Defines what each of sections 5.1–5.7 must cover, lists the primary code files to read when drafting, and records key numerical facts (site counts, thresholds, versions) alongside placeholder `[TBD]` markers for values not yet finalised. Not a draft — used later to auto-draft methods prose from the codebase. |
