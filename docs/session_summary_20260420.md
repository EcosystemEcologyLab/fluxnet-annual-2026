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
