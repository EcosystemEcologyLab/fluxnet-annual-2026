# Figure Inventory — number to file map

The single map from manuscript figure/table number to its file in `draft_manuscript_v1/`.
For build provenance (generating script, panel-by-panel description, data sources,
renumbering history), see `docs/figure_inventory.md` — this file is deliberately short: a
lookup table, not a narrative. Last renumbered 2026-10-12
(`logs/prompts/2026-10-12_renumber_figures.md`).

## Main figures

| # | File (`draft_manuscript_v1/`) | Description |
|---|---|---|
| 1 | `fig_01_map_network.png`/`.pdf` | Global network map, five panels. |
| 2 | `fig_02_cumulative_siteyears_igbp.png`/`.pdf` | Cumulative site-years by IGBP class, with a framed record-length inset. 120 mm wide. |
| 3 | `fig_03_whittaker_current.png`/`.pdf` | Whittaker climate-space hexbin, current network. |
| 4 | `fig_04_flux_comparison_combo_nep_et_h.png`/`.pdf` | Per-IGBP median NEP/ET/H, FLUXNET2015 vs. the snapshot. |
| 5 | — | Not built. |
| 6 | — | Not built. |
| 7 | `fig_07_network_sampling.png`/`.pdf` | Six-panel network sampling (Geo vs Data). Was `fig_05_representativeness.png`. |

## Supplementary figures (`draft_manuscript_v1/SupFigs/`)

| # | File | Description |
|---|---|---|
| S1 | `figS1_record_length_collections.png`/`.pdf`/`.jpg` | Share of sites with >= n years of data, four collections (Marconi, La Thuile, FLUXNET2015, the snapshot). |
| S2 | `figS2_whittaker_nee_gpp_reco.png`/`.pdf`/`.jpg` | Whittaker climate-space, NEE/GPP/RECO. |
| S3 | `figS3_flux_comparison_matched_siteyears.png`/`.pdf`/`.jpg` | FLUXNET2015 vs. the snapshot, matched site-years. |
| S4 | — | Reserved for the round robin. Not built. |
| S5 | `figS5_quality_flags.png`/`.pdf` | Quality flags by variable and time step (daily/monthly ECDFs + sub-daily shares at 31 sites). |
| S6 | `figS6_availability_annual_nee.png`/`.pdf` | Availability of annual NEE by usability category. |
| S7 | `figS7_sampling_gridded_at_tower.png`/`.pdf`/`.jpg` | Network sampling, gridded value at the tower (Geo vs Geo). Companion to Figure 7. |
| S8 | `figS8_sampling_flux_axes.png`/`.pdf`/`.jpg` | Network sampling, four fluxes (NEE/GPP/RECO/ET). |
| S9 | `figS9_sampling_collections.png`/`.pdf`/`.jpg` | Network sampling (Jaccard) through time, four network generations. |

## Supplementary tables (`draft_manuscript_v1/SupTables/`)

| # | File | Description |
|---|---|---|
| S1 | `tableS1_regional_networks.csv` | Regional networks. |
| S2 | `tableS2_record_length_by_igbp.csv` | Record length by IGBP class. |
| S3 | `tableS3_quality_uncertainty_coverage.csv` | Quality and uncertainty variables by time step. |
| S4 | `tableS4_bowen_ratio_by_igbp.csv` | Bowen ratio by IGBP class. |

Unnumbered supplementary data: `supplementary_data_1_sites.csv`, `supplementary_data_2_sampling_ratios.csv`.

## Deprecated / alternatives

Superseded files live in each directory's own `deprecated/` subfolder (`draft_manuscript_v1/deprecated/`,
`SupFigs/deprecated/`, `SupTables/deprecated/`), not deleted, per CLAUDE.md. The pool of candidate
figures and per-category alternatives (network growth, Whittaker variants, representativeness
panels, maps, timeseries, etc.) that fed earlier drafts of the figures above lives in the other
subdirectories of `review/figures/` (`candidates/`, `network/`, `whittaker/`, `flux_medians/`,
`representativeness/`, `maps/`, `maps_point/`, `historical/`, `timeseries/`, `envresponse/`,
`climate/`, `anomalies/`, `Anomalies_GEZ/`, `Anomalies_KG/`, `diagnostic/`, `latitudinal/`) — not
indexed here figure-by-figure; see each generating script for its own outputs.
