Create a new side project, TowerTango, a card game built on site information. Set it up like WUE/isotope_pilot: TowerTango/ at the repo root with README.md, code/, docs/ and tables/. The README says it is not part of the Annual Paper, and that nothing in it writes outside TowerTango/ except SESSION_LOG.md. Reads from data/snapshots/, the local flux store and the BADM files are read-only.

Script: TowerTango/code/update_cards.R ("Update Cards"). It must be re-runnable whenever a new snapshot is downloaded.
- Use the latest snapshot already on disk; never download. Select it by the repository's latest-snapshot rule (docs/decisions_pending.md, 2026-10-12), not by "newest file in the folder", which once picked up a 775-site development listing. Print the file used. Gate: one row per site, site_id unique.
- Output: TowerTango/tables/towertango_cards_<snapshot timestamp>.csv. All snapshot columns unchanged and in order, then the card columns below, plus a .meta.json listing every source file.

Card columns, appended in this order:
- card_site_name: snapshot site_name.
- card_regional_network: snapshot network field, verbatim (may list several).
- card_site_id: site_id.
- card_year_established: first year with data under compute_site_year_presence().
- card_sink_strength_nep_gC_m2_yr: median of usable annual NEP (NEP = -NEE), using compute_site_annual_fluxes() (VUT with CUT fallback, the paper's QC rule). Also card_nep_n_years. Blank where no usable year.
- card_et_mm_yr and card_et_mm_d: median of usable annual ET from LE, using the same LE-to-ET conversion as the paper's ET outputs (flux_medians_by_igbp_et). mm d-1 = mm yr-1 / 365.25. Also card_et_n_years.
- card_tower_height_m: measurement height of the eddy covariance instrument from BADM (the flux variable's instrument height, not canopy height). If several heights or dates, use the most recent and record the BADM field used in card_tower_height_source. Blank if absent.
- card_map_mm_yr and card_mat_degC: WorldClim 2.1 (bio12, bio1) from data/snapshots/site_worldclim.csv. That file was built on an April snapshot, so for any site missing from it, extract from the local WorldClim raster if it is on disk; otherwise leave blank. Do not download.
- card_site_description: BADM SITE_DESC verbatim, using the same sources as the 8 October BADM search. Where a site has several entries, keep all, joined with " | " in BADM order. Also card_site_description_words.

Checks, after one run:
- Coverage table: non-blank count for each card column, overall and by data_hub.
- SITE_DESC word counts: minimum, quartiles, maximum, and the number of sites under 50, 50 to 150, and over 150 words. Also the number of sites with several entries, and whether those are dated versions or separate pieces.
- Spot-check three sites from different hubs by recomputing NEP, ET and tower height by hand from the source files, and show the numbers side by side.
- Any value outside a plausible range: NEP beyond ±1,500, ET above 2,000 mm yr-1, tower height above 100 m. List these; do not drop them.

Put the checks in TowerTango/docs/update_cards_report.md. Save this prompt as logs/prompts/2026-10-12_towertango_update_cards.md. Log, commit (including the card CSV) and push per CLAUDE.md.
