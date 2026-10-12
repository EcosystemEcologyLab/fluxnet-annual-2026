Renumber the manuscript figures and supplement to the new citation order, and rebuild Figure 2 and the record-length supplementary figure. Figures and their text files only: do not build or edit any Word document.

Figure 2
- Rebuild to match Dave's edited version, review/figures/candidates/fig_02alt_mockup_dave_02.png: framed inset in the upper left, single-column IGBP key inside the inset, larger text. 120 x 120 mm, text no larger than 7 pt, inset frame at the main panel's frame line weight.
- Remove all three release-year dashed lines (2000, 2007, 2015).
- Marconi line and label in dark grey (#404040). La Thuile stays red, FLUXNET2015 blue.
- Keep the inset's 5, 10, 20 year lines and labels, clear of all bars. Keep the rev2 legend fixes (key alpha matches fills, keys about text height).
- Move the 5 October fig_02 files in draft_manuscript_v1 to deprecated/, then write the rebuild there as fig_02_cumulative_siteyears_igbp. Mark the candidates/fig_02alt_* outputs as superseded in their legend text.
- Legend text: no release-year sentence; "the snapshot", not "the current snapshot"; describe the inset.
- Gates: 6,200 site-years, 781 sites, 483/225/63.

Supplementary figures (SupFigs/), new number, source:
- S1 record_length_collections: one panel, the share of sites with at least n years for Marconi, La Thuile, FLUXNET2015 and the snapshot (old figS6 panel b only). Collection colours as in Figure 2; snapshot black at a heavier line weight so it is distinct from Marconi. Keep the caveat that earlier collections count years from their published site tables. Gates: 96, 965, 1,532.
- S2 whittaker_nee_gpp_reco (old S1).
- S3 flux_comparison_matched_siteyears (old S2). Legend points to Section 2.6, not 2.5.
- S4 reserved for the round robin. Build nothing.
- S5 quality_flags: from review/technical_validation_interim fig_tv1, daily and monthly panels only (annual goes to Figure 6), plus the sub-daily measured and gap-filled shares at the 31 sites. State in the legend how many of the 31 are ICOS sites, computed, and that US-MMS is hourly from the May extraction.
- S6 availability_annual_nee: from fig_tv4, without the u* method-success panel. Describe "no reported value" as no annual NEE in the product. State which site-year count the figure uses (6,336 annual rows) and how it differs from the 6,200 site-years in Figure 2.
- S7 sampling_gridded_at_tower (old S3), S8 sampling_flux_axes (old S4), S9 sampling_collections (old S5).

Supplementary tables (SupTables/)
- S1 regional networks and S2 record length by IGBP keep their numbers.
- S3 quality and uncertainty variables by time step: from table_tv1_column_coverage_by_site.csv. Mark variables absent from the product as "not provided", not blank or zero.
- S4 Bowen ratio by IGBP (old S3).
- Supplementary Data 1 and 2 unchanged.

Main figures
- fig_05_representativeness becomes fig_07_network_sampling. Title Figure 7, "network sampling" in place of "representativeness" in its legend and meta.json, and its supplementary references updated (S7, S8, S9).
- Figures 5 and 6 are not built in this run.

Mechanics
- Change output names in the generating scripts, not only the files, so regeneration writes the new names. Build to new names in one pass; never rename one numbered file onto another that still exists.
- Update legend texts, meta.json, build_supplementary_pdf.R (skip S4) and review/figures/FIGURE_INVENTORY.md, which becomes the single map from number to file.
- Search live outputs, scripts and FIGURE_INVENTORY.md for old numbers and names (fig_05, figS1 to figS6, Table S3 as Bowen ratio, "representativeness"). Leave SESSION_LOG.md, logs/, deprecated/ and earlier review reports unchanged.
- Remove the duplicate SupFigs/deprecated/fig_02_historical_only.jpg; the presentation copies are already in review/figures/presentation_figures/.
- Run check_figure_format.R on every rebuilt figure, and check Figure 2's legend colours and label clearances by measurement, as in rev2.

Report: the final number-to-file map, any count that changed, and anything not done. Save this prompt as logs/prompts/2026-10-12_renumber_figures.md. Log, commit and push per CLAUDE.md.
