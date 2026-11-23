RSCOTT_AGU26: one site map for Russ Scott's package (v2, 6ef23d5). No change to any data or selection.

Map the 54 kept sites in package_rscott_2026-11-23_v2/sites.csv:
- Panel (a): world map. Panel (b): zoom on the western United States, where most sites cluster.
- One symbol shape per IGBP class (CSH, GRA, OSH, SAV, WSA), filled in the paper's IGBP colours from CLAUDE.md, with a legend.
- Plain grey land, no country labels, no site labels except where symbols would otherwise hide each other in panel (b).
- Labelled DRAFT, PNG at 300 dpi plus PDF, with a .legend.txt caption (plain English: what is shown, the selection rules in one sentence, the 20 September 2026 snapshot) and a .meta.json.

Write to RSCOTT_AGU26/figures/site_map_igbp_DRAFT.* and copy it into package_rscott_2026-11-23_v2/; add it to the package README's file list and rebuild the zip. Open the PNG before committing and check that every site is visible and no symbols or labels overlap; say what you checked.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_rscott_site_map.md. Log, commit and push per CLAUDE.md.
