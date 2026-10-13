TowerTango: fill card_tower_height_m from the variable-information files. Change only TowerTango/.

- Source: each site's *_FLUXNET_BIFVARINFO_YY_*.csv (VAR_INFO groups: VAR_INFO_VARNAME, VAR_INFO_DATE, VAR_INFO_HEIGHT, VAR_INFO_MODEL), not the BIF file.
- Height is VAR_INFO_HEIGHT for NEE_VUT_REF, else NEE_CUT_REF, else USTAR, using the group with the latest VAR_INFO_DATE. Add card_tower_height_date, card_tower_height_variable and card_instrument_model; replace the old comment-field values.
- Report coverage by data_hub, the number of sites whose height changed over time, and any value above 100 m or below 1 m. Spot-check CA-TP1 (10 m from 2014), US-Ha1 (29 m) and DE-Tha (42 m).
- Keep the canopy height columns if that prompt has run; otherwise skip them.
- Re-run Update Cards and update docs/update_cards_report.md.

Save this prompt as logs/prompts/2026-10-13_towertango_tower_height.md. Log, commit and push per CLAUDE.md.
