TowerTango: tower height from VAR_INFO, and fix canopy height. Change only TowerTango/. Use `command grep` or R reads, never plain grep.

- card_tower_height_m: VAR_INFO_HEIGHT from each site's BIFVARINFO_YY file (the file for the snapshot's product, one per site via flux_discover_files()), for NEE_VUT_REF, else NEE_CUT_REF, else USTAR, using the latest VAR_INFO_DATE. Add card_tower_height_date, card_tower_height_variable and card_instrument_model. Replace the comment-field values. Spot-check CA-TP1 10 m, US-Ha1 29 m, DE-Tha 42 m.
- card_canopy_height_max_m: use only HEIGHTC records whose HEIGHTC_STATISTIC is Mean, Median or Expert estimate, or is absent; exclude standard deviation and other dispersion statistics. Report how many records and sites this removes, and re-list values above 60 m and at 0 m.
- Report coverage by data_hub for both columns. Re-run Update Cards and update docs/update_cards_report.md.

Save this prompt as logs/prompts/2026-10-13_towertango_heights.md. Log, commit and push per CLAUDE.md.
