TowerTango: add canopy height to the cards. Change only TowerTango/; keep everything else in update_cards.R as is.

- New columns after card_tower_height_source: card_canopy_height_max_m (the maximum HEIGHTC value across all of a site's dated canopy height records in the BIF files, GRP_HEIGHTC/HEIGHTC, including the non-standard JP-Shn group noted in the 8 October BADM search report), card_canopy_height_date (date of that record) and card_canopy_height_n_records.
- Keep card_tower_height_m unchanged.
- Report coverage by data_hub and IGBP class (the 8 October search found 482 sites), and list values above 60 m or below 0.05 m without dropping them. Spot-check three sites from different hubs against the raw BIF records.
- Re-run Update Cards on the latest snapshot and update docs/update_cards_report.md.

Save this prompt as logs/prompts/2026-10-13_towertango_canopy_height.md. Log, commit and push per CLAUDE.md.
