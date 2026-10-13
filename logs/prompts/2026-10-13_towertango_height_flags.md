TowerTango: remove the HEIGHTC statistic filter added in b0d32aa; card_canopy_height_max_m again takes the maximum numeric HEIGHTC across all records. Keep every value as recorded.

Add card_height_check (blank when nothing to check), listing any of: canopy height above 60 m; canopy height of 0 m; canopy height greater than card_tower_height_m; instrument height changed over the record (give the range). Add a section to docs/update_cards_report.md and a line in README.md telling users that heights are taken from BADM as reported and that flagged sites should be checked by hand before use, with the list of flagged sites.

Report coverage by data_hub for both height columns and the number of sites per flag. Re-run Update Cards. Save this prompt as logs/prompts/2026-10-13_towertango_height_flags.md. Log, commit and push per CLAUDE.md.
