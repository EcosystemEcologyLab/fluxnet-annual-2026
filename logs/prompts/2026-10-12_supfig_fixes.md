Small fixes to three supplementary figures from commit b778437. Same names, gates and format checks.

- S1 record_length_collections: move the key inside the plot's empty upper right and let the plot use the full width. Key label "Snapshot (n=781)", not "the snapshot".
- S5 quality_flags: order variables NEE_VUT, NEE_CUT, LE, H in both panels. Legend text states the QC fraction's direction (1 = all records measured or good-quality gap-filled) and that the dashed line is the 0.5 rule.
- S6 availability_annual_nee: legend keys about text height; category label "no annual NEE in the product" in place of "no reported value". Legend text defines the site bar: a site is "both usable" if at least one year is usable under VUT and at least one under CUT (not necessarily the same year), and "below quality rule" has 0 sites because no site has values that all fail the rule.

Save this prompt as logs/prompts/2026-10-12_supfig_fixes.md. Log, commit and push per CLAUDE.md.
