GSL_definition/expanded: fixes to the methods review figures (commit a4880b2). Revise code/12_methods_review_figures.R in place and regenerate only figures/drafts/methods_review/ and its report. Change nothing else. Same conventions as the first run.

1. Skewness in figures A and D (and anywhere else it is printed or used for sorting or colour): take it from the unscreened NT series (derived_site.csv variant main_unscreened), not the winter-screened main variant, which is no longer used. Report any site whose order or colour changes.
2. Figure B:
   - Remove the horizontal bars for 10% of P99 (Z16), 85% and 95% of annual GPP; they run from the first to the last qualifying day and hide the raster.
   - Instead, under each year's raster row, add three thin strips (one per labelled choice, in the same colours), each marking the individual days counted as in season under that choice. The strips must not cover the raster.
   - Histograms: x-axis from 0 to 120% of P99 for every site, with a note of how many days lie above 120%. Keep the threshold range shaded and the three labelled choices as vertical lines.
3. Figure C: label each method row with its name and the variant plotted (for example "M1 span, summer P99", "M2, 10% of P99", "M3, 1 gC m-2 d-1", "M5, NEP > 0 span", "M6, Hu, anchored", "M7, Ma", "M8, Panwar 0.25"). Show M4 as two rows, 85% and 95% of annual GPP.

Before committing, open each regenerated PNG and check: in B, that the raster is visible at every site and the strips show individual counted days; in C, that every row label is readable and no labels overlap. Say in the report what you checked.

Update docs/methods_review_figures_<date>.md: what changed, and the numbers behind each figure if they moved.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_methods_review_figures_fix.md. Log, commit and push per CLAUDE.md.
