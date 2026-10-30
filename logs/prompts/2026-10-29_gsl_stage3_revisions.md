GSL_definition, stage 3 revisions. Fix the following, rerun the affected scripts, inspect every figure at full size, and update the .legend.txt files and the report-back.

1. Figure 3a, possible bug: at CH-Dav the Z17 (10% of P99) dot lies outside the coloured bar for the range of trends across 5 to 20% of P99, which should contain it. Find the cause: grid, screening, trend function, or which years are used. Check every site for the same problem with a stopifnot(). Fix it, and report whether the 11-of-21 count changes.
2. Figure 1:
   - Choose the high-third example as the site nearest the high-third median skewness among woody-grass mosaic and grassland sites, because the takeaway concerns drylands. Never AU-ASM. Record the rule change in the methods memo. Keep JP-Fjy for the low third unless the rule changes it.
   - Name each site (and its skewness) on its row in panel a.
   - Draw T95 in a distinct line style from the 5, 10 and 20% lines.
   - Add the change in mean GSL between 5% and 10% of P99 as the framework specifies: either in the .legend.txt and as a small labelled bracket on panel b, or say why not.
   - Panel b: extend the x-axis to cover the full distribution (at least to 100% of P99), so the densities are not cut off, and label the y-axis "Density".
3. Use one marker convention everywhere: T95 = red triangle, Z17 = black circle, in Figures 2b, 2c, 3a and 4c, with a proper legend. Remove the floating "T95" and "Z17" text from Figure 2c.
4. Figure 2:
   - Remove the vertical stripes at low thresholds (tile widths or gaps on the log axis).
   - Plot sensitivity as days lost per 1% of P99 (absolute value), with darker meaning more sensitive.
   - Smooth the bottom panel (a running median over about 0.1 log units, or bin the fine grid) and label it as a panel.
5. Skewness colour scale: cap or rescale it so the 0.2 to 1.5 range, where most sites sit, is distinguishable (for example, cap at 2 and mark the higher sites as ">2"). Use the same scale in every figure.
6. Remove "mixed forest" from legends where no site of that group is plotted. Keep legends inside the plotting area without cutting panel borders (Figure 4a, 4c).

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_stage3_revisions.md. Commit and push per CLAUDE.md.
