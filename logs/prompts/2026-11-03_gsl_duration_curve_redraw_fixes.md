GSL_definition: two replacement figures for the all-sites duration curve (Figure P1b), plus fixes to the 3 November duration curve outputs (commit cc1809b). Drafts for discussion with Tash. Same main run as cc1809b (Tash's 62 sites, GPP_NT_VUT_REF, negative daily GPP set to 0, calendar years, no winter screen). Extend code/12_duration_curve_and_target_sweep.R; build from the existing tables/duration_curve_crossings.csv, tables/skew_predictors.csv and the cached fine grid. No new data processing. Keep Figure P1a and P1b as they are. Local write_meta(), DuckDB read-only, FLUXMET rows only.

Why P1b is being replaced: 62 overlapping lines, unlabelled dots, and a skewness colour scale that does not order the curves, because the two kinds of skew push curves in opposite directions (a short burst after many zero days gives a steep curve near the top; many low non-zero days give a low, flat curve).

1. Figure P1c (exemplar duration curves). One panel, the same axes as P1b.
   - Five exemplar sites in colour: AU-How (skewness 0.36, almost no near-zero days), US-MMS (0.58, a third of days near zero), US-Wkg (2.07, many near-zero days, then a short monsoon burst), US-SRG (1.92, long low tail) and AU-ASM (3.88, few near-zero days, long low tail). All other sites as thin light grey lines behind them.
   - Use colours that do not imply an ordered scale. Label each exemplar directly at the end of its line with site and skewness; no colour legend.
   - On each exemplar curve, thicken the stretch covered by Tash's 22-value grid (0.1 to 20% of P99), and mark Z16 (10% of P99), 85% and 95% of annual GPP with distinct marker shapes, explained in one small legend.
   - Caption, in plain words: each site's days are ranked from highest to lowest GPP; x is the number of days counted per year, y the share of annual GPP on those days, both averaged over years; a curve stops short of 365 where the remaining days have zero GPP.
2. Figure P1d (one point per site), two panels sharing the x-axis (skewness):
   - (a) Tash's sensitivity measure (the interquartile range of mean GSL across her 22 thresholds) against skewness.
   - (b) the share of annual GPP inside the Z16 season against skewness.
   - Colour points in both panels by the share of days below 1% of P99, on a continuous scale with a plain-words legend title ("share of days with near-zero GPP"). Label the five exemplar sites so the two figures connect.
   - Add panel (c), panel (a) repeated against the skewness of days at or above 1% of P99, only if it reads clearly; say in the report whether you included it and why.
3. Fixes to the cc1809b outputs:
   - Figure P2 caption: the line key is reversed. Solid is the fit with AU-ASM, dashed without; correct it. In panel (a), draw the quadratic fits instead of straight lines, because Tash's published adjusted R2 comes from the quadratic. In panel (b), move the legend so it no longer covers the lines and bands at target shares 0.80 to 0.83.
   - Report wording (docs/duration_curve_and_target_sweep_2026-11-03.md):
     - Section 2: the quadratic match confirms the reproduction, since Tash's Figure 4 already uses a quadratic; it is not new. State instead that without AU-ASM the quadratic term is not significant at target shares 0.80 to 0.91, and give the AIC comparison.
     - Section 1: AU-How is not a case where Z16 and the 95% rule agree (Z16 gives 362 days, 0.95 gives 317.5, 0.85 gives 258). Both sites diverge, in opposite directions.
     - Section 1: add how many sites select a threshold above 20% of P99, the top of Tash's grid, at 0.85 and at 0.95.
     - Section 3: zero-GPP days add nothing to annual GPP and are never counted at any threshold above zero, so near-zero share cannot drive threshold choice by construction. Partial skewness predicting better than full skewness is therefore consistent with Dave's point (dormant zero days dilute full skewness), not a rejection of it. Rewrite the conclusion accordingly.
     - Section 1 and the P1b caption: replace the statement that high-skew curves sit lower; the ordering by skewness is not monotone (see why P1b is being replaced).

Figures in figures/drafts/proposal/, labelled "Draft for discussion", PeerJ format (300 dpi, text at least 8 pt, panel letters only on multi-panel figures), each with a .legend.txt journal caption (two to four sentences, plain English, no repository paths or variable names) and a .meta.json. Before committing, open every new and changed PNG at full size and check: exemplar labels do not overlap each other or the curves, the grid highlight and markers are distinct, no legend covers data, and axis labels use plain words. Say in the report what you checked.

Add a short section to the report on the new figures and the fixes, with file names and anything unexpected.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_duration_curve_redraw_fixes.md. Log, commit and push per CLAUDE.md.
