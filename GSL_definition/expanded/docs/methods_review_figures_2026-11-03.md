# GSL methods review figures — 2026-11-03

Draft figures for Dave and Tash to discuss, built by `GSL_definition/expanded/code/12_methods_review_figures.R`
from version 4's methods-comparison outputs (commit `4965af6`) plus one fresh daily-data computation
for figure B's AU-ASM panel (AU-ASM is not one of the 15 comparison sites). Does not change
`11_gsl_methods_compare.R` or any of its tables/figures. NT series throughout. Written to
`figures/drafts/methods_review/`, each titled "Draft for discussion".

Framing, per Tash: these figures show a *range* of thresholds and published methods, never a
two-way comparison. Three common choices are marked inside that range — 10% of P99 (Z16), 85% of
annual GPP, and 95% of annual GPP — because Tash's own preferred target (a fraction of annual GPP,
M4) is not yet settled between 85% and 95%. Colour is reserved for skewness, on a continuous scale,
and is never used for vegetation/forest-vs-open grouping anywhere in this figure set.

## Revision (2026-11-03, same day): three fixes requested after the first pass

Three changes, applied to `code/12_methods_review_figures.R` in place (first pass stays in git
history at `a4880b2`); nothing outside `figures/drafts/methods_review/` and this report changed.

1. **Skewness source.** Figures A and D (and everywhere skewness sorts or colours a figure) now use
   the unscreened NT series (`derived_site.csv`, variant `main_unscreened`) instead of the
   winter-screened `main` variant, which this script no longer uses anywhere else. Only two sites
   swap rank order: **US-MMS and CH-Dav** (US-MMS: 0.600 → ranks just below CH-Dav: 0.620, where
   under the old `main` variant CH-Dav, 0.583, ranked just below US-MMS, 0.598). Five sites (AU-How,
   IL-Yat, US-Ton, US-Var, US-SRM) have identical skewness under both variants (no days are
   winter-screened at those sites), so their position/colour is unchanged. Every other site's
   skewness value shifts by a small amount (≤0.04) with no further order changes.
2. **Figure B.** The first-to-last-qualifying-day brackets for the three labelled choices were
   removed — they ran across nearly the whole in-season span and hid the raster underneath. In their
   place, three thin strips now sit *below* each year's raster row (not overlapping it — the raster
   occupies the top half of the row, the three strips the bottom half, each geometrically
   non-overlapping by construction), one per labelled choice, with a tick for every individual day
   counted in-season under that choice. While building this, the Z16 choice's colour (previously
   black) was also changed to blue: a black strip was visually indistinguishable from the raster's
   own black ("in season at every threshold") shading directly above it. Each histogram's x-axis is
   now fixed at 0–120% of P99 for every site, with an in-panel note of how many days exceed 120% (not
   plotted); the threshold-range shading and the three labelled-choice vertical lines are unchanged.
3. **Figure C.** Every method row is now labelled with its name AND the variant plotted (e.g. "M1
   span, summer P99", "M6, Hu, anchored", "M7, Ma", "M8, Panwar 0.25") instead of a bare "M1".."M8".
   M4 is now shown as two separate rows, 85% and 95% of annual GPP, instead of one row at its 0.85
   headline — 9 rows total, not 8.

## Figure A — range of all 8 methods, 15 sites

`Figure A.png` (calendar year_rule, main view) and `Figure A (min_gpp_month companion).png`. Each row
is one site, sorted bottom-to-top by ascending skewness (unscreened NT series, printed and coloured
continuously at left, from AU-How at 0.37 to US-SRM at 2.41 — see the revision note above for the two
sites whose rank order moved). The thin grey bar spans each site's full cross-method range — from
US-Wkg's narrowest (75–225 d) to AU-How's widest under the calendar rule (226–365 d, the 365 d
ceiling itself a sign of the calendar-split artifact discussed in Figure C/D below). The light blue
band is M2's full 22-value %-of-P99 grid. Small shapes mark one point per method, shaped by method
family (circle = threshold count, M2/M3; triangle = fraction of annual GPP, M4; square = span of
smoothed GPP, M1; diamond = NEP-based, M5/M6; asterisk = derivative/normalized, M7/M8); the three
labelled choices are drawn larger, in blue (Z16)/green (85%)/orange (95%). Every method is visually
distinguishable at every site, and the three labelled choices stand out clearly from the smaller
family markers.

The companion figure repeats this under the min_gpp_month rule. It is identical to the calendar-rule
figure at the 9 sites whose lowest-GPP month is already January (NL-Loo, DE-Tha, CH-Dav, US-MMS,
US-Ha1, JP-Api, US-Wkg, US-SRM, US-Mpj — the min_gpp_month window reduces to the calendar window at
those sites) and differs at the 6 sites whose lowest-GPP month is not January: AU-How (Aug), FR-Pue
(Dec), AT-Neu (Feb), IL-Yat (Aug), US-Ton (Oct), US-Var (Aug). The clearest change is at US-Var: its
grey-bar range collapses from 109–365 d (calendar) to a much tighter band under min_gpp_month, and
its M4@0.85/0.95 markers move from straddling the January boundary (overlapping, hard to read as two
separate choices) to cleanly separated points — the clearest single illustration, among the 15 sites,
of why the calendar split matters.

## Figure B — threshold-dependence raster + histogram

`Figure B (AU-ASM vs AU-How).png` and `Figure B (US-SRM vs AU-How).png`, both calendar year_rule.
Each is a 2×2 grid: top row = the paired dryland/savanna site, bottom row = AU-How; left column = the
day (x) × year (y) raster (occupying the top half of each year's row), shaded by the share of Tash's
22 %-of-P99 thresholds at which that day's NT GPP counts as in-season (white = none of the 22, black
= all 22); below the raster, in the bottom half of each row and never overlapping it, three thin
strips — one per labelled choice (blue = Z16, green = 85% of annual GPP, orange = 95%) — mark the
*individual days* counted in-season under that choice (a tick per day, not a first-to-last-day
bracket). Right column = the histogram of daily NT GPP as % of P99, x-axis fixed at 0–120% of P99 for
every site, with the 22-threshold grid's own range (0.1–20% of P99) shaded, the three labelled choices
as vertical lines, and an in-panel count of days above 120% (not plotted).

AU-ASM is not one of the 15 sites; it is computed fresh here from its own passing years under the
same rules (P99 = 5.22 gC m⁻² d⁻¹; M4 thresholds 85%/95% of annual GPP = 7.31%/4.92% of P99; 4 days
above 120% of P99). Its raster — and now, even more clearly, its three day-tick strips — shows rapid
day-to-day flickering between near-zero and well-above-threshold GPP, consistent with a
pulse-rainfall-driven savanna: the Z16/85%/95% strips are dense, broken bands with gaps throughout
the wet season, visually the clearest answer in this figure set to "which days depend on the
threshold." AU-How's strips (P99 = 11.70 gC m⁻² d⁻¹; M4 thresholds = 33.74%/24.84% of P99; 7 days
above 120%), by contrast, are solid, near-continuous bands across most of the wet season, with gaps
only at the shoulders — in-season status there is far less threshold-sensitive day-to-day, even
though the three strips sit at visibly different widths (direct evidence for why AU-How's
cross-method range is so large in Figures A/D despite its low skewness). US-SRM (P99 = 5.98 gC m⁻²
d⁻¹; M4 thresholds = 10.52%/5.24% of P99; 27 days above 120%) shows a single, mostly solid
monsoon-season strip block (roughly May–October) each year, with the Z16 and 85% strips closely
overlapping — much less threshold-sensitive than AU-ASM despite a similarly low P99.

## Figure C — where each method puts the season

`Figure C.png`: (a) US-Ha1, (b) US-Wkg, (c) US-Var, (d) AU-How, all calendar year_rule, plus (e)
US-Var again under min_gpp_month. Each panel's top shows median daily NT GPP across years (black
line) with the interquartile range across years (grey ribbon); the bottom shows one bar per
method-and-variant row (9 rows, not 8 — M4 is now shown twice, "M4, 85% of annual GPP" and "M4, 95%
of annual GPP" — each labelled with its method name and the variant plotted, e.g. "M1 span, summer
P99", "M5, NEP > 0 span", "M6, Hu, anchored", "M7, Ma", "M8, Panwar 0.25") from its median start to
median end across years, with thin whiskers for the interquartile range of start/end. Count methods
(M2/M3/M4) use the 5-day-run start/end rule (`start_date_run5`/`end_date_run5`), not the
first/last-qualifying-day rule, as stated in the panel caption.

Panel (c) is the clearest illustration in this whole figure set of the calendar-split artifact:
US-Var's count methods (M1/M2/M3) show bars spanning nearly the *entire* January–December window,
because the site's actual growing season straddles the calendar-year boundary (GPP rises through
winter/spring, peaks in April, and only truly ends in June) and the first/qualifying-day-in-January,
last-qualifying-day-in-December rule stitches the following year's early rise onto the current year's
late fall. Panel (e), the same site under min_gpp_month (window starting in August), resolves this
cleanly: all 8 methods now show a single compact, late-year-to-spring bar (roughly November–June)
with no window-boundary wraparound. US-Ha1 (a) and US-Wkg (b) show well-separated single-peak/bimodal
seasons respectively (US-Wkg's clear two bumps — a small spring rise and a larger monsoon peak —
visible in the median curve itself). AU-How (d) shows a broad, gently double-humped curve (wet season
plus a smaller late dry-season rise) with methods spread widely in where they place the start,
consistent with Figure A/D's finding of a wide cross-method range there despite low skewness.

## Figure D — spread across methods vs. skewness

`Figure D.png`: x = skewness (NT, `derived_site.csv` "main_unscreened" variant), y = range of mean GSL across
the 8 headline methods (days), one point per site, for both year_rules (blue = calendar, red =
min_gpp_month). AU-How is circled: its cross-method range under the calendar rule is 139 days despite
a skewness of only 0.37 — one of the lowest skewness values in the set but one of the larger ranges,
confirming that skewness alone does not predict how much methods disagree at a site (Figure B's
AU-How panels show why: a broad, gently shaped season gives every threshold/rule a different
reasonable place to put the start/end, even without a sharp pulse-driven distribution).

The min_gpp_month rule moves most points down or leaves them unchanged, as expected since it removes
calendar-boundary artifacts rather than adding new disagreement. The largest drop is at US-Var
(255.8 d → 133.5 d, calendar → min_gpp_month, a 122-day fall — this site's own calendar-split
artifact resolving), followed by AU-How (139.2 d → 122.5 d) and IL-Yat (246.2 d → 217.9 d). FR-Pue
(197.9 d → 190.4 d) and US-Ton (212.7 d → 202.9 d) — the two sites the version-4 analysis flagged as
NOT showing a strong calendar-split artifact despite a non-January lowest-GPP month — show only small
drops here too, consistent with that earlier finding. The 9 sites whose lowest-GPP month is already
January are unchanged between the two colours (single point, since calendar = min_gpp_month there).

## What was checked

**First pass** (committed `a4880b2`): opened every PNG at full resolution and fixed two problems
found along the way — (1) Figure A's three labelled-choice points initially rendered as plain
circles, inconsistent with their own family's shape in the legend, fixed by giving them fixed
(non-aesthetic-mapped) family shapes so they share the existing shape legend; (2) Figure B's and
Figure C's x-axis month labels initially overlapped (e.g. "AugSep", "NovD"), fixed with 45°-rotated,
slightly smaller tick text; Figure C's first single-column draft also compressed 8 method labels into
an unreadable block, fixed with a 2-column × 3-row grid; and `patchwork`'s `+` operator was found to
apply a title only to the last sub-plot added (not the whole composite) unless routed through
`plot_annotation()`.

**This revision**: opened every regenerated PNG again after the three requested changes and the
skewness-source switch, and checked specifically:
- **Figure B**: the raster is fully visible at every site in both pairs (previously, the brackets ran
  across most of the in-season span and visually competed with the raster; now the raster's top-half
  placement and the strips' bottom-half placement never overlap, confirmed by construction and by
  eye). The three strips show individually broken, day-by-day patterns, not solid bars — most
  visible at AU-ASM, whose strips are dense but clearly gapped throughout the wet season, versus
  AU-How's and US-SRM's more continuous bands. **A further problem was found and fixed in this same
  check**: the Z16 strip, originally black, was nearly invisible against the raster's own black ("in
  season at every threshold") shading directly above it in both AU-ASM and AU-How panels — changed to
  blue, now clearly visible and distinguishable from the other two strips at every site in both
  figures.
- **Figure C**: every one of the 9 row labels (8 before this revision, now including both M4 rows) is
  readable at 8 pt in the 2-column grid, with no overlapping text, in all 5 panels (a–e). The longer
  labels (e.g. "M6, Hu, anchored") fit within the left margin at the figure's 200 mm width without
  crowding the plotted bars.
- **Figure A / D**: re-checked that the skewness-sorted site order and the continuous colour tiles
  match the `main_unscreened` values exactly (US-MMS now sits below CH-Dav, as expected), and that
  Figure D's x-axis label and point positions moved consistently with the new source.

## Outputs

- `figures/drafts/methods_review/Figure A.png` (+ companion, `.pdf`, `.legend.txt`, `.meta.json`)
- `figures/drafts/methods_review/Figure B (AU-ASM vs AU-How).png` (+ `.pdf`/`.legend.txt`/`.meta.json`)
- `figures/drafts/methods_review/Figure B (US-SRM vs AU-How).png` (+ `.pdf`/`.legend.txt`/`.meta.json`)
- `figures/drafts/methods_review/Figure C.png` (+ `.pdf`/`.legend.txt`/`.meta.json`)
- `figures/drafts/methods_review/Figure D.png` (+ `.pdf`/`.legend.txt`/`.meta.json`)

No existing table, figure, or script was changed. AU-ASM's passing years, P99, and M4 thresholds
(figure B only) were computed fresh by this script from the daily DuckDB data, using the same rules
(QC day-pass threshold, ≤14 failing days/year, calendar Jul–Jun window) as the 15 comparison sites;
they are not persisted to any table. The min_gpp_month M2 22-value grid (figure A companion) was
likewise computed fresh (not in any committed table — version 4 only persisted the calendar-rule
grid). Every other number is read directly from version 4's committed tables
(`gsl_methods_site_summary.csv`, `gsl_methods_site_year.csv`, `gsl_methods_m2_grid22.csv`,
`gsl_methods_year_rule_range.csv`, `derived_site.csv`).
