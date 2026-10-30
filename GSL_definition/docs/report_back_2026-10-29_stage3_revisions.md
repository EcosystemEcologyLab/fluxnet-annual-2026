# GSL figure rebuild — Stage 3 revisions report back (2026-10-29)

Six revisions to Stage 3's figures, requested after Dave reviewed `docs/report_back_2026-10-29.md`.
Full prompt: `logs/prompts/2026-10-29_gsl_stage3_revisions.md`. Full detail (root causes, exact
fixes) is in `docs/methods_memo.md`'s "Stage 3 revisions" section; this document gives the
headline results and updated numbers only. Main run throughout unless noted: `GPP_NT_VUT_REF`,
negative GPP set to 0, year screening d80, GSL counts `GPP > T` (strict).

## 1. Figure 3a bug: CH-Dav's Z17 dot outside its own trend-range bar

**Confirmed real bug, fixed. Root cause**: `sum(gy > T_abs, na.rm = TRUE)` over an all-NA `gy`
silently returns `0`, not `NA`. CH-Dav 2008 has 366 daily rows, all NA — this fabricated a fake
"GSL = 0" year that distorted the trend regression at every threshold in the 5-20% sweep. No
other trend site has this problem (checked all 21 with a `stopifnot()`, now a permanent
regression guard in `07_fig3.R` and `10_supplementary.R`).

**The headline count does not change**: of the 21 trend sites, **6** sign changes, **6**
significance changes, **11** either — identical to the pre-fix numbers in the original Stage 3
report.

## 2. Figure 1: new high-third site, dryland-restricted

High-third selection rule changed to: nearest the high third's own median skewness, **restricted
to woody-grass mosaic and grassland sites** (the drylands groups the figure's point is actually
about), still excluding AU-ASM.

- Low third (unchanged): **JP-Fjy**, skewness = 0.5123, T95 = 15.4% of P99.
- High third (changed from CA-Ca2): **US-Ton**, skewness = 1.2120, group = woody-grass mosaic,
  T95 = 12.2% of P99.

Change in mean GSL from 5% to 10% of P99: JP-Fjy = **-26.40 days**, US-Ton = **-34.70 days**
(previously CA-Ca2 = -53.43 days under the old site choice; the new dryland-restricted
comparison still shows the high-third site roughly 30% more sensitive to this threshold step
than the low-third site). Now also drawn as a labelled bracket on panel b, not just stated in
`.legend.txt`. Panel a rows are now labelled with site name + skewness; T95 is a dotted firebrick
line, visually distinct from the dashed 5/10/20%-of-P99 lines; panel b's x-axis now covers the
full 0-100% of P99.

Figure 4 panel a reuses the same two sites (recomputed via the same selection rule, not
persisted) — now also JP-Fjy/US-Ton, consistent with Figure 1.

## 3. One marker convention: Z17 = black circle, T95 = red triangle

Applied in Figures 2b, 2c, 3a and 4c (plus the `10_supplementary.R` duplicates of 2b/2c/3a), with
a proper `guide_legend()` in each. The floating "T95"/"Z17" text previously drawn inside Figure
2c is removed.

## 4. Figure 2 heat map

- Vertical striping at low thresholds removed (log-uniform binning, 35 bins across 0.1-35% of
  P99, shared between panels b and d).
- Sensitivity now shown as `|local_sensitivity|` (absolute value), darker = more sensitive.
- Panel d is now a smoothed median-per-bin curve, using the same bins as panel b, and is now
  correctly labelled "d" — found and fixed a second, pre-existing instance of the
  `panel_letter()`/`scale_x_log10()` bug from the original Stage 3 pass (`x = -Inf` is `NaN`
  under a log scale) that had silently dropped panel d's letter from every Figure 2 variant,
  including the already-committed main run.

## 5. Skewness colour scale capped at 2

Fixed `limits = c(0, 2)`, breaks at 0/0.5/1/1.5/2, values above 2 squished to the top colour and
labelled ">2". Same scale (same limits/breaks/labels/palette) used everywhere skewness is
coloured: Figures 1, 2 and 4.

## 6. Legend cleanup

"mixed forest" is now omitted from a figure's group legend whenever no plotted site belongs to
that group (e.g. Figure 3's 21 trend sites). Figure 4 panels a and c, previously flagged for
legends cutting their panel borders, now anchor the legend box's own corner (not its center) to
its position, so the box grows inward instead of spilling past the edge.

## Figure 4 and the benchmark table: unaffected by the revisions

Figure 4's median common-unit sensitivity (Z17 = 11.085, T95 = 8.290 days per 1% change in F)
and the benchmark table (21 trend sites, median r = 0.612 Z17 / 0.683 T95) are unchanged from the
original Stage 3 report — neither `09_benchmark.R` nor Figure 4 panels b/c were touched by this
revision pass (only panel a's site selection and the marker/legend fixes, none of which change
these numbers).

## Supplementary figures: sign/significance counts unaffected

All four supplementary figures were rerun with the full set of fixes (log-binning, absolute-value
sensitivity, marker convention, panel-d letter). Sign/significance-change counts are unchanged
from the original Stage 3 report:

| Variant | Sign changes | Significance changes | Either |
|---|---|---|---|
| Main (NT, zero-floored) | 6 | 6 | 11 |
| DT, zero-floored | 6 | 7 | 13 |
| NT, as-delivered | 6 | 6 | 11 |

## Checks performed

- `07_fig3.R`'s and `10_supplementary.R`'s `stopifnot()` consistency check (Z17's trend within
  its own 5-20% range bar) passes for all 21 trend sites in all three variants (main, DT,
  as-delivered).
- All of Figures 1-4 and all four supplementary figures regenerated and inspected at full size
  after every code change, not just confirmed to exist — this is how the panel-d letter bug (item
  4) was caught, via the "NaNs produced" / "Removed 1 row" warnings emitted on rerun.
- `.legend.txt` files cross-checked against the regenerated figures; one stale line found and
  fixed (Figure 3a's legend described the old "circle/diamond" marker pair, not the new
  black-circle/red-triangle convention).

## What remains

Unchanged from the original Stage 3 report (see `docs/report_back_2026-10-29.md`'s "What
remains"): Check 3 (Figure 1 correlations) is still undefined; the current manuscript's own
Figures 1-4 are still deferred, pending the manuscript; Tash's sensitivity site sets and other
requested materials are still not received.
