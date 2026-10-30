# GSL figure rebuild — PeerJ restyle report back (2026-10-30)

Full prompt: `logs/prompts/2026-10-30_gsl_peerj_restyle.md`. Full detail (exact code changes,
root causes) is in `docs/methods_memo.md`'s "PeerJ restyle" section; this document gives the
headline results only. No pending `*_gsl_fig2_fixes.md` prompt was found, so this was a fresh
task, not a reconciliation. All figures regenerated from the same underlying data as the Stage 3
revisions pass (`docs/report_back_2026-10-29_stage3_revisions.md`) — nothing in this pass
touched data derivation, only figure style and four Figure-2-specific display fixes.

## Style: PeerJ conventions

All GSL figures now follow PeerJ's own author instructions instead of `R/nature_format.R`'s
Nature conventions: 170mm width, >=8pt text (axis titles 9pt, panel letters 10pt bold), 1.5-2pt
data lines / ~1pt reference lines, roughly doubled point/marker sizes, >=300dpi (kept at 600dpi),
and one PNG+PDF pair per figure named `"Figure 1"`…`"Figure 4"` and `"Supplemental Figure
S1"`…`"S4"` (S-numbers in citation order; see `GSL_definition/README.md`). The old
`"fig_0N_..."`-named files are fully superseded, not kept alongside.

The skewness colour scale changed from a continuous, capped-at-2 viridis ramp to a discrete,
colourblind-safe three-colour palette (one colour per skewness third: low/middle/high; three
viridis stops, confirmed monotonic in greyscale), used consistently in Figure 1b, Figure 2a/2d,
and Figure 4a/b.

## Item 1: Figure 1b density lines labelled by site name

Fixed. Panel b's two lines are now labelled/coloured `JP-Fjy`/`US-Ton` directly, not `"low
third"`/`"high third"`, using the skewness-third palette's low/high colours. Figure 4 panel a
(same two sites) was changed the same way for consistency, even though only Figure 1b was
explicitly requested.

## Item 2: Figure 2b colour-scale cap and x-axis start

Fixed. Colour scale now capped at the 95th percentile of each run's own plotted values (not a
fixed number): main Figure 2 24.3 days/%P99, Supplemental S1 (DT) 28.8, S2 (as-delivered) 24.3;
top colour labelled `">= <cap>"`. x-axis now starts at 0.5% of P99 (was 0.1%). Low-threshold
structure remains clearly visible after both changes, confirmed by direct visual inspection.

## Item 3: Figure 2c/2d absolute values; floating text; line distinguishability

Fixed. Panels c and d now plot `|value|`, matching panel b. The floating "Z17"/"T95" text in
panel c was already removed in the original Stage 3 pass (2026-10-29) — confirmed via a
repository-wide grep, no new fix needed there. Panel d's three lines are now both thicker
(1.8pt, up from 0.5pt) and coloured with the shared skewness-third palette, replacing an
inconsistent ad hoc colour set. A side effect found while re-rendering: PeerJ's larger tick text
caused panel c's x-axis labels to overlap in the wider-range supplementary variants — fixed with
an explicit 3-break limit.

## Item 4: CH-Dav 2008 data gap

Documented. Checked every site-year across all 62 sites: exactly one, **CH-Dav 2008**, has
`GPP_NT_VUT_REF` entirely missing (366/366 daily rows `NA`) while `GPP_DT_VUT_REF` and the QC
flags are complete — consistent with Tash's Table S1 listing it as a usable year. No other
site-year shows this pattern. With the Stage 3 all-NA-year fix already in place, this year
correctly contributes `NA`, not a fabricated `GSL = 0`, to every NT-based quantity — **CH-Dav has
15 screened years but 14 usable years for NT-based results** (unaffected for the DT-variant
supplementary figures, which use all 15).

## Data and code availability

New `docs/data_and_code_availability.md`: identifies the primary FLUXNET Shuttle snapshot (20
September 2026; per-site product DOIs in its `product_id`/`product_citation` columns), proposes
four candidate Supplemental Data groupings from `GSL_definition/tables/` (totaling ~840KB,
trivially within PeerJ's 30MB/file and 50MB/total limits), and flags that `GSL_definition/` does
not yet have an archival DOI — a Zenodo release is needed before submission.

## Checks performed

- `07_fig3.R`'s and `10_supplementary.R`'s `stopifnot()` consistency check still passes for all
  21 trend sites in all three variants.
- Sign/significance-change counts unchanged from the Stage 3 report: main 6/6/11, DT 6/7/13,
  as-delivered 6/6/11.
- Figure 1's representative-site numbers unchanged (JP-Fjy/US-Ton); Figure 4 panel a continues to
  reuse the same pair.
- All of Figures 1-4 and all four supplementary figures regenerated and inspected at full size
  after every code change, not just confirmed to exist — caught the panel-c tick-label overlap
  (item 3) and a y-axis label rendering issue (a `|`-adjacent character glued visually to the
  following word) in Figure 2 panel d, both fixed before finalizing.
- `renv`/`testthat` state unaffected — no package or test-suite changes in this pass.

## What remains

Unchanged from the Stage 3 report: Check 3 (Figure 1 correlations) is still undefined; the
current manuscript's own Figures 1-4 are still deferred, pending the manuscript; Tash's
sensitivity site sets and other requested materials are still not received. New from this pass:
the code DOI (Zenodo release) is still outstanding, and the final Supplemental Data S-numbering
depends on the manuscript's own citation order, not yet available.
