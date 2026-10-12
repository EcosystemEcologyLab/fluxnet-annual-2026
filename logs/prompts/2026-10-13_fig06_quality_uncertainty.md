Build main Figure 6, quality and uncertainty of annual fluxes, for Results 3.4. Source data: the interim technical-validation outputs (scripts/technical_validation_interim.R and review/diagnostics/data_quality_uncertainty/), read-only; do not re-run the dq_stage diagnostics. Latest snapshot. Figures and text files only.

Panels, one question each:
a. How much of the record is measured? ECDF of the annual QC fraction for NEE_VUT, NEE_CUT, LE and H, in that order, with the same colours as Supplementary Figure S5 (this is the annual panel removed from S5). Dashed line at the 0.5 rule. Axis label states the direction: fraction of records measured or good-quality gap-filled.
b. How large is each uncertainty term? For usable annual NEE: random uncertainty (RANDUNC), the u* term ((P84 - P16)/2) and the joint uncertainty (JOINTUNC), VUT and CUT side by side, as in interim figure TV2a, log y-axis, g C m-2 yr-1. NEE only; LE and H have no u* term.
c. Does the choice between VUT and CUT change annual sums? Histogram of VUT minus CUT for site-years where both are usable, g C m-2 yr-1. State in the panel how many differences exceed the combined uncertainty, sqrt(JOINTUNC_VUT^2 + JOINTUNC_CUT^2), and how many site-years lack the joint term.

Style: VUT and CUT colours as in S5 and S6. 183 mm wide, three panels in a row, panel letters a to c, text 5 to 7 pt.

Gates before saving: 6,336 annual rows; 3,960 site-years with both VUT and CUT usable; 4,017 with VUT usable; 4,320 with CUT usable; 20 with the joint term missing on at least one side. Stop and report on any mismatch.

Outputs: review/figures/draft_manuscript_v1/fig_06_quality_uncertainty.{png,pdf}, plus .legend.txt and .meta.json, from scripts/generate_fig06_quality_uncertainty.R. The legend text describes each panel's method in enough detail to follow, states the usable rule and counts, and describes patterns neutrally, without causal claims. Update FIGURE_INVENTORY.md. Run check_figure_format.R.

Save this prompt as logs/prompts/2026-10-13_fig06_quality_uncertainty.md. Log, commit and push per CLAUDE.md.
