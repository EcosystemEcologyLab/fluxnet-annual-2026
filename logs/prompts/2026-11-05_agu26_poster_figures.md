# AGU26 poster figures — prompt

Saved 2026-11-05. Verbatim task text follows.

---

AGU26 poster figures: poster versions of paper Fig. 2 (cumulative site-years) and a simplified Fig. 7 (network sampling) for Dave's poster on 10 December 2026 (44 in wide by 42 in high).

Ground rules
- Leave the paper alone. The paper figures, the default outputs of their scripts and review/figures/draft_manuscript_v1/ stay unchanged; confirm with git status at the end.
- Write everything to a new folder, review/figures/poster_agu26/.
- Build through a poster option or a thin wrapper around the existing figure code, never by changing paper defaults. scripts/figure4_representativeness.R already has an env-var-gated FLUXNET_PRESENTATION_MODE block backed by R/presentation_format.R; follow that pattern with a separate poster mode and a new R/poster_format.R. Do not change presentation_format.R: its rule (same figure, just bigger) does not fit the poster, which drops panels and uses direct labels.
- For Fig. 2, first identify which script and options produced draft_manuscript_v1/fig_02_cumulative_siteyears_igbp (see review/figures/FIGURE_INVENTORY.md and RUN_LOG_fig02_promote.txt), and build from that.
- Same data and numbers: the 20 September 2026 snapshot and the existing processing rules; no refresh to a newer listing. Every count and ratio must match the paper's figure tables, including draft_manuscript_v1/SupTables/supplementary_data_2_sampling_ratios.csv for Fig 7. Check this programmatically and report the result.
- Follow CLAUDE.md, including the IGBP palette and order (these override the general colour-blind-safe preference for Fig. 2), Hard Rule 1 labelling of the historical datasets as comparison data, and a .legend.txt and .meta.json for each figure.

Size and typography, at print size
- Build each figure at its printed size so point sizes are true: Fig. 2 about 20 in wide by 15 in tall; Fig. 7 about 11 in wide by 16 in tall.
- Axis titles at least 32 pt, tick labels at least 26 pt, legend text at least 24 pt, nothing anywhere below 22 pt, including inset text and direct labels.
- Lines about three times their paper weight.
- Arial or Helvetica (whichever is installed), white background, colour-blind safe colours where CLAUDE.md does not fix them.
- No figure titles or panel letters inside the images.
- Outputs per figure: vector PDF with embedded fonts (check with pdffonts) and a 300 dpi PNG.

Fig. 2, poster version
- Main panel exactly as in the paper: cumulative site-years stacked by IGBP class, with the Marconi, La Thuile and FLUXNET2015 lines.
- Label those three lines directly at or near their right-hand ends, not in a legend.
- Keep the record-length inset in the paper's position, scaled up to fill the open space, with all its text at least 22 pt. Keep any threshold labels it already carries; add no other site or site-year counts anywhere in the figure.
- IGBP legend in a single column to the right of the plot, in the paper's palette and order.

Fig. 7, simplified version
- Two panels stacked vertically: Köppen-Geiger climate class, then aridity. Drop land cover, biomass, NEE and ET.
- Keep the paper's encoding: log2 sampling ratio by class, a reference line at a ratio of 1, and empty bars marked "none" for classes with land but no sites.
- Short readable class names, for example "Hot desert (BWh)", "Tundra (ET)", "Hyper-arid", not codes alone.
- No Jaccard values in the panel headers.
- One accent colour for the desert classes (BWh, BWk), the polar classes (ET, EF), and the hyper-arid and arid aridity classes; one muted colour for all others. No text that claims causes.

Before committing, open both PNGs at full size and check: nothing overlaps (direct labels, inset, legend), every text element meets the minimum sizes (measure from the built plot, not by eye), and the accent colour falls on the right classes. Say in the report what you checked.

Report in review/figures/poster_agu26/report_<date>.md: file paths; the script and option used for each figure; the list of shortened class names with their codes; confirmation, with the check you ran, that all counts and ratios match the paper's figure tables; the pdffonts result; and anything unexpected.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_agu26_poster_figures.md. Log, commit and push per CLAUDE.md.
