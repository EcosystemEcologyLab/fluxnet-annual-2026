Redo the presentation versions (df76f19). The slides there were redesigned and stretched to 16:9; I want the paper figures, at the paper aspect ratio, made legible on a slide. Text gets added beside them in PowerPoint.

Remove from review/figures/presentation_figures/: slides/, presentation_versions.pptx, presentation_versions.pdf and manifest.csv. Remove the deck builder (scripts/build_presentation_deck.R). Keep the FLUXNET_PRESENTATION_MODE gating and R/presentation_format.R, revised as below. The manuscript figure files must stay byte-identical.

Rules for every slide PNG
- Same design as the paper figure: same layout, legends, panel letters, annotations and colours. Only size changes: text, lines and points scale up. No direct labels in place of legends, no stripped notes, no restyling.
- Aspect ratio equals the paper figure, or the paper sub-layout for a split. Size each PNG to fit 12.3 x 6.5 in (height-limited for most), 200 dpi.
- Text as large as the paper layout tolerates without overlap or clipping: tick labels 16 to 18 pt where they fit, nothing below 12 pt. Report the smallest text size on each slide.
- Use the same snapshot as the paper figure. Do not change snapshot pins.

Slides
- Network growth (new; world map extent and style of Fig. 1a, one symbol and colour throughout, a small corner label with collection name and n): (1) Marconi 2000 sites only, (2) La Thuile 2007 only, (3) FLUXNET2015 only, (4) current Shuttle snapshot. Each slide shows only that collection's sites, matched to current IDs as for Table 1, on identical extents so the slides flip cleanly.
- Figure 1: (1) the full figure; (2) world map panel a alone; (3) North America and Europe; (4) Asia and Australia/New Zealand. Fix the Europe slide: the land layer ends in a diagonal edge on the right, and the "1000 km" label overlaps Spain. Check whether the paper figure has the same faults and report, but do not change the paper figure.
- Figure 2: (1) the three historical lines only, on the final axes; (2) the stacked IGBP areas added, no inset; (3) the full figure with inset.
- Figure 3: (1) land climate envelope only, same axes; (2) the full figure.
- Figure 4: the full figure, one slide.
- Figure 6: the full figure, one slide.
- Figure 7: (1) panels a to d (Köppen, IGBP, aridity, biomass); (2) panels e and f (NEE, ET). Keep the paper's panel letters and J values. The "smaller proportion / greater proportion" titles were clipped in the last version; make sure nothing clips.

Within a build, every step keeps the final step's canvas, axes, extent and layout.

Outputs: PNGs in review/figures/presentation_figures/slides/, named by figure, content and step (e.g. growth_1_marconi.png, fig01_3_namerica_europe.png). Write manifest.csv with: file, source figure, script, step, snapshot file, pixel size, smallest text size, and caption text from the figure's .legend.txt.

Checks: the same gates as each paper figure; site counts on the growth maps match Table 1; inspect every PNG at full size for clipping, overlap and legend collisions; confirm the manuscript figures are unchanged; run check_figure_format.R.

Save this prompt as logs/prompts/2026-10-13_presentation_versions_v2.md. Log, commit and push per CLAUDE.md.
