Create presentation versions of main Figures 1, 2, 3, 4, 6 and 7 (Figure 5 waits for the round robin). The manuscript figures and their scripts' existing outputs must not change.

Approach
- Add a presentation mode to each figure's existing generating script or function (a flag, theme or wrapper), drawing from the same data, gates and colours as the manuscript figure. Do not enlarge or crop the journal PNGs.
- Slide canvas 13.333 x 7.5 in (16:9). Each figure image is about 12.8 x 6.3 in, leaving a top band for a slide title; no title inside the image.
- Type and lines at final size: tick labels at least 18 pt, axis titles 20 to 24 pt, nothing below 14 pt; data lines 2 to 3 pt, points and bars scaled to match.
- One panel per slide. No panel letters. No in-figure notes. Prefer direct labels on lines or groups over legends where they fit; otherwise one large legend.
- Builds: every step of a build keeps the final step's axis limits, extent, colours and layout, so only the new element appears.
- Colours exactly as in the paper.

Slides
- Figure 1: (1) world map with only the sites in the three earlier collections (Marconi, La Thuile, FLUXNET2015, matched to current IDs as for Table 1, coloured as in Fig. 2); (2) world map with all 781 snapshot sites; (3 to 6) each regional panel on its own slide.
- Figure 2: (1) the three historical cumulative lines only, on the final axes (see presentation_figures/fig_02_historical_only); (2) the snapshot's stacked IGBP areas added, no inset; (3) the record-length histogram as its own full-size slide, with the 5, 10 and 20 year lines and counts.
- Figure 3: (1) land climate envelope only; (2) with the site layer.
- Figure 4: NEP, ET and H, one slide each.
- Figure 6: QC distribution, uncertainty terms, VUT minus CUT, one slide each, using the current (dd13453) design.
- Figure 7: six slides, one per axis, keeping the land/tower numbers and J for that axis.

Outputs in review/figures/presentation_figures/slides/
- One PNG per slide (200 dpi at slide size), named by figure, content and build step, e.g. fig02_step1_historical_lines.png.
- One assembled deck, presentation_versions.pptx: one image per slide, slide title left empty, and in the speaker notes the matching caption text and numbers from that figure's .legend.txt, with the snapshot file named.
- manifest.csv: slide number, PNG, source figure, script, build step.

Checks: the same gates as each manuscript figure; render the deck to PDF and inspect every slide for clipping, overlap and text below 14 pt; confirm the manuscript figure files are byte-identical to before.

Save this prompt as logs/prompts/2026-10-13_presentation_versions.md. Log, commit and push per CLAUDE.md.
