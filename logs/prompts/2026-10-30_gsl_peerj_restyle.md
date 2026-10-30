GSL_definition: restyle all figures for PeerJ and fix the pending Figure 2 items. This supersedes logs/prompts/*_gsl_fig2_fixes.md if it was not sent. Rerun the main and supplementary figure scripts, inspect each figure at full size, update the .legend.txt files, and log as usual.

PeerJ requirements (author instructions, read 30 Oct):
- One file per figure, named "Figure 1.png" … "Figure 4.png", and "Supplemental Figure S1.png" etc. for the supplementary figures (assign S numbers in citation order and list them in the README). Keep the PDF alongside each PNG.
- At least 300 dpi at final size; fit an 8.5 × 11 inch page.
- Labels in a readable, uniform font, about 2 mm high: no text below 8 pt at final size.
- Multi-part figures in one file, panels labelled a, b, c; leave clear white space around the figure and between panels.

Style (set once in code/fig_helpers.R; GSL figures no longer use R/nature_format.R's 5 to 7 pt and 1 pt limits):
- Figure width about 170 mm; height as needed, within the page.
- Text: tick labels and legends at least 8 pt, axis titles 9 pt, panel letters 10 pt bold. Site names in Figure 3 at least 8 pt; make the figure taller if needed.
- Lines: main data 1.5 to 2 pt; reference and threshold lines about 1 pt; the faint "other years" lines in Figure 1 darker and thicker than now, but still clearly behind the highlighted year.
- Points and markers about twice the current size.
- Skewness colour: one colourblind-safe palette for the three skewness thirds (low, middle, high), distinct in greyscale, used everywhere skewness is shown by colour: Figure 1, Figure 2a and 2d, Figure 4. Replace the continuous viridis scale. Keep numeric skewness values in the .legend.txt files.

Figure-specific fixes:
1. Figure 1b: label the two density lines with the site names (JP-Fjy, US-Ton), not "low third" and "high third".
2. Figure 2b: cap the colour scale near the 95th percentile of the plotted values (top colour labelled "≥ cap"), and start the x-axis at 0.5% of P99. Confirm the low-threshold structure is visible.
3. Figure 2c and 2d: plot absolute values (days lost per 1% of P99), matching 2b. Remove the floating "Z17" and "T95" text from 2c. Make the three lines in 2d clearly distinguishable (thicker lines, the thirds palette).
4. Methods memo: GPP_NT_VUT_REF is entirely missing for CH-Dav 2008 in our data, although QC flags and GPP_DT are complete and the year is in Tash's Table S1. Record how it is handled (excluded from every NT-based quantity; CH-Dav keeps 14 usable years). Check every other site-year for the same pattern and list any found.

Also write docs/data_and_code_availability.md covering: the FLUXNET Shuttle snapshot used and per-site product DOIs; which derived tables would go in as Supplemental Data S1, S2 and so on, with sizes (PeerJ limit: 30 MB per file, 50 MB total); and a note that the code needs a DOI (e.g. a Zenodo release of GSL_definition/) before submission.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_peerj_restyle.md. Commit and push per CLAUDE.md.
