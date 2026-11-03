WAFNET/fluxcom_comparison: fixes to the stage 2 FLUXCOM-X-BASE outputs (commit 402101b) and one new set of figures. Same data, sites, rules and conventions as stage 2; no new downloads. FLUXCOM-X-BASE remains non-Shuttle comparison data under Hard Rule 1.

Fixes:
1. Methods paragraph (docs/report_back_2026-11-03.md and the methods memo): it says respiration was derived as "GPP minus net ecosystem exchange". With NEE negative for uptake, as the code uses, it is GPP plus NEE (equivalently, GPP minus net uptake). Correct the wording everywhere it appears and check the rest of the paragraph against the code.
2. One site palette for every figure: the same colour for each site in the scatter, map, Taylor diagrams and the new figures below.
3. Site map: BJ-Nhu and BJ-Bfg, and SN-Dhr and SN-Nkr, sit almost on top of each other and their symbols overlap. Offset overlapping symbols slightly, with a short leader line to each true location, so every symbol is visible. Trim the empty space above and below the panels.
4. Taylor diagrams: the "tower" reference label is clipped by the axis; move it so it is fully visible.

New figures: mean seasonal cycle (climatology) per site.
5. One figure per site (GH-Ank, BJ-Nhu, BJ-Bfg, SN-Dhr, ML-Kem, and SN-Nkr marked pending Ossenatou), one panel per flux: NEE, GPP, respiration (X-BASE derived; say so in the panel title) and LE. Where a flux cannot be compared at a site (GH-Ank GPP and respiration, which have no NT tower data), leave the panel out and say why in the caption.
   - x-axis: calendar month, January to December. y-axis: the flux in the stage 2 units.
   - Two series in each panel, tower and X-BASE, each the mean across years for that month, computed only from the site-months used in the stage 2 statistics (QC-passing tower months within 2001 to 2021), so both series cover exactly the same months.
   - Show the spread across years as a shaded band (plus and minus one standard deviation where a month has at least 3 years; otherwise plot the individual years as points instead of a band). Print the number of years behind each month along the bottom of each panel.
   - Title each figure with the site ID, vegetation class, years used, and "training site" where it applies. Use the shared site palette for the tower series and one fixed colour for X-BASE throughout.
   Write to figures/draft_climatology_<site>.png, labelled DRAFT, and the monthly means, standard deviations and year counts to tables/xbase_climatology.csv.

Before committing, open every regenerated and new PNG and check: symbols and labels do not overlap, both series and bands are visible in every panel, year counts are readable, and colours match across figures. Say in the report what you checked.

Update docs/report_back_2026-11-03.md with a short section on the fixes and the climatology figures, including one sentence per site on how the X-BASE seasonal cycle differs from the tower's (amplitude, timing of the peak, dry-season values), with numbers. Update the README.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_wafnet_fluxcom_x_stage2_fixes.md. Log, commit and push per CLAUDE.md.
