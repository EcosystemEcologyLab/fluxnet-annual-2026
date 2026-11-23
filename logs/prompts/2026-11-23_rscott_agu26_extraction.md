New side extraction for Russ Scott (USDA-ARS, Tucson), for his AGU26 poster on productivity against water availability across dryland sites. Russ does the analysis; this run only extracts and packages data, and does not interpret the GPP-precipitation relationship. Put everything in a new folder RSCOTT_AGU26/ (own code/, tables/, docs/, local write_meta(), logs inside the folder), following the side-project conventions in CLAUDE.md. Data: the 20 September 2026 snapshot and the existing extracts and DuckDB store, read only, FLUXMET rows only. No new downloads.

1. Candidate sites: IGBP class OSH, CSH, GRA, SAV or WSA, as reported by the site team, in either hemisphere.

2. Precipitation. Tower P_F is partly gap-filled from downscaled ERA5. Before using it, read the P_F_QC definition at each resolution from the product documentation and confirm its meaning at two or three control sites; do not assume the convention used for other variables (earlier work in this repo found a reversed flag polarity for precipitation). From the daily data, compute for every site-year (both time steps in step 4) the total P_F and the share of it that is ERA-filled. A year counts as having measured precipitation if its ERA-filled share is at most 10% (our threshold; state it as such).

3. Site screen. Mean annual measured precipitation, from calendar years with measured precipitation (at least 2 such years), must be below 800 mm. Sites with fewer than 2 such years cannot be screened: list them in excluded_sites.csv with their IGBP class and record length, not in the main table.

4. Values per site-year, two time steps:
   - Calendar year, from the annual (YY) files.
   - Water year, built from the monthly (MM) files and labelled by the year it ends: October to September for Northern Hemisphere sites and July to June for Southern Hemisphere sites (by tower latitude). Before building water years, check the monthly units by reconstructing several calendar-year totals from monthly values and comparing them with the YY values; report the check.
   For each: GPP_NT and GPP_DT (VUT, else CUT, chosen per site as in the paper and never mixed within a site), ET from LE_F_MDS with the paper's fixed latent heat (2.45e6 J kg-1), ET from LE_CORR as well (it has no QC column of its own; give it the LE_F_MDS QC and say so), and P_F with its ERA-filled share. Units: GPP g C m-2 yr-1, ET and P mm yr-1. Pass flags use qc_passes(): at QC_THRESHOLD_YY for calendar years (GPP on the NEE QC); for water years, a value passes only if all 12 months are present and pass at QC_THRESHOLD_MM, and the number of passing months is reported either way.

5. Keep a site if it has at least 5 years in which GPP_NT, ET (LE_F_MDS) and measured precipitation all pass, counted separately for calendar and water years; keep it if either count reaches 5. Report both counts per site, and also the count without the precipitation condition, so Russ can see what the 10% rule costs. Keep every site-year of a kept site in the table, passing or not, with its flags.

6. Package in RSCOTT_AGU26/package_rscott_<date>/, with a zip:
   - site_years.csv: one row per site, time step and year, with all values, QC, pass flags and the ERA-filled share.
   - sites.csv: site, name, country, IGBP, coordinates, hemisphere and water-year convention, the site's driest month in its mean monthly measured-precipitation climatology (so sites where the convention splits the rainy season, such as winter-rain sites in the south, can be spotted), mean annual measured P, the counts from step 5, dataset PID and citation from the snapshot, product version, and the exact annual and monthly file names used.
   - excluded_sites.csv: every candidate dropped and why (fewer than 2 years of measured precipitation, mean P of 800 mm or more, or fewer than 5 passing years), with the counts.
   - README.md for Russ, in plain English: selection rules, every column with units, the sign and partitioning notes, the QC rule, the ERA-filled share and how to use it, the water-year conventions and the driest-month column, that calendar years split the summer wet season at Southern Hemisphere sites, and that each site dataset must be cited by its PID.
   - One check figure: annual GPP_NT against annual measured P (calendar years with measured precipitation only), coloured by IGBP class, Southern Hemisphere sites marked, labelled DRAFT. Open it before committing and say what you checked.

Report in RSCOTT_AGU26/docs/report_back_<date>.md: the P_F_QC meaning and how it was confirmed, the unit check, counts of candidate, kept and excluded sites by class and hemisphere, the kept sites where the water-year convention may split the rainy season, and anything unexpected.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_rscott_agu26_extraction.md. Log, commit and push per CLAUDE.md.
