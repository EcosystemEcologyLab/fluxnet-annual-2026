GSL_definition/expanded: replace Figure 1 with a simpler two-site figure. Dave's design. Replace the Figure 1 section of code/04_figures_main.R (the old raster version stays in git history), regenerate only "Figure 1.png", and leave every other figure and table unchanged. Main variant from session 2a; report how many winter-screened days each site has (expected near zero). DuckDB read-only, dataset = 'FLUXMET' only; local write_meta().

Sites: US-SRM (WSA, skewness 2.41) and AU-How (WSA, skewness 0.37). Same IGBP class, opposite ends of the skewness range. One column per site, US-SRM left; rows are panels a, b and c.

Common years: derive the years that pass at both sites, by year label. Expected from the committed tables: 2008, 2010, 2011, 2012, 2014, 2015, 2016, 2017, 2019, 2022, 2024 (11 years; AU-How's are Jul-Jun season years, as in the rest of the run). If your derived list differs, use yours and report the difference.

a. Distribution of daily GPP for each site over all its passing years: histogram with x in % of the site's P99 (so the shapes are comparable), vertical lines at Z17 (10% of P99) and the site's T95, and the site's skewness, P99 and number of years printed in the panel.
b. Daily GPP for one year at each site: the common year whose Z17 GSL is closest to that site's median over the common years. x = day of year (day of season year for AU-How), y = gC m-2 d-1, horizontal lines at the Z17 and T95 thresholds in absolute units. Print the year and both GSL values in the panel.
c. GPP summed over the common years: three bars per site for total GPP, GPP on days counted by the Z17 GSL (GPP above the Z17 threshold), and GPP on days counted by the T95 GSL. Label each season bar with its share of the total (%) and the summed GSL in days.

Z17 in black and T95 in red, as in the other figures. PeerJ format: 300 dpi, text >= 8 pt, panel labels a to c, fits 8.5 x 11 in. Write the numbers behind panels b and c to tables/figure1_values.csv. Update the Figure 1 caption, the README and the methods memo.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_figure1_two_sites.md. Log, commit and push per CLAUDE.md.
