GSL_definition/expanded: methods comparison, version 4. Revise code/11_gsl_methods_compare.R in place and regenerate its own tables and draft figures (version 3 stays in git history at cd7e547). Change nothing else; same 15 sites, NT and DT series and conventions as version 3. Literature points below are from the context agent's reading; sources are given per item.

1. Analysis year, run both ways. Calendar years split the season at winter-growing sites (IL-Yat, US-Var, US-Ton, probably FR-Pue), as they did at AU-How. Dave and Tash have not chosen a rule yet, so run every method under two year definitions and carry a year_rule column throughout:
   - calendar: January to December for Northern Hemisphere sites, July to June for AU-How (version 3);
   - min_gpp_month: each site's year starts on the first day of its calendar month with the lowest mean NT GPP over its passing years. Report the month chosen per site.
   Under either rule, a year passes if it has at most 14 failing days under the existing QC rule. Report, by site, how years, GSL, start and end, the start and end rule shifts (version 3 item 3), M8's GSL and the largest differences between methods change between the two rules. Our expectation: most of the large end shifts and M8's short seasons at the Mediterranean sites come from the calendar split. Say whether that holds.

2. M7 smoothing, after Ma et al. (2013, Section 2.3). "Halfway" is in time, as implemented. Their curve is much smoother than ours: singular spectrum analysis over the whole record, on 16-day composites, with a window of 37 composites and four leading components, interpolated to daily values; the derivative is the one-day difference of that curve. Their tower GPP was aggregated to 16-day periods first. Replace M7's 15-day running mean with: GPP aggregated to 16-day means, four-component SSA over each site's full record (Rssa, if it can be added under CLAUDE.md's package rules), interpolated to daily values. If Rssa cannot be added, use a two- or three-harmonic fit per year and report which. Recheck AU-How on July to June years against Ma's 244 to 275 days; if it still falls well short, report the gap as a real difference.

3. AT-Neu cuts. Published cut dates (Hortnagl and Wohlfahrt 2014): 2010 on 5 June, 31 July and 20 September, manure 18 to 22 October; 2011 on 6 June, 1 August and 26 September. The FLUXNET site page says 3 to 4 cuts a year. No dates are published for 2018 to 2020.
   - Rerun the version 3 test for 2010 with the actual dates.
   - Build a cut detector on unsmoothed daily NT GPP: a drop of more than about 50% within 1 to 3 days, followed by regrowth over 2 to 4 weeks. Validate it against the 2010 and 2011 dates (hits, misses, false alarms, timing error), then apply it to every AT-Neu year.
   - For each two-season year, check whether the trough between the two peaks coincides with a detected cut.
   - Remove version 3's claim that management is the main correlate of two-season years. Report AT-Neu (cuts) separately from the US Southwest dryland sites, where two seasons can come from winter and monsoon rain.

4. Management table. Replace tables/site_management_15.csv with the following, with type, dates where known and a source column (context agent's literature read; drop the BADM keyword mining, which produced false positives):
   - AU-How: burned every 1 to 3 years, controlled burns early in the dry season (TERN site page).
   - NL-Loo: Scots pine planted about 1909; very little management since (ICOS station page).
   - DE-Tha: spruce sown 1887; thinned April 2002 (about 30% of trees); 2 ha windthrow 300 m from the tower, January 2007 (Bernhofer et al. 2011).
   - FR-Pue: holm oak coppice, last clear-cut 1942; throughfall-exclusion and thinning experiments since 2003 (DEIMS).
   - CH-Dav: research plots never thinned; harvest in the surrounding forest; no tillage (WSL site page).
   - US-MMS: selective logging, since ended, year not given (AmeriFlux/OSTI).
   - US-Mpj: drought and bark-beetle pinon die-off 2013 to 2015; no grazing reported (AmeriFlux/OSTI).
   - AT-Neu: hay meadow cut 3 to 4 times a year and manured (item 3 dates).
   - US-Ha1: cleared for farming in the 1600s and 1700s; overstorey uprooted by the 1938 hurricane; no recent management; type none (AmeriFlux/OSTI).
   - JP-Api: beech forest about 70 years old; none reported (AsiaFlux).
   - IL-Yat: Aleppo pine planted 1964; fire-protected; grazed by sheep and goats in late spring; irrigation experiment plot in the south of the site, footprint overlap unknown; not burned (ICOS station page).
   - US-Ton, US-Var: cattle grazing, brush periodically removed; season and intensity not found (AmeriFlux/OSTI).
   - US-Wkg: light to moderate grazing during the flux record (Scott et al. 2010).
   - US-SRM: cattle grazing year-round 1957 to 2005, minimal in 2006, none from 2007 (Scott et al. 2009).

5. NT against DT wording. Keenan et al. (2019) did include grasslands (one of 11 ecosystem types in their Fig. 3). Apparent inhibition of daytime respiration was highest in open shrublands (22.9%), savannas, woody savannas and wetlands and lowest in evergreen broadleaf forest (5.1%), with a seasonal cycle that generally matched fAPAR. That predicts an earlier NT start wherever green-up is fast, consistent with AT-Neu and US-Wkg. Their inhibition is estimated from flux data, checked against isotopes only at Harvard Forest, and described by the authors as conservative. Rewrite the version 3 item 6 conclusion accordingly.

Checks as version 3, run under the calendar rule.

Report in docs/gsl_methods_compare_<date>_v4.md: each item with numbers, with the analysis-year comparison first.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_methods_compare_v4.md. Log, commit and push per CLAUDE.md.
