# GSL figure rebuild: corrections to the analysis framework (29 October 2026)

Side project for the MacBean and Moore growing season length (GSL) manuscript, to run in fluxnet-annual-2026 at GSL_definition/. The framework itself ("GSL Figure Rebuild: Analysis Framework", Dave, 29 Oct) was pasted into chat; Claude Code saves it to GSL_definition/docs/framework.md. This note records what was checked and what should change before work starts.

## Checked against the repo (29 Oct)

- The Table S1 site list sums to 505 site-years; groups are grassland 17, evergreen forest 20, deciduous forest 12, woody-grass mosaic 12, mixed forest 1.
- 21 sites have 10 or more years (the Figure 3 trend set).
- All 62 sites are in fluxnet_shuttle_snapshot_20260920T102211.csv, with matching IGBP classes, and every listed year lies within the site's first and last year. Hubs: ICOS 33, AmeriFlux 23, TERN 6.
- Product IDs changed between the 1 and 20 September snapshots at FR-Mej, IT-Tor and NL-Vkp.

## Corrections

1. **Filter to FLUXMET rows.** The pre-QC daily table in DuckDB holds two rows per day, dataset = 'FLUXMET' and dataset = 'ERA5'. ERA5 rows carry meteorology only. Every query must filter to FLUXMET, or P99, skewness and GSL are computed on the wrong rows. The framework does not mention this.
2. **Logging conflict.** log_exclusion() and log_unknown() in R/utils.R write to the repo-root outputs/ folder at a fixed path, which the side project must not touch, and 04_qc.R clears those files at the start of each run. Fix: add an optional log_path argument to both functions, defaulting to the current path so the main pipeline is unchanged, and pass GSL_definition/logs/ paths from the side project.
3. **Do not stop on product differences.** Step 2 says to stop if any product differs from Tash's 17 August snapshot. At least three sites changed between our 1 and 20 September snapshots, so differences are likely. Instead: list and flag changed sites, continue, and drop flagged sites from the match checks only.
4. **Sequence around Tash's files.** Her snapshot, screened daily table (or code), and the negative-GPP and > versus >= answers are needed for step 4, not steps 1 to 3. Request them now. In the meantime, apply her screening rule (NEE_VUT_REF_QC ≥ 0.25 on every day of a year, at least 5 complete years, US-Aud excluded) to every site in our snapshot. It should return exactly her 62 sites and 505 site-years; any difference is reported site by site, not tuned away.
5. **Year screening (open decision 1).** Option (a), scaling GSL by 365 over valid days, assumes days failing QC behave like passing days. QC failures cluster by season (snow, wet seasons, winter), and GSL is itself a count of seasonal days, so the scaling would bias it. Proposed alternative, option (d): count GSL on the full gap-filled daily series (GPP_NT_VUT_REF has a value every day) and use the 0.50 project standard to decide which years are trusted, e.g. keep a year when at least 80% of its days pass, with NA flags counted as not passing. Option (b), all days passing 0.50, stays as a stricter sensitivity check. Not yet decided by Dave; stage 1 reports the counts under each option so the choice can be made from numbers.
6. **Trend set may shrink.** Under project-standard screening some of the 21 trend sites may fall below 10 years. Figure 3 must report any that drop out.
7. **Run size.** Daily data for 62 sites runs in minutes; no background job is needed.

## Staging

1. Stage 1 (prompt drafted 29 Oct): scaffold, framework saved, logging argument, site extraction and product comparison, both screenings, year-screening option counts, Tash-rule reproduction across the whole snapshot. No derived quantities, no figures.
2. Stage 2, after Tash's files arrive: derived quantities and match checks (framework steps 3 to 4).
3. Stage 3, after Dave signs off the reproduction report: figures and benchmark table.
