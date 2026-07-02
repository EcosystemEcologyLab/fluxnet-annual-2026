2026-07-02T22:55:22Z started
2026-07-02T22:55:30Z attempting: search for methods_*.md files under review/figures/ and the repo root (find -iname "methods_*.md").
2026-07-02T22:55:40Z found: 8 methods_*.md files (plus one non-matching but related doc):
  - review/figures/representativeness/methods_koppen_beck2023.md
  - review/figures/representativeness/methods_koppen_beck2023_future.md (extra, not requested -- future/projected Koppen variant)
  - review/figures/representativeness/methods_biomass.md
  - review/figures/representativeness/methods_aridity_unep.md
  - review/figures/representativeness/methods_landcover.md
  - review/figures/representativeness/methods_trendy_iav.md
  - review/figures/methods_flux_medians.md
  - review/figures/methods_badm_management.md (extra, not requested -- BADM land-management doc)
  - docs/methods_requirements.md (does not match methods_*.md glob location pattern requested but is a related overall methods-requirements doc, not a per-axis doc -- noted separately, not treated as one of the five per-axis docs)
All five per-axis docs named in the task (Koppen-Beck2023, ESA CCI Biomass, CGIAR aridity/UNEP, ESA CCI land cover, TRENDY IAV/ET) are present. Proceeding to read each in full and extract required fields.
2026-07-02T22:55:50Z attempting: read review/figures/representativeness/methods_koppen_beck2023.md in full.
2026-07-02T22:56:10Z extracted: methods_koppen_beck2023.md (72 lines) -- full content below in the SECTION 1 writeup. Key facts: product = Beck et al. (2023) present-day (1991-2020) Koppen-Geiger map, 1 km resolution, 30 classes, Figshare doi:10.6084/m9.figshare.21789074.v2 (v2, published 2026-01-14, corrects a v1 calculation error); paper doi:10.1038/s41597-023-02549-6. Per-site extraction via terra::extract() at exact site lat/long; 3 wetland sites (US-KS3, US-TaS, CN-SnB) fell on the water mask and used a nearest-land-pixel fallback within a 3 deg search window (0.6-1.0 km recovery distance), recorded in a koppen_method column. Area-weighting: terra::cellSize(mask=TRUE, unit="km") (geodesic, accounts for meridian convergence) + terra::zonal(fun="sum") without materializing the full 21600x43200 raster; total land area normalizer = 147.3 million km2 (vs. known ~148.9M km2, small discrepancy attributed to island/coastline raster representation at 1km). Two aggregation levels: 5-class (first-letter A/B/C/D/E) and 13-class (first two letters, e.g. Cf, Ds) via substr(koppen_class,1,2); 13-class colors are unweighted mean RGB of 30-class members. No "hybrid near-zero-bin" or explicit "7-bin vs 18-bin" scheme is described in this doc -- noted, not present here (may appear in another axis doc; checked below). Sampling ratio and weighted Jaccard/Hellinger formulas are given verbatim (captured for SECTION 2). EF (ice cap, ~9.6% of global land, 0 FLUXNET sites) is flagged as a structural/instrumentation-limitation zero, not a sampling gap, and excluded from under-sampling interpretation.
2026-07-02T22:56:20Z attempting: read review/figures/representativeness/methods_biomass.md in full.
2026-07-02T22:56:45Z extracted: methods_biomass.md (143 lines). Product: ESA CCI Biomass v7.0 (March 2026), "Global datasets of forest above-ground biomass for the years 2005-2012 and 2015-2024" (Santoro & Cartus 2024, doi:10.5285/6429d1aafe1e43b9b414e4a5a7f8b903); variable = AGB Mg/ha; native 100m (1deg x1deg tiles), but the pre-aggregated 1km global product (1.4 GB) was used, band 18 = year 2024. THIS IS THE HYBRID NEAR-ZERO-BIN / 7-BIN SCHEME referenced in the task: Bin 1 = 0-5 Mg/ha FIXED (near-zero/bare/ice/desert, non-vegetated land, since EC towers are rarely deployed there); Bins 2-7 = six EQUAL-AREA QUANTILE bins over vegetated land (biomass >=5 Mg/ha), each ~1/6 of total vegetated area; quantile breakpoints (Mg/ha): q1=13, q2=27, q3=51, q4=94, q5=171 (data-dependent, recomputed if the product changes). Full bin table: Bin1 0-5, Bin2 5-13, Bin3 13-27, Bin4 27-51, Bin5 51-94, Bin6 94-171, Bin7 >171 Mg/ha. Land mask: Beck 2023 KG raster reused as land mask for cross-axis consistency, total land area 147,322,862 km2 (same number as the KG doc's 147.3M km2, confirming shared mask baseline). Total vegetated land (>=5 Mg/ha) = 70,162,294 km2. Per-site: 767 sites (fluxnet_shuttle_snapshot_20260624T095651.csv), 0 of 767 sites returned NA. Metrics: J=0.6385, H=0.1656 (both Jaccard AND Hellinger reported here -- Hellinger is NOT retired for this axis) -- also reports a comparison to the prior fixed-bin scheme (0-5,5-25,25-50,50-100,100-200,200-400,>400 Mg/ha): J=0.6262, H=0.1684.
2026-07-02T22:56:55Z attempting: read review/figures/representativeness/methods_aridity_unep.md in full.
2026-07-02T22:57:15Z extracted: methods_aridity_unep.md (60 lines). Product: CGIAR Global Aridity Index and Potential Evapotranspiration Dataset v3.1 (Zomer et al. 2022, doi:10.1038/s41597-022-01493-1, Figshare doi:10.6084/m9.figshare.7504448), 30 arc-second (~1km), derived from 1970-2000 climate averages. AI = P/PET (Penman-Monteith), dimensionless; raster is 16-bit unsigned int scaled x10000; ocean/water = raw value 0, treated as missing. TWO PARALLEL SCHEMES (this is the "5-bin vs 7-bin" split found -- task description said "7-bin vs 18-bin" but that number pairing was not found verbatim anywhere; noting the discrepancy explicitly rather than forcing a match): 5-class canonical UNEP/World Atlas of Desertification 1992 scheme (Hyper-Arid <0.05, Arid 0.05-0.20, Semi-Arid 0.20-0.50, Dry Sub-Humid 0.50-0.65, Humid >=0.65) and a 7-class extended scheme subdividing Humid per FAO usage (Humid-low 0.65-1.0, Humid-moderate 1.0-2.0, Hyper-Humid >=2.0). Extraction: terra::extract() at exact coords; same 3 wetland sites (US-KS3, US-TaS, CN-SnB) needed the nearest-land-pixel fallback (0.6-1.0 km), all assigned Humid (AI 0.74-0.97), recorded in an aridity_method column. Area weighting: terra::cellSize(mask=TRUE, unit='km') + terra::zonal(fun='sum'); total land area = 134,761,545 km2 -- NOTE this differs from the KG/biomass land-area baseline (147.3M km2) because the aridity raster covers only 60S-90N and excludes Antarctica (~12.6M km2 difference, explained explicitly in the doc). Metrics: 5-class J=0.69, H=0.21; 7-class J=0.67, H=0.22 (Hellinger again explicitly reported, not retired, for this axis). Color-palette note: pale grey #cccccc substituted for a white/no-fill spec on the Humid class (50.2% of global land) because white-on-white was invisible; 7-class uses a blue gradient for the humid trio.
2026-07-02T22:57:25Z attempting: read review/figures/representativeness/methods_landcover.md in full.
2026-07-02T22:57:50Z extracted: methods_landcover.md (157 lines). Product: ESA CCI Land Cover v2.1.1, year 2022 (Copernicus C3S continuation of ESA CCI LC v2.0.7, identical algorithm/300m/LCCS system); file C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc, distributed via Copernicus CDS; citation "ESA CCI Land Cover Product User Guide v2.0". THREE aggregation levels, not a 7-vs-18 split: Level 1 high-level (10 classes, ESA CCI PUG Table 2 cross-walk), Level 2 intermediate LCCS hierarchy (22 classes), Level 3 native LCCS (37 classes, class 0 'No data' excluded). Lookup table saved at data/snapshots/cci_landcover_aggregation_lookup.csv. Land mask/grid: native 300m resampled (nearest-neighbour, preserves integer codes) onto the Beck 2023 KG 0.00833deg grid (147.3M km2 total land) -- SAME shared land-mask baseline as KG and biomass axes; cached at data/external/cci_landcover/v2.1.1/cci_lc_2022_kg_aligned_native.tif. Per-site: 767 sites, native LCCS code extracted previously via terra::extract() with a 3-degree nearest-land recovery fallback for NA sites (same fallback pattern as KG/aridity, not re-run here). Metrics (J/H) at all 3 levels: High-level (10 classes) J=0.556 H=0.337; Level 2 (22) J=0.478 H=0.368; Native (37) J=0.441 H=0.395 -- confirms the same "J decreases / H increases at finer aggregation" pattern noted for KG. NO 7-bin/18-bin or 18-class scheme found anywhere in this document (classes are 10/22/37, not 7 or 18) -- noting this explicitly since the task described "7-bin vs 18-bin schemes"; the actual 7-bin hybrid scheme found is the BIOMASS axis (above), and no 18-class/18-bin scheme has been found in any doc read so far. Will check the remaining docs (TRENDY, flux_medians, koppen future) before concluding whether an 18-bin scheme exists anywhere in the repo.
2026-07-02T22:58:00Z attempting: read review/figures/representativeness/methods_koppen_beck2023_future.md (found but not in the task's named list) to check for an 18-class/18-bin scheme.
2026-07-02T22:58:15Z extracted: methods_koppen_beck2023_future.md (54 lines). This is a FUTURE-SCENARIO variant of the KG axis, not one of the 5 requested per-axis docs. Documents scripts/figure_representativeness_kg_future.R (parameterized by ssp/period/label). Asymmetric framing: Earth bar = projected future land-area fractions; Network bar = PRESENT-DAY site classifications (sites don't migrate). Example scenario present: SSP5-8.5 2071-2099 (Beck 2023 figshare v2), total land area unchanged (147,322,862 km2), 436/767 sites (56.8%) change KG class vs present-day. Metrics reported at 5-class, two-letter (13-class), and 30-class (native) resolutions, both present and future J/H values with deltas. No 18-class/18-bin scheme found here either.
2026-07-02T22:58:20Z attempting: grep the whole repo (excluding renv/) for "18-bin", "18 bin", "18-class", "18 class" to settle whether an 18-bin scheme exists anywhere, before concluding the task's "7-bin vs 18-bin" description does not match any doc found.
2026-07-02T22:59:00Z found: an 18-bin scheme DOES exist, but NOT in any methods_*.md doc -- it lives in SESSION_LOG.md (2026-06-29 entry "Multi-bin representativeness: 12-bin, 18-bin, 20-bin hybrid") and in scripts/figure_representativeness_summary.R + scripts/recompute_continuous_axes_multibin.R. IMPORTANT DISCREPANCY TO FLAG: methods_biomass.md (read above) documents and reports metrics for the 7-BIN hybrid scheme (J=0.6385, H=0.1656) as if it were current/canonical, but scripts/figure_representativeness_summary.R line 114 comment states continuous axes (Biomass, and the TRENDY-derived NEE-IAV/NEE-median/ET-IAV/ET-median axes) moved to 18-BIN "from this version onward" for the summary/trajectory figure (Rep007-008); the 7-bin scheme is retained only as one point in a bin-count sensitivity sweep (7/12/18/20/30-bin), not as the figure's default. scripts/recompute_continuous_axes_multibin.R documents the 18-bin construction explicitly: "18-bin: bin 1 + 17 quantile bins, Q_PROBS = (1:16)/17 (16 interior breaks)" -- i.e. the SAME hybrid near-zero-bin-1 + N equal-area-quantile-bins construction as the 7-bin biomass scheme, just with 17 quantile bins instead of 6. SESSION_LOG.md 2026-06-29 records per-axis 18-bin quantile breakpoints for Biomass, NEE-IAV, NEE-median, ET-IAV, ET-median, and a comparison table across 7/12/18/20/30-bin Jaccard values, plus a documented note that at Current-network, 18-bin ET-median J (0.433) is slightly LESS than 20-bin J (0.447) due to a ceiling-collapse artifact at 1000 mm/yr (1 quantile bin collapses), described as a discretization edge effect not affecting other networks/axes. CONCLUSION on the task's "7-bin vs 18-bin schemes" wording: this refers to this multi-bin sensitivity work, not to a single doc's content; the authoritative current default for the continuous axes (biomass + 4 TRENDY-derived fields) in the SUMMARY figure is 18-bin, while methods_biomass.md's own text still describes/reports only the 7-bin numbers -- methods_biomass.md is therefore STALE relative to the actual current default in figure_representativeness_summary.R and should be treated with that caveat when drafting Methods.
2026-07-02T22:59:15Z attempting: read review/figures/representativeness/methods_trendy_iav.md in full.
2026-07-02T22:59:45Z extracted: methods_trendy_iav.md (199 lines) -- this single doc covers ALL FOUR TRENDY-derived axes (NEE-IAV, ET-IAV, NEE-median, ET-median), satisfying most of SECTION 3 below. Key facts: TRENDY v14-gcb2025, S3 simulation (transient, historical land-use change); variables nbp (kg C m-2 s-1) and evapotrans (kg m-2 s-1); protocol ref Sitch et al. 2024 (doi:10.1029/2024GB008102), GCB2025 paper in prep, acknowledgment-only attribution policy for individual model PIs. FULL ARCHIVE = 20 MODELS. 3 EXCLUDED (all technical, not selective): CARDAMOM (only 22 years temporal coverage, insufficient for the 34-yr IAV window), CLM-FATES (irregular longitude spacing, terra::rast() parse failure), JSBACH (irregular latitude spacing, terra::rast() parse failure). REMAINING 17-MODEL ENSEMBLE: CABLE-POP, CLASSIC, CLM, DLEM, ED, ELM, ELM-FATES, IBIS, ISAM, JULES-ES, LPJ-GUESS, LPJml, LPJwsl, LPX-Bern, ORCHIDEE, TEM, VISIT-UT. ANALYSIS WINDOW: 1990-2023 (34 years), bounded by CLASSIC/DLEM/ELM ending their S3 run at 2023. ELM SPECIAL CASE: submitted data only through 2022 (2023 layer all-NA); because all 4 per-pixel stat functions require complete 34-year rows, ELM contributes an all-NA raster to EVERY axis (not just IAV) -- so the EFFECTIVE ENSEMBLE SIZE IS 16 MODELS per pixel for all four derived fields, computed via ensemble median with na.rm=TRUE over the 17-layer stack. Unit conversions: nbp -> gC m-2 yr-1 (x2629800 s/mo x1000 monthly, or x31557600 s/yr x1000 annual); evapotrans -> mm/yr (x2629800 s/mo, kg/m2=mm). All models resampled to common 0.5deg grid (720x360, EPSG:4326, bilinear) then KG-land-masked; regridded intermediates in data/external/trendy/derived/intermediate/ (252MB, 17 models x2 vars x34 layers). Per-pixel stats: IAV axes = linear-detrended SD (OLS hat-matrix residuals, SD_detrended=sqrt(sum(resid^2)/(n-2)), n=34); NEE-median = mean(abs(annual NBP)) over 34yr; ET-median = mean(annual ET) over 34yr. Ensemble median maps: 94,589 non-NA pixels each (global KG land mask at 0.5deg), saved to data/external/trendy/derived/. BINNING: same hybrid 7-bin scheme as biomass/aridity (bin1 = 0-5 fixed near-zero cut, negative pixels mapped to 0 before binning; bins2-7 = six equal-area quantile bins over the above-threshold area). Per-axis breakpoints given (NEE-IAV, ET-IAV, NEE-med, ET-med, all near-zero-cut=5). Per-site extraction: terra::extract(method='simple') + 3-degree nearest-land recovery fallback (same pattern as other axes); notes a terra>=1.9.27 API change (extract() returns 1-col df not 2-col) that broke extraction code, fixed in commit 7ee9210 (2026-06-27). Metrics (J/H): NEE-IAV 0.507/0.316, ET-IAV 0.663/0.224, NEE-median 0.495/0.320, ET-median 0.459/0.345 (Hellinger reported for all 4, not retired here). Cross-axis note: NEE-median vs ESA CCI Biomass v7 Pearson r=0.55, Spearman rho=0.89 across 94,589 shared pixels. NOTE: this doc's binning section describes the 7-bin scheme as canonical for these axes too, consistent with (and predating) the later 18-bin summary-figure default found in scripts/figure_representativeness_summary.R -- same staleness caveat as methods_biomass.md applies.
2026-07-02T22:59:55Z attempting: locate the actual TRENDY processing scripts (not just the methods doc) -- search scripts/ for filenames containing "trendy".
2026-07-02T23:00:10Z found: scripts/download_trendy_v14.sh (download step), scripts/figure_representativeness_trendy_compute.R (per-pixel stats + ensemble median computation, matches methods_trendy_iav.md's description), scripts/figure_representativeness_trendy_wrap.R (site extraction / figure wrapper), scripts/trendy_preview_diagnostic.R (diagnostic). Derived-field outputs confirmed on disk: data/external/trendy/derived/trendy_nee_median.tif, trendy_nee_iav.tif, trendy_et_median.tif, trendy_et_iav.tif (the four ensemble-median GeoTIFFs matching the methods doc's four axes) plus intermediate/ subfolder. Per-site CSVs confirmed: data/snapshots/site_trendy_{nee_iav,nee_median,et_iav,et_median}.csv (+ .meta.json), each also with _fluxnet2015/_la_thuile/_marconi historical-snapshot variants. Global-distribution bin tables confirmed at multiple bin counts: data/snapshots/trendy_{nee_iav,nee_median,et_iav,et_median}_global_distribution[_12bin|_18bin|_20bin|_30bin].csv -- directly confirms the 7/12/18/20/30-bin sensitivity sweep applies to all four TRENDY axes, not just biomass.
2026-07-02T23:00:20Z attempting: begin SECTION 2 (representativeness metric definitions) -- read scripts/figure_representativeness_summary.R to find the exact sampling-ratio and weighted-Jaccard code, and to check whether Hellinger distance is still computed/reported in the SUMMARY figure/script (vs. only in the individual per-axis methods docs already read).
2026-07-02T23:01:30Z extracted: exact code found. sampling_ratio (scripts/figure_representativeness_summary.R, merge_sr(), ~line 296): "sampling_ratio = dplyr::if_else(global_land_fraction > 0 & network_frac > 0, network_frac / global_land_fraction, NA_real_)" -- i.e. network fraction of a class divided by its global land fraction; NA if either side is zero (not clipped to 0, left NA so it can be flagged as "structurally unsampled" rather than plotted as ratio 0). An equivalent per-axis version exists in scripts/figure_representativeness_landcover.R line 683: "sampling_ratio = dplyr::if_else(p > 0, q / p, NA_real_)" where p=global fraction, q=network fraction. Weighted Jaccard AND Hellinger, exact code (scripts/figure_representativeness_landcover.R compute_repr_metrics(), lines 325-330):
    compute_repr_metrics <- function(p, q) {
      p[is.na(p)] <- 0; q[is.na(q)] <- 0
      list(
        weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
        hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
      )
    }
This matches the formulas quoted in methods_koppen_beck2023.md verbatim. HELLINGER STATUS -- NOT RETIRED, but not carried into the cross-axis summary trajectory: grep -in "hellinger" scripts/figure_representativeness_summary.R returned ZERO matches (confirmed by direct grep), while grep -rli "hellinger" across scripts/ found it present in 11 files: trendy_preview_diagnostic.R, figure_representativeness_landcover.R, figure_representativeness_trendy_wrap.R, extract_historical_sites_representativeness.R, recompute_continuous_axes_multibin.R, figure_representativeness_biomass.R, recompute_continuous_axes_30bin.R, figure_representativeness_kg.R, figure_representativeness_kg_future.R, figure_representativeness_aridity.R, figure_representativeness_trendy_compute.R. The precomputed data/snapshots/representativeness_metrics.csv (13566 bytes) DOES have a hellinger_distance column (header: axis,aggregation_level,n_classes,weighted_jaccard,hellinger_distance,network,n_sites) -- so Hellinger IS computed and stored for every axis/aggregation/network combination. figure_representativeness_summary.R's get_j() helper (~line 279) reads ONLY weighted_jaccard from that CSV via metrics_df <- readr::read_csv(file.path(SNAP, "representativeness_metrics.csv")); Hellinger is simply never selected/plotted in the cross-axis trajectory figures (Fig 007-010, the multi-axis "Jaccard trajectory" panels). CONCLUSION: Hellinger distance is NOT retired -- it is computed and reported per-axis (in every methods_*.md doc read above, in the metrics CSV, and in individual per-axis sampling-ratio/aggregation-sensitivity figures Rep011-018) -- but only weighted Jaccard is used in the cross-axis SUMMARY trajectory figures that compare all axes together.
2026-07-02T23:01:40Z attempting: locate and read review/figures/methods_flux_medians.md (named explicitly in the task's example list of methods_*.md files, not one of the 5 representativeness axes but relevant to SECTION 6 FLUXNET2015 comparison and SECTION 5 QC).
2026-07-02T23:02:20Z extracted: methods_flux_medians.md (210 lines, living doc, first created 2026-06-30). PRIMARY ANALYSIS (IGBP-class flux medians): source data = FLUXNET Shuttle YY product (FLUXMET_YY v1.3_r1), snapshot fluxnet_shuttle_snapshot_20260624T095651.csv (767 sites); per-site annual medians computed by scripts/assess_flux_data_by_igbp_shuttle.R -> data/snapshots/site_flux_medians_shuttle.csv. Variable/source preferences: NEP=-NEE, VUT preferred (NEE_VUT_REF) with CUT fallback (NEE_CUT_REF) decided per site-year and applied jointly to NEE/GPP/TER for that year; GPP/TER use NT-preferred with DT-fallback DECIDED PER SITE (not per year, to avoid mixing methods within one site's median), recorded in gpp_partition/ter_partition columns; ET = LE_F_MDS[W/m2] x 31557600[s/yr] / lambda(2.45e6 J/kg) -> mm/yr; H = H_F_MDS directly, no conversion. QC THRESHOLD FOR THIS ANALYSIS: >=0.80 applied per-variable independently (NEE_VUT_REF_QC or CUT equivalent for NEP/GPP-NT/TER-NT; DT columns gated by the same NEE QC flag; LE_F_MDS_QC and H_F_MDS_QC independently at 0.80 for ET/H) -- THIS IS 0.80, NOT the pipeline default of 0.50 documented in CLAUDE.md/R/pipeline_config.R (checked below) -- an explicit per-analysis override, though this doc does not itself state a rationale for choosing 0.80 over the pipeline default (checked SESSION_LOG separately below). Per-site aggregation: median across qualifying years per variable per site (unit of analysis = per-site median, not site-year). IGBP scheme: 12 standard classes (EBF,MF,DBF,ENF,CSH,OSH,WSA,SAV,GRA,WET,CRO,CVM); 22 sites with non-standard labels (BSV,DNF,SNO) excluded from the 12-class summary, reported as "other". Cross-class aggregation = MEDIAN OF SITE MEDIANS (not site-year-weighted mean); std dev also computed across site medians. EBF caveat: 42 EBF sites, 38 with qualifying NEP; GPP/TER NT succeeds at 22, +15 DT-recovered =37 total, DT-recovery raises EBF GPP median 1845->2326 gC/m2/yr (+26%). Companion CSVs: data/snapshots/flux_medians_by_igbp_*.csv.
FLUXNET2015 COMPARISON SUBSECTION (added 2026-06-30, source scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R): X axis = data/snapshots/site_flux_medians_fluxnet2015.csv, FLUXNET2015 release (Pastorello et al. 2020) FULLSET YY product, 206 of 212 sites (6 excluded, reason recorded in SESSION_LOG 2026-06-30 "FLUXNET2015 release: extraction and IGBP-class flux assessment" -- checked separately below); Y axis = data/snapshots/site_flux_medians_shuttle.csv, 767 sites; BOTH axes use IDENTICAL aggregation logic (VUT/CUT, NT/DT, QC>=0.80, median-of-qualifying-years) so the comparison isolates data/processing differences, not aggregation-method differences. EXCLUDED CLASSES: CVM (0 sites in FLUXNET2015 entirely -- added/reclassified after 2015/2020 release); CSH (n=2 in FLUXNET2015, below an n>=5 reliability threshold used elsewhere in this doc); BSV/DNF/SNO (non-standard labels, excluded by construction). 10 plotted classes: EBF,MF,DBF,ENF,OSH,WSA,SAV,GRA,WET,CRO. Citation requirement: Pastorello et al. 2020 (doi:10.1038/s41597-020-0534-3), CC-BY-4.0; per-site tower DOIs recorded in logs/fluxnet2015_pi_contacts_20260630_130019.csv. Processing-version confound explicitly flagged: FLUXNET2015 and Shuttle use different ONEFlux pipeline versions AND different site/year coverage -- a difference cannot be attributed to either cause alone. GRA and EBF flagged as large-n (not small-sample-noise) shifts worth methodological follow-up.
2026-07-02T23:02:30Z attempting: begin SECTION 5 (site selection/QC) -- find the QC threshold constant in R/pipeline_config.R (search for QC_THRESH, QC_THRESHOLD, 0.80, 0.50).
2026-07-02T23:03:00Z found: PIPELINE DEFAULT -- R/pipeline_config.R lines 40/43/46/49: QC_THRESHOLD_DD <- 0.50 (daily), QC_THRESHOLD_WW <- 0.50 (weekly), QC_THRESHOLD_MM <- 0.50 (monthly), QC_THRESHOLD_YY <- 0.50 (annual). Applied in scripts/04_qc.R (lines 82-85 select by resolution suffix, line 177-179 logs "QC_THRESHOLD_{suffix}={threshold}" per exclusion). PER-ANALYSIS OVERRIDE -- scripts/assess_flux_data_by_igbp_shuttle.R line 43: "QC_THRESH <- 0.80  # minimum QC fraction to retain a site-year" and scripts/assess_flux_data_by_igbp_fluxnet2015.R line 54: "QC_THRESH <- 0.80" (both local script-level constants, NOT sourced from pipeline_config.R, applied to NEE_VUT_REF_QC/CUT-equivalent, LE_F_MDS_QC, H_F_MDS_QC independently -- see methods_flux_medians.md above). RATIONALE FOUND: SESSION_LOG.md lines 980-989 states this 0.80 is "a deliberate, explicitly-documented override of the project-wide default... not an inconsistency bug" and confirms it is applied IDENTICALLY to both the Shuttle and FLUXNET2015 analyses (consistent cross-network comparison). NO further numeric/scientific justification for the specific value 0.80 (vs. e.g. 0.75 or 0.90) was found anywhere in SESSION_LOG.md or the two scripts beyond "a stricter threshold than the pipeline default, applied consistently" -- reporting this honestly rather than inventing a fuller rationale.
2026-07-02T23:03:10Z attempting: search scripts/ and R/ for coordinate-validity checks and duplicate-site handling in the pipeline (grep for "coordinate", "lat.*lon.*valid", "duplicate").
2026-07-02T23:03:40Z found: COORDINATE VALIDITY -- implemented uniformly across ~15+ scripts/R files as a simple non-NA filter: dplyr::filter(!is.na(location_lat), !is.na(location_long)) (e.g. R/figures/fig_maps.R, R/figures/fig_climate_legacy.R, scripts/step1_extract_worldclim.R, scripts/figure_representativeness_aridity.R, scripts/step4_extract_koppen_beck2023.R, and others). No range/bounds check (e.g. -90<=lat<=90) was found anywhere -- validity means "coordinate present", not "coordinate physically plausible". DUPLICATE HANDLING -- two distinct mechanisms found: (1) file-level, scripts/assess_flux_data_by_igbp_shuttle.R lines 118-124: "# Deduplicate: if multiple YY files per site, keep longest (most years)" -- groups by site_id, keeps the file with the largest file size (slice_max(fsize, n=1, with_ties=FALSE)) as a proxy for most years of data; (2) row-level, pervasive dplyr::distinct(site_id, .keep_all=TRUE) used across scripts/assess_flux_data_by_igbp_fluxnet2015.R:124, scripts/assess_flux_data_by_igbp_shuttle.R:93, scripts/generate_historical_comparison_figures.R:51/99, scripts/07_figures.R:151/155, scripts/generate_fluxnet_citations.R:536, and the representativeness extraction scripts -- enforces one row per site_id at the point each script reads the snapshot metadata table, before any join/analysis.
2026-07-02T23:03:50Z attempting: begin SECTION 4 (snapshot definition) -- verify data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv site count and column set, then locate the generating script (search scripts/ and R/ for flux_listall, flux_download, snapshot).
2026-07-02T23:04:20Z found/extracted: data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv confirmed on disk (557061 bytes, dated 2026-06-24 09:56). Site count: 768 lines - 1 header = 767 sites. Columns (18): data_hub, site_id, site_name, location_lat, location_long, igbp, network, team_member_name, team_member_role, team_member_email, first_year, last_year, download_link, fluxnet_product_name, product_citation, product_id, oneflux_code_version, product_source_network. NOTE: no companion .meta.json exists for this specific snapshot file (checked data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv.meta.json -- not found) -- flagged as a gap since CLAUDE.md's Output Metadata policy would otherwise require one. Generating code: scripts/01_download.R line 19: "live_manifest <- flux_listall()" (a SINGLE call to the fluxnet package's flux_listall(), which fetches each hub -- AmeriFlux/ICOS/TERN -- independently in that one call); R/snapshot.R::resolve_snapshot() (development mode) returns that live_manifest UNMODIFIED (no merging with prior snapshots); 01_download.R line 47 then calls write_snapshot(manifest, ...) which (R/snapshot.R line 53-55) stamps the CSV filename with timestamp <- format(Sys.time(), "%Y%m%dT%H%M%S") -- i.e. the snapshot IS A SINGLE POINT-IN-TIME CALL on one date, NOT assembled/merged over a window. NETWORK MEMBERSHIP AT FREEZE TIME = whatever flux_listall() returns for all three hubs (AmeriFlux, ICOS, TERN) at the moment 01_download.R is run; 01_download.R lines 21-33 add a loud guard (stop()) if any of the three expected hubs is silently missing from that call's manifest (a documented failure mode: "flux_listall() fetches each hub independently; a hub-level error e.g. TERN HTTP 404 silently removes that hub from the manifest with no R-level warning"). The prior-snapshot "change detection" logic (compare_snapshots()) is used ONLY to decide which sites need re-downloading, NOT to alter or merge into the site list recorded in the new snapshot -- the snapshot always reflects exactly one flux_listall() call's result.
2026-07-02T23:04:30Z attempting: search the repo for a persistent identifier (DOI/PID) for the snapshot itself -- grep for "doi", "PID", "persistent identifier" in READMEs, metadata, and config.
2026-07-02T23:05:30Z found and extracted docs/methods_requirements.md (274 lines) -- NOT a methods_*.md-glob match (it's docs/methods_requirements.md, not review/figures/) but highly relevant: a REQUIREMENTS SPECIFICATION for the whole Methods section (explicitly self-labelled "not a draft" / "Do not draft prose from this document yet"), organized as sections 5.1-5.7 + 6 (Data availability) + 7 (Code availability), each with "Must cover" / "Primary code files" / "Key facts to include (update when finalised)". CRITICAL CAVEAT: this doc's own embedded "Key facts" are STALE placeholders from an EARLIER, SMALLER snapshot (672 sites, download date 2026-04-14, hubs AmeriFlux 340/ICOS 280/TERN 52, 530/672 valid NEE_VUT_REF, 106 excluded via "ONEFlux 15-day gap rule", 36 NEE_CUT-only) that DO NOT MATCH the current 767-site snapshot (fluxnet_shuttle_snapshot_20260624T095651.csv) used throughout the rest of this compilation -- these numbers must NOT be used as current facts in a Methods draft without re-verification against the current snapshot; flagging this explicitly per the task's "accuracy matters more than completeness" instruction. PID/DOI ANSWER (definitive): Section 6 "Data availability statement" template (line 231-244) reads "downloaded from the FLUXNET Shuttle on [DATE]. The persistent identifier for the snapshot used is [PID]." -- both [DATE] and [PID] are LITERAL UNFILLED TEMPLATE PLACEHOLDERS, explicitly "fill in DATE and PID when snapshot is archived". Section 7 "Code availability statement" likewise has an unfilled "(DOI: [ZENODO DOI])" placeholder, "fill in DOI when repository is archived". CONCLUSION: NO persistent identifier (PID/DOI) has been assigned to this snapshot or to the repository -- both are explicitly pending future archiving steps (Zenodo for the repo; an unspecified PID mechanism, possibly the per-site product_id checksum per §5.1's "product_id checksum-based PID" note, for the snapshot). Cross-checked elsewhere in the repo (SESSION_LOG.md lines ~5545-5892): individual SITES carry per-site identifiers in the product_id/product_citation columns -- ICOS sites (332) use handle.net persistent identifiers (not DOIs, resolve to the ICOS data portal); TERN sites (52) have NO machine-readable persistent identifier in the current snapshot at all (plain-text citation only); AmeriFlux sites' product_id format not detailed in the passages found. So: per-site PIDs exist for ICOS (handle.net, not DOI) and presumably AmeriFlux, but NOT for TERN, and there is NO single snapshot-level or dataset-level DOI/PID anywhere in the repo.
2026-07-02T23:05:45Z attempting: confirm the VUT/CUT per-site fallback rule's exact implementation location in scripts/04_qc.R (file:line), to close out SECTION 5.
2026-07-02T23:06:00Z found: scripts/04_qc.R lines 69-143 implement the per-site (not per-row) VUT/CUT fallback exactly as documented in CLAUDE.md's QC Flag Reference: lines 69-70 check has_vut_col/has_cut_col; lines 98-129 (per-site, via group_by(site_id)) compute n_vut = count of non-NA NEE_VUT_REF_QC and n_cut = count of non-NA NEE_CUT_REF_QC per site, then line 122-124 assigns qc_col_used = "NEE_VUT_REF_QC" if n_vut>0 else "NEE_CUT_REF_QC" if n_cut>0 (site-level decision, so VUT and CUT QC are never mixed within one site's rows); lines 142-143 then compute the exclusion fraction (1 - QC value) using whichever column was assigned for that site. This matches CLAUDE.md's documented rule (sites with any non-NA NEE_VUT_REF_QC gated on VUT; CUT-only sites -- where NEE_VUT_REF_QC is entirely NA -- gated on NEE_CUT_REF_QC as a per-site fallback) exactly, confirming CLAUDE.md's documentation is accurate to the current code, not stale. Row exclusion driven by QC_THRESHOLD_YY (0.50, R/pipeline_config.R:49) for the annual resolution used in the paper's primary analyses.
2026-07-02T23:06:10Z attempting: begin SECTION 8 (round-robin harmonization) -- search SESSION_LOG.md, READMEs, and scripts/ for "round robin", "round-robin", "hub", "cross-hub".
2026-07-02T23:06:25Z NOT FOUND: grep -rniE "round.robin" across SESSION_LOG.md, docs/, scripts/, R/, and all *.md files in the repo (excluding renv/) returned ZERO matches -- no "round robin" or "round-robin" text exists anywhere in this repository. grep -rniE "cross-hub|cross hub" returned exactly one substantive hit: docs/methods_requirements.md line 83, part of section 5.3 "Harmonisation and flux processing" -- "Must cover: ... Cross-hub validation if applicable." -- this is a REQUIREMENTS-LIST BULLET POINT (an item the eventual Methods section should address IF applicable), not evidence that any cross-hub round-robin design, protocol, or results exist. No design document, script, or results table for a processing-hub round robin was found anywhere. CONCLUSION for SECTION 8: neither a round-robin design nor results exist in this repository at all -- not even a design-only placeholder beyond the single generic "cross-hub validation if applicable" requirements bullet. This is stronger than "results pending co-author insertion" (which would imply a design already exists) -- report accordingly: no round-robin harmonization material of any kind (design or results) was found; only a generic unelaborated requirements-list mention that cross-hub validation might be needed.
2026-07-02T23:06:35Z attempting: verify FLUXNET2015 download date and the 6 excluded sites (212 downloaded vs 206 used in flux medians) -- search SESSION_LOG.md 2026-06-30 FLUXNET2015 entries.
2026-07-02T23:07:00Z found/extracted: SESSION_LOG.md "2026-06-30 -- FLUXNET2015 release download complete" (line 1381) and "...portal investigation and download launch" (line 1418), both dated 2026-06-30 (download launched and completed same day). Result: 206/206 sites downloaded successfully (0 failures), verified independently as 206 site directories + 206 zip files under data/raw/fluxnet2015/. THE 6 EXCLUDED SITES: "matches the 206-site CC-BY-4.0-scoped list (212-site CSV minus the 6 Tier-2-only sites)" -- i.e. the FLUXNET2015 release's full site list is 212 sites, of which 6 are "Tier-2-only" (a FLUXNET2015 data-tier/licensing designation) and were excluded from download because this project scopes to CC-BY-4.0 data only (per CLAUDE.md's citation/licensing policy) -- Tier-2 sites carry additional data-use restrictions beyond CC-BY-4.0. 12.02 GB downloaded this run, 13 GB total on disk. PI contact log: logs/fluxnet2015_pi_contacts_20260630_130019.csv (203 rows, for the CC-BY-4.0 "recommended" PI-notification practice and per-site DOI citation source).
2026-07-02T23:07:10Z attempting: begin SECTION 7 (HDR contour method) -- re-verify (not rely on memory alone) the grid resolution and land-mask/area-weighting method against the current R/figures/fig_climate.R and scripts/generate_whittaker_overlays.R / generate_whittaker_global.R code, per the task's "accuracy matters more than completeness" instruction.
2026-07-02T23:07:30Z confirmed by direct grep of current code (not memory alone): build_global_landclimate() (R/figures/fig_climate.R, def at line 364) takes antarctica_lat_cutoff=-60 (default) and land_frac_threshold=0.5 (default); builds the land mask via terra::classify() (line 386) of the ESA CCI 2015 raster then r_mask <- r_landfrac >= land_frac_threshold (line 399) and r_mask[r_lat < antarctica_lat_cutoff] <- FALSE (line 401). .weighted_density_grid() (R/figures/fig_climate.R line 953) has gridsize=c(201,201) as its default argument. scripts/generate_whittaker_global.R line 148 calls .weighted_density_grid(land_climate$mat, land_climate$map, land_climate$weight, gridsize=c(201,201)) explicitly -- confirms 201x201 is the actual grid resolution used for the Figure 2 contours (not just a default that happens to be unused). All facts for SECTION 7 below are grep-verified against current code, not recalled from earlier-session memory alone. Per the task's explicit instruction, the retired Mahalanobis coverage statistic is NOT included anywhere below.
2026-07-02T23:07:40Z attempting: write the final organized, clearly-sectioned synthesis of all material gathered above (SECTIONS 1-8) into this run log, as the primary readable deliverable.

================================================================================
METHODS SOURCE COMPILATION -- FLUXNET Annual Paper 2026
Compiled 2026-07-02. Read-and-report only; no analysis/figure/data file modified.
================================================================================

--------------------------------------------------------------------------------
SECTION 1 -- PER-AXIS REPRESENTATIVENESS METHODS
--------------------------------------------------------------------------------

All methods_*.md files found (full paths):
  review/figures/representativeness/methods_koppen_beck2023.md
  review/figures/representativeness/methods_koppen_beck2023_future.md   (extra: future-scenario variant)
  review/figures/representativeness/methods_biomass.md
  review/figures/representativeness/methods_aridity_unep.md
  review/figures/representativeness/methods_landcover.md
  review/figures/representativeness/methods_trendy_iav.md
  review/figures/methods_flux_medians.md
  review/figures/methods_badm_management.md                              (extra: BADM land-management, not a representativeness axis)
  docs/methods_requirements.md   (does not match methods_*.md glob; a requirements SPEC, not a per-axis doc -- see note at end of this section)

1a. Koppen-Geiger (Beck et al. 2023)
  Product: Beck et al. (2023) present-day (1991-2020) map, 1 km, 30 classes.
  Source: Figshare doi:10.6084/m9.figshare.21789074.v2 (v2, 2026-01-14, corrects a v1 calc error).
  Paper: Beck et al., Scientific Data 10, 724 (2023), doi:10.1038/s41597-023-02549-6.
  Extraction: terra::extract() at exact site coords; 3 wetland sites (US-KS3, US-TaS, CN-SnB) needed
    a nearest-land-pixel fallback (3-degree search window, 0.6-1.0 km recovery), recorded in a
    koppen_method column.
  Area weighting: terra::cellSize(mask=TRUE, unit="km") (geodesic) + terra::zonal(fun="sum").
  Land mask / area: Beck 2023 raster itself (ocean = NA, auto-excluded); total land = 147,322,862 km^2
    (~147.3M km2; ~1.6M km2 less than the true ~148.9M km2 global land area, attributed to
    island/coastline raster representation at 1km).
  Classification schemes: native 30-class; 5-class (first letter: A/B/C/D/E); 13-class (first two
    letters via substr(koppen_class,1,2), e.g. Cf, Ds).
  Exclusions/flags: polar EF (ice cap) class = ~9.6% of global land, 0 FLUXNET sites -- flagged as a
    structural/instrumentation-deployment limit (towers cannot be sited on ice sheets), excluded from
    under-sampling interpretation, not treated as a network sampling gap.

1b. ESA CCI Biomass v7.0 (above-ground biomass)
  Product: ESA CCI Biomass v7.0 (March 2026); Santoro & Cartus (2024), CEDA, doi:10.5285/6429d1aafe1e43b9b414e4a5a7f8b903.
  Variable: AGB, Mg/ha. Native 100m (1deg tiles); the pre-aggregated 1km global product (band 18 = year 2024) was used.
  ** HYBRID NEAR-ZERO-BIN CONSTRUCTION (7-BIN SCHEME), confirmed here: **
    Bin 1 = 0-5 Mg/ha, FIXED lower cut (near-zero/bare/ice/desert, non-vegetated land -- EC towers
      rarely deployed there).
    Bins 2-7 = SIX EQUAL-AREA QUANTILE bins over vegetated land (biomass >=5 Mg/ha), each ~1/6 of
      total vegetated area. Breakpoints (data-dependent, Mg/ha): q1=13, q2=27, q3=51, q4=94, q5=171.
    Full table: Bin1 0-5, Bin2 5-13, Bin3 13-27, Bin4 27-51, Bin5 51-94, Bin6 94-171, Bin7 >171 Mg/ha.
  Land mask: Beck 2023 KG raster reused (cross-axis consistency); total land = 147,322,862 km^2
    (same baseline as Koppen); total vegetated land (>=5 Mg/ha) = 70,162,294 km^2.
  Per-site: 767 sites (fluxnet_shuttle_snapshot_20260624T095651.csv); 0 of 767 returned NA.
  Metrics (7-bin): J=0.6385, H=0.1656. For comparison, the PRIOR fixed-bin scheme (0-5, 5-25, 25-50,
    50-100, 100-200, 200-400, >400 Mg/ha) gave J=0.6262, H=0.1684.
  *** IMPORTANT STALENESS FLAG: an 18-BIN scheme is the CURRENT DEFAULT for the cross-axis summary
      trajectory figure (Rep007-010), superseding the 7-bin numbers this doc still reports as
      canonical -- see the cross-cutting "7-bin vs 18-bin" note at the end of this section. ***

1c. CGIAR Global Aridity Index / UNEP (Zomer et al. 2022, v3.1)
  Product: CGIAR Global Aridity Index and PET Dataset v3.1; doi:10.1038/s41597-022-01493-1;
    Figshare doi:10.6084/m9.figshare.7504448. 30 arc-second (~1km); 1970-2000 climate averages.
  Variable: AI = P/PET (Penman-Monteith), dimensionless; raster is uint16 scaled x10000; ocean/water=0, treated as missing.
  TWO PARALLEL SCHEMES:
    5-class (canonical UNEP / World Atlas of Desertification 1992): Hyper-Arid (<0.05), Arid
      (0.05-0.20), Semi-Arid (0.20-0.50), Dry Sub-Humid (0.50-0.65), Humid (>=0.65).
    7-class (extended, FAO-usage Humid subdivision): adds Humid-low (0.65-1.0), Humid-moderate
      (1.0-2.0), Hyper-Humid (>=2.0).
  Extraction: terra::extract() at exact coords; same 3 wetland sites (US-KS3, US-TaS, CN-SnB) needed
    the nearest-land fallback (0.6-1.0 km), all assigned Humid.
  Area weighting: terra::cellSize(mask=TRUE, unit="km") + terra::zonal(fun="sum"); total land =
    134,761,545 km^2 -- DIFFERENT baseline from Koppen/Biomass (147.3M km2) because this raster
    covers only 60S-90N and excludes Antarctica (~12.6M km2 difference, documented in the doc).
  Metrics: 5-class J=0.69 H=0.21; 7-class J=0.67 H=0.22.
  Note: this is the actual "5-bin vs 7-bin" split found in the repo for a single axis (see cross-cutting
    note below re: the task's "7-bin vs 18-bin" phrasing).

1d. ESA CCI Land Cover v2.1.1
  Product: ESA CCI LC v2.1.1 (Copernicus C3S continuation of v2.0.7), year 2022, 300m, LCCS classes.
    File: C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc (Copernicus CDS).
  THREE aggregation levels (not a 7-vs-18 split): Level 1 high-level (10 classes, ESA CCI PUG Table 2
    cross-walk), Level 2 intermediate LCCS hierarchy (22 classes), Level 3 native LCCS (37 classes,
    class 0 "No data" excluded). Full lookup: data/snapshots/cci_landcover_aggregation_lookup.csv.
  Land mask/grid: native 300m resampled (nearest-neighbour) onto the Beck 2023 KG 0.00833deg grid --
    SAME 147.3M km2 land-mask baseline as Koppen/Biomass. Cached at
    data/external/cci_landcover/v2.1.1/cci_lc_2022_kg_aligned_native.tif.
  Per-site: 767 sites; native LCCS code extracted previously, 3-degree nearest-land fallback for NA sites.
  Metrics at all 3 levels: High-level(10) J=0.556 H=0.337; Level2(22) J=0.478 H=0.368; Native(37)
    J=0.441 H=0.395 -- J decreases / H increases with finer aggregation (same pattern as Koppen).

1e. TRENDY v14-gcb2025 (NEE-IAV, ET-IAV, NEE-median, ET-median) -- see full SECTION 3 below for the
  ensemble-construction details (model list, exclusions, analysis window). Binning: SAME hybrid 7-bin
  scheme as biomass (bin1=0-5 fixed near-zero cut, negative pixels mapped to 0 before binning; bins2-7
  = six equal-area quantile bins). Per-axis breakpoints: NEE-IAV (gC/m2/yr, cut=5): 21.3,31.9,42.1,
  54.4,70.9. ET-IAV (mm/yr, cut=5): 17.5,24.2,32,41.8,58.1. NEE-med (gC/m2/yr, cut=5): 20.5,31.7,41.8,
  54.3,70.2. ET-med (mm/yr, cut=5): 143.4,263.9,394,634.7,968.6. Metrics: NEE-IAV J=0.507 H=0.316;
  ET-IAV J=0.663 H=0.224; NEE-median J=0.495 H=0.320; ET-median J=0.459 H=0.345. Cross-axis check:
  NEE-median vs ESA CCI Biomass v7 Pearson r=0.55, Spearman rho=0.89 (94,589 shared 0.5deg pixels).

1f. methods_flux_medians.md (not a representativeness axis; IGBP-class flux-median methodology --
  see SECTION 6 below for its FLUXNET2015-comparison content, and SECTION 5 for its QC threshold).

*** CROSS-CUTTING NOTE on "hybrid near-zero-bin construction" and "7-bin vs 18-bin schemes" (as
    phrased in the task): the HYBRID NEAR-ZERO-BIN CONSTRUCTION is confirmed and is the SAME pattern
    across Biomass and all four TRENDY-derived axes: 1 fixed near-zero bin + N equal-area quantile
    bins over the above-threshold area. The "7-bin vs 18-bin" pairing itself was NOT found verbatim in
    any methods_*.md doc -- the aridity doc has a 5-vs-7-class split (different axis, different
    numbers), and none of the per-axis docs mention 18 bins. An 18-bin scheme DOES exist, but only in
    SESSION_LOG.md (2026-06-29 entry "Multi-bin representativeness: 12-bin, 18-bin, 20-bin hybrid") and
    in scripts/figure_representativeness_summary.R + scripts/recompute_continuous_axes_multibin.R, as
    part of a bin-COUNT SENSITIVITY SWEEP (7/12/18/20/30-bin) applied to the 5 continuous axes
    (Biomass + 4 TRENDY fields). scripts/figure_representativeness_summary.R line 114 comment states
    these continuous axes moved to 18-bin as the DEFAULT for the cross-axis summary/trajectory figures
    "from this version onward" -- meaning methods_biomass.md and methods_trendy_iav.md, which still
    report only the 7-bin numbers as if canonical, are STALE relative to the actual current default in
    the summary figure script. Both should be treated with this staleness caveat when drafting Methods;
    the 18-bin construction itself follows the identical hybrid-bin-1-plus-N-quantile-bins logic, just
    with 17 quantile bins (Q_PROBS = (1:16)/17) instead of 6. Full bin-count comparison table (7/12/18/
    20/30-bin Jaccard values per axis) is in SESSION_LOG.md 2026-06-29 and computed by
    scripts/recompute_continuous_axes_multibin.R (also scripts/recompute_continuous_axes_30bin.R for
    the 30-bin point). Global-distribution CSVs at every bin count are in data/snapshots/
    (e.g. trendy_nee_iav_global_distribution_18bin.csv, trendy_et_median_global_distribution_18bin.csv,
    etc., and equivalent biomass files, plus non-suffixed files for the 7-bin baseline).

--------------------------------------------------------------------------------
SECTION 2 -- REPRESENTATIVENESS METRIC DEFINITIONS
--------------------------------------------------------------------------------

Sampling ratio (exact code, scripts/figure_representativeness_summary.R, merge_sr(), ~line 296):
  sampling_ratio = dplyr::if_else(
    global_land_fraction > 0 & network_frac > 0,
    network_frac / global_land_fraction, NA_real_
  )
  i.e. network fraction of a class divided by its global land fraction; values >1 = over-sampled,
  <1 = under-sampled, NA (not 0) when either side is zero, so structurally-zero classes (e.g. Koppen
  EF) can be flagged separately rather than plotted as a ratio of exactly 0. Equivalent per-axis
  version, scripts/figure_representativeness_landcover.R line 683: "sampling_ratio = dplyr::if_else(p
  > 0, q / p, NA_real_)" (p=global fraction, q=network fraction). Displayed on a log2 axis (methods_
  koppen_beck2023.md) so equal over-/under-sampling factors are symmetric about the parity line.

Weighted Jaccard (Ruzicka similarity) and Hellinger distance -- exact code,
scripts/figure_representativeness_landcover.R, compute_repr_metrics(), lines 325-330:
    compute_repr_metrics <- function(p, q) {
      p[is.na(p)] <- 0; q[is.na(q)] <- 0
      list(
        weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
        hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
      )
    }
  where p = global land fraction of class k, q = network fraction of class k. J in [0,1], 1=identical
  distributions; H in [0,1], 0=identical. Matches the formulas quoted in every methods_*.md doc read
  above verbatim.

Hellinger distance status: NOT RETIRED, but NOT part of the cross-axis summary trajectory figures.
  - data/snapshots/representativeness_metrics.csv (the precomputed metrics table feeding the summary
    script) HAS a hellinger_distance column (header: axis,aggregation_level,n_classes,weighted_jaccard,
    hellinger_distance,network,n_sites) -- populated for every axis/aggregation/network combination.
  - Hellinger IS computed and reported per-axis: present in every methods_*.md doc read in Section 1,
    and computed/plotted in the individual per-axis scripts (figure_representativeness_landcover.R,
    figure_representativeness_biomass.R, figure_representativeness_kg.R, figure_representativeness_
    aridity.R, figure_representativeness_trendy_compute.R, figure_representativeness_kg_future.R,
    recompute_continuous_axes_multibin.R, recompute_continuous_axes_30bin.R -- 11 files total grep-
    matched for "hellinger").
  - grep -in "hellinger" scripts/figure_representativeness_summary.R returns ZERO matches: this script
    (which builds Fig 007-010, the cross-axis "Jaccard trajectory" comparison figures) reads ONLY
    weighted_jaccard from representativeness_metrics.csv via its get_j() helper; Hellinger is simply
    never selected or plotted there.
  CONCLUSION: Hellinger distance is computed and reported for every axis (individual panels, the
  metrics CSV, and every per-axis methods doc) but is absent from the multi-axis SUMMARY trajectory
  figures, which report weighted Jaccard only.

--------------------------------------------------------------------------------
SECTION 3 -- TRENDY ENSEMBLE CONSTRUCTION
--------------------------------------------------------------------------------

Version string: TRENDY v14-gcb2025, S3 simulation (transient, with historical land-use change).
Protocol: Sitch et al. (2024), doi:10.1029/2024GB008102; GCB2025 paper in prep; acknowledgment-only
  attribution policy for individual model PIs (cited in Acknowledgements, not formally).
Analysis window: 1990-2023 (34 years), bounded by CLASSIC/DLEM/ELM ending their S3 run at 2023.

Models downloaded (full archive): 20.
Models excluded: 3, all technical (not selective):
  - CARDAMOM: only 22 years of temporal coverage (insufficient for the 34-year IAV window)
  - CLM-FATES: irregular longitude spacing, terra::rast() fails to parse
  - JSBACH: irregular latitude spacing, terra::rast() fails to parse
Remaining archive-level ensemble: 17 models -- CABLE-POP, CLASSIC, CLM, DLEM, ED, ELM, ELM-FATES,
  IBIS, ISAM, JULES-ES, LPJ-GUESS, LPJml, LPJwsl, LPX-Bern, ORCHIDEE, TEM, VISIT-UT.
Effective ensemble per pixel: 16 models. ELM submitted data only through 2022 (its 2023 annual layer
  is all-NA); because every per-pixel statistic (detrended SD, mean|NBP|, mean ET) requires a complete
  34-year row, ELM contributes an all-NA raster to ALL FOUR derived fields (not just the IAV axes) --
  the ensemble median (terra::app(na.rm=TRUE) over the 17-layer stack) is therefore determined by the
  16 non-ELM models at every pixel.

Derived fields (4): NEE-IAV, ET-IAV, NEE-median, ET-median.
  - NEE-IAV / ET-IAV: linear-detrended standard deviation (OLS hat-matrix residuals),
    SD_detrended = sqrt(sum(residuals^2) / (n-2)), n=34.
  - NEE-median: mean(|annual NBP|) across 34 years (absolute value taken before averaging, since NBP
    can be a net source).
  - ET-median: mean(annual ET) across 34 years (non-negative, no absolute value needed).
  - Ensemble-median maps: 94,589 non-NA pixels each (global Beck-2023 KG land mask at 0.5deg).

Grid/units: all models resampled to a common 0.5deg grid (720x360, EPSG:4326, bilinear), then
  KG-land-masked. nbp -> gC m-2 yr-1 (monthly: x2629800 s/mo x1000; annual: x31557600 s/yr x1000);
  evapotrans -> mm/yr (x2629800 s/mo; 1 kg/m2 = 1mm).

File paths:
  Methods doc:        review/figures/representativeness/methods_trendy_iav.md
  Download:            scripts/download_trendy_v14.sh
  Per-pixel/ensemble:  scripts/figure_representativeness_trendy_compute.R
  Site extraction/fig: scripts/figure_representativeness_trendy_wrap.R
  Diagnostic:          scripts/trendy_preview_diagnostic.R
  Derived GeoTIFFs:    data/external/trendy/derived/trendy_{nee_median,nee_iav,et_median,et_iav}.tif
  Intermediate:        data/external/trendy/derived/intermediate/ (252MB, 17 models x2 vars x34 layers)
  Per-site CSVs:       data/snapshots/site_trendy_{nee_iav,nee_median,et_iav,et_median}.csv (+meta.json)

--------------------------------------------------------------------------------
SECTION 4 -- SNAPSHOT DEFINITION
--------------------------------------------------------------------------------

File: data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv (557,061 bytes, dated 2026-06-24 09:56).
Site count: 767 (768 lines including header).
Columns (18): data_hub, site_id, site_name, location_lat, location_long, igbp, network,
  team_member_name, team_member_role, team_member_email, first_year, last_year, download_link,
  fluxnet_product_name, product_citation, product_id, oneflux_code_version, product_source_network.
No companion .meta.json file exists for this specific snapshot (checked; not found) -- a gap relative
  to CLAUDE.md's general Output Metadata policy.

Generating code: scripts/01_download.R line 19: live_manifest <- flux_listall() -- a SINGLE call to
  the fluxnet package, fetching AmeriFlux/ICOS/TERN hubs in that one call. R/snapshot.R::
  resolve_snapshot() (development mode) returns that manifest UNMODIFIED. scripts/01_download.R line
  47 calls write_snapshot(manifest, ...); R/snapshot.R line 53-55 stamps the output filename with
  timestamp <- format(Sys.time(), "%Y%m%dT%H%M%S") at write time.

THE SNAPSHOT IS A SINGLE POINT-IN-TIME CALL, NOT ASSEMBLED OVER A WINDOW. Network membership at
  freeze time = whatever flux_listall() returns for all three hubs at the moment 01_download.R runs.
  01_download.R lines 21-33 add a loud stop() guard if any expected hub (AmeriFlux/ICOS/TERN) is
  silently missing from that call's result (a documented failure mode: a hub-level fetch error, e.g.
  TERN HTTP 404, otherwise silently drops that hub with no R-level warning). The "previous snapshot
  comparison" logic (compare_snapshots()) exists ONLY to decide which sites need re-downloading -- it
  does not merge or alter the site list recorded in the new snapshot itself.

Persistent identifier (PID/DOI) for the snapshot: NOT FOUND / NOT ASSIGNED.
  docs/methods_requirements.md section 6 "Data availability statement" (a template, lines 229-244)
  reads: "downloaded from the FLUXNET Shuttle on [DATE]. The persistent identifier for the snapshot
  used is [PID]." -- both [DATE] and [PID] are literal, UNFILLED placeholders, explicitly marked
  "fill in DATE and PID when snapshot is archived." Section 7 "Code availability statement" likewise
  has an unfilled "(DOI: [ZENODO DOI])" placeholder pending repository archiving. No snapshot-level or
  dataset-level DOI/PID exists anywhere else in the repo (checked READMEs, config, metadata files).
  Per-SITE identifiers do exist in the snapshot's product_id/product_citation columns: ICOS sites
  (332) carry handle.net persistent identifiers (not DOIs; resolve to the ICOS data portal, per
  SESSION_LOG.md ~line 5545-5550); TERN sites (52) have NO machine-readable persistent identifier at
  all in the current snapshot (plain-text citation only, SESSION_LOG.md ~line 5552-5597).

--------------------------------------------------------------------------------
SECTION 5 -- SITE SELECTION AND QC
--------------------------------------------------------------------------------

Coordinate validity: implemented as a simple non-NA presence check,
  dplyr::filter(!is.na(location_lat), !is.na(location_long)), applied uniformly across 15+ scripts/R
  files (e.g. R/figures/fig_maps.R, scripts/step1_extract_worldclim.R, scripts/figure_
  representativeness_aridity.R, scripts/step4_extract_koppen_beck2023.R). No physical-plausibility
  range check (e.g. -90<=lat<=90) was found anywhere -- "valid" means "present", not "in-range".

Duplicate handling, two mechanisms:
  1. File-level (scripts/assess_flux_data_by_igbp_shuttle.R lines 118-124): "Deduplicate: if multiple
     YY files per site, keep longest (most years)" -- groups by site_id, keeps the file with the
     largest file size (slice_max(fsize, n=1, with_ties=FALSE)) as a proxy for most years of data.
  2. Row-level: pervasive dplyr::distinct(site_id, .keep_all=TRUE), applied at the point each script
     reads the snapshot metadata table (e.g. scripts/assess_flux_data_by_igbp_fluxnet2015.R:124,
     scripts/assess_flux_data_by_igbp_shuttle.R:93, scripts/07_figures.R:151/155,
     scripts/generate_fluxnet_citations.R:536).

QC threshold:
  PIPELINE DEFAULT (R/pipeline_config.R lines 40/43/46/49):
    QC_THRESHOLD_DD <- 0.50   (daily)
    QC_THRESHOLD_WW <- 0.50   (weekly)
    QC_THRESHOLD_MM <- 0.50   (monthly)
    QC_THRESHOLD_YY <- 0.50   (annual -- used for the paper's primary flux analyses)
  Applied in scripts/04_qc.R lines 82-85 (selects the constant by resolution) and lines 177-179 (logs
    the threshold used per exclusion).
  PER-ANALYSIS OVERRIDE (both the IGBP flux-median analyses):
    scripts/assess_flux_data_by_igbp_shuttle.R line 43:      QC_THRESH <- 0.80
    scripts/assess_flux_data_by_igbp_fluxnet2015.R line 54:  QC_THRESH <- 0.80
    (local script-level constants, NOT sourced from pipeline_config.R; applied independently to
    NEE_VUT_REF_QC/CUT-equivalent, LE_F_MDS_QC, H_F_MDS_QC).
  Rationale found: SESSION_LOG.md lines 980-989 states this 0.80 override is "deliberate, explicitly-
    documented ... not an inconsistency bug", applied IDENTICALLY to both the Shuttle and FLUXNET2015
    analyses for a consistent cross-network comparison. No further numeric/scientific justification
    for the specific value 0.80 (vs. any other stricter value) was found in the repo -- reported
    honestly as "a stricter, deliberately-applied override, consistent across both networks", not
    embellished with an invented rationale.

VUT/CUT and NT/DT fallback rules (as actually implemented):
  VUT/CUT (per-site, not per-row) -- scripts/04_qc.R lines 69-143: checks has_vut_col/has_cut_col
    (lines 69-70); groups by site_id (lines 98-129) to compute n_vut = count of non-NA
    NEE_VUT_REF_QC and n_cut = count of non-NA NEE_CUT_REF_QC per site; assigns
    qc_col_used = "NEE_VUT_REF_QC" if n_vut>0, else "NEE_CUT_REF_QC" if n_cut>0 (lines 122-124) --
    a site-level decision, so VUT and CUT QC are never mixed within one site's rows. Matches CLAUDE.md's
    documented QC Flag Reference exactly (verified current, not stale).
  NT/DT (per site, not per year) -- methods_flux_medians.md: GPP/TER use NT (nighttime) partitioning
    as first preference (GPP_NT_VUT_REF / RECO_NT_VUT_REF, with the site's VUT/CUT decision applied);
    when NT yields zero qualifying years for a site, DT (daytime) partitioning is used as a SITE-LEVEL
    fallback (GPP_DT_VUT_REF or GPP_DT_CUT_REF); the NT-vs-DT choice is made per site (not per year) to
    avoid mixing partitioning methods within one site's median; recorded in gpp_partition/ter_partition
    columns ("NT" or "DT"). DT columns carry no dedicated QC flag at YY resolution and are gated by the
    same NEE QC flag used in the VUT/CUT decision.

--------------------------------------------------------------------------------
SECTION 6 -- FLUXNET2015 COMPARISON
--------------------------------------------------------------------------------

Data source: FLUXNET2015 release, Pastorello et al. (2020), doi:10.1038/s41597-020-0534-3, CC-BY-4.0,
  FULLSET YY product. Download launched and completed same day: 2026-06-30 (SESSION_LOG.md "FLUXNET2015
  release portal investigation and download launch" / "...download complete").
Sites downloaded: 206 of the release's 212-site full list. Excluded: 6 "Tier-2-only" sites (a
  FLUXNET2015 data-tier/licensing designation carrying additional data-use restrictions beyond
  CC-BY-4.0; this project's CC-BY-4.0-only data policy excludes them). Result: 206/206 succeeded,
  0 failures; verified independently as 206 site directories + 206 zip files under
  data/raw/fluxnet2015/. 12.02 GB downloaded, 13 GB total on disk.
Per-site median construction: same logic as the Shuttle analysis (methods_flux_medians.md) -- VUT
  preferred / CUT fallback per site-year; NT preferred / DT fallback per site; median across
  QC-qualifying years per site, per variable. BOTH the FLUXNET2015 and Shuttle axes of the comparison
  use IDENTICAL aggregation logic, so the comparison isolates data/processing differences, not
  aggregation-method differences.
QC threshold: >=0.80 (scripts/assess_flux_data_by_igbp_fluxnet2015.R line 54, QC_THRESH <- 0.80) --
  same override and same rationale as Section 5 above (deliberate, documented, applied identically to
  both networks; no further numeric justification found beyond that).
Class aggregation: median of site medians per IGBP class (same as the Shuttle-only analysis).
CVM and CSH exclusion rules (methods_flux_medians.md, "Excluded classes"):
  - CVM: absent from the FLUXNET2015 release site list entirely (0 sites) -- the 9 CVM sites in the
    current Shuttle network were added/reclassified after the 2015/2020 release, so there is no
    FLUXNET2015 x-coordinate to plot.
  - CSH: n=2 in FLUXNET2015, below an n>=5 reliability threshold used elsewhere in the same doc;
    plotted positions for n=2 classes are not considered reliable enough for a network-comparison
    figure.
  - BSV/DNF/SNO (non-standard IGBP labels) excluded by construction (only the 12 standard IGBP labels
    are scored).
  - Remaining 10 plotted classes: EBF, MF, DBF, ENF, OSH, WSA, SAV, GRA, WET, CRO.
File paths:
  Methods doc:              review/figures/methods_flux_medians.md
  Shuttle per-site medians:  scripts/assess_flux_data_by_igbp_shuttle.R
  FLUXNET2015 per-site medians: scripts/assess_flux_data_by_igbp_fluxnet2015.R
  Comparison-table builder: scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R
  Output CSVs:               data/snapshots/site_flux_medians_{shuttle,fluxnet2015}.csv
  PI-contact log:            logs/fluxnet2015_pi_contacts_20260630_130019.csv (per-site DOI source)

--------------------------------------------------------------------------------
SECTION 7 -- HDR CONTOUR METHOD FOR THE NEW FIGURE 2
--------------------------------------------------------------------------------
(Retired Mahalanobis coverage statistic deliberately EXCLUDED below, per instruction.)

Climate variables/source: WorldClim v2.1, 2.5 arc-minute BIO1 (mean annual temperature, MAT) and BIO12
  (mean annual precipitation, MAP) -- the same rasters used for the per-site climate extraction
  elsewhere in the paper.
Land mask: ESA CCI land cover 2015 (ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif) -- class 210
  (water bodies) and class 220 (permanent snow/ice) excluded, class 0 (no data) excluded, all other
  classes = ice-free land; aggregated to the WorldClim grid (exact 15x integer factor) and thresholded
  at land_frac_threshold >= 0.5 (default arg, build_global_landclimate(), R/figures/fig_climate.R line
  364-369) per WorldClim cell; a latitude < antarctica_lat_cutoff (-60 deg, default) backstop
  additionally excludes Antarctica (R/figures/fig_climate.R lines 386-401, grep-verified).
Area weighting: cosine of latitude (WorldClim is an equal-angle, not equal-area, grid) -- weight per
  pixel = cos(latitude in radians), applied in build_global_landclimate()'s output.
Density estimator: a dependency-free weighted 2D kernel density estimate -- linear binning onto a
  regular grid (mass-preserving, each point's weight split across its 4 nearest grid nodes) followed
  by separable Gaussian smoothing (bandwidth via a weighted Scott's-rule reference using the effective
  sample size of the area weights), implemented in .weighted_density_grid() (R/figures/fig_climate.R
  line 953). No `ks`/`spatstat` package dependency was introduced.
Grid resolution: 201 x 201 (the default gridsize argument of .weighted_density_grid(), and the actual
  value explicitly passed by scripts/generate_whittaker_global.R line 148 -- grep-confirmed as the
  value actually used for the Figure 2 contours, not just an unused default).
HDR thresholds: 95% and 99% highest-density-region contours, computed via .hdr_levels() (sorts grid
  cell masses descending, finds the density value at which cumulative mass reaches the target
  probability) and drawn with grDevices::contourLines().
Figure construction: the 95%/99% contours are drawn as a geom_path() layer added on top of the
  UNMODIFIED fig_whittaker_worldclim() base plot object (the network NEE hexbin), in
  scripts/generate_whittaker_overlays.R -- solid line = 95%, dashed line = 99%, both black, no
  in-panel label/legend (explanation lives in the figure's own .legend.txt file instead).
File paths:
  Density/contour/coverage functions: R/figures/fig_climate.R (build_global_landclimate(),
    .weighted_density_grid(), .hdr_levels(), fig_whittaker_global_contour(), fig_whittaker_global_
    density())
  Density-surface driver:  scripts/generate_whittaker_global.R
  Figure-2-overlay driver: scripts/generate_whittaker_overlays.R
  Output figure:            review/figures/whittaker/fig_whit_fig2_with_both_contours.png (also
    fig_whit_fig2_with_95contour.png / fig_whit_fig2_with_99contour.png for single-contour variants)

--------------------------------------------------------------------------------
SECTION 8 -- ROUND-ROBIN HARMONIZATION
--------------------------------------------------------------------------------

NOT FOUND. grep -rniE "round.robin" across SESSION_LOG.md, docs/, scripts/, R/, and every *.md file in
  the repository (excluding renv/) returned ZERO matches -- the text "round robin" / "round-robin"
  does not appear anywhere in this repository.
grep -rniE "cross-hub|cross hub" returned exactly one substantive hit: docs/methods_requirements.md
  line 83, part of section 5.3 "Harmonisation and flux processing" -- "Must cover: ... Cross-hub
  validation if applicable." This is a single, unelaborated REQUIREMENTS-LIST bullet point (something
  the eventual Methods section MIGHT need to address), not evidence of any actual cross-hub round-robin
  design, protocol, or results.
CONCLUSION: no round-robin harmonization material of any kind -- neither design nor results -- exists
  in this repository. This is a stronger finding than "results pending, design exists": there is no
  design document, script, or results table for a processing-hub round robin anywhere in the codebase,
  only the single generic requirements-spec bullet noted above.

================================================================================
END OF COMPILED SOURCE MATERIAL
================================================================================
2026-07-02T23:09:00Z completed: wrote the full 8-section organized synthesis (Sections 1-8 above, ~283 lines) into this run log as the primary readable deliverable, pulling together all extracted content from the individual status lines above.
2026-07-02T23:09:10Z attempting: verify no analysis, figure, or data file was modified during this task (read-and-report only) -- git status check restricted to non-run-log paths.
2026-07-02T23:09:30Z completed: git status --short (excluding this run log) shows only files that were already modified/untracked BEFORE this task began (visible in the session's initial git status snapshot -- pre-existing local changes from earlier sessions, e.g. data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv.meta.json, renv/activate.R, review/figures/climate/fig_environmental_response_era5.png, and a long list of pre-existing untracked logs/ files). Confirmed: this task performed read-only operations (Read, Grep/grep, find, wc, git status) throughout, plus writes only to review/methods_source_compilation.txt itself. No analysis, figure, or data file was created, modified, or deleted by this task.
2026-07-02T23:09:40Z attempting: copy the assembled run log to review/methods_source_compilation.md for easy reading.
2026-07-02T23:09:50Z completed (provisional): review/methods_source_compilation.md written as an exact copy of this file's content up to this point (diff confirmed identical). Will re-copy once more at the very end, after the final outcome line below, so the .md reflects the fully-completed run log.
2026-07-02T23:10:00Z attempting: write SESSION_LOG.md entry noting the Methods source compilation, which methods_*.md files were found, and which requested items were not found.
2026-07-02T23:11:00Z completed: wrote SESSION_LOG.md entry (prepended above the "Draft-manuscript Figure 2 replaced" entry, per repo convention).
2026-07-02T23:11:10Z attempting: final re-copy of this run log to review/methods_source_compilation.md (so the .md reflects the fully-completed content, including this line and the final outcome line below).
2026-07-02T23:11:30Z outcome: SUCCESS -- all 8 requested sections compiled (per-axis representativeness methods with all methods_*.md files listed; representativeness metric definitions with formula code quoted verbatim and Hellinger status resolved; TRENDY v14-gcb2025 ensemble construction with file paths; snapshot definition with PID/DOI search resolved as not-found/unfilled-placeholder; site selection/QC with pipeline-default and per-analysis-override file:line citations; FLUXNET2015 comparison with file paths; HDR contour method for the new Figure 2 grep-verified against current code with the retired Mahalanobis statistic deliberately excluded; round-robin harmonization confirmed absent from the repository entirely). Every requested-but-not-found item was reported as an explicit "not found, searched X" line rather than inferred or guessed. No analysis, figure, or data file was modified (git-status-verified). review/methods_source_compilation.md written as a readable copy of this run log. SESSION_LOG.md entry written. Proceeding to commit and push.

