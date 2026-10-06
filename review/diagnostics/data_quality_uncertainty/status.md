# Data quality / uncertainty diagnostic -- run status

Unattended background run. Appended after each stage.

## Stage 0 -- Inventory

Completed 2026-10-06T23:00:37Z.

- Scanned annual/monthly/weekly/daily DuckDB tables (dataset = FLUXMET), 781 sites total.
- Column inventory: 150/256 (table x family x category) combinations exist in the DB.
- Absent combinations (genuinely absent from the FLUXNET product, not dropped at ingest -- confirmed against two sample extracted CSV headers): H/corr_25, H/corr_75, H/corr_jointunc, H/joint_uncertainty, H/mean_variant, H/pctl_05, H/pctl_16, H/pctl_25, H/pctl_50, H/pctl_75, H/pctl_84, H/pctl_95, H/se_variant, H/ustar50, LE/corr_25, LE/corr_75, LE/corr_jointunc, LE/joint_uncertainty, LE/mean_variant, LE/pctl_05, LE/pctl_16, LE/pctl_25, LE/pctl_50, LE/pctl_75, LE/pctl_84, LE/pctl_95, LE/se_variant, LE/ustar50.
- Sites with HH or HR FLUXMET files extracted on disk (live scan, not the stale DB manifest): 31 / 781.
- BIF files scanned: 781. Sites with a GRP_UST_THR group: 781. Variables found: USTAR_CP_SUCCESS_RUN, USTAR_CP_SUCCESS_RUN_YEAR, USTAR_MP_SUCCESS_RUN, USTAR_MP_SUCCESS_RUN_YEAR, USTAR_PERCENTILE, USTAR_PERCENTILE_YEAR, USTAR_THRESHOLD, USTAR_VERSION.
- Hub grouping decision: CLAUDE.md Hard Rule 2 says use the manifest/snapshot 'network' field, not site-ID prefixes, to avoid inferring hub from country code. The manifest actually carries two distinct fields: `network` (semicolon-separated list of every community network a site has ever belonged to, e.g. 'AmeriFlux;NEON;Phenocam') and `data_hub` (single-valued: AmeriFlux/ICOS/TERN/etc, the distributing hub actually used by flux_discover_files()/01_download.R). Existing diagnostics in this repo (scripts/diagnostics/koppen_pi_vs_era5.R, era5_precip_units.R) already group_by(data_hub) for 'by hub' breakdowns. Stages 1-4 of this diagnostic use `data_hub` for all 'by hub' tabulations, matching that precedent -- it is manifest-derived (from the download source), not inferred from site ID prefixes, so it satisfies Hard Rule 2's actual intent.

Outputs: table_stage0_column_inventory.csv, table_stage0_hh_hr_sites.csv, 
table_stage0_bif_ustar_summary.csv, table_stage0_bif_ustar_site_summary.csv (+ .meta.json each).


## Stage 1 -- Gaps

Completed 2026-10-06T23:04:18Z.

- QC flag distribution computed for NEE_VUT, NEE_CUT, LE, H at daily/weekly/monthly/annual, overall + by IGBP + by hub (data_hub). 240 group rows written.
- weekly resolution: only 1 site (US-MMS) in the DuckDB store -- not network-representative; flagged in report.md.
- Sub-daily QC split read from 31 sites' extracted HH/HR CSVs (5145912 sub-daily records pooled); compared against network IGBP/hub composition.

Outputs: table_stage1_qc_distribution.csv, table_stage1_subdaily_qc_by_site.csv, 
table_stage1_subdaily_qc_network_summary.csv, table_stage1_subdaily_vs_network_igbp.csv, 
table_stage1_subdaily_vs_network_hub.csv, fig_stage1_qc_flag_distribution.png, 
fig_stage1_subdaily_qc_split.png (+ .meta.json each).


## Stage 2 -- Uncertainty at the annual step

Completed 2026-10-06T23:07:28Z.

- Qualifying site-years ((1-QC)<=QC_THRESHOLD_YY=0.5), own-QC-gated per variable: VUT=4017, CUT=4320.
- Joint vs RSS(random, ustar_term) test: VUT median|diff|=0 gC/m2/yr (0% of joint, cor=1); CUT median|diff|=0 gC/m2/yr (0% of joint, cor=1).
- VUT: median random=5.56, median ustar_term=15.71, median joint=17.4 gC/m2/yr; ustar_term dominates 88.2% of site-years, random dominates 11.2%.
- CUT: median random=5.58, median ustar_term=17.83, median joint=19.45 gC/m2/yr; ustar_term dominates 90.3% of site-years, random dominates 8.3%.
- LE/H: only RANDUNC exists at the annual step (no ustar ensemble, no JOINTUNC for the uncorrected value); summarised in table_stage2_le_h_uncertainty_summary.csv.

Outputs: table_stage2_site_year_nee_uncertainty.csv, table_stage2_joint_vs_rss_test.csv, 
table_stage2_nee_uncertainty_summary.csv, table_stage2_le_h_uncertainty_summary.csv, 
fig_stage2_joint_vs_rss.png, fig_stage2_uncertainty_terms_boxplot.png, 
fig_stage2_uncertainty_vs_nee_magnitude.png (+ .meta.json each).


## Stage 3 -- VUT against CUT

Completed 2026-10-06T23:09:35Z.

- Site-year level: n=3960 site-years (575 sites) with both VUT and CUT qualifying. median diff=0.03, IQR=[-4.27, 5.77], 5-95pctile=[-33.23, 37.56] gC/m2/yr. share|diff|>25/50/100 = 0.143/0.06/0.022. share smaller than combined joint unc = 0.961. share sign differs = 0.016.
- Site level (site medians): n=575 sites. median diff=0.03, IQR=[-2.62, 3.92], 5-95pctile=[-18.82, 29.49] gC/m2/yr. share|diff|>25/50/100 = 0.097/0.031/0.009. share smaller than combined joint unc = 0.977. share sign differs = 0.012.

Outputs: table_stage3_site_year_vut_vs_cut.csv, table_stage3_site_year_summary.csv, 
table_stage3_site_level_vut_vs_cut.csv, table_stage3_site_level_summary.csv, 
fig_stage3_vut_minus_cut_histogram.png, fig_stage3_vut_vs_cut_scatter.png (+ .meta.json each).


## Stage 4 -- Availability and failure

Completed 2026-10-06T23:12:09Z.

- Site-years (n=6336): both=3960 (62.5%), neither=1959 (30.9%); 'neither' splits into no_value=1935 and fails_qc=24.
- Sites (n=781): both=575 (73.6%), neither=125 (16%). Sites in manifest with zero FLUXMET annual rows: 0.
- u-star BIF method-success records found (Stage 0 confirmed presence at all 781 sites): CP recorded for 6336 site-years (3493 failures), MP recorded for 6336 site-years (305 failures). Failure-by-category breakdown in table_stage4_ustar_failure_by_category.csv.
- Master join table written: table_stage4_site_year_master.csv (6336 rows, one per FLUXMET annual site-year).

Outputs: table_stage4_site_year_master.csv, table_stage4_site_level_availability.csv, 
table_stage4_site_year_category_counts.csv, table_stage4_site_level_category_counts.csv, 
table_stage4_ustar_method_vs_qualification.csv, table_stage4_ustar_failure_by_category.csv, 
fig_stage4_site_year_availability.png, fig_stage4_site_availability.png (+ .meta.json each).

