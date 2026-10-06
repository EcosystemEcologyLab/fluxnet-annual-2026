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

