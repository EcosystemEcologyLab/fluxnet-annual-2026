WAFNET/fluxcom_comparison, stage 3b: rebuild the data package for Ossenatou Mamadou as a fully traceable release, building on stage 3 (2653513). The purpose is to let her do her own science with the comparison of FLUXCOM-X-BASE (0.05 degree, the product's native grid and the appropriate comparison) against the tower observations. Every value must be traceable to a persistent identifier and an exact source file. Same sites, QC rule, VUT/CUT choice and sign conventions as stage 3. Write a new folder, package_ossenatou_<date>/, and leave package_ossenatou_2026-11-21/ unchanged but marked superseded in the WAFNET README. X-BASE is non-Shuttle comparison data under Hard Rule 1.

1. Neighbouring cells. Stage 3 kept only the cell containing each tower and deleted the global files, so re-download the 84 X-BASE 0.05 degree files the same way (one at a time, verify the SHA-256 against the stage 3 manifest, extract, delete). This time extract the 3 x 3 block of cells centred on the tower's cell for the six sites. Report the total size first and stop if anything differs from the stage 3 manifest.
2. Provenance tables.
   - tower_provenance.csv, one row per site: site, dataset PID (handle or DOI) and citation from the snapshot; product version (e.g. v1.3_r1); the exact monthly FLUXMET file name used and its SHA-256; the snapshot listing file (fluxnet_shuttle_snapshot_20260920T102211.csv) and the site's row in it; FLUXNET Shuttle version; ONEFlux version; download date of the file; VUT or CUT. Check that the local monthly file is the version and product identifier listed in the snapshot. If any site's file does not match the snapshot, stop and report it; do not substitute or re-download tower data without asking.
   - xbase_provenance.csv, one row per source file (84): source key, variable, year, file name, ICOS object PID and URL, collection DOI (10.18160/5NZG-JMJE), SHA-256, download date, licence. Take these from the stage 3 manifest and the re-download.
3. Data files. Every row carries the key to its provenance row.
   - xbase_005_monthly.csv: as in stage 3 (cell containing the tower), plus a source key for each variable.
   - xbase_005_neighbours_monthly.csv: the 3 x 3 block, with row and column offset (-1 to +1), cell centre, distance from the tower, and the same variables and source keys.
   - tower_monthly.csv: as in stage 3, plus LE_CORR and H_CORR and monthly meteorology where the files provide it (P_F, TA_F, VPD_F, SW_IN_F, and SWC_F_MDS_1 if present), each with its QC column and pass flag where one exists. Units as given in the product documentation; state them in the README. Mark variables a site does not provide as missing, not zero.
   - sites.csv: as in stage 3.
   - summary.csv: as in stage 3, 0.05 degree columns only (drop the 0.5 degree columns).
   - The stage 3 check figure, regenerated from the new files, with its .legend.txt.
4. README.md for Ossenatou, revised:
   - Purpose: data for comparing FLUXCOM-X-BASE at its native 0.05 degree grid with tower observations; she does the analysis.
   - Every file and column with units; how to trace any value to its PID through the keys.
   - Corrections to the stage 3 README: cite the paper as Nelson et al. (2024), Biogeosciences 21, 5079-5115, doi:10.5194/bg-21-5079-2024, and the dataset as doi:10.18160/5NZG-JMJE; describe VUT and CUT as the variable or constant u* threshold, not partitioning; state the QC rule (monthly QC fraction of at least 0.50) without calling it a published convention; remove pointers to repository documents and give the repository URL instead.
   - Keep the existing caveats (sign convention, derived respiration, no X-BASE sensible heat, nighttime-partitioned training, training sites GH-Ank and SN-Dhr, grid cell versus footprint).
   - Flag, for her to check, monthly tower LE values that pass QC but look high for the season: BJ-Bfg March 2012 (358 W m-2, QC 0.52); BJ-Nhu August 2010 (205), December 2011 (214) and January 2012 (214). Report them as flagged, not as errors, and leave them in the data.
   - Citation requirements: X-BASE and every tower site dataset by its PID.
5. MANIFEST.csv: every file in the package with size and SHA-256, plus the Git commit of the code that built it and the script names. Zip the folder. If the package exceeds 10 MB, stop and report before committing.

Before committing, open the check figure and say what you checked, and spot-check three values end to end (one tower value, one X-BASE centre-cell value, one neighbour value) from the package back to the source file through the keys; report them.

Report in docs/report_back_<date>.md: download size, the snapshot match result per site, package contents with row counts and sizes, the three traced values, and anything unexpected. Update docs/methods_memo.md and the README briefly.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_wafnet_package_v2.md. Log, commit and push per CLAUDE.md.
