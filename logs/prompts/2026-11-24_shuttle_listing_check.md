Diagnostic: how many sites does the FLUXNET Shuttle list today? Listing only. No data downloads, no change to the snapshot of record (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), the pipeline config, FLUXNET_SNAPSHOT_MODE or any analysis.

1. Call check_pipeline_config() and report the installed fluxnet-shuttle version. Run flux_listall() once, live. Use the hub-drop guard from scripts/01_download.R and report the rows returned by each hub. If any hub returns nothing or errors, stop and report; do not retry in a loop.
2. Save the listing with the Shuttle's naming (fluxnet_shuttle_snapshot_<YYYYMMDDTHHMMSS>.csv) in data/snapshots/check_only/, not in data/snapshots/ itself. First grep the repository for code that globs or picks the newest file in data/snapshots/, list what you find, and confirm that none of it reaches the subfolder.
3. Counts from today's listing: rows; distinct site IDs; distinct product IDs; sites by hub (product_source_network); and sites listed more than once, if any, with the reason.
4. Compare with the 20 September snapshot: same counts side by side; sites added and removed (site ID, hub); and sites whose product ID or product version changed (site ID, old, new). State how you matched sites.
5. Write the script as scripts/diagnostics/shuttle_listing_check_<date>.R and the report as scripts/diagnostics/shuttle_listing_check_<date>.md. The report gives the numbers in steps 1, 3 and 4, the Shuttle version, the listing file name, and anything unexpected. Do not interpret the changes.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_shuttle_listing_check.md. Log, commit and push per CLAUDE.md.
