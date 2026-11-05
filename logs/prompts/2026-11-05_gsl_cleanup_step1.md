GSL_definition: tidy up folders and outputs (step 1). No analysis changes and no reruns. Context: on 5 Nov Dave and Tash agreed to write two papers (a short one on GSL methods that are sensitive to skewness, and a longer one on the wider methods work); which outputs go in which paper is not decided, so this step does not organise by paper. Its aims are to retire superseded outputs, give each tree one clear figure layout, and add an index.

Use git mv for every move so history is kept. Do not delete anything. Do not move cached data (.rds and anything git-ignored). Keep the two trees, GSL_definition/ (Tash's 62 sites) and GSL_definition/expanded/ (153 sites), where they are.

1. Archive. Create GSL_definition/archive/, mirroring original paths (for example archive/expanded/figures/...). Move, with each item's .pdf, .legend.txt and .meta.json companions:
   - expanded/figures/: Figures 1 to 6, Figure 1_alt1 and Supplemental Figures S1 to S8 (built with the session 2a winter screen, since dropped; replaced at the planned rerun).
   - expanded/figures/drafts/: Figure GSL-IAV draft, GSL methods agreement draft, GSL methods dotplot draft, GSL methods raster draft.
   - figures/drafts/proposal/: Figure P1b (replaced by P1c and P1d).
   - expanded/docs/: gsl_methods_compare_2026-11-02.md, _v2 and _v3 (v4 is current).
   - expanded/tables/: winter_screen_*, atneu_*, checks_c1_* to checks_c4_*, auhow_wet_season_v3, figure1_alts_years.
   - Scripts whose only outputs are archived (check expanded/code/09_figure_iav_15sites.R and 10_figure1_alts.R first; confirm by grep before moving). In a script that is still live, stop it writing Figure P1b and any archived draft, with the smallest possible edit; if a live script still writes another archived output (for example the winter-screen tables or Figures 1 to 6 at the rerun), leave the logic alone and list it in the report.
   Write archive/README.md: one row per item with original path, date retired, reason, and what replaces it.
2. Figure layout. Figures land in topic folders, nothing in a bare figures/ or drafts/ level:
   - GSL_definition/figures/reproduction/ (Tash's Figures 1 to 4 and S1 to S4), figures/proposal/ (P1a, P1c, P1d, P2; move up from drafts/proposal), figures/selection_audit/.
   - GSL_definition/expanded/figures/methods_review/ (A, A companion, both B, C, D; move up from drafts/methods_review) and expanded/figures/main/ (empty until the rerun; point the main figure scripts' output paths there).
   Keep the "Draft for discussion" stamps as they are.
3. Tables: move the selection_audit_* tables in GSL_definition/tables/ into tables/selection_audit/. Leave the other tables in place.
4. Paths: grep the whole repository (code, docs, READMEs, SESSION_LOG excluded) for every moved path and update references in live code and current docs. Changes to output and input paths only. Then check nothing is broken without rerunning analyses: source each tree's 00_config.R, and re-render Figure P2 from its committed tables to confirm the new paths work. confirm with git status that no table or figure changed content (git mv shows as renames).
5. Index: rewrite GSL_definition/README.md as a map. What each tree is (sites, selection, GPP series and year conventions); then a table of every current figure and current report with path, one-line description, the script that makes it, and status (reproduction check, draft for discussion, internal). Say that the paper allocation is undecided (5 Nov 2026) and point to archive/README.md. Plain English.

Report in GSL_definition/docs/cleanup_<date>.md: what moved where, scripts edited and why, live scripts that still write archived outputs, the path check results, and any further candidates for archiving that you noticed but did not move.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_cleanup_step1.md. Log, commit and push per CLAUDE.md.
