# Figure stage — final report

Unattended run per `logs/figstage_prompt.md`. Written at the end of Stage 6 (numbering,
supplementary PDF, methods, final report). Sourced entirely from `review/figstage_status.md`
(the authoritative per-stage record) and this stage's own work — no numbers re-derived or
re-estimated here.

## Status of each stage

| Stage | Status | Notes |
|---|---|---|
| 1 — two defects found on review | **DONE** | Figure 2 (now Figure 3) key fix; `supp_whittaker_nee_gpp_ter` (now `figS1_whittaker_nee_gpp_ter`) panel a key + blank-band trim. |
| 2 — the map becomes a main-text figure | **DONE** | `fig_map_network`/`fig_cumulative_siteyears_igbp` (now `fig_01_map_network`/`fig_02_cumulative_siteyears_igbp`) promoted; old Figure 1 attempts and `supp_map_regional` retired. |
| 3 — supplementary trajectory figure | **NOT DONE** (no entry in `review/figstage_status.md`; jumps Stage 2 → Stage 4) | Found run **uncommitted** on disk this session: `scripts/figure_representativeness_trajectory.R`, `review/figures/representativeness/supp_representativeness_trajectory.*`, the `SupFigs/` copy, and `data/snapshots/representativeness_metrics_trajectory.*`, all untracked. That script's own log (`logs/figure_representativeness_trajectory_20261002_185348.log`) reports its current-network Geo-vs-Geo rows reproducing `representativeness_metrics_fig4.csv` exactly — but Stage 3 was never closed out (no status entry, no commit, no session log). Left untouched this session (not Stage 6's task to finish Stage 3) — see Decisions for Dave. |
| 4 — supplementary flux figure | **DONE** | `supp_flux_representativeness` (now `figS5_flux_representativeness`); NEE/ET panels confirmed identical to Figure 4 (now Figure 5) rows E/F. |
| 5 — clean-up | **DONE** | Historical figures and flux-medians-by-IGBP regenerated; 4 candidates retired; Stage 3's conditional retirement item skipped (Stage 3 not DONE). |
| 6 — numbering, supplementary PDF, methods, final report | **DONE** (this report) | See below. |

## Stage 6 work (this session)

### a. Numbering

Main-text figures renumbered/staged via their generating scripts (`scripts/generate_fig_map_
network.R`, `scripts/generate_fig_cumulative_siteyears.R` write directly;
`scripts/build_draft_manuscript_v1.R` copies Figures 3–4; `scripts/figure4_representativeness.R`
writes Figure 5 and Supplementary Figure S4 directly):

| New name | Old name | Built by |
|---|---|---|
| `fig_01_map_network` | `fig_map_network` | `scripts/generate_fig_map_network.R` |
| `fig_02_cumulative_siteyears_igbp` | `fig_cumulative_siteyears_igbp` | `scripts/generate_fig_cumulative_siteyears.R` |
| `fig_03_whittaker_current` | `fig_02_whittaker_current` | `scripts/build_draft_manuscript_v1.R` |
| `fig_04_flux_comparison_combo_nep_et_h` | `fig_03_flux_comparison_combo_nep_et_h` | `scripts/build_draft_manuscript_v1.R` |
| `fig_05_representativeness` | `fig_04_representativeness` | `scripts/figure4_representativeness.R` |

Supplementary figures numbered figS1–figS5, in the task's fixed order, **without gaps**:

| # | New name | Old name |
|---|---|---|
| S1 | `figS1_whittaker_nee_gpp_ter` | `supp_whittaker_nee_gpp_ter` |
| S2 | `figS2_flux_comparison_matched_siteyears` | `supp_flux_comparison_matched_siteyears` |
| S3 | `figS3_flux_comparison_six_panel` | `supp_flux_comparison_six_panel` |
| S4 | `figS4_representativeness_geo_vs_geo` | `supp_representativeness_geo_vs_geo` |
| S5 | `figS5_flux_representativeness` | `supp_flux_representativeness` |

`supp_representativeness_trajectory` (Stage 3's would-be figure) is **excluded** — Stage 3 is not
DONE (see above) — so the sequence stops at S5 with no gap at "S6." All source scripts, functions,
and underlying data/table files keep their own names (only the `draft_manuscript_v1`/`SupFigs`
copy filenames changed); `docs/figure_inventory.md` records this explicitly, per the task.

Every legend's self-reference, figure-number cross-references, and "Extended Data" wording were
updated to the new numbering and to "Supplementary Figure" (target journal Scientific Data has no
Extended Data concept) — both in the manuscript copies and, where the same text is written by the
same code at the FIG_DIR source location (`fig_04_representativeness.legend.txt`,
`supp_flux_representativeness.legend.txt`/`.meta.json`, `supp_representativeness_geo_vs_geo.
legend.txt`), there too, since those files are also "legends" carrying a now-stale figure number —
filenames at that source location were not changed. `docs/figure_inventory.md` rewritten
accordingly (new "Main-text figures" table, Figure 5/Supplementary-Figure-S4 renumbering, new
Supplementary Figures table with the S1–S5 order and the excluded-trajectory note).

### b. Supplementary PDF

New `scripts/build_supplementary_pdf.R` (dependency-free: base `grid`/`grDevices` + the
already-used `png` package — no new package added) assembles `SupFigs/supplementary_figures.pdf`:
one Supplementary Figure per A4 page, headed by "Supplementary Figure S*N*" (bold) and the
figure's own legend TITLE (word-wrapped to the page margin), figure image below at ~200 dpi
(downsampled from the 600 dpi source PNGs — the authoritative full-resolution PNG/PDF/JPEG for
each figure is untouched in `SupFigs/`). **Result: 0.57 MB, 5 pages** — well under the 10 MB
limit. Verified by rendering each page to PNG (`pdftoppm`) and visually checking: no clipped
titles or images, every colour key/swatch visible, no blank bands.

### c. Methods (`docs/methods_requirements.md`, §5.1–5.5)

Brought into line with the current code and committed tables; every number sourced from a
committed table, script constant, or `SESSION_LOG.md` (never estimated — items that could not be
verified this way are marked **TO CONFIRM** in the doc itself, not guessed). Key corrections:

- §5.1: snapshot updated to the current locked file (`fluxnet_shuttle_snapshot_20260901T094522.
  csv`, 781 sites, not 672); hub counts from the snapshot's own `data_hub` column (AmeriFlux 381,
  ICOS 348, TERN 52); Shuttle version clarified as `0.3.7.post0+dirty` (canonical self-reported
  string). TO CONFIRM: no snapshot `.meta.json` or snapshot-scoped `download_progress.csv` found.
- §5.2: VUT/CUT/neither split corrected to 616/40/125 (of 781, from `compute_site_annual_fluxes()`);
  the "all-missing NEE" exclusion count (previously "106 of 672") has **no current-network
  equivalent in any committed table** — marked TO CONFIRM rather than rescaled or guessed;
  Köppen-Geiger source rewritten to describe its two genuinely-different current uses (ERA5-local
  everywhere except Figure 5 panel A's Geo-vs-Data side, which is PI-reported-BADM-first since
  2026-10-02) instead of the single, now-superseded description; the functionally-active-site
  definition was **factually wrong** in the prior doc ("≥3 months… in last 4 years") and is
  corrected to what `R/utils.R::is_functionally_active()` actually implements (≥1 month present in
  ≥1 of the last 4 years); UN subregion/FAO GEZ flagged as **not used by any currently-produced
  figure** (verified by grep — only reachable via orphaned, 2026-04-18-vintage code).
- §5.3: QC thresholds and the per-site VUT/CUT rule confirmed unchanged; the shared
  `compute_site_annual_fluxes()`/`compute_site_annual_fluxes_from_df()` function and its confirmed
  caller list added explicitly; the precipitation-defect "4 sites" statement replaced with the
  verified, four-part breakdown (`GRP_ERA_DOWN` 172 sites, `P_ERA_MAX_RATIO`/`P_ERA_MIN_RATIO` +10/
  +22, 4 invalid-ERA5-input sites scoped to panel C only) plus the separate §9a site-year statistic;
  the tower-measured-precipitation (`P_F`) defect is marked TO CONFIRM — still unquantified
  anywhere in the repo.
- §5.4: flagged at the top as describing an **orphaned, not-currently-wired-in** analysis
  (`R/figures/fig_anomaly_context.R` and its callers, last touched 2026-04-18, absent from
  `docs/figure_inventory.md` and `scripts/07_figures.R`) — not deleted, since the task scope is
  bringing text into line with reality, not removing requirements.
- §5.5: Marconi site-years corrected 97 → 96 (recomputed from the raw crosswalk file); La
  Thuile/FLUXNET2015 counts confirmed unchanged; CGIAR aridity scaling confirmed unchanged, with a
  new note distinguishing it from the separate, panel-C-only ERA5-based aridity calculation; FAO
  GEZ flagged not-currently-used, same basis as §5.2.
- §5.8 header and body renumbered Figure 4 → Figure 5 / "supplemental figure" → Supplementary
  Figure S4, with the genuinely-different, already-superseded "previous Figure 4"
  (`fig_rep001_current.png`) explicitly distinguished from the current Figure 5 to avoid confusion.

### d. Final report

This file.

## Check script's table (`scripts/check_figure_format.R`, run after all Stage 6 edits)

```
Discovered 5 main-text figure(s) in review/figures/draft_manuscript_v1 and 6 Extended Data figure(s) in review/figures/draft_manuscript_v1/SupFigs

FIGURE                                     KIND  SIZE (mm)    FONTS  TEXT   LINES(pt)  STATUS
--------------------------------------------------------------------------------------------------------------
fig_01_map_network                         main  182.7x217.3  OK     13w    0.25-0.60  PASS
fig_02_cumulative_siteyears_igbp           main  88.9x88.9    OK     37w    0.25-1.00  PASS
fig_03_whittaker_current                   main  88.9x88.9    OK*    76w    0.25-1.03  PASS
fig_04_flux_comparison_combo_nep_et_h      main  88.9x227.9   OK*    128w   0.25-0.85  PASS
fig_05_representativeness                  main  182.7x173.2  OK*    273w   0.25-1.00  PASS
figS1_whittaker_nee_gpp_ter                ed    179.9x78.0   OK*    135w   0.25-1.03  PASS
figS2_flux_comparison_matched_siteyears    ed    88.9x227.9   OK*    129w   0.25-0.85  PASS
figS3_flux_comparison_six_panel            ed    179.9x219.8  OK*    245w   0.25-0.85  PASS
figS4_representativeness_geo_vs_geo        ed    179.9x173.2  OK*    265w   0.25-1.00  PASS
figS5_flux_representativeness              ed    179.9x172.5  OK*    358w   0.25-1.00  PASS
supp_representativeness_trajectory         ed    119.9x99.8   OK     28w    0.25-0.71  PASS
--------------------------------------------------------------------------------------------------------------
TOTAL: 11 figure(s), 11 PASS, 0 FAIL
```

(`supp_representativeness_trajectory` is Stage 3's uncommitted, not-numbered output — discovered
and still on disk, left exactly as found; not part of Stage 6's own output.)

Invariant (rule 5) held throughout Stage 6: `data/snapshots/representativeness_metrics_fig4.csv`
MD5 `85a1c086ad51aa27d40114c9c6d0e5d9`, unchanged (`git diff` empty) before, during, and after this
stage's edits.

## Numbers each stage reported

### Stage 1
Pixel colour of each of Figure 2's (now Figure 3's) eight key boxes:

| Step | Hex |
|---|---|
| above 200 | #D6604D |
| 100 to 200 | #F4A582 |
| 0 to 100 | #FDDBC7 |
| −100 to 0 | #D1E5F0 |
| −200 to −100 | #92C5DE |
| −300 to −200 | #4393C3 |
| −400 to −300 | #2166AC |
| below −400 | #053061 |

Final size of the three-flux figure (`supp_whittaker_nee_gpp_ter`, now `figS1_whittaker_nee_gpp_ter`): 179.9 × 78.0 mm.

### Stage 2
Final sizes: `fig_map_network` (now `fig_01_map_network`) 182.7 × 217.3 mm; `fig_cumulative_
siteyears_igbp` (now `fig_02_cumulative_siteyears_igbp`) 88.9 × 88.9 mm.

Tower counts: a (outside all 4 regions) 65, b North America 362, c Europe 198, d East/Southeast
Asia 103, e Australia/NZ 53 — all five match the task's expected counts exactly.

### Stage 3
Not reported — stage not DONE (see Status table above).

### Stage 4
Bin edges, n and J for all eight panels of `supp_flux_representativeness` (now
`figS5_flux_representativeness`), `data/snapshots/representativeness_metrics_flux_supp.csv`:

| Panel | Flux | Comparison | Bin edges (bars 2–7 boundaries) | n | J |
|---|---|---|---|---|---|
| a | NEE | Geo vs Geo | −250, −100, −50, −25, 0 | 781 | 0.530 |
| b | NEE | Geo vs Data | −250, −100, −50, −25, 0 | 656 | 0.165 |
| c | GPP | Geo vs Geo | 300, 700, 1100, 1500, 2000 | 781 | 0.488 |
| d | GPP | Geo vs Data | 300, 700, 1100, 1500, 2000 | 651 | 0.529 |
| e | TER | Geo vs Geo | 300, 600, 1000, 1300, 1800 | 781 | 0.482 |
| f | TER | Geo vs Data | 300, 600, 1000, 1300, 1800 | 651 | 0.511 |
| g | ET | Geo vs Geo | 200, 350, 450, 600, 850 | 781 | 0.456 |
| h | ET | Geo vs Data | 200, 350, 450, 600, 850 | 665 | 0.479 |

NEE/ET panels confirmed identical to `representativeness_metrics_fig4.csv` rows E/F to 1e-9.
Final figure size: 180 × 172.7 mm.

### Stage 5
Regenerated: `review/figures/historical/` (6 PNGs) and `review/figures/flux_medians/` (5 PNGs + 5
CSV/meta pairs). Retired: 4 candidates (8 files) to `review/figures/candidates/deprecated/`. Left,
with reason: Stage 3's conditional retirement item (Stage 3 not DONE); three pre-existing
`fig_compare_whittaker_*` title/legend overlaps (cosmetic, out of scope).

### Stage 6 (this stage)
See "a–d" above for the full numbering map; check-script table above; supplementary PDF 0.57 MB /
5 pages.

## Decisions for Dave (all stages, one list)

- **Stage 2:** `fig_map_network`'s (now `fig_01_map_network`'s) final height, 217.3 mm, is above
  the task's "aim for 200 mm tall or less" but under Nature's 247 mm hard limit — driven by panel
  a's own true Equal Earth aspect at full 183 mm width. Took the option that changes least (kept
  panel a full-bleed); getting under 200 mm would require either a letterboxed panel a or changing
  a region's extent (off limits, and would change the task's expected tower counts).
- **Stage 2:** scale-bar/letter placement search (`.grid_candidates()`) checks only against land
  polygons, not tower points or against itself (bar vs. letter) — no collision found this render,
  but not inherently guaranteed for a future network update.
- **Stage 4:** GPP/TER bin edges do not match `scripts/diagnostics/flux_bin_breaks.R`'s own
  diagnostic edges, by design (different, newer tower-value method per the task instruction) —
  flagged in case the discrepancy is noticed without this context. TER's colour ramp was that
  script's own judgement call, reused rather than re-decided.
- **Stage 5:** `fig_compare_whittaker_{2000,2007,2015}.png` (historical diagnostics, not a
  manuscript figure) has a pre-existing title/legend-box text overlap, confirmed identical in the
  last-committed version — left unfixed as out of scope for a data-only regeneration.
- **Stage 6 (new): Stage 3 was found run but never closed out.** `scripts/figure_
  representativeness_trajectory.R` and its full output set (figure, table, log) exist on disk,
  untracked, with the script's own validation log reporting its current-network Geo-vs-Geo rows
  reproducing `representativeness_metrics_fig4.csv` exactly — but `review/figstage_status.md` has
  no Stage 3 entry, nothing is committed, and no `SESSION_LOG.md` entry exists for it. Stage 6
  treated this the same way Stage 5 already did for its own conditional item: not-DONE means
  excluded from this stage's work (the trajectory figure is not part of the figS1–S5 numbering,
  and Stage 6 did not commit, finalise, or write a status/session-log entry for Stage 3 itself,
  since that is not Stage 6's task). **Recommend:** either re-run Stage 3 to completion (commit +
  status + session-log entry) or explicitly decide to drop it; the on-disk files are real work and
  shouldn't sit uncommitted indefinitely.
- **Stage 6 (new):** the supplementary PDF embeds each figure at ~200 dpi (downsampled from the
  600 dpi source) to stay well under the 10 MB limit — a deliberate quality/size trade-off for a
  reading bundle; the authoritative full-resolution PNG/PDF/JPEG per figure is unchanged in
  `SupFigs/`.
- **Stage 6 (new):** `docs/methods_requirements.md` §5.2's "sites excluded for all-missing NEE"
  and the §5.1 snapshot `.meta.json`/download-audit-CSV were both marked **TO CONFIRM** rather
  than estimated — no committed table, script constant, or session-log entry gives a current,
  781-site-network value for either. Recommend recomputing the all-missing-NEE count if it's
  needed for the paper; recommend deciding whether a snapshot metadata sidecar should exist going
  forward.
- **Stage 6 (new):** §5.4 (derived metrics/anomaly figures) describes an analysis
  (`R/figures/fig_anomaly_context.R` and its GEZ/Köppen-stratified callers) that is not wired into
  the current pipeline and hasn't been touched since 2026-04-18 — flagged rather than silently
  left to imply it's part of the current manuscript.
