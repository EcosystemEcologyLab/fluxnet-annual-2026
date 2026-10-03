# Figure stage — final report

Unattended run per `logs/figstage_prompt.md`. Originally written at the end of Stage 6 (numbering,
supplementary PDF, methods, final report); updated at the end of the close-out session (2026-10-02)
that closed out Stage 3 and fixed several defects found on review of Stages 1/2/4. Sourced entirely
from `review/figstage_status.md` (the authoritative per-stage record) and each session's own work —
no numbers re-derived or re-estimated here.

## Status of each stage

| Stage | Status | Notes |
|---|---|---|
| 1 — two defects found on review | **DONE** | Figure 2 (now Figure 3) key fix; `supp_whittaker_nee_gpp_ter` (now `figS1_whittaker_nee_gpp_ter`) panel a key + blank-band trim. Close-out session found and fixed two follow-on defects — see "Close-out session" below. |
| 2 — the map becomes a main-text figure | **DONE** | `fig_map_network`/`fig_cumulative_siteyears_igbp` (now `fig_01_map_network`/`fig_02_cumulative_siteyears_igbp`) promoted; old Figure 1 attempts and `supp_map_regional` retired. Close-out session fixed point size/panel frames/letter placement — see below. |
| 3 — supplementary trajectory figure | **DONE** (closed out in the close-out session, 2026-10-02) | Found run to completion but never closed out by a prior session (no status entry, no commit). Re-confirmed and committed in the close-out session; staged as `figS6_representativeness_trajectory`. See `review/figstage_status.md` Stage 3 entry and "Close-out session" below. |
| 4 — supplementary flux figure | **DONE** | `supp_flux_representativeness` (now `figS5_flux_representativeness`); NEE/ET panels confirmed identical to Figure 4 (now Figure 5) rows E/F. |
| 5 — clean-up | **DONE** | Historical figures and flux-medians-by-IGBP regenerated; 4 candidates retired; Stage 3's conditional retirement item (46 `figure_representativeness_summary.R` outputs) completed in the close-out session now that Stage 3 is DONE. |
| 6 — numbering, supplementary PDF, methods, final report | **DONE** | See below. Supplementary PDF rebuilt with S1-S6 and committed in the close-out session (the Stage-6 build was never committed). |

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

## Check script's table (`scripts/check_figure_format.R`, run at the end of the close-out session)

```
Discovered 5 main-text figure(s) in review/figures/draft_manuscript_v1 and 6 Extended Data figure(s) in review/figures/draft_manuscript_v1/SupFigs

FIGURE                                     KIND  SIZE (mm)    FONTS  TEXT   LINES(pt)  STATUS
--------------------------------------------------------------------------------------------------------------
fig_01_map_network                         main  182.7x217.3  OK     13w    0.25-0.60  PASS
fig_02_cumulative_siteyears_igbp           main  88.9x88.9    OK     37w    0.25-1.00  PASS
fig_03_whittaker_current                   main  88.9x88.9    OK*    76w    0.25-1.03  PASS
fig_04_flux_comparison_combo_nep_et_h      main  88.9x227.9   OK*    128w   0.25-0.85  PASS
fig_05_representativeness                  main  182.7x173.2  OK*    273w   0.25-1.00  PASS
figS1_whittaker_nee_gpp_ter                ed    179.9x129.8  OK*    135w   0.25-1.03  PASS
figS2_flux_comparison_matched_siteyears    ed    88.9x227.9   OK*    129w   0.25-0.85  PASS
figS3_flux_comparison_six_panel            ed    179.9x219.8  OK*    245w   0.25-0.85  PASS
figS4_representativeness_geo_vs_geo        ed    179.9x173.2  OK*    265w   0.25-1.00  PASS
figS5_flux_representativeness              ed    179.9x172.5  OK*    358w   0.25-1.00  PASS
figS6_representativeness_trajectory        ed    119.9x99.8   OK     28w    0.25-0.71  PASS
--------------------------------------------------------------------------------------------------------------
TOTAL: 11 figure(s), 11 PASS, 0 FAIL
```

`figS6_representativeness_trajectory` (Stage 3's figure, previously uncommitted and unnumbered) is
now fully committed and numbered — see "Close-out session" below. `figS1`'s SIZE grew from
78.0 mm to 129.8 mm tall in the close-out session (the NEE key was moved out of panel a into its
own, taller, row — see below); still within the 180x240 mm Supplementary Figure limit.

Invariant (rule 5) held throughout Stage 6 AND the close-out session:
`data/snapshots/representativeness_metrics_fig4.csv` MD5 `85a1c086ad51aa27d40114c9c6d0e5d9`,
unchanged (`git diff` empty) before, during, and after every edit in both sessions.

## Close-out session (2026-10-02)

Closed out Stage 3 and fixed defects found on review of Stages 1, 2, and 4:

- **Stage 3 closure.** Re-ran `scripts/figure_representativeness_trajectory.R` fresh; its
  current-network Geo-vs-Geo values re-confirmed to reproduce
  `representativeness_metrics_fig4.csv`'s six rows exactly. Renamed its `SupFigs/` copy to
  `figS6_representativeness_trajectory` (completing the S1-S6 sequence Stage 6 had left stopped at
  S5), corrected its legend's stale "Extended Data"/"Figure 4" wording, and committed it. With
  Stage 3 DONE, completed Stage 5's own conditional item: all 46 remaining
  `scripts/figure_representativeness_summary.R` outputs `git mv`'d to
  `review/figures/representativeness/deprecated/`; `docs/figure_inventory.md` updated. See
  `review/figstage_status.md` Stage 3 entry for the full J/n table and the two unclassified
  historical sites (CN-Do2, CN-Do3, panel C/aridity/La Thuile only).
- **Figure 1 (`fig_01_map_network`) fixes.** Point size +25% in all five panels (0.5->0.625 panel
  a, 0.7->0.875 panels b-e). Each regional panel (b-e) now has a thin black frame
  (`panel.border`) and a narrow white gap (`plot.margin`) — previously no visible separation.
  Panel letters: replaced the land-avoidance search with a fixed top-left position (white
  background box for legibility over land or water) for all five panels — the search had been
  pushing panel d's letter away from top-left (East/Southeast Asia's landmass covers nearly the
  whole top edge of that panel), ending up at the bottom. Final size unchanged, 182.7x217.3 mm.
- **Figure 3 (`fig_03_whittaker_current`) key-colour fix.** The key's 8 colour swatches were drawn
  fully opaque while the hexagons they represent are drawn at `alpha = 0.85` — e.g. the "below
  -400" swatch was solid `#053061` against hexagons reading closer to `#2A4F79`. Added a single
  shared `HEX_FILL_ALPHA` constant (`R/figures/fig_climate.R`) used by both the hexagon layer and
  the key swatches (`scales::alpha()`), so they can't drift apart again. Verified by direct pixel
  sampling: all 8 swatches now match their alpha-blended-over-white hexagon colour to the nearest
  rounding (e.g. "below -400" swatch measured (42,79,121) = `#2A4F79`, against a computed expected
  blend of (42.5,79.05,120.7) — exact). `figS1_whittaker_nee_gpp_ter`'s GPP/TER key was checked the
  same way and found to already match (ggplot2's `guide_legend()` inherits the layer's constant
  `alpha=` by default) — no change needed there.
- **`figS1_whittaker_nee_gpp_ter`: NEE key moved out of panel a.** The key was drawn via
  `annotation_custom(xmin=-Inf, xmax=Inf, ...)` directly onto panel a, overlapping its hexagon/
  point data. Added a `show_stepped_key` parameter to `fig_whittaker_worldclim()` (default `TRUE`,
  preserving Figure 3's own behaviour) that skips this attachment; the key grob is always
  available via `attr(result, "stepped_key_grob")`. The NEE key now sits in its own patchwork cell
  in the shared legend row, beside the GPP/TER key, not on top of any panel's data. Found and
  fixed a genuine patchwork limitation along the way: nesting `guide_area()` two levels deep inside
  `(A|B|C) / (D|E)`-style operator chaining silently broke BOTH guide collection and column-width
  allocation (confirmed with a minimal reproduction); the fix was a flat `plot_layout(design=...)`
  composition instead. Figure grew from 180x78.0 mm to 180x129.8 mm (the NEE key's 8-step vertical
  stack needs more height than the GPP/TER key's 2-row horizontal layout) — still within the
  180x240 mm limit.
- **Supplementary PDF rebuilt and committed.** `SupFigs/supplementary_figures.pdf` rebuilt with
  all six Supplementary Figures (S1's new layout, S6 added): 6 pages, 0.62 MB. The Stage 6 build
  (S1-S5 only) was never committed; this one is.
- **Methods and decisions.**
  - `docs/methods_requirements.md` §5.4 (derived metrics/anomaly figures) marked **DEPRECATED** in
    its own section heading (previously only "flagged" in prose) — still not wired into any
    current figure, code left in place per rule 3.
  - `docs/decisions_pending.md`'s "Functionally active site definition — RESOLVED 2026-04-20"
    entry marked **SUPERSEDED 2026-10-02**: its "≥3 months" text never matched what
    `R/utils.R::is_functionally_active()` actually implements (≥1 month present, in ≥1 of the last
    4 years) — the April 2026 decision record was factually wrong about its own code. A new
    sub-entry records the correct definition; the code itself (`R/utils.R`) was not changed, only
    the decision record and `docs/methods_requirements.md` (already corrected in Stage 6).
  - Of the two TO CONFIRM facts Stage 6 left open: the current-network "sites with no usable
    annual NEE" count was resolved and filled in — **125 of 781**, directly re-verified
    (`compute_site_annual_fluxes()`'s `site_summary$nee_median` is `NA` for exactly 125 sites),
    using the QC_THRESHOLD_YY-qualifying-year definition (stated explicitly in the doc, since it
    differs from the pre-2026-10-02 text's "ONEFlux 15-day-gap rule" definition, which has no
    current-network equivalent in any committed source). The snapshot `.meta.json`/download-audit-
    CSV fact remains **TO CONFIRM** — re-checked directly, still genuinely absent from the repo
    (no `.meta.json` sidecar for the 2026-09-01 snapshot, no `download_progress*.csv` scoped to
    that date exists on disk).

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
Closed out in the close-out session (2026-10-02) — see "Close-out session" above and the full J/n
table (all 6 axes x 4 networks) plus the two unclassified historical sites in
`review/figstage_status.md`'s Stage 3 entry.

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
- **Stage 6 (new): Stage 3 was found run but never closed out.** *Resolved in the close-out
  session (2026-10-02)* — re-confirmed, renamed to `figS6_representativeness_trajectory`,
  committed; `review/figstage_status.md` now has a Stage 3 entry. See "Close-out session" above.
- **Stage 6 (new):** the supplementary PDF embeds each figure at ~200 dpi (downsampled from the
  600 dpi source) to stay well under the 10 MB limit — a deliberate quality/size trade-off for a
  reading bundle; the authoritative full-resolution PNG/PDF/JPEG per figure is unchanged in
  `SupFigs/`.
- **Stage 6 (new):** `docs/methods_requirements.md` §5.2's "sites excluded for all-missing NEE"
  and the §5.1 snapshot `.meta.json`/download-audit-CSV were both marked **TO CONFIRM** rather
  than estimated. *Partially resolved in the close-out session*: the all-missing-NEE count is now
  filled in as 125 of 781 (QC_THRESHOLD_YY-qualifying-year definition, stated explicitly, re-
  verified directly — see "Close-out session" above); the snapshot metadata sidecar fact remains
  TO CONFIRM — re-checked directly and still genuinely absent from the repo. Recommend deciding
  whether a snapshot metadata sidecar should exist going forward.
- **Stage 6 (new):** §5.4 (derived metrics/anomaly figures) describes an analysis
  (`R/figures/fig_anomaly_context.R` and its GEZ/Köppen-stratified callers) that is not wired into
  the current pipeline and hasn't been touched since 2026-04-18 — flagged rather than silently
  left to imply it's part of the current manuscript.
