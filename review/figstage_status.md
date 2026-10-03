# Figure stage status

Tracks the six unattended stages defined in `logs/figstage_prompt.md`. Each stage appends its own
section below, in order, once it finishes (DONE, SKIPPED, or FAILED).

## Stage 1 — two defects found on review: DONE

- Figure 2 key: fixed. All eight step swatches now draw (including "100 to 200" = #F4A582),
  via a manually-drawn key (`R/figures/fig_climate.R::.stepped_fill_key_grob()` +
  `ggplot2::annotation_custom()`) that replaces `guide_legend()` for `fill_mode = "stepped"`,
  working around a confirmed ggplot2 4.0.x guide regression (blank key swatch for any discrete
  manual-scale level with zero matching rows in the rendered layer data). No hexagon data,
  binning, or site counts changed.
- `supp_whittaker_nee_gpp_ter`: panel a now shows its own NEE key (same eight steps as Figure 2);
  the blank band above the panels and the blank band between the panels and the keys were both
  trimmed (`scripts/generate_whittaker_ed_three_flux.R`: `plot_layout(heights = c(1, 0.22))` →
  `c(1, 0.38)`, figure height 95 → 78 mm, width unchanged at 180 mm).

### Numbers asked for

Pixel colour of each of Figure 2's eight key boxes (sampled directly from
`draft_manuscript_v1/fig_02_whittaker_current.png`):

| Step | Hex | RGB |
|---|---|---|
| above 200 | #D6604D | (214, 96, 77) |
| 100 to 200 | #F4A582 | (244, 165, 130) |
| 0 to 100 | #FDDBC7 | (253, 219, 199) |
| −100 to 0 | #D1E5F0 | (209, 229, 240) |
| −200 to −100 | #92C5DE | (146, 197, 222) |
| −300 to −200 | #4393C3 | (67, 147, 195) |
| −400 to −300 | #2166AC | (33, 102, 172) |
| below −400 | #053061 | (5, 48, 97) |

Final size of the three-flux figure (`supp_whittaker_nee_gpp_ter`): **179.9 × 78.0 mm**
(`scripts/check_figure_format.R`, 11/11 PASS).

### Decisions for Dave

- The manually-drawn key (`.stepped_fill_key_grob()`) bypasses `ggplot2::guide_legend()` for
  every `fill_mode = "stepped"` caller because of a confirmed ggplot2 4.0.x bug, not a
  project-specific styling choice. If a future ggplot2 release fixes the underlying guide
  regression, this workaround could be reverted, but there's no automatic way to detect that, so
  it was left in place.
- `supp_whittaker_nee_gpp_ter`'s new height (78 mm) and `plot_layout(heights=)` ratio (0.38) were
  tuned empirically against the rendered pixel output, not derived analytically. Passes the
  format checker and manual visual review now, but a future content change to any of the three
  panels (e.g. a taller legend) may require re-tuning.

Full write-up: `SESSION_LOG.md`, "2026-10-02 (11) — Figure stage 1: Figure 2 key fix, panel a key
added to the three-flux Extended Data figure".

---

## Stage 2 — the map becomes a main-text figure: DONE

- `draft_manuscript_v1/fig_map_network.png/.pdf/.legend.txt` (new, `scripts/generate_fig_map_network.R`):
  the five panels of the retired `SupFigs/supp_map_regional` (Equal Earth world overview + four
  LAEA regional panels), promoted to a 183 mm-wide main-text figure. Two layout changes from the
  retired Extended Data version, both load-bearing, not cosmetic: (1) each row's height is now
  `183mm / (aspect_left + aspect_right)` from the two regions' own LAEA-projected bbox aspect
  ratios, with each panel's width set to `height x its own aspect` — this packs every row
  edge-to-edge in both directions with zero coord_sf letterboxing, replacing the retired version's
  fixed "2 x panel-a-height" split evenly across two equal-width columns, which letterboxed
  wherever a region's true aspect didn't match that allocation (the visible blank bands above/below
  rows b/c and d/e). (2) scale-bar and panel-letter placement is chosen programmatically per panel:
  a grid of candidate anchor positions, nearest-to-default (bottom-left for the bar, top-left for
  the letter) first, tested with `sf::st_intersects()` against that panel's own land polygon in its
  own LAEA CRS; the nearest candidate clear of land is used. This fixed both defects named in the
  task directly from geometry, not hand-tuned pixel offsets: panel c's "1000 km" bar/label moved
  from over the Spain/Morocco coast to the open mid-Atlantic; panel d's "1500 km" bar/label moved
  from over the Indonesian islands to open water in the South China Sea; panel e's letter moved
  from on top of an Indonesian island to open water south of it. Panel b's bar/letter and panels
  a/c's letters landed at or within one grid step of their original default position (land was
  never actually in the way there) -- confirms the search prefers minimal change, not an
  arbitrary relocation. Tower counts per panel (programmatically counted, not eyeballed): a
  (outside all four regions) 65, b (North America) 362, c (Europe) 198, d (East/Southeast Asia)
  103, e (Australia/NZ) 53 -- all five match the task's expected counts exactly.
- `draft_manuscript_v1/fig_cumulative_siteyears_igbp.png/.pdf/.legend.txt` (new,
  `scripts/generate_fig_cumulative_siteyears.R`): cumulative site-years by IGBP class, 89 mm wide,
  no panel letter, calling the same `fig_cumulative_siteyears_igbp()` function and pinned inputs as
  the retired `fig_01b`/Dur11. Verified all 15 IGBP legend swatches render with distinct non-white
  fill colours (sampled directly from the PNG, not eyeballed) -- rule 7's colour-box check.
- Retired to `draft_manuscript_v1/deprecated/`: `fig_01.*`, `fig_01a_map_current_network.*`,
  `fig_01b_cumulative_siteyears_igbp.*`. Retired to `SupFigs/deprecated/` (new subfolder):
  `supp_map_regional.*`. All via `git mv`, not deleted.
- `scripts/build_draft_manuscript_v1.R` updated: Figure 1 map and cumulative site-years removed
  from its copy map (now built and written directly by their own scripts, the same pattern already
  used for Figure 4); stale header/"Fig 1B note" comments corrected to match. Script re-run clean
  (2 figures, 2 legends: Figure 2 and Figure 3 only, as expected).
- Invariant check (rule 5): `data/snapshots/representativeness_metrics_fig4.csv` untouched by this
  stage (`git diff` empty) -- confirmed before committing.
- `scripts/check_figure_format.R`: 9/9 PASS (5 main-text, 4 Extended Data) after the stage.

### Numbers asked for

Final sizes:
- `fig_map_network`: **182.7 × 217.3 mm** (panel a 79.1mm + row b/c 64.3mm + row d/e 74.1mm).
- `fig_cumulative_siteyears_igbp`: **88.9 × 88.9 mm**.

Tower count in each regional panel and outside all four (task expected 362, 198, 103, 53, 65):

| Panel | Region | n (towers) | Expected |
|---|---|---|---|
| a (outside all 4) | — | 65 | 65 |
| b | North America | 362 | 362 |
| c | Europe | 198 | 198 |
| d | East and Southeast Asia | 103 | 103 |
| e | Australia and New Zealand | 53 | 53 |

### Decisions for Dave

- `fig_map_network`'s final height, 217.3 mm, is above the task's "aim for 200mm tall or less"
  but comfortably under Nature's hard 247mm main-text limit (`scripts/check_figure_format.R`
  PASS). The ~17mm gap is driven almost entirely by panel a: at the full 183mm main-text width,
  the Equal Earth world map's own true aspect ratio requires 79.1mm of height by itself, before
  any of the four regional panels are added. Getting under 200mm would need either shrinking
  panel a below the figure's full width (reintroducing a letterboxed/blank margin beside it --
  the same defect this stage's row-height fix was built to remove from the regional panels), or
  narrowing a region's lon/lat extent (off limits per rule 3, and would also change panel site
  counts away from the task's expected 362/198/103/53/65). Took the option that changes least:
  kept panel a full-bleed at 183mm and accepted 217.3mm. Flagging per rule 2 rather than guessing
  which trade-off Dave would prefer.
- Scale-bar and letter placement candidates are tested on a 4%-step grid (`.grid_candidates()` in
  `scripts/generate_fig_map_network.R`) purely against land polygons -- they do not check for
  overlap with tower points or with each other (bar vs. letter). None collided in this render
  (confirmed visually), but a future network update that shifts point density, or a region extent
  change, could in principle need a re-check; the grid search would still run automatically, just
  worth knowing it isn't point-aware.

---

## Stage 3 — supplementary trajectory figure: DONE (closed out 2026-10-02, close-out session)

Found run to completion but never closed out (no status entry, no commit, no session log) by a
prior session: `scripts/figure_representativeness_trajectory.R` and its full output set (figure,
table, log) existed on disk, uncommitted, with the script's own validation already passing. This
close-out session re-ran the script fresh to reconfirm the pass, renamed its `SupFigs/` copy to
the Stage 6 numbering (`figS6_representativeness_trajectory`, the next number after
`figS5_flux_representativeness` — Stage 6 had stopped at S5 specifically because Stage 3 was not
yet DONE), corrected its legend text's stale "Extended Data"/"Figure 4" references to
"Supplementary Figure"/"Figure 5" (the Stage 6 terminology/renumbering postdates this script's
original, never-closed-out run), and committed everything.

- **Required check (rule, and the task's own gate):** current-network Geo-vs-Geo values reproduce
  `representativeness_metrics_fig4.csv`'s six rows exactly — re-confirmed on this fresh run
  (`logs/figure_representativeness_trajectory_20261002_221901.log`): "Validation PASSED: all six
  current-network Geo-vs-Geo rows match representativeness_metrics_fig4.csv exactly." Per-panel
  fresh-extraction validations also passed: Panel B's fresh MODIS IGBP extraction matches
  `site_igbp_fig4.csv` exactly (781/781 sites); Panels E/F's fresh TRENDY bilinear extraction +
  `classify_flux_sites()` reproduces the committed `bin_geo` exactly for all 781 NEE and 781 ET
  current-network sites.
- Figure staged as `SupFigs/figS6_representativeness_trajectory.png/.pdf/.jpg/.legend.txt` (source,
  unchanged name: `review/figures/representativeness/supp_representativeness_trajectory.*`). Table:
  `data/snapshots/representativeness_metrics_trajectory.csv` (24 rows: 6 axes x 4 networks).
- Following Stage 5's own conditional item (now unblocked): all 46 remaining
  `scripts/figure_representativeness_summary.R` outputs (`fig_rep001`-`fig_rep018`,
  `fig_representativeness_*`, `.png` + `.legend.txt`) `git mv`'d into
  `review/figures/representativeness/deprecated/`; `docs/figure_inventory.md` updated to mark that
  script superseded.
- Invariant (rule 5): `data/snapshots/representativeness_metrics_fig4.csv` unchanged by this stage
  (not read/write target of this script beyond the validation read above; `git diff` empty).
- `scripts/check_figure_format.R`: 11/11 PASS after this stage (the new `figS6_
  representativeness_trajectory ed 119.9x99.8mm OK 28w 0.25-0.71pt PASS`).
- Visual check (rule 7): PNG opened directly — clean line chart, no clipped/overlapping text, no
  stray blank bands; legend box (axis colours) fully inside the plot panel.

### Numbers asked for

J and n (n_classified / n_eligible) for every network and axis, Geo vs Geo
(`data/snapshots/representativeness_metrics_trajectory.csv`):

| Axis | Marconi (35) | La Thuile (252) | FLUXNET2015 (212) | Current (781) |
|---|---|---|---|---|
| A Koppen-Geiger | n=35, J=0.223 | n=252, J=0.292 | n=212, J=0.365 | n=781, J=0.372 |
| B Land cover (IGBP) | n=35, J=0.354 | n=252, J=0.396 | n=212, J=0.491 | n=781, J=0.495 |
| C Aridity | n=35, J=0.479 | n=250, J=0.532 | n=212, J=0.598 | n=781, J=0.666 |
| D Biomass | n=35, J=0.409 | n=252, J=0.532 | n=212, J=0.554 | n=781, J=0.636 |
| E NEE | n=35, J=0.494 | n=252, J=0.445 | n=212, J=0.466 | n=781, J=0.530 |
| F ET | n=35, J=0.458 | n=252, J=0.385 | n=212, J=0.410 | n=781, J=0.456 |

Every `n_classified` equals `n_eligible` (every site on every axis/network was classified) except
one: Panel C (aridity), La Thuile, n_classified=250 of n_eligible=252.

Historical sites that could not be classified, with reason: **2**, both on panel C (aridity),
La Thuile network only — **CN-Do2** and **CN-Do3** (CGIAR Aridity Index v3.1 `unep_class_7` is `NA`
in the existing `data/snapshots/site_aridity_la_thuile.csv` at these two towers' coordinates — a
pre-existing extraction result, not recomputed by this script). No other axis/network combination
had any unclassified site.

### Decisions for Dave

- Line-colour role assignment (Okabe-Ito palette, one colour per axis) carried over unchanged from
  the original (never-closed-out) run: IGBP takes the colour role the retired
  `fig_05_jaccard_trajectory_with_counts.png`'s LULC axis had, since both are land-cover axes — a
  judgement call made in the original run, not re-litigated in this close-out.
- The retirement of `scripts/figure_representativeness_summary.R`'s 46 remaining outputs (above)
  was Stage 5's own conditional item, carried out now that its Stage-3 dependency is DONE — not a
  new decision, just the originally-planned follow-through.

Full write-up: `SESSION_LOG.md`, close-out session entry, 2026-10-02.

---

## Stage 4 — supplementary flux figure, both versions side by side: DONE

- New `review/figures/representativeness/supp_flux_representativeness.png/.pdf/.jpg/.legend.txt`
  (source) + `data/snapshots/representativeness_metrics_flux_supp.csv`, via new
  `scripts/figure_flux_representativeness_supp.R`; copied to
  `draft_manuscript_v1/SupFigs/supp_flux_representativeness.*`. No dependency on Stage 3 (that
  stage's trajectory figure isn't used by this one), so this stage ran regardless of Stage 3's
  status in this file.
- Layout: 4 rows (NEE, GPP, TER, ET) x 2 columns (Geo vs Geo left, Geo vs Data right), panels
  lettered a-h across rows, in Figure 4's own print-rendering style (`draw_panel2()` and its
  supporting layout/measurement functions, ported verbatim from
  `scripts/figure4_representativeness.R`, not sourced -- that script has rendering side effects).
- NEE (a/b) and ET (g/h): Figure 4's own rasters, tower values and binning method
  (`run_flux_panel()`, ported), reused unchanged. Checked programmatically (not just visually)
  before the figure was drawn: all four n/J values match
  `data/snapshots/representativeness_metrics_fig4.csv` rows E/F exactly (see Numbers below) --
  the task's explicit pass/fail gate.
- GPP (c/d) and TER (e/f): new. Model side is the 17-model TRENDY v14 S3 ensemble-median,
  1991-2020 mean (TER = ra+rh), on Figure 4's own Koppen 0.5 deg land mask --
  `data/external/trendy/derived/candidate_gpp_median.tif`/`candidate_ter_median.tif`, already built
  by `scripts/candidate_nee_gpp_ter_panels.R` and already loaded by `figure4_representativeness.R`
  itself as its NEE bar-1 mask -- reused as-is, not recomputed. Tower side is the site median from
  `R/site_annual_fluxes.R::compute_site_annual_fluxes()` (`gpp_median`/`reco_median`,
  `QC_THRESHOLD_YY`), per the task instruction -- deliberately NOT
  `scripts/diagnostics/flux_bin_breaks.R`'s own older QC>=0.80 mean-monthly-cycle tower values,
  which `figure4_representativeness.R`'s own header note already flags as wrong for this paper.
  Bar 1 = own model value <5 gC/m2/yr; bars 2-7 are the rounded (to 100 gC/m2/yr) sextiles of the
  50/50 land/tower mixture CDF outside bar 1 -- same histogram/rounding steps
  `flux_bin_breaks.R` uses for these two fluxes, but the resulting edges differ from that script's
  own GPP/TER edges because the tower-side distribution differs (different QC method, as above).
  Geo vs Data requires a qualifying tower value to be classified at all (`require_own = TRUE`),
  same rule NEE/ET already use.
- Colours: NEE/ET ramps copied verbatim from `figure4_representativeness.R` (same rasters, same
  panel style, must look identical to Figure 4's own e/f panels). GPP (green) and TER (orange)
  ramps reused from `flux_bin_breaks.R`'s own choices (GPP: task-specified family; TER: that
  script's own judgement call, kept here rather than re-litigated) with bar 1 overridden to the
  shared bare/ice colour (`#f7f4f9`, Figure 4's biomass bin-1 colour) on every row, matching
  Figure 4's convention.
- Invariant (rule 5): `data/snapshots/representativeness_metrics_fig4.csv` untouched by this stage
  (`git diff` empty) -- confirmed before and after.
- `scripts/check_figure_format.R`: 11/11 PASS after the stage (the new figure:
  `supp_flux_representativeness ed 179.9x172.5mm OK* 358w 0.25-1.00 PASS`).
- Visual check (rule 7): PNG opened directly -- no clipped or overlapping text, no inappropriate
  blank bands (only the same ~4.4mm inter-row panel-margin gaps Figure 4's own rows have), `%
  land`/`towers` headers present above panels a/b only (by design, same convention as Figure 4: the
  two-number meaning is identical for every row). No separate legend/key swatch grid exists in this
  figure (each row's 7 bars ARE the colour-coded bins, labelled directly on their own axis, unlike
  Figure 2's/`supp_whittaker`'s external key) -- verified instead, per rule 7's spirit, by pixel-
  matching each flux's most extreme bin colour (GPP `#00441b`, TER `#7f2704`, ET `#06305a`, NEE
  `#0b3e09`) against the rendered PNG: tens of thousands of matching pixels each, confirming no bin
  rendered blank/white even for the visually thinnest bars (e.g. GPP/TER's ">2000"/">1800" rows,
  13.8%/15.7% land share).

### Numbers asked for

Bin edges, n and J for all eight panels (`data/snapshots/representativeness_metrics_flux_supp.csv`):

| Panel | Flux | Comparison | Bin edges (bars 2-7 boundaries) | n | J |
|---|---|---|---|---|---|
| a | NEE | Geo vs Geo  | -250, -100, -50, -25, 0 | 781 | 0.530 |
| b | NEE | Geo vs Data | -250, -100, -50, -25, 0 | 656 | 0.165 |
| c | GPP | Geo vs Geo  | 300, 700, 1100, 1500, 2000 | 781 | 0.488 |
| d | GPP | Geo vs Data | 300, 700, 1100, 1500, 2000 | 651 | 0.529 |
| e | TER | Geo vs Geo  | 300, 600, 1000, 1300, 1800 | 781 | 0.482 |
| f | TER | Geo vs Data | 300, 600, 1000, 1300, 1800 | 651 | 0.511 |
| g | ET  | Geo vs Geo  | 200, 350, 450, 600, 850 | 781 | 0.456 |
| h | ET  | Geo vs Data | 200, 350, 450, 600, 850 | 665 | 0.479 |

NEE (a/b) and ET (g/h) n/J confirmed identical to `representativeness_metrics_fig4.csv` rows E/F
(n=781/656 J=0.5298754549363103/0.16481578809724606 for NEE; n=781/665
J=0.45626639245125744/0.47933317597575115 for ET -- matched to the 1e-9 tolerance
`figure4_representativeness.R`'s own self-check uses, not just the 3 d.p. shown above).

Final figure size: 180 x 172.7 mm.

### Decisions for Dave

- GPP/TER's bin edges (c/d, e/f above) do NOT match `scripts/diagnostics/flux_bin_breaks.R`'s own
  GPP/TER edges from its earlier diagnostic run, even though both use the same rasters, bar-1 cut
  and rounding step. The difference is deliberate, per this stage's explicit task instruction
  (tower values must come from `compute_site_annual_fluxes()`/`QC_THRESHOLD_YY`, not that script's
  own older QC>=0.80 mean-monthly-cycle method) -- flagging in case the discrepancy is noticed
  without this context.
- TER's colour ramp (orange/brown) was `flux_bin_breaks.R`'s own judgement call, not specified by
  any task -- reused here rather than choosing independently, so the two diagnostics stay visually
  consistent if ever compared side by side. Flagging per rule 2 since it wasn't re-decided fresh.
- This figure's panel style has no separate legend/key swatch (each bin's colour is on its own
  labelled bar), unlike Figure 2/`supp_whittaker_nee_gpp_ter`'s external key -- rule 7's "every key
  label has its colour box" was therefore satisfied by direct pixel-matching of bin fill colours
  instead, as described above, since there is no key grid to check.

Full write-up: `SESSION_LOG.md`, "2026-10-02 (13) — Figure stage 4: supplementary flux
representativeness figure (NEE/GPP/TER/ET, Geo vs Geo and Geo vs Data)".

---

## Stage 5 — clean-up: DONE

- Regenerated `review/figures/historical/` (6 PNGs) via
  `scripts/generate_historical_comparison_figures.R`, and the five IGBP flux-median scaffold figures
  + companion tables/metadata (`review/figures/flux_medians/fig_flux_{nep,gpp,ter,et,h}_by_igbp.png`,
  `data/snapshots/flux_medians_by_igbp_{nep,gpp,ter,et,h}.csv/.meta.json`) via
  `scripts/figure_flux_medians_by_igbp.R`. Both ran clean against current tables, no code changes, no
  path fixes needed.
- Retired, via `git mv` into a new `review/figures/candidates/deprecated/` (companions included):
  `Supp_sampling_ratio_siteKG_IGBP_NEE_ET.*`, `Supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.*`,
  `Supp_compare_geospatial_vs_sitelevel_grid.*`, `ALT_fig_03_flux_comparison_combo_nep_et_h.*`.
- Stage 3's conditional item (retire `scripts/figure_representativeness_summary.R`'s outputs,
  mark it superseded in `docs/figure_inventory.md`) **SKIPPED**: this file has no Stage 3 entry
  (jumps Stage 2 → Stage 4), so Stage 3 is not DONE.
- `review/diagnostics/` and `data/snapshots/` left untouched beyond the five named table/metadata
  pairs above, per the task's own instruction.
- Invariant (rule 5): `data/snapshots/representativeness_metrics_fig4.csv` unchanged (MD5
  `85a1c086ad51aa27d40114c9c6d0e5d9` before and after).
- `scripts/check_figure_format.R`: 11/11 PASS (this stage touched nothing it checks).
- Visual check (rule 7): all 11 regenerated PNGs opened. Clean except a pre-existing defect, present
  identically in the last-committed version (confirmed via `git show HEAD`) and therefore not a
  regression from this stage: in all three `fig_compare_whittaker_{2000,2007,2015}.png`, the left
  panel's title text is overlapped by the inset NEE-legend box beneath it, leaving a stray `"(r"`/`")"`
  fragment. Left unfixed — see Decisions below.

### Numbers asked for

What was regenerated, what was retired, what was left (and why):

| Action | Items |
|---|---|
| Regenerated | `review/figures/historical/` (6 PNGs, `generate_historical_comparison_figures.R`); `review/figures/flux_medians/` (5 PNGs + 5 CSV/meta.json pairs, `figure_flux_medians_by_igbp.R`) |
| Retired | 4 candidates (8 files incl. companions) from `review/figures/candidates/` to `review/figures/candidates/deprecated/` |
| Left, with reason | Stage 3's conditional retirement of `figure_representativeness_summary.R` outputs — Stage 3 not DONE in this file; the `fig_compare_whittaker_*` title/legend overlap — pre-existing, cosmetic, out of scope for a data-only regeneration |

### Decisions for Dave

- `fig_compare_whittaker_{2000,2007,2015}.png` (in `review/figures/historical/`, not a manuscript or
  supplementary figure): left-panel title text is overlapped by the inset legend box beneath it,
  leaving a stray `"(r"`/`")"` fragment. Confirmed pre-existing (identical in the last-committed PNG),
  cosmetic only, doesn't affect any data. Left unfixed rather than editing
  `R/figures/fig_climate.R`'s legend/annotation layout, which is outside "regenerate from current
  tables" — flagging rather than guessing whether/when a layout fix belongs here.
- Stage 3's conditional retirement item remains outstanding and will need a follow-up run once Stage 3
  is marked DONE.

Full write-up: `SESSION_LOG.md`, "2026-10-02 (14) — Figure stage 5: clean-up".

---

## Stage 6 — numbering, supplementary PDF, methods, final report: DONE

- **Dependency check (rule 1):** part (a) requires Stage 2 DONE — confirmed DONE above, proceeded.
  Part (a)'s figure list also implicitly needs Stage 3 (the `representativeness_trajectory`
  figure) to exist as a finished, numbered-able output; Stage 3 has **no entry** in this file
  (jumps Stage 2 → Stage 4), so it is not DONE. Discovered Stage 3's script, figure, table and log
  all exist on disk, **uncommitted**, with its own log reporting its current-network Geo-vs-Geo
  rows reproducing `representativeness_metrics_fig4.csv` exactly — but no status/session-log entry
  and no commit. Treated identically to how Stage 5 already treated its own Stage-3-conditional
  item: not-DONE, so excluded. `supp_representativeness_trajectory` is not part of the figS1–S5
  numbering and was left exactly as found (untouched, uncommitted). See Decisions for Dave.
- **(a) Numbering.** Main-text figures staged under `fig_01_map_network` …
  `fig_05_representativeness` (via `scripts/generate_fig_map_network.R`,
  `scripts/generate_fig_cumulative_siteyears.R` writing directly; `scripts/build_draft_manuscript_
  v1.R` copying Figures 3–4; `scripts/figure4_representativeness.R` writing Figure 5 directly).
  Supplementary figures numbered `figS1_whittaker_nee_gpp_ter` … `figS5_flux_representativeness`,
  in the task's fixed order, with no gap at "S6" (trajectory excluded per above). Every affected
  `.legend.txt`'s self-reference and figure-number cross-references updated, "Extended Data"
  replaced with "Supplementary Figure" throughout (figures and `docs/figure_inventory.md`), which
  was rewritten with a new main-text-figures table and a new Supplementary Figures table. Scripts,
  functions, and underlying data/table files keep their own names (only `draft_manuscript_v1`/
  `SupFigs` copy filenames changed) — stated explicitly in `docs/figure_inventory.md` per the task.
  All renames verified byte-identical to their pre-rename source (`shasum`/git rename-detection,
  100% similarity) — no figure content changed, only filenames and legend text.
- **(b) Supplementary PDF.** New `scripts/build_supplementary_pdf.R` (base `grid`/`grDevices` +
  the already-used `png` package, no new dependency) builds `SupFigs/supplementary_figures.pdf`:
  one figure per page, headed by "Supplementary Figure S*N*" + the legend TITLE (word-wrapped),
  image at ~200 dpi (downsampled from the 600 dpi source; the full-resolution files are
  untouched). **0.57 MB, 5 pages** — well under the 10 MB limit.
- **(c) Methods.** `docs/methods_requirements.md` §5.1–5.5 (and §5.8's header/cross-references)
  brought into line with the current code and committed tables, using a dedicated research pass
  that read every number from a committed table, script constant, or `SESSION_LOG.md` entry —
  nothing estimated. Several numbers were corrected (snapshot/hub counts, VUT/CUT/neither split,
  Marconi site-years 97→96, the precipitation-defect breakdown replacing a vague "4 sites",
  Köppen-source description reconciled across two genuinely-different current uses, the
  functionally-active-site definition corrected from a factually wrong prior description) and
  several items marked **TO CONFIRM** rather than guessed (current-network all-missing-NEE
  exclusion count, a snapshot `.meta.json`/download-audit CSV, the still-unquantified `P_F` tower
  precipitation defect). §5.4 flagged as describing an orphaned analysis not wired into any
  current figure. See Decisions for Dave and `review/figstage_report.md` for the full list.
- **(d) Final report.** `review/figstage_report.md` — status of every stage, the check script's
  table, every stage's reported numbers, and all Decisions for Dave in one list.
- `scripts/check_figure_format.R`: 11/11 PASS after all Stage 6 edits (5 main-text, 5 numbered
  Supplementary Figures, plus the untouched, un-numbered `supp_representativeness_trajectory`).
- Invariant (rule 5): `data/snapshots/representativeness_metrics_fig4.csv` unchanged throughout —
  MD5 `85a1c086ad51aa27d40114c9c6d0e5d9`, `git diff` empty before and after.
- Visual check (rule 7): every renamed PNG confirmed byte-identical to its pre-rename source (no
  re-render, so no new risk of clipping/overlap/blank bands); the new `supplementary_figures.pdf`
  rendered to PNG per page (`pdftoppm`) and inspected directly — titles wrap within the margin, no
  clipped text or images, all colour keys/swatches visible, no blank bands. (Title-wrapping was in
  fact broken on first render — long titles ran off the page edge — and fixed before this check
  passed; see `scripts/build_supplementary_pdf.R`'s `strwrap()` comment.)

### Numbers asked for

See `review/figstage_report.md` — this report consolidates every stage's requested numbers (1–5)
plus Stage 6's own numbering map, check-script table, and supplementary-PDF size, in one place per
the task's instruction for part (d).

### Decisions for Dave

- Stage 3 (`representativeness_trajectory`) was found fully run but never closed out — script,
  figure, table and log all exist uncommitted, with internal validation passing. Stage 6 did not
  commit or finalise it (not its task) and excluded it from the Supplementary Figure numbering.
  Recommend either re-running Stage 3 to completion (commit + this status file + session log) or
  explicitly deciding to drop it — the work is real and shouldn't sit uncommitted indefinitely.
- The supplementary PDF downsamples each figure to ~200 dpi to stay well under the 10 MB limit — a
  deliberate quality/size trade-off for a reading bundle; full-resolution originals are unchanged.
- Two `docs/methods_requirements.md` facts could not be verified from any committed source and are
  marked TO CONFIRM rather than guessed: a current-network (781-site) count of sites excluded for
  all-missing NEE, and a snapshot metadata sidecar / download-audit CSV scoped to the locked
  2026-09-01 snapshot. Recommend deciding whether either is needed and, if so, generating it.
- §5.4 of the methods doc (derived metrics / anomaly figures) was flagged, not rewritten or
  removed, as describing analysis code (`R/figures/fig_anomaly_context.R` and its callers) that is
  not wired into any currently-produced figure and hasn't been touched since 2026-04-18.

Full write-up: `SESSION_LOG.md`, "2026-10-02 (15) — Figure stage 6: numbering, supplementary PDF,
methods, final report".
