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
