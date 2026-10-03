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
