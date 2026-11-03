# WAFNET x FLUXCOM comparison — methods memo

Side analysis, not part of the FLUXNET Annual Paper 2026 (see `../README.md`).
Every filtering decision and assumption made in `code/` is recorded here as
it is made, same convention as `WAFNET/energy_partitioning/docs/methods_memo.md`.
Stage 1 (pre-analysis only, no download, no statistics) is fully recorded in
`report_back_20261014.md`; this file starts at Stage 2, where the first
real filtering/derivation decisions were made.

## Sites

Two site sets, kept separate everywhere (`code/00_config.R`):

- `STAGE2_SITES` (five, agreed): `GH-Ank`, `BJ-Nhu`, `BJ-Bfg`, `SN-Dhr`,
  `ML-Kem` (added 2026-11-03 at Ossénatou Mamadou's request — Kelma, Mali,
  WSA, 2007–2009).
- `STAGE2_PENDING_SITES` (two, flagged `pending Ossénatou`): `BJ-Db1`,
  `SN-Nkr` — excluded from the agreed set in Stage 1 only for lack of
  temporal overlap with the originally-chosen product, re-checked against
  FLUXCOM-X-BASE's longer period in Stage 2 (`BJ-Db1`: still zero overlap;
  `SN-Nkr`: now has 46/80 overlapping months, 3/6 overlapping years).

## Product choice (2026-11-03)

The original FLUXCOM product (RS+METEO) Ossénatou's 16 October reply chose
is currently unobtainable (MPI-BGC portal link 404, no longer listed).
**Decision (Dave):** proceed with FLUXCOM-X-BASE instead, keep the original
as an option if it resurfaces. A documentation-only backup copy of the
original ensemble (gpp/reco/le/sh) was pulled from the ILAMB reference
collection and is explicitly **not used** in any comparison — see
`report_back_2026-11-03.md` section 2 for its own metadata.

## Partition match (2026-11-03) — a correction from Stage 1's default

Stage 1 used the tower's **DT** partition (`GPP_DT`/`RECO_DT`) by default,
per the WAF-Net paper's general preference (documented in `../README.md`).
FLUXCOM-X-BASE's own ICOS metadata states its GPP is trained on the
**nighttime (NT)** partition only (`report_back_2026-11-03.md` section 3,
high confidence, read directly from the product description, not inferred).
**All X-BASE GPP/RECO comparisons therefore use `GPP_NT`/`RECO_NT`**, not
`GPP_DT`/`RECO_DT`. The DT rows already written to
`tables/site_variable_qc_overlap_{yy,mm}.csv` in Stage 1 are **kept, not
removed** — `code/06_tower_data_update.R` adds NT rows alongside them,
superset not replacement, in case a future DT-partitioned product (e.g. the
original FLUXCOM carbon line, if it becomes available again) is compared.

**`GH-Ank` has no NT-partitioned GPP/RECO at all** in its FLUXMET extract
(`GPP_NT_VUT_REF`/`RECO_NT_VUT_REF` are 100% `NA` across all 48 MM rows,
checked directly; `GPP_DT_VUT_REF`/`RECO_DT_VUT_REF` are present for 34 of
48). This is a genuine data-availability gap, not a QC exclusion — `GH-Ank`
is skipped for GPP/RECO only (NEE and LE are unaffected, since neither
depends on the NT/DT partition).

## NEE sign convention (2026-11-03) — checked, not assumed

X-BASE's own netCDF/ICOS metadata carries no explicit sign-convention
attribute. Checked empirically against `GH-Ank` 2001 (a known strong
tropical carbon sink): X-BASE GPP is positive year-round, X-BASE NEE is
negative year-round — the same atmospheric convention
(negative = ecosystem uptake, positive = release) as the tower's
`NEE_VUT_REF`/`NEE_CUT_REF`. **No sign flip applied.**

## RECO derivation (2026-11-03)

X-BASE distributes no respiration variable. `RECO_derived = GPP + NEE`
(same X-BASE series, same sign convention as above). Labelled `derived`
(a `TRUE`/`FALSE` column) in every table and figure that shows it. Not a
measured-against-measured comparison — this is explicit in
`report_back_2026-11-03.md` and should be stated wherever these numbers are
reused.

## ET -> LE conversion (2026-11-03)

X-BASE's `ET`/`ET_T` are **mean rates in `mm hr-1`**, confirmed by reading
the downloaded netCDF unit attribute directly (not assumed from the paper,
which reports only aggregated km³ totals). Converted using the same
λ = 2.45×10⁶ J/kg as `R/units.R`'s own LE conversion:
`LE[W/m2] = ET[mm/hr] * λ / 3600` (1 mm of water over 1 m² is exactly 1 kg
at ρ_water = 1000 kg/m³, the identity underlying "mm of water" itself — no
separate density factor is needed beyond that). `H` has no X-BASE
equivalent at all and is never compared.

## Carbon unit match (2026-11-03) — no conversion applied

X-BASE's GPP/NEE at monthly resolution are `gC m-2 d-1` mean daily rates —
the **same unit** the tower's native MM FLUXMET extract already uses
(confirmed independently twice: Stage 1's BIFVARINFO read, and this stage's
direct netCDF units-attribute read). Compared as distributed; **not**
multiplied by days-in-month (that `R/units.R` conversion turns a mean rate
into a period *total*, which this comparison does not need).

## Statistics convention (carried from Stage 1 / Ossénatou's reply)

- Monthly statistics are the primary result.
- Annual statistics are a secondary look, computed **only** for site × flux
  combinations with ≥ 3 overlapping annual values, and only from calendar
  years with all 12 qualifying months present (`code/08_extract_statistics.R`).
- **Bias = product (X-BASE) minus tower**, stated in every output.
- Training sites (`GH-Ank`, `SN-Dhr`, confirmed section 3) marked with a
  distinct symbol (triangle) in every figure; pending sites (`BJ-Db1`,
  `SN-Nkr`) with another (square) and a `site_status` column in every table.

## Figure fix, first pass (2026-11-03)

`code/09_figures_draft.R`'s site-bias map initially used one shared
colour/size scale across all four flux panels — carbon fluxes (O(1–5)
gC m⁻² d⁻¹) and LE (O(10–40) W m⁻²) are on different magnitude scales, so
the carbon panels' bubbles were nearly invisible next to LE's. Fixed by
giving each flux panel its own independent colour/size scale
(`patchwork::wrap_plots()` of four separately-scaled `ggplot`s, not
`facet_wrap()` with a single shared scale) before committing.

## Figure fixes, follow-up pass (2026-11-03)

Four more issues found on a later review
(`logs/prompts/2026-11-03_wafnet_fluxcom_x_stage2_fixes.md`), fixed without
any new download:

- **RECO-derivation wording error**, not a code bug: this memo's own RECO
  entry above was already correct (`GPP + NEE`), but `report_back_2026-11-03.md`
  and `08_extract_statistics.R`'s header comment said "GPP minus net
  ecosystem exchange" in two places — wrong, since the code has always
  computed `GPP + NEE`. With NEE negative during uptake, `GPP + NEE` is "GPP
  minus the uptake magnitude," not "GPP minus NEE." Fixed in both places.
- **One shared site palette.** `STAGE2_SITE_COLORS` (00_config.R, keyed by
  `STAGE2_ALL_SITES`) is now the single source of site colour for every
  Stage 2 figure (scatter, map, Taylor, climatology) — previously each
  figure computed its own colours independently (the Taylor diagrams already
  used `hcl.colors(..., "Dark 3")`; the scatter plot used ggplot's default
  hue scale; the map had no site-identity colour at all). `site_coords_for()`
  and `site_igbp_for()` (BIF reads) also moved into `00_config.R` so
  `08`/`09`/`10` share one implementation instead of three.
- **Site map: nudge + leader line for near-overlapping sites.** `BJ-Nhu`/
  `BJ-Bfg` (0.12° apart) and `SN-Dhr`/`SN-Nkr` (~1.0–1.4° apart) had
  genuinely overlapping symbols at the map's original fixed West-Africa-wide
  extent. Fixed with a small hardcoded per-site `(dlon, dlat)` nudge for just
  those four sites, plus a thin dashed `geom_segment` leader line (site
  colour) from the nudged plotting position back to the true
  `(lon_true, lat_true)`. This also required switching the point shapes from
  solid (15/16/17) to fill-capable (21/22/24), so the border colour could
  carry site identity (new palette) while the fill still carries the
  continuous bias value — two encodings on one symbol. Map extent trimmed
  from a fixed `(-20, 15) x (0, 20)` box to the sites' own coordinate range
  ± 2–2.5°, which incidentally also reduced how much the fixed-size nudge
  mattered relative to the panel's zoom level.
- **Taylor diagram "tower" label clipping.** Root cause: the label is placed
  at y = -0.09, just below the x-axis, which is **outside R's default plot
  clip region** (`ylim` starts at 0) — not a font-size or overlap problem.
  Fixed with `par(xpd = NA)` around that one `text()` call, restored
  immediately after via `par(op)`.

### New: mean seasonal cycle (climatology) figures (2026-11-03)

`code/10_climatology.R`. Built entirely from `tables/xbase_monthly_values.csv`
(08's output) grouped by `site_id`/`flux`/`month` — this is deliberate: it
guarantees the tower and X-BASE climatology series cover **exactly** the
same site-months as the Stage 2 statistics, because they're two columns of
the same already-QC-filtered row set, not two independently-aggregated
series that might drift apart in coverage.

- Ribbon vs. points threshold: `n_years >= 3` per month (same threshold used
  for the annual secondary statistics, for consistency) gets a mean ± 1 SD
  ribbon (`geom_ribbon`, `na.rm = TRUE` so months below the threshold leave a
  gap rather than erroring); months below it get the individual years
  plotted as open points instead, via an `inner_join` of the raw monthly
  values against the thin-month `(site_id, flux, month)` keys. The mean
  *line* is still drawn through every month regardless (always well-defined
  from however many years exist) — only the ribbon/points choice depends on
  `n_years`.
- `ML-Kem` (2007-2009, at most 2 calendar years of QC-passing data per
  month) never clears the `n_years >= 3` threshold at any month in any flux
  — its figure is individual-year points throughout, by design, not a bug.
- `GH-Ank`'s GPP/RECO panels are omitted (not blanked/greyed) by returning
  `NULL` from `one_flux_panel()` when there are zero rows for that site x
  flux, filtered out of the panel list before `patchwork::wrap_plots()`. The
  reason is stated in the figure's own caption (`excluded_note()`), not left
  for the reader to infer from a missing panel.
- `patchwork::plot_layout(guides = "collect")` + `& theme(legend.position =
  "bottom")` merges each site's up-to-4 per-panel Tower/X-BASE legends into
  one shared legend at the bottom of the figure — safe here because every
  panel within one site's figure uses the identical two-colour scale (that
  site's palette colour + the fixed X-BASE colour), so there's no
  cross-panel scale conflict for `guides = "collect"` to paper over.
- The bottom caption text ran off the image's right edge on first render (no
  automatic line-wrapping in a ggplot caption) — fixed with
  `stringr::str_wrap()`, caught on the same visual-check pass that verified
  everything else.
- **Cross-site pattern, not just per-site noise:** X-BASE's seasonal
  amplitude is smaller than the tower's at every site and flux checked
  (`tables/xbase_climatology.csv`), most severely at `ML-Kem` (NEE amplitude
  ~11% of the tower's, GPP ~26%). Peak-month timing matches the tower at the
  two training sites and `SN-Nkr` (all September) but is 1-2 months early at
  the two Bénin sites and `ML-Kem`; LE's seasonal *shape* (not just
  amplitude) differs outright at `BJ-Nhu`/`BJ-Bfg`, where X-BASE peaks in
  May against the tower's September/October peak. Full numbers in
  `report_back_2026-11-03.md` section 8.

## Open items

- `BJ-Db1` cannot currently be compared against any FLUXCOM release (zero
  overlap confirmed against both the original product, Stage 1, and X-BASE,
  Stage 2) — its tower record (2022-05 onward) postdates every available
  product period.
- `SN-Nkr`'s annual secondary numbers (n=3 years) should be read as
  low-confidence per Stage 1's original caveat — a 3-year annual sample.
- `BJ-Nhu`'s LE bias has opposite sign between the full monthly sample and
  the annual (3-year) secondary look — different underlying years, not a
  contradiction, but worth knowing before either number is quoted alone
  (`report_back_2026-11-03.md` section 6).
- The original FLUXCOM product's own availability should be rechecked
  periodically; if it resurfaces, a like-for-like Reco/H/DT comparison
  becomes possible again and this memo's "no Reco/H, NT-only" constraints
  would need revisiting, not necessarily discarding (X-BASE may still be
  worth reporting alongside it for its longer period).

## Output metadata and figure captions (2026-11-03)

Every table and figure in this directory now has a `.meta.json` (CLAUDE.md "Output Metadata")
and, for figures, a `.legend.txt` caption written as a journal figure caption for Ossénatou's
paper. Shared helper: `WAFNET/write_meta.R` (`write_meta()`/`write_caption()`, same fields as
`R/utils.R::write_output_metadata()` without the repo-root `outputs/session_info.txt` append —
the `GSL_definition/` pattern), sourced once from `00_config.R`. The three boilerplate caption
paragraphs every FLUXCOM-X-BASE figure shares (data/period, sign convention, training/pending
sites) are `STAGE2_CAPTION_DATA`/`STAGE2_CAPTION_CONVENTION`/`STAGE2_CAPTION_SITES` in
`00_config.R`, so they read identically everywhere rather than being retyped per figure. All 12
captions are also compiled, in figure order, into `docs/figure_captions.md`. `04`/`05` (bash, no
R process) write their `.meta.json` with an equivalent pure-bash block instead. No table or
figure content changed in this pass — `report_back_2026-11-03.md` section 9 has the full
coverage check (every script was re-run and diffed byte-for-byte against the committed output;
the one figure using stochastic `ggrepel` label placement, `draft_site_map_bias.png`, was
reverted to the committed PNG and given a fixed seed for future reproducibility).
