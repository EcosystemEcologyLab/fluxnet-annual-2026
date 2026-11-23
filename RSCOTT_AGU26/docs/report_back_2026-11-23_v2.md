# Report back: RSCOTT_AGU26 v2 -- widened dryland GPP-vs-precipitation selection

2026-11-23. Builds on v1 (`ea96d41`/`4ab4192`, `docs/report_back_2026-11-23.md`). Prompt:
`logs/prompts/2026-11-23_rscott_agu26_v2.md`. Package: `RSCOTT_AGU26/package_rscott_2026-11-23_v2/`
(zipped as `package_rscott_2026-11-23_v2.zip`). v1's package is left unchanged; a new top-level
`RSCOTT_AGU26/README.md` marks it superseded and points to v2.

This report covers only what v2 adds. P_F_QC polarity, the DD-based water-year precipitation
construction (working around the MM `P_F` mean-daily-rate bug), and the VUT/CUT-per-site rule
are all unchanged from v1 and not re-derived here -- see `docs/report_back_2026-11-23.md`. Per
the task, we have not interpreted the GPP-precipitation relationship anywhere below.

## Counts

**Candidates**: unchanged, 231 (IGBP OSH/CSH/GRA/SAV/WSA, either hemisphere).

**Kept: 54** (up from 43 in v1), by class x hemisphere:

| IGBP | North | South |
|---|---|---|
| CSH | 2 | 0 |
| GRA | 22 | 4 |
| OSH | 13 | 0 |
| SAV | 5 | 2 |
| WSA | 5 | 1 |

47 Northern, 7 Southern. By precipitation source: **53 `measured`, 1 `ERA5-only`** (`CN-Sdq`,
Sidaoqiao, OSH, China -- mean annual `P_F` 130.9 mm, 0 measured calendar years on record, 100%
ERA5-filled every year).

**Excluded: 177** -- 112 screened but mean annual P >= 800 mm, 65 passed the wetness screen but
had fewer than 5 relaxed-passing years in either time step. **Zero sites were "unscreenable"**
under the relaxed rule (every one of the 231 candidates has at least one calendar year with some
`P_F` value, so a mean can always be computed) -- this category, present in v1 (79 sites), no
longer exists in v2 by construction.

## How many sites the relaxation added, and under which rule

**11 sites** are kept in v2 that were not kept in v1 (`RSCOTT_AGU26/tables/newly_kept_v2.csv`):

- **1 site** via the **precip_source relaxation** (was unscreenable in v1, which required >=2
  measured calendar years and found 0 for this site): `CN-Sdq`. In v2 it is classified
  `ERA5-only` (0 measured years, so the "measured" path never applies to it), its 800mm screen
  uses the mean of ALL 12 calendar years (130.9 mm, passes), and it reaches the keep threshold on
  `n_passing_calendar_relaxed = 10` (>= 5) -- all 10 of those years have `P_F` present but 0 of
  them have measured-quality precipitation, which is exactly the kind of site the precip_source
  relaxation (not the keep-count relaxation) was designed to admit.
- **10 sites** via the **relaxed keep-count** (already screenable and screen-passing in v1, but
  excluded there for having fewer than 5 years where GPP+ET+**measured** precipitation all
  passed; now reach 5 once the precipitation condition only requires `P_F` to be present):
  `AU-Rig`, `BE-Dor`, `CN-Dan`, `CN-Ha2`, `US-CdM`, `US-KLS`, `US-xAE`, `US-xJR`, `US-xKA`,
  `US-xWD`.

No site was dropped going from v1 to v2 -- v1's keep rule is strictly more restrictive than v2's
(same screen formula for sites with >=2 measured years, and the keep-count condition only
loosens), so every v1-kept site is still kept in v2; we did not separately re-verify this as an
exhaustive set-containment check beyond confirming the count (54 >= 43) and that the relaxations
only ever widen a pass condition, never narrow one.

## US-Aud

Already kept in v1 (confirmed by checking `package_rscott_2026-11-23/sites.csv` before writing
any v2 code) and remains kept in v2 (`precip_source = measured`, `mean_annual_p_mm = 386.8`). No
stray management/cold/mesic filter exists anywhere in the v1 or v2 code (`code/01`-`08`) that
could have dropped it -- the instruction to "keep US-Aud" required no code change, only
confirmation, which we did and are logging here as the task asked.

## Sources of the new `sites.csv` columns, and NA counts

All four new sourced columns are fully populated for all 54 kept sites -- **zero NA** in every
case, for this particular site set (not a general guarantee for the whole 231-candidate pool --
see caveats below):

- **`mean_ta_f_degc`** (native degC): `data/duckdb/fluxnet.duckdb::annual`, raw `TA_F`/`TA_F_QC`
  (not `annual_converted`, which would be Kelvin), averaged over calendar years passing
  `qc_passes(TA_F_QC, QC_THRESHOLD_YY)`. 54/54 kept sites have at least one qualifying year.
- **`koppen_class_paper`**: `data/snapshots/site_koppen_era5_fig4.csv` (`panel_a_class_used`,
  gated on `panel_a_eligible`), built by `scripts/figure4_representativeness.R`'s panel A. We
  read the actual panel-A section of that script (not just its output file) to confirm the
  logic: it uses the site's PI-reported BADM `CLIMATE_KOEPPEN` class where one exists and is
  canonical-Koppen-valid, falling back to the site's own 1991-2020 ERA5-local classification
  only when no PI class exists -- and a PI-sourced site is **never** excluded by the
  precipitation-reliability rules that can exclude an ERA5-fallback site. 54/54 kept sites are
  eligible; none of this project's 54 kept sites happen to fall in the `excluded_grp_era_down` /
  `excluded_p_era_ratio_high` / `excluded_p_era_ratio_low` fallback-exclusion categories.
- **`aridity_index_paper`**: `data/snapshots/site_aridity_era5_fig4.csv` (`ai_value`), built by
  the same script's panel C, `fao56_et0_mm()` -- AI = 1991-2020 mean `P_ERA` / FAO-56
  Penman-Monteith reference ET computed from the site's **own** ERA5 monthly climatology. This is
  explicitly **not** the CGIAR Aridity Index v3.1 raster (`data/external/aridity/`) used
  elsewhere in this repo for a different (gridded-WorldClim) representativeness panel -- we
  confirmed this distinction by reading both panels' code before choosing the source file, since
  the task was specific about wanting the site's-own-ERA5-record version. 54/54 kept sites are
  neither `invalid_era5_input` nor excluded by the panel's own dual-reference precipitation-ratio
  screen; spot-checked `US-Aud`'s value (0.29, plausible for a `BS`-class semi-arid grassland)
  against its Koppen class as a basic sanity check.
- **`n_siteyears_negative_gpp`**: computed directly from `site_years_candidates_full.csv` (both
  time steps, `GPP_NT < 0 | GPP_DT < 0`), no external source needed. 4 of the 54 kept sites have
  at least one: `AU-TTE` (3), `US-Aud` (5), `US-Ses` (1), `US-Whs` (1). One further candidate
  site with a negative-GPP year exists in the full 231-site pool but was not kept.

**Caveat on "zero NA"**: this is a property of which 54 sites happen to pass the GPP/ET/
precipitation screen, not a claim that `site_koppen_era5_fig4.csv`/`site_aridity_era5_fig4.csv`
cover every FLUXNET site unconditionally -- both files do carry NAs/exclusion flags for some of
the 781-site current network (4 sites network-wide have `invalid_era5_input`; more have
Koppen/aridity panel exclusions), they just don't happen to intersect this particular dryland,
sub-800mm, GPP/ET-data-rich site set. If a future re-run with different screening thresholds
pulls in sites from the ERA5-downscaling-regression-unfitted or implausible-PET groups, expect
NAs in these two columns and check `koppen_na_reason`/`aridity_na_reason`.

## Anything unexpected

- No new data-quality surprises beyond v1's (the MM `P_F` unit bug, still flagged in v1's
  report). The relaxation itself behaved exactly as the keep-rule logic predicts: every newly
  kept site falls cleanly into one of the two relaxation categories, with no edge cases requiring
  manual judgement calls.
- All four new sourced columns turned out fully populated for this site set, which was not
  guaranteed in advance -- we checked for NAs rather than assuming this, see above.
- The rainy-season-split-risk flag set is **identical to v1's seven sites** (`US-Fwf`, `US-Seg`,
  `US-Jo2`, `US-Ses`, `US-Wjs`, `US-Mpj`, `ES-Agu`) even after recomputing the climatology for
  the full 54-site v2 set from scratch -- none of the 11 newly-added sites trip the flag.

## Deviations from the spec, and why

- The task names the output report path as `docs/report_back_<date>.md` with the same `<date>`
  (2026-11-23) as v1's report, which already exists at that exact path. We did not overwrite it
  -- this file is `report_back_2026-11-23_v2.md` instead, matching the `_v2` suffix already used
  for the package directory/zip and the prompt-log filename, so v1's report-back stays intact as
  the task also implicitly requires ("leave the first package unchanged").
- Everything else was implemented as specified; no other deviations.
