# Report back: RSCOTT_AGU26 dryland GPP-vs-precipitation extraction

2026-11-23. Prompt: `logs/prompts/2026-11-23_rscott_agu26_extraction.md`. Package:
`RSCOTT_AGU26/package_rscott_2026-11-23/` (zipped as
`package_rscott_2026-11-23.zip`).

## P_F_QC meaning and how it was confirmed

`P_F_QC` is a 0-1 **fraction** at DD/MM/YY resolution (CLAUDE.md's "System 1" convention:
fraction of underlying records that are measured or good-quality gap-fill), where **higher
means more measured**. This is the opposite of the integer System-2 convention used for other
consolidated meteorological `_F_QC` columns at HH/HR resolution (`0` = measured), which an
earlier analysis in this repo mistakenly applied to this fractional field and got backwards
(`scripts/diagnostics/era5_precip_units_v3_partA.R`, finding "A4").

This repo had already established the correct polarity network-wide, independently, at both MM
resolution (`era5_precip_units_v3_partA.R`/`v4.R`) and YY resolution
(`review/diagnostics/precip_site_filter/report.md`). Rather than simply trust those prior
write-ups, this project ran its own independent control-site check
(`RSCOTT_AGU26/code/01_candidates_and_qc_check.R`, table
`RSCOTT_AGU26/tables/pf_qc_polarity_check.csv`) at three sites -- `IT-MBo` (previously examined
in this repo at HH resolution), and one Northern- and one Southern-Hemisphere site drawn from
this project's own candidate pool (`BE-Maa`, `AR-CCg`) -- by cross-checking each site's `P_F`
against the DuckDB's own `ERA5`-dataset `P_ERA` rows for bit-identity, binned by `P_F_QC`. At
every site/resolution with both a low- and high-QC tier available, low `P_F_QC` rows were far
more often bit-identical to `P_ERA` than high `P_F_QC` rows were (e.g. `IT-MBo` at MM:
`P_F_QC<=0.1` is 100% identical to `P_ERA`; `P_F_QC>=0.9` is 0% identical) -- confirming higher
`P_F_QC` = more measured. We additionally confirmed this holds at DD resolution (not previously
checked by the prior diagnostics we found), using `IT-MBo`'s full daily record: the bottom
`P_F_QC` tier (<=0.1) is 99.8% identical to `P_ERA`; the top tier is much less often identical
(16.6%, inflated above 0% by genuine zero-precipitation days where measured and ERA5 both
report exactly 0). Used throughout: measured share ~= `P_F_QC`; ERA-filled share = `1 - P_F_QC`.

## Unit check -- and an unexpected, repo-wide finding

Per the task, we reconstructed several calendar-year totals from monthly values and compared
them to the direct YY values, for both GPP_NT and P_F (`RSCOTT_AGU26/code/02_build_site_years.R`,
tables `unit_check_mm_vs_yy.csv` and `unit_check_dd_vs_yy.csv`).

- **GPP_NT**: summing the 12 `monthly_converted` values (already unit-converted by
  `R/units.R::fluxnet_convert_units()`) and comparing to `annual_converted`'s direct YY value
  gives a median |%diff| of 0.42% (max 125%, n=873 site-years) -- confirms the project's
  existing MM->YY carbon conversion is working correctly for these candidate sites. The 125%
  outlier is a single site-year (`CN-Dan` 2011); we did not investigate further since GPP/ET
  values in this package come from the already-converted project tables, not from this ad hoc
  check, and a single outlier doesn't change that conclusion.
- **P_F: an unexpected, repo-wide unit bug.** Summing the 12 raw monthly `P_F` values and
  comparing to the YY value gives a median |%diff| of **-96.72%** (raw MM-summed P is ~29-30x
  too *low*) -- at every site and year we checked (`AR-CCg`, `AT-Neu` x14 years, plus a wider
  spot check at `US-SRM`, `AU-Sil`). Multiplying each month's `P_F` by its days-in-month before
  summing closes this to 0.0026% median -- i.e. **`P_F` at MM resolution is a mean DAILY rate
  (mm/day), not a monthly total**, exactly the same undocumented convention CLAUDE.md and
  `R/units.R` already document for carbon fluxes (NEE/GPP/RECO) at MM/WW, established in the
  2026-09-28 closure test (`SESSION_LOG.md`) -- but this convention is currently **NOT**
  documented or handled for precipitation. CLAUDE.md states plainly "P: mm per timestep --
  Sum, not average", with no resolution caveat, and `R/units.R::fluxnet_convert_units()`
  deliberately leaves `P_F` untouched ("kept as-is; must be summed"). **Any other analysis in
  this repo that sums raw MM- or WW-resolution `P_F` directly for a period total has the same
  bug** -- this is not specific to this side project. We have not modified `R/units.R` or
  CLAUDE.md ourselves (out of scope for this extraction), but flag it here prominently so it can
  be triaged and fixed centrally.
  - Daily (DD) `P_F` has no such issue: summing a full calendar year of DD values reproduces
    the YY total to a median of 0.0001% (n=1719 site-years, `unit_check_dd_vs_yy.csv`) -- the
    same "a period-1 rate is already that period's total" pattern already established for
    carbon at DD. **As a result, this project builds all water-year precipitation totals and
    ERA-filled shares from DAILY data, not monthly** -- both because it sidesteps reimplementing
    an undocumented correction, and because it matches the task's own instruction to work "from
    the daily data" for the P_F/ERA-filled-share computation. Water-year GPP and ET still use
    monthly data (`monthly_converted`), which the check above confirms is already correctly
    converted.
  - **Which resolution backs which output, stated explicitly**: calendar-year `p_f_mm_yr` /
    `era_filled_share` come directly from the YY table's own `P_F`/`P_F_QC` (no reconstruction
    needed). Water-year `p_f_mm_yr` / `era_filled_share` are built by summing DAILY `P_F` (and
    `P_F_QC`-weighted measured amount) over each water year's days, requiring every day of that
    water year to be present (`n_days_present == 365/366`) or the value is left blank rather
    than reported as a partial total. GPP_NT/GPP_DT/ET at water year are summed from MM
    (`monthly_converted`), requiring all 12 months present and individually passing QC.

## Counts

- **Candidates** (IGBP in OSH/CSH/GRA/SAV/WSA, either hemisphere): **231**
  (CSH 12N; GRA 117N/29S; OSH 38N/3S; SAV 11N/3S; WSA 8N/10S).
- **Excluded**: **188** -- 79 unscreenable (<2 measured calendar years), 64 screened but mean
  measured P >= 800 mm, 45 passed the wetness screen but had <5 passing years in either time
  step. Full detail and per-site counts in `excluded_sites.csv`.
- **Kept**: **43** -- CSH 2N; GRA 16N/3S; OSH 10N; SAV 5N/2S; WSA 4N/1S (37 Northern, 6
  Southern, all 6 Southern sites Australian).

## Kept sites where the water-year convention may split the rainy season

Flag rule (stated in full in `README.md`): the month immediately before the water-year boundary
*and* the boundary month itself must **both** be among the site's top-3 wettest months (by
mean measured-precipitation climatology) for a site to be flagged -- i.e. the cut has to fall
inside a still-wet stretch, not just near the end of one. (We deliberately rejected a simpler
"driest month far from the boundary" rule first: it over-flagged ordinary NH summer-monsoon
sites whose wet season simply *ends* right before the Oct 1 cut, which is not a split at all --
see the design note in `RSCOTT_AGU26/code/03_screen_and_climatology.R`.)

Seven kept sites are flagged, all Northern Hemisphere, all with bimodal or extended
precipitation seasonality: `US-Fwf` (Flagstaff Wildfire), `US-Seg` (Sevilleta grassland),
`US-Jo2` (Jornada Experimental Range), `US-Ses` (Sevilleta shrubland), `US-Wjs` (Willard
Juniper Savannah), `US-Mpj` (Mountainair Pinyon-Juniper), `ES-Agu` (Aguamarga). The Sevilleta/
Jornada/Willard/Mountainair cluster is unsurprising -- these New Mexico/Arizona sites are known
for a biseasonal precipitation regime (summer monsoon plus winter frontal systems), so
September and October can both carry meaningful rainfall. No Southern Hemisphere kept site was
flagged -- all 6 are Australian sites whose wet season does not sit on the Jul 1 boundary.

## Anything unexpected

- The MM `P_F` unit bug above (the main finding of this run).
- One candidate (`CN-Dan`, 2011) shows an unusually large GPP_NT MM-vs-YY mismatch (125%) in
  the unit check; not investigated further (see "Unit check" above) since it doesn't affect any
  delivered value.
- `LE_CORR` (energy-balance-corrected ET) is populated for only 153/231 candidate sites (66%)
  and about 55% of candidate-site calendar-year rows -- an expected FLUXNET data-availability
  gap, not a bug, but worth stating clearly in `README.md` (done) so Russ doesn't read the many
  blank `et_le_corr_mm_yr` cells as an extraction error.
- 61 of the 188 excluded sites have **zero** years of measured precipitation on record (100%
  ERA-filled every year), including `AR-CCg`, checked directly as part of the polarity
  confirmation above -- a real data-availability pattern at some hubs, not an artifact of our
  10% threshold.
- 30 candidate sites have neither a usable `NEE_VUT_REF_QC` nor `NEE_CUT_REF_QC` (no carbon
  source could be assigned); these simply never pass the GPP gate and so were excluded via the
  normal "too few passing years" or "unscreenable" paths -- no special handling was needed.

## Deviations from the spec, and why

- Step 2 literally says "from the daily data, compute... the total P_F and the share... ERA-
  filled" for "every site-year (both time steps in step 4)", while step 4 names YY/MM as the
  two time steps' sources. We resolved this by using YY directly for calendar-year precipitation
  (it already is a validated daily-data-backed total -- see unit check) and DD (not MM) for
  water-year precipitation, for the unit-bug reasons above. This uses daily data more, not less,
  than a literal MM-only reading would have -- and we believe it is what the spec intended,
  given it explicitly invoked "daily data" for exactly this part of the task.
- Everything else was implemented as specified; no other deviations.
