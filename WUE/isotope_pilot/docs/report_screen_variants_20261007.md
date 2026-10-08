# Stage 2 screen-variant check (2026-10-07)

Side analysis, read-and-report, on the three stage 2 test sites (US-Fuf, US-Ho2, US-MMS).
Stage 2 itself stays on hold -- 07_apply_screens.R was not re-run and no stage 2 table was
overwritten.

**Question:** how many valid days per kept site-year survive the Zhou et al. (2015) screens
when (i) the rain source, (ii) the radiation condition in screen c, and (iii, added
2026-10-09) the GPP day test's reference maximum all change.

## Gate 1

`run_zhou_screens()` (code/zhou_screens.R) with rain from `P_ERA`, screen c's radiation test from `NETRAD_filled`, PET pressure fixed at 101.3 kPa, and `gpp_test = "daymean"` (default) reproduces the already-committed `tables/screen_attrition.csv` **exactly** for all 58 site-year rows across the 3 test sites -- confirming the 07_apply_screens.R refactor into zhou_screens.R changed nothing.

## Gate 2 (added 2026-10-09)

With `gpp_test = "daymean"` and PET using the daily `PA_F`, the four rain x radiation variants reproduce the already-committed `tables/screen_variants/attrition_by_variant.csv` **exactly** for all 204 rows -- confirming the new `gpp_test` argument changed nothing at its default.

## Eight variants (PET using the daily PA_F throughout)

Rain source x screen c radiation (as before, 2026-10-08):

- `rain_P_ERA_rad_NETRAD`: rain from `P_ERA > 0`, screen c from `NETRAD_filled >= 0`.
- `rain_P_ERA_rad_SW_IN`: rain from `P_ERA > 0`, screen c from `SW_IN_F >= 0`.
- `rain_P_F_rad_NETRAD`: rain from `P_F > 0` (as distributed: gauge where measured, `P_ERA`
  fill where not), screen c from `NETRAD_filled >= 0`.
- `rain_P_F_rad_SW_IN`: rain from `P_F > 0`, screen c from `SW_IN_F >= 0`.

Crossed with the GPP day test (added 2026-10-09):

- `daymean` (default, 07's current code): a day's mean GPP must be >= 10% of the LARGEST such
  daily mean among the site-year's candidate days.
- `halfhour` (Zhou et al. 2015's own wording): a day's mean GPP must instead be >= 10% of the
  maximum SINGLE-RECORD GPP in the site-year, over every record passing screens a-c.

Quality screen and daylight window are unchanged across all eight; PET always uses
`NETRAD_filled` for net radiation regardless of the screen c column.

## Valid days per kept year, median (range), by site, for all 8 variants

| site_id | rain_P_ERA_rad_NETRAD_daymean | rain_P_ERA_rad_NETRAD_halfhour | rain_P_ERA_rad_SW_IN_daymean | rain_P_ERA_rad_SW_IN_halfhour | rain_P_F_rad_NETRAD_daymean | rain_P_F_rad_NETRAD_halfhour | rain_P_F_rad_SW_IN_daymean | rain_P_F_rad_SW_IN_halfhour |
|---|---|---|---|---|---|---|---|---|
| US-Fuf | 14 (10-29) | 13 (10-29) | 84 (71-98) | 80 (59-92) | 24 (16-35) | 23 (16-35) | 114 (104-130) | 109 (88-123) |
| US-Ho2 | 15 (0-28) | 15 (0-28) | 34 (0-51) | 31 (0-49) | 32 (0-56) | 32 (0-56) | 60 (0-94) | 58 (0-91) |
| US-MMS | 8 (0-17) | 8 (0-16) | 25 (9-44) | 24 (9-44) | 19 (4-33) | 19 (4-33) | 52 (19-77) | 48 (18-77) |

## Valid days by month, P_F + SW_IN_F variant, by GPP test

`rain_P_F_rad_SW_IN`, kept years pooled, one row per site/month:

| site_id | month | gpp_daymean | gpp_halfhour |
|---|---|---|---|
| US-Fuf | 1 | 9 | 2 |
| US-Fuf | 2 | 22 | 5 |
| US-Fuf | 3 | 50 | 47 |
| US-Fuf | 4 | 95 | 95 |
| US-Fuf | 5 | 106 | 106 |
| US-Fuf | 6 | 81 | 80 |
| US-Fuf | 7 | 31 | 30 |
| US-Fuf | 8 | 49 | 47 |
| US-Fuf | 9 | 46 | 45 |
| US-Fuf | 10 | 36 | 32 |
| US-Fuf | 11 | 36 | 29 |
| US-Fuf | 12 | 12 | 9 |
| US-Ho2 | 1 | 0 | 0 |
| US-Ho2 | 2 | 0 | 0 |
| US-Ho2 | 3 | 17 | 7 |
| US-Ho2 | 4 | 119 | 104 |
| US-Ho2 | 5 | 190 | 190 |
| US-Ho2 | 6 | 159 | 159 |
| US-Ho2 | 7 | 214 | 214 |
| US-Ho2 | 8 | 195 | 195 |
| US-Ho2 | 9 | 158 | 158 |
| US-Ho2 | 10 | 83 | 82 |
| US-Ho2 | 11 | 16 | 9 |
| US-Ho2 | 12 | 0 | 0 |
| US-MMS | 1 | 0 | 0 |
| US-MMS | 2 | 0 | 0 |
| US-MMS | 3 | 0 | 0 |
| US-MMS | 4 | 60 | 43 |
| US-MMS | 5 | 205 | 202 |
| US-MMS | 6 | 235 | 235 |
| US-MMS | 7 | 174 | 174 |
| US-MMS | 8 | 199 | 199 |
| US-MMS | 9 | 227 | 227 |
| US-MMS | 10 | 169 | 141 |
| US-MMS | 11 | 7 | 1 |
| US-MMS | 12 | 0 | 0 |

## Supporting tables

`tables/screen_variants/attrition_by_variant.csv`, `valid_days_by_month.csv` (both extended
2026-10-09 with a `gpp_test` column), `gpp_thresholds.csv` (new 2026-10-09),
`records_in_window_by_month.csv`, `gauge_share.csv` (both unchanged from 2026-10-08 -- `gpp_test`
does not affect them) -- each with a `.meta.json` companion.

## What I could not do

Nothing -- both gates passed and all tables were produced for all 3 sites.
