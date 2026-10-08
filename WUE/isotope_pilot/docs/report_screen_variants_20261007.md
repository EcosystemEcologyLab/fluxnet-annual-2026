# Stage 2 screen-variant check (2026-10-07)

Side analysis, read-and-report, on the three stage 2 test sites (US-Fuf, US-Ho2, US-MMS).
Stage 2 itself stays on hold -- 07_apply_screens.R was not re-run and no stage 2 table was
overwritten.

**Question:** how many valid days per kept site-year survive the Zhou et al. (2015) screens
when (i) the rain source and (ii) the radiation condition in screen c change.

## Gate

`run_zhou_screens()` (code/zhou_screens.R) with rain from `P_ERA`, screen c's radiation test from `NETRAD_filled`, and PET pressure fixed at 101.3 kPa reproduces the already-committed `tables/screen_attrition.csv` **exactly** for all 58 site-year rows across the 3 test sites -- confirming the 07_apply_screens.R refactor into zhou_screens.R changed nothing.

## Four variants (PET using the daily PA_F)

- `rain_P_ERA_rad_NETRAD`: rain from `P_ERA > 0`, screen c from `NETRAD_filled >= 0` (closest
  to 07_apply_screens.R's own current code, but with PA_F-based PET rather than the gate's
  fixed 101.3 kPa).
- `rain_P_ERA_rad_SW_IN`: rain from `P_ERA > 0`, screen c from `SW_IN_F >= 0`.
- `rain_P_F_rad_NETRAD`: rain from `P_F > 0` (as distributed: gauge where measured, `P_ERA`
  fill where not), screen c from `NETRAD_filled >= 0`.
- `rain_P_F_rad_SW_IN`: rain from `P_F > 0`, screen c from `SW_IN_F >= 0`.

Everything else (quality screen, daylight window, the 10% GPP day test) is unchanged across
variants; PET always uses `NETRAD_filled` for net radiation regardless of the screen c column.

## Valid days per kept year, median (range), by site and variant

| site_id | rain_P_ERA_rad_NETRAD | rain_P_ERA_rad_SW_IN | rain_P_F_rad_NETRAD | rain_P_F_rad_SW_IN |
|---|---|---|---|---|
| US-Fuf | 14 (10-29) | 84 (71-98) | 24 (16-35) | 114 (104-130) |
| US-Ho2 | 15 (0-28) | 34 (0-51) | 32 (0-56) | 60 (0-94) |
| US-MMS | 8 (0-17) | 25 (9-44) | 19 (4-33) | 52 (19-77) |

## Supporting tables

`tables/screen_variants/attrition_by_variant.csv`, `valid_days_by_month.csv`,
`records_in_window_by_month.csv`, `gauge_share.csv` (each with a `.meta.json` companion).

## What I could not do

Nothing -- the gate passed and all four tables were produced for all 3 sites.
