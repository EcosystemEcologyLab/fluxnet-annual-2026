Panel C of the new Figure 4 (`scripts/figure4_representativeness.R`) keeps the existing CGIAR Aridity
Index v3.1 (7-class UNEP scheme) for the global side and Geo vs Geo comparison unchanged — see
`methods_aridity_unep.md`. This note covers only the new Geo vs Data side: AI computed from each site's
own ERA5 meteorology rather than the CGIAR raster.

**AI = P / PET, 1991–2020.** P reuses the same 1991–2020 mean annual P_ERA already computed for panel A
(Köppen). PET is FAO-56 Penman–Monteith reference evapotranspiration (grass reference surface), computed
monthly from the 1991–2020 ERA5 climatological monthly means and summed to an annual total, using every
ERA5 variable available in this repository's DuckDB `monthly` table for `dataset='ERA5'`: `TA_ERA` (°C),
`SW_IN_ERA`/`LW_IN_ERA` (W/m², incoming short/longwave), `VPD_ERA` (hPa), `PA_ERA` (kPa), `WS_ERA` (m/s).
Units were confirmed empirically against a known site (US-Ha1, Harvard Forest) before use, not assumed
from variable names alone. The implementation was validated against three known climate types before the
full network run: AU-ASM (Alice Springs desert) → AI=0.159 (Arid, correct); US-Ha1 (humid temperate
forest) → AI=1.85 (Humid, correct); US-SRM (Santa Rita semi-arid savanna) → AI=0.228 (Semi-Arid, correct).

**Approximations (accepted as implemented):**
1. **Wind.** `WS_ERA` is assumed to be ERA5's native 10 m wind (not explicitly documented in this
   repository), converted to the FAO-56 reference height of 2 m via the standard log-wind-profile formula.
2. **Net radiation.** This ERA5 bundle has both incoming shortwave and incoming longwave directly, so net
   radiation is computed from them rather than FAO-56's own simplified clear-sky parametrization (built
   for when only Rs is measured): Rns = (1−0.23)×Rs (FAO-56's grass reference albedo); outgoing longwave
   is estimated via Stefan–Boltzmann applied to `TA_ERA` as a proxy for surface skin temperature (not
   available in this bundle), assumed emissivity 0.96; Rnl = LW_in − LW_out.
3. **es/Δ** are computed from monthly mean `TA_ERA`, not averaged from daily Tmax/Tmin as FAO-56
   recommends — true daily/monthly Tmax/Tmin are not in this ERA5 bundle (only `TA_ERA_DAY`/
   `TA_ERA_NIGHT`, an approximate day/night split, deliberately not used to avoid compounding
   approximations). A recognised FAO-56 simplification; slightly underestimates ET0.
4. **Soil heat flux G = 0** — FAO-56's standard simplification at monthly-to-annual timescales, where G
   approximately cancels over a full annual cycle.
5. **ET0 floored at 0 per month** — at high latitude in winter, Rn can be strongly negative, driving the
   raw formula negative for that month; unclipped, this could make the annual PET total itself negative
   or near-zero at cold sites. Standard FAO-56 practice.

**Exclusions** — see `methods_precip_exclusions.md` for the two precipitation-dependent rules shared with
panel A. Panel C additionally excludes 4 sites (`CD-Ygb`, `DE-Zrk`, `FR-LBr`, `US-Sne`) whose raw ERA5
inputs are physically impossible in at least one month (e.g. `LW_IN_ERA` up to ~32,000 W/m², `VPD_ERA` up
to ~1,660 hPa) — an ERA5 data-quality issue in the bundled extraction, screened by a physical-plausibility
check (LW/SW <0 or >1000 W/m²; VPD <0 or >100 hPa; WS ≤0 or >50 m/s; PA outside [50,110] kPa; TA outside
[−90,60]°C), distinct from and additional to the two precipitation-dependent rules. Two further sites
(`DE-SbM`, PET=0 mm/yr; `KE-Aq2`, PET=122 mm/yr) have implausible PET from valid-looking raw inputs — a
known limitation of approximation 2 at sites where true incoming longwave is naturally low for reasons
other than cold temperature (e.g. altitude) — flagged but not screened; both happen to already be
excluded by the other rules regardless.

**Period mismatch.** CGIAR's own Aridity Index v3.1 baseline is 1970–2000; this Geo vs Data calculation
uses 1991–2020 ERA5 to match the other ERA5-derived panels. This mismatch applies only to panel C (its
Geo vs Geo side, unchanged from the existing CGIAR-raster method, has no such mismatch) and is stated in
the figure's legend.

Output: `data/snapshots/site_aridity_era5_fig4.csv`.
