# Methods: Future Köppen-Geiger Representativeness (Beck 2023)

This file covers all future-scenario representativeness analyses.
Each scenario is documented in its own ## section below.
All scenarios use: scripts/figure_representativeness_kg_future.R
(parameterized; run with [ssp] [period_dir] [period_label] [scenario_label]).

Shared design (applies to all scenarios):

  ASYMMETRIC FRAMING. Earth bar = projected global land area fractions for
  the specified future period/scenario. Network bar = PRESENT-DAY FLUXNET
  assignments from site_koppen_beck2023.csv (767 sites, 1991-2020 climate).
  The network occupies fixed locations; sites do not migrate as climate shifts.
  The question: does the current network, classified by its present biogeography,
  sample the distribution projected to dominate the globe in the target period?

  PER-SITE EXTRACTION. terra::extract() from the 1 km raster, with a
  nearest-land fallback (terra::as.points + terra::distance) for sites on
  ocean pixels. The same 3 coastal wetland sites need this fallback across
  all scenarios: US-KS3, US-TaS, CN-SnB (~0.6-1.0 km from land).

  GLOBAL DISTRIBUTION. terra::cellSize(mask=TRUE, unit='km') +
  terra::zonal(fun='sum'). Land mask is consistent; total land area is
  ~147,322,862 km2 across all scenarios.

  METRICS. Weighted Jaccard J and Hellinger distance H.
  p_k = global land fraction for class k (FUTURE raster).
  q_k = fraction of 767-site network in class k (PRESENT-DAY).
  J = sum(min(p,q)) / sum(max(p,q))  [0,1]; 1 = perfect representativeness.
  H = (1/sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))  [0,1]; 0 = identical.

---

## Scenario: SSP5-8.5 / 2071_2099

Raster: koppen_beck2023/2071_2099/ssp585/koppen_geiger_0p00833333.tif
(Beck et al. 2023, figshare v2, doi:10.6084/m9.figshare.21789074.v2)

SSP5-8.5 ("Fossil-fuelled Development") is the highest-emission marker
scenario, in which fossil fuel use and CO2 emissions continue rising throughout
the century. It represents an upper-bound stress test for climate-driven KG class
shifts, assuming approximately 4-5 degrees C of global warming by 2100. Together
with SSP2-4.5 mid-century, it brackets the plausible range of representativeness
challenges the network will face over the coming decades.

Total land area: 147,322,862 km²  (present-day: 147,322,862 km²).
Sites changing class vs present-day: 436 / 767 (56.8%).
Per-site future CSV: site_koppen_beck2023_ssp585_2071_2099.csv (reference only; NOT used as network bar).

Comparison with present-day metrics:
  5-class:    present J=0.401 H=0.329 | future J=0.382 H=0.337 | dJ=-0.018 dH=+0.008
  two-letter: present J=0.373 H=0.410 | future J=0.365 H=0.410 | dJ=-0.009 dH=-0.000
  30-class:   present J=0.350 H=0.440 | future J=0.346 H=0.450 | dJ=-0.003 dH=+0.009
