Future-climate representativeness was assessed using the Beck et al. (2023)
SSP2-4.5 projected Köppen-Geiger map for 2041-2070. SSP2-4.5 ("Middle of the
Road") represents a moderate forcing scenario in which emissions peak around
mid-century before declining; it is one of the more widely used scenarios for
near-term climate assessment. The source raster is at 1 km resolution and uses
the same 30-class KG scheme and legend as the present-day 1991-2020 map. Full
citation and data source are identical to the present-day analysis
(see methods_koppen_beck2023.md); the SSP2-4.5 2041-2070 raster is distributed
within the same figshare archive (doi:10.6084/m9.figshare.21789074.v2).

The core design choice for this axis is asymmetric: the Earth bar in each
figure shows the PROJECTED global land area distribution under 2041-2070
SSP2-4.5, while the FLUXNET network bar shows PRESENT-DAY KG assignments
from site_koppen_beck2023.csv. This asymmetry is intentional. The FLUXNET
network occupies fixed physical locations; sites do not migrate as their local
climate shifts. The question this axis asks is therefore: does the current
network, classified by its present-day biogeography, sample the climate
distribution that is projected to cover the globe in 2041-2070? A network
that already samples a future-like distribution would require less supplementation
under climate change; one that does not would need new sites in projected
growth zones (primarily arid B and expanding semi-arid classes) to remain
representative.

As a secondary output, per-site future KG assignments were also extracted
(site_koppen_beck2023_ssp245_2041_2070.csv). These show which sites are
projected to cross a class boundary under SSP2-4.5 by 2041-2070. This CSV is
for exploratory use; it is NOT the basis for the network bar in any figure.

Global area computation and per-site extraction methods are identical to the
present-day analysis described in methods_koppen_beck2023.md. The same
terra::cellSize(mask=TRUE, unit='km') + terra::zonal(fun='sum') approach was
used. Total land area under the future projection: 147,322,862 km²
(present-day: 147,322,862 km²; difference reflects raster land mask).

Representativeness metrics (weighted Jaccard J and Hellinger distance H) are
computed as described in methods_koppen_beck2023.md, with p = future global
land fraction per class and q = present-day network fraction per class.

Comparison with present-day metrics:
  5-class:    present J=0.401 H=0.329 | future J=0.397 H=0.331 | dJ=-0.004 dH=+0.002
  two-letter: present J=0.373 H=0.410 | future J=0.381 H=0.406 | dJ=+0.007 dH=-0.005
  30-class:   present J=0.350 H=0.440 | future J=0.368 H=0.433 | dJ=+0.018 dH=-0.008
