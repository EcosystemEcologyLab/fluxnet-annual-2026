Per-site aridity values were extracted from the CGIAR Global Aridity Index
and Potential Evapotranspiration Dataset, Version 3.1 (Zomer et al. 2022).
The dataset provides annual mean aridity index (AI) values at 30 arc-second
(~1 km) resolution, derived from long-term (1970–2000) averages of precipitation
and reference evapotranspiration computed by the Penman-Monteith equation. Full
citation: Zomer, R.J., Trabucco, A., Bossio, D.A., van Straaten, O., Verchot,
L.V. (2022). Version 3 of the Global Aridity Index and Potential
Evapotranspiration Database. Scientific Data 9, 409.
doi:10.1038/s41597-022-01493-1. Dataset archived at Figshare:
doi:10.6084/m9.figshare.7504448.

The aridity index is defined as the ratio of annual precipitation to annual
potential evapotranspiration (AI = P/PET), and is dimensionless. Values less
than one indicate more evaporative demand than precipitation supply (i.e., a
water deficit), while values greater than one indicate surplus. The raster
stores values as 16-bit unsigned integers scaled by 10,000, so that raw integer
values must be multiplied by 0.0001 to obtain the true AI.

Sites were classified into the five standard UNEP aridity categories as defined
in the UNEP World Atlas of Desertification (UNEP 1992): Hyper-Arid (AI < 0.05),
Arid (0.05 <= AI < 0.20), Semi-Arid (0.20 <= AI < 0.50),
Dry Sub-Humid (0.50 <= AI < 0.65), and Humid (AI >= 0.65). These five classes
partition a continuous gradient from the most xeric environments to perhumid
conditions.

Per-site AI values were extracted using terra::extract() at the exact reported
latitude and longitude of each FLUXNET site. The CGIAR raster assigns a value
of zero to ocean and water-body pixels (rather than a formal NA flag), so pixels
with raw value zero were treated as missing and a nearest-land recovery procedure
was applied: a progressive circular buffer (0.01 to 0.5 degrees) was searched for
non-zero pixel values, and if unsuccessful a nearest-land pixel search within a
3-degree window was used (terra::as.points(na.rm=TRUE) and terra::distance()
to locate the closest non-zero cell). The aridity_method column in
site_aridity.csv records whether each site was assigned by exact extraction or
by the fallback procedure.

The area-weighted global UNEP class distribution was computed using the same
geodesic approach applied to the Köppen-Geiger axis. The raster was first
classified into five class codes (zero set to NA to exclude ocean) using
terra::classify(), then per-pixel land area in km² was computed with
terra::cellSize(mask=TRUE, unit="km"), accounting for convergence of meridians
at high latitudes. terra::zonal(fun="sum") accumulated land area per class.
Total land area from this raster: 134,761,545 km² (the aridity raster covers 60°S to
90°N; the small difference from the KG total of 147,322,862 km² reflects
differences in land mask and coverage).

The sampling ratio for each class is the network fraction divided by the global
land fraction: values above 1 indicate over-representation of that climate type
in the FLUXNET network relative to its global land area; values below 1 indicate
under-representation.

Two scalar metrics summarise overall representativeness. The weighted Jaccard
similarity (also known as the Ruzicka index):

    J = sum(min(p_k, q_k)) / sum(max(p_k, q_k))

where p_k is the global land fraction and q_k is the network fraction of class k,
is bounded [0, 1]; J = 1 means the distributions are identical and J = 0 means
no overlap. For the UNEP aridity axis at five-class aggregation, J = 0.69 and
the Hellinger distance H = 0.21. Hellinger distance:

    H = (1/sqrt(2)) * sqrt(sum((sqrt(p_k) - sqrt(q_k))^2))

is also bounded [0, 1]; H = 0 means identical distributions and H = 1 means
maximal divergence. Hellinger is more sensitive than Jaccard to specific class
mismatches where one distribution has a large fraction in a class that the other
has near zero.

The Humid class (AI >= 0.65) constitutes 50.2% of global land area by this
analysis but is represented by a moderate share of the network (temperate and
boreal forests, which support high tower density). The Hyper-Arid class
(AI < 0.05) covers 9.8% of global land area and is structurally under-sampled
for the same physical reasons as polar EF in the Köppen-Geiger analysis: eddy
covariance towers in hyperarid environments require substantial logistical
infrastructure and are rarely established in the most remote desert interiors.

Color palette for the figure: Hyper-Arid #d73027, Arid #fc8d59, Semi-Arid
#ffff33, Dry Sub-Humid #66bd63. The original color specification called for
white (no fill) for the Humid class. In a stacked bar where Humid is the
majority global class, a white fill is structurally invisible against the white
panel background. The Humid class is therefore displayed in pale gray (#cccccc)
with a thin grey border (grey70) applied uniformly to all segments.

