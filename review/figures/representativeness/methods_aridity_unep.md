Per-site aridity values were extracted from the CGIAR Global Aridity Index
and Potential Evapotranspiration Dataset, Version 3.1 (Zomer et al. 2022;
doi:10.1038/s41597-022-01493-1; Figshare doi:10.6084/m9.figshare.7504448).
The dataset provides annual mean aridity index (AI) values at 30 arc-second
(~1 km) resolution, derived from long-term (1970-2000) climate averages. The
AI is defined as P/PET (annual precipitation divided by annual potential
evapotranspiration computed by the Penman-Monteith equation) and is
dimensionless. Values below one indicate more evaporative demand than
precipitation supply; values above one indicate a water surplus. The raster
stores 16-bit unsigned integers scaled by 10,000 (multiply by 0.0001 to obtain
the true AI). Ocean and water-body pixels carry raw value 0 (not a formal NA
flag); these were treated as missing data in all computations.

Sites were classified into two parallel aridity schemes. The five-class
canonical UNEP scheme follows the World Atlas of Desertification (UNEP 1992):
Hyper-Arid (AI < 0.05), Arid (0.05 to 0.20), Semi-Arid (0.20 to 0.50), Dry
Sub-Humid (0.50 to 0.65), and Humid (AI >= 0.65). The seven-class extended
scheme retains these four lower thresholds and subdivides the Humid class
following FAO usage: Humid (low) (0.65 to 1.0), Humid (moderate) (1.0 to 2.0),
and Hyper-Humid (AI >= 2.0). The 1.0 threshold approximately marks the
transition from energy-limited to water-excess conditions; the 2.0 threshold
separates the moderately humid zone from rainforest-scale moisture regimes. Both
schemes are shown to support a decision on which granularity goes into the final
paper figure.

Per-site AI values were extracted using terra::extract() at the exact reported
coordinates of each FLUXNET site. Three sites (US-KS3, US-TaS, CN-SnB) are
wetland sites whose coordinates fall on the raster ocean mask. For these, a
nearest-land pixel was identified within a 3-degree search window using
terra::as.points() and terra::distance(); all three were recovered at 0.6-1.0 km
and assigned to the Humid class (AI 0.74-0.97). The aridity_method column in
site_aridity.csv records the extraction method per site.

The area-weighted global distribution was computed geodesically. The raster was
classified to integer class codes using terra::classify() (value 0 mapped to NA
to exclude ocean). terra::cellSize(mask=TRUE, unit='km') computed per-pixel land
area in km2, correctly accounting for meridional convergence at high latitudes.
terra::zonal(fun='sum') accumulated land area per class without materialising the
full raster. Total land area: 134,761,545 km2 (aridity raster covers 60 degrees S
to 90 degrees N; excludes Antarctica, explaining the ~12.6 M km2 difference from
the KG raster total of 147.3 M km2). For the 7-class scheme, the same cell-area
raster was reused and only the zonal summation was re-run.

The sampling ratio for each class is the network fraction divided by the global
land fraction (values above 1 = over-sampled, below 1 = under-sampled). Two
scalar metrics summarise overall representativeness. The weighted Jaccard
(Ruzicka) similarity J = sum(min(p,q)) / sum(max(p,q)) and Hellinger distance
H = (1/sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2)) are both bounded [0,1].
For the 5-class scheme: J = 0.69, H = 0.21. For the 7-class scheme:
J = 0.67, H = 0.22. Jaccard is interpretable as an overlap fraction;
Hellinger is more sensitive to class-specific mismatches.

Color palette note: the 5-class figure uses pale gray (#cccccc) for the Humid
class with a uniform thin grey border on all segments, substituting the original
'white/no-fill' specification. White-on-white is structurally invisible
when the Humid class represents 50.2% of global land area. The 7-class
figure uses a blue gradient for the humid trio (Humid low: #74add1, moderate:
#4575b4, Hyper-Humid: #313695), completing a red-to-blue colorbar that maps the
full aridity continuum from extreme drought to extreme moisture surplus.
