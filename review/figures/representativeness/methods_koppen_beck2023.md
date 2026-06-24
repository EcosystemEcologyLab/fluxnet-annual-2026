Per-site Köppen-Geiger classes were assigned from the Beck et al. (2023) present-day
(1991–2020) map at 1 km resolution, which provides 30 classes under the standard
Köppen-Geiger scheme (Af, Am, Aw, BWh, BWk, BSh, BSk, Csa, Csb, Csc, Cwa, Cwb,
Cwc, Cfa, Cfb, Cfc, Dsa, Dsb, Dsc, Dsd, Dwa, Dwb, Dwc, Dwd, Dfa, Dfb, Dfc, Dfd,
ET, EF). The map was downloaded from Figshare (doi:10.6084/m9.figshare.21789074.v2,
version 2, published 2026-01-14; v2 corrects a calculation error in the original
release). Full citation: Beck, H.E., McVicar, T.R., Vergopolan, N. et al.
High-resolution (1 km) Köppen-Geiger maps for 1901–2099 based on constrained CMIP6
projections. Scientific Data 10, 724 (2023). doi:10.1038/s41597-023-02549-6.

Per-site extraction was performed using terra::extract() at the exact reported
latitude and longitude of each FLUXNET site. Three sites — US-KS3, US-TaS, and
CN-SnB — are wetland (WET IGBP class) sites whose reported coordinates fall on the
raster's water mask (ocean or large water bodies). For these, the KG class was
recovered by identifying the nearest non-NA land pixel within a 3° search window
(terra::as.points() on a local crop + terra::distance()), yielding assignments at
0.6–1.0 km distance. The koppen_method column in site_koppen_beck2023.csv records
whether the assignment used exact extraction or the nearest-land fallback.

The area-weighted global KG distribution was computed geodesically.
terra::cellSize(mask=TRUE, unit="km") computed the area of each land pixel in km²,
correctly accounting for the convergence of meridians at high latitudes (pixels near
the poles are substantially smaller than equatorial pixels at the same longitude
spacing). Ocean pixels are NA in the Beck 2023 raster and were automatically excluded.
terra::zonal(fun="sum", na.rm=TRUE) then summed land area by KG class integer code
without materialising the full 21600 × 43200 cell raster in memory. The 30 class
areas were normalised to fractions of total land (147.3 million km², consistent with
the known global land surface area of ~148.9 million km²; the small discrepancy
reflects raster representation of islands and coastlines at 1 km resolution).

Two aggregation levels are shown alongside the native 30-class distribution.
The 5-class aggregation uses the standard Köppen first-letter groups: A (Tropical),
B (Arid), C (Temperate), D (Cold), E (Polar). The 13-class two-letter aggregation
collapses temperature subclasses but retains precipitation regime distinctions (e.g.,
Cfa/Cfb/Cfc → Cf, Dsa/Dsb/Dsc/Dsd → Ds). The two-letter classes are derived as
substr(koppen_class, 1, 2). Colors for the two-letter figure are the unweighted mean
RGB of all 30-class members within each group, preserving the Beck 2023 palette
character at each aggregation scale.

The sampling ratio for each class is defined as the network fraction divided by the
global land fraction: values greater than 1 indicate that the class is over-represented
in the FLUXNET network relative to its global land area, and values less than 1
indicate under-representation. The ratio is displayed on a log₂ axis so that
equivalent over- and under-sampling factors appear symmetric about the parity line.

Two scalar distance metrics summarise overall representativeness across all classes
simultaneously. The weighted Jaccard distance (Ruzicka similarity):

    J = Σ min(p_k, q_k) / Σ max(p_k, q_k)

where p_k is the global land fraction and q_k is the network fraction of class k.
J is bounded [0, 1]; J = 1 means the two distributions are identical and J = 0 means
they share no overlap. This metric is directly interpretable as the fraction of
distributional "overlap" between the global and network compositions, and is symmetric.

The Hellinger distance:

    H = (1/√2) × √( Σ (√p_k − √q_k)² )

is also bounded [0, 1]; H = 0 means identical distributions and H = 1 means maximal
divergence. Hellinger distance is more sensitive than Jaccard to specific class
mismatches, particularly where one distribution has a large fraction in a class that
the other has near zero, because the square-root transform down-weights both very large
and very small fractions relative to a linear treatment.

The polar EF (ice cap) class covers approximately 9.6% of global land by area but has
zero FLUXNET sites. This reflects a physical constraint: eddy covariance towers cannot
be instrumented on ice sheets or permanent ice caps. The structural zero in the EF
network fraction is therefore not a community sampling choice but an instrument
deployment limitation. EF is flagged as structurally unsampled in the sampling-ratio
panel and is excluded from interpretation of the other under-sampled classes.
