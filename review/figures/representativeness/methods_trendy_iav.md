# Methods: TRENDY v14 Representativeness Axes (NEE-IAV, ET-IAV, NEE-median, ET-median)

## Overview

Four representativeness axes are derived from the TRENDY v14-gcb2025 multi-model
ensemble: two interannual variability (IAV) axes quantified as the linear-detrended
standard deviation of annual carbon and water fluxes, and two magnitude axes
quantified as the temporal mean absolute value (NEE) or mean (ET) across the
analysis window. Together these axes characterise whether the FLUXNET tower network
samples the global range of ecosystem carbon and water flux variability and magnitude
as simulated by current land surface models.

## Data source

TRENDY v14-gcb2025, S3 simulation (transient, with historical land-use change).
Variables: nbp (net biome production, kg C m⁻² s⁻¹, monthly or annual depending
on model) and evapotrans (evapotranspiration, kg m⁻² s⁻¹, monthly).

Protocol reference: Sitch S et al. (2024). The global carbon budget 2024.
Earth System Science Data. doi:10.1029/2024GB008102.
Global Carbon Budget paper: Friedlingstein P et al. (2025). Global Carbon Budget 2025
(in prep.). The acknowledgment-only attribution policy applies to TRENDY model outputs;
individual model PIs are acknowledged in the Acknowledgements section of the paper,
not cited formally.

## Model ensemble

The full TRENDY v14 archive includes 20 models. Three were excluded:

| Model | Reason for exclusion | Type |
|---|---|---|
| CARDAMOM | Temporal coverage only 22 years (insufficient for 34-year IAV analysis) | technical |
| CLM-FATES | Irregular longitude spacing — terra::rast() fails to parse | technical |
| JSBACH | Irregular latitude spacing — terra::rast() fails to parse | technical |

All three exclusions are technical, not selective. The remaining 17-model ensemble
is: CABLE-POP, CLASSIC, CLM, DLEM, ED, ELM, ELM-FATES, IBIS, ISAM, JULES-ES,
LPJ-GUESS, LPJml, LPJwsl, LPX-Bern, ORCHIDEE, TEM, VISIT-UT.

## Analysis window

1990–2023 (34 years). This window is bounded by the intersection of model
availability: CLASSIC, DLEM, and ELM end their S3 simulation at 2023.

ELM note: ELM submitted data through 2022 only; its 2023 annual layer is NA
across all pixels for both nbp and evapotrans. All per-pixel statistics functions
(detrended SD, mean absolute value, mean) require all 34 years to be non-NA before
computing (complete-row requirement). Consequently, ELM contributes an all-NA
per-pixel stat raster to the ensemble stack for all four axes. The ensemble median
is computed with na.rm = TRUE, so the 16 remaining models determine each pixel's
ensemble value. ELM is therefore excluded from all four ensemble maps, not only
the IAV maps. The effective ensemble size for any pixel is 16 models.

This differs from the original expectation (ELM excluded from IAV only, included
in median maps via na.rm). On inspection, the complete-row requirement in
compute_mean_abs() and compute_mean() applies identically to compute_detrended_sd().

## Per-model regridding

Each model's NetCDF was loaded with terra::rast(). Three models (CLM, ISAM,
ELM-FATES) store longitude 0–360°; these were rotated to −180–180° via
terra::rotate() before any subsetting. DLEM, LPJ-GUESS, and LPJml store
nbp as annual time steps; all other models use monthly time steps. For monthly
models, annual sums were computed from complete 12-month blocks; years with
fewer than 12 months were summed over the available months with a console warning.

Unit conversions applied before annual summation:

| Variable | Native unit | Analysis unit | Conversion |
|---|---|---|---|
| nbp (monthly) | kg C m⁻² s⁻¹ | gC m⁻² yr⁻¹ | × 2629800 s mo⁻¹ × 1000 |
| nbp (annual) | kg C m⁻² s⁻¹ | gC m⁻² yr⁻¹ | × 31557600 s yr⁻¹ × 1000 |
| evapotrans | kg m⁻² s⁻¹ | mm yr⁻¹ | × 2629800 s mo⁻¹ (then summed) |

The unit for evapotrans follows from 1 kg m⁻² = 1 mm (water density = 1000 kg m⁻³).

All models were resampled to a common 0.5° global grid (720 × 360 cells,
EPSG:4326) via terra::resample(method = 'bilinear', threads = TRUE). The land
mask (Beck 2023 KG 0.5° raster) was then applied. Regridded intermediates were
written as compressed GeoTIFFs (34 annual layers per model per variable) to
data/external/trendy/derived/intermediate/ (252 MB total; 17 models × 2
variables × 34 layers).

## Per-pixel statistics

### IAV axes: linear-detrended standard deviation

For each pixel with all 34 years non-NA, the annual time series was linearly
detrended using OLS projection (hat matrix H = X(XᵀX)⁻¹Xᵀ; residual
projection P = I − H). The detrended SD was then computed as:

  SD_detrended = sqrt( sum(residuals²) / (n − 2) )

where n = 34 and the df reduction of 2 accounts for the fitted intercept and
slope. Pixels missing any year were assigned NA.

### NEE-median axis: mean of absolute annual NBP

For each pixel, the mean of the absolute value of annual NBP was computed
across all 34 years (complete rows only). NBP can be negative (net source).
Taking the absolute value before averaging gives a signed-symmetric measure of
carbon flux magnitude regardless of sign, parallel to the IAV axis framing.

### ET-median axis: mean annual ET

For each pixel, the mean of annual evapotranspiration was computed across all
34 years (complete rows only). ET is non-negative; no absolute value is needed.

### Ensemble median

After computing the per-pixel statistic for each model, a 17-layer raster stack
was built and the ensemble median was computed per pixel using terra::app()
with na.rm = TRUE. The four ensemble-median maps were saved as single-layer
GeoTIFFs (~260–350 KB each; compressed) to data/external/trendy/derived/.
Each map has 94,589 non-NA pixels (the global KG land mask at 0.5°).

## Binning

All four axes use the same hybrid 7-bin scheme applied to other continuous
representativeness axes (biomass, aridity):

  Bin 1: values 0–5 (fixed near-zero cut) — separates very low-flux or
    non-terrestrial pixels from the main distribution. Negative pixel values
    (net-source pixels in the IAV/median maps) are mapped to 0 before binning.

  Bins 2–7: six equal-area quantile bins computed from the area-weighted
    global distribution of KG-land pixels above the near-zero cut.
    Each bin contains approximately 1/6 of the total area of the
    above-threshold global land.

### Breakpoints by axis

NEE-IAV (gC m⁻² yr⁻¹, near-zero cut 5): 21.3, 31.9, 42.1, 54.4, 70.9
ET-IAV  (mm yr⁻¹,       near-zero cut 5): 17.5, 24.2, 32, 41.8, 58.1
NEE-med (gC m⁻² yr⁻¹, near-zero cut 5): 20.5, 31.7, 41.8, 54.3, 70.2
ET-med  (mm yr⁻¹,       near-zero cut 5): 143.4, 263.9, 394, 634.7, 968.6

Breakpoints are data-dependent and were computed at runtime from the ensemble
maps produced by figure_representativeness_trendy_compute.R. The near-zero
threshold (5.0 for both gC m⁻² yr⁻¹ and mm yr⁻¹) was confirmed to capture
a non-trivial land fraction (≥24% of global land for NEE axes, ~3% for ET axes)
before being used; the code automatically adjusts if the threshold captures
< 1% or > 70% of global land.

## Per-site extraction

Per-site values were extracted from each ensemble-median map at the site's
reported latitude/longitude using terra::extract(method = 'simple') (nearest
pixel). Sites returning NA (ocean pixels, ice, or coverage gaps) were recovered
via a nearest-land search within a 3° window: the closest KG-land cell within
the window was used, and the recovery distance was recorded in the site CSV.

terra API note: terra::extract(raster, matrix) in terra ≥ 1.9.27 returns a
one-column data frame (values only, no ID column prepended). The extraction
code was updated to index raw[[1]] rather than raw[[2]] after this change
caused a 'subscript out of bounds' error during the initial Step 3 run.
(Commit 7ee9210, 2026-06-27.)

## Representativeness metrics

Weighted Jaccard index (J) and Hellinger distance (H) as described in
methods_koppen_beck2023.md. p_k = global land fraction of bin k;
q_k = fraction of 767-site FLUXNET network assigned to bin k.

| Axis | J | H |
|---|---|---|
| NEE-IAV    | 0.507 | 0.316 |
| ET-IAV     | 0.663 | 0.224 |
| NEE-median | 0.495 | 0.320 |
| ET-median  | 0.459 | 0.345 |

Across all four axes, the pattern is consistent with the biomass and
Koppen-Geiger axes: the network over-samples intermediate flux regimes
(temperate mesic forests and grasslands) and under-samples both the
near-zero bin (arid, boreal, or low-productivity land) and the highest
flux bins (tropical forests, wet tropics). ET-IAV yields the highest J
(0.663), indicating reasonably broad sampling of ET variability; ET-median
yields the lowest J (0.459), indicating the strongest magnitude bias.

## Cross-axis comparison: NEE-median vs. above-ground biomass

A pixel-level comparison between the ensemble-median NEE magnitude map and
the ESA CCI Biomass v7.0 map (band 18, 2024, resampled to 0.5° bilinear)
yields Pearson r = 0.55 and Spearman ρ = 0.89 across 94,589 shared non-NA
pixels. The strong rank correlation confirms the expectation that high-biomass
pixels tend to have high carbon flux magnitude in the TRENDY ensemble.
The weaker Pearson correlation (r = 0.55) reflects a nonlinear relationship:
high-biomass tropical forests show high NEE magnitude, but low-biomass
semi-arid and boreal systems occupy a broad range of NEE magnitudes driven
by water and temperature limitation. The implications for network
representativeness are consistent across both axes: the network
under-samples the highest flux/biomass bins (dense tropical forests) and
over-samples the intermediate bins.

Breakpoints for the NEE-median axis differ slightly from those of NEE-IAV
because the underlying distributions differ (mean |flux| vs detrended SD),
but both axes partition global land in the same structural way.

