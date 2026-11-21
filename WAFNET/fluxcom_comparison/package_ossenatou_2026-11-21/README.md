# FLUXCOM-X-BASE 0.05 degree data package for Ossenatou Mamadou (2026-11-21)

This package was prepared for the WAF-Net paper by David Moore's group, extracting the 
FLUXCOM-X-BASE product at its native 0.05 degree monthly grid resolution for the six WAF-Net 
sites, alongside the matching tower (eddy-covariance) monthly data. It is a data package for 
your own analysis and plotting -- it does not contain new figures or interpretation beyond the 
simple statistics in `summary.csv` and one labelled DRAFT check figure.

**FLUXCOM-X-BASE is not FLUXNET/Shuttle data.** It is an independent, machine-learning-based 
gridded product used here only as a comparison against your tower observations.

## Files

- **`xbase_005_monthly.csv`** -- FLUXCOM-X-BASE values at the 0.05 degree grid cell containing 
  each tower, every month 2001-2021 (not filtered by tower QC -- this is the product as 
  distributed):
  - `site_id` -- FLUXNET site code.
  - `year`, `month`.
  - `GPP_gC_m2_d1` -- gross primary productivity, grams carbon per m2 per day (a mean daily 
    rate for the month, not a monthly total).
  - `NEE_gC_m2_d1` -- net ecosystem exchange, same units. **Negative = net uptake by the 
    ecosystem (carbon sink), positive = net release.** This is the same sign convention as the 
    tower's own NEE.
  - `ET_mm_hr`, `ET_T_mm_hr` -- evapotranspiration and its transpiration component, mean mm of 
    water per hour, as distributed by X-BASE.
  - `LE_W_m2` -- latent heat flux, derived from `ET_mm_hr` using the latent heat of 
    vaporization (2.45e6 J/kg): `LE = ET * 2.45e6 / 3600`. This is the unit used for comparison 
    against the tower's own LE.
  - `cell_lon`, `cell_lat` -- the centre of the 0.05 degree grid cell used (see `sites.csv` for 
    the distance from the tower).

- **`tower_monthly.csv`** -- the matching monthly tower (eddy-covariance) data for the same six 
  sites and months, from the FLUXNET Shuttle, same QC rule and VUT/CUT choice as our Stage 2 
  0.5 degree comparison:
  - `carbon_src` -- `VUT` or `CUT`, which partitioning the site's carbon fluxes use (all six 
    sites here are `VUT`).
  - `NEE_VUT_REF`, `GPP_DT_VUT_REF`/`RECO_DT_VUT_REF` (daytime partition), 
    `GPP_NT_VUT_REF`/`RECO_NT_VUT_REF` (nighttime partition), `LE_F_MDS`, `H_F_MDS` -- all in 
    their native FLUXNET units (NEE/GPP/RECO: gC m-2 d-1; LE/H: W m-2).
  - Each flux has a matching `..._QC` column: **the fraction (0-1) of the underlying 
    half-hourly/hourly records that were measured or good-quality gap-filled** -- not the 0/1/2/3 
    integer flag used at half-hourly resolution. `GPP`/`RECO` (both partitions) have no QC 
    column of their own at monthly resolution and are gated on the site's own NEE QC instead 
    (`GPP_RECO_DT_QC`/`GPP_RECO_NT_QC` -- both equal `NEE_VUT_REF_QC`).
  - Each flux also has a `..._pass_qc` TRUE/FALSE column: whether that QC value meets the 
    threshold used throughout this analysis, QC >= 0.50 -- the same rule used for the Stage 2 
    0.5 degree comparison and the FLUXNET2015 published convention.
  - We used **nighttime-partitioned** GPP/RECO (`GPP_NT`/`RECO_NT`) in `summary.csv`, because 
    X-BASE's own documentation states its GPP training targets are nighttime-partitioned; the 
    daytime columns (`GPP_DT`/`RECO_DT`) are included here too in case you want them.

- **`sites.csv`** -- one row per site: name, country, IGBP vegetation class, tower coordinates, 
  the 0.05 degree grid cell centre used and its distance from the tower (`cell005_dist_km`), 
  the number of tower months (2001-2021) that pass the QC rule above, whether the site is in 
  FLUXCOM-X-BASE's own 294-site training set, and the tower dataset's own citation and 
  DOI/handle (read from the FLUXNET Shuttle snapshot of 20 September 2026) -- **please cite 
  each tower site's own dataset**, in addition to X-BASE, in any publication using this data.

- **`summary.csv`** -- one row per site and flux (NEE, GPP, RECO [derived], LE): number of 
  QC-passing overlapping months, bias (X-BASE minus tower), RMSE, and R-squared, computed at 
  the 0.05 degree resolution in this package (`_005` columns) and, alongside, the same numbers 
  from our earlier 0.5 degree comparison (`_050` columns) for reference. `derived = TRUE` for 
  RECO; see below.

- **`check_figure_xbase_005_vs_tower_DRAFT.png`** -- one check figure, monthly X-BASE (0.05 
  degree) against tower, one panel each for NEE/GPP/LE, sites by colour, training sites marked, 
  1:1 reference line. Labelled DRAFT -- a sanity check, not a paper figure.

## Important things to know before analysing this data

- **Sign convention:** NEE is negative for net ecosystem carbon uptake, positive for release, 
  in both the tower and X-BASE series. **Bias is always X-BASE minus tower** throughout.
- **X-BASE has no ecosystem respiration variable of its own.** If you need RECO from X-BASE, 
  derive it as `GPP + NEE` (both from X-BASE, same sign convention) -- this is what `summary.csv` 
  does, and it is marked `derived = TRUE` wherever shown. It is not a measured product.
- **X-BASE has no sensible heat (H) variable at all.** H is not compared anywhere in this 
  package; `tower_monthly.csv`'s `H_F_MDS` has no X-BASE counterpart.
- **X-BASE's own GPP is trained on nighttime-partitioned eddy-covariance data only** -- that is 
  why we used your sites' `GPP_NT`/`RECO_NT` columns, not `GPP_DT`/`RECO_DT`, when computing the 
  comparison statistics in `summary.csv`.
- **QC rule:** a tower month is treated as usable only if its QC fraction is >= 0.50 (the 
  FLUXNET2015 published convention, and our default throughout this analysis).
- **Training-site caveat:** `GH-Ank` and `SN-Dhr` are confirmed to be in FLUXCOM-X-BASE's own 
  294-site training set (Nelson et al. 2024, Table 1) -- the comparison at those two sites is 
  partly within-sample, not fully independent. Marked in `sites.csv` and the check figure.
- **Grid-cell caveat:** the 0.05 degree cell containing each tower is not centred exactly on 
  the tower -- see `cell005_dist_km` in `sites.csv` for how far off it is. The product's spatial 
  resolution (roughly 5 km at the equator) also means the cell may not represent the tower's 
  specific footprint, especially at sites with heterogeneous land cover nearby.

## Citations

**FLUXCOM-X-BASE:** Nelson, J.A., Walther, S., Jung, M., Gans, F., Kraft, B., Weber, U., Hamdi, 
Z., Duveiller, G., Zhang, W. (2023). FLUXCOM-X-BASE. ICOS ERIC - Carbon Portal. 
https://doi.org/10.18160/5NZG-JMJE. Method/product paper: Nelson et al. (2024), 
*Biogeosciences* 21, 5079-5115, doi:10.18160/5NZG-JMJE. Licence: CC BY 4.0.

**Each tower site's own dataset** must also be cited -- see `sites.csv`'s `site_citation` and 
`site_doi_or_handle` columns, one per site.

Questions about how this package was built: see `docs/methods_memo.md` and 
`docs/report_back_2026-11-21.md` in the WAFNET/fluxcom_comparison repository.
