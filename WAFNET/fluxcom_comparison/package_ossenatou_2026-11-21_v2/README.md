# FLUXCOM-X-BASE 0.05 degree data package for Ossenatou Mamadou (2026-11-21, v2)

This package is for comparing FLUXCOM-X-BASE at its native 0.05 degree monthly grid with your 
tower (eddy-covariance) observations at the six WAF-Net sites -- it is data for your own 
analysis and plotting, not new figures or interpretation beyond the simple statistics in 
`summary.csv` and one labelled DRAFT check figure.

**This is v2, a fully traceable release.** Every value in every file below carries a key back 
to an exact source file, checksum, and persistent identifier, in `xbase_provenance.csv` (for 
FLUXCOM-X-BASE values) or `tower_provenance.csv` (for tower values) -- see "Tracing a value" 
below. It supersedes the v1 package (`package_ossenatou_2026-11-21/`); the numbers themselves 
are unchanged (same sites, same QC rule, same VUT/CUT choice, same sign conventions), this 
release adds the neighbouring-cell data, more tower variables, and full provenance.

**FLUXCOM-X-BASE is not FLUXNET/Shuttle data.** It is an independent, machine-learning-based 
gridded product used here only as a comparison against your tower observations.

## Files

- **`xbase_005_monthly.csv`** -- FLUXCOM-X-BASE values at the 0.05 degree grid cell containing 
  each tower, every month 2001-2021 (the product as distributed, not filtered by tower QC):
  - `site_id`, `year`, `month`.
  - `GPP_gC_m2_d1` -- gross primary productivity, grams carbon per m2 per day (a mean daily 
    rate for the month, not a monthly total).
  - `NEE_gC_m2_d1` -- net ecosystem exchange, same units. **Negative = net uptake by the 
    ecosystem (carbon sink), positive = net release** -- the same sign convention as the 
    tower's own NEE.
  - `ET_mm_hr`, `ET_T_mm_hr` -- evapotranspiration and its transpiration component, mean mm of 
    water per hour, as distributed by X-BASE.
  - `LE_W_m2` -- latent heat flux, derived from `ET_mm_hr` using the latent heat of 
    vaporization (2.45e6 J/kg): `LE = ET * 2.45e6 / 3600`.
  - `cell_lon`, `cell_lat`, `cell_dist_km` -- the grid cell centre used and its distance from 
    the tower (also in `sites.csv`).
  - `source_key_GPP`, `source_key_NEE`, `source_key_ET`, `source_key_ET_T` -- one key per 
    variable (each variable is a separate source file); look each up in `xbase_provenance.csv` 
    for that exact file's name, checksum, and ICOS persistent identifier.

- **`xbase_005_neighbours_monthly.csv`** -- the same values and source keys as 
  `xbase_005_monthly.csv`, but for the full 3x3 block of cells centred on each tower's own cell 
  (9 cells per site), in case you want to look at spatial heterogeneity around the tower:
  - `row_offset`, `col_offset` -- both in {-1, 0, 1}; `(0, 0)` is the tower's own cell (the same 
    cell as `xbase_005_monthly.csv`). `row_offset = -1` is one cell **north**, `+1` one cell 
    **south**; `col_offset = -1` one cell **west**, `+1` one cell **east** (confirmed from the 
    grid's own coordinate metadata, not assumed).
  - `cell_lon`, `cell_lat`, `cell_dist_km` -- that specific neighbour cell's centre and its 
    distance from the tower (always larger than the `(0, 0)` cell's distance).

- **`tower_monthly.csv`** -- the matching monthly tower (eddy-covariance) data for the same six 
  sites and months, same QC rule and VUT/CUT choice throughout this analysis:
  - `carbon_src` -- `VUT` or `CUT`: whether the site's carbon fluxes use the **variable** or 
    **constant** u* (friction velocity) threshold method for storage/turbulence filtering. 
    (This is a different choice from daytime/nighttime GPP-RECO partitioning, below -- all six 
    sites here are `VUT`.)
  - `NEE_VUT_REF`, `GPP_DT_VUT_REF`/`RECO_DT_VUT_REF` (daytime partition), 
    `GPP_NT_VUT_REF`/`RECO_NT_VUT_REF` (nighttime partition), `LE_F_MDS`, `H_F_MDS` -- native 
    FLUXNET units (NEE/GPP/RECO: gC m-2 d-1; LE/H: W m-2).
  - `LE_CORR`, `H_CORR` -- the energy-balance-corrected latent and sensible heat flux (W m-2). 
    **The product provides no QC column of its own for either** -- no `..._pass_qc` column is 
    given for them; use `LE_F_MDS_QC`/`H_F_MDS_QC` as a guide to data quality in that month if 
    you need one.
  - `P_F` (mm d-1, mean daily rate for the month, not a monthly total -- same convention as the 
    carbon fluxes), `TA_F` (deg C), `VPD_F` (hPa), `SW_IN_F` (W m-2), `SWC_F_MDS_1` (%, shallow 
    soil water content -- **absent from `ML-Kem`'s data entirely; its rows are `NA`, meaning the 
    site does not provide this variable, not that the value is zero**). Each has its own 
    `..._QC` (fraction) and `..._pass_qc` column, same rule as the fluxes.
  - Each flux/variable has a matching `..._QC` column: **the fraction (0-1) of the underlying 
    half-hourly/hourly records that were measured or good-quality gap-filled** -- not the 
    0/1/2/3 integer flag used at half-hourly resolution. `GPP`/`RECO` (both partitions) have no 
    QC column of their own at monthly resolution and are gated on the site's own NEE QC instead 
    (`GPP_RECO_DT_QC`/`GPP_RECO_NT_QC`, both equal to `NEE_VUT_REF_QC`).
  - Each flux/variable (except `LE_CORR`/`H_CORR`, above) also has a `..._pass_qc` TRUE/FALSE 
    column: whether that month's QC fraction is at least 0.50, the threshold used throughout 
    this analysis.
  - **`LE_flagged_for_review`** -- TRUE for four site-months where `LE_F_MDS` passes the QC rule 
    above but looks high for the season, flagged for you to look at, not removed from the data: 
    `BJ-Bfg` 2012-03 (358 W m-2, QC 0.52), `BJ-Nhu` 2010-08 (205 W m-2), `BJ-Nhu` 2011-12 
    (214 W m-2), `BJ-Nhu` 2012-01 (214 W m-2). These are flagged, not known errors.
  - We used **nighttime-partitioned** GPP/RECO (`GPP_NT`/`RECO_NT`) in `summary.csv`, because 
    X-BASE's own documentation states its GPP training targets are nighttime-partitioned; the 
    daytime columns (`GPP_DT`/`RECO_DT`) are included here too in case you want them.

- **`sites.csv`** -- one row per site: name, country, IGBP vegetation class, tower coordinates, 
  the 0.05 degree grid cell centre used and its distance from the tower (`cell005_dist_km`), 
  the number of tower months (2001-2021) that pass the QC rule above, whether the site is in 
  FLUXCOM-X-BASE's own training set, and the tower dataset's own citation and persistent 
  identifier (`site_doi_or_handle`) -- **please cite each tower site's own dataset**, in 
  addition to X-BASE, in any publication using this data.

- **`summary.csv`** -- one row per site and flux (NEE, GPP, RECO [derived], LE), at the 0.05 
  degree resolution in this package: number of QC-passing overlapping months, bias (X-BASE 
  minus tower), RMSE, and R-squared. `derived = TRUE` for RECO; see below.

- **`check_figure_xbase_005_vs_tower_DRAFT.png`** -- one check figure, monthly X-BASE (0.05 
  degree) against tower, one panel each for NEE/GPP/LE, sites by colour, training sites marked, 
  1:1 reference line. Labelled DRAFT -- a sanity check, not a paper figure.

- **`tower_provenance.csv`**, **`xbase_provenance.csv`** -- see "Tracing a value" below.

- **`MANIFEST.csv`** -- every file in this package, its size and SHA-256, the git commit and 
  script names that built it.

## Tracing a value to its source

Every row in `xbase_005_monthly.csv` and `xbase_005_neighbours_monthly.csv` carries one 
`source_key_<VARIABLE>` per variable (each variable is downloaded as a separate yearly file). 
Look that key up in `xbase_provenance.csv`'s `source_key` column to get the exact FLUXCOM-X-BASE 
file name, its ICOS persistent identifier (`icos_object_pid_url`), its SHA-256 checksum (in both 
hex and the base64 form ICOS itself reports), its download date, and its licence.

Every row in `tower_monthly.csv` and `sites.csv` is for one `site_id`. Look that up in 
`tower_provenance.csv` to get the tower dataset's persistent identifier (`dataset_pid_url`) and 
citation, the exact monthly file name and its SHA-256 checksum, the FLUXNET Shuttle snapshot 
file and row it was confirmed against, the FLUXNET Shuttle and ONEFlux processing-code 
versions, and the VUT/CUT choice.

## Important things to know before analysing this data

- **Sign convention:** NEE is negative for net ecosystem carbon uptake, positive for release, 
  in both the tower and X-BASE series. **Bias is always X-BASE minus tower** throughout.
- **X-BASE has no ecosystem respiration variable of its own.** If you need RECO from X-BASE, 
  derive it as `GPP + NEE` (both from X-BASE, same sign convention) -- this is what `summary.csv` 
  does, and it is marked `derived = TRUE` wherever shown. It is not a measured product.
- **X-BASE has no sensible heat (H) variable at all.** H is not compared anywhere in this 
  package; `tower_monthly.csv`'s `H_F_MDS`/`H_CORR` have no X-BASE counterpart.
- **X-BASE's own GPP is trained on nighttime-partitioned eddy-covariance data only** -- that is 
  why we used your sites' `GPP_NT`/`RECO_NT` columns, not `GPP_DT`/`RECO_DT`, when computing the 
  comparison statistics in `summary.csv`.
- **QC rule:** a tower month is treated as usable only if its QC fraction is at least 0.50 -- 
  our own threshold choice throughout this analysis.
- **Training-site caveat:** `GH-Ank` and `SN-Dhr` are in FLUXCOM-X-BASE's own training set -- 
  the comparison at those two sites is partly within-sample, not fully independent. Marked in 
  `sites.csv` and the check figure.
- **Grid-cell caveat:** the 0.05 degree cell containing each tower is not centred exactly on 
  the tower -- see `cell005_dist_km` in `sites.csv` (and `cell_dist_km` for every neighbour cell 
  in `xbase_005_neighbours_monthly.csv`) for how far off it is. The product's spatial resolution 
  (roughly 5 km at the equator) also means no single cell may fully represent the tower's own 
  footprint, especially at sites with heterogeneous land cover nearby -- the neighbouring-cell 
  file is provided so you can check this yourself.

## Citations

**FLUXCOM-X-BASE dataset:** Nelson, J.A., Walther, S., Jung, M., Gans, F., Kraft, B., Weber, U., 
Hamdi, Z., Duveiller, G., Zhang, W. (2023). FLUXCOM-X-BASE. ICOS ERIC - Carbon Portal. 
https://doi.org/10.18160/5NZG-JMJE. Licence: CC BY 4.0.

**FLUXCOM-X-BASE method/product paper:** Nelson, J.A. et al. (2024). Biogeosciences 21, 
5079-5115. https://doi.org/10.5194/bg-21-5079-2024.

**Each tower site's own dataset** must also be cited -- see `sites.csv`'s `site_citation` and 
`site_doi_or_handle` columns (also in `tower_provenance.csv`), one per site.

Repository: https://github.com/EcosystemEcologyLab/fluxnet-annual-2026
