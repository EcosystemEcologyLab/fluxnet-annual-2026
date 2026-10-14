New side analysis: WAFNET/fluxcom_comparison/, set up the same way as WAFNET/energy_partitioning/ (own README, code, docs, tables and figures, with a gitignored data/). It is not part of the Annual Paper and never writes to the paper's directories. FLUXCOM is non-Shuttle data, so label it as a comparison dataset in the code and in every output, per CLAUDE.md Hard Rule 1.

Request (Ossénatou Mamadou, WAF-Net paper), sites GH-Ank, BJ-Db1, BJ-Nhu, BJ-Bfg, SN-Nkr and SN-Dhr:
(1) scatter plots of observed EC fluxes against FLUXCOM for GPP, Reco, NEE, H and LE, at monthly and annual scales;
(2) bias, RMSE and R² per site, plus a site map with symbols sized and coloured by bias;
(3) multi-site Taylor diagrams, monthly and annual.
GPP and Reco use the daytime partition (GPP_DT, RECO_DT), with VUT where the site has it and CUT otherwise, chosen per site as in the paper. The WUE pilot's ban on daytime partitioning does not apply here.

This stage is a pre-analysis report only. Do not download gridded products and do not compute any statistic. Report:

1. FLUXCOM products. For FLUXCOM-X-BASE and the original FLUXCOM (carbon RS and RS+METEO; energy RS and RS+METEO), give: variables (whether Reco/TER, H and LE exist, or LE must come from ET); period; spatial and temporal resolution; whether daytime-partition GPP and TER members exist and how they are labelled; access route, licence and approximate size of a point extraction for six sites; and the citation for each. Use the products' own documentation and papers. Mark anything not found as not found, and say where you searched.
2. Training sites. Was each of the six sites in each product's training set? Use the published site lists or supplements. If no list exists, say so.
3. Observations. From the repo-root YY and MM extracts (read only), report per site and variable the months and years that pass the paper's QC rule, using compute_site_annual_fluxes() logic for years and the same QC gating at monthly scale. Report units as found, with the conversions you would apply.
4. Overlap. A table of site × variable × product giving the number of usable months and years falling within each product's period. Flag BJ-Db1 (2022 to 2024) and SN-Nkr (2018 to 2024) specifically.
5. Grid scale. The product cell size at each site, and the dominant land cover in that cell against the tower's IGBP class, from data already in the repo.
6. Decisions needed: which product, or which combination, per variable; how training-site membership is shown in the figures; what to do where overlap is short.

Write the report to WAFNET/fluxcom_comparison/docs/report_back_<date>.md and the tables to tables/. Save this prompt as logs/prompts/<date>_wafnet_fluxcom_stage1.md. Log, commit and push per CLAUDE.md.
