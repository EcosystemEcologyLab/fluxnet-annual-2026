# FLUXCOM-X-BASE comparison -- figure captions

Compiled from each figure's own `.legend.txt` (same directory as the PNG), in the order the
figures are produced (`code/09_figures_draft.R` then `code/10_climatology.R`). See
`report_back_2026-11-03.md` for the full Stage 2 methods and `methods_memo.md` for the running
decision log. All figures are DRAFT and all data shown is non-Shuttle FLUXCOM-X-BASE comparison
data (CLAUDE.md Hard Rule 1) alongside FLUXNET Shuttle tower data -- see each caption for the
sign convention, units, and training/pending-site markers.

## 1. `figures/draft_monthly_scatter.png`

Figure (DRAFT). Monthly FLUXCOM-X-BASE values plotted against tower values, one panel per flux (NEE, GPP, RECO, LE). Each point is one site-month; the dashed line is the 1:1 line (points above it are X-BASE higher than the tower, below it lower). Colour identifies site (fixed palette, consistent across every Stage 2 figure); point shape identifies site status (circle = agreed, triangle = agreed and a confirmed X-BASE training site, square = pending Ossenatou).

Sites: BJ-Bfg, BJ-Nhu, GH-Ank, ML-Kem, SN-Dhr, SN-Nkr. Site-months per flux: NEE n=316, GPP n=285, RECO n=285, LE n=354 (per-site breakdown: tables/xbase_monthly_stats.csv).

Units: NEE, GPP, RECO in gC m-2 d-1; LE in W m-2.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 2. `figures/draft_site_map_bias.png`

Figure (DRAFT). Map of mean monthly bias (FLUXCOM-X-BASE minus tower) by site, one panel per flux (NEE, GPP, RECO, LE). Each site is one symbol: the border colour identifies the site (fixed palette, consistent across every Stage 2 figure), the fill colour and size both encode the mean bias for that flux (positive = X-BASE higher than tower, shown red; negative = X-BASE lower, shown blue). Symbol shape identifies site status (circle = agreed, triangle = agreed and a confirmed X-BASE training site, square = pending Ossenatou). Each panel has its own fill/size scale, since the fluxes are on different magnitude scales -- bubble sizes are not comparable across panels.

BJ-Nhu/BJ-Bfg and SN-Dhr/SN-Nkr are close enough that their symbols would overlap at this map's scale; both pairs are nudged apart by a small fixed offset, with a thin dashed line (site colour) back to the true coordinate.

Sites: BJ-Bfg, BJ-Nhu, GH-Ank, ML-Kem, SN-Dhr, SN-Nkr. Each site's bias is the mean over its own QC-passing site-months within 2001-2021 (tables/xbase_monthly_stats.csv has the exact n per site and flux).

Units: NEE, GPP, RECO bias in gC m-2 d-1; LE bias in W m-2.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 3. `figures/draft_taylor_diagram_NEE.png`

Figure (DRAFT). Taylor diagram for NEE (monthly), comparing FLUXCOM-X-BASE against the tower at each site. Each point's distance from the origin is the ratio of X-BASE's standard deviation to the tower's (sd(X-BASE)/sd(tower)) over that site's own monthly values; its angle from the x-axis is the Pearson correlation between the two series (labelled arcs at r = 0.1 to 0.99). The asterisk at (1, 0), labelled "tower", is the reference point a perfect match would sit on. Colour identifies site (fixed palette, consistent across every Stage 2 figure); point shape identifies site status (circle = agreed, triangle = agreed and a confirmed X-BASE training site, square = pending Ossenatou).

Sites shown (n=6): BJ-Bfg, BJ-Nhu, GH-Ank, ML-Kem, SN-Dhr, SN-Nkr. n site-months per site: tables/xbase_monthly_stats.csv.

Units: gC m-2 d-1.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 4. `figures/draft_taylor_diagram_GPP.png`

Figure (DRAFT). Taylor diagram for GPP (monthly), comparing FLUXCOM-X-BASE against the tower at each site. Each point's distance from the origin is the ratio of X-BASE's standard deviation to the tower's (sd(X-BASE)/sd(tower)) over that site's own monthly values; its angle from the x-axis is the Pearson correlation between the two series (labelled arcs at r = 0.1 to 0.99). The asterisk at (1, 0), labelled "tower", is the reference point a perfect match would sit on. Colour identifies site (fixed palette, consistent across every Stage 2 figure); point shape identifies site status (circle = agreed, triangle = agreed and a confirmed X-BASE training site, square = pending Ossenatou).

Sites shown (n=5): BJ-Bfg, BJ-Nhu, ML-Kem, SN-Dhr, SN-Nkr. n site-months per site: tables/xbase_monthly_stats.csv.

Units: gC m-2 d-1.

GH-Ank is not shown: it has no nighttime-partitioned tower data.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 5. `figures/draft_taylor_diagram_RECO.png`

Figure (DRAFT). Taylor diagram for RECO (monthly), comparing FLUXCOM-X-BASE against the tower at each site. Each point's distance from the origin is the ratio of X-BASE's standard deviation to the tower's (sd(X-BASE)/sd(tower)) over that site's own monthly values; its angle from the x-axis is the Pearson correlation between the two series (labelled arcs at r = 0.1 to 0.99). The asterisk at (1, 0), labelled "tower", is the reference point a perfect match would sit on. Colour identifies site (fixed palette, consistent across every Stage 2 figure); point shape identifies site status (circle = agreed, triangle = agreed and a confirmed X-BASE training site, square = pending Ossenatou).

Sites shown (n=5): BJ-Bfg, BJ-Nhu, ML-Kem, SN-Dhr, SN-Nkr. n site-months per site: tables/xbase_monthly_stats.csv.

Units: gC m-2 d-1 (derived).

GH-Ank is not shown: it has no nighttime-partitioned tower data.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 6. `figures/draft_taylor_diagram_LE.png`

Figure (DRAFT). Taylor diagram for LE (monthly), comparing FLUXCOM-X-BASE against the tower at each site. Each point's distance from the origin is the ratio of X-BASE's standard deviation to the tower's (sd(X-BASE)/sd(tower)) over that site's own monthly values; its angle from the x-axis is the Pearson correlation between the two series (labelled arcs at r = 0.1 to 0.99). The asterisk at (1, 0), labelled "tower", is the reference point a perfect match would sit on. Colour identifies site (fixed palette, consistent across every Stage 2 figure); point shape identifies site status (circle = agreed, triangle = agreed and a confirmed X-BASE training site, square = pending Ossenatou).

Sites shown (n=6): BJ-Bfg, BJ-Nhu, GH-Ank, ML-Kem, SN-Dhr, SN-Nkr. n site-months per site: tables/xbase_monthly_stats.csv.

Units: W m-2.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 7. `figures/draft_climatology_GH-Ank.png`

Figure (DRAFT). Mean seasonal cycle at GH-Ank (EBF), tower vs. FLUXCOM-X-BASE, one panel per flux (NEE, LE). GPP and RECO panels omitted for GH-Ank: it has no nighttime-partitioned (GPP_NT/RECO_NT) tower data at all (0 of 48 monthly rows), the partition X-BASE's own GPP training uses (see docs/report_back_2026-11-03.md section 4) -- not a QC exclusion. X-axis: calendar month (Jan-Dec). Line and points: mean value across the contributing years for that month (tower in GH-Ank's colour -- fixed palette, consistent across every Stage 2 figure; X-BASE in a single fixed colour used for every site). Shaded band: mean +/- 1 SD across years, shown only for months with >= 3 contributing years; open circles: the individual years' values instead, shown for months with fewer than 3. The number printed along the bottom of each panel is the count of contributing years for that month.

Years used: 2011, 2012, 2014. Site-months per flux: LE=24, NEE=22.

Units: NEE, GPP, RECO in gC m-2 d-1; LE in W m-2.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 8. `figures/draft_climatology_BJ-Nhu.png`

Figure (DRAFT). Mean seasonal cycle at BJ-Nhu (SAV), tower vs. FLUXCOM-X-BASE, one panel per flux (NEE, GPP, RECO, LE).  X-axis: calendar month (Jan-Dec). Line and points: mean value across the contributing years for that month (tower in BJ-Nhu's colour -- fixed palette, consistent across every Stage 2 figure; X-BASE in a single fixed colour used for every site). Shaded band: mean +/- 1 SD across years, shown only for months with >= 3 contributing years; open circles: the individual years' values instead, shown for months with fewer than 3. The number printed along the bottom of each panel is the count of contributing years for that month.

Years used: 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017. Site-months per flux: GPP=78, LE=100, NEE=86, RECO=78.

Units: NEE, GPP, RECO in gC m-2 d-1; LE in W m-2.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 9. `figures/draft_climatology_BJ-Bfg.png`

Figure (DRAFT). Mean seasonal cycle at BJ-Bfg (DBF), tower vs. FLUXCOM-X-BASE, one panel per flux (NEE, GPP, RECO, LE).  X-axis: calendar month (Jan-Dec). Line and points: mean value across the contributing years for that month (tower in BJ-Bfg's colour -- fixed palette, consistent across every Stage 2 figure; X-BASE in a single fixed colour used for every site). Shaded band: mean +/- 1 SD across years, shown only for months with >= 3 contributing years; open circles: the individual years' values instead, shown for months with fewer than 3. The number printed along the bottom of each panel is the count of contributing years for that month.

Years used: 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017. Site-months per flux: GPP=69, LE=84, NEE=70, RECO=69.

Units: NEE, GPP, RECO in gC m-2 d-1; LE in W m-2.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 10. `figures/draft_climatology_SN-Dhr.png`

Figure (DRAFT). Mean seasonal cycle at SN-Dhr (GRA), tower vs. FLUXCOM-X-BASE, one panel per flux (NEE, GPP, RECO, LE).  X-axis: calendar month (Jan-Dec). Line and points: mean value across the contributing years for that month (tower in SN-Dhr's colour -- fixed palette, consistent across every Stage 2 figure; X-BASE in a single fixed colour used for every site). Shaded band: mean +/- 1 SD across years, shown only for months with >= 3 contributing years; open circles: the individual years' values instead, shown for months with fewer than 3. The number printed along the bottom of each panel is the count of contributing years for that month.

Years used: 2010, 2011, 2012, 2013, 2015, 2016, 2017, 2019, 2020, 2021. Site-months per flux: GPP=70, LE=77, NEE=70, RECO=70.

Units: NEE, GPP, RECO in gC m-2 d-1; LE in W m-2.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 11. `figures/draft_climatology_ML-Kem.png`

Figure (DRAFT). Mean seasonal cycle at ML-Kem (WSA), tower vs. FLUXCOM-X-BASE, one panel per flux (NEE, GPP, RECO, LE).  X-axis: calendar month (Jan-Dec). Line and points: mean value across the contributing years for that month (tower in ML-Kem's colour -- fixed palette, consistent across every Stage 2 figure; X-BASE in a single fixed colour used for every site). Shaded band: mean +/- 1 SD across years, shown only for months with >= 3 contributing years; open circles: the individual years' values instead, shown for months with fewer than 3. The number printed along the bottom of each panel is the count of contributing years for that month.

Years used: 2007, 2008, 2009. Site-months per flux: GPP=22, LE=23, NEE=22, RECO=22.

Units: NEE, GPP, RECO in gC m-2 d-1; LE in W m-2.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

---

## 12. `figures/draft_climatology_SN-Nkr.png`

Figure (DRAFT). Mean seasonal cycle at SN-Nkr (CRO), tower vs. FLUXCOM-X-BASE, one panel per flux (NEE, GPP, RECO, LE).  X-axis: calendar month (Jan-Dec). Line and points: mean value across the contributing years for that month (tower in SN-Nkr's colour -- fixed palette, consistent across every Stage 2 figure; X-BASE in a single fixed colour used for every site). Shaded band: mean +/- 1 SD across years, shown only for months with >= 3 contributing years; open circles: the individual years' values instead, shown for months with fewer than 3. The number printed along the bottom of each panel is the count of contributing years for that month.

Years used: 2018, 2019, 2020, 2021. Site-months per flux: GPP=46, LE=46, NEE=46, RECO=46.

Units: NEE, GPP, RECO in gC m-2 d-1; LE in W m-2.

Tower data: FLUXNET Shuttle snapshot of 20 September 2026 (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv), QC-passing monthly values (QC >= 0.50, qc_passes()/QC_THRESHOLD_MM), VUT where available else CUT per site. Comparison data: FLUXCOM-X-BASE (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024; ICOS Carbon Portal, doi:10.18160/5NZG-JMJE; CC BY 4.0), 0.5 degree, monthly, 2001-2021 -- non-Shuttle comparison data (CLAUDE.md Hard Rule 1), used only as a labelled comparison, never as Annual Paper data.

Bias = X-BASE minus tower. NEE is negative for net uptake by the ecosystem, positive for net release (confirmed empirically against X-BASE; matches the tower's own convention). X-BASE has no respiration variable; RECO shown here is derived as GPP + NEE (both from X-BASE, same sign convention) and labelled "derived" wherever shown. X-BASE trains its GPP on the nighttime-partitioning method only; tower GPP and respiration are matched to it (GPP_NT/RECO_NT, not GPP_DT/RECO_DT) -- GH-Ank has no nighttime-partitioned tower data at all and is omitted from any GPP/RECO panel. X-BASE has no sensible-heat (H) variable; H is never shown.

GH-Ank and SN-Dhr are confirmed in FLUXCOM-X-BASE's own 294-site training set (Nelson et al. 2024 Table 1, direct match) -- marked with a distinct symbol. SN-Nkr is shown pending Ossenatou Mamadou's decision on whether to add it to the agreed site set (FLUXCOM-X-BASE's longer period gives it overlap the original FLUXCOM product did not) -- marked separately. BJ-Db1 has zero overlapping site-months with FLUXCOM-X-BASE and does not appear in any Stage 2 figure.

