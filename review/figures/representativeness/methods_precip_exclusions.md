**This note supports:** Figure 4 panels A and C, Geo vs Data side only (Geo vs Geo has no precipitation
dependency and no exclusions — every tower has a value in the gridded product by construction).

The new Figure 4's two precipitation-dependent Geo vs Data panels — Köppen (A) and aridity (C) — both
depend on each site's own 1991–2020 mean annual P_ERA, which is unreliable for some sites (see
`review/diagnostics/precip_downscaling_provenance/report.md`). Two exclusion rules apply to both panels,
additive to each other and applied in addition to each panel's own classification logic. Both are
site-level: an excluded site is removed from that panel's numerator *and* denominator, not merely left
unclassified.

**Rule 1 — GRP_ERA_DOWN (172 sites).** Sites in the `precip_downscaling_provenance` diagnostic's
`not_fitted_slope_9999` group: their BIF-recorded `ERA_SLOPE` for precipitation is the sentinel value
−9999 — a second, distinct sentinel pattern from the other 609 current-network sites' `ERA_SLOPE = 1.0`.
Precipitation is not regressed at any of the 781 current-network sites: `ERA_INTERCEPT`/`ERA_RMSE`/
`ERA_CORRELATION` are also −9999 for every site, and zero sites show a genuinely fitted combination of
these four fields (`precip_downscaling_provenance/report.md` §2). "No usable regression slope" therefore
does not distinguish this group from the other 609 — the −9999 `ERA_SLOPE` sentinel itself is the only
distinguishing signal used, with no claim that the 609 sites' P_ERA is regression-validated. Source:
`review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv`.

**Rule 2 — P_ERA_MAX_RATIO (`R/pipeline_config.R`, = 3).** A site's 1991–2020 mean annual P_ERA exceeds
`P_ERA_MAX_RATIO` times *every* reference available for it: PI-reported BADM `MAP` where present and
non-zero, **and** WorldClim v2.1 BIO12 at the tower coordinate (extracted for all 781 sites into
`data/snapshots/site_precip_reference.csv`). Where only one reference exists (no BADM MAP), that one
decides alone. The threshold is set above the up-to-~2× differences topography alone can produce between
a point and a gridded climatology, and below the ~4× inflation seen at sites already known to carry ERA5
spatial-averaging artifacts.

*Revision history*: the rule originally used only one preferred reference (BADM MAP where available, else
BIO12), which caught 12 sites beyond the 172 and included two sites (`US-RGF`, `EE-Rng`) that BIO12 did
not independently confirm as implausible. Revised to require agreement from *every* available reference
(AND logic) so a single wrong or stale BADM value cannot alone exclude a site whose precipitation is
otherwise sound — the revised rule catches 10 sites beyond the 172. All four previously-identified cases
(`CA-CF2`, `IT-Niv`, `NO-And`, `US-HB4` — the last a known ERA5 spatial-averaging artifact) remain caught
under both versions. Full site lists, ratios, and the old-vs-new comparison are in SESSION_LOG.md
(2026-10-02 entries).

**Tracing n.** Every exclusion is logged via `log_exclusion()` (`outputs/exclusion_log.csv`, gitignored),
naming which rule triggered it. Panel A: 172 Rule-1-only + 10 Rule-2-only = 182 excluded, n=599/781.
Panel C: 171 Rule-1-only + 10 Rule-2-only + 4 invalid-ERA5-input (see `methods_aridity_era5.md`) = 185
excluded, n=596/781 — one fewer Rule-1-only than panel A because `DE-Zrk` is in both the 172-site group
*and* the aridity-only invalid-input screen, counted once under the latter.
