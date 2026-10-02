**This note supports:** Figure 4 panels A and C, Geo vs Data side only (Geo vs Geo has no precipitation
dependency and no exclusions — every tower has a value in the gridded product by construction).

The new Figure 4's two precipitation-dependent Geo vs Data panels — Köppen (A) and aridity (C) — both
depend on each site's own 1991–2020 mean annual P_ERA, which is unreliable for some sites (see
`review/diagnostics/precip_downscaling_provenance/report.md`). Three exclusion rules apply, additive to
each other and applied in addition to each panel's own classification logic. All three are site-level: an
excluded site is removed from that panel's numerator *and* denominator, not merely left unclassified.

**Revised 2026-10-02: panel A applies these rules only to its ERA5-fallback sites.** Panel A's Geo vs Data
side now classifies from the PI-reported class (BADM `CLIMATE_KOEPPEN`) for every site that has one, and
falls back to the ERA5-local class (gated by the three rules below) only for the rest — see
`methods_koppen_era5.md`, "PI-reported class used first". A PI-sourced site is never excluded by these
rules, even if its own ERA5 climatology would fail one. Panel C has no PI-reported analogue, so all three
rules still apply to every site there, unchanged by this revision.

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

**Rule 3 — P_ERA_MIN_RATIO (`R/pipeline_config.R`, = 1/3; added 2026-10-02).** The low-side mirror of Rule
2: a site's 1991–2020 mean annual P_ERA falls *below* `P_ERA_MIN_RATIO` times *every* reference available
for it, same dual-reference AND logic, same "only reference available decides alone" fallback. Catches 22
sites beyond the 172 (e.g. `CA-TP2`, P_ERA≈0 mm/yr against BADM MAP 1036 mm/yr and BIO12 970 mm/yr) — the
same class of ERA5 spatial-averaging/extraction artifact as Rule 2, in the opposite direction. Sensitivity:
33 sites at ratio < 1/2, 22 at < 1/3 (the adopted threshold), 16 at < 1/4. Full site list, ratios, and
sensitivity counts are in `SESSION_LOG.md` (2026-10-02 entry for this change).

**Tracing n.** Every exclusion is logged via `log_exclusion()` (`outputs/exclusion_log.csv`, gitignored),
naming which rule triggered it.
- **Panel A** (ERA5-fallback sites only, since the 2026-10-02 PI-first revision — see above): of the 178
  sites without a PI-reported class, 28 are Rule-1-only (GRP_ERA_DOWN), 0 Rule-2-only
  (P_ERA_MAX_RATIO — all ten of the general Rule-2 catch have a PI-reported class and so are never
  evaluated by this rule here), 3 Rule-3-only (P_ERA_MIN_RATIO) = 31 excluded. n = 603 PI + 147 ERA5
  fallback = 750/781, J=0.399 (previous ERA5-only design: n=599/781, J=0.411).
- **Panel C** (all 781 sites, unaffected by the PI-first revision): 171 Rule-1-only + 10 Rule-2-only + 22
  Rule-3-only + 4 invalid-ERA5-input (see `methods_aridity_era5.md`) = 207 excluded, n=574/781, J=0.675
  (previous two-rule design: n=596/781, J=0.718) — one fewer Rule-1-only than the raw 172 because
  `DE-Zrk` is in both the 172-site group *and* the aridity-only invalid-input screen, counted once under
  the latter.
