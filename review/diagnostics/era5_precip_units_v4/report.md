# ERA5 precipitation units, v4 — scoping follow-up to v3

Read-and-report only. No counterfactual, no reclassification, no pipeline edits.
Confined to `review/diagnostics/era5_precip_units_v4/`. Reuses v3's Part B
empirical clustering (`table_b1_factor_estimates.csv`, 123 sites near 4x/8x
vs. 323 near 1x, unrefit) and v2's reference table (`table_t2_ratios.csv`)
verbatim.

## Verdict

**123 of 781 sites are in doubt, and the cluster is defined almost entirely
by the absence of independent ground truth, not by hub, network, or
temporal resolution** — only 4/123 clustered sites have any
genuinely-measured tower precipitation at all (P_F_QC≥0.9, corrected
polarity from v3 A4), against 282/323 in the near-1x control group; the
next-best separator, `product_source_network`, only partially isolates the
cluster (JPF over-represented, ICOS under-represented, AMF and EUF present
in both groups at similar rates), and the clustered sites are not
regionally concentrated (AMF cluster sites alone span -55°S to 71°N).
**15 of the 123 are already among the 26 sites the KG_ERA5_MAP_MAX_MM
screen excludes; the other 108 currently carry a successful (non-NA) KG
classification that a future correction would change — that is the number
to plan around.** At the only 4 clustered sites where an independent
gauge exists, applying the nearest canonical factor leaves the corrected
value 2.6×–18.9× off from the gauge (not confirming the correction, though
n=4 gives this no real power), while a separate check finds BADM MAP is
*not* simply a WorldClim BIO12 lookup (only 1 of 399 sites with all three
references ties exactly, so v3's two-reference corroboration does not
collapse to one reference) — so the 108 figure is a **defensible upper
bound on sites in doubt**, not a validated correction target.

---

## 1. Grouping

### Separation score (max |row-proportion difference| between clustered and near-1x control)

| variable | score |
|---|---|
| has_measured_precip | **0.841** |
| product_source_network | 0.169 |
| data_hub | 0.093 |
| temporal_resolution | 0.018 |

`has_measured_precip` separates the two groups by far the most cleanly.
Full contingency tables in `table_1a_crosstab_hub.csv` through
`table_1d_crosstab_has_measured_precip.csv`.

### data_hub (row proportions)

| group | AmeriFlux | ICOS | TERN |
|---|---|---|---|
| clustered (n=123) | 0.472 | 0.520 | 0.008 |
| near-1x control (n=323) | 0.495 | 0.427 | 0.077 |

Nearly identical AmeriFlux share in both groups — hub does not separate them.

### product_source_network (row proportions)

| group | AMF | CNF | EUF | FLX | ICOS | JPF | KOF | SAEON | TERN |
|---|---|---|---|---|---|---|---|---|---|
| clustered | 0.472 | 0.065 | 0.187 | 0.008 | 0.016 | **0.203** | 0.041 | 0.000 | 0.008 |
| near-1x control | 0.495 | 0.040 | 0.146 | 0.019 | **0.158** | 0.034 | 0.025 | 0.006 | 0.077 |

JPF is over-represented in the cluster (0.203 vs. 0.034); ICOS is
under-represented (0.016 vs. 0.158). Still a partial separator only — AMF
and EUF are present at similar rates in both groups, so most of the
cluster (58/123) is not JPF at all.

### temporal_resolution (BADM `PRODUCT_TIME_RESOLUTION`; row proportions)

| group | HH | HR | NA |
|---|---|---|---|
| clustered | 0.951 | 0.024 | 0.024 |
| near-1x control | 0.966 | 0.006 | 0.028 |

Effectively no separation. **Note on this variable**: native HH/HR
resolution is not recoverable from the manifest/snapshot CSV or from
`data/processed/file_inventory.rds` — that file's `time_resolution` column
records the resolution of the *extracted product file* (MM/DD/WW/YY), not
the site's native collection interval, since
`FLUXNET_EXTRACT_RESOLUTIONS="y m d"` means HH/HR files were never
extracted for all but one site (US-MMS). Found instead in BADM's
`PRODUCT_TIME_RESOLUTION` field (759/781 sites covered); 22 sites have no
BADM record at all and are `NA` here.

### has_measured_precip (row proportions)

| group | FALSE | TRUE |
|---|---|---|
| clustered | **0.967** | 0.033 |
| near-1x control | 0.127 | **0.873** |

**This is the cleanest and most consequential separator.** 119/123
clustered sites have *zero* months of genuinely-measured tower
precipitation in 1991–2020 at any QC level; 282/323 near-1x sites do. The
4x/8x cluster is, in large part, simply the set of sites where nothing in
the tower record could have caught an ERA5 discrepancy even in principle —
this is a statement about detectability, not about mechanism.

### Coordinates of clustered sites (bounding box by network)

| network | n | lat range | long range |
|---|---|---|---|
| AMF | 58 | -54.8 to 71.3 | -156.6 to -53.1 |
| JPF | 25 | 12.7 to 62.3 | 105.5 to 142.3 |
| EUF | 23 | 37.0 to 69.1 | -7.0 to 26.7 |
| CNF | 8 | 37.8 to 42.1 | 98.9 to 110.3 |
| KOF | 5 | 34.6 to 37.3 | 126.6 to 127.3 |
| ICOS | 2 | 51.0 to 53.3 | -7.6 to 13.5 |
| FLX | 1 | 42.4 | 117.4 |
| TERN | 1 | -12.5 | 131.3 |

**Not regionally concentrated.** The largest single group (AMF, 58 sites)
spans nearly the full latitudinal range of the Americas — this is not a
single mis-registered region or a coastal/polar edge-of-grid effect. JPF
and CNF sites cluster more tightly (East Asia, ~13–62°N), consistent with
the JPF over-representation above, but account for only 33/123 sites. Full
per-site coordinates in `table_1e_clustered_site_coordinates.csv`.

---

## 2. Gauge check

Clustered sites with **any** genuinely-measured tower precipitation
(P_F_QC≥0.9, ≥1 month, 1991–2020): **4 of 123.**

**Low power warning, as instructed**: this is far too few to confirm or
refute the correction for the cluster as a whole. Reported for
transparency, not as a validation result:

| site_id | network | factor | ERA5 (raw) | ERA5 (corrected) | gauge (mm/yr) | corrected/gauge | n genuine months |
|---|---|---|---|---|---|---|---|
| CA-CF1 | AMF | 8 | 3649 | 456 | 179 | 2.55 | 3 |
| NO-And | EUF | 8 | 9318 | 1165 | 287 | 4.05 | 28 |
| US-DS1 | AMF | 4 | 1578 | 395 | 60 | 6.62 | 15 |
| US-DS2 | AMF | 4 | 1543 | 386 | 20 | 18.87 | 8 |

At all 4, the corrected value is still 2.6×–18.9× the gauge — the
correction does not bring these sites into agreement with the one
reference not used to fit the factor. CA-CF1 and NO-And have thin (3 and
28 months) or otherwise low sample; US-DS1/US-DS2 are desert sites (Mojave)
where a few genuinely-measured months may themselves be unrepresentative
of the annual total. This does not rule out the 4x/8x correction being
directionally right — it means the correction is **unconfirmed** at every
site where it could in principle be checked. Full table:
`table_2_gauge_check.csv`.

---

## 3. Is BADM independent of WorldClim?

637/781 sites have both BADM MAP and WorldClim BIO12.

- Ratio BADM/BIO12: min 0.00, 1st Qu. 0.945, **median 1.006**, mean 1.034,
  3rd Qu. 1.100, max 4.72.
- Within 2% of BIO12: **122/637 (19.2%)**.
- BADM value is an exact multiple of 100 mm: 44/637 (6.9%).
- BADM value is an exact multiple of 50 mm: 72/637 (11.3%).

The median/mean sitting close to 1 shows the two are correlated, as
expected for two real estimates of the same physical quantity — but only
19% match within 2%, and only a modest, not dominant, fraction of BADM
values are suspiciously round.

**Closer to gauge, at the 399 sites with all three (BADM, BIO12, and a
genuinely-measured tower gauge):**

| closer to | n |
|---|---|
| BADM | 180 |
| BIO12 | **218** |
| tie (badm == bio12 exactly) | 1 |

**BADM does not look like a WorldClim lookup.** If it were, `badm_map_mm`
would equal `bio12_mm` at most sites, producing many exact ties against the
gauge — instead there is exactly 1 tie in 399, and BIO12 is independently
closer to the gauge slightly more often than BADM is. v3's corroboration
argument (two independent references agreeing at 91% of clustered sites)
does not collapse to a single reference in disguise. Full tables:
`table_3a_badm_bio12_ratio.csv`, `table_3b_closer_to_gauge.csv`.

---

## 4. The count

| metric | count |
|---|---|
| Sites in doubt for ERA5 precipitation (cluster near 4x or 8x vs. BADM+BIO12) | **123** |
| ...of those, in the original 26 sites excluded by KG_ERA5_MAP_MAX_MM | 15 |
| ...of those, NOT in the original 26 (currently pass the >5000mm screen unflagged) | 108 |
| ...of those, currently carry a successful (non-NA) KG classification today | **108** |

**108 currently-classified sites carry ERA5 precipitation in doubt** —
roughly 14% of the 781-site network, none of them caught by the existing
`KG_ERA5_MAP_MAX_MM` absolute screen because their raw values, while
inflated ~4x–8x, don't clear 5000 mm/yr. This is the figure to plan around:
it is larger than the 26-site screen already known about, is not confined
to one hub/network/region, and — per §2 — is not yet independently
validated at any site where validation is possible. Full table:
`table_4_final_count.csv`.

## Not-found / carried-forward notes

- Native HH/HR temporal resolution per site: not present in the manifest
  snapshot CSV or in `file_inventory.rds` (searched both); recovered from
  BADM `PRODUCT_TIME_RESOLUTION` instead (759/781 sites). See §1.
- This report does not attempt to explain *why* BADM and BIO12 jointly
  disagree with ERA5 by ~4x/~8x at these 108 sites — that remains open, as
  it was in v3.
- No note to the FLUXNET Coordination Project is drafted here, per scope.
