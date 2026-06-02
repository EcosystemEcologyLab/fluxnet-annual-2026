# Figure Dependency Audit — 2026-06-02

Static inspection of `R/figures/fig_*.R`, `R/utils.R`, `scripts/07_figures.R`,
`scripts/00_candidate_figures.R`, and all `scripts/generate_*.R` files.
No pipeline run was performed. Findings are from source code only.

Three issue types were checked:

- **(A)** NEE_VUT_REF-only filtering — figures that silently drop the 36 CUT-only sites
- **(B)** Presence-definition assumption — NEE-only vs any-flux presence logic
- **(C)** Hardcoded column names, site lists, or numeric assumptions

---

## Issue Type A — NEE_VUT_REF-only filtering (36 CUT-only sites silently dropped)

### A1 — `fig_map_nee_mean()` (`R/figures/fig_maps.R`, ~line 269)

```r
dplyr::filter(is.finite(NEE_VUT_REF))
```

Filters to rows where `NEE_VUT_REF` is finite. The 36 CUT-only sites have `NA`
throughout for `NEE_VUT_REF` and are silently excluded from this figure.

### A2 — `fig_map_nee_delta()` (`R/figures/fig_maps.R`, ~line 371)

```r
dplyr::filter(is.finite(NEE_VUT_REF))
```

Same issue. CUT-only sites excluded from the ΔNEE map.

### A3 — `00_candidate_figures.R` (~line 284, NEE time-series diagnostic)

```r
dplyr::filter(df, !is.na(.data$NEE_VUT_REF))
```

Explicit VUT-only filter. No CUT fallback. CUT-only sites absent from diagnostic plots.

---

## Issue Type B — Presence-definition assumption

### B1 — `compute_site_year_presence()` docstring (`R/utils.R`, ~line 142–144)

The function itself uses a configurable `flux_vars` parameter (defaulting to
NEE VUT/CUT + GPP/RECO NT/DT × VUT/CUT + LE + H — the correct any-flux
definition). However, the docstring says "valid NEE data" and
`is_functionally_active()` docstring says "non-NA `NEE_VUT_REF` months."
The docstrings are misleading; the implementation is correct, but callers
relying on the docstring description may misunderstand what "has_data" flags.

**Impact:** No current data error — the implementation is already broad. Risk
is in future callers or documentation that treats `has_data` as NEE_VUT_REF-only.

### B2 — `is_functionally_active()` fallback (`R/utils.R`, ~line 298)

```r
result <- as.integer(last_year_vec) >= yr - thresh
```

The fallback path (used when `presence_df` is absent) tests only `last_year`
metadata, with no distinction between VUT and CUT sites. Any site with a recent
`last_year` (regardless of flux variable availability) is flagged as active.

**Impact:** Affects Fig 01 (network growth, functionally active line) when the
fallback is triggered.

### B3 — `scripts/07_figures.R` (~line 67–72)

```r
data_yy_flux <- data_yy_raw |>
  filter(dataset == "FLUXMET") |>
  mutate(TIMESTAMP = YEAR)
```

No "any flux present" filter applied at this stage. All FLUXMET rows pass
through regardless of whether any flux variable has valid data. This is
appropriate — row-level inclusion is delegated to each figure function — but
means figure functions are fully responsible for their own site filtering.
Gaps in figure-level filtering (A1, A2, A3 above) are therefore the only
defence.

---

## Issue Type C — Hardcoded column names and assumptions

### C1 — `fig_map_nee_mean()` (`R/figures/fig_maps.R`, ~line 258)

```r
stop("NEE_VUT_REF column not found in data_yy.")
```

Hard stop if `NEE_VUT_REF` is absent. A dataset containing only CUT data
would crash this function before plotting.

### C2 — `fig_map_nee_delta()` (`R/figures/fig_maps.R`, ~line 360)

Same hard stop on missing `NEE_VUT_REF`.

### C3 — `fig_whittaker_worldclim()` (`R/figures/fig_climate.R`, ~line 121)

```r
.check_cols_climate(data_yy, c("site_id", "NEE_VUT_REF", "YEAR"))
```

Hardcoded requirement for `NEE_VUT_REF`. CUT-only sites cannot appear in the
Whittaker figure even if their climate covariates are available.

### C4 — `scripts/generate_whittaker.R` (~line 65)

```r
nee_q <- quantile(data_yy$NEE_VUT_REF, probs = c(0.05, 0.95), na.rm = TRUE)
```

Direct column access. Does not check column existence first. If `NEE_VUT_REF`
were absent from the data (not the current case, but relevant for CUT-only
subsets), this would error without a diagnostic message.

### C5 — `scripts/00_candidate_figures.R` (~line 284)

```r
dplyr::filter(df, !is.na(.data$NEE_VUT_REF))
```

Hardcoded column name with no CUT fallback. (Overlaps with A3.)

---

## Files confirmed clean on all three issue types

- `R/figures/fig_igbp.R` — filters on `!is.na({{ flux_var }})` using a
  function argument, so caller controls which column is used. No hardcoded VUT.
- `R/figures/fig_latitudinal.R` — same pattern; flux variable is a parameter.
- `R/figures/fig_seasonal.R` — filters on `!is.na(NEE_VUT_REF)` and
  `!is.na(GPP_NT_VUT_REF)` for the seasonal cycle figures, which require DD
  data — these are not in scope until DD pipeline runs. The VUT-only filter
  here is consistent with DD data structure.
- `R/figures/fig_timeseries.R` — uses flux_var parameter; no hardcoded column.
- `R/figures/fig_network_growth.R` — uses metadata only (snapshot CSV, presence
  file). No flux data filtering.
- `R/figures/fig_growing_season.R` — uses DD + YY data; filters on
  `!is.na(NEE_VUT_REF)` at YY level, which is a concern (same as A-class) if
  CUT-only sites have meaningful growing-season data, but lower priority.
- `scripts/generate_maps.R`, `generate_duration_histograms.R`,
  `generate_historical_comparison_figures.R` — no flux column filtering;
  use snapshot metadata or historical site lists only.
- `scripts/generate_gez_anomaly_figures.R`, `generate_kg_anomaly_figures.R` —
  pass flux variable as a parameter; no hardcoded VUT.

---

## Priority assessment

| Finding | Affected figures | Fix complexity | Priority |
|---|---|---|---|
| A1/A2 — map filters drop CUT-only sites | `fig_map_nee_mean`, `fig_map_nee_delta` | Low — add `coalesce(NEE_VUT_REF, NEE_CUT_REF)` or filter on either | High |
| A3/C5 — candidate figure filter | `00_candidate_figures.R` diagnostic | Low — add `\| !is.na(NEE_CUT_REF)` | Medium |
| C3 — Whittaker column check | `fig_whittaker_worldclim` | Low — loosen check to accept either VUT or CUT | Medium |
| C4 — generate_whittaker quantile | `generate_whittaker.R` | Low — `coalesce` before quantile | Medium |
| B1 — misleading docstrings | `R/utils.R` | Trivial — text fix | Low |
| B2 — fallback in `is_functionally_active()` | Fig 01 active-site line | Medium — requires confirmed presence CSV | Low (deferred to Fig 01 pass) |
| C1/C2 — hard stops on missing VUT column | `fig_maps.R` | Low — check for CUT fallback before stopping | Medium |

---

## Recommended fixes before pipeline rerun

The two map functions (A1/A2, C1/C2) and the Whittaker column check (C3) should
be fixed before running `07_figures.R`. All involve the same pattern: replace
`NEE_VUT_REF`-only logic with `coalesce(NEE_VUT_REF, NEE_CUT_REF)` or an
explicit OR filter. The 36 CUT-only sites represent a small but non-zero fraction
of the 759-site set; excluding them silently from the NEE maps would be incorrect.

The `generate_whittaker.R` quantile fix (C4) is also straightforward and should
be applied at the same time.

`fig_igbp.R`, `fig_latitudinal.R`, `fig_timeseries.R` are already parameter-driven
and do not need changes — the caller in `07_figures.R` controls which flux column
is passed.
