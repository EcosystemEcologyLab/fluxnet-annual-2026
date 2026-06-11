# Session Log

A running record of Claude Code investigation reports, audits, and summaries for this project. Each session is demarcated by a date/time header; reports within a session appear under it as they're produced.

Convention: Claude Code appends its structured outputs (reports, audits, investigation summaries) to this file as they're produced, then commits and pushes immediately. Prompts and back-and-forth are not logged here, only Claude Code's reports.

## 2026-06-03 — fluxnet package metadata investigation

### fluxnet R package metadata audit

**Purpose:** Assess whether the fluxnet R package (Eric Scott, EcosystemEcologyLab) exposes enough site metadata to build a standalone FLUXNET citation tool — one that takes a site list and returns formatted citations without forcing a data download.

---

#### 1. Package version and source

| Field | Value |
|---|---|
| Version | 0.3.2.9000 |
| Source | GitHub — EcosystemEcologyLab/fluxnet-package |
| Branch | main |
| Commit SHA | d824d13f1f9ec4b637f795eb4f403f6edd4c96e6 |
| Built | 2026-05-24 (R 4.6.0, aarch64-apple-darwin23) |
| Locked in macos renv profile | yes |

---

#### 2. Exported functions (11 total)

**Download / installation**

| Function | Signature summary |
|---|---|
| `flux_install_shuttle()` | `(venv, shuttle_version, from, reinitialize)` — installs the Python fluxnet-shuttle CLI into a reticulate venv |
| `flux_download()` | `(file_list_df, site_ids, download_dir, overwrite, user_info, ...)` — downloads zip files per site |
| `flux_amf_credentials()` | `(user_name, user_email, intended_use, description)` — constructs the credential list from env vars |

**Discovery and extraction**

| Function | Signature summary |
|---|---|
| `flux_listall()` | `(cache_dir, cache_age, clean_cache, log_file, echo_cmd, ...)` — wraps shuttle `listall`; returns data frame of all available sites with metadata |
| `flux_extract()` | `(zip_dir, output_dir, site_ids, networks, resolutions, extract_varinfo, extract_txt, overwrite)` — unzips downloaded files |
| `flux_discover_files()` | `(data_dir, ...)` — builds a manifest data frame from unzipped files; required by read/metadata functions |

**Read and metadata (all require a manifest from `flux_discover_files()`)**

| Function | Signature summary |
|---|---|
| `flux_read()` | `(manifest, resolution, datasets, networks, site_ids)` — reads flux data CSVs |
| `flux_varinfo()` | `(manifest, resolution, networks, site_ids)` — reads BIFVARINFO files; returns variable-level info (units, height, measurement date, etc.) |
| `flux_badm()` | `(manifest, variable_group, site_ids, networks)` — reads BIF files for any BADM variable group (e.g. `"SOIL_CHEM"`, `"LAI"`, `"GRP_TEAM"`) |
| `flux_map_sites()` | `(manifest, color_var)` — plots site locations on a world map; `color_var` in `{data_hub, igbp, network, first_year, last_year}` |

**QC**

| Function | Signature summary |
|---|---|
| `flux_qc()` | `(data, qc_vars, max_gapfilled, operator)` — flags rows with gap-fill fraction above threshold |

---

#### 3. Site metadata accessible without downloading data?

**Yes — fully, via `flux_listall()`.**

`flux_listall()` calls the fluxnet-shuttle `listall` command (Python, network request to FLUXNET servers) and returns a data frame. No data download is required. The returned data frame has 18 columns:

```
data_hub              site_id               site_name
location_lat          location_long         igbp
network               team_member_name      team_member_role
team_member_email     first_year            last_year
download_link         fluxnet_product_name  product_citation
product_id            oneflux_code_version  product_source_network
```

This covers the core fields a citation tool needs:
- Site identity: `site_id`, `site_name`, `igbp`, `location_lat`, `location_long`
- Data hub / network: `data_hub` (AmeriFlux / ICOS / TERN), `network` (raw multi-value string, e.g. `"AmeriFlux;Phenocam"`)
- PI/team: `team_member_name`, `team_member_role`, `team_member_email` (semicolon-delimited multi-person strings)
- Pre-formatted citation: `product_citation` (see §4)
- Temporal coverage: `first_year`, `last_year`

The result can be cached locally (package default: invalidated after 1 day); the project already pins snapshots to `data/snapshots/*.csv`, which is the canonical locked source.

The package note in `flux_varinfo()` source confirms that listall is the primary metadata source: "much of the other metadata they contain can be found in the results of `flux_listall()`."

---

#### 4. Per-site citation strings retrievable?

**Yes — all 759 sites in the 2026-06-01 snapshot have a non-empty `product_citation` string.** No download required.

Citation format varies by data hub:

**AmeriFlux (375 sites)** — APA-style with DOI:
```
Maria Isabel Gassmann, Natalia Edith Tonti (2026), AmeriFlux FLUXNET-1F
AR-Bal Balcarce BA, Ver. v1.3_r1, AmeriFlux AMP, (Dataset).
https://doi.org/10.17190/AMF/2571144
```
All 375 AmeriFlux sites have `doi.org` links.

**ICOS hub (332 sites)** — sentence-style with handle.net persistent identifier:
```
Smart, K. (2025). Fluxnet Archive Product from Umhlabuyalingana, 2023–2024,
FLUXNET, https://hdl.handle.net/11676/STLJfs_nkGgsI6sPzHNIgYE0
```
Persistent identifier present but not a DOI; handle.net URIs resolve to the ICOS data portal.

**TERN hub (52 sites)** — sentence-style with no persistent identifier in current snapshot:
```
Feitz, A., Schroder, I., Kitchen, M. (2026): Emerald FLUXNET Release 2026_r1.
Version 2026_r1. Terrestrial Ecosystem Research Network (TERN). (Dataset).
```
No DOI or handle URI in the sampled TERN citations.

Coverage summary (snapshot 2026-06-01, 759 sites):

| Hub | Sites | Has `product_citation` | Has DOI (doi.org) | Has persistent ID |
|---|---|---|---|---|
| AmeriFlux | 375 | 375 (100%) | 375 (100%) | 375 |
| ICOS | 332 | 332 (100%) | 0 | 332 (handle.net) |
| TERN | 52 | 52 (100%) | 0 | 0 (in samples checked) |

---

#### 5. Recommended workflow per package docs

The vignette ("Getting Started") explicitly names `flux_listall()` as the citation source:

> "The list returned by `flux_listall()` contains metadata on the available sites including, importantly, citations for site-level data attribution which is required by FLUXNET."

And in the startup message registered via `.onAttach()`:

> "Citations for individual sites' datasets are returned by `fluxnet::flux_listall()`"

The documented workflow is:
```
flux_listall()  →  inspect / filter  →  flux_download()
                                      →  flux_extract()
                                      →  manifest <- flux_discover_files()
                                      →  flux_read(manifest)
                                      →  flux_varinfo(manifest)
                                      →  flux_badm(manifest, "VARIABLE_GROUP")
```

For citation-only use, the workflow ends after `flux_listall()`.

Additional site metadata available post-download via `flux_badm()` with `variable_group = "TEAM"`: full team member records from BIF files including `TEAM_MEMBER_NAME`, `TEAM_MEMBER_ROLE` (`PI`, `CO-PI`, `SCI`, `DATA`), `TEAM_MEMBER_EMAIL`, `TEAM_MEMBER_INSTITUTION`. This is richer than what `flux_listall()` returns (which has names/roles/emails but not institution), but requires a download.

---

#### 6. Gaps the citation tool would need to fill

1. **TERN DOIs absent.** TERN `product_citation` strings in the current snapshot contain no machine-readable persistent identifier. A citation tool that needs to hyperlink or verify all citations would need a fallback for TERN — either accept the plain-text citation or resolve identifiers from the TERN data portal separately.

2. **Citation format inconsistency across hubs.** AmeriFlux uses one format, ICOS another, TERN a third. A tool producing a uniform bibliography (APA, BibTeX, etc.) would need per-hub parsing/normalisation logic. The `product_citation` field is a pre-formatted display string, not structured metadata.

3. **`network` field is a multi-value semicolon-delimited string.** `flux_listall()` exposes `network` (e.g. `"AmeriFlux;Phenocam;NEON"`) and `data_hub` (single value: `AmeriFlux`/`ICOS`/`TERN`). A tool filtering by network membership would need to split on `";"`.

4. **`team_member_name` is also semicolon-delimited.** Multiple PIs are concatenated (e.g. `"Aaron Todd;Elyn Humphreys"`). Formatting as "Todd, A., Humphreys, E." for a bibliography would require name parsing.

5. **`flux_listall()` is a live network call.** It hits the FLUXNET shuttle servers. For a reproducible offline citation tool, the caller must supply a pinned snapshot CSV (as `data/snapshots/` already does in this project). The package's `cache_age` parameter and `cache_dir` are the controls; there is no argument to pass a pre-existing snapshot path directly — callers must set `cache_dir` and `cache_age` to point to an existing file.

6. **No elevation, land-use history, or canopy height in `flux_listall()`.** Those are in BADM/BIF (`flux_badm()`) and require a download. Not needed for citations, but relevant if the citation tool is extended to a full site-metadata lookup.

7. **`flux_varinfo()` is variable-level metadata, not site-level.** It returns instrument height, variable units, measurement dates, etc. per variable per site. Not relevant to citations.

---

**Bottom line for the citation tool design:**  
`flux_listall()` (or a pinned snapshot CSV from it) is sufficient. All 759 sites have `product_citation` strings. No data download is required. The tool's main engineering work is normalising three hub-specific citation formats and handling the multi-value `team_member_name` field if name-formatted output is needed.
