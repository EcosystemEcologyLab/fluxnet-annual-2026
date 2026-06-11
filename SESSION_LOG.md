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

---

### Citation generator inspection: two-mode portability

**Purpose:** Assess `scripts/generate_fluxnet_citations.R` for what would need to change to become a portable two-mode standalone tool — a "full" mode using BIF files on disk plus a "lightweight" fallback using only `flux_listall()` snapshot output.

---

#### 1. Script overview

**File:** `scripts/generate_fluxnet_citations.R`  
**Length:** 583 lines, 22 KB  
**Committed:** 2026-05-28

**Section structure:**

| Lines | Section | What it does |
|---|---|---|
| 1–38 | Header + setup | Usage docs; loads `.env`; `source()`s `R/pipeline_config.R` and `R/utils.R`; calls `check_pipeline_config()` |
| 40–47 | Imports + constants | `dplyr`, `readr`, `stringr`; defines `ICOS_NETWORKS` and `KNOWN_NETWORKS` |
| 49–96 | BibTeX helpers | `bib_escape()`, `bib_field()`, `format_misc_entry()`, `format_article_entry()` |
| 98–169 | Per-family parsers | `parse_amf()`, `parse_icos()`, `parse_tern()`, `parse_saeon()` — each takes `(cit, pid)` from snapshot columns |
| 171–176 | Cite key builder | `make_cite_key(site_id, network, year)` |
| 178–228 | Single-entry builder | `build_bib_entry(row)` — dispatch + guards + verbatim-preservation flags |
| 230–286 | Mandated references | `mandated_bib(networks)` — hardcoded Pastorello 2020 + conditional TERN and JPF refs |
| 288–378 | Acknowledgments | `build_acknowledgments(networks)` — network-conditional boilerplate |
| 380–451 | Review flags | `build_review_flags(flags_named, sites_df)` — assembles human-review markdown |
| 453–464 | Snapshot loader | `find_latest_snapshot()` — scans `data/snapshots/` for newest CSV |
| 466–583 | Main function | `generate_fluxnet_citations()` — entry point, 118 lines |

**Inputs:**
- Snapshot CSV: path resolved by `find_latest_snapshot()` using `FLUXNET_DATA_ROOT` env var, or via explicit `snapshot_path` argument
- Optional: `site_ids_csv` (any CSV with a site ID column)
- Environment: `FLUXNET_DATA_ROOT`, `AMERIFLUX_USER_NAME`, `AMERIFLUX_USER_EMAIL`, `FLUXNET_SHUTTLE_VERSION`, `FLUXNET_SNAPSHOT_MODE`

**Outputs (all under `output_prefix`):**
- `{prefix}.bib` — one `@misc` per site + mandated `@article` refs
- `{prefix}_acknowledgments.md` — global + network-conditional acknowledgment boilerplate
- `{prefix}_review_flags.md` — per-site human-review notes

---

#### 2. Paper-repo dependencies and portability classification

| Dependency | Where used | Classification | Notes |
|---|---|---|---|
| `source("R/pipeline_config.R")` | Line 36, top-level | **Non-portable** | Assigns `FLUXNET_DATA_ROOT`; paper-repo relative path |
| `source("R/utils.R")` | Line 37, top-level | **Non-portable** | Paper-repo relative path; functions not actually called in this script (see below) |
| `check_pipeline_config()` | Line 38, top-level | **Non-portable** | Requires `AMERIFLUX_USER_NAME`, `AMERIFLUX_USER_EMAIL`; calls `fluxnet::flux_install_shuttle()`; checks shuttle version. None of this is needed for citation generation |
| `FLUXNET_DATA_ROOT` | `find_latest_snapshot()` (line 457) | **Trivially portable** | Used only to locate `snapshots/*.csv`. Replace with a function argument defaulting to `"data"` or any user-supplied path |
| `find_latest_snapshot()` as `snapshot_path` default | Function signature line 485 | **Trivially portable** | Already a function argument — callers can pass any CSV path directly |
| `data/snapshots/` directory layout | `find_latest_snapshot()` | **Trivially portable** | Caller passes path explicitly; layout assumption lives only in the default |
| `R/utils.R` functions | Not called anywhere in this script | **Trivially portable** | The `source("R/utils.R")` at the top is dead weight for this script — none of `write_output_metadata()`, `log_exclusion()`, or `log_unknown()` are called |

**Critical finding:** `source("R/utils.R")` is sourced but none of its exported functions are actually called by this script. It is inherited boilerplate from the pipeline script template. Removing it eliminates one paper-repo dependency at zero functional cost.

**Summary:** The only genuine non-portable elements are the three lines at the top of the script (lines 32–38). Everything from line 40 onward — all the parsing, formatting, and output logic — has no paper-repo dependencies.

---

#### 3. BIF-reading logic

**There is none. The current script does not read BIF files.**

This is the most important finding for the refactor plan. All citation data is sourced exclusively from the **snapshot CSV** (the output of `flux_listall()`). The script reads exactly four columns:

| Column | Used in |
|---|---|
| `site_id` | Filtering, cite keys, flags |
| `product_citation` | All four parsers — primary citation string |
| `product_id` | All four parsers — DOI or handle identifier |
| `product_source_network` | Routing to parser family; mandated refs; ack sections |

The "full mode" described in the prompt — using BIF files for deeper verbatim author strings — does not yet exist. The current script is already, in functional terms, what the prompt calls the "lightweight mode." The verbatim-preservation features (double-brace wrapping, preserved typos) operate on the `product_citation` string from the snapshot, not on BIF-sourced data.

**Implication for the refactor:** The two-mode split is an addition of a BIF-reading code path, not a decomposition of existing BIF logic. The "full mode" requires new development; the "lightweight mode" is what exists today.

A `load_metadata()` function wrapping BIF access would be straightforward to isolate: it either enriches the snapshot data frame with BIF-derived structured author/institution fields (full mode) or returns the snapshot data frame as-is (lightweight mode). This is the clean seam for the split.

---

#### 4. Format-family routing logic

The routing decision is made in `build_bib_entry()` (lines 202–213), in a single `if/else if` chain inside `tryCatch`:

```r
if      (net == "AMF")           parse_amf(cit, pid)
else if (net %in% ICOS_NETWORKS) parse_icos(cit, pid)
else if (net == "TERN")          parse_tern(cit, pid)
else                             parse_saeon(cit, pid)
```

The per-format formatting logic is **already well-factored**:
- Four dedicated parser functions (`parse_amf`, `parse_icos`, `parse_tern`, `parse_saeon`) each accept `(cit, pid)` and return a normalized `list(authors, year, title, doi=NULL, url=NULL)`
- After dispatch, a single call to `format_misc_entry()` handles all families uniformly
- The cite key is built with `make_cite_key()` after parsing regardless of family

**No restructuring of the dispatch layer is required.** The parsers are already self-contained; the dispatch is a clean four-way branch. For the refactor, the main change is whether `cit` and `pid` come from the snapshot's `product_citation`/`product_id` columns (current path) or from a BIF-derived normalized structure (full mode).

---

#### 5. Verbatim-preservation and cosmetic-cleanup logic

**`{{...}}` double-brace wrapping** (prevents BibTeX name-parsing and title-casing):  
Located in `format_misc_entry()` (line 72). The author string is wrapped as `sprintf("{%s}", bib_escape(author))` before being passed to `bib_field()`, which wraps in a second `{...}` — yielding `author = {{...}}` in the `.bib` output. This is applied to every entry regardless of family. No mode-sensitivity needed here.

**Cosmetic cleanups (two locations):**
1. **ICOS double-apostrophe fix** — `parse_icos()` line 114: `gsub("''", "'", cit, fixed = TRUE)`. Normalises an encoding artifact in approximately 5 UK ICOS sites (e.g. `D''Acunha` → `D'Acunha`). Silently applied; no flag.
2. **TERN trailing-whitespace / double-space collapse** — `parse_tern()` line 152: `str_squish(m[[4]])` on the title. Handles the 36 sites with a space-before-period variant (` .`) in the `Version` boundary. Silently applied; no flag.

**Preserved-typo flags (hardcoded by site_id, in `build_bib_entry()` lines 219–222):**
- `AU-Cow`: flags if `FLUXNEXT` appears in citation (typo for `FLUXNET`)
- `US-Hsm`: flags if `Kyle_Delwiche` appears (underscore in author name; also notes that it is escaped as `Kyle\_Delwiche` in BibTeX output)

These are point fixes — hardcoded string checks against specific site IDs. They are portable as-is; they will silently not trigger if those sites are absent from the requested site list, and will trigger correctly if those site IDs are present.

**Risk note:** The `US-Hsm` flag comment says the underscore is "escaped as `Kyle\_Delwiche` in BibTeX" — this is handled by `bib_escape()` at line 58 (`gsub("_", "\\_", s, fixed = TRUE)`), which runs on the full author string. The flag is informational; the escaping is already automatic.

---

#### 6. Output generation

All three outputs are built from a shared intermediate state: the `results` list (one entry per site, each a `list(site_id, bib, flag)`) plus the `networks` character vector.

| Output | Built from | Mode-sensitivity needed? |
|---|---|---|
| `.bib` | `bib_entries` (formatted strings) + `mandated_bib(networks)` | Header comment: add lightweight-mode warning note |
| `_acknowledgments.md` | `build_acknowledgments(networks)` | Not mode-sensitive — acknowledgments are network-conditional, not data-source-conditional |
| `_review_flags.md` | `build_review_flags(flags_named, sites_df)` | Add a top-level `## Data source warning` section in lightweight mode |

**Sketch of lightweight-mode changes:**

`.bib` header addition (new comment lines):
```
%% WARNING: LIGHTWEIGHT MODE — citations derived from flux_listall() snapshot
%% product_citation field only. Author strings have not been verified against
%% BIF files. Review _review_flags.md before submission.
```

`_review_flags.md` addition (new section at top):
```markdown
## Data source warning

**Lightweight mode**: citations were generated from the FLUXNET shuttle snapshot
(`product_citation` field) without BIF verification. Author strings, titles, and
identifiers are reproduced verbatim from the snapshot. Before journal submission,
verify against the FLUXNET data registry (https://fluxnet.org) or re-run in full
mode with BIF files present.
```

`_acknowledgments.md`: no change needed.

---

#### 7. Hard-guard and `stop()` logic

**In `check_pipeline_config()` (called at script top-level, not inside the function):**
1. `stop("AMERIFLUX_USER_NAME is not set")` — not needed for citations
2. `stop("AMERIFLUX_USER_EMAIL is not set")` — not needed for citations
3. `stop(...)` on invalid `FLUXNET_SNAPSHOT_MODE` — not needed for citations
4. `stop(...)` on missing `FLUXNET_SNAPSHOT_FILE` when mode is locked — not needed for citations

These four stops fire **before `generate_fluxnet_citations()` is ever called**. In a standalone tool, the entire `check_pipeline_config()` call is dropped. None of these guards are inside the citation logic.

**In `generate_fluxnet_citations()` and helpers — guards that remain valid in both modes:**

| Guard | Location | Mode impact |
|---|---|---|
| Both/neither `site_ids` and `site_ids_csv` provided | Lines 493–497 | Keep as-is in both modes |
| `site_ids_csv` file not found | Line 499 | Keep as-is |
| Column not in CSV | Lines 502–505 | Keep as-is |
| `stop("GUARD: requested site(s) not in snapshot: ...")` | Line 519 | Keep as-is — valid in both modes |
| `stop("GUARD: site %s has empty product_citation")` | Line 190 | Keep as-is — `product_citation` is present in all 759 sites |
| `stop("GUARD: site %s has empty product_id")` | Line 192 | **Soften for lightweight mode**: TERN sites in current snapshot have a product_id, but future snapshots may not guarantee this; guard should be a warning-flag rather than a stop in lightweight mode |
| `stop("GUARD: site %s has unknown product_source_network")` | Line 194 | Keep as-is — if a new network appears, the user needs to know |
| `find_latest_snapshot()` stop on no CSVs found | Line 462 | Keep as-is (becomes a user-supplied argument in portable form) |

Parser `stop()` calls inside `tryCatch` (lines 105, 108, 116, 123, 126, 133, 147, 160, 164) — these are already caught and converted to per-site flags rather than aborting. No mode change needed.

**Net change for lightweight mode:** drop `check_pipeline_config()` (4 stops disappear); optionally soften `product_id` empty-guard to a warning-flag rather than abort. Everything else is unchanged.

---

#### 8. Proposed function decomposition

The refactor has two distinct parts: (A) decoupling from the paper repo, and (B) adding a BIF full-mode path. These are independent and can be done sequentially.

**Part A — Portability (no new functionality, no BIF reads):**

The minimum change to make the script a self-contained portable tool is removing lines 32–38 (the `dotenv`/`source`/`check_pipeline_config` block). The rest of the script already has no paper-repo dependencies. This can be done as a one-commit surgical change: delete those 7 lines, add a comment saying the script requires only base R + `dplyr`/`readr`/`stringr`, adjust `find_latest_snapshot()` to accept a root directory argument rather than reading `FLUXNET_DATA_ROOT`.

**Part B — Two-mode decomposition:**

```
load_metadata(site_ids, snapshot_path, bif_dir = NULL)
```
- **Does:** Reads snapshot CSV; filters to `site_ids`; if `bif_dir` is non-NULL and BIF files exist for the requested sites, reads `flux_badm()`-equivalent BIF data and joins structured author/institution fields; attaches a `mode` attribute (`"full"` or `"lightweight"`) to the returned data frame
- **Absorbs:** Current snapshot-reading block (lines 512–519) plus new BIF-reading logic
- **Tricky:** Detecting partial BIF presence — if BIF files exist for 90/100 sites, does the tool switch to full mode for 90 and lightweight for 10, or force a uniform mode choice? A clean default: full mode requires BIF files for **all** requested sites; partial coverage falls back to lightweight with a per-site flag

```
parse_citation_string(row)
```
- **Does:** Dispatches to `parse_amf`/`parse_icos`/`parse_tern`/`parse_saeon` based on `product_source_network`; returns `list(authors, year, title, doi, url, flag)` — essentially the current dispatch block inside `build_bib_entry()`
- **Absorbs:** Lines 202–213 (dispatch) + the four parser functions + `make_cite_key()`
- **Tricky:** In full mode, the `authors` field would come from BIF-derived structured data rather than parsed `product_citation`. The BIF author string construction would be a separate code path within this function, gated on `row$mode` or a supplied `bif_authors` column. Parser functions themselves (`parse_amf` etc.) don't need to change — they're the fallback path.

```
build_bib_entry(row)    [keep as-is or thin wrapper]
```
- **Does:** Calls `parse_citation_string()` + `format_misc_entry()` + verbatim-preservation flag checks
- **Absorbs:** Current `build_bib_entry()` (lines 178–228), which is already well-scoped
- **Tricky:** None; this function is already clean

```
write_bib_file(bib_entries, mandated_refs, networks, metadata, output_path)
```
- **Does:** Assembles header + entries + mandated refs; in lightweight mode, adds the `%% WARNING` header block
- **Absorbs:** Lines 532–565 of the main function
- **Tricky:** Header comment needs a `mode` flag; otherwise no change

```
write_acknowledgments(networks, output_path)    [unchanged]
```
- **Does:** Unchanged from current `build_acknowledgments(networks)` + `writeLines()` 
- **No mode-sensitivity needed**

```
write_review_flags(flags, sites_df, mode, output_path)
```
- **Does:** Adds top-level lightweight-mode warning section if `mode == "lightweight"`; otherwise identical to current logic
- **Absorbs:** Current `build_review_flags()` + `writeLines()` call

```
generate_citations(site_ids, output_prefix, snapshot_path, bif_dir = NULL, ...)
```
- **Does:** Top-level entry point; calls `load_metadata()` → row-wise `build_bib_entry()` → three `write_*()` functions; reports mode and flag count
- **Absorbs:** Current `generate_fluxnet_citations()` main body, renamed and stripped of paper-repo setup code
- **Tricky:** None; the current function is already cleanly structured

---

#### 9. Refactor risks

**Risk 1: "Full mode" is new code, not a refactor of existing code.**  
The prompt characterizes the refactor as splitting existing BIF-reading logic into a mode-gated path. In reality, BIF reading does not exist in the current script. The full-mode path is new development. This is not a problem, but it means the refactor's scope is larger than the prompt implies: Part A (portability) is a surgical 7-line change; Part B (full mode) is a new feature. Conflating them risks scope creep. Recommended: do Part A as a standalone PR before designing the BIF author enrichment.

**Risk 2: BIF author strings vs. snapshot `product_citation` may diverge.**  
The current script trusts `product_citation` from the snapshot as the authoritative citation string. BIF `GRP_TEAM` data provides structured per-person records (name, role, email, institution) — but the formatted order, punctuation, and year in `product_citation` is the FLUXNET registry's own formatting. If full mode constructs citations from BIF `TEAM_MEMBER_NAME` records, the result may differ from `product_citation` in name order, formatting, or year. Deciding what "full mode" means exactly — verbatim BIF data + reconstructed formatting, or `product_citation` + BIF-derived supplementary fields — is a design decision that must be made before writing the code.

**Risk 3: 22 no-author ICOS sites behave identically in both modes.**  
`parse_icos()` already handles the no-author pattern (`authors = NA_character_`). In lightweight mode, these sites produce a flagged entry with no `author` field in BibTeX — which is the correct behavior. In full mode, BIF `GRP_TEAM` data would likely provide a team member list, but if BIF has no PI-role member for a given site, the same NA path applies. No special handling needed; the existing `tryCatch` and flag mechanism covers it.

**Risk 4: Hardcoded preserved-typo checks (`AU-Cow`, `US-Hsm`) will not catch future typos.**  
These are maintenance items. In a standalone tool distributed to other researchers, the hardcoded checks would fire for those two sites but miss any new upstream typos. A more robust approach for a standalone tool would be to flag any `product_citation` containing a raw underscore not inside a URL — but that is an enhancement, not a blocking issue for the initial refactor.

**Risk 5: `find_latest_snapshot()` default couples the function signature to a directory layout.**  
`snapshot_path = find_latest_snapshot()` is evaluated at function call time, not at definition time. If no `data/snapshots/` directory exists (which is the case in a standalone tool repo), calling `generate_fluxnet_citations()` without an explicit `snapshot_path` will stop immediately. The fix is simple: default `snapshot_path = NULL` and resolve it inside the function body, with a clear error if neither a path nor a resolvable layout is available.

**Risk 6: `mandated_bib()` network routing uses `product_source_network` codes, not hub names.**  
`mandated_bib(networks)` checks for `"TERN"` and `"JPF"` in the `product_source_network` column. SAEON sites have `product_source_network == "SAEON"` (matched by the else-branch in the parser) but no conditional mandated reference — this is intentional per the current acknowledgments section. No mode risk here, but note that if a new hub appears in the snapshot with a new network code, it will silently fall to the SAEON parser and produce no mandated reference. This is worth a note in `KNOWN_NETWORKS`.

---

### Portability refactor: generate_fluxnet_citations.R

**Commit:** 7f31497 — `Refactor generate_fluxnet_citations.R for portability; add snapshot-only warnings.`

**Changes made:**

1. **Removed paper-repo dependency block** (7 lines: dotenv load, `source("R/pipeline_config.R")`, `source("R/utils.R")`, `check_pipeline_config()`). Replaced with a self-contained file header listing requirements (R 4+, dplyr/readr/stringr, a snapshot CSV), limitations (no BIF verification, no data download, no credential checks), and a one-line example invocation.

2. **`find_latest_snapshot()` now accepts `snapshot_dir` argument** defaulting to `"data/snapshots"` instead of reading `FLUXNET_DATA_ROOT`. No longer depends on any external variable.

3. **`snapshot_path` default changed to `NULL`**, resolved inside the function body. If NULL at call time, `find_latest_snapshot()` is called; if that also fails, a clear error explains how to supply an explicit path. A `file.exists()` check added to produce an actionable error on bad paths.

4. **product_id empty-guard softened** from `stop()` to a per-site review flag (`"NOTICE: site X has no product_id — entry generated without persistent identifier"`), with `pid` set to `NA_character_` for downstream handling. Pattern-format checks now guarded with `!is.na(pid)`. No behavioral change for the current 759-site snapshot (all sites have non-empty product_ids), but future TERN or other hub data without identifiers will no longer abort the run.

5. **Notice block added to every `.bib` file** (7 comment lines at the top): states that citations are snapshot-derived, not BIF-verified, and directs users to review `_review_flags.md`.

6. **Data source notice section added to every `_review_flags.md`** (`## Data source notice`): same message in prose form, positioned before all per-site flags.

**Verification diff (8-site test across all parser families: AMF, EUF/ICOS, JPF, TERN, SAEON; including AU-Cow FLUXNEXT and US-Hsm Kyle_Delwiche preserved-typo sites):**

- `.bib`: 8 lines added at top (notice block + "generated by" path trimmed from `scripts/generate_fluxnet_citations.R` to `generate_fluxnet_citations.R`). All entries, mandated refs, and metadata identical.
- `_acknowledgments.md`: byte-identical — zero diff.
- `_review_flags.md`: 8 lines added (data source notice section). All flags and mandatory-ref section identical.

No parser functions, BibTeX helpers, cite-key builder, mandated-references logic, acknowledgments builder, or verbatim-preservation behavior were changed.

**Script is now ready to copy to a standalone `fluxnet-citations` repo** — no paper-repo files, env vars, or pipeline infrastructure required.
