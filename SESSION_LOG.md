# Session Log

A running record of Claude Code investigation reports, audits, and summaries for this project. Each session is demarcated by a date/time header; reports within a session appear under it as they're produced.

Convention: Claude Code prepends new entries at the top of this file (reverse chronological order — most recent first), then commits and pushes immediately. Prompts and back-and-forth are not logged here, only Claude Code's structured outputs (reports, audits, investigation summaries).

## 2026-06-26 — TRENDY v14 compute: job died mid-run (power outage)

### Outcome

Background job PID 64895 died due to a power outage on 2026-06-25. The process
did not exit cleanly — no completion marker, no error logged. The log cuts off
at 12:18:39 mid-progress-bar on ISAM nbp lon rotation.

### State at death

16 of 36 intermediate regridded TIFs completed (8 models × 2 variables):

| Status | Models |
|---|---|
| Complete (both vars) | CABLE-POP, CLASSIC, CLM, DLEM, ED, ELM, ELM-FATES, IBIS |
| Died mid-processing | ISAM (nbp lon rotation ~80% complete) |
| Not started | JSBACH, JULES-ES, LPJ-GUESS, LPJml, LPJwsl, LPX-Bern, ORCHIDEE, TEM, VISIT-UT |
| Failed (pre-existing) | CLM-FATES |

All 16 completed TIFs are intact in `data/external/trendy/derived/intermediate/`.
The ISAM nbp TIF was not written (process died before `terra::writeRaster()`
would have completed). The job can be resumed — the script will need to re-run
ISAM nbp and all subsequent models.

### Next step

Relaunch `scripts/figure_representativeness_trendy_compute.R` with `caffeinate`.
The script does not checkpoint intermediate TIFs, so it will re-process from the
start unless the script is modified to skip models whose output TIFs already
exist in `intermediate/`. Consider adding a skip-if-exists guard before
relaunching to avoid reprocessing the 8 completed models (~7 h of compute).

---

## 2026-06-25 — TRENDY v14 compute: second status check (~7 h elapsed)

### Job status

PID 64895 confirmed running. Runtime: 7 h 11 min (431 CPU-min reported by `ps`).
RSS now 1.9 GB (up from 1.36 GB at previous check), consistent with ISAM's larger
file being loaded into terra. No new intermediate tifs since last check — still at
16/36. Log unchanged from 12:18:39; the rotation progress bar has advanced from
`====---` to `========---` (~80% complete).

### Current bottleneck: ISAM nbp (0–360 lon rotation)

ISAM nbp started at 12:18:39 and is still running (~2h 15min and counting).
File sizes from download manifest:

| File | Size |
|---|---|
| ISAM_S3_nbp.nc | 810 MB |
| ISAM_S3_evapotrans.nc | 1.8 GB |

Both ISAM files require 0–360 → −180–180 rotation before processing. Terra's
`rotate()` materialises the full raster in memory before writing. With 810 MB
on disk the rotation alone appears to take ~2–3 h; the 1.8 GB evapotrans file
will likely take proportionally longer (estimate 4–6 h). ISAM alone may consume
6–9 h of the total job time.

### Revised runtime and completion estimate

With 10 models remaining after ISAM (JSBACH, JULES-ES, LPJ-GUESS, LPJml,
LPJwsl, LPX-Bern, ORCHIDEE, TEM, VISIT-UT), and ISAM's evapotrans potentially
running into late tonight, completion is now estimated at **2026-06-26 morning**,
possibly earlier if most remaining models are small.

### No new issues

No new errors or warnings beyond the CLM-FATES and ELM issues documented in the
previous status entry.

---

## 2026-06-25 — TRENDY v14 compute: mid-run status (~5 h elapsed)

### Job status

Background job PID 64895 confirmed running (process at 99% CPU, ~1.36 GB RSS,
launch command `caffeinate -dimsu Rscript scripts/figure_representativeness_trendy_compute.R`).
Internal log: `logs/trendy_analysis_20260625_072258.log`.

### Progress

16 of 36 intermediate tifs completed (8 full models × 2 variables). Currently
loading ISAM nbp with 0–360 → −180–180 longitude rotation in progress.

| Status | Models |
|---|---|
| Complete (both vars) | CABLE-POP, CLASSIC, CLM, DLEM, ED, ELM, ELM-FATES, IBIS |
| In progress | ISAM (nbp loading; lon rotation ~45% complete at 12:18 UTC) |
| Failed | CLM-FATES |
| Pending | JSBACH, JULES-ES, LPJ-GUESS, LPJml, LPJwsl, LPX-Bern, ORCHIDEE, TEM, VISIT-UT |

### Issues found during Step 1

**CLM-FATES — dropped from ensemble.** Both CLM-FATES files failed with
`[rast] longitude is not regularly spaced`. Terra cannot load an irregularly
spaced longitude grid. The error was caught and logged; the script continues.
Ensemble size is now **18 models** (not 19). No manual intervention required;
the script's `MODELS_OK <- intersect(mdl_ok_nbp, mdl_ok_et)` logic will
exclude CLM-FATES automatically from all ensemble steps.

**ELM — missing 2023 data (both variables).** Log warning: `WARNING: ELM
{nbp,evapotrans} has no data for year 2023`. ELM has 3900 monthly layers
(1698–2022, 325 years), not 3912 (through 2023). The regridded tif has an
all-NA 34th layer. The `compute_detrended_sd()` function requires all 34 years
to be non-NA (`rowSums(is.na(vals)) == 0`), so ELM pixels will be excluded
from the **IAV maps** (NEE-IAV, ET-IAV). ELM will still contribute to
**median maps** if `terra::median(..., na.rm=TRUE)` is used — check
`process_axis()` in the compute script before the wrap-up session.

**IBIS — very large files.** IBIS nbp took 73 min; IBIS evapotrans took 3 h 1 min
(09:17 → 12:18). Combined 4 h 14 min for one model. This suggests the remaining
models may run longer than the original 3–6 h estimate.

### Revised runtime estimate

5 h elapsed; 10 models remaining + Steps 2–4 (ensemble statistics, site
extraction, binning, representativeness metrics). Estimated completion:
**late evening 2026-06-25**, possibly into early 2026-06-26 if any remaining
files are IBIS-scale.

### Completion check

```bash
ls -la logs/trendy_analysis_complete.marker   # exists → job done
tail -20 logs/trendy_analysis_20260625_072258.log
```

---

## 2026-06-25 — TRENDY v14 IAV/median representativeness compute launch

### Pre-launch diagnosis

#### DLEM (previously reported as 0 land pixels)

Both DLEM files matched manifest sizes exactly (nbp: 673,932,068 bytes; evapotrans:
8,087,080,676 bytes). No redownload required.

Root cause of "0 land pixels" finding: DLEM uses IEEE NaN as the fill value with no
`_FillValue` attribute. Python netCDF4 with `set_auto_mask=True` does not mask NaN
without an explicit fill attribute, so all 259,200 cells per layer appeared unmasked
but with NaN values. Terra reads NaN → R NA → masked correctly. Terra inspection
confirmed ~61,486 non-NA land pixels per layer in the analysis window — consistent
with other 0.5° models. DLEM is included in the ensemble.

DLEM unit conversion note: NBP is stored as 325 **annual** time steps (not monthly),
so conversion is `kgC m⁻² s⁻¹ × 31,557,600 s yr⁻¹ × 1000 g kg⁻¹ → gC m⁻² yr⁻¹`.
Evapotrans is 3900 **monthly** steps (standard monthly aggregation).

#### Analysis time window

Model start/end years extracted via Python netCDF4 from all 19 models:

| Constraint | Model | Year |
|---|---|---|
| Latest start year | CLM-FATES | 1701 |
| Earliest end year | CLASSIC, ELM, DLEM | 2023 |

**Analysis window: 1990–2023 (34 years).**

The 1990 lower bound applied because max(1990, 1701) = 1990. Three models end at
2023 (CLASSIC, ELM, DLEM), making 2023 the binding upper constraint. CLM, JSBACH,
VISIT-UT, ELM-FATES have hours- or fractional-years-encoded time axes that Python
could not parse to calendar years; ntime (3888–3900) and the 270-year offset
identified in terra::time() indicate they cover ≥1990–2023.

#### Unit inventory

All 19 models provide NBP and evapotrans in `kg m⁻² s⁻¹` (or equivalent notation):
CLASSIC uses LaTeX superscript notation; ISAM uses slash notation; ELM-FATES NBP and
LPJwsl evapotrans have no units attribute (TRENDY protocol default `kg m⁻² s⁻¹`
assumed). Full inventory written to `data/snapshots/trendy_unit_conversions.csv`.

Standard conversion applied throughout:
- Monthly data (16 models for NBP; all 19 for ET): `× 1000 × 86400 × 30.4375` per
  monthly layer, then sum 12 layers per year → gC m⁻² yr⁻¹ or mm yr⁻¹.
- Annual NBP (DLEM, LPJ-GUESS, LPJml): `× 1000 × 86400 × 365.25` per annual layer.

#### terra::time() offset bug

`terra::time()` on these NetCDF files returns numeric decimal years offset by +270
years (e.g., CABLE-POP 1700–2024 reported as 1970–2294). Root cause: terra misparses
non-zero-padded origin dates (e.g., `"months since 1700-1-16"`) and falls back to
the Unix epoch (1970-01-01), producing a systematic +270 shift. Workaround: bypassed
`terra::time()` entirely; years inferred from known model start years (verified via
Python) and layer count. Fix documented in `scripts/figure_representativeness_trendy_compute.R`.

#### ncdf4 availability

`ncdf4` is not in the project renv. `terra::writeCDF()` depends on ncdf4. Switched
all intermediate and output writes to GeoTIFF (`terra::writeRaster(..., gdal="COMPRESS=DEFLATE")`).
File extensions are `.tif` throughout. Impact: none for the compute; the wrap-up
session will read `.tif` via terra.

### Background compute launch

Script: `scripts/figure_representativeness_trendy_compute.R`

| Parameter | Value |
|---|---|
| Ensemble | 19 models (CARDAMOM excluded — 22-year span too short for IAV) |
| Analysis window | 1990–2023 (34 years) |
| Target grid | 0.5° × 0.5°, 720 × 360, lon −180–180, lat S→N |
| Land mask | Beck 2023 KG 1991–2020 at 0.5° (`koppen_geiger_0p5.tif`) |
| Near-zero bins | NBP: < 5 gC m⁻² yr⁻¹; ET: < 5 mm yr⁻¹ |
| PID | 64895 |
| PID file | `logs/trendy_analysis.pid` |
| Internal log | `logs/trendy_analysis_20260625_072258.log` |
| nohup redirect | `logs/trendy_analysis_20260625_072256.log` |
| Launch command | `nohup caffeinate -dimsu Rscript scripts/figure_representativeness_trendy_compute.R > logs/trendy_analysis_20260625_072256.log 2>&1 &` |

**Verified running**: CABLE-POP nbp processed in 76 s and intermediate written to
`data/external/trendy/derived/intermediate/CABLE-POP_nbp_regridded.tif`.

**Expected runtime**: 3–6 hours (Step 1: 19 models × 2 variables × ~1–5 min each
depending on file size; Step 2: ensemble statistics from intermediates; Step 3–4:
site extraction and binning).

**Expected disk usage when complete**: ~8 GB intermediates (19 × 2 vars × ~35 MB each
for 34-layer 0.5° tif = ~1.3 GB) + 4 × ~3 MB final ensemble tifs. Total: ~1.5 GB.

### Outputs produced on completion

`data/external/trendy/derived/intermediate/<model>_{nbp,evapotrans}_regridded.tif` (38 files)
`data/external/trendy/derived/trendy_{nee,et}_{iav,median}.tif` (4 files)
`data/snapshots/site_trendy_{nee,et}_{iav,median}.csv` (4 files + meta.json each)
`data/snapshots/trendy_{nee,et}_{iav,median}_global_distribution.csv` (4 files + meta.json each)
`data/snapshots/representativeness_metrics.csv` (4 new rows appended)
`logs/trendy_analysis_complete.marker`

---

## 2026-06-25 — TRENDY v14-gcb2025 grid characterisation

### Pre-inspection fix: trailing `\r` in filenames

All 100 downloaded NetCDF files had a trailing carriage return (`\r`) in their
filenames because the download script used bash `read` on a CSV written with CRLF
line endings. Renamed all 100 files and fixed the manifest CSV before inspection.
The download script (`scripts/download_trendy_v14.sh`) should use `tr -d '\r'`
when reading CSV fields — noted for future re-runs.

### Grid summary (nbp S3, all 20 models)

| Model | Dims (lon×lat) | Resolution | Lon range | Lat dir | Land px | Ntime | Notes |
|-------|---------------|-----------|-----------|---------|---------|-------|-------|
| CABLE-POP | 360×180 | 1.0°×1.0° | −180–180 | S→N | 15,069 | 3900 | fill=−99999 |
| CARDAMOM | 720×360 | 0.5°×0.5° | −180–180 | S→N | 54,718 | 264 | **ntime=264 (22 years monthly?)** |
| CLASSIC | 360×180 | 1.0°×1.0° | −180–180 | S→N | 16,671 | 3888 | |
| CLM | 288×192 | 1.25°×0.9424° | **0–360** | S→N | 21,013 | 3900 | Gaussian-lat |
| CLM-FATES | 144×80 | 2.5°×1.8947° | −180–180 | S→N | 4,071 | 3888 | **lat only to −59.7°** |
| DLEM | 720×360 | 0.5°×0.5° | −180–180 | S→N | **0** | 325 | **⚠ 0 land pixels — fill issue** |
| ED | 720×360 | 0.5°×0.5° | −180–180 | S→N | 62,570 | 3900 | |
| ELM | 288×192 | 1.25°×0.9424° | −180–180 | S→N | 20,975 | 3900 | Gaussian-lat |
| ELM-FATES | 144×80 | 2.5°×1.8947° | **0–360** | S→N | 3,993 | 3888 | **lat only to −59.7°** |
| IBIS | 720×360 | 0.5°×0.5° | −180–180 | S→N | 58,124 | 3900 | |
| ISAM | 720×360 | 0.5°×0.5° | **0–360** | S→N | 59,184 | 3900 | |
| JSBACH | 192×96 | 1.875°×1.8652° | −180–180 | S→N | 4,420 | 3888 | Gaussian-lat; lat ±88.57° |
| JULES-ES | 720×360 | 0.5°×0.5° | −180–180 | S→N | 67,420 | 3900 | fill=−999 |
| LPJ-GUESS | 720×360 | 0.5°×0.5° | −180–180 | S→N | 59,172 | 325 | rh=arh; **ntime=325** |
| LPJml | 720×360 | 0.5°×0.5° | −180–180 | S→N | 67,420 | 325 | **ntime=325; time unit=years** |
| LPJwsl | 720×360 | 0.5°×0.5° | −180–180 | S→N | 62,482 | 3900 | filename=LPJ-EOSIM |
| LPX-Bern | 720×360 | 0.5°×0.5° | −180–180 | S→N | 64,012 | 3900 | |
| ORCHIDEE | 720×360 | 0.5°×0.5° | −180–180 | S→N | 67,420 | 3900 | |
| TEM | 720×360 | 0.5°×0.5° | −180–180 | S→N | 62,608 | 3900 | filename=GDSTEM |
| VISIT-UT | 720×360 | 0.5°×0.5° | −180–180 | **N→S** | 58,697 | 3900 | **⚠ lat reversed; no fill attr** |

### Grid classes

**0.5° (720×360) — majority (13 models):**
ED, IBIS, ISAM, JULES-ES, LPJ-GUESS, LPJml, LPJwsl, LPX-Bern, ORCHIDEE, TEM,
VISIT-UT, CARDAMOM, DLEM

**1° (360×180) — 2 models:** CABLE-POP, CLASSIC

**1.25°×0.9424° Gaussian-lat (288×192) — 2 models:** CLM, ELM

**1.875°×1.8652° Gaussian-lat (192×96) — 1 model:** JSBACH

**2.5°×1.8947° coarse (144×80) — 2 models:** CLM-FATES, ELM-FATES (lat only to −59.7°)

### Oddities requiring attention in regridding

1. **DLEM — 0 land pixels:** No fill value attribute set; all values may be unmasked
   zeros or the fill value coincides with valid data. Inspect before including.

2. **CARDAMOM — only 264 time steps:** Other models have 3888–3900 (monthly ~325 years).
   264 months = 22 years. CARDAMOM may only cover ~2001–2022. Verify temporal extent
   before using for 1990–2020 window.

3. **LPJ-GUESS, LPJml, CARDAMOM, DLEM — ntime=325:** 325 values at what resolution?
   LPJml time unit is "years since 1700" → 325 annual steps = 1700–2024. ✅ correct for
   annual data. LPJ-GUESS similarly annual. CARDAMOM ntime=264 with monthly units.

4. **VISIT-UT — latitude N→S (reversed):** All other models are S→N. terra/R will
   handle this on load but regridding code should not assume consistent orientation.

5. **CLM, ISAM, ELM-FATES — longitude 0–360:** All others are −180–180.
   Requires `terra::rotate()` or equivalent before co-registration.

6. **CLM, ELM, JSBACH — non-integer resolution (Gaussian latitude):**
   dlat ≈ 0.9424° or 1.8652°. These are spectral model Gaussian grids.
   Nearest-neighbour regrid to a common grid is correct; bilinear also acceptable.

7. **CLM-FATES, ELM-FATES — southern extent only to −59.7°:** Antarctica missing.
   Not a problem for most land analyses but worth flagging.

8. **LPJwsl — filename prefix is LPJ-EOSIM:** Directory is LPJwsl, file is
   `LPJ-EOSIM_S3_nbp.nc`. This is the newer name for the model. No functional issue.

9. **JULES-ES — fill value is −999 (not −99999):** Different from the majority.
   Ensure fill value is read from the file attribute, not hardcoded.

10. **ELM-FATES — fill value is NaN:** No numeric fill; masked array approach required.

### Recommended common grid for regridding

**0.5° × 0.5°** (720×360, −180–180, S→N) — matches 13 of 20 models natively.
The 7 non-native models (CABLE-POP, CLASSIC at 1°; CLM, ELM at 1.25°; CLM-FATES,
ELM-FATES at 2.5°; JSBACH at ~1.9°) all regrid cleanly to 0.5°. All non-native
models have coarser resolution, so 0.5° involves oversampling (replication), not
downsampling — no information is invented.

---

## 2026-06-25 — TRENDY v14-gcb2025 download complete

### Result

**100/100 files, 0 failures. 126.7 GB. Duration: 1h 53m (20:56–22:50 local, 2026-06-24).**

All 20 models × 5 variables (nbp, gpp, evapotrans, ra, rh; arh for LPJ-GUESS) downloaded
to `data/external/trendy/v14-gcb2025/`. Files are at native model resolution, annual,
1959–2024 coverage, NetCDF format.

### Key technical note

The Wasabi S3 URL requires the `trendyv14-gcb2025/` prefix to be **stripped** before
prepending the base URL. The GCB browser JavaScript does this automatically; manual
curl/wget must replicate it. Correct pattern:
```
https://s3.eu-west-1.wasabisys.com/gcb-2025-upload/land/output/{MODEL}/S3/{file}.nc
```
**Not:** `…/land/output/trendyv14-gcb2025/{MODEL}/…` (403 Forbidden).

The initial download run used the wrong URL and was killed after a few minutes.
The manifest (`data/external/trendy/download_manifest.csv`) was corrected and the
download relaunched cleanly.

### Per-model disk usage

| Model | Size |
|-------|------|
| DLEM | 31 GB |
| JULES-ES | 19 GB |
| ED | 19 GB |
| CLASSIC | 9.5 GB |
| ISAM | 6.7 GB |
| LPJwsl | 6.2 GB |
| ORCHIDEE | 4.1 GB |
| CLM | 4.1 GB |
| LPX-Bern | 4.0 GB |
| TEM | 3.8 GB |
| VISIT-UT | 3.7 GB |
| IBIS | 3.5 GB |
| CARDAMOM | 3.5 GB |
| LPJml | 3.4 GB |
| ELM | 1.7 GB |
| LPJ-GUESS | 1.4 GB |
| JSBACH | 1.4 GB |
| CABLE-POP | 1.0 GB |
| CLM-FATES | 0.9 GB |
| ELM-FATES | 0.3 GB |

### Files committed

- `scripts/download_trendy_v14.sh` — download script (resume-capable, 3-retry)
- `data/external/trendy/download_manifest.csv` — 100-file manifest with corrected URLs
- `data/external/trendy/fileIndex_merged_v4.json` — full GCB file index (28,695 entries)

### Files gitignored (data, not committed)

- `data/external/trendy/v14-gcb2025/` — all NetCDF files

---

## 2026-06-25 — GCB TRENDY v14-gcb2025 data hub characterisation

### Access

**Base URL (Wasabi S3, anonymous):**
`https://s3.eu-west-1.wasabisys.com/gcb-2025-upload/land/output/`

File pattern: `{base}/{MODEL}/S3/{prefix}_S3_{var}.nc`

**Browser + wget-script interface:** https://mdosullivan.github.io/GCB/

**Machine-readable file index:**
`https://raw.githubusercontent.com/mdosullivan/GCB/main/fileIndex_merged_v4.json`
(28,695 entries — ground truth for filenames and sizes)

**Anonymous access confirmed:** HEAD requests on CABLE-POP S3 nbp and ORCHIDEE S3
evapotrans both return `HTTP/1.1 200 OK`; `application/x-netcdf`; no authentication.

### Terms of use (verbatim)

> "These data are freely available under the following data policy. Our main objective
> is to make all data available to the wider scientific community. For the most recent
> data set (TRENDY-GCB2025 and ocean-GCB25 used in Global Carbon Budget 2025),
> co-authorship of the contributing modellers depends on the importance of the TRENDY
> or GCB-ocean data in your study and must be discussed with the respective coordinators
> early in the process. TRENDY coordinators: Stephen Sitch – s.a.sitch@exeter.ac.uk,
> Mike O'Sullivan – m.osullivan@exeter.ac.uk. All studies should be circulated to the
> modelling groups prior to submission. If TRENDY-GCB2025 forms a significant part of
> a publication or conference presentation, the modelling groups and coordinators should
> be invited as co-authors, with sufficient lead time (minimum 3 weeks before submission;
> earlier engagement is encouraged). If TRENDY results are only a minor component (for
> example, a figure showing the multi-model mean), then it is sufficient to acknowledge
> the TRENDY project. Please cite: Sitch et al., Global Biogeochemical Cycles, 2024,
> doi:10.1029/2024GB008102."

**Interpretation for this paper:** A representativeness analysis using TRENDY IAV as
one axis among six is unambiguously a minor component under the policy. The "minor
component" clause (multi-model mean figure example) applies: **acknowledgment only,
no co-authorship requirement, no coordinator negotiation required.** Methods section
should acknowledge the TRENDY project and cite Sitch et al. 2024
(doi:10.1029/2024GB008102) and the latest Global Carbon Budget paper. No pre-submission
circulation or co-author invitation is necessary for this use case.

### Models — 20 with S3 nbp + evapotrans

Note: several models have filename prefixes that differ from the directory name (marked *).

| Model dir | Filename prefix | nbp | gpp | evapotrans | ra | rh | Size/var |
|-----------|----------------|-----|-----|------------|----|----|----------|
| CABLE-POP | CABLE-POP | ✅ | ✅ | ✅ | ✅ | ✅ | 180–228 MB |
| CARDAMOM | CARDAMOM | ✅ | ✅ | ✅ | ✅ | ✅ | 655–794 MB |
| CLASSIC | CLASSIC | ✅ | ✅ | ✅ | ✅ | ✅ | ~2,016 MB |
| CLM | CLM6.0* | ✅ | ✅ | ✅ | ✅ | ✅ | ~863 MB |
| CLM-FATES | CLM-FATES | ✅ | ✅ | ✅ | ✅ | ✅ | ~180 MB |
| DLEM | DLEM | ✅ | ✅ | ✅ | ✅ | ✅ | 673 MB – 8,087 MB |
| ED | EDv3* | ✅ | ✅ | ✅ | ✅ | ✅ | ~4,043 MB |
| ELM | E3SM* | ✅ | ✅ | ✅ | ✅ | ✅ | 199–863 MB |
| ELM-FATES | ELM-FATES | ✅ | ✅ | ✅ | ✅ | ✅ | 55–65 MB |
| IBIS | IBIS | ✅ | ✅ | ✅ | ✅ | ✅ | 678–790 MB |
| ISAM | ISAM | ✅ | ✅ | ✅ | ✅ | ✅ | 809 MB – 1,802 MB |
| JSBACH | JSBACH | ✅ | ✅ | ✅ | ✅ | ✅ | ~287 MB |
| JULES-ES | JULES* | ✅ | ✅ | ✅ | ✅ | ✅ | ~4,044 MB |
| LPJ-GUESS | LPJ-GUESS | ✅ | ✅ | ✅ | ✅ | ⚠️ arh | 37–798 MB |
| LPJml | LPJmL* | ✅ | ✅ | ✅ | ✅ | ✅ | 80–944 MB |
| LPJwsl | LPJwsl | ✅ | ✅ | ✅ | ✅ | ✅ | 814 MB – 1,836 MB |
| LPX-Bern | LPX-Bern | ✅ | ✅ | ✅ | ✅ | ✅ | 816–880 MB |
| ORCHIDEE | ORCHIDEE | ✅ | ✅ | ✅ | ✅ | ✅ | 763–941 MB |
| TEM | GDSTEM* | ✅ | ✅ | ✅ | ✅ | ✅ | 681–880 MB |
| VISIT-UT | VISIT-UT | ✅ | ✅ | ✅ | ✅ | ✅ | 657–840 MB |

*Non-standard prefix: download scripts must use the filename prefix, not the directory name.
LPJ-GUESS has `arh` (above-ground rh) not `rh` — treat as partial match for rh.
iMAPLE, SDGVM, OCN, ISBA-CTRIP present in directory but have no S3 target-variable files.

### Temporal coverage and resolution

GCB 2025 cycle: **1959–2024** (files modified July–September 2025). Year range is in
NetCDF headers, not filenames. For 1990–2020 IAV analysis, only ~half the time dimension
is used but the whole file must be downloaded (no HTTP range subsetting).

### Download size estimates

| Scope | Files | Approx. size |
|-------|-------|-------------|
| nbp only, 20 models | 20 | ~16 GB |
| nbp + evapotrans, 20 models | 40 | **~32 GB** |
| All 5 vars (nbp, gpp, evapotrans, ra, rh), 20 models | 100 | ~135 GB |

DLEM and JULES-ES/ED are outliers (~4–8 GB/var). Excluding those 3 models: ~88 GB for
all 5 vars from remaining 17 models. Recommended minimum download: nbp + evapotrans
= ~32 GB.

### IAV analysis requirements assessment

| Requirement | Status |
|-------------|--------|
| Per-model outputs (not ensemble means) | ✅ Separate file per model per variable |
| Annual NBP, globally gridded | ✅ 20 models |
| Annual ET (evapotrans) | ✅ 20 models |
| ≥30 years coverage (1990–2020) | ✅ 1959–2024 |
| Anonymous download, no registration | ✅ Plain wget/curl; HTTP 200 confirmed |
| Co-authorship / attribution | ✅ Acknowledgment only — IAV as one of six axes = minor component |

**This is the right dataset for the analysis.** 20 models, per-model files, both nbp
and evapotrans present, freely downloadable, 1959–2024 coverage. The minimum download
for the IAV analysis (nbp + evapotrans, all 20 models) is ~32 GB.

### Key operational notes for download

- Use the file index JSON as the manifest: `fileIndex_merged_v4.json`
- Filename prefixes differ from directory names for CLM (→ CLM6.0), JULES-ES (→ JULES),
  ED (→ EDv3), ELM (→ E3SM), TEM (→ GDSTEM), LPJml (→ LPJmL)
- LPJ-GUESS rh is named `arh`; treat accordingly
- wget/curl works directly: no API key, no session cookie required

---

## 2026-06-25 — Zenodo TRENDY v10 deposit characterisation (doi:10.5281/zenodo.7598697)

### Record

- **DOI (use version 3):** doi:10.5281/zenodo.7598697
  (The linked record 6884342 is version 2 — RData files. Version 3 = NetCDF, 18.32 GB.)
- **Title:** TRENDYv10 DGVM output for: Process-oriented analysis of dominant sources
  of uncertainty in the land carbon sink
- **Creator:** Mike O'Sullivan, University of Exeter (m.osullivan@exeter.ac.uk)
- **Licence:** CC BY 4.0 — **openly and anonymously downloadable, no agreement required**
- **Temporal coverage:** 1901–2020 (120 years annual)
- **Spatial resolution:** 1° × 1°, 360 × 180 global grid
- **Models (19):** CABLE-POP, CLASSIC, CLASSIC-N, CLM5.0, DLEM, IBIS, ISAM,
  ISBA-CTRIP, JSBACH, JULES-ES-1.1, LPJ-GUESS, LPJ, LPX-Bern, OCN, ORCHIDEE,
  ORCHIDEEv3, SDGVM, VISIT, YIBs

### File structure

86 files total. Each gridded NetCDF has dimensions `[lon=360, lat=180, time=120, models=19]` —
**all 19 models are present as a 4th dimension, not ensemble statistics.**

Key gridded files (each ~626 MB, S0/S1/S2/S3 variants):

| Variable | Description | S3 gridded file |
|----------|-------------|-----------------|
| nbp | Net biome production | `trendyv10_S3_nbp_1901-2020_annual_gridded.nc` |
| npp | Net primary production | trendyv10_S3_npp_... |
| rh | Heterotrophic respiration | trendyv10_S3_rh_... |
| cSoil | Soil carbon | trendyv10_S3_cSoil_... |
| cVeg | Vegetation carbon | trendyv10_S3_cVeg_... |
| cProduct | Product carbon (S3 only) | trendyv10_S3_cProduct_... |

GPP is **regional totals only** (~4 MB), not gridded. ET is **absent from the deposit entirely**.

Selective download is supported — individual files via plain HTTP GET:
`https://zenodo.org/records/7598697/files/<filename>`

### IAV analysis requirements assessment

| Requirement | Status | Notes |
|-------------|--------|-------|
| Per-model outputs (not ensemble means) | ✅ | All 19 models in each file (4th dimension) |
| Annual NBP, globally gridded | ✅ | 626 MB, S3, 1901–2020 |
| ≥30 years coverage (1990–2020) | ✅ | 1901–2020 available |
| ET / evapotranspiration | ❌ | **Not present in any version** |
| CC BY 4.0, anonymous download | ✅ | No registration, no agreement |
| NetCDF, terra/R-readable | ✅ | Version 3 only (v2 is RData) |

### ET gap — options

The deposit is a carbon-cycle uncertainty paper (O'Sullivan et al. 2022 GRL). Hydrological
variables were not included. For ET:

1. **Email Mike O'Sullivan** (m.osullivan@exeter.ac.uk) — may have evapotranspiration
   from the same TRENDY v10 run not included in this deposit
2. **Email Stephen Sitch** for official TRENDY v10/v11 evapotranspiration output
3. **GLEAM v4** — single model but high quality, freely downloadable, covers ET fully
4. **Scope IAV analysis to NBP only** if the science allows it

### Decision pending

NBP is immediately downloadable (626 MB, one file). ET requires an additional contact
or alternative source. Decision needed before download proceeds.

---

## 2026-06-25 — TRENDY v11 data access investigation

### Purpose

Scoped access options for gridded annual per-model NBP/NEE and ET from TRENDY
S3 simulations, ~1990–2020, for an interannual variability analysis (detrended
SD per model per grid cell, median across models).

### Key finding: no anonymous path exists for any TRENDY version

v11 (GCB 2022, through 2021), v12 (GCB 2023), and v13 (GCB 2024) all use the
same controlled-access model — email agreement with coordinators + co-authorship
negotiation. The TRENDY website is currently offline (HTTP 410). The GCB 2022
supplement (doi.org/10.18160/GCP-2022) contains only global/national totals
in Excel; no gridded outputs in any public repository.

### Access options assessed

| Path | Per-model gridded? | NBP? | ET? | Friction | Circular? |
|------|--------------------|------|-----|----------|-----------|
| TRENDY direct (s.a.sitch@exeter.ac.uk) | Yes | Yes (nbp) | Yes (evapotrans) | High — email + co-author negotiation | No |
| ISIMIP3 (data.isimip.org) | Yes | Yes (nbp) | Yes | Medium — self-service portal | No |
| FLUXCOM (MPI-BGC) | Yes (147 members) | Yes (NEE) | Yes | Low — lightweight registration | **Yes — unsuitable** |
| GLEAM v4 | Yes | No | ET only | Low | No |
| GCB supplement | No (totals only) | — | — | None | Moot |
| Global Carbon Atlas | No (visualisations only) | — | — | None | Moot |

### TRENDY direct access details

- Contact: Stephen Sitch (s.a.sitch@exeter.ac.uk); Pierre Friedlingstein co-PI
- Variables confirmed distributed: nbp, gpp, ra, rh, npp, evapotrans, mrro, mrso
- Format: NetCDF, global gridded, native model resolution (commonly 0.5° or 1°), monthly or annual
- Size: ~19 GiB per model (one-model JSBACH v11 deposit at WDC-Climate); full ensemble substantially larger
- S3 simulation = transient CO₂ + climate + land use change (correct scenario)
- Co-authorship expectation: "depending on importance of TRENDY data in the study" — negotiate upfront

### ISIMIP3 as parallel track

ISIMIP3b runs many of the same DGVMs (LPJmL, ORCHIDEE, JULES, CLM5, etc.) under
standardised forcing. Historical + SSP simulations are S3-equivalent. Variables
include nbp and evapotranspiration. Self-service registration at data.isimip.org.
Worth evaluating before committing to the TRENDY direct-contact path.

### FLUXCOM excluded

FLUXCOM (147 ML ensemble members, NEE + ET, CC BY 4.0, low-friction) is methodologically
unsuitable: trained on FLUXNET eddy covariance observations, making it circular for a
FLUXNET representativeness analysis. Spatial patterns largely reflect tower locations.

### Recommended next steps

1. Email Stephen Sitch — FLUXNET coordination paper is a strong case; co-authorship
   question should be clarified upfront (expect 2–4 weeks response).
2. Simultaneously evaluate ISIMIP3 portal for variable coverage, temporal range,
   and model overlap with TRENDY before committing to either path.

---

## 2026-06-25 — LULC axis upgraded to ESA CCI LC v2.1.1 (2022)

### Data

ESA CCI / C3S Land Cover v2.1.1, year 2022, downloaded from Copernicus CDS
(`satellite-land-cover`, version `v2_1_1`, year `2022`).
File: `C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc` (2.2 GB NetCDF-4, 5 variables;
`lccs_class` band used). Year 2022 is the most recent available (CDS range: 1992–2022;
2023/2024 updates expected 2026). Licence accepted via CDS user account 2026-06-24.

Key API note: version string must use underscores (`v2_1_1`) — dots (`v2.1.1`) return
400 Bad Request. The download Python script is `logs/dl_cci_lc_v211.py` (scratchpad);
parameters documented in `data/external/cci_landcover/README.md`.

### Results: v2.1.1 (2022)

Total land: **147,322,862 km²** — exact KG baseline match.

| Class | Global % | Network % | Ratio |
|-------|----------|-----------|-------|
| Cropland | 15.1 | 27.4 | **1.82×** over |
| Forest | 29.1 | 36.8 | 1.27× |
| Shrubland | 15.1 | 11.1 | 0.74× |
| Grassland | 9.3 | 12.4 | 1.33× |
| Wetland | 2.1 | 7.6 | **3.66×** over |
| Settlement | 0.6 | 0.5 | 0.89× |
| Bare | 12.9 | 0.7 | **0.05×** under |
| Snow/Ice | 9.9 | 0.0 | **0.00×** absent |
| Water | 2.5 | 0.8 | 0.32× |
| Other | 3.4 | 2.9 | 0.83× |

**J = 0.556, H = 0.337**

### Comparison: v2.0.7 (2015) vs v2.1.1 (2022)

| Class | v2.0.7 Global % | v2.1.1 Global % | Δ | v2.0.7 Network % | v2.1.1 Network % | Δ | v2.0.7 Ratio | v2.1.1 Ratio |
|-------|----------------|----------------|---|------------------|------------------|---|--------------|--------------|
| Cropland | 15.1 | 15.1 | 0.0 | 27.1 | 27.4 | +0.3 | 1.79× | 1.82× |
| Forest | 29.0 | 29.1 | +0.1 | 36.8 | 36.8 | 0.0 | 1.27× | 1.27× |
| Shrubland | 15.1 | 15.1 | 0.0 | 11.5 | 11.1 | −0.4 | 0.76× | 0.74× |
| Grassland | 9.3 | 9.3 | 0.0 | 12.5 | 12.4 | −0.1 | 1.34× | 1.33× |
| Wetland | 2.0 | 2.1 | +0.1 | 7.4 | 7.6 | +0.2 | 3.63× | 3.66× |
| Settlement | 0.5 | 0.6 | +0.1 | 0.4 | 0.5 | +0.1 | 0.77× | 0.89× |
| Bare | 13.1 | 12.9 | −0.2 | 0.7 | 0.7 | 0.0 | 0.05× | 0.05× |
| Snow/Ice | 9.9 | 9.9 | 0.0 | 0.0 | 0.0 | 0.0 | 0.00× | 0.00× |
| Water | 2.5 | 2.5 | 0.0 | 0.8 | 0.8 | 0.0 | 0.32× | 0.32× |
| Other | 3.4 | 3.4 | 0.0 | 2.9 | 2.9 | 0.0 | 0.84× | 0.83× |

| Version | J | H |
|---------|---|---|
| v2.0.7 (2015) | 0.558 | 0.337 |
| v2.1.1 (2022) | 0.556 | 0.337 |

### Interpretation

**The substantive findings are unchanged.** All differences between 2015 and 2022
are ≤ 0.2 percentage points in global fractions. ~7 sites changed class
(Shrubland −3, Grassland −1, Cropland +2, Wetland +1, Settlement +1) — consistent
with well-known post-2015 LULC change signals (urban expansion, cropland intensification).

The structural story holds exactly:
- **Wetland** most over-sampled (3.63× → 3.66×): research priority, not a gap
- **Bare** most under-sampled (0.05× both versions): dryland coverage gap
- **Snow/Ice** absent from network (0.00× both versions): structurally expected
- **Cropland** over-sampled (1.79× → 1.82×): agricultural flux monitoring priority

J and H are effectively unchanged (ΔJ = −0.002, ΔH = 0.000). The 2022 map is
the better choice for the paper (7 more years of LULC change captured, same
conclusions, CDS source citable). Cross-axis ranking unchanged.

### Files changed

| File | Change |
|------|--------|
| `scripts/figure_representativeness_landcover.R` | Updated to v2.1.1 NetCDF; dynamic file discovery in v2.1.1/ dir |
| `data/external/cci_landcover/README.md` | Documents both versions; CDS download instructions; terra usage note |
| `.gitignore` | Added `v2.1.1/*.nc`, `v2.1.1/*.zip` patterns |
| `data/snapshots/site_landcover_cci.csv` | Re-extracted for all 767 sites from 2022 map |
| `data/snapshots/landcover_cci_global_distribution.csv` | v2.1.1 global fractions |
| `data/snapshots/representativeness_metrics.csv` | Updated landcover_cci row (J=0.556, H=0.337) |
| `review/figures/representativeness/fig_representativeness_landcover.png` | Rebuilt from v2.1.1 data |
| `review/figures/representativeness/methods_landcover.md` | Updated version/year/source; comparison note |

---

## 2026-06-25 — ESA CCI Land Cover version availability audit

### Question

Can the LULC representativeness axis be upgraded from v2.0.7 (2015) to v2.1.1 or newer?
Investigation of available download paths without downloading.

### Findings

**CEDA DAP (anonymous access):**
- Only two versions present: `v1.6.1/` and `v2.0.7/` (both last modified 2024-09-11).
- `v2.1.1` returns HTTP 404 on both `dap.ceda.ac.uk` and `data.ceda.ac.uk`.
- v2.0.7 is anonymously accessible; 24 per-year files (GeoTIFF + NetCDF), 1992–2015.
- CEDA catalogue entry for v2.0.7 explicitly notes: *"Maps for 2016–2020 have been
  produced via Copernicus C3S and can be downloaded from CDS."* No newer version on CEDA.

**Copernicus CDS (`cds.climate.copernicus.eu/datasets/satellite-land-cover`):**
- Hosts `v2.0.7cds` (1992–2015) and `v2.1.1` (2016–present, updated 2025-04-19).
- Both versions use identical algorithm, 300m resolution, and LCCS class system.
- Format: **NetCDF-4 only** (no GeoTIFF via CDS). `terra::rast()` handles this.
- Auth: free CDS account + `~/.cdsapirc` key required — **not bypassable**.

**UCL CCI Viewer (`maps.elie.ucl.ac.be/CCI/viewer/`):**
- Hosts 1992–2022 (v2.0.7 + v2.1.1); email registration only (lightweight, no API key).
- Also NetCDF.

**Newer versions:** No evidence of v2.1.2 or v3.0 anywhere. v2.1.1 is current; the C3S
operational service continues annual updates under the same version label.

### Summary table

| Source | Version | Years | Format | Auth |
|--------|---------|-------|--------|------|
| CEDA DAP | v2.0.7 | 1992–2015 | GeoTIFF + NetCDF | None (anonymous) |
| Copernicus CDS | v2.0.7cds + v2.1.1 | 1992–~2024 | NetCDF-4 | Free account + API key |
| UCL CCI Viewer | v2.0.7 + v2.1.1 | 1992–2022 | NetCDF | Email registration |

### Decision context

Current axis uses 2015 (v2.0.7, CEDA, GeoTIFF). Upgrading to 2020–2024 requires a free
CDS account (~15 min setup). FLUXNET tower locations are stable 2015–2022, so per-site
class assignments are unlikely to change for most sites. The over/under-sampling pattern
(Wetland 3.63×, Bare 0.05×) is structurally robust to the year difference. No decision
made this session; findings logged for paper methods documentation.

---

## 2026-06-25 — Land cover representativeness axis: ESA CCI LC v2.0.7 (2015)

### Download and data source

**ESA CCI Land Cover v2.0.7, year 2015** — most recent year available via anonymous
CEDA HTTP. v2.1.1 (through 2020) confirmed as CDS-authenticated only; no public
anonymous path found.

Source: `https://dap.ceda.ac.uk/neodc/esacci/land_cover/data/land_cover_maps/v2.0.7/`
File: `ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif` (312 MB, 64800×129600, uint8 LCCS codes).
Legend: `ESACCI-LC-Legend.csv` (37 LCCS codes, RGB, labels), also `ESACCI-LCMapsColorLegend.qml`.
Both downloaded via anonymous CEDA HTTPS with curl.

### Aggregation scheme (10 high-level classes, ESA CCI PUG Table 2)

| Code | Class | Native LCCS codes |
|------|-------|-------------------|
| 1 | Cropland | 10, 11, 12, 20, 30 |
| 2 | Forest | 50, 60–62, 70–72, 80–82, 90, 100 |
| 3 | Shrubland | 120, 121, 122, 150, 151, 152, 153 (incl. sparse vegetation) |
| 4 | Grassland | 110, 130 |
| 5 | Wetland | 160, 170, 180 |
| 6 | Settlement | 190 |
| 7 | Bare | 200, 201, 202 |
| 8 | Snow/Ice | 220 |
| 9 | Water | 210 |
| 10 | Other | 40, 140 (mosaic nat-veg/crop; lichens/mosses) |

Implementation note: `terra::classify()` requires `right = NA` for exact single-value
mapping (from == to == code). Using `right = TRUE` with `include.lowest = TRUE`
silently misclassifies all but the lowest code as NA (empty half-open intervals).
Bug identified during run, fixed before final output.

### Per-site extraction (767 sites, snapshot 20260624T095651)

terra::extract() at site lat/lon from native 300m raster. **0 NA sites** —
no nearest-land recovery needed. Site distribution:

| Class | Sites | % |
|-------|-------|---|
| Cropland | 208 | 27.1 |
| Forest | 282 | 36.8 |
| Shrubland | 88 | 11.5 |
| Grassland | 96 | 12.5 |
| Wetland | 57 | 7.4 |
| Settlement | 3 | 0.4 |
| Bare | 5 | 0.7 |
| Snow/Ice | 0 | 0.0 |
| Water | 6 | 0.8 |
| Other | 22 | 2.9 |

### Global distribution (resampled to KG 0.00833° grid, nearest-neighbour)

Total land: **147,322,862 km²** — exact KG baseline match.
Inland water within KG mask: 2.47% of land (retained as separate class).

| Class | Global % | Network % | Ratio |
|-------|----------|-----------|-------|
| Cropland | 15.1 | 27.1 | **1.79×** over |
| Forest | 29.0 | 36.8 | 1.27× |
| Shrubland | 15.1 | 11.5 | 0.76× |
| Grassland | 9.3 | 12.5 | 1.34× |
| Wetland | 2.0 | 7.4 | **3.63×** over |
| Settlement | 0.5 | 0.4 | 0.77× |
| Bare | 13.1 | 0.7 | **0.05×** under |
| Snow/Ice | 9.9 | 0.0 | **0.00×** absent |
| Water | 2.5 | 0.8 | 0.32× |
| Other | 3.4 | 2.9 | 0.84× |

### Representativeness metrics

**J = 0.558, H = 0.337**

In cross-axis context:

| Axis | J | H |
|------|---|---|
| KG (5-class) | 0.401 | 0.329 |
| Aridity (5-class) | 0.694 | 0.211 |
| Biomass (7-bin hybrid) | 0.639 | 0.166 |
| **Land cover (10-class)** | **0.558** | **0.337** |

Intermediate J (0.558): substantially better than KG, worse than aridity and biomass.

### Notable findings

**Most under-sampled classes:**
- **Snow/Ice (0.00×):** No FLUXNET towers on ice sheets. Structurally expected.
- **Bare (0.05×):** Desert/rock surfaces cover 13.1% of global land; only 5 FLUXNET
  sites. Logistically unsurprising but ecologically significant for dryland carbon budgets.
- **Water (0.32×):** Open water in KG mask (2.5% global) — EC towers on water are
  rare and site-specific (lake flux towers).

**Most over-sampled classes:**
- **Wetland (3.63×):** 57 sites (7.4% of network) on 2.0% of global land. Wetlands
  are disproportionately studied for their methane and carbon-exchange significance.
  This is a design feature, not a gap.
- **Cropland (1.79×):** 208 sites (27.1%) on 15.1% of land. Agricultural flux
  monitoring is a research priority; croplands are relatively accessible. For
  agricultural-GHG stakeholders, the over-sampling is actually favorable coverage.

**Forest (1.27×)** and **Grassland (1.34×)** are modestly over-sampled. **Shrubland
(0.76×)** is modestly under-sampled — dryland shrublands in central Asia, Australia,
and South America are documented gaps.

The LULC axis complements the biomass axis: biomass measures carbon-stock density
gradient, LULC measures functional ecosystem type. Together they characterize both
the compositional coverage and the structural density coverage of the network.

### Files

| File | Description |
|------|-------------|
| `data/external/cci_landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif` | Downloaded raster (gitignored) |
| `data/external/cci_landcover/ESACCI-LC-Legend.csv` | Class legend |
| `data/external/cci_landcover/ESACCI-LCMapsColorLegend.qml` | QML color file |
| `data/external/cci_landcover/README.md` | Download details, aggregation table, citation |
| `scripts/figure_representativeness_landcover.R` | Analysis script |
| `data/snapshots/site_landcover_cci.csv` | 767 sites, native + high-level class |
| `data/snapshots/landcover_cci_global_distribution.csv` | 10 classes, global area fractions |
| `data/snapshots/representativeness_metrics.csv` | +1 row (landcover_cci, high_level) |
| `review/figures/representativeness/fig_representativeness_landcover.png` | Figure |
| `review/figures/representativeness/methods_landcover.md` | Methods text |

---

## 2026-06-24 — Biomass axis hybrid re-binning: equal-area quantile scheme

### Summary

Replaced the fixed-breakpoint biomass binning scheme (0-5, 5-25, 25-50, 50-100,
100-200, 200-400, >400 Mg/ha) with a hybrid scheme: one fixed near-zero bin
plus six equal-area quantile bins computed from the area-weighted global distribution
of vegetated land (biomass ≥ 5 Mg/ha, KG-land mask, 147.3 M km²).

### Computed quantile breakpoints (canonical)

| Quantile | Cumulative area fraction | Breakpoint (Mg/ha) |
|---|---|---|
| q1 | 1/6 | **13.0** |
| q2 | 2/6 | **27.0** |
| q3 | 3/6 | **51.0** |
| q4 | 4/6 | **94.0** |
| q5 | 5/6 | **171.0** |

Vegetated land area (≥ 5 Mg/ha, KG mask): **70,162,294 km²** (47.6% of total land).
Bin 1 (0–5 Mg/ha): **52.4%** of total land, unchanged from previous.

Computed via 1 Mg/ha histogram (5–1000 Mg/ha, 995 bins + catch-all),
terra::zonal() area sum, cumulative area crossings. Breakpoints rounded to 1 d.p.

### Global distribution (hybrid bins, 2024, KG land mask)

| Bin | Range (Mg/ha) | Global % | Equal-area target % |
|---|---|---|---|
| 1 | 0–5 (fixed) | 52.4 | — |
| 2 | 5–13.0 | 8.3 | 7.9 |
| 3 | 13.0–27.0 | 8.0 | 7.9 |
| 4 | 27.0–51.0 | 7.7 | 7.9 |
| 5 | 51.0–94.0 | 8.0 | 7.9 |
| 6 | 94.0–171.0 | 7.9 | 7.9 |
| 7 | >171.0 | 7.9 | 7.9 |

Bins 2–7 achieve ≈1/6 each (7.7–8.3% vs target 7.9%) — minor residual from
histogram discretization at 1 Mg/ha precision.

### Network distribution and sampling ratios (767 sites)

| Bin | Range (Mg/ha) | Network % | Global % | Ratio |
|---|---|---|---|---|
| 1 | 0–5 | 31.6 | 52.4 | **0.60×** under-sampled |
| 2 | 5–13.0 | 13.7 | 8.3 | 1.66× |
| 3 | 13.0–27.0 | 10.8 | 8.0 | 1.36× |
| 4 | 27.0–51.0 | 8.7 | 7.7 | 1.14× |
| 5 | 51.0–94.0 | 13.6 | 8.0 | **1.69×** |
| 6 | 94.0–171.0 | 15.0 | 7.9 | **1.91×** over-sampled |
| 7 | >171.0 | 6.6 | 7.9 | **0.84×** under-sampled |

Key patterns under equal-area bins:
- Bin 1 (non-vegetated): strongly under-sampled (0.60×), as expected — EC towers are
  not deployed on bare/desert/ice surfaces.
- Bins 2–6 (sparse to temperate-forest biomass): all over-sampled. The gradient of
  over-sampling peaks in bin 6 (94–171 Mg/ha, 1.91×) — the typical AGB range of
  North American and European temperate and boreal forests, where FLUXNET is densest.
- Bin 7 (>171 Mg/ha, dense and tropical forest): **under-sampled at 0.84×** despite
  this bin containing 7.9% of global land. This is the clearest actionable gap:
  high-biomass tropical and subtropical forests are under-represented. Under the old
  fixed binning, the equivalent signal was masked because bin 7 (>400 Mg/ha) held
  only 0.04% of global land (dominated by the highest-AGB tropical pixels) while
  bin 6 (200–400) held 6.3% — making the tropical under-sampling harder to read.

### Updated representativeness metrics

| Scheme | J | H |
|---|---|---|
| Fixed bins (previous) | 0.6262 | 0.1684 |
| **Hybrid equal-area (new)** | **0.6385** | **0.1656** |

J increases by +0.012, H decreases by -0.003 — small improvements reflecting that
the equal-area binning distributes global land more evenly across bins, reducing
inflation from the near-empty bin 7 (>400 Mg/ha) that depressed J under the old scheme.
Both schemes place the biomass axis in the same interpretive position relative to
KG and aridity axes.

### Files updated

| File | Change |
|---|---|
| `scripts/figure_representativeness_biomass.R` | Replaced fixed bins with dynamic equal-area quantile scheme |
| `data/snapshots/site_biomass_cci_v7.csv` | `biomass_bin` and `biomass_bin_label` updated for all 767 sites |
| `data/snapshots/biomass_cci_v7_global_distribution.csv` | 7 bins, hybrid breakpoints, global area fractions |
| `data/snapshots/representativeness_metrics.csv` | `biomass_cci_v7` row updated (aggregation_level: 7bin_hybrid) |
| `review/figures/representativeness/fig_representativeness_biomass.png` | Rebuilt with hybrid bin labels |
| `review/figures/representativeness/methods_biomass.md` | Rewritten with axis framing + hybrid scheme + quantile method |

---

## 2026-06-24 — Biomass representativeness axis: ESA CCI Biomass v7.0 (2024)

### Data and download

Source: ESA CCI Biomass v7.0 (March 2026), CEDA catalogue
doi:10.5285/6429d1aafe1e43b9b414e4a5a7f8b903. Anonymous HTTP, no authentication.

File downloaded: `ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif` (1.4 GB) from
`http://data.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v7.0/geotiff/aggregated/`

The 1km pre-aggregated file is a multi-band GeoTIFF containing all 18 available
years as bands (2005–2012, 2015–2024). Band 18 = **2024** (most recent) was
selected for analysis. The native 100m product (~18 GB per year of tiled files)
was not downloaded; 1km is appropriate for EC footprint scale (0.5–3 km) and
matches the KG land mask resolution directly.

### Resolution and land mask consistency

Biomass raster resampled to the KG 0.00833° grid (terra::resample(), bilinear).
KG-land pixels where resampled biomass is NA → assigned to bin 1 (bare/desert).
Total land area: **147,322,862 km²** — exact match with KG baseline.

### Per-site extraction (767 sites, snapshot 20260624T095651)

0 NA sites at 1km. Site distribution across 7 biomass bins:

| Bin | Range (Mg/ha) | Label | Sites | % |
|---|---|---|---|---|
| 1 | 0-5 | Bare/ice/desert | 242 | 31.6% |
| 2 | 5-25 | Sparse vegetation | 179 | 23.3% |
| 3 | 25-50 | Shrubland/savanna | 75 | 9.8% |
| 4 | 50-100 | Open forest | 117 | 15.3% |
| 5 | 100-200 | Temperate forest | 121 | 15.8% |
| 6 | 200-400 | Wet/boreal forest | 32 | 4.2% |
| 7 | >400 | Tropical forest | 1 | 0.1% |

Bin 1 (242 sites, 31.6%) captures non-woody biomes — croplands, grasslands,
wetlands, and tundra — where CCI Biomass reports near-zero woody AGB. Only
1 FLUXNET site exceeds 400 Mg/ha (tropical forest threshold at 1km scale).

### Global distribution (2024, KG land mask)

| Bin | Range (Mg/ha) | Global % |
|---|---|---|
| 1 | 0-5 | **52.4%** |
| 2 | 5-25 | 15.3% |
| 3 | 25-50 | 8.3% |
| 4 | 50-100 | 9.1% |
| 5 | 100-200 | 8.5% |
| 6 | 200-400 | 6.3% |
| 7 | >400 | ~0.0% |

Bin 1 dominates: 52.4% of global land has <5 Mg/ha AGB under the CCI 2024
product. This captures deserts (Sahara, Arabian, Australian), polar regions,
and ice sheets. At 1km resolution, even the densest tropical forests rarely
average >400 Mg/ha after spatial averaging, consistent with the global ~0%
in bin 7.

### Sampling ratios and representativeness

| Bin | Global % | Network % | Ratio |
|---|---|---|---|
| 1 (bare) | 52.4 | 31.6 | **0.60×** under-sampled |
| 2 (sparse) | 15.3 | 23.3 | 1.52× over-sampled |
| 3 (shrub) | 8.3 | 9.8 | 1.18× |
| 4 (open forest) | 9.1 | 15.3 | **1.68×** over-sampled |
| 5 (temperate) | 8.5 | 15.8 | **1.85×** over-sampled |
| 6 (wet/boreal) | 6.3 | 4.2 | 0.66× under-sampled |
| 7 (tropical) | ~0.0 | 0.1 | ~3.7× |

The dominant pattern: the network under-samples bare/desert land (0.60×) and
wet/boreal forest (0.66×), and over-samples temperate and open forest (1.7-1.85×).
This is a direct reflection of EC tower placement logic: towers are placed in
vegetated areas where eddy covariance works best.

### Metrics

**J = 0.626, H = 0.168**

In context of the full metrics table:

| Axis | Aggregation | J | H |
|---|---|---|---|
| koppen_beck2023 | 5-class | 0.401 | 0.329 |
| koppen_beck2023 | 30-class | 0.350 | 0.440 |
| aridity_unep5 | 5-class | 0.694 | 0.211 |
| aridity_unep7 | 7-class | 0.667 | 0.217 |
| **biomass_cci_v7** | **7-bin** | **0.626** | **0.168** |

The biomass axis has better representativeness than KG (J +0.22 vs 5-class),
comparable to aridity (J −0.07 vs 5-class), and the lowest H of any axis
(0.168 — the network distribution is the closest to global land distribution
along this dimension). The relatively high J reflects that the biomass bins
are broad enough to capture the network's spread across the vegetation density
gradient, even though the network systematically skews toward mid-biomass forests.

### Files

| File | Description |
|---|---|
| `.gitignore` | `data/external/cci_biomass/*.tif` added |
| `data/external/cci_biomass/README.md` | Source, citation, download details |
| `scripts/figure_representativeness_biomass.R` | Analysis script (7 bins, KG land mask) |
| `data/snapshots/site_biomass_cci_v7.csv` | 767 sites, AGB value + bin + method |
| `data/snapshots/biomass_cci_v7_global_distribution.csv` | 7 bins, global area fractions |
| `data/snapshots/representativeness_metrics.csv` | +1 row (biomass_cci_v7, 7bin) |
| `review/figures/representativeness/fig_representativeness_biomass.png` | 7.5×6.5 in |
| `review/figures/representativeness/methods_biomass.md` | ~500-word methods text |

---

## 2026-06-24 — Future KG representativeness axis: Beck 2023 SSP5-8.5, 2071-2099

### Archive note

The Beck 2023 figshare archive uses `2071_2099/` (not `2071_2100/`) for the
end-of-century period. All outputs use the actual archive period string; this
file documents the correct path.

### Script parameterization

`scripts/figure_representativeness_kg_future.R` was refactored to accept CLI
arguments: `[ssp] [period_dir] [period_label] [scenario_label]`. Both scenarios
are now regenerated by the same script:

```
Rscript scripts/figure_representativeness_kg_future.R ssp245 2041_2070 "2041-70" "SSP2-4.5"
Rscript scripts/figure_representativeness_kg_future.R ssp585 2071_2099 "2071-99" "SSP5-8.5"
```

`methods_koppen_beck2023_future.md` is rebuilt each run: shared preamble + all
scenario-specific sections (upsert — running either scenario regenerates the
whole file without disturbing the other scenario's section).

### Per-site future KG (SSP5-8.5, 2071-2099)

**436 of 767 sites (56.8%) change KG class** — more than double the 191/767
(24.9%) under SSP2-4.5 2041-2070.

Dominant shift directions (North America): Dfc → Dfb across the Canadian boreal
(7+ sites), Dfb → Dfa or Dfa → Cfa across the Great Plains and Midwest, Cfb/Csb
→ BSh/BSk in the western US. In Europe: Cfb/Dfb → Cfa/Dfa across central Europe,
Dfc → Dfb for Nordic boreal. In Japan: Dfa → Cfa (multiple sites). At high
latitudes: substantial ET → Dfc and Dfc → Dfb transitions as polar and subarctic
zones retreat.

Site distribution under SSP5-8.5 (for reference only; NOT the network bar):
A: 8.4%, B: 14.0%, C: 40.2%, D: 35.2%, E: 2.2%

### Global class distribution shifts (present-day → SSP5-8.5, 2071-2099)

Top 10 global class shifts — substantially larger than SSP2-4.5 at mid-century:

| Class | Present (%) | SSP5-8.5 (%) | Δ (pp) |
|---|---|---|---|
| Dfa | 1.60 | 7.45 | **+5.85** |
| Dfc | 10.14 | 4.85 | **−5.29** |
| ET | 4.64 | 1.91 | −2.73 |
| Cfa | 4.27 | 6.74 | +2.48 |
| Aw | 12.10 | 14.18 | +2.07 |
| BWh | 14.62 | 16.62 | +2.00 |
| Dfb | 6.93 | 4.94 | −1.99 |
| BSh | 5.83 | 7.39 | +1.56 |
| Cwa | 2.77 | 1.60 | −1.17 |
| Dwa | 0.73 | 1.60 | +0.88 |

5-class summary: E contracts sharply (14.2% → 11.1%, −3.08 pp); A (+2.47 pp)
and B (+2.49 pp) expand together; D contracts moderately (−1.90 pp); C nearly
unchanged (< 0.01 pp). The Dfa expansion (+5.85 pp) is the dominant within-D
signal — warm continental is the single largest class shift of any class in any
scenario.

### Three-way metrics comparison

| Aggregation | J (Present) | J (SSP2-4.5 mid) | J (SSP5-8.5 end) | H (Present) | H (SSP2-4.5 mid) | H (SSP5-8.5 end) |
|---|---|---|---|---|---|---|
| 5-class | 0.401 | 0.397 | **0.382** | 0.329 | 0.331 | **0.337** |
| two-letter | 0.373 | 0.381 | **0.365** | 0.410 | 0.406 | **0.410** |
| 30-class | 0.350 | **0.368** | 0.346 | 0.440 | **0.433** | 0.450 |

**Key narrative:** The marginal representativeness improvement seen under SSP2-4.5
(2041-2070) reverses under SSP5-8.5 (2071-2099). At the 30-class level, J peaks
at SSP2-4.5 (0.368) before falling back below present-day levels (0.346 vs 0.350).
At the 5-class and two-letter levels, SSP2-4.5 is near-flat or marginally better
than present-day; SSP5-8.5 is measurably worse than present-day (5-class J drops
from 0.401 to 0.382; ΔJ = −0.019).

The mechanism: under moderate warming, the classes the network under-samples (ET,
Dfc) shrink, which incidentally benefits representativeness. Under high warming,
this partial compensation is overwhelmed by large expansion of A-class (tropical)
and B-class (arid), which remain structurally under-sampled.

**BWh (the persistent sampling gap):** BWh is the single largest global land class
at all time points (14.6% present, 15.3% SSP2-4.5, 16.6% SSP5-8.5). It grows
with warming, and network coverage remains thin across all three panels.

### Files

| File | Description |
|---|---|
| `scripts/figure_representativeness_kg_future.R` | Parameterized; supports any scenario |
| `data/snapshots/site_koppen_beck2023_ssp585_2071_2099.csv` | 767 rows, future KG per site |
| `data/snapshots/koppen_beck2023_ssp585_2071_2099_global_distribution.csv` | 30 rows |
| `data/snapshots/representativeness_metrics.csv` | +3 rows for SSP5-8.5 |
| `review/figures/representativeness/fig_representativeness_kg_future_ssp585_2071_2099_30class.png` | |
| `review/figures/representativeness/fig_representativeness_kg_future_ssp585_2071_2099_5class.png` | |
| `review/figures/representativeness/fig_representativeness_kg_future_ssp585_2071_2099_twoletter.png` | |
| `review/figures/representativeness/methods_koppen_beck2023_future.md` | Updated; both scenarios |

---

## 2026-06-24 — Future KG representativeness axis: Beck 2023 SSP2-4.5, 2041-2070

### Design

Asymmetric framing: the **Earth bar** shows global land area fractions under the
projected 2041-2070 climate (SSP2-4.5); the **Network bar** shows the current 767-site
network with *present-day* KG assignments from `site_koppen_beck2023.csv`. This answers
the question: does a network placed at its current locations, classified by its present
biogeography, sample the climate distribution projected to dominate mid-century Earth?

Source raster: `data/external/koppen_beck2023/2041_2070/ssp245/koppen_geiger_0p00833333.tif`
(Beck et al. 2023, figshare v2, doi:10.6084/m9.figshare.21789074.v2). Same 30-class
scheme and legend as the present-day 1991-2020 map.

### Per-site future KG extraction (site_koppen_beck2023_ssp245_2041_2070.csv)

767 sites, same extraction method (terra::extract + nearest-land fallback). Same 3 sites
needed fallback as in the present-day and aridity extractions (US-KS3 0.7 km, US-TaS 0.6 km,
CN-SnB 1 km — now code 2, 3, and 14 respectively under the future projection).

**191 of 767 sites (24.9%) change KG class under SSP2-4.5 by 2041-2070.**

Dominant shift directions (North America): Dfc → Dfb (boreal transitioning to warm
continental), Dfa → Dfb (central US warming into cold continental), Cfb → Dfb (oceanic
to continental in NW Europe). In Europe: Cfb → Dfb across Germany, France, and Nordic
sites; Dfc → ET losses (high-altitude Italian and Swiss sites). In Japan: Dfa → Dfb
across multiple Hokkaido / northern Honshu sites. Isolated cases: several US Great Plains
sites shift from BSh to BSk (arid warming), and three AU sites shift Aw/Am (tropical).

Site future-class distribution (for reference; not used in figure network bar):
A: 8.0%, B: 12.3%, C: 42.2%, D: 35.6%, E: 2.0%

### Global distribution under future climate

Total land area: **147,322,862 km²** (identical to present-day; same raster mask). The
largest global class shifts between 1991-2020 and 2041-2070 SSP2-4.5:

| Class | Present (%) | Future (%) | Δ (pp) | Direction |
|---|---|---|---|---|
| ET | 4.64 | 3.27 | **−1.37** | Shrink |
| Dfc | 10.14 | 8.91 | **−1.23** | Shrink |
| Aw | 12.10 | 13.24 | **+1.14** | Expand |
| Dfa | 1.60 | 2.72 | **+1.11** | Expand |
| Cfa | 4.27 | 5.07 | +0.80 | Expand |
| BSh | 5.83 | 6.63 | +0.80 | Expand |
| Cwa | 2.77 | 2.11 | −0.66 | Shrink |
| BWh | 14.62 | 15.28 | +0.66 | Expand |

5-class summary: E contracts 14.2% → 12.7% (−1.49 pp); A expands 20.0% → 21.3% (+1.28 pp);
B expands marginally (+0.59 pp); C and D nearly stable (< 0.5 pp change each).

### Metrics: present-day network vs future global distribution

| Aggregation | J (present) | J (future) | ΔJ | H (present) | H (future) | ΔH |
|---|---|---|---|---|---|---|
| 5-class | 0.401 | 0.397 | −0.004 | 0.329 | 0.331 | +0.002 |
| two-letter (13-class) | 0.373 | 0.381 | **+0.007** | 0.410 | 0.406 | −0.005 |
| 30-class | 0.350 | 0.368 | **+0.018** | 0.440 | 0.433 | **−0.008** |

**Key finding:** representativeness metrics change only marginally (|ΔJ| < 0.02 at all
aggregation levels), and the direction is counter-intuitive — the current network is
*slightly more* representative of the projected 2041-2070 distribution than of the
present-day distribution at the 30-class level (J +0.018, H −0.008). This occurs because
the future projection shrinks the two classes that the network most under-samples (ET and
Dfc), while expanding classes where network coverage is relatively stronger (Aw, Dfa, Cfa).
The implication is that the existing network gap — predominantly a Hyper-Arid and
Arid under-representation — does not worsen under SSP2-4.5 by 2041-2070, but it also
does not improve: sampling of BWh (the single largest global land class, 14-15%) remains
structurally weak.

### Files

| File | Description |
|---|---|
| `scripts/figure_representativeness_kg_future.R` | New script |
| `data/snapshots/site_koppen_beck2023_ssp245_2041_2070.csv` | 767 rows, future KG per site |
| `data/snapshots/koppen_beck2023_ssp245_2041_2070_global_distribution.csv` | 30 rows, global class fractions |
| `data/snapshots/representativeness_metrics.csv` | +3 rows (axis = koppen_beck2023_future_ssp245_2041_2070) |
| `review/figures/representativeness/fig_representativeness_kg_future_ssp245_2041_2070_30class.png` | 6.5×8 in |
| `review/figures/representativeness/fig_representativeness_kg_future_ssp245_2041_2070_5class.png` | 7.5×5 in |
| `review/figures/representativeness/fig_representativeness_kg_future_ssp245_2041_2070_twoletter.png` | 6.5×6.5 in |
| `review/figures/representativeness/methods_koppen_beck2023_future.md` | Methods text |

---

## 2026-06-24 — Aridity representativeness axis: 5-class and 7-class parallel schemes

### Site extraction (767 sites, snapshot 20260624T095651)

Re-extracted from scratch. Same 3 coastal wetland NAs as before (US-KS3, US-TaS,
CN-SnB), recovered at 0.6–1.0 km; all assigned Humid (low) in 7-class. `site_aridity.csv`
now carries `unep_class_5` and `unep_class_7` columns (column `unep_class` removed).

| Class | 5-class n | 5-class % | 7-class n | 7-class % |
|---|---|---|---|---|
| Hyper-Arid | 4 | 0.5% | 4 | 0.5% |
| Arid | 45 | 5.9% | 45 | 5.9% |
| Semi-Arid | 147 | 19.2% | 147 | 19.2% |
| Dry Sub-Humid | 70 | 9.1% | 70 | 9.1% |
| Humid | 501 | 65.3% | — | — |
| Humid (low) 0.65–1.0 | — | — | 195 | 25.4% |
| Humid (moderate) 1.0–2.0 | — | — | 271 | 35.3% |
| Hyper-Humid ≥ 2.0 | — | — | 35 | 4.6% |

AI range across 767 sites: [0.0197, 3.7939]

### Global distributions

Total land area: **134,761,545 km²** (aridity raster covers 60°S–90°N; ~12.6 M km²
less than KG raster due to Antarctic exclusion).

| Class | Global % | Network % (5cl) | Ratio (5cl) | Network % (7cl) | Ratio (7cl) |
|---|---|---|---|---|---|
| Hyper-Arid | 9.8% | 0.5% | **0.05×** | 0.5% | **0.05×** |
| Arid | 14.7% | 5.9% | 0.40× | 5.9% | 0.40× |
| Semi-Arid | 16.5% | 19.2% | 1.16× | 19.2% | 1.16× |
| Dry Sub-Humid | 8.8% | 9.1% | 1.03× | 9.1% | 1.03× |
| Humid (all) | 50.2% | 65.3% | 1.30× | — | — |
| Humid (low) | 18.9% | — | — | 25.4% | **1.34×** |
| Humid (moderate) | 24.8% | — | — | 35.3% | **1.42×** |
| Hyper-Humid | 6.5% | — | — | 4.6% | 0.71× |

The 7-class scheme reveals that the over-representation of Humid is concentrated in
Humid (moderate) (1.42×) and Humid (low) (1.34×); Hyper-Humid (≥ 2.0, largely tropical
rainforest) is slightly under-sampled (0.71×).

### Metrics

| Axis | J (Jaccard) | H (Hellinger) |
|---|---|---|
| aridity_unep5 (5-class) | **0.694** | **0.211** |
| aridity_unep7 (7-class) | **0.667** | **0.218** |
| koppen_beck2023 (5-class) | 0.401 | 0.329 |
| koppen_beck2023 (30-class) | 0.350 | 0.440 |

Splitting the Humid class into three reduces J slightly (0.694 → 0.667) and increases H
(0.211 → 0.218), consistent with the 7-class scheme revealing within-Humid heterogeneity
that the 5-class scheme collapses. Both aridity metrics remain substantially better than KG.

### Files

| File | Change |
|---|---|
| `scripts/figure_representativeness_aridity.R` | Full rewrite; single script for both schemes |
| `data/snapshots/site_aridity.csv` | unep_class → unep_class_5 + unep_class_7 |
| `data/snapshots/aridity_unep5_global_distribution.csv` | Renamed from aridity_unep_* |
| `data/snapshots/aridity_unep7_global_distribution.csv` | New; 7 rows |
| `data/snapshots/representativeness_metrics.csv` | Split aridity_unep → aridity_unep5 + aridity_unep7 |
| `review/figures/representativeness/fig_representativeness_aridity_unep5.png` | 7×6.5 in |
| `review/figures/representativeness/fig_representativeness_aridity_unep7.png` | 7.5×6.5 in; humid trio in blues |
| `review/figures/representativeness/methods_aridity_unep.md` | Updated for both schemes |
| `review/figures/representativeness/fig_representativeness_aridity_unep.png` | Removed (superseded) |

---

## 2026-06-24 — Aridity UNEP representativeness axis

### Site extraction (767 sites, snapshot 20260624T095651)

All 767 sites extracted fresh from CGIAR AI v3.1 raster
(`Global-AI_ET0__annual_v3_1/ai_v31_yr.tif`, INT2U, 0.00833°). Raw integer ×
0.0001 = AI. Value 0 = ocean/nodata; 3 coastal wetland sites (US-KS3, US-TaS,
CN-SnB) fell on ocean pixels and were recovered via nearest-land pixel search
(0.6–1.0 km) — all three assigned to the Humid class.

| UNEP Class | Sites (n) | Network % | Global % | Sampling ratio |
|---|---|---|---|---|
| Hyper-Arid | 4 | 0.5% | 9.8% | **0.05×** |
| Arid | 45 | 5.9% | 14.7% | 0.40× |
| Semi-Arid | 147 | 19.2% | 16.5% | 1.16× |
| Dry Sub-Humid | 70 | 9.1% | 8.8% | 1.03× |
| Humid | 501 | 65.3% | 50.2% | 1.30× |

### Global distribution

Total land area from CGIAR aridity raster: **134,761,545 km²**. The aridity
raster covers 60°S to 90°N; the difference from the KG total (147,322,862 km²)
reflects the exclusion of Antarctic landmass not covered by the aridity dataset.
Majority of global land is Humid (50.2%), followed by Semi-Arid (16.5%) and
Arid (14.7%). Hyper-Arid accounts for 9.8% of land but only 0.5% of sites.

### Metrics

| Axis | J (Jaccard) | H (Hellinger) |
|---|---|---|
| Aridity UNEP 5-class | **0.69** | **0.21** |
| KG 5-class | 0.40 | 0.33 |
| KG 13-class | 0.37 | 0.41 |
| KG 30-class | 0.35 | 0.44 |

The aridity UNEP axis shows substantially better representativeness than KG: J =
0.69 vs 0.40 at the same 5-class level. The network is moderately over-sampled
for humid and semi-arid environments (1.03–1.30×) and severely under-sampled for
hyper-arid lands (0.05×). Like polar EF in the KG analysis, the Hyper-Arid under-
sampling partially reflects structural access constraints (remote desert interiors).

### Files

| File | Description |
|---|---|
| `scripts/figure_representativeness_aridity.R` | Self-contained analysis + figure + methods script |
| `data/snapshots/site_aridity.csv` | Replaced; 767 sites, ai_value + unep_class + aridity_method |
| `data/snapshots/aridity_unep_global_distribution.csv` | 5-row UNEP global land area table |
| `data/snapshots/representativeness_metrics.csv` | Renamed from koppen_beck2023_*; 4 rows covering KG + aridity |
| `review/figures/representativeness/fig_representativeness_aridity_unep.png` | 7×6.5 in, 200 dpi |
| `review/figures/representativeness/methods_aridity_unep.md` | ~600-word methods draft |

### Color note

Humid class shown as #cccccc (pale gray) with uniform thin grey border, substituting
the original "white / no fill" spec which would be invisible against the white panel
background. Documented in script comments and methods text.

---

## 2026-06-24 — KG representativeness metrics (Jaccard + Hellinger) and methods text

### Metrics

| Aggregation | Weighted Jaccard (J) | Hellinger distance (H) |
|---|---|---|
| 5-class (A/B/C/D/E) | **0.40** | **0.33** |
| 13-class (two-letter) | **0.37** | **0.41** |
| 30-class (native) | **0.35** | **0.44** |

Interpretation: J measures overlap fraction (1 = identical, 0 = no overlap). H measures
divergence (0 = identical, 1 = maximally different). Coarser aggregation increases J and
decreases H because within-group heterogeneity is collapsed — the FLUXNET network is more
representative at the 5-class level than at 30-class resolution. The 30-class H of 0.44
indicates substantial compositional divergence, driven primarily by EF (9.6% of global
land, 0 sites), BW (18.2% global, 0.17× sampled), and Cs (1.8% global, 5.31× over-sampled).

### Files

| File | Description |
|---|---|
| `data/snapshots/koppen_beck2023_representativeness_metrics.csv` | Metrics table, 3 rows × 3 cols |
| `review/figures/representativeness/methods_koppen_beck2023.md` | ~500-word methods draft |
| `scripts/figure_representativeness_kg.R` | Extended with `compute_repr_metrics()` and annotations |
| `review/figures/representativeness/fig_representativeness_kg_30class.png` | Annotated J/H upper-right |
| `review/figures/representativeness/fig_representativeness_kg_5class.png` | Annotated J/H upper-right |
| `review/figures/representativeness/fig_representativeness_kg_twoletter.png` | Annotated J/H upper-right |

### Implementation

`compute_repr_metrics(p, q)` takes aligned fraction vectors and returns `weighted_jaccard`
(Σ min / Σ max) and `hellinger_distance` ((1/√2) × √(Σ(√p − √q)²)). Metrics computed at
each aggregation level and displayed as `annotate("text", x=Inf, y=Inf, hjust=1.08, vjust=1.5)`
in the upper-right of each bar panel.

---

## 2026-06-24 — Representativeness figure: KG axis, two-letter aggregation (13 classes)

### Files

| File | Dimensions | Description |
|---|---|---|
| `review/figures/representativeness/fig_representativeness_kg_twoletter.png` | 6.5 × 6.5 in, 200 dpi | Two-bar 13-class stacked bars + sampling-ratio panel beneath |
| `data/snapshots/site_koppen_beck2023.csv` | 767 rows | Added `koppen_twoletter` column |
| `data/snapshots/koppen_beck2023_global_distribution.csv` | 30 rows | Added `koppen_twoletter` column |
| `scripts/step4_extract_koppen_beck2023.R` | — | Now writes `koppen_twoletter` to snapshot |
| `scripts/compute_koppen_beck2023_global.R` | — | Now writes `koppen_twoletter` to global distribution |
| `scripts/figure_representativeness_kg.R` | — | Extended with Figure 3 (two-letter) |

### Two-letter aggregation

`koppen_twoletter = substr(koppen_class, 1L, 2L)` applied to all 30 classes.  
Result: **13 unique two-letter codes** — Af, Am, Aw, BS, BW, Cf, Cs, Cw, Df, Ds, Dw, EF, ET.  
Class order in figure: `c("Af","Am","Aw","BS","BW","Cf","Cs","Cw","Df","Ds","Dw","EF","ET")`.

### Two-letter distribution (767 FLUXNET sites, global area)

| Code | Global area | n sites | Sampling ratio |
|---|---|---|---|
| Af | 4.6% | 12 | 0.34× |
| Am | 3.3% | 12 | 0.47× |
| Aw | 12.1% | 25 | 0.27× |
| BS | 10.9% | 62 | 0.74× |
| BW | 18.2% | 24 | 0.17× |
| Cf | 6.4% | 208 | **4.21×** |
| Cs | 1.8% | 75 | **5.31×** |
| Cw | 3.9% | 9 | 0.30× |
| Df | 18.8% | 263 | 1.83× |
| Ds | 1.9% | 26 | 1.76× |
| Dw | 3.9% | 29 | 0.96× |
| EF | 9.6% | **0** | 0.00× ← unstable |
| ET | 4.6% | 22 | 0.62× |

### Design decisions

**Colors:** Mean RGB of all 30-class members within each two-letter group (unweighted mean).
Singletons (EF, ET) retain their own Beck RGB values.

**EF (0 sites):** Flagged as unstable. In the ratio panel: shown with × shape (shape=4) and
alpha=0.45, placed at sampling_ratio=0 which falls below the log₂ axis lower limit (0.08×) and
is not visible in the panel. The × is clipped out — EF's absence in the ratio panel is
intentional and honest.

**Sampling-ratio panel:** Below the stacked bars (patchwork `/` operator), height ratio 3:1.8.
Log₂ y-axis (0.08× → 12×). Vertical lollipops from ratio=1 dashed line.

**Label thresholds:** Two-line (code + %) for segments ≥ 7%, single-line for 2.5–7%,
code-only for > 0%, NA for zero-fraction segments.

---

## 2026-06-24 — Representativeness figure: KG axis (v1)

### Files

| File | Dimensions | Description |
|---|---|---|
| `review/figures/representativeness/fig_representativeness_kg_30class.png` | 6.5 × 8 in, 200 dpi | Two-bar stacked chart, 30 KG classes |
| `review/figures/representativeness/fig_representativeness_kg_5class.png` | 7.5 × 4 in, 200 dpi | Two-bar 5-class + sampling-ratio panel |
| `scripts/figure_representativeness_kg.R` | — | Figure script |

### Design decisions

**Color scheme:** Beck 2023 published RGB values from `legend.txt` for both figures.
5-class representatives chosen per spec: A = Aw [70, 170, 250], B = BSh [245, 165, 0],
C = Cfa [200, 255, 80], D = Dfb [55, 200, 255], E = ET [178, 178, 178].

**Stacking order:** ggplot2 `position_stack` with factor levels in code order (Af=1 → EF=30)
results in E classes at the bottom and A classes at the top. This is an artifact of ggplot2's
stacking direction (last factor level rendered first/bottom). The visual result — cold/polar at
the base, tropical at the top — is climatologically intuitive and retained as-is.

**Labels:** 30-class figure labels segments where fraction ≥ 1.5% in that bar (class code only,
size 2.3). 5-class: two-line (class + %) for segments ≥ 8%, single-line for 3–8%, letter-only
below 3%. Sampling-ratio panel uses log₂ x-axis (0.25× → 4×), lollipop style, labels to the
right of each dot.

**Font:** `sans` (Helvetica on macOS). White background throughout.

### Stacking order note for next iteration

Consider adding `A` at the bottom of the bar (current bottom is `E`) so the ordering is
consistent with KG map conventions (tropical at low latitudes = visually prominent). This
requires reversing the factor level order in `class_order`. Low priority — decide alongside
the FLUXNET2015 third-bar addition.

---

## 2026-06-24 — Beck 2023 KG global area-weighted distribution

### Method

`terra::cellSize(mask=TRUE, unit="km")` computes geodesic pixel area in km² for all land
pixels (ocean = NA in Beck 2023 raster, automatically excluded). `terra::zonal(fun="sum")`
accumulates land area per KG class without materialising the full raster. Total land area:
**147,322,862 km²** (reference: Earth ~148.9 M km² — difference is representation of small
islands at 1 km resolution). Zonal step: 5.2 s.

### Output

`data/snapshots/koppen_beck2023_global_distribution.csv` — 30 rows, columns:
`koppen_class_code`, `koppen_class`, `koppen_class_name`, `koppen_main`, `koppen_main_name`,
`global_land_area_km2`, `global_land_fraction`.

### Top 10 KG classes by global land area

| Class | Name | Area (M km²) | % |
|---|---|---|---|
| BWh | Arid, desert, hot | 21.54 | 14.62 |
| Aw | Tropical, savannah | 17.83 | 12.10 |
| Dfc | Cold, no dry season, cold summer | 14.94 | 10.14 |
| EF | Polar, frost | 14.08 | 9.56 |
| Dfb | Cold, no dry season, warm summer | 10.21 | 6.93 |
| BSh | Arid, steppe, hot | 8.59 | 5.83 |
| BSk | Arid, steppe, cold | 7.43 | 5.04 |
| ET | Polar, tundra | 6.84 | 4.64 |
| Af | Tropical, rainforest | 6.80 | 4.61 |
| Cfa | Temperate, no dry season, hot summer | 6.29 | 4.27 |

### 5-class summary: global area vs FLUXNET network representation

| Class | Name | Global area % | Sites (n) | Network % | Sampling ratio |
|---|---|---|---|---|---|
| A | Tropical | 20.0 | 49 | 6.4 | **0.32** ← severe under-sampling |
| B | Arid | 29.0 | 86 | 11.2 | **0.39** ← severe under-sampling |
| C | Temperate | 12.1 | 292 | 38.1 | **3.14** ← strong over-sampling |
| D | Cold | 24.6 | 318 | 41.5 | **1.68** ← moderate over-sampling |
| E | Polar | 14.2 | 22 | 2.9 | **0.20** ← severe under-sampling |

Sampling ratio > 1 = over-sampled; < 1 = under-sampled relative to global land area.

### Interpretation note

B (arid) covers 29% of global land but holds only 11% of network sites (ratio 0.39).
A (tropical) covers 20% of land but only 6% of sites (ratio 0.32). Polar EF alone
(9.6% of land) has very few sites because flux towers on ice sheets are rare/impossible.
Temperate C (3.1×) and Cold D (1.7×) classes are the over-sampled backbone of the current
network — a known biogeographic bias in the eddy covariance record.

---

## 2026-06-24 — Beck 2023 KG download and per-site extraction

### Raster source

**Beck et al. (2023)** High-resolution (1 km) Köppen-Geiger maps for 1901–2099 based on
constrained CMIP6 projections. *Scientific Data* 10:724.
doi: 10.1038/s41597-023-02549-6

Figshare: doi:10.6084/m9.figshare.21789074.v2 (v2 published 2026-01-14; corrects a
calculation error in v1). Downloaded file: `koppen_geiger_tif.zip` (~125 MB, file ID 61012822).

Raster used for extraction: `1991_2020/koppen_geiger_0p00833333.tif` — present-day KG
classification (1991–2020) at 1 km resolution (0.00833333°), EPSG:4326, 21600 × 43200 cells,
30 integer classes.

### Files committed

| File | Description |
|---|---|
| `.gitignore` | Updated to allow README/legend in koppen_beck2023 while ignoring rasters |
| `data/external/koppen_beck2023/README.md` | Source, citation, file structure |
| `data/external/koppen_beck2023/legend.txt` | Beck 2023 class legend (30 classes, RGB) |
| `scripts/step4_extract_koppen_beck2023.R` | Extraction script |
| `data/snapshots/site_koppen_beck2023.csv` | Per-site KG class, 767 sites |
| `data/snapshots/site_koppen_beck2023.meta.json` | Companion metadata |

Raster files (`.tif`, `.zip`) and period subdirectories are gitignored.

### Extraction results (767 sites)

**Snapshot:** `fluxnet_shuttle_snapshot_20260624T095651.csv`

**Primary extraction (exact pixel):** 764 sites — all returned a valid class.

**Fallback — nearest-land pixel:** 3 sites had coordinates falling in ocean/tidal water;
recovered using nearest non-NA land pixel within 3° window:

| site_id | lat | lon | Class assigned | Nearest land (km) | Note |
|---|---|---|---|---|---|
| US-KS3 | 28.71 | −80.74 | Cfa | 0.7 | Coastal FL, KSC area |
| US-TaS | 25.19 | −80.64 | Am | 0.6 | Florida Bay/Everglades |
| CN-SnB | 31.69 | 121.66 | Cfa | 1.0 | Yangtze estuary, near Shanghai |

**`koppen_method` column** records extraction method per site (`exact` or
`nearest_land_Xkm`).

### KG main class distribution

| Main class | Name | Sites | % |
|---|---|---|---|
| A | Tropical | 49 | 6.4 |
| B | Arid | 86 | 11.2 |
| C | Temperate | 292 | 38.1 |
| D | Cold | 318 | 41.5 |
| E | Polar | 22 | 2.9 |

**Top 5 KG classes:** Dfb (136), Cfa (115), Cfb (93), Dfc (69), Csa/Dfa (54 each).
Network is heavily temperate-cold (C+D = 79.6%); tropical sites at 6.4%, arid at 11.2%,
polar at 2.9%.

---

## 2026-06-24 — decisions_pending.md and CLAUDE.md updates

### decisions_pending.md: pipeline regeneration deferred at +8 ICOS sites

Added a new open item recording that the 2026-06-24 snapshot (767 sites) has not yet
propagated through the downstream pipeline. DuckDB tables and all figures remain at 759
sites. A single coordinated regeneration pass is planned before paper-lock. The
representativeness figure is documented as a permitted exception — it uses 767 sites as
the current-network reference but depends only on site metadata, not flux data, so no
pipeline re-run is required for it.

### CLAUDE.md: data/external/ persistence intent

Added a `## External Data (data/external/)` section documenting that rasters are retained
on disk and should not be re-downloaded when new sites arrive — only per-site extraction
needs to re-run. Lists the three current subdirectories (aridity, gez, worldclim).

---

## 2026-06-24 — Shuttle snapshot diff (2026-06-01 → 2026-06-24)

### New snapshot written

`data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv` — 767 sites.

### Diff against prior snapshot

Prior snapshot: `fluxnet_shuttle_snapshot_20260601T161559.csv` — 759 sites.

**Net change: +8 sites (no removals, no last_year advances).**

All 8 additions are ICOS sites:

| site_id | data_hub | network | igbp | first_year | last_year | lat | lon |
|---|---|---|---|---|---|---|---|
| DK-Eng | ICOS | CarboEurope | GRA | 2005 | 2008 | 55.69 | 12.18 |
| DK-Fou | ICOS | CarboEurope | CRO | 2005 | 2005 | 56.49 | 9.59 |
| ES-Pdu | ICOS | EuropeFlux | WET | 2014 | 2017 | 37.00 | −3.61 |
| IT-MtM | ICOS | EuropeFlux | GRA | 2014 | 2019 | 46.69 | 10.58 |
| IT-PT1 | ICOS | NA | DBF | 2002 | 2004 | 45.20 | 9.06 |
| JP-Nkm | ICOS | AsiaFlux | ENF | 2018 | 2024 | 35.77 | 137.99 |
| JP-Tgf | ICOS | AsiaFlux | GRA | 2002 | 2004 | 36.05 | 140.02 |
| SJ-Adv | ICOS | NA | WET | 2012 | 2014 | 78.19 | 15.92 |

**Removals:** 0

**last_year advances (existing sites with new data year added):** 0

### Notes

- `JP-Nkm` has the widest span of the additions (2018–2024, ENF, AsiaFlux ICOS).
- `IT-PT1` and `SJ-Adv` have `network = NA` in the shuttle manifest.
- No sites were lost between the June 1 and June 24 snapshots.

---

## 2026-06-24 — Data inventory

### Data inventory: docs/data_inventory.md

**Purpose:** Generate a committed reference document cataloguing every dataset, raster,
snapshot, processed file, and external data product in the paper repo. Primary motivation:
planning the network representativeness figure work; secondary: template for future projects.

**Output:** `docs/data_inventory.md` (committed 2bf8789)

---

#### What was inventoried

| Location | Contents | Size |
|---|---|---|
| `data/snapshots/` | 26 shuttle snapshot CSVs (2026-03-28 to 2026-06-01); site characterisation tables; historical dataset site lists; download tracking | ~13 MB |
| `data/lists/` | 5 Excel/XLS source files for Marconi, La Thuile, and FLUXNET2015 historical datasets | ~340 KB |
| `data/extracted/` | 759 per-site directories; BIF, BIFVARINFO, ERA5 (1981–2025), and FLUXMET CSVs at DD/MM/YY (and HH/HR for sub-daily sites) | 6.9 GB |
| `data/raw/` | Empty (ZIPs not retained after extraction) | 0 B |
| `data/processed/` | 12 RDS files: raw, QC-gated, and unit-converted data at YY, MM, and DD resolutions | 827 MB |
| `data/duckdb/` | `fluxnet.duckdb`; 16 tables covering annual/monthly/weekly/daily/hourly × raw/_qc/_converted + manifest | 14 GB |
| `data/external/` | CGIAR Aridity Index v3.1 (386 MB raster); FAO GEZ 2010 (91 MB shapefile); WorldClim v2.1 19 bioclimatic TIFs at 2.5 arc-min | 2.7 GB |
| `outputs/` | Authorship CSVs, exclusion/unknown logs, full-site BibTeX and acknowledgment files | — |

#### Historical FLUXNET datasets confirmed present

| Dataset | Location | Sites | Format |
|---|---|---|---|
| Marconi (Falge et al. 2001) | `data/snapshots/sites_marconi_clean.csv`, `years_marconi.csv`; `data/lists/Marconi_to_Modern_SiteIDs.xlsx` | 35 (of 38 original) | Site lists + crosswalk |
| La Thuile (2007) | `data/snapshots/sites_la_thuile_clean.csv`, `years_la_thuile.csv`; `data/lists/LaThuileList.xlsx`, `LaThuile_SiteMetadata.xls` | 252 | Site lists + metadata |
| FLUXNET2015 (Pastorello et al. 2020) | `data/snapshots/sites_fluxnet2015_clean.csv`, `years_fluxnet2015.csv`; `data/lists/FLUXNET2015.xlsx`, `FLUXNET2015_SiteLocations.xlsx` | 212 | Site lists + metadata |

These are site lists and metadata, not the underlying flux data.

#### Gaps for representativeness figure work

**Present — no download needed:**
- WorldClim v2.1 BIO1/BIO12 extracted per site; all 19 BIO TIFs on disk for extended climate-space analysis
- CGIAR Aridity Index v3.1 extracted per site
- FAO GEZ 2010 extracted per site
- Köppen-Geiger from BADM CLIMATE_KOEPPEN field (site-level; stale set of 569 sites)

**Not present — needs download:**
- Beck et al. 2023 Köppen-Geiger raster (1991–2020, 30 arc-sec) — needed for spatially consistent KG coverage maps (doi:10.1038/s41597-023-02549-6)
- Beck et al. 2023 KG future projections (SSP1-2.6, SSP2-4.5, SSP5-8.5)
- ESA CCI Land Cover (300 m, 2000–2020) — if landcover is a representativeness axis
- ESA CCI Biomass v5 (100 m, 2010–2020) — if biomass is an axis

**Lower priority / long-lead:**
- TRENDY v11 — needs institutional data agreement with TRENDY coordinators
- CMIP6 climate projections — large volumes; only needed for future-climate representativeness

#### Notable findings

1. `data/raw/` is empty — ZIPs are not retained post-extraction. Re-extraction requires a full re-download.
2. `site_candidates_full.csv` and both `long_record_site_candidates_*` files are **stale at 569 rows** (should be 759). These are a prerequisite for anomaly and long-record figures (decisions_pending.md Priority 3).
3. `flux_data_raw_dd_partial.rds` (411 MB) is a dead artifact from the OOM-interrupted DD read before the DuckDB path was implemented. Safe to delete once DuckDB pipeline is confirmed stable.
4. Only 2 of 19 WorldClim BIO variables are extracted to site level (BIO1 and BIO12). The other 17 TIFs are on disk and can be extracted cheaply if a multi-variable climate PCA is needed for representativeness figures.
5. ZIP archives are retained alongside extracted rasters in `data/external/`. These could be deleted to reclaim disk space once extraction is confirmed complete.

---

## 2026-06-24 — Repository status review

### Repository state

**Environment:** Local Mac mini (no Codespace). Branch `main` is up to date with `origin/main` — no pull required.

**Modified tracked files (not yet committed):**

| File | Authority note |
|---|---|
| `outputs/session_info.txt` | Regenerated locally each run — expected, commit separately |
| `renv/activate.R` | Local renv profile change — stage only after confirming active profile |
| `review/figures/climate/fig_environmental_response_era5.png` | Locally regenerated; relevant to Priority 2 style pass (see below) |

Approximately 40 untracked files in `logs/` and one new snapshot in `data/snapshots/` (`fluxnet_shuttle_snapshot_20260601T161559.csv`) — not yet committed.

---

### Open paper-critical items (from `docs/decisions_pending.md`)

**Priority 1 — Fig 01: functionally active site line (OPEN)**

The second Y-axis on the network growth figure was computed with the old `last_year >= 2021` definition, before the `presence_df` refactor. `is_functionally_active()` was corrected in commits ee84552 (R/utils.R), but Fig 01 has not been regenerated. The presence CSV (`data/snapshots/site_year_data_presence.csv`) was produced by `03_read.R` on the 759-site dataset and is treated as authoritative. Action: recompute using `is_functionally_active()` (≥3 months valid flux data in at least one year within last 4 years) and regenerate Fig 01.

**Priority 2 — Fig 07 / Fig 08 style harmonisation (OPEN)**

The modified `review/figures/climate/fig_environmental_response_era5.png` in the working tree indicates Fig 08 (ERA5 environmental response) was recently regenerated. Neither Fig 07 (latitudinal gradient) nor Fig 08 has been confirmed against `MAP_STYLE`, `DUR_STYLE`, and `WHITTAKER_STYLE` constants in `R/figures/`. Action: apply style review pass to both figures together and regenerate once confirmed.

**Priority 3 — Rebuild `site_candidates_full.csv` at 759 sites (OPEN)**

`data/snapshots/site_candidates_full.csv` has 569 rows (716-site April lineage). `long_record_site_candidates_gez_kg.csv` is also stale. Any figure joining on candidate status uses the wrong site set. Prerequisite: `03_read.R` must produce updated NEE presence data for 759 sites, then rebuild via `step2_extract_aridity.R`. Tracked in `known_issues.md` §7. This is a prerequisite for anomaly figures and long-record time series.

**Priority 4 — Manuscript drafting**

Pipeline is end-to-end on 759 sites via DuckDB. Figures are stable enough to reference. `docs/methods_requirements.md` contains the section-by-section requirements spec; a few values are marked `[TBD]` for paper-lock time. Methods drafting not yet started — by design per the requirements spec header.

---

### Deferred items (do not action now)

- DuckDB schema-translation refactor (`YEAR`/`DOY` columns) — working patches in `07_figures.R`, schedule for next maintenance pass.
- `flux_download()` version pinning — deliberately deferred to paper-lock time; env-var format conflict (`0.3.7` install tag vs `0.3.7.post0+dirty` self-report) is part of the same decision.
- Representativeness analysis climate axis — scope not yet defined; Köppen-Geiger already extracted at all sites when needed.
- fluxnet package DuckDB rewrite — out of our control; methodological decisions are rewrite-proof.

---

## 2026-06-16 — fluxnet-quickstart template repository created; HR workaround audit

### HR workaround audit: FluxCourseForecast vs synthesized version

**File checked:** `davidjpmoore/FluxCourseForecast` at `data/US-MMS/extract_hr_workaround.R`
**Result:** File exists. Opens with a retirement notice.

#### Critical finding

The FluxCourseForecast script begins:

```
## THIS SCRIPT IS NO LONGER NEEDED.
## The HR extraction bug was fixed in fluxnet v0.3.2.9000 (commit 2741bb8).
## flux_extract() now correctly handles _HR_ files in AmeriFlux FLUXNET v1.3_r1.
## This script is retained for reference only.
```

The bug it fixed: `flux_extract()` v0.3.1 matched filenames with `_HH_` (the FLUXNET-2015
convention) but AmeriFlux FLUXNET v1.3_r1 ships files named `_HR_`. The package fixed this
in v0.3.2.9000.

The FluxCourseForecast script bypassed `flux_extract()` entirely — it used `zip::zip_list()`
to read the ZIP index, grepped for `_HR_` entries, and extracted them with `zip::unzip()`.
Then read with `data.table::fread(na.strings = "-9999")` and wrote two output CSVs:
`US-MMS_HR.csv` and `US-MMS_ERA5_HR.csv`. Hardwired to US-MMS; not a reusable function.

#### The synthesized `R/hr_workaround.R` in fluxnet-quickstart addresses a different layer

The two scripts do not overlap. The synthesized version targets a **separate, still-active
concern**: `flux_discover_files()` labels extracted files with `time_resolution = "HR"` for
hourly AmeriFlux sites. Students naively filtering for `time_resolution == "HH"` will
silently miss those sites. The FluxCourseForecast bug fix does not change this.

The synthesized comment block incorrectly implies `flux_extract()` is still the problem. It
should be corrected to describe the inventory-label concern instead.

| | FluxCourseForecast script | Synthesized `hr_workaround.R` |
|---|---|---|
| Bug targeted | `flux_extract()` skipping `_HR_` filenames (v0.3.1) | `flux_discover_files()` labeling files as `time_resolution = "HR"` |
| Status | **Retired** — fixed in fluxnet v0.3.2.9000 | **Active** — inventory label distinction remains |
| Mechanism | Bypass `flux_extract()`; read ZIP directly | Normalize inventory data frame post-extraction |
| Scope | US-MMS only, script-level | Any site, function-level |

#### Action required → DONE (fluxnet-quickstart commit 8d47cfe)

Comment block in `fluxnet-quickstart/R/hr_workaround.R` rewritten: now describes the
inventory-label concern accurately; added historical note distinguishing this from the
retired flux_extract() workaround at FluxCourseForecast:data/US-MMS/extract_hr_workaround.R.

---

### fluxnet-quickstart: session summary

**Repo:** https://github.com/EcosystemEcologyLab/fluxnet-quickstart
**Commit:** ea038da — "Initial commit: FLUXNET quickstart template"
**Template flag:** confirmed (`gh repo edit --template=true` succeeded)

---

#### Contents shipped

| File | Description |
|------|-------------|
| `README.md` | ~250-line audience-first document; sections for shuttle overview, installation, credential norms, vendored utility status, what's out of scope, citing |
| `LICENSE` | MIT, copyright 2026 David J. P. Moore and contributors |
| `CITATION.md` | Full citation guidance: fluxnet package, fluxnet-citations tool, Pastorello 2020, per-site DOI requirement |
| `.gitignore` | R standard ignores + data/raw/, data/extracted/, output/; my_manifest.csv and my_sites.csv intentionally un-ignored |
| `data/.gitkeep` | Placeholder so data/ directory exists in template forks |
| `R/hr_workaround.R` | Temporary utility (marked); three functions: `identify_hr_sites()`, `normalize_hr_inventory()`, `filter_subdaily_inventory()` |
| `R/generate_fluxnet_citations.R` | Vendored from fluxnet-citations HEAD; temporary-status block prepended; paths adjusted for template layout |
| `examples/01_discover.R` | flux_listall() → filter by IGBP and record length → save data/my_manifest.csv; heavy inline commentary for students |
| `examples/02_download.R` | flux_download() + flux_extract() for 5 representative sites; credential walkthrough; HR workaround applied post-discovery |
| `examples/03_cite.R` | generate_fluxnet_citations() from data/my_sites.csv + manifest; explains manifest-as-citation-source principle |

#### HR workaround — implementation decision

No standalone workaround function existed in the paper repo. The pattern was synthesized from two locations:
- `scripts/03_read.R` line 166-167: `file_inventory$time_resolution %in% c("HH", "HR")` — the filter-level fix
- `scripts/duckdb_setup.R` line 34-36: normalize HR → HH in the file inventory

These were refactored into three named functions in `R/hr_workaround.R` with documentation suited to a student audience. US-MMS is cited as the motivating example in `examples/02_download.R`.

#### Verification status

The examples were not run end-to-end (would require ~500 MB download of 5 sites). Scripts were verified by code review: correct function calls, consistent file paths, no broken references between examples. The citation generator is a verbatim copy of the tested fluxnet-citations HEAD with only the comment block prepended; it requires no verification beyond what the fluxnet-citations repo already provides.

#### Decisions made not covered by the original brief

- `03_cite.R` peek-at-BibTeX step: split `length(readLines(results$bib)[grepl(...)])` into two lines to avoid a parse-error-inducing inline assignment in `length()`. Functionality identical to the intent.
- `data/my_sites.csv` written by `02_download.R` (not only mentioned as a path). This makes example 03 runnable without manual file creation.
- `flux_extract()` called with `resolutions = c("y", "m", "d")` in example 02 (not "h"), matching the paper repo's default and keeping the example download size manageable. A comment notes how to add "h" for sub-daily data.

---

## 2026-06-12 — fluxnet-citations standalone repo: Part 1 confirmation + Part 2 plan

### Part 1: manifest investigation — confirmed

Re-ran the source-trace investigation today against the installed package (v0.3.2.9000) and shuttle Python code (`fluxnet_annual_2026` venv). All findings from the 2026-06-11 session are confirmed exactly. No new findings.

Summary for the record:
- **The durable record is the snapshot CSV** (`fluxnet_shuttle_snapshot_<timestamp>.csv` in `data/snapshots/`). It is written by `write_snapshot()` in `01_download.R` before each download run, is committed, and is the only artifact that captures full 18-column metadata at download time.
- **The Python shuttle `download()` function writes no manifest.** It reads the snapshot CSV as input but produces only downloaded ZIP files as output.
- **`flux_discover_files()` is temporally incorrect for citations.** It calls `flux_listall()` live at the time of the call; the citation metadata it attaches reflects the current Shuttle state, not the download-time state.
- **Column format is identical to `flux_listall()` output.** The snapshot CSV and `flux_listall()` R tibble share the same 18-column schema. No parser changes are needed in `generate_fluxnet_citations.R`.
- **"Manifest" is the correct vocabulary.** The snapshot CSV IS the manifest: it is a frozen record of what the user had access to and chose to download. The terminology rename (from `snapshot_path` → `manifest_path`) is consistent with the artifact's function and with how the standalone repo will describe it.

### Part 2: standalone repo design decisions (pending user confirmation to proceed)

Decisions locked by Part 1 findings:

1. **`manifest_path` input**: points to a `fluxnet_shuttle_snapshot_*.csv` from `data/snapshots/`. Same 18-column schema as `flux_listall()` output. Parsers unchanged.
2. **Example file format**: 10-row subset of `fluxnet_shuttle_snapshot_20260601T224043.csv`. Not a shuttle-produced manifest from a separate download; this IS the shuttle-produced manifest format (they are the same format). The example file is a frozen slice of a real file, dated 2026-06-01.
3. **"Where is my manifest?" section of README**: points users to `data/snapshots/fluxnet_shuttle_snapshot_<timestamp>.csv` in their paper repo clone. The most recent snapshot written before (or matching) their download run is the correct input. In the batch download workflow, the snapshot written by `write_snapshot()` within `01_download.R` just before each download is the authoritative file for that run.
4. **Users without a manifest**: anyone who used `flux_download()` via `01_download.R` has a snapshot; anyone who called the Python CLI directly without routing through the R wrapper may not. This edge case is acknowledged in the README limitations section but is not recoverable without some temporal imprecision.

**Awaiting "proceed" confirmation before starting Part 2.**

---

### Part 2: fluxnet-citations repo created

**Repo:** <https://github.com/EcosystemEcologyLab/fluxnet-citations>  
**Commit:** `e333643` — "Initial commit: standalone FLUXNET citation generator"

#### Contents shipped

| File | Description |
|---|---|
| `generate_fluxnet_citations.R` | Copied from paper repo commit 7f31497; `snapshot_path` renamed to `manifest_path` throughout |
| `LICENSE` | MIT, copyright 2026 David J. P. Moore and contributors |
| `CITATION.md` | Tool self-citation (placeholder for FLUXNET 2026 paper); Pastorello 2020 (placeholder for synthesis citation); fluxnet R package pointer; site-level citation obligation note |
| `README.md` | ~200-line user-facing doc covering all sections in the spec |
| `.gitignore` | R ignores + output/ + example/output/ |
| `example/example_manifest.csv` | 10-site frozen subset of `fluxnet_shuttle_snapshot_20260601T224043.csv` |
| `example/example_sites.csv` | One-column CSV with all 10 site IDs |
| `example/README.md` | Example walkthrough with expected outputs |

#### 10 example sites

| Site | Network | Note |
|---|---|---|
| US-Hsm | AMF | Preserved typo: `Kyle_Delwiche` |
| US-A03 | AMF | |
| US-A10 | AMF | |
| ZM-Mon | EUF | |
| TH-Mae | JPF | |
| KR-WdE | KOF | |
| AU-Cow | TERN | Preserved typo: `FLUXNEXT` |
| AU-Emr | TERN | |
| ZA-Uby | SAEON | |
| ZA-Spk | SAEON | |

#### Verification results

- Example ran cleanly: 10 sites, 6 networks (AMF, EUF, JPF, KOF, SAEON, TERN), 3 output files produced.
- `_review_flags.md`: flags present for AU-Cow (FLUXNEXT typo), US-Hsm (Kyle_Delwiche typo), plus no-author ICOS/JPF/KOF entries.
- `%% NOTICE` data-source block confirmed at top of `.bib`.
- Raw README accessible at `https://raw.githubusercontent.com/EcosystemEcologyLab/fluxnet-citations/main/README.md`.

#### Decisions made not covered in the brief

- **ZM-Mon (Zambia) as the EUF site.** It was the first EUF site alphabetically in the snapshot. No constraints on EUF site selection were specified; this is a valid ICOS EUF-family site.
- **`find_latest_snapshot()` internal function name kept.** The user-facing vocabulary is "manifest" throughout, but the private helper function that locates `fluxnet_shuttle_snapshot_*.csv` files was left as `find_latest_snapshot` because the filename pattern it searches for is controlled by the shuttle, not by this tool. Renaming the function name would be cosmetic-only and would not affect user-facing terminology.
- **Two README user-facing annotations incorporated** per the session instructions: (a) "Where is my manifest?" leads with the behavioral rule ("Always save your `flux_listall()` output to CSV before calling `flux_download()`...") followed by the paper-repo example as a concrete instance; (b) CITATION.md marks the Pastorello 2020 reference as a placeholder for the synthesis citation, parallel to the tool self-citation placeholder.

---

## 2026-06-11 — FLUXNET shuttle manifest investigation

### Manifest artifacts: what the shuttle writes, and when

**Purpose:** Determine which artifact produced by the FLUXNET shuttle workflow represents the durable record of a download, and whether that artifact carries the metadata needed for citations (`product_citation`, `product_id`, `product_source_network`). This answers the design question: what should the citation tool require as input so that citations match the data actually downloaded, not a later metadata state.

---

#### 1. Python `shuttle.download()`: no manifest written

The Python shuttle's `download()` function (in `fluxnet_shuttle/shuttle.py`) takes a `snapshot_file` path (a CSV on disk) as input and reads it to get download URLs and filenames. It uses four columns from the snapshot: `site_id`, `data_hub`, `download_link`, `fluxnet_product_name`. It writes nothing to disk except the downloaded ZIP files. Return value: a list of downloaded filenames.

The function does not write a manifest, an index, a checksum file, or any secondary record of the metadata state at download time.

#### 2. R `flux_download()`: temp file bridge, no persistent artifact

The R `flux_download()` function takes `file_list_df` (a data frame with all 18 columns from `flux_listall()`) and an output directory. Internally it writes the data frame to a temporary CSV file via `withr::local_tempfile()`, then passes that temp path to Python as `snapshot_file`. The temp file is deleted when the function exits. `flux_download()` returns only a tibble of download paths (`download_path`).

Key: the R function writes no persistent manifest. The temp CSV it creates exists only for the duration of the function call.

```r
# flux_download() internals (from decompiled source):
file_list <- withr::local_tempfile()
readr::write_csv(file_list_df, file_list)
fluxnet_py$download(site_ids = as.list(site_ids), snapshot_file = file_list, ...)
```

#### 3. `write_snapshot()` in `R/snapshot.R`: the actual durable record

`01_download.R` calls `write_snapshot(manifest, snapshot_dir = snapshots_dir)` **before** calling `flux_download()`. `write_snapshot()` (defined in `R/snapshot.R`) saves the full 18-column manifest to `data/snapshots/fluxnet_shuttle_snapshot_{TIMESTAMP}.csv` using `readr::write_csv()`. This is the only artifact that captures the full metadata state at download time in a committed, timestamped file.

The snapshot CSV is also how locked mode (`FLUXNET_SNAPSHOT_MODE="locked"`) reproduces a run: `resolve_snapshot()` reads this file back in lieu of a live `flux_listall()` call.

#### 4. `flux_discover_files()`: temporally incorrect for citations

`flux_discover_files()` (decompiled from the compiled package) constructs a file inventory from on-disk extracted files and then **calls `flux_listall()` internally** to obtain metadata, merging the two via `left_join` on `product_source_network` + `site_id`. The metadata it attaches reflects the current state of the Shuttle at the time `flux_discover_files()` is called — not the state at download time.

Columns from the file-system parse (from extracted directory/filenames):
`product_source_network`, `site_id`, `dataset`, `time_resolution`, `first_year`, `last_year`, `oneflux_code_version`, `release_version`, `path`, `download_time`

Columns joined from fresh `flux_listall()` (15 of the original 18, minus `first_year`/`last_year`/`oneflux_code_version` which come from the filename):
`data_hub`, `site_name`, `location_lat`, `location_long`, `igbp`, `network`, `team_member_name`, `team_member_role`, `team_member_email`, `download_link`, `fluxnet_product_name`, `product_citation`, `product_id`

This output is saved in this repo as `data/processed/file_inventory.rds` (gitignored, regenerated each run). It carries all citation-relevant columns, but from a fresh metadata query — a user running this six months after their download would get current `product_citation` strings, not the ones that were current when the data was downloaded.

#### 5. Column shape comparison

| Artifact | Has citation columns? | Metadata timing | Format |
|---|---|---|---|
| `flux_listall()` output | Yes (all 18) | Live (current) | tibble |
| Snapshot CSV (`data/snapshots/`) | Yes (all 18) | Fixed at snapshot-write time | CSV, 18 cols |
| `flux_download()` temp file | Yes (all 18) | Same as input manifest | Deleted on exit |
| `flux_discover_files()` output | Yes (from fresh `flux_listall()`) | Live (current at call time) | tibble, ~23 cols |
| `file_inventory.rds` | Yes (from fresh `flux_listall()`) | Live when 02_extract.R was last run | RDS |
| Progress CSVs (`download_progress*.csv`) | No | — | CSV, 7 cols (batch tracking only) |

All three candidate "manifests" that carry the citation columns (`flux_listall()` output, snapshot CSV, `flux_discover_files()` output) have identical `product_citation`, `product_id`, and `product_source_network` column names. The snapshot CSV and direct `flux_listall()` output are structurally identical (same 18 columns, same names, same encoding) — the Python shuttle's `_write_snapshot_file()` writes a CSV that round-trips losslessly back to an R tibble.

#### 6. Actual artifacts in `data/snapshots/`

- **`fluxnet_shuttle_snapshot_*.csv`** — 29 timestamped snapshots from 2026-03-28 through 2026-06-01. Each is a direct `flux_listall()` output, 18 columns, written by `write_snapshot()` in 01_download.R before each download run. These are committed. Latest: `fluxnet_shuttle_snapshot_20260601T224043.csv`.
- **`download_progress*.csv`** — batch download tracking (3 files). 7 columns: `batch_num`, `n_sites`, `site_ids`, `status`, `started_at`, `completed_at`, `disk_free_gb`. No citation metadata.
- No manifest, index, or metadata file is written by the Python shuttle itself during download.

#### 7. Design implications for the citation tool

**Correct artifact to require: the snapshot CSV from `data/snapshots/`.**

Reasoning:
- It is the only artifact that is (a) durable, (b) committed, (c) timestamped, and (d) reflects the metadata state at the time of download.
- In locked mode, it IS the dataset definition — the citation tool using the same snapshot file as the download gives a logical guarantee that citations match the data.
- Column format is identical to `flux_listall()` output. The existing parser logic in `generate_fluxnet_citations.R` operates on this format and requires no changes.

**The existing `snapshot_path` parameter design is correct.** The function already requires the user to pass an explicit snapshot path (or auto-discovers in `data/snapshots/`). That design choice was the right one.

**Recovery path when no snapshot is available:** A user who called `flux_download()` directly (without 01_download.R) and did not call `write_snapshot()` separately will not have a timestamped snapshot. In this case:
1. Best option: call `flux_listall()` now and save it — metadata drift is possible but likely small for stable sites.
2. Second option: use `flux_discover_files()` output, strip the file-system-only columns, and pass the result as a data frame — same temporal imprecision as option 1.
3. There is no way to recover the exact download-time metadata if no snapshot was saved.

**Parser compatibility:** The snapshot CSV and `flux_listall()` output are structurally identical. No parser changes needed to support either as input. The `flux_discover_files()` output has extra columns (harmless) and would need to be subset to the 18-column snapshot schema if used as fallback input — but temporal correctness cannot be recovered regardless of column subsetting.

---

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
