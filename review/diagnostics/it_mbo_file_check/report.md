# IT-MBo file check: the monthly read was wrong, and here is exactly why

**Verdict.** Our circulated IT-MBo monthly numbers (4,017 and 4,058 mm for 2013-01 and
2013-06) are wrong because every script that produced them read
`data/extracted/ICOS_IT-MBo_FLUXNET_2003-2024_v1.3_r1/ICOS_IT-MBo_FLUXNET_FLUXMET_MM_2003-2024_v1.3_r1.csv`
-- a local extraction from 2026-06-01 that does not match the FLUXNET product currently
distributed under PID `enS2fTzGG_9PS5-51hqet8iH`. Two independent fresh downloads of that
exact PID, both done today, are byte-identical to each other, differ from the on-disk copy,
and reproduce Dario's numbers (1.856 and 3.134 mm/day, i.e. 57.5 and 94.0 mm for those two
months) exactly. This is a local file-provenance defect, not a units, day-weighting, or
arithmetic error in this repository's own code.

**sha256 of the file our scripts read** (for direct comparison against Dario's copy):
`6d2a4318a40d54902ce7e80bbfe6d3c707ea9c033e4619f2c23bb9ffcf7533d2`
(`ICOS_IT-MBo_FLUXNET_FLUXMET_MM_2003-2024_v1.3_r1.csv`, 712,507 bytes).

**PID**: `enS2fTzGG_9PS5-51hqet8iH` (ICOS handle), product v1.3, confirmed via a live
`flux_listall()` call immediately before triggering the new download in this task --
unchanged from the PID recorded in `it_mbo_bug_hunt/table_d4_pid_provenance.csv` earlier today.

---

## 1. Every MM-named file in the IT-MBo download, and which script reads which

Two extracted directories exist on disk for IT-MBo:

- **`old_dir`** = `data/extracted/ICOS_IT-MBo_FLUXNET_2003-2024_v1.3_r1/` -- mtime 2026-06-01,
  the pipeline's ordinary y/m/d extraction (`FLUXNET_EXTRACT_RESOLUTIONS="y m d"`). Contains
  every MM file that exists on disk for this site.
- **`fresh_dir`** = `data/extracted/ICOS_IT-MBo_FLUXNET_2003-2025_v1.3_r1/` -- mtime
  2026-09-20 (today), created by `it_mbo_bug_hunt.R`'s single-site HH-only re-download.
  Contains **zero** MM-named files -- only HH files were ever extracted from its raw zip to
  disk (the raw zip itself does contain DD/MM/WW/YY members; see Section 3).

| directory | file | bytes | sha256 |
|---|---|---|---|
| `old_dir` | `ICOS_IT-MBo_FLUXNET_BIFVARINFO_MM_2003-2024_v1.3_r1.csv` | 314,894 | `c327bf5de95042047368e206fdff52203c0314fcbb4b8bc12101e891d3362494` |
| `old_dir` | `ICOS_IT-MBo_FLUXNET_ERA5_MM_1981-2024_v1.3_r1.csv` | 41,912 | `6add0ca3c92a5aa4c92299a9bfb4468b65615e6b44e92d9ef32a142caf54b97d` |
| `old_dir` | `ICOS_IT-MBo_FLUXNET_FLUXMET_MM_2003-2024_v1.3_r1.csv` | 712,507 | `6d2a4318a40d54902ce7e80bbfe6d3c707ea9c033e4619f2c23bb9ffcf7533d2` |

Full table: `table_d1_mm_files.csv`.

**Which script reads which, traced by grep against each script (not re-executed -- several
have side effects or long runtimes out of scope here):**

| script | resolution logic |
|---|---|
| `era5_precip_units.R` | `list.files("data/extracted", pattern=paste0("_", site_id, "_FLUXNET_ERA5_MM_.*\\.csv$"))` -- single glob, only match at the time it ran |
| `era5_precip_units_v2.R` | same pattern, parameterised by resolution |
| `era5_precip_units_v3_partA.R` | `find_dir()`/`find_file()`, first dir match -- only one dir existed when it ran |
| `era5_precip_units_v4.R` | does not read raw MM files -- reuses v2's/v3's tables |
| `era5_reference_plots.R` | does not read raw MM files -- reuses v3's table |
| `era5_cumulative_test.R` | `site_dir_lookup[[site_id]]` on a name-keyed vector, first match -- only one dir existed when it ran |
| `era5_share_for_coordination.R` | identical `site_dir_lookup[[site_id]]` pattern (lines 46-56) -- only one dir existed when it ran (2026-09-18) |
| `it_mbo_bug_hunt.R` | `find_dirs()` matches **both** directories; `find_file()` returns the first dir containing the pattern -- `old_dir` wins for MM/DD/YY (checked first, has the file), `fresh_dir` wins for HH (only dir with an HH file) |
| `it_mbo_parsimony.R` | identical to `it_mbo_bug_hunt.R` -- same `old_dir`-for-MM/DD/YY, `fresh_dir`-for-HH split |

Full table: `table_d1_script_trace.csv`.

**Important correction of scope**: every script that ran *before* today (v1 through
`era5_share_for_coordination.R`) was not exercising any directory-selection bug -- `old_dir`
was the *only* IT-MBo directory that existed when they ran. They simply read the only copy on
disk, and that copy is wrong. `it_mbo_bug_hunt.R` and `it_mbo_parsimony.R`, run today *after*
`fresh_dir` already existed, are a different case (see the implication section at the end).

## 2. Raw, unparsed text -- header and the 2013-01/2013-06 rows, `old_dir`'s FLUXMET_MM file

Full text: `raw_header_and_2013_rows_olddir.txt`. Column positions (1-indexed, confirmed by
splitting the header, not assumed): **`P_ERA` = column 47, `P_F` = column 48, `P_F_QC` =
column 49**, of 340 total columns. Both data rows have exactly 340 fields, matching the
header -- this rules out a column-shift/misaligned-row corruption.

```
201301,...,P_ERA=39.426,P_F=4017.367,P_F_QC=1.0,...
201306,...,P_ERA=66.586,P_F=4058.441,P_F_QC=1.0,...
```

## 3. Re-download, byte-for-byte comparison

Two independent fresh downloads of PID `enS2fTzGG_9PS5-51hqet8iH` were compared against the
on-disk copy:

| copy | bytes | sha256 |
|---|---|---|
| `old_dir` on-disk (read by our scripts) | 712,507 | `6d2a4318a4...533d2` |
| earlier-fresh (today's `it_mbo_bug_hunt.R` zip -- its bundled MM member, never previously extracted, extracted now) | 747,140 | `b44a360e9d...d44f8` |
| new-fresh (a brand-new, independent download triggered by this task) | 747,140 | `b44a360e9d...d44f8` |

Full table + full hashes: `table_d3_hash_comparison.csv`.

**The two fresh downloads are byte-identical to each other (sha256 match exactly) and differ
from the on-disk copy.** This rules out a caching artifact specific to today's earlier
download -- the currently-distributed product is stable and consistent, and it is not what
`old_dir` contains. The brand-new download's 2013-01/2013-06 values:

```
TIMESTAMP  P_ERA   P_F   P_F_QC
201301     1.856   1.856   0
201306     3.134   3.134   0
```

-- exact match to Dario's numbers.

(Both fresh copies extracted into `data/raw/it_mbo_file_check_scratch/` -- gitignored,
consistent with Hard Rule 4; not committed. The sha256/byte-size table above is the committed
record.)

## 4. Arithmetic trace for "4,017"

Full trace: `d4_arithmetic_trace.txt`. Raw value read: `P_F = 4017.367` (column 48,
`old_dir`'s FLUXMET_MM file, `TIMESTAMP=201301`). **No multiplier was applied** -- `P_F` at MM
resolution is already a monthly total in the distributed product (unlike `P_ERA`, which our
own `read_era5_mm()`/`read_fluxmet_mm()` day-weighting multiplies by days-in-month). So
"4,017" is the file's raw value, rounded for display, quoted unmultiplied. The file's own
value is wrong; nothing downstream of reading it is.

**Where "4,017"/"4,058" were actually quoted** (grep, `-E` -- grep's default BRE treats `?` as
literal, not optional, and an initial `-E`-less run of this check silently found nothing):

- `review/diagnostics/it_mbo_parsimony/report.md:98` -- *"June 2013**: `P_F_mm_monthly` =
  4,017 and 4,058 mm/month respectively..."*
- `review/diagnostics/it_mbo_parsimony/report.md:110-111` -- the two-row table giving
  `4,017.4`/`4,058.4` alongside the correct HH-summed values (`57.5`/`94.0`) and the ratios
  (`69.8x`/`43.2x`).

`it_mbo_parsimony/report.md` had already correctly identified these two months as a *"second,
independent piece of"* localized defect, distinct from the general ~21.25x DD-vs-HH
inflation, and never attributed the values to a units or arithmetic bug in our own code --
that framing holds. What it could not yet know is *why* the file disagrees with the live
product; this task supplies that (Section 3).

## 5. Where the 69.8-fold difference enters

Full text: `d5_verdict_sentence.txt`.

> The 69.8-fold difference enters because every script that circulated an IT-MBo monthly
> number read `data/extracted/ICOS_IT-MBo_FLUXNET_2003-2024_v1.3_r1/ICOS_IT-MBo_FLUXNET_FLUXMET_MM_2003-2024_v1.3_r1.csv`
> -- a local extraction from 2026-06-01 that does not match the FLUXNET product currently
> distributed under PID `enS2fTzGG_9PS5-51hqet8iH` (confirmed identical across two
> independent fresh downloads today, both of which reproduce Dario's numbers exactly) -- and
> that stale local copy's `P_F` column is corrupted at a sparse subset of months, including
> January and June 2013 specifically, independently of the file's separate, uniform ~21.24x
> `P_ERA` problem; this is a local file-provenance issue, not a units, day-weighting, or
> arithmetic error in this repository's own code, which applies the correct (and, for `P_F`
> at MM resolution, correctly zero) multiplier to whatever value the file hands it.

(Arithmetic: old raw `P_F` for 2013-01 = 4017.367 mm/month; new download's `P_F` = 1.856
mm/day x 31 days = 57.536 mm/month; ratio = 69.82.)

---

## Implication for today's earlier `it_mbo_bug_hunt.R` finding (not applied -- flagged only)

`it_mbo_bug_hunt.R`'s D2 step reported P_ERA's DD/MM/YY-resolution branch as a near-constant
~21.25x multiple of the HH-resolution branch, at IT-MBo specifically, and concluded this was
a defect in the *distributed product's* aggregation/regression step. That comparison
necessarily read DD/MM/YY from `old_dir` (the only directory with those files) and HH from
`fresh_dir` (the only directory with HH) -- **it was comparing the stale, corrupted local
extraction against a genuinely fresh one, not two resolution branches of the same
distributed product.**

Directly checking this: `old_dir`'s `P_ERA_MM` against the brand-new download's `P_ERA_MM`,
every common month (264 months, 2003-01 through 2024-12):

**median ratio = 21.2431, range [20.8947, 21.4583]** -- table: `table_implication_old_vs_new_era_mm.csv`

This is the same ~21.25x factor `it_mbo_bug_hunt.R` attributed to a DD-vs-HH product defect.
It is at minimum equally well explained -- and more parsimoniously explained, since it now has
an identified, demonstrated mechanism (a stale local file) rather than a hypothesized
upstream aggregation bug -- by `old_dir` simply being wrong, uniformly, across its entire
`P_ERA` column, for reasons this task does not determine (whether that reflects a
since-corrected upstream reprocessing between 2026-06-01 and today, a corruption during the
original download/extraction, or something else is not established here and is not asserted).
**This is a revision candidate for `it_mbo_bug_hunt/report.md`'s DD-vs-HH conclusion, stated
here only -- `it_mbo_bug_hunt/report.md` and `it_mbo_parsimony/report.md` are not edited by
this task**, per its read-only scope.

Two separate corruption patterns are present in `old_dir`, worth keeping distinct:

1. **`P_ERA`**: uniform ~21.24-21.27x inflation, every one of 264 overlapping months checked
   -- looks systematic, not sparse.
2. **`P_F`**: identical to the fresh copy for most months (ratio 1.000), but spikes at
   scattered specific months (200301: 3.35x; 200307: 8.72x; 200308: 12.10x; 201301:
   69.8x; 201306: 43.2x) -- a different, sparser pattern than `P_ERA`'s.

## Files

- `scripts/diagnostics/it_mbo_file_check.R`
- `table_d1_mm_files.csv`, `table_d1_script_trace.csv`
- `raw_header_and_2013_rows_olddir.txt`
- `table_d3_hash_comparison.csv`
- `d4_arithmetic_trace.txt`, `d5_verdict_sentence.txt`
- `table_implication_old_vs_new_era_mm.csv`

All `.meta.json` companions present per CLAUDE.md's Output Metadata requirement.
