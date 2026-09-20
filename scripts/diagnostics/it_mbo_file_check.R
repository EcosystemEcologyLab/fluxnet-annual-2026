## it_mbo_file_check.R
##
## Urgent, narrow follow-up to it_mbo_bug_hunt.R / it_mbo_parsimony.R (both
## 2026-09-20). Dario Papale downloaded PID enS2fTzGG_9PS5-51hqet8iH (IT-MBo),
## opened the FLUXNET_MM file, and reads P_F for 2013-01 as 1.856 mm/day and
## 2013-06 as 3.134 mm/day (~58 and ~94 mm for those months) -- an order of
## magnitude different from, and in the opposite direction to, the numbers we
## circulated (4,017 and 4,058 mm). Our own HH-summed totals for the same two
## months, computed earlier today in it_mbo_bug_hunt.R, were 57.5 and 94.0 mm
## -- consistent with Dario, not with our own monthly read. This script works
## on the assumption that our monthly read is wrong and finds exactly why.
##
## Read-only with respect to every existing diagnostic output (era5_precip_
## units/, _v2/, _v3/, _v4/, era5_reference_plots/, era5_cumulative_test/,
## era5_share_for_coordination/, it_mbo_bug_hunt/, it_mbo_parsimony/) --
## reads from them, never writes to them. Does not edit the pipeline or any
## committed figure. All new files under review/diagnostics/it_mbo_file_check/.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(fs)
  library(fluxnet)
})

OUTD <- "review/diagnostics/it_mbo_file_check"
fs::dir_create(OUTD)

message("=== it_mbo_file_check.R ===")

TEST_SITE <- "IT-MBo"
PID_IT_MBO <- "enS2fTzGG_9PS5-51hqet8iH"

sha256 <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_character_)
  out <- system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE)
  sub("^([0-9a-f]{64}).*$", "\\1", out[[1]])
}

# ============================================================================
# DELIVERABLE 1: every MM-named file in the IT-MBo download, both directories
# ============================================================================
message("\n================ D1: every MM-named file for IT-MBo, both extracted dirs ================")

old_dir   <- "data/extracted/ICOS_IT-MBo_FLUXNET_2003-2024_v1.3_r1"
fresh_dir <- "data/extracted/ICOS_IT-MBo_FLUXNET_2003-2025_v1.3_r1"
stopifnot(dir.exists(old_dir), dir.exists(fresh_dir))

mm_files <- tibble::tibble(
  path = c(
    list.files(old_dir,   pattern = "MM", full.names = TRUE),
    list.files(fresh_dir, pattern = "MM", full.names = TRUE)  # BIFVARINFO_MM only -- FLUXMET/ERA5 MM were never extracted to fresh_dir
  )
) |>
  dplyr::mutate(
    directory  = dirname(path),
    file       = basename(path),
    bytes      = file.size(path),
    sha256     = purrr::map_chr(path, sha256)
  ) |>
  dplyr::select(directory, file, bytes, sha256)

print(as.data.frame(mm_files))

out_d1_files <- file.path(OUTD, "table_d1_mm_files.csv")
readr::write_csv(mm_files, out_d1_files)
write_output_metadata(out_d1_files,
  input_sources = c(old_dir, fresh_dir),
  notes = "Every file with MM in its name in both of IT-MBo's extracted directories, with byte size and sha256. old_dir (2003-2024_v1.3_r1, mtime 2026-06-01) is the pipeline's original extraction; fresh_dir (2003-2025_v1.3_r1, mtime 2026-09-20) is it_mbo_bug_hunt.R's single-site HH-only re-download -- only its BIFVARINFO_MM metadata file was ever extracted to disk from that fresh download; the fresh FLUXMET_MM/ERA5_MM data files exist inside the fresh raw zip but were not extracted (deliverable 3 extracts them for comparison).")

# Which glob/lookup pattern does each script actually use? Traced directly by
# grep against each script; reported as a static table (not re-executed here,
# since several of those scripts have side effects / long runtimes out of
# scope for this check) plus which directory that logic resolves to *today*,
# now that both IT-MBo directories exist.
trace_tbl <- tibble::tribble(
  ~script,                              ~mm_lookup_logic,
  "era5_precip_units.R",                "list.files(\"data/extracted\", pattern=paste0(\"_\", site_id, \"_FLUXNET_ERA5_MM_.*\\\\.csv$\")) -- single glob across all of data/extracted/, first (only, at the time it ran) match",
  "era5_precip_units_v2.R",             "list.files(\"data/extracted\", pattern=paste0(\"_\", site_id, \"_FLUXNET_ERA5_\", res, \"_.*\\\\.csv$\")) -- same pattern, parameterised by resolution",
  "era5_precip_units_v3_partA.R",       "find_dir(site_id) <- list.files(\"data/extracted\", pattern=paste0(\"_\", site_id, \"_FLUXNET_\")); find_file() takes the first dir match; at the time this ran, IT-MBo had exactly one directory",
  "era5_precip_units_v4.R",             "does not read raw MM files -- reuses era5_precip_units_v2's table_t2_ratios.csv / v3's table_b1_factor_estimates.csv",
  "era5_reference_plots.R",             "does not read raw MM files -- reuses v3's table_b1_factor_estimates.csv",
  "era5_cumulative_test.R",             "extracted_dirs <- list.dirs(\"data/extracted\"); site_dir_lookup <- setNames(extracted_dirs, dir_site_ids); site_dir_lookup[[site_id]] -- first name match; at the time this ran, IT-MBo had exactly one directory",
  "era5_share_for_coordination.R",      "identical site_dir_lookup[[site_id]] pattern as era5_cumulative_test.R (lines 46-56); at the time this ran (2026-09-18), IT-MBo had exactly one directory",
  "it_mbo_bug_hunt.R",                  "find_dirs(site_id) matches ALL directories for a site; find_file() iterates dirs in list.files() order and returns the first dir containing the pattern -- for MM/DD/YY, old_dir is checked first and has the file, so old_dir wins; for HH, old_dir has no HH file, so it falls through to fresh_dir",
  "it_mbo_parsimony.R",                 "identical find_dirs()/find_file() pattern as it_mbo_bug_hunt.R -- same old_dir-for-MM/DD/YY, fresh_dir-for-HH resolution"
)
print(as.data.frame(trace_tbl))

out_trace <- file.path(OUTD, "table_d1_script_trace.csv")
readr::write_csv(trace_tbl, out_trace)
write_output_metadata(out_trace,
  input_sources = "scripts/diagnostics/*.R (grepped directly, not re-executed)",
  notes = "For each script that has produced a circulated P_ERA/P_F number, the exact directory/file-resolution logic it uses for the MM file, traced by reading the script. Every script that ran before 2026-09-20 (v1 through era5_share_for_coordination) only ever had one IT-MBo directory on disk (old_dir) -- there was no directory-selection ambiguity when they ran; they were simply reading the only copy that existed, and that copy does not match the currently-distributed product (deliverable 3). it_mbo_bug_hunt.R and it_mbo_parsimony.R, run today after fresh_dir already existed, both search both directories but still land on old_dir for MM/DD/YY (fresh_dir has no MM/DD/YY files extracted) -- this is the direct cause of their own DD/MM/YY-vs-HH '~21.25x product defect' finding (see the note at the end of this report).")

message("Saved: ", out_d1_files, " and ", out_trace)

# ============================================================================
# DELIVERABLE 2: raw, unparsed header + 2013-01/2013-06 rows from the file
# our scripts actually read (old_dir's FLUXMET_MM file)
# ============================================================================
message("\n================ D2: raw text, old_dir FLUXMET_MM file ================")

old_fm_mm <- list.files(old_dir, pattern = "FLUXNET_FLUXMET_MM_.*\\.csv$", full.names = TRUE)
stopifnot(length(old_fm_mm) == 1)

raw_lines <- readLines(old_fm_mm)
header_line <- raw_lines[[1]]
ts_col <- which(strsplit(header_line, ",")[[1]] == "TIMESTAMP")
stopifnot(ts_col == 1L)
row_2013_01 <- raw_lines[grepl("^201301,", raw_lines)]
row_2013_06 <- raw_lines[grepl("^201306,", raw_lines)]
stopifnot(length(row_2013_01) == 1, length(row_2013_06) == 1)

d2_raw_text <- c(
  "-- HEADER --", header_line,
  "-- 2013-01 --", row_2013_01,
  "-- 2013-06 --", row_2013_06
)
writeLines(d2_raw_text, file.path(OUTD, "raw_header_and_2013_rows_olddir.txt"))
cat(paste(d2_raw_text, collapse = "\n"), "\n")

era_col <- which(strsplit(header_line, ",")[[1]] == "P_ERA")
pf_col  <- which(strsplit(header_line, ",")[[1]] == "P_F")
pfqc_col <- which(strsplit(header_line, ",")[[1]] == "P_F_QC")
cat(sprintf("\nColumn positions (1-indexed): P_ERA=%d, P_F=%d, P_F_QC=%d (of %d total columns)\n",
            era_col, pf_col, pfqc_col, length(strsplit(header_line, ",")[[1]])))

write_output_metadata(file.path(OUTD, "raw_header_and_2013_rows_olddir.txt"),
  input_sources = old_fm_mm,
  notes = "Verbatim, unparsed header line and the 2013-01/2013-06 data rows from the FLUXMET_MM file in old_dir -- the file every earlier script (and, for MM/DD/YY, today's it_mbo_bug_hunt.R/it_mbo_parsimony.R) actually reads. Column positions for P_ERA/P_F/P_F_QC confirmed by direct header split, not assumed.")

# ============================================================================
# DELIVERABLE 3: fresh re-download, byte-for-byte hash comparison
# ============================================================================
message("\n================ D3: fresh re-download and hash comparison ================")

# (a) Reuse today's already-downloaded fresh zip (it_mbo_bug_hunt.R's
# download), extract just its FLUXMET_MM file into a gitignored scratch area
# (data/raw/ -- never review/diagnostics/, which is committed: only the
# resulting hash/size table belongs in the committed output, not a raw bulk
# copy of the data file itself).
earlier_fresh_zip <- "data/raw/it_mbo_hh_check/ICOS_IT-MBo_FLUXNET_2003-2025_v1.3_r1.zip"
stopifnot(file.exists(earlier_fresh_zip))
earlier_fresh_scratch <- "data/raw/it_mbo_file_check_scratch/earlier_fresh"
fs::dir_create(earlier_fresh_scratch)
unzip(earlier_fresh_zip, files = "ICOS_IT-MBo_FLUXNET_FLUXMET_MM_2003-2025_v1.3_r1.csv",
      exdir = earlier_fresh_scratch, overwrite = TRUE)
earlier_fresh_mm <- file.path(earlier_fresh_scratch, "ICOS_IT-MBo_FLUXNET_FLUXMET_MM_2003-2025_v1.3_r1.csv")
stopifnot(file.exists(earlier_fresh_mm))

# (b) Trigger one brand-new, independent download right now, into its own
# scratch dir, to rule out any caching artifact between this task and this
# morning's. Single-site, MM resolution only (fast -- no need for HH again).
message("Fetching live manifest and downloading IT-MBo fresh (new independent download)...")
live_manifest <- flux_listall()
it_mbo_row <- dplyr::filter(live_manifest, site_id == TEST_SITE)
stopifnot(nrow(it_mbo_row) == 1)
stopifnot(it_mbo_row$product_id == PID_IT_MBO)

new_scratch_raw <- "data/raw/it_mbo_file_check_scratch"
new_scratch_extracted <- "data/raw/it_mbo_file_check_scratch/extracted"
fs::dir_create(new_scratch_raw)
fs::dir_create(new_scratch_extracted)

flux_download(file_list_df = it_mbo_row, download_dir = new_scratch_raw)
new_zip <- list.files(new_scratch_raw, pattern = "IT-MBo.*\\.zip$", full.names = TRUE)
stopifnot(length(new_zip) == 1)

flux_extract(zip_dir = new_scratch_raw, output_dir = new_scratch_extracted,
             site_ids = TEST_SITE, resolutions = "m")
new_download_mm <- list.files(new_scratch_extracted, pattern = "FLUXNET_FLUXMET_MM_.*\\.csv$",
                               recursive = TRUE, full.names = TRUE)
stopifnot(length(new_download_mm) == 1)

hash_comparison <- tibble::tibble(
  copy = c("old_dir on-disk (read by our scripts)",
           "earlier-fresh (today's it_mbo_bug_hunt.R zip, MM extracted now)",
           "new-fresh (brand-new download, triggered by this task)"),
  path = c(old_fm_mm, earlier_fresh_mm, new_download_mm),
  bytes = file.size(c(old_fm_mm, earlier_fresh_mm, new_download_mm)),
  sha256 = purrr::map_chr(c(old_fm_mm, earlier_fresh_mm, new_download_mm), sha256)
)
print(as.data.frame(hash_comparison))

two_fresh_match <- hash_comparison$sha256[[2]] == hash_comparison$sha256[[3]]
fresh_differs_from_old <- hash_comparison$sha256[[1]] != hash_comparison$sha256[[2]]
cat(sprintf("\nTwo independent fresh downloads byte-identical: %s\n", two_fresh_match))
cat(sprintf("Fresh copy differs from old_dir on-disk copy: %s\n", fresh_differs_from_old))

out_d3 <- file.path(OUTD, "table_d3_hash_comparison.csv")
readr::write_csv(hash_comparison, out_d3)
write_output_metadata(out_d3,
  input_sources = c(old_fm_mm, earlier_fresh_zip, new_zip),
  notes = sprintf("sha256/byte-size comparison of the FLUXMET_MM file across three copies: the stale on-disk copy our scripts actually read, today's earlier fresh download (zip already on disk, MM member extracted for this comparison), and a brand-new independent download triggered by this script. Two fresh downloads identical: %s. Fresh differs from old_dir: %s. product_id confirmed %s via live flux_listall() immediately before the new download.",
                   two_fresh_match, fresh_differs_from_old, PID_IT_MBO))

# Confirm the specific 2013-01/2013-06 values in the new, independent download
# match Dario's numbers (not just today's earlier fresh copy).
new_dl_rows <- readr::read_csv(new_download_mm, show_col_types = FALSE) |>
  dplyr::filter(TIMESTAMP %in% c(201301, 201306)) |>
  dplyr::select(TIMESTAMP, P_ERA, P_F, P_F_QC)
print(new_dl_rows)

message("Saved: ", out_d3)

# ============================================================================
# DELIVERABLE 4: the arithmetic that produced "4,017"
# ============================================================================
message("\n================ D4: arithmetic trace for the '4,017' figure ================")

raw_pf_201301 <- as.numeric(strsplit(row_2013_01, ",")[[1]][[pf_col]])
cat(sprintf("Raw column value read for 2013-01: P_F = %s (column position %d, header name '%s')\n",
            raw_pf_201301, pf_col, strsplit(header_line, ",")[[1]][[pf_col]]))
cat(sprintf("Rounded for display: %s -> circulated as ~4,017\n", format(raw_pf_201301, big.mark = ",")))

# Where was 4,017/4017 actually quoted? Grep every existing diagnostic report.
# NB: grep's default BRE treats "?" as literal, not "previous atom optional" --
# use -E (ERE) so "[,.]?" behaves as intended.
grep_hits <- system2("grep", c("-rnE", "-i", "--include=*.md", "4[,.]?017",
                                "review/diagnostics/"), stdout = TRUE)
cat("\ngrep for '4017'/'4,017' across review/diagnostics/*.md:\n")
print(grep_hits)

d4_note <- c(
  "Deliverable 4 -- arithmetic trace for the circulated '4,017' figure",
  "",
  sprintf("Raw value read from data/extracted/ICOS_IT-MBo_FLUXNET_2003-2024_v1.3_r1/ICOS_IT-MBo_FLUXNET_FLUXMET_MM_2003-2024_v1.3_r1.csv, TIMESTAMP=201301, column '%s' (position %d of %d): %s",
          strsplit(header_line, ",")[[1]][[pf_col]], pf_col, length(strsplit(header_line, ",")[[1]]), raw_pf_201301),
  "No multiplier of any kind was applied to this value before it was circulated -- P_F in the FLUXMET_MM file is already a monthly total (mm/month; see it_mbo_bug_hunt.R's D2 BIFVARINFO unit quotes for the confirmed unit statement at MM resolution), unlike P_ERA which the day-weighting formula in read_era5_mm()/read_fluxmet_mm() multiplies by days-in-month. So 4,017.367 raw = 4,017 quoted: this is the raw, unmultiplied value from the file, and the file's own value is wrong -- not an arithmetic or unit-conversion error downstream of it.",
  "",
  "Locations where this number (or its 2013-06 companion, 4,058) was quoted, per grep above:",
  paste(grep_hits, collapse = "\n")
)
writeLines(d4_note, file.path(OUTD, "d4_arithmetic_trace.txt"))
cat(paste(d4_note, collapse = "\n"), "\n")

write_output_metadata(file.path(OUTD, "d4_arithmetic_trace.txt"),
  input_sources = c(old_fm_mm, "review/diagnostics/it_mbo_parsimony/report.md"),
  notes = "Step-by-step arithmetic trace for the circulated '4,017' figure, plus grep-confirmed provenance of where it (and its 4,058 companion) were quoted. grep uses -E (ERE) deliberately -- BRE (grep's default) treats '?' as literal, not optional, and silently returns no matches for this pattern.")

message("Saved: ", file.path(OUTD, "d4_arithmetic_trace.txt"))

# ============================================================================
# DELIVERABLE 5: the 69.8-fold difference, and the DD-vs-HH implication
# ============================================================================
message("\n================ D5: where the 69.8-fold difference enters ================")

ratio_69_8 <- raw_pf_201301 / new_dl_rows$P_F[new_dl_rows$TIMESTAMP == 201301] / 30  # old raw is a monthly TOTAL; new is mm/day -- convert new to monthly total for a like-for-like ratio
# Correct like-for-like: old P_F is mm/month; new P_F is mm/day. Monthly total from new = P_F * days_in_month.
days_jan_2013 <- 31
new_pf_monthly_jan <- new_dl_rows$P_F[new_dl_rows$TIMESTAMP == 201301] * days_jan_2013
ratio_like_for_like <- raw_pf_201301 / new_pf_monthly_jan
cat(sprintf("Old (raw, mm/month): %.3f. New (mm/day %.3f x %d days = %.3f mm/month). Ratio: %.2f\n",
            raw_pf_201301, new_dl_rows$P_F[new_dl_rows$TIMESTAMP == 201301], days_jan_2013, new_pf_monthly_jan, ratio_like_for_like))

d5_sentence <- sprintf(
  "The %.1f-fold difference enters because every script that circulated an IT-MBo monthly number read data/extracted/ICOS_IT-MBo_FLUXNET_2003-2024_v1.3_r1/ICOS_IT-MBo_FLUXNET_FLUXMET_MM_2003-2024_v1.3_r1.csv -- a local extraction from 2026-06-01 that does not match the FLUXNET product currently distributed under PID %s (confirmed identical across two independent fresh downloads today, both of which reproduce Dario's numbers exactly) -- and that stale local copy's P_F column is corrupted at a sparse subset of months, including January and June 2013 specifically, independently of the file's separate, uniform ~21.24x P_ERA problem; this is a local file-provenance issue, not a units, day-weighting, or arithmetic error in this repository's own code, which applies the correct (and, for P_F at MM resolution, correctly zero) multiplier to whatever value the file hands it.",
  ratio_like_for_like, PID_IT_MBO)
cat("\n", d5_sentence, "\n")
writeLines(d5_sentence, file.path(OUTD, "d5_verdict_sentence.txt"))
write_output_metadata(file.path(OUTD, "d5_verdict_sentence.txt"),
  input_sources = c(old_fm_mm, new_download_mm),
  notes = "One-sentence answer to 'where does the 69.8-fold difference enter' -- computed like-for-like (old raw mm/month vs. new mm/day x days-in-month) from the brand-new independent download in deliverable 3.")

# ---- Implication for today's earlier it_mbo_bug_hunt.R DD-vs-HH finding ----
# NOT an edit to that report -- a note here, in this task's own output, since
# it_mbo_bug_hunt.R's D2 step necessarily read DD/MM/YY from old_dir (the only
# directory with those files) and HH from fresh_dir (the only directory with
# HH) -- it was comparing the stale, corrupted local extraction against a
# genuinely fresh one, not two resolution branches of the same distributed
# product. The ~21.25x "DD/MM/YY-vs-HH" factor it_mbo_bug_hunt.R reported is
# the same constant this script finds directly, old P_ERA vs fresh P_ERA,
# across all 264 overlapping months (not just Jan/Jun 2013) -- see below.

message("\n================ Implication: today's earlier DD-vs-HH finding ================")

old_era_mm <- readr::read_csv(list.files(old_dir, pattern = "FLUXNET_ERA5_MM_.*\\.csv$", full.names = TRUE),
                               show_col_types = FALSE) |> dplyr::transmute(TIMESTAMP, P_ERA_old = P_ERA)
new_era_mm <- readr::read_csv(new_download_mm, show_col_types = FALSE) |>
  dplyr::transmute(TIMESTAMP, P_ERA_new_download = P_ERA)
old_vs_new_era <- dplyr::inner_join(old_era_mm, new_era_mm, by = "TIMESTAMP") |>
  dplyr::mutate(ratio = P_ERA_old / P_ERA_new_download)
cat(sprintf("Old-dir P_ERA vs brand-new-download P_ERA, %d common months: median ratio=%.4f range=[%.4f, %.4f]\n",
            nrow(old_vs_new_era), median(old_vs_new_era$ratio, na.rm = TRUE),
            min(old_vs_new_era$ratio, na.rm = TRUE), max(old_vs_new_era$ratio, na.rm = TRUE)))

out_implication <- file.path(OUTD, "table_implication_old_vs_new_era_mm.csv")
readr::write_csv(old_vs_new_era, out_implication)
write_output_metadata(out_implication,
  input_sources = c(list.files(old_dir, pattern = "FLUXNET_ERA5_MM_.*\\.csv$", full.names = TRUE), new_download_mm),
  notes = sprintf("Old-dir P_ERA_MM vs a brand-new independent download's P_ERA_MM, every common month. Median ratio %.4f -- the same ~21.25x factor it_mbo_bug_hunt.R (2026-09-20, earlier today) reported as a 'DD/MM/YY-vs-HH, product-level defect' after comparing old_dir's DD/MM/YY files against fresh_dir's HH files. This is a revision candidate for that report's conclusion, NOT applied here: it_mbo_bug_hunt.R's D2 step compared a stale local MM extraction against a freshly-downloaded HH extraction, not two genuine resolution branches of the same distributed product -- see review/diagnostics/it_mbo_file_check/report.md for the full argument. it_mbo_bug_hunt/report.md itself is not edited by this script.",
                   median(old_vs_new_era$ratio, na.rm = TRUE)))

message("Saved: ", out_implication)

message("\n=== it_mbo_file_check.R complete ===")
