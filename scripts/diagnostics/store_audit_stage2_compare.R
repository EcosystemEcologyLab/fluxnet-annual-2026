## store_audit_stage2_compare.R -- content comparison for Stage 2 of the store
## audit. For every site in the Stage 2 site list (freshly downloaded into
## data/raw/store_audit_scratch/extracted/) plus the 5 sites already
## free-checked from earlier same-day zips, compares MM-resolution
## meteorological and flux variables against the on-disk data/extracted/
## copy: byte size, sha256, year range, per-column ratio/diff, and a
## uniform-vs-scattered classification.

if (file.exists(".env")) { library(dotenv); dotenv::load_dot_env() }
source("R/pipeline_config.R")
check_pipeline_config()
suppressPackageStartupMessages({library(dplyr); library(readr); library(purrr); library(digest)})

MET_VARS  <- c("P_ERA", "P_F", "TA_ERA", "TA_F")
FLUX_VARS <- c("NEE_VUT_REF", "GPP_NT_VUT_REF", "LE_F_MDS", "H_F_MDS")
ALL_VARS  <- c(MET_VARS, FLUX_VARS)

## Fixed, per-variable absolute-difference severity floors, at MM resolution's
## native units (P in mm/day rate; TA in deg C; NEE/GPP already period-
## integrated gC/m2/month per CLAUDE.md; LE/H in W/m2). Chosen as round
## numbers well above ordinary month-to-month reprocessing noise (FI-Hyy's
## largest MM-resolution drift across all 8 variables was ~1 unit) and well
## below IT-MBo's actual defect (>37 mm/day on P_ERA). A pure ratio
## threshold is unusable for NEE/TA, which cross zero -- a -0.05 to +0.05
## gC/m2 NEE swing is a physically trivial difference but an undefined or
## wildly "severe" ratio; these floors sidestep that entirely.
SEVERITY_FLOOR <- c(P_ERA = 2, P_F = 2, TA_ERA = 2, TA_F = 2,
                     NEE_VUT_REF = 10, GPP_NT_VUT_REF = 10,
                     LE_F_MDS = 10, H_F_MDS = 10)

find_old_dir <- function(site_id) {
  d <- list.dirs("data/extracted", recursive = FALSE, full.names = TRUE)
  hits <- d[grepl(paste0("_", site_id, "_FLUXNET_"), basename(d))]
  # Exclude any fresh (Sep 2026) HH-only dirs created by earlier same-day
  # diagnostics -- we want the original on-disk MM/DD/YY extraction.
  mm_hits <- hits[map_lgl(hits, ~ length(list.files(.x, pattern = "FLUXMET_MM_.*\\.csv$")) > 0)]
  if (length(mm_hits) == 0) return(NA_character_)
  mm_hits[[1]]
}

## NOTE: store_audit_stage2_download.R's utils::unzip(zip_path, exdir = EXT_DIR)
## extracts every site's files FLAT into EXT_DIR (the distributed zips have no
## top-level per-site folder) -- there are no per-site subdirectories to list.
## find_new_dir() therefore just confirms at least one file for this site_id
## exists in the flat scratch dir; compare_site() below does its own
## site_id-anchored file lookup within that flat directory (not a bare
## "FLUXMET_MM_.*.csv$" pattern, which would match every site's file).
find_new_dir <- function(site_id, scratch_root) {
  hits <- list.files(scratch_root, pattern = paste0("_", site_id, "_FLUXNET_"), full.names = TRUE)
  if (length(hits) == 0) return(NA_character_)
  scratch_root
}

compare_site <- function(site_id, old_dir, new_dir) {
  if (is.na(old_dir) || is.na(new_dir)) {
    return(tibble(site_id = site_id, status = "MISSING_DIR",
                  old_dir = old_dir, new_dir = new_dir))
  }
  ## Site-id-anchored on both sides: new_dir may be a flat, multi-site
  ## directory (the download script's unzip target has no per-site
  ## subfolders), so an unanchored "FLUXMET_MM_.*.csv$" pattern would match
  ## an arbitrary other site's file.
  old_f <- list.files(old_dir, pattern = paste0("_", site_id, "_.*FLUXMET_MM_.*\\.csv$"), full.names = TRUE)
  new_f <- list.files(new_dir, pattern = paste0("_", site_id, "_.*FLUXMET_MM_.*\\.csv$"), full.names = TRUE)
  if (length(old_f) == 0 || length(new_f) == 0) {
    return(tibble(site_id = site_id, status = "MISSING_MM_FILE",
                  old_dir = old_dir, new_dir = new_dir))
  }
  old_f <- old_f[[1]]; new_f <- new_f[[1]]
  old_hash <- digest(file = old_f, algo = "sha256")
  new_hash <- digest(file = new_f, algo = "sha256")
  old_size <- file.info(old_f)$size
  new_size <- file.info(new_f)$size

  old_df <- read_csv(old_f, show_col_types = FALSE)
  new_df <- read_csv(new_f, show_col_types = FALSE)
  old_years <- range(old_df$TIMESTAMP %/% 100L)
  new_years <- range(new_df$TIMESTAMP %/% 100L)

  common_ts <- intersect(old_df$TIMESTAMP, new_df$TIMESTAMP)
  var_rows <- map(ALL_VARS, function(v) {
    if (!v %in% names(old_df) || !v %in% names(new_df)) {
      return(tibble(variable = v, n = 0L, n_differing = NA_integer_,
                     median_ratio = NA_real_, max_abs_diff = NA_real_, pattern = "column_missing"))
    }
    ov <- old_df[[v]][match(common_ts, old_df$TIMESTAMP)]
    nv <- new_df[[v]][match(common_ts, new_df$TIMESTAMP)]
    ov <- suppressWarnings(as.numeric(ov)); nv <- suppressWarnings(as.numeric(nv))
    valid <- !is.na(ov) & !is.na(nv) & ov != -9999 & nv != -9999
    ov <- ov[valid]; nv <- nv[valid]
    if (length(ov) == 0) return(tibble(variable = v, n = 0L, n_differing = 0L,
                                          median_ratio = NA_real_, max_abs_diff = NA_real_, pattern = "no_overlap"))
    diffs <- ov - nv
    ratios <- ifelse(nv != 0, ov / nv, NA_real_)
    n_differing <- sum(abs(diffs) > 1e-6)
    med_ratio <- median(ratios, na.rm = TRUE)
    max_diff <- max(abs(diffs))
    ## Pattern classification uses a fixed, stated MAGNITUDE threshold, not
    ## just "did it change" -- tiny, near-1.0 differences at every month
    ## (ordinary reprocessing rounding, e.g. FI-Hyy's median ratio 1.004)
    ## must NOT be classified the same as IT-MBo's uniform ~21x factor. Two
    ## earlier versions of this script got this wrong: v1 used only
    ## "changed at every month", mislabeling several minor-drift sites as
    ## DIFFERS_UNIFORM; v2 used a >2x/<0.5x RATIO floor, which is unreliable
    ## for zero-crossing variables (NEE, TA) and mislabeled sites with a
    ## handful of near-zero-crossing months as DIFFERS_SCATTERED. Fixed here
    ## with an absolute-difference floor instead.
    ## Severity is judged on the ABSOLUTE difference against a fixed
    ## per-variable floor (SEVERITY_FLOOR), not a ratio -- NEE_VUT_REF and
    ## TA_ERA/TA_F cross zero, where a ratio is undefined or misleadingly
    ## extreme for a physically trivial swing (e.g. -0.05 to +0.05 gC/m2).
    diff_idx <- which(abs(diffs) > 1e-6)
    severe_idx <- diff_idx[abs(diffs[diff_idx]) > SEVERITY_FLOOR[[v]]]
    n_severe <- length(severe_idx)
    pattern <- if (n_differing == 0) {
      "identical"
    } else if (n_severe == 0) {
      "minor_drift"
    } else if (n_severe >= 0.9 * n_differing && n_severe > 1 &&
               sd(ratios[severe_idx], na.rm = TRUE) / abs(median(ratios[severe_idx], na.rm = TRUE)) < 0.05) {
      # >=90%, not literally 100%, of differing months severe -- tolerates a
      # handful of near-zero months (e.g. dry-season P_ERA) that an absolute
      # floor correctly excludes from "severe" without breaking an otherwise
      # uniform factor across the rest of the record (IT-MBo: 262/264).
      "uniform_factor"
    } else {
      "scattered"
    }
    tibble(variable = v, n = length(ov), n_differing = n_differing, n_severe = n_severe,
           median_ratio = med_ratio, max_abs_diff = max_diff, pattern = pattern)
  }) |> bind_rows() |> mutate(site_id = site_id)

  overall_status <- if (old_hash == new_hash) {
    "IDENTICAL"
  } else if (any(var_rows$pattern == "uniform_factor")) {
    "DIFFERS_UNIFORM"
  } else if (any(var_rows$pattern == "scattered")) {
    "DIFFERS_SCATTERED"
  } else {
    "DIFFERS_MINOR"
  }

  attr(var_rows, "site_summary") <- tibble(
    site_id = site_id, status = overall_status,
    old_file = basename(old_f), new_file = basename(new_f),
    old_size = old_size, new_size = new_size,
    old_sha256 = old_hash, new_sha256 = new_hash,
    old_years = paste(old_years, collapse = "-"), new_years = paste(new_years, collapse = "-")
  )
  var_rows
}

site_list <- read_csv("review/diagnostics/store_audit/table_stage2_site_list.csv", show_col_types = FALSE)
free_checked <- read_csv("review/diagnostics/store_audit/table_stage2_free_checked_sites.csv", show_col_types = FALSE)
all_sites <- unique(c(site_list$site_id, free_checked$site_id))

SCRATCH_EXT <- "data/raw/store_audit_scratch/extracted"
FREE_EXT    <- "data/raw/store_audit_scratch/extract"  # from the earlier free-check (flat files, not dirs)

var_results <- list(); site_summaries <- list()
for (site_id in all_sites) {
  old_dir <- find_old_dir(site_id)
  if (site_id %in% free_checked$site_id) {
    # already extracted as flat files earlier in this session; reuse via a synthetic "dir"
    # by pointing directly at the flat scratch extract directory (filtered by site via filename match).
    flat_files <- list.files(FREE_EXT, pattern = paste0("_", gsub("-", "-", site_id), "_"), full.names = TRUE)
    if (length(flat_files) == 0 && site_id == "IT-MBo") {
      # IT-MBo was handled by an earlier task; reuse its already-hashed fresh file directly.
      flat_files <- list.files("data/raw/it_mbo_file_check_scratch/extracted", recursive = TRUE,
                                pattern = "FLUXMET_MM_.*\\.csv$", full.names = TRUE)
    }
    if (length(flat_files) > 0) {
      tmp_dir <- file.path(tempdir(), paste0("free_", site_id))
      fs::dir_create(tmp_dir)
      fs::file_copy(flat_files, file.path(tmp_dir, basename(flat_files)), overwrite = TRUE)
      new_dir <- tmp_dir
    } else new_dir <- NA_character_
  } else {
    new_dir <- find_new_dir(site_id, SCRATCH_EXT)
  }
  res <- compare_site(site_id, old_dir, new_dir)
  var_results[[site_id]] <- res
  summ <- attr(res, "site_summary")
  if (!is.null(summ)) site_summaries[[site_id]] <- summ else
    site_summaries[[site_id]] <- tibble(site_id = site_id, status = if("status" %in% names(res)) res$status[1] else "UNKNOWN",
                                          old_file = NA, new_file = NA, old_size = NA, new_size = NA,
                                          old_sha256 = NA, new_sha256 = NA, old_years = NA, new_years = NA)
}

var_df <- bind_rows(var_results)
summary_df <- bind_rows(site_summaries)

fs::dir_create("review/diagnostics/store_audit")
write_csv(var_df, "review/diagnostics/store_audit/table_stage2_variable_comparison.csv")
write_csv(summary_df, "review/diagnostics/store_audit/table_stage2_site_summary.csv")

cat("\n=== Stage 2 site-level summary ===\n")
print(count(summary_df, status))
cat("\nSites DIFFERS_UNIFORM (IT-MBo-like, systematic factor):\n")
print(summary_df |> filter(status == "DIFFERS_UNIFORM") |> select(site_id))
cat("\nSites DIFFERS_SCATTERED (sporadic corruption pattern):\n")
print(summary_df |> filter(status == "DIFFERS_SCATTERED") |> select(site_id))
