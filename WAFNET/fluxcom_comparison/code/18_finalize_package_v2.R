## 18_finalize_package_v2.R — Stage 3b step 5: MANIFEST.csv and the package size check.
##
## Writes MANIFEST.csv (every file in package_ossenatou_<date>_v2/, its size and SHA-256, the
## git commit and script names that built it) LAST, after every other file in the package
## (including the check figure) already exists -- MANIFEST.csv lists everything else, not
## itself. Stops (does not zip) and reports if the package exceeds 10 MB, per the Stage 3b
## prompt; zipping is a separate shell step the caller runs only if this check passes.

source("WAFNET/fluxcom_comparison/code/00_config.R")

PKG_DATE <- format(Sys.Date(), "%Y-%m-%d")
PKG_DIR <- file.path(FLUXCOM_ROOT, paste0("package_ossenatou_", PKG_DATE, "_v2"))
if (!dir.exists(PKG_DIR)) stop("[18_finalize_package_v2] Package directory not found: ", PKG_DIR)

fig_path <- file.path(PKG_DIR, "check_figure_xbase_005_vs_tower_DRAFT.png")
if (!file.exists(fig_path)) stop("[18_finalize_package_v2] Check figure not found -- run 17_check_figure_005_v2.R first.")

compute_sha256_hex <- function(path) {
  out <- system(sprintf("openssl dgst -sha256 %s", shQuote(path)), intern = TRUE)
  sub("^.*= ", "", out)
}

git_commit <- tryCatch(system("git rev-parse HEAD", intern = TRUE), error = function(e) NA_character_)
scripts_used <- c(
  "WAFNET/fluxcom_comparison/code/14_download_extract_xbase_005_v2.R",
  "WAFNET/fluxcom_comparison/code/15_build_tower_provenance.R",
  "WAFNET/fluxcom_comparison/code/16_package_ossenatou_v2.R",
  "WAFNET/fluxcom_comparison/code/17_check_figure_005_v2.R",
  "WAFNET/fluxcom_comparison/code/18_finalize_package_v2.R"
)

pkg_files <- list.files(PKG_DIR, recursive = TRUE, full.names = TRUE)
manifest_df <- data.frame(
  file = sub(paste0("^", PKG_DIR, "/"), "", pkg_files),
  size_bytes = file.size(pkg_files),
  sha256_hex = vapply(pkg_files, compute_sha256_hex, character(1)),
  git_commit = git_commit,
  scripts = paste(scripts_used, collapse = "; "),
  stringsAsFactors = FALSE
)
manifest_df <- manifest_df[order(manifest_df$file), ]

MANIFEST_OUT <- file.path(PKG_DIR, "MANIFEST.csv")
utils::write.csv(manifest_df, MANIFEST_OUT, row.names = FALSE)

pkg_size_bytes <- sum(manifest_df$size_bytes) + file.size(MANIFEST_OUT)
message("\n[18_finalize_package_v2] MANIFEST.csv written: ", nrow(manifest_df), " files listed.")
message("[18_finalize_package_v2] Package total size (including MANIFEST.csv): ",
        round(pkg_size_bytes / 1e6, 3), " MB")

if (pkg_size_bytes > 10e6) {
  message("\n[18_finalize_package_v2] STOP: package exceeds 10 MB (", round(pkg_size_bytes / 1e6, 2),
          " MB). Per the Stage 3b prompt, reporting before committing rather than zipping/committing automatically.")
} else {
  message("[18_finalize_package_v2] Under the 10 MB threshold -- OK to zip and commit.")
}

cat("\n[18_finalize_package_v2] Files:\n")
print(manifest_df[, c("file", "size_bytes")])
