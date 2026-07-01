## scripts/build_draft_manuscript_v1.R
## Assembles the six draft-manuscript candidate figures (rebuilt at
## consistent journal-ready specifications — see SESSION_LOG.md 2026-06-30)
## into review/figures/draft_manuscript_v1/ under descriptive filenames,
## alongside a matching .legend.txt for each.
##
## This is a pure file-copy utility (no flux data read, no credentials) —
## it intentionally does not call check_pipeline_config(). Source figures
## must already exist; run the relevant generate_*.R / figure_*.R script
## first if a source is missing or stale.
##
## Fig 1B note: fig_dur11_CumulativeSiteYears_IGBP.png has no script that
## writes it directly under a fig_01b_* name — scripts/generate_duration_
## histograms.R writes it under its canonical Dur11 name in
## review/figures/network/, and this script performs the copy/rename step
## explicitly (as a committed, reusable script) rather than via an
## undocumented one-off shell command, addressing the "missing direct
## source" gap noted in the 2026-06-30 figure audit.

library(fs)

out_dir <- file.path("review", "figures", "draft_manuscript_v1")
fs::dir_create(out_dir)

# ---- Figure copy map ---------------------------------------------------------
# source PNG -> descriptive draft-manuscript filename
figs <- list(
  list(src = "review/figures/candidates/fig_03_map_current.png",
       dst = "fig_01a_map_current_network.png"),
  list(src = "review/figures/network/fig_dur11_CumulativeSiteYears_IGBP.png",
       dst = "fig_01b_cumulative_siteyears_igbp.png"),
  list(src = "review/figures/candidates/fig_05_whittaker_current.png",
       dst = "fig_02_whittaker_current.png"),
  list(src = "review/figures/flux_medians/fig_flux_comparison_combo_nep_et_h.png",
       dst = "fig_03_flux_comparison_combo_nep_et_h.png"),
  list(src = "review/figures/representativeness/fig_rep008_jaccard_trajectory_with_counts.png",
       dst = "fig_04_jaccard_trajectory_with_counts.png"),
  list(src = "review/figures/representativeness/fig_rep001_current.png",
       dst = "fig_05_current_network_sampling_ratios.png")
)

for (f in figs) {
  if (!file.exists(f$src)) {
    stop("Source figure not found: ", f$src,
         " — run its generate_*.R / figure_*.R script first.")
  }
  dst_path <- file.path(out_dir, f$dst)
  file.copy(f$src, dst_path, overwrite = TRUE)
  message("Copied: ", f$src, " -> ", dst_path)
}

# ---- Legend copy map ---------------------------------------------------------
# source .legend.txt -> descriptive draft-manuscript legend filename
# Fig 4/5 legends already existed (Rep008/Rep001) and are copied verbatim
# except for a corrected DIMENSIONS line reflecting this session's resize.
legends <- list(
  list(src = "review/figures/candidates/fig_03_map_current.legend.txt",
       dst = "fig_01a_map_current_network.legend.txt",
       fix_dims = NULL),
  list(src = "review/figures/network/fig_dur11_CumulativeSiteYears_IGBP.legend.txt",
       dst = "fig_01b_cumulative_siteyears_igbp.legend.txt",
       fix_dims = NULL),
  list(src = "review/figures/candidates/fig_05_whittaker_current.legend.txt",
       dst = "fig_02_whittaker_current.legend.txt",
       fix_dims = NULL),
  list(src = "review/figures/flux_medians/fig_flux_comparison_combo_nep_et_h.legend.txt",
       dst = "fig_03_flux_comparison_combo_nep_et_h.legend.txt",
       fix_dims = NULL),
  list(src = "review/figures/representativeness/fig_rep008_jaccard_trajectory_with_counts.legend.txt",
       dst = "fig_04_jaccard_trajectory_with_counts.legend.txt",
       fix_dims = "DIMENSIONS:    3.5 x 4 inches, 300 dpi"),
  list(src = "review/figures/representativeness/fig_rep001_current.legend.txt",
       dst = "fig_05_current_network_sampling_ratios.legend.txt",
       fix_dims = "DIMENSIONS:    7 x 5 inches, 300 dpi")
)

for (l in legends) {
  if (!file.exists(l$src)) {
    stop("Source legend not found: ", l$src)
  }
  dst_path <- file.path(out_dir, l$dst)
  txt <- readLines(l$src)
  if (!is.null(l$fix_dims)) {
    txt <- sub("^DIMENSIONS:.*$", l$fix_dims, txt)
  }
  writeLines(txt, dst_path)
  message("Copied: ", l$src, " -> ", dst_path)
}

message("\nDone. draft_manuscript_v1/ contains ", length(figs), " figures and ",
        length(legends), " legends.")
