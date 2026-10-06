## dq_stage3_vut_vs_cut.R
##
## Data quality / uncertainty diagnostic -- Stage 3: VUT against CUT.
##
## Unattended background run. Diagnostic only, writes to
## review/diagnostics/data_quality_uncertainty/. Reads Stage 2's
## table_stage2_site_year_nee_uncertainty.csv (itself derived from the
## pre-QC `annual` DuckDB table, dataset = 'FLUXMET') -- no new DB query
## needed, since Stage 2 already computed VUT and CUT qualification
## independently (each gated on its own QC column) for every site-year.
##
## For site-years where BOTH NEE_VUT_REF and NEE_CUT_REF qualify (paper
## rule, QC_THRESHOLD_YY, each on its own QC column): distribution of
## VUT - CUT, share |diff| > {25,50,100} gC/m2/yr, share |diff| < combined
## joint uncertainty (sqrt(JOINTUNC_VUT^2 + JOINTUNC_CUT^2) -- the
## propagated uncertainty of a difference of two independent-ish
## estimates; documented explicitly since the task wording doesn't specify
## how to combine the two sides' joint terms), share where sign(NEE)
## differs. Repeated at site level using each site's median qualifying
## VUT/CUT value.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(jsonlite); library(ggplot2); library(tidyr)
})

write_meta <- function(output_path, input_sources, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                                 error = function(e) NA_character_),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

OUT_DIR <- "review/diagnostics/data_quality_uncertainty"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

dir.create("logs", showWarnings = FALSE)
LOG_FILE <- file.path("logs", paste0("dq_stage3_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output"); sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== Stage 3: VUT vs CUT ===")

stage2_path <- file.path(OUT_DIR, "table_stage2_site_year_nee_uncertainty.csv")
stage2 <- read_csv(stage2_path, show_col_types = FALSE)

## ---------------------------------------------------------------------------
## Site-year pairing: inner join VUT and CUT rows -- both must independently
## qualify under the paper's own-QC rule for that site-year.
## ---------------------------------------------------------------------------
vut <- stage2 |> filter(carbon_type == "VUT") |>
  select(site_id, year, igbp, data_hub, NEE_VUT = NEE, joint_VUT = joint)
cut <- stage2 |> filter(carbon_type == "CUT") |>
  select(site_id, year, NEE_CUT = NEE, joint_CUT = joint)

paired <- inner_join(vut, cut, by = c("site_id", "year")) |>
  mutate(
    diff = NEE_VUT - NEE_CUT,
    abs_diff = abs(diff),
    diff_joint = sqrt(joint_VUT^2 + joint_CUT^2),
    smaller_than_joint = abs_diff < diff_joint,
    sign_vut = sign(NEE_VUT), sign_cut = sign(NEE_CUT),
    sign_differs = sign_vut != sign_cut
  )

msg("Site-years with both VUT and CUT qualifying: ", nrow(paired),
    " (", n_distinct(paired$site_id), " sites)")

write_csv(paired, file.path(OUT_DIR, "table_stage3_site_year_vut_vs_cut.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage3_site_year_vut_vs_cut.csv"),
  input_sources = stage2_path,
  notes = paste0(
    "Site-years where NEE_VUT_REF and NEE_CUT_REF both independently qualify under ",
    "QC_THRESHOLD_YY (=", QC_THRESHOLD_YY, "), each gated on its own QC column. diff = ",
    "NEE_VUT - NEE_CUT (gC m-2 yr-1). diff_joint = sqrt(JOINTUNC_VUT^2 + JOINTUNC_CUT^2) ",
    "-- the propagated uncertainty of a difference of two estimates, combining the two ",
    "sides' own joint uncertainty terms in quadrature. This combination rule is not ",
    "specified by the task and is this script's explicit choice; see report.md."
  )
)

## ---------------------------------------------------------------------------
## Distribution stats helper (site-year and site level share the same shape)
## ---------------------------------------------------------------------------
dist_stats <- function(df, diff_col = "diff", abs_col = "abs_diff",
                        joint_bool_col = "smaller_than_joint", sign_bool_col = "sign_differs") {
  d <- df[[diff_col]]
  ad <- df[[abs_col]]
  tibble(
    n = length(d),
    median_diff = median(d, na.rm = TRUE),
    q25_diff = quantile(d, 0.25, na.rm = TRUE),
    q75_diff = quantile(d, 0.75, na.rm = TRUE),
    p05_diff = quantile(d, 0.05, na.rm = TRUE),
    p95_diff = quantile(d, 0.95, na.rm = TRUE),
    share_abs_gt_25 = mean(ad > 25, na.rm = TRUE),
    share_abs_gt_50 = mean(ad > 50, na.rm = TRUE),
    share_abs_gt_100 = mean(ad > 100, na.rm = TRUE),
    share_smaller_than_joint = mean(df[[joint_bool_col]], na.rm = TRUE),
    share_sign_differs = mean(df[[sign_bool_col]], na.rm = TRUE)
  )
}

site_year_summary <- dist_stats(paired)
write_csv(site_year_summary, file.path(OUT_DIR, "table_stage3_site_year_summary.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage3_site_year_summary.csv"),
  input_sources = "table_stage3_site_year_vut_vs_cut.csv (derived)",
  notes = "Distribution of VUT-CUT (gC m-2 yr-1) and derived shares, at the site-year level (both must qualify)."
)
msg("Site-year summary:")
print(site_year_summary)

## ---------------------------------------------------------------------------
## Site level: each site's median qualifying VUT and CUT value (independently
## computed per side -- a site's VUT median uses all its qualifying VUT years,
## not only years where CUT also qualified, and vice versa -- then paired by
## site_id). This matches the task's "using site medians", distinct from
## collapsing the already-paired site-year table.
## ---------------------------------------------------------------------------
site_vut <- stage2 |> filter(carbon_type == "VUT") |>
  group_by(site_id) |> summarise(NEE_VUT_site = median(NEE, na.rm = TRUE),
                                   joint_VUT_site = median(joint, na.rm = TRUE), .groups = "drop")
site_cut <- stage2 |> filter(carbon_type == "CUT") |>
  group_by(site_id) |> summarise(NEE_CUT_site = median(NEE, na.rm = TRUE),
                                   joint_CUT_site = median(joint, na.rm = TRUE), .groups = "drop")

site_paired <- inner_join(site_vut, site_cut, by = "site_id") |>
  mutate(
    diff = NEE_VUT_site - NEE_CUT_site,
    abs_diff = abs(diff),
    diff_joint = sqrt(joint_VUT_site^2 + joint_CUT_site^2),
    smaller_than_joint = abs_diff < diff_joint,
    sign_differs = sign(NEE_VUT_site) != sign(NEE_CUT_site)
  )

msg("Sites with both a VUT and a CUT median: ", nrow(site_paired))

write_csv(site_paired, file.path(OUT_DIR, "table_stage3_site_level_vut_vs_cut.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage3_site_level_vut_vs_cut.csv"),
  input_sources = stage2_path,
  notes = paste0(
    "Per-site median of qualifying NEE_VUT_REF and median of qualifying NEE_CUT_REF ",
    "(each computed over that side's own qualifying years independently, then paired ",
    "by site_id -- not a site-level collapse of the already-paired site-year table). ",
    "diff/diff_joint/smaller_than_joint/sign_differs defined identically to the ",
    "site-year table, using each side's median joint uncertainty."
  )
)

site_summary <- dist_stats(site_paired)
write_csv(site_summary, file.path(OUT_DIR, "table_stage3_site_level_summary.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage3_site_level_summary.csv"),
  input_sources = "table_stage3_site_level_vut_vs_cut.csv (derived)",
  notes = "Distribution of VUT-CUT (gC m-2 yr-1) and derived shares, at the site level (site medians)."
)
msg("Site-level summary:")
print(site_summary)

## ---------------------------------------------------------------------------
## Figures
## ---------------------------------------------------------------------------
n_clipped <- sum(abs(paired$diff) > 150)
p1 <- ggplot(paired, aes(x = diff)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", boundary = 0) +
  geom_vline(xintercept = c(-100, -50, -25, 25, 50, 100), linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(xlim = c(-150, 150)) +
  labs(title = "Annual NEE_VUT - NEE_CUT, site-years where both qualify",
       subtitle = paste0("n = ", nrow(paired), " site-years, ", n_distinct(paired$site_id),
                          " sites. Dotted lines at ±25/50/100 gC m-2 yr-1. X-axis clipped to ±150 (",
                          n_clipped, " site-years outside this range, long tail not shown)."),
       x = "NEE_VUT - NEE_CUT (gC m-2 yr-1)", y = "Count") +
  theme_bw()
ggsave(file.path(OUT_DIR, "fig_stage3_vut_minus_cut_histogram.png"), p1, width = 7.5, height = 5.5, dpi = 150, bg = "white")

p2 <- ggplot(paired, aes(x = NEE_CUT, y = NEE_VUT)) +
  geom_point(alpha = 0.25, size = 0.8, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, color = "grey70") + geom_vline(xintercept = 0, color = "grey70") +
  coord_equal() +
  labs(title = "Annual NEE: VUT vs CUT, site-years where both qualify",
       subtitle = "Dashed = 1:1. Points in off-diagonal quadrants (relative to the grey cross-hairs) are sign-disagreement site-years.",
       x = "NEE_CUT (gC m-2 yr-1)", y = "NEE_VUT (gC m-2 yr-1)") +
  theme_bw()
ggsave(file.path(OUT_DIR, "fig_stage3_vut_vs_cut_scatter.png"), p2, width = 7, height = 7, dpi = 150, bg = "white")

msg("Stage 3 complete.")

## ---------------------------------------------------------------------------
## Status
## ---------------------------------------------------------------------------
status_path <- file.path(OUT_DIR, "status.md")
status_entry <- c(
  "",
  "## Stage 3 -- VUT against CUT",
  "",
  paste0("Completed ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "."),
  "",
  paste0("- Site-year level: n=", site_year_summary$n, " site-years (", n_distinct(paired$site_id),
         " sites) with both VUT and CUT qualifying. median diff=", round(site_year_summary$median_diff, 2),
         ", IQR=[", round(site_year_summary$q25_diff, 2), ", ", round(site_year_summary$q75_diff, 2),
         "], 5-95pctile=[", round(site_year_summary$p05_diff, 2), ", ", round(site_year_summary$p95_diff, 2),
         "] gC/m2/yr. share|diff|>25/50/100 = ", round(site_year_summary$share_abs_gt_25, 3), "/",
         round(site_year_summary$share_abs_gt_50, 3), "/", round(site_year_summary$share_abs_gt_100, 3),
         ". share smaller than combined joint unc = ", round(site_year_summary$share_smaller_than_joint, 3),
         ". share sign differs = ", round(site_year_summary$share_sign_differs, 3), "."),
  paste0("- Site level (site medians): n=", site_summary$n, " sites. median diff=",
         round(site_summary$median_diff, 2), ", IQR=[", round(site_summary$q25_diff, 2), ", ",
         round(site_summary$q75_diff, 2), "], 5-95pctile=[", round(site_summary$p05_diff, 2), ", ",
         round(site_summary$p95_diff, 2), "] gC/m2/yr. share|diff|>25/50/100 = ",
         round(site_summary$share_abs_gt_25, 3), "/", round(site_summary$share_abs_gt_50, 3), "/",
         round(site_summary$share_abs_gt_100, 3), ". share smaller than combined joint unc = ",
         round(site_summary$share_smaller_than_joint, 3), ". share sign differs = ",
         round(site_summary$share_sign_differs, 3), "."),
  "",
  "Outputs: table_stage3_site_year_vut_vs_cut.csv, table_stage3_site_year_summary.csv, ",
  "table_stage3_site_level_vut_vs_cut.csv, table_stage3_site_level_summary.csv, ",
  "fig_stage3_vut_minus_cut_histogram.png, fig_stage3_vut_vs_cut_scatter.png (+ .meta.json each).",
  ""
)
write_lines(status_entry, status_path, append = TRUE)

cat("\n--- dq_stage3_vut_vs_cut.R: done. See ", LOG_FILE, " ---\n")
