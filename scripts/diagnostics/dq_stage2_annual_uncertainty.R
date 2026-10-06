## dq_stage2_annual_uncertainty.R
##
## Data quality / uncertainty diagnostic -- Stage 2: uncertainty at the
## annual step.
##
## Unattended background run. Diagnostic only, writes to
## review/diagnostics/data_quality_uncertainty/. Reads the pre-QC `annual`
## DuckDB table (dataset = 'FLUXMET') only.
##
## For site-years with a qualifying annual NEE under the paper's own rule
## ((1 - QC) <= QC_THRESHOLD_YY), applied separately to VUT and CUT (each
## variable gated on its own QC column, not the single per-site VUT/CUT
## fallback 04_qc.R uses for row exclusion -- Stage 3 needs both sides
## independently qualified at the same site-year, which the per-site
## fallback by construction prevents):
##   random          = NEE_{VUT,CUT}_REF_RANDUNC
##   ustar_term      = (NEE_{VUT,CUT}_84 - NEE_{VUT,CUT}_16) / 2
##   joint           = NEE_{VUT,CUT}_REF_JOINTUNC
## Tests joint == sqrt(random^2 + ustar_term^2); summarises medians/IQRs,
## the ustar/random ratio, dominance shares, and the relation to |NEE|,
## overall and by IGBP class. LE/H get whatever uncertainty columns exist
## (RANDUNC only, confirmed in Stage 0 -- no ustar ensemble, no JOINTUNC,
## for the uncorrected value at annual resolution).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr)
  library(jsonlite); library(ggplot2); library(tidyr)
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
LOG_FILE <- file.path("logs", paste0("dq_stage2_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output"); sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== Stage 2: Annual uncertainty ===")
msg("QC_THRESHOLD_YY = ", QC_THRESHOLD_YY)

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
con <- dbConnect(duckdb(), db_path, read_only = TRUE)

site_lookup <- dbGetQuery(con, "
  SELECT site_id, max(igbp) AS igbp, max(data_hub) AS data_hub FROM manifest GROUP BY site_id
") |> as_tibble()

annual <- dbGetQuery(con, "
  SELECT site_id, TIMESTAMP AS year,
    NEE_VUT_REF, NEE_VUT_REF_QC, NEE_VUT_REF_RANDUNC, NEE_VUT_REF_JOINTUNC, NEE_VUT_16, NEE_VUT_84,
    NEE_CUT_REF, NEE_CUT_REF_QC, NEE_CUT_REF_RANDUNC, NEE_CUT_REF_JOINTUNC, NEE_CUT_16, NEE_CUT_84,
    LE_F_MDS, LE_F_MDS_QC, LE_RANDUNC, LE_CORR,
    H_F_MDS, H_F_MDS_QC, H_RANDUNC, H_CORR
  FROM annual WHERE dataset = 'FLUXMET'
") |> as_tibble() |> left_join(site_lookup, by = "site_id")
dbDisconnect(con, shutdown = TRUE)

annual$year <- as.integer(annual$year)
msg("Annual FLUXMET rows: ", nrow(annual))

## ---------------------------------------------------------------------------
## Build the per-site-year, per-carbon-type uncertainty table.
## ---------------------------------------------------------------------------
build_carbon_type <- function(df, type) {
  ref_col <- paste0("NEE_", type, "_REF")
  qc_col  <- paste0("NEE_", type, "_REF_QC")
  rand_col  <- paste0("NEE_", type, "_REF_RANDUNC")
  joint_col <- paste0("NEE_", type, "_REF_JOINTUNC")
  p16_col <- paste0("NEE_", type, "_16")
  p84_col <- paste0("NEE_", type, "_84")

  df |>
    transmute(
      site_id, year, igbp, data_hub,
      carbon_type = type,
      NEE      = .data[[ref_col]],
      QC       = .data[[qc_col]],
      random   = .data[[rand_col]],
      joint    = .data[[joint_col]],
      ustar_term = (.data[[p84_col]] - .data[[p16_col]]) / 2
    ) |>
    filter(!is.na(QC), !is.na(NEE), (1 - QC) <= QC_THRESHOLD_YY) |>
    mutate(
      rss = sqrt(random^2 + ustar_term^2),
      joint_minus_rss = joint - rss,
      ratio_ustar_to_random = ustar_term / random,
      dominant = case_when(
        ustar_term > random ~ "ustar_term",
        random > ustar_term ~ "random",
        TRUE ~ "tie"
      )
    )
}

nee_unc <- bind_rows(build_carbon_type(annual, "VUT"), build_carbon_type(annual, "CUT"))
msg("Qualifying site-years: VUT=", sum(nee_unc$carbon_type == "VUT"),
    ", CUT=", sum(nee_unc$carbon_type == "CUT"))

write_csv(nee_unc, file.path(OUT_DIR, "table_stage2_site_year_nee_uncertainty.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage2_site_year_nee_uncertainty.csv"),
  input_sources = db_path,
  notes = paste0(
    "One row per (site_id, year, carbon_type in {VUT, CUT}) for site-years qualifying ",
    "under the paper's own rule, applied separately to VUT and CUT: (1 - QC) <= ",
    "QC_THRESHOLD_YY (=", QC_THRESHOLD_YY, "). random = NEE_{VUT,CUT}_REF_RANDUNC, ",
    "ustar_term = (NEE_{VUT,CUT}_84 - NEE_{VUT,CUT}_16)/2, joint = ",
    "NEE_{VUT,CUT}_REF_JOINTUNC, all in gC m-2 yr-1 (annual YY carbon passes through ",
    "unconverted per CLAUDE.md Unit Conversion Reference). rss = sqrt(random^2 + ",
    "ustar_term^2); joint_minus_rss = joint - rss (should be ~0 if joint is the RSS ",
    "of the other two). dominant = whichever of random/ustar_term is larger for that ",
    "site-year. NOT the same site set as 04_qc.R's output: that applies a single ",
    "per-site VUT/CUT choice; here VUT and CUT are gated independently so both can ",
    "qualify at the same site-year (needed for Stage 3)."
  )
)

## ---------------------------------------------------------------------------
## Joint-vs-RSS test
## ---------------------------------------------------------------------------
joint_test <- nee_unc |>
  filter(!is.na(joint_minus_rss)) |>
  group_by(carbon_type) |>
  summarise(
    n = n(),
    median_joint = median(joint, na.rm = TRUE),
    median_rss = median(rss, na.rm = TRUE),
    median_diff = median(joint_minus_rss, na.rm = TRUE),
    median_abs_diff = median(abs(joint_minus_rss), na.rm = TRUE),
    median_pct_diff = median(abs(joint_minus_rss) / joint * 100, na.rm = TRUE),
    cor_joint_rss = cor(joint, rss, use = "complete.obs"),
    share_within_1pct = mean(abs(joint_minus_rss) / joint <= 0.01, na.rm = TRUE),
    .groups = "drop"
  )
msg("Joint vs RSS test:")
for (i in seq_len(nrow(joint_test))) {
  msg("  ", joint_test$carbon_type[i], ": median|diff|=", round(joint_test$median_abs_diff[i], 4),
      " (", round(joint_test$median_pct_diff[i], 2), "% of joint), cor=", round(joint_test$cor_joint_rss[i], 6),
      ", share within 1%=", round(joint_test$share_within_1pct[i], 4))
}

write_csv(joint_test, file.path(OUT_DIR, "table_stage2_joint_vs_rss_test.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage2_joint_vs_rss_test.csv"),
  input_sources = "table_stage2_site_year_nee_uncertainty.csv (derived)",
  notes = "Tests whether JOINTUNC equals sqrt(RANDUNC^2 + ustar_term^2) per site-year; summarised here by carbon_type."
)

## ---------------------------------------------------------------------------
## Summary stats: overall and by IGBP, each carbon_type
## ---------------------------------------------------------------------------
summarise_group <- function(df) {
  df |> summarise(
    n = n(),
    median_random = median(random, na.rm = TRUE),
    iqr_random = IQR(random, na.rm = TRUE),
    median_ustar = median(ustar_term, na.rm = TRUE),
    iqr_ustar = IQR(ustar_term, na.rm = TRUE),
    median_joint = median(joint, na.rm = TRUE),
    iqr_joint = IQR(joint, na.rm = TRUE),
    median_ratio_ustar_to_random = median(ratio_ustar_to_random, na.rm = TRUE),
    share_ustar_dominates = mean(dominant == "ustar_term", na.rm = TRUE),
    share_random_dominates = mean(dominant == "random", na.rm = TRUE),
    cor_random_abs_nee = suppressWarnings(cor(random, abs(NEE), use = "complete.obs")),
    cor_ustar_abs_nee = suppressWarnings(cor(ustar_term, abs(NEE), use = "complete.obs")),
    cor_joint_abs_nee = suppressWarnings(cor(joint, abs(NEE), use = "complete.obs")),
    .groups = "drop"
  )
}

summary_overall <- nee_unc |> group_by(carbon_type) |> summarise_group() |>
  mutate(group_type = "overall", group_value = "ALL", .before = 1)
summary_igbp <- nee_unc |> filter(!is.na(igbp)) |> group_by(carbon_type, group_value = igbp) |>
  summarise_group() |> mutate(group_type = "igbp", .before = 1) |>
  relocate(carbon_type, .after = group_type)

nee_summary <- bind_rows(summary_overall, summary_igbp) |>
  select(group_type, group_value, carbon_type, everything())

write_csv(nee_summary, file.path(OUT_DIR, "table_stage2_nee_uncertainty_summary.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage2_nee_uncertainty_summary.csv"),
  input_sources = "table_stage2_site_year_nee_uncertainty.csv (derived)",
  notes = paste0(
    "Medians/IQRs of random, ustar_term, joint (gC m-2 yr-1); median ratio ustar_term/",
    "random; share of site-years where each term dominates; Pearson correlation of ",
    "each term with |NEE|. Overall and by IGBP class, each carbon_type (VUT/CUT)."
  )
)

msg("Overall summary:")
print(summary_overall |> select(carbon_type, n, median_random, median_ustar, median_joint,
                                  median_ratio_ustar_to_random, share_ustar_dominates))

## ---------------------------------------------------------------------------
## LE / H: whatever uncertainty columns exist (RANDUNC only at annual step,
## per Stage 0). No ustar ensemble, no JOINTUNC for the uncorrected value.
## ---------------------------------------------------------------------------
le_h_unc <- bind_rows(
  annual |> filter(!is.na(LE_F_MDS_QC), !is.na(LE_F_MDS), (1 - LE_F_MDS_QC) <= QC_THRESHOLD_YY) |>
    transmute(site_id, year, igbp, data_hub, variable = "LE", value = LE_F_MDS,
              random = LE_RANDUNC, corr_value = LE_CORR),
  annual |> filter(!is.na(H_F_MDS_QC), !is.na(H_F_MDS), (1 - H_F_MDS_QC) <= QC_THRESHOLD_YY) |>
    transmute(site_id, year, igbp, data_hub, variable = "H", value = H_F_MDS,
              random = H_RANDUNC, corr_value = H_CORR)
)

le_h_summary <- bind_rows(
  le_h_unc |> group_by(variable) |> summarise(
    n = n(), median_value = median(value, na.rm = TRUE),
    median_random = median(random, na.rm = TRUE), iqr_random = IQR(random, na.rm = TRUE),
    median_random_pct_of_value = median(abs(random) / abs(value) * 100, na.rm = TRUE),
    cor_random_abs_value = suppressWarnings(cor(random, abs(value), use = "complete.obs")),
    n_with_corr_value = sum(!is.na(corr_value)),
    .groups = "drop"
  ) |> mutate(group_type = "overall", group_value = "ALL", .before = 1),
  le_h_unc |> filter(!is.na(igbp)) |> group_by(variable, group_value = igbp) |> summarise(
    n = n(), median_value = median(value, na.rm = TRUE),
    median_random = median(random, na.rm = TRUE), iqr_random = IQR(random, na.rm = TRUE),
    median_random_pct_of_value = median(abs(random) / abs(value) * 100, na.rm = TRUE),
    cor_random_abs_value = suppressWarnings(cor(random, abs(value), use = "complete.obs")),
    n_with_corr_value = sum(!is.na(corr_value)),
    .groups = "drop"
  ) |> mutate(group_type = "igbp", .before = 1)
) |> select(group_type, group_value, variable, everything())

write_csv(le_h_summary, file.path(OUT_DIR, "table_stage2_le_h_uncertainty_summary.csv"))
write_meta(
  file.path(OUT_DIR, "table_stage2_le_h_uncertainty_summary.csv"),
  input_sources = db_path,
  notes = paste0(
    "LE/H qualifying site-years (own QC >= ", 1 - QC_THRESHOLD_YY, "). Only RANDUNC exists ",
    "as an uncertainty column for LE/H at the annual step (Stage 0): no ustar-threshold ",
    "ensemble, no JOINTUNC for the uncorrected value, no spread column for LE_CORR/H_CORR ",
    "at this resolution (CORR spread columns exist only in the daily table). n_with_corr_value ",
    "counts qualifying site-years that also have a non-NA energy-balance-corrected value."
  )
)
msg("LE/H overall summary:")
print(le_h_summary |> filter(group_type == "overall"))

## ---------------------------------------------------------------------------
## Figures
## ---------------------------------------------------------------------------
p1 <- nee_unc |>
  filter(!is.na(joint), !is.na(rss)) |>
  ggplot(aes(x = rss, y = joint, color = carbon_type)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  coord_equal() +
  labs(title = "Annual NEE: JOINTUNC vs sqrt(RANDUNC^2 + ustar_term^2)",
       subtitle = "Dashed line = 1:1 (what JOINTUNC would equal if it were the RSS of the other two terms)",
       x = "sqrt(random^2 + ustar_term^2)  (gC m-2 yr-1)", y = "JOINTUNC  (gC m-2 yr-1)",
       color = "Carbon type") +
  theme_bw()
ggsave(file.path(OUT_DIR, "fig_stage2_joint_vs_rss.png"), p1, width = 7.5, height = 6, dpi = 150, bg = "white")

p2 <- nee_unc |>
  select(carbon_type, random, ustar_term, joint) |>
  pivot_longer(cols = c(random, ustar_term, joint), names_to = "term", values_to = "value") |>
  mutate(term = factor(term, levels = c("random", "ustar_term", "joint"))) |>
  ggplot(aes(x = term, y = value, fill = carbon_type)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.5, position = position_dodge(width = 0.8)) +
  labs(title = "Annual NEE uncertainty terms, qualifying site-years",
       x = NULL, y = "gC m-2 yr-1", fill = "Carbon type") +
  theme_bw()
ggsave(file.path(OUT_DIR, "fig_stage2_uncertainty_terms_boxplot.png"), p2, width = 7.5, height = 5.5, dpi = 150, bg = "white")

p3 <- nee_unc |>
  filter(carbon_type == "VUT") |>
  ggplot(aes(x = abs(NEE))) +
  geom_point(aes(y = random, color = "random"), alpha = 0.2, size = 0.7) +
  geom_point(aes(y = ustar_term, color = "ustar_term"), alpha = 0.2, size = 0.7) +
  geom_smooth(aes(y = random, color = "random"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = ustar_term, color = "ustar_term"), method = "loess", se = FALSE) +
  labs(title = "Annual NEE_VUT: uncertainty terms vs |NEE|",
       x = "|NEE| (gC m-2 yr-1)", y = "Uncertainty term (gC m-2 yr-1)", color = "Term") +
  theme_bw()
ggsave(file.path(OUT_DIR, "fig_stage2_uncertainty_vs_nee_magnitude.png"), p3, width = 7.5, height = 5.5, dpi = 150, bg = "white")

msg("Stage 2 complete.")

## ---------------------------------------------------------------------------
## Status
## ---------------------------------------------------------------------------
status_path <- file.path(OUT_DIR, "status.md")
vut_row <- summary_overall |> filter(carbon_type == "VUT")
cut_row <- summary_overall |> filter(carbon_type == "CUT")
jt_vut <- joint_test |> filter(carbon_type == "VUT")
jt_cut <- joint_test |> filter(carbon_type == "CUT")

status_entry <- c(
  "",
  "## Stage 2 -- Uncertainty at the annual step",
  "",
  paste0("Completed ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "."),
  "",
  paste0("- Qualifying site-years ((1-QC)<=QC_THRESHOLD_YY=", QC_THRESHOLD_YY, "), own-QC-gated per variable: VUT=",
         vut_row$n, ", CUT=", cut_row$n, "."),
  paste0("- Joint vs RSS(random, ustar_term) test: VUT median|diff|=", round(jt_vut$median_abs_diff, 3),
         " gC/m2/yr (", round(jt_vut$median_pct_diff, 2), "% of joint, cor=", round(jt_vut$cor_joint_rss, 5),
         "); CUT median|diff|=", round(jt_cut$median_abs_diff, 3), " gC/m2/yr (",
         round(jt_cut$median_pct_diff, 2), "% of joint, cor=", round(jt_cut$cor_joint_rss, 5), ")."),
  paste0("- VUT: median random=", round(vut_row$median_random, 2), ", median ustar_term=",
         round(vut_row$median_ustar, 2), ", median joint=", round(vut_row$median_joint, 2),
         " gC/m2/yr; ustar_term dominates ", round(vut_row$share_ustar_dominates * 100, 1),
         "% of site-years, random dominates ", round(vut_row$share_random_dominates * 100, 1), "%."),
  paste0("- CUT: median random=", round(cut_row$median_random, 2), ", median ustar_term=",
         round(cut_row$median_ustar, 2), ", median joint=", round(cut_row$median_joint, 2),
         " gC/m2/yr; ustar_term dominates ", round(cut_row$share_ustar_dominates * 100, 1),
         "% of site-years, random dominates ", round(cut_row$share_random_dominates * 100, 1), "%."),
  "- LE/H: only RANDUNC exists at the annual step (no ustar ensemble, no JOINTUNC for the uncorrected value); summarised in table_stage2_le_h_uncertainty_summary.csv.",
  "",
  "Outputs: table_stage2_site_year_nee_uncertainty.csv, table_stage2_joint_vs_rss_test.csv, ",
  "table_stage2_nee_uncertainty_summary.csv, table_stage2_le_h_uncertainty_summary.csv, ",
  "fig_stage2_joint_vs_rss.png, fig_stage2_uncertainty_terms_boxplot.png, ",
  "fig_stage2_uncertainty_vs_nee_magnitude.png (+ .meta.json each).",
  ""
)
write_lines(status_entry, status_path, append = TRUE)

cat("\n--- dq_stage2_annual_uncertainty.R: done. See ", LOG_FILE, " ---\n")
