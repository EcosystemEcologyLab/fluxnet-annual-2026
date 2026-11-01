## 09_figure_iav_15sites.R — GSL expanded analysis, draft figure: missed GPP
## and interannual variability across 15 sites spanning the skewness range.
## Dave's design, 2026-10-31 -- see
## logs/prompts/2026-10-31_gsl_figure_iav_15sites.md.
##
## Read-only against the already-committed session 2a tables (main variant):
## carbon_gain_data.csv (annual GPP/NEP, Jul-Jun season years for AU-How --
## the SAME documented simplification 03_models.R's own cg_data already
## uses, not a new one introduced here) and derived_site_year.csv (per-year
## F at Z17/T95, for the "missed fraction" in panel a). No raw daily pull is
## needed -- both source tables were themselves built read-only from the
## repo-root DuckDB store, dataset = 'FLUXMET' only (01_select.R/02_derive.R).
## Local write_meta(). Does NOT modify any existing script, table, or
## figure -- a brand-new script, writing only to figures/drafts/ and a new
## tables/iav_15sites.csv.

source("GSL_definition/expanded/code/00_config.R")
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(ggplot2); library(patchwork); library(ggrepel); library(scales)
})

exp_sites          <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
derived_site       <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE) |> filter(variant == "main")
derived_site_year  <- read_csv(file.path(EXP_TABLES, "derived_site_year.csv"), show_col_types = FALSE) |> filter(variant == "main")
cg_data            <- read_csv(file.path(EXP_TABLES, "carbon_gain_data.csv"), show_col_types = FALSE)

IAV_SITES <- c("DE-Tha", "NL-Loo", "AU-How", "CH-Dav", "US-MMS", "FR-Pue", "AT-Neu", "US-Mpj",
               "JP-Api", "US-Ha1", "IL-Yat", "US-Var", "US-Ton", "US-Wkg", "US-SRM")

## ---- Validation: every site present, >=12 R2/N=14 passing years. stop()
## (not a warning) if not, per the prompt's own instruction. ----------------
missing_sites <- setdiff(IAV_SITES, exp_sites$site_id)
if (length(missing_sites) > 0) {
  stop("09_figure_iav_15sites.R: site(s) missing from the expanded set: ", paste(missing_sites, collapse = ", "))
}
site_nyears <- exp_sites |> filter(site_id %in% IAV_SITES) |> select(site_id, igbp, n_passing_years_r2g14) |>
  left_join(derived_site |> select(site_id, skewness), by = "site_id") |>
  arrange(skewness)
few_years <- site_nyears$site_id[site_nyears$n_passing_years_r2g14 < 12L]
if (length(few_years) > 0) {
  stop("09_figure_iav_15sites.R: site(s) with fewer than 12 passing years: ",
       paste(sprintf("%s (%d yr)", few_years, site_nyears$n_passing_years_r2g14[site_nyears$site_id %in% few_years]), collapse = ", "))
}
message("All 15 sites present with >=12 R2/N=14 passing years:")
for (i in seq_len(nrow(site_nyears))) {
  message(sprintf("  %-7s %s  skewness = %6.3f  n = %2d yr", site_nyears$site_id[i], site_nyears$igbp[i],
                   site_nyears$skewness[i], site_nyears$n_passing_years_r2g14[i]))
}

## ============================================================================
## Steiger (1980) Z1* test for the difference between two dependent,
## overlapping correlations that share one variable -- here, GPP's
## correlation with Z17-GSL (r12) vs. with T95-GSL (r13), using r23 = the
## correlation between the two (detrended) GSL series themselves, as the
## prompt specifies. Steiger, J.H. (1980) "Tests for comparing elements of
## a correlation matrix", Psychological Bulletin 87:245-251 -- the Z1*
## formula as widely reproduced (e.g. Lee & Preacher 2013; cocor::cocor.
## dep.groups.overlap(test = "steiger1980")). Implemented directly: neither
## `cocor` nor `psych` is in this project's renv lockfile, and CLAUDE.md
## asks that new package dependencies not be introduced without discussion.
## ============================================================================
steiger1980_z <- function(r12, r13, r23, n) {
  if (any(is.na(c(r12, r13, r23))) || n < 4) return(c(z = NA_real_, p = NA_real_))
  fisher_z <- function(r) 0.5 * log((1 + r) / (1 - r))
  z12 <- fisher_z(r12); z13 <- fisher_z(r13)
  rbar <- (r12 + r13) / 2
  denom <- 1 - rbar^2
  if (denom <= 0) return(c(z = NA_real_, p = NA_real_)) # degenerate (|rbar| ~= 1); cannot happen for these data, guarded anyway
  f <- min(1, (1 - r23) / (2 * denom))
  h <- (1 - f * rbar^2) / denom
  z_stat <- (z12 - z13) * sqrt(n - 3) / sqrt(2 * (1 - r23) * h)
  c(z = unname(z_stat), p = unname(2 * (1 - pnorm(abs(z_stat)))))
}

## ---- Per-site table -------------------------------------------------------
## "Use every passing year at each site": missed_frac_* uses ALL of a
## site's R2/N=14 passing years (f_z17/f_t95 need no GPP join). The
## detrended correlations/Steiger's test additionally require a non-NA
## gsl_z17/gsl_t95/annual_gpp/annual_nep for that year, which is not true
## of every passing year, for two distinct, pre-existing reasons (neither
## introduced here -- both are already present in the committed source
## tables, just surfaced by this script's complete-case filter):
##   1. AU-How: cg_data's annual_gpp/annual_nep (from annual_inputs.csv)
##      are only populated for AU-How's 9 Jul-Jun-valid season years, not
##      all 13 of its passing-year labels -- the same documented
##      simplification 03_models.R's own carbon_gain_data.csv.meta.json
##      already carries.
##   2. CH-Dav (2008/2010/2011) and IL-Yat (2010): genuine site-years with
##      NO underlying daily GPP data at all -- derived_site_year.csv's
##      gsl_z17/gsl_t95 are correctly NA for these (derive_one()'s
##      year_threshold_matrices() leaves a year NA when it has zero
##      non-missing daily values), but annual_inputs.csv's annual_gpp for
##      the SAME year is 0, not NA (sum(..., na.rm = TRUE) over zero
##      non-missing days returns 0, not NA) -- an inconsistency between
##      the two source tables for an empty year, not a Jul-Jun artifact
##      (both are Northern-Hemisphere, calendar-year-labelled sites). The
##      gsl_z17/gsl_t95 NA correctly excludes these years here either way.
## n_years_used reports the smaller count actually used wherever it
## differs from n_years; both reasons are logged to the console below.
f_tbl <- derived_site_year |> filter(site_id %in% IAV_SITES) |> select(site_id, year, f_z17, f_t95)

site_iav_one <- function(sid) {
  d <- cg_data |> filter(site_id == sid) |> arrange(year) |> left_join(f_tbl, by = c("site_id", "year"))
  n_years_full <- nrow(d)

  missed <- 1 - d$f_z17
  missed_mean <- mean(missed, na.rm = TRUE)
  missed_q1 <- as.numeric(quantile(missed, 0.25, na.rm = TRUE, type = 7))
  missed_q3 <- as.numeric(quantile(missed, 0.75, na.rm = TRUE, type = 7))

  dc <- d |> filter(!is.na(annual_gpp), !is.na(annual_nep), !is.na(gsl_z17), !is.na(gsl_t95))
  n_years_used <- nrow(dc)

  detrend <- function(y) residuals(lm(y ~ dc$year))
  r_gpp  <- detrend(dc$annual_gpp)
  r_nep  <- detrend(dc$annual_nep)
  r_gz17 <- detrend(dc$gsl_z17)
  r_gt95 <- detrend(dc$gsl_t95)

  ct <- function(a, b) { t <- cor.test(a, b); c(r = unname(t$estimate), p = t$p.value) }
  gpp_z17 <- ct(r_gz17, r_gpp); gpp_t95 <- ct(r_gt95, r_gpp)
  nep_z17 <- ct(r_gz17, r_nep); nep_t95 <- ct(r_gt95, r_nep)
  r_gsl12 <- cor(r_gz17, r_gt95)

  st <- steiger1980_z(gpp_z17["r"], gpp_t95["r"], r_gsl12, n_years_used)

  tibble(
    site_id = sid, igbp = exp_sites$igbp[exp_sites$site_id == sid],
    n_years = n_years_full, n_years_used = n_years_used,
    skewness = derived_site$skewness[derived_site$site_id == sid],
    missed_frac_mean = missed_mean, missed_frac_q1 = missed_q1, missed_frac_q3 = missed_q3,
    missed_frac_iqr = missed_q3 - missed_q1,
    r_gpp_z17 = unname(gpp_z17["r"]), p_gpp_z17 = unname(gpp_z17["p"]),
    r_gpp_t95 = unname(gpp_t95["r"]), p_gpp_t95 = unname(gpp_t95["p"]),
    r_nep_z17 = unname(nep_z17["r"]), p_nep_z17 = unname(nep_z17["p"]),
    r_nep_t95 = unname(nep_t95["r"]), p_nep_t95 = unname(nep_t95["p"]),
    r_gsl_z17_t95 = r_gsl12, steiger_z = unname(st["z"]), steiger_p = unname(st["p"])
  )
}

iav_tbl <- bind_rows(lapply(IAV_SITES, site_iav_one)) |> arrange(skewness)

n_reduced <- iav_tbl |> filter(n_years_used < n_years)
if (nrow(n_reduced) > 0) {
  message("Site(s) where n_years_used < n_years for the detrended correlations/Steiger's test: ",
          paste(sprintf("%s (%d of %d)", n_reduced$site_id, n_reduced$n_years_used, n_reduced$n_years), collapse = "; "),
          ". AU-How: Jul-Jun annual_inputs.csv join gap (2008/2017/2019/2024). CH-Dav/IL-Yat: genuine ",
          "empty site-years (gsl_z17/gsl_t95 NA; annual_gpp a trivial 0, not NA, for the same year) -- see ",
          "the comment above site_iav_one() and iav_15sites.csv's own .meta.json.")
}

write_csv(iav_tbl, file.path(EXP_TABLES, "iav_15sites.csv"))
write_meta(
  file.path(EXP_TABLES, "iav_15sites.csv"),
  input_sources = c(file.path(EXP_TABLES, "sites.csv"), file.path(EXP_TABLES, "derived_site.csv"),
                     file.path(EXP_TABLES, "derived_site_year.csv"), file.path(EXP_TABLES, "carbon_gain_data.csv")),
  notes = paste0(
    "15-site draft IAV table (Dave's design). n_years = R2/N=14 passing years (sites.csv); ",
    "n_years_used = years with non-NA annual_gpp/annual_nep/gsl_z17/gsl_t95 actually used for the ",
    "detrended correlations and Steiger's test -- less than n_years for AU-How (9 of 13: the Jul-Jun ",
    "season-year join already documented in carbon_gain_data.csv's own .meta.json), CH-Dav (24 of 27) ",
    "and IL-Yat (12 of 13) (genuine empty site-years -- gsl_z17/gsl_t95 NA, annual_gpp a trivial 0 not ",
    "NA for the same year; see the script's own comments). ",
    "missed_frac_* = 1 - f_z17 per year (fraction of that year's annual GPP outside the Z17 GSL), ",
    "mean/Q1/Q3/IQR across n_years. r_*/p_* = Pearson correlation/p-value between OLS-on-year ",
    "residuals of GSL (at Z17 or T95) and of GPP or NEP. r_gsl_z17_t95 = correlation between the ",
    "two detrended GSL series (the 'shared-variable' correlation Steiger's test needs). steiger_z/",
    "steiger_p = Steiger (1980) Z1* test for r_gpp_z17 vs. r_gpp_t95 (dependent, overlapping, GPP ",
    "in common), using r_gsl_z17_t95."
  )
)
message("Saved: ", file.path(EXP_TABLES, "iav_15sites.csv"))

## ============================================================================
## Panel a: fraction of annual GPP outside the Z17 GSL (1 - F at 10% of
## P99) against skewness, one point per site at the across-year mean, IQR
## error bar, dashed line at 5% (T95's by-construction excluded share).
## ============================================================================
pa_cor <- cor.test(iav_tbl$skewness, iav_tbl$missed_frac_mean)
message(sprintf("Panel a: r = %.3f (p = %.3g), n = %d.", pa_cor$estimate, pa_cor$p.value, nrow(iav_tbl)))

p_a <- ggplot(iav_tbl, aes(x = skewness, y = missed_frac_mean)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "grey50", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_errorbar(aes(ymin = missed_frac_q1, ymax = missed_frac_q3), width = 0.03, colour = "grey40",
                linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_point(size = 1.8, colour = "grey20") +
  ggrepel::geom_text_repel(aes(label = site_id), size = PEERJ_ANNOTATION_PT / .pt, seed = 1,
                            min.segment.length = 0, colour = "grey20", box.padding = 0.5,
                            point.padding = 0.25, force = 4, force_pull = 0.5, max.overlaps = Inf,
                            segment.size = 0.25, max.iter = 20000) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.5,
           label = sprintf("r = %.2f, n = %d", pa_cor$estimate, nrow(iav_tbl)),
           size = PEERJ_ANNOTATION_PT / .pt) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  labs(x = "Skewness", y = "Fraction of annual GPP outside\nthe Z17 GSL (mean, IQR bar)") +
  theme_gsl() + gsl_panel_letter("a")

## ============================================================================
## Panel b: interannual link between GSL and GPP -- detrended Pearson r at
## Z17 (black) and T95 (red), against skewness, the two points for a site
## joined by a thin vertical segment; filled = p < 0.05, open otherwise.
## ============================================================================
pb_long <- bind_rows(
  iav_tbl |> transmute(site_id, skewness, threshold = "Z17", r = r_gpp_z17, p = p_gpp_z17),
  iav_tbl |> transmute(site_id, skewness, threshold = "T95", r = r_gpp_t95, p = p_gpp_t95)
) |> mutate(sig = p < 0.05)

p_b <- ggplot(pb_long, aes(x = skewness, y = r)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_line(aes(group = site_id), colour = "grey70", linewidth = nature_lwd(0.5)) +
  geom_point(aes(colour = threshold, shape = sig), size = 2) +
  scale_color_manual(values = GSL_MARKER_COLORS, breaks = c("Z17", "T95"), name = NULL) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                      labels = c(`TRUE` = "p < 0.05", `FALSE` = "p >= 0.05"), name = NULL) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  labs(x = "Skewness", y = "Detrended Pearson r,\nGSL vs. annual GPP") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("b")

fig_iav <- p_a / p_b + plot_layout(heights = c(1.25, 1))

## ---- Save: PNG only (as asked), under figures/drafts/ -- NOT
## save_gsl_figure() (writes PDF, and into GSL_FIG_DIR's root "Figure N"
## numbered sequence, which this explicitly must not touch/overwrite). ----
drafts_dir <- file.path(GSL_FIG_DIR, "drafts")
if (!dir.exists(drafts_dir)) dir.create(drafts_dir, recursive = TRUE)
draft_stem <- file.path(drafts_dir, "Figure GSL-IAV draft")

ggplot2::ggsave(paste0(draft_stem, ".png"), fig_iav, width = PEERJ_FIG_WIDTH_MM, height = 190, units = "mm",
                 dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")

iav_caption <- c(
  "Figure GSL-IAV (draft). Missed GPP and interannual variability (IAV) across 15 sites spanning",
  "the expanded set's skewness range (0.37-2.41), each with >=12 R2/N=14 passing years, every",
  "passing year used (AU-How: Jul-Jun Southern-Hemisphere season years, as in carbon_gain_data.csv).",
  "The detrended correlations/Steiger's test use a smaller, complete-case n at 3 sites -- AU-How (9",
  "of 13, Jul-Jun join gap), CH-Dav (24 of 27) and IL-Yat (12 of 13, genuine empty site-years) -- see",
  "tables/iav_15sites.csv's own notes.",
  "Panel a: fraction of annual GPP outside the Z17 GSL (1 - F at 10% of P99), one point per site at",
  "its across-year mean, bar = interannual IQR; dashed line at 5% (the share T95 excludes by",
  "construction); Pearson r and n printed in-panel.",
  "Panel b: the interannual link between annual GSL and annual GPP -- annual GSL and annual GPP each",
  "detrended (OLS on year) separately, then Pearson r between the residuals, at Z17 (black) and T95",
  "(red); the two points for a site joined by a thin vertical segment; filled = p < 0.05, open",
  "otherwise. Numbers behind both panels, plus NEP correlations and Steiger's (1980) Z1* test for",
  "each site's Z17- vs. T95-GPP correlation (sharing GPP; r between the detrended GSL series used as",
  "the required third correlation): tables/iav_15sites.csv."
)
writeLines(iav_caption, paste0(draft_stem, ".legend.txt"))

write_meta(
  paste0(draft_stem, ".png"),
  input_sources = c(file.path(EXP_TABLES, "iav_15sites.csv"), file.path(EXP_TABLES, "carbon_gain_data.csv"),
                     file.path(EXP_TABLES, "derived_site_year.csv"), file.path(EXP_TABLES, "sites.csv")),
  notes = "Draft figure (Dave's design) -- not a numbered manuscript figure. PNG only, PeerJ sizing/dpi."
)
message("Saved: ", draft_stem, ".png/.legend.txt/.meta.json")

message("--- 09_figure_iav_15sites.R: done ---")
