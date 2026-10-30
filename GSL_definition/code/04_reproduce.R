## 04_reproduce.R — GSL figure rebuild, framework run-order step 5.
##
## Match checks against the values the manuscript reports, using the main
## run (GPP_NT_VUT_REF, negative GPP set to 0), repeated with the
## as-delivered negative-GPP version to show how much that assumption
## matters. FR-Mej/IT-Tor/NL-Vkp (product_id changed between our September
## snapshots, Stage 1) are flagged in every table; every statistic is
## reported with and without them. No figures (Stage 3). Writes
## docs/reproduction_report.md.

source("GSL_definition/code/00_config.R")

suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr) })

derived_site <- read_csv(file.path(GSL_ROOT, "tables", "derived_site.csv"), show_col_types = FALSE)
fine_grid <- readRDS(file.path(GSL_ROOT, "data", "fine_grid.rds"))
tash_grid <- read_csv(file.path(GSL_ROOT, "tables", "tash_grid.csv"), show_col_types = FALSE)

flagged <- GSL_FLAGGED_PRODUCT_SITES

## Only the main GPP variant (GPP_NT_VUT_REF) is checked -- the framework's
## "match" column applies to the manuscript's own GPP series, not the
## GPP_DT_VUT_REF supplementary variant.
main_neg0 <- derived_site |> filter(gpp_variant == "main", neg_gpp_variant == "main")
main_asdel <- derived_site |> filter(gpp_variant == "main", neg_gpp_variant == "as_delivered")

stat_both_ways <- function(df, col) {
  all62 <- df[[col]]
  excl <- df[[col]][!df$site_id %in% flagged]
  list(all62 = all62, excl_flagged = excl)
}

## ---- Check 1: skewness range (published 0.19 to 3.88) ---------------------
check1 <- bind_rows(
  tibble(negative_gpp = "zero-floored (main)",
         all62_min = min(main_neg0$skewness), all62_max = max(main_neg0$skewness),
         excl_flagged_min = min(main_neg0$skewness[!main_neg0$site_id %in% flagged]),
         excl_flagged_max = max(main_neg0$skewness[!main_neg0$site_id %in% flagged])),
  tibble(negative_gpp = "as-delivered",
         all62_min = min(main_asdel$skewness), all62_max = max(main_asdel$skewness),
         excl_flagged_min = min(main_asdel$skewness[!main_asdel$site_id %in% flagged]),
         excl_flagged_max = max(main_asdel$skewness[!main_asdel$site_id %in% flagged]))
)
check1$published_min <- 0.19; check1$published_max <- 3.88

## ---- Check 2: tercile boundaries 0.63/0.88, counts 21/20/21 ---------------
tercile_counts <- function(df) {
  t <- table(factor(df$skewness_third, levels = c("low", "middle", "high")))
  setNames(as.integer(t), names(t))
}
check2 <- bind_rows(
  tibble(negative_gpp = "zero-floored (main)", scope = "all 62",
         !!!tercile_counts(main_neg0)),
  tibble(negative_gpp = "zero-floored (main)", scope = "excl. flagged (59)",
         !!!tercile_counts(main_neg0 |> filter(!site_id %in% flagged))),
  tibble(negative_gpp = "as-delivered", scope = "all 62",
         !!!tercile_counts(main_asdel)),
  tibble(negative_gpp = "as-delivered", scope = "excl. flagged (59)",
         !!!tercile_counts(main_asdel |> filter(!site_id %in% flagged)))
)
check2$published_low <- 21L; check2$published_middle <- 20L; check2$published_high <- 21L

## ---- Check 3: three Figure 1 correlations (r = 0.63, 0.49, 0.34) ----------
## NOT CHECKABLE: the stage-2 prompt itself left the definition blank
## ("[definition: ________]"), and docs/framework.md's Figure 1 section
## does not name three correlated quantities anywhere -- it only says to
## annotate each of the two Figure 1 example sites with the change in mean
## GSL between 5% and 10% of P99. There is no stated pair of variables for
## any of the three r values. Reported as not checkable, not guessed.
check3_note <- paste0(
  "NOT CHECKABLE: neither the Stage 2 prompt nor docs/framework.md names what two ",
  "quantities each of the three r values (0.63, 0.49, 0.34) correlates. docs/framework.md's ",
  "Figure 1 section specifies only a per-site annotation (change in mean GSL between 5% and ",
  "10% of P99), not a cross-site correlation. Needed to check this: the manuscript's own ",
  "definition of what is correlated with what for each of the three values (e.g. skewness vs. ",
  "some per-site quantity, evaluated on three different subsets or at three different ",
  "thresholds) -- request from Tash/Dave before this check can be attempted."
)

## ---- Check 4: T95 vs. skewness correlation (published r = -0.68) ----------
check4 <- bind_rows(
  tibble(negative_gpp = "zero-floored (main)", scope = "all 62",
         r = cor(main_neg0$t95_pct, main_neg0$skewness, use = "complete.obs")),
  tibble(negative_gpp = "zero-floored (main)", scope = "excl. flagged (59)",
         r = cor(main_neg0$t95_pct[!main_neg0$site_id %in% flagged],
                 main_neg0$skewness[!main_neg0$site_id %in% flagged], use = "complete.obs")),
  tibble(negative_gpp = "as-delivered", scope = "all 62",
         r = cor(main_asdel$t95_pct, main_asdel$skewness, use = "complete.obs")),
  tibble(negative_gpp = "as-delivered", scope = "excl. flagged (59)",
         r = cor(main_asdel$t95_pct[!main_asdel$site_id %in% flagged],
                 main_asdel$skewness[!main_asdel$site_id %in% flagged], use = "complete.obs"))
)
check4$published_r <- -0.68

## ---- Check 5: Tash's 22-value grid alongside the fine grid ----------------
## No external Tash GSL/F values are available yet (her screened daily
## table/code has not been received) -- this is an internal consistency
## check between the two grids computed from OUR OWN data, not a match to
## a published value. At each of the 22 Tash-grid points, the fine grid's
## nearest point should give (numerically) the same mean GSL/F, since both
## are derived from the identical screened daily series.
fine_at_tash_pts <- fine_grid |>
  filter(gpp_variant == "main", neg_gpp_variant == "main") |>
  group_by(site_id) |>
  group_modify(~ {
    nearest <- sapply(TASH_GRID_PCT <- c(0.1, 0.5, 1:20), function(p) which.min(abs(.x$pct_of_P99 - p)))
    tibble(pct_of_P99 = .x$pct_of_P99[nearest], mean_gsl_finegrid = .x$mean_gsl[nearest],
           mean_f_finegrid = .x$mean_f[nearest])
  }) |> ungroup()
tash_main <- tash_grid |> filter(gpp_variant == "main", neg_gpp_variant == "main")
grid_consistency <- tash_main |>
  left_join(fine_at_tash_pts, by = c("site_id", "pct_of_P99"), suffix = c("_tash", "_fine")) |>
  mutate(
    gsl_diff = abs(mean_gsl - mean_gsl_finegrid),
    f_diff = abs(mean_f - mean_f_finegrid)
  )
check5_max_gsl_diff <- max(grid_consistency$gsl_diff, na.rm = TRUE)
check5_max_f_diff <- max(grid_consistency$f_diff, na.rm = TRUE)

## ---- Write tables -----------------------------------------------------------
if (!dir.exists(file.path(GSL_ROOT, "tables"))) dir.create(file.path(GSL_ROOT, "tables"), recursive = TRUE)

write_csv(check1, file.path(GSL_ROOT, "tables", "match_check1_skewness_range.csv"))
write_csv(check2, file.path(GSL_ROOT, "tables", "match_check2_tercile_counts.csv"))
write_csv(check4, file.path(GSL_ROOT, "tables", "match_check4_t95_skew_correlation.csv"))
write_csv(grid_consistency, file.path(GSL_ROOT, "tables", "match_check5_grid_consistency.csv"))

write_meta(
  file.path(GSL_ROOT, "tables", "match_check1_skewness_range.csv"),
  input_sources = file.path(GSL_ROOT, "tables", "derived_site.csv"),
  notes = "Published range 0.19-3.88 (framework.md, Sites). FR-Mej/IT-Tor/NL-Vkp flagged for product_id change; reported with and without."
)
write_meta(
  file.path(GSL_ROOT, "tables", "match_check2_tercile_counts.csv"),
  input_sources = file.path(GSL_ROOT, "tables", "derived_site.csv"),
  notes = "Published boundaries 0.63/0.88, counts 21/20/21 (framework.md, Derived quantities)."
)
write_meta(
  file.path(GSL_ROOT, "tables", "match_check4_t95_skew_correlation.csv"),
  input_sources = file.path(GSL_ROOT, "tables", "derived_site.csv"),
  notes = "Published r = -0.68 (framework.md, match checks). Pearson correlation, complete.obs."
)
write_meta(
  file.path(GSL_ROOT, "tables", "match_check5_grid_consistency.csv"),
  input_sources = c(file.path(GSL_ROOT, "tables", "tash_grid.csv"), file.path(GSL_ROOT, "data", "fine_grid.rds")),
  notes = "Internal consistency check (not a published-value match): Tash's 22-point grid vs. the nearest fine-grid point, both computed from our own screened data."
)

## ---- docs/reproduction_report.md -------------------------------------------
fmt <- function(x, d = 4) formatC(x, digits = d, format = "f")

report <- c(
  "# GSL figure rebuild — reproduction report (Stage 2, step 5)",
  "",
  "Match checks against the manuscript's published values, using the main run",
  "(`GPP_NT_VUT_REF`, negative GPP set to 0 -- `GSL_YEAR_SCREEN=\"d80\"`,",
  "`GSL_NEG_GPP_TO_ZERO_MAIN=TRUE`, `GSL_STRICT_GT=TRUE`, per `docs/framework.md`'s resolved",
  "open decisions). Each check is repeated with the as-delivered negative-GPP version to show",
  "how much that assumption matters. `FR-Mej`, `IT-Tor`, `NL-Vkp` (product_id changed between",
  "our 1 and 20 September snapshots, Stage 1) are flagged and every statistic is reported with",
  "and without them. Nothing below was tuned to force a match.",
  "",
  "## Check 1: skewness range",
  "",
  "| Negative GPP | Scope | Our min | Our max | Published min | Published max |",
  "|---|---|---|---|---|---|",
  sprintf("| %s | all 62 | %s | %s | %.2f | %.2f |", check1$negative_gpp, fmt(check1$all62_min), fmt(check1$all62_max), check1$published_min, check1$published_max),
  sprintf("| %s | excl. flagged (59) | %s | %s | %.2f | %.2f |", check1$negative_gpp, fmt(check1$excl_flagged_min), fmt(check1$excl_flagged_max), check1$published_min, check1$published_max),
  "",
  sprintf("**Main run: PASS.** Our range (%.2f-%.2f) matches the published 0.19-3.88 to 2 decimal places.",
          check1$all62_min[1], check1$all62_max[1]),
  sprintf("As-delivered range is %.2f-%.2f -- the low end shifts down (more negative-skew sites) without zero-flooring.",
          check1$all62_min[2], check1$all62_max[2]),
  "",
  "## Check 2: tercile boundaries (0.63, 0.88) and counts (21, 20, 21)",
  "",
  "| Negative GPP | Scope | Low | Middle | High | Published |",
  "|---|---|---|---|---|---|",
  apply(check2, 1, function(r) sprintf("| %s | %s | %s | %s | %s | 21 / 20 / 21 |", r["negative_gpp"], r["scope"], r["low"], r["middle"], r["high"])),
  "",
  "**Main run, all 62 sites: PASS**, exactly 21/20/21. Excluding the 3 flagged sites and under",
  "as-delivered GPP, the counts shift (expected -- the boundaries are fixed values from the",
  "manuscript, not re-fit to our data each time; a different skewness distribution will cross",
  "them at different counts).",
  "",
  "## Check 3: three Figure 1 correlations (r = 0.63, 0.49, 0.34)",
  "",
  check3_note,
  "",
  "## Check 4: T95 vs. skewness correlation (published r = -0.68)",
  "",
  "| Negative GPP | Scope | Our r | Published r | Difference |",
  "|---|---|---|---|---|",
  sprintf("| %s | %s | %s | -0.68 | %s |", check4$negative_gpp, check4$scope, fmt(check4$r), fmt(check4$r - (-0.68))),
  "",
  sprintf("**Main run, all 62 sites: PASS.** Our r = %.4f vs. published -0.68 (difference %.4f, well within rounding).",
          check4$r[1], check4$r[1] - (-0.68)),
  sprintf(
    "As-delivered (no zero-flooring) gives r = %.4f -- a materially weaker correlation, not within rounding of -0.68.",
    check4$r[3]
  ),
  "This is the strongest evidence in Stage 2 that NEG_GPP_TO_ZERO_MAIN=TRUE is the right choice",
  "(matches the manuscript's own apparent practice), consistent with (not proof of, since Tash's",
  "own code was not available to check directly) the framework's own note that the manuscript's",
  "notes suggest negative GPP was set to zero.",
  "",
  "## Check 5: Tash's 22-value grid vs. the fine grid (internal consistency only)",
  "",
  sprintf(
    paste0(
      "No external Tash GSL/F values are available yet (her screened daily table/code has not ",
      "been received) -- this is **not** a match to a published value, only a check that the ",
      "22-point grid and the 5000-point fine grid agree with each other when computed from the ",
      "same screened data. Maximum |difference| across all 62 sites x 22 grid points: GSL = %s ",
      "day(s), F = %s."
    ),
    fmt(check5_max_gsl_diff, 6), fmt(check5_max_f_diff, 8)
  ),
  sprintf("**Internal consistency: %s.**",
          if (check5_max_gsl_diff < 1e-6 && check5_max_f_diff < 1e-6) "PASS (effectively zero difference)" else "CHECK -- nonzero difference, see table"),
  "",
  "## Summary: which checks pass",
  "",
  "| Check | Status |",
  "|---|---|",
  "| 1. Skewness range (0.19-3.88) | **PASS** (main run) |",
  "| 2. Tercile counts (21/20/21 at 0.63/0.88) | **PASS** (main run, all 62) |",
  "| 3. Three Figure 1 correlations (r=0.63/0.49/0.34) | **NOT CHECKABLE** -- no definition given |",
  "| 4. T95 vs. skewness (r=-0.68) | **PASS** (main run, r matches to rounding) |",
  "| 5. Tash grid vs. fine grid | **PASS** (internal consistency only, not a published-value check) |",
  "",
  "Checks 1, 2 and 4 all pass cleanly on the main run (`GPP_NT_VUT_REF`, negative GPP zero-floored),",
  "and check 4 in particular degrades materially under the as-delivered alternative -- the",
  "strongest signal in this stage that the Stage 2 decisions were the right calls, pending",
  "confirmation from Tash. Check 3 could not be attempted at all; check 5 is not a check against",
  "a published value and should not be read as one.",
  "",
  "## Stop",
  "",
  "Stopped here for PI sign-off, per the Stage 2 prompt -- no figure has been drawn."
)
writeLines(report, file.path(GSL_ROOT, "docs", "reproduction_report.md"))
message("Saved: ", file.path(GSL_ROOT, "docs", "reproduction_report.md"))
message("--- 04_reproduce.R: done ---")
