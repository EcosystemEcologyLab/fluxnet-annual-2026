## 00_config.R — GSL expanded-site analysis, side-project config.
##
## Sourced by every script in GSL_definition/expanded/code/. Not run directly.
##
## Reuses GSL_definition/code/00_config.R (site table, QC thresholds,
## qc_passes(), write_meta(), log paths) and GSL_definition/code/fig_helpers.R
## (PeerJ figure style) BY SOURCING, not copying -- see
## ../../logs/prompts/2026-10-30_gsl_expanded.md. Everything this script
## writes goes under GSL_definition/expanded/ so the existing 62-site outputs
## (GSL_definition/tables/, figures/, data/) are never touched.

source("GSL_definition/code/00_config.R")
source("GSL_definition/code/fig_helpers.R")

## ---- Redirect every shared helper's output location to expanded/ ----------
## GSL_FIG_DIR is a plain global set by fig_helpers.R; save_gsl_figure() reads
## it from the global environment at call time (not at definition time), so
## reassigning it here is sufficient -- no need to touch fig_helpers.R itself.
EXP_ROOT    <- file.path(GSL_ROOT, "expanded")
EXP_CODE    <- file.path(EXP_ROOT, "code")
EXP_TABLES  <- file.path(EXP_ROOT, "tables")
EXP_DATA    <- file.path(EXP_ROOT, "data")   # gitignored, same convention as GSL_ROOT/data
EXP_DOCS    <- file.path(EXP_ROOT, "docs")
GSL_FIG_DIR <- file.path(EXP_ROOT, "figures")

for (d in c(EXP_TABLES, EXP_DATA, GSL_FIG_DIR, EXP_DOCS)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

## ---- Functional-group mapping, derived from the manuscript's own Sites
## table (GSL_SITES: a full IGBP x group crosstab is one-to-one, no IGBP code
## appears under two groups) -- not invented for the expanded population.
## Confirmed the expanded population's IGBP set (after rule 1) is exactly
## these 10 codes, nothing left unmapped.
EXP_IGBP_TO_GROUP <- c(
  GRA = "grassland",
  ENF = "evergreen forest", EBF = "evergreen forest",
  DBF = "deciduous forest", DNF = "deciduous forest",
  CSH = "woody-grass mosaic", OSH = "woody-grass mosaic",
  SAV = "woody-grass mosaic", WSA = "woody-grass mosaic",
  MF  = "mixed forest"
)
stopifnot(all(GSL_SITES$igbp %in% names(EXP_IGBP_TO_GROUP)))
stopifnot(identical(unname(EXP_IGBP_TO_GROUP[GSL_SITES$igbp]), GSL_SITES$group))

## ---- Selection rule for this expanded analysis (chosen from
## docs/selection_rule_options_2026-10-30.md, Dave's decision) --------------
EXP_RULE_QC_DAY        <- 0.25
EXP_RULE_MAX_FAIL_DAYS <- 14L   # R2, N = 14
EXP_RULE_MIN_YEARS     <- 5L    # rule 4, unchanged
EXP_RULE_NEGGPP_VAL    <- -1
EXP_RULE_NEGGPP_SHR    <- 0.10  # rule 5, unchanged

## ============================================================================
## Session 2a config switches (2026-10-30, Dave's decisions on the session 1
## checks report, docs/checks_report_2026-10-30.md). Each switch's default is
## the DECIDED, now-main behaviour for this rerun; flipping it back to the
## alternative reproduces a22e893-era behaviour for comparison, without
## editing the scripts below. See logs/prompts/2026-10-30_gsl_expanded_session2a.md.
## ============================================================================

## Item 1: per-site VUT/CUT fallback keyed to GPP availability as well as QC
## (VUT iff the site has both non-NA VUT NEE QC and non-NA VUT GPP_NT; else
## CUT iff it has both for CUT; else drop) -- not QC alone (a22e893's rule,
## which let a site through on a GPP column that was entirely NA, e.g. AU-Cow).
EXP_CFG_CUT_FALLBACK_USE_GPP <- TRUE

## Item 2: winter partitioning-artifact screen on GPP_NT_REF (main variant
## only): a day is screened (GPP_NT_REF set to 0) if GPP_NT_REF>0, NEE_REF>=0,
## TA_F<0 and GPP_DT_REF < EXP_CFG_WINTER_SCREEN_DT_PCT of the site's
## UNSCREENED P99 -- all four non-missing. The unscreened variant is always
## kept alongside (02_derive.R writes both; this switch controls only which
## one `derived_site*`/figures treat as "main").
EXP_CFG_WINTER_SCREEN        <- TRUE
EXP_CFG_WINTER_SCREEN_DT_PCT <- 0.10

## Item 4: Southern-Hemisphere annual GPP/NEP/mean-TA_F for the carbon-gain
## and temperature-sensitivity models summed/averaged over the Jul-Jun
## season year (matching season_boundaries' own SH convention), not the
## same-numbered calendar year (a22e893's join).
EXP_CFG_SH_SEASON_YEAR <- TRUE

## Item 6: local-sensitivity centred-difference half-width, in % of P99, for
## the expanded run's OWN fine-grid sensitivity curve (recomputed in
## 02_derive.R from fine_grid_main/dt's mean_gsl -- NOT GSL_definition/code/
## 03_derive.R's shared derive_one(), which stays fixed at 0.05% because it
## is also used, unmodified, by the 62-site reproduction).
EXP_CFG_LOCAL_SENS_HALFWIDTH_PCT <- 1.0

## Item 7: trend-size grid (days/yr) for the years-to-detect-a-trend-of-this-
## SIZE sweep (Figure 6c) -- replaces the dropped years-to-detect-the-
## OBSERVED-trend labels on Figure 6b.
EXP_CFG_DETECT_TREND_GRID <- seq(0.1, 1.0, by = 0.1)
EXP_CFG_DETECT_MARK_TYPICAL <- 0.25
EXP_CFG_DETECT_MARK_STRONG  <- 0.5

## Item 8: bounds check (S2) redesign -- only failing days (QC < EXP_RULE_QC_DAY
## or NA, in a passing year) whose gap-filled GPP lies within +/- this % of
## P99 of the threshold are treated as ambiguous (counted once as "above",
## once as "below"); failing days further from the threshold keep their own
## reported value either way, since QC alone does not put their
## above/below classification in doubt.
EXP_CFG_BOUNDS_NEAR_PCT <- 0.02

## Item 9: functional groups excluded from pooled TREND models/panels (too
## few trend sites, i.e. too few sites with >=10 passing years, for a
## meaningful pooled trend) -- mixed forest has 7 sites total but only 2
## with >=10 years. This does not affect carbon-gain or temperature-
## sensitivity pooled models (which use the >=3-site floor per subset, not a
## trend-specific one) -- see fit_pooled_slope()'s own n_sites<3 guard for those.
EXP_CFG_TREND_DROP_GROUPS <- c("mixed forest")

message("GSL expanded-analysis config loaded: EXP_ROOT=", EXP_ROOT,
        ", rule = per-site VUT/CUT fallback (QC+GPP) + <=", EXP_RULE_MAX_FAIL_DAYS,
        " failing days/yr (R2, N=14). Session 2a switches: winter_screen=",
        EXP_CFG_WINTER_SCREEN, ", sh_season_year=", EXP_CFG_SH_SEASON_YEAR,
        ", local_sens_halfwidth_pct=", EXP_CFG_LOCAL_SENS_HALFWIDTH_PCT,
        ", bounds_near_pct=", EXP_CFG_BOUNDS_NEAR_PCT,
        ", trend_drop_groups=", paste(EXP_CFG_TREND_DROP_GROUPS, collapse = ","))
