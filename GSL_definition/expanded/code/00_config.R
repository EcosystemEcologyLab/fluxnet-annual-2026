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

message("GSL expanded-analysis config loaded: EXP_ROOT=", EXP_ROOT,
        ", rule = per-site VUT/CUT fallback + <=", EXP_RULE_MAX_FAIL_DAYS, " failing days/yr (R2, N=14).")
