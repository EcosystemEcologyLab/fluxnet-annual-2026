## 05_units_duckdb.R — Unit conversion for the DuckDB pipeline
##
## Reads *_qc tables from data/duckdb/fluxnet.duckdb, applies the same
## conversion logic as the RDS-based 05_units.R, and writes *_converted
## tables back into the database.  All conversion operations execute in
## DuckDB — no full-table R materialisation.
##
## Conversion rules (identical to fluxnet_convert_units() in R/units.R):
##   Carbon (NEE/GPP/RECO):
##     HH/HR only: µmol CO₂ m⁻² s⁻¹ → gC m⁻² per period (* 12e-6 * spp)
##     DD/MM/WW/YY: already gC m⁻² period⁻¹ from ONEFlux — PASS THROUGH
##     (The pass-through guard prevents the ~800× over-correction from
##     commit 31e653b when the µmol→gC factor was wrongly applied to
##     pre-integrated coarse-resolution data.)
##   LE:            W m⁻² → mm H₂O per period  (* spp / 2.45e6)
##   H, SW_IN:      W m⁻² → MJ m⁻² per period  (* spp * 1e-6)
##   TA:            °C → K                      (+ 273.15)
##   VPD:           hPa → kPa                   (/ 10)
##   All others:    pass through unchanged
##
## Hard Rule #5: check_pipeline_config() is called near the top.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

source("R/utils.R")

library(dplyr)
library(duckdb)
library(glue)

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
con     <- dbConnect(duckdb(), db_path)

# ── Resolution map ────────────────────────────────────────────────────────────
# secs = NULL → computed per-row from TIMESTAMP (YY leap-year aware)
res_map <- list(
  annual  = list(suffix = "YY", secs = NULL),
  monthly = list(suffix = "MM", secs = round(30.4375 * 86400)),  # ~30.44-day month
  weekly  = list(suffix = "WW", secs = 604800L),
  daily   = list(suffix = "DD", secs = 86400L),
  hourly  = list(suffix = "HH", secs = 1800L)   # HH/HR both treated as 30 min
)

cur_tables <- dbListTables(con)

cat("=========================================\n")
cat("  05_units_duckdb.R\n")
cat("=========================================\n\n")

for (tbl_name in names(res_map)) {
  src  <- paste0(tbl_name, "_qc")
  dst  <- paste0(tbl_name, "_converted")
  info <- res_map[[tbl_name]]

  if (!src %in% cur_tables) {
    message("  ", src, " not found in database — skipping ", tbl_name)
    next
  }

  suffix    <- info$suffix
  is_coarse <- suffix %in% c("YY", "MM", "WW", "DD")

  cat("\n--- 05_units_duckdb.R:", suffix, "---\n")

  data <- tbl(con, src)
  cols <- colnames(data)

  # ── Identify columns in each conversion class ────────────────────────────
  # Exclude _QC flag columns from every class (they are adimensional).
  no_qc <- function(x) x[!grepl("_QC$", x)]

  carbon_cols <- no_qc(grep("^(NEE|GPP|RECO)_", cols, value = TRUE))
  le_cols     <- no_qc(grep("^LE_",              cols, value = TRUE))
  h_sw_cols   <- no_qc(grep("^(H|SW_IN)_",       cols, value = TRUE))
  ta_cols     <- no_qc(grep("^TA_",              cols, value = TRUE))
  vpd_cols    <- no_qc(grep("^VPD_",             cols, value = TRUE))

  # Carbon is only converted at HH/HR.  At coarse resolutions the values are
  # pre-integrated gC m⁻² period⁻¹ and must not receive the µmol→gC factor.
  converted_carbon <- if (!is_coarse) carbon_cols else character(0L)

  # Full list of columns that will be numerically changed
  converted_cols <- c(converted_carbon, le_cols, h_sw_cols, ta_cols, vpd_cols)

  cat("  Columns in table:", length(cols), "\n")
  cat("  Converting:", length(converted_cols),
      "  Passing through:", length(cols) - length(converted_cols), "\n")

  result <- data

  # ── Step 1: Save _native companion copies before overwriting originals ────
  # Consistent with fluxnet_convert_units() behaviour in the RDS pipeline.
  if (length(converted_cols) > 0L) {
    result <- result |>
      dplyr::mutate(
        across(all_of(converted_cols), list(native = ~.x), .names = "{.col}_native")
      )
  }

  # ── Step 2: Add seconds_per_period (spp) as a helper column ──────────────
  # YY: TIMESTAMP is an integer year; leap-year check follows Gregorian rules.
  # All others: constant for the resolution.
  if (is.null(info$secs)) {
    result <- result |>
      dplyr::mutate(
        spp = dplyr::if_else(
          TIMESTAMP %% 4L == 0L &
            (TIMESTAMP %% 100L != 0L | TIMESTAMP %% 400L == 0L),
          366L * 86400L,
          365L * 86400L
        )
      )
    cat("  spp: per-row leap-year calculation (YY)\n")
  } else {
    spp_val <- info$secs
    result  <- result |> dplyr::mutate(spp = spp_val)
    cat("  spp:", spp_val, "s per period (", suffix, ")\n")
  }

  # ── Step 3: Apply conversions in DuckDB (no R materialisation) ───────────
  # Each across() block generates SQL operating on the current lazy query.
  # The `spp` column is available in the query context at this point.

  # Carbon: HH/HR only (is_coarse guard prevents 31e653b-style bug)
  if (length(converted_carbon) > 0L) {
    result <- result |>
      dplyr::mutate(across(all_of(converted_carbon), ~ .x * 12e-6 * spp))
  }

  # LE: W m⁻² → mm H₂O per period  (λ ≈ 2.45×10⁶ J kg⁻¹, ρ_w = 1000 kg m⁻³)
  if (length(le_cols) > 0L) {
    result <- result |>
      dplyr::mutate(across(all_of(le_cols), ~ .x * spp / 2.45e6))
  }

  # H and SW_IN: W m⁻² → MJ m⁻² per period
  if (length(h_sw_cols) > 0L) {
    result <- result |>
      dplyr::mutate(across(all_of(h_sw_cols), ~ .x * spp * 1e-6))
  }

  # TA: °C → K
  if (length(ta_cols) > 0L) {
    result <- result |>
      dplyr::mutate(across(all_of(ta_cols), ~ .x + 273.15))
  }

  # VPD: hPa → kPa
  if (length(vpd_cols) > 0L) {
    result <- result |>
      dplyr::mutate(across(all_of(vpd_cols), ~ .x / 10))
  }

  # ── Step 4: Drop the spp helper column, compute to DuckDB ────────────────
  result <- result |> dplyr::select(-spp)

  if (dst %in% dbListTables(con)) {
    dbExecute(con, glue("DROP TABLE {dst}"))
  }
  result |> dplyr::compute(name = dst, temporary = FALSE)

  # ── Step 5: Row-count sanity check ───────────────────────────────────────
  n_src <- tbl(con, src) |> dplyr::count() |> dplyr::pull(n)
  n_dst <- tbl(con, dst) |> dplyr::count() |> dplyr::pull(n)
  if (n_src != n_dst) {
    warning(sprintf(
      "Row count mismatch: %s=%d rows vs %s=%d rows",
      src, n_src, dst, n_dst
    ))
  } else {
    cat(sprintf("  %s → %s: %d rows OK\n", src, dst, n_dst))
  }

  # ── Step 6: NEE range assertion (YY only) ─────────────────────────────────
  # IT-Lav and US-Bi2 have anomalous NEE values (|NEE| > 2000).
  # See known_issues.md §2. This is a warning, not an error.
  if (suffix == "YY") {
    nee_ref <- "NEE_VUT_REF"
    if (nee_ref %in% colnames(tbl(con, dst))) {
      nee_check <- tbl(con, dst) |>
        dplyr::filter(!is.na(.data[[nee_ref]])) |>
        dplyr::summarise(
          min_nee   = min(.data[[nee_ref]], na.rm = TRUE),
          max_nee   = max(.data[[nee_ref]], na.rm = TRUE),
          n_extreme = sum(as.integer(abs(.data[[nee_ref]]) > 2000L),
                         na.rm = TRUE)
        ) |>
        collect()
      cat(sprintf(
        "  YY NEE_VUT_REF range: min=%.1f  max=%.1f  gC m-2 y-1\n",
        nee_check$min_nee, nee_check$max_nee
      ))
      if (nee_check$n_extreme > 0L) {
        warning(
          nee_check$n_extreme,
          " NEE_VUT_REF value(s) exceed |2000| gC m-2 y-1 — ",
          "see known_issues.md §2 (IT-Lav, US-Bi2). Not an error."
        )
      }
    }
  }
}

dbDisconnect(con)

cat("\n=========================================\n")
cat(" 05_units_duckdb.R complete.\n")
cat(" Tables written: annual_converted, monthly_converted,\n")
cat("                 weekly_converted, daily_converted, hourly_converted\n")
cat("=========================================\n")
