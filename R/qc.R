## QC design decision — row exclusion gated on NEE_VUT_REF_QC only
##
## For coarser-resolution data (DD/MM/WW/YY), stage-1 QC (flux_qc()) is
## called with qc_vars = "NEE_VUT_REF" rather than all _QC columns.
## This means a year-row is excluded only when NEE_VUT_REF itself is too
## heavily gap-filled — the primary variable for the FLUXNET Annual Paper
## 2026. Secondary variable QC columns (GPP, RECO, LE, H) are retained in
## the output for per-variable filtering by downstream figure functions but
## do not drive row exclusion.
##
## Rationale: gating on all _QC columns caused 347 of 672 sites (52%) to
## lose every annual record because a single secondary variable (e.g.,
## RECO_NT_VUT_REF with high winter gap-fill) contaminated all years, even
## when NEE was well-observed. See docs/decisions_pending.md.
##
## For HH/HR data, stage-2 QC (fluxnet_qc_hh()) still uses all integer
## _QC columns because at sub-daily resolution all flux variables are
## independent measurements, not derived/partitioned quantities.

#' Test whether a fractional QC value passes a threshold
#'
#' At DD/WW/MM/YY resolution, `_QC` flags are the fraction of underlying
#' HH/HR records that were measured or good-quality gap-fill (Pastorello et
#' al. 2020, SM1). The rule is "keep when `QC >= threshold`" -- raising the
#' threshold must always be stricter. This is the single implementation of
#' that rule; callers must not reimplement it as `(1 - QC) <= threshold`,
#' which is only equivalent at `threshold = 0.50`.
#'
#' @param qc Numeric vector of fractional QC values (0-1), or `NA`.
#' @param threshold Numeric threshold to compare against (e.g.
#'   `QC_THRESHOLD_YY`).
#' @return A logical vector: `TRUE` when `qc >= threshold`, `FALSE` when
#'   `qc < threshold`, `NA` when `qc` is `NA`.
#' @export
qc_passes <- function(qc, threshold) {
  ifelse(is.na(qc), NA, qc >= threshold)
}

#' Apply QC filtering to half-hourly (HH) or hourly (HR) FLUXNET data
#'
#' Filters rows based on the `*_QC` flag system used at HH/HR resolution:
#' - `0` = measured
#' - `1` = good quality gap-fill (MDS)
#' - `2` = medium quality gap-fill
#' - `3` = poor quality gap-fill
#'
#' By default, only records with `_QC <= 1` are retained (measured or good
#' gap-fill). USTAR filtering is already applied in the distributed data and
#' must NOT be re-applied here.
#'
#' @param data A data frame of HH or HR FLUXNET data containing at least one
#'   `*_QC` column.
#' @param max_qc Integer. Maximum `_QC` flag value to retain. Default `1L`.
#' @param qc_cols Character vector of `_QC` column names to filter on. If
#'   `NULL` (default), all columns whose names end in `"_QC"` are used.
#' @return A filtered data frame. Rows where *any* of the selected `_QC`
#'   columns exceed `max_qc` are dropped.
#' @export
fluxnet_qc_hh <- function(data, max_qc = 1L, qc_cols = NULL) {
  if (is.null(qc_cols)) {
    qc_cols <- grep("_QC$", names(data), value = TRUE)
  }

  if (length(qc_cols) == 0) {
    warning("No '_QC' columns found in data. Returning data unchanged.")
    return(data)
  }

  missing_cols <- setdiff(qc_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following QC columns are not in data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Keep rows where all selected QC flags are <= max_qc (NAs are kept)
  keep <- Reduce(`&`, lapply(data[qc_cols], function(x) is.na(x) | x <= max_qc))
  data[keep, , drop = FALSE]
}
