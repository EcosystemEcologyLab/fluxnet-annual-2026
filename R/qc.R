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
