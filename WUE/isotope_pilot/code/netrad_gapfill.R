## netrad_gapfill.R — shared per-site NETRAD ~ SW_IN_F gap-fill, used by BOTH
## 06_build_site_years.R (stage 2's own NETRAD_filled series) and
## 11_precip_compare.R (the "consequence for the rain rule" check needs the
## same filled Rn series feeding the same PET formula) -- one implementation,
## not two.

#' Per-site OLS gap-fill of NETRAD from SW_IN_F (both present, any QC -- a
#' magnitude fit, not a QC-gated analysis).
#'
#' @param netrad,sw_in_f Numeric vectors, same length/order.
#' @return A list: `filled` (NETRAD, observed where present, fitted
#'   prediction where not -- same length as input), `estimated` (logical,
#'   TRUE where `filled` rests on the fit), `n`/`slope`/`intercept`/`r_squared`
#'   (the fit diagnostics, `NA` if too thin to fit, n < 30).
fit_netrad_gapfill <- function(netrad, sw_in_f) {
  fit_data <- data.frame(NETRAD = netrad, SW_IN_F = sw_in_f)
  fit_data <- fit_data[!is.na(fit_data$NETRAD) & !is.na(fit_data$SW_IN_F), ]
  n_fit <- nrow(fit_data)

  if (n_fit >= 30L) {
    fit <- stats::lm(NETRAD ~ SW_IN_F, data = fit_data)
    s <- summary(fit)
    slope <- unname(stats::coef(fit)[2]); intercept <- unname(stats::coef(fit)[1])
    r2 <- s$r.squared
    predicted <- intercept + slope * sw_in_f
  } else {
    slope <- NA_real_; intercept <- NA_real_; r2 <- NA_real_
    predicted <- rep(NA_real_, length(netrad))
  }

  list(
    filled    = ifelse(is.na(netrad), predicted, netrad),
    estimated = is.na(netrad),
    n = n_fit, slope = slope, intercept = intercept, r_squared = r2
  )
}
