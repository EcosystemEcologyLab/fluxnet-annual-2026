## rain_rule.R — shared Priestley-Taylor PET and Zhou et al. (2015) rain-day
## exclusion logic. Sourced by BOTH 07_apply_screens.R (stage 2's own rain
## screen, driven by P_ERA) and 11_precip_compare.R (the precip-vs-gauge
## side analysis's "consequence for the rain rule" check, driven by either
## P_ERA or the tower gauge) -- one implementation, not two, so a change here
## can never make the two diverge silently. Not sourced by 00_config.R;
## sourced explicitly by whichever script needs it.

#' Priestley-Taylor PET (alpha = 1.26), G = 0, fixed sea-level pressure.
#' See docs/methods_memo.md "Stage 2 -- Priestley-Taylor PET" for the reading
#' behind the fixed-pressure assumption (the user's instructions name only
#' daily mean net radiation and air temperature as inputs).
pt_pet_mm_day <- function(rn_wm2_mean, ta_degc, alpha = 1.26) {
  rn_mj_day <- rn_wm2_mean * 86400 * 1e-6          # G = 0, so (Rn - G) = Rn
  lambda    <- 2.501 - 0.002361 * ta_degc          # MJ/kg (FAO-56)
  es        <- 0.6108 * exp(17.27 * ta_degc / (ta_degc + 237.3))  # kPa
  delta     <- 4098 * es / (ta_degc + 237.3)^2     # kPa/degC
  gamma     <- 1.013e-3 * 101.3 / (0.622 * lambda) # kPa/degC, fixed sea-level P
  alpha * (delta / (delta + gamma)) * rn_mj_day / lambda
}

#' Zhou et al. (2015) rain-day exclusion rule (MY DECISION 4: no threshold --
#' every day with P > 0 is excluded). Also excludes the two following days
#' when P > 2*PET, or the one following day when P > PET.
#'
#' @param daily A data frame ordered by a CONTINUOUS daily date sequence (no
#'   gaps -- e.g. built via `seq(min(date), max(date), by = "day")` and
#'   left-joined), with columns `P_day` and `PET_day`.
#' @return A logical vector, same length/order as `daily`: TRUE where the day
#'   is excluded by the rain rule.
rain_rule_excluded <- function(daily) {
  rainy <- !is.na(daily$P_day) & daily$P_day > 0
  sev2  <- !is.na(daily$P_day) & !is.na(daily$PET_day) & daily$P_day > 2 * daily$PET_day
  sev1  <- !is.na(daily$P_day) & !is.na(daily$PET_day) & daily$P_day > daily$PET_day & !sev2

  n <- nrow(daily)
  excluded_following <- rep(FALSE, n)
  for (i in which(sev2)) {
    for (off in 1:2) if (i + off <= n) excluded_following[i + off] <- TRUE
  }
  for (i in which(sev1)) {
    if (i + 1 <= n) excluded_following[i + 1] <- TRUE
  }

  rainy | excluded_following
}
