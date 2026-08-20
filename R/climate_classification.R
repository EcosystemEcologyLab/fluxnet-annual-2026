# R/climate_classification.R
#
# Local computation of Köppen-Geiger (KG) climate classification from the
# ERA5 monthly reanalysis data bundled with every FLUXNET Shuttle site
# download, following the Beck et al. (2018/2023) classification rules
# applied to a 30-year monthly temperature/precipitation normal — the same
# approach used by ICOS's KG_classification notebook.
#
# Replaces two previously-inconsistent per-figure-family KG sources (BADM
# CLIMATE_KOEPPEN metadata; Beck 2023 raster extraction) for the current
# 767-site FLUXNET Shuttle network with a single authoritative source. See
# review/figures/representativeness/methods_koppen_era5.md for the full
# methods write-up and SESSION_LOG.md for the implementation record.
#
# Reference: Beck, H.E., McVicar, T.R., Vergopolan, N. et al. High-resolution
# (1 km) Köppen-Geiger maps for 1901-2099 based on constrained CMIP6
# projections. Sci Data 10, 724 (2023). doi:10.1038/s41597-023-02549-6

#' Classify a single site into a Köppen-Geiger climate class
#'
#' Applies the Beck et al. classification rule cascade to one site's 12
#' calendar-month climate normal (mean temperature and total precipitation).
#' This is a direct, faithful port of the boolean-cascade algorithm used by
#' ICOS's `KG_classificator_data()` (itself a port of Beck's original MATLAB
#' code) — every threshold and branch matches that source one-for-one.
#'
#' The "summer" half-year is determined dynamically as whichever of
#' April-September or October-March is warmer at this site (`T_AMJJAS` vs
#' `T_ONDJFM` below), not by a hemisphere flag. This is what makes the
#' algorithm correct for both hemispheres without any special-casing — do
#' not add a separate Southern Hemisphere branch; it is unnecessary and
#' would double-count what this comparison already does.
#'
#' @param ta_monthly Numeric vector, length 12. Mean air temperature (°C)
#'   for each calendar month, in order January through December.
#' @param p_monthly Numeric vector, length 12. Total precipitation (mm) for
#'   each calendar month, in order January through December.
#'
#' @return A list with `kg_class` (character, 2-3 letter full code, e.g.
#'   `"Cfb"` or `"ET"`), `kg_second` (character, first two letters), and
#'   `kg_main` (character, first letter: A/B/C/D/E). All three are `NA` if
#'   either input contains `NA` values, is not length 12, or the rule
#'   cascade does not resolve to exactly one class (which should not happen
#'   for valid inputs — treated as a data problem, not classified).
#'
#' @examples
#' \dontrun{
#' # Tropical rainforest-like: warm all year, wet every month
#' classify_koppen_geiger(
#'   ta_monthly = rep(26, 12),
#'   p_monthly  = rep(150, 12)
#' )
#' }
classify_koppen_geiger <- function(ta_monthly, p_monthly) {
  bad <- list(kg_class = NA_character_, kg_second = NA_character_,
              kg_main = NA_character_)

  if (length(ta_monthly) != 12L || length(p_monthly) != 12L) return(bad)
  if (anyNA(ta_monthly) || anyNA(p_monthly)) return(bad)

  Ta <- as.numeric(ta_monthly)
  P  <- as.numeric(p_monthly)

  amjjas <- 4:9   # Apr-Sep
  ondjfm <- c(10:12, 1:3) # Oct-Mar

  T_AMJJAS <- mean(Ta[amjjas])
  T_ONDJFM <- mean(Ta[ondjfm])
  summer_is_amjjas <- T_AMJJAS > T_ONDJFM

  # SUM_SEL: TRUE for the six months belonging to the warmer ("summer") half
  sum_sel <- logical(12)
  sum_sel[amjjas] <- summer_is_amjjas
  sum_sel[ondjfm] <- !summer_is_amjjas

  Pw <- sum(P[!sum_sel]) # winter-half total precip
  Ps <- sum(P[sum_sel])  # summer-half total precip
  Pdry <- min(P)

  Psdry <- min(P[sum_sel])
  Pswet <- max(P[sum_sel])
  Pwdry <- min(P[!sum_sel])
  Pwwet <- max(P[!sum_sel])

  MAT <- mean(Ta)
  MAP <- sum(P)
  Tmon10 <- sum(Ta > 10)
  Thot <- max(Ta)
  Tcold <- min(Ta)

  Pthresh <- 2 * MAT + 14
  if (Pw * 2.333 > Ps) Pthresh <- 2 * MAT
  if (Ps * 2.333 > Pw) Pthresh <- 2 * MAT + 28

  # ---- B: Arid ----
  B  <- MAP < 10 * Pthresh
  BW <- B && MAP < 5 * Pthresh
  BWh <- BW && MAT >= 18
  BWk <- BW && MAT < 18
  BS <- B && MAP >= 5 * Pthresh
  BSh <- BS && MAT >= 18
  BSk <- BS && MAT < 18

  # ---- A: Tropical ----
  A  <- Tcold >= 18 && !B
  Af <- A && Pdry >= 60
  Am <- A && !Af && Pdry >= 100 - MAP / 25
  Aw <- A && !Af && Pdry < 100 - MAP / 25

  # ---- C: Temperate ----
  C  <- Thot > 10 && Tcold > 0 && Tcold < 18 && !B
  Cs <- C && Psdry < 40 && Psdry < Pwwet / 3
  Cw <- C && Pwdry < Pswet / 10
  if (Cs && Cw) {
    if (Ps > Pw) Cs <- FALSE else Cw <- FALSE
  }
  Csa <- Cs && Thot >= 22
  Csb <- Cs && !Csa && Tmon10 >= 4
  Csc <- Cs && !Csa && !Csb && Tmon10 >= 1 && Tmon10 < 4
  Cwa <- Cw && Thot >= 22
  Cwb <- Cw && !Cwa && Tmon10 >= 4
  Cwc <- Cw && !Cwa && !Cwb && Tmon10 >= 1 && Tmon10 < 4
  Cf  <- C && !Cs && !Cw
  Cfa <- Cf && Thot >= 22
  Cfb <- Cf && !Cfa && Tmon10 >= 4
  Cfc <- Cf && !Cfa && !Cfb && Tmon10 >= 1 && Tmon10 < 4

  # ---- D: Cold ----
  D  <- Thot > 10 && Tcold <= 0 && !B
  Ds <- D && Psdry < 40 && Psdry < Pwwet / 3
  Dw <- D && Pwdry < Pswet / 10
  if (Ds && Dw) {
    if (Ps > Pw) Ds <- FALSE else Dw <- FALSE
  }
  Dsa <- Ds && Thot >= 22
  Dsb <- Ds && !Dsa && Tmon10 >= 4
  Dsd <- Ds && !Dsa && !Dsb && Tcold < -38
  Dsc <- Ds && !Dsa && !Dsb && !Dsd
  Dwa <- Dw && Thot >= 22
  Dwb <- Dw && !Dwa && Tmon10 >= 4
  Dwd <- Dw && !Dwa && !Dwb && Tcold < -38
  Dwc <- Dw && !Dwa && !Dwb && !Dwd
  Df  <- D && !Ds && !Dw
  Dfa <- Df && Thot >= 22
  Dfb <- Df && !Dfa && Tmon10 >= 4
  Dfd <- Df && !Dfa && !Dfb && Tcold < -38
  Dfc <- Df && !Dfa && !Dfb && !Dfd

  # ---- E: Polar ----
  E  <- Thot <= 10 && !B
  ET <- E && Thot > 0
  EF <- E && Thot <= 0

  flags <- c(
    Af = Af, Am = Am, Aw = Aw,
    BWh = BWh, BWk = BWk, BSh = BSh, BSk = BSk,
    Csa = Csa, Csb = Csb, Csc = Csc,
    Cwa = Cwa, Cwb = Cwb, Cwc = Cwc,
    Cfa = Cfa, Cfb = Cfb, Cfc = Cfc,
    Dsa = Dsa, Dsb = Dsb, Dsc = Dsc, Dsd = Dsd,
    Dwa = Dwa, Dwb = Dwb, Dwc = Dwc, Dwd = Dwd,
    Dfa = Dfa, Dfb = Dfb, Dfc = Dfc, Dfd = Dfd,
    ET = ET, EF = EF
  )

  hits <- names(flags)[flags]
  if (length(hits) != 1L) return(bad) # ambiguous/no-match — treat as unclassifiable

  kg_class <- hits[[1L]]
  list(
    kg_class  = kg_class,
    kg_second = substr(kg_class, 1L, 2L),
    kg_main   = substr(kg_class, 1L, 1L)
  )
}

#' Compute a 30-year monthly ERA5 climate normal per site
#'
#' Takes raw ERA5 monthly rows from the pipeline's DuckDB `monthly` table
#' (`dataset = 'ERA5'`) and computes, per site, a mean-monthly-temperature /
#' total-monthly-precipitation climatology averaged over `period`, screening
#' out implausible precipitation years first.
#'
#' `P_ERA` in the raw monthly file is a **daily-mean** value (mm/day), not a
#' monthly total — confirmed against on-disk `*_FLUXNET_ERA5_MM_*.csv` files
#' (magnitudes of ~0.2-2.5 mm/day, not plausible as monthly totals). This
#' function multiplies by the number of days in each month before summing,
#' matching the ICOS reference implementation. Skipping this step would
#' silently corrupt every aridity/seasonality threshold downstream in
#' [classify_koppen_geiger()].
#'
#' @param monthly_era5 Data frame. Raw DuckDB `monthly` rows already
#'   filtered to `dataset == "ERA5"`, with columns `site_id`, `TIMESTAMP`
#'   (Date, first of month), `TA_ERA` (°C), `P_ERA` (mm/day, daily mean for
#'   that month).
#' @param period Integer vector of length 2, `c(start_year, end_year)`
#'   inclusive. Default [KG_ERA5_PERIOD].
#' @param min_years Integer. Minimum number of valid (post-screening) years
#'   required within `period` for a site to be classified. Sites below this
#'   get `NA` climatology and are logged via [log_unknown()]. Default
#'   [KG_ERA5_MIN_YEARS].
#' @param map_max_mm Numeric. Site-years with total annual precipitation
#'   above this (mm) are dropped as ERA5 spatial-averaging artifacts before
#'   averaging, and logged via [log_exclusion()]. Default
#'   [KG_ERA5_MAP_MAX_MM]. See `docs/known_issues.md` §9a.
#' @param excluded_by Character. Script name recorded in the exclusion/
#'   unknown logs. Default `"step5_compute_koppen_era5.R"`.
#'
#' @return Data frame, one row per `site_id`, with columns `t_01`..`t_12`
#'   (mean monthly temperature, °C, Jan-Dec), `p_01`..`p_12` (mean monthly
#'   total precipitation, mm, Jan-Dec), `n_years_used` (integer), `mat_degc`
#'   (mean annual temperature), `map_mm` (mean annual precipitation total).
#'   Climate columns are `NA` for sites with fewer than `min_years` valid
#'   years.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(), "data/duckdb/fluxnet.duckdb")
#' era5_monthly <- DBI::dbGetQuery(con,
#'   "SELECT site_id, TIMESTAMP, TA_ERA, P_ERA FROM monthly WHERE dataset = 'ERA5'")
#' DBI::dbDisconnect(con)
#' clim <- compute_era5_monthly_climatology(era5_monthly)
#' }
compute_era5_monthly_climatology <- function(
    monthly_era5,
    period      = KG_ERA5_PERIOD,
    min_years   = KG_ERA5_MIN_YEARS,
    map_max_mm  = KG_ERA5_MAP_MAX_MM,
    excluded_by = "step5_compute_koppen_era5.R") {

  for (pkg in c("dplyr", "tidyr", "lubridate")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('",
           pkg, "')", call. = FALSE)
    }
  }

  req_cols <- c("site_id", "TIMESTAMP", "TA_ERA", "P_ERA")
  missing_cols <- setdiff(req_cols, names(monthly_era5))
  if (length(missing_cols) > 0L) {
    stop("compute_era5_monthly_climatology: monthly_era5 is missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  df <- monthly_era5 |>
    dplyr::mutate(
      TIMESTAMP = as.Date(.data$TIMESTAMP),
      year      = lubridate::year(.data$TIMESTAMP),
      month     = lubridate::month(.data$TIMESTAMP),
      p_tot     = .data$P_ERA * lubridate::days_in_month(.data$TIMESTAMP)
    ) |>
    dplyr::filter(
      .data$year >= period[1], .data$year <= period[2],
      !is.na(.data$TA_ERA), !is.na(.data$p_tot)
    )

  # ---- Screen implausible precipitation years (annual total) ----
  site_year_map <- df |>
    dplyr::group_by(.data$site_id, .data$year) |>
    dplyr::summarise(
      n_months = dplyr::n(),
      map_mm   = sum(.data$p_tot),
      .groups  = "drop"
    ) |>
    dplyr::filter(.data$n_months == 12L) # only complete calendar years

  bad_years <- dplyr::filter(site_year_map, .data$map_mm > map_max_mm)
  if (nrow(bad_years) > 0L && exists("log_exclusion", mode = "function")) {
    for (i in seq_len(nrow(bad_years))) {
      log_exclusion(
        site_id     = bad_years$site_id[i],
        variable    = "P_ERA",
        timestamp   = paste0(bad_years$year[i], "-ALL"),
        reason      = paste0(
          "Computed annual P_ERA total (", round(bad_years$map_mm[i], 0),
          " mm) exceeds plausible maximum — ERA5 spatial-averaging artifact ",
          "(docs/known_issues.md §9a)"
        ),
        threshold   = paste0("KG_ERA5_MAP_MAX_MM=", map_max_mm),
        excluded_by = excluded_by
      )
    }
  }

  good_site_years <- site_year_map |>
    dplyr::filter(.data$map_mm <= map_max_mm) |>
    dplyr::select("site_id", "year")

  df_screened <- df |>
    dplyr::semi_join(good_site_years, by = c("site_id", "year"))

  # ---- Average by site x calendar month across valid years ----
  clim_long <- df_screened |>
    dplyr::group_by(.data$site_id, .data$month) |>
    dplyr::summarise(
      t_mean = mean(.data$TA_ERA, na.rm = TRUE),
      p_mean = mean(.data$p_tot,  na.rm = TRUE),
      .groups = "drop"
    )

  n_years <- good_site_years |>
    dplyr::distinct(.data$site_id, .data$year) |>
    dplyr::count(.data$site_id, name = "n_years_used")

  # sites with fewer than min_years: log as unknown, exclude from climatology
  insufficient <- dplyr::filter(
    dplyr::full_join(
      dplyr::distinct(df, .data$site_id),
      n_years, by = "site_id"
    ) |> dplyr::mutate(n_years_used = dplyr::coalesce(.data$n_years_used, 0L)),
    .data$n_years_used < min_years
  )
  if (nrow(insufficient) > 0L && exists("log_unknown", mode = "function")) {
    for (i in seq_len(nrow(insufficient))) {
      log_unknown(
        record_id = insufficient$site_id[i],
        reason    = paste0(
          "Insufficient valid ERA5 years in ", period[1], "-", period[2],
          " window (", insufficient$n_years_used[i], "/", min_years,
          " required) to compute a KG climate normal"
        ),
        logged_by = excluded_by
      )
    }
  }

  valid_sites <- n_years |> dplyr::filter(.data$n_years_used >= min_years)

  wide <- clim_long |>
    dplyr::semi_join(valid_sites, by = "site_id") |>
    tidyr::pivot_wider(
      names_from  = "month",
      values_from = c("t_mean", "p_mean"),
      names_glue  = "{.value}_{sprintf('%02d', month)}"
    )
  # rename t_mean_01 -> t_01, p_mean_01 -> p_01
  names(wide) <- sub("^t_mean_", "t_", names(wide))
  names(wide) <- sub("^p_mean_", "p_", names(wide))

  diagnostics <- clim_long |>
    dplyr::semi_join(valid_sites, by = "site_id") |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(
      mat_degc = mean(.data$t_mean),
      map_mm   = sum(.data$p_mean),
      .groups  = "drop"
    )

  out <- wide |>
    dplyr::left_join(n_years, by = "site_id") |>
    dplyr::left_join(diagnostics, by = "site_id")

  # ensure every site in the input appears, even if unclassifiable
  all_sites <- dplyr::distinct(df, .data$site_id)
  out <- all_sites |>
    dplyr::left_join(out, by = "site_id") |>
    dplyr::mutate(n_years_used = dplyr::coalesce(.data$n_years_used, 0L))

  out
}

#' Compute site-level Köppen-Geiger classification from ERA5 climatology
#'
#' Orchestrates [compute_era5_monthly_climatology()] and
#' [classify_koppen_geiger()] to produce one KG classification row per site,
#' with comparison columns against the BADM `CLIMATE_KOEPPEN` metadata field
#' and the Beck et al. (2023) raster extraction — mirroring the map-vs-data
#' comparison the ICOS reference script itself performs. The ERA5 result is
#' authoritative; the comparison columns are for QA and methods reporting.
#'
#' The output carries both existing column-naming schemes used elsewhere in
#' this repo so downstream figure scripts require no restructuring:
#' `kg_class`/`kg_second`/`kg_main` (as used by the BADM-derived
#' `Anomalies_KG` figures) are aliases of `koppen_class`/`koppen_twoletter`/
#' `koppen_main` (as used by the Beck-2023-derived `representativeness`
#' figures) — same values, both names present.
#'
#' @param monthly_era5 Data frame. Raw DuckDB `monthly` rows filtered to
#'   `dataset == "ERA5"`. Passed through to
#'   [compute_era5_monthly_climatology()].
#' @param badm Data frame or `NULL`. BADM metadata (e.g. loaded from
#'   `data/processed/badm.rds`) with columns `SITE_ID`, `VARIABLE`,
#'   `DATAVALUE`. If supplied, the `CLIMATE_KOEPPEN` value per site is
#'   joined in as `badm_kg_class`. If `NULL`, `badm_kg_class` is `NA` for
#'   every site.
#' @param beck2023 Data frame or `NULL`. Beck 2023 raster extraction (e.g.
#'   loaded from `data/snapshots/site_koppen_beck2023.csv`) with columns
#'   `site_id`, `koppen_class`. If supplied, joined in as
#'   `beck2023_kg_class`. If `NULL`, `beck2023_kg_class` is `NA` for every
#'   site.
#' @param legend Data frame or `NULL`. Beck legend lookup (as parsed from
#'   `data/external/koppen_beck2023/legend.txt`, e.g. by
#'   `step4_extract_koppen_beck2023.R`'s legend-parsing block) with columns
#'   `koppen_class_code`, `koppen_class`, `koppen_class_name`, `koppen_main`,
#'   `koppen_main_name`. If supplied, used to attach human-readable class
#'   names. If `NULL`, those columns are `NA`.
#' @param ... Passed through to [compute_era5_monthly_climatology()] (e.g.
#'   `period`, `min_years`, `map_max_mm`).
#'
#' @return Data frame, one row per site, with columns: `site_id`,
#'   `n_years_used`, `mat_degc`, `map_mm`, `kg_class`, `kg_second`,
#'   `kg_main`, `koppen_class`, `koppen_twoletter`, `koppen_main`,
#'   `koppen_class_code`, `koppen_class_name`, `koppen_main_name`,
#'   `koppen_method` (always `"era5_local"`), `badm_kg_class`,
#'   `agree_badm`, `beck2023_kg_class`, `agree_beck2023`.
#'
#' @examples
#' \dontrun{
#' badm     <- readRDS("data/processed/badm.rds")
#' beck2023 <- readr::read_csv("data/snapshots/site_koppen_beck2023.csv")
#' result <- compute_site_koppen_era5(era5_monthly, badm = badm, beck2023 = beck2023)
#' }
compute_site_koppen_era5 <- function(monthly_era5, badm = NULL, beck2023 = NULL,
                                      legend = NULL, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.", call. = FALSE)
  }

  clim <- compute_era5_monthly_climatology(monthly_era5, ...)

  t_cols <- paste0("t_", sprintf("%02d", 1:12))
  p_cols <- paste0("p_", sprintf("%02d", 1:12))

  classified <- lapply(seq_len(nrow(clim)), function(i) {
    row <- clim[i, ]
    if (row$n_years_used < 1L || anyNA(row[t_cols]) || anyNA(row[p_cols])) {
      return(list(kg_class = NA_character_, kg_second = NA_character_,
                  kg_main = NA_character_))
    }
    classify_koppen_geiger(
      ta_monthly = as.numeric(row[1, t_cols]),
      p_monthly  = as.numeric(row[1, p_cols])
    )
  })

  out <- clim |>
    dplyr::mutate(
      kg_class  = vapply(classified, `[[`, character(1L), "kg_class"),
      kg_second = vapply(classified, `[[`, character(1L), "kg_second"),
      kg_main   = vapply(classified, `[[`, character(1L), "kg_main"),
      koppen_class     = .data$kg_class,
      koppen_twoletter = .data$kg_second,
      koppen_main      = .data$kg_main,
      koppen_method    = "era5_local"
    ) |>
    dplyr::select(-dplyr::all_of(c(t_cols, p_cols)))

  if (!is.null(legend)) {
    out <- out |>
      dplyr::left_join(
        dplyr::select(legend, "koppen_class", "koppen_class_code",
                       "koppen_class_name", "koppen_main_name"),
        by = "koppen_class"
      )
  } else {
    out$koppen_class_code <- NA_integer_
    out$koppen_class_name <- NA_character_
    out$koppen_main_name  <- NA_character_
  }

  if (!is.null(badm)) {
    badm_kg <- badm |>
      dplyr::filter(.data$VARIABLE == "CLIMATE_KOEPPEN", !is.na(.data$DATAVALUE)) |>
      dplyr::distinct(.data$SITE_ID, .keep_all = TRUE) |>
      dplyr::select(site_id = "SITE_ID", badm_kg_class = "DATAVALUE")
    out <- dplyr::left_join(out, badm_kg, by = "site_id")
  } else {
    out$badm_kg_class <- NA_character_
  }

  if (!is.null(beck2023)) {
    beck_kg <- beck2023 |>
      dplyr::select(site_id = "site_id", beck2023_kg_class = "koppen_class")
    out <- dplyr::left_join(out, beck_kg, by = "site_id")
  } else {
    out$beck2023_kg_class <- NA_character_
  }

  out |>
    dplyr::mutate(
      agree_badm     = .data$kg_class == .data$badm_kg_class,
      agree_beck2023 = .data$kg_class == .data$beck2023_kg_class
    )
}
