#AGU dailys
# • DD / ERAI → 113 sites, 4194 total site-years across 113 files
# • DD / FULLSET → 232 sites, 2287 total site-years across 262 files
# • DD / L2 → 70 sites, 0 total site-years across 70 files
flux_vars <- c("GPP_NT_VUT_MEAN","GPP_NT_VUT_REF", "GPP_DT_CUT_REF", "GPP_DT_CUT_MEAN", "NEE_CUT_REF", "NEE_VUT_REF",  "RECO_NT_VUT_REF","RECO_NT_VUT_MEAN", "RECO_DT_VUT_REF", "RECO_DT_VUT_MEAN") # This could be a vetor
flux_vars_QC <- c("GPP_NT_VUT_MEAN_QC","GPP_NT_VUT_REF_QC", "GPP_DT_CUT_REF_QC", "GPP_DT_CUT_MEAN_QC", "NEE_CUT_REF_QC", "NEE_VUT_REF_QC",  "RECO_NT_VUT_REF_QC","RECO_NT_VUT_MEAN_QC", "RECO_DT_VUT_REF_QC", "RECO_DT_VUT_MEAN_QC") # This could be a vetor


daily_manifest <- manifest %>%
  dplyr::filter(
    # Keep what you already had: YY FULLSET (AMF + legacy FLX)
    (time_integral == "DD" & dataset == "FULLSET") |
      # NEW: ICOS L2 annual files
      (time_integral == "DD" & dataset == "L2" & data_center == "ICOSETC")
  )
# apparently not all files have a GPP_NT_VUT_MEAN column, so uses any_of()
daily <- map2(daily_manifest$path, daily_manifest$site,\(path, site) {
  read_csv(
    path,
    col_select = c(TIMESTAMP, any_of(flux_vars_QC),  any_of(flux_vars)),
    show_col_types = FALSE
  ) %>%
    mutate(site = unique(site), .before = 1)
}) %>%
  list_rbind() %>%
  mutate(
    across(any_of(flux_vars), \(x) na_if(x, -9999)),
    date = ymd(TIMESTAMP),
    .after = site
  ) %>%
  select(-TIMESTAMP) %>%
  left_join(metadata_full %>% select(site, LOCATION_LAT, LOCATION_LONG, COUNTRY, IGBP, -SITEID, -SITE_ID), by = join_by(site))

daily <- daily %>%
  dplyr::mutate(across(where(is.numeric), \(x) na_if(x, -9999))) 

# ----- DAILY QA/QC -----
daily_flagged <- daily %>%
  flag_bad_gapfilled(
    gate_vars       = qc_gate_vars,
    max_gapfilled   = max_gapfilled_bad,
    drop_if_missing = drop_if_qc_missing
  )

daily_clean <- daily_flagged %>% filter(!is_bad)

daily_excluded <- daily_flagged %>%
  filter(is_bad) %>%
  select(
    site, date, pct_gapfilled,
    dplyr::all_of(paste0(qc_gate_vars, "_QC")),
    dplyr::any_of(c(qc_gate_vars, paste0(qc_gate_vars, "_JOINTUNC")))
  )

message(sprintf("QA/QC (daily): kept %s of %s rows (%.1f%%).",
                scales::comma(nrow(daily_clean)),
                scales::comma(nrow(daily_flagged)),
                100 * nrow(daily_clean) / nrow(daily_flagged)))

p_qaqc_daily_nee <- ggplot(daily_clean, aes(NEE_VUT_REF)) +
  geom_histogram(linewidth = poster_stroke, bins = 60) +
  labs(x = lab_nee_daily, y = "Count", title = "Daily NEE after QA/QC")
save_plot_draft(p_qaqc_daily_nee, "qaqc_daily_nee_hist.png", width = 8, height = 6, dpi = 300)

# ============================
# 4b) Seasonal weekly cycles by IGBP
# ============================

# poster_geom_point / poster_geom_line can be defined here if not in your util file:
# poster_geom_point <- function(...) ggplot2::geom_point(size = 3.5, stroke = 0.7, alpha = 0.8, ...)
# poster_geom_line  <- function(...) ggplot2::geom_line(linewidth = 0.9, alpha = 0.9, ...)

min_sites <- 10  # only keep IGBPs with at least this many sites

igbp_site_counts <- daily_clean %>%
  dplyr::distinct(site, IGBP) %>%
  dplyr::count(IGBP, name = "n_sites")

eligible_igbp <- igbp_site_counts %>%
  dplyr::filter(n_sites >= min_sites) %>%
  dplyr::arrange(IGBP) %>%
  dplyr::pull(IGBP)

print(igbp_site_counts)
cat("Including IGBP:", paste(eligible_igbp, collapse = ", "), "\n")

flux_weekly <- daily_clean %>%
  dplyr::filter(IGBP %in% eligible_igbp) %>%
  dplyr::mutate(week = lubridate::isoweek(date)) %>%
  #, .after = doy) 
  dplyr::group_by(IGBP, week) %>%
  dplyr::summarize(
    GPP_NT_VUT_REF  = median(GPP_NT_VUT_REF,  na.rm = TRUE),
    RECO_NT_VUT_REF = median(RECO_NT_VUT_REF, na.rm = TRUE),
    NEE_VUT_REF     = median(NEE_VUT_REF,     na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  dplyr::mutate(fake_date = lubridate::ymd("2020-01-01") + lubridate::weeks(week))

# Stable colors/shapes (printer-friendly), ordered by eligible IGBPs
igbp_levels <- sort(unique(flux_weekly$IGBP))
pal <- colorspace::qualitative_hcl(length(igbp_levels), palette = "Dark 3")
names(pal) <- igbp_levels
shape_vec <- c(16,17,15,3,7,8,18,0,1,2,4,5,6,9,10,11)
shp <- setNames(shape_vec[seq_along(igbp_levels)], igbp_levels)

flux_weekly <- flux_weekly %>%
  dplyr::mutate(IGBP = factor(IGBP, levels = igbp_levels))
# Helper for month ticks – define this BEFORE add_top_right_axes is used
month_breaks <- scales::breaks_width("2 months")

y_axis_parallel <- theme(
  axis.title.y = ggtext::element_markdown(angle = 90, vjust = 0.5, hjust = 0.5, size = 16)
)

# GPP
p_seasonal_gpp <- ggplot2::ggplot(
  flux_weekly, ggplot2::aes(fake_date, GPP_NT_VUT_REF, color = IGBP, shape = IGBP)
) +
  poster_geom_point() + poster_geom_line() +
  ggplot2::scale_color_manual(values = pal, guide = ggplot2::guide_legend(ncol = 2)) +
  ggplot2::scale_shape_manual(values = shp) +
  ggplot2::labs(x = "", y = lab_gpp_daily, color = "IGBP", shape = "IGBP") +
  ggplot2::theme(axis.title.y = ggtext::element_markdown())
p_seasonal_gpp <- add_top_right_axes(p_seasonal_gpp)

# NEE
p_seasonal_nee <- ggplot2::ggplot(
  flux_weekly, ggplot2::aes(fake_date, NEE_VUT_REF*7, color = IGBP, shape = IGBP)
) +
  poster_geom_point() + poster_geom_line() +
  ggplot2::scale_color_manual(values = pal, guide = ggplot2::guide_legend(ncol = 2)) +
  ggplot2::scale_shape_manual(values = shp) +
  ggplot2::labs(x = "", y = lab_nee_daily, color = "IGBP", shape = "IGBP") +
  ggplot2::theme(axis.title.y = ggtext::element_markdown())
p_seasonal_nee <- add_top_right_axes(p_seasonal_nee)

# RECO
p_seasonal_reco <- ggplot2::ggplot(
  flux_weekly, ggplot2::aes(fake_date, RECO_NT_VUT_REF, color = IGBP, shape = IGBP)
) +
  poster_geom_point() + poster_geom_line() +
  ggplot2::scale_color_manual(values = pal, guide = ggplot2::guide_legend(ncol = 2)) +
  ggplot2::scale_shape_manual(values = shp) +
  ggplot2::labs(x = "", y = lab_reco_daily, color = "IGBP", shape = "IGBP") +
  ggplot2::theme(axis.title.y = ggtext::element_markdown())
p_seasonal_reco <- add_top_right_axes(p_seasonal_reco)

# pick your limits
yl_gpp  <- c(0, 12)
yl_nee  <- c(-60, 40)
yl_reco <- c(0, 12)

p_seasonal_gpp  <- p_seasonal_gpp  + coord_cartesian(ylim = yl_gpp)
p_seasonal_nee  <- p_seasonal_nee  + coord_cartesian(ylim = yl_nee)
p_seasonal_reco <- p_seasonal_reco + coord_cartesian(ylim = yl_reco)

p_seasonal_gpp
p_seasonal_nee
p_seasonal_reco


### Harvard plots 

daily_triplet <- daily_clean %>%
  dplyr::filter(site %in% c("FI-Hyy", "US-Dk3", "US-Me2"))

weekly_triplet <- daily_triplet %>%
  dplyr::mutate(week = lubridate::isoweek(date)) %>%
  dplyr::group_by(site, week) %>%
  dplyr::summarize(
    # central tendency
    NEE_med = median(NEE_CUT_REF, na.rm = TRUE),
    # uncertainty band (IQR; change probs if you want)
    NEE_lo  = quantile(NEE_CUT_REF, probs = 0.25, na.rm = TRUE, names = FALSE),
    NEE_hi  = quantile(NEE_CUT_REF, probs = 0.75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    fake_date  = lubridate::ymd("2020-01-01") + lubridate::weeks(week),
    NEE_med_wk = NEE_med * 4,
    NEE_lo_wk  = NEE_lo  * 4,
    NEE_hi_wk  = NEE_hi  * 4
  )

# --- choose site order + shapes (edit these) ---
site_levels <- c("FI-Hyy", "US-Dk3", "US-Me2")   # <- sets legend + mapping order

shp_triplet <- c(
  "FI-Hyy" = 15,  # square
  "US-Dk3" = 16,  # dot
  "US-Me2" = 25   # upside-down triangle
)

plot(daily_triplet$NEE_VUT_REF)

# --- ribbon colors (edit these; any hex names ok) ---
rib_cols <- c(
  "FI-Hyy" = "#F58518",
  "US-Dk3" = "#4C78A8",
  "US-Me2" = "#54A24B"
)

weekly_triplet2 <- weekly_triplet %>%
  dplyr::mutate(
    site = factor(site, levels = site_levels)
  )

Triplet_seasonal_nee <- ggplot2::ggplot(
  weekly_triplet2,
  ggplot2::aes(fake_date, NEE_med_wk, group = site)
) +
  # ribbons get color via fill mapping
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = NEE_lo_wk, ymax = NEE_hi_wk, fill = site),
    alpha = 0.18, color = NA
  ) +
  ggplot2::scale_fill_manual(values = rib_cols, guide = "none") +
  # lines + points stay B&W (black)
  poster_geom_line(color = "black") +
  poster_geom_point(ggplot2::aes(shape = site), color = "black") +
  ggplot2::scale_shape_manual(values = shp_triplet) +
  ggplot2::labs(x = "", y = lab_nee_monthly, shape = "site") +
  ggplot2::theme(axis.title.y = ggtext::element_markdown())

Triplet_seasonal_nee <- add_top_right_axes(Triplet_seasonal_nee)

Triplet_seasonal_nee <- Triplet_seasonal_nee + y_axis_parallel +
  coord_cartesian(ylim = c(-40, 10))

Triplet_seasonal_nee





##### JUST DBF


# poster_geom_point / poster_geom_line can be defined here if not in your util file:
# poster_geom_point <- function(...) ggplot2::geom_point(size = 3.5, stroke = 0.7, alpha = 0.8, ...)
# poster_geom_line  <- function(...) ggplot2::geom_line(linewidth = 0.9, alpha = 0.9, ...)

min_sites <- 10  # only keep IGBPs with at least this many sites

igbp_site_counts <- daily_clean %>%
  dplyr::distinct(site, IGBP) %>%
  dplyr::count(IGBP, name = "n_sites")

eligible_igbp <- "DBF" 

library(dplyr)
library(lubridate)
library(ggplot2)

# ----------------------------
# 0) Pick the date column safely
# ----------------------------
date_col <- dplyr::case_when(
  "date_object" %in% names(daily_clean) ~ "date_object",
  "date"        %in% names(daily_clean) ~ "date",
  "TIMESTAMP"   %in% names(daily_clean) ~ "TIMESTAMP",
  TRUE ~ NA_character_
)

if (is.na(date_col)) {
  stop("Could not find a date column. Expected one of: date_object, date, TIMESTAMP")
}

# If TIMESTAMP is numeric yyyymmdd, convert it; otherwise assume it is already Date-ish
daily_for_weekly <- daily_clean %>%
  mutate(
    .date = dplyr::case_when(
      date_col == "TIMESTAMP" ~ lubridate::ymd(as.character(.data[[date_col]])),
      TRUE ~ as.Date(.data[[date_col]])
    )
  )

# ----------------------------
# 1) Weekly summaries + ribbons
# ----------------------------
flux_weekly <- daily_for_weekly %>%
  dplyr::filter(IGBP %in% eligible_igbp) %>%
  dplyr::mutate(week = lubridate::isoweek(.date)) %>%
  dplyr::group_by(IGBP, week) %>%
  dplyr::summarise(
    NEE_med = median(NEE_VUT_REF, na.rm = TRUE),
    NEE_lo  = quantile(NEE_VUT_REF, probs = 0.05, na.rm = TRUE, names = FALSE),
    NEE_hi  = quantile(NEE_VUT_REF, probs = 0.95, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    fake_date  = lubridate::ymd("2020-01-01") + lubridate::weeks(week),
    NEE_med_wk = NEE_med * 7,
    NEE_lo_wk  = NEE_lo  * 7,
    NEE_hi_wk  = NEE_hi  * 7
  )

# (Optional) keep factor levels aligned with pal/shp if you’re using named vectors
if (!is.null(names(pal))) flux_weekly <- flux_weekly %>% mutate(IGBP = factor(IGBP, levels = names(pal)))
if (!is.null(names(shp))) flux_weekly <- flux_weekly %>% mutate(IGBP = factor(IGBP, levels = names(shp)))

# ----------------------------
# 2) Plot (ribbon + your points/lines styling)
# ----------------------------
p_seasonal_nee <- ggplot2::ggplot(flux_weekly, ggplot2::aes(x = fake_date)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = NEE_lo_wk, ymax = NEE_hi_wk, fill = IGBP),
    alpha = 0.18, color = NA
  ) +
  poster_geom_line(ggplot2::aes(y = NEE_med_wk, color = IGBP)) +
  poster_geom_point(ggplot2::aes(y = NEE_med_wk, color = IGBP, shape = IGBP)) +
  ggplot2::scale_color_manual(values = pal, guide = ggplot2::guide_legend(ncol = 2)) +
  ggplot2::scale_fill_manual(values = pal, guide = "none") +
  ggplot2::scale_shape_manual(values = shp) +
  ggplot2::labs(x = "", y = lab_nee_weekly, color = "IGBP", shape = "IGBP") +
  ggplot2::theme(axis.title.y = ggtext::element_markdown()) +
  y_axis_parallel +
  ggplot2::coord_cartesian(ylim = c(-60, 40))

# Apply your helper AFTER the plot exists
p_seasonal_nee <- add_top_right_axes(p_seasonal_nee)

p_seasonal_nee


# ============================================================
# Figure 4 replication (consistent + robust)
# - Growing season length from daily_clean (two definitions)
# - Annual NEE from annual
# - Keep only sites with >= 10 years of annual data (after merge)
# - Color + shape by IGBP (stable, printer-friendly)
# - Numeric x-axis 100..365 (days), never months
# - Horizontal lines at y = 100 and y = -800
# - Regression line + dashed 95% SE bounds (no IGBP inheritance)
# ============================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(colorspace)

min_days_per_year <- 330

# Force numeric day-axis (NOT months)
fig4_x_limits <- c(100, 365)
fig4_x_breaks <- seq(100, 365, by = 50)

# Your chosen y-axis range
fig4_y_limits <- c(-800, 100)
fig4_y_breaks <- seq(100, -800, by = -50)

lab_fig4_x <- expression("Length of growing season (days)")
lab_fig4_y <- expression(paste("NEE (g C ", m^{-2}, " ", yr^{-1}, ")"))

# ------------------------------------------------------------
# 0) Numeric-safe top/right ticks helper (does NOT touch scales)
# ------------------------------------------------------------
add_top_right_axes_numeric <- function(p) {
  p + theme(
    axis.ticks.length = grid::unit(0.18, "cm"),
    axis.ticks.x.top  = element_line(),
    axis.ticks.y.right= element_line(),
    axis.text.x.top   = element_blank(),
    axis.text.y.right = element_blank()
  )
}

# ------------------------------------------------------------
# 1) Growing season length from DAILY (site-year)
#    - gsl_count: number of uptake days (NEE < 0)
#    - gsl_span : span from first to last uptake day (NEE < 0)
#    (computed safely to avoid Inf/NA coercion warnings)
# ------------------------------------------------------------
gsl_by_siteyear <- daily_clean %>%
  mutate(year = lubridate::year(date)) %>%
  select(site, year, date, NEE = NEE_VUT_REF, IGBP) %>%
  filter(!is.na(date), !is.na(NEE)) %>%
  group_by(site, year) %>%
  summarise(
    n_days = n(),
    IGBP = dplyr::first(IGBP),
    gsl_count = sum(NEE < 0, na.rm = TRUE),
    gsl_span  = if (any(NEE < 0, na.rm = TRUE)) {
      as.integer(max(date[NEE < 0], na.rm = TRUE) - min(date[NEE < 0], na.rm = TRUE) + 1)
    } else {
      NA_integer_
    },
    .groups = "drop"
  ) %>%
  # drop years with too few daily observations
  filter(n_days >= min_days_per_year)

# ------------------------------------------------------------
# 2) Annual NEE from ANNUAL + merge by site + year
# ------------------------------------------------------------
fig4_merged <- annual %>%
  select(site, year, NEE_annual = NEE_VUT_REF) %>%
  filter(!is.na(site), !is.na(year), !is.na(NEE_annual)) %>%
  inner_join(
    gsl_by_siteyear %>% select(site, year, IGBP, gsl_count, gsl_span),
    by = c("site", "year")
  )
fig4_merged <- fig4_merged %>%
  dplyr::filter(IGBP %in% c("DBF", "EBF"))
# ------------------------------------------------------------
# 3) Keep only sites with >= 10 years of annual data
# ------------------------------------------------------------
sites_10yr <- fig4_merged %>%
  distinct(site, year) %>%
  count(site, name = "n_years") %>%
  filter(n_years >= 10) %>%
  pull(site)

fig4_merged <- fig4_merged %>%
  filter(site %in% sites_10yr)

# ------------------------------------------------------------
# 4) Stable colors/shapes by IGBP (based on Fig 4 data)
# ------------------------------------------------------------
igbp_levels <- sort(unique(na.omit(fig4_merged$IGBP)))

pal <- colorspace::qualitative_hcl(length(igbp_levels), palette = "Dark 3")
names(pal) <- igbp_levels

shape_vec <- c(16,17,15,3,7,8,18,0,1,2,4,5,6,9,10,11)
shp <- setNames(shape_vec[seq_along(igbp_levels)], igbp_levels)

fig4_merged <- fig4_merged %>%
  mutate(IGBP = factor(IGBP, levels = igbp_levels))

# ------------------------------------------------------------
# 5) Plot helper (numeric x, IGBP points, black regression + dashed bounds)
# ------------------------------------------------------------
plot_fig4 <- function(df, x_col, title = NULL) {
  
  df <- df %>%
    filter(!is.na(.data[[x_col]]), !is.na(NEE_annual), !is.na(IGBP))
  
  fit <- lm(stats::as.formula(paste0("NEE_annual ~ ", x_col)), data = df)
  
  xgrid <- seq(fig4_x_limits[1], fig4_x_limits[2], length.out = 200)
  pred  <- predict(fit, newdata = setNames(data.frame(xgrid), x_col), se.fit = TRUE)
  tval  <- qt(0.975, df = df.residual(fit))
  
  pred_df <- data.frame(
    x = xgrid,
    yhat = pred$fit,
    ylo  = pred$fit - tval * pred$se.fit,
    yhi  = pred$fit + tval * pred$se.fit
  )
  
  p <- ggplot(df, aes(x = .data[[x_col]], y = NEE_annual, color = IGBP, shape = IGBP)) +
    # points by IGBP (your poster style)
    poster_geom_point() +
    
    # horizontal reference lines (do not inherit IGBP aesthetics)
    geom_hline(yintercept = 100,  color = "black", linewidth = 0.6, inherit.aes = FALSE) +
    geom_hline(yintercept = -800, color = "black", linewidth = 0.6, inherit.aes = FALSE) +
    
    # regression + dashed 95% SE bounds (do not inherit IGBP aesthetics)
    geom_line(data = pred_df, aes(x = x, y = yhat), color = "black", linewidth = 0.9, inherit.aes = FALSE) +
    geom_line(data = pred_df, aes(x = x, y = ylo),  color = "black", linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE) +
    geom_line(data = pred_df, aes(x = x, y = yhi),  color = "black", linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE) +
    
    # numeric axes (days) + your y range
    scale_x_continuous(limits = fig4_x_limits, breaks = fig4_x_breaks) +
    scale_y_continuous(limits = fig4_y_limits, breaks = fig4_y_breaks) +
    
    # IGBP scales
    scale_color_manual(values = pal, guide = guide_legend(ncol = 2)) +
    scale_shape_manual(values = shp) +
    
    labs(x = lab_fig4_x, y = lab_fig4_y, title = title, color = "IGBP", shape = "IGBP") +
    theme_classic() +
    theme(axis.title.y = ggtext::element_markdown())
  
  # IMPORTANT: use numeric-safe top/right ticks (avoid month/date scales)
  add_top_right_axes_numeric(p)
}

# ------------------------------------------------------------
# 6) Two separate plots
# ------------------------------------------------------------
p_fig4_count <- plot_fig4(
  fig4_merged, "gsl_count",
  "Figure 4: Annual NEE vs length of growing season (uptake days)"
)

p_fig4_span <- plot_fig4(
  fig4_merged, "gsl_span",
  "Figure 4: Annual NEE vs length of growing season (span first–last uptake day)"
)

p_fig4_count
p_fig4_span


