# fcn_plot_FLUXNET.R
# Centralized plotting functions for FLUXNET workflow
# -----------------------------------------------
# fcn_plot_FLUXNET.R
# Master Plotting Functions for FLUXNET Workflow
# -----------------------------------------------
# This script contains modular plotting functions designed to visualize 
# flux tower data at multiple temporal and spatial scales. It supports 
# annual and daily summaries, time series, latitudinal trends, and 
# categorical comparisons by IGBP.

# ------------------------
# Function Overview
# ------------------------

# 1. plot_flux_by_igbp()
#    - Boxplot of fluxes aggregated by IGBP class (across all years)
#    - Also returns bar plots of medians and site-year counts
#    - Returns: list(flux_plot, median_plot, count_plot, composite_plot)

# 2. plot_flux_by_igbp_timeslice_grouped()
#    - Boxplot of fluxes grouped by IGBP and binned into 5-year slices
#    - Useful for tracking trends within vegetation types
#    - Returns: list(flux_plot, median_plot, count_plot)

# 3. plot_flux_box_by_group()
#    - Boxplots of annual fluxes grouped by biome categories:
#      (Forest, Shrub/Opens, Grass/Crops/Wet, Other)
#    - Includes optional y-axis squishing to reduce influence of outliers
#    - Returns: list(Forest, ShrubOpens, GrassCropsWet, Other)

# 4. plot_flux_timeseries_by_igbp()
#    - Time series of median annual fluxes with 95% CI, faceted by IGBP
#    - Allows interannual trend tracking across vegetation types
#    - Returns: a ggplot object

# 5. plot_latitudinal_flux()
#    - Ribbon plot of flux min/max range by latitude band
#    - Includes site-level mean flux points
#    - Inputs: annual_data + site_metadata (must include SITE_ID, LAT, IGBP)
#    - Returns: a ggplot object

# 6. plot_seasonal_cycle()
#    - Seasonal cycle of daily mean flux by biome group with 95% CI ribbon
#    - Customizable y-axis handling (full/squish)
#    - Returns: list(Forest, ShrubOpens, GrassCropsWet, Other)

# 7. plot_annual_fluxnet_data()
#    - Simple scatterplots:
#        (a) Precipitation vs NEE
#        (b) Temperature vs GPP
#    - Useful for quick climate-flux exploratory plots
#    - Returns: list(precip_vs_nee, temp_vs_gpp)

# 8. PlotXY_annual()
#    - General XY scatterplot with IGBP as shape aesthetic
#    - Fully flexible input of any x/y variables
#    - Returns: a ggplot object

# helper function - add top and bottom axes?

# ------------------------
# Notes:
# - All functions use tidyverse (dplyr, ggplot2) grammar.
# - `flux_var` argument (e.g., "NEE_VUT_REF", "GPP_NT_VUT_REF") controls which flux is visualized.
# - y-axis units are formatted automatically using expression() based on variable name.
# - Most functions accept optional settings for controlling y-axis limits, time windows, or grouping behavior.
# - All plots return either a single ggplot object or a list of them that can be composed using patchwork or gridExtra.

# ------------------------
# Usage Example:
#   plots <- plot_flux_by_igbp(annual_data, "NEE_VUT_REF")
#   plots$composite_plot
#
#   ts_plot <- plot_flux_timeseries_by_igbp(annual_data, "GPP_NT_VUT_REF")
#   print(ts_plot)
# ------------------------

# -----------------------------
# Boxplot: IGBP Aggregated Flux Summary
# -----------------------------



# plot_flux_by_igbp <- function(annual_data, flux_var = "NEE_VUT_REF") {
#   igbp_order <- c("DBF", "ENF", "MF", "DNF", "EBF", "OSH", "CSH", "WSA", "SAV", "GRA", "CRO", "WET", "BSV")
# 
#   annual_data <- annual_data %>%
#     mutate(
#       FLUX = .data[[flux_var]],
#       flux_sign = factor(ifelse(FLUX < 0, "Negative", "Positive"), levels = c("Negative", "Positive")),
#       IGBP = factor(IGBP, levels = igbp_order)
#     )
# 
#   y_label <- if (str_starts(flux_var, "NEE")) {
#     expression(NEE~(mu*mol~m^{-2}~s^{-1}))
#   } else if (str_starts(flux_var, "GPP")) {
#     expression(GPP~(mu*mol~m^{-2}~s^{-1}))
#   } else if (str_starts(flux_var, "RECO")) {
#     expression(Reco~(mu*mol~m^{-2}~s^{-1}))
#   } else if (str_starts(flux_var, "LE")) {
#     expression(LE~(W~m^{-2}))
#   } else if (str_starts(flux_var, "WUE")) {
#     expression(WUE~(GPP/LE))
#   } else {
#     flux_var
#   }
# 
#   p_flux <- ggplot(annual_data, aes(x = IGBP, y = FLUX)) +
#     geom_boxplot(color = "black", fill = NA, outlier.shape = NA) +
#     geom_jitter(aes(color = flux_sign), width = 0.25, alpha = 0.4, size = 1) +
#     scale_color_manual(values = c("Negative" = "#1b9e77", "Positive" = "#d95f02"), name = "Flux Sign") +
#     labs(x = NULL, y = y_label, title = "Flux Distribution by IGBP (All Years)") +
#     theme_classic(base_size = 14) +
#     theme(panel.background = element_rect(color = "black"), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom")
# 
#   summary_data <- annual_data %>%
#     group_by(IGBP) %>%
#     summarize(median_flux = median(FLUX, na.rm = TRUE), .groups = "drop") %>%
#     mutate(IGBP = factor(IGBP, levels = igbp_order))
# 
#   p_median <- ggplot(summary_data, aes(x = IGBP, y = median_flux)) +
#     geom_col(fill = "black", alpha = 0.6) +
#     labs(x = NULL, y = "Median") +
#     theme_classic(base_size = 14) +
#     theme(panel.background = element_rect(color = "black"), axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
#   site_counts <- annual_data %>%
#     group_by(IGBP) %>%
#     summarize(n_siteyears = n(), .groups = "drop") %>%
#     mutate(IGBP = factor(IGBP, levels = igbp_order))
# 
#   p_count <- ggplot(site_counts, aes(x = IGBP, y = n_siteyears)) +
#     geom_col(fill = "gray40", alpha = 0.7) +
#     labs(x = "IGBP Class", y = "# Site Years") +
#     theme_classic(base_size = 14) +
#     theme(panel.background = element_rect(color = "black"), axis.text.x = element_text(angle = 45, hjust = 1))
# 
#   p_composite <- p_flux / p_median / p_count + patchwork::plot_layout(heights = c(0.7, 0.2, 0.1))
# 
#   list(
#     flux_plot = p_flux,
#     median_plot = p_median,
#     count_plot = p_count,
#     composite_plot = p_composite
#   )
# }

#' Plot annual new sites and site-years by country (stacked)
#'
#' @param annual_df Annual FLUXNET data with columns: site, year, COUNTRY
#' @return A list with two ggplot objects: $new_sites and $site_years
plot_sites_by_country <- function(annual_df) {
  # Ensure required columns exist
  stopifnot(all(c("site", "year", "COUNTRY") %in% names(annual_df)))
  
  # Annual site-years per country
  siteyears_by_year_country <- annual_df %>%
    dplyr::filter(!is.na(year), !is.na(COUNTRY)) %>%
    dplyr::count(year, COUNTRY, name = "n_site_years")
  
  # First year each site appears (new sites)
  site_first_year <- annual_df %>%
    dplyr::filter(!is.na(year), !is.na(COUNTRY)) %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(
      first_year = min(year, na.rm = TRUE),
      COUNTRY    = dplyr::first(COUNTRY),
      .groups    = "drop"
    )
  
  new_sites_by_year_country <- site_first_year %>%
    dplyr::count(first_year, COUNTRY, name = "n_new_sites") %>%
    dplyr::rename(year = first_year)
  
  # Stable country ordering (by total site-years)
  country_levels <- siteyears_by_year_country %>%
    dplyr::group_by(COUNTRY) %>%
    dplyr::summarise(total_site_years = sum(n_site_years), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total_site_years)) %>%
    dplyr::pull(COUNTRY)
  
  p_new_sites <- new_sites_by_year_country %>%
    mutate(COUNTRY = factor(COUNTRY, levels = country_levels)) %>%
    ggplot(aes(x = year, y = n_new_sites, fill = COUNTRY)) +
    geom_col() +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_country_fill(country_levels, name = "Country") +   # <- this line
    poster_theme() +
    labs(x = "Year", y = "New sites added")
  
  p_siteyears <- siteyears_by_year_country %>%
    mutate(COUNTRY = factor(COUNTRY, levels = country_levels)) %>%
    ggplot(aes(x = year, y = n_site_years, fill = COUNTRY)) +
    geom_col() +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_country_fill(country_levels, name = "Country") +   # <- and here
    poster_theme() +
    labs(x = "Year", y = "Site-years")
  
  
  list(
    new_sites  = p_new_sites,
    site_years = p_siteyears
  )
}


# ------------------------------------------------------------
# Derive ISO2 codes and COUNTRY from site ID
# - site IDs like "US-XYZ", "CA-ABC", "UK-..." etc.
# - first 2 letters are ISO2, except UK -> GB, SP -> ES
# - fills COUNTRY if missing, keeps existing COUNTRY if present
# ------------------------------------------------------------
assign_country_from_site <- function(df, site_col = "site") {
  stopifnot(site_col %in% names(df))
  
  df %>%
    dplyr::mutate(
      Alpha2_country = substr(.data[[site_col]], 1, 2),
      Alpha2_country = dplyr::recode(
        Alpha2_country,
        "UK" = "GB",
        "SP" = "ES",
        .default = Alpha2_country
      ),
      COUNTRY_DERIVED = countrycode::countrycode(
        Alpha2_country,
        origin      = "iso2c",
        destination = "country.name.en"
      ),
      COUNTRY = dplyr::coalesce(COUNTRY, COUNTRY_DERIVED, Alpha2_country),
      .after = {{site_col}}
    )
}


  plot_flux_by_igbp <- function(annual_data, flux_var = "NEE_VUT_REF",
                                igbp_order = NULL) {
    
    default_igbp_order <- c(
      "DBF","ENF","MF","DNF","EBF",
      "OSH","CSH","WSA","SAV","GRA","CRO","WET","BSV",
      "CVM","WAT"
    )
    
    # Use custom order if provided
    if (is.null(igbp_order)) igbp_order <- default_igbp_order
    
    # Filter to requested IGBPs (critical)
    annual_data <- annual_data %>%
      dplyr::filter(IGBP %in% igbp_order) %>%
      dplyr::mutate(
        IGBP = factor(IGBP, levels = igbp_order),
        FLUX = .data[[flux_var]]
      )
  
  annual_data <- annual_data %>%
    dplyr::mutate(
      FLUX = .data[[flux_var]],
      flux_sign = factor(ifelse(FLUX < 0, "Negative", "Positive"),
                         levels = c("Negative", "Positive")),
      IGBP = factor(as.character(IGBP), levels = igbp_order)
    )
  
  y_label <- if (stringr::str_starts(flux_var, "NEE")) {
    expression(NEE~(mu*mol~m^{-2}~s^{-1}))
  } else if (stringr::str_starts(flux_var, "GPP")) {
    expression(GPP~(mu*mol~m^{-2}~s^{-1}))
  } else if (stringr::str_starts(flux_var, "RECO")) {
    expression(Reco~(mu*mol~m^{-2}~s^{-1}))
  } else if (stringr::str_starts(flux_var, "LE")) {
    expression(LE~(W~m^{-2}))
  } else if (stringr::str_starts(flux_var, "WUE")) {
    expression(WUE~(GPP/LE))
  } else {
    flux_var
  }
  
  poster_style <- ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(
      axis.text         = ggplot2::element_text(size = 14, color = "black"),
      axis.title        = ggplot2::element_text(size = 16),
      axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.6),
      axis.ticks.length = grid::unit(-3, "pt"),
      panel.background  = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8),
      legend.position   = "bottom"
    )
  
  p_flux <- ggplot2::ggplot(annual_data, ggplot2::aes(x = IGBP, y = FLUX)) +
    ggplot2::geom_jitter(width = 0.25, alpha = 0.35, size = 2.6,
                         color = "grey40", show.legend = FALSE) +
    ggplot2::geom_boxplot(color = "black", fill = NA, outlier.shape = NA, linewidth = 0.9) +
    ggplot2::labs(x = NULL, y = y_label, title = "Flux Distribution by IGBP (All Years)") +
    poster_style +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x     = ggplot2::element_blank(),
      axis.ticks.x    = ggplot2::element_blank(),
      axis.title.x    = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(clip = "off")
  
  summary_table <- annual_data %>%
    dplyr::group_by(IGBP) %>%
    dplyr::summarize(
      median_flux = median(FLUX, na.rm = TRUE),
      n_siteyears = dplyr::n(),
      n_sites     = dplyr::n_distinct(site),
      p25_flux    = stats::quantile(FLUX, 0.25, na.rm = TRUE),
      p75_flux    = stats::quantile(FLUX, 0.75, na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    dplyr::mutate(IGBP = factor(as.character(IGBP), levels = igbp_order)) %>%
    dplyr::arrange(IGBP)
  
  p_median <- ggplot2::ggplot(summary_table, ggplot2::aes(x = IGBP, y = median_flux)) +
    ggplot2::geom_col(fill = "black", alpha = 0.6) +
    ggplot2::labs(x = NULL, y = "Median") +
    poster_style +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  
  p_count <- ggplot2::ggplot(summary_table, ggplot2::aes(x = IGBP, y = n_siteyears)) +
    ggplot2::geom_col(fill = "gray40", alpha = 0.7) +
    ggplot2::labs(x = "IGBP", y = "# Site Years") +
    poster_style +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  p_composite <- p_flux / p_median / p_count +
    patchwork::plot_layout(heights = c(0.7, 0.2, 0.1))
  
  list(
    flux_plot      = p_flux,
    median_plot    = p_median,
    count_plot     = p_count,
    composite_plot = p_composite,
    summary_table  = summary_table
  )
}


# -----------------------------
# Boxplot: IGBP Aggregated Flux by Time Slice (Grouped by IGBP)
# -----------------------------
plot_flux_by_igbp_timeslice_grouped <- function(annual_data, flux_var = "NEE_VUT_REF") {
  igbp_order <- c("DBF", "ENF", "MF", "DNF", "EBF", "OSH", "CSH", "WSA", "SAV", "GRA", "CRO", "WET", "BSV")
  
  annual_data <- annual_data %>%
    mutate(
      FLUX = .data[[flux_var]],
      year = as.integer(TIMESTAMP),
      TimeSlice = cut(year,
                      breaks = seq(1999, 2025, by = 5),
                      labels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024"),
                      right = TRUE),
      IGBP = factor(IGBP, levels = igbp_order)
    ) %>%
    filter(!is.na(TimeSlice), !is.na(IGBP))
  
  y_label <- if (str_starts(flux_var, "NEE")) {
    expression(NEE~(mu*mol~m^{-2}~s^{-1}))
  } else if (str_starts(flux_var, "GPP")) {
    expression(GPP~(mu*mol~m^{-2}~s^{-1}))
  } else if (str_starts(flux_var, "RECO")) {
    expression(Reco~(mu*mol~m^{-2}~s^{-1}))
  } else if (str_starts(flux_var, "LE")) {
    expression(LE~(W~m^{-2}))
  } else if (str_starts(flux_var, "WUE")) {
    expression(WUE~(GPP/LE))
  } else {
    flux_var
  }
  
  p_flux <- ggplot(annual_data, aes(x = IGBP, y = FLUX, fill = TimeSlice)) +
    geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
    labs(
      x = "IGBP Class",
      y = y_label,
      fill = "Time Slice"
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(color = "black")
    )
  
  summary_data <- annual_data %>%
    group_by(IGBP, TimeSlice) %>%
    summarize(median_flux = median(FLUX, na.rm = TRUE), .groups = "drop")
  
  p_median <- ggplot(summary_data, aes(x = IGBP, y = median_flux, fill = TimeSlice)) +
    geom_col(position = position_dodge(width = 0.75)) +
    labs(
      title = "Median Flux by IGBP and Time Slice",
      x = "IGBP Class",
      y = "Median",
      fill = "Time Slice"
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(color = "black")
    )
  
  site_counts <- annual_data %>%
    group_by(IGBP, TimeSlice) %>%
    summarize(n_siteyears = n(), .groups = "drop")
  
  p_count <- ggplot(site_counts, aes(x = IGBP, y = n_siteyears, fill = TimeSlice)) +
    geom_col(position = position_dodge(width = 0.75)) +
    labs(
      title = "Site Years by IGBP and Time Slice",
      x = "IGBP Class",
      y = "# Site Years",
      fill = "Time Slice"
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(color = "black")
    )
  
  return(list(
    flux_plot = p_flux,
    median_plot = p_median,
    count_plot = p_count
  ))
}

# -----------------------------
# Boxplot: Flux by IGBP Group Over Time
# -----------------------------
plot_flux_box_by_group <- function(annual_data, flux_var = "NEE_VUT_REF", y_mode = "full") {
  # Define IGBP groups
  group1 <- c("DBF", "ENF", "MF", "EBF")
  group2 <- c("OSH", "CSH", "WSA", "SAV")
  group3 <- c("GRA", "CRO", "WET")
  
  # Optionally calculate WUE
  if (flux_var == "WUE") {
    annual_data <- annual_data %>%
      mutate(FLUX = GPP_NT_VUT_REF / LE_F_MDS)
  } else {
    annual_data <- annual_data %>%
      mutate(FLUX = .data[[flux_var]])
  }
  
  # Clean and group
  annual_data <- annual_data %>%
    mutate(
      year = TIMESTAMP,
      flux_sign = factor(
        ifelse(FLUX < 0, "Negative", "Positive"),
        levels = c("Negative", "Positive")
      ),
      IGBP_group = case_when(
        IGBP %in% group1 ~ "Forest",
        IGBP %in% group2 ~ "Shrub/Opens",
        IGBP %in% group3 ~ "Grass/Crops/Wet",
        TRUE ~ "Other"
      )
    )
  
  # Site count annotations
  site_counts <- annual_data %>%
    group_by(year, IGBP) %>%
    summarize(n_sites = n_distinct(site), .groups = "drop")
  
  # Label formatting
  y_label <- if (str_starts(flux_var, "NEE")) {
    expression(NEE~(mu*mol~m^{-2}~s^{-1}))
  } else if (str_starts(flux_var, "GPP")) {
    expression(GPP~(mu*mol~m^{-2}~s^{-1}))
  } else if (str_starts(flux_var, "RECO")) {
    expression(Reco~(mu*mol~m^{-2}~s^{-1}))
  } else if (str_starts(flux_var, "LE")) {
    expression(LE~(W~m^{-2}))
  } else if (str_starts(flux_var, "WUE")) {
    expression(WUE~(GPP/LE))
  } else {
    flux_var
  }
  
  plot_box <- function(data, counts, label) {
    # Y-axis configuration
    y_limits <- if (y_mode == "squish") {
      upper <- quantile(data$FLUX, probs = 0.95, na.rm = TRUE)
      scale_y_continuous(limits = c(0, upper), oob = scales::squish)
    } else {
      scale_y_continuous()
    }
    
    ggplot(data, aes(x = factor(year), y = FLUX)) +
      geom_boxplot(outlier.shape = NA, fill = NA, color = "black", alpha = 0.4) +
      geom_jitter(aes(color = flux_sign), size = 0.8, alpha = 0.3, width = 0.25) +
      geom_text(
        data = counts,
        aes(x = factor(year), y = max(data$FLUX, na.rm = TRUE) * 1.05, label = paste0("n=", n_sites)),
        inherit.aes = FALSE,
        size = 3, vjust = 0
      ) +
      scale_color_manual(
        values = c("Negative" = "#1b9e77", "Positive" = "#d95f02"),
        name = paste0(label, " Sign")
      ) +
      y_limits +
      facet_wrap(vars(IGBP), ncol = 1, strip.position = "right") +
      labs(title = label, x = "Year", y = y_label) +
      theme_classic() +
      theme(
        panel.background = element_rect(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  }
  
  grouped_data <- function(label) {
    filter(annual_data, IGBP_group == label)
  }
  
  grouped_counts <- function(label) {
    filter(site_counts, IGBP %in% unique(grouped_data(label)$IGBP))
  }
  
  list(
    Forest = plot_box(grouped_data("Forest"), grouped_counts("Forest"), "Forest"),
    ShrubOpens = plot_box(grouped_data("Shrub/Opens"), grouped_counts("Shrub/Opens"), "Shrub/Opens"),
    GrassCropsWet = plot_box(grouped_data("Grass/Crops/Wet"), grouped_counts("Grass/Crops/Wet"), "Grass/Crops/Wet"),
    Other = plot_box(grouped_data("Other"), grouped_counts("Other"), "Other")
  )
}

# -----------------------------
# Timeseries: Median Annual Fluxes by IGBP
#' plot_flux_timeseries_by_igbp: Plot median annual fluxes with 95% CI by IGBP over time
#'
#' @param annual_data A data frame containing annual flux summaries with columns 'site', 'TIMESTAMP', and flux variables.
#' @param flux_var The name of the flux variable to plot. Options: 'NEE_VUT_REF', 'GPP_NT_VUT_REF', 'RECO_NT_VUT_REF', 'LE_F_MDS', or 'WUE'.
#'
#' @return A ggplot object showing median flux and 95% CI over time, faceted by IGBP class.
# -----------------------------
plot_flux_timeseries_by_igbp <- function(annual_data, flux_var = "NEE_VUT_REF") {
  library(dplyr)
  library(ggplot2)
  
  # Define IGBP groupings and order
  group1 <- c("DBF", "ENF", "MF", "EBF")
  group2 <- c("OSH", "CSH", "WSA", "SAV")
  group3 <- c("GRA", "CRO", "WET")
  igbp_levels <- c(group1, group2, group3)
  
  # Add FLUX column and reorder IGBP
  annual_data <- annual_data %>%
    mutate(
      FLUX = if (flux_var == "WUE") GPP_NT_VUT_REF / LE_F_MDS else .data[[flux_var]],
      year = as.numeric(TIMESTAMP),
      IGBP = factor(IGBP, levels = igbp_levels)  # control facet order
    )
  
  # Compute summary stats
  ts_summary <- annual_data %>%
    group_by(IGBP, year) %>%
    summarise(
      median_flux = median(FLUX, na.rm = TRUE),
      lower_CI    = quantile(FLUX, 0.025, na.rm = TRUE),
      upper_CI    = quantile(FLUX, 0.975, na.rm = TRUE),
      .groups     = "drop"
    )
  
  # Y-axis label
  y_label <- if (str_starts(flux_var, "NEE")) {
    # expression(NEE~(mu*mol~m^{-2}~s^{-1}))
    "NEE ($\\mu mol\\ m^{-2}s^{-2}$)"
  } else if (str_starts(flux_var, "GPP")) {
    # expression(GPP~(mu*mol~m^{-2}~s^{-1}))
    "GPP ($\\mu mol\\ m^{-2}s^{-2}$)"
  } else if (str_starts(flux_var, "RECO")) {
    # expression(Reco~(mu*mol~m^{-2}~s^{-1}))
    "Reco ($\\mu mol\\ m^{-2}s^{-2}$)"
  } else if (str_starts(flux_var, "LE")) {
    # expression(LE~(W~m^{-2}))
    "LE ($W\\ m^{-2}$)"
  } else if (str_starts(flux_var, "WUE")) {
    # expression(WUE~(GPP/LE))
    "WUE (GPP/LE)"
  } else {
    flux_var
  }
  
  # Plot with ordered facets
  ggplot(ts_summary, aes(x = year, y = median_flux)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                fill = "steelblue", alpha = 0.3) +
    geom_line(color = "steelblue", size = 1) +
    facet_wrap(~ IGBP, scales = "free_y", ncol = 4) +
    labs(
      x = "Year",
      y = latex2exp::TeX(y_label),
      title = latex2exp::TeX(paste("Median", y_label, "\u00b195% CI by Year & IGBP"))
    ) +
    theme_classic(base_size = 14)
}


# -----------------------------
# Latitudinal Summary of Fluxes
# -----------------------------
plot_latitudinal_flux <- function(annual_data, site_metadata, flux_var = "NEE_VUT_REF", bin_width = 5, shape_palette = 0:15) {
  # Optionally filter out negative GPP values
  if (flux_var == "GPP_NT_VUT_REF") {
    annual_data <- annual_data %>%
      filter(.data[[flux_var]] >= 0)
  }
  
  # Step 1: Summarize flux per site
  site_summary <- annual_data %>%
    group_by(site) %>%
    summarize(
      min_flux  = min(.data[[flux_var]], na.rm = TRUE),
      max_flux  = max(.data[[flux_var]], na.rm = TRUE),
      mean_flux = mean(.data[[flux_var]], na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    left_join(site_metadata %>% select(SITE_ID, LOCATION_LAT, IGBP), by = c("site" = "SITE_ID")) %>%
    rename(lat = LOCATION_LAT)
  
  # Step 2: Bin sites by latitude
  breaks_vec <- seq(-90, 90, by = bin_width)
  binned_summary <- site_summary %>%
    mutate(
      lat_bin = cut(lat, breaks = breaks_vec, include.lowest = TRUE, right = FALSE)
    ) %>%
    group_by(lat_bin) %>%
    summarize(
      lat_lower = first(breaks_vec[as.integer(lat_bin)]),
      lat_upper = lat_lower + bin_width,
      min_flux  = min(min_flux, na.rm = TRUE),
      max_flux  = max(max_flux, na.rm = TRUE),
      .groups   = "drop"
    )
  
  # Create a y-axis label and title based on flux_var
  y_label <- if (str_starts(flux_var, "NEE")) {
    expression(NEE~(mu*mol~m^{-2}~s^{-1}))
  } else if (str_starts(flux_var, "GPP")) {
    expression(GPP~(mu*mol~m^{-2}~s^{-1}))
  } else if (str_starts(flux_var, "RECO")) {
    expression(Reco~(mu*mol~m^{-2}~s^{-1}))
  } else if (str_starts(flux_var, "LE")) {
    expression(LE~(W~m^{-2}))
  } else if (str_starts(flux_var, "WUE")) {
    expression(WUE~(GPP/LE))
  } else {
    flux_var
  }
  
  title_text <- paste0("Flux range in ", bin_width, "° lat bands")
  
  # Step 3: Generate the plot
  ggplot() +
    geom_ribbon(
      data = binned_summary,
      aes(
        x = lat_lower + bin_width / 2,
        ymin = min_flux,
        ymax = max_flux
      ),
      fill = "steelblue",
      alpha = 0.4
    ) +
    coord_flip() +
    geom_point(
      data = site_summary,
      aes(x = lat, y = mean_flux, shape = IGBP),
      size = 3,
      color = "black",
      stroke = 0.8,
      inherit.aes = FALSE
    ) +
    scale_shape_manual(values = shape_palette) +
    labs(
      x = "Latitude (°)",
      y = y_label,
      title = title_text
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 10),
      panel.background = element_rect(color = "black"),
      legend.position = "right"
    )
}


# -----------------------------
# Daily Seasonal Cycle by IGBP Group
# -----------------------------

plot_seasonal_cycle <- function(daily_data, flux_var = "GPP_NT_VUT_REF", y_mode = "full") {
  # Group definitions (reuse from annual boxplot)
  group1 <- c("DBF", "ENF", "MF", "EBF")
  group2 <- c("OSH", "CSH", "WSA", "SAV")
  group3 <- c("GRA", "CRO", "WET")
  
  daily_data <- daily_data %>%
    mutate(
      DOY = yday(date_object),
      FLUX = ifelse(.data[[flux_var]] < -9000, NA, .data[[flux_var]]),
      IGBP_group = case_when(
        IGBP %in% group1 ~ "Forest",
        IGBP %in% group2 ~ "Shrub/Opens",
        IGBP %in% group3 ~ "Grass/Crops/Wet",
        TRUE ~ "Other"
      )
    )
  
  y_label <- case_when(
    flux_var == "NEE_VUT_REF" ~ "NEE (μmol m-2 s-1)",
    flux_var == "GPP_NT_VUT_REF" ~ "GPP (μmol m-2 s-1)",
    flux_var == "RECO_NT_VUT_REF" ~ "Reco (μmol m-2 s-1)",
    flux_var == "LE_F_MDS" ~ "LE (W m-2)",
    TRUE ~ flux_var
  )
  
  
  y_scale_func <- function(data) {
    if (y_mode == "squish") {
      upper <- quantile(data$mean_flux, probs = 0.95, na.rm = TRUE)
      scale_y_continuous(limits = c(0, upper), oob = scales::squish)
    } else {
      scale_y_continuous()
    }
  }
  
  plot_group <- function(data, group_label) {
    group_data <- data %>% filter(IGBP_group == group_label)
    seasonal_summary <- group_data %>%
      group_by(IGBP, DOY) %>%
      summarize(
        mean_flux = mean(FLUX, na.rm = TRUE),
        se_flux = sd(FLUX, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(
        ci_lower = mean_flux - qt(0.975, df = n() - 1) * se_flux,
        ci_upper = mean_flux + qt(0.975, df = n() - 1) * se_flux
      )
    
    site_stats <- group_data %>%
      filter(!is.na(FLUX)) %>%
      group_by(site, IGBP) %>%
      summarize(n_years = n_distinct(year(date_object)), .groups = "drop")
    
    igbp_labels <- site_stats %>%
      group_by(IGBP) %>%
      summarize(
        n_sites = n_distinct(site),
        site_years = sum(n_years), .groups = "drop"
      ) %>%
      mutate(
        label = paste0(IGBP, ": ", n_sites, " sites, ", site_years, " site-years")
      )
    
    y_max <- max(seasonal_summary$ci_upper, na.rm = TRUE)
    y_min <- min(seasonal_summary$ci_lower, na.rm = TRUE)
    spacing <- (y_max - y_min) * 0.05
    igbp_labels <- igbp_labels %>%
      arrange(desc(IGBP)) %>%
      mutate(
        x = 365,
        y = y_max - (row_number() - 1) * spacing
      )
    
    ggplot(seasonal_summary, aes(x = DOY, y = mean_flux, color = IGBP, fill = IGBP)) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
      geom_line(linewidth = 1.1) +
      y_scale_func(seasonal_summary) +
      geom_text(
        data = igbp_labels,
        aes(x = x, y = y, label = label, color = IGBP),
        hjust = 1.1, vjust = 1, inherit.aes = FALSE, size = 3
      ) +
      labs(
        x = "Day of Year",
        y = y_label,
        title = paste("Seasonal Cycle of", y_label, "for", group_label),
        color = "IGBP",
        fill = "IGBP"
      ) +
      theme_classic(base_size = 14) +
      theme(
        panel.background = element_rect(color = "black"),
        legend.position = "right"
      )
  }
  
  list(
    Forest = plot_group(daily_data, "Forest"),
    ShrubOpens = plot_group(daily_data, "Shrub/Opens"),
    GrassCropsWet = plot_group(daily_data, "Grass/Crops/Wet"),
    Other = plot_group(daily_data, "Other")
  )
}

# -----------------------------
# Annual Climate vs Flux Scatterplots
# -----------------------------

plot_annual_fluxnet_data <- function(annual_data) {
  p1 <- ggplot(annual_data, aes(x = P_ERA, y = NEE_VUT_REF, color = IGBP)) +
    geom_point(alpha = 0.6) +
    labs(x = "Annual Precipitation (mm)", y = "Annual NEE") +
    theme_minimal()
  
  p2 <- ggplot(annual_data, aes(x = TA_ERA, y = GPP_NT_VUT_REF, color = IGBP)) +
    geom_point(alpha = 0.6) +
    labs(x = "Annual Temperature (°C)", y = "Annual GPP") +
    theme_minimal()
  
  return(list(precip_vs_nee = p1, temp_vs_gpp = p2))
}

# -----------------------------
# General XY Scatterplot by IGBP Shape
# -----------------------------
PlotXY_annual <- function(annual_data, x_var, y_var, shape_palette = 0:15) {
  ggplot(annual_data, aes(x = .data[[x_var]], y = .data[[y_var]], shape = IGBP)) +
    geom_point(size = 2.5, stroke = 0.7, color = "black") +
    scale_shape_manual(values = shape_palette) +
    labs(
      x = x_var,
      y = y_var,
      shape = "IGBP"
    ) +
    theme_classic(base_size = 14) +
    theme(
      panel.background = element_rect(color = "black"),
      legend.position = "right"
    )
}


# Adds a top x-axis and right y-axis (no labels), inward ticks, and top+bottom axis add_top_bottom_axis <- function(p) {
# Top/Right axes that mirror primary ticks. Labels off by default.
add_top_right_axes <- function(p, x_breaks = month_breaks,
                               show_top_labels = FALSE,
                               show_right_labels = FALSE) {
  p +
    ggplot2::scale_x_date(
      breaks = x_breaks, date_labels = "%b",
      sec.axis = ggplot2::dup_axis(labels = if (show_top_labels) ggplot2::waiver() else NULL,
                                   name   = NULL)
    ) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::dup_axis(labels = if (show_right_labels) ggplot2::waiver() else NULL,
                                   name   = NULL)
    ) +
    ggplot2::theme(
      # inward ticks on all sides
      axis.ticks.length = grid::unit(-3, "pt"),
      # draw axis lines on all four sides
      axis.line.x.bottom = ggplot2::element_line(),
      axis.line.x.top    = ggplot2::element_line(),
      axis.line.y.left   = ggplot2::element_line(),
      axis.line.y.right  = ggplot2::element_line(),
      # hide duplicate labels unless requested
      axis.text.x.top    = if (show_top_labels) ggplot2::element_text() else ggplot2::element_blank(),
      axis.text.y.right  = if (show_right_labels) ggplot2::element_text() else ggplot2::element_blank(),
      # keep bottom/left labels as-is
      axis.title.x       = ggplot2::element_blank()
    )
}