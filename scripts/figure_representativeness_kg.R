## figure_representativeness_kg.R
## Representativeness figure — Köppen-Geiger axis.
## Two stacked bar charts comparing the area-weighted global KG land
## distribution (Beck 2023, 1991-2020, 1 km) against the 767-site
## FLUXNET network distribution.
##
## Outputs (review/figures/representativeness/):
##   fig_representativeness_kg_30class.png  — 30 KG classes
##   fig_representativeness_kg_5class.png   — 5 main classes + sampling-ratio panel (right)
##   fig_representativeness_kg_twoletter.png — 13 two-letter classes + ratio panel (below)
##
## Color scheme: Beck 2023 published RGB values from legend.txt.
## 5-class representatives: A=Aw, B=BSh, C=Cfa, D=Dfb, E=ET (per user spec).
## Two-letter colors: mean RGB of all 30-class members within each two-letter group
##   (e.g., Cf color = mean(Cfa, Cfb, Cfc) RGB). Equal-weight mean, documented below.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")

library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)

# ---- Paths ------------------------------------------------------------------
kg_dir     <- file.path("data", "external", "koppen_beck2023")
leg_path   <- file.path(kg_dir, "legend.txt")
global_path <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "koppen_beck2023_global_distribution.csv")
sites_path  <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "site_koppen_beck2023.csv")
out_dir     <- file.path("review", "figures", "representativeness")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Parse legend (class codes, names, RGB colors) --------------------------
leg_lines  <- readLines(leg_path)
leg_data   <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]

legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
  m <- regmatches(ln, regexec(
    "^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[(\\d+)\\s+(\\d+)\\s+(\\d+)\\]",
    ln, perl = TRUE
  ))[[1]]
  if (length(m) < 7L) return(NULL)
  data.frame(
    koppen_class_code = as.integer(m[2]),
    koppen_class      = m[3],
    koppen_class_name = trimws(m[4]),
    r = as.integer(m[5]),
    g = as.integer(m[6]),
    b = as.integer(m[7]),
    stringsAsFactors  = FALSE
  )
}))

main_map <- c(A = "Tropical", B = "Arid", C = "Temperate", D = "Cold", E = "Polar")
legend_df <- legend_df |>
  dplyr::mutate(
    color_hex    = grDevices::rgb(r, g, b, maxColorValue = 255),
    koppen_main  = substr(koppen_class, 1L, 1L),
    koppen_main_name = main_map[koppen_main]
  )

# Canonical class order: A → B → C → D → E (legend order = code order)
class_order <- legend_df$koppen_class   # 30 classes, A at bottom when stacked
color_30    <- setNames(legend_df$color_hex, legend_df$koppen_class)

# 5-class representative colors (user spec): Aw, BSh, Cfa, Dfb, ET
main_reps <- c(A = "Aw", B = "BSh", C = "Cfa", D = "Dfb", E = "ET")
color_5   <- setNames(
  legend_df$color_hex[match(main_reps, legend_df$koppen_class)],
  names(main_reps)
)
main_order <- c("A", "B", "C", "D", "E")

# ---- Load data --------------------------------------------------------------
global_df <- readr::read_csv(global_path, show_col_types = FALSE)
sites_df  <- readr::read_csv(sites_path,  show_col_types = FALSE)
n_sites   <- nrow(sites_df)

# ---- Representativeness metrics (Jaccard + Hellinger) -----------------------
# Computed here for all aggregation levels; used as figure annotations and CSV.

compute_repr_metrics <- function(p, q) {
  # p, q: aligned numeric fraction vectors (same length, same class order)
  p[is.na(p)] <- 0
  q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

# Helper: build q (site fraction) vector aligned to a given class-order vector
site_fracs <- function(col_name, levels_vec, df = sites_df) {
  counts <- table(df[[col_name]])
  vapply(levels_vec, function(lv) {
    n <- counts[lv]
    if (is.na(n)) 0 else as.numeric(n) / n_sites
  }, numeric(1L))
}

# 30-class
p_30 <- global_df$global_land_fraction[match(class_order, global_df$koppen_class)]
p_30[is.na(p_30)] <- 0
q_30 <- site_fracs("koppen_class", class_order)
m30  <- compute_repr_metrics(p_30, q_30)

# 5-class
p_5 <- vapply(main_order, function(m)
  sum(global_df$global_land_fraction[global_df$koppen_main == m], na.rm = TRUE),
  numeric(1L))
q_5  <- site_fracs("koppen_main", main_order)
m5   <- compute_repr_metrics(p_5, q_5)

message(sprintf("Metrics — 30-class: J = %.4f, H = %.4f",
                m30$weighted_jaccard, m30$hellinger_distance))
message(sprintf("Metrics —  5-class: J = %.4f, H = %.4f",
                m5$weighted_jaccard,  m5$hellinger_distance))

# ---- Build plot data frames -------------------------------------------------

# 30-class long format
site_30 <- sites_df |>
  dplyr::count(koppen_class, name = "n") |>
  dplyr::mutate(fraction = n / n_sites)

plot30 <- dplyr::tibble(koppen_class = class_order) |>
  dplyr::left_join(
    dplyr::select(global_df, koppen_class, global = global_land_fraction),
    by = "koppen_class"
  ) |>
  dplyr::left_join(
    dplyr::select(site_30, koppen_class, network = fraction),
    by = "koppen_class"
  ) |>
  dplyr::mutate(
    global  = dplyr::coalesce(global,  0),
    network = dplyr::coalesce(network, 0)
  ) |>
  tidyr::pivot_longer(
    cols      = c(global, network),
    names_to  = "bar",
    values_to = "fraction"
  ) |>
  dplyr::mutate(
    bar          = factor(bar,
                          levels = c("global", "network"),
                          labels = c("Global land", "FLUXNET\n(767 sites)")),
    koppen_class = factor(koppen_class, levels = class_order),
    label        = dplyr::if_else(fraction >= 0.015, koppen_class, NA_character_)
  )

# 5-class long format
site_5 <- sites_df |>
  dplyr::count(koppen_main, name = "n") |>
  dplyr::mutate(fraction = n / n_sites)

global_5 <- global_df |>
  dplyr::group_by(koppen_main) |>
  dplyr::summarise(global_land_fraction = sum(global_land_fraction), .groups = "drop")

plot5 <- dplyr::tibble(koppen_main = main_order) |>
  dplyr::left_join(
    dplyr::select(global_5, koppen_main, global = global_land_fraction),
    by = "koppen_main"
  ) |>
  dplyr::left_join(
    dplyr::select(site_5, koppen_main, network = fraction),
    by = "koppen_main"
  ) |>
  dplyr::mutate(
    global  = dplyr::coalesce(global,  0),
    network = dplyr::coalesce(network, 0)
  ) |>
  tidyr::pivot_longer(
    cols      = c(global, network),
    names_to  = "bar",
    values_to = "fraction"
  ) |>
  dplyr::mutate(
    bar         = factor(bar,
                         levels = c("global", "network"),
                         labels = c("Global land", "FLUXNET\n(767 sites)")),
    koppen_main = factor(koppen_main, levels = main_order),
    # Two-line label for segments >= 8%; single-line for 3–8%; letter only below 3%
    label = dplyr::case_when(
      fraction >= 0.08 ~ sprintf("%s\n%s%%", koppen_main,
                                 formatC(round(fraction * 100, 1), format = "f", digits = 1)),
      fraction >= 0.03 ~ sprintf("%s  %s%%", koppen_main,
                                 formatC(round(fraction * 100, 1), format = "f", digits = 1)),
      TRUE             ~ koppen_main
    )
  )

# Sampling ratio data (for 5-class right panel)
ratio_df <- dplyr::tibble(koppen_main = main_order) |>
  dplyr::left_join(
    dplyr::rename(global_5, global_frac = global_land_fraction),
    by = "koppen_main"
  ) |>
  dplyr::left_join(
    dplyr::rename(site_5, network_frac = fraction),
    by = "koppen_main"
  ) |>
  dplyr::mutate(
    network_frac  = dplyr::coalesce(network_frac, 0),
    sampling_ratio = network_frac / global_frac,
    koppen_main   = factor(koppen_main, levels = rev(main_order)),
    label         = sprintf("%.2f×", sampling_ratio)
  )

# ---- Common theme elements --------------------------------------------------
base_theme <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "right",
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA)
  )

# ---- Figure 1: 30-class stacked bar -----------------------------------------
# Compact legend: 2 columns, 15 rows — too long. Use guide_legend ncol.
# Classes with fraction < 0.005 in BOTH bars are placed last alphabetically
# and may be hard to distinguish; still included for completeness.

p30 <- ggplot(plot30, aes(x = bar, y = fraction, fill = koppen_class)) +
  geom_bar(stat = "identity", width = 0.6, colour = "white", linewidth = 0.15) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size     = 2.3,
    family   = "sans",
    colour   = "black",
    na.rm    = TRUE
  ) +
  scale_fill_manual(
    values = color_30,
    breaks = class_order,
    name   = NULL,
    guide  = guide_legend(ncol = 2, reverse = FALSE,
                          override.aes = list(colour = NA))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01)),
    labels = scales::percent_format(accuracy = 1),
    name   = "Fraction of total"
  ) +
  scale_x_discrete(name = NULL) +
  annotate(
    "text",
    x      = Inf, y = Inf,
    label  = sprintf("J = %.2f\nH = %.2f",
                     m30$weighted_jaccard, m30$hellinger_distance),
    hjust  = 1.08, vjust = 1.5,
    size   = 2.9, family = "sans", colour = "grey25", lineheight = 1.2
  ) +
  base_theme +
  theme(
    legend.key.size  = unit(0.35, "cm"),
    legend.text      = element_text(size = 7, family = "sans"),
    legend.spacing.y = unit(0.1, "cm")
  )

# ---- Figure 2: 5-class stacked bar + sampling-ratio panel -------------------

p5_bars <- ggplot(plot5, aes(x = bar, y = fraction, fill = koppen_main)) +
  geom_bar(stat = "identity", width = 0.55, colour = "white", linewidth = 0.4) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size     = 3.0,
    family   = "sans",
    colour   = "black",
    lineheight = 0.9
  ) +
  scale_fill_manual(
    values = color_5,
    breaks = main_order,
    labels = c(
      A = "A — Tropical",
      B = "B — Arid",
      C = "C — Temperate",
      D = "D — Cold",
      E = "E — Polar"
    ),
    name  = NULL,
    guide = guide_legend(ncol = 1, reverse = FALSE,
                         override.aes = list(colour = NA))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01)),
    labels = scales::percent_format(accuracy = 1),
    name   = "Fraction of total"
  ) +
  scale_x_discrete(name = NULL) +
  annotate(
    "text",
    x      = Inf, y = Inf,
    label  = sprintf("J = %.2f\nH = %.2f",
                     m5$weighted_jaccard, m5$hellinger_distance),
    hjust  = 1.08, vjust = 1.5,
    size   = 2.9, family = "sans", colour = "grey25", lineheight = 1.2
  ) +
  base_theme +
  theme(
    legend.key.size = unit(0.45, "cm"),
    legend.text     = element_text(size = 8.5)
  )

p5_ratio <- ggplot(ratio_df,
                   aes(x = sampling_ratio, y = koppen_main, colour = koppen_main)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_segment(aes(xend = 1, yend = koppen_main),
               colour = "grey75", linewidth = 0.5) +
  geom_point(size = 3.5) +
  geom_text(
    aes(label = label),
    hjust  = -0.25,
    size   = 2.9,
    family = "sans",
    colour = "black"
  ) +
  scale_colour_manual(values = color_5, guide = "none") +
  scale_x_continuous(
    name   = "Sampling ratio\n(network / global)",
    limits = c(0.1, 7),
    breaks = c(0.25, 0.5, 1, 2, 4),
    trans  = "log2",
    labels = c("0.25×", "0.5×", "1×", "2×", "4×")
  ) +
  scale_y_discrete(name = NULL, labels = main_map[rev(main_order)]) +
  base_theme +
  theme(
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 8.5),
    axis.title.x       = element_text(size = 8)
  )

p5 <- p5_bars + p5_ratio +
  plot_layout(widths = c(3, 2), guides = "keep") &
  theme(plot.background = element_rect(fill = "white", colour = NA))

# ---- Figure 3: two-letter stacked bar + sampling-ratio panel beneath --------

# Two-letter class ordering: A (Af, Am, Aw), B (BS, BW), C (Cf, Cs, Cw),
# D (Df, Ds, Dw), E (EF, ET) — alphabetical within main class.
tl_order <- c("Af", "Am", "Aw", "BS", "BW", "Cf", "Cs", "Cw",
               "Df", "Ds", "Dw", "EF", "ET")

# Two-letter colors: mean RGB of all 30-class members in each group.
# EF and ET are singletons; the rest are means across 2–4 members.
color_tl <- setNames(
  vapply(tl_order, function(tl) {
    members <- legend_df[substr(legend_df$koppen_class, 1L, 2L) == tl, ]
    grDevices::rgb(
      mean(members$r), mean(members$g), mean(members$b),
      maxColorValue = 255
    )
  }, character(1L)),
  tl_order
)

# Global two-letter fractions
global_tl <- global_df |>
  dplyr::group_by(koppen_twoletter) |>
  dplyr::summarise(global_frac = sum(global_land_fraction), .groups = "drop") |>
  dplyr::rename(koppen_twoletter_code = koppen_twoletter)

# Site two-letter fractions
site_tl <- sites_df |>
  dplyr::count(koppen_twoletter, name = "n") |>
  dplyr::mutate(network_frac = n / n_sites) |>
  dplyr::rename(koppen_twoletter_code = koppen_twoletter, n_tl_sites = n)

# Full join to include EF (no sites) and any class present in only one source
all_tl <- dplyr::tibble(koppen_twoletter_code = tl_order) |>
  dplyr::left_join(global_tl, by = "koppen_twoletter_code") |>
  dplyr::left_join(site_tl,   by = "koppen_twoletter_code") |>
  dplyr::mutate(
    global_frac  = dplyr::coalesce(global_frac,   0),
    n_tl_sites   = dplyr::coalesce(n_tl_sites,    0L),
    network_frac = dplyr::coalesce(network_frac,   0),
    sampling_ratio = dplyr::if_else(
      global_frac > 0, network_frac / global_frac, NA_real_
    ),
    # Flag: < 5 network sites → unstable ratio
    flag_unstable = n_tl_sites < 5L
  )

# Two-letter metrics (computed here, after all_tl is available)
p_tl <- all_tl$global_frac
q_tl <- all_tl$network_frac
m_tl <- compute_repr_metrics(p_tl, q_tl)
message(sprintf("Metrics — two-letter: J = %.4f, H = %.4f",
                m_tl$weighted_jaccard, m_tl$hellinger_distance))

# ---- Write metrics CSV -------------------------------------------------------
metrics_df <- data.frame(
  aggregation_level  = c("5class", "13class_twoletter", "30class"),
  weighted_jaccard   = c(m5$weighted_jaccard,  m_tl$weighted_jaccard,  m30$weighted_jaccard),
  hellinger_distance = c(m5$hellinger_distance, m_tl$hellinger_distance, m30$hellinger_distance),
  stringsAsFactors   = FALSE
)
metrics_path <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "representativeness_metrics.csv")
readr::write_csv(metrics_df, metrics_path)
message("Saved metrics: ", metrics_path)

message("\n=== TWO-LETTER CLASS SUMMARY ===")
print(as.data.frame(dplyr::select(
  all_tl, koppen_twoletter_code, global_frac, n_tl_sites, sampling_ratio, flag_unstable
)))
cat("Unstable (< 5 sites):", all_tl$koppen_twoletter_code[all_tl$flag_unstable], "\n")
cat("Top over-sampled:   ",
    all_tl$koppen_twoletter_code[which.max(all_tl$sampling_ratio)],
    sprintf("(%.2f×)\n", max(all_tl$sampling_ratio, na.rm = TRUE)))
cat("Top under-sampled:  ",
    all_tl$koppen_twoletter_code[which.min(replace(all_tl$sampling_ratio, is.na(all_tl$sampling_ratio), Inf))],
    sprintf("(%.2f×)\n", min(all_tl$sampling_ratio, na.rm = TRUE)))

# Long format for stacked bars
plotTL <- all_tl |>
  tidyr::pivot_longer(
    cols      = c(global_frac, network_frac),
    names_to  = "bar",
    values_to = "fraction"
  ) |>
  dplyr::mutate(
    bar = factor(bar,
                 levels = c("global_frac", "network_frac"),
                 labels = c("Global land", "FLUXNET\n(767 sites)")),
    koppen_twoletter_code = factor(koppen_twoletter_code, levels = tl_order),
    label = dplyr::case_when(
      fraction >= 0.07 ~ sprintf("%s\n%s%%", koppen_twoletter_code,
                                 formatC(round(fraction * 100, 1), format = "f", digits = 1)),
      fraction >= 0.025 ~ sprintf("%s %s%%", koppen_twoletter_code,
                                  formatC(round(fraction * 100, 1), format = "f", digits = 1)),
      fraction > 0      ~ as.character(koppen_twoletter_code),
      TRUE              ~ NA_character_
    )
  )

# Stacked bars
pTL_bars <- ggplot(plotTL, aes(x = bar, y = fraction, fill = koppen_twoletter_code)) +
  geom_bar(stat = "identity", width = 0.55, colour = "white", linewidth = 0.3) +
  geom_text(
    aes(label = label),
    position   = position_stack(vjust = 0.5),
    size       = 2.6,
    family     = "sans",
    colour     = "black",
    lineheight = 0.9,
    na.rm      = TRUE
  ) +
  scale_fill_manual(
    values = color_tl,
    breaks = tl_order,
    name   = NULL,
    guide  = guide_legend(ncol = 2, reverse = FALSE,
                          override.aes = list(colour = NA))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01)),
    labels = scales::percent_format(accuracy = 1),
    name   = "Fraction of total"
  ) +
  scale_x_discrete(name = NULL) +
  annotate(
    "text",
    x      = Inf, y = Inf,
    label  = sprintf("J = %.2f\nH = %.2f",
                     m_tl$weighted_jaccard, m_tl$hellinger_distance),
    hjust  = 1.08, vjust = 1.5,
    size   = 2.9, family = "sans", colour = "grey25", lineheight = 1.2
  ) +
  base_theme +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.text     = element_text(size = 8)
  )

# Sampling-ratio panel beneath — dot plot, one dot per two-letter class
ratio_tl <- all_tl |>
  dplyr::mutate(
    koppen_twoletter_code = factor(koppen_twoletter_code, levels = tl_order),
    # EF: sampling_ratio is NA (0 sites); treat as 0 for display, flag with shape
    ratio_plot = dplyr::coalesce(sampling_ratio, 0),
    dot_shape  = dplyr::if_else(flag_unstable, 4L, 19L),   # × for unstable, ● for stable
    dot_alpha  = dplyr::if_else(flag_unstable, 0.45, 1.0),
    ratio_label = dplyr::case_when(
      flag_unstable       ~ sprintf("%.2f×\n(n=%d)", ratio_plot, n_tl_sites),
      !is.na(ratio_plot)  ~ sprintf("%.2f×", sampling_ratio),
      TRUE                ~ NA_character_
    )
  )

pTL_ratio <- ggplot(ratio_tl,
                    aes(x = koppen_twoletter_code, y = ratio_plot,
                        colour = koppen_twoletter_code,
                        shape  = factor(dot_shape),
                        alpha  = dot_alpha)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_segment(aes(xend = koppen_twoletter_code, yend = 1),
               colour = "grey75", linewidth = 0.5, alpha = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = ratio_label),
    vjust    = -0.65,
    size     = 2.4,
    family   = "sans",
    colour   = "black",
    lineheight = 0.85,
    na.rm    = TRUE
  ) +
  scale_colour_manual(values = color_tl, guide = "none") +
  scale_shape_manual(
    values = c("4" = 4, "19" = 19),
    guide  = guide_legend(
      title      = NULL,
      override.aes = list(colour = "grey40", alpha = c(1, 0.45)),
      label.theme  = element_text(size = 7, family = "sans")
    ),
    labels = c("4" = "Stable (≥ 5 sites)", "19" = "Unstable (< 5 sites)")
  ) +
  scale_alpha_identity() +
  scale_y_continuous(
    name   = "Sampling ratio\n(network / global)",
    trans  = "log2",
    breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
    labels = c("0.125×", "0.25×", "0.5×", "1×", "2×", "4×", "8×"),
    limits = c(0.08, 12)
  ) +
  scale_x_discrete(name = NULL) +
  base_theme +
  theme(
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 8),
    axis.title.y       = element_text(size = 8),
    legend.position    = "right",
    legend.text        = element_text(size = 7)
  )

pTL <- pTL_bars / pTL_ratio +
  plot_layout(heights = c(3, 1.8), guides = "keep") &
  theme(plot.background = element_rect(fill = "white", colour = NA))

# ---- Save -------------------------------------------------------------------
out_30 <- file.path(out_dir, "fig_representativeness_kg_30class.png")
out_5  <- file.path(out_dir, "fig_representativeness_kg_5class.png")
out_tl <- file.path(out_dir, "fig_representativeness_kg_twoletter.png")

ggsave(out_30, plot = p30,  width = 6.5, height = 8,   dpi = 200, bg = "white")
ggsave(out_5,  plot = p5,   width = 7.5, height = 4,   dpi = 200, bg = "white")
ggsave(out_tl, plot = pTL,  width = 6.5, height = 6.5, dpi = 200, bg = "white")

message("Saved:\n  ", out_30, "\n  ", out_5, "\n  ", out_tl)
