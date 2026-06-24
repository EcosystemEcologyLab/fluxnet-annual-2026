## figure_representativeness_kg.R
## Representativeness figure — Köppen-Geiger axis.
## Two stacked bar charts comparing the area-weighted global KG land
## distribution (Beck 2023, 1991-2020, 1 km) against the 767-site
## FLUXNET network distribution.
##
## Outputs (review/figures/representativeness/):
##   fig_representativeness_kg_30class.png  — 30 KG classes
##   fig_representativeness_kg_5class.png   — 5 main classes + sampling-ratio panel
##
## Color scheme: Beck 2023 published RGB values from legend.txt.
## 5-class representatives: A=Aw, B=BSh, C=Cfa, D=Dfb, E=ET (per user spec).

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

# ---- Save -------------------------------------------------------------------
out_30 <- file.path(out_dir, "fig_representativeness_kg_30class.png")
out_5  <- file.path(out_dir, "fig_representativeness_kg_5class.png")

ggsave(out_30, plot = p30, width = 6.5, height = 8, dpi = 200, bg = "white")
ggsave(out_5,  plot = p5,  width = 7.5, height = 4, dpi = 200, bg = "white")

message("Saved:\n  ", out_30, "\n  ", out_5)
