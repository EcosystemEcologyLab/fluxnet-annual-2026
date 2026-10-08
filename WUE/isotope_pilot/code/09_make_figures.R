## 09_make_figures.R — Stage 2 figures, from tables/wue_annual.csv.
## One figure per yearly metric (WUE, IWUE, uWUE) vs. year, one panel per
## site; one figure of valid days per site-year. Repo figure convention
## (CLAUDE.md): white (opaque) background on every ggsave() call.
##
## Output (figures/, git-tracked):
##   fig_wue_annual.png
##   fig_iwue_annual.png
##   fig_uwue_annual.png
##   fig_valid_days.png

source("WUE/isotope_pilot/code/00_config.R")
library(ggplot2)

tables_dir  <- file.path(WUE_ROOT, "tables")
figures_dir <- file.path(WUE_ROOT, "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

wue_annual <- readr::read_csv(file.path(tables_dir, "wue_annual.csv"), show_col_types = FALSE)

base_theme <- ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey90", color = NA)
  )

metric_figure <- function(metric_col, ylab, filename) {
  p <- ggplot2::ggplot(wue_annual, ggplot2::aes(x = year, y = .data[[metric_col]])) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~site, scales = "free_y") +
    ggplot2::labs(x = "Year", y = ylab) +
    base_theme
  ggplot2::ggsave(file.path(figures_dir, filename), plot = p, width = 11, height = 8,
                   units = "in", dpi = 300, bg = "white")
  message("[WUE] Figure written: ", filename)
}

metric_figure("WUE_y",  "WUE (g C / kg H2O)",                 "fig_wue_annual.png")
metric_figure("IWUE_y", "IWUE (g C hPa / kg H2O)",            "fig_iwue_annual.png")
metric_figure("uWUE_y", "uWUE (g C hPa^0.5 / kg H2O)", "fig_uwue_annual.png")

p_valid <- ggplot2::ggplot(wue_annual, ggplot2::aes(x = year, y = valid_days)) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(~site) +
  ggplot2::labs(x = "Year", y = "Valid days") +
  base_theme
ggplot2::ggsave(file.path(figures_dir, "fig_valid_days.png"), plot = p_valid,
                 width = 11, height = 8, units = "in", dpi = 300, bg = "white")
message("[WUE] Figure written: fig_valid_days.png")

message("[WUE] 09_make_figures.R complete.")
