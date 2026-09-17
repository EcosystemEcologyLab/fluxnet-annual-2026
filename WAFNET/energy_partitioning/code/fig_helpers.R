## fig_helpers.R — shared helpers for the WAFNET figure pack.
## Sourced by every 0X_figureN_*.R script (which have already sourced
## 00_config.R). Not run directly.

library(ggplot2)

WAFNET_SITE_ORDER <- c("GH-Ank", "BJ-Bfg", "BJ-Nhu", "BJ-Db1", "SN-Nkr", "SN-Dhr")
WAFNET_SITE_COLORS <- c(
  "GH-Ank" = "#1b7837", "BJ-Bfg" = "#5aae61", "BJ-Nhu" = "#a6dba0",
  "BJ-Db1" = "#d9f0d3", "SN-Nkr" = "#e08214", "SN-Dhr" = "#8c510a"
)

theme_wafnet <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = NA),
      plot.title = element_text(face = "bold", size = base_size),
      legend.position = "bottom"
    )
}

## A registry of {id, caption} pairs accumulated across figure scripts, so
## the final assembly step (11_assemble_figure_pack.R) can build the combined
## PDF without re-deriving captions. Persisted so it survives re-running
## individual figure scripts out of order.
FIG_REGISTRY_PATH <- file.path(WAFNET_ROOT, "data", "processed", "fig_registry.rds")

register_figure <- function(id, caption) {
  reg <- if (file.exists(FIG_REGISTRY_PATH)) readRDS(FIG_REGISTRY_PATH) else list()
  reg[[id]] <- list(id = id, caption = caption)
  saveRDS(reg, FIG_REGISTRY_PATH)
  invisible(reg)
}

#' Save a ggplot (or patchwork) figure as vector PDF + 300 dpi PNG, and
#' register its caption for the combined figure-pack PDF.
#'
#' @param plot ggplot/patchwork object
#' @param id figure id, e.g. "figure1a_wilson_fixed_window"
#' @param caption character vector; each element is one paragraph
#' @param width,height inches
save_figure <- function(plot, id, caption, width = 9, height = 6.5) {
  fig_dir <- file.path(WAFNET_ROOT, "figures")
  pdf_path <- file.path(fig_dir, paste0(id, ".pdf"))
  png_path <- file.path(fig_dir, paste0(id, ".png"))
  # NOTE (2026-09-08): grDevices::cairo_pdf silently writes NO file at all on
  # this machine -- the system cairo/X11 libraries it dlopen()s
  # (libSM/libXrender under /opt/X11/lib) are missing, so ggsave() emits only
  # a "failed to load cairo DLL" warning and produces nothing, while the
  # script otherwise continues as if it had succeeded. Found while building
  # Figure 2, when figures/*.pdf did not exist despite a "Saved figure"
  # message. Using the base pdf() device instead -- it always produces a
  # real file here, but only supports 8-bit (Latin-1-range) text, so plot
  # titles/labels must stick to ASCII (or use plotmath expression() for
  # anything needing superscripts/special symbols, which renders fine
  # either way since it isn't subject to this text-encoding limitation).
  ggplot2::ggsave(pdf_path, plot = plot, width = width, height = height,
                   device = grDevices::pdf, bg = "white")
  ggplot2::ggsave(png_path, plot = plot, width = width, height = height,
                   dpi = 300, bg = "white")
  register_figure(id, caption)
  message("[WAFNET] Saved figure: ", id, " (", pdf_path, ", ", png_path, ")")
  invisible(list(pdf = pdf_path, png = png_path))
}
