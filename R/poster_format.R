#' Shared poster-mode constants and helpers for the AGU26 poster figures
#'
#' Parallel to `R/presentation_format.R`, but for Dave's AGU 2026 poster (10
#' December 2026) -- NOT sourced BY `presentation_format.R`/`nature_format.R`
#' and does not alter either of them. Used only when a figure script's own
#' `FLUXNET_POSTER_MODE=1` block runs; the default (unset) path for every
#' figure script is completely unaffected by this file's existence.
#'
#' Design rule (deliberately NOT presentation mode's "same figure, just
#' bigger" rule): the poster drops panels, moves/removes legends in favour of
#' direct labels, and suppresses panel letters/titles/Jaccard values -- see
#' `logs/prompts/2026-11-05_agu26_poster_figures.md`. What IS shared with
#' presentation mode is the *mechanism*: build the exact paper-scale plot
#' object(s) (same data, same `nature_theme()`/`NATURE_BASE_PT`/
#' `NATURE_SMALL_PT` scale the manuscript already uses), then scale every
#' already-set text/line size on it, rather than re-deriving the plot at a
#' large base_size directly (`nature_theme()` itself stops outside 5-7pt, so
#' that path isn't available).
#'
#' [AGU_POSTER_TEXT_SCALE] (4.6) is chosen so that applying it to every text
#' size already used in the manuscript figures clears every minimum the
#' poster task sets, with the smallest paper text (`NATURE_SMALL_PT`, 5pt --
#' the Figure 2 inset's "n=" threshold labels) as the binding constraint:
#'   - `NATURE_SMALL_PT` (5pt) -> 5 * 4.6 = 23pt, clear of the 22pt hard floor.
#'   - `NATURE_BASE_PT` (7pt, axis titles/tick labels/legend text in every
#'     figure this touches) -> 7 * 4.6 = 32.2pt, clear of the 32/26/24pt
#'     axis-title/tick-label/legend-text minimums simultaneously (the paper
#'     uses one uniform base size for all three, so one scale factor clears
#'     all three at once).
#' A figure with its own smaller baked-in text needs a locally larger scale
#' for that one element (checked per figure after rendering --
#' [agu_poster_measure_fontsizes()] measures the built plot directly rather
#' than relying on this derivation alone).
#'
#' [AGU_POSTER_LINE_SCALE] (3) is independent of the text scale -- "lines
#' about three times their paper weight", not the text scale factor -- so
#' [agu_poster_scale_ggplot()] takes separate `text_scale`/`line_scale`
#' arguments and scales each set of elements independently.

AGU_POSTER_FONT              <- "Helvetica"   # base grDevices::pdf() only accepts base-14 PostScript names; Arial is not one
AGU_POSTER_DPI                <- 300
AGU_POSTER_TEXT_SCALE         <- 4.6   # see header derivation
AGU_POSTER_LINE_SCALE         <- 3     # "about three times their paper weight"
AGU_POSTER_FLOOR_PT           <- 22    # nothing anywhere below this
AGU_POSTER_AXIS_TITLE_MIN_PT  <- 32
AGU_POSTER_TICK_MIN_PT        <- 26
AGU_POSTER_LEGEND_MIN_PT      <- 24
AGU_POSTER_GS_BIN             <- Sys.which("gs")   # installed via `brew install ghostscript` for this task -- see SESSION_LOG.md 2026-11-05

#' Scale every fixed text/line size already set on a ggplot object
#'
#' Walks the plot's theme and layers and multiplies any *already-set, fixed*
#' size by `text_scale` (text) or `line_scale` (lines) -- it does not add,
#' remove, or restyle anything, does not touch aesthetics mapped from data,
#' and does not touch legend/colour/breaks/layout. Same walking mechanism as
#' `presentation_format.R::presentation_scale_ggplot()` (a separate,
#' unmodified function in a different file) but with two independent scale
#' factors instead of one, since the poster's text and line scale rules
#' differ (see header).
#'
#' Theme elements scaled: `text`, `axis.text`, `axis.title`, `legend.text`,
#' `legend.title`, `strip.text` (`$size`, by `text_scale`); `axis.line`,
#' `axis.ticks`, `panel.border`, `plot.background` (`$linewidth`, by
#' `line_scale` -- `plot.background` is included because the Figure 2 inset
#' draws its outer frame on `plot.background`, not `panel.border`).
#'
#' Layers scaled: any layer with a fixed (non-mapped) `size` and/or
#' `linewidth` in `aes_params` -- `size` by `text_scale` (covers
#' `geom_text()`/`geom_label()`'s direct-label and annotation sizes),
#' `linewidth` by `line_scale` (covers `geom_line()`/`geom_vline()`/
#' `geom_col()`'s border, etc.).
#'
#' @param plot A single ggplot object (not a composed patchwork object --
#'   scale each panel before composing it, as the manuscript's own
#'   presentation-mode blocks already do).
#' @param text_scale,line_scale Multipliers (default [AGU_POSTER_TEXT_SCALE],
#'   [AGU_POSTER_LINE_SCALE]).
#' @export
agu_poster_scale_ggplot <- function(plot, text_scale = AGU_POSTER_TEXT_SCALE,
                                     line_scale = AGU_POSTER_LINE_SCALE) {
  text_elements <- c("text", "axis.text", "axis.title", "legend.text",
                      "legend.title", "strip.text")
  for (nm in text_elements) {
    el <- plot$theme[[nm]]
    if (!is.null(el) && !is.null(el$size)) {
      el$size <- el$size * text_scale
      plot$theme[[nm]] <- el
    }
  }
  line_elements <- c("axis.line", "axis.ticks", "panel.border", "plot.background")
  for (nm in line_elements) {
    el <- plot$theme[[nm]]
    if (!is.null(el) && !is.null(el$linewidth)) {
      el$linewidth <- el$linewidth * line_scale
      plot$theme[[nm]] <- el
    }
  }
  for (i in seq_along(plot$layers)) {
    ap <- plot$layers[[i]]$aes_params
    if (!is.null(ap$size) && is.numeric(ap$size)) {
      ap$size <- ap$size * text_scale
    }
    if (!is.null(ap$linewidth) && is.numeric(ap$linewidth)) {
      ap$linewidth <- ap$linewidth * line_scale
    }
    plot$layers[[i]]$aes_params <- ap
  }
  plot
}

#' Measure every rendered text size (pt) in a built plot/grob
#'
#' Renders `plot_or_expr` to a grob tree (via [grid::grid.grabExpr()], so it
#' works for a ggplot object, a patchwork composite, or a raw grid gTree such
#' as `figure4_representativeness.R`'s `header_grob()`/`caption_grob()`) and
#' recursively collects every `gpar(fontsize = ...)` found -- grid text
#' fontsize is already in points, the same unit the poster task's minimums
#' are stated in. This is the "measure from the built plot, not by eye"
#' check the AGU26 poster task asks for.
#'
#' @param plot_or_expr A plot object, or a `quote()`d/`{}`-wrapped expression
#'   that draws one (passed to `grid::grid.grabExpr()`).
#' @return Numeric vector of every font size (pt) found, unsorted, possibly
#'   with duplicates (one per text grob).
#' @export
agu_poster_measure_fontsizes <- function(plot_or_expr) {
  grob <- grid::grid.grabExpr(print(plot_or_expr))
  .collect <- function(g) {
    sizes <- numeric(0)
    if (is.null(g)) return(sizes)
    gp <- if (!is.null(g$gp)) g$gp else attr(g, "gp")
    if (!is.null(gp) && !is.null(gp$fontsize)) sizes <- c(sizes, gp$fontsize)
    if (!is.null(g$children))  for (ch in g$children)  sizes <- c(sizes, .collect(ch))
    if (!is.null(g$grobs))      for (ch in g$grobs)      sizes <- c(sizes, .collect(ch))
    sizes
  }
  .collect(grob)
}

#' Embed fonts into a PDF in place, via Ghostscript
#'
#' Base `grDevices::pdf(family = "Helvetica")` references the standard-14
#' PostScript Helvetica by name rather than embedding it -- confirmed true of
#' every existing PDF in this repo (`pdffonts` reports `emb: no` on, e.g.,
#' `review/figures/draft_manuscript_v1/fig_02_cumulative_siteyears_igbp.pdf`).
#' The AGU26 poster task requires embedded fonts (checked with `pdffonts`);
#' Ghostscript's `pdfwrite` device, run as a post-process step, substitutes
#' and embeds Nimbus Sans (a metrically-compatible Helvetica clone bundled
#' with Ghostscript) and subsets it into the file. Ghostscript was installed
#' via `brew install ghostscript` specifically for this task (confirmed by
#' the user) -- see SESSION_LOG.md 2026-11-05 and
#' `logs/prompts/2026-11-05_agu26_poster_figures.md`.
#'
#' @param pdf_path Path to an existing PDF, overwritten in place with the
#'   font-embedded version.
#' @return (Invisibly) `pdf_path`.
#' @export
agu_poster_embed_fonts <- function(pdf_path) {
  if (!nzchar(AGU_POSTER_GS_BIN)) {
    stop("agu_poster_embed_fonts(): Ghostscript ('gs') not found on PATH -- required to embed ",
         "fonts in the poster PDFs. Install via `brew install ghostscript` (done once already ",
         "for this task on this machine -- see SESSION_LOG.md 2026-11-05).")
  }
  tmp_out <- paste0(pdf_path, ".embedded.pdf")
  status <- system2(AGU_POSTER_GS_BIN,
                     c("-dEmbedAllFonts=true", "-dSubsetFonts=true", "-dPDFSETTINGS=/prepress",
                       "-dNOPAUSE", "-dBATCH", "-sDEVICE=pdfwrite",
                       paste0("-sOutputFile=", tmp_out), pdf_path),
                     stdout = FALSE, stderr = FALSE)
  if (status != 0 || !file.exists(tmp_out)) {
    stop("agu_poster_embed_fonts(): Ghostscript font embedding failed for ", pdf_path,
         " (exit status ", status, ").")
  }
  fs::file_move(tmp_out, pdf_path)
  invisible(pdf_path)
}

#' Save a poster figure: 300dpi PNG + vector PDF with embedded fonts
#'
#' Saves at the figure's true print size (inches) -- NOT a box-fit rescale
#' like `presentation_format.R::save_presentation_figure()` -- so point
#' sizes already set on `plot` are the sizes that print. PNG via
#' `ragg::agg_png`; PDF via base `grDevices::pdf(family = "Helvetica")`,
#' then [agu_poster_embed_fonts()] in place.
#'
#' @param plot A ggplot or patchwork object.
#' @param path_stem Output path without extension.
#' @param width_in,height_in Final figure size in inches.
#' @param dpi Default [AGU_POSTER_DPI] (300).
#' @return (Invisibly) a named list of the file paths written.
#' @export
save_agu_poster_figure <- function(plot, path_stem, width_in, height_in, dpi = AGU_POSTER_DPI) {
  fs::dir_create(dirname(path_stem))
  png_path <- paste0(path_stem, ".png")
  pdf_path <- paste0(path_stem, ".pdf")
  ggplot2::ggsave(png_path, plot, width = width_in, height = height_in, units = "in",
                   dpi = dpi, device = ragg::agg_png, bg = "white")
  ggplot2::ggsave(pdf_path, plot, width = width_in, height = height_in, units = "in",
                   device = grDevices::pdf, family = "Helvetica", bg = "white")
  agu_poster_embed_fonts(pdf_path)
  invisible(list(png = png_path, pdf = pdf_path))
}
