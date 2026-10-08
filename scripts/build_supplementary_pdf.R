## build_supplementary_pdf.R
## Assembles SupFigs/supplementary_figures_proof.pdf (figure-proof-only
## follow-up, 2026-10-08, SESSION_LOG.md -- renamed from
## supplementary_information.pdf, which in turn had been renamed from
## supplementary_figures.pdf in the supplementary material restructure,
## also 2026-10-08): one Supplementary Figure per page, each headed by its
## number and a full publication legend (not the internal TITLE line
## alone). No longer includes table pages -- the actual Supplementary
## Information document (figures + Tables S1-S3) is now assembled in Word
## outside this pipeline; this PDF is a figure-order/legend proof only, not
## a submission-ready document. Does not recompute or re-render any figure;
## pure assembly, dependency-free beyond grid/grDevices/png (already used).
##
## Does not call check_pipeline_config() -- no flux data read, no
## credentials, pure file assembly (same convention as
## scripts/build_draft_manuscript_v1.R).

suppressPackageStartupMessages({
  library(grid)
  library(png)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

SUPFIGS_DIR <- file.path("review", "figures", "draft_manuscript_v1", "SupFigs")
OUT_PDF <- file.path(SUPFIGS_DIR, "supplementary_figures_proof.pdf")

## Order fixed by the supplementary material restructure (2026-10-08,
## SESSION_LOG.md; see docs/figure_inventory.md).
STEMS <- c("figS1_whittaker_nee_gpp_reco",
           "figS2_flux_comparison_matched_siteyears",
           "figS3_sampling_gridded_at_tower",
           "figS4_sampling_flux_axes",
           "figS5_sampling_collections",
           "figS6_record_length")

for (s in STEMS) {
  png_path <- file.path(SUPFIGS_DIR, paste0(s, ".png"))
  txt_path <- file.path(SUPFIGS_DIR, paste0(s, ".legend.txt"))
  if (!file.exists(png_path)) stop("Missing PNG: ", png_path)
  if (!file.exists(txt_path)) stop("Missing legend: ", txt_path)
}

## ---- Read each figure's full (possibly multi-line) PUBLICATION LEGEND,
## stopping at the first blank line after "PUBLICATION LEGEND:" -- a
## separate, publication-facing field from the internal TITLE/DESCRIPTION
## provenance block (which stays in the .legend.txt file for internal use,
## per the task's "do not include script paths or internal notes" rule for
## this compiled PDF). The leading "Supplementary Figure SN. " sentence is
## stripped here (not from the source file, which reads standalone) since
## the page already carries that as its own bold running header.
read_publication_legend <- function(legend_path) {
  lines <- readLines(legend_path, warn = FALSE)
  start <- which(lines == "PUBLICATION LEGEND:")[1]
  if (is.na(start)) stop("No PUBLICATION LEGEND: section found in ", legend_path)
  out <- character(0)
  i <- start + 1L
  while (i <= length(lines) && nzchar(trimws(lines[i]))) {
    out <- c(out, trimws(lines[i]))
    i <- i + 1L
  }
  text <- paste(out, collapse = " ")
  sub("^Supplementary Figure S[0-9]+\\.\\s*", "", text)
}

n_words <- function(text) length(strsplit(trimws(text), "\\s+")[[1]])

## ---- Downsample factor: source PNGs are 600 dpi (Nature format); embedding
## at full resolution in a multi-page PDF would bloat file size well past the
## 10 MB task limit for no on-screen/review benefit. Target ~200 dpi (ample
## for reading figures and legends on screen or a working printout) by
## simple nearest-neighbour index subsampling -- no image-resizing package
## needed. The authoritative, full 600 dpi PNG/PDF for each figure remains
## in SupFigs/ unchanged; this bundle is a reading copy, not a replacement.
TARGET_DPI <- 200
SOURCE_DPI <- 600

downsample <- function(img, factor) {
  if (factor <= 1) return(img)
  d <- dim(img)
  rows <- seq(1, d[1], by = factor)
  cols <- seq(1, d[2], by = factor)
  if (length(d) == 3) img[rows, cols, , drop = FALSE] else img[rows, cols, drop = FALSE]
}

## ---- Page: fixed A4 portrait for every figure regardless of its own native
## mm size -- simplest way to get one-figure-per-page without irregular page
## sizes; image is scaled to fit within the page margins, aspect ratio
## preserved, and centred under the title block.
PAGE_W_MM <- 210
PAGE_H_MM <- 297
MARGIN_MM <- 12
## Raised from 34 to 55mm (vs the prior single-line-TITLE version of this
## script): a full publication legend (several sentences, up to 350 words)
## needs more room than a one-line figure title did.
TITLE_BLOCK_MM <- 55

grDevices::pdf(OUT_PDF, width = PAGE_W_MM / 25.4, height = PAGE_H_MM / 25.4,
                onefile = TRUE, title = "FLUXNET Annual Paper 2026 -- Supplementary Figures (proof)",
                family = "Helvetica")

for (i in seq_along(STEMS)) {
  s <- STEMS[i]
  png_path <- file.path(SUPFIGS_DIR, paste0(s, ".png"))
  txt_path <- file.path(SUPFIGS_DIR, paste0(s, ".legend.txt"))
  ## Base grDevices::pdf()'s PostScript Helvetica has no em-dash glyph (same
  ## issue as scripts/figure4_representativeness.R's panel titles) -- use a
  ## plain hyphen directly rather than let R silently substitute one in.
  legend_text <- gsub("—", "-", read_publication_legend(txt_path))
  wc <- n_words(legend_text)
  if (wc > 350) stop(s, ": publication legend is ", wc, " words, exceeds the 350-word limit.")
  num_label <- paste0("Supplementary Figure S", i)

  img <- png::readPNG(png_path)
  d <- dim(img)
  img_w_mm <- d[2] / SOURCE_DPI * 25.4
  img_h_mm <- d[1] / SOURCE_DPI * 25.4
  factor <- max(1L, round(SOURCE_DPI / TARGET_DPI))
  img_small <- downsample(img, factor)

  avail_w_mm <- PAGE_W_MM - 2 * MARGIN_MM
  avail_h_mm <- PAGE_H_MM - 2 * MARGIN_MM - TITLE_BLOCK_MM
  scale <- min(avail_w_mm / img_w_mm, avail_h_mm / img_h_mm, 1)
  draw_w_mm <- img_w_mm * scale
  draw_h_mm <- img_h_mm * scale

  grid::grid.newpage()
  ## Title block: number (bold) then publication legend, wrapped, top-left
  ## of margin.
  grid::pushViewport(grid::viewport(
    x = unit(MARGIN_MM, "mm"), y = unit(PAGE_H_MM - MARGIN_MM, "mm"),
    width = unit(avail_w_mm, "mm"), height = unit(TITLE_BLOCK_MM, "mm"),
    just = c("left", "top")
  ))
  grid::grid.text(num_label, x = 0, y = 1, just = c("left", "top"),
                   gp = grid::gpar(fontface = "bold", fontsize = 12, fontfamily = "Helvetica"))
  ## Word-wrap the legend to the available page width -- grid.text() does
  ## not auto-wrap, and an unwrapped long paragraph runs off the right
  ## margin. ~100 characters/line is conservative for 9pt Helvetica at this
  ## width (186mm avail_w_mm / ~1.6mm per character).
  wrapped_legend <- paste(strwrap(legend_text, width = 100), collapse = "\n")
  grid::grid.text(wrapped_legend, x = 0, y = 0.85, just = c("left", "top"),
                   gp = grid::gpar(fontsize = 9, fontfamily = "Helvetica"),
                   check.overlap = TRUE)
  grid::popViewport()

  ## Figure image, centred in the remaining page area below the title block
  grid::pushViewport(grid::viewport(
    x = unit(PAGE_W_MM / 2, "mm"),
    y = unit((PAGE_H_MM - MARGIN_MM - TITLE_BLOCK_MM) / 2 + MARGIN_MM, "mm"),
    width = unit(draw_w_mm, "mm"), height = unit(draw_h_mm, "mm"),
    just = "center"
  ))
  grid::grid.raster(img_small, interpolate = TRUE)
  grid::popViewport()

  msg("Page ", i, ": ", s, " (", num_label, "), ", wc, " words, downsample 1/", factor,
      " (", round(SOURCE_DPI / factor), " dpi effective)")
}

grDevices::dev.off()

size_mb <- file.info(OUT_PDF)$size / 1e6
msg("Saved: ", OUT_PDF, " (", round(size_mb, 2), " MB, ", length(STEMS), " pages, figures only -- ",
    "tables are no longer included; the Supplementary Information document is assembled in Word).")
if (size_mb > 10) {
  stop("supplementary_figures_proof.pdf is ", round(size_mb, 2),
       " MB -- exceeds the 10 MB task limit. Reduce TARGET_DPI and re-run.")
}
msg("=== Done ===")
