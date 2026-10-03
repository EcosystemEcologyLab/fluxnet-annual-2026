## build_supplementary_pdf.R
## Figure stage 6b (logs/figstage_prompt.md, 2026-10-02): assembles
## SupFigs/supplementary_figures.pdf, one Supplementary Figure per page,
## each headed by its number and legend title -- from the already-rendered,
## already-numbered figS1-figS5 PNGs and their .legend.txt TITLE lines (see
## figure stage 6a / docs/figure_inventory.md). Does not recompute or
## re-render any figure; pure assembly, dependency-free (base grid/grDevices
## + the already-used png package -- no new package added, per CLAUDE.md
## "Package preferences").
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
OUT_PDF <- file.path(SUPFIGS_DIR, "supplementary_figures.pdf")

## Order fixed by figure stage 6a's numbering (docs/figure_inventory.md).
STEMS <- c("figS1_whittaker_nee_gpp_ter",
           "figS2_flux_comparison_matched_siteyears",
           "figS3_flux_comparison_six_panel",
           "figS4_representativeness_geo_vs_geo",
           "figS5_flux_representativeness")

for (s in STEMS) {
  png_path <- file.path(SUPFIGS_DIR, paste0(s, ".png"))
  txt_path <- file.path(SUPFIGS_DIR, paste0(s, ".legend.txt"))
  if (!file.exists(png_path)) stop("Missing PNG: ", png_path)
  if (!file.exists(txt_path)) stop("Missing legend: ", txt_path)
}

## ---- Read each figure's full (possibly multi-line) TITLE, stopping at the
## first blank line after "TITLE:" -- matches the legend.txt convention used
## throughout this repo (TITLE: <line 1>, optionally continued on following
## non-blank lines, then a blank line before DESCRIPTION:).
read_title <- function(legend_path) {
  lines <- readLines(legend_path, warn = FALSE)
  start <- which(startsWith(lines, "TITLE:"))[1]
  if (is.na(start)) stop("No TITLE: line found in ", legend_path)
  out <- sub("^TITLE:\\s*", "", lines[start])
  i <- start + 1L
  while (i <= length(lines) && nzchar(trimws(lines[i]))) {
    out <- paste(out, trimws(lines[i]))
    i <- i + 1L
  }
  out
}

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
## mm size (figures range 88.9x88.9 to 179.9x219.8 mm) -- simplest way to get
## one-figure-per-page without irregular page sizes; image is scaled to fit
## within the page margins, aspect ratio preserved, and centred under the
## title block.
PAGE_W_MM <- 210
PAGE_H_MM <- 297
MARGIN_MM <- 12
TITLE_BLOCK_MM <- 34

grDevices::pdf(OUT_PDF, width = PAGE_W_MM / 25.4, height = PAGE_H_MM / 25.4,
                onefile = TRUE, title = "FLUXNET Annual Paper 2026 -- Supplementary Figures",
                family = "Helvetica")

for (i in seq_along(STEMS)) {
  s <- STEMS[i]
  png_path <- file.path(SUPFIGS_DIR, paste0(s, ".png"))
  txt_path <- file.path(SUPFIGS_DIR, paste0(s, ".legend.txt"))
  ## Base grDevices::pdf()'s PostScript Helvetica has no em-dash glyph (same
  ## issue as scripts/figure4_representativeness.R's panel titles) -- use a
  ## plain hyphen directly rather than let R silently substitute one in.
  title_text <- gsub("—", "-", read_title(txt_path))
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
  ## Title block: number (bold) then legend title, wrapped, top-left of margin
  grid::pushViewport(grid::viewport(
    x = unit(MARGIN_MM, "mm"), y = unit(PAGE_H_MM - MARGIN_MM, "mm"),
    width = unit(avail_w_mm, "mm"), height = unit(TITLE_BLOCK_MM, "mm"),
    just = c("left", "top")
  ))
  grid::grid.text(num_label, x = 0, y = 1, just = c("left", "top"),
                   gp = grid::gpar(fontface = "bold", fontsize = 12, fontfamily = "Helvetica"))
  ## Word-wrap the (often long, multi-sentence) legend TITLE to the available
  ## page width -- grid.text() does not auto-wrap, and an unwrapped long
  ## title runs off the right margin (found on first render, 2026-10-02).
  ## ~100 characters/line is conservative for 9pt Helvetica at this width
  ## (186mm avail_w_mm / ~1.6mm per character).
  wrapped_title <- paste(strwrap(title_text, width = 100), collapse = "\n")
  grid::grid.text(wrapped_title, x = 0, y = 0.68, just = c("left", "top"),
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

  msg("Page ", i, ": ", s, " (", num_label, "), downsample 1/", factor,
      " (", round(SOURCE_DPI / factor), " dpi effective)")
}

grDevices::dev.off()

size_mb <- file.info(OUT_PDF)$size / 1e6
msg("Saved: ", OUT_PDF, " (", round(size_mb, 2), " MB, ", length(STEMS), " pages)")
if (size_mb > 10) {
  stop("supplementary_figures.pdf is ", round(size_mb, 2),
       " MB -- exceeds the 10 MB task limit. Reduce TARGET_DPI and re-run.")
}
msg("=== Done ===")
