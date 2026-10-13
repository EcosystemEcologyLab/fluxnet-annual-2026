## build_presentation_deck.R
##
## Assembles review/figures/presentation_figures/presentation_versions.pptx
## from manifest.csv: one slide per PNG in review/figures/presentation_figures
## /slides/, title placeholder left empty (top ~1.2in band reserved above
## each 12.8x6.3in image), speaker notes carrying that figure's caption
## text/numbers (from manifest.csv's speaker_notes column, itself drawn from
## each figure's .legend.txt -- see review/figures/presentation_figures/
## manifest.csv) and the snapshot file used.
##
## No new package dependency: `officer` 0.7.5 is already resolvable from the
## project's renv lockfile; only `external_img()` is used (plain image
## placement), not `rvg` (editable vector graphics, not installed, not
## needed here).
##
## officer's read_pptx() always starts from its built-in 4:3 (10x7.5in)
## template -- there is no width/height argument and no 16:9 template ships
## with the package. The deck is built normally, then the slide canvas is
## widened to the PowerPoint-standard 16:9 widescreen size (13.333x7.5in,
## i.e. <p:sldSz cx="12192000" cy="6858000"/> in EMU -- confirmed against
## PowerPoint's own exported widescreen value) by a small post-process on
## the saved .pptx's ppt/presentation.xml (a pptx is a zip of XML files).
## Content is placed with explicit ph_location() inches computed against the
## FINAL 13.333x7.5in canvas, so this resize is purely a canvas-size fix, not
## a rescale of already-placed content. Verified end-to-end against a LibreOffice
## --headless --convert-to pdf render (pdfinfo reports exactly 960x540pt =
## 13.333x7.5in).

source("R/pipeline_config.R")
check_pipeline_config()
suppressPackageStartupMessages({
  library(officer)
  library(readr)
  library(dplyr)
  library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

SLIDE_DIR    <- "review/figures/presentation_figures/slides"
MANIFEST_CSV <- "review/figures/presentation_figures/manifest.csv"
OUT_PPTX     <- "review/figures/presentation_figures/presentation_versions.pptx"

SLIDE_W_IN <- 13.333
SLIDE_H_IN <- 7.5
IMG_W_IN   <- 12.8
IMG_H_IN   <- 6.3
IMG_LEFT_IN <- (SLIDE_W_IN - IMG_W_IN) / 2
IMG_TOP_IN  <- SLIDE_H_IN - IMG_H_IN   # flush to the bottom edge; title band above

manifest <- readr::read_csv(MANIFEST_CSV, show_col_types = FALSE) |>
  dplyr::arrange(slide_number)

missing <- manifest$png[!file.exists(file.path(SLIDE_DIR, manifest$png))]
if (length(missing) > 0) {
  stop("build_presentation_deck.R: manifest references missing slide PNG(s): ",
       paste(missing, collapse = ", "))
}
msg("Manifest: ", nrow(manifest), " slides.")

doc <- officer::read_pptx()

for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, ]
  doc <- officer::add_slide(doc, layout = "Title Only", master = "Office Theme")
  ## Title placeholder present but left empty, per spec ("slide title left empty").
  doc <- officer::ph_with(doc, value = "", location = officer::ph_location_type(type = "title"))
  doc <- officer::ph_with(
    doc,
    value = officer::external_img(file.path(SLIDE_DIR, row$png), width = IMG_W_IN, height = IMG_H_IN, unit = "in"),
    location = officer::ph_location(left = IMG_LEFT_IN, top = IMG_TOP_IN, width = IMG_W_IN, height = IMG_H_IN)
  )
  doc <- officer::set_notes(doc, value = row$speaker_notes, location = officer::notes_location_type(type = "body"))
  msg("  Slide ", row$slide_number, ": ", row$png, " (", row$source_figure, ", ", row$build_step, ")")
}

fs::dir_create(dirname(OUT_PPTX))
print(doc, target = OUT_PPTX)
msg("Saved: ", OUT_PPTX, " (", nrow(manifest), " slides, 4:3 canvas pre-resize)")

## ---- Widen the canvas to 16:9 (13.333 x 7.5 in) -----------------------------
pres_xml_path <- "ppt/presentation.xml"
tmp_extract <- tempfile("pptx_extract_")
fs::dir_create(tmp_extract)
utils::unzip(OUT_PPTX, files = pres_xml_path, exdir = tmp_extract)
xml_path <- file.path(tmp_extract, pres_xml_path)
xml_txt <- readLines(xml_path, warn = FALSE)
xml_txt <- gsub('<p:sldSz cx="9144000" cy="6858000" type="screen4x3"/>',
                 '<p:sldSz cx="12192000" cy="6858000" type="screen16x9"/>',
                 xml_txt, fixed = TRUE)
if (!any(grepl('cx="12192000"', xml_txt, fixed = TRUE))) {
  stop("build_presentation_deck.R: failed to widen the slide canvas -- ",
       "the expected 4:3 <p:sldSz> string was not found in ", xml_path,
       " (officer's default template may have changed).")
}
writeLines(xml_txt, xml_path)
old_wd <- getwd()
setwd(tmp_extract)
zip_status <- system2("zip", args = c("-q", shQuote(file.path(old_wd, OUT_PPTX)), pres_xml_path))
setwd(old_wd)
if (zip_status != 0) stop("build_presentation_deck.R: `zip` failed to update ", pres_xml_path, " in ", OUT_PPTX)
unlink(tmp_extract, recursive = TRUE)
msg("Widened slide canvas to 13.333 x 7.5 in (16:9) via ppt/presentation.xml.")

msg("Done: ", OUT_PPTX)
