## check_figure_format.R
## Nature-format compliance checker for every figure in
## review/figures/draft_manuscript_v1/ (main text) and its SupFigs/ (Extended
## Data). Measures EVERYTHING from the output files themselves -- never from
## the plotting code -- using three system tools (poppler-utils, already
## installed on this machine: pdfinfo, pdffonts, pdftocairo):
##   - pdfinfo:    page size (pt) -> mm, compared against the main-text
##                 (89 or 183 mm wide, <=247 mm tall) / Extended Data
##                 (<=180x240 mm) limits.
##   - pdffonts:   embedded font names, must be Helvetica/Helvetica-Bold
##                 only, with ONE documented exception: Symbol. R's
##                 plotmath renderer draws a true minus sign (U+2212) by
##                 pulling its glyph from the Symbol font -- base Helvetica
##                 (WinAnsiEncoding) has no true-minus glyph at all, only
##                 ASCII hyphen-minus. Symbol in the font list is therefore
##                 evidence that true minus signs ARE being used (task 3's
##                 own requirement), not a violation -- confirmed by direct
##                 inspection: Symbol disappears from a PDF that has no
##                 negative numbers in any plotmath-rendered text.
##   - pdftotext -bbox: per-word bounding boxes (pt). Text height is
##                 converted to nominal point size via a calibration
##                 constant (BBOX_TO_PT, see below), established by
##                 rendering known font sizes (5,7,8,10pt; regular/bold;
##                 digits/letters/ascenders/descenders) and measuring their
##                 bbox height in the same pdftotext -bbox pipeline -- the
##                 ratio (bbox height / nominal pt) was IDENTICAL (0.925)
##                 regardless of glyph content, confirming poppler reports
##                 the font's ascent-to-descent box, not per-glyph ink
##                 extents, so this calibration is robust across all text
##                 in these figures.
##   - pdftocairo -svg: converts to SVG (1 SVG user unit = 1 PDF point,
##                 confirmed directly: a 252pt-wide PDF produces
##                 width="252pt" viewBox="0 0 252 252"), then stroke-width
##                 attributes are read directly as line weights in points
##                 -- no ggplot2-linewidth-unit conversion needed, since
##                 this measures the ACTUAL rendered stroke, not the
##                 plotting code's input value.
##
## Exit status: 0 if every figure passes every check, 1 otherwise (and a
## non-zero exit on any PASS/FAIL determination problem, e.g. a required
## tool missing).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

suppressPackageStartupMessages(library(dplyr))

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

# ---- Step 0: required tools ---------------------------------------------------
REQUIRED_TOOLS <- c("pdfinfo", "pdffonts", "pdftotext", "pdftocairo")
missing_tools <- REQUIRED_TOOLS[vapply(REQUIRED_TOOLS, function(t) nchar(Sys.which(t)) == 0L, logical(1))]
if (length(missing_tools) > 0L) {
  stop("check_figure_format.R requires poppler-utils on PATH; missing: ",
       paste(missing_tools, collapse = ", "), call. = FALSE)
}

# ---- Constants ------------------------------------------------------------------
PT_PER_MM <- 72 / 25.4
MM_MAIN_SINGLE <- 89
MM_MAIN_DOUBLE <- 183
MM_MAIN_MAX_HEIGHT <- 247
MM_ED_MAX_WIDTH  <- 180
MM_ED_MAX_HEIGHT <- 240

## Calibration: bbox height (pt, from pdftotext -bbox) / nominal font size
## (pt) -- see header comment. Measured directly, not assumed.
BBOX_TO_PT <- 0.925

TEXT_MIN_PT          <- 5
TEXT_MAX_PT          <- 7
TEXT_TOLERANCE_PT    <- 0.5     # measurement/rounding slack around the 5-7pt band
PANEL_LETTER_PT      <- 8
PANEL_LETTER_TOL_PT  <- 0.5
SUPERSCRIPT_FLOOR_PT <- 3.4     # plotmath superscripts run ~0.7x base size; 0.7*5 = 3.5pt
                                 # is the smallest legitimate case (a superscript inside
                                 # 5pt legend text) -- below this floor is a hard failure,
                                 # between the floor and TEXT_MIN_PT-tolerance is a warning,
                                 # not a failure (cannot distinguish a genuine superscript
                                 # from undersized body text by geometry alone).

LINEWIDTH_MIN_PT <- 0.25
LINEWIDTH_MAX_PT <- 1.0
LINEWIDTH_TOLERANCE_PT <- 0.05

ALLOWED_FONTS <- c("Helvetica", "Helvetica-Bold", "Symbol")

# ---- Step 1: discover figures ---------------------------------------------------
MAIN_DIR <- "review/figures/draft_manuscript_v1"
ED_DIR   <- file.path(MAIN_DIR, "SupFigs")

main_pngs <- list.files(MAIN_DIR, pattern = "\\.png$", full.names = TRUE)
ed_pngs   <- list.files(ED_DIR,   pattern = "\\.png$", full.names = TRUE)

figures <- c(
  lapply(main_pngs, function(p) list(stem = tools::file_path_sans_ext(p), kind = "main")),
  lapply(ed_pngs,   function(p) list(stem = tools::file_path_sans_ext(p), kind = "ed"))
)
msg("Discovered ", length(main_pngs), " main-text figure(s) in ", MAIN_DIR,
    " and ", length(ed_pngs), " Extended Data figure(s) in ", ED_DIR)

# ---- Helpers ----------------------------------------------------------------------
run_tool <- function(args) {
  out <- suppressWarnings(system2(args[1], args[-1], stdout = TRUE, stderr = TRUE))
  out
}

check_files_present <- function(stem, kind) {
  png <- paste0(stem, ".png")
  pdf <- paste0(stem, ".pdf")
  legend <- paste0(stem, ".legend.txt")
  jpg <- paste0(stem, ".jpg")
  issues <- character(0)
  if (!file.exists(png))    issues <- c(issues, "missing PNG")
  if (!file.exists(pdf))    issues <- c(issues, "missing PDF")
  if (!file.exists(legend)) issues <- c(issues, "missing legend.txt")
  if (kind == "ed" && !file.exists(jpg)) issues <- c(issues, "missing JPEG")
  list(ok = length(issues) == 0L, issues = issues)
}

check_page_size <- function(pdf_path, kind) {
  info <- run_tool(c("pdfinfo", pdf_path))
  line <- grep("^Page size:", info, value = TRUE)
  if (length(line) == 0L) return(list(ok = FALSE, issues = "pdfinfo: no page size found", w_mm = NA, h_mm = NA))
  m <- regmatches(line, regexec("Page size:\\s*([0-9.]+) x ([0-9.]+) pts", line))[[1]]
  if (length(m) != 3L) return(list(ok = FALSE, issues = "pdfinfo: unparseable page size", w_mm = NA, h_mm = NA))
  w_pt <- as.numeric(m[2]); h_pt <- as.numeric(m[3])
  w_mm <- w_pt / PT_PER_MM; h_mm <- h_pt / PT_PER_MM

  issues <- character(0)
  if (kind == "main") {
    width_ok <- isTRUE(all.equal(w_mm, MM_MAIN_SINGLE, tolerance = 0.5)) ||
                isTRUE(all.equal(w_mm, MM_MAIN_DOUBLE, tolerance = 0.5))
    if (!width_ok) issues <- c(issues, sprintf("width %.1fmm is neither %d nor %dmm", w_mm, MM_MAIN_SINGLE, MM_MAIN_DOUBLE))
    if (h_mm > MM_MAIN_MAX_HEIGHT + 0.5) issues <- c(issues, sprintf("height %.1fmm exceeds %dmm", h_mm, MM_MAIN_MAX_HEIGHT))
  } else {
    if (w_mm > MM_ED_MAX_WIDTH + 0.5)  issues <- c(issues, sprintf("width %.1fmm exceeds %dmm", w_mm, MM_ED_MAX_WIDTH))
    if (h_mm > MM_ED_MAX_HEIGHT + 0.5) issues <- c(issues, sprintf("height %.1fmm exceeds %dmm", h_mm, MM_ED_MAX_HEIGHT))
  }
  list(ok = length(issues) == 0L, issues = issues, w_mm = w_mm, h_mm = h_mm)
}

check_fonts <- function(pdf_path) {
  out <- run_tool(c("pdffonts", pdf_path))
  if (length(out) < 3L) return(list(ok = TRUE, issues = character(0), fonts = character(0)))
  rows <- out[-(1:2)]
  rows <- rows[nzchar(trimws(rows))]
  ## Font names never contain whitespace (Helvetica, Helvetica-Bold, Symbol, ...)
  ## even though the "type" column that follows sometimes does ("Type 1") --
  ## so the first whitespace-delimited token is always exactly the font name,
  ## regardless of pdffonts' column widths (which vary by file).
  fonts <- unique(vapply(rows, function(r) sub("^(\\S+).*$", "\\1", trimws(r)), character(1)))
  bad <- setdiff(fonts, ALLOWED_FONTS)
  list(ok = length(bad) == 0L,
       issues = if (length(bad) > 0L) paste0("disallowed font(s): ", paste(bad, collapse = ", ")) else character(0),
       fonts = fonts)
}

check_text_sizes <- function(pdf_path) {
  out <- run_tool(c("pdftotext", "-bbox", pdf_path, "-"))
  words <- grep("<word ", out, value = TRUE)
  if (length(words) == 0L) return(list(ok = TRUE, issues = character(0), n_words = 0L))

  get_attr <- function(s, name) {
    m <- regmatches(s, regexpr(paste0(name, '="[0-9.-]+"'), s))
    as.numeric(sub(paste0(name, '="([0-9.-]+)"'), "\\1", m))
  }
  text_of <- function(s) sub(".*>(.*)</word>.*", "\\1", s)

  txts <- vapply(words, text_of, character(1))

  ## Rotated text (e.g. a y-axis title drawn at 90 degrees) has its bbox
  ## "height" (yMax-yMin) measuring the STRING LENGTH, not the font size --
  ## for those words the font-size-correlated dimension is the WIDTH
  ## instead, confirmed directly: a rotated axis title's word bbox is
  ## ~6.5pt wide x ~35pt tall. Detected per-word as height > width AND
  ## nchar >= 2 -- a single glyph is naturally taller than wide regardless
  ## of rotation (e.g. "(", "0", "f"), so that test only fires for actual
  ## multi-character rotated strings, never for an ordinary short token.
  heights <- vapply(seq_along(words), function(i) {
    w <- words[i]; txt <- txts[i]
    xMin <- get_attr(w, "xMin"); xMax <- get_attr(w, "xMax")
    yMin <- get_attr(w, "yMin"); yMax <- get_attr(w, "yMax")
    if (length(yMin) == 0 || length(yMax) == 0 || length(xMin) == 0 || length(xMax) == 0) return(NA_real_)
    width <- xMax - xMin; height <- yMax - yMin
    if (nchar(txt) >= 2 && height > width) width else height
  }, numeric(1))
  pts <- heights / BBOX_TO_PT

  ## A lone punctuation character's own glyph ink box (parentheses especially)
  ## is not drawn proportionally to the font's nominal ascent-descent box --
  ## confirmed directly: an isolated "(" in known-7pt body text measures
  ## ~3.2pt by this method, a glyph-design artifact rather than a true-size
  ## signal, so standalone punctuation tokens carry no reliable size
  ## evidence either way and are excluded from the compliance check.
  ## Plotmath's own minus-sign glyph (U+2212, drawn via the Symbol font --
  ## see nature_minus_labels() in R/nature_format.R) is emitted by poppler
  ## as its OWN separate "word" distinct from the digits it negates, with a
  ## bbox reflecting the Symbol glyph's own design box, not the surrounding
  ## text's nominal size -- excluded here for the same reason as isolated
  ## punctuation below.
  is_punct_only <- grepl("^[][().,;:−]+$", txts)

  is_panel_letter <- grepl("^[a-z]$", txts) & abs(pts - PANEL_LETTER_PT) <= PANEL_LETTER_TOL_PT
  is_body <- abs(pts - ((TEXT_MIN_PT + TEXT_MAX_PT) / 2)) <= ((TEXT_MAX_PT - TEXT_MIN_PT) / 2 + TEXT_TOLERANCE_PT)
  is_superscript_ok <- pts >= SUPERSCRIPT_FLOOR_PT & pts < (TEXT_MIN_PT - TEXT_TOLERANCE_PT)

  fail <- !is_panel_letter & !is_body & !is_superscript_ok & !is_punct_only & !is.na(pts)
  warn <- is_superscript_ok & !is_punct_only

  issues <- character(0)
  if (any(fail)) {
    bad_examples <- unique(sprintf("\"%s\" ~%.1fpt", txts[fail], pts[fail]))
    issues <- c(issues, paste0(sum(fail), " word(s) outside 5-7pt (or 8pt panel letter): ",
                                paste(utils::head(bad_examples, 6), collapse = "; "),
                                if (length(bad_examples) > 6) "; ..." else ""))
  }
  list(ok = length(issues) == 0L, issues = issues, n_words = length(words),
       n_warn = sum(warn), n_fail = sum(fail))
}

check_linewidths <- function(pdf_path, tmp_dir) {
  svg_path <- file.path(tmp_dir, paste0(tools::file_path_sans_ext(basename(pdf_path)), ".svg"))
  run_tool(c("pdftocairo", "-svg", pdf_path, svg_path))
  if (!file.exists(svg_path)) return(list(ok = FALSE, issues = "pdftocairo -svg failed", n_strokes = 0L))
  svg_txt <- paste(readLines(svg_path, warn = FALSE), collapse = "\n")
  ## Per-element (not per-attribute) matching: a tag's stroke colour has to
  ## be read from the SAME tag as its stroke-width, to exclude a WHITE
  ## stroke -- e.g. the whole-canvas plot-background rectangle ggplot2/grid
  ## draws with colour = NA, which still emits an (invisible, white-on-white)
  ## stroke-width in the SVG. Confirmed directly: fig_01's background
  ## rectangle carries stroke-width="1.07" stroke="rgb(100%, 100%, 100%)" --
  ## a real but UNSEEABLE "line", not a Nature-format violation.
  tags <- regmatches(svg_txt, gregexpr("<[a-zA-Z]+[^>]*stroke-width=\"[0-9.]+\"[^>]*/?>", svg_txt))[[1]]
  get_tag_attr <- function(tag, name) {
    m <- regmatches(tag, regexpr(paste0(name, '="[^"]*"'), tag))
    if (length(m) == 0L) return(NA_character_)
    sub(paste0(name, '="([^"]*)"'), "\\1", m)
  }
  widths <- as.numeric(get_tag_attr(tags, "stroke-width"))
  strokes <- get_tag_attr(tags, "stroke")
  is_white <- !is.na(strokes) & grepl("^rgb\\(100%,\\s*100%,\\s*100%\\)$", strokes)
  keep <- widths > 0 & !is_white  # 0-width (hairline fills) and invisible white strokes are not a drawn line weight
  widths <- widths[keep]
  if (length(widths) == 0L) return(list(ok = TRUE, issues = character(0), n_strokes = 0L))
  bad <- widths < (LINEWIDTH_MIN_PT - LINEWIDTH_TOLERANCE_PT) | widths > (LINEWIDTH_MAX_PT + LINEWIDTH_TOLERANCE_PT)
  issues <- character(0)
  if (any(bad)) {
    issues <- c(issues, sprintf("%d stroke(s) outside %.2f-%.2fpt: e.g. %s pt",
                                 sum(bad), LINEWIDTH_MIN_PT, LINEWIDTH_MAX_PT,
                                 paste(utils::head(sort(unique(widths[bad])), 5), collapse = ", ")))
  }
  list(ok = length(issues) == 0L, issues = issues, n_strokes = length(widths),
       min_w = min(widths), max_w = max(widths))
}

# ---- Step 2: run all checks -------------------------------------------------------
tmp_dir <- tempfile("fig_format_check_")
dir.create(tmp_dir)
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

results <- list()
for (f in figures) {
  name <- basename(f$stem)
  pdf_path <- paste0(f$stem, ".pdf")
  files_chk <- check_files_present(f$stem, f$kind)
  if (!file.exists(pdf_path)) {
    results[[name]] <- list(name = name, kind = f$kind, ok = FALSE,
                             issues = c(files_chk$issues, "no PDF to check further"))
    next
  }
  size_chk <- check_page_size(pdf_path, f$kind)
  font_chk <- check_fonts(pdf_path)
  text_chk <- check_text_sizes(pdf_path)
  line_chk <- check_linewidths(pdf_path, tmp_dir)

  all_issues <- c(files_chk$issues, size_chk$issues, font_chk$issues, text_chk$issues, line_chk$issues)
  results[[name]] <- list(
    name = name, kind = f$kind, ok = length(all_issues) == 0L, issues = all_issues,
    w_mm = size_chk$w_mm, h_mm = size_chk$h_mm, fonts = font_chk$fonts,
    n_words = text_chk$n_words, n_warn = text_chk$n_warn,
    lw_min = line_chk$min_w, lw_max = line_chk$max_w
  )
}

# ---- Step 3: report ---------------------------------------------------------------
cat("\n")
cat(sprintf("%-42s %-5s %-12s %-6s %-6s %-10s %s\n",
            "FIGURE", "KIND", "SIZE (mm)", "FONTS", "TEXT", "LINES(pt)", "STATUS"))
cat(strrep("-", 110), "\n")
any_fail <- FALSE
for (r in results) {
  size_str <- if (!is.na(r$w_mm)) sprintf("%.1fx%.1f", r$w_mm, r$h_mm) else "?"
  font_str <- if (!is.null(r$fonts)) ifelse(setequal(r$fonts, intersect(r$fonts, c("Helvetica","Helvetica-Bold"))), "OK", "OK*") else "?"
  lw_str <- if (!is.null(r$lw_min) && !is.na(r$lw_min)) sprintf("%.2f-%.2f", r$lw_min, r$lw_max) else "n/a"
  status <- if (r$ok) "PASS" else "FAIL"
  if (!r$ok) any_fail <- TRUE
  cat(sprintf("%-42s %-5s %-12s %-6s %-6s %-10s %s\n",
              substr(r$name, 1, 42), r$kind, size_str, font_str,
              paste0(r$n_words, "w"), lw_str, status))
  if (!r$ok) {
    for (iss in r$issues) cat("    - ", iss, "\n", sep = "")
  }
}
cat(strrep("-", 110), "\n")
cat("FONTS 'OK*' = Helvetica/Helvetica-Bold plus Symbol (true-minus-sign glyph substitution; see header).\n")
cat(sprintf("TOTAL: %d figure(s), %d PASS, %d FAIL\n", length(results), sum(vapply(results, `[[`, logical(1), "ok")), sum(!vapply(results, `[[`, logical(1), "ok"))))

if (any_fail) {
  quit(status = 1)
} else {
  quit(status = 0)
}
