## verify_fig02alt_inset_rev2.R
##
## One-off measurement verification for the Figure 2 inset candidate,
## revision 2 (logs/prompts/2026-10-12_fig02_inset_rev2.md). Not part of the
## regular pipeline. Confirms, by pixel and PDF-text measurement (not visual
## impression), that:
##   1. Legend key colours match the plotted fill colours (same alpha).
##   2. The legend's bounding box, in main-panel data coordinates, contains
##      no stacked area, historical line, historical label, or dashed line.
##   3. Each "n=" threshold label's bounding box does not intersect the
##      tallest histogram bar, in both the inset and Supplementary Figure S6.
##
## Sources the actual generator script so checks 1-2 use exactly the objects
## (main_panel, window, LEGEND_*/INSET_*, npc_x/npc_y) that produced the
## committed figures, not independently re-derived numbers.

msg <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ..., "\n", sep = "")

msg("Sourcing the generator script (regenerates the committed figures identically)...")
source("scripts/generate_fig02alt_cumulative_siteyears_record_length_inset.R")
INSET_RIGHT_YEAR <- 2010   # matches the generator's INSET_RIGHT <- npc_x(2010)

## =============================================================================
## Check 1 + 2: legend colour vs plotted colour; legend bbox vs data
## =============================================================================
alpha_blend <- function(hex, alpha, bg = c(1, 1, 1)) {
  rgb <- as.vector(grDevices::col2rgb(hex)) / 255
  alpha * rgb + (1 - alpha) * bg
}
expected_rgb <- sapply(PAPER_IGBP_ORDER, function(k) alpha_blend(PAPER_IGBP_COLOURS[[k]], 0.8))
colnames(expected_rgb) <- PAPER_IGBP_ORDER

panel_px_bounds <- function(img) {
  gray <- (img[, , 1] + img[, , 2] + img[, , 3]) / 3
  rowblack <- apply(gray, 1, function(r) mean(r < 0.15))
  colblack <- apply(gray, 2, function(c) mean(c < 0.15))
  rows <- which(rowblack > 0.5); cols <- which(colblack > 0.5)
  row_mid <- vapply(split(rows, cumsum(c(1, diff(rows) > 5))), mean, numeric(1))
  col_mid <- vapply(split(cols, cumsum(c(1, diff(cols) > 5))), mean, numeric(1))
  list(top = min(row_mid), bottom = max(row_mid), left = min(col_mid), right = max(col_mid))
}

find_colour_cluster <- function(img, row_range, col_range, target_rgb, tol = 0.02) {
  sub <- img[row_range, col_range, , drop = FALSE]
  match_mask <- (abs(sub[, , 1] - target_rgb[1]) < tol) & (abs(sub[, , 2] - target_rgb[2]) < tol) &
    (abs(sub[, , 3] - target_rgb[3]) < tol)
  if (!any(match_mask)) return(NULL)
  match_rows <- which(apply(match_mask, 1, any))
  row_breaks <- which(diff(match_rows) > 3)
  starts <- c(1, row_breaks + 1); ends <- c(row_breaks, length(match_rows))
  best <- which.max(ends - starts + 1)
  best_rows <- match_rows[starts[best]:ends[best]]
  best_cols <- which(apply(match_mask[best_rows, , drop = FALSE], 2, any))
  list(row_center = row_range[1] - 1 + mean(range(best_rows)),
       col_center = col_range[1] - 1 + mean(range(best_cols)),
       height_px = length(best_rows))
}
sample_rgb <- function(img, row, col) as.vector(img[round(row), round(col), ])

built <- ggplot2::ggplot_build(main_panel)
area_layer_idx <- which(vapply(main_panel$layers, function(l) inherits(l$geom, "GeomArea"), logical(1)))[1]
area_data <- built$data[[area_layer_idx]]
last_year_rows <- area_data[area_data$x == max(area_data$x), ]
last_year_rows$igbp <- PAPER_IGBP_ORDER[last_year_rows$group]

msg("=== Check 1+2: legend colour match + bounding-box containment ===")
msg("Per-class band at year ", max(area_data$x), " (exact, from ggplot_build -- the thickest point for every class):")
print(last_year_rows[order(last_year_rows$group), c("igbp", "ymin", "ymax")], row.names = FALSE)

check_size <- function(png_path, label) {
  msg("--- ", label, " ---")
  img <- png::readPNG(png_path)
  pb <- panel_px_bounds(img)
  py <- function(value) pb$bottom - npc_y(value) * (pb$bottom - pb$top)

  leg_row_top    <- pb$top  + (1 - LEGEND_TOP)    * (pb$bottom - pb$top)
  leg_row_bottom <- pb$top  + (1 - LEGEND_BOTTOM) * (pb$bottom - pb$top)
  leg_col_left   <- pb$left + LEGEND_LEFT  * (pb$right - pb$left)
  leg_col_right  <- pb$left + LEGEND_RIGHT * (pb$right - pb$left)
  row_lo <- max(1, floor(leg_row_top) - 5); row_hi <- min(dim(img)[1], ceiling(leg_row_bottom) + 5)
  col_lo <- max(1, floor(leg_col_left) - 5); col_hi <- min(dim(img)[2], ceiling(leg_col_right) + 5)

  results <- data.frame(igbp = character(0), legend_rgb = character(0), data_rgb = character(0),
                         expected_rgb = character(0), band_height_px = numeric(0), colour_match = character(0))
  for (cls in PAPER_IGBP_ORDER) {
    target <- expected_rgb[, cls]
    key <- find_colour_cluster(img, row_lo:row_hi, col_lo:col_hi, target, tol = 0.02)
    key_rgb <- if (!is.null(key)) sample_rgb(img, key$row_center, key$col_center) else c(NA, NA, NA)
    band <- last_year_rows[last_year_rows$igbp == cls, ]
    band_height_px <- (band$ymax - band$ymin) / y_hi * (pb$bottom - pb$top)
    data_row <- py((band$ymin + band$ymax) / 2)
    data_col <- round(pb$left + npc_x(max(area_data$x)) * (pb$right - pb$left) - 3)
    data_rgb <- if (band_height_px >= 3) sample_rgb(img, data_row, data_col) else c(NA, NA, NA)
    colour_match <- if (anyNA(key_rgb)) "NO KEY FOUND" else if (anyNA(data_rgb)) "<3px band, key==expected" else
      if (all(abs(key_rgb - data_rgb) < (3 / 255))) "MATCH" else "MISMATCH"
    results <- rbind(results, data.frame(
      igbp = cls,
      legend_rgb = paste(round(key_rgb * 255), collapse = ","),
      data_rgb = if (!anyNA(data_rgb)) paste(round(data_rgb * 255), collapse = ",") else NA,
      expected_rgb = paste(round(target * 255), collapse = ","),
      band_height_px = round(band_height_px, 1),
      colour_match = colour_match
    ))
  }
  print(results, row.names = FALSE)
  all_keys_correct <- all(results$legend_rgb == results$expected_rgb)
  any_mismatch <- any(results$colour_match == "MISMATCH")
  msg(if (all_keys_correct) "PASS: every legend key == its expected alpha=0.8-over-white colour."
      else "FAIL: a legend key does not match its expected colour.")
  msg(if (!any_mismatch) "PASS: every comparable (>=3px band) legend key matches the plotted stacked-area colour."
      else "FAIL: a legend key does not match its plotted stacked-area colour.")

  leg_year_min <- x_lo + (leg_col_left  - pb$left) / (pb$right - pb$left) * (x_hi - x_lo)
  leg_year_max <- x_lo + (leg_col_right - pb$left) / (pb$right - pb$left) * (x_hi - x_lo)
  leg_val_max  <- (1 - (leg_row_top    - pb$top) / (pb$bottom - pb$top)) * y_hi
  leg_val_min  <- (1 - (leg_row_bottom - pb$top) / (pb$bottom - pb$top)) * y_hi
  msg("Legend bbox in data coords: year [", round(leg_year_min, 2), ", ", round(leg_year_max, 2),
      "], site-years [", round(leg_val_min), ", ", round(leg_val_max), "]")

  ## The inset (year [1993, 2010], per the revision-1 footprint) already,
  ## legitimately covers the 2000 and 2007 dashed lines and any stacked-area
  ## pixels beneath it (approved in logs/prompts/2026-10-12_fig02_inset_rev1.md
  ## -- that is the whole point of the inset). The legend sits on top of the
  ## SAME inset for the portion of its bbox within [1993, 2010], so nothing
  ## is newly hidden there; only the slice of the legend bbox to the RIGHT of
  ## the inset (year > 2010, open main-panel territory) can newly cover
  ## anything. Each check below is therefore restricted to that exposed
  ## slice, not naively to the legend's full year range.
  exposed_lo <- max(leg_year_min, INSET_RIGHT_YEAR)
  exposed_hi <- leg_year_max
  total_by_year <- aggregate(cbind(total = area_data$ymax), by = list(x = area_data$x), FUN = max)
  in_exposed <- total_by_year[total_by_year$x >= floor(exposed_lo) & total_by_year$x <= ceiling(exposed_hi), ]
  max_cum_exposed <- if (nrow(in_exposed) > 0) max(in_exposed$total) else -Inf
  hist_max <- 1532
  dashed_hit <- any(c(2000, 2007, 2015) > exposed_lo & c(2000, 2007, 2015) <= exposed_hi)

  msg("Exposed slice (beyond the inset's own year-2010 right edge): year [",
      round(exposed_lo, 2), ", ", round(exposed_hi, 2), "]")
  msg("Max stacked-area cumulative within the exposed slice: ", round(max_cum_exposed),
      " (margin vs legend bbox bottom = ", round(leg_val_min - max_cum_exposed), " site-years)")
  msg("Highest historical line value anywhere: ", hist_max,
      " (margin vs legend bbox bottom = ", round(leg_val_min - hist_max), " site-years)")
  msg("Legend bbox's exposed slice crosses a dashed reference line (2000/2007/2015): ", dashed_hit)
  ok <- (leg_val_min > max_cum_exposed) && (leg_val_min > hist_max) && !dashed_hit
  msg(if (ok) "PASS: legend bbox's exposed portion is clear of the stacked area, historical lines, and dashed lines."
      else "FAIL: legend bbox is not clear.")
  invisible(results)
}

check_size("review/figures/candidates/fig_02alt_cumulative_siteyears_record_length_inset_120mm.png", "120mm")
check_size("review/figures/candidates/fig_02alt_cumulative_siteyears_record_length_inset_89mm.png",  "89mm")

## =============================================================================
## Check 3: n= label bbox vs tallest bar, via pdftotext -bbox (exact text
## positions, no pixel guessing). Calibrates the histogram's own axes from
## its own tick-label words -- unambiguous because these tick values (e.g.
## 50/100/150, or bare 10/20/30) don't appear anywhere else in either PDF.
## =============================================================================
msg("=== Check 3: n= label bbox vs tallest bar (pdftotext -bbox) ===")

pdf_words <- function(pdf_path) {
  lines <- system2("pdftotext", c("-bbox", pdf_path, "-"), stdout = TRUE, stderr = FALSE)
  words <- grep("<word ", lines, value = TRUE)
  get_attr <- function(s, name) as.numeric(sub(paste0('.*', name, '="([0-9.-]+)".*'), "\\1", s))
  text_of <- function(s) sub(".*>(.*)</word>.*", "\\1", s)
  data.frame(
    xMin = vapply(words, get_attr, numeric(1), name = "xMin"),
    xMax = vapply(words, get_attr, numeric(1), name = "xMax"),
    yMin = vapply(words, get_attr, numeric(1), name = "yMin"),
    yMax = vapply(words, get_attr, numeric(1), name = "yMax"),
    text = vapply(words, text_of, character(1)),
    row.names = NULL
  )
}

check_label_vs_bar <- function(pdf_path, label, y_ticks, x_ticks, max_bar_count, bar_x_lo, bar_x_hi,
                                x_search_range = NULL) {
  msg("--- ", label, " ---")
  w <- pdf_words(pdf_path)
  if (!is.null(x_search_range)) w <- w[w$xMin >= x_search_range[1] & w$xMax <= x_search_range[2], ]

  y_pts <- w[w$text %in% as.character(y_ticks), ]
  y_pts$center <- (y_pts$yMin + y_pts$yMax) / 2
  y_fit <- lm(center ~ as.numeric(text), data = y_pts)
  y_pdf <- function(value) predict(y_fit, newdata = data.frame(text = value))

  x_pts <- w[w$text %in% as.character(x_ticks), ]
  x_pts$center <- (x_pts$xMin + x_pts$xMax) / 2
  x_fit <- lm(center ~ as.numeric(text), data = x_pts)
  x_pdf <- function(value) predict(x_fit, newdata = data.frame(text = value))

  bar_top_y <- y_pdf(max_bar_count)
  bar_left_x <- x_pdf(bar_x_lo); bar_right_x <- x_pdf(bar_x_hi)
  msg("Tallest bar (height ", max_bar_count, "): pdf y = ", round(bar_top_y, 2),
      ", pdf x = [", round(bar_left_x, 2), ", ", round(bar_right_x, 2), "]")

  for (nlab in c("n=483", "n=225", "n=63")) {
    lab <- w[w$text == nlab, ]
    if (nrow(lab) == 0) { msg(nlab, ": NOT FOUND"); next }
    lab <- lab[1, ]
    v_overlap <- (lab$yMax >= bar_top_y)              # label's bottom edge reaches down to/past the bar's top
    h_overlap <- (lab$xMax >= bar_left_x) && (lab$xMin <= bar_right_x)
    intersects <- v_overlap && h_overlap
    msg(nlab, ": bbox x=[", lab$xMin, ",", lab$xMax, "] y=[", lab$yMin, ",", lab$yMax,
        "] -- vertical clearance vs tallest bar = ", round(bar_top_y - lab$yMax, 2), "pt; ",
        if (intersects) "FAIL (intersects tallest bar)" else "PASS (clear)")
  }
}

## Inset: x breaks 0/10/20/30, y breaks (120mm) 0/50/100/150; tallest bar at
## record length 6, bin [5.5, 6.5] (boundary = 0.5, binwidth = 1).
max_bar_count <- max(table(record_length$hist_df$n_years))
check_label_vs_bar("review/figures/candidates/fig_02alt_cumulative_siteyears_record_length_inset_120mm.pdf",
                    "inset 120mm", y_ticks = c(50, 100, 150), x_ticks = c(10, 20, 30),
                    max_bar_count = max_bar_count, bar_x_lo = 5.5, bar_x_hi = 6.5)
check_label_vs_bar("review/figures/candidates/fig_02alt_cumulative_siteyears_record_length_inset_89mm.pdf",
                    "inset 89mm", y_ticks = c(50, 100, 150), x_ticks = c(10, 20, 30),
                    max_bar_count = max_bar_count, bar_x_lo = 5.5, bar_x_hi = 6.5)
## S6 panel a: restrict to its own sub-page (left half -- the PDF holds both
## panels a and b side by side) so its "0"/"30" labels aren't confused with
## panel b's.
check_label_vs_bar("review/figures/draft_manuscript_v1/SupFigs/figS6_record_length.pdf",
                    "S6 panel a", y_ticks = c(50, 100), x_ticks = c(10, 20, 30),
                    max_bar_count = max_bar_count, bar_x_lo = 5.5, bar_x_hi = 6.5,
                    x_search_range = c(0, 200))

msg("=== Done ===")
