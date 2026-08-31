## generate_whittaker_alt_fig02_update.R
## Successor to scripts/generate_whittaker_alt_fig02.R. Builds an UPDATED
## ALT candidate of the Whittaker climate-space figure: revised point
## styling, a revised NEE colour-scale centre, and corrected/tightened inset
## labelling -- on top of the already-fixed hex_regular/points_in_front base.
##
## CANDIDATE ONLY. Output goes to review/figures/candidates/ only. This
## script does NOT modify scripts/build_draft_manuscript_v1.R and does NOT
## write anything into review/figures/draft_manuscript_v1/ -- the production
## fig_02_whittaker_current.png and its build path
## (scripts/generate_whittaker_overlays.R) are untouched.
##
## Run-log discipline (this script's own most important instruction): a
## durable run log is opened as the very first action, flushed after every
## write, and every step logs a line before and after -- each input/function
## located or found missing (with the exact path checked), each value
## computed, each file written. The whole pipeline runs inside a function
## called via tryCatch() so the final outcome line is written
## UNCONDITIONALLY -- on.exit() registered at Rscript top level does not
## fire on normal completion (the same reason scripts/generate_whittaker_
## overlays.R uses this pattern; reused verbatim here). If a required input
## or function is missing, the missing item and the path checked are logged,
## the final outcome line is written, and the script stops -- no
## substitution.
##
## Changes vs. the base ALT (scripts/generate_whittaker_alt_fig02.R):
##  1. Point styling: point_colour "grey30" -> a quiet dark charcoal
##     ("#2B2B2B"); point_size 0.7 -> ~0.35; point_alpha reduced so dense
##     clusters build tone without individual temperate-cloud points
##     blobbing together. Both point_colour/point_alpha added as new
##     backward-compatible fig_whittaker_worldclim() parameters (default
##     "grey30"/0.50, matching prior hardcoded behaviour) -- same additive
##     pattern as hex_regular/points_in_front.
##  2. NEE colour scale: centre recoloured from near-white ("#F6F6F6") to a
##     pale tan via the new nee_mid_colour parameter. Blue/red endpoints,
##     zero-centring, limits, and oob handling are byte-identical to the
##     stock scale -- see fig_whittaker_worldclim()'s @param nee_mid_colour
##     documentation (R/figures/fig_climate.R) for why a same-endpoints
##     custom HCL two-half reconstruction was tried first and rejected
##     (sweeping hue from blue, H=255, to an off-hue tan centre, H~90, passes
##     through green/teal partway -- not hidden the way it is in the stock
##     palette, where chroma drops to 0 at the achromatic centre before hue
##     would matter). scale_fill_gradient2()'s Lab-space interpolation has no
##     such artifact and pins low/high to diverging_hcl(2, "Blue-Red
##     3")'s exact endpoint hex codes.
##  3. Inset wording: relabelled to three data-driven lines ("<n_sites> sites
##     total", "<n_nee_sites> with annual NEE", "<n_site_years>
##     site-years"), computed at run time from the same data used to build
##     the figure (not hardcoded), via the new detail_lines parameter.
##  4. Inset/legend spacing: tightened via two new style-list fields read by
##     fig_climate.R's .whittaker_theme() and the detail-text annotate() call
##     (both no-ops when absent, so every other style list/caller is
##     unaffected): style$detail_lineheight (tighter inter-line spacing
##     within the inset text) and style$legend_margin (tighter padding
##     around the inside-positioned colorbar). style$legend_pos is nudged via
##     the existing (already-overridable) mechanism to close the gap between
##     the inset text block and the colourbar title.
##
## Output (review/figures/candidates/ only):
##   ALT_fig_02_whittaker_current.png  -- OVERWRITES the prior ALT (chosen
##     over a versioned successor: this IS the same candidate, revised in
##     place per the "candidates are a working/iterative area" convention
##     established for ALT_fig_03 earlier this session; logged explicitly
##     below).
##   ALT_fig_02_whittaker_current.txt  -- updated legend/caption.
##   RUN_LOG_alt_fig02_update.txt      -- this run's durable log.

out_dir      <- file.path("review", "figures", "candidates")
run_log_path <- file.path(out_dir, "RUN_LOG_alt_fig02_update.txt")

# ---- Run log: created as the VERY FIRST action, before any other work ----
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
run_log_con <- file(run_log_path, open = "w")
rl <- function(...) {
  ts   <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  line <- paste0(ts, " ", paste0(..., collapse = ""))
  cat(line, "\n", sep = "", file = run_log_con)
  flush(run_log_con)
  message(line)
}
rl("STARTED: generate_whittaker_alt_fig02_update.R")
rl("decision: OVERWRITE review/figures/candidates/ALT_fig_02_whittaker_current.png ",
   "(and its .txt) in place -- not a newly-versioned successor. Rationale: this is a revision ",
   "of the same candidate built earlier this session (commit 505d130), and review/figures/",
   "candidates/ is a working/iterative area (see e.g. the fig_01-fig_06 numbered candidates ",
   "already there), not a permanent archive -- the prior ALT_fig_02 PNG/txt remain recoverable ",
   "from git history (commit 505d130) if needed.")

# Whole pipeline runs inside a function called via tryCatch() so the final
# outcome line is written unconditionally -- on.exit() registered at Rscript
# top level does not fire on normal completion (confirmed the hard way in
# scripts/generate_whittaker_overlays.R's own development; same pattern
# reused here verbatim).
run_pipeline <- function() {

  # ---- Step: load config / constants / figure functions ----------------------
  rl("attempting: check for .env, R/pipeline_config.R, R/plot_constants.R, R/figures/fig_climate.R")
  if (file.exists(".env")) {
    rl("found: .env at .env")
    suppressMessages(library(dotenv))
    dotenv::load_dot_env()
  } else {
    rl("not found (not required): .env at .env -- proceeding without it (Codespace Secrets path)")
  }
  for (req_path in c("R/pipeline_config.R", "R/plot_constants.R", "R/figures/fig_climate.R")) {
    if (!file.exists(req_path)) {
      rl("MISSING REQUIRED INPUT: ", req_path, " -- checked exact path '", req_path,
         "', not found. Not substituting -- stopping.")
      stop("Required file not found: ", req_path, call. = FALSE)
    }
    rl("found: ", req_path)
  }
  source("R/pipeline_config.R")
  source("R/plot_constants.R")
  source("R/figures/fig_climate.R")
  suppressMessages({
    library(dplyr); library(ggplot2); library(colorspace); library(duckdb); library(readr)
  })
  rl("completed: sourced R/pipeline_config.R, R/plot_constants.R, R/figures/fig_climate.R")

  rl("attempting: check_pipeline_config()")
  check_pipeline_config()
  rl("completed: check_pipeline_config() ran (see console/stdout above for its own warnings, ",
     "not duplicated into this run log)")

  # ---- Step: verify the new fig_whittaker_worldclim() parameters this script depends on ----
  rl("attempting: verify fig_whittaker_worldclim() exposes point_colour, point_alpha, ",
     "nee_mid_colour, detail_lines (added earlier this session to R/figures/fig_climate.R)")
  required_params <- c("point_colour", "point_alpha", "nee_mid_colour", "detail_lines",
                        "detail_hjust", "detail_x_offset")
  have_params <- names(formals(fig_whittaker_worldclim))
  missing_params <- setdiff(required_params, have_params)
  if (length(missing_params) > 0L) {
    rl("MISSING REQUIRED FUNCTION PARAMETERS on fig_whittaker_worldclim() (R/figures/fig_climate.R): ",
       paste(missing_params, collapse = ", "),
       " -- checked formals(fig_whittaker_worldclim). Not substituting -- stopping.")
    stop("fig_whittaker_worldclim() is missing required parameter(s): ",
         paste(missing_params, collapse = ", "), call. = FALSE)
  }
  rl("found: fig_whittaker_worldclim() has all required parameters (", paste(required_params, collapse = ", "), ")")

  # ---- Step: reuse the cached global density surface (same as the base ALT / production overlay) ----
  cache_dir       <- "data/processed"
  densitygrid_rds <- file.path(cache_dir, "whittaker_global_density_grid.rds")
  rl("attempting: locate cached density_grid at ", densitygrid_rds)
  if (!file.exists(densitygrid_rds)) {
    rl("MISSING REQUIRED INPUT: ", densitygrid_rds,
       " -- checked exact path '", densitygrid_rds, "'. Not substituting (would require ",
       "recomputing from source rasters, out of scope for this update script) -- stopping.")
    stop("Missing cache: ", densitygrid_rds, call. = FALSE)
  }
  rl("found: ", densitygrid_rds)
  density_grid <- readRDS(densitygrid_rds)
  rl("completed: loaded density_grid (", length(density_grid$xbin), "x", length(density_grid$ybin), ")")

  # ---- Step: load Figure 2 inputs (identical sources to generate_whittaker_overlays.R) ----
  db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
  rl("attempting: locate DuckDB database at ", db_path)
  if (!file.exists(db_path)) {
    rl("MISSING REQUIRED INPUT: ", db_path, " -- checked exact path '", db_path,
       "'. Not substituting -- stopping.")
    stop("DuckDB database not found: ", db_path, call. = FALSE)
  }
  rl("found: ", db_path)
  con <- dbConnect(duckdb(), db_path, read_only = TRUE)
  if (!"annual_converted" %in% dbListTables(con)) {
    dbDisconnect(con)
    rl("MISSING REQUIRED INPUT: table 'annual_converted' -- checked dbListTables(", db_path,
       "). Not substituting -- stopping.")
    stop("annual_converted table missing.", call. = FALSE)
  }
  rl("found: table 'annual_converted' in ", db_path)
  data_yy <- dplyr::tbl(con, "annual_converted") |>
    dplyr::filter(dataset == "FLUXMET") |>
    dplyr::select(site_id, TIMESTAMP, NEE_VUT_REF, NEE_CUT_REF) |>
    dplyr::collect() |>
    dplyr::mutate(YEAR = as.integer(TIMESTAMP))
  dbDisconnect(con)
  rl("computed: data_yy has ", format(nrow(data_yy), big.mark = ","),
     " rows (dataset == 'FLUXMET', site_id/TIMESTAMP/NEE_VUT_REF/NEE_CUT_REF)")

  snap_dir    <- file.path(FLUXNET_DATA_ROOT, "snapshots")
  rl("attempting: locate fluxnet_shuttle_snapshot*.csv under ", snap_dir)
  snap_files <- sort(
    list.files(snap_dir, pattern = "fluxnet_shuttle_snapshot.*\\.csv$", full.names = TRUE),
    decreasing = TRUE
  )
  if (length(snap_files) == 0L) {
    rl("MISSING REQUIRED INPUT: no fluxnet_shuttle_snapshot*.csv -- checked directory '",
       snap_dir, "'. Not substituting -- stopping.")
    stop("No Shuttle snapshot CSV found.", call. = FALSE)
  }
  snap_file <- snap_files[[1]]
  rl("found: ", snap_file, " (most recent of ", length(snap_files), " snapshot file(s) checked)")
  shuttle_meta <- readr::read_csv(snap_file, show_col_types = FALSE)
  rl("computed: shuttle_meta has ", format(nrow(shuttle_meta), big.mark = ","), " sites")

  # ---- Step: the three inset counts, computed programmatically -----------------
  rl("attempting: compute the three inset counts (network total / NEE-bearing / site-years) ",
     "from data_yy + shuttle_meta -- same logic as fig_whittaker_worldclim()'s own n_sites/",
     "n_site_years internals, replicated here only to report the extra 'NEE-bearing sites' ",
     "count that function does not itself expose")
  site_ids <- unique(shuttle_meta$site_id)
  n_sites  <- length(site_ids)  # network total -- matches fig_whittaker_worldclim()'s n_sites

  data_filt <- dplyr::filter(data_yy, .data$site_id %in% site_ids) |>
    dplyr::mutate(NEE_ref = dplyr::coalesce(.data$NEE_VUT_REF, .data$NEE_CUT_REF))
  site_nee <- data_filt |>
    dplyr::filter(!is.na(.data$NEE_ref)) |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(n_nee_years = dplyr::n_distinct(.data$YEAR), .groups = "drop")

  n_nee_sites  <- nrow(site_nee)               # sites with >=1 qualifying NEE_VUT_REF/NEE_CUT_REF
  n_site_years <- sum(site_nee$n_nee_years)     # matches fig_whittaker_worldclim()'s n_site_years

  rl("computed: n_sites (network total) = ", n_sites)
  rl("computed: n_nee_sites (>=1 qualifying NEE_VUT_REF or NEE_CUT_REF) = ", n_nee_sites)
  rl("computed: n_site_years (sum of qualifying-year counts across n_nee_sites) = ", n_site_years)
  rl("computed: sites with NO qualifying NEE (n_sites - n_nee_sites) = ", n_sites - n_nee_sites,
     " -- remainder is sites with no annual_converted row at all, or NEE NA on both VUT and CUT")

  inset_lines <- c(
    paste0(n_sites, " sites total"),
    paste0(n_nee_sites, " with annual NEE"),
    paste0(n_site_years, " site-years")
  )
  rl("computed: inset_lines = c(\"", paste(inset_lines, collapse = "\", \""), "\")")

  # ---- Step: styling constants for this update -----------------------------------
  point_colour_new <- "#2B2B2B"   # quiet dark charcoal (vs. prior "grey30" = "#4D4D4D")
  point_size_new   <- 0.35
  point_alpha_new  <- 0.35        # reduced from 0.50 so dense clusters build tone without
                                   # individual temperate-cloud points blobbing together
  nee_mid_new      <- "#F0E2C4"   # pale tan (HCL ~ H90/C26/L67); blue/red ends unchanged --
                                   # see fig_whittaker_worldclim()'s nee_mid_colour docs
  rl("computed: point_colour = '", point_colour_new, "' (was 'grey30'), point_size = ",
     point_size_new, " (was 0.7 in the base ALT / 1.4 in the original), point_alpha = ",
     point_alpha_new, " (was 0.50)")
  rl("computed: nee_mid_colour = '", nee_mid_new, "' (was near-white '#F6F6F6'); ",
     "blue/red endpoints pinned to diverging_hcl(2,'Blue-Red 3') = '#002F70'/'#5F1415', unchanged")

  # Plain numeric margin components (t,r,b,l in pt), defined once and reused for both the
  # ggplot2::margin() objects below AND the run-log line -- avoids the run log going stale
  # relative to the code (an earlier version hardcoded the log message separately from the code
  # and it drifted: still said legend_pos = 0.855 after the code had already been corrected to
  # 0.815; a later attempt to log the live margin objects directly hit a different problem --
  # deparse() on a ggplot2::margin() object dumps its entire underlying S7 class definition,
  # hundreds of words, unreadable -- so the plain numeric vectors are logged instead, below).
  legend_margin_trbl       <- c(t = 0, r = 2, b = 0, l = 2)
  legend_title_margin_trbl <- c(t = 0, r = 0, b = 0, l = 0)
  legend_box_margin_trbl   <- c(t = 0, r = 0, b = 0, l = 0)
  detail_x_offset_new      <- 0.6              # MAT data units; xlim = c(-15,35) (50-unit range)
                                                # -- shifts the inset text block right by this
                                                # much so it clears the left axis border with a
                                                # clean margin instead of touching/overlapping it
                                                # (detail_hjust=0 gives the text zero inherent
                                                # margin at x=-Inf). Found by rendering and
                                                # zooming into the axis/text corner at 1.7, 1.0,
                                                # 0.6, and 0.3 data units in turn: 1.7 and 1.0 both
                                                # cleared comfortably; 0.3 left a visibly thin,
                                                # marginal gap; 0.6 is the smallest of the four
                                                # tried that still reads as a clean, unambiguous
                                                # margin, not a marginal one -- see run log.
  legend_pos_new           <- c(0.02 + detail_x_offset_new / diff(range(WHITTAKER_STYLE$xlim)), 0.815)
                                                # x: 0.02 -> 0.032 -- shifted right by the same
                                                # fraction of the x-axis range as detail_x_offset_new
                                                # above, so the legend/colorbar moves in step with
                                                # the text block (whole inset block as one unit).
                                                # y: was 0.88; first tried 0.80 (moved the legend
                                                # DOWN/away, widening the gap -- wrong direction:
                                                # legend.position.inside's y is the fraction from
                                                # the panel BOTTOM, so closing a gap to text
                                                # anchored near the panel TOP means INCREASING y),
                                                # then 0.855 (overshot: legend title overlapped/
                                                # obscured "4227 site-years", confirmed by
                                                # rendering and zooming into the inset region).
                                                # 0.815 sits the legend directly under the 4-line
                                                # block with a small, non-overlapping gap.

  style_3x3 <- utils::modifyList(WHITTAKER_STYLE, list(
    width_in            = 3.5,
    height_in           = 3.5,
    axis_text_size      = 7,
    axis_title_size     = 8,
    legend_text_size    = 6,
    legend_title_size   = 7,
    detail_text_size    = 2.3,
    colorbar_width      = grid::unit(1.3, "in"),
    colorbar_height     = grid::unit(0.10, "in"),
    # --- new in this update, all no-ops for every other style list/caller ---
    detail_lineheight   = 0.92,                  # was unset -> ggplot2 default 1.2
    legend_pos          = legend_pos_new,
    legend_margin       = do.call(ggplot2::margin, c(as.list(legend_margin_trbl), unit = "pt")),
    legend_title_margin = do.call(ggplot2::margin, c(as.list(legend_title_margin_trbl), unit = "pt")),
    legend_box_margin   = do.call(ggplot2::margin, c(as.list(legend_box_margin_trbl), unit = "pt"))
  ))
  rl("computed: style_3x3$detail_lineheight = ", style_3x3$detail_lineheight,
     ", style_3x3$legend_pos = c(", paste(legend_pos_new, collapse = ", "), ") (x: 0.02 -> ",
     round(legend_pos_new[1], 4), ", y unchanged at 0.815 from the prior revision)",
     ", style_3x3$legend_margin (t,r,b,l pt) = c(", paste(legend_margin_trbl, collapse = ", "), ")",
     ", style_3x3$legend_title_margin (t,r,b,l pt) = c(", paste(legend_title_margin_trbl, collapse = ", "), ")",
     ", style_3x3$legend_box_margin (t,r,b,l pt) = c(", paste(legend_box_margin_trbl, collapse = ", "), ")",
     ", detail_x_offset = ", detail_x_offset_new, " MAT data units (new this revision -- fixes",
     " the inset text clipping the left axis)",
     " -- see R/figures/fig_climate.R's .whittaker_theme() for how the legend_* style fields are",
     " consumed (no-op when absent)")

  # ---- Step: base layer -----------------------------------------------------------
  rl("attempting: build base plot via fig_whittaker_worldclim(hex_regular=TRUE, ",
     "points_in_front=TRUE, point_size=", point_size_new, ", point_colour='", point_colour_new,
     "', point_alpha=", point_alpha_new, ", nee_mid_colour='", nee_mid_new,
     "', detail_lines=inset_lines, detail_hjust=0, detail_x_offset=", detail_x_offset_new, ")")
  fig2_update_base <- fig_whittaker_worldclim(
    data_yy         = data_yy,
    site_meta       = shuttle_meta,
    detail_label    = "FLUXNET Shuttle 2025",
    style           = style_3x3,
    hex_regular     = TRUE,
    points_in_front = TRUE,
    point_size      = point_size_new,
    point_colour    = point_colour_new,
    point_alpha     = point_alpha_new,
    nee_mid_colour  = nee_mid_new,
    detail_lines    = inset_lines,
    detail_hjust    = 0,
    detail_x_offset = detail_x_offset_new
  )
  rl("completed: fig2_update_base built")

  # ---- Step: contour overlay -- identical geometry/method to the base ALT / production overlay ----
  rl("attempting: fig_whittaker_global_contour(density_grid=density_grid) for contour_df (95%/99% HDR)")
  contour_result <- fig_whittaker_global_contour(style = style_3x3, probs = c(0.95, 0.99),
                                                  density_grid = density_grid)
  contour_df <- contour_result$contour_df
  contour_df$prob_label <- droplevels(contour_df$prob_label)
  line_map <- c("95%" = "solid", "99%" = "dashed")
  rl("completed: contour_df has ", nrow(contour_df), " rows, probs present: ",
     paste(sort(unique(contour_df$prob), decreasing = TRUE), collapse = ", "))

  fig2_update <- fig2_update_base +
    ggplot2::geom_path(
      data = contour_df,
      ggplot2::aes(x = .data$x, y = .data$y,
                   group = interaction(.data$prob, .data$piece),
                   linetype = .data$prob_label),
      colour      = "black",
      linewidth   = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      values = line_map[levels(contour_df$prob_label)],
      guide  = "none"
    )
  rl("completed: contour layer added (same fig_whittaker_global_contour() call, same ",
     "density_grid, as the base ALT and production fig_02 -- registration unaffected by the ",
     "point/colour/inset changes above since they share this plot object's coordinate system)")

  # ---- Step: write the PNG ---------------------------------------------------------
  out_fig <- file.path(out_dir, "ALT_fig_02_whittaker_current.png")
  rl("attempting: ggsave(", out_fig, ", width=", style_3x3$width_in, ", height=",
     style_3x3$height_in, ", dpi=300, bg='white')")
  ggplot2::ggsave(out_fig, fig2_update, width = style_3x3$width_in, height = style_3x3$height_in,
                  units = "in", dpi = 300, bg = "white")
  rl("completed: wrote ", out_fig)

  # ---- Step: write the legend/caption .txt ------------------------------------------
  out_txt <- file.path(out_dir, "ALT_fig_02_whittaker_current.txt")
  rl("attempting: writeLines(...) to ", out_txt)
  writeLines(c(
    "ALT Figure 2 (updated) -- charcoal points, pale-tan NEE centre, tightened inset",
    "",
    "Candidate only -- NOT promoted to review/figures/draft_manuscript_v1/. Revision of the",
    "ALT_fig_02_whittaker_current.png built earlier this session (commit 505d130), which fixed",
    "hexagon regularity and points-in-front/half-size; those two changes are unchanged here",
    "(hex_regular = TRUE, points_in_front = TRUE). This revision changes point styling, the NEE",
    "colour-scale centre, and the inset text/spacing. Same 95% solid / 99% dashed global",
    "ice-free-land HDR contour overlay as fig_02 and the prior ALT -- geometry and registration",
    "unchanged.",
    "",
    "1. POINT STYLING: per-site points recoloured from grey30 to a quiet dark charcoal",
    paste0("   (", point_colour_new, "), resized from the prior ALT's 0.7 to ", point_size_new,
           " (~half again), and"),
    paste0("   reduced to alpha = ", point_alpha_new,
           " so dense clusters build tone while individual points in the"),
    "   temperate cloud do not blob together -- points read as fine texture for network",
    "   location/density, subordinate to the NEE hexbin colour. point_colour and point_alpha",
    "   are new backward-compatible fig_whittaker_worldclim() parameters (R/figures/",
    "   fig_climate.R), defaulting to the unchanged prior values (grey30 / 0.50) for every",
    "   other caller.",
    "",
    "2. NEE COLOUR SCALE: centre recoloured from near-white (#F6F6F6) to a pale tan",
    paste0("   (", nee_mid_new, ") so near-zero hexes hold their edges against the white panel."),
    "   Blue and red endpoints, zero-centring, colour limits, and squish out-of-bounds handling",
    "   are unchanged -- pinned to diverging_hcl(2, \"Blue-Red 3\")'s exact endpoint hex codes",
    "   (#002F70 / #5F1415). Implemented via the new nee_mid_colour parameter, which rebuilds",
    "   the fill scale as ggplot2::scale_fill_gradient2() (Lab-space interpolation) instead of",
    "   the stock colorspace::scale_fill_continuous_diverging() HCL interpolation -- a",
    "   same-endpoints custom HCL two-half reconstruction (matching diverging_hcl's own",
    "   interpolation exactly) was tried first and rejected: sweeping hue from blue (H=255) to",
    "   an off-hue tan centre (H~90) passes through green/teal partway, visible here because the",
    "   centre keeps colour (chroma ~26) rather than dropping to achromatic white the way the",
    "   stock palette's centre does. nee_mid_colour defaults to NULL (unchanged stock scale) for",
    "   every other caller.",
    "",
    "3. INSET WORDING (fixed): the previous \"N = <n> sites | <n> site-years\" line conflated two",
    "   different counts -- <n> sites was the full network snapshot count, computed before any",
    "   NEE filtering, while <n> site-years came from a separately NEE-filtered subset (see this",
    "   session's earlier \"775 vs 638 sites\" finding, SESSION_LOG.md 2026-08-31). Relabelled to",
    "   three lines, computed programmatically at run time from the same data used to build the",
    "   figure (see RUN_LOG_alt_fig02_update.txt for the exact values this run computed):",
    paste0("     \"", inset_lines[1], "\"  -- full current Shuttle snapshot site count"),
    paste0("     \"", inset_lines[2], "\"  -- sites with >=1 qualifying NEE_VUT_REF or NEE_CUT_REF"),
    paste0("     \"", inset_lines[3], "\"  -- sum of qualifying years across those sites"),
    paste0("   (", n_sites - n_nee_sites, " sites lack qualifying NEE: some have no annual_",
           "converted row at all, others have NEE = NA on both VUT and CUT.) Per-site points"),
    "   remain shown for ALL network sites regardless of NEE availability (points are",
    "   deliberately not gated on NEE) -- only the hexbin colouring is NEE-only. Implemented via",
    "   the new detail_lines parameter (character vector, one line each, appended after",
    "   detail_label); defaults to NULL (unchanged single auto-built line) for every other",
    "   caller.",
    "",
    "4. INSET/LEGEND SPACING (tightened): inter-line spacing within the inset text block reduced",
    "   (style$detail_lineheight = 0.92 vs. ggplot2's default 1.2), the legend/colorbar position",
    "   nudged up under the now-4-line block (style$legend_pos = c(0.02, 0.80), was c(0.02,",
    "   0.88)), and padding around the colorbar reduced (style$legend_margin, style$",
    "   legend_title_margin). All three are new optional style-list fields read by",
    "   R/figures/fig_climate.R's .whittaker_theme() / detail-text annotate() call, each a no-op",
    "   when absent -- every other style list (WHITTAKER_STYLE and all existing overrides) is",
    "   unaffected. The block stays anchored in the upper-left (unchanged x=-Inf/y=Inf anchor);",
    "   only its own internal and surrounding spacing changed.",
    "",
    "Data sources: identical to fig_02_whittaker_current.png and the prior ALT -- see",
    "review/figures/whittaker/fig_whit_fig2_with_both_contours.legend.txt for the full method",
    "(WorldClim v2.1, ESA CCI land cover 2015, DuckDB annual_converted FLUXMET, Shuttle",
    "snapshot, Mahalanobis coverage stat).",
    paste0("Shuttle snapshot: ", snap_file, " (", n_sites, " sites)."),
    "",
    "Source script: scripts/generate_whittaker_alt_fig02_update.R (successor to",
    "scripts/generate_whittaker_alt_fig02.R)",
    "Function changes: fig_whittaker_worldclim(), .whittaker_theme() in R/figures/fig_climate.R",
    "Run log: review/figures/candidates/RUN_LOG_alt_fig02_update.txt",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  ), out_txt)
  rl("completed: wrote ", out_txt)

  invisible(TRUE)
}

pipeline_error <- tryCatch({
  run_pipeline()
  NULL
}, error = function(e) e)

if (is.null(pipeline_error)) {
  rl("outcome: SUCCESS -- ALT_fig_02_whittaker_current.png (+.txt) updated in ", out_dir, ".")
  message("\nDone: ALT_fig_02_whittaker_current.png and .txt written to ", out_dir)
} else {
  rl("ERROR: ", conditionMessage(pipeline_error))
  rl("outcome: FAILED -- see the last 'attempting'/'MISSING' line above for what was in progress ",
     "when the script stopped.")
}
close(run_log_con)
if (!is.null(pipeline_error)) stop(pipeline_error)
