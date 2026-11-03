## write_meta.R — shared output-metadata/caption helper for every WAFNET side
## analysis (energy_partitioning/, fluxcom_comparison/). Sourced once by each
## subproject's own code/00_config.R, not run directly.
##
## NOT R/utils.R::write_output_metadata() -- that function always appends a
## sessionInfo() block to the repo-root outputs/session_info.txt, which no
## WAFNET side analysis may write to (same problem GSL_definition/code/00_config.R
## solved the same way, 2026-10-29 -- this mirrors that helper rather than
## adding a second repo-root behaviour change to R/utils.R). write_meta()
## writes the same fields write_output_metadata() does -- run_datetime_utc,
## pipeline_version, input_sources, notes -- minus r_session_info, which
## stays centralised at outputs/session_info.txt for the Annual Paper only.

#' Write a .meta.json companion for one WAFNET output file
#'
#' @param output_path Path to the output file (.csv, .png, ...); the
#'   metadata is written alongside it with the same base name and a
#'   .meta.json extension (CLAUDE.md "Output Metadata").
#' @param input_sources Character vector of the file(s)/table(s) this output
#'   was built from.
#' @param notes Free text: manual decisions, overrides, or deviations from
#'   defaults. Use "" (not omitted) when there are none -- the field must
#'   always be present, per CLAUDE.md.
#' @return Invisibly, the path written.
write_meta <- function(output_path, input_sources, notes = "") {
  if (!is.character(notes) || length(notes) != 1) {
    stop("write_meta(): notes must be a single character string (use \"\" if none).")
  }
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                                 error = function(e) NA_character_),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

#' Write a .legend.txt caption file alongside a figure
#'
#' One file per figure, same base name, .legend.txt extension. Caption text
#' is written as a journal figure caption (what is plotted, how to read it,
#' data/period/units/sign convention) -- not a results interpretation.
#'
#' @param figure_path Path to the figure file (e.g. .png).
#' @param caption Character scalar or vector; vector elements are joined
#'   with a blank line between them (paragraph breaks).
#' @return Invisibly, the path written.
write_caption <- function(figure_path, caption) {
  caption_path <- paste0(tools::file_path_sans_ext(figure_path), ".legend.txt")
  writeLines(paste(caption, collapse = "\n\n"), caption_path)
  invisible(caption_path)
}
