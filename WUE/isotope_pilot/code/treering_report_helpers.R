## treering_report_helpers.R — parsing helpers for tables/treering_inventory.csv
## and tables/treering_site_map.csv, sourced by 04_report_preanalysis.R.
##
## Kept separate from 04 because the edi.401 entity structure can only be
## known by actually fetching it (see 03_fetch_treering.R) -- this file is
## expected to be revised once real data has been inspected.

#' Map a tower ID to the common/park name this analysis expects to find it
#' under in edi.401, for the name/coordinate inference fallback when the EML
#' metadata does not state the mapping explicitly.
WUE_EXPECTED_SITE_NAMES <- data.frame(
  site_id = c("US-Ha1", "US-Ho2", "US-Bar", "US-MMS", "US-Slt", "US-Dk2", "US-SP1", "US-Fuf"),
  expected_name_fragment = c(
    "Harvard", "Howland", "Bartlett", "Morgan Monroe", "Silas Little",
    "Duke", "Austin Cary", "Flagstaff"
  ),
  mapping_basis_prior = c(
    "stated", "stated", "stated", "stated", "stated", "stated",
    "inferred (author list)", "inferred (author list)"
  ),
  stringsAsFactors = FALSE
)

#' Build tables/treering_inventory.csv from whatever was fetched into
#' data/external/treering/edi_401/.
build_treering_inventory <- function(edi_dir) {
  manifest_path <- file.path(edi_dir, "entity_manifest.csv")
  if (!file.exists(manifest_path)) {
    return(data.frame(
      site = character(0), species = character(0), variables_present = character(0),
      first_year = integer(0), last_year = integer(0), n_trees = integer(0),
      note = character(0)
    ))
  }
  entity_manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  data_files <- entity_manifest$filename[
    entity_manifest$downloaded &
      grepl("\\.csv$", entity_manifest$filename, ignore.case = TRUE)
  ]
  if (length(data_files) == 0) {
    return(data.frame(
      site = NA_character_, species = NA_character_, variables_present = NA_character_,
      first_year = NA_integer_, last_year = NA_integer_, n_trees = NA_integer_,
      note = "no downloaded CSV data entity found in edi_401/"
    ))
  }

  rows <- lapply(data_files, function(fname) {
    fpath <- file.path(edi_dir, fname)
    dat <- tryCatch(readr::read_csv(fpath, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(dat) || nrow(dat) == 0) {
      return(data.frame(site = NA_character_, species = NA_character_,
                         variables_present = fname, first_year = NA_integer_,
                         last_year = NA_integer_, n_trees = NA_integer_,
                         note = paste0("unreadable or empty: ", fname)))
    }
    cols <- names(dat)
    site_col    <- cols[grepl("^site", cols, ignore.case = TRUE)][1]
    species_col <- cols[grepl("species", cols, ignore.case = TRUE)][1]
    year_col    <- cols[grepl("^year$", cols, ignore.case = TRUE)][1]
    tree_col    <- cols[grepl("tree.?id|tree.?num", cols, ignore.case = TRUE)][1]

    group_keys <- Filter(Negate(is.na), c(site_col, species_col))
    if (length(group_keys) == 0) {
      return(data.frame(
        site = NA_character_, species = NA_character_,
        variables_present = paste(cols, collapse = "; "),
        first_year = if (!is.na(year_col)) min(dat[[year_col]], na.rm = TRUE) else NA_integer_,
        last_year  = if (!is.na(year_col)) max(dat[[year_col]], na.rm = TRUE) else NA_integer_,
        n_trees = if (!is.na(tree_col)) dplyr::n_distinct(dat[[tree_col]]) else NA_integer_,
        note = paste0("no site/species column detected in ", fname, " -- file-level summary only")
      ))
    }
    dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_keys))) |>
      dplyr::summarise(
        first_year = if (!is.na(year_col)) suppressWarnings(min(.data[[year_col]], na.rm = TRUE)) else NA_integer_,
        last_year  = if (!is.na(year_col)) suppressWarnings(max(.data[[year_col]], na.rm = TRUE)) else NA_integer_,
        n_trees    = if (!is.na(tree_col)) dplyr::n_distinct(.data[[tree_col]]) else NA_integer_,
        .groups = "drop"
      ) |>
      dplyr::mutate(
        variables_present = paste(cols, collapse = "; "),
        note = fname
      ) |>
      (\(d) {
        if (!"site" %in% names(d)) d$site <- NA_character_ else names(d)[names(d) == site_col] <- "site"
        if (!"species" %in% names(d)) d$species <- NA_character_
        d
      })()
  })
  out <- dplyr::bind_rows(rows)
  if (!"site" %in% names(out)) out$site <- NA_character_
  if (!"species" %in% names(out)) out$species <- NA_character_
  out[, c("site", "species", "variables_present", "first_year", "last_year", "n_trees", "note")]
}

#' Build tables/treering_site_map.csv — map each edi.401 site name to a tower
#' ID. `mapping_basis` is "stated" when the EML metadata itself names the
#' FLUXNET/AmeriFlux tower, "inferred" when this script matched on name/
#' coordinates instead.
build_treering_site_map <- function(edi_dir, wue_sites) {
  eml_files <- list.files(edi_dir, pattern = "\\.eml\\.xml$", full.names = TRUE)
  if (length(eml_files) == 0) {
    return(data.frame(
      edi_site_name = NA_character_, tower_id = wue_sites,
      mapping_basis = NA_character_, note = "no EML metadata file found in edi_401/ -- edi.401 fetch did not succeed"
    ))
  }
  eml <- xml2::read_xml(eml_files[1])
  site_nodes <- xml2::xml_find_all(eml, ".//site")
  site_names <- unique(trimws(xml2::xml_text(xml2::xml_find_all(eml, ".//siteName"))))
  if (length(site_names) == 0) {
    site_names <- unique(trimws(xml2::xml_text(xml2::xml_find_all(eml, ".//keyword"))))
  }

  rows <- lapply(wue_sites, function(tower) {
    expected <- WUE_EXPECTED_SITE_NAMES[WUE_EXPECTED_SITE_NAMES$site_id == tower, ]
    if (nrow(expected) == 0) {
      return(data.frame(edi_site_name = NA_character_, tower_id = tower,
                         mapping_basis = NA_character_,
                         note = "flux-only site -- not expected in edi.401 tree-ring data"))
    }
    hit <- site_names[grepl(expected$expected_name_fragment, site_names, ignore.case = TRUE)]
    if (length(hit) == 0) {
      return(data.frame(edi_site_name = NA_character_, tower_id = tower,
                         mapping_basis = "not found in EML site names",
                         note = paste0("expected fragment '", expected$expected_name_fragment,
                                        "' not matched -- verify manually")))
    }
    data.frame(edi_site_name = hit[1], tower_id = tower,
               mapping_basis = if (expected$mapping_basis_prior == "stated") "inferred from name match" else "inferred (name match + prior author-list inference)",
               note = "")
  })
  dplyr::bind_rows(rows)
}
