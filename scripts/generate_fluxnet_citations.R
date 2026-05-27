## scripts/generate_fluxnet_citations.R
## Generate BibTeX citations, acknowledgments, and review flags for a set of
## FLUXNET sites.  Reads from the snapshot CSV; routes by product_source_network.
##
## Usage:
##   source("scripts/generate_fluxnet_citations.R")
##   generate_fluxnet_citations(
##     site_ids       = c("US-MMS", "DE-Tha", "AU-How", "ZA-Jks"),
##     output_prefix  = "outputs/citations/fluxnet_2026"
##   )
##
## Outputs:
##   {prefix}.bib                   one @misc per site + mandated @article refs
##   {prefix}_acknowledgments.md    global + network-conditional acknowledgment texts
##   {prefix}_review_flags.md       sites needing human attention before submission

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

library(dplyr)
library(readr)
library(stringr)

# ---- Constants ---------------------------------------------------------------

ICOS_NETWORKS  <- c("CNF", "EUF", "FLX", "ICOS", "JPF", "KOF")
KNOWN_NETWORKS <- c("AMF", ICOS_NETWORKS, "TERN", "SAEON")

# ---- BibTeX helpers ----------------------------------------------------------

# Escape LaTeX control characters; preserve UTF-8 (diacritics, en-dashes stay).
bib_escape <- function(s) {
  s <- gsub("\\\\", "\\\\textbackslash{}", s)
  s <- gsub("&",    "\\&",    s, fixed = TRUE)
  s <- gsub("%",    "\\%",    s, fixed = TRUE)
  s <- gsub("\\$",  "\\$",    s)
  s <- gsub("#",    "\\#",    s, fixed = TRUE)
  s <- gsub("_",    "\\_",    s, fixed = TRUE)
  s <- gsub("\\^",  "\\^{}",  s)
  s <- gsub("~",    "\\~{}",  s, fixed = TRUE)
  s
}

bib_field <- function(key, value, indent = "  ") {
  sprintf("%s%-14s= {%s}", indent, key, value)
}

format_misc_entry <- function(key, author, title, year,
                               doi = NULL, url = NULL, note = "Dataset") {
  fields <- character(0)
  if (!is.na(author) && nzchar(author)) {
    fields <- c(fields, bib_field("author", sprintf("{%s}", bib_escape(author))))
  }
  fields <- c(fields, bib_field("title",  sprintf("{%s}", bib_escape(title))))
  fields <- c(fields, bib_field("year",   year))
  if (!is.null(doi) && !is.na(doi) && nzchar(doi))
    fields <- c(fields, bib_field("doi",  doi))
  if (!is.null(url) && !is.na(url) && nzchar(url))
    fields <- c(fields, bib_field("url",  url))
  fields <- c(fields, bib_field("note",  note))
  paste0("@misc{", key, ",\n", paste(fields, collapse = ",\n"), "\n}\n")
}

format_article_entry <- function(key, author, title, journal, year,
                                  volume, pages, doi) {
  fields <- c(
    bib_field("author",  sprintf("{%s}", author)),
    bib_field("title",   sprintf("{%s}", bib_escape(title))),
    bib_field("journal", sprintf("{%s}", journal)),
    bib_field("year",    year),
    bib_field("volume",  volume),
    bib_field("pages",   pages),
    bib_field("doi",     doi)
  )
  paste0("@article{", key, ",\n", paste(fields, collapse = ",\n"), "\n}\n")
}

# ---- Per-family parsers ------------------------------------------------------
# All parsers return list(authors, year, title, doi=NULL, url=NULL).
# Authors may be NA for no-author ICOS sites.  Errors propagate to the caller.

parse_amf <- function(cit, pid) {
  # Structure: {authors} ({year}), AmeriFlux {title}. https://doi.org/{doi}
  parts <- strsplit(cit, ". https://doi.org/", fixed = TRUE)[[1]]
  if (length(parts) < 2L) stop("AMF: no DOI separator in: ", cit)
  m <- str_match(parts[[1]], "^(.*) \\((\\d{4})\\), (AmeriFlux .+)$")
  if (is.na(m[[1]])) stop("AMF: cannot parse author/year/title from: ", parts[[1]])
  list(authors = m[[2]], year = m[[3]], title = m[[4]], doi = pid)
}

parse_icos <- function(cit, pid) {
  # Structure: [{authors} ]({pub_year}). {title}, FLUXNET, https://hdl.handle.net/11676/{handle}
  # Normalize double-apostrophe encoding artifact (5 UK sites: D''Acunha → D'Acunha)
  cit_norm <- gsub("''", "'", cit, fixed = TRUE)
  parts    <- strsplit(cit_norm, ", https://", fixed = TRUE)[[1]]
  if (length(parts) < 2L) stop("ICOS: no URL separator in: ", cit)
  lhs <- parts[[1]]
  url <- paste0("https://hdl.handle.net/11676/", pid)

  if (startsWith(lhs, "(")) {
    # No-author: citation begins with (year).
    m <- str_match(lhs, "^\\(([^)]+)\\)\\. (.+)$")
    if (is.na(m[[1]])) stop("ICOS no-author: cannot parse year/title from: ", lhs)
    list(
      authors = NA_character_,
      year    = sub("^(\\d{4}).*", "\\1", m[[2]]),
      title   = m[[3]],
      url     = url
    )
  } else {
    m <- str_match(lhs, "^(.*) \\(([^)]+)\\)\\. (.+)$")
    if (is.na(m[[1]])) stop("ICOS: cannot parse author/year/title from: ", lhs)
    list(
      authors = m[[2]],
      year    = sub("^(\\d{4}).*", "\\1", m[[3]]),
      title   = m[[4]],
      url     = url
    )
  }
}

parse_tern <- function(cit, pid) {
  # Structure: {authors} ({year}): {title}[ ]. Version {ver}. TERN. (Dataset).
  # The " ." space-before-period variant (36 sites) is handled by \s*\.\s*Version\s+
  parts <- str_split_fixed(cit, "\\s*\\.\\s*Version\\s+", n = 2L)
  if (!nzchar(parts[1L, 2L])) stop("TERN: no Version boundary in: ", cit)
  m <- str_match(parts[1L, 1L], "^(.+) \\((\\d{4})\\): (.+)$")
  if (is.na(m[[1]])) stop("TERN: cannot parse author/year/title from: ", parts[1L, 1L])
  list(
    authors = str_trim(m[[2]]),
    year    = m[[3]],
    title   = str_squish(m[[4]]),   # collapse internal double-spaces + trim
    doi     = sub("^https://dx\\.doi\\.org/", "", pid)
  )
}

parse_saeon <- function(cit, pid) {
  # Structure: {authors} ({year}). Fluxnet Archive Product from {site}, {range}, FLUXNET, https://...
  parts <- strsplit(cit, ", https://", fixed = TRUE)[[1]]
  if (length(parts) < 2L) stop("SAEON: no URL separator in: ", cit)
  m <- str_match(parts[[1]], "^(.*) \\((\\d{4})\\)\\. (.+)$")
  if (is.na(m[[1]])) stop("SAEON: cannot parse author/year/title from: ", parts[[1]])
  list(
    authors = m[[2]],
    year    = m[[3]],
    title   = m[[4]],
    url     = paste0("https://hdl.handle.net/11676/", pid)
  )
}

# ---- Cite key ----------------------------------------------------------------

make_cite_key <- function(site_id, network, year) {
  base <- paste(site_id, network, year, sep = "_")
  gsub("[^A-Za-z0-9_:.-]", "_", base)
}

# ---- Build a single BibTeX entry ---------------------------------------------
# Returns list(site_id, bib = character or NULL, flag = character or NULL)

build_bib_entry <- function(row) {
  sid  <- row[["site_id"]]
  cit  <- row[["product_citation"]]
  pid  <- row[["product_id"]]
  net  <- row[["product_source_network"]]
  flag <- NULL

  # Hard guard: blank fields
  if (is.na(cit) || !nzchar(trimws(cit)))
    stop(sprintf("GUARD: site %s has empty product_citation", sid))
  if (is.na(pid) || !nzchar(trimws(pid)))
    stop(sprintf("GUARD: site %s has empty product_id", sid))
  if (!net %in% KNOWN_NETWORKS)
    stop(sprintf("GUARD: site %s has unknown product_source_network '%s'", sid, net))

  # Structural pattern guard (product_id)
  if (net == "AMF"  && !grepl("^10\\.17190/AMF/", pid))
    flag <- sprintf("product_id fails AMF DOI pattern: %s", pid)
  if (net == "TERN" && !grepl("^https://dx\\.doi\\.org/10\\.25901/", pid))
    flag <- sprintf("product_id fails TERN DOI URL pattern: %s", pid)

  parsed <- tryCatch(
    {
      if      (net == "AMF")              parse_amf(cit, pid)
      else if (net %in% ICOS_NETWORKS)    parse_icos(cit, pid)
      else if (net == "TERN")             parse_tern(cit, pid)
      else                                parse_saeon(cit, pid)
    },
    error = function(e) {
      flag <<- paste("PARSE ERROR:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(parsed)) return(list(site_id = sid, bib = NULL, flag = flag))

  # Per-site preserved-verbatim flags
  if (net %in% ICOS_NETWORKS && is.na(parsed$authors))
    flag <- "No author block in citation (author field omitted from BibTeX entry)"
  if (sid == "AU-Cow" && grepl("FLUXNEXT", cit))
    flag <- "Title contains 'FLUXNEXT' (typo for 'FLUXNET') — preserved from upstream source"
  if (sid == "US-Hsm" && grepl("Kyle_Delwiche", cit))
    flag <- "Author 'Kyle_Delwiche' contains underscore — preserved; escaped as Kyle\\_Delwiche in BibTeX"

  key   <- make_cite_key(sid, net, parsed$year)
  entry <- format_misc_entry(key, parsed$authors, parsed$title, parsed$year,
                              doi = parsed$doi, url = parsed$url)
  list(site_id = sid, bib = entry, flag = flag)
}

# ---- Mandated references (appended to every .bib) ----------------------------

mandated_bib <- function(networks) {
  refs <- list()

  # Always: Pastorello 2020
  refs$p2020 <- format_article_entry(
    key     = "Pastorello2020",
    author  = "Pastorello, G. and others",
    title   = "The FLUXNET2015 dataset and the ONEFlux processing pipeline for eddy covariance data",
    journal = "Scientific Data",
    year    = "2020", volume = "7", pages = "225",
    doi     = "10.1038/s41597-020-0534-3"
  )

  # TERN present: Beringer 2016 + Isaac 2017
  if ("TERN" %in% networks) {
    refs$b2016 <- format_article_entry(
      key     = "Beringer2016",
      author  = "Beringer, J. and others",
      title   = "An introduction to the Australian and New Zealand flux tower network -- OzFlux",
      journal = "Biogeosciences",
      year    = "2016", volume = "13", pages = "5895--5916",
      doi     = "10.5194/bg-13-5895-2016"
    )
    refs$i2017 <- format_article_entry(
      key     = "Isaac2017",
      author  = "Isaac, P. and others",
      title   = "OzFlux Data: Network integration from collection to curation",
      journal = "Earth System Science Data",
      year    = "2017", volume = "9", pages = "349--361",
      doi     = "10.5194/essd-9-349-2017"
    )
  }

  # JPF present: Ueyama 2025 + Yu 2006
  if ("JPF" %in% networks) {
    refs$u2025 <- format_article_entry(
      key     = "Ueyama2025",
      author  = "Ueyama, M. and others",
      title   = "AsiaFlux dataset of ecosystem carbon, water, and energy fluxes",
      journal = "Earth System Science Data",
      year    = "2025", volume = "17", pages = "3807--3831",
      doi     = "10.5194/essd-17-3807-2025"
    )
    refs$y2006 <- format_article_entry(
      key     = "Yu2006",
      author  = "Yu, G.-R. and others",
      title   = "Overview of ChinaFLUX and evaluation of its eddy covariance measurement",
      journal = "Agricultural and Forest Meteorology",
      year    = "2006", volume = "137", pages = "125--137",
      doi     = "10.1016/j.agrformet.2006.02.011"
    )
  }

  paste(unlist(refs), collapse = "\n")
}

# ---- Acknowledgments ---------------------------------------------------------

build_acknowledgments <- function(networks) {
  lines <- c(
    "# FLUXNET Data Acknowledgments",
    "",
    "## Global",
    "",
    "The FLUXNET data used in this study were collected and quality-controlled by site",
    "principal investigators and processed through the ONEFlux pipeline (Pastorello et",
    "al. 2020, doi:10.1038/s41597-020-0534-3). Per-site DOIs are listed in the",
    "supplementary material. We acknowledge the FLUXNET Coordination Project and all",
    "contributing site PIs for making these data openly available under CC-BY-4.0.",
    ""
  )

  if ("AMF" %in% networks) {
    lines <- c(lines,
      "## AmeriFlux",
      "",
      "Funding for AmeriFlux data resources was provided by the U.S. Department of",
      "Energy's Office of Science.",
      ""
    )
  }

  if (any(c("EUF", "ICOS", "FLX") %in% networks)) {
    lines <- c(lines,
      "## ICOS",
      "",
      "The authors acknowledge the ICOS Research Infrastructure, its National Networks,",
      "and the ICOS Carbon Portal for data sharing (https://www.icos-cp.eu/).",
      ""
    )
  }

  if (any(c("CNF", "KOF") %in% networks)) {
    lines <- c(lines,
      "## ChinaFlux / KoFlux",
      "",
      "The authors acknowledge the ChinaFLUX and KoFlux networks and their contributing",
      "site PIs for data access.",
      ""
    )
  }

  if ("JPF" %in% networks) {
    lines <- c(lines,
      "## AsiaFlux / Japan Flux Program",
      "",
      "The authors acknowledge the AsiaFlux/JPF network and its site PIs for data",
      "access. Cite Ueyama et al. (2025, doi:10.5194/essd-17-3807-2025) and Yu et al.",
      "(2006, doi:10.1016/j.agrformet.2006.02.011) when using JPF data.",
      ""
    )
  }

  if ("TERN" %in% networks) {
    lines <- c(lines,
      "## TERN / OzFlux",
      "",
      "This research was enabled by data and infrastructure provided by the Terrestrial",
      "Ecosystem Research Network (TERN; https://www.tern.org.au), supported by the",
      "Australian Government's National Collaborative Research Infrastructure Strategy",
      "(NCRIS). Cite Beringer et al. (2016, doi:10.5194/bg-13-5895-2016) and Isaac",
      "et al. (2017, doi:10.5194/essd-9-349-2017) when using OzFlux/TERN data.",
      ""
    )
  }

  if ("SAEON" %in% networks) {
    lines <- c(lines,
      "## SAEON",
      "",
      "The authors acknowledge the South African Environmental Observation Network",
      "(SAEON; https://www.saeon.ac.za) for providing South African flux tower data.",
      ""
    )
  }

  lines <- c(lines,
    "## Data Availability Statement",
    "",
    "The FLUXNET data used in this study are available from the FLUXNET Shuttle at",
    "[SOURCE], accessed [DATE]. Per-site dataset DOIs and handle identifiers are listed",
    "in Supplementary Table S1. All data are distributed under CC-BY-4.0.",
    ""
  )

  paste(lines, collapse = "\n")
}

# ---- Review flags ------------------------------------------------------------

build_review_flags <- function(flags_named, sites_df) {
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  lines <- c(
    "# FLUXNET Citation Review Flags",
    "",
    paste("Generated:", ts),
    paste("Sites processed:", nrow(sites_df)),
    "",
    "Sites listed here need human attention before the bibliography is finalised.",
    ""
  )

  # No-author ICOS/JPF sites — those flagged in entries
  no_author_ids <- names(flags_named)[
    sapply(flags_named, function(f) grepl("No author block", f))
  ]
  if (length(no_author_ids) > 0L) {
    lines <- c(lines,
      "## No-author ICOS/JPF sites",
      "",
      sprintf(
        "%d site(s) have no author block in their product_citation (citation starts with",
        length(no_author_ids)
      ),
      "the year). The `author` field is omitted from their BibTeX entries. These sites",
      "should be verified against the upstream FLUXNET registry before submission.",
      "",
      paste("-", sort(no_author_ids), collapse = "\n"),
      ""
    )
  }

  # All per-site flags
  non_na_ids <- names(flags_named)[!sapply(flags_named, grepl, pattern = "No author block")]
  if (length(non_na_ids) > 0L) {
    lines <- c(lines, "## Per-site flags", "")
    for (sid in sort(non_na_ids)) {
      lines <- c(lines, paste0("- **", sid, "**: ", flags_named[[sid]]))
    }
    lines <- c(lines, "")
  }

  # Mandatory reference verification
  nets <- unique(sites_df$product_source_network)
  lines <- c(lines,
    "## Mandatory reference verification",
    "",
    "Verify these hardcoded references against the authoritative source before submission.",
    "",
    "- **Pastorello2020**: doi:10.1038/s41597-020-0534-3 — Sci. Data 7:225 — **confirmed**"
  )
  if ("TERN" %in% nets) {
    lines <- c(lines,
      "- **Beringer2016** (TERN sites present): doi:10.5194/bg-13-5895-2016 — Biogeosciences 13:5895 — verify",
      "- **Isaac2017** (TERN sites present): doi:10.5194/essd-9-349-2017 — ESSD 9:349 — verify"
    )
  }
  if ("JPF" %in% nets) {
    lines <- c(lines,
      "- **Ueyama2025** (JPF sites present): doi:10.5194/essd-17-3807-2025 — ESSD 17:3807 — **confirmed (given DOI)**",
      "- **Yu2006** (JPF sites present): doi:10.1016/j.agrformet.2006.02.011 — Agric. For. Met. 137:125 — **verify;",
      "  this is the ChinaFLUX paper — confirm it is the correct network attribution for all JPF sites**"
    )
  }
  lines <- c(lines, "")

  paste(lines, collapse = "\n")
}

# ---- Snapshot loader ---------------------------------------------------------

find_latest_snapshot <- function() {
  snaps <- sort(list.files(
    file.path(FLUXNET_DATA_ROOT, "snapshots"),
    pattern    = "fluxnet_shuttle_snapshot.*\\.csv$",
    full.names = TRUE
  ), decreasing = TRUE)
  if (length(snaps) == 0L)
    stop("No snapshot CSV found in ", file.path(FLUXNET_DATA_ROOT, "snapshots"))
  snaps[[1L]]
}

# ---- Main function -----------------------------------------------------------

#' Generate FLUXNET citations, acknowledgments, and review flags
#'
#' @param site_ids character vector of FLUXNET site IDs to cite
#' @param snapshot_path path to snapshot CSV; defaults to most recent committed snapshot
#' @param output_prefix file path prefix for outputs (no extension)
#' @return invisibly: list with paths bib, ack, flags, and summary counts
generate_fluxnet_citations <- function(
    site_ids,
    snapshot_path = find_latest_snapshot(),
    output_prefix
) {
  stopifnot(is.character(site_ids), length(site_ids) > 0L,
            is.character(output_prefix), length(output_prefix) == 1L)

  message("Snapshot: ", basename(snapshot_path))
  snap  <- readr::read_csv(snapshot_path, show_col_types = FALSE)
  sites <- snap |>
    dplyr::distinct(site_id, .keep_all = TRUE) |>
    dplyr::filter(site_id %in% site_ids)

  missing <- setdiff(site_ids, sites$site_id)
  if (length(missing) > 0L)
    stop("GUARD: requested site(s) not in snapshot: ", paste(missing, collapse = ", "))

  message("Processing ", nrow(sites), " site(s)")

  results    <- lapply(seq_len(nrow(sites)), function(i) build_bib_entry(as.list(sites[i, ])))
  bib_entries <- Filter(Negate(is.null), lapply(results, `[[`, "bib"))
  flags_raw   <- lapply(results, `[[`, "flag")
  names(flags_raw) <- sapply(results, `[[`, "site_id")
  flags_named <- Filter(Negate(is.null), flags_raw)

  networks <- sort(unique(sites$product_source_network))
  message("Networks: ", paste(networks, collapse = ", "))

  # Assemble .bib
  header <- paste(c(
    "%% FLUXNET site citations — generated by scripts/generate_fluxnet_citations.R",
    paste0("%% Generated : ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    paste0("%% Sites     : ", nrow(sites)),
    paste0("%% Networks  : ", paste(networks, collapse = ", ")),
    paste0("%% Source    : ", basename(snapshot_path)),
    "%%",
    "%% UTF-8 encoded; diacritics and en-dashes preserved as-is (requires",
    "%% \\usepackage[utf8]{inputenc} or LuaLaTeX/XeLaTeX in the LaTeX preamble).",
    "%% Author strings are verbatim from the FLUXNET data registry, wrapped in",
    "%% double-braces {{...}} to prevent BibTeX name-parsing and title-casing.",
    "%% See _review_flags.md for sites needing attention before submission.",
    "%%",
    "%% --- Site datasets ---"
  ), collapse = "\n")

  mandated_section <- paste0(
    "\n%% --- Mandated references ---\n\n",
    mandated_bib(networks)
  )

  bib_content <- paste0(
    header, "\n\n",
    paste(bib_entries, collapse = "\n"),
    mandated_section
  )

  dir.create(dirname(output_prefix), showWarnings = FALSE, recursive = TRUE)
  bib_path  <- paste0(output_prefix, ".bib")
  ack_path  <- paste0(output_prefix, "_acknowledgments.md")
  flag_path <- paste0(output_prefix, "_review_flags.md")

  writeLines(bib_content,                          bib_path,  useBytes = FALSE)
  writeLines(build_acknowledgments(networks),      ack_path,  useBytes = FALSE)
  writeLines(build_review_flags(flags_named, sites), flag_path, useBytes = FALSE)

  message("\nOutputs:")
  message("  ", bib_path)
  message("  ", ack_path)
  message("  ", flag_path)
  if (length(flags_named) > 0L)
    message("\n", length(flags_named), " review flag(s) — see ", basename(flag_path))

  invisible(list(
    bib      = bib_path,
    ack      = ack_path,
    flags    = flag_path,
    networks = networks,
    n_flags  = length(flags_named)
  ))
}
