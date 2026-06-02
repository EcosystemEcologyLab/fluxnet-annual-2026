## R/parse_bib_to_apa.R
## Parse a FLUXNET BibTeX file and write APA 7th Edition dataset citations.
##
## Usage (source then call):
##   source("R/parse_bib_to_apa.R")
##
##   # All sites — plain-text output (default)
##   bib_to_apa()
##
##   # Subset via character vector
##   bib_to_apa(site_ids = c("US-MMS", "DE-Tha", "AU-How"))
##
##   # Subset via CSV (any column named "site_id" by default)
##   bib_to_apa(site_ids_csv = "data/my_sites.csv")
##   bib_to_apa(site_ids_csv = "data/my_sites.csv", site_id_col = "SITE_ID")
##
##   # Word document output
##   bib_to_apa(output_format = "docx")
##   bib_to_apa(site_ids_csv = "data/my_sites.csv", output_format = "docx",
##              out_path = "~/Desktop/subset_citations.docx")
##
##   # Native Google Doc (requires one-time browser OAuth login)
##   bib_to_apa(output_format = "gdoc")
##   bib_to_apa(site_ids = c("US-MMS", "DE-Tha"), output_format = "gdoc",
##              out_path = "Subset Citations 2026", gdrive_path = "My Drive/Paper2026")

# ── Default paths ─────────────────────────────────────────────────────────────
.GDRIVE_ROOT <- path.expand(
  "~/Library/CloudStorage/GoogleDrive-setanta.research@gmail.com/My Drive"
)

# ── Low-level BibTeX field extractor ─────────────────────────────────────────

# Extract the value between the outermost braces on a single BibTeX field line.
# Handles both {value} and {{value}} — strips one inner brace layer if present.
.extract_field_value <- function(line) {
  line  <- trimws(sub(",\\s*$", "", line))
  start <- regexpr("\\{", line)
  if (start == -1L) return(NA_character_)
  end <- nchar(line)
  while (end > start && substr(line, end, end) != "}") end <- end - 1L
  val <- substr(line, start + 1L, end - 1L)
  if (grepl("^\\{.*\\}$", val)) val <- substr(val, 2L, nchar(val) - 1L)
  trimws(val)
}

# ── Author name formatting ────────────────────────────────────────────────────

# Convert one name in "First [Middle] Last" format to "Last, F. M." APA form.
.parse_first_last <- function(name) {
  name <- trimws(gsub("_", " ", name))
  if (!nzchar(name)) return("")
  if (grepl("\\(", name)) return(name)

  words <- strsplit(name, "\\s+")[[1]]
  n <- length(words)
  if (n == 0L) return("")
  if (n == 1L) return(words[[1L]])

  particles <- c(
    "von", "van", "de", "del", "della", "di", "da", "do",
    "das", "dos", "des", "du", "la", "le", "les", "los",
    "las", "el", "al", "af", "av", "e", "y", "zu", "zur"
  )

  surname_start <- n
  i <- n - 1L
  while (i >= 1L && tolower(words[[i]]) %in% particles) {
    surname_start <- i
    i <- i - 1L
  }

  surname <- paste(words[surname_start:n], collapse = " ")
  given   <- if (surname_start > 1L) words[seq_len(surname_start - 1L)] else character(0L)

  make_initial <- function(w) if (grepl("\\.$", w)) w else paste0(substr(w, 1L, 1L), ".")
  initials <- if (length(given) > 0L) paste(vapply(given, make_initial, ""), collapse = " ") else ""

  if (!nzchar(initials)) return(surname)
  paste0(surname, ", ", initials)
}

# Format a raw author string (outer braces already stripped) to APA author list.
#
# Two formats exist in FLUXNET BibTeX files:
#   AMF         — "First [Middle] Last, First Last, ..."  (needs conversion)
#   ICOS/TERN/+ — "Last, F., Last, F., ..."               (already APA-ready)
#
# Detection: split by ", " before a title-case word; if the first piece
# contains a comma the string is already in "Last, F." form.
.format_authors_apa <- function(raw) {
  if (is.na(raw) || !nzchar(trimws(raw))) return("[No author listed]")
  raw <- trimws(gsub("_", " ", raw))

  parts <- trimws(strsplit(raw, ",\\s+(?=[A-Z][a-z])", perl = TRUE)[[1]])

  if (!grepl(",", parts[[1L]], fixed = TRUE))
    parts <- vapply(parts, .parse_first_last, "")

  n <- length(parts)
  if (n == 1L) return(parts[[1L]])
  if (n == 2L) return(paste0(parts[[1L]], ", & ", parts[[2L]]))
  paste0(paste(parts[-n], collapse = ", "), ", & ", parts[[n]])
}

# ── Title cleaning ────────────────────────────────────────────────────────────

.clean_title <- function(title) {
  if (is.na(title)) return("[No title]")
  title <- gsub("\\\\(.)", "\\1", title, perl = TRUE)
  title <- sub(",?\\s*\\(Dataset\\)\\s*$", "", title)
  trimws(title)
}

# ── BibTeX file parser ────────────────────────────────────────────────────────

# Parse all @misc entries in a BibTeX file.  Returns a list of lists with
# fields: key, author, title, year, doi, url.  @article entries are skipped.
.parse_bib <- function(path) {
  lines   <- readLines(path, encoding = "UTF-8", warn = FALSE)
  entries <- list()
  cur     <- NULL

  for (line in lines) {
    if (grepl("^@misc\\{", line, ignore.case = TRUE, perl = TRUE)) {
      key <- trimws(sub("^@misc\\{([^,]+).*", "\\1", line,
                        ignore.case = TRUE, perl = TRUE))
      cur <- list(key    = key,
                  author = NA_character_, title = NA_character_,
                  year   = NA_character_, doi   = NA_character_,
                  url    = NA_character_)
    } else if (!is.null(cur) && grepl("^\\}\\s*$", line)) {
      entries[[length(entries) + 1L]] <- cur
      cur <- NULL
    } else if (!is.null(cur) && grepl("^\\s*\\w+\\s*=", line)) {
      field <- trimws(sub("^\\s*(\\w+)\\s*=.*", "\\1", line))
      if (field %in% names(cur)) cur[[field]] <- .extract_field_value(line)
    }
  }
  entries
}

# Extract site_id from cite key.  Cite keys follow the convention
# {site_id}_{NETWORK}_{year} set by generate_fluxnet_citations().
.site_id_from_key <- function(key) {
  parts <- strsplit(key, "_")[[1]]
  if (length(parts) < 3L) return(key)
  paste(parts[seq_len(length(parts) - 2L)], collapse = "_")
}

# ── Single-entry APA formatter ────────────────────────────────────────────────

.make_apa <- function(entry) {
  authors <- .format_authors_apa(entry$author)
  year    <- if (!is.na(entry$year)) entry$year else "n.d."
  title   <- .clean_title(entry$title)

  url <- if (!is.na(entry$doi) && nzchar(entry$doi)) {
    paste0("https://doi.org/", entry$doi)
  } else if (!is.na(entry$url) && nzchar(entry$url)) {
    entry$url
  } else {
    "[No URL available]"
  }

  sep <- if (endsWith(authors, ".")) " " else ". "
  paste0(authors, sep, "(", year, "). ", title, " [Dataset]. ", url)
}

# ── CSV site-ID reader ────────────────────────────────────────────────────────

.read_site_ids_csv <- function(path, col) {
  if (!file.exists(path)) stop("site_ids_csv not found: ", path, call. = FALSE)
  csv <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!col %in% names(csv))
    stop("Column '", col, "' not found in ", path,
         ". Available columns: ", paste(names(csv), collapse = ", "), call. = FALSE)
  ids <- unique(csv[[col]][!is.na(csv[[col]]) & nzchar(csv[[col]])])
  message("  Read ", length(ids), " site IDs from ", basename(path))
  ids
}

# ── Format-specific writers ───────────────────────────────────────────────────

.write_txt <- function(citations, meta_lines, out_path) {
  header <- paste0(
    paste(meta_lines, collapse = "\n"), "\n",
    paste(rep("-", 72L), collapse = "")
  )
  writeLines(c(header, "", citations), con = out_path, useBytes = FALSE)
  message("Written ", length(citations), " citations to:\n  ", out_path)
}

.write_docx <- function(citations, meta_lines, out_path) {
  if (!requireNamespace("officer", quietly = TRUE))
    stop("Install 'officer' for docx output:  install.packages('officer')",
         call. = FALSE)

  doc <- officer::read_docx()
  doc <- officer::body_add_par(
    doc, "FLUXNET Site Dataset Citations — APA 7th Edition",
    style = "heading 1"
  )
  for (m in meta_lines) doc <- officer::body_add_par(doc, m, style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  for (cit in citations) doc <- officer::body_add_par(doc, cit, style = "Normal")

  print(doc, target = out_path)
  message("Written ", length(citations), " citations to:\n  ", out_path)
}

.write_gdoc <- function(citations, meta_lines, doc_name, gdrive_path) {
  if (!requireNamespace("officer", quietly = TRUE))
    stop("Install 'officer' and 'googledrive' for gdoc output.",  call. = FALSE)
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop("Install 'googledrive' for gdoc output:  install.packages('googledrive')",
         call. = FALSE)

  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  .write_docx(citations, meta_lines, tmp)

  parent <- if (!is.null(gdrive_path)) {
    googledrive::as_dribble(gdrive_path)
  } else {
    NULL
  }

  dr <- googledrive::drive_upload(
    media = tmp,
    name  = doc_name,
    path  = parent,
    type  = "document",
    overwrite = TRUE
  )
  message("Google Doc created: ", googledrive::drive_link(dr))
  invisible(dr)
}

# ── Main function ─────────────────────────────────────────────────────────────

#' Convert a FLUXNET BibTeX file to APA 7th Edition plain-text citations
#'
#' Reads \code{@misc} entries from a BibTeX file produced by
#' \code{generate_fluxnet_citations()}, formats each as an APA 7th Edition
#' dataset citation, and writes the results to a file.  Output format can be
#' plain text, a Word document (\code{.docx} via \pkg{officer}), or a native
#' Google Doc (via \pkg{googledrive}).
#'
#' Two author formats are handled automatically: AMF sites store authors as
#' "First Last" and are converted to "Last, F. M."; ICOS/TERN/SAEON/KOF/JPF
#' sites already use "Last, F." form and are passed through unchanged.
#'
#' @param bib_path Path to the input \code{.bib} file.
#' @param out_path Output path.  For \code{"txt"} and \code{"docx"}: a local
#'   file path; a \code{.txt} extension is automatically replaced with
#'   \code{.docx} when \code{output_format = "docx"}.  For \code{"gdoc"}: the
#'   name to give the Google Doc on Drive (not a local path).  Defaults are
#'   set per format (see Details).
#' @param site_ids Optional character vector of FLUXNET site IDs
#'   (e.g. \code{c("US-MMS", "DE-Tha")}).  When \code{NULL} (default) every
#'   \code{@misc} entry is included.  Cannot be used together with
#'   \code{site_ids_csv}.
#' @param site_ids_csv Optional path to a CSV file that contains a column of
#'   site IDs.  The column is identified by \code{site_id_col}.  Duplicate and
#'   \code{NA} values are dropped automatically.  Cannot be used together with
#'   \code{site_ids}.
#' @param site_id_col Column name in \code{site_ids_csv} that holds the site
#'   IDs.  Defaults to \code{"site_id"}.
#' @param output_format One of \code{"txt"} (default), \code{"docx"}, or
#'   \code{"gdoc"}.  \code{"docx"} requires the \pkg{officer} package;
#'   \code{"gdoc"} requires both \pkg{officer} and \pkg{googledrive} (a
#'   browser OAuth login is triggered on first use).
#' @param gdrive_path For \code{output_format = "gdoc"}: Google Drive folder
#'   path or \code{dribble} to place the document in.  \code{NULL} (default)
#'   uploads to the root of My Drive.
#'
#' @return Invisibly: a list with \code{out_path} and \code{n} (number of
#'   citations written) for \code{"txt"}/\code{"docx"}, or a \code{dribble}
#'   for \code{"gdoc"}.
#'
#' @examples
#' \dontrun{
#' source("R/parse_bib_to_apa.R")
#'
#' # All sites, plain text (default)
#' bib_to_apa()
#'
#' # Subset by character vector
#' bib_to_apa(site_ids = c("US-MMS", "DE-Tha", "AU-How"))
#'
#' # Subset by CSV — column named "site_id"
#' bib_to_apa(site_ids_csv = "data/my_sites.csv")
#'
#' # CSV with a different column name
#' bib_to_apa(site_ids_csv = "data/analysis_sites.csv", site_id_col = "SITE_ID")
#'
#' # Word document
#' bib_to_apa(output_format = "docx")
#' bib_to_apa(site_ids_csv = "data/my_sites.csv", output_format = "docx",
#'            out_path = "~/Desktop/subset_citations.docx")
#'
#' # Native Google Doc
#' bib_to_apa(output_format = "gdoc")
#' bib_to_apa(site_ids = c("US-MMS", "DE-Tha"), output_format = "gdoc",
#'            out_path = "Subset Citations 2026",
#'            gdrive_path = "My Drive/Paper2026")
#' }
bib_to_apa <- function(
    bib_path      = "outputs/full_site_citations.bib",
    out_path      = NULL,
    site_ids      = NULL,
    site_ids_csv  = NULL,
    site_id_col   = "site_id",
    output_format = c("txt", "docx", "gdoc"),
    gdrive_path   = NULL
) {
  output_format <- match.arg(output_format)

  # ── Validate mutually exclusive site_id inputs ─────────────────────────────
  if (!is.null(site_ids) && !is.null(site_ids_csv))
    stop("Provide either site_ids or site_ids_csv, not both.", call. = FALSE)

  # ── Resolve default out_path per format ───────────────────────────────────
  if (is.null(out_path)) {
    out_path <- switch(output_format,
      txt  = file.path(.GDRIVE_ROOT, "FLUXNET_APA_Citations.txt"),
      docx = file.path(.GDRIVE_ROOT, "FLUXNET_APA_Citations.docx"),
      gdoc = "FLUXNET_APA_Citations"
    )
  }

  # ── For docx: auto-correct .txt extension ─────────────────────────────────
  if (output_format == "docx" && grepl("\\.txt$", out_path, ignore.case = TRUE))
    out_path <- sub("\\.txt$", ".docx", out_path, ignore.case = TRUE)

  # ── Read site IDs from CSV if provided ────────────────────────────────────
  if (!is.null(site_ids_csv))
    site_ids <- .read_site_ids_csv(site_ids_csv, site_id_col)

  if (!is.null(site_ids))
    stopifnot(is.character(site_ids), length(site_ids) > 0L)

  # ── Parse BibTeX ──────────────────────────────────────────────────────────
  message("Parsing ", bib_path, " ...")
  entries <- .parse_bib(bib_path)
  message("  Found ", length(entries), " @misc entries")

  # ── Filter by site IDs ────────────────────────────────────────────────────
  if (!is.null(site_ids)) {
    entry_ids <- vapply(entries, function(e) .site_id_from_key(e$key), "")
    keep      <- entry_ids %in% site_ids
    missing   <- setdiff(site_ids, entry_ids[keep])
    if (length(missing) > 0L)
      warning("site_ids not found in ", bib_path, ": ",
              paste(missing, collapse = ", "), call. = FALSE)
    entries <- entries[keep]
    message("  Filtered to ", length(entries), " entries for ",
            length(site_ids), " requested site(s)")
  }

  citations <- vapply(entries, .make_apa, "")

  # ── Build metadata block ──────────────────────────────────────────────────
  filter_line <- if (!is.null(site_ids))
    paste0("Site filter : ", length(site_ids), " site(s) requested, ",
           length(citations), " matched")
  else character(0L)

  meta_lines <- c(
    "FLUXNET Site Dataset Citations — APA 7th Edition",
    paste0("Source    : ", bib_path),
    filter_line,
    paste0("Generated : ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S %Z")),
    paste0("Sites     : ", length(citations))
  )

  # ── Write output ──────────────────────────────────────────────────────────
  if (output_format == "txt") {
    out_exp <- path.expand(out_path)
    if (!dir.exists(dirname(out_exp))) stop("Output directory not found: ", dirname(out_exp))
    .write_txt(citations, meta_lines, out_exp)
    return(invisible(list(out_path = out_exp, n = length(citations))))
  }

  if (output_format == "docx") {
    out_exp <- path.expand(out_path)
    if (!dir.exists(dirname(out_exp))) stop("Output directory not found: ", dirname(out_exp))
    .write_docx(citations, meta_lines, out_exp)
    return(invisible(list(out_path = out_exp, n = length(citations))))
  }

  if (output_format == "gdoc") {
    dr <- .write_gdoc(citations, meta_lines, doc_name = out_path,
                      gdrive_path = gdrive_path)
    return(invisible(dr))
  }
}
