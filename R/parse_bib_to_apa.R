## R/parse_bib_to_apa.R
## Parse a FLUXNET BibTeX file and write APA 7th Edition dataset citations.
##
## Usage (source then call):
##   source("R/parse_bib_to_apa.R")
##
##   # All sites in the .bib file
##   bib_to_apa()
##
##   # Subset of sites
##   bib_to_apa(site_ids = c("US-MMS", "DE-Tha", "AU-How"))
##
##   # Custom paths
##   bib_to_apa(
##     bib_path = "outputs/citations/fluxnet_2026.bib",
##     out_path = "~/Desktop/my_citations.txt",
##     site_ids = c("US-MMS", "DE-Tha")
##   )

# ── Low-level BibTeX field extractor ────────────────────────────────────────

# Extract the value between the outermost braces on a single BibTeX field line.
# Handles both {value} and {{value}} — strips one inner brace layer if present.
.extract_field_value <- function(line) {
  line  <- trimws(sub(",\\s*$", "", line))      # drop trailing comma
  start <- regexpr("\\{", line)
  if (start == -1L) return(NA_character_)
  end <- nchar(line)
  while (end > start && substr(line, end, end) != "}") end <- end - 1L
  val <- substr(line, start + 1L, end - 1L)
  if (grepl("^\\{.*\\}$", val)) val <- substr(val, 2L, nchar(val) - 1L)
  trimws(val)
}

# ── Author name formatting ───────────────────────────────────────────────────

# Convert one name in "First [Middle] Last" format to "Last, F. M." APA form.
.parse_first_last <- function(name) {
  name <- trimws(gsub("_", " ", name))
  if (!nzchar(name)) return("")
  if (grepl("\\(", name)) return(name)           # institutional — keep as-is

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
# Detection: split by ", " before a title-case word; if the first piece contains
# a comma the string is already in "Last, F." form, otherwise "First Last" form.
.format_authors_apa <- function(raw) {
  if (is.na(raw) || !nzchar(trimws(raw))) return("[No author listed]")
  raw <- trimws(gsub("_", " ", raw))

  parts <- trimws(strsplit(raw, ",\\s+(?=[A-Z][a-z])", perl = TRUE)[[1]])

  if (!grepl(",", parts[[1L]], fixed = TRUE)) {
    parts <- vapply(parts, .parse_first_last, "")
  }

  n <- length(parts)
  if (n == 1L) return(parts[[1L]])
  if (n == 2L) return(paste0(parts[[1L]], ", & ", parts[[2L]]))
  paste0(paste(parts[-n], collapse = ", "), ", & ", parts[[n]])
}

# ── Title cleaning ───────────────────────────────────────────────────────────

.clean_title <- function(title) {
  if (is.na(title)) return("[No title]")
  title <- gsub("\\\\(.)", "\\1", title, perl = TRUE)   # strip LaTeX backslash escapes
  title <- sub(",?\\s*\\(Dataset\\)\\s*$", "", title)    # strip trailing "(Dataset)"
  trimws(title)
}

# ── BibTeX file parser ───────────────────────────────────────────────────────

# Parse all @misc entries in a BibTeX file.
# Returns a list of lists, each with fields: key, author, title, year, doi, url.
# @article entries (mandated refs) are intentionally skipped.
.parse_bib <- function(path) {
  lines   <- readLines(path, encoding = "UTF-8", warn = FALSE)
  entries <- list()
  cur     <- NULL

  for (line in lines) {
    if (grepl("^@misc\\{", line, ignore.case = TRUE, perl = TRUE)) {
      key <- sub("^@misc\\{([^,]+).*", "\\1", line,
                 ignore.case = TRUE, perl = TRUE)
      cur <- list(key    = trimws(key),
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

# Extract the site_id prefix from a BibTeX cite key.
# Cite keys follow the pattern {site_id}_{NETWORK}_{year} (from generate_fluxnet_citations.R).
# The last two underscore-delimited tokens are always network and year.
.site_id_from_key <- function(key) {
  parts <- strsplit(key, "_")[[1]]
  if (length(parts) < 3L) return(key)
  paste(parts[seq_len(length(parts) - 2L)], collapse = "_")
}

# ── Single-entry APA formatter ───────────────────────────────────────────────

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

# ── Main function ─────────────────────────────────────────────────────────────

#' Convert a FLUXNET BibTeX file to APA 7th Edition plain-text citations
#'
#' Reads \code{@misc} entries from a BibTeX file produced by
#' \code{generate_fluxnet_citations()}, formats each as an APA 7th Edition
#' dataset citation, and writes the results to a plain-text file.
#'
#' Two author formats are handled automatically: AMF sites store authors as
#' "First Last" and are converted to "Last, F. M."; ICOS/TERN/SAEON/KOF/JPF
#' sites already use "Last, F." form and are passed through unchanged.
#'
#' @param bib_path  Path to the input \code{.bib} file.
#' @param out_path  Path to write the plain-text output file.
#' @param site_ids  Optional character vector of FLUXNET site IDs
#'   (e.g. \code{c("US-MMS", "DE-Tha")}). When \code{NULL} (default) every
#'   \code{@misc} entry in \code{bib_path} is included. Site IDs are matched
#'   against the site-ID prefix of each entry's cite key; a warning is issued
#'   for any requested ID not found in the file.
#'
#' @return Invisibly: a list with elements \code{out_path} (character) and
#'   \code{n} (integer number of citations written).
#'
#' @examples
#' \dontrun{
#' source("R/parse_bib_to_apa.R")
#'
#' # All sites in the default .bib
#' bib_to_apa()
#'
#' # Subset: three sites from different networks
#' bib_to_apa(site_ids = c("US-MMS", "DE-Tha", "AU-How"))
#'
#' # Custom input and output paths
#' bib_to_apa(
#'   bib_path = "outputs/citations/subset_2026.bib",
#'   out_path = "~/Desktop/subset_apa.txt",
#'   site_ids = c("US-MMS", "DE-Tha")
#' )
#' }
bib_to_apa <- function(
    bib_path = "outputs/full_site_citations.bib",
    out_path = path.expand(
      "~/Library/CloudStorage/GoogleDrive-setanta.research@gmail.com/My Drive/FLUXNET_APA_Citations.txt"
    ),
    site_ids = NULL
) {
  stopifnot(is.character(bib_path), length(bib_path) == 1L,
            is.character(out_path), length(out_path)  == 1L)
  if (!is.null(site_ids)) stopifnot(is.character(site_ids), length(site_ids) > 0L)

  message("Parsing ", bib_path, " ...")
  entries <- .parse_bib(bib_path)
  message("  Found ", length(entries), " @misc entries")

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

  filter_line <- if (!is.null(site_ids))
    paste0("Site filter : ", length(site_ids), " site(s) requested, ",
           length(citations), " matched\n")
  else ""

  header <- paste0(
    "FLUXNET Site Dataset Citations — APA 7th Edition\n",
    "Source    : ", bib_path, "\n",
    filter_line,
    "Generated : ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S %Z"), "\n",
    "Sites     : ", length(citations), "\n",
    paste(rep("-", 72L), collapse = "")
  )

  out_path_exp <- path.expand(out_path)
  out_dir <- dirname(out_path_exp)
  if (!dir.exists(out_dir)) stop("Output directory not found: ", out_dir)

  writeLines(c(header, "", citations), con = out_path_exp, useBytes = FALSE)
  message("Written ", length(citations), " citations to:\n  ", out_path_exp)

  invisible(list(out_path = out_path_exp, n = length(citations)))
}
