## 03_fetch_treering.R — Fetch tree-ring isotope reference data for the WUE
## isotope pilot. Two independent sources; each is wrapped so a failure in
## one is recorded and does NOT stop the other, or this script, from running
## (04_report_preanalysis.R must be able to run on whatever exists).
##
## 1. Guerrieri et al. (2019, PNAS) tree-ring isotope data, deposited at the
##    Environmental Data Initiative (EDI) as package edi.401. The newest
##    revision is resolved dynamically via the PASTA+ REST API (never
##    hardcoded), and every data entity + the EML metadata document are
##    downloaded to data/external/treering/edi_401/.
## 2. Belmecheri et al. (2021) NE tree-ring isotope data, cloned from
##    https://github.com/SBelmecheri/NE_Tree-Rings_Isotopes into
##    data/external/treering/NE_Tree-Rings_Isotopes/.
##
## Per instructions: do NOT search the ITRDB.

source("WUE/isotope_pilot/code/00_config.R")

treering_dir <- file.path(WUE_ROOT, "data", "external", "treering")
edi_dir      <- file.path(treering_dir, "edi_401")
dir.create(edi_dir, recursive = TRUE, showWarnings = FALSE)

# --- 1. EDI package edi.401 --------------------------------------------------

fetch_edi_401 <- function() {
  base <- "https://pasta.lternet.edu/package"
  scope <- "edi"
  identifier <- "401"

  rev_resp <- httr::GET(sprintf("%s/eml/%s/%s", base, scope, identifier))
  httr::stop_for_status(rev_resp)
  revisions <- strsplit(httr::content(rev_resp, "text", encoding = "UTF-8"), "\n")[[1]]
  revisions <- trimws(revisions[nchar(trimws(revisions)) > 0])
  if (length(revisions) == 0) stop("No revisions returned for edi.", identifier)
  revision <- revisions[length(revisions)] # PASTA returns revisions ascending; last = newest
  message("[WUE] edi.", identifier, " newest revision resolved: ", revision)

  doi_resp <- httr::GET(sprintf("%s/doi/eml/%s/%s/%s", base, scope, identifier, revision))
  doi <- if (httr::status_code(doi_resp) == 200) {
    trimws(httr::content(doi_resp, "text", encoding = "UTF-8"))
  } else {
    NA_character_
  }

  eml_resp <- httr::GET(sprintf("%s/metadata/eml/%s/%s/%s", base, scope, identifier, revision))
  httr::stop_for_status(eml_resp)
  eml_path <- file.path(edi_dir, sprintf("edi.%s.%s.eml.xml", identifier, revision))
  writeBin(httr::content(eml_resp, "raw"), eml_path)
  message("[WUE] EML metadata written: ", eml_path)

  ents_resp <- httr::GET(sprintf("%s/data/eml/%s/%s/%s", base, scope, identifier, revision))
  httr::stop_for_status(ents_resp)
  entity_ids <- strsplit(httr::content(ents_resp, "text", encoding = "UTF-8"), "\n")[[1]]
  entity_ids <- trimws(entity_ids[nchar(trimws(entity_ids)) > 0])
  message("[WUE] ", length(entity_ids), " data entity(ies) listed for edi.", identifier, ".", revision)

  entity_log <- do.call(rbind, lapply(entity_ids, function(eid) {
    name_resp <- httr::GET(sprintf("%s/name/eml/%s/%s/%s/%s", base, scope, identifier, revision, eid))
    fname <- if (httr::status_code(name_resp) == 200) {
      trimws(httr::content(name_resp, "text", encoding = "UTF-8"))
    } else {
      eid
    }
    data_resp <- httr::GET(sprintf("%s/data/eml/%s/%s/%s/%s", base, scope, identifier, revision, eid))
    ok <- httr::status_code(data_resp) == 200
    if (ok) {
      writeBin(httr::content(data_resp, "raw"), file.path(edi_dir, fname))
      message("[WUE]   -> ", fname)
    } else {
      message("[WUE]   entity ", eid, " failed to download (HTTP ", httr::status_code(data_resp), ")")
    }
    data.frame(entity_id = eid, filename = fname, downloaded = ok, stringsAsFactors = FALSE)
  }))
  write.csv(entity_log, file.path(edi_dir, "entity_manifest.csv"), row.names = FALSE)

  list(
    status = "ok", revision = revision, doi = doi,
    n_entities = nrow(entity_log), n_downloaded = sum(entity_log$downloaded),
    error = NA_character_
  )
}

edi_status <- tryCatch(
  fetch_edi_401(),
  error = function(e) {
    message("[WUE] edi.401 fetch failed: ", conditionMessage(e))
    list(
      status = "failed", revision = NA_character_, doi = NA_character_,
      n_entities = 0L, n_downloaded = 0L, error = conditionMessage(e)
    )
  }
)
saveRDS(edi_status, file.path(treering_dir, "edi_401_fetch_status.rds"))

# --- 2. Belmecheri et al. 2021 GitHub repo -----------------------------------

gh_url <- "https://github.com/SBelmecheri/NE_Tree-Rings_Isotopes"
gh_dir <- file.path(treering_dir, "NE_Tree-Rings_Isotopes")

fetch_github_repo <- function() {
  if (dir.exists(gh_dir) && length(list.files(gh_dir)) > 0) {
    message("[WUE] ", gh_dir, " already present -- skipping clone, listing existing files.")
  } else {
    rc <- system2(
      "git", c("clone", "--depth", "1", gh_url, shQuote(gh_dir)),
      stdout = TRUE, stderr = TRUE
    )
    status <- attr(rc, "status")
    if (!is.null(status) && !identical(status, 0L)) {
      stop("git clone exited with status ", status, ": ", paste(rc, collapse = "\n"))
    }
  }
  files <- list.files(gh_dir, recursive = TRUE, full.names = FALSE)
  list(status = "ok", n_files = length(files), files = files, error = NA_character_)
}

gh_status <- tryCatch(
  fetch_github_repo(),
  error = function(e) {
    message("[WUE] GitHub clone failed: ", conditionMessage(e))
    list(status = "failed", n_files = 0L, files = character(0), error = conditionMessage(e))
  }
)
saveRDS(gh_status, file.path(treering_dir, "github_fetch_status.rds"))
write.csv(
  data.frame(file = gh_status$files, stringsAsFactors = FALSE),
  file.path(treering_dir, "github_repo_file_listing.csv"),
  row.names = FALSE
)

message(
  "[WUE] 03_fetch_treering.R complete. EDI status: ", edi_status$status,
  " (revision ", edi_status$revision, ", DOI ", edi_status$doi, ", ",
  edi_status$n_downloaded, "/", edi_status$n_entities, " entities); ",
  "GitHub status: ", gh_status$status, " (", gh_status$n_files, " file(s))"
)
