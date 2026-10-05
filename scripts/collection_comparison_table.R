## scripts/collection_comparison_table.R
## Collection comparison tables: Marconi (2000), La Thuile (2007), FLUXNET2015,
## and the current FLUXNET Shuttle network, side by side on sites, site-years,
## region, and IGBP class. No new analysis — every number here is read from
## data already on disk (the historical xlsx lists, the historical clean/
## years snapshot CSVs, the pinned current-network snapshot, and
## data/snapshots/site_year_data_presence.csv).
##
## Site-year definitions (per collection, each is that collection's own
## source convention — see docs/methods_requirements.md 5.5):
##   Marconi      — sum(last_year - first_year + 1) from the "Years in
##                  Marconi" ranges in data/lists/Marconi_to_Modern_SiteIDs.xlsx
##                  (ranges are all that source gives; its own records are
##                  contiguous, so this is also what the indicator-style count
##                  would give).
##   La Thuile    — count of 1s across the year-indicator columns (1991-2007)
##                  of data/lists/LaThuileList.xlsx. NOT the same as summing
##                  last_year - first_year + 1 from years_la_thuile.csv (that
##                  span sum is 1008 -- La Thuile site records are not
##                  contiguous within their first/last year).
##   FLUXNET2015  — count of non-NA cells across the two-digit year columns
##                  (91-14) of data/lists/FLUXNET2015.xlsx.
##   Current      — compute_site_year_presence() output
##                  (data/snapshots/site_year_data_presence.csv), rows where
##                  has_data is TRUE, year window data-driven via
##                  R/utils.R::data_year_window() (first/last year any site
##                  has data -- see 2026-10-05 SESSION_LOG entry).
##
## NOTE: Marconi/La Thuile/FLUXNET2015 are historical comparison datasets only
## -- non-Shuttle primary data (CLAUDE.md Hard Rule 1).
##
## Outputs (data/snapshots/), each with a .meta.json companion:
##   collection_sites_siteyears.csv  — sites + site-years per collection
##   collection_sites_by_region.csv — sites per Figure 1 region + 3 extra
##                                    buckets (South and Central America,
##                                    Africa, Other), with Other site IDs
##   collection_sites_by_igbp.csv   — per-class counts (PAPER_IGBP_ORDER),
##                                    classes present, missing/non-standard
##   collection_sites_per_year.csv  — sites with data per year 1991-2007,
##                                    La Thuile vs current

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(readxl); library(tidyr)
  library(countrycode)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Collection comparison table ===")

OUT_DIR <- file.path(FLUXNET_DATA_ROOT, "snapshots")

CURRENT_SNAPSHOT <- file.path(OUT_DIR, "fluxnet_shuttle_snapshot_20260901T094522.csv")
if (!file.exists(CURRENT_SNAPSHOT)) stop("Pinned snapshot not found: ", CURRENT_SNAPSHOT, call. = FALSE)
PRESENCE_PATH <- file.path(OUT_DIR, "site_year_data_presence.csv")

## ---- Reuse Figure 1's regional extents verbatim (do not retype) -----------
## scripts/generate_map_regional.R defines REGIONS as a literal `list(...)`
## assignment on one contiguous block of lines, immediately followed by its
## REGION_LETTERS assignment -- extract and eval that block directly so this
## script can never drift from the figure's own extents.
.map_script_lines <- readLines("scripts/generate_map_regional.R")
.start_idx <- grep("^REGIONS <- list\\(", .map_script_lines)
.end_idx   <- grep("^REGION_LETTERS <- c\\(", .map_script_lines) - 1L
if (length(.start_idx) != 1L || length(.end_idx) != 1L || .end_idx < .start_idx) {
  stop("Could not locate REGIONS <- list(...) block in scripts/generate_map_regional.R", call. = FALSE)
}
REGIONS <- eval(parse(text = paste(.map_script_lines[.start_idx:.end_idx], collapse = "\n")))
msg("Reused REGIONS from scripts/generate_map_regional.R: ", paste(names(REGIONS), collapse = ", "))

REGION_ORDER <- c("North America", "Europe", "East and Southeast Asia",
                   "Australia and New Zealand", "South and Central America",
                   "Africa", "Other")

.in_region <- function(lat, lon, r) {
  dplyr::between(lat, r$lat_min, r$lat_max) & dplyr::between(lon, r$lon_min, r$lon_max)
}

## Country-code-based region for sites outside all four Figure 1 extents.
## Uses the site_id prefix for what CLAUDE.md Hard Rule 2 says it actually
## encodes -- a country code -- not for hub/network inference. countrycode's
## "region23" field (unlike "un.regionsub.name") splits Latin America into
## South America / Central America, which is what this table needs.
.country_bucket <- function(site_ids) {
  iso2 <- dplyr::if_else(substr(site_ids, 1L, 2L) == "UK", "GB", substr(site_ids, 1L, 2L))
  sub  <- countrycode::countrycode(iso2, "iso2c", "region23", warn = FALSE)
  dplyr::case_when(
    sub %in% c("South America", "Central America") ~ "South and Central America",
    grepl("Africa", sub)                            ~ "Africa",
    TRUE                                             ~ "Other"
  )
}

.classify_region <- function(df) {
  df <- df |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::filter(!is.na(.data$location_lat), !is.na(.data$location_long))
  region <- rep(NA_character_, nrow(df))
  for (nm in names(REGIONS)) {
    r   <- REGIONS[[nm]]
    hit <- .in_region(df$location_lat, df$location_long, r) & is.na(region)
    region[hit] <- r$label
  }
  remaining <- is.na(region)
  region[remaining] <- .country_bucket(df$site_id[remaining])
  df$region <- region
  df
}

## ---- Load the four collections' site metadata (lat/lon/igbp) -------------
sites_marconi     <- read_csv(file.path(OUT_DIR, "sites_marconi_clean.csv"), show_col_types = FALSE)
sites_la_thuile   <- read_csv(file.path(OUT_DIR, "sites_la_thuile_clean.csv"), show_col_types = FALSE)
sites_fluxnet2015 <- read_csv(file.path(OUT_DIR, "sites_fluxnet2015_clean.csv"), show_col_types = FALSE)
sites_current     <- read_csv(CURRENT_SNAPSHOT, show_col_types = FALSE) |>
  dplyr::distinct(.data$site_id, .keep_all = TRUE)

COLLECTIONS <- c("Marconi", "La Thuile", "FLUXNET2015", "Current")

## =============================================================================
## Table 1: collection_sites_siteyears.csv
## =============================================================================
msg("--- Table 1: sites + site-years ---")

marconi_xlsx <- read_excel("data/lists/Marconi_to_Modern_SiteIDs.xlsx")
marconi_fy   <- as.integer(sub("^(\\d{4}).*", "\\1", marconi_xlsx$`Years in Marconi`))
marconi_ly   <- suppressWarnings(as.integer(sub("^\\d{4}-(\\d{4})$", "\\1", marconi_xlsx$`Years in Marconi`)))
marconi_ly   <- dplyr::if_else(is.na(marconi_ly), marconi_fy, marconi_ly)
marconi_site_years <- as.integer(sum(marconi_ly - marconi_fy + 1L, na.rm = TRUE))

la_thuile_xlsx    <- read_excel("data/lists/LaThuileList.xlsx")
la_thuile_yrcols  <- names(la_thuile_xlsx)[grepl("^[0-9]{4}$", names(la_thuile_xlsx))]
la_thuile_site_years <- as.integer(sum(as.matrix(la_thuile_xlsx[, la_thuile_yrcols]), na.rm = TRUE))

fluxnet2015_xlsx   <- read_excel("data/lists/FLUXNET2015.xlsx")
fluxnet2015_yrcols <- names(fluxnet2015_xlsx)[grepl("^[0-9]{2}", names(fluxnet2015_xlsx))]
fluxnet2015_site_years <- as.integer(sum(!is.na(as.matrix(fluxnet2015_xlsx[, fluxnet2015_yrcols]))))

presence_df <- read_csv(PRESENCE_PATH, show_col_types = FALSE) |>
  mutate(year = as.integer(.data$year), has_data = as.logical(.data$has_data))
## Data-driven year window (R/utils.R::data_year_window()) -- replaces the
## previous hard-coded 1991-2024 filter, so this table's current-network
## total and Figure 2's own total (scripts/generate_fig_cumulative_siteyears.R)
## are read from the same window and cannot silently disagree.
current_year_window <- data_year_window(presence_df)
msg("Current-network year window: ", current_year_window$first_year, "-",
    current_year_window$last_year, " (", current_year_window$n_sites_last_year,
    " sites report ", current_year_window$last_year, ")")
current_site_years <- presence_df |>
  dplyr::filter(.data$has_data,
                .data$year >= current_year_window$first_year,
                .data$year <= current_year_window$last_year) |>
  nrow() |>
  as.integer()

table1 <- tibble::tibble(
  collection = COLLECTIONS,
  sites      = c(
    dplyr::n_distinct(sites_marconi$site_id),
    dplyr::n_distinct(sites_la_thuile$site_id),
    dplyr::n_distinct(sites_fluxnet2015$site_id),
    dplyr::n_distinct(sites_current$site_id)
  ),
  site_years = c(marconi_site_years, la_thuile_site_years,
                  fluxnet2015_site_years, current_site_years)
)
print(table1)

write_csv(table1, file.path(OUT_DIR, "collection_sites_siteyears.csv"))
write_output_metadata(
  file.path(OUT_DIR, "collection_sites_siteyears.csv"),
  input_sources = c(
    "data/lists/Marconi_to_Modern_SiteIDs.xlsx", "data/lists/LaThuileList.xlsx",
    "data/lists/FLUXNET2015.xlsx", CURRENT_SNAPSHOT, PRESENCE_PATH
  ),
  notes = "Site-years use each collection's own source convention (ranges for Marconi, year-indicator matrices for La Thuile/FLUXNET2015, has_data presence for Current) -- see script header."
)

## =============================================================================
## Table 2: collection_sites_by_region.csv
## =============================================================================
msg("--- Table 2: sites by region ---")

region_table <- dplyr::bind_rows(
  .classify_region(sites_marconi)     |> dplyr::mutate(collection = "Marconi"),
  .classify_region(sites_la_thuile)   |> dplyr::mutate(collection = "La Thuile"),
  .classify_region(sites_fluxnet2015) |> dplyr::mutate(collection = "FLUXNET2015"),
  .classify_region(sites_current)     |> dplyr::mutate(collection = "Current")
)

table2 <- region_table |>
  dplyr::mutate(collection = factor(.data$collection, levels = COLLECTIONS),
                region     = factor(.data$region, levels = REGION_ORDER)) |>
  dplyr::count(.data$collection, .data$region, name = "n_sites", .drop = FALSE) |>
  dplyr::left_join(
    region_table |>
      dplyr::filter(.data$region == "Other") |>
      dplyr::group_by(.data$collection) |>
      dplyr::summarise(other_site_ids = paste(sort(.data$site_id), collapse = "; "), .groups = "drop"),
    by = "collection"
  ) |>
  dplyr::mutate(
    other_site_ids = dplyr::if_else(.data$region == "Other", dplyr::coalesce(.data$other_site_ids, ""), ""),
    collection      = factor(.data$collection, levels = COLLECTIONS)
  ) |>
  dplyr::arrange(.data$collection, .data$region)
print(table2, n = Inf)

write_csv(table2, file.path(OUT_DIR, "collection_sites_by_region.csv"))
write_output_metadata(
  file.path(OUT_DIR, "collection_sites_by_region.csv"),
  input_sources = c(
    "data/snapshots/sites_marconi_clean.csv", "data/snapshots/sites_la_thuile_clean.csv",
    "data/snapshots/sites_fluxnet2015_clean.csv", CURRENT_SNAPSHOT,
    "scripts/generate_map_regional.R (REGIONS extents, reused verbatim)"
  ),
  notes = "Region = Figure 1's four regional extents (lat/lon box) where a site falls inside one; otherwise bucketed by the country code the site_id prefix encodes (countrycode::region23), into South and Central America / Africa / Other -- per CLAUDE.md Hard Rule 2, not a hub/network inference."
)

## =============================================================================
## Table 3: collection_sites_by_igbp.csv
## =============================================================================
msg("--- Table 3: sites by IGBP class ---")

.igbp_row <- function(df, collection_label) {
  df <- dplyr::distinct(df, .data$site_id, .keep_all = TRUE)
  counts <- vapply(PAPER_IGBP_ORDER, function(cl) sum(df$igbp == cl, na.rm = TRUE), integer(1L))
  nonstandard <- df |>
    dplyr::filter(is.na(.data$igbp) | !(.data$igbp %in% PAPER_IGBP_ORDER)) |>
    dplyr::mutate(label = paste0(.data$site_id, ":", dplyr::coalesce(.data$igbp, "NA")))
  tibble::tibble(
    collection        = collection_label,
    !!!setNames(as.list(counts), PAPER_IGBP_ORDER),
    classes_present    = sum(counts > 0L),
    nonstandard_sites  = paste(sort(nonstandard$label), collapse = "; ")
  )
}

table3 <- dplyr::bind_rows(
  .igbp_row(sites_marconi,     "Marconi"),
  .igbp_row(sites_la_thuile,   "La Thuile"),
  .igbp_row(sites_fluxnet2015, "FLUXNET2015"),
  .igbp_row(sites_current,     "Current")
)
print(table3, width = Inf)

write_csv(table3, file.path(OUT_DIR, "collection_sites_by_igbp.csv"))
write_output_metadata(
  file.path(OUT_DIR, "collection_sites_by_igbp.csv"),
  input_sources = c(
    "data/snapshots/sites_marconi_clean.csv", "data/snapshots/sites_la_thuile_clean.csv",
    "data/snapshots/sites_fluxnet2015_clean.csv", CURRENT_SNAPSHOT,
    "R/plot_constants.R::PAPER_IGBP_ORDER"
  ),
  notes = "Counts are per-site (one row per distinct site_id), not per-site-year. nonstandard_sites lists site_id:class for any site whose igbp is missing (NA) or outside PAPER_IGBP_ORDER (e.g. La Thuile's 'TBD')."
)

## =============================================================================
## Table 4: collection_sites_per_year.csv
## =============================================================================
msg("--- Table 4: sites with data per year, 1991-2007, La Thuile vs Current ---")

la_thuile_per_year <- tibble::tibble(
  year    = as.integer(la_thuile_yrcols),
  n_sites = as.integer(colSums(as.matrix(la_thuile_xlsx[, la_thuile_yrcols]), na.rm = TRUE))
) |>
  dplyr::mutate(collection = "La Thuile")

current_per_year <- presence_df |>
  dplyr::filter(.data$has_data, .data$year >= 1991L, .data$year <= 2007L) |>
  dplyr::count(.data$year, name = "n_sites") |>
  dplyr::mutate(collection = "Current", year = as.integer(.data$year))

table4 <- dplyr::bind_rows(la_thuile_per_year, current_per_year) |>
  dplyr::mutate(collection = factor(.data$collection, levels = c("La Thuile", "Current"))) |>
  dplyr::arrange(.data$collection, .data$year) |>
  dplyr::select("collection", "year", "n_sites")
print(table4, n = Inf)

write_csv(table4, file.path(OUT_DIR, "collection_sites_per_year.csv"))
write_output_metadata(
  file.path(OUT_DIR, "collection_sites_per_year.csv"),
  input_sources = c("data/lists/LaThuileList.xlsx", PRESENCE_PATH),
  notes = "n_sites per year, not cumulative. La Thuile: column sum of the 1991-2007 year-indicator matrix. Current: count of site_id with has_data = TRUE in compute_site_year_presence() output, restricted to 1991-2007 for comparison with La Thuile's own range."
)

msg("=== Done ===")
