## step4_extract_koppen_beck2023.R
## Extract Beck 2023 Köppen-Geiger class (1991-2020, 1 km) at FLUXNET site locations.
## Output: data/snapshots/site_koppen_beck2023.csv
##
## Source raster:
##   data/external/koppen_beck2023/1991_2020/koppen_geiger_0p00833333.tif
##   Beck et al. (2023) Scientific Data 10:724. doi:10.1038/s41597-023-02549-6
##   Figshare v2: doi:10.6084/m9.figshare.21789074.v2 (updated 2026-01-14)
##
## Method: terra::extract() at exact site coordinates; nearest-pixel fallback
## (terra::extract with small buffer) for any NA returns (coastal/ocean edge).
## The koppen_method column records which approach was used per site.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

library(terra)
library(dplyr)
library(readr)

# ---- Paths ------------------------------------------------------------------
kg_dir   <- file.path("data", "external", "koppen_beck2023")
rast_path <- file.path(kg_dir, "1991_2020", "koppen_geiger_0p00833333.tif")
leg_path  <- file.path(kg_dir, "legend.txt")

if (!file.exists(rast_path)) {
  stop(
    "KG raster not found: ", rast_path, "\n",
    "Download koppen_geiger_tif.zip from:\n",
    "  https://ndownloader.figshare.com/files/61012822\n",
    "  (figshare doi:10.6084/m9.figshare.21789074.v2)\n",
    "Extract to data/external/koppen_beck2023/",
    call. = FALSE
  )
}

# ---- Load legend ------------------------------------------------------------
message("Parsing legend: ", leg_path)

leg_lines <- readLines(leg_path)
leg_data  <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]

legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
  # Format: "   1:  Af   Tropical, rainforest  [0 0 255]"
  m <- regmatches(ln, regexec(
    "^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[",
    ln, perl = TRUE
  ))[[1]]
  if (length(m) < 4L) return(NULL)
  data.frame(
    koppen_class_code = as.integer(m[2]),
    koppen_class      = trimws(m[3]),
    koppen_class_name = trimws(m[4]),
    stringsAsFactors  = FALSE
  )
}))

# Main climate group (first letter) and readable name
main_map <- c(
  A = "Tropical",
  B = "Arid",
  C = "Temperate",
  D = "Cold",
  E = "Polar"
)
legend_df <- legend_df |>
  dplyr::mutate(
    koppen_main      = substr(koppen_class, 1L, 1L),
    koppen_main_name = main_map[koppen_main]
  )

message("Legend loaded: ", nrow(legend_df), " classes")
print(legend_df[, c("koppen_class_code", "koppen_class", "koppen_class_name")])

# ---- Load raster ------------------------------------------------------------
message("\nLoading KG raster: ", rast_path)
kg_rast <- terra::rast(rast_path)
message("  Resolution: ", paste(round(terra::res(kg_rast), 8), collapse = " x "),
        "°  |  CRS: ", terra::crs(kg_rast, describe = TRUE)$code)
message("  Dimensions: ", nrow(kg_rast), " rows x ", ncol(kg_rast), " cols")

# ---- Load snapshot ----------------------------------------------------------
snap_file <- "data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv"
if (!file.exists(snap_file)) {
  stop("Snapshot not found: ", snap_file, call. = FALSE)
}
message("Snapshot: ", basename(snap_file))
snapshot <- readr::read_csv(snap_file, show_col_types = FALSE)

site_coords <- snapshot |>
  dplyr::distinct(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))
N <- nrow(site_coords)
message("Sites with coordinates: ", N)

# ---- Primary extraction: exact pixel ----------------------------------------
pts <- terra::vect(
  data.frame(x = site_coords$location_long, y = site_coords$location_lat),
  geom = c("x", "y"),
  crs  = "EPSG:4326"
)

kg_raw <- terra::extract(kg_rast, pts, ID = FALSE)
names(kg_raw)[1] <- "koppen_class_code"
site_coords$koppen_class_code <- kg_raw$koppen_class_code
site_coords$koppen_method     <- "exact"

# ---- Fallback: buffer nearest for NA sites ----------------------------------
na_idx <- which(is.na(site_coords$koppen_class_code))
message("NA after exact extraction: ", length(na_idx), " site(s)")

if (length(na_idx) > 0L) {
  # Try progressively larger circular buffers (in degrees, ~1–50 km)
  buffer_degs <- c(0.01, 0.05, 0.1, 0.25, 0.5)
  still_na    <- na_idx

  # Mode function: most frequent non-NA integer value
  int_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(NA_integer_)
    as.integer(names(sort(table(x), decreasing = TRUE))[[1L]])
  }

  # Stage 1: circular buffer (fast, works for land-edge sites)
  recovered_any <- logical(length(still_na))
  for (j in seq_along(still_na)) {
    pt_j <- pts[still_na[j], ]
    for (buf in buffer_degs) {
      buf_vals <- terra::extract(kg_rast, pt_j, buffer = buf, ID = TRUE)
      vals     <- buf_vals[[2L]]
      mode_val <- int_mode(vals)
      if (!is.na(mode_val)) {
        site_coords$koppen_class_code[still_na[j]] <- mode_val
        site_coords$koppen_method[still_na[j]] <-
          paste0("buffer_", round(buf * 111, 0), "km")
        message("  ", site_coords$site_id[still_na[j]],
                " recovered at buffer ", buf, "° (~", round(buf * 111, 0),
                " km): class code ", mode_val)
        recovered_any[j] <- TRUE
        break
      }
    }
  }
  still_na <- still_na[!recovered_any]

  # Stage 2: nearest-land pixel search for sites still NA (coordinates in ocean)
  if (length(still_na) > 0L) {
    message(length(still_na),
            " site(s) still NA — using nearest-land pixel within 3° search window:")
    for (j in seq_along(still_na)) {
      idx <- still_na[j]
      lng <- site_coords$location_long[idx]
      lat <- site_coords$location_lat[idx]
      message("  Searching near ", site_coords$site_id[idx],
              " (", round(lat, 3), "N, ", round(lng, 3), "E) ...")
      # Crop to 3° window and extract all non-NA land cells
      search_ext <- terra::ext(lng - 3, lng + 3, lat - 3, lat + 3)
      local_rast <- terra::crop(kg_rast, search_ext)
      land_pts   <- terra::as.points(local_rast, na.rm = TRUE)
      if (terra::nrow(land_pts) == 0L) {
        message("    No land pixels found within 3° — remains NA")
        next
      }
      pt_j        <- pts[idx, ]
      dists       <- terra::distance(pt_j, land_pts)
      nearest_idx <- which.min(dists)
      nearest_val <- as.integer(terra::values(land_pts)[[1]][nearest_idx])
      nearest_km  <- round(min(dists) / 1000, 1)
      site_coords$koppen_class_code[idx] <- nearest_val
      site_coords$koppen_method[idx]     <- paste0("nearest_land_", nearest_km, "km")
      message("    Assigned class ", nearest_val, " from nearest land pixel ",
              nearest_km, " km away")
    }
    still_na <- still_na[is.na(site_coords$koppen_class_code[still_na])]
  }

  if (length(still_na) > 0L) {
    message("WARNING: ", length(still_na),
            " site(s) remain NA after all buffer attempts:\n  ",
            paste(site_coords$site_id[still_na], collapse = ", "))
  }
}

# ---- Join legend ------------------------------------------------------------
out <- site_coords |>
  dplyr::left_join(legend_df, by = "koppen_class_code") |>
  dplyr::select(
    site_id, location_lat, location_long,
    koppen_class_code, koppen_class, koppen_class_name,
    koppen_main, koppen_main_name, koppen_method
  )

# ---- Summary ----------------------------------------------------------------
cat("\n--- KG main class distribution (", N, " sites) ---\n")
main_tbl <- out |>
  dplyr::count(koppen_main, koppen_main_name, name = "n") |>
  dplyr::arrange(koppen_main) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1))
print(as.data.frame(main_tbl))

cat("\n--- KG class distribution ---\n")
class_tbl <- out |>
  dplyr::count(koppen_class, koppen_class_name, name = "n") |>
  dplyr::arrange(dplyr::desc(n))
print(as.data.frame(class_tbl), row.names = FALSE)

na_final <- out |> dplyr::filter(is.na(koppen_class_code))
cat("\n--- NA summary ---\n")
if (nrow(na_final) > 0L) {
  cat("Unresolved NA sites (", nrow(na_final), "):\n")
  print(dplyr::select(na_final, site_id, location_lat, location_long))
} else {
  cat("All ", N, " sites assigned a KG class.\n")
}

fb_sites <- out |>
  dplyr::filter(koppen_method != "exact") |>
  dplyr::select(site_id, koppen_method, koppen_class)
if (nrow(fb_sites) > 0L) {
  cat("\n--- Fallback extraction sites ---\n")
  print(as.data.frame(fb_sites))
}

# ---- Save -------------------------------------------------------------------
out_path <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_koppen_beck2023.csv")
readr::write_csv(out, out_path)
message("\nSaved: ", out_path, " (", nrow(out), " rows)")

write_output_metadata(
  out_path,
  input_sources = c(
    snap_file,
    paste0(
      "Beck et al. (2023) KG 1991-2020 1 km raster — ",
      "data/external/koppen_beck2023/1991_2020/koppen_geiger_0p00833333.tif",
      " (figshare doi:10.6084/m9.figshare.21789074.v2, file 61012822)"
    )
  ),
  notes = paste0(
    "KG class extracted at exact site coordinates via terra::extract(); ",
    "buffer modal fallback (up to 0.5°) for NA returns. ",
    "koppen_method column records extraction method per site. ",
    "Snapshot: ", basename(snap_file), " (767 sites, 2026-06-24)."
  )
)
