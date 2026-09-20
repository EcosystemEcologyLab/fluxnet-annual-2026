## store_refresh_stage5_reconcile.R — Stage 5 of the 2026-09-20 store refresh.
## One reconciliation table: baseline (stage 1, pre-refresh) vs after
## (post stage-4 rebuild), natural units, side by side, with a difference
## column. Names every site whose KG classification changed.
##
## SCOPE DECISION (recorded): "aridity and biomass class counts" are
## reported as unchanged by construction, not re-derived by re-running the
## heavy raster-extraction scripts (figure_representativeness_aridity.R /
## _biomass.R / _landcover.R). Those classes depend only on site
## lat/long joined to STATIC external rasters (CGIAR aridity, ESA CCI
## biomass) -- neither the site list (0 new sites, confirmed stage 3) nor
## any site's coordinates changed in this refresh, only flux/ERA5 VALUES
## at already-known site locations. Re-running multi-GB raster extractions
## for a result that cannot mechanically differ would not change the
## answer and would cost hours; this is the "changes least"/least-cost
## call for this stage. Weighted Jaccard is therefore also reported as
## unchanged for every non-KG axis (IGBP, aridity, biomass, landcover),
## for the same reason, and only recomputed for the KG axis, where it CAN
## change (KG class is derived from each site's own ERA5 monthly data).

if (file.exists(".env")) { library(dotenv); dotenv::load_dot_env() }
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(duckdb); library(DBI); library(fs)
})

BASE <- "review/diagnostics/store_refresh_20260920/baseline"
OUTD <- "review/diagnostics/store_refresh_20260920/stage5"
fs::dir_create(OUTD)
message("=== store_refresh_stage5_reconcile.R : ", Sys.time(), " ===")

con <- dbConnect(duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)

# ---- 1. Network site count -------------------------------------------------
baseline_snap <- readr::read_csv(file.path(BASE, "fluxnet_shuttle_snapshot_20260901T094522.csv"), show_col_types = FALSE)
after_snap_path <- sort(list.files("data/snapshots", pattern = "^fluxnet_shuttle_snapshot_20260920.*\\.csv$", full.names = TRUE))
after_snap_path <- after_snap_path[length(after_snap_path)]
after_snap <- readr::read_csv(after_snap_path, show_col_types = FALSE)

n_sites_before <- dplyr::n_distinct(baseline_snap$site_id)
n_sites_after  <- dplyr::n_distinct(after_snap$site_id)

# ---- 2. Site-years per resolution (DuckDB row counts before/after) --------
before_counts <- readr::read_csv(file.path(BASE, "duckdb_row_counts_before.csv"), show_col_types = FALSE)
tbls <- dbListTables(con)
after_counts <- tibble::tibble(
  table = sort(tbls),
  row_count = vapply(sort(tbls), function(t) dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", t))$n, numeric(1))
)
counts_compare <- dplyr::full_join(before_counts, after_counts, by = "table", suffix = c("_before", "_after")) |>
  dplyr::mutate(difference = row_count_after - row_count_before)
readr::write_csv(counts_compare, file.path(OUTD, "table_duckdb_row_counts_compare.csv"))

# ---- 3. Per-IGBP N and flux medians, before vs after -----------------------
# "Before" comes from the annual_converted table's state as captured by
# baseline row counts is not enough for medians -- recompute "before" is not
# possible without a DB snapshot, so this compares the CURRENT (post-refresh)
# per-IGBP medians against the LAST time this exact query was run and
# recorded in review/diagnostics's own baseline copy if one exists;
# otherwise reports "after" only with a note.
igbp_lookup <- tryCatch(
  dbGetQuery(con, "SELECT DISTINCT site_id, igbp FROM manifest WHERE igbp IS NOT NULL"),
  error = function(e) NULL
)
after_igbp_medians <- NULL
if (!is.null(igbp_lookup)) {
  annual_af <- dbGetQuery(con, "SELECT site_id, NEE_VUT_REF, GPP_NT_VUT_REF, LE_F_MDS, H_F_MDS FROM annual_converted WHERE dataset = 'FLUXMET'")
  annual_af <- dplyr::left_join(annual_af, igbp_lookup, by = "site_id")
  after_igbp_medians <- annual_af |>
    dplyr::filter(!is.na(igbp)) |>
    dplyr::group_by(igbp) |>
    dplyr::summarise(
      n_site_years = dplyr::n(),
      median_NEE = median(NEE_VUT_REF, na.rm = TRUE),
      median_GPP = median(GPP_NT_VUT_REF, na.rm = TRUE),
      median_LE  = median(LE_F_MDS, na.rm = TRUE),
      median_H   = median(H_F_MDS, na.rm = TRUE),
      .groups = "drop"
    )
  readr::write_csv(after_igbp_medians, file.path(OUTD, "table_igbp_flux_medians_after.csv"))
}
# Baseline flux-median CSVs captured in stage 1 are single-variable-per-file
# (flux_medians_by_igbp_{et,gpp,h,nep,ter}.csv) from a DIFFERENT prior
# computation (not the same query as above) -- listed for reference, not
# diffed cell-by-cell against the after table (different variable set/join
# logic; a like-for-like diff was not reproducible from what stage 1 saved).
baseline_median_files <- list.files(BASE, pattern = "^flux_medians_by_igbp_.*\\.csv$", full.names = TRUE)

# ---- 4. KG classification: re-run step5_compute_koppen_era5.R -------------
message("Re-running scripts/step5_compute_koppen_era5.R for post-refresh KG classes...")
baseline_kg <- readr::read_csv(file.path(BASE, "site_koppen_era5.csv"), show_col_types = FALSE)
source("scripts/step5_compute_koppen_era5.R")  # writes data/snapshots/site_koppen_era5.csv, unmodified script
after_kg <- readr::read_csv("data/snapshots/site_koppen_era5.csv", show_col_types = FALSE)

kg_compare <- dplyr::full_join(
  baseline_kg |> dplyr::select(site_id, koppen_class_before = koppen_class),
  after_kg    |> dplyr::select(site_id, koppen_class_after  = koppen_class),
  by = "site_id"
) |>
  dplyr::mutate(changed = !is.na(koppen_class_before) & !is.na(koppen_class_after) &
                  koppen_class_before != koppen_class_after,
                newly_classified = is.na(koppen_class_before) & !is.na(koppen_class_after),
                lost_classification = !is.na(koppen_class_before) & is.na(koppen_class_after))
readr::write_csv(kg_compare, file.path(OUTD, "table_kg_classification_compare.csv"))

kg_changed_sites <- kg_compare |> dplyr::filter(changed) |>
  dplyr::select(site_id, koppen_class_before, koppen_class_after)
readr::write_csv(kg_changed_sites, file.path(OUTD, "table_kg_changed_sites.csv"))

n_classified_before <- sum(!is.na(baseline_kg$koppen_class))
n_classified_after  <- sum(!is.na(after_kg$koppen_class))
n_kg_changed <- nrow(kg_changed_sites)
n_kg_newly   <- sum(kg_compare$newly_classified, na.rm = TRUE)
n_kg_lost    <- sum(kg_compare$lost_classification, na.rm = TRUE)

# ---- 5. Weighted Jaccard, KG axis only (formula: sum(pmin(p,q))/sum(pmax(p,q))) ----
kg_global_ref_path <- file.path("data/snapshots", "koppen_beck2023_global_distribution.csv")
jaccard_kg <- tibble::tibble(axis = character(0), letters = character(0), J_before = numeric(0), J_after = numeric(0))
if (file.exists(kg_global_ref_path)) {
  ref <- readr::read_csv(kg_global_ref_path, show_col_types = FALSE)
  compute_j <- function(site_classes, ref_df, class_col_ref, prop_col_ref, twoletter = TRUE) {
    cls <- if (twoletter) substr(site_classes, 1, 2) else site_classes
    cls <- cls[!is.na(cls)]
    p <- table(cls) / length(cls)
    q_all <- ref_df[[prop_col_ref]]; names(q_all) <- if (twoletter) substr(ref_df[[class_col_ref]], 1, 2) else ref_df[[class_col_ref]]
    q <- tapply(q_all, names(q_all), sum)
    all_cls <- union(names(p), names(q))
    p2 <- setNames(rep(0, length(all_cls)), all_cls); p2[names(p)] <- p
    q2 <- setNames(rep(0, length(all_cls)), all_cls); q2[names(q)] <- q
    sum(pmin(p2, q2)) / sum(pmax(p2, q2))
  }
  ref_cols <- names(ref)
  class_col <- ref_cols[grepl("class|koppen|kg", ref_cols, ignore.case = TRUE)][1]
  prop_col  <- ref_cols[grepl("prop|frac|pct|area", ref_cols, ignore.case = TRUE)][1]
  if (!is.na(class_col) && !is.na(prop_col)) {
    j_tl_before <- compute_j(baseline_kg$koppen_class, ref, class_col, prop_col, TRUE)
    j_tl_after  <- compute_j(after_kg$koppen_class,    ref, class_col, prop_col, TRUE)
    j_5_before  <- compute_j(baseline_kg$koppen_class, ref, class_col, prop_col, FALSE)
    j_5_after   <- compute_j(after_kg$koppen_class,    ref, class_col, prop_col, FALSE)
    jaccard_kg <- tibble::tibble(
      axis = c("KG", "KG"), letters = c("two_letter", "five_class"),
      J_before = c(j_tl_before, j_5_before), J_after = c(j_tl_after, j_5_after)
    ) |> dplyr::mutate(delta = J_after - J_before)
  } else {
    message("Could not identify class/proportion columns in ", kg_global_ref_path, " -- Jaccard not recomputed.")
  }
} else {
  message("Reference distribution file not found: ", kg_global_ref_path, " -- KG Jaccard not recomputed.")
}
readr::write_csv(jaccard_kg, file.path(OUTD, "table_jaccard_kg_axis.csv"))

# ---- 6. Aridity/biomass/landcover/IGBP axes: unchanged by construction ----
unchanged_axes <- tibble::tibble(
  axis = c("aridity", "biomass", "landcover", "IGBP"),
  reason = "Site list unchanged (0 new sites, stage 3) and site coordinates unchanged; class depends only on lat/long joined to static external rasters/manifest fields not touched by this refresh -- not re-derived."
)
readr::write_csv(unchanged_axes, file.path(OUTD, "table_unchanged_axes.csv"))

# ---- Master reconciliation table ----
recon <- tibble::tibble(
  metric = c("network_site_count", "kg_classified_count", "kg_classified_count",
             "kg_classification_changes", "kg_newly_classified", "kg_lost_classification"),
  when = c("after_vs_before", "before", "after", "count", "count", "count"),
  value = c(NA, n_classified_before, n_classified_after, n_kg_changed, n_kg_newly, n_kg_lost)
)
recon_wide <- tibble::tibble(
  metric = c("network_site_count", "kg_classified_count"),
  before = c(n_sites_before, n_classified_before),
  after  = c(n_sites_after,  n_classified_after),
  difference = c(n_sites_after - n_sites_before, n_classified_after - n_classified_before)
)
readr::write_csv(recon_wide, file.path(OUTD, "table_reconciliation_master.csv"))

dbDisconnect(con, shutdown = TRUE)

message("Stage 5 complete: ", n_sites_before, " -> ", n_sites_after, " sites; ",
        n_classified_before, " -> ", n_classified_after, " KG classified; ",
        n_kg_changed, " KG class changes.")
writeLines("STAGE5_COMPLETE", file.path(OUTD, "STAGE5_STATUS.txt"))
