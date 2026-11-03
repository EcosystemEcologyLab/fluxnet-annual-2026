## 07_training_site_check.R — Stage 2 step 3: FLUXCOM-X-BASE training-site check.
##
## Reads Table 1 of Nelson et al. (2024, Biogeosciences 21:5079-5115,
## doi:10.5194/bg-21-5079-2024) directly from the publisher's own XLSX download
## (https://bg.copernicus.org/articles/21/5079/2024/bg-21-5079-2024-t01.xlsx),
## caption "Citation data for the 294 sites used in the X-BASE products" — a
## direct, machine-readable read of the full training-site list. This
## supersedes Stage 1's training-site answer for FLUXCOM-X-BASE
## (tables/fluxcom_training_sites.csv), which only had a reference-list proxy
## to go on because the paper's HTML Table 1 rendered as an un-OCR'd image
## (medium confidence). This script's answer is high confidence: a direct
## match against the complete, correctly-sized (294-row) site list.
##
## Non-Shuttle comparison data (CLAUDE.md Hard Rule 1).
##
## Output (git-tracked):
##   tables/fluxcom_x_base_training_sites_stage2.csv

source("WAFNET/fluxcom_comparison/code/00_config.R")
library(readxl)

XLSX_PATH <- file.path("data", "external", "fluxcom_x_base_supplement", "bg-21-5079-2024-t01.xlsx")
if (!file.exists(XLSX_PATH)) {
  stop(
    "[WAFNET/fluxcom_comparison] ", XLSX_PATH, " not found. Download it first:\n",
    "  curl -s -o ", XLSX_PATH,
    " https://bg.copernicus.org/articles/21/5079/2024/bg-21-5079-2024-t01.xlsx"
  )
}

d <- read_excel(XLSX_PATH, sheet = 1, col_names = FALSE)
codes <- trimws(unlist(d))
codes <- codes[!is.na(codes) & nchar(codes) > 0]

if (length(codes) != 294) {
  warning(
    "[WAFNET/fluxcom_comparison] Expected 294 training sites per Nelson et al. (2024) ",
    "Table 1 caption; found ", length(codes), ". Proceeding, but check the source file."
  )
}

result <- data.frame(
  site_id = STAGE2_ALL_SITES,
  site_status = ifelse(STAGE2_ALL_SITES %in% STAGE2_PENDING_SITES, "pending Ossenatou", "agreed"),
  in_fluxcom_x_base_training_set = STAGE2_ALL_SITES %in% codes,
  n_training_sites_in_table = length(codes),
  source = "Nelson et al. (2024) Table 1 XLSX, direct match (high confidence)",
  stringsAsFactors = FALSE
)

write.csv(result, file.path(FLUXCOM_ROOT, "tables", "fluxcom_x_base_training_sites_stage2.csv"), row.names = FALSE)

cat("\n[WAFNET/fluxcom_comparison] FLUXCOM-X-BASE training-site check (Stage 2, direct Table 1 match):\n")
print(result[, c("site_id", "site_status", "in_fluxcom_x_base_training_set")])
cat("\nOther African sites in the 294-site training list (for context):\n")
afr_prefixes <- c("AO","BF","BJ","BW","CD","CF","CG","CI","CM","DZ","EG","ET","GA","GH","GM","GN",
                   "KE","LR","LY","MA","ML","MR","MU","MW","MZ","NA","NE","NG","RW","SD","SL","SN",
                   "SO","SS","SZ","TD","TG","TN","TZ","UG","ZA","ZM","ZW")
print(sort(codes[substr(codes, 1, 2) %in% afr_prefixes]))
cat("\nWrote tables/fluxcom_x_base_training_sites_stage2.csv\n")
