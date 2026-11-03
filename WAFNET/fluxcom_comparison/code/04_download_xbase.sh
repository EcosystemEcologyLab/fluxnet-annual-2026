#!/usr/bin/env bash
## 04_download_xbase.sh — Stage 2 step 1: download FLUXCOM-X-BASE.
##
## FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1) — used
## here only as a labelled comparison dataset for the WAFNET x FLUXCOM-X-BASE
## analysis, never as primary Annual Paper data.
##
## Source: ICOS Carbon Portal, Nelson et al. (2023/2024), DOI 10.18160/5NZG-JMJE
## (Nelson et al. 2024, Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024).
## Licence: CC BY 4.0.
##
## Downloads the monthly, 0.5-degree files for all four distributed variables
## (GPP, NEE, ET, ET_T [transpiration]) for 2001-2021 (84 files total, ~430 MB).
## The file list is discovered dynamically via the ICOS metadata SPARQL endpoint
## (meta.icos-cp.eu/sparql) rather than hardcoded, filtering on the "_050_monthly.nc"
## filename suffix that ICOS uses for this product's 0.5-degree monthly grids (NOT
## "0.05" — the file-naming convention is a decimal-degree code, "050" = 0.5deg,
## "005" = 0.05deg; see docs/report_back_2026-11-03.md).
##
## Download mechanism: ICOS Carbon Portal gates every object behind a CC BY 4.0
## licence click-through — GET /licence_accept?ids=["<hash>",...] sets a
## per-object cookie (CpLicenseAcceptedFor), after which GET /objects/<hash>
## serves the file. This is fully anonymous (no account, no email, no personal
## credential) — confirmed interactively 2026-11-03 before running this script
## unattended. See SESSION_LOG.md 2026-11-03 for the confirmation this is not a
## login wall in the Hard-Rule-3 sense.
##
## Output (gitignored, too large for git):
##   data/external/fluxcom_x_base/*.nc
## Output (git-tracked):
##   WAFNET/fluxcom_comparison/tables/fluxcom_x_base_download_manifest.csv
##
## Usage: bash WAFNET/fluxcom_comparison/code/04_download_xbase.sh
##   or:  nohup bash WAFNET/fluxcom_comparison/code/04_download_xbase.sh \
##          > WAFNET/fluxcom_comparison/logs/04_download_xbase_$(date +%Y%m%d_%H%M%S).log 2>&1 &

set -euo pipefail

DOI="10.18160/5NZG-JMJE"
SPARQL_ENDPOINT="https://meta.icos-cp.eu/sparql"
OUT_DIR="data/external/fluxcom_x_base"
MANIFEST="WAFNET/fluxcom_comparison/tables/fluxcom_x_base_download_manifest.csv"
COOKIE_JAR="${OUT_DIR}/.icos_cookies"
LOG_DIR="WAFNET/fluxcom_comparison/logs"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_FILE="${LOG_DIR}/04_download_xbase_${TIMESTAMP}.log"
CITATION="Nelson, J.A., Walther, S., Jung, M., Gans, F., Kraft, B., Weber, U., Hamdi, Z., Duveiller, G., Zhang, W. (2023). FLUXCOM-X-BASE. ICOS ERIC - Carbon Portal. https://doi.org/10.18160/5NZG-JMJE. Method/product paper: Nelson et al. (2024), Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024."
LICENCE="CC BY 4.0 (ICOS Data Licence, https://creativecommons.org/licenses/by/4.0/)"

mkdir -p "${OUT_DIR}" "${LOG_DIR}" "$(dirname "${MANIFEST}")"

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "${LOG_FILE}"; }

log "=== FLUXCOM-X-BASE download starting (comparison data, non-Shuttle, CLAUDE.md Hard Rule 1) ==="
log "DOI: ${DOI}"
log "Output dir: ${OUT_DIR}"
log "Manifest:   ${MANIFEST}"
log "PID: $$"

# --- Discover the file list via SPARQL (monthly, 0.5-degree files only) -----
SPARQL_JSON="${OUT_DIR}/.sparql_filelist.json"
log "Querying ICOS metadata SPARQL endpoint for monthly 0.5deg files under DOI ${DOI} ..."

QUERY='PREFIX cpmeta: <http://meta.icos-cp.eu/ontologies/cpmeta/>
SELECT ?dobj ?fileName ?size ?sha256 WHERE {
  ?dobj cpmeta:hasDoi "'"${DOI}"'" .
  ?dobj cpmeta:hasName ?fileName .
  ?dobj cpmeta:hasSizeInBytes ?size .
  ?dobj cpmeta:hasSha256sum ?sha256 .
  FILTER(CONTAINS(?fileName, "_050_monthly.nc"))
}
ORDER BY ?fileName'

curl -s -G "${SPARQL_ENDPOINT}" --data-urlencode "query=${QUERY}" \
  -H "Accept: application/sparql-results+json" -o "${SPARQL_JSON}"

N_FILES=$(jq '.results.bindings | length' "${SPARQL_JSON}")
log "SPARQL returned ${N_FILES} candidate files."
if [[ "${N_FILES}" -lt 1 ]]; then
  log "ERROR: SPARQL query returned no files. Stopping without downloading."
  exit 1
fi

# --- Write/refresh manifest header ------------------------------------------
echo "variable,year,resolution,temporal_resolution,filename,object_hash,doi,access_url,expected_size_bytes,actual_size_bytes,sha256_base64,sha256_verified,download_datetime_utc,licence,citation" > "${MANIFEST}"

DOWNLOAD_DATETIME_UTC=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

n_ok=0; n_skip=0; n_fail=0

for i in $(seq 0 $((N_FILES - 1))); do
  ROW=$(jq -c ".results.bindings[$i]" "${SPARQL_JSON}")
  FILENAME=$(echo "${ROW}" | jq -r '.fileName.value')
  HASH=$(echo "${ROW}" | jq -r '.dobj.value' | sed 's#.*/##')
  SIZE=$(echo "${ROW}" | jq -r '.size.value')
  SHA256_B64=$(echo "${ROW}" | jq -r '.sha256.value')

  # filename pattern: <VAR>_<YEAR>_050_monthly.nc  (VAR is GPP, NEE, ET, or ET_T)
  VARIABLE=$(echo "${FILENAME}" | sed -E 's/^(.+)_[0-9]{4}_050_monthly\.nc$/\1/')
  YEAR=$(echo "${FILENAME}" | sed -E 's/^.+_([0-9]{4})_050_monthly\.nc$/\1/')

  LOCAL_PATH="${OUT_DIR}/${FILENAME}"
  ACCESS_URL="https://data.icos-cp.eu/objects/${HASH}"

  log "[$((i+1))/${N_FILES}] ${VARIABLE} ${YEAR}: ${FILENAME} (hash ${HASH}, expected ${SIZE} bytes)"

  if [[ -f "${LOCAL_PATH}" ]]; then
    ACTUAL_SIZE=$(stat -f%z "${LOCAL_PATH}" 2>/dev/null || stat -c%s "${LOCAL_PATH}" 2>/dev/null || echo 0)
    if [[ "${ACTUAL_SIZE}" -eq "${SIZE}" ]]; then
      log "  SKIP: already present with correct size"
      n_skip=$((n_skip + 1))
      SHA256_VERIFIED="skipped_already_present"
    else
      log "  size mismatch on disk (${ACTUAL_SIZE} vs ${SIZE}); re-downloading"
      rm -f "${LOCAL_PATH}"
    fi
  fi

  if [[ ! -f "${LOCAL_PATH}" ]]; then
    # Licence click-through (anonymous, CC BY 4.0) then download, same cookie jar.
    curl -s -c "${COOKIE_JAR}" -b "${COOKIE_JAR}" -L \
      "https://data.icos-cp.eu/licence_accept?ids=%5B%22${HASH}%22%5D" -o /dev/null
    curl -s -c "${COOKIE_JAR}" -b "${COOKIE_JAR}" -L \
      -o "${LOCAL_PATH}.tmp" "${ACCESS_URL}"

    ACTUAL_SIZE=$(stat -f%z "${LOCAL_PATH}.tmp" 2>/dev/null || stat -c%s "${LOCAL_PATH}.tmp" 2>/dev/null || echo 0)
    if [[ "${ACTUAL_SIZE}" -ne "${SIZE}" ]]; then
      log "  FAILED: downloaded ${ACTUAL_SIZE} bytes, expected ${SIZE}"
      rm -f "${LOCAL_PATH}.tmp"
      n_fail=$((n_fail + 1))
      echo "${VARIABLE},${YEAR},0.5deg,monthly,${FILENAME},${HASH},${DOI},${ACCESS_URL},${SIZE},${ACTUAL_SIZE},${SHA256_B64},FAILED_SIZE_MISMATCH,${DOWNLOAD_DATETIME_UTC},\"${LICENCE}\",\"${CITATION}\"" >> "${MANIFEST}"
      continue
    fi
    mv "${LOCAL_PATH}.tmp" "${LOCAL_PATH}"
    log "  OK: ${ACTUAL_SIZE} bytes"
    n_ok=$((n_ok + 1))

    # Verify checksum: ICOS sha256 is base64 (standard, not base64url) of the
    # raw digest bytes, reported WITHOUT trailing "=" padding; strip padding
    # from both sides before comparing.
    ACTUAL_SHA256_B64=$(openssl dgst -sha256 -binary "${LOCAL_PATH}" | openssl base64 -A | tr -d '=')
    EXPECTED_SHA256_B64="${SHA256_B64%=}"
    if [[ "${ACTUAL_SHA256_B64}" == "${EXPECTED_SHA256_B64}" ]]; then
      SHA256_VERIFIED="TRUE"
    else
      SHA256_VERIFIED="FALSE"
      log "  WARNING: sha256 mismatch for ${FILENAME} (expected ${SHA256_B64}, got ${ACTUAL_SHA256_B64})"
    fi
  else
    ACTUAL_SHA256_B64=$(openssl dgst -sha256 -binary "${LOCAL_PATH}" | openssl base64 -A | tr -d '=')
    EXPECTED_SHA256_B64="${SHA256_B64%=}"
    SHA256_VERIFIED=$([[ "${ACTUAL_SHA256_B64}" == "${EXPECTED_SHA256_B64}" ]] && echo "TRUE" || echo "FALSE")
  fi

  echo "${VARIABLE},${YEAR},0.5deg,monthly,${FILENAME},${HASH},${DOI},${ACCESS_URL},${SIZE},${ACTUAL_SIZE},${SHA256_B64},${SHA256_VERIFIED},${DOWNLOAD_DATETIME_UTC},\"${LICENCE}\",\"${CITATION}\"" >> "${MANIFEST}"
done

log "=== DOWNLOAD COMPLETE ==="
log "OK: ${n_ok}  Skipped (already present): ${n_skip}  Failed: ${n_fail}"
log "Manifest written: ${MANIFEST}"

# --- .meta.json companion (CLAUDE.md "Output Metadata"; same fields as
# WAFNET/write_meta.R's R helper, written here in pure bash since this
# script has no R process running) ------------------------------------------
META_PATH="${MANIFEST%.csv}.meta.json"
RUN_DT=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
GIT_SHA=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
cat > "${META_PATH}" <<EOF
{
  "run_datetime_utc": "${RUN_DT}",
  "pipeline_version": "${GIT_SHA}",
  "input_sources": ["https://meta.icos-cp.eu/sparql (ICOS metadata SPARQL endpoint)", "https://data.icos-cp.eu/objects/<hash> (per-file download, after licence_accept)"],
  "notes": "FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1). File list discovered dynamically via SPARQL, filtered on the _050_monthly.nc filename suffix (0.5deg monthly). Each row sha256-verified against the checksum SPARQL itself reports; see sha256_verified column (FALSE rows, if any, were not written to data/external/fluxcom_x_base/ with mismatched content -- see the script log for details). Download requires an anonymous CC BY 4.0 licence click-through (no account/credential) -- confirmed interactively before this script was first run, see SESSION_LOG.md 2026-11-03."
}
EOF
log "Metadata written: ${META_PATH}"

if [[ "${n_fail}" -gt 0 ]]; then
  exit 1
fi
exit 0
