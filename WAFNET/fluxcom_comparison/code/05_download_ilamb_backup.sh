#!/usr/bin/env bash
## 05_download_ilamb_backup.sh — Stage 2 step 2: backup copy of the original
## FLUXCOM ensemble, via the ILAMB reference data collection.
##
## This is a documentation-only backup copy of the ORIGINAL FLUXCOM product
## (the one chosen in Ossenatou Mamadou's 16 October reply, now unobtainable
## from its native MPI-BGC portal — see docs/report_back_2026-11-03.md). It is
## NOT used in the WAFNET x FLUXCOM-X-BASE comparison computed by this Stage 2
## work; it is kept only so the original product remains available on disk if
## it is needed later. Non-Shuttle data (CLAUDE.md Hard Rule 1) — label
## accordingly in any output that touches it.
##
## Source: ILAMB-Data reference collection, https://www.ilamb.org/ILAMB-Data/DATA/
## (anonymous HTTP, no login or licence click-through found).
##
## Output (gitignored, too large for git):
##   data/external/fluxcom_original_ilamb/{gpp,reco,le,sh}.nc
## Output (git-tracked):
##   WAFNET/fluxcom_comparison/tables/fluxcom_original_ilamb_manifest.csv
##
## Usage: bash WAFNET/fluxcom_comparison/code/05_download_ilamb_backup.sh

set -euo pipefail

BASE_URL="https://www.ilamb.org/ILAMB-Data/DATA"
OUT_DIR="data/external/fluxcom_original_ilamb"
MANIFEST="WAFNET/fluxcom_comparison/tables/fluxcom_original_ilamb_manifest.csv"
LOG_DIR="WAFNET/fluxcom_comparison/logs"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_FILE="${LOG_DIR}/05_download_ilamb_backup_${TIMESTAMP}.log"
CITATION_GPP_RECO="Jung, M. et al. (2020). Biogeosciences 17:1343-1365, doi:10.5194/bg-17-1343-2020 (FLUXCOM carbon RS+METEO; ILAMB reference copy)."
CITATION_LE_SH="Jung, M. et al. (2019). Scientific Data 6:74, doi:10.1038/s41597-019-0076-8 (FLUXCOM energy RS+METEO; ILAMB reference copy)."
LICENCE="CC BY 4.0 (FLUXCOM product licence; ILAMB redistribution for benchmarking use)"

mkdir -p "${OUT_DIR}" "${LOG_DIR}" "$(dirname "${MANIFEST}")"
log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "${LOG_FILE}"; }

log "=== FLUXCOM original-ensemble ILAMB backup download starting (non-Shuttle, CLAUDE.md Hard Rule 1; NOT used in the comparison) ==="
log "PID: $$"

echo "variable,ilamb_path,filename,url,expected_size_bytes,actual_size_bytes,sha256_base64,download_datetime_utc,licence,citation" > "${MANIFEST}"

DOWNLOAD_DATETIME_UTC=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

declare -a ITEMS=(
  "gpp|gpp/FLUXCOM/gpp.nc|gpp.nc|${CITATION_GPP_RECO}"
  "reco|reco/FLUXCOM/reco.nc|reco.nc|${CITATION_GPP_RECO}"
  "le|hfls/FLUXCOM/le.nc|le.nc|${CITATION_LE_SH}"
  "sh|hfss/FLUXCOM/sh.nc|sh.nc|${CITATION_LE_SH}"
)

n_ok=0; n_skip=0; n_fail=0

for item in "${ITEMS[@]}"; do
  IFS='|' read -r VARIABLE ILAMB_PATH FILENAME CITATION <<< "${item}"
  URL="${BASE_URL}/${ILAMB_PATH}"
  LOCAL_PATH="${OUT_DIR}/${FILENAME}"

  EXPECTED_SIZE=$(curl -sI "${URL}" | grep -i '^content-length' | tr -d '\r' | awk '{print $2}')
  log "${VARIABLE}: ${URL} (expected ${EXPECTED_SIZE} bytes)"

  if [[ -f "${LOCAL_PATH}" ]]; then
    ACTUAL_SIZE=$(stat -f%z "${LOCAL_PATH}" 2>/dev/null || stat -c%s "${LOCAL_PATH}" 2>/dev/null || echo 0)
    if [[ "${ACTUAL_SIZE}" -eq "${EXPECTED_SIZE}" ]]; then
      log "  SKIP: already present with correct size"
      n_skip=$((n_skip + 1))
    else
      log "  size mismatch on disk (${ACTUAL_SIZE} vs ${EXPECTED_SIZE}); re-downloading"
      rm -f "${LOCAL_PATH}"
    fi
  fi

  if [[ ! -f "${LOCAL_PATH}" ]]; then
    curl -s -L --retry 3 --retry-delay 10 -o "${LOCAL_PATH}.tmp" "${URL}"
    ACTUAL_SIZE=$(stat -f%z "${LOCAL_PATH}.tmp" 2>/dev/null || stat -c%s "${LOCAL_PATH}.tmp" 2>/dev/null || echo 0)
    if [[ "${ACTUAL_SIZE}" -ne "${EXPECTED_SIZE}" ]]; then
      log "  FAILED: downloaded ${ACTUAL_SIZE} bytes, expected ${EXPECTED_SIZE}"
      rm -f "${LOCAL_PATH}.tmp"
      n_fail=$((n_fail + 1))
      echo "${VARIABLE},${ILAMB_PATH},${FILENAME},${URL},${EXPECTED_SIZE},${ACTUAL_SIZE},FAILED,${DOWNLOAD_DATETIME_UTC},\"${LICENCE}\",\"${CITATION}\"" >> "${MANIFEST}"
      continue
    fi
    mv "${LOCAL_PATH}.tmp" "${LOCAL_PATH}"
    log "  OK: ${ACTUAL_SIZE} bytes"
    n_ok=$((n_ok + 1))
  fi

  SHA256_B64=$(openssl dgst -sha256 -binary "${LOCAL_PATH}" | openssl base64 -A)
  echo "${VARIABLE},${ILAMB_PATH},${FILENAME},${URL},${EXPECTED_SIZE},${ACTUAL_SIZE},${SHA256_B64},${DOWNLOAD_DATETIME_UTC},\"${LICENCE}\",\"${CITATION}\"" >> "${MANIFEST}"
done

log "=== DOWNLOAD COMPLETE === OK: ${n_ok}  Skipped: ${n_skip}  Failed: ${n_fail}"
log "Manifest written: ${MANIFEST}"

# --- .meta.json companion (CLAUDE.md "Output Metadata"; pure bash, same
# fields as WAFNET/write_meta.R's R helper) ----------------------------------
META_PATH="${MANIFEST%.csv}.meta.json"
RUN_DT=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
GIT_SHA=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
cat > "${META_PATH}" <<EOF
{
  "run_datetime_utc": "${RUN_DT}",
  "pipeline_version": "${GIT_SHA}",
  "input_sources": ["${BASE_URL}/gpp/FLUXCOM/gpp.nc", "${BASE_URL}/reco/FLUXCOM/reco.nc", "${BASE_URL}/hfls/FLUXCOM/le.nc", "${BASE_URL}/hfss/FLUXCOM/sh.nc"],
  "notes": "Documentation-only backup copy of the ORIGINAL FLUXCOM ensemble (non-Shuttle comparison data, CLAUDE.md Hard Rule 1) -- NOT used in the WAFNET x FLUXCOM-X-BASE comparison. gpp.nc/reco.nc: FLUXCOM RS+METEO, CRUNCEPv6 forcing, ANN method, 1979-12 to 2013-11, g m-2 day-1, 0.5deg. le.nc/sh.nc: FLUXCOM Energy RS+METEO, GSWP3v1 forcing, 1979-12 to 2014-11, watt/m2 (converted from native MJ/m2/day), 0.5deg. See docs/report_back_2026-11-03.md section 2 for the gpp.nc/reco.nc references-attribute quirk (cites the energy product papers, not Jung et al. 2020 -- likely an ILAMB packaging artefact, reported as observed not resolved)."
}
EOF
log "Metadata written: ${META_PATH}"

if [[ "${n_fail}" -gt 0 ]]; then
  exit 1
fi
exit 0
