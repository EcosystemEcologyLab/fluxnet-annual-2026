#!/usr/bin/env bash
# download_trendy_v14.sh
# Download GCB TRENDY v14-gcb2025 S3 outputs (nbp, gpp, evapotrans, ra, rh)
# for all 20 contributing models from the anonymous Wasabi S3 bucket.
#
# Source: https://s3.eu-west-1.wasabisys.com/gcb-2025-upload/
# Manifest: data/external/trendy/download_manifest.csv
# Terms: CC-BY acknowledgment; cite Sitch et al. 2024 doi:10.1029/2024GB008102
#
# Usage: bash scripts/download_trendy_v14.sh
#   or:  nohup bash scripts/download_trendy_v14.sh > logs/trendy_download_nohup.out 2>&1 &

set -euo pipefail

MANIFEST="data/external/trendy/download_manifest.csv"
LOG_DIR="logs"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_FILE="${LOG_DIR}/trendy_download_${TIMESTAMP}.log"
LOCAL_ROOT="data/external/trendy/v14-gcb2025"

MAX_RETRIES=3
CONNECT_TIMEOUT=30   # seconds to establish connection
MAX_TIME=7200        # 2-hour cap per file (DLEM/JULES-ES can be ~8 GB)

mkdir -p "${LOG_DIR}"
mkdir -p "${LOCAL_ROOT}"

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "${LOG_FILE}"; }

log "=== TRENDY v14-gcb2025 download starting ==="
log "Manifest:  ${MANIFEST}"
log "Log file:  ${LOG_FILE}"
log "Local root: ${LOCAL_ROOT}"
log "PID: $$"

if [[ ! -f "${MANIFEST}" ]]; then
    log "ERROR: Manifest not found: ${MANIFEST}"
    exit 1
fi

# Count total files and bytes from manifest (skip header)
TOTAL_FILES=$(tail -n +2 "${MANIFEST}" | wc -l | tr -d ' ')
TOTAL_BYTES=$(tail -n +2 "${MANIFEST}" | awk -F',' '{sum += $5} END {print sum}')
log "Files to download: ${TOTAL_FILES}"
log "Expected total:    $(echo "scale=1; ${TOTAL_BYTES}/1073741824" | bc) GB"
log ""

n_ok=0
n_skip=0
n_fail=0
n_total=0
bytes_downloaded=0
FAILED_FILES=()
START_TIME=$(date +%s)

# Read manifest CSV (skip header)
while IFS=',' read -r model variable filename url expected_size local_path; do
    [[ "${model}" == "model" ]] && continue   # skip header
    n_total=$((n_total + 1))

    # Create directory for this model
    dir=$(dirname "${local_path}")
    mkdir -p "${dir}"

    log "[${n_total}/${TOTAL_FILES}] ${model}/${variable} — ${filename}"
    log "  URL:  ${url}"
    log "  Dest: ${local_path}"
    log "  Expected: $(echo "scale=0; ${expected_size}/1048576" | bc) MB"

    # Skip if file already exists with the correct size
    if [[ -f "${local_path}" ]]; then
        actual_size=$(stat -f%z "${local_path}" 2>/dev/null || stat -c%s "${local_path}" 2>/dev/null || echo 0)
        if [[ "${actual_size}" -eq "${expected_size}" ]]; then
            log "  SKIP: already exists with correct size"
            n_skip=$((n_skip + 1))
            continue
        else
            log "  RESUME: exists but size mismatch (${actual_size} vs ${expected_size}); re-downloading"
            rm -f "${local_path}"
        fi
    fi

    # Download with retries
    success=0
    for attempt in $(seq 1 ${MAX_RETRIES}); do
        log "  Attempt ${attempt}/${MAX_RETRIES} ..."
        if curl -L \
               --connect-timeout "${CONNECT_TIMEOUT}" \
               --max-time "${MAX_TIME}" \
               --retry 2 \
               --retry-delay 10 \
               --retry-max-time 300 \
               -o "${local_path}.tmp" \
               --progress-bar \
               "${url}" 2>>"${LOG_FILE}"; then

            # Verify size
            actual_size=$(stat -f%z "${local_path}.tmp" 2>/dev/null || stat -c%s "${local_path}.tmp" 2>/dev/null || echo 0)
            if [[ "${actual_size}" -eq "${expected_size}" ]]; then
                mv "${local_path}.tmp" "${local_path}"
                log "  OK: ${actual_size} bytes"
                n_ok=$((n_ok + 1))
                bytes_downloaded=$((bytes_downloaded + actual_size))
                success=1
                break
            else
                log "  SIZE MISMATCH: got ${actual_size}, expected ${expected_size} — retrying"
                rm -f "${local_path}.tmp"
            fi
        else
            log "  curl failed (exit $?) — retrying"
            rm -f "${local_path}.tmp"
        fi
        sleep 15
    done

    if [[ "${success}" -eq 0 ]]; then
        log "  FAILED after ${MAX_RETRIES} attempts: ${filename}"
        n_fail=$((n_fail + 1))
        FAILED_FILES+=("${model}/${variable}/${filename}")
    fi

    log ""
done < "${MANIFEST}"

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))
ELAPSED_H=$((ELAPSED / 3600))
ELAPSED_M=$(( (ELAPSED % 3600) / 60 ))

log "=========================================="
log "=== DOWNLOAD COMPLETE ==="
log "=========================================="
log "Duration:    ${ELAPSED_H}h ${ELAPSED_M}m"
log "Total files: ${TOTAL_FILES}"
log "  Succeeded: ${n_ok}"
log "  Skipped (already present): ${n_skip}"
log "  Failed:    ${n_fail}"
log "Bytes downloaded: $(echo "scale=1; ${bytes_downloaded}/1073741824" | bc) GB"

if [[ "${n_fail}" -gt 0 ]]; then
    log ""
    log "FAILED FILES:"
    for f in "${FAILED_FILES[@]}"; do
        log "  ${f}"
    done
    exit 1
fi

log ""
log "All files downloaded successfully."
exit 0
