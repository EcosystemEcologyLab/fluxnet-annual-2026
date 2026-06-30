#!/usr/bin/env bash
## download_fluxnet2015.sh
##
## Downloads the original FLUXNET2015 release (Pastorello et al. 2020,
## doi:10.1038/s41597-020-0534-3) FULLSET product ZIPs for the FLUXNET2015
## comparison site list, distinct from any current FLUXNET Shuttle
## reprocessing of the same sites (see SESSION_LOG.md 2026-06-30,
## "FLUXNET2015 release YY data inventory").
##
## Mechanism (confirmed by direct investigation 2026-06-30 — see
## SESSION_LOG.md "FLUXNET2015 portal investigation and download"):
##   1. Log in at https://fluxnet.org/login/ (WordPress session login,
##      FLUXNET_USERNAME/FLUXNET_PASSWORD from .env) to obtain the
##      account's registered user_id/email from the post-login
##      "Download Historical Data" page. The data API below does not
##      itself require the session cookie, but logging in is the
##      sanctioned access path and avoids hardcoding account details
##      (user_id/email) in this committed script.
##   2. POST a per-site JSON request to the AmeriFlux Cyberinfrastructure
##      data-delivery API (https://amfcdn.lbl.gov/api/v1/data_download),
##      the same backend used by fluxnet.org's own download form. The
##      response includes a direct, unauthenticated download URL, expected
##      size, and an MD5 checksum for that site's FULLSET ZIP.
##   3. Download the ZIP from that URL to data/raw/fluxnet2015/<site_id>/.
##
## ZIP contents include all five temporal resolutions (HR/DD/WW/MM/YY) plus
## AUX files for that site; the YY product inside is named
##   FLX_<site_id>_FLUXNET2015_FULLSET_YY_<years>_<sitever>-<codever>.csv
## This script does NOT extract ZIPs — extraction is a separate concern.
##
## Site scope: 206 of the 212 sites in data/snapshots/sites_fluxnet2015_clean.csv.
## Six sites (RU-Sam, RU-SkP, RU-Tks, RU-Vrk, SE-St1, ZA-Kru) are Tier-2-only
## under the FLUXNET2015 Data Policy. Tier 2 requires contacting data
## providers before any publication that uses the data and can carry a
## co-authorship obligation, and mixing CC-BY-4.0 and Tier 2 data means the
## *entire* synthesis must be treated as Tier 2. Per user decision
## (2026-06-30), this download is scoped to the open CC-BY-4.0 tier only,
## so those six sites are deliberately excluded here.
##
## Data policy / citation (CC-BY-4.0 tier — see https://fluxnet.org/data/data-policy/):
##   - Cite Pastorello et al. 2020, doi:10.1038/s41597-020-0534-3
##   - List each site used by its FLUXNET ID and/or per-site DOI
##   - (Recommended, not required) inform data providers of forthcoming
##     publications — pi_contact_emails returned by the API for each site
##     are logged to logs/fluxnet2015_pi_contacts_<timestamp>.csv for this
##     purpose.
##
## Usage:
##   bash scripts/download_fluxnet2015.sh
##   nohup caffeinate -dimsu bash scripts/download_fluxnet2015.sh \
##       > logs/fluxnet2015_download_nohup.out 2>&1 &

set -uo pipefail

SITE_CSV="${SITE_CSV:-data/snapshots/sites_fluxnet2015_clean.csv}"
RAW_DIR="data/raw/fluxnet2015"
LOG_DIR="logs"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_FILE="${LOG_DIR}/fluxnet2015_download_${TIMESTAMP}.log"
PI_CONTACTS_FILE="${LOG_DIR}/fluxnet2015_pi_contacts_${TIMESTAMP}.csv"

LOGIN_URL="https://fluxnet.org/login/"
HISTORICAL_URL="https://fluxnet.org/data/download-historical-data/"
API_URL="https://amfcdn.lbl.gov/api/v1/data_download"
UA="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"

DATA_POLICY="CCBY4.0"
DATA_PRODUCT="FLUXNET2015"
DATA_VARIANT="FULLSET"
INTENDED_USE="Research - Multi-site synthesis"
DESCRIPTION="FLUXNET2015 release YY product requested as a reference comparison axis against current FLUXNET Shuttle reprocessing, for the FLUXNET Annual Paper 2026 multi-site synthesis (PI: Trevor Keenan; Co-PIs: David Moore, Kim Novick)."

MAX_RETRIES=3
BASE_BACKOFF=10   # seconds; doubles each retry (10, 20, 40)
CONNECT_TIMEOUT=30
API_MAX_TIME=120
DOWNLOAD_MAX_TIME=1800

# Tier-2-only sites excluded from this CC-BY-4.0-scoped download (see header).
TIER2_ONLY_SITES=(RU-Sam RU-SkP RU-Tks RU-Vrk SE-St1 ZA-Kru)

mkdir -p "${LOG_DIR}" "${RAW_DIR}"

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "${LOG_FILE}"; }

COOKIE_JAR=$(mktemp)
LOGIN_RESPONSE=$(mktemp)
cleanup() { rm -f "${COOKIE_JAR}" "${LOGIN_RESPONSE}"; }
trap cleanup EXIT

log "=== FLUXNET2015 release download starting ==="
log "Site list: ${SITE_CSV}"
log "Raw dir:   ${RAW_DIR}"
log "Log file:  ${LOG_FILE}"
log "PID: $$"

if [[ ! -f "${SITE_CSV}" ]]; then
    log "ERROR: Site list not found: ${SITE_CSV}"
    exit 1
fi

# ---- Credentials: read from .env, never logged ------------------------------
if [[ ! -f ".env" ]]; then
    log "ERROR: .env not found"
    exit 1
fi
FLUXNET_USERNAME=$(grep -E '^FLUXNET_USERNAME=' .env | head -1 | cut -d'=' -f2-)
FLUXNET_PASSWORD=$(grep -E '^FLUXNET_PASSWORD=' .env | head -1 | cut -d'=' -f2-)
if [[ -z "${FLUXNET_USERNAME}" || -z "${FLUXNET_PASSWORD}" ]]; then
    log "ERROR: FLUXNET_USERNAME / FLUXNET_PASSWORD not set in .env"
    exit 1
fi

# ---- Log in to establish a session, then read account user_id/email --------
log "Logging in to fluxnet.org as registered user..."
curl -s -c "${COOKIE_JAR}" -b "${COOKIE_JAR}" -A "${UA}" "${LOGIN_URL}" -o /dev/null || true
curl -s -c "${COOKIE_JAR}" -b "${COOKIE_JAR}" -A "${UA}" \
    --data-urlencode "log=${FLUXNET_USERNAME}" \
    --data-urlencode "pwd=${FLUXNET_PASSWORD}" \
    --data-urlencode "redirect_to=${HISTORICAL_URL}" \
    --data-urlencode "testcookie=1" \
    -L --connect-timeout "${CONNECT_TIMEOUT}" --max-time 60 \
    -o "${LOGIN_RESPONSE}" \
    "${LOGIN_URL}" || true
unset FLUXNET_PASSWORD

FLUXNET_USER_ID=$(grep -oE "var userid = '[^']*'" "${LOGIN_RESPONSE}" | sed -E "s/.*= '([^']*)'/\1/" | head -1)
FLUXNET_USER_EMAIL=$(grep -oE "var email = '[^']*'" "${LOGIN_RESPONSE}" | sed -E "s/.*= '([^']*)'/\1/" | head -1)

if [[ -z "${FLUXNET_USER_ID}" || -z "${FLUXNET_USER_EMAIL}" ]]; then
    log "ERROR: Login did not yield a recognised account (user_id/email not found on ${HISTORICAL_URL})."
    log "       Check FLUXNET_USERNAME/FLUXNET_PASSWORD in .env, or the portal page structure may have changed."
    exit 1
fi
log "Logged in as: ${FLUXNET_USER_ID}"

# ---- Build site list: 212-site CSV minus the 6 Tier-2-only sites -----------
# (avoid mapfile/readarray — macOS ships bash 3.2, which lacks both)
ALL_SITES=()
while IFS=',' read -r site_id _rest; do
    [[ -z "${site_id}" ]] && continue
    ALL_SITES+=("${site_id}")
done < <(tail -n +2 "${SITE_CSV}")
SITES=()
for s in "${ALL_SITES[@]}"; do
    skip=0
    for t2 in "${TIER2_ONLY_SITES[@]}"; do
        [[ "${s}" == "${t2}" ]] && skip=1 && break
    done
    [[ "${skip}" -eq 0 ]] && SITES+=("${s}")
done
N_SITES=${#SITES[@]}
log "Sites in CSV: ${#ALL_SITES[@]}; Tier-2-only excluded: ${#TIER2_ONLY_SITES[@]}; requesting: ${N_SITES}"

echo "site_id,pi_contact_emails,filename,download_timestamp" > "${PI_CONTACTS_FILE}"

n_total=0
n_ok=0
n_skip=0
n_fail=0
bytes_downloaded=0
FAILED_SITES=()
START_TIME=$(date +%s)

for site in "${SITES[@]}"; do
    n_total=$((n_total + 1))
    site_dir="${RAW_DIR}/${site}"
    mkdir -p "${site_dir}"

    log "[${n_total}/${N_SITES}] ${site}"

    # Resume: skip if a valid FULLSET ZIP is already present.
    existing_zip=$(find "${site_dir}" -maxdepth 1 -name "FLX_${site}_FLUXNET2015_FULLSET_*.zip" 2>/dev/null | head -1)
    if [[ -n "${existing_zip}" && -s "${existing_zip}" ]] && unzip -tq "${existing_zip}" >/dev/null 2>&1; then
        log "  SKIP: valid ZIP already present ($(basename "${existing_zip}"))"
        n_skip=$((n_skip + 1))
        continue
    fi
    [[ -n "${existing_zip}" ]] && rm -f "${existing_zip}"

    success=0
    for attempt in $(seq 1 "${MAX_RETRIES}"); do
        log "  Attempt ${attempt}/${MAX_RETRIES}: requesting download URL..."

        payload_file=$(mktemp)
        SITE="${site}" FLUXNET_USER_ID="${FLUXNET_USER_ID}" FLUXNET_USER_EMAIL="${FLUXNET_USER_EMAIL}" \
            INTENDED_USE="${INTENDED_USE}" DESCRIPTION="${DESCRIPTION}" DATA_POLICY="${DATA_POLICY}" \
            DATA_PRODUCT="${DATA_PRODUCT}" DATA_VARIANT="${DATA_VARIANT}" \
            python3 -c '
import json, os
print(json.dumps({
    "user_id": os.environ["FLUXNET_USER_ID"],
    "user_email": os.environ["FLUXNET_USER_EMAIL"],
    "site_ids": [os.environ["SITE"]],
    "intended_use": os.environ["INTENDED_USE"],
    "description": os.environ["DESCRIPTION"],
    "data_policy": os.environ["DATA_POLICY"],
    "data_product": os.environ["DATA_PRODUCT"],
    "data_variant": os.environ["DATA_VARIANT"],
    "is_test": False
}))
' > "${payload_file}"

        resp_file=$(mktemp)
        http_code=$(curl -s -o "${resp_file}" -w "%{http_code}" -A "${UA}" -X POST \
            -H "Content-Type: application/json; charset=utf-8" \
            --data-binary "@${payload_file}" \
            --connect-timeout "${CONNECT_TIMEOUT}" --max-time "${API_MAX_TIME}" \
            "${API_URL}") || http_code="000"
        rm -f "${payload_file}"

        url=$(jq -r '.data_urls[0].url // empty' "${resp_file}" 2>/dev/null)
        expected_size=$(jq -r '.data_urls[0].download_size // empty' "${resp_file}" 2>/dev/null)
        checksum=$(jq -r '.data_urls[0].download_checksum // empty' "${resp_file}" 2>/dev/null)
        pi_emails=$(jq -r '(.pi_contact_emails // []) | join(";")' "${resp_file}" 2>/dev/null)

        if [[ "${http_code}" != "200" || -z "${url}" ]]; then
            log "  API request failed (http=${http_code}, empty response or no data_urls)"
            rm -f "${resp_file}"
        else
            zip_name=$(basename "${url}" | cut -d'?' -f1)
            tmp_zip="${site_dir}/${zip_name}.part"

            log "  Downloading ${zip_name} (expected ${expected_size} bytes)..."
            dl_http=$(curl -s -L -o "${tmp_zip}" -w "%{http_code}" \
                --connect-timeout "${CONNECT_TIMEOUT}" --max-time "${DOWNLOAD_MAX_TIME}" \
                "${url}") || dl_http="000"

            actual_size=$(stat -f%z "${tmp_zip}" 2>/dev/null || stat -c%s "${tmp_zip}" 2>/dev/null || echo 0)
            size_ok=0
            if [[ -n "${expected_size}" ]]; then
                [[ "${actual_size}" == "${expected_size}" ]] && size_ok=1
            else
                [[ "${actual_size}" -gt 0 ]] && size_ok=1
            fi

            checksum_ok=1
            if [[ -n "${checksum}" && -f "${tmp_zip}" ]]; then
                actual_md5=$(md5 -q "${tmp_zip}" 2>/dev/null || md5sum "${tmp_zip}" 2>/dev/null | awk '{print $1}')
                [[ "${actual_md5}" == "${checksum}" ]] || checksum_ok=0
            fi

            zip_ok=0
            [[ -f "${tmp_zip}" ]] && unzip -tq "${tmp_zip}" >/dev/null 2>&1 && zip_ok=1

            if [[ "${dl_http}" == "200" && "${size_ok}" == "1" && "${checksum_ok}" == "1" && "${zip_ok}" == "1" ]]; then
                mv "${tmp_zip}" "${site_dir}/${zip_name}"
                log "  OK: ${zip_name} (${actual_size} bytes, checksum+zip integrity verified)"
                echo "${site},\"${pi_emails}\",${zip_name},$(date '+%Y-%m-%dT%H:%M:%SZ')" >> "${PI_CONTACTS_FILE}"
                bytes_downloaded=$((bytes_downloaded + actual_size))
                success=1
                rm -f "${resp_file}"
                break
            else
                log "  FAILED: http=${dl_http} size_ok=${size_ok} checksum_ok=${checksum_ok} zip_ok=${zip_ok}"
                rm -f "${tmp_zip}" "${resp_file}"
            fi
        fi

        if [[ "${attempt}" -lt "${MAX_RETRIES}" ]]; then
            backoff=$((BASE_BACKOFF * (2 ** (attempt - 1))))
            log "  Retrying in ${backoff}s..."
            sleep "${backoff}"
        fi
    done

    if [[ "${success}" -eq 1 ]]; then
        n_ok=$((n_ok + 1))
    else
        log "  GAVE UP after ${MAX_RETRIES} attempts: ${site}"
        n_fail=$((n_fail + 1))
        FAILED_SITES+=("${site}")
    fi
done

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))
ELAPSED_H=$((ELAPSED / 3600))
ELAPSED_M=$(((ELAPSED % 3600) / 60))

log "=========================================="
log "=== FLUXNET2015 DOWNLOAD COMPLETE ==="
log "=========================================="
log "Duration:    ${ELAPSED_H}h ${ELAPSED_M}m"
log "Sites requested: ${N_SITES}"
log "  Succeeded: ${n_ok}"
log "  Skipped (already present): ${n_skip}"
log "  Failed:    ${n_fail}"
log "Bytes downloaded this run: $(echo "scale=2; ${bytes_downloaded}/1073741824" | bc 2>/dev/null || echo "${bytes_downloaded}") GB"
log "PI contact log: ${PI_CONTACTS_FILE}"

if [[ "${n_fail}" -gt 0 ]]; then
    log ""
    log "FAILED SITES:"
    for f in "${FAILED_SITES[@]}"; do
        log "  ${f}"
    done
    exit 1
fi

log ""
log "All sites downloaded successfully."
exit 0
