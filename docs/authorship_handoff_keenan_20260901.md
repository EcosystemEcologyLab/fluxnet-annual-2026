# Authorship handoff — FLUXNET Annual Paper 2026 — 2026-09-01

**To:** Trevor Keenan (PI, FLUXNET Coordination Project)
**From:** David J.P. Moore, University of Arizona
**Purpose:** Reproducible site list, contact list, and slot method for the
invitation email system. Everything below is derived from a single,
pinned, hash-verified run — reproduce it byte-for-byte before wiring up
any emails, using the instructions in "How to reproduce" below.

---

## 1. The method — authoritative rubric and qualifying-year rule

A year counts toward a site's total if it has **at least one month** of
ONEFlux-processed data (non-NA in any of: `NEE_VUT_REF`, `NEE_CUT_REF`,
`GPP_NT_VUT_REF`, `GPP_DT_VUT_REF`, `GPP_NT_CUT_REF`, `GPP_DT_CUT_REF`,
`RECO_NT_VUT_REF`, `RECO_DT_VUT_REF`, `RECO_NT_CUT_REF`, `RECO_DT_CUT_REF`,
`LE_F_MDS`, `H_F_MDS`, for that month) — credit scales with the **count**
of such years, never with completeness within a year. Recency is anchored
to the 2026 publication: the most recent qualifying year sets the recency
band.

| Years submitted | through 2022 | through 2023 | through 2024 or 2025 |
|---|---|---|---|
| 5 or fewer | 2 | 3 | 4 |
| 6 to 10    | 3 | 4 | 5 |
| 11 to 15   | 4 | 5 | 6 |
| 16 to 20   | 5 | 6 | 7 |
| 21 or more | 6 | 7 | 8 |

"Through 2022" = most recent qualifying year is 2022 or earlier. "Through
2023" = exactly 2023. "Through 2024 or 2025" = 2024 or 2025. This is a
fixed, already-circulated rubric (locked 2026-05-07) — do not vary it when
reproducing.

---

## 2. The two deliverable files

### `outputs/authorship/site_authors.csv` — per-site slots

One row per site. Columns:

| Column | Meaning |
|---|---|
| `site_id` | FLUXNET site ID |
| `submitting_network` | `product_source_network` from the pinned snapshot (9 codes: AMF, EUF, ICOS, JPF, TERN, CNF, KOF, FLX, SAEON) |
| `presence_status` | `"verified"` (has ≥1 qualifying year) or `"no_presence_data"` |
| `years_of_data` | count of qualifying years (≥1 month each) — the rubric's "years submitted" |
| `years_full_year` | count of years with all 12 months present (informational only, not used in the rubric lookup) |
| `years_partial_year` | count of years with 1–11 months present (informational only, not used in the rubric lookup) |
| `latency_years` | `2026 − most_recent_qualifying_year` |
| `n_invited_authors` | the rubric slot count (2–8) |

### `outputs/authorship/authorship_invitations.csv` — per-contact invitation list (what the email system consumes)

Long format: one row per contact person per site.

| Column | Meaning |
|---|---|
| `site_id` | FLUXNET site ID |
| `submitting_network` | same as above |
| `n_invited_authors_for_site` | the slot count from `site_authors.csv`, repeated on every contact row for that site — this is the number the site's PI is allocating among their team |
| `contact_role` | e.g. `PI`, `CO-PI`, `DATA`/`DataManager`, or a raw BADM role string |
| `contact_name` | contact's name |
| `contact_email` | contact's email |
| `contact_source` | `snapshot` (from the pinned snapshot's `team_member_*` fields), `badm_fallback` (site has no snapshot contact at all), `badm_extra` (BADM has a contact not in the snapshot for a site that does have snapshot contacts), or `missing` (no contact in any source) |
| `n_contacts_for_site` | total contact rows for that site across all sources |

Row sort order within a site: PI → CO-PI → DATA/DataManager → everyone
else, alphabetical by name as a tiebreak.

---

## 3. Totals to reproduce

**781 of 781 sites qualify. 3,146 total author slots.**

By network:

| Network | Sites | Slots |
|---|---|---|
| AMF   | 381 | 1538 |
| EUF   | 138 |  515 |
| ICOS  |  80 |  438 |
| JPF   |  54 |  186 |
| TERN  |  52 |  221 |
| CNF   |  32 |  116 |
| KOF   |  21 |   53 |
| FLX   |  18 |   59 |
| SAEON |   5 |   20 |
| **Total** | **781** | **3146** |

15-cell breakdown (years submitted × recency band):

| Years submitted | through 2022 | through 2023 | through 2024/2025 |
|---|---|---|---|
| 5 or fewer | 206 | 23 | 126 |
| 6 to 10    |  99 | 13 | 127 |
| 11 to 15   |  30 |  3 |  45 |
| 16 to 20   |  11 |  4 |  39 |
| 21 or more |   7 |  6 |  42 |

Column totals: 353 / 49 / 379. Row totals: 355 / 239 / 78 / 54 / 55. Sum: 781.

**Join integrity:** every one of the 781 sites received a non-missing
`submitting_network` from the pinned snapshot — 0 sites dropped. (A prior
run, pinned to an older, April-28 snapshot, silently dropped 65 sites this
way; that pin has been corrected — see §5.)

**IT-SR2:** present, `submitting_network = ICOS`. Its source hub changed
from FLX to ICOS between an earlier Shuttle listing and the current one;
that reassignment is still an open question with the Shuttle team (see
`docs/correspondence/shuttle_release_query_20260901.md`), but it does not
affect this run — IT-SR2's row here is keyed on `site_id` alone, joined
against the current snapshot's current hub assignment (ICOS), and it
carries 6 slots (12 qualifying years, most recent 2024).

---

## 4. Contact coverage — where the email system has gaps

From `outputs/authorship/diagnostics/contact_coverage_comparison.csv` and
`authorship_invitations.csv` (2,961 contact rows, 781 sites):

| Source | Contact rows | Sites contributing |
|---|---|---|
| `snapshot` (pinned Shuttle snapshot `team_member_*`) | 2,940 | 779 |
| `badm_fallback` (site has zero snapshot contacts) | 2 | 2 |
| `badm_extra` (BADM has a contact the snapshot doesn't) | 19 | 13 |
| `missing` (no contact anywhere) | 0 | 0 |

**Sites with slots but no routable contact: 0.** Every one of the 781
sites has at least one contact row with a non-empty email address — none
of the 781 slot allocations is currently un-actionable for the email
system. (Site counts above are not mutually exclusive — 13 sites have both
`snapshot` and `badm_extra` contact rows.)

---

## 5. How to reproduce — pinned inputs, script commit, exact commands

**Do not run a fresh `flux_listall()` pull.** The live Shuttle listing
changes between calls (see `docs/shuttle_gap_download_20260901.md` for a
concrete before/after example — 8 sites' releases moved and one site's hub
reassigned in the space of one week). To get an identical site and contact
list, run against the **committed snapshot file**
`data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv` and verify
its sha256 **before** running anything. If you pull your own copy of this
file (e.g. from a different checkout), hash it first and compare against
the value below — do not proceed if it doesn't match.

### Pinned inputs (sha256)

| File | sha256 |
|---|---|
| `data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv` | `5591884a5a722e09cef948647a259926d5c7d85b9614c0dd499684a0b4b66d11` |
| `data/snapshots/site_year_data_presence.csv` | `89e1ecfb71677c1acd9b0185d070894192e1649c67832d6f7ca5bbb4bea7cf33` |

### Script commit and tag

- Git tag: **`authorship-781-20260901`** — `git checkout authorship-781-20260901`
  checks out the exact commit that contains the two script edits below and
  the output files this document describes.
- Resolve the exact commit hash with `git rev-parse authorship-781-20260901`
  (recorded in `SESSION_LOG.md`'s 2026-09-01 entry for this run, and
  printed by that command against the tag once you have the repository).
- R version used for this run: **R 4.6.0** (`R.version.string`).
- renv: `macos` profile, lockfile `renv/profiles/macos/renv.lock` at the
  state committed alongside the tag (not modified by this task).

### Exact run commands (from the repository root, in this order)

```sh
git checkout authorship-781-20260901
Rscript scripts/authorship_models.R --compute
Rscript scripts/authorship_invitations.R
Rscript scripts/authorship_diagnostics.R
```

`authorship_models.R` must run first — the other two consume its output
(`outputs/authorship/site_authors.csv`).

### Expected output hashes (sha256) — confirm before wiring up emails

| File | sha256 |
|---|---|
| `outputs/authorship/site_authors.csv` | `7fedadd94fa7ff6d35bcbf103fe9942abb423386b5f06ebc9138a4f632405514` |
| `outputs/authorship/authorship_invitations.csv` | `fa0ac4c4d0a309ff5f4c2c0f49f20c5fd179703ef8c031d25ceacfc305e1eac5` |

If either hash doesn't match after running the exact commands above
against the pinned inputs, stop and compare environments (R version, package
versions via the renv lockfile) before treating the output as
authoritative — do not average or reconcile two different runs by hand.

---

## 6. "The code" — exact file set (for a self-contained zip if you can't clone at the tag)

If you cannot check out the tag directly, these five files are the
complete, self-contained implementation — nothing else is read for the
slot computation or the contact merge:

- `scripts/authorship_models.R` — computes `years_of_data`, `latency_years`,
  and the rubric slot lookup; writes `site_authors.csv`.
- `scripts/authorship_invitations.R` — merges `site_authors.csv` with
  contact data (snapshot + BADM fallback/extra); writes
  `authorship_invitations.csv`.
- `scripts/authorship_diagnostics.R` — diagnostics only, not required to
  reproduce the two deliverable files, but reproduces the sensitivity and
  2025-availability checks referenced above.
- `R/utils.R` — specifically `compute_site_year_presence()` (the
  month/year presence logic each script's `PRESENCE_FILE` input was built
  from — not called directly by the three scripts above at run time, since
  `site_year_data_presence.csv` is precomputed, but needed to reproduce
  that file from raw monthly data if starting from scratch) and
  `write_output_metadata()` (companion `.meta.json` writer).
- `R/pipeline_config.R` — environment/config bootstrap each script sources
  at startup (`check_pipeline_config()`); no rubric logic lives here.

None of `RUBRIC_TABLE`, `row_bin()`, `latency_col()`, or `apply_rubric()`
(all in `authorship_models.R`) were changed for this run — only the
`SNAPSHOT_FILE` path constant in `authorship_models.R` and
`authorship_invitations.R` was re-pinned from an April-28 snapshot to the
current one. The rubric-vs-code verification (all 15 cells, both
boundaries, and the qualifying-year rule) was audited and passed in full
before this run — see `SESSION_LOG.md`, "Authorship rubric audit" entry,
earlier on 2026-09-01.
