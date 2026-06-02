# FLUXNET Annual Paper 2026 — Authorship Report

**Prepared:** 2026-05-12 | **Updated:** 2026-05-12 (presence file regenerated — see §Methodology update)
**Rubric locked:** 2026-05-07 (team meeting) | **Snapshot:** Apr-28 2026 (716 sites) | **Reference year:** 2026

The authorship rubric was locked at the team meeting on 2026-05-07. This report documents the locked
numbers, one open caveat, and one pending followup task that must be resolved before the author
list is finalised. The headline increased from 2,877 (initial May-12 run) to **2,905** following
presence file regeneration with a broader all-variable definition (see §Methodology update below).

---

## Locked rubric

Three latency tiers × five data-volume bins, fully monotone, author counts 2–8:

|  | >3 yr latency | ≤3 yr latency | ≤2 yr latency |
|---|---|---|---|
| <5 years | 2 | 3 | 4 |
| 6–10 years | 3 | 4 | 5 |
| 11–15 years | 4 | 5 | 6 |
| 16–20 years | 5 | 6 | 7 |
| ≥21 years | 6 | 7 | 8 |

Latency = 2026 − max\_data\_year per site. Data-volume bins count distinct years with any
non-NA flux variable (NEE VUT/CUT, GPP/RECO NT/DT × VUT/CUT, LE\_F\_MDS, H\_F\_MDS) present
in the presence file. See §Methodology update.

---

## Methodology update — Presence file regenerated (2026-05-12)

The presence file (`data/snapshots/site_year_data_presence.csv`) was regenerated on 2026-05-12
with a broader, all-variable definition, replacing the earlier NEE\_VUT\_REF-only indicator.

**Old definition:** a site-year was "present" if `NEE_VUT_REF` was non-NA in at least one month.
This excluded years where only energy or alternative-USTAR-method flux data were available.

**New definition:** a site-year is "present" if any of the following 12 columns is non-NA in at
least one month: `NEE_VUT_REF`, `NEE_CUT_REF`, `GPP_NT_VUT_REF`, `GPP_DT_VUT_REF`,
`GPP_NT_CUT_REF`, `GPP_DT_CUT_REF`, `RECO_NT_VUT_REF`, `RECO_DT_VUT_REF`,
`RECO_NT_CUT_REF`, `RECO_DT_CUT_REF`, `LE_F_MDS`, `H_F_MDS`.

**Coverage change:** The old file covered 672 sites (with 88 of the 716 snapshot sites on a
snapshot-span fallback). The new file covers all 716 sites directly from the presence data —
the fallback is no longer needed and has been removed from the authorship script.

**44 sites recovered.** These sites were invisible to the NEE\_VUT\_REF-only indicator because
NEE was either CUT-only (VUT statistically infeasible) or NEE processing failed downstream, but
the sites have valid energy or GPP/RECO data. This is a methodologically important shift: these
are not fringe sites. They include some of the longest-running towers in the AmeriFlux network:

| Site | Network | years\_of\_data (new) | Note |
|---|---|---|---|
| US-NR1 | AMF | 28 | Niwot Ridge — continuous since 1998, NEE CUT-only |
| US-Ho2 | AMF | 27 | Howland Forest West — VUT infeasible, NEE CUT-only |
| US-WCr | AMF | 24 | Willow Creek — 24 yrs GPP/RECO/LE data; NEE processing failure |

The correct invited-author count for these sites under the locked rubric is 8 (≥21 yr data,
≤2 yr latency) — the maximum on the rubric. They were previously allocated based on a
snapshot-span estimate that happened to give the same result in most cases, but the presence
file now provides an auditable, data-derived basis for the assignment.

**The US-WCr investigation (previously on the open issues list) is resolved.** US-WCr had a
27-year snapshot span but zero annual NEE\_VUT\_REF records. Investigation confirmed 24 years
of verified presence under the all-variable definition; the NEE absence is a downstream
processing limitation, not missing data.

**Net effect on headline:** +28 authors (2,877 → 2,905). The majority of this shift reflects
sites whose years\_of\_data changed when measured against the broader definition rather than a
snapshot span.

---

## Headline number

**2,905 invited authors** across 716 sites from the Apr-28 2026 FLUXNET Shuttle snapshot.
All 716 sites are presence-file verified. No snapshot fallback used.

---

## Per-network breakdown

Sorted by site count. All sites are presence-file verified (no fallback).

| Network | Sites | Invited authors |
|---|---|---|
| AMF | 340 | 1,361 |
| EUF | 118 | 465 |
| ICOS | 79 | 432 |
| JPF | 52 | 179 |
| TERN | 52 | 221 |
| CNF | 31 | 111 |
| KOF | 21 | 53 |
| FLX | 18 | 63 |
| SAEON | 5 | 20 |
| **TOTAL** | **716** | **2,905** |

---

## Resolved caveat

### ~~Caveat 1 — 5-year rubric row~~ — CLOSED 2026-05-20

**Decision (2026-05-20):** Sites with exactly 5 years of data are placed in the **<5 row (lower
author count)**. This was the implementation at the time of the 2026-05-07 team meeting and remains
policy. The rubric label is understood as "≤5 years". No data change or recomputation is required.

The 50 affected sites and their per-network counts are shown below for reference; the +50-author
alternative (moving 5-year sites to the "6–10" row) was considered and rejected.

| Network | 5-yr sites | Alt swing (not taken) |
|---|---|---|
| AMF | 22 | +22 |
| ICOS | 8 | +8 |
| EUF | 6 | +6 |
| JPF | 3 | +3 |
| CNF | 3 | +3 |
| KOF | 2 | +2 |
| FLX | 2 | +2 |
| SAEON | 2 | +2 |
| TERN | 2 | +2 |
| **TOTAL** | **50** | **+50** |

---

## Pending followup tasks

*All tasks resolved:*

*Resolved since initial report:*

- ~~**FOLLOWUP-A**: Resolve the 5-year rubric row label.~~ **Closed 2026-05-20** — 5-year sites
  remain in the <5 row (lower author count). Current implementation is correct; no data change
  required. Headline stays at 2,905.

- ~~**FOLLOWUP-C**: Regenerate `site_year_data_presence.csv` for all 716 Shuttle sites.~~ **Done
  2026-05-12** — all 716 sites now presence-file verified under the all-variable definition.

- ~~**FOLLOWUP-E1**: Rebuild the presence indicator to cover all variables, not just NEE.~~ **Done
  2026-05-12** — 12-column union (NEE/GPP/RECO VUT+CUT, NT+DT; LE\_F\_MDS; H\_F\_MDS).

- ~~**FOLLOWUP-E2**: Add month counts to the presence file to enable full-year vs. partial-year
  classification.~~ **Done 2026-05-12** — `n_months_present` column added; `years_full_year` and
  `years_partial_year` now computed per site.
