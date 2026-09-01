# Shuttle release-tag and hub-assignment query — sent 2026-09-01

**To:** FLUXNET Shuttle team (support@fluxnet.org)
**From:** David J.P. Moore
**Subject:** Release-tag and hub-assignment discrepancies in the current Shuttle listing

---

Hi all,

We refreshed our local copy of the Shuttle this week and hit a few things in
the current listing worth flagging. A `flux_listall()` call on 2026-09-01
returned 781 sites; comparing that against what we already hold on disk
(from a 2026-06-24 listing), most of the differences are straightforward new
additions, but six sites look off in a way that seems like a listing issue
rather than something on our end.

Three sites, DE-Hai, FR-CLt, and FR-EM2, now show a lower product release tag
(`_r`) in the live listing than the release we downloaded earlier. My read is
that the live listing has rolled back to an older release for these three.
Could you confirm whether that is intentional, a withdrawal or reprocessing
of the newer release, or an indexing error, and which release we should
treat as authoritative for citation?

Two others, DE-Hte and JP-Api, show a newer release than our copy, which we
are reading as normal reprocessing; we will re-pull those unless you tell us
otherwise.

Separately, IT-SR2 has changed source network from FLX to ICOS between the
June listing and now. We assume ICOS is the correct current hub, but that
change breaks any tooling that joins product metadata on source network and
site_id together, which silently drops the site's metadata in our pipeline.
Is site_id meant to stay stable across a hub reassignment, and is there a
recommended way to track sites that move between networks?

Happy to send the exact release tags and file paths for any of these if it
helps you locate them.

Best wishes,
Dave
