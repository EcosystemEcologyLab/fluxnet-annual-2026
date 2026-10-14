Generate the site reference list for the paper, following the bbbd02c audit. Decisions:

1. Apply the parse_saeon() no-author fix (mirror parse_icos()); expect 781 of 781 sites.
2. Use data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv, passed explicitly (no auto-discovery). Do not regenerate any figures.
3. Correct the network references in scripts/generate_fluxnet_citations.R against the FLUXNET data policy (https://data.fluxnet.org/?p=30):
   - JPF: Ueyama et al. 2025 only. Correct title: "The JapanFlux2024 dataset for eddy covariance observations covering Japan and East Asia from 1990 to 2023", Earth Syst. Sci. Data 17, 3807–3833, doi:10.5194/essd-17-3807-2025.
   - CNF: Yu et al. 2006 (doi:10.1016/j.agrformet.2006.02.011), triggered by CNF, not JPF.
   - TERN/OzFlux: Isaac et al. 2017 is Biogeosciences 14, 2903–2928, doi:10.5194/bg-14-2903-2017. Check the title against the DOI.
   - Acknowledgments: use the policy's global acknowledgment text (regional networks and the modified ERA5 credit), the AmeriFlux DOE funding sentence, the ChinaFLUX NESDC sentence, the OzFlux and TERN portal sentences, and the Data availability sentence. Remove "Per-site DOIs are listed in the supplementary material"; the policy requires site citations in the reference section.
   Remove the review flag about Yu 2006 once fixed.
4. Write outputs to review/citations/ (git-tracked): the .bib, the APA list (.txt and .docx), acknowledgments .md, review flags .md, and a .meta.json via write_output_metadata(). Never write to bib_to_apa()'s default Google Drive path.

Also: fix CLAUDE.md "data_citation" to "product_citation", and update the citation section of CONTRIBUTOR_QUICKSTART.md (site count, placeholder script name).

Report: sites parsed; reference counts by network; remaining review flags; a few example APA entries, including one each from JPF, CNF and SAEON. Note in the log that fluxnet-quickstart and fluxnet-citations carry the same bugs; do not edit those repos.

Save this prompt as logs/prompts/2026-10-13_site_reference_list.md. Log, commit and push per CLAUDE.md.
