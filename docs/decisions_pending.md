> Technical bugs and data quality issues with external dependencies (fluxnet R package,
> FLUXNET Shuttle) are tracked in [docs/known_issues.md](known_issues.md), not here.

## QC threshold review needed

US-Ha1 (Harvard Forest) excluded entirely at QC_THRESHOLD_YY = 0.75 — all 35 FLUXMET YY records exceed gap-fill threshold. Threshold lowered to 0.50 temporarily. Co-authors should review and agree on final threshold before full 600-site run. Consider site-level overrides or flag-rather-than-exclude approach.
