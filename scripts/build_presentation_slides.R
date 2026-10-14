## build_presentation_slides.R
##
## Driver for the talk-deck presentation slides (v2, 2026-10-13): sets
## FLUXNET_PRESENTATION_MODE=1 and sources the new network-growth script plus
## each of the six modified figure scripts in turn, each in its own fresh
## environment so their local variables can't collide. Every sourced figure
## script already runs its own manuscript render (unconditional, as always)
## followed by an additive presentation-mode block (gated on the env var)
## that writes that figure's slide PNGs into
## review/figures/presentation_figures/slides/. The manuscript PNG outputs
## are therefore identical to a plain run of these scripts; only the
## PDFs/.meta.json pick up a fresh run timestamp (expected, harmless -- see
## SESSION_LOG.md's presentation-versions entries).
##
## No deck is assembled here -- v2 drops the pptx builder (one PNG per slide,
## at the paper figure's own aspect ratio; text is added beside each image in
## PowerPoint by hand, per the task prompt). See manifest.csv for the slide
## list, captions and smallest on-slide text size.
##
## Figure 5 is excluded (waits for the round-robin, per the task prompt).
## Run from the repository root: Rscript scripts/build_presentation_slides.R

Sys.setenv(FLUXNET_PRESENTATION_MODE = "1")

scripts <- c(
  "scripts/generate_fig_network_growth_slides.R",
  "scripts/generate_fig_map_network.R",
  "scripts/generate_fig_cumulative_siteyears.R",
  "scripts/generate_whittaker_alt_fig02_update.R",
  "scripts/figure_flux_comparison_combo.R",
  "scripts/generate_fig06_quality_uncertainty.R",
  "scripts/figure4_representativeness.R"
)

for (s in scripts) {
  message("=== Sourcing ", s, " (FLUXNET_PRESENTATION_MODE=1) ===")
  source(s, local = new.env(parent = globalenv()))
}

message("=== Done: slides written to review/figures/presentation_figures/slides/ ===")
message("See review/figures/presentation_figures/manifest.csv for the slide list/captions.")
