# Workflow notes

- `2helpers.Rmd` explains the current numerical helper layer and exactness
  boundary.
- `4gibbs.Rmd` explains one MCMC sweep, the five internal method families and
  diagnostics.
- `5simulations.Rmd` records the latest K=10 debugging state and safe entry
  points in this handoff.

These files are documentation, not independent implementations. Some historical
examples mention the byte-identical original approved context, whose full hash
is intentionally checked by `countdlm_road_load_approved_context()`. That large
OSM-derived evidence file is not distributed here. For the minimized public
export, use `scripts/check_inputs.R` and the low-level reproduction script.

