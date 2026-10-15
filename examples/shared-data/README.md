# Shared data: collaborator guide

This is an additive interface. Existing `gmde_fit()`, `BDCN::bdcn_fit()` and
`lpr_fit()` calls continue to work. BDCN remains a separate installation.
After obtaining this revision, reinstall the package from the repository root:

```sh
R CMD INSTALL bdynets
Rscript examples/shared-data/run.R /tmp/my-new-shared-data-example
Rscript -e 'rmarkdown::render("examples/shared-data/network-data-schemas.Rmd")'
```

The R Markdown report needs rmarkdown and knitr. Both entrypoints call package
functions; neither fits models. The checked-in HTML is a synthetic interface
demonstration. Large real data and model draws belong in an external store.
Record the repository revision and session information with each analysis.
There is no need to rewrite an existing paper pipeline or convert saved fits.

## Objects and model adapters

`network_data()` records physical nodes and edges. `network_panel()` stores a
time-by-unit matrix, ordered times and IDs, exposure, measurement semantics and
provenance. NA means missing; zero is observed. `panel_long()` retains both.
Use numeric or POSIXct times with an explicit source timezone. The first version
uses timestamps rather than a full interval table; record bin widths and
boundary conventions in aggregation/provenance metadata.

```r
# Existing call remains valid:
fit <- gmde_fit(Y, graph, K, expert, mcmc = control)
# Optional shared-data route: adapter explicitly transposes time-by-unit values.
args <- as_gmde_inputs(panel, graph)
fit <- do.call(gmde_fit, c(args, list(K=K, expert=expert, mcmc=control)))

# Existing BDCN graph still supplies its model-specific A and W.
args <- as_bdcn_inputs(edge_panel, bdcn_graph, graph_unit_ids=edge_ids)
fit <- do.call(BDCN::bdcn_fit, c(args, list(control=bdcn_control)))

# Current LPR is static: select a time slice explicitly.
data <- as_lpr_data(edge_panel, time_index=1)
fit <- lpr_fit(data)
```

Graph IDs must agree exactly in order. BDCN and graphMoDE adapters reject
missing values and non-unit exposures unsupported by those APIs. BDCN validates
counts. LPR omits unobserved edges, retains isolated nodes, and carries exposure.
It retains the current symmetric endpoint interaction model even if the physical
network is directed; use it only when that likelihood matches the application.
No adapter silently symmetrizes graphs, imputes data or rescales counts.

## Real Beijing data

Read the existing processed CSVs outside the package; pass their tables directly:

```r
panel <- beijing_panel(nodes, links, observations,
  time_bins=complete_bins, time=actual_times, value_column="x_ijt",
  measure="entry count", unit="vehicles",
  aggregation="entries per one-minute [start, end) bin")
```

Nodes need node_id/x_84/y_84; links need edge_index/from_node_id/to_node_id;
observations need edge_index/time_bin/x_ijt. Supply the complete bin sequence.
Absent rows default to missing. Set absent="zero" only when the export is known
to omit observed zeros. Explicit NA entries remain missing in either mode.
Use the actual occupancy or entry-count definition of the source export.

## Real LargeST data

```r
panel <- largest_panel(metadata, values, sensor_ids=hdf5_axis0, time=actual_times)
```

Metadata needs ID/Lat/Lng; values must be time by sensor. Supply sensor IDs from
the HDF5 column axis, not inferred positions. Metadata subsets and ordering are
resolved by ID. The importer does not read HDF5 itself or invent connections
between sensors. Source-specific Python preparation can remain in place.

## Shared visualization interface

```r
payload <- network_view_data(panel)
plot(panel, time_index=2)
saveRDS(payload, "view.rds")
```

The version-1 payload includes topology, coordinates/CRS, observation unit IDs,
time-by-unit values, missingness, fixed limits, units and provenance. Both source
adapters feed the same renderer. This initial implementation provides static
frames and an R-list payload, not an interactive HTML widget or JSON serializer.
Edges use straight endpoint geometry; matched road polylines, display-only links,
map projections and playback are future extensions. Do not interpret these
schematic plots as authoritative road geometry.
