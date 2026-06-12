# Dynamic Visualization Handoff

This folder contains only the code and handoff notes needed to reproduce the current dynamic traffic visualization from the initial data. It does not include raw input data or generated output data.

## Folder Contents

```text
handoff_dynamic_visualization/
  README_HANDOFF.md
  requirements.txt
  code/
    process_real_traffic_data.py
    estimate_xijt_capacity.py
    match_osm_geometry.py
    route_osm_geometry.py
    visualize_real_network.py
```

## Initial Data Required

Put the following initial files in the same folder as the Python scripts before running the pipeline:

```text
input_node.csv
input_node_WGS84.csv
input_link.csv
output_agent.csv
```

`input_node_WGS84.csv` is required for map visualization. It provides the WGS84 longitude/latitude columns `x_84` and `y_84`. Without it, `processed_real_network/nodes_clean.csv` will not contain `x_84/y_84`, and `visualize_real_network.py` will fail when drawing the Leaflet/OpenStreetMap map.

The following initial files are not required for the current dynamic relative-load map:

```text
input_zone.csv
input_node_control_type.csv
output_LinkTDMOE.csv
```

`output_LinkTDMOE.csv` is only used by the optional MOE branch in `process_real_traffic_data.py`; the current script has `USE_MOE_FEATURES = False`.

## Important Run Location Note

The Python scripts use their own file location as `BASE_DIR`. For the simplest handoff run, use one of these two methods:

1. Copy the five files from `code/` into the folder that already contains the initial CSV files, then run the commands there.
2. Or copy the initial CSV files into `handoff_dynamic_visualization/code/`, then run the commands inside `code/`.

Do not run the scripts from a separate directory while leaving the initial data elsewhere, unless you also edit `BASE_DIR` in the scripts.

## Pipeline

Run these commands in order from the folder that contains both the Python scripts and the initial data:

```bash
python process_real_traffic_data.py
python estimate_xijt_capacity.py
python match_osm_geometry.py
python route_osm_geometry.py
python visualize_real_network.py --tile osm --metric saturation --geometry-source osm-matched --osm-geometry-file processed_real_network/osm_routed_edge_geometry.csv --unmatched-geometry hide --dynamic-html --skip-animation --out-dir processed_real_network/visualizations/runs/osm_xijt_relative_load_dynamic_only
```

## What Each Step Does

### 1. `process_real_traffic_data.py`

Reads:

```text
input_node.csv
input_node_WGS84.csv
input_link.csv
output_agent.csv
```

Creates the core processed network and dynamic traffic tables:

```text
processed_real_network/nodes_clean.csv
processed_real_network/links_clean.csv
processed_real_network/edge_flow_xijt.csv
processed_real_network/edge_flow_xijt_nonzero.csv
processed_real_network/node_flow_xit.csv
processed_real_network/edge_neighbors.csv
processed_real_network/node_neighbors.csv
processed_real_network/edge_flow_xijt_wide.csv
```

The visualization mainly needs:

```text
processed_real_network/nodes_clean.csv
processed_real_network/links_clean.csv
processed_real_network/edge_flow_xijt_nonzero.csv
```

### 2. `estimate_xijt_capacity.py`

Reads:

```text
processed_real_network/links_clean.csv
processed_real_network/edge_flow_xijt_nonzero.csv
```

Creates:

```text
processed_real_network/edge_xijt_capacity_estimates.csv
```

This file is used by the current relative-load metric:

```text
relative_load = x_ijt / capacity_xijt
```

`capacity_xijt` is calibrated to the same 5-minute interval-presence definition as `x_ijt`, so the map avoids dividing `x_ijt` by an incompatible hourly flow capacity.

### 3. `match_osm_geometry.py`

Reads:

```text
processed_real_network/nodes_clean.csv
processed_real_network/links_clean.csv
```

Downloads OSM road data through Overpass and creates:

```text
processed_real_network/osm_highway_ways.json
processed_real_network/osm_matched_edge_geometry.csv
```

The current final map uses the routed geometry from the next step, but this step is still needed because it creates `osm_highway_ways.json`.

This step needs internet access. If all Overpass endpoints fail, rerun it later or provide a previously downloaded `processed_real_network/osm_highway_ways.json`.

### 4. `route_osm_geometry.py`

Reads:

```text
processed_real_network/nodes_clean.csv
processed_real_network/links_clean.csv
processed_real_network/osm_highway_ways.json
```

Creates:

```text
processed_real_network/osm_routed_edge_geometry.csv
processed_real_network/osm_routed_edge_geometry_report.txt
```

This gives the map road-shaped OSM polylines instead of simple straight lines between model nodes.

### 5. `visualize_real_network.py`

Reads:

```text
processed_real_network/nodes_clean.csv
processed_real_network/links_clean.csv
processed_real_network/edge_flow_xijt_nonzero.csv
processed_real_network/edge_xijt_capacity_estimates.csv
processed_real_network/osm_routed_edge_geometry.csv
```

Creates the current dynamic-only visualization:

```text
processed_real_network/visualizations/runs/osm_xijt_relative_load_dynamic_only/saturation_dynamic_leaflet_osm_osmgeom.html
```

The page focuses on dynamic relative load. It does not create or require the old single-time-bin 107 static map.

## Common Problems

### `KeyError: "None of [Index(['x_84', 'y_84'], ...)] are in the [columns]"`

`input_node_WGS84.csv` was missing when `process_real_traffic_data.py` was run. Put `input_node_WGS84.csv` beside the scripts and raw data, then rerun `process_real_traffic_data.py`.

### `FileNotFoundError` for `osm_highway_ways.json`

Run `python match_osm_geometry.py` before `python route_osm_geometry.py`.

### The map opens but road geometry looks too straight

Make sure the final visualization command uses:

```bash
--geometry-source osm-matched --osm-geometry-file processed_real_network/osm_routed_edge_geometry.csv
```

## Initial Data Integrity Note

The initial data files were not copied into this handoff folder. In the working project folder, their current file modification times are still from July 2025, not from the recent visualization work:

```text
input_link.csv                  Jul  1 07:00:00 2025
input_node.csv                  Jul  1 07:00:00 2025
input_node_WGS84.csv            Jul  1 07:10:00 2025
input_node_control_type.csv     Jul  1 07:00:00 2025
input_zone.csv                  Jul  1 07:00:00 2025
output_LinkTDMOE.csv            Jul  1 07:01:00 2025
output_agent.csv                Jul  1 07:00:00 2025
```

This confirms that the current handoff work did not rewrite the initial CSV files. A stronger byte-level comparison would require original checksums from the data provider.
