# Divvy Bike multi-scale OD network analysis

This example explores how spatial aggregation changes the structure and visual interpretation of the Chicago Divvy Bike directed origin–destination (OD) network. It compares the processed January 2026 hourly flow data at three resolutions: raw stations, 500 m aggregation, and 1,000 m aggregation.

Install the R packages `rmarkdown`, `knitr`, and `reticulate`, and the Python packages `pandas`, `numpy`, and `matplotlib`. From the repository root, render the report with:

```bash
cd examples/divvybike
Rscript -e 'rmarkdown::render("bike_report.Rmd")'
```

Open `bike_report.html` to view the completed report. The HTML file is self-contained. Rendering runs locally, reads the CSV files from `data/`, and does not download data or map tiles.

`bike_report.Rmd` contains the report narrative and executes Python through `reticulate`. `bike_report.py` provides data loading, validation, summaries, and plotting functions. The report validates node and OD identifiers, compares network compression and flow concentration, and displays the raw, 500 m, and 1,000 m OD maps as full-width vertical panels.

The `data/` directory contains paired node and sparse hourly OD tables. Files named `nodes_<scale>.csv` define node identifiers, coordinates, representative stations, and the number of raw stations represented by each node. Files named `od_flow_2026_01_dt01h_<scale>.csv` contain hourly `timestamp`, `origin_node_idx`, `destination_node_idx`, and `trip_count` values. An absent time-by-OD combination is interpreted as zero trips. Rows whose origin and destination are the same node are retained as within-node flows.

The main report uses the `raw`, `r500`, and `r1000` datasets. Additional `r2000` and `r2500` files are included for coarser-scale visualization. `bike_r2500_map.png` illustrates the result at the 2,500 m (2.5 km) aggregation scale and must remain beside `bike_report.Rmd` so the report can be rendered successfully.

Open `divvy_multi_scale.html` for the separate interactive multi-scale map. `visualization.py` is the corresponding Python generator. The interactive map uses endpoint links to show direction and volume; these links are visual connections and are not observed bicycle trajectories or map-matched routes.

The CSV files are processed analytical datasets rather than original trip-level Divvy records. Spatial aggregation preserves total trip counts while reducing the number of nodes and active OD pairs, increasing within-node flows, and concentrating trips into a smaller set of directed links.
