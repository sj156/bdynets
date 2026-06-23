# Bdynets - Urban Traffic Datasets

This directory (`/data`) serves as the core data repository for the project, dedicated to storing, preprocessing, and visualizing traffic flow observations across different urban road networks.

It currently consists of three primary submodules representing traffic networks in Beijing, Paris, and California (PeMS). Each submodule integrates raw/processed data, data processing workflows (R Markdown), and geospatial visualizations.

## Repository Structure

```text
data/
├── Beijing Data/              # Beijing Traffic Dataset
│   ├── code/                  # Data processing and analysis scripts
│   ├── map/                   # Spatial network visualizations
│   └── data_process.Rmd       # R Markdown report for Beijing data preprocessing & EDA
│
├── Paris_road_data/           # Paris Road Traffic Conditions Dataset (2023)
│   ├── Paris-Road-Traffic-Conditions-2023.pdf  # Official dataset documentation and reports
│   └── paris_detectors_osm_map.html            # Interactive OpenStreetMap-based detector visualization
│
└── PeMS-Datasets/             # Caltrans Performance Measurement System (California)
    ├── code/                  # Cleaning and feature engineering scripts for PeMS
    ├── map/                   # Detector station distribution maps
    └── PeMS_2020_OSM_processing_report.Rmd     # R Markdown report for PeMS data map-matching with OSM
