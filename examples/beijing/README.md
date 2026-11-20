# Beijing simulated entry flow

The MOE source is simulation day 10. The original example averages five minute-sampled hourly all-lane entry rates per frame. The result is 288 frames in vehicles/hour. Road geometry joins by the directed source pair. All native minute observations remain in the snapshot; 545 pairs without matched geometry are excluded from the display.

`beijing_fields` maps the four source fields. `beijing_records()` validates pair-minute keys and the declared grid. `beijing_input()` applies the existing average-rate calculation. The report imports the original CSV and project geometry formats, saves/reads a native source snapshot, then applies the five-minute width and shows actual source and road records.

The original-source command is:

```sh
Rscript prepare.R --raw original/output_LinkTDMOE.csv original/saturation_dynamic_leaflet_osm_osmgeom.html rebuilt
```

The field reader handles the original trailing CSV comma by selecting the named header positions. It reads values from the MOE file and geometry from the network FeatureCollection, without reading relative-load values from the map.

## Original-format source lesson

The report begins with `data/raw` source files and calls the same original-file reader used by the complete reconstruction command above. `data/raw/source-manifest.csv` identifies the original files, projected columns or selected rows, reference inputs and checksums. The report displays the imported native fields before standardization, writes the intermediate data to a new temporary destination, reads it back, and compares prepared source-record values with the supplied day. The complete source and prepared files remain unchanged.

## Fields and conversion in the report

The [report](beijing-traffic-report.html) presents the source fields, an editable configuration, real original-format rows, the intermediate representation, road tables and their `traffic_data()` arguments. It includes an invalid-input example and correction, prepared road rows, source coverage and validation results, and prepared-data export with readback.

The renamed-column demonstration identifies source roles explicitly. The source adapters remain in `prepare.R`; the package consumes the resulting road tables. The original-format teaching excerpts explain individual calculations and are checked against the same one-day `data/input.rds` used for the maps and daily curves. The report also shows the prepared road-time rows arranged as a time-by-road matrix, preserving numeric values, zeros and missingness. Rendering keeps source data unchanged and writes reconstruction and export demonstrations to temporary directories.

## Reports and prepared output

`run.R` renders `beijing-traffic-report.html` from `beijing-traffic-report.Rmd`. VS Code **Run Code** on `run.R` invokes the same entry point as `Rscript run.R`. Each case installs the bundled package into a temporary library for that R session. Dependencies are R, the package imports, `rmarkdown`, `knitr` and Pandoc.

`Rscript run.R --offline` performs the source conversion checks and data plots without background streets. The default report uses verified OpenStreetMap files supplied in `data/basemap-cache`. For a new geographic range, obtain a matching cache before rendering the report with a basemap. The rendered report embeds its figures.

`Rscript run.R` uses the 08:00 frame by default. `Rscript run.R 17:00` prepares a different existing frame for the map. The all-day trend uses the same prepared day in either case. `Rscript run.R 08:00 --export` also creates the source-method PNG/PDF figures and selected tables in fresh directories under `output`.

From the bundle root, run `Rscript examples/beijing/run.R --offline` to render without a street background.

`data/map-frame-0800.rds` contains the default single-frame drawing input. From a fresh R session in this example directory, run:

```r
source("../../bdynets/inst/preparation-session.R")
preparation_session(getwd())
library(bdynets)
traffic_flow_map("data/map-frame-0800.rds", basemap = NULL)
```


The R snippet uses the example directory as its working directory. A report run with `--export` also writes the chosen frame to `output/figures/map-input.rds`, ready for the same call. The export includes `selected-frame.rds` as a verification snapshot. Use the colour and basemap options in the report to match its styling.

`Rscript prepare.R prepared` exports the road-input RDS, readable tables, ordered geometry, dictionary, validation, metadata, actual source identities and checksums into a new directory. Existing destinations, source files and protected aliases are rejected. Source data and previous prepared objects are not replaced by the preparation function.

`data/input.rds` saves road-map arguments: roads, road-time values, declared timing and units, and applicable events, metrics and source records. The source clock is elapsed simulation-day seconds. Frame labels are generated from the stored source times when rendering. The report reads this prepared day once and passes a separately prepared single-frame view to `traffic_flow_map()`. `data/data-dictionary.csv` describes all its fields; `validation.csv` records counts and checks. `source-files.csv` identifies its actual immediate preparation inputs; `original-sources.csv` identifies original archives separately. `metadata.json` records units, timing and source metadata. The reports show the raw fields, their conversion, actual prepared rows, concise component tables and verified CSV/RDS readback.

`Rscript tests/validation.R .` checks the original case method and prepared values, relevant invalid inputs and source protection in temporary directories. The [example input guide](../INPUT-FORMAT.md) describes the prepared inputs and their plotting calls; [source records](../DATA-SOURCES.md) state archive access and known sharing conditions.
