# Beijing simulated entry flow

The MOE source is simulation day 10. The original example averages five minute-sampled hourly all-lane entry rates per frame. The result is 288 frames in vehicles/hour. Road geometry joins by the directed source pair. All native minute observations remain in the snapshot; 545 pairs without matched geometry are excluded from the display.

`beijing_fields` maps the four source fields. `beijing_records()` validates pair-minute keys and the declared grid. `beijing_input()` applies the existing average-rate calculation. The report imports the original CSV and project geometry formats, saves/reads a native source snapshot, then applies the five-minute width and shows actual source and standard records.

The original-source command is:

```sh
Rscript prepare.R --raw original/output_LinkTDMOE.csv original/saturation_dynamic_leaflet_osm_osmgeom.html rebuilt
```

The field reader handles the original trailing CSV comma by selecting the named header positions. It reads values from the MOE file and geometry from the network FeatureCollection, without reading relative-load values from the map.

## Original-format source lesson

The report begins with `data/raw` source files and calls the same original-file reader used by the complete reconstruction command above. `data/raw/source-manifest.csv` identifies the original files, projected columns or selected rows, reference inputs and checksums. The report displays the imported native fields before standardization, writes the intermediate data to a new temporary destination, reads it back, and compares prepared source-record values with the supplied day. The complete source and prepared files remain unchanged.

## Fields and conversion in the report

The [report](beijing-traffic-report.html) presents the source fields, an editable configuration, real original-format rows, the intermediate representation, canonical tables and the `traffic_input()` constructor. It includes an invalid-input example and correction, standard output rows, source coverage and validation results, and prepared-data export with readback.

The renamed-column demonstration identifies source roles explicitly. The source adapters remain in `prepare.R`; the package consumes the resulting standard tables. Small conversion examples are compared with the supplied day-level `data/input.rds`, which supplies the map. Rendering keeps source data unchanged and writes reconstruction and export demonstrations to temporary directories.

## Reports and prepared output

`run.R` renders `beijing-traffic-report.html` from `beijing-traffic-report.Rmd`. VS Code **Run Code** on `run.R` invokes the same entry point as `Rscript run.R`. Each case installs the bundled package into a temporary library for that R session. Dependencies are R, the package imports, `rmarkdown`, `knitr` and Pandoc.

`Rscript run.R --offline` performs the source conversion checks and data plots without background streets. Street rendering first reuses matching verified files in the visible `data/basemap-cache`. New or missing requests obtain OpenStreetMap context from the input coordinates and cache the result. The rendered report embeds its figures.

`Rscript run.R 17:00` selects an existing frame for the map, without changing the data interval. `Rscript run.R 08:00 --export` also creates the source-method PNG/PDF figures and selected tables in fresh directories under `output`.

`Rscript prepare.R prepared` exports the standard RDS, readable tables, ordered geometry, dictionary, validation, metadata, actual source identities and checksums into a new directory. Existing destinations, source files and protected aliases are rejected. Source data and previous prepared objects are not replaced by the preparation function.

`data/input.rds` contains the standard plotting object. `data/data-dictionary.csv` describes all its fields; `validation.csv` records counts and checks. `source-files.csv` identifies its actual immediate preparation inputs; `original-sources.csv` identifies original archives separately. `metadata.json` records units, timing and source metadata. The reports show the raw fields, their conversion, actual standard rows, concise component tables and verified CSV/RDS readback.

`Rscript tests/validation.R .` checks the original case method and standard values, relevant invalid inputs and source protection in temporary directories. The [common format](../../bdynets/inst/INPUT-FORMAT.md) defines how the plotting interface reads the prepared tables; [source records](../DATA-SOURCES.md) state archive access and known sharing conditions.
