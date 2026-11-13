# Divvy individual station departures

The source is the January 2026 trip archive, limited to departures on 2026-01-01. The included CSV retains all 13 source fields and 1,950 rows. The daily extraction removes only trailing ASCII spaces from the two station-name fields: 13 departure-name values and 11 arrival-name values change. IDs, clocks, coordinates and all other source fields are retained exactly. The station method uses median valid paired coordinates and the most frequent nonblank recorded name. It retains 1,230 departures across 512 stations, including 85 within-station rides. The supplied standard input retains individual departures. Separate report sections derive hourly station OD and 1000 m spatial groups from the same retained day.

`divvy_fields` maps the source roles. `ride_input()` checks IDs, source clocks, duplicates and endpoint references, then builds nodes and individual OD records. The field dictionary identifies generated node indices separately from raw trip fields. The package OD renderer supplies the directed-curve method. The report configures seven integer-count classes for its grouped map.

```sh
Rscript prepare.R --raw original/202601-divvy-tripdata.csv rebuilt
```

The original monthly file is filtered to the same one-day departure window. `extract_divvy_day(source_file, output_file, day)` writes the original-column day CSV to a new file after `clean_divvy_station_names()` applies the stated name cleaning, then verifies exact readback. The report shows the original and cleaned names and checks every unchanged column against the selected source rows. The report executes it on the provided real archive excerpt, including one adjacent-date row. Missing origin, missing destination and missing both are separately counted. The three unused source attributes remain in the CSV. Departure timestamps keep their fractional seconds in the Chicago timezone. The delivered primary input remains at station resolution.

## Original-format source lesson

The report begins with `data/raw` source files and calls the same original-file reader used by the complete reconstruction command above. `data/raw/source-manifest.csv` identifies the original files, projected columns or selected rows, reference inputs and checksums. The report displays the imported native fields before standardization, writes the intermediate data to a new temporary destination, reads it back, and compares prepared source-record values with the supplied day. The complete source and prepared files remain unchanged.

## Fields and conversion in the report

The [report](divvy-od-report.html) presents the source fields, an editable configuration, real original-format rows, the intermediate representation, canonical tables and the `traffic_input()` constructor. It includes an invalid-input example and correction, standard output rows, source coverage and validation results, and prepared-data export with readback.

The renamed-column demonstration identifies source roles explicitly. The source adapters remain in `prepare.R`; the package consumes the resulting standard tables. Small conversion examples are compared with the supplied day-level `data/input.rds`, which supplies the map. Rendering keeps source data unchanged and writes reconstruction and export demonstrations to temporary directories.

## Hourly and spatial derivatives

The report's **Hourly station OD** section calls the existing `ride_input(..., bin_seconds=3600)`. The 1,230 retained departures become 1,157 local-hour OD records at the same 512 stations, with total trips and the 85 within-station trips unchanged. Counts use half-open hourly intervals in `America/Chicago`. Clock-hour pooling across dates is a separate operation; absent observations are not zero-filled.

The **Spatially grouped OD** section independently calls the existing `aggregate_od_data()` on the individual departures. Its 1000 m radius follows the preserved aggregation call. The resulting 105 groups retain 1,230 trips; within-group trips are 317. Membership records every source station and its representative. The distance bound refers to a station and its representative. Grouped records retain source departure instants; no hourly aggregation occurs in this branch. A different grouping radius produces different node definitions and within-group totals.

Both results use `bdynets-traffic-input-v1`. **Derived-data export and readback** writes separate standard RDS/CSV outputs, field dictionaries, validation summaries, metadata, source identities and checksums into temporary directories. Spatial membership is retained in RDS/JSON and a readable CSV. The report checks values, clocks, missingness, membership, total counts and source-file preservation. The primary `data/input.rds` remains at station resolution. The map groups the same departures within a 1500 m radius for display, independently of the 1000 m preparation lesson. It uses seven count classes (1 through 6 and 7 or more). The grouped view reports within-group trips separately and preserves total trips.

## Reports and prepared output

`run.R` renders `divvy-od-report.html` from `divvy-od-report.Rmd`. VS Code **Run Code** on `run.R` invokes the same entry point as `Rscript run.R`. Each case installs the bundled package into a temporary library for that R session. Dependencies are R, the package imports, `rmarkdown`, `knitr` and Pandoc.

`Rscript run.R --offline` performs the source conversion checks and data plots without background streets. Street rendering first reuses matching verified files in the visible `data/basemap-cache`. New or missing requests obtain OpenStreetMap context from the input coordinates and cache the result. The rendered report embeds its figures.

`Rscript prepare.R prepared` exports the standard RDS, readable tables, ordered geometry, dictionary, validation, metadata, actual source identities and checksums into a new directory. Existing destinations, source files and protected aliases are rejected. Source data and previous prepared objects are not replaced by the preparation function.

`data/input.rds` contains the standard plotting object. `data/data-dictionary.csv` describes all its fields; `validation.csv` records counts and checks. `source-files.csv` identifies its actual immediate preparation inputs; `original-sources.csv` identifies original archives separately. `metadata.json` records units, timing and source metadata. The reports show the raw fields, their conversion, actual standard rows, concise component tables and verified CSV/RDS readback.

`Rscript tests/validation.R .` checks the original case method and standard values, relevant invalid inputs and source protection in temporary directories. The [common format](../../bdynets/inst/INPUT-FORMAT.md) defines how the plotting interface reads the prepared tables; [source records](../DATA-SOURCES.md) state archive access and known sharing conditions.

The optional complete-archive check is `Rscript tests/validation.R . original/202601-divvy-tripdata.csv`. It compares the monthly archive's selected rows with the teaching excerpt, extracts a new temporary day CSV, verifies the declared name changes and every other field, and checks the station/OD tables against `data/input.rds`. The monthly archive and supplied day files remain unchanged.
