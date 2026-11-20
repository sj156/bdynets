# Divvy individual station departures

The source is the January 2026 trip archive, limited to departures on 2026-01-01. The included CSV retains all 13 source fields and 1,950 rows. The daily extraction removes only trailing ASCII spaces from the two station-name fields: 13 departure-name values and 11 arrival-name values change. IDs, clocks, coordinates and all other source fields are retained exactly. The station method uses median valid paired coordinates and the most frequent nonblank recorded name. It retains 1,230 departures across 512 stations, including 85 within-station rides. Data preparation publishes station-resolution departures, a separate 1500 m grouped day, and a final map-ready OD window.

`divvy_fields` maps the source roles. `ride_input()` checks IDs, source clocks, duplicates and endpoint references, then builds nodes and individual OD records. The field dictionary identifies generated node indices separately from raw trip fields. The package OD renderer supplies the directed-curve method. The report configures seven integer-count classes for its grouped map.

```sh
Rscript prepare.R --day 2026-01-01 --start 00:00 --end 03:00 --raw original/202601-divvy-tripdata.csv rebuilt
```

The original monthly file is filtered to the same one-day departure window. `extract_divvy_day(source_file, output_file, day)` writes the original-column day CSV to a new file after `clean_divvy_station_names()` applies the stated name cleaning, then verifies exact readback. The report shows the original and cleaned names and checks every unchanged column against the selected source rows. The report executes it on the provided real archive excerpt, including one adjacent-date row. Missing origin, missing destination and missing both are separately counted. The three unused source attributes remain in the CSV. Departure timestamps keep their fractional seconds in the Chicago timezone. The delivered primary input remains at station resolution.

## Original-format source lesson

The report begins with `data/raw` source files and calls the same original-file reader used by the complete reconstruction command above. `data/raw/source-manifest.csv` identifies the original files, projected columns or selected rows, reference inputs and checksums. The report displays the imported native fields before constructing the OD input, writes the intermediate data to a new temporary destination, reads it back, and compares prepared source-record values with the supplied day. The complete source and prepared files remain unchanged.

## Fields and conversion in the report

The [report](divvy-od-report.html) presents the source fields, an editable configuration, real original-format rows, the intermediate representation, station and trip tables and a plain OD input list. It includes an invalid-input example and correction, prepared OD rows, source coverage and validation results, and prepared-data export with readback.

The renamed-column demonstration identifies source roles explicitly. The source adapters in `prepare.R` produce the station (`nodes`) and trip (`flows`) tables used in preparation. The original-format archive excerpt contains the complete selected day and one adjacent-date record used to check filtering. The converted station and trip tables are compared with the one-day station-level `data/input.rds`; the grouped day then supplies the preparation step that produces the final OD window. OD endpoints join the station register by ID; this representation does not require a time-by-road matrix. Rendering keeps source data unchanged and writes reconstruction and export demonstrations to temporary directories.

## Hourly and spatial derivatives

The report's **Hourly station OD** section calls the existing `ride_input(..., bin_seconds=3600)`. The 1,230 retained departures become 1,157 local-hour OD records at the same 512 stations, with total trips and the 85 within-station trips unchanged. Counts use half-open hourly intervals in `America/Chicago`. Clock-hour pooling across dates is a separate operation; absent observations are not zero-filled.

`prepare_spatial_example()` groups the retained departures within a 1500 m station-to-representative bound. Its 62 groups retain all 1,230 trips, including 435 within-group trips. Each group uses the observed coordinates of its representative. Grouped departures retain their source timestamps and record the membership of every station. The grouped-day reference is `data/spatial-1500m/input.rds`.

## Prepare the input to the map

`prepare.R` selects departures in the local half-open interval `[start, end)`, sums trips by directed origin and destination, attaches both endpoint coordinates, and separates within-group trips. The resulting `bdynets_od_pairs` contains the nodes and routes to draw, the grouped node register, the pair totals, and the actual start, end, timezone, unit, radius, and source provenance. The report formats the display window from those timestamps. For the delivered 00:00–03:00 window, 288 trips consist of 201 cross-group trips on 129 directed routes and 87 trips within groups.

In a fresh R session in this example directory, load the bundled package and draw the final RDS. Choose another window or set of routes during preparation.

```r
source("../../bdynets/inst/preparation-session.R")
preparation_session(getwd())
library(bdynets)
map_input <- readRDS("data/spatial-1500m/window-0000-0300.rds")
traffic_od_map(map_input, basemap = NULL)
```

The report builds this result from the bundled raw source excerpt and uses the read-back RDS for its map and PNG/PDF exports. Its exported `selected-window.rds` also contains the ready OD input, with the figure summary attached. The map uses seven count classes (1 through 6 and 7 or more). Curves represent direction between endpoints.

## Reports and prepared output

`run.R` renders `divvy-od-report.html` from `divvy-od-report.Rmd`. VS Code **Run Code** on `run.R` invokes the same entry point as `Rscript run.R`. Each case installs the bundled package into a temporary library for that R session. Dependencies are R, the package imports, `rmarkdown`, `knitr` and Pandoc.

`Rscript run.R` uses the supplied matching cache in `data/basemap-cache`. `Rscript run.R --offline` omits background streets. A new map extent needs a matching OpenStreetMap cache acquired separately before rendering with the backdrop. The rendered report embeds its figures.

`Rscript prepare.R prepared` reproduces the default. To choose another window on the bundled day, run `Rscript prepare.R --start 01:00 --end 02:00 prepared-hour`. For another day, use `--day` with `--raw` and a source file containing that day's departures. Both clocks use `HH:MM`; an end of `24:00` means midnight starting the next day. The output directory must be new.

The preparation command writes `station/input.rds`, `spatial-1500m/input.rds`, and `spatial-1500m/window-HHMM-HHMM.rds`. The day datasets include readable tables, dictionaries, validation, source identities and checksums. The prepared map RDS shares the spatial directory and carries its own time bounds and provenance. `data/input.rds` remains the supplied station-level reference.

`Rscript tests/validation.R .` checks the original method, prepared and read-back values, exact window boundaries, alternate dates, endpoint coordinates, direct plotting input and source protection. See the [Flow and OD drawing input formats](../INPUT-FORMAT.md) and [source records](../DATA-SOURCES.md).

The optional complete-archive check is `Rscript tests/validation.R . original/202601-divvy-tripdata.csv`. It compares the monthly archive's selected rows with the teaching excerpt, extracts a new temporary day CSV, verifies the declared name changes and every other field, and checks the station/OD tables against `data/input.rds`. The monthly archive and supplied day files remain unchanged.
