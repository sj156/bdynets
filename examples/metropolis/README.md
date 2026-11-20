# City-traffic-M source method

The source is City Traffic Benchmarks v4, Yekaterinburg, 2024-10-18. The supplied 720-road register and fifteen-minute source method are retained. The historical selection used a 34 × 22 midpoint grid, the highest-volume road in each of 497 occupied cells and 223 remaining maximum-volume choices. The source date was the highest-total-volume complete day for this register. The conversion reads the supplied register and does not select new roads or dates.

`read_parquet.py` reads the native columns and full-history speed calibration. It uses the source map's pandas quarter means and interpolation, then NumPy summaries before per-road rounding. Those supplied summaries are retained alongside unrounded native observations. `metropolis_input()` constructs standard flow, speed/index measurements, trends and native masks from these source quantities. The complete day follows the source fill rule; a short excerpt without day-boundary context does not invent unsupported boundary observations.

```sh
Rscript prepare.R --raw original/city-traffic-benchmarks rebuilt
```

Python 3, NumPy, pandas and PyArrow are required for both the Parquet teaching import and the complete original-file reconstruction. Normal rendering reads the included Parquet projections, so it does not require the large original archives. The raw reader checks both complete source clocks and the exact selected day. The lesson uses the existing two teaching IDs and declared six-slot excerpt with its original full-reference calibration; complete reconstruction keeps all 720 supplied IDs. The fifteen-minute preparation interval is fixed in this example; each frame combines three five-minute source slots. The physical volume unit and timezone remain unspecified. The derived index retains the source speed P90 and frame-volume P95 formula, without introducing a new threshold.

`data/source.rds` preserves native five-minute values, unrounded calibration and source regional summaries. `data/input.rds` retains the original fifteen-minute road values and all original trend values exactly.

## Original-format source lesson

The report begins with `data/raw` source files and calls the same original-file reader used by the complete reconstruction command above. `data/raw/source-manifest.csv` identifies the original files, projected columns or selected rows, reference inputs and checksums. The report displays the imported native fields before standardization, writes the intermediate data to a new temporary destination, reads it back, and compares prepared source-record values with the supplied day. The complete source and prepared files remain unchanged.

## Fields and conversion in the report

The [report](metropolis-traffic-report-demo.html) presents the source fields, an editable configuration, real original-format rows, the intermediate representation, road tables and their `traffic_data()` arguments. It includes an invalid-input example and correction, prepared road rows, source coverage and validation results, and prepared-data export with readback.

The renamed-column demonstration identifies source roles explicitly. The source adapters remain in `prepare.R`; the package consumes the resulting road tables. The original-format teaching excerpts explain individual calculations and are checked against the same one-day `data/input.rds` used for the maps and daily curves. The report also shows the prepared road-time rows arranged as a time-by-road matrix, preserving numeric values, zeros and missingness. Rendering keeps source data unchanged and writes reconstruction and export demonstrations to temporary directories.

## Reports and prepared output

`run.R` renders `metropolis-traffic-report-demo.html` from `metropolis-traffic-report-demo.Rmd`. VS Code **Run Code** on `run.R` invokes the same entry point as `Rscript run.R`. Each case installs the bundled package into a temporary library for that R session. Dependencies are R, the package imports, `rmarkdown`, `knitr` and Pandoc.

`Rscript run.R --offline` performs the source conversion checks and data plots without background streets. The default report uses verified OpenStreetMap files supplied in `data/basemap-cache`. For a new geographic range, obtain a matching cache before rendering the report with a basemap. The rendered report embeds its figures.

`Rscript run.R` uses the 08:00 frame by default. `Rscript run.R 17:00` prepares a different existing frame for the map. The all-day trend uses the same prepared day in either case. `Rscript run.R 08:00 --export` also creates the source-method PNG/PDF figures and selected tables in fresh directories under `output`.

From the bundle root, run `Rscript examples/metropolis/run.R --offline` to render without a street background.

`data/map-frame-0800.rds` contains the default single-frame drawing input. From a fresh R session in this example directory, run:

```r
source("../../bdynets/inst/preparation-session.R")
preparation_session(getwd())
library(bdynets)
traffic_flow_map("data/map-frame-0800.rds", basemap = NULL)
```


The R snippet uses the example directory as its working directory. A report run with `--export` also writes the chosen frame to `output/figures/map-input.rds`, ready for the same call. The export includes `selected-frame.rds` as a verification snapshot. Use the colour and basemap options in the report to match its styling.

`Rscript prepare.R prepared` exports the road-input RDS, readable tables, ordered geometry, dictionary, validation, metadata, actual source identities and checksums into a new directory. Existing destinations, source files and protected aliases are rejected. Source data and previous prepared objects are not replaced by the preparation function.

`data/input.rds` saves road-map arguments: roads, road-time values, declared timing and units, and applicable events, metrics and source records. The source clock is the supplied date and wall-clock encoding. Its timezone is unspecified. Frame labels are generated from the stored source times when rendering. The report reads this prepared day once and passes a separately prepared single-frame view to `traffic_flow_map()`. `data/data-dictionary.csv` describes all its fields; `validation.csv` records counts and checks. `source-files.csv` identifies its actual immediate preparation inputs; `original-sources.csv` identifies original archives separately. `metadata.json` records units, timing and source metadata. The reports show the raw fields, their conversion, actual prepared rows, concise component tables and verified CSV/RDS readback.

`Rscript tests/validation.R .` checks the original case method and prepared values, relevant invalid inputs and source protection in temporary directories. The [example input guide](../INPUT-FORMAT.md) describes the prepared inputs and their plotting calls; [source records](../DATA-SOURCES.md) state archive access and known sharing conditions.
