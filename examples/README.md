# Traffic map examples

Each case includes its source sample in `data/raw/` and any reference files in
its own `data/` directory. After [installing `bdynets`](../bdynets/README.md#installation),
run each example from its case directory. The HTML reports explain preparation
and include the selected map figures, statistics and trends. Run the visible R
blocks from the directory they specify, or edit the `.Rmd` report source.

| Case | Report | Source and commands | Static maps | Prepared period |
|---|---|---|---|---|
| Beijing Flow | [HTML](beijing/beijing-traffic-report.html) · [R Markdown](beijing/beijing-traffic-report.Rmd) | [README](beijing/README.md) · [Data](beijing/data/) · [Prepare](beijing/prepare.R) · [Render](beijing/run.R) | [Beijing](beijing/maps/beijing-flow.png) | 288 five-minute road entry-rate frames |
| PeMS Flow | [HTML](pems-flow/pems-flow-report.html) · [R Markdown](pems-flow/pems-flow-report.Rmd) | [README](pems-flow/README.md) · [Data](pems-flow/data/README.md) · [Prepare](pems-flow/prepare.R) · [Render](pems-flow/run.R) | [Los Angeles](pems-flow/maps/pems-flow-los-angeles.png) · [Bay Area](pems-flow/maps/pems-flow-bay-area.png) | 24 hourly flow frames with CHP incident starts |
| PeMS Speed | [HTML](pems-speed/pems-speed-report.html) · [R Markdown](pems-speed/pems-speed-report.Rmd) | [README](pems-speed/README.md) · [Data](pems-speed/data/README.md) · [Prepare](pems-speed/prepare.R) · [Render](pems-speed/run.R) | [Los Angeles](pems-speed/maps/pems-speed-los-angeles.png) · [Bay Area](pems-speed/maps/pems-speed-bay-area.png) | 24 hourly speed frames in mph with CHP incident starts |
| City-traffic-M Flow | [HTML](metropolis/metropolis-traffic-report.html) · [R Markdown](metropolis/metropolis-traffic-report.Rmd) | [README](metropolis/README.md) · [Data](metropolis/data/) · [Prepare](metropolis/prepare.R) · [Render](metropolis/run.R) | [Yekaterinburg](metropolis/maps/metropolis-flow.png) | 96 quarter-hour road volume frames |
| DivvyBike OD | [HTML](DivvyBike/divvy-od-report.html) · [R Markdown](DivvyBike/divvy-od-report.Rmd) | [README](DivvyBike/README.md) · [Data](DivvyBike/data/) · [Prepare](DivvyBike/prepare.R) · [Render](DivvyBike/run.R) | [Chicago, 00:00–03:00](DivvyBike/maps/divvy-od.png) | 8 three-hour windows of directed departures between station groups |

The `plot_time` parameter in each `.Rmd` sets the default map time. Pass an
exact frame start as the clock argument to draw another time. Each report
draws one map per displayed area with padding suited to the study extent.
With `Rscript run.R --export`, the report saves the complete series in
`output/prepared-data/` and copies its map PNGs to `output/maps/`. The
package’s drawing functions can use the exported series directly.

## Source files and caches

- Beijing reads the supplied [one-day gzip](beijing/data/raw/output_LinkTDMOE.csv.gz) and [road geometry HTML](beijing/data/raw/saturation_dynamic_leaflet_osm_osmgeom.html). The [supplied-file manifest](beijing/data/raw/source-manifest.csv) describes those inputs. The [street cache](beijing/data/basemap-cache/) supplies optional map context.
- PeMS Flow and PeMS Speed each include the station records, incident records, road references and offline caches used by that case. The [Flow file guide](pems-flow/data/README.md) and [Speed file guide](pems-speed/data/README.md) list the files and their purpose.
- City-traffic-M Flow uses the [static](metropolis/data/raw/city-traffic-M-raw-static-features.parquet) and [volume](metropolis/data/raw/city-traffic-M-raw-volume.parquet) Parquets and the fixed [road IDs](metropolis/data/selected-roads.csv), the [supplied-file manifest](metropolis/data/raw/source-manifest.csv) and the [street cache](metropolis/data/basemap-cache/). The [Parquet reader](metropolis/read_parquet.py) feeds the Flow preparation.
- DivvyBike includes the [supplied one-day trip CSV](DivvyBike/data/raw/202601-divvy-tripdata.csv), [source manifest](DivvyBike/data/raw/source-manifest.csv) and Chicago [street cache](DivvyBike/data/basemap-cache/).

The examples generate a `*-frames.rds` file ready to pass to the package’s map function. For your own data, choose the source conversion and supply the corresponding prepared object. See the [prepared drawing formats](../bdynets/inst/INPUT-FORMAT.md) for the three drawing inputs.

The repository also keeps its earlier figures and scripts in
[traffic-reference](traffic-reference/). Reproducing those archived scripts
requires the package version used to create them.
