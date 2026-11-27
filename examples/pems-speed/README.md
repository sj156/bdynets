# PeMS freeway speed and CHP reported starts

Open the 08:00 [Los Angeles](maps/pems-speed-los-angeles.png) or [Bay Area](maps/pems-speed-bay-area.png) map.

Install the accompanying `bdynets` R package once. Run the commands and report blocks from this example directory. All case scripts and source samples are included here.

[Open the report](pems-speed-report.html) for the original station fields, detector links and worked road-hour calculations for 16 June 2026. The report draws one map for Los Angeles and one for the San Francisco Bay Area at the chosen hour. The prepared drawing input keeps all 24 hours for both regions.

## Files in this example

| File | Purpose |
|---|---|
| `README.md` | Commands and file inventory for the speed example |
| `prepare.R` | Rebuilds the linked native source, intermediate speed tables and aligned drawing input from the source data included here |
| `scripts/` | `read-source.R` reads the supplied sources. `prepare-input.R` aligns and writes the prepared input. The other helpers load the street layer and arrange report output |
| `data/` | Inputs and offline basemaps for this case |
| `run.R` | Renders the report and optionally exports the complete prepared data and the two selected maps |
| `pems-speed-report.Rmd` | Executable walkthrough from source records to the regional maps |
| `pems-speed-report.html` | Rendered report with two embedded map figures |

The [input inventory](data/README.md) describes the supplied Caltrans text files, project references, case register and offline street caches. The report itself shows how the input fields and reference IDs join. For each freeway link, the hourly speed is the mean of the available five-minute endpoint means in mph.

## Run from this example directory

```bash
Rscript run.R
Rscript run.R 09:00 --no-basemap
Rscript run.R --export --output-dir pems-speed-export
```

Edit `plot_time` in `pems-speed-report.Rmd` to change the default from 08:00.
A source-clock hour argument overrides it for both regions for that run.
Rendering uses the installed package, rmarkdown, knitr and Pandoc. The
bundled OpenStreetMap backgrounds are read locally by default. Use
`--no-basemap` to omit them. Each region uses its full prepared extent with a
narrow margin. The export copies the PNG drawn for each region.
Without `--output-dir`, `--export` writes to a new `output/` directory.

| Generated export | Contents |
|---|---|
| `prepared-data/source.rds` | Linked native speed observations, roads and CHP starts |
| `prepared-data/input.rds` | Intermediate speed tables with all road-hours and provenance |
| `prepared-data/speed-frames.rds` | Aligned drawing input for all 24 hours and both regions |
| `maps/pems-speed-los-angeles.png`, `maps/pems-speed-bay-area.png` | Separate regional maps for the selected hour |

## Prepare or draw without rendering the report

```bash
Rscript prepare.R pems-speed-prepared
```

This command generates the three RDS files plus CSV tables, a dictionary,
metadata and source identities. In an R session in this example directory,
choose the 08:00 frame and the Los Angeles region from the complete input.

```r
bdynets::traffic_speed_map(
  "pems-speed-prepared/speed-frames.rds", time_label = "08:00",
  region = "Los Angeles Area", basemap = "data/basemap-cache/los-angeles.geojson.gz",
  output_file = "pems-speed-los-angeles.png", draw = FALSE, full_extent = TRUE
)
```

The [prepared input guide](../../bdynets/inst/INPUT-FORMAT.md) describes the
same drawing format for your own data.

In an interactive R session, run the visible report blocks in order to draw on the current graphics device. Preparation uses a temporary directory. To run from a terminal and retain the report, maps and prepared inputs, use `Rscript run.R --export`.

## Prepare a background for another extent

The [prepared input guide](../../bdynets/inst/INPUT-FORMAT.md#prepare-an-openstreetmap-background) shows how this example’s `scripts/acquire-osm.py` prepares and reuses OpenStreetMap context from your drawing data.
