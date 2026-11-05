# Traffic case reports

The bdynets package provides one input constructor and one plotting
method for road measurements, timestamped point events, or both. The executable
case reports follow the same sequence: source and scope, actual table fields,
preparation and checks, common package calls, static maps, statistics and results.
PeMS contains separate Los Angeles and San Francisco Bay Area sections.

## Contents

| Location | Contents |
|---|---|
| ../../bdynets/ | R package, help, shared rendering assets and tests |
| examples/beijing/ | Simulation day 10; 11,742 roads and 288 five-minute flow frames |
| examples/largest/ | 2026-06-16; 6,638 links in two regions, 24 hourly frames and 1,019 CHP starts |
| examples/metropolis/ | Yekaterinburg, 2024-10-18; 720 segments and 96 source-volume frames |
| examples/synthetic/ | Generated common-format road/event fixture with missing cells and unspecified units |
| run.R | Shared local-package installation and case-rendering entry point |

Each case contains an executable Rmd, its static HTML, prepare.R, run.R, README.md,
and two data files. data/source.rds holds selected source-level records with
supplied geometry and provenance. prepare.R executes the source-to-common
transformation and writes data/input.rds. Beijing retains minute-level hourly
rates; PeMS retains active-position hours, D04 five-minute station counts and
CHP district-day summaries; metropolis retains source segments and volume frames.
The preserved source cache makes the daily examples independent of full archives
and external files. The original selected road sets, measurements and units are
retained. Full basemap source URLs, build/retrieval dates, extraction bounds,
zoom, tile records and geometry identities accompany the observations.

## Input contract

Roads have unique road_id values and geometry matrices containing longitude and
latitude in travel order. Optional region and reciprocal opposite_id columns
provide explicit grouping and opposing relationships. Other scalar attributes
are descriptive. Flow uses unique road_id, time keys and nonnegative flow values.
Zero is observed; NA and omitted pairs are missing. Optional events require time,
lon and lat; id, category, description, region and time_label are optional.

Frame starts, observations and events share numeric source seconds or consistent
POSIXct clocks. Interval widths define half-open event bins. A unit is supplied
as a label; omitted units remain unspecified. The constructor infers no physical
unit, timezone, event duration or event-to-road assignment. Optional offline
GeoJSON context carries its own attribution.

All cases use the same seven-color green-to-red palette and automatic log1p
thresholds. The scale spans the input maximum across all supplied roads and
frames and remains fixed for regional selections. Common geometry and density
rules determine opposing separation and stroke widths. No dataset name chooses
a drawing branch.

## Runtime and outputs

R 4.1 or later and jsonlite support the package. Executable Rmd reports also use
knitr, rmarkdown and Pandoc. The root run.R installs the repository bdynets package into a
temporary R library and renders the discovered cases. Its example setting is
NULL for all cases or one folder name, such as "beijing", for a single case.
VS Code Code Runner runs the complete entry file; the working directory does not
select the input. Its command-line equivalent at this directory is Rscript run.R.

Individual case run.R files render one report with an installed bdynets package. An optional HH:MM argument selects another supplied frame.
Their default is 08:00. Temporary figures and intermediates are removed. Normal rendering regenerates
the prepared input and named HTML report. Each report contains static
figures, code, tables, source metadata and session information. It opens offline.

The following package calls use one case folder as the R working directory:

```r
library(bdynets)
x <- read_traffic("data/input.rds")
plot(x, index=1)
traffic_summary(x, index=1)
traffic_trend(x)
traffic_report(x, "summary.html", index=1)
traffic_export(x, "new-output", index=1, input_files=c(input="data/input.rds"))
```

traffic_report writes a generic static summary, with whole-period coverage and
explicit selected-interval distributions. The case Rmd files provide their own
source explanations and data-specific result discussion. traffic_map remains an
optional standalone interactive map writer; it is not called by the reports.
PNG/PDF export examples use ordinary R graphics devices and the same plot method.

## Shared exports

traffic_export accepts the same object, frame and optional region for every
input. PNG/PDF maps and each applicable flow/event trend accompany selected
records, summary/trend CSV tables, provenance, source/input MD5 identities,
output MD5 checksums and session information. The selection record freezes the
complete input scale; it is an inspection artifact, not a constructor bundle.
Existing output directories are refused. A different relative export_dir keeps
another run beside each case. All export contents are ordinary files.

Root run.R has example <- NULL, time_label <- "08:00", export <- FALSE and
export_dir <- "output" settings. NULL renders all cases; one folder name renders
that case. time_label selects a supplied source-clock label. TRUE requests the
export shown in each report; ordinary rendering leaves output/ absent. PeMS uses separate regional
export directories and one shared package implementation. An individual runner
accepts a source-clock label and optional --export after it. Full preparation
code and validation details appear in each report's appendix.
