# Traffic examples

`bdynets` draws nonnegative road observations and timestamped events from one
input contract. Every example uses the same R implementation and browser renderer.

## Input records

| Table | Required fields | Optional fields |
|---|---|---|
| roads | road_id, geometry | region, opposite_id; descriptive scalar attributes |
| flow | road_id, time, flow | descriptive fields |
| events | time, lon, lat | id, category, description, region, time_label |

Road geometry is a numeric longitude/latitude matrix in a list-column, with at
least two vertices, ordered in travel direction. Construction and travel-order
normalization belong to data preparation. The package retains these source
coordinates and computes display separation for opposing geometry through one
common matcher. Direction, lane, speed-limit, maxVolume and other descriptive
fields do not control pairing, color, line width or opacity.

Flow records have unique road-time keys and refer to supplied road IDs. Zero is
observed; NA and omitted pairs are missing. Values are not converted or normalized.
The optional unit is a supplied label; omission displays `unit unspecified`.
Physical units are neither inferred nor verified from numeric values.

Time is numeric seconds on a shared source clock or consistently typed POSIXct
instants. Flow timestamps match frame starts. Positive nonoverlapping intervals
define event membership as [start, end). Numeric labels do not establish a timezone.
The default interval is the frame difference, or one hour for a single frame and
for event-only binning when an interval is absent.

## Common display rules

Every input opens at its first frame with all records selected. Region and event
category controls apply only after selection. Named regions match provided labels
exactly. Empty region labels are unassigned and remain in the all-record scope.

Road colors use the same green, light green, yellow, yellow-orange, orange,
red-orange and red palette. Six boundaries divide zero through the global maximum
into seven equal widths after log1p transformation. The maximum covers all supplied
times and records. Exact thresholds remain fixed; displayed limits are rounded.
Missing values are gray. All-zero and entirely missing inputs have explicit legends.
Line widths use one continuous zoom baseline with a local density cap; opacity
is fixed. Density is visible road length per display area, measured on a 96-unit
grid and smoothed over neighboring cells. Segment samples are at most 24 units
apart after clipping to the view plus one grid cell. Per-road density is length
weighted. Dense views receive finer strokes, narrower opposing gaps and lighter
white casing; sparse views retain wider strokes. Panning, zooming and resizing
recompute layout without changing records, classifications or event positions.
Road count, flow values and descriptive attributes do not select a drawing style.

The optional `opposite_id` column names a road's opposite `road_id`. Nonempty
relationships are reciprocal and reference existing, distinct road IDs. Empty or
NA entries use automatic geometry matching. Declared relationships take priority
and restrict matching to the supplied partner; they do not reverse vertices or
connect unrelated geometry. The two paths still need overlapping reverse segments.

Automatic matching first accepts unique exact reverse paths. Remaining segments
use at least three probes, at most 40 ground metres apart, with reverse headings
within 15 degrees and lateral proximity within 30 ground metres. A probe needs a
mutual nearest counterpart at its projected position. Competing paths within
2 metres of the best distance are ambiguous; contiguous aligned segments sharing
an endpoint count as a continuation. At least 60 percent of each original segment's
probes need consistent support on both sides. Segments whose supported matches
switch lateral sides substantially are excluded. Ambiguous records retain their
source geometry. Matching is independent of road IDs and descriptive attributes,
except for the explicit opposite_id relationship. Geometry alone does not establish
legal travel direction or a shared road identity.

Crowded opposing paths are separated to a centerline gap of line width plus a
bounded edge gap. Width ranges from 0.85 to 4.5 display units; the edge gap ranges
from 0.55 to 1.8 units and varies continuously with width. Existing lateral distance contributes to the gap; sufficient original
separation receives no shift. Coincident paths use the right side of each path as
a drawing convention. Noncoincident paths move away from their nearest opposing
path. Adjacent segment shifts are averaged at vertices. Interactive separation
uses pixels and is recomputed after view changes; static figures use 1/96-inch
graphics line-width units with the same density calculation.
Source geometry, observations and event positions are retained. Popups refer to
the original road records; display displacement is not a physical road location.

Layer controls independently show roads, events, basemap and road casing. Controls
are disabled when that input component is absent. Layer visibility does not alter
statistics, filters, time membership or geographic extents. Turning roads off also
removes their casing. Clicking an event-list entry makes its event layer visible.
Road and event popups retain their record information.

Events always use a red diamond with a white border and exclamation mark. Categories
filter records without changing symbols. No duration or road assignment is inferred.

Roads and events both contribute to geographic focus across the supplied times.
Reset view uses the central 15-85 percent spatial range, or the complete range for
fewer than five locations. Full extent includes all road vertices and event locations
in the selected region and category scope. Numerical summaries are independent of
the visible map extent.

Coordinate projections and road bounds are cached. A spatial index finds roads
intersecting the view plus its density margin. Road layers are created when needed
and reused across views; offscreen layers are detached from the renderer. Full
input records remain available to statistics, time controls, filters and Full
extent. Repeated unchanged views reuse layout and display geometry. These caches
are internal to the standalone HTML and do not create additional files.

## Optional context

The basemap field is a GeoJSON FeatureCollection with numeric longitude/latitude
coordinates. Supported geometry is LineString (a matrix), Polygon (a list of closed
ring matrices) and Point (a numeric pair). Context has one neutral geometry-type
style. Optional Point captions use `properties.label`. Other properties, including
highway, population, name and custom style fields, do not change context rendering.

The attribution string is supplied as `basemap$attribution`. Its absence displays
`Basemap source unspecified`. The package does not infer a provider, license or
geographic meaning from a polygon or property name.

## Shared calls

```r
library(bdynets)
roads <- data.frame(road_id=c('a','b'))
roads$geometry <- list(
  rbind(c(10,50),c(10.01,50)),
  rbind(c(10.01,50),c(10.01,50.01)))
flow <- data.frame(road_id=c('a','b','a','b'),
  time=c(0,0,3600,3600),flow=c(120,300,180,NA))
events <- data.frame(time=1800,lon=10.005,lat=50.005,
  category='Recorded event',description='Timestamped location')
x <- traffic_data(roads,flow,events,interval_seconds=3600)
plot(x)
traffic_map(x,'map.html')
traffic_report(x,'report.html')
```

A declared opposing pair uses reciprocal identifiers:

```r
paired_roads <- roads
paired_roads$geometry[[2]] <- roads$geometry[[1]][2:1, , drop=FALSE]
paired_roads$opposite_id <- c('b','a')
paired <- traffic_data(paired_roads,flow,events,interval_seconds=3600)
```

The same constructor accepts roads with flow, events alone, or both together.
`traffic_summary()` and `traffic_trend()` use the same records as the maps. The
report summarizes all records and contains a geographic figure, each applicable
flow and event trend, statistics, preparation and source descriptions. It does
not automatically split the report by region or choose a peak frame.

## Examples and runtime

R 4.1 or later is required. Package imports are data.table, ggplot2, graphics,
grid, grDevices, jsonlite, magrittr, stats, tools and utils. Graphics, grid,
grDevices, stats, tools and utils are included with R. The optional test suite
also uses testthat, posterior, BayesLogit and beepr.

The shared examples/run.R installs the adjacent bdynets source package into a
temporary library when the source is present, or uses an installed bdynets package
when the examples are distributed separately. The same loop discovers each
*/data/input.rds and writes its report.html. VS Code Code Runner runs this entry
file regardless of the working directory. Its example setting is NULL for all
inputs or one folder name, such as "beijing", for a single report. Its repository-root command-line
equivalent is `Rscript examples/run.R`. Reports need no Pandoc, R Markdown or
network connection. An installed package also supports the Shared calls above;
input and output paths there are relative to the active R working directory.

| Folder | Records |
|---|---|
| beijing | Simulation day 10; 11,742 directed edges; 288 five-minute mean all-lane flow frames, in vehicles/hour. |
| metropolis | 2024-10-18; 720 roads; 96 fifteen-minute source-volume frames. |
| largest | 2026-06-16; 24 hourly frames; 4,435 Los Angeles and 2,203 Bay Area links, in vehicles/hour; 1,019 CHP reported event starts: 700 in the Los Angeles selection and 319 in D04. |
| synthetic | Generated grid; 48 roads including 8 reciprocal paths, 12 frames and 18 events; two named groups and unassigned records; unit and context omitted. |

The event selection in largest covers D04, D07, D08 and D12 on 2026-06-16.
All 319 D04 event summaries have valid coordinates; six have no linked detail
messages and remain included.

Each folder contains data/input.rds and report.html. The RDS schema is
trafficmap-input-v1; other top-level fields are named traffic_data arguments.
Descriptive metadata belongs to the input. Source-specific plotting profiles
are not accepted fields.

HTML embeds the records, Leaflet, context and report figures. A modern browser
with DecompressionStream support displays it offline. The bdynets package includes its
MIT license and Leaflet's BSD license; context attribution comes from the input.
