# Flow and OD map examples

The [package input specification](../bdynets/inst/INPUT-FORMAT.md) defines both ready inputs and the compatibility paths. Prepare the records, values and time scope before calling the corresponding map function.

| Example | Prepared map input | Plotting function |
|---|---|---|
| [Beijing](beijing/README.md) | [08:00 five-minute Flow frame](beijing/data/map-frame-0800.rds) | `traffic_flow_map()` |
| [PeMS](largest/README.md) | [08:00 hourly Flow frame with reported CHP starts](largest/data/map-frame-0800.rds) | `traffic_flow_map()` |
| [City-traffic-M](metropolis/README.md) | [08:00 fifteen-minute Flow frame with metrics](metropolis/data/map-frame-0800.rds) | `traffic_flow_map()` |
| [Divvy](DivvyBike/README.md) | [00:00–03:00 directed OD routes and stations](DivvyBike/data/spatial-1500m/window-0000-0300.rds) | `traffic_od_map()` |

From the bundle root, run this in a fresh R session to load the bundled package.

```r
source("bdynets/inst/preparation-session.R")
preparation_session("examples/beijing")
library(bdynets)
traffic_flow_map(readRDS("examples/beijing/data/map-frame-0800.rds"), basemap = NULL)
traffic_od_map(readRDS("examples/DivvyBike/data/spatial-1500m/window-0000-0300.rds"),
               basemap = NULL)
```

Each report shows how its source records become a drawable input. The full-day Flow files support preparation and trends. A selected-time Flow export writes `output/figures/map-input.rds` for reuse. Divvy produces a fresh ready RDS when its preparation window changes.
