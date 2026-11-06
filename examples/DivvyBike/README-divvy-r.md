# Divvy OD aggregation and maps

The example uses aggregate_od_data() and traffic_od_map() from the same bundled
bdynets package. The supplied 2 km tables pool hours 00:00 through 03:00 across
all January 2026 dates in America/Chicago. That figure contains 53 active nodes,
206 directed nonloop pairs and 969 nonloop trips. The five fixed flow classes
are 1, 2-3, 4-5, 6-7 and at least 8; the OD drawing algorithm is retained.

The report also executes preparation of the raw station and OD tables with the
1000 m parameter from the generic aggregation example, and checks that CSV re-reading preserves the selected trip counts. This radius
cover need not reproduce the membership of the supplied processed r2000 table.
Both preserve the full source trip count.

The ten supplied CSV files remain unchanged. Background geometry is obtained by
traffic_basemap from the active endpoint coordinates at runtime and cached in
data/basemap-cache. The delivered example contains no background geometry file.
The first acquisition needs a network connection; a matching cache supports
later offline use. The report records the actual request bounds and source
identities. The aggregation checks do not create an additional map variant.

run.R installs ../../bdynets into a temporary library and renders divvy-od-report.Rmd to divvy-od-report.html
from a fresh R session. It is independent of the caller's working directory and
of the installed package version. VS Code Code Runner executes the complete
run.R. An optional new output-directory argument also writes the main PNG map;
normal rendering creates no output directory or separate PNG. Temporary libraries,
prepared CSV files and intermediate figures are removed.

R 4.1 or later, the imports declared in ../../bdynets/DESCRIPTION, knitr,
rmarkdown and Pandoc are required. Each code block is followed by its result.
The report has no table of contents and opens offline.
