# Synthetic roads and events

synthetic-traffic-maps.Rmd is the executable static report, with its saved HTML
beside it. prepare.R converts data/source.rds into data/input.rds. The source
cache contains this example's selected source-level records and supplied geometry;
the prepared file follows the common trafficmap-input-v1 contract. Both source
and prepared file identities appear in the report. No external source files,
USB paths or downloads are needed for routine preparation and rendering.

run.R executes preparation through the Rmd and renders the report at 08:00.
A different HH:MM label is an optional argument. With an installed bdynets package, Rscript run.R 08:00 --export also writes the shared static
export. The root runner installs the repository bdynets package into a temporary
library and discovers all cases. Its example, time_label, export and export_dir settings select a case, frame
and optional output folder in VS Code Code Runner. knitr, rmarkdown and Pandoc are the
report dependencies, alongside R 4.1 or later and jsonlite.

Normal rendering changes only the prepared input and named HTML. With export
selected, output/ contains PNG/PDF maps and applicable trends, selected-frame.rds,
summary.csv, trend.csv, provenance.json, inputs.csv, checksums.csv and
session-info.txt. PeMS creates a separate export directory for each region.
Existing export directories are refused. The selection snapshot retains the
full input scale for inspection and is not a new input bundle.
