# beijing

Simulation day 10; 11,742 roads and 288 five-minute road-flow frames.

beijing-traffic-report.Rmd is the executable static report; its self-contained `beijing-traffic-report.html`
is beside it. prepare.R maps data/source.rds to data/input.rds without downloading
or resampling data. The report includes the executed preparation, input fields,
statistics, static maps, interpretation and source identities.

run.R installs ../../bdynets into a temporary library and renders this report in
a fresh R session. The default time is 08:00; another supplied HH:MM is an optional
argument. A second --export argument requests the shared PNG/PDF and data export
in a new output directory. Existing export directories are refused. No common
runner or preinstalled trafficmap package is used.

R 4.1 or later and the imports in ../../bdynets/DESCRIPTION are required, together
with knitr, rmarkdown and Pandoc. VS Code Code Runner executes the complete run.R.
Normal rendering regenerates only data/input.rds and the named HTML. Temporary
libraries, intermediate documents and figures are removed.

The report acquires OpenStreetMap context from its input coordinates through
traffic_basemap. No background geometry is bundled. Initial acquisition needs
network access; data/basemap-cache supports later reuse.
