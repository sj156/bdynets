# Beijing figure reproduction

From the repository root, install `bdynets` (`R CMD INSTALL bdynets`). The reader
needs jsonlite; rendering needs rmarkdown and knitr. No fitting or downloads occur.

```sh
Rscript examples/beijing/run.R "data/Beijing Data/processed/beijing-visualization.json.gz" /tmp/new-beijing-figures
Rscript -e 'rmarkdown::render("examples/beijing/report.Rmd")'
```

Read `report.html` for the tutorial and three saved-data figures. `run.R` and
`report.Rmd` call the same `bdynets::traffic_figures()` workflow. The source HTML
is unchanged. Optional render parameters accept another compatible standalone dataset;
use an absolute path when supplying one. Outputs record the input MD5 and frame.

For a new compatible HTML snapshot, run `extract_traffic_data(path, source,
file="/path/to/new-data.json.gz")` once. Subsequent analyses read that standalone
file. Extraction preserves decoded JSON values and records source identity;
it neither downloads raw data nor reconstructs unrounded measurements.
