# Metropolis figure tutorial

The checked-in HTML refers to a missing companion JS payload. The bundled example
is explicitly SIMULATED, following the same data schema and visualization rules.
Original-city numerical reproduction remains pending restoration of that file.

From the repository root, with bdynets/jsonlite installed:

```sh
Rscript examples/metropolis/run.R "data/Metropolis-Scale Road Network Datasets/processed/metropolis-simulated.json.gz" /tmp/new-metropolis-figures
Rscript -e 'rmarkdown::render("examples/metropolis/metropolis-traffic-maps.Rmd")'
```

Rendering also requires rmarkdown/knitr. Both entrypoints call package functions;
no fitting, simulation or downloading occurs during reporting. Read metropolis-traffic-maps.html
for the runnable tutorial, four figures and instructions to import the real data.
