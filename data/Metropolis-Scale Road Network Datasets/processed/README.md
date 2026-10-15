# Metropolis-format simulated example

`metropolis-simulated.json.gz` is **simulated**, not extracted real observations.
It contains 142 synthetic road segments and 96 fifteen-minute frames in the
original viewer's metadata/segments/frames format. Preparation is reproducible:

```r
bdynets::simulate_metropolis_data("/path/to/new/metropolis-simulated.json.gz",
                                 seed=20261015L)
```

The original HTML requires `../map/city_traffic_M_osm_congestion_sample.js`.
That companion file and the referenced raw Parquet inputs are absent from this
checkout. The HTML itself contains no observation payload. Searches of local
project copies and Git history did not recover the companion file.

TODO: restore the real companion JS payload, then use
`bdynets::extract_traffic_data(js_path, "metropolis", new_json_gz_path)`.
Keep the simulated and real files distinct. Reporting reads standalone data;
it never generates simulations or downloads data implicitly.
