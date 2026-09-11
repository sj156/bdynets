# graphMoDE module in bdynets

The maintained graphMoDE implementation is now a module of the single
[`bdynets` package](../bdynets), with scripts in [`bdynets/R`](../bdynets/R)
named `gmde-*.R`. Its private helpers use the `.gmde_` prefix.

After pushing, install with:

```r
pak::pak("sj156/bdynets/bdynets", dependencies = TRUE)
library(bdynets)
```

Use `gmde_graph()`, `gmde_expert()`, `gmde_gate()`, `gmde_mcmc()`, `gmde_fit()`
and `gmde_partition()`. The Gaussian/static scope and statistical contracts
are unchanged. See the [example and module documentation](../bdynets/README.md)
and [migration record](../MIGRATION.md).

The earlier standalone `graphMoDE/packages/graphMoDE` development copy has been
retired from the Git checkout to avoid two maintained implementations.
The older scripts in this folder's root `R/` remain the historical research
snapshot; they are not loaded by bdynets and have not been overwritten.
