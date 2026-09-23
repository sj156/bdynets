# Bundled Divvy bike example data

These files are frozen copies of the prepared data under
`simulaiton_1/divvy_real_data/output/data/` at the time the BDCN package was
created. They contain aggregate edge counts and network/design metadata, not
raw trip records.

- Period: January--June 2026.
- Resolution: 3-hour wall-clock bins.
- Network: 24 directed non-loop OD links selected using January--April only.
- Split: rows 13--960 training, 961--1208 May validation, and 1209--1448 June
  test; rows 1--12 are common lag history.
- `calendar.csv`: the frozen 13-column design selected by the original
  validation workflow (trend, six weekday indicators, and three daily Fourier
  harmonics).
- `A.csv` and `W.csv`: edge adjacency and row-normalized physical network.

Use `bdcn_bike_data()` to load and validate these files. An external directory
with the same six CSV files can be supplied through its `data_dir` argument.

