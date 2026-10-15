# Historical litr sources

These `.Rmd.txt` files are inert archival text, not package build inputs or
maintained tutorials. Do not execute the old generation/deletion commands.
The authoritative implementation is maintained directly in `bdynets/R/`.

The original migration preserved the metadata/build instructions here.
On 2026-10-15, the remaining litr source chapters and obsolete root stubs were
removed from the root and preserved byte-for-byte under `retired-chapters/`:

- `1description.Rmd.txt`: superseded metadata instructions.
- `2helpers.Rmd.txt`: historical helper definitions with `send_to` chunks.
- `3data.Rmd.txt`: empty synthetic-data chapter heading.
- `4gibbs.Rmd.txt`: historical sampler definitions with `send_to` chunks.
- `5simulations_SANDNM.Rmd.txt`: historical SANDNM generator source, including
  code not ported to the maintained package; retained for research provenance.
- `6real.Rmd.txt`: superseded build instructions and real-data placeholder.

`index.Rmd` remains an ordinary development guide; `5simulations.Rmd` remains
a historical simulation example. Dataset and project tutorials/reports remain
in their existing directories. Archival sources are not claims that historical
experiments execute against the current package. The obsolete generated `_book/` and `_main_files/` directories were removed
on 2026-10-15. One verified archival copy of their three simulation figures
is retained in [figures/](figures/README.md), with SHA-256 checksums. Dataset
visualizations and project reports remain in place.
