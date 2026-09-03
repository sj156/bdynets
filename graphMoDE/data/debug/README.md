# Truth-blinded debugging data

These files contain synthetic data and small diagnostic controls only. They do
not contain the latent truth, real traffic counts, population values or private
observations.

| file | purpose |
|---|---|
| `k10_blinded_simulation_input.rds` | synthetic `Y` (100x168), `Fmat` (168x2), data seed and their serialized SHA-256; truth fields are absent |
| `k10_debug_controls.rds` | six reviewed starts, 12 task seeds, neutral theta/gamma, trace fingerprints and the explicit `formal_simulation_authorized=FALSE` gate |
| `k10_known_failure.rds` | original 824-byte failure record from the incomplete diagnostic run |
| `conditional-allocation-mode-specification.csv` | human-readable provenance for the six reviewed starting partitions |
| `conditional-allocation-rule-specification.csv` | the pre-registered passive diagnostic quantities and thresholds |
| `parent-scientific-trace-audit.csv` | expected and observed fingerprints for the 12 parent scientific traces |

The synthetic `Y`/`Fmat` identity is
`1e5d62f5c8d2978b62c862dbb3d30e84b217af22b87208563f41fc08742ce845`.
The GMDE-W graph-basis object used by the failing chain has serialized hash
`111b28887bfc8d12389f8f0f211276754671ed0d9520cb231e990949c04d62b0`.

The large parent and failed-result archives are not prerequisites for the
low-level reproduction script and are intentionally not distributed.

