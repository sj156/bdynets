# Current limitations

The initial 2026-10-15 core pilot stopped before 4000 iterations. An SE-W bandwidth proposal caused positive edge weights to underflow in double precision; the existing guard stopped the run. It did not clip weights, drop physical edges, or reject the proposal as an artificial prior restriction. This numerical issue remains unresolved. Complete core convergence and predictive comparisons have not been established.

The preceding public commit cb273d26810a563ffeddcb7f0d1556fe30f12d1d also overwrote C's own calibrated kappa prior center with the W reference. This release corrects that configuration: only Exp-W and SE-W share the fixed Exp-reference center; C retains its own binary-adjacency calibration (2.0000 on the 121-node path). Old C chains must not be reused as draws from the corrected prior. Original run records and failures are retained outside Git.
