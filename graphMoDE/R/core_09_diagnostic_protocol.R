# Exact selected definitions from R/graphmode4-design.R
graphmode4_protocol <- "graphMoDE-simple-roads-2026-09-12-r4"

graphmode4_methods <- c("graphMoDE-W", "graphMoDE-C", "EucMoDE", "MoDE", "PottsMoDE")

graphmode4_permutation <- function(x, n) {
    if (!is.numeric(x) || length(x) != n || anyNA(x) ||
        !setequal(x, seq_len(n))) stop("Supply a complete permutation.", call. = FALSE)
    as.integer(x)
}
