# Exact selected definitions from R/graphmode-runner.R
graphmode_digest <- function(object) {
    if (!requireNamespace("digest", quietly = TRUE))
        stop("Package 'digest' is required for source/checkpoint identities; do not auto-install.", call. = FALSE)
    digest::digest(object, algo = "sha256", serialize = TRUE)
}
