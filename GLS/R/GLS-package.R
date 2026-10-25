#' GLS: Structured Global-Local Shrinkage and Selection
#'
#' The package provides a reusable implementation of the graph-shrinkage GLS
#' sampler, joint DSS, sequential prediction, and the sparse multi-hop
#' simulation used by the accompanying research project.
#'
#' @keywords internal
#' @import stats
#' @importFrom utils head tail
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "chain", "contribution_rms", "discount", "draw", "DSS_selected",
    "is_active", "pair_class", "pairs", "posterior_amplitude",
    "probability", "type", "unregularized_amplitude", "value"
  ))
}
