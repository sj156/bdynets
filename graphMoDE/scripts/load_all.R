# Load the byte-identified graphMoDE development snapshot without installing it.

graphmode_find_root <- function(start = getwd()) {
    candidate <- normalizePath(start, winslash = "/", mustWork = TRUE)
    repeat {
        if (dir.exists(file.path(candidate, "R")) &&
            file.exists(file.path(
                candidate, "provenance", "FROZEN_SOURCE_SHA256.csv"
            ))) {
            return(candidate)
        }
        parent <- dirname(candidate)
        if (identical(parent, candidate)) break
        candidate <- parent
    }
    stop(
        "Cannot find the graphMoDE root. Start R in the graphMoDE directory.",
        call. = FALSE
    )
}

graphmode_root <- graphmode_find_root()

if (!requireNamespace("digest", quietly = TRUE)) {
    stop(
        "Package 'digest' is required to verify the frozen source before loading.",
        call. = FALSE
    )
}

graphmode_frozen_manifest <- utils::read.csv(
    file.path(graphmode_root, "provenance", "FROZEN_SOURCE_SHA256.csv"),
    stringsAsFactors = FALSE
)
if (!identical(
    names(graphmode_frozen_manifest), c("source_file", "sha256")
) || nrow(graphmode_frozen_manifest) != 16L ||
    anyDuplicated(graphmode_frozen_manifest$source_file)) {
    stop("The frozen-source manifest is malformed.", call. = FALSE)
}

graphmode_source_files <- file.path(
    graphmode_root, "R", graphmode_frozen_manifest$source_file
)
if (!all(file.exists(graphmode_source_files))) {
    stop("One or more frozen R source files are missing.", call. = FALSE)
}

graphmode_observed_source_sha256 <- vapply(
    graphmode_source_files,
    function(path) digest::digest(
        file = path, algo = "sha256", serialize = FALSE
    ),
    character(1)
)
if (!identical(
    unname(graphmode_observed_source_sha256),
    graphmode_frozen_manifest$sha256
)) {
    mismatch <- graphmode_frozen_manifest$source_file[
        unname(graphmode_observed_source_sha256) !=
            graphmode_frozen_manifest$sha256
    ]
    stop(
        "Frozen R source hash mismatch: ", paste(mismatch, collapse = ", "),
        call. = FALSE
    )
}

invisible(lapply(
    graphmode_source_files,
    sys.source,
    envir = .GlobalEnv
))

stopifnot(
    identical(
        countdlm_gmde_sampler_version,
        "exact-poisson-info-ffbs-joint-ess-devroye-2026-08-31-v2"
    ),
    identical(
        countdlm_road_conditional_allocation_api_version,
        "countdlm-road-k10-conditional-allocation-diagnostic-2026-09-03-v1"
    )
)

invisible(graphmode_source_files)
