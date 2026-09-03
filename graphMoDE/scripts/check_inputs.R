# Fast, read-only audit of the public graph and truth-blinded debug fixtures.

if (!file.exists(file.path(getwd(), "scripts", "load_all.R"))) {
    stop("Start R in the graphMoDE directory before running this script.",
         call. = FALSE)
}
source(file.path("scripts", "load_all.R"))

graph_file <- file.path(
    graphmode_root, "data", "osm-derived",
    "central_beijing_100_graph_input.rds"
)
node_file <- file.path(
    graphmode_root, "data", "osm-derived", "central_beijing_100_nodes.csv"
)
edge_file <- file.path(
    graphmode_root, "data", "osm-derived", "central_beijing_q4_edges.csv"
)
blinded_file <- file.path(
    graphmode_root, "data", "debug", "k10_blinded_simulation_input.rds"
)
controls_file <- file.path(
    graphmode_root, "data", "debug", "k10_debug_controls.rds"
)
failure_file <- file.path(
    graphmode_root, "data", "debug", "k10_known_failure.rds"
)
figure_file <- file.path(
    graphmode_root, "figures", "central_beijing_100_fixed_road_network.png"
)

expected_file_hash <- c(
    graph = "72f26d33c86842677417a20901abbbecac831df6eb04f47ebc8ecb6c8c1a08fd",
    nodes = "aa32365cf424e33fad100f348621849f9860bdfa4bf049da19f4fbecdd3c7989",
    edges = "68103ca6523ab7c97d8dce25292c0db9e3ed564164d70be4e9640269a29feef6",
    blinded = "f9a4f01cb0e852f38588147faf1f7289e0ed79f0e66825633ee6f0d286b15a3c",
    controls = "4e465e74d6d394a68c30fc3ec8eae25c678516cbc3893eae45955feca172f0bb",
    failure = "14b78f7ea8c9b594126add5c38eb9adf9cef479aae8f42e04b0f1a3860b745b4",
    figure = "e186d99e005d4ce4c366cde4039e459c035028f00383ca98eb02904f6caeade4"
)
handoff_files <- c(
    graph = graph_file,
    nodes = node_file,
    edges = edge_file,
    blinded = blinded_file,
    controls = controls_file,
    failure = failure_file,
    figure = figure_file
)
if (!all(file.exists(handoff_files))) {
    stop(
        "Missing handoff file(s): ",
        paste(names(handoff_files)[!file.exists(handoff_files)], collapse = ", "),
        call. = FALSE
    )
}
observed_file_hash <- vapply(
    handoff_files,
    function(path) digest::digest(
        file = path, algo = "sha256", serialize = FALSE
    ),
    character(1)
)
if (!identical(observed_file_hash, expected_file_hash)) {
    stop(
        "Handoff file hash mismatch: ",
        paste(names(handoff_files)[observed_file_hash != expected_file_hash],
              collapse = ", "),
        call. = FALSE
    )
}

context <- readRDS(graph_file)
blinded <- readRDS(blinded_file)
controls <- readRDS(controls_file)
failure <- readRDS(failure_file)
nodes <- utils::read.csv(
    node_file,
    stringsAsFactors = FALSE,
    colClasses = c(road_vertex_id = "character")
)
edges <- utils::read.csv(edge_file, stringsAsFactors = FALSE)

required_locations <- c(
    "location_id", "road_vertex_id", "longitude", "latitude", "cell_id",
    "center_road_distance_km", "ring_label"
)
stopifnot(
    is.list(context),
    identical(names(context$locations), required_locations),
    nrow(context$locations) == 100L,
    isTRUE(all.equal(
        nodes, context$locations, tolerance = 1e-14,
        check.attributes = TRUE
    )),
    !anyDuplicated(context$locations$location_id),
    !anyDuplicated(context$locations$road_vertex_id),
    all(is.finite(context$locations$longitude)),
    all(is.finite(context$locations$latitude)),
    identical(
        as.integer(tabulate(context$locations$cell_id, nbins = 25L)),
        rep(4L, 25L)
    ),
    identical(
        as.integer(tabulate(context$locations$ring_label, nbins = 5L)),
        rep(20L, 5L)
    ),
    identical(dim(context$D_km), c(100L, 100L)),
    identical(dim(context$W), c(100L, 100L)),
    identical(dim(context$A), c(100L, 100L)),
    identical(dim(context$L), c(100L, 100L)),
    identical(dim(context$Phi), c(100L, 40L)),
    all(is.finite(c(context$D_km, context$W, context$L, context$Phi))),
    max(abs(context$D_km - t(context$D_km))) <= 1e-8,
    max(abs(context$W - t(context$W))) <= 1e-10,
    all(diag(context$W) == 0),
    identical(1L * (context$W > 0), context$A),
    max(abs(context$L - (diag(rowSums(context$W)) - context$W))) <= 1e-10,
    sum(context$A[upper.tri(context$A)]) == 242L,
    nrow(edges) == 242L,
    all(rowSums(context$A) > 0L),
    countdlm_road_component_count(context$A) == 1L,
    identical(as.integer(context$q), 4L),
    isTRUE(all.equal(as.numeric(context$h), 3.8978354, tolerance = 1e-7)),
    identical(
        context$metadata$selected_id_sha256,
        "3e76c75c869c35774f167f08280e6e44f8104c8cd0f6fa3a31d7f63dc27e8dbf"
    ),
    identical(context$metadata$public_data_license, "ODbL-1.0")
)

selected_id_hash <- digest::digest(
    paste(context$locations$road_vertex_id, collapse = ","),
    algo = "sha256", serialize = FALSE
)
stopifnot(identical(
    selected_id_hash,
    "3e76c75c869c35774f167f08280e6e44f8104c8cd0f6fa3a31d7f63dc27e8dbf"
))

stopifnot(
    identical(
        blinded$schema_version,
        "graphMoDE-k10-blinded-input-2026-09-03-v1"
    ),
    identical(dim(blinded$Y), c(100L, 168L)),
    identical(dim(blinded$Fmat), c(168L, 2L)),
    identical(as.integer(blinded$data_seed), 2026091101L),
    identical(blinded$truth_fields_stored, FALSE),
    identical(
        blinded$Y_Fmat_sha256,
        "1e5d62f5c8d2978b62c862dbb3d30e84b217af22b87208563f41fc08742ce845"
    ),
    identical(
        digest::digest(
            list(Y = blinded$Y, Fmat = blinded$Fmat),
            algo = "sha256", serialize = TRUE
        ),
        blinded$Y_Fmat_sha256
    )
)

had_rng <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
if (had_rng) old_rng <- get(".Random.seed", envir = .GlobalEnv)
generated <- countdlm_road_generate_moderate(context, blinded$data_seed)
if (had_rng) {
    assign(".Random.seed", old_rng, envir = .GlobalEnv)
} else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
}
stopifnot(
    identical(generated$Y, blinded$Y),
    identical(generated$Fmat, blinded$Fmat)
)
rm(generated)

method_inputs <- countdlm_road_method_inputs(context)
stopifnot(
    identical(
        digest::digest(
            method_inputs[["GMDE-W"]]$Phi,
            algo = "sha256", serialize = TRUE
        ),
        "111b28887bfc8d12389f8f0f211276754671ed0d9520cb231e990949c04d62b0"
    ),
    identical(
        controls$schema_version,
        "graphMoDE-k10-debug-controls-2026-09-03-v1"
    ),
    nrow(controls$tasks) == 12L,
    length(controls$modes) == 6L,
    identical(dim(controls$neutral_theta), c(10L, 168L, 2L)),
    identical(dim(controls$neutral_gamma), c(40L, 9L)),
    identical(controls$formal_simulation_authorized, FALSE),
    identical(controls$known_failure$task_id, "GMDE-W-mode-A-seed-2"),
    identical(as.integer(controls$known_failure$seed), 2026094102L),
    identical(failure$task_id, controls$known_failure$task_id),
    identical(as.integer(failure$seed), 2026094102L),
    identical(failure$status, "error"),
    identical(failure$warnings, character()),
    identical(failure$error, controls$known_failure$error),
    identical(failure$completed_fit_retained, FALSE),
    identical(failure$terminal_state_retained, FALSE)
)

cat(
    "PASS: 16 frozen R sources verified.\n",
    "PASS: public 100-node q=4 graph and figure verified.\n",
    "PASS: blinded K=10 data regenerate exactly from seed 2026091101.\n",
    "PASS: debug controls and known failure record verified.\n",
    "STATUS: formal_simulation_authorized = FALSE.\n",
    sep = ""
)
