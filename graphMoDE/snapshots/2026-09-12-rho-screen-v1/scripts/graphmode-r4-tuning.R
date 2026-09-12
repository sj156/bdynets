#!/usr/bin/env Rscript
# D-037: no default stochastic action, and no automatic registration or resume.
arguments <- commandArgs(trailingOnly = TRUE)
entry <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(entry) != 1L) stop("Use Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
command <- if (length(arguments)) arguments[1L] else "help"
if (command == "help") {
    cat("graphmode-r4-tuning.R check\n",
        "graphmode-r4-tuning.R preflight NEW_CONTROLLED_PLAN.rds\n",
        "graphmode-r4-tuning.R run NEW_CONTROLLED_PLAN.rds --authorized\n",
        "New source freeze/preregistration and separate run approval required. No automatic rho selection, resume or formal simulation.\n", sep = "")
} else if (command == "check" && length(arguments) == 1L) {
    source(file.path(graphmode_root, "scripts/tests/graphmode-r4-tuning-deterministic.R"))
} else {
    valid <- command == "preflight" && length(arguments) == 2L ||
        command %in% c("run", "worker-run") && length(arguments) == 3L && arguments[3L] == "--authorized"
    if (!valid) stop("Invalid command; no run launched.", call. = FALSE)
    for (f in c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$"))))
        source(file.path(graphmode_root, "R", f))
    options(warn = 2)
    plan <- readRDS(arguments[2L])
    result <- switch(command,
        preflight = graphmode4_controlled_preflight(plan, graphmode_root),
        run = graphmode4_controlled_launch(plan, arguments[2L], graphmode_root, authorized = TRUE),
        `worker-run` = graphmode4_controlled_run(plan, graphmode_root, authorized = TRUE))
    print(result)
    if (!is.null(result$ready) && !result$ready) quit(status = 2L)
}
