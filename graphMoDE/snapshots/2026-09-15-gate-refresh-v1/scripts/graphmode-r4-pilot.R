#!/usr/bin/env Rscript
# No-argument/help/check/prepare/preflight do not start scientific random work.
arguments <- commandArgs(trailingOnly = TRUE)
entry <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(entry) != 1L) stop("Use Rscript --vanilla.", call. = FALSE)
graphmode_root <- normalizePath(file.path(dirname(sub("^--file=", "", entry)), ".."), mustWork = TRUE)
command <- if (length(arguments)) arguments[1L] else "help"
if (command == "help") {
    cat("graphmode-r4-pilot.R check\n",
        "graphmode-r4-pilot.R prepare NEW_EXTERNAL_REGISTRATION_DIR\n",
        "graphmode-r4-pilot.R preflight REGISTRATION.rds\n",
        "graphmode-r4-pilot.R run REGISTRATION.rds --authorized\n",
        "Only the last command launches after later user confirmation. No resume/formal mode.\n", sep = "")
} else if (command == "check" && length(arguments) == 1L) {
    source(file.path(graphmode_root, "scripts/tests/graphmode-r4-pilot-deterministic.R"))
} else {
    valid <- command %in% c("prepare", "preflight", "chain-preflight") && length(arguments) == 2L ||
        command %in% c("run", "chain-run") && length(arguments) == 3L && arguments[3L] == "--authorized"
    if (!valid) stop("Invalid arguments. No simulation started.", call. = FALSE)
    for (f in c("gmde-helpers.R", "gmde-state-update.R",
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode-.*[.]R$")),
        sort(list.files(file.path(graphmode_root, "R"), "^graphmode4-.*[.]R$"))))
        source(file.path(graphmode_root, "R", f))
    options(warn = 2)
    if (command == "prepare") result <- graphmode4_pilot_prepare(graphmode_root, arguments[2L]) else {
        record <- readRDS(arguments[2L])
        result <- switch(command,
            preflight = graphmode4_pilot_preflight(record, graphmode_root),
            run = graphmode4_pilot_run(record, graphmode_root, authorized = TRUE),
            `chain-preflight` = graphmode4_pilot_worker(record, graphmode_root, check_only = TRUE),
            `chain-run` = graphmode4_pilot_worker(record, graphmode_root, authorized = TRUE))
    }
    print(result)
    if (!is.null(result$ready) && !result$ready) quit(status = 2L)
}
