# Implementation specification, not a promotion of manuscript/current and not
# a run registration. Names are relative to the collaboration paper directory.
graphmode4_specification <- function() {
    list(protocol = graphmode4_protocol, sha256 = c(
        "graphMoDE-model.tex" = "de40dcaed8da6d97a5e70f14e9eaca82ab80f923b16402eb539e358efc3fd5ba",
        "graphMoDE-computation.tex" = "36bf053c3de4be4746d0435e3b9c6184d52a58c1c6f42e159fd1d03f76c87bcb",
        "graphMoDE-Appendix.tex" = "39ff8a8be492a3bd40d5a4d32e513441ed971a29e013ef98d2474284a83f878d",
        "graphMoDE-simple-road-experiments.tex" = "193cff2eb27fda5da5cad1561890ded2a73af62afeae3292a175f44774930402",
        "graphMoDE-simulation-implementation-note.md" = "84cd5350f21963fa1d9cc5f039a7541ea1e7bdfac1319b7701b13fb1315f9c9f"))
}

graphmode4_source_identity <- function(repository) {
    repository <- normalizePath(repository, mustWork = TRUE)
    core <- graphmode_source_identity(repository)
    files <- c(names(core$sha256), file.path("R", sort(list.files(file.path(repository, "R"),
        pattern = "^graphmode4-.*[.]R$"))), "scripts/graphmode-r4.R",
        "scripts/tests/graphmode-r4-deterministic.R", "docs/requirements/graphMoDE-simple-roads-2026-09-10.md")
    sha256 <- vapply(file.path(repository, files), function(f)
        digest::digest(file = f, algo = "sha256", serialize = FALSE), character(1))
    names(sha256) <- files
    git <- function(args) {
        output <- system2("git", c("-C", shQuote(repository), args), stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(output, "status"))) stop("Cannot resolve r4 Git identity.", call. = FALSE)
        output
    }
    changed <- git(c("status", "--porcelain", "--untracked-files=all", "--", shQuote(files)))
    tracked <- git(c("ls-files", "--", shQuote(files)))
    specification <- graphmode4_specification()
    list(protocol = graphmode4_protocol, core = core, sha256 = sha256,
        committed = !length(changed) && setequal(files, tracked), specification = specification,
        requirements_match = identical(unname(sha256[["docs/requirements/graphMoDE-simple-roads-2026-09-10.md"]]),
            unname(specification$sha256[["graphMoDE-simulation-implementation-note.md"]])),
        run_authorized = FALSE, note = "Read-only identity; not a registration or proof of current loaded-function identity.")
}
