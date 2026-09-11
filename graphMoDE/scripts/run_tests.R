# Run focused portable regression tests without installing a package.

if (!file.exists(file.path(getwd(), "scripts", "load_all.R"))) {
    stop("Start R in the graphMoDE directory before running this script.",
         call. = FALSE)
}
source(file.path("scripts", "load_all.R"))
source(file.path("tests", "test_allocation_conditionals.R"))
cat("PASS: focused graphMoDE regression tests completed.\n")

