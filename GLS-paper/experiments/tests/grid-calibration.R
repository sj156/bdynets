# Strong grid dynamics can diverge at a trial scale during pilot calibration.
script <- sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE))
if (!file.exists(script)) script <- gsub("~+~"," ",script,fixed=TRUE)
src <- dirname(dirname(normalizePath(script)))
source(file.path(src,"designs.R"))
source(file.path(src,"workflow.R"))

seed <- experiment_panel_seed("02-sparse-grid","full",1L,200425L)
panel <- sparse_grid_panel("mixed-dynamic-strong","full",seed)
cal <- panel$calibration
stopifnot(cal$method=="bracketed",abs(cal$pilot_rms-.2)<=cal$relative_tolerance*.2,
          all(is.finite(panel$lambda)),is.finite(panel$realized_rms))
# An infeasible pilot target must be identified with the scenario and seed.
failure <- tryCatch(
  calibrate_grid_scale(.2,function(x) if(x>=.1) Inf else x,"test seed 1"),
  error=conditionMessage)
stopifnot(is.character(failure),grepl("unresolved for test seed 1",failure,fixed=TRUE))
cat("Grid calibration recovered a divergent pilot and reported an infeasible target.\n")
