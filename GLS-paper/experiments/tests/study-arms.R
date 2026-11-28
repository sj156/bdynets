# Shared-panel contract for all six public study arms (small real R fits).
script <- sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE))
if (!file.exists(script)) script <- gsub("~+~"," ",script,fixed=TRUE)
src <- dirname(dirname(normalizePath(script)))
source(file.path(src,"designs.R"))
source(file.path(src,"workflow.R"))
source(file.path(src,"study.R"))

check_study_arms <- function() {
  tmp <- tempfile("bdynets study arms ")
  dir.create(tmp)
  on.exit(unlink(tmp,recursive=TRUE))
  data_option <- character()
  cli <- function(path, ..., profile="smoke", output=tmp, error=NULL) {
    args <- c(file.path(src,path), if (!is.null(output)) paste0("--output=",output),
              if (!is.null(profile)) paste0("--profile=",profile),data_option,...)
    log <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
      vapply(args,shQuote,character(1)),stdout=TRUE,stderr=TRUE))
    if(is.null(error)) {
      if (!is.null(attr(log,"status"))) stop(paste(log,collapse="\n"))
    } else if (is.null(attr(log,"status")) || !any(grepl(error,log,fixed=TRUE))) {
      stop("Expected error: ",error,"\n",paste(log,collapse="\n"))
    }
    invisible(log)
  }
  prev <- setwd(tmp)
  on.exit(setwd(prev),add=TRUE,after=FALSE)

  stopifnot(identical(study_replicates(NULL,"full"),1:50),
            identical(study_replicates(NULL,"smoke"),1L),
            identical(study_replicates("4:6,1","full"),c(4:6,1L)),
            inherits(try(study_replicates("51","full"),silent=TRUE),"try-error"),
            inherits(try(study_replicates("1:2,2","full"),silent=TRUE),"try-error"))
  cases <- c("T100-error0","mixed-dynamic","local-on-transmit-on")
  competitors <- c("pooled-pnar","poisson-lasso","pnar")
  for (i in seq_along(cases)) {
    # Study 2 uses a separate frozen-panel directory for both arms.
    data_parent <- if (i==2L) file.path(tmp,"frozen counts") else tmp
    data_option <- if (i==2L) paste0("--data-root=",data_parent) else character()
    case_root <- file.path(tmp,study_cases[i])
    panels_root <- file.path(data_parent,study_cases[i])
    scenario <- paste0("--scenario=",cases[i])
    gls <- file.path("..","tools",paste0("study",i,"-gls.R"))
    competitor <- file.path("..","tools",paste0("study",i,"-competitors.R"))
    for (entry in c(gls,competitor)) {
      plan_root <- file.path(tmp,"no-such-output")
      plan <- cli(entry,profile=NULL,output=plan_root)
      stopifnot(any(grepl("49, 50",plan)),any(grepl("Method jobs: ",plan)),
                !file.exists(plan_root))
      subset_plan <- cli(entry,"--action=plan",scenario,"--replicate=1,3",output=NULL)
      stopifnot(any(subset_plan == "Replicate IDs: 1, 3"), any(subset_plan == "Shared panels: 2"))
    }
    cli(gls,"--action=run",scenario,"--method=gls",error="Generate the shared panels first")
    cli(gls,"--action=data",scenario,"--replicate=1:2")
    paths <- read.csv(file.path(panels_root,"panels.csv"))
    stopifnot(identical(paths$replicate,1:2),length(unique(paths$seed))==2,
              identical(unname(tools::md5sum(file.path(panels_root,paths$path))),paths$panel_md5))
    cli(gls,"--action=run",scenario,paste0("--method=",competitors[i]),error="Choose from")
    cli(competitor,"--action=run",scenario,"--method=gls",error="Choose from")
    cli(gls,"--action=run",scenario,"--method=gls")
    cli(competitor,"--action=run",scenario,paste0("--method=",competitors[i]))
    for (method in c("gls",competitors[i])) {
      job <- readRDS(file.path(case_root,dirname(paths$path[1]),method,"job.rds"))
      entry <- if (method=="gls") gls else competitor
      expected <- experiment_source_digest(src,c("methods.R","workflow.R","study.R",entry))
      stopifnot(identical(job$identity$code,expected),!anyNA(expected))
    }
    if (i==2L) stopifnot(!any(file.exists(file.path(case_root,paths$path))))
    input_hashes <- tools::md5sum(file.path(panels_root,paths$path))
    cli(competitor,"--action=report")
    stopifnot(file.exists(file.path(tmp,"reports", paste0(study_cases[i], ".html"))),
              identical(input_hashes, tools::md5sum(file.path(panels_root,paths$path))))
    jobs <- read.csv(file.path(case_root,"jobs.csv"))
    selected <- jobs[jobs$status=="complete",]
    stopifnot(setequal(selected$method,c("gls",competitors[i])),
              all(selected$replicate==1L),all(selected$panel_md5==paths$panel_md5[1]))
    pairs <- read.csv(file.path(case_root,"paired-replicates.csv"))
    stopifnot(any(pairs$method=="gls" & pairs$reference==competitors[i]) ||
              any(pairs$method==competitors[i] & pairs$reference=="gls"))
    cli(competitor,"--action=data",scenario)
    stopifnot(identical(read.csv(file.path(panels_root,"panels.csv")),paths))
    # A copied dataset and checksum from another replicate must also fail.
    from <- file.path(panels_root,dirname(paths$path[2]),c("panel.rds","panel-info.rds"))
    dest <- file.path(panels_root,dirname(paths$path[1]))
    stopifnot(all(file.copy(from,dest,overwrite=TRUE)))
    cli(competitor,"--action=run",scenario,paste0("--method=",competitors[i]),
        error="Saved panel belongs to a different study, condition, replicate, seed or profile")
  }
}
check_study_arms()
cat("Six arm entry points, panel sharing, pairing, provenance and schedules passed.\n")
