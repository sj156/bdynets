# Both arms can consume the same read-only inputs from a different volume.
script <- sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE))
if (!file.exists(script)) script <- gsub("~+~"," ",script,fixed=TRUE)
experiments <- dirname(dirname(normalizePath(script)))

check_separate_roots <- function() {
  tmp <- tempfile("separate study roots ")
  dir.create(tmp)
  on.exit(unlink(tmp,recursive=TRUE))
  frozen <- file.path(tmp,"inputs")
  output <- file.path(tmp,"results")
  study <- "01-graph-error"
  dataset <- file.path(frozen,study)
  dest <- file.path(output,study)
  tools_dir <- file.path(dirname(experiments),"tools")
  cli <- function(arm,action,...,data=frozen,error=NULL) {
    args <- c(file.path(tools_dir,paste0("study1-",arm,".R")),paste0("--action=",action),
      paste0("--output=",output),paste0("--data-root=",data),"--profile=smoke",...)
    log <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
      vapply(args,shQuote,character(1)),stdout=TRUE,stderr=TRUE))
    if (is.null(error)) {
      if(!is.null(attr(log,"status"))) stop(paste(log,collapse="\n"))
    } else if (is.null(attr(log,"status")) || !any(grepl(error,log,fixed=TRUE)))
      stop("Expected ",error,"\n",paste(log,collapse="\n"))
    TRUE
  }
  scenario <- "--scenario=T100-error0"
  cli("gls","data",scenario)
  stopifnot(file.exists(file.path(dataset,"panels.csv")),!file.exists(output))
  files <- list.files(frozen,recursive=TRUE,full.names=TRUE,all.files=TRUE)
  dirs <- list.dirs(frozen,recursive=TRUE)
  before <- tools::md5sum(files)
  on.exit({Sys.chmod(dirs,"0755"); Sys.chmod(files,"0644")},add=TRUE,after=FALSE)
  Sys.chmod(files,"0444"); Sys.chmod(dirs,"0555")
  ok <- parallel::mclapply(c("gls","competitors"),function(arm)
    cli(arm,"run",scenario,if(arm=="gls") "--method=gls" else "--method=pooled-pnar"),
    mc.cores=if (.Platform$OS.type=="windows") 1L else 2L)
  stopifnot(all(vapply(ok,isTRUE,logical(1))))
  cli("competitors","report")
  stopifnot(file.exists(file.path(output,"reports",paste0(study,".html"))))
  jobs <- read.csv(file.path(dest,"jobs.csv"))
  selected <- jobs[jobs$status=="complete",]
  stopifnot(setequal(selected$method,c("gls","pooled-pnar")),length(unique(selected$panel_md5))==1L,
            identical(before,tools::md5sum(files)),identical(files,list.files(frozen,
              recursive=TRUE,full.names=TRUE,all.files=TRUE)),
            !length(list.files(output,"^panel(-info)?[.]rds$",recursive=TRUE)))
  cli("gls","run",scenario,"--seed=1",error="seed differs from the shared dataset")
  Sys.chmod(dirs,"0755"); Sys.chmod(files,"0644")
  dir <- file.path(dataset,"T100-error0","rep-00001")
  path <- file.path(dir,"panel.rds")
  info_path <- file.path(dir,"panel-info.rds")
  p <- readRDS(path)
  p$split$train <- rev(p$split$train)
  saveRDS(p,path)
  cli("gls","report",error="Saved panel checksum changed")
  info <- readRDS(info_path)
  info$panel_md5 <- unname(tools::md5sum(path))
  saveRDS(info,info_path)
  cli("competitors","run",scenario,error="results use a different shared panel")
  cli("competitors","report",error="Job input differs from its generated panel")
  unlink(info_path)
  cli("gls","report",error="Job input differs from its generated panel")
}
check_separate_roots()
cat("Read-only shared inputs, concurrent arms, joint HTML and independent checksums passed.\n")
