# Fast integration check with real Poisson fits on copied code and saved
# panels. Temporary result folders and paths containing spaces exercise the CLI.
args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value=TRUE))
if(!file.exists(script)) script <- gsub("~+~"," ",script,fixed=TRUE)
experiments <- dirname(dirname(normalizePath(script)))
test <- function() {
  tmp <- tempfile("bdynets provenance ")
  dir.create(tmp)
  on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
  src <- file.path(tmp,"example code")
  dir.create(src)
  case <- "01-graph-error"
  dir.create(file.path(src,case))
  files <- c("designs.R","methods.R","workflow.R")
  stopifnot(all(file.copy(file.path(experiments,files),src)))
  wk <- c("generate.R","fit.R","compare.R")
  stopifnot(all(file.copy(file.path(experiments,case,wk),file.path(src,case))))
  output <- file.path(tmp,"model output")
  cli <- function(action,rep=1L,success=TRUE) {
    shell_args <- c(file.path(src,case,paste0(action,".R")),paste0("--output=",output),
      "--scenario=T100-error0","--profile=smoke","--method=pooled-pnar",paste0("--replicate=",rep))
    result <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
      vapply(shell_args,shQuote,character(1)),stdout=TRUE,stderr=TRUE))
    stopifnot(identical(is.null(attr(result,"status")),success))
    result
  }
  for(rep in 1:2) { cli("generate",rep); cli("fit",rep) }
  cli("compare")
  root <- file.path(output,case)
  provenance <- function() read.csv(file.path(root,"provenance.csv"))
  stopifnot(all(provenance()$current_source_matches))
  cat("\n# Fixture for a later checkout.\n",file=file.path(src,"methods.R"),append=TRUE)
  cli("fit",success=FALSE)
  cli("compare")
  stopifnot(!any(provenance()$current_source_matches))
  # A changed active codebase has its own legitimate fits, but those fits
  # must not enter a paired estimate of a different scientific implementation.
  cli("generate",3L); cli("fit",3L)
  stopifnot(any(grepl("Incompatible saved method versions",cli("compare",success=FALSE))))
  new_case <- file.path(root,"T100-error0","rep-00003")
  unlink(new_case,recursive=TRUE)
  config_path <- file.path(root,"config.rds")
  original <- readRDS(config_path)
  changed <- original
  changed$runtime$R <- "deliberately different test runtime"
  saveRDS(changed,config_path)
  stopifnot(any(grepl("different generator code, R runtime",cli("generate",success=FALSE))))
  saveRDS(original,config_path)
  job_path <- file.path(root,"T100-error0","rep-00001","pooled-pnar","job.rds")
  job <- readRDS(job_path)
  job$scores$value[1] <- job$scores$value[1]+1
  saveRDS(job,job_path)
  stopifnot(any(grepl("result checksum differs",cli("compare",success=FALSE))))
}
test()
cat("Experiment runtime, code-drift, mixed-replication, integrity and path checks passed.\n")
