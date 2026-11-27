experiment_catalog <- list(
  `01-graph-error`=list(generator=graph_error_panel,ref="pooled-pnar",
    scenarios=c(outer(c("T100","T200"),c("error0","error25","error50"),paste,sep="-")),
    methods=c("pooled-pnar","gls-restricted","gls","gls-flat")),
  `02-sparse-grid`=list(generator=sparse_grid_panel,ref="poisson-lasso",
    scenarios=c("seasonal-null","local-static","local-dynamic","mixed-static",
                "mixed-dynamic","mixed-dynamic-strong"),
    methods=c("poisson-lasso","gls-static","gls","gls-flat")),
  `03-surges`=list(generator=surge_panel,ref="gls-own",
    scenarios=c(outer(c("local-off","local-on"),c("transmit-off","transmit-on"),paste,sep="-")),
    methods=c("gls-own","gls","pnar","sa-pgss","graph-wavenet"))
)

save_atomic <- function(x, path) {
  tmp <- tempfile("pending-",dirname(path))
  on.exit(unlink(tmp))
  saveRDS(x,tmp)
  if (!file.rename(tmp,path)) stop("Could not finalize ",path)
}
write_table <- function(x, path) {
  tmp <- tempfile("pending-",dirname(path))
  on.exit(unlink(tmp))
  write.csv(x,tmp,row.names=FALSE)
  if (!file.rename(tmp,path)) stop("Could not finalize ",path)
}
cli_options <- function(args) {
  if (any(!grepl("^--[a-z-]+=.+" ,args))) stop("Options use --name=value")
  keys <- sub("^--([^=]+)=.*","\\1",args)
  ok <- c("output","profile","replicate","scenario","method","cores","seed",
          "checkpoint-every","chunk-size","resume")
  if (anyDuplicated(keys)||!all(keys%in%ok)) stop("Invalid or duplicate option")
  setNames(sub("^[^=]+=","",args),keys)
}
choose_names <- function(x,possible) {
  if (x=="all") return(possible)
  x <- strsplit(x,",",fixed=TRUE)[[1]]
  if (anyDuplicated(x)||!all(x%in%possible)) stop("Choose from ",paste(possible,collapse=", "))
  x
}
whole_number <- function(x, upper=1000000L) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value)!=1L||!is.finite(value)||value<1||value>upper||value!=floor(value))
    stop("Expected a positive integer no larger than ", upper)
  as.integer(value)
}
job_lock <- function(dir, timeout=0) {
  if(!dir.exists(dir)) dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  lock <- filelock::lock(file.path(dir,"worker.lock"),timeout=timeout)
  if(is.null(lock)) stop("Another worker owns ",dir)
  lock
}

experiment_runtime <- function() {
  packages <- c("bdynets", "BayesLogit", "Rcpp", "digest", "filelock")
  fingerprint <- function(pkg) {
    root <- find.package(pkg)
    files <- list.files(root,recursive=TRUE)
    files <- files[grepl("^(R/.*[.](rdb|rdx)|libs/.*[.](dll|so|dylib)|NAMESPACE)$",files)]
    if(!length(files)) stop("Cannot fingerprint the installed ",pkg," package")
    hashes <- vapply(file.path(root,files),function(path)
      digest::digest(file=path,algo="sha256"),character(1),USE.NAMES=FALSE)
    list(version=as.character(utils::packageVersion(pkg)),sha256=setNames(hashes,files))
  }
  list(R=R.version.string,platform=R.version$platform,math_libraries=extSoftVersion(),
    rng=RNGkind(),packages=setNames(lapply(packages,fingerprint),packages))
}

experiment_panel_seed <- function(case,profile,replicate,base_seed) {
  base_seed + replicate*211L + as.integer(substr(case,1,2))*193L +
    switch(profile,smoke=0L,pilot=400000000L,full=800000000L)
}

experiment_source_digest <- function(source_dir,files) {
  paths <- file.path(source_dir,files)
  setNames(vapply(paths,function(path) {
    if(!file.exists(path)) return(NA_character_)
    digest::digest(file=path,algo="sha256")
  },character(1),USE.NAMES=FALSE),files)
}

experiment_job_identity <- function(source_dir,case,spec,runtime) {
  common <- experiment_source_digest(source_dir,c("methods.R","workflow.R",file.path(case,"fit.R")))
  runtime_id <- digest::digest(runtime,algo="sha256")
  external <- new.env(parent=emptyenv())
  function(method,profile,method_seed,controls,output) {
    if (!method %in% spec$methods) stop("Unknown experiment method: ",method)
    if (is.null(external[[method]]) && method %in% c("sa-pgss","graph-wavenet")) {
      files <- c("external.R",if(method=="graph-wavenet") "graph_wavenet.py")
      external[[method]] <- list(files=experiment_source_digest(source_dir,files),
        python=if(method=="graph-wavenet") graph_wavenet_runtime())
    }
    key <- list(method=method,profile=profile,seed=method_seed,runtime=runtime_id,
      code=common,external=external[[method]],controls=controls,
      upstream=if(method=="graph-wavenet") graph_wavenet_model_identity(output))
    list(id=digest::digest(key,algo="sha256"),details=key)
  }
}

collect_experiment <- function(root,case,profile,spec,config,source_dir) {
  records <- list.files(root,pattern="^job[.]rds$",recursive=TRUE,full.names=TRUE)
  if (!length(records)) stop("Run method fits before collecting the experiment")
  jobs <- lapply(records,readRDS)
  jobs <- Filter(function(x) identical(x$profile,profile),jobs)
  if(!length(jobs)) stop("No jobs for this profile")
  panel_files <- list.files(root,pattern="^panel[.]rds$",recursive=TRUE,full.names=TRUE)
  info <- lapply(panel_files,function(path) {
    record <- readRDS(file.path(dirname(path),"panel-info.rds"))
    if(!identical(record$panel_md5,unname(tools::md5sum(path))))
      stop("Saved panel checksum changed: ",path)
    record
  })
  lookup <- setNames(lapply(info,"[[","panel_md5"),vapply(info,function(x)
    paste(x$scenario,x$replicate),character(1)))
  provenances <- list()
  generator_changed <- !identical(config$generator_id,unname(experiment_source_digest(source_dir,"designs.R")))
  for(j in jobs) {
    if (!identical(j$panel_md5,lookup[[paste(j$scenario,j$replicate)]]))
      stop("Job input differs from its generated panel: ",j$scenario," ",j$replicate)
    if (!identical(j$case,case) || !j$scenario %in% spec$scenarios)
      stop("Unknown job case or scenario: ", j$scenario)
    expected_seed <- experiment_panel_seed(case,profile,j$replicate,config$seed) +
      match(j$method,spec$methods)*311L
    if(!identical(expected_seed,j$method_seed) || is.null(j$execution_controls))
      stop("Invalid stored job controls or method seed: ",j$method," ",j$scenario)
    if(identical(j$status,"complete")) {
      method_dir <- file.path(root,j$scenario,sprintf("rep-%05d",j$replicate),j$method)
      id <- j$identity
      controls_match <- identical(id$controls,j$execution_controls) &&
        identical(id$seed,expected_seed) && identical(id$method,j$method) &&
        identical(id$profile,profile)
      if(!controls_match || !identical(id$runtime,digest::digest(config$runtime,algo="sha256")) ||
         !identical(j$fit_id,digest::digest(id,algo="sha256")) ||
         !identical(j$record_id,digest::digest(j[setdiff(names(j),"record_id")],algo="sha256")))
        stop("Saved job identity or result checksum differs: ",method_dir)
      if(length(j$artifact_hashes) && !identical(j$artifact_hashes,
          experiment_source_digest(method_dir,names(j$artifact_hashes))))
        stop("Scored artifact checksum differs: ",method_dir)
      # Scheduling and storage options are allowed to differ across workers.
      # Each replicate has its own seed. Every other part of a method's
      # scientific and software identity must match before scores are pooled.
      group <- id[setdiff(names(id),c("seed","controls"))]
      saved_source <- c(id$code,id$external$files)
      checkout_matches <- identical(saved_source,
        experiment_source_digest(source_dir,names(saved_source))) && !generator_changed
      provenances[[length(provenances)+1L]] <- data.frame(
        case=case,scenario=j$scenario,replicate=j$replicate,method=j$method,
        method_version=digest::digest(group,algo="sha256"),job_identity=j$fit_id,
        current_source_matches=checkout_matches)
    }
  }
  if(length(provenances)) {
    version <- do.call(rbind,provenances)
    mixed <- vapply(split(version$method_version,version$method),
                    function(x) length(unique(x))>1L,logical(1))
    if(any(mixed)) stop("Incompatible saved method versions across panels: ",
                        paste(names(mixed)[mixed],collapse=", "))
    write_table(version,file.path(root,"provenance.csv"))
    if(any(!version$current_source_matches))
      message("Collecting consistent archived fits; the current checkout differs. See provenance.csv.")
  }
  status <- do.call(rbind,lapply(jobs,function(j)
    data.frame(case=case,scenario=j$scenario,replicate=j$replicate,method=j$method,
      status=j$status,detail=j$detail,panel_md5=j$panel_md5,seed=j$seed,
      elapsed_seconds=j$elapsed,graph_error=j$graph_error)))
  # Compare all intended methods on each generated panel, including jobs which
  # the caller has not submitted. Report fit failures and omissions explicitly.
  panels <- Filter(function(x) identical(x$profile,profile),info)
  plan <- do.call(rbind,lapply(panels,function(x) data.frame(case=case,
    scenario=x$scenario,replicate=x$replicate,method=spec$methods)))
  status <- merge(plan,status,all.x=TRUE)
  status$status[is.na(status$status)] <- "pending"
  write_table(status,file.path(root,"jobs.csv"))
  write_table(status[status$status!="complete",,drop=FALSE],file.path(root,"failures.csv"))
  scores <- do.call(rbind,lapply(jobs,function(x) if(x$status=="complete") x$scores))
  if (is.null(scores)) scores <- data.frame(case=character(),scenario=character(),
    replicate=integer(),method=character(),window=character(),metric=character(),value=numeric(),cells=integer())
  write_table(scores,file.path(root,"comparison.csv"))
  if(case=="02-sparse-grid") {
    curves <- lapply(jobs,function(j) {
      if(j$status!="complete" || !j$method%in%c("gls","gls-flat")) return(NULL)
      path <- file.path(root,j$scenario,sprintf("rep-%05d",j$replicate),j$method,"validation.csv")
      d <- read.csv(path)
      data.frame(case=case,scenario=j$scenario,replicate=j$replicate,method=j$method,d)
    })
    curves <- do.call(rbind,curves)
    if(!is.null(curves)) write_table(curves,file.path(root,"support-path.csv"))
  }
  key <- c("case","scenario","replicate","window","metric")
  references <- unique(c(spec$ref, spec$methods[grepl("^gls", spec$methods)]))
  comparisons <- lapply(references, function(reference) {
    ref <- scores[scores$method==reference,c(key,"value"),drop=FALSE]
    names(ref)[ncol(ref)] <- "reference_value"
    pairs <- merge(scores,ref,by=key)
    pairs <- pairs[pairs$method!=reference,,drop=FALSE]
    pairs$difference <- pairs$value-pairs$reference_value
    pairs$reference <- rep(reference,nrow(pairs))
    pairs
  })
  pairs <- do.call(rbind,comparisons)
  write_table(pairs,file.path(root,"paired-replicates.csv"))
  cols <- c("case","scenario","method","window","metric","reference")
  if(nrow(pairs)) {
    groups <- split(seq_len(nrow(pairs)),interaction(pairs[cols],drop=TRUE))
    out <- do.call(rbind,lapply(groups,function(ix) {
      x <- pairs$difference[ix]
      valid <- is.finite(x)
      finite_mean <- if(any(valid)) mean(x[valid]) else NA_real_
      aggregate_mean <- if(anyNA(x)) NA_real_ else mean(x)
      data.frame(pairs[ix[1],cols,drop=FALSE],paired_n=length(x),
        finite_n=sum(valid), nonfinite_n=sum(!valid),
        difference=aggregate_mean, finite_mean=finite_mean,
        mcse=if(all(valid) && length(x)>1) sd(x)/sqrt(length(x)) else NA_real_)

    }))
  } else out <- data.frame(pairs[,cols,drop=FALSE],paired_n=integer(),finite_n=integer(),nonfinite_n=integer(),difference=numeric(),finite_mean=numeric(),mcse=numeric())
  write_table(out,file.path(root,"paired.csv"))
  message("Collected ",nrow(status)," expected jobs at ",root)
}

run_experiment <- function(case,action,args) {
  opt <- cli_options(args)
  get <- function(key,default) if(key%in%names(opt)) opt[[key]] else default
  profile <- match.arg(get("profile","smoke"),c("smoke","pilot","full"))
  output <- path.expand(get("output",""))
  if (!grepl("^(/|[A-Za-z]:[/\\\\])",output)) stop("--output must be an absolute result root")
  root <- file.path(output,case)
  dir.create(root,recursive=TRUE,showWarnings=FALSE)
  # Each root represents a single study configuration across replicates.
  base_seed <- whole_number(get("seed","200425"),50000000)
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if(!file.exists(script)) script <- gsub("~+~", " ", script, fixed=TRUE)
  source_dir <- dirname(dirname(normalizePath(script,mustWork=TRUE)))
  config_file <- file.path(root,"config.rds")
  spec <- experiment_catalog[[case]]
  if(action=="compare") {
    if(!file.exists(config_file)) stop("Generate and fit the experiment first.")
    saved <- readRDS(config_file)
    if(!identical(saved$case,case) || !identical(saved$profile,profile) ||
       ("seed" %in% names(opt) && !identical(saved$seed,base_seed)))
      stop("Requested case, profile or seed differs from the saved study")
    return(collect_experiment(root,case,profile,spec,saved,source_dir))
  }
  generator_id <- digest::digest(file=file.path(source_dir,"designs.R"),algo="sha256")
  runtime <- experiment_runtime()
  config <- list(case=case,profile=profile,seed=base_seed,generator_id=generator_id,
                 runtime=runtime)
  lock <- job_lock(root)
  if (file.exists(config_file)) {
    if(!identical(config,readRDS(config_file))) stop("Saved study uses different generator code, R runtime, packages, seed or profile")
  } else save_atomic(config,config_file)
  filelock::unlock(lock)
  signature <- experiment_job_identity(source_dir,case,spec,runtime)
  rep <- whole_number(get("replicate","1"))
  cores <- whole_number(get("cores","1"))
  chunk_size <- whole_number(get("chunk-size","100"))
  checkpoint_every <- whole_number(get("checkpoint-every","100"))
  resume <- match.arg(get("resume","true"),c("true","false"))=="true"
  scenarios <- choose_names(get("scenario","all"),spec$scenarios)
  methods <- choose_names(get("method","all"),spec$methods)
  # Distinct independent RNG streams for pilots and confirmatory panels.
  seed <- experiment_panel_seed(case,profile,rep,base_seed)
  controls <- list(cores=cores,chunk_size=chunk_size,checkpoint_every=checkpoint_every)

  failed <- character()
  for (scenario in scenarios) {
    dir <- file.path(root,scenario,sprintf("rep-%05d",rep))
    dir.create(dir,recursive=TRUE,showWarnings=FALSE)
    panel_path <- file.path(dir,"panel.rds")
    info_path <- file.path(dir,"panel-info.rds")
    if(action=="generate") {
      lock <- job_lock(dir)
      tryCatch({
        if(!file.exists(info_path)) {
          panel <- spec$generator(scenario,profile,seed)
          save_atomic(panel,panel_path)
          info <- list(case=case,scenario=scenario,replicate=rep,seed=seed,profile=profile,
            panel_md5=unname(tools::md5sum(panel_path)),R=R.version.string,
            package=as.character(utils::packageVersion("bdynets")))
          save_atomic(info,info_path)
        }
        if (!identical(readRDS(info_path)$panel_md5,unname(tools::md5sum(panel_path))))
          stop("Saved panel checksum changed")
      },finally=filelock::unlock(lock))
      next
    }
    if (!file.exists(info_path)) stop("Generate the shared panel first: ",dir)
    info <- readRDS(info_path)
    if(!identical(info$panel_md5,unname(tools::md5sum(panel_path)))) stop("Panel checksum changed")
    panel <- readRDS(panel_path)
    for(method in methods) {
      method_dir <- file.path(dir,method)
      lock <- job_lock(method_dir)
      tryCatch({
        path <- file.path(method_dir,"job.rds")
        method_seed <- seed+match(method,spec$methods)*311L
        identity <- signature(method,profile,method_seed,controls,method_dir)
        fit_id <- identity$id
        old <- if(file.exists(path)) readRDS(path)
        if(!is.null(old)){
          if(!identical(old$panel_md5,info$panel_md5))
            stop("Saved method job belongs to a different count panel: ",method_dir)
          # External fits restart from their seed following a dependency
          # failure. Incomplete MCMC fits keep their original checkpoint.
          retry_external <- identical(old$status,"failed") &&
            method %in% c("sa-pgss","graph-wavenet")
          if(!retry_external && (!identical(old$fit_id,fit_id) ||
               !identical(old$identity,identity$details)))
            stop("Saved method job differs from current settings, code or runtime: ",method_dir)
          if(identical(old$status,"complete")) next
        }
        status <- c(info,list(method=method,method_seed=method_seed,fit_id=fit_id,
          identity=identity$details,execution_controls=controls,
          command=commandArgs(),session=capture.output(sessionInfo()),cores=cores,
          graph_error=if(is.null(panel$graph_error)) NA_real_ else panel$graph_error))
        started <- proc.time()[["elapsed"]]
        status$status <- "running"; status$detail <- ""; status$elapsed <- 0
        save_atomic(status,path)
        ans <- tryCatch({
          fit_experiment_method(method,panel,profile,method_seed,method_dir,cores,
                                checkpoint_every,chunk_size,resume,fit_id)
        },error=function(e)e)
        status$elapsed <- proc.time()[["elapsed"]]-started
        if(inherits(ans,"error")) {
          status$status <- "failed"; status$detail <- conditionMessage(ans)
          failed <- c(failed,paste(scenario,method,status$detail))
        } else {
          status$status <- "complete"
          status$scores <- do.call(rbind,lapply(names(ans),function(label)
            data.frame(case=case,scenario=scenario,replicate=rep,method=label,ans[[label]])))
        }
        if(identical(status$status,"complete")) {
          artifacts <- if(case=="02-sparse-grid" && method%in%c("gls","gls-flat")) "validation.csv" else character()
          status$artifact_hashes <- experiment_source_digest(method_dir,artifacts)
          status$record_id <- digest::digest(status,algo="sha256")
        }
        save_atomic(status,path)
      },finally=filelock::unlock(lock))
    }
  }
  if(length(failed)) stop(paste(failed,collapse="\n"),call.=FALSE)
}
