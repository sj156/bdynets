# Both arms use the same saved panels. A plan requires only the study catalog.
study_cases <- c("01-graph-error", "02-sparse-grid", "03-surges")

study_replicates <- function(value, profile) {
  if (is.null(value)) return(if (profile == "full") seq_len(50L) else 1L)
  if (!grepl("^[0-9]+(:[0-9]+)?(,[0-9]+(:[0-9]+)?)*$",value))
    stop("--replicate accepts IDs from 1 to 50, for example 1:5 or 1,3")
  groups <- strsplit(value,",",fixed=TRUE)[[1]]
  ids <- unlist(lapply(groups,function(group) {
    bounds <- vapply(strsplit(group,":",fixed=TRUE)[[1]],whole_number,integer(1),upper=50L)
    if (length(bounds)==1L) return(bounds)
    if (bounds[2]<bounds[1]) stop("Replicate ranges must be increasing")
    seq.int(bounds[1],bounds[2])
  }),use.names=FALSE)
  if (anyDuplicated(ids)) stop("Replicate IDs must be unique")
  ids
}

# Each panel-info.rds is the authoritative record. Rebuild its sorted index
# under a lock so concurrent/resumed generation cannot drop a manifest row.
write_panel_manifest <- function(root,scenarios) {
  lock <- job_lock(file.path(root,".manifest-lock"),timeout=Inf)
  on.exit(filelock::unlock(lock))
  paths <- panel_records(root,scenarios)
  entries <- lapply(paths,function(path) {
    info <- readRDS(path)
    relative <- file.path(info$scenario,sprintf("rep-%05d",info$replicate))
    if (!identical(dirname(path),file.path(root,relative)))
      stop("Panel identity does not match its path: ",path)
    data.frame(case=info$case,profile=info$profile,scenario=info$scenario,
      replicate=info$replicate,seed=info$seed,path=file.path(relative,"panel.rds"),
      panel_md5=info$panel_md5)
  })
  manifest <- do.call(rbind,entries)
  write_table(manifest[order(manifest$scenario,manifest$replicate),],
              file.path(root,"panels.csv"))
}

study_arm_cli <- function(study, arm, entry, args) {
  root <- dirname(dirname(entry))
  shared <- file.path(root, "experiments")
  for (file in c("designs.R", "methods.R", "external.R", "workflow.R"))
    source(file.path(shared,file), local = environment(study_arm_cli))
  action_option <- startsWith(args, "--action=")
  if (sum(action_option) > 1L) stop("Supply --action once")
  action <- if (any(action_option)) sub("^--action=", "", args[action_option]) else "plan"
  actions <- c(plan = "plan", data = "generate", run = "fit", report = "report")
  if (!action %in% names(actions)) stop("Choose --action=plan, data, run or report")
  run_study(shared, actions[[action]], args[!action_option], study, arm, entry)
}

run_study <- function(source_dir, action, args, study=NULL, arm=NULL, entry=NULL) {
  study_arg <- startsWith(args,"--study=")
  if (is.null(study)) {
    if (sum(study_arg)!=1L) stop("Specify --study=1, --study=2 or --study=3")
    study <- whole_number(sub("^--study=","",args[study_arg]),3L)
    args <- args[!study_arg]
  }
  opt <- cli_options(args)
  get <- function(key, default) if (key %in% names(opt)) opt[[key]] else default
  profile <- match.arg(get("profile", "full"), c("full","pilot","smoke"))
  case <- study_cases[[study]]
  spec <- experiment_catalog[[case]]
  replicates <- study_replicates(get("replicate", NULL), profile)
  scenarios <- choose_names(get("scenario","all"),spec$scenarios)
  fit_sources <- character()
  if (action %in% c("plan","fit")) {
    selected <- choose_names(get("method","all"), spec$arms[[arm]])
    if (action == "plan") {
      cat("Study ",study," / ",arm," / ",profile,"\n",
          "Conditions: ",paste(scenarios,collapse=", "),"\n",
          "Replicate IDs: ",paste(replicates,collapse=", "),"\n",
          "Methods: ",paste(selected,collapse=", "),"\n",
          "Shared panels: ",length(scenarios) * length(replicates),"\n",
          "Method jobs: ",length(scenarios) * length(replicates) * length(selected),"\n",sep="")
      data_root <- get("data-root", get("output", NULL))
      if (!is.null(data_root))
        cat("Panel directory: ", file.path(path.expand(data_root),case), "\n", sep = "")
      if ("output" %in% names(opt))
        cat("Fit directory: ", file.path(path.expand(opt[["output"]]),case), "\n", sep = "")
      return(invisible(NULL))
    }
    fit_sources <- c("study.R", file.path("..","tools",basename(entry)))
    args <- c(args[!startsWith(args,"--method=")], paste0("--method=",paste(selected,collapse=",")))
  }
  if (!"output" %in% names(opt)) stop("Supply --output as an absolute results directory")
  if (!"profile" %in% names(opt)) args <- c(args,paste0("--profile=",profile))
  if (action %in% c("compare","report")) {
    if(any(names(opt)%in%c("scenario","replicate","method")))
      stop("The report collects the entire study; omit --scenario, --replicate and --method")
    planned <- if(profile=="full") expand.grid(scenario=spec$scenarios,
      replicate=seq_len(50L),stringsAsFactors=FALSE)
    if (action=="report") {
      results <- path.expand(opt[["output"]])
      case_root <- file.path(results,case)
      if (!file.exists(file.path(case_root,"config.rds"))) stop("No saved study to report: ",case_root)
      # Both public arms produce the same joint report.
      lock <- job_lock(file.path(case_root,".report-lock"),timeout=Inf)
      on.exit(filelock::unlock(lock))
    }
    run_experiment(case,"compare",args,source_dir=source_dir,planned=planned)
    if (action=="report") {
      renderer <- file.path(dirname(source_dir),"reports","render.R")
      paths <- c(renderer,case,file.path(results,case),file.path(results,"reports"))
      status <- system2(file.path(R.home("bin"), "Rscript"),shQuote(paths))
      if (status != 0L) stop("Joint report rendering failed for ",case)
      message("Joint report: ",file.path(results,"reports",paste0(case,".html")))
    }
    return(invisible(NULL))
  }
  if (action=="generate" && "method"%in%names(opt)) stop("The shared generator takes no --method")
  args <- args[!startsWith(args,"--replicate=")]
  runtime <- experiment_runtime()
  for (replicate in replicates) {
    message("Study ",study," / ",if(action=="fit") arm else action,
            " / replicate ",replicate)
    run_experiment(case,action,c(args,paste0("--replicate=",replicate)),
                   source_dir=source_dir,runtime=runtime,fit_sources=fit_sources)
  }
  if(action=="generate") write_panel_manifest(file.path(path.expand(get("data-root",opt[["output"]])),case),spec$scenarios)
}
