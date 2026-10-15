# Public portable API. Existing scientific transitions are loaded unchanged.
graphmode_load <- function(root, monotonic=TRUE) {
    root <- normalizePath(root,mustWork=TRUE)
    e <- new.env(parent=globalenv())
    for(f in sort(list.files(file.path(root,'R'),'^core_.*[.]R$',full.names=TRUE))) sys.source(f,e)
    for(f in c('api.R','execution.R')) sys.source(file.path(root,'R',f),e)
    e$source_root <- root
    original_spec <- e$graphmode_c9_spec
    e$graphmode_c9_spec <- local({base <- original_spec;function() {
        s <- base();s$geometry$boundary_gap_factors <- c(3,4,3,2)
        s$amplitudes <- .05;s$repetitions <- 2L;s$bayes$iterations <- 4000L;s$bayes$warmup <- 2000L;s
    }})
    if(monotonic) e$proc.time <- e$graphmode_q1_clock()
    e
}

graphmode_input <- function(Y, adjacency, edge_lengths, Fmat, Ffuture, ids=seq_len(nrow(Y))) {
    graphmode_matrix(Y,'Y');graphmode_matrix(Fmat,'Fmat');graphmode_matrix(Ffuture,'Ffuture')
    n <- nrow(Y);p <- ncol(Fmat)
    if(n<10L || ncol(Y)!=nrow(Fmat) || ncol(Ffuture)!=p || p<1L ||
       any(Y<0 | Y!=round(Y))) stop('Require n>=10, integer counts Y[n,T], Fmat[T,p], Ffuture[H,p].')
    graphmode_validate_weight(adjacency)
    if(!identical(dim(adjacency),c(n,n)) || any(!adjacency %in% c(0,1))) stop('Adjacency must be n by n, symmetric binary with zero diagonal.')
    graphmode_matrix(edge_lengths,'edge_lengths')
    if(!identical(dim(edge_lengths),c(n,n)) || !isTRUE(all.equal(edge_lengths,t(edge_lengths))) ||
       any(diag(edge_lengths)!=0) || any(edge_lengths[adjacency==1]<=0) ||
       any(edge_lengths[adjacency==0]!=0)) stop('Lengths must be symmetric, positive on every physical edge and zero off support.')
    if(!sum(adjacency)) stop('Require physical edges.')
    if(length(ids)!=n || anyNA(ids) || anyDuplicated(ids)) stop('Unique aligned IDs required.')
    list(Y=Y,adjacency=adjacency,edge_lengths=edge_lengths,Fmat=Fmat,Ffuture=Ffuture,ids=ids)
}

graphmode_make_fit <- function(input, method=c('Exp-W','SE-W','C'), m0=NULL, C0=NULL) {
    method <- match.arg(method);x <- do.call(graphmode_input,input);n <- nrow(x$Y);p <- ncol(x$Fmat)
    s <- graphmode_c9_spec();ij <- which(upper.tri(x$adjacency)&x$adjacency==1,arr.ind=TRUE)
    geometry <- list(support='direct',ids=x$ids,distance=x$edge_lengths,selected=ij,edges=ij,
        factor=rep(1,nrow(ij)),d_ref=median(x$edge_lengths[ij]),normalization='positive-mean-one')
    reference <- graphmode_h_weight(geometry,'exp',0)
    ref <- graphmode_c8_kernel_context('graph',reference)
    context <- if(method=='C') graphmode_c8_kernel_context('graph',x$adjacency) else ref
    # Only the W families share the fixed Exp-reference center.
    # C retains calibration on its own binary adjacency (median edge correlation .5).
    if(method != 'C') context$log_center <- ref$log_center
    if(is.null(m0)) m0 <- c(log(max(.1,mean(x$Y))),rep(0,p-1L))
    if(is.null(C0)) C0 <- diag(c(1,rep(.5,p-1L)),p)
    core <- graphmode_config(x$Y,x$Fmat,m0,C0,method=if(method=='C') 'graphMoDE-C' else 'graphMoDE-W',
        K=10L,G=diag(p),W=diag(s$prior$q_shape/s$prior$q_rate,p),
        Phi=graphmode_c8_kernel(context,context$log_center),rho=s$bayes$rho,
        guidance_proposal_sd=s$bayes$guidance_proposal_sd)
    fit <- list(version=graphmode_c8_version,core=core,kernel=context,prior=s$prior,
        scale_proposal_sd=s$bayes$scale_proposal_sd,log_q_proposal_sd=s$bayes$log_q_proposal_sd,
        policy=graphmode_c9_base_policy())
    if(method!='C') {
        fit$version <- graphmode_h_version
        fit$bandwidth <- list(family=if(method=='Exp-W') 'exp' else 'se',geometry=geometry,
            shape=4,rate=4,proposal_sd=.15,start_log_offsets=c(-1,-.3,.3,1))
        fit$core$Phi <- graphmode_h_root(graphmode_h_spectrum(fit,0),fit$kernel$log_center)
    }
    graphmode_revalidate_config(fit$core);fit
}

graphmode_plan <- function(smoke=FALSE) {
    seed <- if(smoke) 2031102500L else 2031101500L
    seeds <- integer();jobs <- list()
    add <- function(key) {seed <<- seed+1L;seeds[key] <<- seed;seed}
    for(d in 1:2) {
        group <- sprintf('D%02d',d)
        for(k in c('geometry','profile','units','counts')) add(paste(group,k,sep='-'))
        for(k in c('Z','v','x','theta')) for(j in 1:4) add(sprintf('%s-%s_%02d',group,k,j))
        for(j in 1:4) for(m in c('Exp-W','SE-W','C')) {
            id <- sprintf('%s-%s-chain%d',group,m,j)
            jobs[[length(jobs)+1L]] <- data.frame(id=id,dataset=group,method=m,chain=j,seed=add(id))
        }
    }
    list(version='graphmode-core-20261015-v1',role=if(smoke) 'entry-smoke-only' else 'two-data core pilot; not formal comparison',
        smoke=smoke,seeds=seeds,jobs=do.call(rbind,jobs),boundary_gap_factors=c(3,4,3,2),
        amplitude=.05,period_mean=25,train=168L,horizon=48L,n=121L,Kfit=10L,Ktrue=5L,
        initial_iterations=if(smoke) 4L else 4000L,max_iterations=if(smoke) 4L else 4000L,
        warmup=if(smoke) 2L else 2000L,chunk=if(smoke) 2L else 100L,
        extension='Stop at4000. Extension to8000 requires a new explicit decision; never retry or change seeds.',
        thresholds=graphmode_c9_spec()$thresholds,base_policy=graphmode_c9_base_policy(),
        max_workers=6L,numeric_threads=1L,min_free_gib=12,output_cap_gib=30,
        kappa_reference='W: common fixed Exp-direct at h=d_ref; C: its own binary-adjacency calibration',
        statistical_thresholds=list(rank_rhat=1.01,folded_rhat=1.01,bulk_ess=400,tail_ess=400,psm_rms=.05))
}

graphmode_generate <- function(plan,directory) {
    for(group in sprintf('D%02d',1:2)) {
        seeded <- function(k,expr) graphmode_pilot_seeded(plan$seeds[[paste(group,k,sep='-')]],expr)
        road <- graphmode_c9_road(seeded('geometry',graphmode_uniform(121L)))
        profiles <- seeded('profile',sample.int(5));units <- seeded('units',sample.int(121))
        uniforms <- seeded('counts',matrix(graphmode_uniform(121L*216L),121L,216L))
        dat <- graphmode_c8_panel_from_uniforms(.05,uniforms,profiles,units)
        A <- matrix(0,121,121);A[cbind(1:120,2:121)] <- 1;A <- A+t(A)
        lengths <- road$road_distance*A
        input <- graphmode_input(dat$training,A[units,units],lengths[units,units],
            dat$Fmat[1:168,],dat$Fmat[169:216,],road$ids[units])
        fits <- setNames(lapply(c('Exp-W','SE-W','C'),function(m) graphmode_make_fit(input,m)),c('Exp-W','SE-W','C'))
        keys <- unlist(lapply(c('Z','v','x','theta'),function(k) sprintf('%s_%02d',k,1:4)))
        init <- list(start_occupancy=c(1L,3L,7L,10L),seeds=setNames(plan$seeds[paste(group,keys,sep='-')],keys))
        raw <- lapply(1:4,function(j) graphmode_warmup_initial(fits[[1]]$core,j,init))
        graphmode_warmup_starts_check(raw,fits[[1]],init)
        starts <- lapply(fits,function(f) lapply(1:4,function(j) if(is.null(f$bandwidth))
            graphmode_c8_common_start(f,raw[[j]]$state,j) else graphmode_h_common_start(f,raw[[j]]$state,j)))
        path <- file.path(directory,group);dir.create(path)
        graphmode_store(list(input=input,fits=fits,starts=starts),file.path(path,'blind.rds'))
        truth <- dat$evaluation_only;truth$ids <- input$ids
        graphmode_store(truth,file.path(path,'evaluation-only.rds'))
        graphmode_store(list(road=road,profiles=profiles,units=units),file.path(path,'design.rds'))
    }
}
