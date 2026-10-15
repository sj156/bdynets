# Exact selected definitions from R/graphmode_c9_design.R
graphmode_c9_version <- "graphmode-direct-road-a10-20260929-v3"

graphmode_c9_spec <- function() {
    base <- graphmode_c8_spec()
    base$version <- graphmode_c9_version
    base$run_authorized <- FALSE
    base$repetitions <- 10L
    base$amplitudes <- .10
    base$scenarios <- c("length", "noisy")
    base$geometry <- list(sizes=c(30L,20L,21L,20L,30L),
        boundary_gap_factors=c(2,4,3,1.3), within_log_jitter=.25,
        fixed_support="physical direct adjacency only; one predecessor and one successor",
        weight_error_log_sd=.5, cost="continuous-road arc length; no travel time")
    base$bayes$iterations <- 2000L
    base$bayes$warmup <- 1000L
    base$potts <- list(beta=c(.25,1,4), origins=c(96L,120L), chains=c(1L,3L),
        iterations=600L, warmup=300L, selection="RMSE of pooled-chain predictive mean; lower beta on numerical ties")
    base$storage <- list(draw_chunk=100L, cumulative_checkpoints=FALSE,
        keep_all_failures=TRUE, gzip=TRUE, planned_cap_gib=64, min_free_gib=12,
        start_free_gib=76)
    base$execution <- list(max_workers=6L, numeric_threads=1L,
        worker_rss_cap_bytes=12*1024^3, monitor_interval_seconds=2,
        disk_check_interval_seconds=30)
    base$budget <- list(bayes_worker_seconds=6000L, potts_validation_worker_seconds=1800L,
        neural_fit_seconds=600L, pnar_fit_seconds=300L, summary_seconds_per_pair=7200L,
        total_cap_hours=336, retry=FALSE, auto_extend=FALSE,
        note="Caps, not runtime estimates; ten paired repetitions on the user's Mac")
    base$scope <- paste("Ten independent geometry/count replicates at amplitude0.10, paired across length/noisy inputs.",
        "Equal-edge GP equivalence is a fixed-input check, not a third dataset.",
        "Incomplete classification is not a code acceptance failure.",
        "Dynamic drift and travel time are deferred; no W-not-worse requirement.")
    base
}

graphmode_c9_road <- function(uniforms=rep(.5,121L)) {
    if(length(uniforms)!=121L || any(!is.finite(uniforms) | uniforms<=0 | uniforms>=1))
        stop("Supply 121 open-unit uniforms; this constructor never draws them.")
    spec <- graphmode_c9_spec(); old <- graphmode_c8_road("regular")
    bounds <- old$boundaries; sizes <- spec$geometry$sizes
    step <- diff(bounds)/sizes
    gaps <- spec$geometry$boundary_gap_factors * (head(step,-1L)+tail(step,-1L))/2
    left <- c(step[1]/2,gaps/2); right <- c(gaps/2,tail(step,1)/2)
    positions <- numeric(); offset <- 0L
    for(k in seq_along(sizes)) {
        n <- sizes[k]; j <- seq_len(n-1L)
        # Several dense/sparse groups per region, plus independent geometry jitter.
        widths <- exp(.6*sin(2*pi*j/6)+.3*sin(2*pi*j/13)+
            spec$geometry$within_log_jitter*(2*uniforms[offset+j]-1))
        lo <- bounds[k]+left[k]; hi <- bounds[k+1L]-right[k]
        if(hi<=lo) stop("Road-region margins exhausted the available arc length.")
        positions <- c(positions,lo+c(0,cumsum(widths)/sum(widths))*(hi-lo))
        offset <- offset+n
    }
    # Map arc positions onto the same continuous two-arm spiral, including its
    # straight central connections. Arc lengths are analytic, not chord lengths.
    slope <- 5/(8*pi); angle <- seq(0,4*pi,length.out=8001L)
    radius <- .5+slope*angle
    primitive <- (radius*sqrt(radius^2+slope^2)+slope^2*asinh(radius/slope))/(2*slope)
    arm <- cbind(radius*cos(angle),radius*sin(angle))
    path <- rbind(arm[nrow(arm):1,],c(0,0),-arm)
    arc <- c(0,cumsum(c(rev(diff(primitive)),.5,.5,diff(primitive))))
    xy <- cbind(approx(arc,path[,1],xout=positions)$y,approx(arc,path[,2],xout=positions)$y)
    colnames(xy) <- c("x","y")
    list(version=graphmode_c9_version,ids=seq_len(121L),coordinates=xy,s=positions,
        road_distance=abs(outer(positions,positions,"-")),
        physical_edges=data.frame(from=1:120,to=2:121,length=diff(positions)),
        partition=rep(seq_len(5L),sizes),boundaries=bounds,path=path,path_s=arc,
        note="Truth-correlated geometry is an explicit DGP assumption; fitting never receives labels.")
}

graphmode_c9_config <- function(road,Y,method,weight=NULL,beta=1,unit_order=seq_len(nrow(Y))) {
    method <- match.arg(method,graphmode4_methods); n <- nrow(Y)
    if(n!=nrow(road$coordinates) || ncol(Y)>168L ||
       !identical(sort(as.integer(unit_order)),seq_len(n))) stop("Invalid training alignment.")
    s <- graphmode_c9_spec(); context <- NULL
    if(method %in% c("graphMoDE-W","graphMoDE-C","PottsMoDE")) {
        graphmode_validate_weight(weight)
        if(nrow(weight)!=n) stop("Wrong graph dimensions.")
        weight <- weight[unit_order,unit_order,drop=FALSE]
    }
    if(method %in% c("graphMoDE-W","graphMoDE-C"))
        context <- graphmode_c8_kernel_context("graph",if(method=="graphMoDE-W") weight else 1*(weight>0))
    if(method=="EucMoDE") context <- graphmode_c8_kernel_context("euclidean",
        road$coordinates[unit_order,,drop=FALSE],road$ids[unit_order])
    if(method=="PottsMoDE" && !beta %in% s$potts$beta) stop("Unregistered Potts candidate.")
    time <- seq_len(ncol(Y)); q0 <- s$prior$q_shape/s$prior$q_rate
    core <- graphmode_config(Y,cbind(1,sin(2*pi*time/24),cos(2*pi*time/24)),
        m0=c(log(max(.1,mean(Y))),0,0),C0=diag(c(1,.5,.5)),method=method,
        K=10L,G=diag(3),W=diag(q0,3),
        Phi=if(is.null(context)) NULL else graphmode_c8_kernel(context,context$log_center),
        graph_weight=if(method=="PottsMoDE") weight else NULL,
        potts_beta=if(method=="PottsMoDE") beta else NULL,
        rho=s$bayes$rho,guidance_proposal_sd=s$bayes$guidance_proposal_sd,
        dirichlet_alpha=if(method=="MoDE") rep(.1,10) else NULL)
    # The unchanged C8 transition and state schema are reusable mathematics;
    # C9 execution receipts must bind the new design, fit hash and run identity.
    list(version=graphmode_c8_version,design_version=graphmode_c9_version,
        core=core,kernel=context,prior=s$prior,
        scale_proposal_sd=s$bayes$scale_proposal_sd,log_q_proposal_sd=s$bayes$log_q_proposal_sd,
        policy=list(gate=graphmode_gate_refresh_policy(s$bayes$gate_scans,s$bayes$guidance_proposal_sd),
            block=graphmode_expert_blocks_policy(42L,"uniform"),ess_scheme="contrast",factor_cache=TRUE))
}
