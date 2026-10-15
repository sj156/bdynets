count <- 0L
check <- function(name,expr) {force(expr);count <<- count+1L;cat('PASS',name,'\n')}
fails <- function(expr) stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
p <- api$graphmode_plan()
check('frozen plan and unique role seeds',stopifnot(nrow(p$jobs)==24L,length(p$seeds)==64L,!anyDuplicated(p$seeds),
    all(table(p$jobs$dataset,p$jobs$method)==4L),p$warmup==2000L,p$initial_iterations==4000L,p$max_iterations==8000L,p$max_workers==6L))
road <- api$graphmode_c9_road()
check('121 nodes, 120 physical edges and approved gaps',{
    stopifnot(nrow(road$coordinates)==121,nrow(road$physical_edges)==120,identical(tabulate(road$partition),c(30L,20L,21L,20L,30L)))
    old <- api$graphmode_c8_road('regular');step <- diff(old$boundaries)/c(30,20,21,20,30)
    stopifnot(max(abs(road$physical_edges$length[c(30,50,71,91)]-c(3,4,3,2)*(head(step,-1)+tail(step,-1))/2))<1e-12)
})
check('exact prescribed mean 25 and amplitude .05',{
    profiles <- api$graphmode_c8_profiles(.05)
    stopifnot(max(abs(rowMeans(profiles$lambda[,1:24])-25))<1e-12,
        max(abs(sqrt(rowSums(profiles$theta[,2:3]^2))-.05))<1e-15)
})
A <- 1*(abs(outer(1:121,1:121,'-'))==1);L <- A*road$road_distance
F <- api$graphmode_c8_profiles(.05)$Fmat
input <- api$graphmode_input(matrix(25,121,168),A,L,F[1:168,],F[169:216,])
fits <- lapply(c('Exp-W','SE-W','C'),function(m) api$graphmode_make_fit(input,m))
check('overfit 10, W common prior and C self calibration; full rank and support',{
    stopifnot(all(vapply(fits,function(f) f$core$K==10L,TRUE)),identical(fits[[1]]$kernel$log_center,fits[[2]]$kernel$log_center),
        abs(exp(fits[[3]]$kernel$log_center)-2)<1e-8)
    for(f in fits) stopifnot(qr(f$core$Phi)$rank==121,f$prior$q_shape==2,f$prior$q_rate==20000)
    for(f in fits[1:2]) for(z in c(-.5,0,.5)) {
        W <- api$graphmode_h_weight(f$bandwidth$geometry,f$bandwidth$family,z)
        stopifnot(sum(W>0)==240,abs(mean(W[W>0])-1)<1e-12)
    }
})
check('custom input rejects transposition, off-support lengths and negative counts',{
    fails(api$graphmode_input(t(input$Y),A,L,input$Fmat,input$Ffuture))
    bad <- L;bad[1,3] <- bad[3,1] <- 1;fails(api$graphmode_input(input$Y,A,bad,input$Fmat,input$Ffuture))
    bad <- input$Y;bad[1,1] <- -1;fails(api$graphmode_input(bad,A,L,input$Fmat,input$Ffuture))
})
check('bandwidth prior Jacobian and covariance',{
    f <- fits[[1]];z <- c(-.7,.2,.9)
    stopifnot(max(abs(diff(vapply(z,api$graphmode_h_logprior,0,bandwidth=f$bandwidth))-diff(dgamma(exp(z),4,4,log=TRUE)+z)))<1e-12)
    W <- api$graphmode_h_weight(f$bandwidth$geometry,'exp',.2);lap <- diag(rowSums(W))-W
    expected <- solve(diag(2/exp(.4)^2,121)+lap);expected <- expected*121/sum(diag(expected))
    actual <- tcrossprod(api$graphmode_h_root(api$graphmode_h_spectrum(f,.2),.4))
    stopifnot(max(abs(actual-expected))<1e-10)
})
check('monotonic clock detects backward time without changing RNG',{
    set.seed(17);seed <- .Random.seed;t <- 0
    now <- function() {t <<- t+1;c(100,200,150)[t]}
    clock <- api$graphmode_q1_clock(now);stopifnot(clock()[3]>0);fails(clock());stopifnot(identical(seed,.Random.seed))
})
# All three methods: deterministic tapes exercise the unchanged full transition.
fixed <- graphmode_load(root,FALSE)
fixed$graphmode_normal <- function(n) .05*sin(seq_len(n))
fixed$graphmode_uniform <- function(n) rep(.37,n)
fixed$graphmode_gamma <- function(shape,rate) shape/rate
fixed$graphmode_pg <- function(b,z) rep(.4,length(b))
small <- fixed$graphmode_input(matrix(25,12,42),A[1:12,1:12],L[1:12,1:12],F[1:42,],F[43:90,])
check('complete fixed-tape transitions for all three methods',{
    for(m in c('Exp-W','SE-W','C')) {
        fit <- fixed$graphmode_make_fit(small,m)
        raw <- fixed$graphmode_initial_state(fit$core,rep(1L,12))
        before <- if(m=='C') fixed$graphmode_c8_common_start(fit,raw,2L) else fixed$graphmode_h_common_start(fit,raw,2L)
        step <- if(m=='C') fixed$graphmode_c8_step else fixed$graphmode_h_step
        validate <- if(m=='C') fixed$graphmode_c8_check_transition else fixed$graphmode_h_check_transition
        after <- step(before,fit);validate(before,after,fit,list(base_policy=fit$policy,thresholds=p$thresholds))
        stopifnot(after$state$core$iteration==1L)
    }
})
cat(count,'test groups passed.\n')
check('checkpoint continuation preserves exact state/RNG and warmup boundary',{
    tmp <- tempfile('checkpoint-test-');dir.create(tmp);dir.create(file.path(tmp,'jobs'));dir.create(file.path(tmp,'D01'))
    plan <- api$graphmode_plan(TRUE);plan$initial_iterations <- 2L;plan$max_iterations <- 4L;plan$warmup <- 1L;plan$chunk <- 1L
    plan$jobs <- data.frame(id='fixture',dataset='D01',method='C',chain=1L,seed=12345L)
    r <- list(plan=plan,source_sha256=api$graphmode_identity(root));r$signature <- api$graphmode_digest(r)
    api$graphmode_store(r,file.path(tmp,'registration.rds'))
    api$graphmode_store(list(fits=list(C=fit),starts=list(C=list(before))),file.path(tmp,'D01','blind.rds'))
    fake <- function(state,fit) {state$core$iteration <- state$core$iteration+1L;state$log_q <- state$log_q+runif(1);list(state=state,base=list())}
    set.seed(12345L);expected <- before;for(j in 1:4) expected <- fake(expected,fit)$state;expected_rng <- .Random.seed
    api$graphmode_worker(tmp,1L,2L,fake,function(...) TRUE)
    initial_receipt <- api$graphmode_c8_hash(file.path(tmp,'jobs','fixture','receipt-2.rds'))
    api$graphmode_worker(tmp,1L,4L,fake,function(...) TRUE)
    last <- api$graphmode_read(file.path(tmp,'jobs','fixture','chunk-00004.rds'))
    stopifnot(identical(last$state,expected),identical(last$rng,expected_rng),
        api$graphmode_c8_hash(file.path(tmp,'jobs','fixture','receipt-2.rds'))==initial_receipt)
    chain <- api$graphmode_chain(tmp,plan$jobs,r,4L,fit)
    stopifnot(identical(vapply(chain$draws,function(x) x$core$iteration,1L),2:4))
    # Tampering must be rejected, and the test's temporary files are disposable.
    cat('corrupt',file=file.path(tmp,'jobs','fixture','chunk-00004.rds'),append=TRUE)
    fails(api$graphmode_chain(tmp,plan$jobs,r,4L,fit));unlink(tmp,recursive=TRUE)
})
cat(count,'total test groups passed.\n')
check('complete diagnostic/report path retains failures and applies frozen extension/stop',{
    tmp <- tempfile('summary-test-');dir.create(tmp)
    e <- graphmode_load(root,FALSE)
    e$graphmode_guard <- function(...) TRUE
    e$graphmode_normal <- function(n) .05*sin(seq_len(n))
    e$graphmode_uniform <- function(n) rep(.37,n)
    e$graphmode_gamma <- function(shape,rate) shape/rate
    model_fits <- setNames(lapply(c('Exp-W','SE-W','C'),function(m) e$graphmode_make_fit(input,m)),c('Exp-W','SE-W','C'))
    r <- list(plan=e$graphmode_plan(),signature='FIXTURE')
    e$graphmode_store(r,file.path(tmp,'registration.rds'))
    for(d in c('D01','D02')) {
        dir.create(file.path(tmp,d))
        e$graphmode_store(list(input=input,fits=model_fits),file.path(tmp,d,'blind.rds'))
        e$graphmode_store(list(Y=matrix(25,121,48),Z=road$partition),file.path(tmp,d,'evaluation-only.rds'))
    }
    e$graphmode_chain <- function(directory,job,r,target,fit) {
        raw <- e$graphmode_initial_state(fit$core,rep(1L,121))
        s <- if(is.null(fit$bandwidth)) e$graphmode_c8_common_start(fit,raw,1L) else e$graphmode_h_common_start(fit,raw,1L)
        list(draws=rep(list(s),4))
    }
    for(target in c(4000,8000)) e$graphmode_summary(tmp,target)
    first <- jsonlite::read_json(file.path(tmp,'summary-4000','decision.json'))
    last <- jsonlite::read_json(file.path(tmp,'summary-8000','decision.json'))
    rows <- read.csv(file.path(tmp,'summary-8000','fits.csv'))
    pairs <- read.csv(file.path(tmp,'summary-8000','paired.csv'))
    stopifnot(!first$all_passed,grepl('extend',first$decision),!last$all_passed,grepl('STOP',last$decision),
        nrow(rows)==6L,nrow(pairs)==6L,all(!rows$valid),all(rows$boundary_errors==4),all(rows$internal_cuts==0),
        all(rows$boundary_merge_probability==1),all(rows$internal_cut_probability==0))
    unlink(tmp,recursive=TRUE)
})
cat(count,'final test groups passed.\n')
