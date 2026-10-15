# Synthetic twelve-node example; tiny chain is an execution demo, not inference.
out <- a[2];if(file.exists(out)) stop('Use an absent example output directory.')
dir.create(out)
n <- 12L;TT <- 48L;H <- 48L
adj <- 1*(abs(outer(1:n,1:n,'-'))==1)
lengths <- adj;lengths[6,7] <- lengths[7,6] <- 2
F <- cbind(1,sin(2*pi*(1:(TT+H))/24),cos(2*pi*(1:(TT+H))/24))
set.seed(2031103501L);Y <- matrix(rpois(n*TT,25),n,TT)
input <- api$graphmode_input(Y,adj,lengths,F[1:TT,],F[TT+1:H,])
saveRDS(input,file.path(out,'input.rds'))
for(m in c('Exp-W','SE-W','C')) {
    fit <- api$graphmode_make_fit(input,m)
    init <- list(start_occupancy=c(1L,3L,7L,10L),seeds=setNames(2031103511:2031103514,c('Z_01','v_01','x_01','theta_01')))
    raw <- api$graphmode_warmup_initial(fit$core,1L,init)
    state <- if(m=='C') api$graphmode_c8_common_start(fit,raw$state,1L) else api$graphmode_h_common_start(fit,raw$state,1L)
    step <- if(m=='C') api$graphmode_c8_step else api$graphmode_h_step
    check <- if(m=='C') api$graphmode_c8_check_transition else api$graphmode_h_check_transition
    set.seed(2031103520L+match(m,c('Exp-W','SE-W','C')));draws <- list()
    for(i in 1:8) {
        next_state <- step(state,fit);check(state,next_state,fit,list(base_policy=fit$policy,thresholds=api$graphmode_c9_spec()$thresholds))
        state <- next_state$state;draws[[i]] <- state
    }
    saveRDS(list(fit=fit,draws=draws),file.path(out,paste0(m,'.rds')))
    cat(m,'8 steps completed; demonstration only\n')
}
