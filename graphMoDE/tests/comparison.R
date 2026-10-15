# Lightweight closure test, separate from registered scientific RNG streams.
entry <- sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
root <- normalizePath(file.path(dirname(entry),'..'))
source(file.path(root,'R/api.R'));a <- graphmode_load(root)
n <- 12L;TT <- 48L;t <- seq_len(TT);future <- TT+seq_len(48L)
A <- matrix(0,n,n);A[cbind(1:11,2:12)] <- 1;A <- A+t(A)
Y <- matrix(20L+rep(0:6,length.out=n*TT),n,TT)
x <- a$graphmode_input(Y,A,A,cbind(1,sin(2*pi*t/24),cos(2*pi*t/24)),cbind(1,sin(2*pi*future/24),cos(2*pi*future/24)))
xy <- cbind(seq_len(n),sin(seq_len(n)))
record <- list(base_policy=a$graphmode_c9_base_policy(),thresholds=a$graphmode_c9_spec()$thresholds)
for(m in c('EucMoDE','MoDE','PottsMoDE')) {
    fit <- a$graphmode_extra_fit(x,xy,m)
    set.seed(998003L)
    initial <- a$graphmode_c8_initial(fit,rep(1:3,length.out=n))
    out <- a$graphmode_c8_step(initial,fit)
    a$graphmode_c8_check_transition(initial,out,fit,record)
    stopifnot(out$state$core$iteration==1L,length(unique(out$state$core$Z))<=10L)
}
stopifnot(identical(a$graphmode_c9_spec()$potts$beta,c(.25,1,4)))
Y <- matrix(rep(20:26,length.out=121*168),121,168)
grid <- expand.grid(beta=c(.25,1,4),origin=c(96L,120L),chain=c(1L,3L))
grid$valid <- TRUE;grid$prediction <- lapply(grid$origin,function(o) Y[,o+seq_len(48L)])
selection <- a$graphmode_c9_potts_select(grid,Y)
stopifnot(selection$selected$beta==.25)
grid$valid[grid$beta==.25] <- FALSE
stopifnot(a$graphmode_c9_potts_select(grid,Y)$selected$beta==1)
grid$valid[] <- FALSE
stopifnot(inherits(try(a$graphmode_c9_potts_select(grid,Y),silent=TRUE),'try-error'))
cat('Three unchanged sampler transitions and frozen Potts selection/failure tests passed.\n')
