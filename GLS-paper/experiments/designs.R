panel_split <- function(n) bdynets::gls_split_rows(sum(n), n[1L], sum(n[1:2]))

network_graph <- function(A) {
  bdynets::gls_graph(data.frame(name = sprintf("s%02d", seq_len(nrow(A)))), A)
}

swap_graph <- function(A, fraction) {
  B <- A
  requested <- fraction*sum(A)
  distance <- 0
  if (requested > 0 && sum(A) >= 2) for (i in seq_len(10000)) {
    edges <- which(B == 1, arr.ind=TRUE)
    pair <- edges[sample.int(nrow(edges), 2), , drop=FALSE]
    a <- pair[,1]; b <- pair[,2]
    crossed <- cbind(a, rev(b))
    if (any(a == rev(b)) || any(B[crossed] != 0)) next
    step <- sum(A[pair]) - sum(A[crossed])
    if (step <= 0 || abs(distance + step - requested) >= abs(distance - requested)) next
    B[pair] <- 0
    B[crossed] <- 1
    distance <- distance + step
    if (abs(distance-requested) <= .5) break
  }
  B
}

copula_poisson <- function(rate, chol_R) {
  k <- length(rate)
  clock <- numeric(k)
  count <- integer(k)
  while (any(clock <= 1)) {
    # Each column of successive draws forms iid exponential waiting times.
    normal <- drop(stats::rnorm(k) %*% chol_R)
    clock <- clock - stats::pnorm(normal, log.p=TRUE)/rate
    count <- count + (clock <= 1)
  }
  count
}

graph_error_panel <- function(scenario, profile, seed) {
  fields <- strsplit(scenario, "-", fixed=TRUE)[[1]]
  ntrain <- as.integer(sub("T", "", fields[1]))
  fraction <- as.integer(sub("error", "", fields[2]))/100
  k <- if (profile == "smoke") 10L else 20L
  sizes <- if (profile == "smoke") c(if(ntrain==100) 28 else 40,12,16) else c(ntrain,100,200)
  split <- panel_split(sizes)
  set.seed(seed)
  blocks <- sample.int(5, k, replace=TRUE)
  p <- ifelse(outer(blocks, blocks, "=="), k^-.3, 1/k)
  A <- matrix(as.numeric(stats::runif(k*k) < p), k)
  diag(A) <- 0
  truth <- network_graph(A)
  chol_R <- chol(.5 ^ abs(outer(seq_len(k), seq_len(k), "-")))
  n <- sum(sizes)
  burn <- 100L
  Y <- mu <- matrix(0, n + burn, k)
  previous <- rep(1, k)
  for (t in seq_len(nrow(Y))) {
    z <- log1p(previous)
    mu[t,] <- exp(.2 + .2*z + .3 * (truth$W %*% z))
    previous <- Y[t,] <- copula_poisson(mu[t,], chol_R)
  }
  keep <- burn + seq_len(n)
  set.seed(seed + 17L)
  B <- swap_graph(A, fraction)
  list(Y=Y[keep,], lambda=mu[keep,], pre_history=Y[c(burn-1L,burn),],
       graph=network_graph(B), covariates=matrix(0,n,0L), split=split,
       base=.2*diag(k)+.3*truth$W, modulation=0*truth$W, factor=numeric(n),
       generating_scale=rep(1,k), true_A=A,
       graph_error=sum(A == 1 & B == 0)/max(1,sum(A)), requested_error=fraction)
}

simulate_grid_process <- function(n, graph, base, modulation, phase, mean_target,
                                  noise, period, source_scale, scale_signal) {
  k <- nrow(base)
  t <- seq_len(n)
  Q <- cbind(cos(2*pi*t/period), sin(2*pi*t/period),
             cos(4*pi*t/period), sin(4*pi*t/period))
  theta <- seq(0, 2*pi, length.out=k+1L)[seq_len(k)]
  calendar <- Q %*% rbind(.25*cos(theta), .25*sin(theta), .09*sin(theta), .09*cos(theta))
  factor <- sqrt(2)*sin(2*pi*t/(period*1.35) + phase)
  mu <- Y <- contribution <- matrix(0,n,k)
  previous <- round(mean_target)
  for (tt in t) {
    x <- (log1p(previous) - log1p(mean_target))/source_scale
    contribution[tt,] <- scale_signal * (base + modulation*factor[tt]) %*% x
    mu[tt,] <- exp(log(mean_target) + calendar[tt,] + contribution[tt,])
    if (any(!is.finite(mu[tt,]) | mu[tt,] > 1e5))
      stop(errorCondition("Unstable grid intensity during simulation",class="grid_intensity_error"))
    previous <- Y[tt,] <- stats::qpois(noise[tt,],mu[tt,])
  }
  list(Y=Y,lambda=mu,net=contribution,factor=factor,covariates=Q)
}

# Retain the four proportional pilot updates when they meet the RMS target.
# If a proposed scale diverges, bracket the target between zero and that
# scale. A fixed pilot innovation matrix makes all trials comparable.
calibrate_grid_scale <- function(target, pilot_rms, context, tolerance=0.02) {
  result <- function(scale,rms,method)
    list(scale=scale,target_rms=target,pilot_rms=rms,relative_tolerance=tolerance,method=method)
  if (target==0) return(result(0,0,"null"))
  scale <- target
  rms <- pilot_rms(scale)
  for (iteration in seq_len(4L)) {
    if (!is.finite(rms) || rms<=0) break
    scale <- scale*(target/rms)^.7
    rms <- pilot_rms(scale)
  }
  if (is.finite(rms) && abs(rms-target)<=tolerance*target)
    return(result(scale,rms,"proportional"))

  lower <- 0
  upper <- scale
  # A stable trial below target first needs an upper bracket. Treat only
  # the generator's explicitly diagnosed runaway intensities as an upper bound.
  for (iteration in seq_len(20L)) {
    if (!is.finite(rms) || rms>=target) break
    lower <- upper
    upper <- upper*2
    rms <- pilot_rms(upper)
  }
  best_scale <- 0
  best_rms <- 0
  if (!is.finite(rms) || rms>=target) {
    for (iteration in seq_len(30L)) {
      scale <- (lower+upper)/2
      rms <- pilot_rms(scale)
      if (is.finite(rms) && abs(rms-target)<abs(best_rms-target)) {
        best_scale <- scale
        best_rms <- rms
        if (abs(rms-target)<=0.0025*target) return(result(scale,rms,"bracketed"))
      }
      if (!is.finite(rms) || rms>target) upper <- scale else lower <- scale
    }
    if (abs(best_rms-target)<=tolerance*target)
      return(result(best_scale,best_rms,"bracketed"))
  }
  stop(sprintf("Grid calibration unresolved for %s: target RMS %.6g, last pilot RMS %.6g, scale bracket [%.6g, %.6g]",
               context,target,rms,lower,upper),call.=FALSE)
}

sparse_grid_panel <- function(scenario, profile, seed) {
  graph <- bdynets::gls_grid(3L)
  pairs <- bdynets::gls_all_pairs(graph)
  k <- nrow(graph$A)
  sizes <- if (profile == "smoke") c(48,16,20) else c(1200,300,300)
  n <- sum(sizes)
  q <- if (scenario == "seasonal-null") 0 else if (scenario == "mixed-dynamic-strong") .2 else .1
  dynamic <- grepl("dynamic",scenario)
  mixed <- grepl("mixed",scenario)
  set.seed(seed)
  targets <- sample.int(k,20)
  src <- vapply(targets, function(a) { pool <- which(graph$A[a,]>0); pool[sample.int(length(pool),1)] }, integer(1))
  # Twelve active sources change distance class, preserving each target's degree.
  if (mixed) for (j in 9:20) {
    pool <- pairs$source[pairs$target == targets[j] &
      pairs$pair_class == if (j <= 16) "two-hop" else "far/unreachable"]
    src[j] <- pool[sample.int(length(pool),1)]
  }
  set.seed(seed+1L)
  base <- modulation <- matrix(0,k,k)
  active <- rbind(cbind(seq_len(k),seq_len(k)), cbind(targets,src))
  signs <- sample(rep(c(-1,1), length.out=nrow(active)))
  amp <- signs*c(rep(.45,k),runif(20,.65,1))
  base[active] <- amp * if (dynamic) sqrt(.4) else 1
  if (dynamic) modulation[active] <- amp*sqrt(.6)*sample(c(-1,1),nrow(active),replace=TRUE)
  target <- seq(12,20,length.out=k)
  source_scale <- sqrt(target)/(target+1)
  phase <- runif(1,0,2*pi)
  period <- if (profile=="smoke") 24L else 300L
  pilot_n <- if (profile=="smoke") 120L else 1000L
  set.seed(seed+101L)
  pilot_noise <- matrix(runif(k*pilot_n),pilot_n,k)
  pilot_rms <- function(scale) tryCatch({
    pilot <- simulate_grid_process(pilot_n,graph,base,modulation,phase,target,
                                   pilot_noise,period,source_scale,scale)
    sqrt(mean(tail(pilot$net, floor(pilot_n*.8))^2))
  },grid_intensity_error=function(e) Inf)
  calibration <- calibrate_grid_scale(q,pilot_rms,paste(scenario,profile,"seed",seed))
  scale <- calibration$scale
  set.seed(seed+102L)
  burn <- period
  realised <- simulate_grid_process(n+burn,graph,base,modulation,phase,target,
    matrix(runif((n+burn)*k),n+burn,k),period,source_scale,scale)
  keep <- burn + seq_len(n)
  list(Y=realised$Y[keep,],lambda=realised$lambda[keep,],
    covariates=realised$covariates[keep,],pre_history=realised$Y[c(burn-1,burn),],
    base=base*scale,modulation=modulation*scale,
    factor=realised$factor[keep],generating_scale=source_scale,
    split=panel_split(sizes),graph=graph,signal=q,calibration=calibration,
    realized_rms=sqrt(mean(realised$net[keep,]^2)))
}

surge_panel <- function(scenario, profile, seed) {
  sizes <- if (profile=="smoke") c(70,36,48) else c(600,150,250)
  split <- panel_split(sizes)
  n <- sum(sizes)
  groups <- if (profile=="smoke") 3L else 8L
  k <- groups*3
  upstream <- seq_len(groups)
  downstream <- groups + upstream
  A <- matrix(0,k,k)
  A[cbind(downstream,upstream)] <- 1
  graph <- network_graph(A)
  rates <- if (profile=="smoke") seq(12,24,length.out=k) else seq(65,95,length.out=k)
  set.seed(seed)
  # Space episode starts within a stream, and redraw schedules independently
  # across streams. Every split contains recurrent surprises on long profiles.
  events <- function() {
    tab <- matrix(FALSE,n,groups)
    for (j in upstream) for (rows in split) {
      eligible <- rows[2L:(length(rows)-12L)]
      gap <- 14L
      count <- max(2L, floor(length(rows)/(if (profile=="smoke") 30L else 42L)))
      count <- min(count, 1L+(length(eligible)-1L)%/%gap)
      # Uniform subset conditional on a minimum gap. Independent draws give
      # each source and local process its own phase throughout each split.
      reduced <- length(eligible)-(count-1L)*(gap-1L)
      times <- sort(sample.int(reduced,count))+(seq_len(count)-1L)*(gap-1L)
      tab[eligible[times],j] <- TRUE
    }
    tab
  }
  source_event <- events()
  target_event <- events()
  local <- grepl("local-on",scenario,fixed=TRUE)
  transmission <- grepl("transmit-on",scenario,fixed=TRUE)
  rate_multiplier <- c(115,150,185,220,220,220,220,220,200,180,160,110)/80
  ramp <- function(onsets) {
    m <- matrix(1,n,groups)
    for (j in upstream) for (at in which(onsets[,j])) {
      rows <- at + seq_along(rate_multiplier)-1L
      rows <- rows[rows<=n]
      m[rows,j] <- rate_multiplier[seq_along(rows)]
    }
    m
  }
  baseline <- matrix(rep(rates,each=n),n,k)
  baseline[,upstream] <- sweep(ramp(source_event),2,rates[upstream],"*")
  if (local) baseline[,downstream] <- sweep(ramp(target_event),2,rates[downstream],"*")
  # Independent reference sample of source lag values for the coefficient unit.
  pilot_rate <- matrix(rep(rates[upstream],each=4000),4000,groups) *
    matrix(sample(c(rep(1,23),rate_multiplier),4000*groups,replace=TRUE),4000,groups)
  pilot <- matrix(log1p(rpois(length(pilot_rate),pilot_rate)),4000,groups)
  center <- colMeans(pilot)
  scale <- apply(pilot,2,sd)
  set.seed(seed+100L)
  noise <- matrix(runif(n*k),n,k)
  lambda <- baseline
  Y <- matrix(qpois(noise,lambda),n,k)
  previous <- rbind(round(rates[upstream]), head(Y[,upstream,drop=FALSE],-1L))
  b <- if (transmission) log(1.25) else 0
  lag <- sweep(sweep(log1p(previous),2,center),2,scale,"/")
  lambda[,downstream] <- baseline[,downstream] * exp(b*lag)
  Y[,downstream] <- qpois(noise[,downstream],lambda[,downstream])
  base <- A*b
  # Rows by target masks permit an event on one stream to coexist with quiet
  # time on another. Primary early and late windows may overlap target surges.
  shift <- function(event, delays) {
    out <- matrix(FALSE,n,k)
    for (delay in delays) {
      rows <- seq.int(1L+delay, n)
      out[rows,downstream] <- out[rows,downstream] | event[rows-delay,,drop=FALSE]
    }
    out
  }
  windows <- list(target=shift(matrix(TRUE,n,groups),0), first=shift(source_event,1),
    early=shift(source_event,1:3), late=shift(source_event,4:8),
    local=shift(target_event & local,0:11))
  windows$source_only_first <- windows$first & !windows$local
  windows$source_only_early <- windows$early & !windows$local
  windows$source_only_late <- windows$late & !windows$local
  windows$local_only <- windows$local & !shift(source_event,0:12)
  windows$quiet <- windows$target & !shift(source_event,0:12) & !windows$local
  list(Y=Y,lambda=lambda,covariates=matrix(0,n,0L),split=split,graph=graph,
    pre_history=matrix(rep(round(rates),2),2,byrow=TRUE),base=base,modulation=0*base,
    factor=numeric(n),generating_scale=c(scale,rep(1,k-groups)),windows=windows,
    upstream=upstream,downstream=downstream,source_event=source_event,
    target_event=target_event & local)
}
