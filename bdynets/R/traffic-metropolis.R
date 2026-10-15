.metropolis_decode <- function(data) {
  n <- length(data$segments); tt <- length(data$frames)
  if(!n || !tt || data$metadata$frameCount!=tt || data$metadata$dynamicSegmentCount!=n) stop('Invalid Metropolis dimensions')
  .data_ids(vapply(data$segments,function(s) as.character(s$id),''),'segment IDs')
  for(f in data$frames) for(k in c('volume','speed','congestion')) {
    v <- .traffic_vector(f[[k]])
    if(length(v)!=n || any(!is.finite(v)) || any(v<0) || (k=='congestion' && any(v>1))) stop('Invalid Metropolis values')
  }
  list(data=data,initial_index=data$metadata$peakFrameIndex+1L)
}

.metropolis_colors <- function(t) {
  t <- pmin(1,pmax(0,t))
  vapply(t,function(z) {
    a <- if(z<.52) c(0,166,81) else c(255,176,0)
    b <- if(z<.52) c(255,176,0) else c(224,36,36)
    u <- if(z<.52) z/.52 else (z-.52)/.48
    rgb <- floor(a+(b-a)*u+.5) # JavaScript Math.round, not R ties-to-even.
    grDevices::rgb(rgb[1],rgb[2],rgb[3],maxColorValue=255)
  },'')
}

.metropolis_frame <- function(x,time_index,metric) {
  d <- x$data; f <- d$frames[[time_index]]; meta <- d$metadata
  value <- .traffic_vector(f[[metric]])
  scale <- if(metric=='volume') max(1,meta$volumeP95) else 1
  t <- pmin(1,pmax(0,value/scale))
  g <- lapply(d$segments,function(s) rbind(c(s$lon1,s$lat1),c(s$lon2,s$lat2)))
  b <- d$bounds
  synthetic <- isTRUE(meta$synthetic)
  structure(list(source='metropolis',mode=metric,time_index=time_index,metric=metric,
    ids=vapply(d$segments,function(s)as.character(s$id),''),geometry=g,value=value,
    color=.metropolis_colors(t),width=if(metric=='volume') 2.2+6.2*t else 2.6+7.4*t,
    opacity=.74+.24*t,halo_opacity=.34+.3*t,active=rep(TRUE,length(t)),
    bounds=rbind(c(b$lonMin,b$latMin),c(b$lonMax,b$latMax)),
    legend=if(metric=='volume') c('0',format(.52*scale),paste0(format(scale),'+')) else c('0: free','0.52: slower','1: severe'),
    legend_colors=.metropolis_colors(c(0,.52,1)),
    title=paste(if(synthetic)'SIMULATED Metropolis-format' else 'Metropolis',metric,f$time),
    unit=if(metric=='volume') 'Volume: mean of three 5-minute observations' else 'Congestion: slowdown x clipped log-volume weight',
    summary=f[c('congestionMean','congestionP95','meanSpeed','meanVolume','active')],
    provenance=x$provenance),class='traffic_frame')
}

#' Extract the Metropolis viewer's whole-day trend
#' @param x A Metropolis traffic_snapshot.
#' @param metric Congestion or volume.
#' @param time_index Selected frame to mark; default is the source peak frame.
#' @return A traffic_trend with saved aggregate values (not recomputed means).
#' @export
traffic_trend <- function(x,metric=c('congestion','volume'),time_index=x$initial_index) {
  metric <- match.arg(metric)
  if(!inherits(x,'traffic_snapshot') || x$source!='metropolis') stop('Metropolis snapshot required')
  traffic_frame(x,time_index,metric=metric)
  field <- if(metric=='volume') 'meanVolume' else 'congestionMean'
  structure(list(value=vapply(x$data$frames,`[[`,0,field),
    time=vapply(x$data$frames,`[[`,'','time'),time_index=time_index,metric=metric,
    synthetic=isTRUE(x$data$metadata$synthetic),provenance=x$provenance),class='traffic_trend')
}

#' Plot the saved Metropolis aggregate trend
#' @param x A traffic_trend.
#' @param ... Additional arguments passed to plot.
#' @return Invisibly x. The selected frame is marked with a vertical line.
#' @export
plot.traffic_trend <- function(x,...) {
  old <- graphics::par(no.readonly=TRUE); on.exit(graphics::par(old))
  graphics::par(mar=c(4,4,3,1))
  graphics::plot(seq_along(x$value),x$value,type='l',lwd=2,
    col=if(x$metric=='volume') '#0057b8' else '#e02424',xaxt='n',
    ylim=c(0,max(x$value,.001)),xlab='Time (source clock)',ylab=paste('Mean',x$metric),
    main=paste(if(x$synthetic)'SIMULATED' else 'Metropolis',x$metric,'trend'),...)
  at <- unique(round(seq(1,length(x$value),length.out=7)))
  graphics::axis(1,at=at,labels=substr(x$time[at],12,16))
  graphics::abline(v=x$time_index,col='#64717a',lty=2)
  invisible(x)
}

#' Simulate a small dataset in the Metropolis visualization schema
#' @param seed Integer seed. The caller's RNG state is restored.
#' @param file New .json.gz destination.
#' @return Invisibly file. All metadata and figures identify simulated data.
#'   This demonstrates the source metric and display format, not the original city.
#' @export
simulate_metropolis_data <- function(file,seed=20261015L) {
  if(!requireNamespace('jsonlite',quietly=TRUE)) stop('Install jsonlite')
  if(file.exists(file) || !grepl('\\.json\\.gz$',file)) stop('Supply a new .json.gz file')
  old <- if(exists('.Random.seed',.GlobalEnv,inherits=FALSE)) get('.Random.seed',.GlobalEnv) else NULL
  on.exit(if(is.null(old)) {if(exists('.Random.seed',.GlobalEnv,inherits=FALSE)) rm('.Random.seed',envir=.GlobalEnv)} else assign('.Random.seed',old,.GlobalEnv))
  set.seed(seed)
  nodes <- expand.grid(x=0:9,y=0:7); segments <- list()
  for(i in seq_len(nrow(nodes))) for(delta in list(c(1,0),c(0,1))) {
    j <- which(nodes$x==nodes$x[i]+delta[1] & nodes$y==nodes$y[i]+delta[2])
    if(length(j)) segments[[length(segments)+1L]] <- list(id=length(segments),
      lat1=35+nodes$y[i]*.005,lon1=110+nodes$x[i]*.005,
      lat2=35+nodes$y[j]*.005,lon2=110+nodes$x[j]*.005,
      length=500,region=1,category=1,edgeType=1,speedLimitCode=5,speedLimit='50 km/h',freeFlowSpeed=50,maxVolume=0)
  }
  n <- length(segments); hour <- seq(0,24,length.out=289)[1:288]
  demand <- .15+exp(-((hour-8)/2)^2)+.8*exp(-((hour-18)/2.5)^2)
  capacity <- stats::runif(n,35,180)
  volume <- sapply(capacity,function(c) stats::rpois(288,c*demand))
  speed <- pmax(3,50*(1-.65*matrix(demand,288,n)*matrix(stats::runif(n,.3,1),288,n,byrow=TRUE))+matrix(stats::rnorm(288*n,0,2),288,n))
  dim(speed) <- c(288L,n)
  aggregate <- function(a) rowsum(a,rep(1:96,each=3))/3
  v <- aggregate(volume); s <- aggregate(speed); free <- apply(speed,2,stats::quantile,.9)
  scale <- max(1,as.numeric(stats::quantile(v,.95)))
  congestion <- pmin(1,pmax(0,1-sweep(s,2,free,'/')))*pmin(1,log1p(v)/log1p(scale))
  dim(congestion) <- dim(v)
  for(j in seq_len(n)) {segments[[j]]$freeFlowSpeed<-round(free[j],2);segments[[j]]$maxVolume<-max(volume[,j])}
  frames <- lapply(1:96,function(i) list(i=i-1L,time=sprintf('2026-01-01 %02d:%02d',((i-1)*15)%/%60,((i-1)*15)%%60),
    congestionMean=round(mean(congestion[i,]),3),congestionP95=round(as.numeric(stats::quantile(congestion[i,],.95)),3),
    meanSpeed=round(mean(s[i,v[i,]>0]),2),meanVolume=round(mean(v[i,]),2),active=sum(v[i,]>0),
    congestion=as.list(round(congestion[i,],3)),volume=as.list(round(v[i,],2)),speed=as.list(round(s[i,],2))))
  peak <- which.max(rowSums(congestion))
  data <- list(metadata=list(title='SIMULATED Metropolis-format example',synthetic=TRUE,seed=seed,
    nativeTimeStepMinutes=5,frameIntervalMinutes=15,frameAggregation='mean of three 5-minute observations',
    sampleDay='2026-01-01',frameCount=96L,dynamicSegmentCount=n,peakFrameIndex=peak-1L,peakFrame=frames[[peak]]$time,
    volumeP95=round(scale,2),congestionP95=round(as.numeric(stats::quantile(congestion,.95)),3),
    congestionDefinition='max(0, 1 - speed / segment_p90_speed) times clipped log-volume weight'),
    bounds=list(latMin=35,latMax=35.035,lonMin=110,lonMax=110.045),segments=segments,frames=frames)
  .metropolis_decode(data)
  artifact <- list(schema_version=1L,source='metropolis',provenance=list(source_html='unavailable companion data; simulated fallback',
    representation='SIMULATED schema-compatible data',seed=seed),payload=list(data=data))
  dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE)
  con <- gzfile(file,'wt'); on.exit(close(con),add=TRUE)
  writeLines(jsonlite::toJSON(artifact,auto_unbox=TRUE,null='null',digits=NA),con)
  invisible(file)
}
