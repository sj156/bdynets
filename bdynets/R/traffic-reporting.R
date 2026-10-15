.traffic_mercator <- function(x) cbind(x[,1]*pi/180,log(tan(pi/4+x[,2]*pi/360)))

#' Plot a traffic visualization data layer without web tiles
#' @param x A traffic_frame from traffic_frame().
#' @param sensors Add LargeST sensor points (the source viewer's optional layer).
#' @param ... Reserved for future plotting options.
#' @return Invisibly x. Uses Web Mercator geometry and source color thresholds.
#'   Basemap tiles, labels and browser controls are excluded. Line widths are
#'   adapted to the static graphics device; LargeST directional offsets use the
#'   source zoom. Beijing uses the saved matched polylines, not straight chords.
#' @export
plot.traffic_frame <- function(x,sensors=FALSE,...) {
  if(!inherits(x,'traffic_frame')) stop('Invalid traffic frame')
  old <- graphics::par(no.readonly=TRUE); on.exit(graphics::par(old))
  graphics::par(mar=c(2,1,3,1),oma=c(0,0,0,0))
  geometry <- lapply(x$geometry,.traffic_mercator)
  if(x$source=='largest') {
    pixel <- 2*pi/(256*2^x$zoom)
    offset <- if(x$zoom<8.8) .5 else if(x$zoom<9.6) 2.6 else if(x$zoom<10.6) 4.8 else if(x$zoom<11.6) 6.4 else 8
    geometry <- Map(function(g,d) {
      delta <- g[nrow(g),]-g[1,]; len <- sqrt(sum(delta^2))
      if(len==0) return(g)
      normal <- c(delta[2],-delta[1])/len
      axis <- if(d %in% c('N','S')) c(0,1) else c(1,0)
      if(sum(delta*axis)<0) normal <- -normal
      side <- if(d %in% c('N','E')) 1 else if(d %in% c('S','W')) -1 else 0
      sweep(g,2,normal*side*offset*pixel,'+')
    },geometry,x$direction)
  }
  limits <- if(is.null(x$bounds)) do.call(rbind,geometry) else .traffic_mercator(x$bounds)
  xr <- range(limits[,1]); yr <- range(limits[,2]); dy <- diff(yr)
  # Dedicated upper band for the legend, outside the data extent.
  graphics::plot.new(); graphics::plot.window(xlim=xr,ylim=c(yr[1],yr[2]+.23*dy),asp=1)
  graphics::title(main=x$title,cex.main=.95)
  if(x$source=='beijing') for(g in geometry) graphics::lines(g,col='#d4d8dc',lwd=.35)
  if(x$source=='metropolis') for(i in which(x$active)) graphics::lines(geometry[[i]],col=grDevices::adjustcolor('#151a1f',alpha.f=x$halo_opacity[i]),lwd=(x$width[i]+4)*.55)
  for(i in which(x$active)) graphics::lines(geometry[[i]],col=if(x$source=='metropolis') grDevices::adjustcolor(x$color[i],alpha.f=x$opacity[i]) else x$color[i],lwd=x$width[i]*.55)
  if(sensors && x$source=='largest') {
    xy <- do.call(rbind,lapply(x$sensors,function(s)c(s$lng,s$lat)))
    xy <- .traffic_mercator(xy)
    v <- vapply(x$sensors,function(s) .traffic_vector(s$values)[x$time_index],0)
    colors <- rep('#94a3b8',length(v))
    colors[!is.na(v)] <- x$legend_colors[vapply(v[!is.na(v)],function(z) 1L+sum(z>x$breaks),1L)]
    graphics::points(xy,pch=21,cex=.5,bg=colors,col='white')
  }
  graphics::legend(xr[1],yr[2]+.22*dy,legend=x$legend,col=x$legend_colors,lwd=3,
    ncol=if(length(x$legend)>5) 4 else length(x$legend),bty='n',cex=.7,xjust=0,yjust=1)
  graphics::mtext(x$unit,side=1,line=.5,cex=.75)
  invisible(x)
}

#' Reproduce the bundled traffic snapshot figure set
#' @param path Standalone JSON or gzipped JSON visualization dataset.
#' @param source Snapshot schema.
#' @param time_index One-based frame. Default Beijing 107 (08:50) and LargeST 195
#'   (16:10, the original viewer default). Beijing 107 is the saved frame with
#'   highest mean load; selection is descriptive, not inferential.
#' @param output_dir Optional directory for PDF and PNG figures plus provenance.
#'   Existing files with the same names are refused.
#' @return Named list of frames. Without output_dir, draw on the current device.
#'   The same workflow is used by batch scripts and R Markdown tutorials.
#' @export
traffic_figures <- function(path,source=c('beijing','largest','metropolis'),time_index=NULL,output_dir=NULL) {
  source <- match.arg(source); snapshot <- read_traffic_data(path)
  if(snapshot$source!=source) stop('Source does not match dataset')
  if(is.null(time_index)) time_index <- if(source=='beijing') 107L else snapshot$initial_index
  frames <- if(source=='metropolis') list(
    congestion=traffic_frame(snapshot,time_index,metric='congestion'),
    volume=traffic_frame(snapshot,time_index,metric='volume'),
    congestion_trend=traffic_trend(snapshot,'congestion',time_index),
    volume_trend=traffic_trend(snapshot,'volume',time_index)) else if(source=='beijing') stats::setNames(lapply(c('current','delta','duration'),function(m)
    traffic_frame(snapshot,time_index,mode=m)),c('current','delta','duration')) else
    stats::setNames(lapply(names(snapshot$data$regions),function(r) traffic_frame(snapshot,time_index,region=r)),
      c('los-angeles','bay-area'))
  if(is.null(output_dir)) { for(f in frames) plot(f); return(invisible(frames)) }
  names_out <- c(paste0(names(frames),'.pdf'),paste0(names(frames),'.png'),'provenance.csv','session-info.txt')
  if(any(file.exists(file.path(output_dir,names_out)))) stop('Output files exist; use a new directory')
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  for(name in names(frames)) {
    for(ext in c('pdf','png')) {
      file <- file.path(output_dir,paste0(name,'.',ext))
      if(ext=='pdf') grDevices::pdf(file,width=9,height=7,useDingbats=FALSE)
      else grDevices::png(file,width=1800,height=1400,res=200)
      tryCatch(plot(frames[[name]]),finally=grDevices::dev.off())
    }
  }
  utils::write.csv(data.frame(source=source,file=snapshot$provenance$file,md5=snapshot$provenance$md5,
    time_index=time_index,source_html=snapshot$provenance$source_html,package_version=as.character(utils::packageVersion('bdynets'))),
    file.path(output_dir,'provenance.csv'),row.names=FALSE)
  writeLines(utils::capture.output(utils::sessionInfo()),file.path(output_dir,'session-info.txt'))
  invisible(frames)
}
