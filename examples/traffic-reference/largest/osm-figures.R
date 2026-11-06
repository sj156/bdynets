.traffic_mercator <- function(x) cbind(x[,1]*pi/180,log(tan(pi/4+x[,2]*pi/360)))

# Plot saved traffic display links over locally cached OSM roads
plot_osm_frame <- function(x,osm,sensors=FALSE,...) {
  if(!inherits(x,'traffic_frame')) stop('Invalid traffic frame')
  old <- graphics::par(no.readonly=TRUE); on.exit(graphics::par(old))
  graphics::par(mar=c(4,1,4,1),oma=c(0,0,0,0),bg='#ffffff',fg='#25364a')
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
  xr <- range(limits[,1]); yr <- range(limits[,2])
  xr <- xr+c(-1,1)*.008*diff(xr); yr <- yr+c(-1,1)*.008*diff(yr); dy <- diff(yr)
  # Dedicated upper band for the legend, outside the data extent.
  graphics::plot.new(); graphics::plot.window(xlim=xr,ylim=c(yr[1],yr[2]+.13*dy),asp=1)
  graphics::title(main=x$title,cex.main=.95)
  graphics::rect(xr[1],yr[1],xr[2],yr[2],col='#f5f5f1',border=NA)
  graphics::clip(xr[1],xr[2],yr[1],yr[2])
  water <- Filter(function(e) identical(e$type,'water'),osm$elements)
  for(e in water) {
    rings <- lapply(e$rings,function(r) .traffic_mercator(do.call(rbind,lapply(r,function(p)c(p$lon,p$lat)))))
    rings <- Filter(function(r) nrow(unique(r))>=3L,rings)
    if(!length(rings)) next
    xy <- do.call(rbind,lapply(rings,function(r) rbind(r,c(NA,NA))))
    xy <- xy[-nrow(xy),,drop=FALSE]
    graphics::polypath(xy[,1],xy[,2],col='#dcecf2',border=NA,rule='evenodd')
  }
  roads <- Filter(function(e) identical(e$type,'way') && length(e$geometry)>1,osm$elements)
  for(e in roads) {
    xy <- .traffic_mercator(do.call(rbind,lapply(e$geometry,function(p)c(p$lon,p$lat))))
    major <- grepl('motorway|trunk',e$tags$highway)
    graphics::lines(xy,col=if(major) '#b4bdc5' else '#d9dcd9',lwd=if(major) .9 else .45)
  }
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
  cities <- Filter(function(e) identical(e$type,'node') && !is.null(e$tags$name),osm$elements)
  cities <- cities[order(vapply(cities,function(e) if(is.null(e$population)) 0 else e$population,0),decreasing=TRUE)]
  occupied <- matrix(numeric(),ncol=2)
  for(e in cities) {
    xy <- .traffic_mercator(matrix(c(e$lon,e$lat),nrow=1))
    if(xy[1]<xr[1] || xy[1]>xr[2] || xy[2]<yr[1] || xy[2]>yr[2]) next
    if(nrow(occupied) && any(abs(occupied[,1]-xy[1])<.16*diff(xr) & abs(occupied[,2]-xy[2])<.055*diff(yr))) next
    graphics::points(xy,pch=20,cex=.35,col='#52606a')
    label_width <- graphics::strwidth(e$tags$name,cex=.55)
    label_x <- max(xr[1]+label_width/2,min(xr[2]-label_width/2,xy[1]))
    label_y <- min(yr[2]-.012*dy,xy[2]+.018*dy)
    graphics::text(label_x,label_y,labels=e$tags$name,cex=.55,col='#334155')
    occupied <- rbind(occupied,xy)
  }
  graphics::clip(par('usr')[1],par('usr')[2],par('usr')[3],par('usr')[4])
  graphics::mtext('Map: (c) OpenStreetMap contributors | Protomaps | openstreetmap.org/copyright',side=1,line=2,cex=.6,col='#52606a')
  graphics::legend(xr[1],yr[2]+.12*dy,legend=x$legend,col=x$legend_colors,lwd=3,
    ncol=if(length(x$legend)>5) 4 else length(x$legend),bty='n',cex=.7,xjust=0,yjust=1)
  graphics::mtext(x$unit,side=1,line=.5,cex=.75)
  invisible(x)
}


# Offline batch export. The same function is used in the report and command line.
largest_osm_figures <- function(snapshot, time_index=97L, output_dir=NULL, data_dir="data") {
  if(!inherits(snapshot,'traffic_snapshot') || snapshot$source!='largest') stop('Expected a LargeST snapshot')
  frames <- setNames(lapply(names(snapshot$data$regions),function(region)
    bdynets::traffic_frame(snapshot,time_index=time_index,region=region)),
    c("los-angeles","bay-area"))
  maps <- lapply(names(frames),function(name) {
    con <- gzfile(file.path(data_dir,paste0(name,"-osm.json.gz")),"rt")
    on.exit(close(con))
    jsonlite::fromJSON(paste(readLines(con,warn=FALSE),collapse="\n"),simplifyVector=FALSE)
  })
  if(!is.null(output_dir)) {
    dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
    if(!dir.exists(output_dir) || file.access(output_dir,2)!=0)
      stop("Cannot create or write output directory: ",output_dir)
  }
  for(i in seq_along(frames)) {
    if(is.null(output_dir)) plot_osm_frame(frames[[i]],maps[[i]]) else {
      for(ext in c("pdf","png")) {
        dest <- file.path(output_dir,paste0(names(frames)[i],".",ext))
        w <- if(names(frames)[i]=="bay-area") 7 else 10
        h <- if(names(frames)[i]=="bay-area") 9 else 8
        if(ext=="pdf") grDevices::pdf(dest,width=w,height=h,useDingbats=FALSE)
        else grDevices::png(dest,width=w*240,height=h*240,res=240)
        tryCatch(plot_osm_frame(frames[[i]],maps[[i]]),finally=grDevices::dev.off())
      }
    }
  }
  if(!is.null(output_dir)) {
    saveRDS(frames,file.path(output_dir,"selected-frames.rds"))
    files <- list.files(data_dir,full.names=TRUE)
    utils::write.csv(data.frame(file=basename(files),md5=unname(tools::md5sum(files))),
      file.path(output_dir,"input-checksums.csv"),row.names=FALSE)
    writeLines(c(paste("time_index:",time_index),utils::capture.output(sessionInfo())),
      file.path(output_dir,"session-info.txt"))
  }
  invisible(frames)
}
