# Static traffic maps over cached OSM-derived geometry.
read_osm_basemap <- function(path='data/osm-basemap.json.gz') {
  con <- gzfile(path,'rt'); on.exit(close(con))
  x <- jsonlite::fromJSON(paste(readLines(con,warn=FALSE,encoding='UTF-8'),collapse='\n'),
                          simplifyVector=FALSE)
  if(!identical(x$schema,'protomaps-display-extract-v1') || !length(x$elements))
    stop('OSM display-cache schema mismatch')
  x
}

plot_traffic_osm <- function(frame,osm) {
  if(!inherits(frame,'traffic_frame') || !frame$source %in% c('beijing','metropolis'))
    stop('Expected a Beijing or Metropolis traffic frame')
  project <- function(x) cbind(x[,1]*pi/180,log(tan(pi/4+x[,2]*pi/360)))
  geometry <- lapply(frame$geometry,project)
  limits <- if(is.null(frame$bounds)) do.call(rbind,geometry) else project(frame$bounds)
  xr <- range(limits[,1]); yr <- range(limits[,2]); dy <- diff(yr)
  bb <- unlist(osm$provenance$bbox_south_west_north_east,use.names=FALSE)
  geographic <- if(is.null(frame$bounds)) do.call(rbind,frame$geometry) else frame$bounds
  if(length(bb)!=4L || bb[1]>min(geographic[,2]) || bb[2]>min(geographic[,1]) ||
     bb[3]<max(geographic[,2]) || bb[4]<max(geographic[,1]))
    stop('Cached basemap does not cover the traffic extent')
  old <- graphics::par(no.readonly=TRUE); on.exit(graphics::par(old))
  graphics::par(mar=c(4,1,3,1),oma=c(0,0,0,0))
  graphics::plot.new(); graphics::plot.window(xlim=xr,ylim=c(yr[1],yr[2]+.23*dy),asp=1)
  graphics::title(main=frame$title,cex.main=.95)
  graphics::rect(xr[1],yr[1],xr[2],yr[2],col='#f5f5f1',border=NA)
  graphics::clip(xr[1],xr[2],yr[1],yr[2])
  points_xy <- function(points) project(do.call(rbind,lapply(points,function(p)c(p$lon,p$lat))))
  for(e in osm$elements) {
    if(identical(e$type,'water')) {
      rings <- lapply(e$rings,points_xy)
      rings <- Filter(function(x) nrow(unique(x))>=3L,rings)
      if(!length(rings)) next
      polygon <- do.call(rbind,lapply(rings,function(x)rbind(x,c(NA,NA))))
      polygon <- polygon[-nrow(polygon),,drop=FALSE]
      graphics::polypath(polygon[,1],polygon[,2],col='#dcecf2',border=NA,rule='evenodd')
    }
  }
  for(e in osm$elements) if(identical(e$type,'way') && length(e$geometry)>1L) {
    g <- points_xy(e$geometry)
    major <- grepl('motorway|trunk',e$tags$highway)
    graphics::lines(g,col=if(major)'#bcc4cb' else '#daddd9',lwd=if(major).8 else .4)
  }
  if(frame$source=='beijing')
    for(g in geometry) graphics::lines(g,col='#d4d8dc',lwd=.35)
  if(frame$source=='metropolis') for(i in which(frame$active))
    graphics::lines(geometry[[i]],col=grDevices::adjustcolor('#151a1f',alpha.f=frame$halo_opacity[i]),
                    lwd=(frame$width[i]+4)*.55)
  for(i in which(frame$active))
    graphics::lines(geometry[[i]],col=if(frame$source=='metropolis')
      grDevices::adjustcolor(frame$color[i],alpha.f=frame$opacity[i]) else frame$color[i],
      lwd=frame$width[i]*.55)
  places <- Filter(function(e) identical(e$type,'node') && !is.null(e$tags$name),osm$elements)
  places <- places[order(vapply(places,function(e)if(is.null(e$population))0 else e$population,0),decreasing=TRUE)]
  occupied <- matrix(numeric(),ncol=2)
  for(e in places) {
    name <- e$tags$name
    if(!grepl('^[ -~]+$',name)) next
    g <- project(matrix(c(e$lon,e$lat),nrow=1))
    if(g[1]<xr[1] || g[1]>xr[2] || g[2]<yr[1] || g[2]>yr[2]) next
    if(nrow(occupied) && any(abs(occupied[,1]-g[1])<.22*diff(xr) & abs(occupied[,2]-g[2])<.06*dy)) next
    width <- graphics::strwidth(name,cex=.6)
    padding <- .015*diff(xr)
    label_x <- max(xr[1]+padding+width/2,min(xr[2]-padding-width/2,g[1]))
    label_y <- max(yr[1]+.025*dy,min(yr[2]-.025*dy,g[2]))
    graphics::text(label_x,label_y,labels=name,cex=.6,col='#334155')
    occupied <- rbind(occupied,g)
  }
  graphics::clip(graphics::par('usr')[1],graphics::par('usr')[2],
                 graphics::par('usr')[3],graphics::par('usr')[4])
  graphics::legend(xr[1],yr[2]+.22*dy,legend=frame$legend,col=frame$legend_colors,lwd=3,
                   ncol=if(length(frame$legend)>5)4 else length(frame$legend),
                   bty='n',cex=.7,xjust=0,yjust=1)
  graphics::mtext(frame$unit,side=1,line=.5,cex=.75)
  graphics::mtext('Map: (c) OpenStreetMap contributors | Protomaps | openstreetmap.org/copyright',
                  side=1,line=2,cex=.55,col='#52606a')
  invisible(frame)
}

traffic_osm_figures <- function(path,source=c('beijing','metropolis'),time_index=NULL,
                               output_dir=NULL,basemap=file.path(dirname(path),'osm-basemap.json.gz')) {
  source <- match.arg(source)
  snapshot <- bdynets::read_traffic_data(path)
  if(snapshot$source!=source) stop('Source does not match dataset')
  if(is.null(time_index)) time_index <- if(source=='beijing')107L else snapshot$initial_index
  frames <- if(source=='beijing') stats::setNames(lapply(c('current','delta','duration'),function(mode)
    bdynets::traffic_frame(snapshot,time_index,mode=mode)),c('current','delta','duration')) else list(
      congestion=bdynets::traffic_frame(snapshot,time_index,metric='congestion'),
      volume=bdynets::traffic_frame(snapshot,time_index,metric='volume'),
      congestion_trend=bdynets::traffic_trend(snapshot,'congestion',time_index),
      volume_trend=bdynets::traffic_trend(snapshot,'volume',time_index))
  osm <- read_osm_basemap(basemap)
  draw <- function(frame) {
    if(inherits(frame,'traffic_frame')) plot_traffic_osm(frame,osm) else plot(frame)
  }
  if(is.null(output_dir)) { for(frame in frames)draw(frame); return(invisible(frames)) }
  if(file.exists(output_dir))stop('Output directory already exists')
  if(!dir.create(output_dir,recursive=TRUE))stop('Output directory creation failed')
  for(name in names(frames)) for(ext in c('pdf','png')) {
    file <- file.path(output_dir,paste0(name,'.',ext))
    if(ext=='pdf')grDevices::pdf(file,width=9,height=7,useDingbats=FALSE)
    else grDevices::png(file,width=1800,height=1400,res=200)
    tryCatch(draw(frames[[name]]),finally=grDevices::dev.off())
  }
  utils::write.csv(data.frame(source=source,file=basename(path),md5=unname(tools::md5sum(path)),
    time_index=time_index,source_html=snapshot$provenance$source_html,
    package_version=as.character(utils::packageVersion('bdynets')),
    basemap_file=basename(basemap),basemap_md5=unname(tools::md5sum(basemap)),
    basemap_source=osm$provenance$source_url,basemap_build=osm$provenance$build_date,
    basemap_geometry_sha256=osm$provenance$geometry_sha256),
    file.path(output_dir,'provenance.csv'),row.names=FALSE)
  invisible(frames)
}
