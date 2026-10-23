# Saved-data reader for the one-day PeMS FLOW example; no raw processing runs.
read_pems_flow <- function(path='data/pems-flow.json.gz') {
  con <- gzfile(path,'rt'); on.exit(close(con))
  artifact <- jsonlite::fromJSON(paste(readLines(con,warn=FALSE,encoding='UTF-8'),collapse='\n'),
                                 simplifyVector=FALSE)
  if(!identical(artifact$schema_version,1L) || !identical(artifact$source,'pems-flow'))
    stop('Expected the one-day PeMS FLOW example schema')
  x <- artifact$payload
  x$flow <- do.call(rbind,lapply(x$flowByHour,unlist,use.names=FALSE))
  x$flowByHour <- NULL
  x$timeLabels <- unlist(x$timeLabels,use.names=FALSE)
  if(!is.matrix(x$flow) || nrow(x$flow)!=24L || ncol(x$flow)!=length(x$segmentPositions) ||
     length(x$timeLabels)!=24L || x$stepMinutes!=60L || x$missingValue!=65535L ||
     any(!is.finite(x$flow)) || any(x$flow<0 | x$flow>65535 | x$flow!=floor(x$flow)))
    stop('Invalid one-day hourly flow dimensions or encoding')
  expected <- paste(x$date,sprintf('%02d:00',0:23))
  if(!identical(x$timeLabels,expected)) stop('Expected all 24 consecutive source-clock hours')
  x$provenance <- artifact$provenance
  x$input_md5 <- unname(tools::md5sum(path))
  x
}

pems_flow_colors <- function(value) {
  breaks <- log1p(c(400,700,1200,2100,3600,6200))
  colors <- c('#16a43a','#7ad151','#f7ea2a','#ffc928','#f5a623','#f36b2b','#f01818')
  result <- rep('#94a3b8',length(value))
  valid <- is.finite(value) & value>=0 & value!=65535
  result[valid] <- colors[findInterval(log1p(value[valid]),breaks)+1L]
  result
}

pems_flow_frame <- function(x,time_index=x$initialTimeIndex+1L) {
  if(length(time_index)!=1L || !is.finite(time_index) || time_index!=floor(time_index) ||
     time_index<1L || time_index>24L) stop('time_index must be an integer from 1 to 24')
  selected <- x$flow[time_index,]
  selected[selected==x$missingValue] <- NA_integer_
  station_ids <- unlist(x$positionStations,use.names=FALSE)[unlist(x$segmentPositions,use.names=FALSE)+1L]
  # Original viewer: average active positions within station, then available endpoints.
  station <- vapply(split(selected,station_ids),function(v) {
    ok <- is.finite(v); if(any(ok)) mean(v[ok]) else NA_real_
  },numeric(1))
  volume <- vapply(x$links,function(link) {
    ends <- station[c(as.character(link$from_station),as.character(link$to_station))]
    ok <- is.finite(ends); if(any(ok)) mean(ends[ok]) else NA_real_
  },numeric(1))
  links <- data.frame(
    link_id=vapply(x$links,function(z) as.integer(z$id),integer(1)),
    district=vapply(x$links,`[[`,character(1),'district'),
    freeway=vapply(x$links,`[[`,character(1),'freeway'),
    direction=vapply(x$links,`[[`,character(1),'direction'),
    from_station=vapply(x$links,`[[`,character(1),'from_station'),
    to_station=vapply(x$links,`[[`,character(1),'to_station'),
    volume=volume,color=pems_flow_colors(volume),stringsAsFactors=FALSE)
  links$coords <- I(lapply(x$links,function(z) do.call(rbind,lapply(z$coords,unlist))))
  list(date=x$date,time_index=as.integer(time_index),timestamp=x$timeLabels[time_index],
       source_month_frame_index=x$provenance$source_month_frame_indices_zero_based[[1]]+time_index,
       links=links,zoom=x$zoom,source_asset=x$provenance$source_asset,
       source_sha256=x$provenance$source_asset_sha256,dataset_md5=x$input_md5)
}

read_pems_osm <- function(path='data/pems-osm.json.gz') {
  con <- gzfile(path,'rt'); on.exit(close(con))
  x <- jsonlite::fromJSON(paste(readLines(con,warn=FALSE,encoding='UTF-8'),collapse='\n'),
                          simplifyVector=FALSE)
  if(!identical(x$schema,'protomaps-display-extract-v1') || !length(x$elements))
    stop('OSM display-cache schema mismatch')
  x
}

pems_flow_plot <- function(frame,osm=read_pems_osm()) {
  project <- function(coords) cbind(6378137*coords[,2]*pi/180,
                                   6378137*log(tan(pi/4+coords[,1]*pi/360)))
  xy <- lapply(frame$links$coords,project)
  # Directional display offsets follow the viewer's saved initial zoom.
  z <- frame$zoom
  offset <- if(z<8.8) .4 else if(z<9.6) 2.4 else if(z<10.6) 4.8 else if(z<11.6) 6.2 else 8
  pixel <- 2*pi*6378137/(256*2^z)
  xy <- Map(function(g,direction) {
    delta <- g[nrow(g),]-g[1L,]; len <- sqrt(sum(delta^2))
    if(len==0) return(g)
    normal <- c(delta[2],-delta[1])/len
    axis <- if(direction %in% c('N','S')) c(0,1) else c(1,0)
    if(sum(delta*axis)<0) normal <- -normal
    side <- if(direction %in% c('N','E')) 1 else if(direction %in% c('S','W')) -1 else 0
    sweep(g,2,normal*side*offset*pixel,'+')
  },xy,frame$links$direction)
  extent <- do.call(rbind,xy); xr <- range(extent[,1]); yr <- range(extent[,2])
  xr <- xr+c(-1,1)*.008*diff(xr); yr <- yr+c(-1,1)*.008*diff(yr); dy <- diff(yr)
  old <- par(no.readonly=TRUE); on.exit(par(old))
  par(mar=c(4,1,4,1),oma=c(0,0,0,0),bg='white',fg='#25364a')
  plot.new(); plot.window(xlim=xr,ylim=c(yr[1],yr[2]+.13*dy),asp=1)
  title(main=paste('LA-area PeMS flow |',frame$timestamp),cex.main=.95)
  rect(xr[1],yr[1],xr[2],yr[2],col='#f5f5f1',border=NA)
  clip(xr[1],xr[2],yr[1],yr[2])
  project_points <- function(points) {
    coords <- do.call(rbind,lapply(points,function(p)c(p$lat,p$lon)))
    project(coords)
  }
  water <- Filter(function(e) identical(e$type,'water'),osm$elements)
  for(e in water) {
    rings <- lapply(e$rings,project_points)
    rings <- Filter(function(r) nrow(unique(r))>=3L,rings)
    if(!length(rings)) next
    polygon <- do.call(rbind,lapply(rings,function(r) rbind(r,c(NA,NA))))
    polygon <- polygon[-nrow(polygon),,drop=FALSE]
    polypath(polygon[,1],polygon[,2],col='#dcecf2',border=NA,rule='evenodd')
  }
  roads <- Filter(function(e) identical(e$type,'way') && length(e$geometry)>1L,osm$elements)
  for(e in roads) {
    g <- project_points(e$geometry)
    major <- grepl('motorway|trunk',e$tags$highway)
    lines(g,col=if(major) '#b4bdc5' else '#d9dcd9',lwd=if(major) .9 else .45)
  }
  for(i in seq_along(xy)) lines(xy[[i]],col=frame$links$color[i],lwd=1.7)
  cities <- Filter(function(e) identical(e$type,'node') && !is.null(e$tags$name),osm$elements)
  cities <- cities[order(vapply(cities,function(e) if(is.null(e$population)) 0 else e$population,0),decreasing=TRUE)]
  occupied <- matrix(numeric(),ncol=2)
  for(e in cities) {
    g <- project(matrix(c(e$lat,e$lon),nrow=1))
    if(g[1]<xr[1] || g[1]>xr[2] || g[2]<yr[1] || g[2]>yr[2]) next
    if(nrow(occupied) && any(abs(occupied[,1]-g[1])<.16*diff(xr) & abs(occupied[,2]-g[2])<.055*diff(yr))) next
    points(g,pch=20,cex=.35,col='#52606a')
    width <- strwidth(e$tags$name,cex=.55)
    label_x <- max(xr[1]+width/2,min(xr[2]-width/2,g[1]))
    label_y <- min(yr[2]-.012*dy,g[2]+.018*dy)
    text(label_x,label_y,labels=e$tags$name,cex=.55,col='#334155')
    occupied <- rbind(occupied,g)
  }
  clip(par('usr')[1],par('usr')[2],par('usr')[3],par('usr')[4])
  legend(xr[1],yr[2]+.12*dy,
    legend=c('<400','400-<700','700-<1200','1200-<2100','2100-<3600','3600-<6200','>=6200','Missing'),
    col=c(pems_flow_colors(c(0,400,700,1200,2100,3600,6200)),'#94a3b8'),
    lwd=3,ncol=4,bty='n',cex=.68,xjust=0,yjust=1)
  mtext('Flow (vehicles/hour): endpoint display means',side=1,line=.5,cex=.75)
  mtext('Map: (c) OpenStreetMap contributors | Protomaps | openstreetmap.org/copyright',
        side=1,line=2,cex=.6,col='#52606a')
  invisible(frame)
}

pems_flow_figures <- function(path='data/pems-flow.json.gz',time_index=NULL,output_dir=NULL) {
  x <- read_pems_flow(path)
  if(is.null(time_index)) time_index <- x$initialTimeIndex+1L
  frame <- pems_flow_frame(x,time_index)
  osm <- read_pems_osm(file.path(dirname(path),'pems-osm.json.gz'))
  if(is.null(output_dir)) { pems_flow_plot(frame,osm); return(invisible(frame)) }
  if(file.exists(output_dir)) stop('Output directory already exists')
  if(!dir.create(output_dir,recursive=TRUE)) stop('Output directory creation failed')
  pdf(file.path(output_dir,'flow.pdf'),width=10,height=7,useDingbats=FALSE)
  tryCatch(pems_flow_plot(frame,osm),finally=dev.off())
  png(file.path(output_dir,'flow.png'),width=2400,height=1680,res=240)
  tryCatch(pems_flow_plot(frame,osm),finally=dev.off())
  saveRDS(frame,file.path(output_dir,'selected-frame.rds'))
  write.csv(frame$links[setdiff(names(frame$links),'coords')],file.path(output_dir,'flow-links.csv'),row.names=FALSE)
  writeLines(c(paste('Original monthly asset:',frame$source_asset),paste('Original SHA256:',frame$source_sha256),
               paste('One-day dataset MD5:',frame$dataset_md5),paste('Timestamp:',frame$timestamp),
               paste('One-based daily frame:',frame$time_index),
               paste('One-based source-month frame:',frame$source_month_frame_index),
               paste('OSM source:',osm$provenance$source_url),
               paste('OSM build:',osm$provenance$build_date),
               paste('OSM geometry SHA256:',osm$provenance$geometry_sha256)),file.path(output_dir,'provenance.txt'))
  invisible(frame)
}

# The preparation function serializes a fixed source-day slice and its provenance.
prepare_pems_flow <- function(html_file,flow_file,file) {
  if(file.exists(file)) stop('Preparation output already exists')
  if(!requireNamespace('digest',quietly=TRUE)) stop('Missing preparation dependency: digest')
  line <- grep('^const config = ',readLines(html_file,warn=FALSE,encoding='UTF-8'),value=TRUE)
  if(length(line)!=1L) stop('Source map configuration is absent or ambiguous')
  cfg <- jsonlite::fromJSON(sub(';[[:space:]]*$','',sub('^const config = ','',line)),simplifyVector=FALSE)
  at <- which(vapply(cfg$months,function(m) identical(m$key,'2026_06'),logical(1)))
  if(length(at)!=1L) stop('June 2026 configuration is absent or ambiguous')
  m <- cfg$months[[at]]; n <- length(m$segmentPositions)
  con <- gzfile(flow_file,'rb')
  bytes <- tryCatch(readBin(con,'raw',n=m$timeSteps*n*2L+1L),finally=close(con))
  if(length(bytes)!=m$timeSteps*n*2L || m$timeSteps!=720L) stop('June flow dimensions mismatch')
  day_bytes <- bytes[(360L*n*2L+1L):(384L*n*2L)]
  values <- readBin(day_bytes,integer(),n=24L*n,size=2L,signed=FALSE,endian='little')
  frames <- lapply(0:23,function(i) as.list(values[i*n+seq_len(n)]))
  sha_file <- function(path) digest::digest(file=path,algo='sha256',serialize=FALSE)
  payload <- list(date='2026-06-16',stepMinutes=60L,
    timeLabels=as.list(paste('2026-06-16',sprintf('%02d:00',0:23))),
    initialTimeIndex=16L,zoom=cfg$zoom,stationCount=length(cfg$stations),
    links=cfg$links,positionStations=cfg$positionStations,segmentPositions=m$segmentPositions,
    missingValue=65535L,flowByHour=frames)
  artifact <- list(schema_version=1L,source='pems-flow',
    provenance=list(source_html=basename(html_file),source_sha256=sha_file(html_file),
      source_version='PeMS hourly FLOW, January 2020 to June 2026',
      source_asset=basename(flow_file),source_asset_sha256=sha_file(flow_file),
      source_month='2026_06',source_month_frame_indices_zero_based=list(360L,383L),
      source_slice_sha256=digest::digest(day_bytes,algo='sha256',serialize=FALSE),
      representation='24 original hourly rows; original integer values and network mappings',
      label_convention='Source viewer hour labels; UTC arithmetic without verified raw-timezone conversion'),
    payload=payload)
  dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE)
  con <- gzfile(file,'wt')
  tryCatch(writeLines(jsonlite::toJSON(artifact,auto_unbox=TRUE,digits=NA,null='null'),con,useBytes=TRUE),
           finally=close(con))
  invisible(file)
}
