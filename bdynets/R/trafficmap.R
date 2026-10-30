.tm_assert <- function(ok,message) if(!isTRUE(ok))stop(message,call.=FALSE)
.tm_palette <- c('#16a43a','#7ad151','#f7ea2a','#ffc928','#f5a623','#f36b2b','#f01818')
.tm_time <- function(time) {
  .tm_assert((is.numeric(time)||inherits(time,'POSIXct'))&&!anyNA(time)&&all(is.finite(as.numeric(time))),
    'Time must contain finite numeric seconds or POSIXct timestamps')
  list(value=as.numeric(time),clock=if(inherits(time,'POSIXct'))'absolute' else 'source_clock')
}

.tm_basemap <- function(base) {
 if(is.null(base))return(NULL)
 .tm_assert(is.list(base)&&identical(base$type,'FeatureCollection')&&is.list(base$features),
  'basemap must be a GeoJSON FeatureCollection')
 valid <- function(g,n) is.matrix(g)&&is.numeric(g)&&ncol(g)==2L&&nrow(g)>=n&&
  all(is.finite(g))&&all(abs(g[,1])<=180)&&all(abs(g[,2])<=85.0511)
 for(f in base$features) {
  .tm_assert(is.list(f)&&identical(f$type,'Feature')&&is.list(f$geometry),'Invalid context feature')
  g<-f$geometry
  .tm_assert(is.character(g$type)&&length(g$type)==1L&&g$type%in%c('LineString','Polygon','Point'),
   'Context geometry must be LineString, Polygon or Point')
  ok<-switch(g$type,LineString=valid(g$coordinates,2L),
   Polygon=is.list(g$coordinates)&&length(g$coordinates)>0L&&all(vapply(g$coordinates,
    function(r)valid(r,4L)&&all(r[1,]==r[nrow(r),]),logical(1))),
   Point=is.numeric(g$coordinates)&&length(g$coordinates)==2L&&valid(matrix(g$coordinates,1),1L))
  .tm_assert(ok,'Invalid context coordinates or polygon ring')
  .tm_assert(is.null(f$properties)||is.list(f$properties),'Context properties must be a list')
  label<-f$properties$label
  .tm_assert(is.null(label)||(is.character(label)&&length(label)==1L&&!is.na(label)),
   'Context label must be one string')
 }
 if(is.null(base$attribution))base$attribution<-'Basemap source unspecified'
 .tm_assert(is.character(base$attribution)&&length(base$attribution)==1L&&!is.na(base$attribution)&&
  nzchar(base$attribution),'Context attribution must be one nonempty string')
 base
}

#' @export
#' @noRd
flow_scale <- function(values) {
  .tm_assert(is.numeric(values)&&!any(is.infinite(values))&&!any(is.nan(values))&&
    all(values[!is.na(values)]>=0),'Flow must be nonnegative and finite or NA')
  v <- values[!is.na(values)]
  if(!length(v))return(list(kind='missing',maximum=NA_real_,breaks=numeric(),palette=.tm_palette))
  maximum <- max(v)
  if(maximum==0)return(list(kind='zero',maximum=0,breaks=numeric(),palette=.tm_palette))
  list(kind='log1p',maximum=maximum,
    breaks=expm1(seq(0,log1p(maximum),length.out=8L)[2:7]),palette=.tm_palette)
}
.tm_colors <- function(values,scale) {
  out <- rep('#94a3b8',length(values));valid <- which(!is.na(values))
  if(length(valid))out[valid] <- scale$palette[findInterval(log1p(values[valid]),log1p(scale$breaks))+1L]
  out
}

#' @export
#' @noRd
traffic_data <- function(roads=NULL,flow=NULL,events=NULL,times=NULL,time_labels=NULL,
                         interval_seconds=NULL,unit='unit unspecified',title='Traffic map',
                         basemap=NULL,description='',preparation=character(),notes=character(),
                         provenance=list()) {
  .tm_assert(is.character(unit)&&length(unit)==1L&&!is.na(unit)&&nzchar(unit),'unit must be one label')
  .tm_assert(is.character(title)&&length(title)==1L&&!is.na(title),'title must be one string')
  if(is.null(roads)) {
    roads <- data.frame(road_id=character(),region=character());roads$geometry <- list()
  }
  .tm_assert(is.data.frame(roads)&&all(c('road_id','geometry')%in%names(roads)),
    'roads needs road_id and geometry columns')
  roads$road_id <- as.character(roads$road_id)
  .tm_assert(!anyNA(roads$road_id)&&all(nzchar(roads$road_id))&&!anyDuplicated(roads$road_id),'Road IDs must be unique and nonempty')
  .tm_assert(is.list(roads$geometry),'geometry must be a list-column')
  for(g in roads$geometry).tm_assert(is.matrix(g)&&is.numeric(g)&&ncol(g)==2L&&nrow(g)>=2L&&
    all(is.finite(g))&&all(abs(g[,1])<=180)&&all(abs(g[,2])<=85.0511),
    'Road geometry must be longitude/latitude matrices with at least two vertices')
  if(!'region'%in%names(roads))roads$region <- rep('',nrow(roads))
  roads$region <- as.character(roads$region)
  .tm_assert(!anyNA(roads$region),'Road region labels must be strings; an empty label is unassigned')
  for(name in setdiff(names(roads),'geometry')).tm_assert(is.atomic(roads[[name]]),'Road attributes must be scalar columns')
  clocks <- character();observed_times <- numeric()
  if(is.null(flow))flow <- data.frame(road_id=character(),time=numeric(),flow=numeric())
  .tm_assert(is.data.frame(flow)&&all(c('road_id','time','flow')%in%names(flow)),'flow needs road_id, time and flow columns')
  if(nrow(flow)) {
    t <- .tm_time(flow$time);clocks <- c(clocks,t$clock);flow$time <- t$value
    observed_times <- c(observed_times,flow$time)
  }
  flow$road_id <- as.character(flow$road_id)
  .tm_assert(all(flow$road_id%in%roads$road_id),'Flow references unknown road IDs')
  flow_scale(flow$flow)
  if(is.null(events))events <- data.frame(time=numeric(),lon=numeric(),lat=numeric())
  .tm_assert(is.data.frame(events)&&all(c('time','lon','lat')%in%names(events)),
    'events needs time, lon and lat columns')
  if(nrow(events)) {
    if(!'time_label'%in%names(events))events$time_label <- if(inherits(events$time,'POSIXct'))format(events$time,tz='UTC',usetz=FALSE) else format(events$time,trim=TRUE)
    t <- .tm_time(events$time);clocks <- c(clocks,t$clock);events$time <- t$value
    observed_times <- c(observed_times,events$time)
  }
  .tm_assert(is.numeric(events$lon)&&is.numeric(events$lat)&&all(is.finite(events$lon))&&
    all(is.finite(events$lat))&&all(abs(events$lon)<=180)&&all(abs(events$lat)<=85.0511),
    'Event coordinates must be finite longitude/latitude degrees')
  if(!'id'%in%names(events))events$id <- as.character(seq_len(nrow(events)))
  events$id <- as.character(events$id)
  .tm_assert(!anyNA(events$id)&&all(nzchar(events$id))&&!anyDuplicated(events$id),'Event IDs must be unique and nonempty')
  for(name in c('category','description','region')) {
    if(!name%in%names(events))events[[name]] <- rep(if(name=='category')'Uncategorized' else '',nrow(events))
    events[[name]] <- as.character(events[[name]])
    events[[name]][is.na(events[[name]])] <- if(name=='category')'Uncategorized' else ''
    if(name=='category')events[[name]][!nzchar(events[[name]])] <- 'Uncategorized'
  }
  .tm_assert(nrow(roads)>0L||nrow(events)>0L,'At least one road or event is required')
  .tm_assert(!is.null(times)||length(observed_times)>0L,'A frame clock or observations are required')
  if(is.null(times)) {
    if(nrow(flow))times <- sort(unique(flow$time)) else {
      if(is.null(interval_seconds))interval_seconds <- 3600
      step <- interval_seconds[1]
      .tm_assert(is.numeric(step)&&length(step)==1L&&is.finite(step)&&step>0,'Invalid interval')
      times <- seq(floor(min(observed_times)/step)*step,floor(max(observed_times)/step)*step,by=step)
    }
  } else {
    t <- .tm_time(times);clocks <- c(clocks,t$clock);times <- t$value
  }
  .tm_assert(length(unique(clocks))<=1L,'Flow, events and frame clocks must have the same type')
  clock <- if(length(clocks))clocks[1] else 'source_clock'
  .tm_assert(length(times)>0L&&all(is.finite(times))&&all(diff(times)>0),'Frame times must be strictly increasing')
  if(is.null(interval_seconds))interval_seconds <- if(length(times)>1L)c(diff(times),utils::tail(diff(times),1)) else 3600
  if(length(interval_seconds)==1L)interval_seconds <- rep(interval_seconds,length(times))
  .tm_assert(is.numeric(interval_seconds)&&length(interval_seconds)==length(times)&&
    all(is.finite(interval_seconds))&&all(interval_seconds>0)&&
    (length(times)==1L||all(utils::head(times+interval_seconds,-1)<=utils::tail(times,-1))),
    'Frame intervals must be positive, aligned and nonoverlapping')
  if(is.null(time_labels))time_labels <- if(clock=='absolute')
    format(as.POSIXct(times,origin='1970-01-01',tz='UTC'),tz='UTC',usetz=FALSE) else format(times,trim=TRUE)
  .tm_assert(is.character(time_labels)&&length(time_labels)==length(times)&&!anyNA(time_labels),'Invalid time labels')
  rows <- match(flow$time,times);columns <- match(flow$road_id,roads$road_id)
  .tm_assert(!anyNA(rows),'Flow timestamps must match frame starts')
  keys <- (rows-1)*nrow(roads)+columns
  .tm_assert(!anyDuplicated(keys),'Duplicate road-time observations')
  values <- matrix(NA_real_,length(times),nrow(roads))
  if(length(rows))values[cbind(rows,columns)] <- flow$flow
  events$frame <- findInterval(events$time,times)
  if(nrow(events)).tm_assert(all(events$frame>0L)&&
    all(events$time<times[events$frame]+interval_seconds[events$frame]),
    'Events lie outside the supplied half-open frame intervals')
  basemap <- .tm_basemap(basemap)
  .tm_assert(is.list(provenance)&&is.character(description)&&length(description)==1L&&!is.na(description)&&is.character(preparation)&&!anyNA(preparation)&&is.character(notes)&&!anyNA(notes),'Invalid descriptive metadata')
  out <- structure(list(roads=roads,values=values,events=events,times=times,time_labels=time_labels,
    interval_seconds=interval_seconds,clock=clock,unit=unit,title=title,basemap=basemap,
    description=description,preparation=preparation,notes=notes,provenance=provenance,
    regions=unique(c(roads$region[nzchar(roads$region)],events$region[nzchar(events$region)])),scale=flow_scale(values)),class='trafficmap_data')
  out$opposing <- .tm_opposing(roads$geometry,roads$opposite_id,roads$road_id)
  out$initial <- 1L
  out
}

#' @export
#' @noRd
read_traffic <- function(file) {
  input <- readRDS(file)
  .tm_assert(is.list(input)&&identical(input$schema,'trafficmap-input-v1'),'Unsupported traffic input schema')
  input$schema <- NULL
  .tm_assert(all(names(input)%in%names(formals(traffic_data))),'Unknown traffic input fields')
  do.call(traffic_data,input)
}
.tm_select <- function(x,region=NULL) {
  .tm_assert(inherits(x,'trafficmap_data'),'Expected trafficmap_data')
  if(!is.null(region)).tm_assert(is.character(region)&&length(region)==1L&&region%in%x$regions,'Unknown region')
  list(roads=if(is.null(region))seq_len(nrow(x$roads)) else which(x$roads$region==region),
    events=if(is.null(region))seq_len(nrow(x$events)) else which(x$events$region==region))
}
#' @export
#' @noRd
traffic_trend <- function(x,region=NULL) {
  selected <- .tm_select(x,region);v <- x$values[,selected$roads,drop=FALSE]
  mean <- vapply(seq_along(x$times),function(i) {
    finite <- v[i,][is.finite(v[i,])];if(length(finite))base::mean(finite) else NA_real_
  },numeric(1))
  count <- tabulate(x$events$frame[selected$events],nbins=length(x$times))
  data.frame(time=x$times,label=x$time_labels,available=rowSums(!is.na(v)),mean=mean,events=count)
}
#' @export
#' @noRd
traffic_summary <- function(x,index=x$initial,region=NULL) {
  selected <- .tm_select(x,region)
  .tm_assert(is.numeric(index)&&length(index)==1L&&is.finite(index)&&index==floor(index)&&index>=1&&index<=length(x$times),'Invalid frame index')
  v <- x$values[index,selected$roads];finite <- v[is.finite(v)]
  data.frame(frame=as.integer(index),time=x$time_labels[index],roads=length(selected$roads),
    available=length(finite),missing=sum(is.na(v)),zero=sum(finite==0),
    minimum=if(length(finite))min(finite) else NA_real_,median=if(length(finite))stats::median(finite) else NA_real_,
    mean=if(length(finite))mean(finite) else NA_real_,p95=if(length(finite))unname(stats::quantile(finite,.95)) else NA_real_,
    maximum=if(length(finite))max(finite) else NA_real_,
    events=sum(x$events$frame[selected$events]==index),stringsAsFactors=FALSE)
}

.tm_project <- function(g)cbind(g[,1]*pi/180,log(tan(pi/4+g[,2]*pi/360)))
# Explicit reciprocal relationships take priority over conservative geometry matches.
.tm_opposing <- function(geometry,opposite=NULL,ids=as.character(seq_along(geometry))) {
 n<-length(geometry)
 result<-lapply(geometry,function(g)cbind(gap=rep(NA_real_,nrow(g)-1L),side=rep(0,nrow(g)-1L)))
 partner<-rep(NA_integer_,n)
 if(!is.null(opposite)) {
  .tm_assert(is.atomic(opposite)&&length(opposite)==n,'opposite_id must be one road identifier per road')
  opposite<-as.character(opposite);opposite[is.na(opposite)]<-'';declared<-which(nzchar(opposite))
  partner[declared]<-match(opposite[declared],ids)
  .tm_assert(!anyNA(partner[declared]),'opposite_id references an unknown road')
  .tm_assert(all(partner[declared]!=declared),'A road cannot be its own opposite_id')
  .tm_assert(all(!is.na(partner[partner[declared]]))&&all(partner[partner[declared]]==declared),
   'opposite_id relationships must be reciprocal')
 }
 if(n<2L)return(result)
 # Unique exact reversals avoid spatial searches on shared centerlines.
 signature<-function(g)paste(sprintf('%.9f,%.9f',g[,1],g[,2]),collapse=';')
 keys<-vapply(geometry,signature,'');back<-vapply(geometry,function(g)signature(g[nrow(g):1,,drop=FALSE]),'')
 counts<-table(keys);exact<-match(back,keys)
 eligible<-which(is.na(partner)&!is.na(exact)&keys!=back)
 eligible<-eligible[is.na(partner[exact[eligible]])&counts[keys[eligible]]==1L&counts[back[eligible]]==1L]
 done<-logical(n)
 for(j in eligible) {
  other<-geometry[[exact[j]]][nrow(geometry[[exact[j]]]):1,,drop=FALSE]
  if(identical(dim(geometry[[j]]),dim(other))&&max(abs(geometry[[j]]-other))<=1e-9) {
   result[[j]][,1]<-0;result[[j]][,2]<-1;done[j]<-TRUE
  }
 }
 remaining<-which(!done);if(length(remaining)<2L)return(result)
 segments<-do.call(rbind,lapply(remaining,function(j) {
  g<-.tm_project(geometry[[j]])*6378137
  a<-g[-nrow(g),,drop=FALSE];d<-g[-1,,drop=FALSE]-a;len<-sqrt(rowSums(d*d));k<-which(len>1e-6)
  if(!length(k))return(NULL)
  cosine<-cos((geometry[[j]][k,2]+geometry[[j]][k+1,2])*pi/360)
  cbind(road=j,edge=k,x=a[k,1],y=a[k,2],ux=d[k,1]/len[k],uy=d[k,2]/len[k],length=len[k],cosine=cosine,
   probes=pmax(3,ceiling(len[k]*cosine/40)))
 }))
 if(is.null(segments))return(result)
 ns<-nrow(segments)
 probes<-do.call(rbind,lapply(seq_len(ns),function(k) {
  t<-(seq_len(segments[k,'probes'])-.5)/segments[k,'probes']
  cbind(segment=k,x=segments[k,'x']+t*segments[k,'length']*segments[k,'ux'],
   y=segments[k,'y']+t*segments[k,'length']*segments[k,'uy'])
 }))
 owner<-as.integer(probes[,'segment']);best<-integer(nrow(probes));gap<-rep(NA_real_,nrow(probes));side<-numeric(nrow(probes))
 nearest<-function(points,owners,candidates,explicit=FALSE) {
  count<-nrow(points);out<-list(best=integer(count),gap=rep(NA_real_,count),side=numeric(count),x=numeric(count),y=numeric(count))
  if(!count||!length(candidates))return(out)
  qi<-rep(seq_len(count),each=length(candidates));si<-rep(candidates,count);own<-owners[qi]
  ok<-segments[own,'road']!=segments[si,'road']&
   segments[own,'ux']*segments[si,'ux']+segments[own,'uy']*segments[si,'uy']<=-cos(pi/12)
  qi<-qi[ok];si<-si[ok];own<-own[ok];if(!length(qi))return(out)
  dx<-points[qi,1]-segments[si,'x'];dy<-points[qi,2]-segments[si,'y']
  along<-dx*segments[si,'ux']+dy*segments[si,'uy']
  distance<-abs(dx*segments[si,'uy']-dy*segments[si,'ux'])
  ok<-along>=-1e-7&along<=segments[si,'length']+1e-7
  if(!explicit)ok<-ok&distance*segments[own,'cosine']<=30
  qi<-qi[ok];si<-si[ok];own<-own[ok];along<-along[ok];distance<-distance[ok]
  if(!length(qi))return(out)
  order<-order(qi,distance);win<-order[!duplicated(qi[order])];q<-qi[win];s<-si[win]
  out$best[q]<-s;out$gap[q]<-distance[win]
  out$x[q]<-segments[s,'x']+along[win]*segments[s,'ux']
  out$y[q]<-segments[s,'y']+along[win]*segments[s,'uy']
  signed<-(points[q,1]-out$x[q])*segments[owners[q],'uy']-(points[q,2]-out$y[q])*segments[owners[q],'ux']
  out$side[q]<-ifelse(distance[win]<1e-5,1,sign(signed))
  if(!explicit) {
   chosen_along<-numeric(count);chosen_along[q]<-along[win]
   chosen<-out$best[qi];epsilon<-.05/segments[own,'cosine']
   joint<-((along<=epsilon&chosen_along[qi]>=segments[chosen,'length']-epsilon)|
    (along>=segments[si,'length']-epsilon&chosen_along[qi]<=epsilon))&
    segments[si,'ux']*segments[chosen,'ux']+segments[si,'uy']*segments[chosen,'uy']>=cos(pi/12)&
    sqrt((segments[si,'x']+along*segments[si,'ux']-out$x[qi])^2+
     (segments[si,'y']+along*segments[si,'uy']-out$y[qi])^2)*segments[own,'cosine']<=.5
   competing<-segments[si,'road']!=segments[chosen,'road']&!joint&
    (distance-out$gap[qi])*segments[own,'cosine']<=2
   out$best[unique(qi[competing])]<-0L
  }
  out
 }
 choose<-function(query,candidates,explicit=FALSE) {
  if(!length(query)||!length(candidates))return(invisible(NULL))
  self<-owner[query];forward<-nearest(probes[query,c('x','y'),drop=FALSE],self,candidates,explicit)
  valid<-which(forward$best>0L)
  if(length(valid)&&!explicit) {
   reverse<-nearest(cbind(forward$x[valid],forward$y[valid]),forward$best[valid],unique(c(candidates,self)),FALSE)
   mutual<-reverse$best>0L
   mutual[mutual]<-segments[reverse$best[mutual],'road']==segments[self[valid[mutual]],'road']
   forward$best[valid[!mutual]]<-0L
  }
  best[query]<<-forward$best;gap[query]<<-forward$gap;side[query]<<-forward$side
  invisible(NULL)
 }
 # Authoritative IDs limit the search to the declared partner; distance is measured, not inferred.
 for(j in remaining[!is.na(partner[remaining])]) {
  queries<-which(segments[owner,'road']==j);candidates<-which(segments[,'road']==partner[j])
  if(!length(queries)||!length(candidates))next
  for(start in seq.int(1L,max(1L,length(queries)),by=128L))choose(queries[seq.int(start,min(length(queries),start+127L))],candidates,TRUE)
 }
 automatic<-which(is.na(partner[segments[,'road']]))
 if(length(automatic)>1L) {
  pieces<-do.call(rbind,lapply(automatic,function(k) {
   count<-ceiling(segments[k,'length']/100);t<-(seq_len(count)-.5)/count
   cbind(segment=k,x=floor((segments[k,'x']+t*segments[k,'length']*segments[k,'ux'])/250),
    y=floor((segments[k,'y']+t*segments[k,'length']*segments[k,'uy'])/250))
  }))
  buckets<-lapply(split(as.integer(pieces[,'segment']),paste(pieces[,'x'],pieces[,'y'],sep=':')),unique)
  lookup<-list2env(buckets,hash=TRUE,parent=emptyenv())
  query<-which(owner%in%automatic);cells<-floor(probes[query,c('x','y'),drop=FALSE]/250)
  groups<-split(query,paste(cells[,1],cells[,2],sep=':'))
  radius<-ceiling((50+max(30/segments[automatic,'cosine']))/250)
  offsets<-expand.grid(x=seq.int(-radius,radius),y=seq.int(-radius,radius))
  for(q in groups) {
   cell<-floor(probes[q[1],c('x','y')]/250)
   names<-paste(cell[1]+offsets$x,cell[2]+offsets$y,sep=':')
   candidates<-unique(unlist(mget(names,envir=lookup,inherits=FALSE,ifnotfound=rep(list(NULL),length(names))),use.names=FALSE))
   choose(q,candidates)
  }
 }
 valid<-which(best>0L)
 if(!length(valid))return(result)
 supported<-best>0L
 positive<-tabulate(owner[supported&gap>1e-5&side>0],ns)
 negative<-tabulate(owner[supported&gap>1e-5&side<0],ns)
 ambiguous<-pmin(positive,negative)/pmax(1,positive+negative)>.2&is.na(partner[segments[,'road']])
 alive<-!ambiguous&tabulate(owner[supported],ns)/segments[,'probes']>=.6
 repeat {
  keep<-supported;keep[valid]<-keep[valid]&alive[best[valid]]
  next_alive<-alive&tabulate(owner[keep],ns)/segments[,'probes']>=.6
  if(identical(alive,next_alive))break
  alive<-next_alive
 }
 groups<-split(which(keep&alive[owner]),owner[keep&alive[owner]])
 for(q in groups) {
  k<-owner[q[1]]
  signs<-sum(side[q][gap[q]>1e-5]);offset_side<-if(signs==0)1 else sign(signs)
  result[[segments[k,'road']]][segments[k,'edge'],]<-c(min(gap[q]),offset_side)
 }
 result
}

.tm_layout_rules <- list(tile=96,sample=24,budget=.24,min_width=.85,max_width=4.5,
 base_width=2,zoom_slope=.5,reference_zoom=8,gap_min=.55,gap_max=1.8,
 gap_base=.35,gap_slope=.32,halo_min=.2,halo_max=.55,halo_slope=.12)

# Road length per display area controls ink density independently of record count.
.tm_view_style <- function(paths,width,height,zoom,rules=.tm_layout_rules) {
 tile<-rules$tile;n<-length(paths)
 base<-max(rules$min_width,min(rules$max_width,rules$base_width+
  rules$zoom_slope*(zoom-rules$reference_zoom)))
 samples<-lapply(seq_along(paths),function(j) {
  g<-paths[[j]];a<-g[-nrow(g),,drop=FALSE];d<-g[-1,,drop=FALSE]-a
  lower<-rep(0,nrow(a));upper<-rep(1,nrow(a));valid<-rep(TRUE,nrow(a))
  for(edge in list(cbind(-d[,1],a[,1]+tile),cbind(d[,1],width+tile-a[,1]),
                   cbind(-d[,2],a[,2]+tile),cbind(d[,2],height+tile-a[,2]))) {
   p<-edge[,1];q<-edge[,2];valid<-valid&!(p==0&q<0)
   negative<-which(p<0);positive<-which(p>0)
   lower[negative]<-pmax(lower[negative],q[negative]/p[negative])
   upper[positive]<-pmin(upper[positive],q[positive]/p[positive])
  }
  indices<-which(valid&upper>lower&rowSums(d*d)>1e-20)
  if(!length(indices))return(NULL)
  do.call(rbind,lapply(indices,function(k) {
   len<-sqrt(sum(d[k,]^2))*(upper[k]-lower[k]);count<-ceiling(len/rules$sample)
   t<-lower[k]+(seq_len(count)-.5)/count*(upper[k]-lower[k])
   cbind(road=j,x=floor((a[k,1]+t*d[k,1])/tile),
    y=floor((a[k,2]+t*d[k,2])/tile),length=len/count)
  }))
 })
 points<-do.call(rbind,samples);density<-numeric(n)
 if(!is.null(points)) {
  key<-paste(points[,'x'],points[,'y'],sep=':')
  bins<-rowsum(points[,'length'],key,reorder=FALSE)[,1]
  local<-numeric(nrow(points))
  for(dx in -1:1)for(dy in -1:1) {
   value<-unname(bins[paste(points[,'x']+dx,points[,'y']+dy,sep=':')])
   value[is.na(value)]<-0;local<-local+value/(9*tile^2)
  }
  sums<-rowsum(cbind(local*points[,'length'],points[,'length']),points[,'road'],reorder=FALSE)
  density[as.integer(rownames(sums))]<-sums[,1]/sums[,2]
 }
 stroke<-pmax(rules$min_width,base/(1+(base*density/rules$budget)^4)^.25)
 list(width=stroke,gap=pmax(rules$gap_min,pmin(rules$gap_max,rules$gap_base+rules$gap_slope*stroke)),
  halo=pmax(rules$halo_min,pmin(rules$halo_max,rules$halo_slope*stroke)),density=density)
}

# Display units are pixels for HTML and 1/96 inch for graphics line widths.
.tm_display_paths <- function(geometry,opposing,units_per_metre,width,gap=2.5) {
 width<-rep_len(width,length(geometry));gap<-rep_len(gap,length(geometry))
 lapply(seq_along(geometry),function(j) {
  q<-.tm_project(geometry[[j]])*6378137
  d<-q[-1,,drop=FALSE]-q[-nrow(q),,drop=FALSE];len<-sqrt(rowSums(d*d))
  pair<-opposing[[j]];amount<-pmax(0,(width[j]+gap[j]-pair[,1]*units_per_metre)/2)
  amount[is.na(amount)|len<=1e-6]<-0
  shift<-cbind(d[,2],-d[,1])/pmax(len,1e-6)*(amount*pair[,2]/units_per_metre)
  if(nrow(q)==2L)return(q+rbind(shift,shift))
  offsets<-rbind(shift[1,,drop=FALSE],
   (shift[-nrow(shift),,drop=FALSE]+shift[-1,,drop=FALSE])/2,
   shift[nrow(shift),,drop=FALSE])
  q+offsets
 })
}

.tm_locations <- function(x,region=NULL,complete=FALSE) {
 s <- .tm_select(x,region)
 roads<-if(length(s$roads))do.call(rbind,lapply(x$roads$geometry[s$roads],function(g)
  if(complete)g else matrix(apply(g,2,stats::median),1))) else matrix(numeric(),0,2)
 events<-as.matrix(x$events[s$events,c('lon','lat'),drop=FALSE])
 rbind(roads,events)
}
.tm_focus <- function(coords) {
 if(!nrow(coords))return(c(-.01,.01,-.01,.01))
 projected <- .tm_project(coords)
 p <- if(nrow(projected)>=5)c(.15,.85) else c(0,1)
 xr <- as.numeric(stats::quantile(projected[,1],p));yr <- as.numeric(stats::quantile(projected[,2],p))
 span <- max(diff(xr),diff(yr),.00005)
 c(xr+c(-.08,.08)*span,yr+c(-.08,.08)*span)
}
.tm_static_context <- function(base,limits) {
 if(is.null(base))return(invisible(NULL))
 for(f in base$features) {
  g<-f$geometry
  if(g$type=='LineString') {
   q<-.tm_project(as.matrix(g$coordinates));graphics::lines(q,col='#ccd1d3',lwd=.65)
  } else if(g$type=='Polygon') {
   q<-do.call(rbind,lapply(g$coordinates,function(r)rbind(.tm_project(as.matrix(r)),c(NA_real_,NA_real_))))
   q<-q[-nrow(q),,drop=FALSE]
   graphics::polypath(q[,1],q[,2],col='#e1e5e2',border=NA,rule='evenodd')
  }
 }
 places<-Filter(function(f)f$geometry$type=='Point'&&!is.null(f$properties$label)&&nzchar(f$properties$label),base$features)
 shown<-0L
 for(f in places) {
  q<-.tm_project(matrix(unlist(f$geometry$coordinates),1))
  if(q[1]>=limits[1]&&q[1]<=limits[2]&&q[2]>=limits[3]&&q[2]<=limits[4]) {
   label<-f$properties$label
   if(identical(names(grDevices::dev.cur()),'pdf'))label<-iconv(label,to='ASCII//TRANSLIT',sub='?')
   graphics::text(q[1],q[2],label,col='#657278',cex=.55,pos=3)
   shown<-shown+1L;if(shown>=24L)break
  }
 }
 invisible(NULL)
}
.tm_labels <- function(scale) {
 if(scale$kind=='zero')return('0')
 if(scale$kind=='missing')return(character())
 v<-round(scale$breaks)
 if(any(diff(c(0,v))<=0))for(d in 2:15) {
  v<-signif(scale$breaks,d)
  if(all(diff(c(0,v))>0))break
 }
 b<-vapply(v,function(n)format(n,trim=TRUE,scientific=FALSE,big.mark=','),'')
 c(paste0('0 to < ',b[1]),paste0(utils::head(b,-1),' to < ',utils::tail(b,-1)),paste0('>= ',utils::tail(b,1)))
}

#' @export
#' @noRd
plot.trafficmap_data <- function(x,index=x$initial,region=NULL,...) {
 summary<-traffic_summary(x,index,region);selected<-.tm_select(x,region)
 saved<-graphics::par(no.readonly=TRUE);on.exit(graphics::par(saved),add=TRUE)
 graphics::par(mar=c(3,1,4,1));graphics::plot.new()
 bounds<-.tm_focus(.tm_locations(x,region))
 graphics::plot.window(xlim=bounds[1:2],ylim=bounds[3:4],asp=1,xaxs='i',yaxs='i')
 graphics::rect(graphics::par('usr')[1],graphics::par('usr')[3],graphics::par('usr')[2],graphics::par('usr')[4],col='#f4f3ee',border=NA)
 .tm_static_context(x$basemap,graphics::par('usr'))
 values<-x$values[index,selected$roads];colors<-.tm_colors(values,x$scale);drawn<-list()
 units_per_metre<-graphics::par('pin')[1]*96/diff(graphics::par('usr')[1:2])/6378137
 screen_paths<-lapply(x$roads$geometry,function(g) {
  q<-.tm_project(g)
  cbind((q[,1]-graphics::par('usr')[1])*units_per_metre*6378137,
   (graphics::par('usr')[4]-q[,2])*units_per_metre*6378137)
 })
 scoped<-screen_paths[selected$roads]
 layout<-.tm_view_style(scoped,graphics::par('pin')[1]*96,graphics::par('pin')[2]*96,
  log2(units_per_metre*2*pi*6378137/256))
 paths<-.tm_display_paths(x$roads$geometry[selected$roads],x$opposing[selected$roads],
  units_per_metre,layout$width,layout$gap)
 for(k in seq_along(selected$roads)) {
  q<-paths[[k]]/6378137

  drawn[[k]]<-q
 }
 for(k in seq_along(drawn))graphics::lines(drawn[[k]],col='#ffffff',lwd=layout$width[k]+2*layout$halo[k])
 for(k in seq_along(drawn))graphics::lines(drawn[[k]],col=colors[k],lwd=layout$width[k])
 events<-x$events[selected$events,,drop=FALSE];events<-events[events$frame==index,,drop=FALSE]
 if(nrow(events)) {
  p<-.tm_project(as.matrix(events[,c('lon','lat')]))
  graphics::points(p,pch=23,bg='#c5372d',col='white',cex=1.4,lwd=1.3)
  graphics::text(p,labels='!',col='white',cex=.65,font=2)
 }
 title<-paste(c(x$title,region,x$time_labels[index]),collapse=' | ')
 graphics::title(main=title,cex.main=.9)
 if(length(selected$roads))graphics::legend('topleft',legend=c(.tm_labels(x$scale),'Missing'),
   col=c(if(x$scale$kind=='zero')x$scale$palette[1] else if(x$scale$kind=='missing')character() else x$scale$palette,'#94a3b8'),
   lwd=3,title='Flow limits (rounded)',bg=grDevices::adjustcolor('white',.97),box.col='#dbe2e7',bty='o',cex=.55,ncol=2)
 graphics::mtext(if(length(selected$roads))paste('Flow [',x$unit,'] - logarithmic classes') else 'Reported event starts',side=1,line=.3,cex=.7)
 if(!is.null(x$basemap))graphics::mtext(x$basemap$attribution,side=1,line=1.6,cex=.55,col='#657278')
 invisible(summary)
}

.tm_escape <- function(value) {
 value<-as.character(value)
 for(pair in list(c('&','&amp;'),c('<','&lt;'),c('>','&gt;'),c('"','&quot;'),c("'",'&#39;')))value<-gsub(pair[1],pair[2],value,fixed=TRUE)
 value
}
.tm_table <- function(x,caption) {
 fmt<-function(v)if(is.numeric(v))ifelse(is.na(v),'NA',format(signif(v,6),trim=TRUE,scientific=FALSE,big.mark=',')) else as.character(v)
 rows<-if(nrow(x))vapply(seq_len(nrow(x)),function(i)paste0('<tr>',paste0('<td>',.tm_escape(vapply(x,function(v)fmt(v[i]),'')),'</td>',collapse=''),'</tr>'),'') else ''
 paste0('<div class="table-wrap"><table><caption>',.tm_escape(caption),'</caption><thead><tr>',
  paste0('<th>',.tm_escape(names(x)),'</th>',collapse=''),'</tr></thead><tbody>',paste(rows,collapse=''),'</tbody></table></div>')
}
.tm_asset <- function(name) {
 folder<-getOption('trafficmap.assets',system.file('package_assets',package='bdynets'))
 p<-file.path(folder,name);.tm_assert(file.exists(p),paste('Missing package asset:',name))
 paste(readLines(p,warn=FALSE,encoding='UTF-8'),collapse='\n')
}
.tm_image <- function(draw) {
 file<-tempfile(fileext='.png');on.exit(unlink(file),add=TRUE)
 grDevices::png(file,width=1200,height=800,res=140,type=getOption('bitmapType'))
 tryCatch(draw(),finally=grDevices::dev.off())
 paste0('<img alt="Geographic observations or time trend" src="data:image/png;base64,',jsonlite::base64_enc(readBin(file,'raw',n=file.info(file)$size)),'">')
}
.tm_report <- function(x) {
 figure<-.tm_image(function()graphics::plot(x))
 trend<-traffic_trend(x)
 make_trend<-function(value,title,unit) .tm_image(function() {
  graphics::par(mar=c(4,4,3,2))
  limits<-range(c(0,value[is.finite(value)]));if(diff(limits)==0)limits<-limits+c(0,1)
  graphics::plot(x$times,value,type='l',col='#365c70',lwd=2,xaxt='n',xlab='Source time',
   ylab=unit,ylim=limits,main=title)
  at<-unique(round(seq(1,length(x$times),length.out=min(7,length(x$times)))))
  graphics::axis(1,at=x$times[at],labels=x$time_labels[at],cex.axis=.65)
 })
 flow_trend<-if(nrow(x$roads))paste0('<h4>Flow trend</h4>',
  make_trend(trend$mean,'Flow trend',paste('Mean [',x$unit,']'))) else ''
 event_trend<-if(nrow(x$events))paste0('<h4>Event trend</h4>',
  make_trend(trend$events,'Event trend','Events')) else ''
 s<-traffic_summary(x)
 overview<-data.frame(frames=length(x$times),roads=nrow(x$roads),events=nrow(x$events),
  first_time=x$time_labels[1],stringsAsFactors=FALSE)
 tables<-paste0(.tm_table(overview,'Dataset'),
  .tm_table(s[,c('frame','time','roads','events'),drop=FALSE],'Displayed interval'),
  if(nrow(x$roads))paste0(.tm_table(s[,c('available','missing','zero'),drop=FALSE],'Flow availability'),
   .tm_table(s[,c('minimum','median','mean','p95','maximum'),drop=FALSE],'Flow distribution')) else '')
 paragraph<-function(values)paste0('<p>',.tm_escape(values),'</p>',collapse='')
 paste0('<section id="summary"><h2>Summary</h2><section><h3>All records</h3>',
  figure,flow_trend,event_trend,tables,'</section>',
  '<p>Statistics cover all input records. Means and distributions use finite road values; missing observations are counted separately. Reported events are counted by start interval. Road-value sums are not interpreted as distinct vehicle counts.</p></section>',
  '<section id="preparation"><h2>Preparation</h2>',paragraph(x$preparation),
  '<p>The input contains road_id and longitude/latitude geometry, road_id, time and flow records, and optional event time, lon and lat records. Zero is observed; omitted road-time records and explicit NA are missing. Events belong to half-open frame intervals.</p>',
  '<p>One green-to-red seven-color palette and one log1p algorithm apply to every flow input. Exact thresholds use the global maximum across all times and records and remain fixed during interaction; class labels are rounded for display.</p>',
  '<pre><code>library(bdynets)\nx &lt;- read_traffic("data/input.rds")\nplot(x)\ntraffic_map(x, "map.html")\ntraffic_report(x, "report.html")</code></pre></section>',
  '<section><h2>Fields and scope</h2>',paragraph(x$notes),
  '<p>Units are supplied labels; an omitted unit is shown as unit unspecified. Values and source road coordinates are retained without inferred conversions. Geometry vertices follow travel direction. Optional opposite_id values declare reciprocal relationships between existing road IDs. Without a declaration, unique exact reverse paths or sustained mutual nearest geometry matches are used. Automatic matching uses reverse headings within 15 degrees, proximity within 30 ground metres, and at least 60 percent probe support per segment. Competing alternatives within 2 metres and substantial lateral side changes are ambiguous. Ambiguous records retain source geometry. Explicit partners still need overlapping reversed geometry. Descriptive attributes do not control pairing.</p>',
  '<p>Every input opens at its first frame with all records selected. Region and category controls filter only after an explicit selection. Named region selections match provided labels exactly; unassigned records remain in the all-record scope. The report summarizes all records and shows each applicable flow and event trend.</p>',
  '<p>Display offsets do not represent physical road positions. Existing separation contributes to the gap. Coincident paths use the right side of each directed path as a drawing convention; other paths move apart. Adjacent segment shifts are averaged at vertices. Line width, opposing edge gap and white casing adapt continuously to zoom and local visible road-length density. Panning, zooming and resizing recompute layout. Static figures use 1/96-inch graphics line-width units and interactive maps use pixels. Event locations remain as supplied.</p>',
  '<p>Roads and events both contribute to geographic focus across the supplied times. Reset view uses the central spatial range; Full extent includes all road vertices and event locations in the selected scope. Unknown timezones, event durations and event-to-road assignments are not inferred.</p>',
  '<p>Projected geometry and road bounds are cached. A spatial index limits layout and layer updates to the view plus its density margin. Offscreen layers are detached and reused; all records remain available to statistics, filters, time controls and Full extent. Unchanged views reuse layout and display coordinates. These caches create no additional files.</p>',
  '<p>Roads, events, basemap and road casing have independent visibility controls. Controls are disabled for absent input components. Hiding roads also removes their casing. Layer visibility does not change statistics, record selection, time membership or geographic extents. Road and event popups retain their record information.</p>',
  '<p>Optional context geometry uses one neutral style. Point captions use properties.label. Attribution is supplied with the context; an omitted source is shown as unspecified.</p>',
  if(length(x$provenance))paste0('<details><summary>Source identities</summary><pre>',.tm_escape(jsonlite::toJSON(x$provenance,pretty=TRUE,auto_unbox=TRUE,null='null')),'</pre></details>') else '',
  if(!is.null(x$basemap))paste0('<p>Map context: ',.tm_escape(x$basemap$attribution),'.</p>') else '',
  '<p>Leaflet is distributed under its included BSD license.</p></section>')
}
.tm_document <- function(x,report=FALSE) {
 .tm_assert(inherits(x,'trafficmap_data'),'Expected trafficmap_data')
 roads<-lapply(seq_len(nrow(x$roads)),function(j)list(id=x$roads$road_id[j],region=x$roads$region[j],
   geometry=unname(x$roads$geometry[[j]]),opposing=unname(x$opposing[[j]]),attributes=as.list(x$roads[j,setdiff(names(x$roads),'geometry'),drop=FALSE])))
 payload<-list(layout=.tm_layout_rules,roads=roads,values=x$values,events=x$events,times=as.list(x$times),labels=as.list(x$time_labels),
   intervals=as.list(x$interval_seconds),unit=x$unit,title=x$title,clock=x$clock,initial=x$initial-1L,
   regions=as.list(x$regions),scale=list(kind=x$scale$kind,breaks=as.list(x$scale$breaks),
     maximum=x$scale$maximum,palette=as.list(x$scale$palette),labels=as.list(.tm_labels(x$scale))),basemap=x$basemap)
 encoded<-jsonlite::base64_enc(memCompress(charToRaw(enc2utf8(jsonlite::toJSON(payload,
   auto_unbox=TRUE,na='null',null='null',digits=NA,dataframe='rows',matrix='rowmajor'))),'gzip'))
 replacements<-list('@@TITLE@@'=.tm_escape(x$title),'@@DESCRIPTION@@'=.tm_escape(x$description),
   '@@BODY@@'=if(report).tm_report(x) else '',
   '@@LEAFLET_CSS@@'=.tm_asset('leaflet.css'),'@@STYLE@@'=.tm_asset('style.css'),
   '@@LEAFLET@@'=.tm_asset('leaflet.js'),'@@LICENSE@@'=.tm_asset('LICENSE.leaflet'),
   '@@PAYLOAD@@'=encoded,'@@APP@@'=.tm_asset('map.js'))
 html<-.tm_asset('template.html')
 for(token in names(replacements)) {
  at<-regexpr(token,html,fixed=TRUE)[1];.tm_assert(at>0,paste('Template token is missing',token))
  html<-paste0(substr(html,1,at-1L),replacements[[token]],substring(html,at+nchar(token),nchar(html)))
 }
 html
}
.tm_write <- function(x,file,report) {
 if(is.character(x)&&length(x)==1L)x<-read_traffic(x)
 html<-.tm_document(x,report)
 con<-file(file,'wt',encoding='UTF-8');on.exit(close(con),add=TRUE);writeLines(html,con,useBytes=TRUE)
 invisible(file)
}
#' @export
#' @noRd
traffic_map <- function(x,file) .tm_write(x,file,FALSE)
#' @export
#' @noRd
traffic_report <- function(x,file) .tm_write(x,file,TRUE)
