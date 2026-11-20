# Geometry and display layout
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
 if(!is.null(scale$labels))return(scale$labels)
 if(scale$kind%in%c('change','classes','gradient'))return(scale$labels)
 if(scale$kind=='zero')return('0')
 if(scale$kind=='missing')return(character())
 v<-round(scale$breaks)
 if(any(diff(c(0,v))<=0))for(d in 2:15) {
  v<-signif(scale$breaks,d)
  if(all(diff(c(0,v))>0))break
 }
 b<-vapply(v,function(n)format(n,trim=TRUE,scientific=FALSE,big.mark=','),'')
 middle<-if(length(b)>1L)paste0(utils::head(b,-1),' to < ',utils::tail(b,-1)) else character()
 c(paste0('0 to < ',b[1]),middle,paste0('>= ',utils::tail(b,1)))
}

# Shared point events
# One event symbol for road maps and external OD compositions.
.tm_event_layer <- function(events) {
  projected <- .tm_project(as.matrix(events[,c('lon','lat'),drop=FALSE]))
  if(nrow(events)) {
    graphics::points(projected,pch=23,bg='#c5372d',col='white',cex=1.4,lwd=1.3)
    graphics::text(projected,labels='!',col='white',cex=.65,font=2)
  }
  invisible(projected)
}

.tm_od_event_input <- function(events,mapping,args) {
  events <- .tm_rename_table(events,mapping,'events')
  .tm_assert(all(c('time','lon','lat')%in%names(events)),'OD events need time, lon and lat columns')
  .tm_assert(all(abs(events$lon)<=180)&&all(abs(events$lat)<=85.0511),'Event coordinates must be longitude/latitude degrees')
  if('id'%in%names(events)) .tm_assert(!anyNA(events$id)&&all(nzchar(as.character(events$id)))&&!anyDuplicated(events$id),'Event IDs must be unique and nonempty')
  time <- .od_parse_time(events$time,args$timezone)
  .tm_assert(!anyNA(time),'Event timestamps are invalid')
  keep <- rep(TRUE,nrow(events))
  if(!is.null(args$start_time)) {
    start <- .od_parse_time(args$start_time,args$timezone);end <- .od_parse_time(args$end_time,args$timezone)
    keep <- time>=start & time<end
  } else if(!is.null(args$hour_window)) {
    hour <- as.integer(format(time,'%H',tz=args$timezone))
    a <- args$hour_window[1] %% 24;b <- args$hour_window[2] %% 24
    keep <- if(a<b)hour>=a & hour<b else hour>=a | hour<b
  }
  events[keep,,drop=FALSE]
}

.tm_od_with_events <- function(args,events,mapping,preselected=FALSE) {
  if(!preselected)events <- .tm_od_event_input(events,mapping,args)
  args$events <- events
  do.call(.tm_od_map_reference,args)
}

# Road maps
.tm_map_canvas <- function(limits,margins=c(3,1,4,1)) {
 .tm_assert(is.numeric(limits)&&length(limits)==4L&&all(is.finite(limits))&&
  limits[1]<limits[2]&&limits[3]<limits[4],'Map limits must be finite and increasing')
 graphics::par(mar=margins,xpd=FALSE);graphics::plot.new()
 graphics::plot.window(xlim=limits[1:2],ylim=limits[3:4],asp=1,xaxs='i',yaxs='i')
 usr<-graphics::par('usr')
 graphics::rect(usr[1],usr[3],usr[2],usr[4],col='#f4f3ee',border=NA)
 invisible(usr)
}

.tm_draw_flow_lines <- function(x,index,selected) {
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
 invisible(drawn)
}

#' @method plot trafficmap_data
#' @export
#' @noRd
plot.trafficmap_data <- function(x,index=x$initial,region=NULL,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,...) {
 x<-.tm_view(x,mode,reference,threshold,comparison,metric,duration_rule)
 .tm_assert(!is.null(x$scale),'This metric has no supplied display scale')
 summary<-traffic_summary(x,index,region);selected<-.tm_select(x,region)
 x<-.tm_background(x,region)
 saved<-graphics::par(no.readonly=TRUE);on.exit(graphics::par(saved),add=TRUE)
 bounds<-.tm_focus(.tm_locations(x,region))
 .tm_map_canvas(bounds)
 .tm_static_context(x$basemap,graphics::par('usr'))
 .tm_draw_flow_lines(x,index,selected)
 events<-x$events[selected$events,,drop=FALSE];events<-events[events$frame==index,,drop=FALSE]
 .tm_event_layer(events)
 title<-paste(c(x$title,region,x$time_labels[index]),collapse=' | ')
 graphics::title(main=title,cex.main=.9)
 if(length(selected$roads))graphics::legend('topleft',legend=c(.tm_labels(x$scale),'Missing'),
   col=c(if(x$scale$kind=='zero')x$scale$palette[1] else if(x$scale$kind=='missing')character() else x$scale$palette,'#94a3b8'),
   lwd=3,title=if(is.null(x$measurement))'Flow limits (rounded)' else paste(x$measurement$label,'limits'),bg=grDevices::adjustcolor('white',.97),box.col='#dbe2e7',bty='o',cex=.55,ncol=2)
 graphics::mtext(if(length(selected$roads)) {if(is.null(x$measurement))paste('Flow [',x$unit,'] - logarithmic classes') else paste(x$measurement$label,'[',x$unit,']',if(x$measurement$mode=='change')'- signed classes' else if(x$measurement$mode=='duration')paste('-',x$measurement$time_alignment) else '- supplied scale')} else 'Reported event starts',side=1,line=.3,cex=.7)
 if(!is.null(x$basemap))graphics::mtext(x$basemap$attribution,side=1,line=1.6,cex=.55,col='#657278')
 invisible(summary)
}

# Directed OD maps
.tm_od_layer_style <- function(routes, nodes, flow_breaks, flow_labels,
    flow_palette, flow_right, single_flow_dashed, curvature, arrow_length,
    node_fill, node_colour, node_size, scale_node_size) {
  .tm_assert(is.numeric(flow_breaks) && length(flow_breaks) >= 1L &&
    all(is.finite(flow_breaks)) && all(diff(flow_breaks) > 0),
    "flow_breaks must be strictly increasing finite numbers")
  .tm_assert(is.character(flow_palette) &&
    length(flow_palette) == length(flow_breaks) + 1L &&
    all(!is.na(flow_palette)) && all(nzchar(flow_palette)),
    "flow_palette must contain one more colour than flow_breaks")
  .tm_assert(is.character(flow_labels) && length(flow_labels) == length(flow_palette) &&
    all(!is.na(flow_labels)) && all(nzchar(flow_labels)),
    "flow_labels must contain one label per flow colour")
  .tm_assert(is.numeric(curvature) && length(curvature) == 1L &&
    is.finite(curvature) && curvature >= 0,
    "curvature must be one nonnegative number")
  .tm_assert(is.numeric(arrow_length) && length(arrow_length) == 1L &&
    is.finite(arrow_length) && arrow_length > 0,
    "arrow_length must be one positive number")
  .tm_assert(is.numeric(node_size) && length(node_size) == 1L &&
    is.finite(node_size) && node_size > 0, "node_size must be one positive number")
  .tm_assert(is.logical(flow_right) && length(flow_right) == 1L && !is.na(flow_right),
    "flow_style right must be TRUE or FALSE")
  class_index <- as.integer(cut(routes$flow,
    breaks = c(-Inf, flow_breaks, Inf), right = flow_right, labels = FALSE))
  opacity_palette <- if (length(flow_palette) == 5L) {
    c(0.72, 0.70, 0.85, 0.95, 1)
  } else seq(0.72, 1, length.out = length(flow_palette))
  node_cex <- if (isTRUE(scale_node_size) && nrow(nodes) && max(nodes$size) > 0) {
    0.65 + 1.05 * sqrt(nodes$size / max(nodes$size))
  } else rep(node_size, nrow(nodes))
  list(flow_breaks = flow_breaks, flow_labels = flow_labels,
    flow_palette = flow_palette, flow_right = flow_right, class_index = class_index,
    route_colours = flow_palette[class_index],
    route_opacity = opacity_palette[class_index],
    route_width = ifelse(routes$flow == 1, 0.85, 1.7),
    route_type = ifelse(isTRUE(single_flow_dashed) & routes$flow == 1, 2, 1),
    curvature = curvature, arrow_length = arrow_length,
    node_fill = node_fill, node_colour = node_colour, node_cex = node_cex)
}

.tm_od_bezier <- function(start, control, end, n = 36L) {
  t <- seq(0, 1, length.out = n); one <- 1 - t
  cbind(one^2 * start[1] + 2 * one * t * control[1] + t^2 * end[1],
    one^2 * start[2] + 2 * one * t * control[2] + t^2 * end[2])
}

.tm_draw_od_layer <- function(routes, nodes, style,
    legend_position = "topleft", legend = TRUE, legend_horizontal = FALSE) {
  saved_clip <- graphics::par(xpd=FALSE)
  on.exit(graphics::par(saved_clip),add=TRUE)
  curves <- vector("list", nrow(routes))
  if (nrow(routes)) for (i in seq_len(nrow(routes))) {
    endpoints <- .tm_project(matrix(c(routes$origin_longitude[i],
      routes$origin_latitude[i], routes$destination_longitude[i],
      routes$destination_latitude[i]), ncol = 2, byrow = TRUE))
    delta <- endpoints[2, ] - endpoints[1, ]; distance <- sqrt(sum(delta^2))
    if (!is.finite(distance) || distance <= 0) next
    direction <- if (routes$origin_id[i] < routes$destination_id[i]) 1 else -1
    control <- colMeans(endpoints) + direction * style$curvature * c(-delta[2], delta[1])
    curves[[i]] <- .tm_od_bezier(endpoints[1, ], control, endpoints[2, ])
    graphics::lines(curves[[i]],
      col = grDevices::adjustcolor(style$route_colours[i],
        alpha.f = style$route_opacity[i]),
      lwd = style$route_width[i], lty = style$route_type[i], lend = "round")
  }
  projected_nodes <- .tm_project(as.matrix(nodes[, c("longitude", "latitude")]))
  graphics::points(projected_nodes, pch = 21, bg = style$node_fill,
    col = style$node_colour, cex = style$node_cex, lwd = 0.8)
  usr <- graphics::par("usr"); head_scale <- min(diff(usr[1:2]), diff(usr[3:4]))
  for (i in seq_along(curves)) {
    curve <- curves[[i]]
    if (is.null(curve)) next
    tip_index <- nrow(curve) - 4L; tail_index <- nrow(curve) - 8L
    direction <- curve[tip_index, ] - curve[tail_index, ]
    direction <- direction / sqrt(sum(direction^2))
    route_span <- sqrt(sum((curve[nrow(curve), ] - curve[1, ])^2))
    head_length <- min((style$arrow_length / 6.84) * head_scale, 0.16 * route_span)
    half_width <- 0.48 * head_length
    tip <- curve[tip_index, ]; base_center <- tip - direction * head_length
    perpendicular <- c(-direction[2], direction[1])
    triangle <- rbind(tip, base_center + perpendicular * half_width,
      base_center - perpendicular * half_width)
    graphics::polygon(triangle[, 1], triangle[, 2],
      col = grDevices::adjustcolor(style$route_colours[i],
        alpha.f = max(0.86, style$route_opacity[i])), border = NA)
  }
  graphics::par(saved_clip)
  if (nrow(routes) && isTRUE(legend)) graphics::legend(legend_position,
    legend = style$flow_labels, col = style$flow_palette,
    lwd = if (legend_horizontal) 5.5 else 3,
    horiz = legend_horizontal, cex = if (legend_horizontal) 0.65 else 0.55,
    seg.len = if (legend_horizontal) 1.3 else 2,
    x.intersp = if (legend_horizontal) 0.65 else 1,
    ncol = if (legend_horizontal) 1 else 2,
    bty = if (legend_horizontal) "n" else "o",
    bg = if (legend_horizontal) "white" else grDevices::adjustcolor("white", 0.97),
    box.col = "#dbe2e7", title = "Trips per directed OD pair")
  invisible(curves)
}

.tm_od_map_reference <- function(
    nodes = NULL, flows = NULL, basemap = "auto",
    node_columns = c(id = "node_id", longitude = "longitude",
      latitude = "latitude", size = "member_weight"),
    flow_columns = c(origin = "origin_id", destination = "destination_id",
      count = "flow", time = "time"),
    basemap_columns = c(longitude = "longitude", latitude = "latitude",
      group = "street_group", class = "street_class"),
    start_time = NULL, end_time = NULL, hour_window = NULL,
    timezone = "UTC", top_n = Inf,
    include_loops = FALSE, active_nodes_only = TRUE,
    bounds = NULL, padding = 0.045,
    title = "Directed OD flows", subtitle = NULL,
    caption = "Curved arrows connect aggregate endpoints; they are not observed paths.",
    flow_breaks = c(1, 3, 5, 7),
    flow_labels = c("1", "2-3", "4-5", "6-7", ">= 8"),
    flow_palette = c("#69afe0", "#a1c181", "#e9c46a", "#f4a261", "#e76f51"),
    flow_right = TRUE,
    single_flow_dashed = TRUE,
    curvature = 0.10, arrow_length = 0.065,
    node_fill = "white", node_colour = "#17324d", node_size = 0.82,
    scale_node_size = FALSE,
    north_arrow = TRUE, scale_bar = TRUE,
    output_file = NULL, width = 11, height = 9, dpi = 220, draw = TRUE,
    pairs = NULL, events = NULL) {
  if (!is.null(pairs)) {
    .tm_assert(inherits(pairs, "bdynets_od_pairs"),
      "pairs must be the object returned by traffic_od_pairs()")
    .tm_assert(is.null(nodes) && is.null(flows),
      "Supply either pairs or nodes/flows, not both")
    node_data <- pairs$all_nodes
    window_label <- pairs$time_window
    selected_flow <- pairs$selected_flow
    routes <- pairs$routes
    plot_nodes <- pairs$nodes
    totals <- pairs$totals
    active_pairs <- pairs$active_pairs
    loop_total <- pairs$within_node_flow
  } else {
    selection <- traffic_od_select(nodes = nodes, flows = flows,
      node_columns = node_columns, flow_columns = flow_columns,
      start_time = start_time, end_time = end_time, hour_window = hour_window,
      timezone = timezone)
    node_data <- selection$nodes
    window_label <- selection$time_window
    selected_flow <- sum(selection$selected$flow)
    pairs <- traffic_od_pairs(selection, top_n = top_n, include_loops = include_loops,
      active_nodes_only = active_nodes_only)
    routes <- pairs$routes
    plot_nodes <- pairs$nodes
    totals <- pairs$totals
    active_pairs <- pairs$active_pairs
    loop_total <- pairs$within_node_flow
  }
  extent <- traffic_map_extent(plot_nodes, bounds = bounds, padding = padding)
  base <- traffic_map_base(basemap, extent, basemap_columns = basemap_columns,
    draws = isTRUE(draw) || !is.null(output_file))
  layer_style <- .tm_od_layer_style(routes, plot_nodes, flow_breaks, flow_labels,
    flow_palette, flow_right, single_flow_dashed, curvature, arrow_length,
    node_fill, node_colour, node_size, scale_node_size)

  event_overlay <- NULL
  draw_map <- function() {
    saved <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(saved), add = TRUE)
    # Share the viewport across the OD and event layers before restoring the device state.
    canvas <- traffic_map_canvas(extent, base, c(3.6, 1.2, 5.1, 1.2))
    .tm_draw_od_layer(routes, plot_nodes, layer_style, "topleft", legend = TRUE)
    if (isTRUE(north_arrow)) traffic_map_north_arrow()
    if (isTRUE(scale_bar)) traffic_map_scale_bar()
    traffic_map_caption(title, subtitle, caption, window_label)
    if (!is.null(events)) {
      event_overlay <<- list(events = events, projected = .tm_event_layer(events),
        viewport = canvas$usr)
    }
  }

  output_file <- traffic_map_output(draw_map, output_file, width, height, dpi, draw)
  result <- list(nodes = plot_nodes, all_nodes = node_data, routes = routes,
    summary = list(selected_flow = selected_flow,
      within_node_flow = loop_total,
      nonloop_flow = sum(totals$flow) - loop_total,
      active_nonloop_pairs = active_pairs,
      displayed_pairs = nrow(routes), displayed_flow = sum(routes$flow),
      displayed_nodes = nrow(plot_nodes),
      flow_classes = stats::setNames(
        tabulate(layer_style$class_index, nbins = length(flow_labels)), flow_labels),
      time_window = window_label,
      loops_included_in_summary = isTRUE(include_loops)),
    basemap = base, output_file = output_file)
  if (!is.null(events)) {
    if (is.null(event_overlay)) {
      projected <- .tm_project(as.matrix(events[, c("lon", "lat"), drop = FALSE]))
      event_overlay <- list(events = events, projected = projected, viewport = NULL)
    }
    result$event_overlay <- event_overlay
  }
  invisible(result)
}

.tm_validate_flow_style <- function(flow_style, allowed) {
  fields <- names(flow_style)
  .tm_assert(is.list(flow_style) &&
    (!length(flow_style) ||
      (!is.null(fields) && !anyNA(fields) && all(nzchar(fields)) && !anyDuplicated(fields))),
    "flow_style must be a uniquely named list")
  .tm_assert(!length(setdiff(fields, allowed)), "Unknown flow_style field")
}


#' Draw stations and directed origin-destination flows
#'
#' Input column names are supplied through mappings, so unrelated columns are
#' ignored and the function is not tied to Divvy data.  A grouped line table is
#' converted to trafficmap's GeoJSON basemap format before it is drawn.
#' For prepared pairs, apply time windows, route limits, loop and station
#' selection, and column mappings during OD preparation. `timezone` applies to
#' external events when drawing prepared pairs.
#'
#' @param nodes Node table, CSV/RDS path, a named OD list containing `nodes`
#'   and `flows`, or a `traffic_od_data`, `od_aggregation` or prepared
#'   `bdynets_od_pairs` object.
#' @param flows Flow table or CSV/RDS path; omit when nodes is an OD object.
#' @param basemap Defaults to `NULL` for a map without street context. Use
#'   `"auto"` or a cache settings list to obtain context while drawing, or
#'   supply a GeoJSON FeatureCollection, grouped line table, or CSV/RDS path.
#' @param node_columns Mappings for `id`, `longitude`, `latitude`, optional `size`.
#' @param flow_columns Mappings for `origin`, `destination`, optional `count`, `time`.
#' @param basemap_columns Mappings for line `longitude`, `latitude`, `group`,
#'   and optional `class`.
#' @param start_time,end_time Optional half-open timestamp window.
#' @param hour_window Optional two-element clock-hour window such as `c(0,3)`.
#'   It pools the matching hours across every date and is mutually exclusive
#'   with `start_time` and `end_time`.
#' @param timezone IANA timezone used to parse timestamps.
#' @param top_n Maximum nonloop OD pairs drawn; use `Inf` for all.
#' @param include_loops Whether within-node flows are retained in the summary.
#' @param active_nodes_only Whether to draw only endpoints of displayed routes.
#' @param bounds Optional `c(west,east,south,north)` longitude/latitude extent.
#' @param padding Fractional padding when bounds are derived from nodes.
#' @param title,subtitle,caption Map text.
#' @param flow_style Shared map-style list with optional `breaks`, `labels`,
#'   `palette`, `right`, and OD-specific `single_flow_dashed` fields.
#' @param flow_breaks,flow_labels,flow_palette,flow_right Legacy direct flow-style fields.
#' @param single_flow_dashed Whether flow-one routes use dashed lines.
#' @param curvature Curve magnitude as a fraction of line length.
#' @param arrow_length Arrowhead length in inches.
#' @param node_fill,node_colour,node_size Station-symbol styling.
#' @param scale_node_size Whether the mapped node-size column controls symbol size.
#' @param north_arrow,scale_bar Whether to draw the shared map annotations.
#' @param output_file Optional PNG or PDF output path.
#' @param width,height Output dimensions in inches.
#' @param dpi PNG resolution.
#' @param draw Whether to also draw on the current graphics device.
#' @param events Optional timestamped point events overlaid within the OD view.
#' @param event_columns Mappings for event fields including `time`, `lon` and `lat`.
#' @return Invisibly, standardized nodes, plotted routes, summary and basemap.
#' @export
#' @noRd
traffic_od_map <- function(
    nodes, flows = NULL, basemap = NULL,
    node_columns = c(id = "node_id", longitude = "longitude",
      latitude = "latitude", size = "member_weight"),
    flow_columns = c(origin = "origin_id", destination = "destination_id",
      count = "flow", time = "time"),
    basemap_columns = c(longitude = "longitude", latitude = "latitude",
      group = "street_group", class = "street_class"),
    start_time = NULL, end_time = NULL, hour_window = NULL,
    timezone = "UTC", top_n = Inf,
    include_loops = FALSE, active_nodes_only = TRUE,
    bounds = NULL, padding = 0.045,
    title = "Directed OD flows", subtitle = NULL,
    caption = "Curved arrows connect aggregate endpoints; they are not observed paths.",
    flow_breaks = c(1, 3, 5, 7),
    flow_labels = c("1", "2-3", "4-5", "6-7", ">= 8"),
    flow_palette = c("#69afe0", "#a1c181", "#e9c46a", "#f4a261", "#e76f51"),
    flow_right = TRUE, single_flow_dashed = TRUE, flow_style = list(),
    curvature = 0.10, arrow_length = 0.065,
    node_fill = "white", node_colour = "#17324d", node_size = 0.82,
    scale_node_size = FALSE,
    north_arrow = TRUE, scale_bar = TRUE,
    output_file = NULL, width = 11, height = 9, dpi = 220, draw = TRUE,
    events = NULL, event_columns = NULL) {
  args <- as.list(environment())
  .tm_validate_flow_style(flow_style,c('breaks','labels','palette','right','single_flow_dashed'))
  style_targets<-c(breaks='flow_breaks',labels='flow_labels',palette='flow_palette',
    right='flow_right',single_flow_dashed='single_flow_dashed')
  for(name in names(flow_style))args[[style_targets[[name]]]]<-flow_style[[name]]
  args$flow_style<-NULL
  if(inherits(nodes,'traffic_od_data')) {
    .tm_assert(is.null(flows),'When nodes is a traffic_od_data object, omit flows')
    if(missing(node_columns))args$node_columns <- nodes$node_columns
    if(missing(flow_columns))args$flow_columns <- nodes$flow_columns
    if(missing(timezone))args$timezone <- nodes$timezone
    args$flows <- nodes$flows;args$nodes <- nodes$nodes
  } else if(inherits(nodes,'od_aggregation')) {
    .tm_assert(is.null(flows),'When nodes is an od_aggregation object, omit flows')
    args$flows <- nodes$flows;args$nodes <- nodes$nodes
  } else if(inherits(nodes,'bdynets_od_pairs')) {
    .tm_assert(is.null(flows),'When nodes is a bdynets_od_pairs object, omit flows')
    conflicts <- c(start_time = !is.null(start_time), end_time = !is.null(end_time),
      hour_window = !is.null(hour_window), top_n = !missing(top_n),
      include_loops = !missing(include_loops), active_nodes_only = !missing(active_nodes_only),
      node_columns = !missing(node_columns), flow_columns = !missing(flow_columns))
    if (any(conflicts)) {
      stop(sprintf("Prepared OD input already defines routes and stations. Apply %s during OD preparation.",
        paste(names(conflicts)[conflicts], collapse = ", ")), call. = FALSE)
    }
    args$pairs<-nodes;args$nodes<-NULL;args$flows<-NULL
  } else if(is.list(nodes)&&all(c('nodes','flows')%in%names(nodes))) {
    .tm_assert(is.null(flows),'When nodes is a named OD list, omit flows')
    .tm_assert(is.null(nodes[['roads']])&&is.null(nodes[['flow']]),
      'Flow and OD inputs are separate; pass prepared road frames to traffic_flow_map()')
    bundle<-nodes
    if(missing(timezone)&&is.character(bundle$timezone)&&length(bundle$timezone)==1L)
      args$timezone<-bundle$timezone
    if(missing(title)&&is.character(bundle$title)&&length(bundle$title)==1L)
      args$title<-bundle$title
    args$flows<-bundle$flows;args$nodes<-bundle$nodes
  }
  if(is.null(args$pairs)) {
    input <- .tm_od_input(args$nodes,args$flows,args$node_columns,args$flow_columns,missing(node_columns),missing(flow_columns))
    args$nodes <- input$nodes;args$flows <- input$flows
    args$node_columns <- input$node_columns;args$flow_columns <- input$flow_columns
  }
  # Normalize factor coordinates in table basemaps without changing acquisition.
  if(!is.null(basemap)&&!(is.character(basemap)&&length(basemap)==1L&&basemap=='auto')&&
      !(is.list(basemap)&&(identical(basemap$type,'FeatureCollection')||
        any(c('mode','cache_dir','offline','refresh')%in%names(basemap))))) {
    b <- .tm_read_table(basemap,'basemap')
    for(key in c('longitude','latitude')) {
      col <- unname(basemap_columns[key])
      if(length(col)&&!is.na(col)&&col%in%names(b))b[[col]] <- .tm_numeric(b[[col]],paste('Basemap',key))
    }
    args$basemap <- b
  }
  args[c('events','event_columns')] <- NULL
  if(!is.null(events))return(.tm_od_with_events(args,events,event_columns))
  do.call(.tm_od_map_reference,args)
}

#' @method plot traffic_od_data
#' @export
#' @noRd
plot.traffic_od_data <- function(x,timezone=x$timezone,...) traffic_od_map(x,timezone=timezone,...)

# Reports and exports
# A raw array's JSON values must retain its dimensions. A plain raw vector
# keeps jsonlite's existing base64 representation.
.tm_provenance_for_json <- function(value) {
  if (is.raw(value) && !is.null(dim(value))) {
    storage.mode(value) <- "integer"
  } else if (is.list(value) && (!is.object(value) || is.data.frame(value))) {
    value[] <- lapply(value, .tm_provenance_for_json)
  }
  value
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
.tm_report <- function(x,index=x$initial,region=NULL,trend_method='mean') {
 s<-traffic_summary(x,index,region); selected<-.tm_select(x,region)
 values<-x$values[,selected$roads,drop=FALSE]; trend<-traffic_trend(x,region,method=trend_method)
 scope<-if(is.null(region))'All records' else region
 coverage<-traffic_coverage(x,region)
 figure<-.tm_image(function()graphics::plot(x,index=index,region=region))
 draw_trend<-function(metric) .tm_image(function()graphics::plot(trend,metric=metric,index=index))
 metric_code<-if(is.null(x$selected_metric)||x$selected_metric=='flow')'' else paste0(', metric = ',encodeString(x$selected_metric,quote='"'))
 mode_code<-if(is.null(x$measurement)||x$measurement$mode=='provided')'' else paste0(', mode = "',x$measurement$mode,'"',if(x$measurement$mode=='change'&&!is.null(x$measurement$reference))paste0(', reference = ',x$measurement$reference) else '')
 if(!is.null(x$measurement)&&x$measurement$mode=='duration'&&x$measurement$origin=='computed')mode_code<-paste0(mode_code,', threshold = ',format(x$measurement$threshold,scientific=FALSE),', comparison = ',encodeString(x$measurement$comparison,quote='"'),', duration_rule = duration_rule')
 rule_code<-if(!is.null(x$measurement)&&identical(x$measurement$origin,'computed')&&x$measurement$mode=='duration')paste0('duration_rule <- ',paste(utils::capture.output(dput(x$measurement$rule)),collapse='\n'),'\n') else ''
 paragraph<-function(v)paste0('<p>',.tm_escape(v),'</p>',collapse='')
 paste0('<section><h2>Summary</h2><h3>',.tm_escape(scope),'</h3>',
  '<p>Source clock: ',.tm_escape(x$time_labels[1]),' through ',.tm_escape(utils::tail(x$time_labels,1)),
  paste0('. Numeric clocks do not establish a timezone. ',if(is.null(x$measurement))'Flow unit: ' else 'Measurement unit: '),.tm_escape(x$unit),'.</p>',
  .tm_table(coverage,'Whole-period coverage'),
  '<p>Road-time cells cover every supplied frame and selected road. Zero is an observed value. Missing cells include omitted records and explicit NA. Event starts cover the complete supplied period.</p>',
  if(!is.null(x$measurement))paste0('<h3>Measurement definition</h3>',paragraph(x$measurement$description)) else '',
  '<h3>Selected interval</h3>',paragraph(paste('Frame',index,'starts at',x$time_labels[index],
   'and spans',x$interval_seconds[index],'seconds.',if(!is.null(x$measurement$time_alignment))paste('Measurement alignment:',x$measurement$time_alignment) else '')),figure,
  .tm_table(s[,c('time','roads','events','available','missing','zero'),drop=FALSE],'Selected-interval availability'),
  if(length(selected$roads)).tm_table(s[,c('minimum','median','mean','p95','maximum'),drop=FALSE],if(is.null(x$measurement))'Selected-interval flow distribution' else 'Selected-interval measurement distribution') else '',
  '<p>Distribution statistics describe finite road values in this interval only. They are not whole-period statistics. Link-value sums are not interpreted as distinct vehicle counts.</p>',
  if(length(selected$roads))paste0('<h3>',if(is.null(x$measurement))'Flow trend' else paste(x$measurement$label,'trend'),'</h3>',draw_trend('mean'),
   paragraph(attr(trend,'definition'))) else '',
  if(length(selected$events))paste0('<h3>Event trend</h3>',draw_trend('events'),
   '<p>Events are counted by reported start in half-open frame intervals. No duration or causal relationship is inferred.</p>') else '',
  '</section><section><h2>Preparation</h2>',paragraph(x$preparation),
  '<pre><code>library(bdynets)\nx &lt;- read_traffic("data/input.rds")\n',.tm_escape(rule_code),'plot(x, index = ',index,
  .tm_escape(if(is.null(region))'' else paste0(', region = ',encodeString(region,quote='"'))),
  .tm_escape(paste0(metric_code,mode_code)),')\ntraffic_report(x, "report.html", index = ',index,.tm_escape(if(is.null(region))'' else paste0(', region = ',encodeString(region,quote='"'))),.tm_escape(paste0(metric_code,mode_code)),', trend_method = &quot;',trend_method,'&quot;)</code></pre></section>',
  '<section><h2>Fields and scope</h2>',paragraph(x$notes),
  '<p>Road geometry contains longitude and latitude vertices in travel order. Flow records use road_id, time and flow; event records use time, lon and lat. Values and unit labels retain their supplied meanings.</p>',
  if(is.null(x$measurement))'<p>Flow uses the common seven-color green-to-red log1p scale. Missing observations are gray. Display separation does not change source geometry.</p>' else if(x$measurement$mode=='change')'<p>Red denotes increases, blue denotes decreases, and neutral denotes zero change. Missing observations are gray. Differences retain the source unit.</p>' else '<p>Metric values and their definitions are supplied by the source or explicit computation rule. The display scale is supplied separately; no percentile threshold is inferred.</p>',
  if(!is.null(x$basemap))paragraph(paste('Map context:',x$basemap$attribution)) else '',
  if(length(x$provenance))paste0('<h3>Source identities</h3><pre>',.tm_escape(jsonlite::toJSON(.tm_provenance_for_json(x$provenance),pretty=TRUE,auto_unbox=TRUE,null='null')),'</pre>') else '',
  '</section>')
}
.tm_document <- function(x,report=FALSE,index=NULL,region=NULL,trend_method='mean') {
 if(report) {
  if(is.null(index))index<-x$initial
  body<-.tm_report(x,index,region,trend_method)
  return(paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>',
   .tm_escape(x$title),'</title><style>body{max-width:1000px;margin:36px auto;padding:0 22px;font:16px/1.65 system-ui,sans-serif;color:#25364a}h1,h2,h3{line-height:1.25}h2{margin-top:2em}img{display:block;width:100%;height:auto}table{border-collapse:collapse;font-size:14px;width:100%}td,th{padding:8px;border-bottom:1px solid #dbe2e7;text-align:left}caption{text-align:left;font-weight:600;padding:10px 0}.table-wrap{overflow-x:auto;margin:1em 0}pre{background:#f4f6f7;padding:14px;overflow-x:auto;white-space:pre-wrap;overflow-wrap:anywhere}</style></head><body><h1>',
   .tm_escape(x$title),'</h1><p>',.tm_escape(x$description),'</p>',body,'</body></html>'))
 }
 .tm_assert(inherits(x,'trafficmap_data'),'Expected trafficmap_data')
 .tm_assert(!is.null(x$scale),'This metric has no supplied display scale')
 trend_method<-match.arg(trend_method,c('mean','provided'));if(trend_method=='provided')traffic_trend(x,method='provided')
 roads<-lapply(seq_len(nrow(x$roads)),function(j)list(id=x$roads$road_id[j],region=x$roads$region[j],
   geometry=unname(x$roads$geometry[[j]]),opposing=unname(x$opposing[[j]]),attributes=as.list(x$roads[j,setdiff(names(x$roads),'geometry'),drop=FALSE])))
 payload<-list(layout=.tm_layout_rules,roads=roads,values=x$values,events=x$events,times=as.list(x$times),labels=as.list(x$time_labels),
   intervals=as.list(x$interval_seconds),unit=x$unit,title=x$title,clock=x$clock,initial=x$initial-1L,
   regions=as.list(x$regions),measurement=x$measurement,trend_method=trend_method,selected_metric=x$selected_metric,provided_trends=if(trend_method=='provided')x$provided_trends else NULL,scale=list(kind=x$scale$kind,breaks=as.list(x$scale$breaks),
     maximum=x$scale$maximum,right=x$scale$right,palette=as.list(x$scale$palette),labels=as.list(.tm_labels(x$scale))),basemap=x$basemap)
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
.tm_write <- function(x,file,report,index=NULL,region=NULL,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,trend_method='mean') {
 if(is.character(x)&&length(x)==1L)x<-read_traffic(x)
 x<-.tm_view(x,mode,reference,threshold,comparison,metric,duration_rule)
 if(report)traffic_summary(x,if(is.null(index))x$initial else index,region)
 x<-.tm_background(x,region)
 html<-.tm_document(x,report,index,region,trend_method)
 con<-file(file,'wt',encoding='UTF-8');on.exit(close(con),add=TRUE);writeLines(html,con,useBytes=TRUE)
 invisible(file)
}

.tm_standard_road_data <- function(input) {
 if(is.character(input)&&length(input)==1L) {
  .tm_assert(file.exists(input)&&!dir.exists(input),'input must be a road RDS file or object')
  input<-readRDS(input)
 }
 if(inherits(input,'trafficmap_data'))return(input)
 .tm_assert(is.list(input)&&(identical(input[['schema']],'bdynets-traffic-input-v1')||
  .tm_untagged_road_input(input)),
  'Flow requires a prepared road frame or road arguments with bdynets-traffic-input-v1 or no schema')
 .tm_assert(all(vapply(input[c('nodes','flows','od')],is.null,logical(1))),
  'Flow and OD inputs are separate; pass prepared OD routes to traffic_od_map()')
 .tm_assert(!is.null(input[['roads']])||!is.null(input[['events']]),
  'The road input has no roads or events')
 args<-input[intersect(names(input),names(formals(traffic_data)))]
 do.call(traffic_data,args)
}

.tm_map_device <- function(draw_fun,output_file,width,height,dpi,draw) {
 .tm_assert(is.logical(draw)&&length(draw)==1L&&!is.na(draw),'draw must be TRUE or FALSE')
 if(!is.null(output_file)) {
  .tm_assert(is.character(output_file)&&length(output_file)==1L&&!is.na(output_file)&&nzchar(output_file),
   'output_file must be one path')
  extension<-tolower(tools::file_ext(output_file))
  .tm_assert(extension%in%c('png','pdf'),'Static map output must end in .png or .pdf')
  dir.create(dirname(output_file),recursive=TRUE,showWarnings=FALSE)
  if(extension=='png')grDevices::png(output_file,width=width,height=height,units='in',res=dpi,bg='white')
  else grDevices::pdf(output_file,width=width,height=height,useDingbats=FALSE,bg='white')
  tryCatch(draw_fun(),finally=grDevices::dev.off())
  output_file<-normalizePath(output_file,mustWork=TRUE)
 }
 if(isTRUE(draw))draw_fun()
 output_file
}

.tm_map_north_arrow <- function() {
 usr<-graphics::par('usr');x<-usr[2]-.055*diff(usr[1:2])
 bottom<-usr[4]-.14*diff(usr[3:4]);top<-usr[4]-.045*diff(usr[3:4])
 graphics::arrows(x,bottom,x,top,length=.12,angle=24,code=2,col='#172033',lwd=1.2,xpd=NA)
 graphics::text(x,usr[4]-.025*diff(usr[3:4]),'N',font=2,cex=.8,col='#172033',xpd=NA)
 invisible(NULL)
}

.tm_map_scale_bar <- function() {
 usr<-graphics::par('usr');radius<-6378137
 latitude<-2*atan(exp(mean(usr[3:4])))-pi/2
 ground_width<-diff(usr[1:2])*radius*cos(latitude)
 if(!is.finite(ground_width)||ground_width<=0)return(invisible(NULL))
 target<-ground_width*.18;power<-10^floor(log10(target));choices<-c(1,2,5,10)*power
 metres<-max(choices[choices<=target]);if(!is.finite(metres))return(invisible(NULL))
 span<-metres/(radius*cos(latitude));x1<-usr[1]+.045*diff(usr[1:2]);x2<-x1+span
 y<-usr[3]+.055*diff(usr[3:4]);tick<-.008*diff(usr[3:4])
 graphics::segments(x1,y,x2,y,lwd=2.2,col='#172033',xpd=NA)
 graphics::segments(c(x1,x2),y-tick,c(x1,x2),y+tick,lwd=1.2,col='#172033',xpd=NA)
 label<-if(metres>=1000)paste0(format(metres/1000,trim=TRUE),' km') else paste0(format(metres,trim=TRUE),' m')
 graphics::text((x1+x2)/2,y+2.5*tick,label,cex=.62,col='#172033',xpd=NA)
 invisible(NULL)
}

.tm_map_restore_road_window <- function(x,region=NULL) {
 graphics::par(mar=c(3,1,4,1),xpd=NA)
 bounds<-.tm_focus(.tm_locations(x,region))
 graphics::plot.window(xlim=bounds[1:2],ylim=bounds[3:4],asp=1,xaxs='i',yaxs='i')
 invisible(bounds)
}

#' @export
#' @noRd
traffic_map_html <- function(x,file,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,trend_method='mean') {
 if(is.character(x)&&length(x)==1L)x<-read_traffic(x)
 if(is.list(x)&&identical(x$schema,'bdynets-traffic-input-v1'))
  x<-.tm_standard_road_data(x)
 .tm_write(x,file,FALSE,mode=mode,reference=reference,threshold=threshold,comparison=comparison,metric=metric,duration_rule=duration_rule,trend_method=trend_method)
}

.tm_map_time_index <- function(x,index=NULL,time_label=NULL) {
 if(is.null(x$time_labels)&&!is.null(x$time_label_format)) {
  label_format <- x$time_label_format
  .tm_assert(is.character(label_format)&&length(label_format)==1L&&!is.na(label_format)&&nzchar(label_format),
   'time_label_format must be one nonblank string')
  # Decode the supplied numeric clock for display without modifying frame times.
  encoded <- as.POSIXct(x$times,origin='1970-01-01',tz='UTC')
  x$time_labels <- format(encoded,format=label_format,tz='UTC')
 }
 .tm_assert(is.null(time_label)||(is.character(time_label)&&length(time_label)==1L&&!is.na(time_label)&&nzchar(time_label)),
  'time_label must be one nonblank string')
 if(!is.null(time_label)) {
  .tm_assert(is.null(index),'Supply index or time_label, not both')
  encoded<-as.POSIXct(x$times,origin='1970-01-01',tz='UTC')
  candidates<-list(stored=x$time_labels,
    full=format(encoded,'%Y-%m-%d %H:%M',tz='UTC'),
    clock=format(encoded,'%H:%M',tz='UTC'))
  hits<-unique(unlist(lapply(candidates,function(value)which(value==time_label)),use.names=FALSE))
  .tm_assert(length(hits)==1L,'time_label must identify exactly one prepared frame')
  index<-hits
  x$time_labels[index]<-time_label
 }
 if(is.null(index))index<-x$initial
 .tm_assert(is.numeric(index)&&length(index)==1L&&!is.na(index)&&is.finite(index)&&
  index==floor(index)&&index>=1L&&index<=length(x$times),'index must identify one prepared frame')
 list(data=x,index=as.integer(index))
}

.tm_apply_flow_style <- function(road,flow_style) {
 if(!length(flow_style))return(road)
 if(!is.null(flow_style$right)).tm_assert(is.logical(flow_style$right)&&length(flow_style$right)==1L&&!is.na(flow_style$right),'flow_style right must be TRUE or FALSE')
 if(!is.null(flow_style$breaks)).tm_assert(is.numeric(flow_style$breaks)&&all(is.finite(flow_style$breaks))&&all(diff(flow_style$breaks)>0),'flow_style breaks must be finite and strictly increasing')
 if(!is.null(flow_style$labels)).tm_assert(is.character(flow_style$labels)&&!anyNA(flow_style$labels)&&all(nzchar(flow_style$labels)),'flow_style labels must be nonblank strings')
 if(!is.null(flow_style$palette))grDevices::col2rgb(flow_style$palette)
 if(any(c('right','breaks')%in%names(flow_style))) {
  .tm_assert(road$scale$kind%in%c('log1p','classes','zero','missing'),'Custom breaks/right require a classified road scale')
  road$scale$kind<-'classes';if(is.null(road$scale$right))road$scale$right<-FALSE
 }
 for(name in names(flow_style))road$scale[[name]]<-flow_style[[name]]
 if(road$scale$kind=='classes'&&is.null(flow_style$labels)) {
  .tm_assert(!is.null(road$scale$breaks)&&length(road$scale$breaks)>0L,'Custom classes require at least one break')
  limits<-vapply(road$scale$breaks,function(v)format(v,trim=TRUE,scientific=FALSE,digits=15,big.mark=','),'')
  middle<-if(length(limits)>1L) {
   if(isTRUE(road$scale$right))paste0('> ',utils::head(limits,-1),' to <= ',utils::tail(limits,-1))
   else paste0(utils::head(limits,-1),' to < ',utils::tail(limits,-1))
  } else character()
  labels<-if(isTRUE(road$scale$right))c(paste0('0 to <= ',limits[1]),middle,paste0('> ',utils::tail(limits,1)))
   else c(paste0('0 to < ',limits[1]),middle,paste0('>= ',utils::tail(limits,1)))
  road$scale$labels<-labels
 }
 expected<-if(road$scale$kind=='zero')1L else if(road$scale$kind=='missing')0L else length(road$scale$palette)
 .tm_assert(length(.tm_labels(road$scale))==expected,'flow_style palette and labels do not agree')
 if(road$scale$kind=='classes').tm_assert(length(road$scale$palette)==length(road$scale$breaks)+1L,'flow_style palette must have one more colour than breaks')
 road
}

#' Draw one prepared road-flow frame
#' @export
#' @noRd
traffic_flow_map <- function(input,index=NULL,time_label=NULL,region=NULL,mode='flow',reference=NULL,
 threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,basemap=NULL,
 title=NULL,subtitle=NULL,caption=NULL,flow_style=list(),north_arrow=TRUE,
 scale_bar=TRUE,output_file=NULL,width=11,height=9,dpi=220,draw=TRUE) {
 road<-.tm_standard_road_data(input)
 if(!is.null(title))road$title<-title
 if(!is.null(subtitle)&&nzchar(subtitle))road$title<-paste(road$title,subtitle,sep=' | ')
 if(is.null(basemap))road$basemap<-NULL
 else if(is.list(basemap)&&any(c('mode','cache_dir','offline','refresh')%in%names(basemap))) {
  spec<-basemap
  road$basemap<-traffic_basemap(road,region=region,
   cache_dir=if(is.null(spec$cache_dir))file.path('data','basemap-cache') else spec$cache_dir,
   offline=if(is.null(spec$offline))FALSE else isTRUE(spec$offline),
   refresh=if(is.null(spec$refresh))FALSE else isTRUE(spec$refresh),road_classes=character())
 } else if(!(is.character(basemap)&&length(basemap)==1L&&basemap=='auto'))road$basemap<-.tm_basemap(basemap)
 if(is.character(basemap)&&length(basemap)==1L&&basemap=='auto'&&is.null(road$basemap)&&
    isTRUE(getOption('bdynets.basemap.auto',TRUE))&&(isTRUE(draw)||!is.null(output_file)))
  road$basemap<-traffic_basemap(road,region=region,road_classes=character())
 road<-.tm_view(road,mode,reference,threshold,comparison,metric,duration_rule)
 selected_time<-.tm_map_time_index(road,index,time_label)
 road<-selected_time$data;index<-selected_time$index
 traffic_summary(road,index,region)
 if(!is.null(flow_style)) .tm_validate_flow_style(flow_style,c('palette','breaks','labels','right'))
 road<-.tm_apply_flow_style(road,flow_style)
 old_auto<-getOption('bdynets.basemap.auto',TRUE);on.exit(options(bdynets.basemap.auto=old_auto),add=TRUE)
 if(is.null(basemap))options(bdynets.basemap.auto=FALSE)
 draw_fun<-function() {
  saved<-graphics::par(no.readonly=TRUE);on.exit(graphics::par(saved),add=TRUE)
  graphics::plot(road,index=index,region=region)
  .tm_map_restore_road_window(road,region)
  if(isTRUE(north_arrow)).tm_map_north_arrow()
  if(isTRUE(scale_bar)).tm_map_scale_bar()
  if(!is.null(caption)&&nzchar(caption))graphics::mtext(caption,side=1,line=2.7,cex=.58,col='#56636a')
 }
 output_file<-.tm_map_device(draw_fun,output_file,width,height,dpi,draw)
 invisible(list(type='flow',index=index,time_label=road$time_labels[index],region=region,data=road,
  road_summary=traffic_summary(road,index,region),basemap=road$basemap,output_file=output_file))
}

#' @export
#' @noRd
traffic_report <- function(x,file,index=NULL,region=NULL,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,trend_method='mean') .tm_write(x,file,TRUE,index,region,mode,reference,threshold,comparison,metric,duration_rule,trend_method)

.tm_export_trend <- function(x,region,field,index,trend_method='mean') {
 trend<-traffic_trend(x,region,method=trend_method);graphics::plot(trend,metric=field,index=index);invisible(trend)
}

#' @export
#' @noRd
traffic_export <- function(x,output_dir,index=NULL,region=NULL,input_files=character(),mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,trend_method='mean') {
 if(is.character(x)&&length(x)==1L) {
  if(!length(input_files))input_files<-c(input=x)
  x<-read_traffic(x)
 }
 x<-.tm_view(x,mode,reference,threshold,comparison,metric,duration_rule)
 .tm_assert(inherits(x,'trafficmap_data'),'Expected trafficmap_data')
 if(is.null(index))index<-x$initial
 summary<-traffic_summary(x,index,region);selected<-.tm_select(x,region)
 .tm_assert(is.character(output_dir)&&length(output_dir)==1L&&!is.na(output_dir)&&nzchar(output_dir),'output_dir must be one path')
 .tm_assert(is.character(input_files)&&!anyNA(input_files)&&all(file.exists(input_files)&!dir.exists(input_files)),
  'Every input_file must be an existing file')
 target<-path.expand(output_dir)
 .tm_assert(!file.exists(target),'Output directory already exists')
 x<-.tm_background(x,region)
 parent<-dirname(target);dir.create(parent,recursive=TRUE,showWarnings=FALSE)
 .tm_assert(dir.exists(parent),'Output parent directory is unavailable')
 staging<-tempfile('traffic-export-',tmpdir=parent);dir.create(staging)
 on.exit(unlink(staging,recursive=TRUE),add=TRUE)
 images<-list(map=function()graphics::plot(x,index=index,region=region))
 if(length(selected$roads))images[['flow-trend']]<-function().tm_export_trend(x,region,'mean',index,trend_method)
 if(length(selected$events))images[['event-trend']]<-function().tm_export_trend(x,region,'events',index,trend_method)
 for(name in names(images))for(format in c('png','pdf')) {
  file<-file.path(staging,paste0(name,'.',format))
  if(format=='png')grDevices::png(file,width=2400,height=1800,res=240,type=getOption('bitmapType'))
  else grDevices::pdf(file,width=10,height=7.5,useDingbats=FALSE)
  tryCatch(images[[name]](),finally=grDevices::dev.off())
 }
 ids<-selected$roads;ei<-selected$events;ei<-ei[x$events$frame[ei]==index]
 frame<-list(schema='trafficmap-selection-v1',title=x$title,clock=x$clock,frame=as.integer(index),
  time=x$times[index],time_label=x$time_labels[index],interval_seconds=x$interval_seconds[index],
  unit=x$unit,region=region,roads=x$roads[ids,,drop=FALSE],
  flow=data.frame(road_id=x$roads$road_id[ids],time=rep(x$times[index],length(ids)),flow=as.numeric(x$values[index,ids])),
  events=x$events[ei,,drop=FALSE],metric=x$selected_metric,metric_info=x$metric_info[[x$selected_metric]],scale=x$scale,
  provenance=x$provenance)
 if(!is.null(x$measurement)) {
  frame$measurement<-x$measurement
  frame$source_flow<-data.frame(road_id=x$roads$road_id[ids],time=rep(x$times[index],length(ids)),flow=as.numeric(x$source_values[index,ids]))
  if(x$measurement$mode=='change') {
   ref<-x$measurement$reference_frames[index]
   frame$reference_flow<-data.frame(road_id=x$roads$road_id[ids],time=rep(if(is.na(ref))NA_real_ else x$times[ref],length(ids)),flow=if(is.na(ref))rep(NA_real_,length(ids)) else as.numeric(x$source_values[ref,ids]))
  } else if(x$measurement$origin=='computed')frame$source_history<-list(times=x$times[seq_len(index)],interval_seconds=x$interval_seconds[seq_len(index)],values=x$source_values[seq_len(index),ids,drop=FALSE])
 }
 saveRDS(frame,file.path(staging,'selected-frame.rds'),compress='xz')
 utils::write.csv(summary,file.path(staging,'summary.csv'),row.names=FALSE,na='NA')
 utils::write.csv(traffic_trend(x,region,method=trend_method),file.path(staging,'trend.csv'),row.names=FALSE,na='NA')
 provenance<-list(schema='trafficmap-export-v1',title=x$title,clock=x$clock,unit=x$unit,
  selection=list(frame=as.integer(index),time=x$times[index],time_label=x$time_labels[index],
    interval_seconds=x$interval_seconds[index],region=region),
  metric=x$selected_metric,metric_info=x$metric_info[[x$selected_metric]],trend_method=trend_method,trend_definition=attr(traffic_trend(x,region,method=trend_method),'definition'),flow_scale=x$scale,source=.tm_provenance_for_json(x$provenance),basemap_attribution=if(is.null(x$basemap))NULL else x$basemap$attribution,
  selection_scope='Selected road and event records; flow scale retains the complete input thresholds. The selection snapshot is an inspection artifact, not a trafficmap-input-v1 bundle.')
 writeLines(jsonlite::toJSON(provenance,auto_unbox=TRUE,pretty=TRUE,na='null',null='null',digits=NA),file.path(staging,'provenance.json'),useBytes=TRUE)
 labels<-names(input_files);if(is.null(labels))labels<-rep('',length(input_files))
 inputs<-data.frame(label=labels,file=basename(input_files),bytes=unname(file.info(input_files)$size),
  md5=unname(tools::md5sum(input_files)),stringsAsFactors=FALSE)
 utils::write.csv(inputs,file.path(staging,'inputs.csv'),row.names=FALSE)
 writeLines(utils::capture.output(utils::sessionInfo()),file.path(staging,'session-info.txt'),useBytes=TRUE)
 files<-sort(list.files(staging))
 utils::write.csv(data.frame(file=files,bytes=unname(file.info(file.path(staging,files))$size),
  md5=unname(tools::md5sum(file.path(staging,files)))),file.path(staging,'checksums.csv'),row.names=FALSE)
 .tm_assert(file.rename(staging,target),'Unable to finalize export directory')
 invisible(normalizePath(target,mustWork=TRUE))
}
