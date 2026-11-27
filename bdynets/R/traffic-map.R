# Geometry and display layout
# Keep each view on one continuous longitude interval before projecting it.
.tm_longitude_center <- function(longitudes) {
  if (!length(longitudes)) return(0)
  values <- sort(unique(longitudes))
  gap <- which.max(diff(c(values, values[1L] + 360)))
  if (gap == length(values)) mean(range(values)) else mean(c(values[gap + 1L], values[gap] + 360))
}

.tm_longitude_near <- function(longitudes, center) {
  longitudes + 360 * floor((center - longitudes + 180) / 360)
}

.tm_project <- function(g, longitude_center = NULL) {
  longitudes <- g[, 1L]
  if (!is.null(longitude_center)) longitudes <- .tm_longitude_near(longitudes, longitude_center)
  cbind(longitudes * pi / 180, log(tan(pi / 4 + g[, 2L] * pi / 360)))
}
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
 center <- .tm_longitude_center(unlist(lapply(geometry, function(g) g[, 1L]), use.names = FALSE))
 segments<-do.call(rbind,lapply(remaining,function(j) {
  g<-.tm_project(geometry[[j]], center)*6378137
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

.tm_road_opposing <- function(x) {
  if (!is.null(x[["opposing"]])) return(x[["opposing"]])
  .tm_opposing(x$roads$geometry, x$roads[["opposite_id"]], x$roads$road_id)
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
.tm_display_paths <- function(geometry,opposing,units_per_metre,width,gap=2.5,longitude_center=NULL) {
 width<-rep_len(width,length(geometry));gap<-rep_len(gap,length(geometry))
 lapply(seq_along(geometry),function(j) {
  q<-.tm_project(geometry[[j]], longitude_center)*6378137
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

.tm_locations <- function(x, region = NULL, complete = FALSE) {
 selected <- .tm_select(x, region)
 geometry <- x$roads$geometry[selected$roads]
 events <- if (length(selected$events)) {
   as.matrix(x$events[selected$events, c("lon", "lat"), drop = FALSE])
 } else matrix(numeric(), ncol = 2L, nrow = 0L)
 coordinates <- rbind(do.call(rbind, geometry), events)
 if (complete) return(coordinates)
 center <- .tm_longitude_center(coordinates[, 1L])
 road_centers <- lapply(geometry, function(g) {
   c(stats::median(.tm_longitude_near(g[, 1L], center)), stats::median(g[, 2L]))
 })
 events[, 1L] <- .tm_longitude_near(events[, 1L], center)
 rbind(do.call(rbind, road_centers), events)
}

.tm_projected_extent <- function(limits) {
  stats::setNames(c(limits[1:2] * 180 / pi, atan(sinh(limits[3:4])) * 180 / pi),
    c("west", "east", "south", "north"))
}

.tm_check_padding <- function(padding) {
  .tm_assert(is.numeric(padding) && length(padding) == 1L && is.finite(padding) && padding >= 0,
    "padding must be one nonnegative number")
}

.tm_check_full_extent <- function(full_extent) {
  .tm_assert(is.logical(full_extent) && length(full_extent) == 1L && !is.na(full_extent),
    "full_extent must be TRUE or FALSE")
}

.tm_pad_projected <- function(xr, yr, padding, minimum_span) {
  span <- max(diff(xr), diff(yr), minimum_span)
  margin <- padding * span
  x_margin <- if (diff(xr) == 0 && margin == 0) minimum_span / 2 else margin
  y_margin <- if (diff(yr) == 0 && margin == 0) minimum_span / 2 else margin
  c(xr + c(-1, 1) * x_margin, yr + c(-1, 1) * y_margin)
}

.tm_focus <- function(coords, full_extent = FALSE, padding = .08) {
 if(!nrow(coords))return(c(-.01,.01,-.01,.01))
 center <- if (full_extent) .tm_longitude_center(coords[, 1L]) else NULL
 projected <- .tm_project(coords, center)
 p <- if (!full_extent && nrow(projected) >= 5L) c(.15,.85) else c(0,1)
 xr <- as.numeric(stats::quantile(projected[,1],p));yr <- as.numeric(stats::quantile(projected[,2],p))
 .tm_pad_projected(xr, yr, padding, minimum_span = .00005)
}

.tm_road_plot_limits <- function(x, region, full_extent = TRUE, padding = .08) {
  .tm_focus(.tm_locations(x, region, complete = full_extent), full_extent, padding)
}

.tm_static_context <- function(base,limits) {
 if(is.null(base))return(invisible(NULL))
 center <- mean(limits[1:2]) * 180 / pi
 for(f in base$features) {
  g<-f$geometry
  if(g$type=='LineString') {
   colour <- if (identical(f$properties$context, 'waterway')) '#a4c5d7' else '#ccd1d3'
   q<-.tm_project(as.matrix(g$coordinates), center);graphics::lines(q,col=colour,lwd=.65)
  } else if(g$type=='Polygon') {
   q<-do.call(rbind,lapply(g$coordinates,function(r)rbind(.tm_project(as.matrix(r), center),c(NA_real_,NA_real_))))
   q<-q[-nrow(q),,drop=FALSE]
   fill <- if (identical(f$properties$context, 'water')) '#c5dce8' else '#e1e5e2'
   graphics::polypath(q[,1],q[,2],col=fill,border=NA,rule='evenodd')
  }
 }
 places<-Filter(function(f)f$geometry$type=='Point'&&!is.null(f$properties$label)&&nzchar(f$properties$label),base$features)
 shown<-0L
 for(f in places) {
  q<-.tm_project(matrix(unlist(f$geometry$coordinates),1), center)
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
.tm_event_layer <- function(events, center = mean(graphics::par('usr')[1:2]) * 180 / pi,
    keep_edges = FALSE) {
  projected <- .tm_project(as.matrix(events[,c('lon','lat'),drop=FALSE]), center)
  if(nrow(events)) {
    graphics::points(projected,pch=23,bg='#c5372d',col='white',cex=1.4,lwd=1.3,xpd=keep_edges)
    graphics::text(projected,labels='!',col='white',cex=.65,font=2,xpd=keep_edges)
  }
  invisible(projected)
}

.tm_map_window <- function(limits) {
 viewport <- .tm_fit_map_viewport(limits, graphics::par("pin"))
 graphics::plot.window(xlim=viewport[1:2],ylim=viewport[3:4],xaxs='i',yaxs='i')
}

.tm_map_canvas <- function(limits,margins=.tm_road_map_margins) {
 .tm_assert(is.numeric(limits)&&length(limits)==4L&&all(is.finite(limits))&&
  limits[1]<limits[2]&&limits[3]<limits[4],'Map limits must be finite and increasing')
 graphics::par(mar=margins,xpd=FALSE);graphics::plot.new()
 .tm_map_window(limits)
 usr<-graphics::par('usr')
 graphics::rect(usr[1],usr[3],usr[2],usr[4],col='#f4f3ee',border=NA)
 invisible(usr)
}

.tm_draw_flow_lines <- function(x,index,selected,keep_edges=FALSE) {
 values<-x$values[index,selected$roads];colors<-.tm_colors(values,x$scale);drawn<-list()
 units_per_metre<-graphics::par('pin')[1]*96/diff(graphics::par('usr')[1:2])/6378137
 center <- mean(graphics::par('usr')[1:2]) * 180 / pi
 screen_paths<-lapply(x$roads$geometry,function(g) {
  q<-.tm_project(g, center)
  cbind((q[,1]-graphics::par('usr')[1])*units_per_metre*6378137,
   (graphics::par('usr')[4]-q[,2])*units_per_metre*6378137)
 })
 scoped<-screen_paths[selected$roads]
 layout<-.tm_view_style(scoped,graphics::par('pin')[1]*96,graphics::par('pin')[2]*96,
  log2(units_per_metre*2*pi*6378137/256))
 paths<-.tm_display_paths(x$roads$geometry[selected$roads],.tm_road_opposing(x)[selected$roads],
  units_per_metre,layout$width,layout$gap,center)
 for(k in seq_along(selected$roads)) {
  q<-paths[[k]]/6378137

  drawn[[k]]<-q
 }
 for(k in seq_along(drawn))graphics::lines(drawn[[k]],col='#ffffff',lwd=layout$width[k]+2*layout$halo[k],xpd=keep_edges)
 for(k in seq_along(drawn))graphics::lines(drawn[[k]],col=colors[k],lwd=layout$width[k],xpd=keep_edges)
 invisible(drawn)
}

plot.trafficmap_data <- function(x,index=NULL,region=NULL,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,...) {
 x<-.tm_road_display(.tm_view(x,mode,reference,threshold,comparison,metric,duration_rule))
 index <- .tm_map_time_index(x,index)
 summary<-traffic_summary(x,index,region)
 .tm_draw_road_map(x,index,region)
 invisible(summary)
}

.tm_draw_road_map <- function(x,index,region,bounds=.tm_road_plot_limits(x,region),
    keep_edges=FALSE,basemap="auto") {
 .tm_assert(!is.null(x$scale),'This metric has no supplied display scale')
 selected<-.tm_select(x,region)
 saved<-graphics::par(no.readonly=TRUE);on.exit(graphics::par(saved),add=TRUE)
 viewport <- .tm_map_canvas(bounds)
 base <- .tm_road_basemap(x,region,viewport,basemap)
 .tm_static_context(base,viewport)
 .tm_draw_flow_lines(x,index,selected,keep_edges)
 if (length(selected$events)) {
  events <- x$events[selected$events,,drop=FALSE]
  .tm_event_layer(events[events$frame==index,,drop=FALSE], keep_edges = keep_edges)
 }
 speed <- .tm_road_kind(x) == "speed"
 label <- .tm_road_label(x)
 title<-paste(c(x$title,region,x$time_labels[index]),collapse=' | ')
 graphics::title(main=title,cex.main=.9)
 if(length(selected$roads))graphics::legend('topleft',legend=c(.tm_labels(x$scale),'Missing'),
   col=c(if(x$scale$kind=='zero')x$scale$palette[1] else if(x$scale$kind=='missing')character() else x$scale$palette,'#94a3b8'),
   lwd=3,title=if(!speed && is.null(x$measurement))'Flow limits (rounded)' else paste(label,'limits'),bg=grDevices::adjustcolor('white',.97),box.col='#dbe2e7',bty='o',cex=.55,ncol=2)
 description <- if (!length(selected$roads)) "Reported event starts" else if (speed) {
  paste(label, "[", x$unit, "] - fixed classes")
 } else if (is.null(x$measurement)) {
  paste("Flow [", x$unit, "] - logarithmic classes")
 } else {
  detail <- switch(x$measurement$mode, change = "- signed classes",
    duration = paste("-", x$measurement$time_alignment), "- supplied scale")
  paste(label, "[", x$unit, "]", detail)
 }
 graphics::mtext(description,side=1,line=.3,cex=.7)
 if(!is.null(base))graphics::mtext(base$attribution,side=1,line=1.6,cex=.55,col='#657278')
 invisible(list(viewport=viewport,basemap=base))
}

# Directed OD maps
.tm_od_timezone <- function(frame) {
  timezone <- frame$timezone
  if (is.null(timezone)) timezone <- attr(frame$start_time, "tzone")
  .tm_display_timezone(if (length(timezone) && nzchar(timezone[1L])) timezone[1L] else "UTC")
}

.tm_od_window_label <- function(frame) {
  timezone <- .tm_od_timezone(frame)
  if (is.null(frame$start_time) && is.null(frame$end_time)) {
    hours <- frame$hour_window
    if (is.null(hours)) return(NULL)
    return(sprintf("all dates pooled over %02d:00-%02d:00 (%s)",
      hours[1L] %% 24, hours[2L] %% 24, timezone))
  }
  .tm_assert(length(frame$start_time) == 1L && length(frame$end_time) == 1L,
    "Prepared OD frame needs both start_time and end_time")
  start <- .tm_od_plot_time(frame$start_time, timezone)
  end <- .tm_od_plot_time(frame$end_time, timezone)
  .tm_assert(end > start, "Prepared OD frame end_time must be after start_time")
  times <- as.numeric(c(start, end))
  clock <- if (all(times %% 60 == 0)) "%H:%M" else "%H:%M:%S"
  local_dates <- as.Date(c(start, end), tz = timezone)
  date_clock <- paste("%Y-%m-%d", clock)
  paste(.tm_format_zoned(start, timezone, date_clock), "to",
    .tm_format_zoned(end, timezone, if (local_dates[1L] == local_dates[2L]) clock else date_clock))
}

.tm_od_frame_index <- function(frames, index, time_label) {
  if (is.null(time_label)) return(.tm_map_index(index, length(frames)))
  .tm_check_time_selector(index, time_label)
  request <- .tm_start_request(time_label)
  hits <- which(vapply(frames, function(frame) {
    if (is.null(request)) return(identical(time_label, .tm_od_window_label(frame)))
    if (is.null(frame$start_time)) return(FALSE)
    zone <- .tm_od_timezone(frame)
    start <- .tm_od_plot_time(frame$start_time, zone)
    .tm_matches_start(as.numeric(start), request, calendar = TRUE, timezone = zone)
  }, logical(1L)))
  .tm_assert(length(hits) == 1L, "time_label must identify exactly one prepared OD window by its start time")
  hits
}

.tm_od_count_scale <- function(counts, breaks = NULL, labels = NULL, palette = NULL, right = TRUE) {
  .tm_assert(is.logical(right) && length(right) == 1L && !is.na(right),
    "flow_style right must be TRUE or FALSE")
  if (is.null(breaks)) {
    breaks <- numeric()
    if (length(counts) && min(counts) < max(counts)) {
      limits <- range(counts)
      cuts <- floor(expm1(seq(log1p(limits[1L]), log1p(limits[2L]), length.out = 8L)[2:7]))
      breaks <- unique(cuts[cuts >= limits[1L] & cuts < limits[2L]])
      if (!right) breaks <- breaks + 1
    }
  }
  .tm_assert(is.numeric(breaks) && all(is.finite(breaks)) && all(diff(breaks) > 0),
    "flow_breaks must be strictly increasing finite numbers")
  if (is.null(labels)) {
    if (!length(breaks)) {
      labels <- if (length(counts)) format(counts[1L], trim = TRUE, big.mark = ",", scientific = FALSE) else "Positive counts"
    } else {
      limits <- format(breaks, trim = TRUE, big.mark = ",", scientific = FALSE)
      first <- paste0(if (right) "<= " else "< ", limits[1L])
      between <- if (right) paste0("> ", utils::head(limits, -1L), " to <= ", limits[-1L]) else
        paste0(utils::head(limits, -1L), " to < ", limits[-1L])
      last <- paste0(if (right) "> " else ">= ", utils::tail(limits, 1L))
      labels <- c(first, if (length(limits) > 1L) between, last)
    }
  }
  if (is.null(palette)) {
    n <- length(breaks) + 1L
    palette <- .tm_palette[round(seq(1L, length(.tm_palette), length.out = n))]
  }
  list(breaks = breaks, labels = labels, palette = palette, right = right)
}

.tm_od_drawable_routes <- function(routes) {
  origin <- as.character(routes$origin_id)
  destination <- as.character(routes$destination_id)
  drawable <- which(routes$flow > 0 & origin != destination)
  routes[drawable, , drop = FALSE]
}

.tm_od_layer_style <- function(routes, nodes, flow_breaks, flow_labels,
    flow_palette, flow_right, single_flow_dashed, curvature, arrow_length,
    node_fill, node_colour, node_size, scale_node_size) {
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

.tm_od_control <- function(start, end, curvature) {
  delta <- end - start
  # A consistent side relative to travel puts reciprocal routes on opposite arcs.
  colMeans(rbind(start, end)) + curvature * c(-delta[2L], delta[1L])
}

.tm_od_bezier <- function(start, control, end, t = seq(0, 1, length.out = 36L)) {
  one <- 1 - t
  cbind(one^2 * start[1] + 2 * one * t * control[1] + t^2 * end[1],
    one^2 * start[2] + 2 * one * t * control[2] + t^2 * end[2])
}

.tm_od_coordinates <- function(routes, nodes, events = NULL) {
  rbind(unname(as.matrix(nodes[, c("longitude", "latitude"), drop = FALSE])),
    unname(as.matrix(routes[, c("origin_longitude", "origin_latitude"), drop = FALSE])),
    unname(as.matrix(routes[, c("destination_longitude", "destination_latitude"), drop = FALSE])),
    if (!is.null(events)) unname(as.matrix(events[, c("lon", "lat"), drop = FALSE])))
}

.tm_od_frame_layout <- function(routes, nodes, all_nodes, events, curvature, padding) {
  coordinates <- .tm_od_coordinates(routes, nodes, events)
  if (!nrow(coordinates)) coordinates <- as.matrix(all_nodes[, c("longitude", "latitude"), drop = FALSE])
  .tm_assert(nrow(coordinates) > 0L && all(is.finite(coordinates)) &&
    all(abs(coordinates[, 1L]) <= 180) && all(abs(coordinates[, 2L]) <= .tm_mercator_limit),
    "Supply finite longitude/latitude coordinates within Web Mercator limits")
  center <- .tm_longitude_center(coordinates[, 1L])
  projected <- .tm_project(coordinates, center)
  origins <- .tm_project(as.matrix(routes[c("origin_longitude", "origin_latitude")]), center)
  destinations <- .tm_project(as.matrix(routes[c("destination_longitude", "destination_latitude")]), center)
  extrema <- lapply(seq_len(nrow(routes)), function(i) {
    start <- origins[i, ]
    end <- destinations[i, ]
    control <- .tm_od_control(start, end, curvature)
    turning <- (start - control) / (start - 2 * control + end)
    .tm_od_bezier(start, control, end, turning[is.finite(turning) & turning > 0 & turning < 1])
  })
  projected <- rbind(projected, do.call(rbind, extrema))
  limits <- .tm_pad_projected(range(projected[, 1L]), range(projected[, 2L]),
    padding, minimum_span = 1e-6 * pi / 180)
  list(limits = limits, center = center)
}

.tm_draw_od_layer <- function(routes, nodes, style,
    legend_position = "topleft", legend = TRUE, legend_horizontal = FALSE,
    center = mean(graphics::par('usr')[1:2]) * 180 / pi, keep_edges = FALSE) {
  saved_clip <- graphics::par(xpd = keep_edges)
  on.exit(graphics::par(saved_clip),add=TRUE)
  curves <- vector("list", nrow(routes))
  origins <- .tm_project(as.matrix(routes[c("origin_longitude", "origin_latitude")]), center)
  destinations <- .tm_project(as.matrix(routes[c("destination_longitude", "destination_latitude")]), center)
  coincident <- origins[, 1] == destinations[, 1] & origins[, 2] == destinations[, 2]
  for (i in which(!coincident)) {
    start <- origins[i, ]
    end <- destinations[i, ]
    control <- .tm_od_control(start, end, style$curvature)
    curves[[i]] <- .tm_od_bezier(start, control, end)
    graphics::lines(curves[[i]],
      col = grDevices::adjustcolor(style$route_colours[i],
        alpha.f = style$route_opacity[i]),
      lwd = style$route_width[i], lty = style$route_type[i], lend = "round")
  }
  projected_nodes <- .tm_project(as.matrix(nodes[, c("longitude", "latitude")]), center)
  graphics::points(projected_nodes, pch = 21, bg = style$node_fill,
    col = style$node_colour, cex = style$node_cex, lwd = 0.8)
  if (any(coincident)) {
    rows <- which(coincident)
    locations <- origins[rows, , drop = FALSE]
    # One concentric ring per pair, centred at its shared endpoint.
    keys <- sprintf("%.17g / %.17g", locations[, 1], locations[, 2])
    levels <- stats::ave(seq_along(rows), keys, FUN = seq_along)
    graphics::points(locations, pch = 1,
      cex = max(style$node_cex) + 0.65 * levels,
      col = grDevices::adjustcolor(style$route_colours[rows], alpha.f = 0.92),
      lwd = style$route_width[rows])
    if (isTRUE(legend)) graphics::mtext("Rings show routes between different nodes at the same location.",
      side = 1, line = 1, cex = 0.62, col = "#56636a")
  }
  usr <- graphics::par("usr"); head_scale <- min(diff(usr[1:2]), diff(usr[3:4]))
  for (i in which(!coincident)) {
    curve <- curves[[i]]
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
    pairs, basemap = NULL,
    basemap_columns = c(longitude = "longitude", latitude = "latitude",
      group = "street_group", class = "street_class"),
    bounds = NULL, padding = 0.045,
    title = "Directed OD flows", subtitle = NULL,
    caption = "Curved arrows connect aggregate endpoints; they are not observed paths.",
    flow_breaks, flow_labels, flow_palette, flow_right,
    single_flow_dashed = TRUE,
    curvature = 0.10, arrow_length = 0.065,
    node_fill = "white", node_colour = "#17324d", node_size = 0.82,
    scale_node_size = FALSE,
    north_arrow = TRUE, scale_bar = TRUE,
    output_file = NULL, width = 11, height = 9, dpi = 220, draw = TRUE,
    events = NULL, extent_mode = "series") {
  .tm_assert(inherits(pairs, "bdynets_od_pairs"), "Supply a prepared OD frame")
  counts <- c(pairs$totals$flow, pairs$routes$flow, pairs$selected_flow, pairs$within_node_flow)
  .tm_assert(all(is.finite(counts) & counts >= 0 &
    abs(counts - round(counts)) <= sqrt(.Machine$double.eps)),
    "Flow counts must be nonnegative integers; weighted OD is unsupported")
  node_data <- pairs$all_nodes
  window_label <- .tm_od_window_label(pairs)
  selected_flow <- pairs$selected_flow
  no_records <- identical(pairs$status, "no_records") ||
    (is.null(pairs$status) && isTRUE(pairs$selected_records == 0))
  routes <- .tm_od_drawable_routes(pairs$routes)
  plot_nodes <- pairs$nodes
  active_pairs <- pairs$active_pairs
  loop_total <- pairs$within_node_flow
  layer_style <- .tm_od_layer_style(routes, plot_nodes, flow_breaks, flow_labels,
    flow_palette, flow_right, single_flow_dashed, curvature, arrow_length,
    node_fill, node_colour, node_size, scale_node_size)

  focused <- extent_mode == "frame" && is.null(bounds)
  margins <- .tm_od_map_margins
  if (focused) {
    layout <- .tm_od_frame_layout(routes, plot_nodes, node_data, events, curvature, padding)
    limits <- layout$limits
    extent <- .tm_projected_extent(limits)
  } else {
    extent <- traffic_map_extent(node_data, bounds = bounds, padding = padding)
    limits <- .tm_project_extent(extent)
  }
  load_basemap <- function(viewport = NULL) {
    context_extent <- if (is.null(viewport)) extent else .tm_projected_extent(viewport)
    traffic_map_base(basemap, .tm_context_coverage(context_extent),
      basemap_columns = basemap_columns, draws = !is.null(viewport))
  }
  center <- if (focused) layout$center else mean(extent[1:2])
  base <- event_overlay <- viewport <- NULL
  draw_map <- function() {
    saved <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(saved), add = TRUE)
    usr <- .tm_map_canvas(limits, margins)
    current_base <- load_basemap(usr)
    .tm_static_context(current_base, usr)
    if (is.null(viewport)) {
      viewport <<- .tm_projected_extent(usr)
      base <<- current_base
    }
    draw_center <- if (focused) center else mean(usr[1:2]) * 180 / pi
    .tm_draw_od_layer(routes, plot_nodes, layer_style, "topleft", legend = TRUE,
      center = draw_center, keep_edges = focused)
    if (isTRUE(north_arrow)) traffic_map_north_arrow()
    if (isTRUE(scale_bar)) traffic_map_scale_bar()
    traffic_map_caption(title, subtitle, caption, window_label)
    if (!is.null(current_base)) graphics::mtext(current_base$attribution, side = 1,
      line = 3.15, cex = 0.55, col = "#657278")
    if (no_records || selected_flow == 0) graphics::mtext(
      if (no_records) "No records in this window" else "0 trips in this window",
      side = 3, line = 0.15, cex = 0.66, col = "#56636a")
    if (!is.null(events)) {
      event_overlay <<- list(events = events, projected = .tm_event_layer(events, draw_center, focused),
        viewport = usr)
    }
  }

  output_file <- traffic_map_output(draw_map, output_file, width, height, dpi, draw)
  if (is.null(viewport)) base <- load_basemap()
  result <- list(nodes = plot_nodes, all_nodes = node_data, routes = routes,
    summary = list(selected_records = pairs$selected_records, selected_flow = selected_flow,
      within_node_flow = loop_total,
      nonloop_flow = selected_flow - loop_total,
      active_nonloop_pairs = active_pairs,
      displayed_pairs = nrow(routes), displayed_flow = sum(routes$flow),
      displayed_nodes = nrow(plot_nodes),
      flow_classes = stats::setNames(
        tabulate(layer_style$class_index, nbins = length(flow_labels)), flow_labels),
      time_window = window_label,
      loops_included_in_summary = isTRUE(pairs$include_loops)),
    basemap = base, extent = extent, viewport = viewport, output_file = output_file)
  if (no_records) result$status <- "no_records" else if (!is.null(pairs$status)) result$status <- pairs$status
  if (!is.null(events)) {
    if (is.null(event_overlay)) {
      projected <- .tm_project(as.matrix(events[, c("lon", "lat"), drop = FALSE]), center)
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
#' Draw one of the prepared OD windows. Distinct nodes that share coordinates
#' appear as rings at their common location.
#'
#' @param nodes Ordered list of prepared OD frames, one frame, or an RDS path.
#' @param index,time_label Select one prepared window, by index or start label.
#' @param flows,node_columns,flow_columns,start_time,end_time,hour_window,timezone,top_n,include_loops,active_nodes_only
#'   Leave preparation arguments at their defaults. If supplied, `timezone`
#'   must agree with the prepared frame.
#' @param basemap Prepared street context, or `NULL` to omit it.
#' @param basemap_columns Optional field mappings for a local grouped line table.
#' @param bounds Optional `c(west,east,south,north)` longitude/latitude extent.
#' @param padding Fractional padding when bounds are derived.
#' @param extent The complete series or the drawn components of the selected frame.
#' @param title,subtitle,caption Map text.
#' @param flow_style Shared map-style list with optional `breaks`, `labels`,
#'   `palette`, `right`, and OD-specific `single_flow_dashed` fields.
#' @param flow_breaks,flow_labels,flow_palette,flow_right Legacy direct flow-style fields.
#' @param single_flow_dashed Whether flow-one routes use dashed lines.
#' @param curvature Curve magnitude as a fraction of line length.
#' @param arrow_length Arrowhead length in inches.
#' @param node_fill,node_colour,node_size Node-symbol styling.
#' @param scale_node_size Whether the mapped node-size column controls symbol size.
#' @param north_arrow,scale_bar Whether to draw the shared map annotations.
#' @param output_file Optional PNG or PDF output path.
#' @param width,height Output dimensions in inches.
#' @param dpi PNG resolution.
#' @param draw Whether to also draw on the current graphics device.
#' @param events Optional timestamped point events overlaid within the OD view.
#' @param event_columns Leave at its default. Supply event coordinates as `lon` and `lat`.
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
    flow_breaks = NULL, flow_labels = NULL, flow_palette = NULL,
    flow_right = TRUE, single_flow_dashed = TRUE, flow_style = list(),
    curvature = 0.10, arrow_length = 0.065,
    node_fill = "white", node_colour = "#17324d", node_size = 0.82,
    scale_node_size = FALSE,
    north_arrow = TRUE, scale_bar = TRUE,
    output_file = NULL, width = 11, height = 9, dpi = 220, draw = TRUE,
    events = NULL, event_columns = NULL, index = NULL, time_label = NULL,
    extent = c("series", "frame")) {
  extent <- match.arg(extent)
  .tm_check_padding(padding)
  args <- as.list(environment())
  if (!is.null(output_file)) .tm_check_map_destination(output_file, nodes, basemap)
  if (is.character(nodes) && length(nodes) == 1L) nodes <- readRDS(nodes)
  single <- inherits(nodes, "bdynets_od_pairs")
  frames <- if (single) list(nodes) else nodes
  .tm_assert(is.list(frames) && length(frames) > 0L &&
    all(vapply(frames, inherits, logical(1L), "bdynets_od_pairs")),
    "Supply a prepared OD frame, ordered list of frames, or its RDS path.")
  selected <- .tm_od_frame_index(frames, index, time_label)
  nodes <- frames[[selected]]
  .tm_assert(all(vapply(frames, function(frame) is.null(frame$unit) ||
    identical(frame$unit, "trips"), logical(1L))),
    "Prepared OD requires integer trip counts in trips")
  conflicts <- c(flows = !is.null(flows), start_time = !is.null(start_time),
    end_time = !is.null(end_time), hour_window = !is.null(hour_window),
    timezone = !missing(timezone) && !identical(timezone, nodes$timezone),
    top_n = !missing(top_n), include_loops = !missing(include_loops),
    active_nodes_only = !missing(active_nodes_only),
    node_columns = !missing(node_columns), flow_columns = !missing(flow_columns),
    event_columns = !is.null(event_columns))
  if (any(conflicts)) {
    stop(sprintf("Prepared OD input already defines routes and stations. Apply %s during OD preparation.",
      paste(names(conflicts)[conflicts], collapse = ", ")), call. = FALSE)
  }
  if (!is.null(events)) {
    .tm_assert(is.data.frame(events) && all(c('lon','lat') %in% names(events)) &&
      all(is.finite(events$lon)) && all(is.finite(events$lat)),
      'Supply prepared event coordinates in lon and lat.')
  }
  args[c('nodes','flows','node_columns','flow_columns','start_time','end_time',
    'hour_window','timezone','top_n','include_loops','active_nodes_only',
    'event_columns','flow_style','index','time_label','extent')] <- NULL
  .tm_validate_flow_style(flow_style,c('breaks','labels','palette','right','single_flow_dashed'))
  style <- utils::modifyList(list(breaks = flow_breaks, labels = flow_labels,
    palette = flow_palette, right = flow_right), flow_style, keep.null = TRUE)
  counts <- unlist(lapply(frames, function(frame) .tm_od_drawable_routes(frame$routes)$flow),
    use.names = FALSE)
  .tm_assert(all(is.finite(counts) & abs(counts - round(counts)) <= sqrt(.Machine$double.eps)),
    "Flow counts must be nonnegative integers; weighted OD is unsupported")
  scale <- .tm_od_count_scale(counts, style$breaks, style$labels, style$palette, style$right)
  args[paste0("flow_", names(scale))] <- scale
  if (!is.null(style$single_flow_dashed)) args$single_flow_dashed <- style$single_flow_dashed
  args$extent_mode <- extent
  use_series <- extent == "series" || (!nrow(nodes$all_nodes) &&
    !nrow(.tm_od_coordinates(.tm_od_drawable_routes(nodes$routes), nodes$nodes, events)))
  if (is.null(bounds) && use_series) {
    coordinates <- lapply(frames, function(frame) frame$all_nodes[, c("longitude", "latitude"), drop = FALSE])
    args$bounds <- traffic_map_extent(do.call(rbind, unname(coordinates)), padding = padding)
  }
  result <- do.call(.tm_od_map_reference, c(list(pairs = nodes), args))
  result$index <- selected
  result$time_label <- result$summary$time_window
  invisible(result)
}

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
.tm_basemap_source <- function(provenance, context) {
 if (!is.null(context) || "basemap" %in% names(provenance)) provenance$basemap <- context
 provenance
}
.tm_image <- function(draw) {
 file<-tempfile(fileext='.png');on.exit(unlink(file),add=TRUE)
 grDevices::png(file,width=1200,height=800,res=140,type=getOption('bitmapType'))
 drawn <- tryCatch(draw(),finally=grDevices::dev.off())
 html <- paste0('<img alt="Geographic observations or time trend" src="data:image/png;base64,',
  jsonlite::base64_enc(readBin(file,'raw',n=file.info(file)$size)),'">')
 list(html=html,drawn=drawn)
}
.tm_report <- function(x,index,region,trend_method,coverage) {
 s<-traffic_summary(x,index,region); selected<-.tm_select(x,region)
 s$time<-x$time_labels[index]
 trend<-traffic_trend(x,region,method=trend_method)
 scope<-if(is.null(region))'All records' else region
 map <- .tm_image(function().tm_draw_road_map(x,index,region))
 base <- map$drawn$basemap
 provenance <- .tm_basemap_source(x$provenance, base$provenance)
 draw_trend<-function(metric) .tm_image(function()graphics::plot(trend,metric=metric,index=index))$html
 metric_code<-if(is.null(x$selected_metric)||x$selected_metric=='flow')'' else paste0(', metric = ',encodeString(x$selected_metric,quote='"'))
 kind <- .tm_road_kind(x)
 label <- .tm_road_label(x)
 frames_file <- paste0(kind, "-frames.rds")
 paragraph<-function(v)paste0('<p>',.tm_escape(v),'</p>',collapse='')
 absolute <- identical(x$clock, "absolute")
 clock_label <- if (absolute) "Absolute time" else "Source clock"
 clock_note <- if (absolute) paste0("Displayed in ", .tm_display_timezone(x$timezone), ".") else
   "Numeric clocks do not establish a timezone."
 paste0('<section><h2>Summary</h2><h3>',.tm_escape(scope),'</h3>',
  '<p>',clock_label,': ',.tm_escape(x$time_labels[1]),' through ',.tm_escape(utils::tail(x$time_labels,1)),
  '. ',.tm_escape(clock_note),' ',if(kind=='flow'&&is.null(x$measurement))'Flow unit: ' else 'Measurement unit: ',.tm_escape(x$unit),'.</p>',
  .tm_table(coverage,'Whole-period coverage'),
  '<p>Road-time cells cover every supplied frame and selected road for the primary measurement. Zero is an observed value. Missing cells include omitted records and explicit NA. Event starts cover the complete supplied period.</p>',
  if(!is.null(x$measurement))paste0('<h3>Measurement definition</h3>',paragraph(x$measurement$description)) else '',
  '<h3>Selected interval</h3>',paragraph(paste('Frame',index,'starts at',x$time_labels[index],
   'and spans',x$interval_seconds[index],'seconds.',if(!is.null(x$measurement$time_alignment))paste('Measurement alignment:',x$measurement$time_alignment) else '')),map$html,
  .tm_table(s[,c('time','roads','events','available','missing','zero'),drop=FALSE],'Selected-interval availability'),
  if(length(selected$roads)).tm_table(s[,c('minimum','median','mean','p95','maximum'),drop=FALSE],if(kind=='flow'&&is.null(x$measurement))'Selected-interval flow distribution' else 'Selected-interval measurement distribution') else '',
  '<p>Distribution statistics describe finite road values in this interval only. They are not whole-period statistics. Link-value sums are not interpreted as distinct vehicle counts.</p>',
  if(length(selected$roads))paste0('<h3>',paste(label,'trend'),'</h3>',draw_trend('mean'),
   paragraph(attr(trend,'definition'))) else '',
  if(length(selected$events))paste0('<h3>Event trend</h3>',draw_trend('events'),
   '<p>Events are counted by reported start in half-open frame intervals. No duration or causal relationship is inferred.</p>') else '',
  '</section><section><h2>Preparation</h2>',paragraph(x$preparation),
  '<p>Use the path to your prepared frames file in the example below.</p>',
  '<pre><code>library(bdynets)\nx &lt;- read_traffic("',frames_file,'")\n', 'plot(x, index = ',index,
  .tm_escape(if(is.null(region))'' else paste0(', region = ',encodeString(region,quote='"'))),
  .tm_escape(metric_code),')\ntraffic_report(x, "report.html", index = ',index,.tm_escape(if(is.null(region))'' else paste0(', region = ',encodeString(region,quote='"'))),.tm_escape(metric_code),', trend_method = &quot;',trend_method,'&quot;)</code></pre></section>',
  '<section><h2>Fields and scope</h2>',paragraph(x$notes),
  '<p>Road geometry contains longitude and latitude vertices in travel order. ',
  if(identical(x$input_type,'speed'))'Speed records use road_id, time and speed. ' else 'Flow records use road_id, time and flow. ',
  'Event records use time, lon and lat. Values and unit labels retain their supplied meanings.</p>',
  if(identical(x$measurement$mode,'change'))'<p>Red denotes increases, blue denotes decreases, and neutral denotes zero change. Missing observations are gray.</p>' else if(kind=='speed')'<p>Speed uses fixed class boundaries in the supplied unit. Missing observations are gray.</p>' else if(is.null(x$measurement))'<p>Flow uses the common seven-color green-to-red log1p scale. Missing observations are gray. Display separation does not change source geometry.</p>' else '<p>Metric values and their definitions are supplied by the source or explicit computation rule. The display scale is supplied separately; no percentile threshold is inferred.</p>',
  if(!is.null(base))paragraph(paste('Map context:',base$attribution)) else '',
  if(length(provenance))paste0('<h3>Source identities</h3><pre>',.tm_escape(jsonlite::toJSON(.tm_provenance_for_json(provenance),pretty=TRUE,auto_unbox=TRUE,null='null')),'</pre>') else '',
  '</section>')
}
.tm_report_document <- function(x, index, region, trend_method, coverage) {
 index <- .tm_map_time_index(x, index)
 body <- .tm_report(x, index, region, trend_method, coverage)
 paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>',
   .tm_escape(x$title),'</title><style>body{max-width:1000px;margin:36px auto;padding:0 22px;font:16px/1.65 system-ui,sans-serif;color:#25364a}h1,h2,h3{line-height:1.25}h2{margin-top:2em}img{display:block;width:100%;height:auto}table{border-collapse:collapse;font-size:14px;width:100%}td,th{padding:8px;border-bottom:1px solid #dbe2e7;text-align:left}caption{text-align:left;font-weight:600;padding:10px 0}.table-wrap{overflow-x:auto;margin:1em 0}pre{background:#f4f6f7;padding:14px;overflow-x:auto;white-space:pre-wrap;overflow-wrap:anywhere}</style></head><body><h1>',
   .tm_escape(x$title),'</h1><p>',.tm_escape(x$description),'</p>',body,'</body></html>')
}
.tm_check_map_destination <- function(output, ...) {
  .tm_assert(is.character(output) && length(output) == 1L && !is.na(output) &&
    nzchar(output), "Supply one map output path.")
  if (!file.exists(output)) return(invisible(NULL))
  destination <- normalizePath(output, winslash = "/", mustWork = TRUE)
  for (input in list(...)) {
    if (is.character(input) && length(input) == 1L && !is.na(input) && file.exists(input)) {
      .tm_assert(!identical(destination, normalizePath(input, winslash = "/", mustWork = TRUE)),
        "The map output path must differ from its input files.")
    }
  }
  invisible(NULL)
}

.tm_publish_map_file <- function(staged, output) {
  .tm_assert(file.rename(staged, output), "Unable to publish the map output file.")
  invisible(output)
}

.tm_write_report_html <- function(html, output) {
  staged <- tempfile("map-", tmpdir = dirname(output))
  on.exit(unlink(staged), add = TRUE)
  writeLines(enc2utf8(html), staged, useBytes = TRUE)
  .tm_publish_map_file(staged, output)
}

# Road map input and readback ---------------------------------------------------
.tm_map_pdf <- function(file, width, height) {
 if (capabilities("aqua")) {
   grDevices::quartz(type = "pdf", file = file, width = width, height = height, bg = "white")
   fonts <- c(ArialUnicodeMS = "/System/Library/Fonts/Supplemental/Arial Unicode.ttf",
     ArialUnicodeMS = "/Library/Fonts/Arial Unicode.ttf",
     `PingFangSC-Regular` = "/System/Library/Fonts/PingFang.ttc",
     `STHeitiSC-Light` = "/System/Library/Fonts/STHeiti Light.ttc")
   installed <- names(fonts)[file.exists(fonts)]
   if (length(installed)) {
     if (!"bdynetsUnicode" %in% names(grDevices::quartzFonts())) {
       grDevices::quartzFonts(bdynetsUnicode = grDevices::quartzFont(rep(installed[1L], 4L)))
     }
     graphics::par(family = "bdynetsUnicode")
   }
 } else if (capabilities("cairo") && nzchar(suppressWarnings(grDevices::grSoftVersion()[["cairo"]]))) {
   grDevices::cairo_pdf(file, width = width, height = height, bg = "white")
 } else {
   grDevices::pdf(file, width = width, height = height, useDingbats = FALSE, bg = "white")
 }
}

.tm_map_device <- function(draw_fun,output_file,width,height,dpi,draw) {
 .tm_assert(is.logical(draw)&&length(draw)==1L&&!is.na(draw),'draw must be TRUE or FALSE')
 if(!is.null(output_file)) {
  .tm_assert(is.character(output_file)&&length(output_file)==1L&&!is.na(output_file)&&nzchar(output_file),
   'output_file must be one path')
  extension<-tolower(tools::file_ext(output_file))
  .tm_assert(extension%in%c('png','pdf'),'Static map output must end in .png or .pdf')
  dir.create(dirname(output_file),recursive=TRUE,showWarnings=FALSE)
  stage<-tempfile('map-',tmpdir=dirname(output_file),fileext=paste0('.',extension))
  on.exit(unlink(stage),add=TRUE)
  if(extension=='png')grDevices::png(stage,width=width,height=height,units='in',res=dpi,bg='white')
  else .tm_map_pdf(stage, width, height)
  tryCatch(draw_fun(),finally=grDevices::dev.off())
  .tm_publish_map_file(stage,output_file)
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

.tm_map_restore_road_window <- function(bounds) {
 graphics::par(mar=.tm_road_map_margins,xpd=NA)
 .tm_map_window(bounds)
 invisible(bounds)
}

.tm_check_road_map_input <- function(x, input_type) {
  label <- if (input_type == "flow") "Flow" else "Speed"
  .tm_assert(inherits(x, "trafficmap_data") &&
    identical(x$input_type, input_type),
    paste0("Supply prepared ", label, " frames (trafficmap_data or its RDS)."))
  times <- x$times
  .tm_assert(is.numeric(times) && is.null(dim(times)) && length(times) > 0L &&
    all(is.finite(times)) && all(diff(times) > 0),
    "Prepared frame times must be strictly increasing numeric seconds.")
  .tm_assert(is.character(x$clock) && length(x$clock) == 1L &&
    x$clock %in% c("source_clock", "absolute"),
    "Prepared road frames must declare clock as source_clock or absolute.")
  widths <- x$interval_seconds
  interval_error <- "Supply one positive interval length in seconds per frame, with no overlap."
  .tm_assert(is.numeric(widths) && is.null(dim(widths)) && length(widths) == length(times) &&
    all(is.finite(widths)) && all(widths > 0), interval_error)
  start <- utils::head(times, -1L)
  duration <- utils::head(widths, -1L)
  next_start <- utils::tail(times, -1L)
  resolution <- .tm_time_resolution(abs(start), duration, abs(next_start))
  .tm_assert(all(start + duration - next_start <= resolution), interval_error)
  .tm_assert(is.data.frame(x$roads) && is.matrix(x$values) &&
    identical(dim(x$values), c(length(times), nrow(x$roads))),
    "Prepared values must have one row per frame and one column per road.")
  .tm_assert(is.character(x$unit) && length(x$unit) == 1L &&
    !is.na(x$unit) && nzchar(x$unit), "Supply the prepared measurement unit.")
  invisible(x)
}

.tm_check_time_selector <- function(index, time_label) {
  .tm_assert(is.character(time_label) && length(time_label) == 1L &&
    !is.na(time_label) && nzchar(time_label), 'time_label must be one nonblank string')
  .tm_assert(is.null(index), 'Supply index or time_label, not both')
}

.tm_map_index <- function(index, n, initial = 1L) {
  if (is.null(index)) index <- if (is.null(initial)) 1L else initial
  .tm_assert(is.numeric(index) && length(index) == 1L && is.finite(index) &&
    index == floor(index) && index >= 1L && index <= n,
    'index must identify one prepared frame')
  as.integer(index)
}

.tm_map_time_index <- function(x,index=NULL,time_label=NULL) {
 if(!is.null(time_label)) {
  .tm_check_time_selector(index, time_label)
  request <- .tm_start_request(time_label)
  calendar <- identical(x$clock, "absolute") || !is.null(x$time_label_format)
  hits <- which(.tm_matches_start(x$times, request, calendar, x$timezone))
  .tm_assert(length(hits)==1L,'time_label must identify exactly one prepared frame')
  index<-hits
 }
 .tm_map_index(index, length(x$times), x$initial)
}

.tm_flow_style <- function(road,flow_style) {
 if(!is.null(flow_style)) .tm_validate_flow_style(flow_style,c('palette','breaks','labels','right'))
 if(length(flow_style)) {
  if(!is.null(flow_style$right)).tm_assert(is.logical(flow_style$right)&&length(flow_style$right)==1L&&!is.na(flow_style$right),'flow_style right must be TRUE or FALSE')
  if(!is.null(flow_style$breaks)).tm_assert(is.numeric(flow_style$breaks)&&all(is.finite(flow_style$breaks))&&all(diff(flow_style$breaks)>0),'flow_style breaks must be finite and strictly increasing')
  if(!is.null(flow_style$labels)).tm_assert(is.character(flow_style$labels)&&!anyNA(flow_style$labels)&&all(nzchar(flow_style$labels)),'flow_style labels must be nonblank strings')
  if(!is.null(flow_style$palette)) {
   rgba<-grDevices::col2rgb(flow_style$palette,alpha=TRUE)
   flow_style$palette<-apply(rgba,2L,function(color) {
    channels<-if(color[4L]==255L)color[1:3] else color
    paste0('#',paste(sprintf('%02x',channels),collapse=''))
   })
  }
  class_override<-any(c('right','breaks')%in%names(flow_style))
  if(class_override) {
   .tm_assert(road$scale$kind%in%c('log1p','classes','zero','missing'),'Custom breaks/right require a classified road scale')
   road$scale$kind<-'classes';if(is.null(road$scale$right))road$scale$right<-FALSE
  }
  for(name in names(flow_style))road$scale[[name]]<-flow_style[[name]]
  if(class_override&&is.null(flow_style$labels)) {
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
 }
 road
}

#' Draw every spatial component of a standard traffic input
#' @export
#' @noRd
traffic_flow_map <- function(input,index=NULL,time_label=NULL,region=NULL,mode='flow',reference=NULL,
 threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,basemap=NULL,
 title=NULL,subtitle=NULL,caption=NULL,flow_style=list(),north_arrow=TRUE,
 scale_bar=TRUE,output_file=NULL,width=11,height=9,dpi=220,draw=TRUE,full_extent=TRUE,padding=.08) {
  .tm_draw_static_road(input, "flow", index, time_label, region, mode, reference,
    threshold, comparison, metric, duration_rule, basemap, title, subtitle, caption,
    flow_style, north_arrow, scale_bar, output_file, width, height, dpi, draw,
    full_extent, missing(basemap), padding)
}

#' Draw a static speed map from prepared Speed frames
#' @export
#' @noRd
traffic_speed_map <- function(input,index=NULL,time_label=NULL,region=NULL,basemap=NULL,
 title=NULL,subtitle=NULL,caption=NULL,north_arrow=TRUE,scale_bar=TRUE,
 output_file=NULL,width=11,height=9,dpi=220,draw=TRUE,full_extent=TRUE,padding=.08) {
  .tm_draw_static_road(input, "speed", index, time_label, region, "flow", NULL,
    NULL, NULL, NULL, NULL, basemap, title, subtitle, caption,
    list(), north_arrow, scale_bar, output_file, width, height, dpi, draw,
    full_extent, missing(basemap), padding)
}

.tm_draw_static_road <- function(input,input_type,index,time_label,region,mode,reference,
 threshold,comparison,metric,duration_rule,basemap,title,subtitle,caption,
 flow_style,north_arrow,scale_bar,output_file,width,height,dpi,draw,full_extent,use_attached,padding) {
 .tm_check_full_extent(full_extent)
 .tm_check_padding(padding)
 if(!is.null(output_file)) .tm_check_map_destination(output_file,input,basemap)
 road<-read_traffic(input)
 .tm_check_road_map_input(road, input_type)
 if(!is.null(title))road$title<-title
 if(!is.null(subtitle)&&nzchar(subtitle))road$title<-paste(.tm_road_title(road),subtitle,sep=' | ')
 if(use_attached)basemap<-road$basemap
 road<-.tm_road_display(.tm_view(road,mode,reference,threshold,comparison,metric,duration_rule))
 index<-.tm_map_time_index(road,index,time_label)
 summary<-traffic_summary(road,index,region)
 road<-.tm_flow_style(road,flow_style)
 bounds <- .tm_road_plot_limits(road, region, full_extent, padding)
 base <- viewport <- NULL
 draw_fun<-function() {
  saved<-graphics::par(no.readonly=TRUE);on.exit(graphics::par(saved),add=TRUE)
  drawn <- .tm_draw_road_map(road,index,region,bounds,keep_edges=full_extent && padding == 0,
    basemap=basemap)
  if (is.null(viewport)) {
   viewport <<- .tm_projected_extent(drawn$viewport)
   base <<- drawn$basemap
  }
  .tm_map_restore_road_window(bounds)
  if(isTRUE(north_arrow)).tm_map_north_arrow()
  if(isTRUE(scale_bar)).tm_map_scale_bar()
  if(!is.null(caption)&&nzchar(caption))graphics::mtext(caption,side=1,line=2.2,cex=.58,col='#56636a')
 }
 output_file<-.tm_map_device(draw_fun,output_file,width,height,dpi,draw)
 road$basemap <- if (is.null(viewport)) .tm_road_basemap(road,region,NULL,basemap) else base
 invisible(list(type=if(identical(road$input_type,'speed'))'speed' else 'flow',
  index=index,time_label=road$time_labels[index],region=region,data=.tm_road_clock_data(road),
  road_summary=summary,basemap=road$basemap,extent=.tm_projected_extent(bounds),
  viewport=viewport,output_file=output_file))
}
#' @export
#' @noRd
traffic_report <- function(x,file,index=NULL,region=NULL,mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,trend_method='mean') {
 .tm_check_map_destination(file,x)
 x <- read_traffic(x)
 coverage <- traffic_coverage(x, region)
 x <- .tm_road_display(.tm_view(x, mode, reference, threshold,
   comparison, metric, duration_rule))
 .tm_write_report_html(.tm_report_document(x, index, region, trend_method, coverage), file)
}

#' @export
#' @noRd
traffic_export <- function(x,output_dir,index=NULL,region=NULL,input_files=character(),mode='flow',reference=NULL,threshold=NULL,comparison=NULL,metric=NULL,duration_rule=NULL,trend_method='mean') {
 if(is.character(x)&&length(x)==1L) {
  if(!length(input_files))input_files<-c(input=x)
 }
 x<-.tm_road_display(.tm_view(read_traffic(x),mode,reference,threshold,comparison,metric,duration_rule))
 index <- .tm_map_time_index(x,index)
 summary<-traffic_summary(x,index,region);selected<-.tm_select(x,region)
 .tm_assert(is.character(output_dir)&&length(output_dir)==1L&&!is.na(output_dir)&&nzchar(output_dir),'output_dir must be one path')
 .tm_assert(is.character(input_files)&&!anyNA(input_files)&&all(file.exists(input_files)&!dir.exists(input_files)),
  'Every input_file must be an existing file')
 target<-path.expand(output_dir)
 .tm_assert(!file.exists(target),'Output directory already exists')
 parent<-dirname(target);dir.create(parent,recursive=TRUE,showWarnings=FALSE)
 .tm_assert(dir.exists(parent),'Output parent directory is unavailable')
 staging<-tempfile('traffic-export-',tmpdir=parent);dir.create(staging)
 on.exit(unlink(staging,recursive=TRUE),add=TRUE)
 images<-list(map=function().tm_draw_road_map(x,index,region))
 field <- .tm_road_kind(x)
 metric <- if (is.null(x$selected_metric)) field else x$selected_metric
 trend_image <- paste0(field, '-trend')
 trend <- traffic_trend(x, region, method=trend_method)
 if(length(selected$roads))images[[trend_image]]<-function()graphics::plot(trend,metric='mean',index=index)
 if(length(selected$events))images[['event-trend']]<-function()graphics::plot(trend,metric='events',index=index)
 map_basemaps <- list()
 for(name in names(images))for(format in c('png','pdf')) {
  image <- paste0(name,'.',format)
  file <- file.path(staging,image)
  if(format=='png')grDevices::png(file,width=2400,height=1800,res=240,type=getOption('bitmapType'))
  else .tm_map_pdf(file, 10, 7.5)
  drawn <- tryCatch(images[[name]](),finally=grDevices::dev.off())
  if (name == 'map') {
   base <- drawn$basemap
   map_basemaps[image] <- list(if (!is.null(base))
    list(attribution=base$attribution,source=base$provenance))
  }
 }
 png_base <- map_basemaps[["map.png"]]
 source <- .tm_basemap_source(x$provenance, png_base$source)
 ids<-selected$roads;ei<-selected$events;ei<-ei[x$events$frame[ei]==index]
 observations <- function(time, values) {
  as.data.frame(setNames(list(x$roads$road_id[ids], rep(time, length(ids)), as.numeric(values)),
    c('road_id', 'time', field)), stringsAsFactors=FALSE)
 }
 frame<-c(list(schema='trafficmap-selection-v1',title=x$title,clock=x$clock,frame=as.integer(index),
  time=x$times[index],timezone=x$timezone,interval_seconds=x$interval_seconds[index],
  unit=x$unit,region=region,roads=x$roads[ids,,drop=FALSE]),
  setNames(list(observations(x$times[index], x$values[index,ids])), field),
  list(events=if(!is.null(x$events))x$events[ei,,drop=FALSE],metric=metric,
    metric_info=x$metric_info[[metric]],scale=x$scale,provenance=source))
 if(!is.null(x$measurement)) {
  frame$measurement<-x$measurement
  frame[[paste0('source_', field)]] <- observations(x$times[index], x$source_values[index,ids])
  if(x$measurement$mode=='change') {
   ref<-x$measurement$reference_frames[index]
   frame[[paste0('reference_', field)]] <- if (is.na(ref)) {
    observations(NA_real_, rep(NA_real_, length(ids)))
   } else {
    observations(x$times[ref], x$source_values[ref,ids])
   }
  } else if(x$measurement$origin=='computed')frame$source_history<-list(times=x$times[seq_len(index)],interval_seconds=x$interval_seconds[seq_len(index)],values=x$source_values[seq_len(index),ids,drop=FALSE])
 }
 saveRDS(frame,file.path(staging,'selected-frame.rds'),compress='xz')
 utils::write.csv(summary,file.path(staging,'summary.csv'),row.names=FALSE,na='NA')
 utils::write.csv(trend,file.path(staging,'trend.csv'),row.names=FALSE,na='NA')
 provenance<-list(schema='trafficmap-export-v1',title=x$title,clock=x$clock,unit=x$unit,
  timezone=x$timezone,selection=list(frame=as.integer(index),time=x$times[index],
    interval_seconds=x$interval_seconds[index],region=region),
  metric=metric,metric_info=x$metric_info[[metric]],trend_method=trend_method,
  trend_definition=attr(trend,'definition'))
 provenance[[paste0(field, '_scale')]] <- x$scale
 provenance$source <- .tm_provenance_for_json(source)
 provenance$map_basemaps <- .tm_provenance_for_json(map_basemaps)
 provenance <- c(provenance, list(basemap_attribution=png_base$attribution,
  selection_scope=paste('Selected road and event records. The', field,
    'scale covers the complete input series. The selection snapshot is an inspection artifact.')))
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
