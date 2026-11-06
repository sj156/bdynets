.bm_bounds <- function(x, region=NULL) {
  focus <- function(coords) {
    z <- .tm_focus(coords)
    c(west=z[1]*180/pi,east=z[2]*180/pi,
      south=(2*atan(exp(z[3]))-pi/2)*180/pi,north=(2*atan(exp(z[4]))-pi/2)*180/pi)
  }
  if(inherits(x,"trafficmap_data")) {
    if(!is.null(region))return(list(focus(.tm_locations(x,region))))
    if(length(x$regions)>1L) {
      out<-lapply(x$regions,function(r)focus(.tm_locations(x,r)))
      if(any(!nzchar(x$roads$region))||any(!nzchar(x$events$region))) {
        y<-x;y$roads<-x$roads[!nzchar(x$roads$region),,drop=FALSE];y$events<-x$events[!nzchar(x$events$region),,drop=FALSE]
        out<-c(out,list(focus(.tm_locations(y))))
      }
      return(out)
    }
    return(list(focus(.tm_locations(x))))
  }
  if(inherits(x,"od_aggregation"))x<-x$nodes
  if(is.list(x)&&!is.data.frame(x)&&!is.null(x$nodes))x<-x$nodes
  if(is.data.frame(x)) {
    x<-as.data.frame(x)
    if(all(c("longitude","latitude")%in%names(x)))x<-as.matrix(x[,c("longitude","latitude")])
    else if(all(c("lon","lat")%in%names(x)))x<-as.matrix(x[,c("lon","lat")])
    else stop("Coordinate tables require longitude/latitude or lon/lat fields.",call.=FALSE)
  }
  .tm_assert(is.matrix(x)&&is.numeric(x)&&ncol(x)==2L&&nrow(x)>0L&&all(is.finite(x)),"Supply coordinates, a traffic object, an OD object or explicit bounds")
  xr<-range(x[,1]);yr<-range(x[,2])
  if(diff(xr)==0||diff(yr)==0)return(list(focus(x)))
  # The margin matches the existing OD viewport convention.
  list(c(west=xr[1]-.045*diff(xr),east=xr[2]+.045*diff(xr),south=yr[1]-.045*diff(yr),north=yr[2]+.045*diff(yr)))
}
.bm_hash <- function(x) {
  p<-tempfile("basemap-identity-");on.exit(unlink(p),add=TRUE)
  writeBin(serialize(x,NULL,version=3),p);unname(tools::md5sum(p))
}
.bm_detail <- function(bounds,road_classes,detail,view_size) {
  .tm_assert(is.numeric(bounds)&&length(bounds)==4L&&all(is.finite(bounds))&&bounds[1]<bounds[2]&&bounds[3]<bounds[4]&&all(abs(bounds[1:2])<=180)&&all(abs(bounds[3:4])<=85.0511),"Bounds must be west,east,south,north within longitude/latitude limits")
  .tm_assert(is.character(detail)&&length(detail)==1L&&!is.na(detail)&&detail%in%c("auto","all"),"detail must be auto or all")
  .tm_assert(is.numeric(view_size)&&length(view_size)==2L&&all(is.finite(view_size))&&all(view_size>0),"view_size must contain positive pixel width and height")
  .tm_assert(is.character(road_classes),"road_classes must use OSM highway strings; missing values mean unknown")
  known<-road_classes[!is.na(road_classes)&nzchar(road_classes)]
  span<-c(diff(bounds[1:2])/360,diff(asinh(tan(bounds[3:4]*pi/180)))/(2*pi))
  zoom<-max(0L,min(19L,as.integer(floor(log2(min(view_size/(256*span)))))))
  standard<-c("motorway","trunk","primary","secondary","tertiary","unclassified","residential","living_street","service","pedestrian","track","bus_guideway","busway","escape","raceway","footway","bridleway","steps","corridor","path","cycleway","construction","proposed","elevator","services","rest_area",paste0(c("motorway","trunk","primary","secondary","tertiary"),"_link"))
  unknown<-anyNA(road_classes)||any(!nzchar(road_classes))||any(!known%in%standard)
  classes<-c("motorway","trunk")
  if(zoom>=8L)classes<-c(classes,"primary")
  if(zoom>=9L)classes<-c(classes,"secondary")
  if(zoom>=10L)classes<-c(classes,"tertiary")
  classes<-c(classes,paste0(classes,"_link"))
  if(zoom>=12L)classes<-c(classes,"residential","unclassified")
  if(zoom>=13L)classes<-c(classes,"living_street","cycleway","bridleway","track")
  classes<-sort(unique(c(classes,known)))
  all_roads<-detail=="all"||unknown||zoom>=14L
  filter<-if(all_roads)'["highway"]' else paste0('["highway"~"^(',paste(classes,collapse="|"),')$"]')
  list(zoom=zoom,classes=if(all_roads)NULL else classes,filter=filter,view_size=unname(view_size),
    rule=if(unknown)"All highway types: input road types are unknown" else if(detail=="all")"All highway types: explicitly requested" else "Scale detail plus every declared input highway type")
}
.bm_query <- function(bounds,timeout,classes=NULL) {
  b<-format(unname(bounds[c(3,1,4,2)]),digits=15,scientific=FALSE,trim=TRUE)
  box<-paste0("(",paste(b,collapse=","),");")
  selection<-if(is.null(classes))paste0('way["highway"]',box) else paste0("(",
    paste0('way["highway"="',classes,'"]',box,collapse=""),");")
  paste0('[out:json][timeout:',min(90L,as.integer(timeout)),'];',selection,'out skel geom;')
}
.bm_response <- function(path) {
  header<-readBin(path,"raw",n=2L)
  con<-if(identical(header,as.raw(c(31,139))))gzfile(path,"rb") else file(path,"rb")
  on.exit(close(con),add=TRUE)
  response<-jsonlite::fromJSON(con,simplifyVector=FALSE)
  .tm_assert(is.list(response)&&is.list(response$elements)&&is.null(response$remark),paste("Incomplete basemap response",response$remark))
  response
}
.bm_request <- function(query,endpoint,timeout) {
  url<-paste0(endpoint,if(grepl('?',endpoint,fixed=TRUE))'&' else '?','data=',utils::URLencode(query,reserved=TRUE))
  temp<-tempfile("osm-response-",fileext=".json");on.exit(unlink(temp),add=TRUE)
  handle<-curl::new_handle(useragent="bdynets/0.1 coordinate-based research map",
    accept_encoding="gzip",timeout=timeout)
  result<-curl::curl_fetch_disk(url,temp,handle=handle)
  if(result$status_code!=200L) {
    lines<-if(file.exists(temp))readLines(temp,n=100L,warn=FALSE) else character()
    relevant<-grepl("runtime error|parse error|dispatcher|timed out|timeout|capacity|busy|error",lines,ignore.case=TRUE)
    detail<-gsub("<[^>]+>"," ",paste(if(any(relevant))lines[relevant] else lines,collapse=" "))
    stop(paste("HTTP",result$status_code,substr(detail,1,1000)),call.=FALSE)
  }
  response<-.bm_response(temp)
  list(data=response,md5=unname(tools::md5sum(temp)),endpoint=endpoint)
}
.bm_one <- function(bounds,cache_dir,offline,refresh,endpoints,timeout,detail,road_classes,view_size) {
  bounds<-as.numeric(bounds);level<-.bm_detail(bounds,road_classes,detail,view_size)
  query<-.bm_query(bounds,timeout,level$classes)
  selector<-if(identical(level$filter,'["highway"]'))"osm-highway-skel-geom-v1" else paste0("osm-scale-highway-v1:",level$filter)
  key<-.bm_hash(list(schema=1L,bounds=bounds,selector=selector))
  path<-file.path(cache_dir,paste0("osm-",key,".rds"))
  if(file.exists(path)&&!refresh) {
    cached<-readRDS(path)
    .tm_assert(is.list(cached)&&identical(cached$schema,"trafficmap-basemap-cache-v1")&&identical(cached$bounds,bounds)&&identical(cached$selector,selector)&&identical(cached$checksum,.bm_hash(cached$basemap)),"Invalid basemap cache; refresh it rather than using altered geometry")
    .tm_basemap(cached$basemap)
    return(cached$basemap)
  }
  .tm_assert(!offline,"No matching basemap cache; initial acquisition needs a network connection")
  failures<-character();answer<-NULL
  for(endpoint in endpoints) {
    trial<-tryCatch(.bm_request(query,endpoint,timeout),error=identity)
    if(!inherits(trial,"error")){answer<-trial;break}
    failures<-c(failures,paste(endpoint,conditionMessage(trial)))
  }
  .tm_assert(!is.null(answer),paste("Unable to obtain coordinate-based basemap:",paste(failures,collapse="; ")))
  features<-list()
  for(e in answer$data$elements) {
    if(!identical(e$type,"way"))next
    .tm_assert(!is.null(e$geometry),"Incomplete OSM way geometry; response was not cached")
    .tm_assert(is.list(e$geometry)&&all(vapply(e$geometry,function(p)is.list(p)&&is.numeric(p$lon)&&length(p$lon)==1L&&is.numeric(p$lat)&&length(p$lat)==1L&&all(is.finite(c(p$lon,p$lat))),logical(1))),"Invalid OSM coordinate; incomplete ways are not joined across gaps")
    g<-do.call(rbind,lapply(e$geometry,function(p)c(p$lon,p$lat)))
    .tm_assert(is.matrix(g)&&nrow(g)>=2L&&ncol(g)==2L&&all(is.finite(g)),"Invalid OSM way geometry")
    features[[length(features)+1L]]<-list(type="Feature",geometry=list(type="LineString",coordinates=unname(g)),properties=list(osm_id=as.character(e$id)))
  }
  base<-list(type="FeatureCollection",features=features,attribution="OpenStreetMap contributors (ODbL)",
    provenance=list(provider="OpenStreetMap via Overpass",endpoint=answer$endpoint,query=query,bounds=unname(bounds),
      detail=detail,query_zoom=level$zoom,highway_classes=level$classes,detail_rule=level$rule,reference_view_size=level$view_size,
      retrieved_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),osm_base=answer$data$osm3s$timestamp_osm_base,
      response_md5=answer$md5,ways=length(features),cache_file=basename(path)))
  .tm_basemap(base)
  dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
  .tm_assert(dir.exists(cache_dir),"Basemap cache directory is unavailable")
  temporary<-tempfile("basemap-cache-",tmpdir=cache_dir);on.exit(unlink(temporary),add=TRUE)
  saveRDS(list(schema="trafficmap-basemap-cache-v1",bounds=bounds,selector=selector,query=query,basemap=base,checksum=.bm_hash(base)),temporary,compress="xz")
  .tm_assert(file.rename(temporary,path),"Unable to save basemap cache")
  base
}

.bm_scopes <- function(boxes,levels) {
  keep<-vapply(seq_along(boxes),function(i) {
    b<-boxes[[i]]
    covered<-vapply(seq_along(boxes),function(j) {
      a<-boxes[[j]]
      contains<-j!=i&&a[1]<=b[1]&&a[2]>=b[2]&&a[3]<=b[3]&&a[4]>=b[4]
      strict<-any(a!=b)||j<i
      detail<-is.null(levels[[j]]$classes)||(!is.null(levels[[i]]$classes)&&all(levels[[i]]$classes%in%levels[[j]]$classes))
      contains&&strict&&detail
    },logical(1))
    !any(covered)
  },logical(1))
  which(keep)
}

#' Acquire an OpenStreetMap background from input coordinates
#' @param x A trafficmap_data or od_aggregation object, coordinate matrix, or table
#'   with longitude/latitude or lon/lat. No dataset name is inspected.
#' @param bounds Optional west,east,south,north bounds, overriding x.
#' @param region Optional exact traffic region; otherwise labelled regions are
#'   requested separately and combined, without querying the space between them.
#' @param cache_dir Visible cache directory, relative to the caller by default.
#' @param offline Read a matching cache without network access.
#' @param refresh Obtain a new response instead of reusing a cache.
#' @param endpoints Overpass endpoints, attempted in order for a complete response.
#' @param timeout Maximum request time in seconds. Server queries are capped at 90 seconds.
#' @param detail `auto` follows a scale hierarchy; `all` requests every highway type.
#' @param road_classes Optional OSM highway values required by the input. Otherwise
#'   traffic objects use roads$road_class. Every declared class is included at any
#'   zoom; unknown, unmapped or absent types on observed roads request all highway types.
#'   OD endpoints do not imply a travelled path or a highway class.
#' @param view_size Reference viewport width and height in CSS pixels for the
#'   Web Mercator zoom estimate. The default is 1024 by 768; it is independent
#'   of dataset name, flow values and units.
#' @details The scale hierarchy follows the road levels used in OpenStreetMap
#'   Carto: motorway/trunk, primary from zoom 8, secondary from 9, tertiary from
#'   10, residential/unclassified from 12, additional local/path types from 13,
#'   and all highway values from 14. Link types follow their parent class.
#'   Declared input types extend the hierarchy; unknown observed-road types
#'   disable background filtering. This selection never filters input roads.
#'   Nested scopes already covered by an equally detailed scope are requested once.
#'   Reference: https://github.com/gravitystorm/openstreetmap-carto/blob/master/style/roads.mss
#' @return Offline GeoJSON context with request bounds, source time and identity.
#' @export
traffic_basemap <- function(x=NULL,bounds=NULL,region=NULL,
  cache_dir=file.path("data","basemap-cache"),offline=FALSE,refresh=FALSE,
  endpoints=c("https://maps.mail.ru/osm/tools/overpass/api/interpreter","https://overpass-api.de/api/interpreter","https://overpass.private.coffee/api/interpreter"),timeout=900,
  detail="auto",road_classes=NULL,view_size=c(1024,768)) {
  .tm_assert(is.character(cache_dir)&&length(cache_dir)==1L&&!is.na(cache_dir)&&nzchar(cache_dir),"cache_dir must be one visible directory path")
  .tm_assert(is.logical(offline)&&length(offline)==1L&&!is.na(offline)&&is.logical(refresh)&&length(refresh)==1L&&!is.na(refresh),"offline and refresh must be logical flags")
  .tm_assert(is.character(endpoints)&&length(endpoints)>0L&&!anyNA(endpoints)&&all(grepl('^https?://',endpoints)),"Supply HTTP(S) Overpass endpoints")
  .tm_assert(is.numeric(timeout)&&length(timeout)==1L&&is.finite(timeout)&&timeout>=1,"timeout must be positive seconds")
  if(is.null(road_classes)) {
    road_classes<-character()
    if(inherits(x,"trafficmap_data")) {
      roads<-x$roads[.tm_select(x,region)$roads,,drop=FALSE]
      if(nrow(roads))road_classes<-if("road_class"%in%names(roads))as.character(roads$road_class) else NA_character_
    }
  }
  boxes<-if(is.null(bounds)).bm_bounds(x,region) else list(bounds)
  levels<-lapply(boxes,.bm_detail,road_classes=road_classes,detail=detail,view_size=view_size)
  boxes<-boxes[.bm_scopes(boxes,levels)]
  parts<-lapply(boxes,.bm_one,cache_dir=cache_dir,offline=offline,refresh=refresh,endpoints=endpoints,timeout=timeout,detail=detail,road_classes=road_classes,view_size=view_size)
  features<-unlist(lapply(parts,function(p)p$features),recursive=FALSE)
  if(is.null(features))features<-list()
  if(length(features))features<-features[!duplicated(vapply(features,function(f)f$properties$osm_id,''))]
  .tm_basemap(list(type="FeatureCollection",features=features,attribution="OpenStreetMap contributors (ODbL)",
    provenance=list(provider="OpenStreetMap via Overpass",requests=lapply(parts,function(p)p$provenance))))
}
.tm_background <- function(x,region=NULL) {
  if(is.null(x$basemap)&&isTRUE(getOption("bdynets.basemap.auto",TRUE))) {
    x$basemap<-traffic_basemap(x,region=region)
    x$provenance$basemap<-x$basemap$provenance
  }
  x
}
