# MOE observations are hourly all-lane entry rates sampled at each minute.
beijing_fields <- c(pair='link_id_from_to',day='day_no',minute='timestamp_in_min',
                    rate='link_volume_in_veh_per_hour_for_all_lanes')
beijing_day_minutes <- 0:1439

beijing_network <- function(network) {
  if(!identical(network$type,'FeatureCollection')||any(network$features$geometry$type!='LineString'))
    stop('The geometry source must contain directed LineString features.')
  roads<-network$features$properties
  roads$road_id<-as.character(roads$edge_index)
  roads$geometry<-network$features$geometry$coordinates
  if(!nrow(roads)||anyNA(roads$road_id)||anyNA(roads$edge_id)||anyDuplicated(roads$road_id)||
     anyDuplicated(roads$edge_id)||!identical(roads$edge_id,paste(roads$from_node_id,roads$to_node_id,sep='->')))
    stop('Road IDs and directed endpoint pairs must be unique and agree.')
  roads
}

beijing_records <- function(records,roads,expected_minutes,fields=beijing_fields,day=10L) {
  if(!setequal(names(fields),names(beijing_fields))||anyNA(fields)||anyDuplicated(fields)||
     any(!fields%in%names(records)))stop('Map each required MOE role to one distinct source field.')
  records<-as.data.frame(records)[unname(fields[names(beijing_fields)])]
  names(records)<-names(beijing_fields)
  if(!nrow(records)||anyNA(records[c('pair','day')])||!is.numeric(records$minute)||!is.numeric(records$rate)||
     any(!is.finite(records$minute))||any(records$minute!=trunc(records$minute))||
     any(!is.finite(records$rate)|records$rate<0)||any(!nzchar(as.character(records$pair))))
    stop('MOE rows need a directed pair, a whole source minute and a nonnegative finite hourly rate.')
  if(any(records$day!=day))stop('The original-format CSV must contain only the example day.')
  if(!length(expected_minutes)||anyNA(expected_minutes)||any(diff(expected_minutes)!=1))
    stop('Declare the consecutive source-minute domain.')
  if(anyDuplicated(records[c('pair','minute')]))stop('Duplicate directed-pair and minute key.')
  pairs<-unique(as.character(records$pair))
  column<-match(records$minute,expected_minutes)
  if(anyNA(column)||nrow(records)!=length(pairs)*length(expected_minutes))
    stop('Missing or unexpected directed-pair and minute record.')
  values<-matrix(NA_real_,length(pairs),length(expected_minutes))
  values[cbind(match(records$pair,pairs),column)]<-records$rate
  if(any(!roads$edge_id%in%pairs))stop('Source minutes or directed geometry matches are incomplete.')
  list(schema='beijing-native-minute-v1',road_pairs=pairs,minute=expected_minutes,
       minute_rates=values,roads=roads,day_no=day,source_field=unname(fields['rate']),source_columns=fields)
}

beijing_input <- function(native,interval_minutes=5L) {
  if(!identical(native$schema,'beijing-native-minute-v1'))stop('Expected native MOE minute observations.')
  minute<-native$minute;rates<-native$minute_rates;roads<-native$roads
  if(!length(minute)||anyNA(minute)||any(diff(minute)!=1)||ncol(rates)!=length(minute)||
     nrow(rates)!=length(native$road_pairs)||anyDuplicated(native$road_pairs)||
     any(!is.finite(rates)|rates<0))stop('Native MOE IDs, values and minute grid do not agree.')
  if(length(interval_minutes)!=1L||!is.finite(interval_minutes)||interval_minutes<1||
     interval_minutes!=trunc(interval_minutes)||length(minute)%%interval_minutes||minute[1]%%interval_minutes)
    stop('The interval must contain complete aligned source minutes.')
  road_row<-match(roads$edge_id,native$road_pairs)
  if(anyNA(road_row))stop('A directed geometry has no source MOE pair.')
  first<-seq.int(1L,length(minute),by=interval_minutes)
  values<-matrix(0,nrow(roads),length(first))
  for(offset in seq_len(interval_minutes)-1L)values<-values+rates[road_row,first+offset,drop=FALSE]
  values<-values/interval_minutes
  starts<-minute[first]*60
  roads$road_id<-as.character(roads$edge_index)
  flow<-data.frame(road_id=rep(roads$road_id,length(starts)),time=rep(starts,each=nrow(roads)),flow=as.vector(values))
  result <- list(crs='EPSG:4326',roads=roads,flow=flow,times=starts,
    title = 'Beijing simulated entry flow', time_label_format = '%H:%M',
    interval_seconds=rep(interval_minutes*60,length(starts)),unit='vehicles/hour',
    provenance=list(source_fields=if(is.null(native$source_columns))beijing_fields else native$source_columns,day_no=native$day_no,source=native$source,
      source_pairs=length(native$road_pairs),display_roads=nrow(roads),
      excluded_pairs=length(native$road_pairs)-nrow(roads),native_minutes=length(minute),
      native_records=length(rates),retained_minute_records=nrow(roads)*length(minute),
      operation='Arithmetic mean of the minute-sampled hourly entry rates; original example interval is five minutes.'))
  result
}

beijing_read_network <- function(geometry_file) {
  lines<-readLines(geometry_file,warn=FALSE,encoding='UTF-8')
  hit<-grep('^[[:space:]]*const network[[:space:]]*=',lines,value=TRUE)
  if(length(hit)!=1L)stop('Geometry file must contain one network FeatureCollection.')
  literal<-sub(';[[:space:]]*$','',sub('^[[:space:]]*const network[[:space:]]*=[[:space:]]*','',hit))
  beijing_network(jsonlite::fromJSON(literal))
}

# Restore the original CSV bytes for the report and command-line preparation.
restore_beijing_csv <- function(archive_file, work_dir) {
  csv_file <- file.path(work_dir, sub("[.]gz$", "", basename(archive_file)))
  if (file.exists(csv_file)) stop("Choose an empty directory for the restored CSV.")
  archive <- gzfile(archive_file, "rb")
  on.exit(close(archive), add = TRUE)
  output <- file(csv_file, "wb")
  on.exit(close(output), add = TRUE)
  repeat {
    bytes <- readBin(archive, "raw", n = 1024L * 1024L)
    if (!length(bytes)) break
    writeBin(bytes, output)
  }
  csv_file
}

beijing_read_moe <- function(moe_file,fields=beijing_fields) {
  header<-names(read.csv(text=readLines(moe_file,n=1L),check.names=FALSE))
  at<-match(unname(fields),header)
  if(anyNA(at))stop('Required source columns are missing.')
  data.table::fread(moe_file,skip=1L,header=FALSE,select=at,col.names=unname(fields),showProgress=FALSE)
}

read_beijing_raw <- function(moe_file,geometry_file,fields=beijing_fields,day=10L,expected_minutes=beijing_day_minutes) {
  raw<-beijing_read_moe(moe_file,fields)
  roads<-beijing_read_network(geometry_file)
  native<-beijing_records(raw,roads,expected_minutes,fields,day)
  native$source<-c(basename(moe_file),basename(geometry_file))
  native
}
write_beijing_prepared <- function(native, output_dir, sources, interval_minutes=5L) {
  meanings<-c(edge_index='Source GeoJSON display-road index.',edge_id='Directed source pair, from_node_id -> to_node_id.',
    from_node_id='Source upstream node ID.',to_node_id='Source downstream node ID.',length_km='Source link length in kilometres.',
    lanes='Source all-lane count.',speed_limit='Source speed-limit attribute, unchanged.',link_type_name='Source link-type description.')
  dictionary<-data.frame(table='roads',field=names(meanings),meaning=unname(meanings))
  write_traffic_input(beijing_input(native,interval_minutes),output_dir,
    source_files=sources,native=native,dictionary=dictionary)
}

prepare_example <- function(base='.',output_dir=file.path(base,'prepared'),interval_minutes=5L) {
  archive_file <- file.path(base, "data", "raw", "output_LinkTDMOE.csv.gz")
  work_dir <- tempfile("beijing-csv-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  moe_file <- restore_beijing_csv(archive_file, work_dir)
  geometry_file<-file.path(base,'data','raw','saturation_dynamic_leaflet_osm_osmgeom.html')
  native<-read_beijing_raw(moe_file,geometry_file)
  write_beijing_prepared(native,output_dir,c(moe_file,geometry_file),interval_minutes)
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
  if(length(script)!=1L)stop('Rscript entrypoint is required.')
  if(!file.exists(script))script<-gsub('~+~',' ',script,fixed=TRUE)
  base<-dirname(normalizePath(script,mustWork=TRUE))
  source(file.path(base, 'scripts', 'prepare-input.R'))
  args<-commandArgs(TRUE)
  if(length(args)>1L)stop('Usage: Rscript prepare.R [NEW_DIRECTORY]')
  result<-if(length(args))prepare_example(base,output_dir=args[1]) else prepare_example(base)
  cat('Drawing input',file.path(dirname(result),'flow-frames.rds'),'\n')
}
