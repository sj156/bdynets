options(bdynets.basemap.auto=FALSE)
library(bdynets)
stopifnot(exists('traffic_od_map',mode='function'))
out<-tempfile('od-integration-');dir.create(out);on.exit(unlink(out,recursive=TRUE))
nodes<-data.frame(node_id=c('u','v','w'),longitude=c(-87.64,-87.63,-87.62),latitude=c(41.88,41.89,41.88))
flows<-data.frame(origin_id=c('u','u','v','v','w','u'),destination_id=c('v','v','u','w','u','u'),flow=c(1,1,3,5,8,7),time='2026-01-05 06:00:00')
z<-traffic_od_map(nodes,flows,node_columns=c(id='node_id',longitude='longitude',latitude='latitude'),
 start_time='2026-01-05 06:00:00',end_time='2026-01-05 09:00:00',timezone='America/Chicago',
 output_file=file.path(out,'od.png'),draw=FALSE)
stopifnot(z$summary$selected_flow==25,z$summary$within_node_flow==7,z$summary$nonloop_flow==18,z$summary$displayed_pairs==4,
 identical(unname(z$summary$flow_classes),c(0L,2L,1L,0L,1L)))
expected<-c('#69afe0','#a1c181','#e9c46a','#f4a261','#e76f51')
stopifnot(identical(formals(traffic_od_map)$flow_palette,quote(c("#69afe0", "#a1c181", "#e9c46a", "#f4a261", "#e76f51"))))
# Field mapping remains generic rather than depending on Divvy identifiers.
names(nodes)<-c('site','x','y');names(flows)<-c('from','to','amount','when')
y<-traffic_od_map(nodes,flows,node_columns=c(id='site',longitude='x',latitude='y'),
 flow_columns=c(origin='from',destination='to',count='amount',time='when'),
 start_time='2026-01-05 06:00:00',end_time='2026-01-05 09:00:00',timezone='America/Chicago',draw=FALSE)
stopifnot(identical(z$routes,y$routes),identical(z$summary,y$summary))
unlink(out,recursive=TRUE);cat('OD totals, loops, five colors, generic field mappings and PNG output passed.\n')
