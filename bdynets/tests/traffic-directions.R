if(nzchar(Sys.getenv('TRAFFICMAP_SOURCE')))source(Sys.getenv('TRAFFICMAP_SOURCE')) else library(bdynets)
inside<-function(n)if(exists(n,mode='function'))get(n,mode='function') else getFromNamespace(n,'bdynets')
if(nzchar(Sys.getenv('TRAFFICMAP_SOURCE'))&&!exists('.tm_opposing',mode='function'))stop('Opposing geometry recognition is absent')
match_paths<-inside('.tm_opposing')
line<-rbind(c(10,50),c(10.004,50))
reverse<-line[2:1,,drop=FALSE]
active<-function(x)sum(vapply(x,function(g)sum(is.finite(g[,1])),0L))
a<-match_paths(list(line,reverse));stopifnot(active(a)==2L,all(a[[1]][,1]<1e-5),all(a[[1]][,2]==1))
# Vertex count and source segmentation do not define pairing.
resampled<-rbind(reverse[1,],colMeans(line),reverse[2,])
stopifnot(active(match_paths(list(line,resampled)))==3L)
# A crossing, same-direction duplicate, continuation or distant parallel is not opposing.
cross<-rbind(c(10.002,49.999),c(10.002,50.001))
next_line<-line+cbind(rep(.004,2),c(0,0))
far<-reverse+cbind(c(0,0),rep(.001,2))
stopifnot(active(match_paths(list(line,cross,line,next_line,far)))==0L)
near<-reverse+cbind(c(0,0),rep(.00005,2))
b<-match_paths(list(line,near));stopifnot(active(b)==2L,all(b[[1]][,1]>1),all(b[[2]][,1]>1))
# A tiny junction contact does not constitute an overlapping road.
touch<-rbind(c(10.004,50),c(10.008,50.00001))
stopifnot(active(match_paths(list(line,touch)))==0L)
# Degenerate edges and closed lines do not manufacture reciprocal directions.
stopifnot(active(match_paths(list(line[rep(1,3),,drop=FALSE],line)))==0L)
r<-data.frame(road_id=c('forward','reverse','single'));r$geometry<-list(line,reverse,cross)
x<-traffic_data(r,times=0)
original<-x$roads$geometry
shift<-inside('.tm_display_paths')(x$roads$geometry,x$opposing,1,5)
p<-lapply(original,function(g)inside('.tm_project')(g)*6378137)
stopifnot(identical(original,x$roads$geometry),identical(shift[[3]],p[[3]]),
 sqrt(sum((shift[[1]][1,]-shift[[2]][2,])^2))>7)
# Reordering records and descriptive attributes leave display placement unchanged.
y<-traffic_data(r[3:1,],times=0);decorated<-r;decorated$direction<-c('S','N','W');decorated$lanes<-c(9,1,30)
z<-traffic_data(decorated,times=0)
stopifnot(identical(x$opposing,z$opposing),identical(x$opposing,rev(y$opposing)))
# Existing physical separation is preserved at high zoom.
stopifnot(identical(inside('.tm_display_paths')(list(line,near),b,100,5),lapply(list(line,near),function(g)inside('.tm_project')(g)*6378137)))
cat('Opposing paths, segmentation, exclusions, permutation, source preservation and zoom separation passed.\n')

style<-inside('.tm_view_style')
make_grid<-function(step) {
 p<-seq(20,780,by=step)
 c(lapply(p,function(x)rbind(c(x,20),c(x,780))),lapply(p,function(y)rbind(c(20,y),c(780,y))))
}
sparse<-make_grid(190);dense<-make_grid(12)
a<-style(sparse,800,800,12);b<-style(dense,800,800,12)
stopifnot(median(b$width)<median(a$width)*.65,median(b$gap)<median(a$gap),
 all(b$width>=.85),all(b$gap>=.55),all(a$width<=4.5),all(b$halo<a$halo[1]))
# Distant geometry must not thicken or thin visible roads.
remote<-lapply(dense,function(g)g+100000)
c<-style(c(sparse,remote),800,800,12)
stopifnot(identical(a$width,c$width[seq_along(sparse)]),identical(a$density,c$density[seq_along(sparse)]))
# Panning to a dense cluster changes layout, while isolated paths remain legible.
both<-c(sparse,lapply(dense,function(g)g+cbind(rep(1000,nrow(g)),rep(0,nrow(g)))))
left<-style(both,800,800,12);right<-style(lapply(both,function(g)g-cbind(rep(1000,nrow(g)),rep(0,nrow(g)))),800,800,12)
stopifnot(median(right$width[length(sparse)+seq_along(dense)])<median(left$width[seq_along(sparse)]))
# Smooth zoom response rather than a fixed threshold jump.
z1<-style(sparse,800,800,10);z2<-style(sparse,800,800,10.01)
stopifnot(max(abs(z1$width-z2$width))<.02)
# Extra collinear vertices do not count as extra roads.
segmented<-lapply(sparse,function(g)rbind(g[1,],colMeans(g),g[2,]))
split<-style(segmented,800,800,12)
stopifnot(max(abs(split$width-a$width))<.06)
# Both tracks retain a visible gap at density-adapted widths.
line<-rbind(c(10,50),c(10.004,50));g<-list(line,line[2:1,,drop=FALSE]);opp<-inside('.tm_opposing')(g)
p<-inside('.tm_display_paths')(g,opp,1,rep(b$width[1],2),rep(b$gap[1],2))
distance<-sqrt(sum((p[[1]][1,]-p[[2]][2,])^2))
stopifnot(abs(distance-b$width[1]-b$gap[1])<1e-6)
cat('Density adaptation, remote exclusion, panning, continuous zoom, segmentation and opposing gap passed.\n')

xy<-function(x,y)cbind(x/6378137*180/pi,(atan(exp(y/6378137))-pi/4)*360/pi)
a<-xy(c(0,1000),c(0,0));b<-xy(c(1000,0),c(10,10));c<-xy(c(1000,0),c(-10,-10))
active<-function(x)which(vapply(x,function(g)any(is.finite(g[,1])),FALSE))
checks<-list(
 ambiguous_parallel=function()stopifnot(length(active(inside('.tm_opposing')(list(a,b,c))))==0),
 short_spur=function()stopifnot(length(active(inside('.tm_opposing')(list(a,xy(c(515,485),c(0,0))))))==0),
 acute_crossing=function()stopifnot(length(active(inside('.tm_opposing')(list(a,xy(c(1000,0),c(-10,10))))))==0),
 reciprocal_nearest=function(){d<-xy(c(0,1000),c(11,11));stopifnot(identical(active(inside('.tm_opposing')(list(a,b,d))),2:3))},
 fine_segmentation=function(){g<-c(list(a),lapply(0:49,function(k)xy(c((k+1)*20,k*20),c(10,10))));stopifnot(length(active(inside('.tm_opposing')(g)))==51)},
 unequal_segmentation=function(){g<-c(list(a),lapply(0:9,function(k)xy(c((k+1)*100,k*100),c(0,0))));stopifnot(length(active(inside('.tm_opposing')(g)))==11)},
 exact_reverse=function()stopifnot(length(active(inside('.tm_opposing')(list(a,a[2:1,]))))==2),
 duplicate_ambiguity=function()stopifnot(length(active(inside('.tm_opposing')(list(a,a,a[2:1,]))))==0),
 declared_priority=function(){r<-data.frame(road_id=c('a','b','c'),opposite_id=c('b','a',''));r$geometry<-list(a,b,c);x<-traffic_data(r,times=0);stopifnot(identical(active(x$opposing),1:2))},
 invalid_unknown=function(){r<-data.frame(road_id=c('a','b'),opposite_id=c('missing','a'));r$geometry<-list(a,b);stopifnot(inherits(try(traffic_data(r,times=0),silent=TRUE),'try-error'))},
 invalid_self=function(){r<-data.frame(road_id=c('a','b'),opposite_id=c('a','b'));r$geometry<-list(a,b);stopifnot(inherits(try(traffic_data(r,times=0),silent=TRUE),'try-error'))},
 invalid_nonreciprocal=function(){r<-data.frame(road_id=c('a','b'),opposite_id=c('b',''));r$geometry<-list(a,b);stopifnot(inherits(try(traffic_data(r,times=0),silent=TRUE),'try-error'))},
 declared_far_separation=function(){r<-data.frame(road_id=c('a','b'),opposite_id=c('b','a'));r$geometry<-list(a,xy(c(1000,0),c(100,100)));x<-traffic_data(r,times=0);stopifnot(length(active(x$opposing))==2)},
 no_geometry_reversal=function(){r<-data.frame(road_id=c('a','b'),opposite_id=c('b','a'));r$geometry<-list(a,b[2:1,]);x<-traffic_data(r,times=0);stopifnot(length(active(x$opposing))==0,identical(x$roads$geometry,r$geometry))},
 declared_permutation=function(){r<-data.frame(road_id=c('a','b','c'),opposite_id=c('b','a',''));r$geometry<-list(a,b,c);x<-traffic_data(r,times=0);y<-traffic_data(r[3:1,],times=0);stopifnot(identical(x$opposing,rev(y$opposing)))})
fail<-character()
for(n in names(checks))tryCatch({checks[[n]]();cat('PASS',n,'\n')},error=function(e){cat('FAIL',n,conditionMessage(e),'\n');fail<<-c(fail,n)})
if(length(fail))stop(length(fail),' pairing checks failed',call.=FALSE)
