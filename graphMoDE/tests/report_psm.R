# Report arithmetic only: no transitions or diagnostic recomputation.
entry <- sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
root <- normalizePath(file.path(dirname(entry),'..'))
source(file.path(root,'R/api.R'));api <- graphmode_load(root)
a <- matrix(c(1,.2,.2,1),2);b <- matrix(c(1,.7,.7,1),2)
stopifnot(identical(api$graphmode_report_psm_rms(a,b),api$graphmode4_psm_rms(a,b)))
rounded <- matrix(0,2,2)
for(i in 1:8000) rounded <- rounded+matrix(1/8000,2,2)
stopifnot(max(rounded)>1,max(rounded)-1<1e-12)
before <- rounded
stopifnot(is.finite(api$graphmode_report_psm_rms(rounded,a)),identical(rounded,before))
stopifnot(inherits(try(api$graphmode4_psm_rms(rounded,a),silent=TRUE),'try-error'))
bad <- rounded;bad[1,2] <- 1.001
stopifnot(inherits(try(api$graphmode_report_psm_rms(bad,a),silent=TRUE),'try-error'))
cat('Report roundoff repair passed; strict chain validator unchanged.\n')
