script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
if(length(script)!=1L)stop('Rscript entrypoint is required.')
if(!file.exists(script))script<-gsub('~+~',' ',script,fixed=TRUE)
base<-dirname(normalizePath(script,mustWork=TRUE))
source(file.path(base,'..','..','bdynets','inst','preparation-session.R'))
preparation_session(base)
if(!requireNamespace('rmarkdown',quietly=TRUE)||!requireNamespace('knitr',quietly=TRUE)||!rmarkdown::pandoc_available())stop('Report rendering requires rmarkdown, knitr and Pandoc.')
args<-commandArgs(TRUE)
offline<-'--offline'%in%args
export<-'--export'%in%args
args<-args[!args%in%c('--offline','--export')]
if(length(args)||export)stop('Usage: Rscript run.R [--offline]')
parameters<-list(basemap=!offline)
scratch<-tempfile('report-render-');dir.create(scratch)
intermediates<-file.path(scratch,'intermediates');dir.create(intermediates)
rendered_dir<-file.path(scratch,'rendered');dir.create(rendered_dir)
rendered_name<-basename(tempfile('rendered-report-',tmpdir=rendered_dir,fileext='.html'))
started<-proc.time()[['elapsed']]
rendered<-rmarkdown::render(file.path(base,'divvy-od-report.Rmd'),output_file=rendered_name,output_dir=rendered_dir,
 intermediates_dir=intermediates,knit_root_dir=base,params=parameters,envir=new.env(parent=globalenv()),quiet=TRUE,clean=TRUE)
final_report<-file.path(base,'divvy-od-report.html')
if(!file.copy(rendered,final_report,overwrite=TRUE))stop('Cannot update the rendered report.')
if(!identical(unname(tools::md5sum(rendered)),unname(tools::md5sum(final_report))))stop('Published report differs from the completed render.')
unlink(scratch,recursive=TRUE)
cat('Built divvy-od-report.html in',round(proc.time()[['elapsed']]-started,1),'seconds\n')
