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
if(length(args)>1L)stop('Usage: Rscript run.R [HH:MM] [--offline] [--export]')
time_label<-if(length(args))args[1] else '08:00'
parameters<-list(time_label=time_label,basemap=!offline,export=export,export_dir=file.path(base,'output'))
scratch<-tempfile('report-render-');dir.create(scratch)
intermediates<-file.path(scratch,'intermediates');dir.create(intermediates)
rendered_dir<-file.path(scratch,'rendered');dir.create(rendered_dir)
rendered_name<-basename(tempfile('rendered-report-',tmpdir=rendered_dir,fileext='.html'))
started<-proc.time()[['elapsed']]
rendered<-rmarkdown::render(file.path(base,'beijing-traffic-report.Rmd'),output_file=rendered_name,output_dir=rendered_dir,
 intermediates_dir=intermediates,knit_root_dir=base,params=parameters,envir=new.env(parent=globalenv()),quiet=TRUE,clean=TRUE)
final_report<-file.path(base,'beijing-traffic-report.html')
if(!file.copy(rendered,final_report,overwrite=TRUE))stop('Cannot update the rendered report.')
if(!identical(unname(tools::md5sum(rendered)),unname(tools::md5sum(final_report))))stop('Published report differs from the completed render.')
unlink(scratch,recursive=TRUE)
cat('Built beijing-traffic-report.html in',round(proc.time()[['elapsed']]-started,1),'seconds\n')
