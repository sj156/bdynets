preparation_session <- function(example_dir) {
  package_dir<-normalizePath(file.path(example_dir,'..','..','bdynets'),mustWork=TRUE)
  if('bdynets'%in%loadedNamespaces())stop('Start a fresh R session before loading the bundled package.')
  library_dir<-tempfile('bdynets-session-');dir.create(library_dir)
  .libPaths(c(library_dir,.libPaths()))
  source_dir<-tempfile('bdynets-source-');dir.create(source_dir)
  on.exit(unlink(source_dir,recursive=TRUE),add=TRUE)
  if(!file.copy(package_dir,source_dir,recursive=TRUE))stop('Cannot stage the package source for installation.')
  utils::install.packages(file.path(source_dir,basename(package_dir)),repos=NULL,
    type='source',lib=library_dir,quiet=TRUE,INSTALL_opts='--clean')
  installed_dir<-file.path(library_dir,'bdynets')
  if(!file.exists(file.path(installed_dir,'DESCRIPTION')))
    stop('Bundled package installation failed. Check its dependencies and C++17 compiler.')
  if(!requireNamespace('bdynets',quietly=TRUE,lib.loc=library_dir)||
    !identical(normalizePath(getNamespaceInfo('bdynets','path')),normalizePath(installed_dir))||
    !exists('write_traffic_input',asNamespace('bdynets'),inherits=FALSE))
    stop('The bundled preparation package did not load from its session library.')
  invisible(library_dir)
}
