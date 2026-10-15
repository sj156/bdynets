# Exact selected definitions from R/graphmode_c8_execution.R
graphmode_c8_execution_version <- "graphmode-c8-executor-20260924-v1"

graphmode_c8_save <- function(object,path) {
    # gzcon rejects R's exclusive "wxb" mode. Reserve the new path first,
    # then compress into that exclusively reserved (still empty) file.
    reservation <- file(path,open="wxb");close(reservation)
    con <- gzfile(path,open="wb"); closed <- FALSE
    on.exit(if(!closed) close(con),add=TRUE)
    saveRDS(object,con,version=3); close(con); closed <- TRUE
    if(!identical(graphmode_digest(object),graphmode_digest(readRDS(path)))) stop("C8 saved content audit failed; keep file.")
    invisible(path)
}

graphmode_c8_hash <- function(path) digest::digest(file=path,algo="sha256",serialize=FALSE)

graphmode_c8_artifact <- function(directory,name) {
    path <- file.path(directory,name); receipt <- paste0(path,".sha256")
    if(!file.exists(path) || !file.exists(receipt) || !identical(readLines(receipt,warn=FALSE),graphmode_c8_hash(path)))
        stop("Missing or changed accepted artifact: ",path)
    path
}

graphmode_c8_accept <- function(path) {
    con <- file(paste0(path,".sha256"),"wx"); on.exit(close(con))
    writeLines(graphmode_c8_hash(path),con)
    invisible(path)
}
