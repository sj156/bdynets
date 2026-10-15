#' Prepare observations for the no-GP latent-position model
#' @param y Nonnegative integer counts (or binary outcomes).
#' @param edges Two-column matrix of one-based node indices, one row per observation.
#' @param n Number of nodes, including any isolated nodes.
#' @param exposure Positive scalar or observation-length vector; Bernoulli requires one.
#' @param family Either "poisson" or "bernoulli".
#' @return Validated observation list. Missing edges are omitted, not zero-filled.
#' @export
lpr_data <- function(y, edges, n, exposure = 1, family = c("poisson", "bernoulli")) {
  family <- match.arg(family)
  if (length(n) != 1L || !is.finite(n) || n < 2 || n != as.integer(n)) stop("Invalid n")
  if (!is.matrix(edges) || ncol(edges) != 2L || nrow(edges) != length(y) || !length(y) ||
      any(!is.finite(edges)) || any(edges != as.integer(edges)) ||
      any(edges < 1 | edges > n) || any(edges[,1] == edges[,2])) stop("Invalid edge list")
  if (any(!is.finite(y)) || any(y < 0 | y != floor(y))) stop("Invalid outcomes")
  if (family == "bernoulli" && any(y > 1)) stop("Bernoulli outcomes must be binary")
  if (!(length(exposure) %in% c(1L, length(y))) || any(!is.finite(exposure)) || any(exposure <= 0)) stop("Invalid exposures")
  if (family == "bernoulli" && any(exposure != 1)) stop("Bernoulli requires exposure=1")
  structure(list(y=as.numeric(y), edges=edges, n=as.integer(n),
                 exposure=rep_len(exposure,length(y)), family=family), class="lpr_data")
}

.lpr_center <- function(z) sweep(z, 2L, colMeans(z), "-")
.lpr_inner <- function(z, edges) rowSums(z[edges[,1],,drop=FALSE]*z[edges[,2],,drop=FALSE])
.lpr_eta <- function(data, state) {
  a <- state$b-mean(state$b)
  state$mu+a[data$edges[,1]]+a[data$edges[,2]]+.lpr_inner(.lpr_center(state$raw),data$edges)
}
.lpr_loglik <- function(data, eta) {
  x <- log(data$exposure)+eta
  if (data$family == "poisson") sum(data$y*x-exp(x)) else
    sum(data$y*x-pmax(x,0)-log1p(exp(-abs(x))))
}
.lpr_ball <- function(n,d,radius) {
  x <- matrix(stats::rnorm(n*d),n,d)
  x/sqrt(rowSums(x*x))*(radius*stats::runif(n)^(1/d))
}
.lpr_z_change <- function(z, k, delta, edges) {
  c <- as.numeric(seq_len(nrow(z))==k)-1/nrow(z)
  i<-edges[,1]; j<-edges[,2]
  as.vector((c[i]*z[j,,drop=FALSE]+c[j]*z[i,,drop=FALSE])%*%delta)+c[i]*c[j]*sum(delta^2)
}
.lpr_b_change <- function(n,k,delta,edges) delta*((edges[,1]==k)+(edges[,2]==k)-2/n)

#' Align a centered configuration by rotation or reflection
#' @param z,reference Numeric matrices of identical dimensions.
#' @return Centered z after an orthogonal transform; scale is never fitted.
#' @export
lpr_align <- function(z, reference) {
  if (!is.matrix(z) || !is.matrix(reference) || !identical(dim(z),dim(reference)) ||
      any(!is.finite(z)) || any(!is.finite(reference))) stop("Invalid configurations")
  z <- .lpr_center(z); reference <- .lpr_center(reference)
  s <- svd(crossprod(z,reference))
  z%*%s$u%*%t(s$v)
}

#' Read or write a complete LPR artifact
#' @param object Object to store, including draws and checkpoint when available.
#' @param path RDS filename outside the code repository for large fits.
#' @return For save, invisible normalized path; for read, the stored object.
#' @name lpr_storage
NULL
#' @rdname lpr_storage
#' @export
lpr_save <- function(object, path) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  tmp<-tempfile(tmpdir=dirname(path)); on.exit(unlink(tmp),add=TRUE)
  saveRDS(object,tmp,version=3)
  if (!file.rename(tmp,path)) stop("Could not replace result file")
  invisible(normalizePath(path))
}
#' @rdname lpr_storage
#' @export
lpr_read <- function(path) readRDS(path)

.lpr_identity <- function(x) {
  path<-tempfile(); on.exit(unlink(path),add=TRUE)
  saveRDS(x,path,version=3,compress=FALSE)
  unname(tools::md5sum(path))
}
.lpr_code_id <- function() {
  names<-c('.lpr_run','.lpr_center','.lpr_eta','.lpr_inner','.lpr_loglik',
           '.lpr_z_change','.lpr_b_change','.lpr_ball','lpr_fit','lpr_continue')
  .lpr_identity(lapply(names,function(n) list(name=n,formals=formals(get(n)),body=deparse(body(get(n))))))
}
