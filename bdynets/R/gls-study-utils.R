.gls_study_logdet_spd <- function(A) 2 * sum(log(diag(chol_jitter(A))))

.gls_study_log_sum_exp <- function(x) {
  m <- max(x); m + log(sum(exp(x - m)))
}

.gls_study_seed <- function(stage, label = "", cfg) {
  z <- utf8ToInt(paste(stage, label, sep = "::"))
  as.integer((cfg$seed + sum((seq_along(z) + 31L) * z)) %% 2147483646 + 1)
}

.gls_study_fourier_design <- function(n, period, order, indices = seq_len(n)) {
  if (order == 0L) return(matrix(numeric(0), nrow = n, ncol = 0L))
  z <- do.call(cbind, lapply(seq_len(order), function(j)
    cbind(cos(2 * pi * j * indices / period), sin(2 * pi * j * indices / period))))
  colnames(z) <- unlist(lapply(seq_len(order), function(j)
    c(paste0("cos", j), paste0("sin", j))))
  z
}

.gls_study_transform_counts <- function(y, center, scale) {
  sweep(sweep(log1p(y), 2L, center, "-"), 2L, scale, "/")
}

.gls_study_make_lag1 <- function(Y, pre_y, center, scale) {
  hist <- rbind(pre_y, Y); .gls_study_transform_counts(hist[seq_len(nrow(Y)), , drop = FALSE],
                                             center, scale)
}

.gls_study_standardize_from_train <- function(X, train) {
  center <- colMeans(X[train, , drop = FALSE]); scale <- apply(X[train, , drop = FALSE], 2, sd)
  scale[!is.finite(scale) | scale < 1e-8] <- 1
  list(X = sweep(sweep(X, 2L, center, "-"), 2L, scale, "/"),
       center = center, scale = scale)
}

.gls_study_choose_r_nb_time <- function(Y, cfg) {
  TT <- nrow(Y); k <- ncol(Y); smooth <- pmax(colMeans(Y), 1); out <- numeric(TT)
  for (t in seq_len(TT)) {
    q <- as.numeric(quantile(smooth, cfg$r_nb_quantile, type = 8))
    raw <- ceiling(q / cfg$r_nb_epsilon)
    out[t] <- clamp(ceiling(raw / cfg$r_nb_round_to) * cfg$r_nb_round_to,
                    cfg$r_nb_min, cfg$r_nb_max)
    smooth <- cfg$r_nb_smoothing * smooth + (1 - cfg$r_nb_smoothing) * Y[t, ]
  }
  out
}


