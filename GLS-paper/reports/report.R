read_experiment <- function(path, case) {
  config <- readRDS(file.path(path, "config.rds"))
  if (!identical(config$case, case)) stop("Report and saved experiment do not match.")
  list(config = config,
       jobs = read.csv(file.path(path, "jobs.csv")),
       scores = read.csv(file.path(path, "comparison.csv")),
       paired = read.csv(file.path(path, "paired.csv")))
}

primary_report <- function(x, ref, window, metric = "deviance") {
  both <- x$paired[x$paired$window == window & x$paired$metric == metric &
                   x$paired$reference == ref, , drop = FALSE]
  if (nrow(both)) {
    format_value <- function(z) formatC(z, digits = 3, format = "f")
    value <- ifelse(is.na(both$difference), "Unavailable", format_value(both$difference))
    with_se <- is.finite(both$mcse)
    value[with_se] <- paste0(value[with_se], " ± ", format_value(both$mcse[with_se]))
    both$estimate <- paste0(value, " (", both$paired_n, ")")
    wide <- stats::reshape(both[c("method", "scenario", "estimate")], idvar = "method",
                          timevar = "scenario", direction = "wide")
    names(wide) <- sub("^estimate[.]", "", names(wide))
    knitr::kable(wide, row.names = FALSE,
                 caption = "Mean paired difference ± Monte Carlo SE when estimable (matched panels)")
  } else cat("No matched method and reference runs are available for this contrast.")
}

report_panels <- function(x) {
  by_condition <- split(x$jobs, x$jobs$scenario)
  counts <- lapply(by_condition, function(jobs) {
    replicates <- split(jobs, jobs$replicate)
    complete <- vapply(replicates, function(panel) {
      hashes <- panel$panel_md5[panel$status == "complete"]
      if (anyNA(hashes) || any(!nzchar(hashes)) || length(unique(hashes)) > 1L)
        stop("Inconsistent completed-job panel checksums for ", panel$scenario[1],
             ", replicate ", panel$replicate[1])
      all(panel$status == "complete")
    }, logical(1))
    data.frame(Condition = jobs$scenario[1], Scheduled = length(replicates),
               Started = length(unique(jobs$replicate[jobs$status == "complete"])),
               Complete = sum(complete))
  })
  knitr::kable(do.call(rbind, counts), row.names = FALSE,
               col.names = c("Condition", "Scheduled panels", "At least one fit complete", "All methods complete"),
               caption = "Distinct replicate panels per condition. Completed jobs on a panel share its checksum.")
}

report_status <- function(x) {
  status <- as.data.frame.matrix(xtabs(~ method + status, data = x$jobs))
  knitr::kable(status, caption = "Method jobs by completion status")
}

read_application <- function(path, case) {
  x <- readRDS(file.path(path, "application_results.rds"))
  if (!identical(x$case_id, case)) stop("Report and saved application do not match.")
  if (!is.data.frame(x$forecast_summary)) stop("The application needs compact forecast summaries.")
  x
}

application_scores <- function(frames) {
  values <- lapply(split(frames, frames$model), function(z) {
    y <- as.matrix(z$observed)
    forecast <- list(mean = as.matrix(z$mean_intensity))
    if ("log_score" %in% names(z))
      forecast$logprob <- as.matrix(as.numeric(z$log_score))
    if (all(c("count_lower", "count_upper") %in% names(z))) {
      forecast$lower <- as.matrix(as.numeric(z$count_lower))
      forecast$upper <- as.matrix(as.numeric(z$count_upper))
    }
    values <- bdynets::gls_forecast_metrics(y, forecast)
    score <- setNames(values$value, values$metric)
    data.frame(Method = z$model[1L], Cells = length(y), Deviance = score["deviance"],
      MAE = score["mae"], RMSE = sqrt(mean((y - forecast$mean)^2)),
      Log_score = score["log_score"], Coverage = score["coverage95"],
      Width = score["width95"], row.names = NULL)
  })
  knitr::kable(do.call(rbind, values), digits = 3, row.names = FALSE)
}

application_plot <- function(x) {
  Y <- x$fit$Y
  observed <- rowSums(Y)
  draws <- x$forecast_summary
  chosen <- draws[draws$model == "GLS joint DSS", ]
  estimate <- tapply(chosen$mean_intensity, chosen$row, sum)
  graphics::plot(observed, type = "l", col = "#234b56", xlab = "Time bin",
    ylab = "Total count across included series", ylim = range(c(observed, estimate)))
  graphics::abline(v = c(x$split$validation[1], x$split$test[1]) - .5, lty = 3)
  graphics::lines(as.integer(names(estimate)), estimate, col = "#bf522e", lwd = 2)
  graphics::legend("topleft", c("Observed", "DSS one step ahead"), col = c("#234b56", "#bf522e"),
    lty = 1, bty = "n", cex = .8)
}

diagnostic_counts <- function(x) {
  d <- x$diagnostics
  data.frame(Statistics = nrow(d), Available_Rhat = sum(is.finite(d$SplitRhat)),
    Rhat_above_target = sum(d$SplitRhat > x$control$mcmc_rhat_threshold, na.rm = TRUE),
    Available_ESS = sum(is.finite(d$ESS)),
    ESS_below_target = sum(d$ESS < x$control$mcmc_ess_threshold, na.rm = TRUE))
}
