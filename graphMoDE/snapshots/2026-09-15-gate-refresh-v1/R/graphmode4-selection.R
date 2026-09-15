# Pure offline selection. Input validity must come from the four-chain/native
# adapter audit, never from ARI or from choosing a preferred chain.
graphmode4_pools <- function(records) {
    required <- c("pool", "data_id", "seed")
    if (!is.data.frame(records) || !all(required %in% names(records)) || !nrow(records) ||
        anyNA(records[required]) || !setequal(unique(records$pool), c("calibration", "selection", "formal")) ||
        any(!nzchar(as.character(records$data_id))) || anyDuplicated(records$data_id) ||
        !is.numeric(records$seed) || any(records$seed < 1 | records$seed != floor(records$seed) |
                                      records$seed > .Machine$integer.max) || anyDuplicated(records$seed))
        stop("Register disjoint calibration/selection/formal data IDs and seeds.", call. = FALSE)
    list(records = records, protocol = graphmode4_protocol,
        signature = graphmode_digest(records), note = "Distinct records are necessary, not proof of independent generation.")
}

graphmode4_select <- function(attempts, pools) {
    if (!identical(pools, graphmode4_pools(pools$records)))
        stop("Pool registry changed.", call. = FALSE)
    fields <- c("method", "setting", "replicate", "data_id", "valid", "ARI",
                "end_to_end_seconds", "cost_context", "failure")
    if (!is.data.frame(attempts) || !all(fields %in% names(attempts)))
        stop("Incomplete selection attempt table.", call. = FALSE)
    settings <- graphmode4_main_grid()$setting
    a <- attempts
    if (nrow(a) != 5L * 12L * 20L || anyNA(a[setdiff(fields, "ARI")]) ||
        !setequal(a$method, graphmode4_methods) || !setequal(a$setting, settings) ||
        !is.numeric(a$replicate) || any(!a$replicate %in% 1:20) ||
        anyDuplicated(a[c("method", "setting", "replicate")]) ||
        !is.logical(a$valid) || !is.numeric(a$ARI) ||
        any(!is.finite(a$ARI[a$valid]) | a$ARI[a$valid] < -1 | a$ARI[a$valid] > 1) ||
        any(!is.na(a$ARI[!a$valid])) || any(!nzchar(a$failure[!a$valid])) ||
        any(nzchar(a$failure[a$valid])))
        stop("Need all 1200 attempts; failures require causes and unavailable ARI.", call. = FALSE)
    failure_kinds <- c("numerical", "statistical", "resource", "infrastructure", "missing-output")
    if (any(!a$failure[!a$valid] %in% failure_kinds))
        stop("Unknown failure category; missing adapters/source bugs block the stage, not a model score.", call. = FALSE)
    if (!is.numeric(a$end_to_end_seconds) || any(!is.finite(a$end_to_end_seconds) | a$end_to_end_seconds < 0) ||
        length(unique(a$cost_context)) != 1L || !nzchar(a$cost_context[1L]))
        stop("Record finite all-attempt costs on matched hardware/threads, including tuning and all starts.", call. = FALSE)
    dataset_key <- paste(a$setting, a$replicate, sep = "::")
    data_by_key <- split(as.character(a$data_id), dataset_key)
    if (any(vapply(data_by_key, function(x) length(unique(x)) != 1L, logical(1))))
        stop("Methods must use the same dataset within each replicate.", call. = FALSE)
    data_ids <- vapply(data_by_key, `[`, character(1), 1L)
    registered <- as.character(pools$records$data_id[pools$records$pool == "selection"])
    if (anyDuplicated(data_ids) || !setequal(data_ids, registered))
        stop("Use exactly the registered 240 independent selection datasets, never formal data.", call. = FALSE)
    valid_counts <- matrix(0L, 5L, 12L, dimnames = list(graphmode4_methods, settings))
    for (m in graphmode4_methods) for (h in settings)
        valid_counts[m, h] <- sum(a$valid[a$method == m & a$setting == h])
    eligible <- graphmode4_methods[apply(valid_counts >= 18L, 1L, all)]
    report <- list(protocol = graphmode4_protocol, status = "unresolved", selected = NULL,
        eligible = eligible, valid_counts = valid_counts,
        total_attempt_seconds = tapply(a$end_to_end_seconds, a$method, sum),
        attempts = a, pools_signature = pools$signature, common = list(),
        note = "Accuracy is conditional on common validity; all failures remain in the table.")
    if (!length(eligible)) {report$reason <- "No method meets 18/20 in every setting."; return(report)}
    for (h in settings) report$common[[h]] <- Reduce(intersect, lapply(eligible,
        function(m) a$replicate[a$setting == h & a$method == m & a$valid]))
    report$common_counts <- lengths(report$common)
    if (any(report$common_counts < 16L)) {
        report$reason <- "Common-valid intersection below 16; no candidates or settings may be removed."
        return(report)
    }
    means <- costs <- matrix(NA_real_, length(eligible), 12L, dimnames = list(eligible, settings))
    for (m in eligible) for (h in settings) {
        rows <- a$method == m & a$setting == h & a$replicate %in% report$common[[h]]
        means[m, h] <- mean(a$ARI[rows]); costs[m, h] <- mean(a$end_to_end_seconds[rows])
    }
    score <- rowMeans(means)
    tie <- eligible[score >= max(score) - 0.01]
    minimum_validity <- apply(valid_counts[tie, , drop = FALSE], 1L, min) / 20
    overall_validity <- rowMeans(valid_counts[tie, , drop = FALSE]) / 20
    chosen <- order(-minimum_validity, -overall_validity, rowMeans(costs[tie, , drop = FALSE]),
                    match(tie, graphmode4_tie_order))[1L]
    # Independent datasets are the sampling units. These plug-in paired MCSEs
    # describe the common-valid conditional contrast, not selection-adjusted CIs.
    contrasts <- list()
    if (length(eligible) > 1L) for (pair in combn(eligible, 2L, simplify = FALSE)) {
        deltas <- lapply(settings, function(h) {
            ids <- sort(report$common[[h]])
            score_for <- function(m) {
                rows <- a[a$method == m & a$setting == h, ]
                rows$ARI[match(ids, rows$replicate)]
            }
            score_for(pair[1L]) - score_for(pair[2L])
        })
        contrasts[[length(contrasts) + 1L]] <- data.frame(method_a = pair[1L], method_b = pair[2L],
            difference = mean(vapply(deltas, mean, numeric(1))),
            paired_mcse = sqrt(sum(vapply(deltas, function(x) stats::var(x) / length(x), numeric(1)))) / 12)
    }
    report$status <- "resolved-selection-only"
    report$selected <- tie[chosen]
    report$practical_tie <- tie
    report$setting_ARI <- means; report$score <- score
    report$setting_cost <- costs
    report$paired_contrasts <- if (length(contrasts)) do.call(rbind, contrasts) else NULL
    report$accuracy_win_claim <- FALSE
    report$single_eligible <- length(eligible) == 1L
    report$input_identity <- graphmode_digest(list(attempts = attempts, pools = pools))
    report
}

graphmode4_freeze_representative <- function(attempts, pools, configuration_identity, decision_id) {
    report <- graphmode4_select(attempts, pools)
    if (report$status != "resolved-selection-only") stop(report$reason, call. = FALSE)
    graphmode4_text(configuration_identity, "frozen configuration identity")
    graphmode4_text(decision_id, "representative decision ID")
    record <- list(method = report$selected, protocol = graphmode4_protocol,
        selection_identity = report$input_identity, configuration_identity = configuration_identity,
        decision_id = decision_id, scope = "one representative across all subsequent settings/tasks",
        formal_authorized = FALSE)
    record$signature <- graphmode_digest(record)
    record
}
