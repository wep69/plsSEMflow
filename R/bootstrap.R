# Bootstrap inference ------------------------------------------------------

.pls_boot_stat_native <- function(fit, idx) {
  f <- .pls_native_fit(fit$model, fit$data[idx, , drop = FALSE])
  p <- f$paths; p$key <- paste(p$from, p$to, sep = " -> ")
  l <- f$loadings; l$key <- paste(l$construct, l$indicator, sep = " =~ ")
  w <- f$weights; w$key <- paste(w$construct, w$indicator, sep = " <~ ")
  c(setNames(p$estimate, paste0("path::", p$key)),
    setNames(l$loading, paste0("loading::", l$key)),
    setNames(w$weight, paste0("weight::", w$key)))
}

.pls_bca_limits <- function(theta0, boot, jack, level) {
  alpha <- (1 - level) / 2
  z0 <- stats::qnorm(mean(boot < theta0, na.rm = TRUE))
  jbar <- mean(jack, na.rm = TRUE)
  num <- sum((jbar - jack)^3, na.rm = TRUE)
  den <- 6 * (sum((jbar - jack)^2, na.rm = TRUE)^(3/2))
  a <- if (is.finite(den) && den > 0) num / den else 0
  z <- stats::qnorm(c(alpha, 1 - alpha))
  adj <- stats::pnorm(z0 + (z0 + z) / (1 - a * (z0 + z)))
  .pls_quantile(boot, adj)
}

#' Bootstrap a fitted PLS-SEM model
#'
#' @param fit Fitted model.
#' @param R Number of bootstrap replications. This is deliberately user-controlled.
#' @param seed Optional seed.
#' @param level Confidence level.
#' @param interval `percentile`, `basic`, or `bca`.
#' @param unit Resampling unit. `cluster` performs cluster bootstrap.
#' @param cluster For `unit = "cluster"`, the **name** of a column in the fitted
#'   data holding the group labels, given as a single string, for example
#'   `cluster = "farm_id"`. A vector of labels is rejected, because the
#'   resampling groups are read from the column. Report the number of clusters
#'   next to the interval: a cluster bootstrap over very few groups can give a
#'   **narrower** interval than the case bootstrap rather than a wider one.
#' @param parallel Logical; native parallelization is currently not automatic.
#' @return A `plssem_bootstrap` object containing draws and confidence intervals.
#' @export
pls_bootstrap <- function(fit, R = 999L, seed = NULL, level = 0.95,
                          interval = c("percentile", "basic", "bca"),
                          unit = c("case", "cluster"), cluster = NULL, parallel = FALSE) {
  .pls_check_fit(fit); interval <- match.arg(interval); unit <- match.arg(unit)
  R <- as.integer(R); if (R < 20L) .pls_warn("Very small R: confidence intervals may be unstable. This may be acceptable only for code demonstrations.")
  .pls_st <- .pls_rng_save(); on.exit(.pls_rng_restore(.pls_st), add = TRUE)
  if (!is.null(seed)) set.seed(seed)
  if (fit$engine == "cSEM") {
    res <- cSEM::resamplecSEMResults(fit$backend, .resample_method = "bootstrap", .R = R, .seed = seed)
    return(structure(list(engine = "cSEM", backend = res, R = R, seed = seed,
                          level = level, interval = interval), class = "plssem_bootstrap"))
  }
  if (fit$engine == "plssem") {
    .pls_abort("For plssem-backed fits, request bootstrap during pls_fit() through backend arguments (bootstrap=TRUE, boot.R=...).")
  }
  if (fit$engine != "native") .pls_abort("Unified bootstrap is currently implemented for native and cSEM engines.")
  n <- nrow(fit$data)
  theta0 <- .pls_boot_stat_native(fit, seq_len(n))
  draws <- matrix(NA_real_, nrow = R, ncol = length(theta0), dimnames = list(NULL, names(theta0)))
  if (unit == "cluster") {
    # Validate the *type* before using it. `%in%` with a vector on the left
    # returns a vector, and `!vector` inside `if` fails with the internal
    # "coercion to logical(1)" message, which says nothing about clusters.
    if (is.null(cluster)) {
      .pls_abort(paste(
        "Cluster bootstrap needs the NAME of a column in the data, for example",
        "cluster = 'farm_id'. It does not take the vector of group labels."))
    }
    if (!is.character(cluster) || length(cluster) != 1L) {
      .pls_abort(paste(
        "cluster must be a single column name given as a character string,",
        "for example cluster = 'farm_id'. A vector of group labels was given;",
        "the resampling groups are read from the column inside the data."))
    }
    if (!cluster %in% names(fit$data)) {
      .pls_abort(paste0("Column '", cluster, "' is not in the data. Available: ",
                        paste(names(fit$data), collapse = ", "), "."))
    }
    cl <- unique(fit$data[[cluster]])
    if (length(cl) < 2L) {
      .pls_abort("Cluster bootstrap needs at least two distinct groups in the column.")
    }
  }
  for (b in seq_len(R)) {
    if (unit == "case") {
      idx <- sample.int(n, n, replace = TRUE)
    } else {
      chosen <- sample(cl, length(cl), replace = TRUE)
      idx <- unlist(lapply(chosen, function(g) which(fit$data[[cluster]] == g)), use.names = FALSE)
    }
    val <- try(.pls_boot_stat_native(fit, idx), silent = TRUE)
    if (!inherits(val, "try-error")) draws[b, names(val)] <- val
  }
  alpha <- (1 - level) / 2
  ci <- matrix(NA_real_, nrow = length(theta0), ncol = 2,
               dimnames = list(names(theta0), c("conf_low", "conf_high")))
  jack <- NULL
  if (interval == "bca") {
    if (n > 500L) .pls_warn("BCa requires jackknife refits and may be expensive for n > 500.")
    jack <- matrix(NA_real_, nrow = n, ncol = length(theta0), dimnames = list(NULL, names(theta0)))
    for (i in seq_len(n)) {
      val <- try(.pls_boot_stat_native(fit, setdiff(seq_len(n), i)), silent = TRUE)
      if (!inherits(val, "try-error")) jack[i, names(val)] <- val
    }
  }
  for (j in seq_along(theta0)) {
    b <- draws[, j]
    if (interval == "percentile") ci[j, ] <- .pls_quantile(b, c(alpha, 1 - alpha))
    if (interval == "basic") {
      q <- .pls_quantile(b, c(1 - alpha, alpha)); ci[j, ] <- 2 * theta0[j] - q
    }
    if (interval == "bca") ci[j, ] <- .pls_bca_limits(theta0[j], b, jack[, j], level)
  }
  parts <- sub("::.*$", "", names(theta0))
  labels <- sub("^[^:]+::", "", names(theta0))
  tab <- data.frame(parameter = names(theta0), component = parts, label = labels,
                    estimate = as.numeric(theta0), boot_mean = colMeans(draws, na.rm = TRUE),
                    boot_se = apply(draws, 2, stats::sd, na.rm = TRUE),
                    conf_low = ci[, 1], conf_high = ci[, 2], stringsAsFactors = FALSE)
  structure(list(engine = "native", draws = draws, table = tab, R = R, seed = seed,
                 level = level, interval = interval, unit = unit,
                 # Record the grouping actually used, not the column name, so the
                 # object can be audited. The name is kept separately.
                 cluster = if (identical(unit, "cluster")) fit$data[[cluster]] else NULL,
                 cluster_name = if (identical(unit, "cluster")) cluster else NULL,
                 n_clusters = if (identical(unit, "cluster")) length(cl) else NULL,
                 convergence_fraction = mean(stats::complete.cases(draws))), class = "plssem_bootstrap")
}

#' Print bootstrap results
#' @param x Bootstrap object.
#' @param ... Unused.
#' @export
print.plssem_bootstrap <- function(x, ...) {
  cat("<plsSEMflow bootstrap>
")
  cat("Engine:", x$engine, "| R:", x$R, "| CI:", x$interval, "| level:", x$level, "
")
  if (!is.null(x$table)) print(utils::head(x$table, 12), row.names = FALSE)
  invisible(x)
}

#' Extract bootstrap intervals
#' @param boot Bootstrap object.
#' @param component Optional component: `path`, `loading`, or `weight`.
#' @return Data frame of intervals.
#' @export
pls_boot_ci <- function(boot, component = NULL) {
  if (!inherits(boot, "plssem_bootstrap")) .pls_abort("Expected a plssem_bootstrap object.")
  if (boot$engine != "native") .pls_abort("Unified CI extraction is currently available for native bootstrap objects; use cSEM::infer() for cSEM bootstrap results.")
  out <- boot$table
  if (!is.null(component)) out <- out[out$component %in% component, , drop = FALSE]
  out
}
