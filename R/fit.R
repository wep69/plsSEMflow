#' Inspect available engines and features
#'
#' @return A data frame with installation status, validation tier, and intended use.
#' @export
pls_capabilities <- function() {
  engines <- data.frame(
    engine = c("native", "cSEM", "seminr", "plssem", "plspm", "blavaan", "reticulate"),
    package = c(NA, "cSEM", "seminr", "plssem", "plspm", "blavaan", "reticulate"),
    tier = c(1L, 1L, 1L, 2L, 2L, 2L, 3L),
    role = c(
      "Continuous linear PLS-PM core, bootstrap, diagnostics and teaching",
      "Composite/factor PLS, PLSc, nonlinear, MICOM, MGD, CVPAT, endogeneity",
      "PLS/PLSc workflow, interactions, bootstrap and prediction",
      "Ordinal, nonlinear and multilevel PLS/PLSc",
      "Classical PLS-PM, nonmetric and REBUS workflows",
      "Bayesian covariance-based SEM companion analysis",
      "Optional Python interoperability"
    ), stringsAsFactors = FALSE
  )
  engines$installed <- vapply(engines$package, function(p) is.na(p) || requireNamespace(p, quietly = TRUE), logical(1))
  engines
}

.pls_choose_engine <- function(model, data, estimator, engine, ordered = NULL, multilevel = FALSE) {
  if (!identical(engine, "auto")) return(engine)
  if (isTRUE(multilevel) || length(ordered)) {
    if (requireNamespace("plssem", quietly = TRUE)) return("plssem")
  }
  if (identical(estimator, "PLSc") && requireNamespace("cSEM", quietly = TRUE)) return("cSEM")
  if (.pls_is_advanced(model)) {
    if (requireNamespace("cSEM", quietly = TRUE)) return("cSEM")
    if (requireNamespace("plssem", quietly = TRUE)) return("plssem")
  }
  "native"
}

#' Fit a PLS-SEM model
#'
#' @param model A model created with `pls_model()`.
#' @param data Data frame.
#' @param engine Estimation engine: `auto`, `native`, `cSEM`, `seminr`, `plssem`, or `plspm`.
#' @param estimator `PLS` or `PLSc`.
#' @param bootstrap Logical. If `TRUE`, bootstrap after fitting.
#' @param R Number of bootstrap replications chosen by the user.
#' @param seed Optional reproducibility seed.
#' @param level Confidence level.
#' @param interval Bootstrap interval method.
#' @param ordered Optional ordered indicator names for compatible backends.
#' @param multilevel Logical indicating that a multilevel-compatible backend is required.
#' @param ... Additional arguments passed to the selected backend.
#' @return A `plssem_fit` object.
#' @export
pls_fit <- function(model, data, engine = c("auto", "native", "cSEM", "seminr", "plssem", "plspm"),
                    estimator = c("PLS", "PLSc"), bootstrap = FALSE, R = 999L,
                    seed = NULL, level = 0.95, interval = c("percentile", "basic", "bca"),
                    ordered = NULL, multilevel = FALSE, ...) {
  engine <- match.arg(engine); estimator <- match.arg(estimator); interval <- match.arg(interval)
  .pls_validate_model(model, data)
  chosen <- .pls_choose_engine(model, data, estimator, engine, ordered, multilevel)
  audit <- list(requested_engine = engine, selected_engine = chosen, estimator = estimator,
                timestamp = as.character(Sys.time()), ordered = ordered, multilevel = multilevel)
  backend <- NULL; native <- NULL

  if (chosen == "native") {
    if (estimator == "PLSc") .pls_abort("The native engine estimates PLS-PM, not PLSc. Install cSEM/seminr or choose estimator='PLS'.")
    native <- .pls_native_fit(model, data, ...)
  } else if (chosen == "cSEM") {
    .pls_require("cSEM", "for the cSEM backend.")
    args <- list(.data = data, .model = pls_syntax(model, dialect = "cSEM"), .approach_weights = "PLS-PM",
                 .disattenuate = identical(estimator, "PLSc"), ...)
    backend <- do.call(cSEM::csem, args)
  } else if (chosen == "plssem") {
    .pls_require("plssem", "for ordinal, nonlinear, or multilevel PLS-SEM.")
    args <- list(model = pls_syntax(model), data = data, ordered = ordered, ...)
    # plssem::pls uses first unnamed model argument in current releases.
    args$model <- NULL
    backend <- do.call(plssem::pls, c(list(pls_syntax(model, dialect = "plssem"), data = data, ordered = ordered), list(...)))
  } else if (chosen == "seminr") {
    .pls_require("seminr", "for the SEMinR backend.")
    backend <- .pls_fit_seminr(model, data, estimator = estimator, ...)
  } else if (chosen == "plspm") {
    .pls_require("plspm", "for the plspm backend.")
    backend <- .pls_fit_plspm(model, data, ...)
  }

  out <- structure(list(
    call = match.call(), model = model, data = data, engine = chosen, estimator = estimator,
    native = native, backend = backend, audit = audit, bootstrap = NULL
  ), class = "plssem_fit")
  if (isTRUE(bootstrap)) out$bootstrap <- pls_bootstrap(out, R = R, seed = seed, level = level, interval = interval)
  out
}

#' Print a fitted PLS-SEM model
#' @param x A `plssem_fit`.
#' @param ... Unused.
#' @export
print.plssem_fit <- function(x, ...) {
  cat("<plsSEMflow fit>
")
  cat("Engine:", x$engine, "| Estimator:", x$estimator, "
")
  if (identical(x$engine, "native")) {
    cat("Observations used:", x$native$preprocessing$n_used, "
")
    cat("Iterations:", x$native$iterations, "| Converged:", x$native$converged, "
")
    if (nrow(x$native$paths)) print(x$native$paths, row.names = FALSE)
  } else {
    cat("Backend object class:", paste(class(x$backend), collapse = ", "), "
")
    cat("Use pls_backend_object() for backend-specific post-estimation.
")
  }
  invisible(x)
}

#' Extract the raw backend object
#' @param fit A fitted model.
#' @return Raw backend result or native result list.
#' @export
pls_backend_object <- function(fit) {
  .pls_check_fit(fit)
  if (identical(fit$engine, "native")) fit$native else fit$backend
}

#' Extract latent/composite scores
#' @param fit A fitted model.
#' @return Data frame of construct scores when available.
#' @export
pls_scores <- function(fit) {
  .pls_check_fit(fit)
  if (fit$engine == "native") return(fit$native$scores)
  .pls_abort("Unified score extraction is currently guaranteed for the native engine. Use pls_backend_object() for engine-specific scores.")
}
