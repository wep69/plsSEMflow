# Unobserved heterogeneity -------------------------------------------------

#' REBUS-PLS unobserved heterogeneity analysis
#'
#' Uses the classical `plspm` REBUS workflow as an optional specialist
#' backend. This function is intended for sensitivity analysis when
#' theoretically defensible observed groups do not explain all heterogeneity.
#'
#' @param fit A `plssem_fit` estimated with `engine = "plspm"`.
#' @param data Optional data frame supplied to `plspm::rebus.pls()` when
#'   required by the installed plspm version.
#' @param stop_crit Convergence criterion for REBUS iterations.
#' @param iter_max Maximum number of REBUS iterations.
#' @param ... Additional arguments passed to `plspm::rebus.pls()`.
#' @return A `plssem_heterogeneity` object containing the backend result and
#'   analysis metadata.
#' @export
pls_rebus <- function(fit, data = NULL, stop_crit = 0.005, iter_max = 100L, ...) {
  .pls_check_fit(fit)
  if (fit$engine != "plspm") {
    .pls_abort("REBUS currently requires a model fitted with engine='plspm'.")
  }
  .pls_require("plspm", "for REBUS-PLS unobserved heterogeneity analysis.")
  obj <- plspm::rebus.pls(
    fit$backend,
    Y = data,
    stop.crit = stop_crit,
    iter.max = as.integer(iter_max),
    ...
  )
  structure(
    list(
      method = "REBUS-PLS",
      engine = "plspm",
      backend = obj,
      stop_crit = stop_crit,
      iter_max = as.integer(iter_max),
      note = paste(
        "Unobserved-heterogeneity sensitivity analysis.",
        "Substantive interpretation should be checked against theory and observed grouping variables."
      )
    ),
    class = "plssem_heterogeneity"
  )
}

#' Route an unobserved-heterogeneity workflow
#'
#' @param fit Fitted PLS-SEM model.
#' @param method Heterogeneity method. `REBUS` is executable through plspm.
#'   `FIMIX` and `PLS-POS` are registered as external specialist workflows
#'   because no stable open R backend is assumed by plsSEMflow.
#' @param ... Additional method-specific arguments.
#' @return A heterogeneity result or an explicit external-workflow descriptor.
#' @export
pls_heterogeneity <- function(fit, method = c("REBUS", "FIMIX", "PLS-POS"), ...) {
  method <- match.arg(method)
  if (method == "REBUS") return(pls_rebus(fit, ...))
  structure(
    list(
      method = method,
      executable = FALSE,
      recommended_external = "SmartPLS",
      fit_engine = fit$engine,
      message = paste0(
        method,
        " is exposed as an auditable external-validation workflow rather than emulated by an unrelated estimator. ",
        "Export inputs/results with pls_export_external(), run the specialist software, then compare with pls_compare_external()."
      )
    ),
    class = "plssem_external_workflow"
  )
}

#' Print a heterogeneity result
#' @param x A `plssem_heterogeneity` object.
#' @param ... Unused.
#' @export
print.plssem_heterogeneity <- function(x, ...) {
  cat("<plsSEMflow heterogeneity analysis>\n")
  cat("Method:", x$method, "| Engine:", x$engine, "\n")
  cat(x$note, "\n")
  invisible(x)
}

#' Print an external specialist workflow descriptor
#' @param x A `plssem_external_workflow` object.
#' @param ... Unused.
#' @export
print.plssem_external_workflow <- function(x, ...) {
  cat("<plsSEMflow external specialist workflow>\n")
  cat("Method:", x$method, "| Executable internally:", x$executable, "\n")
  cat(x$message, "\n")
  invisible(x)
}
