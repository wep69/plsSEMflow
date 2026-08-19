#' plsSEMflow: Integrated Partial Least Squares Structural Equation Modeling
#'
#' @description
#' An R-first integrator for PLS-SEM, bootstrap inference, model assessment,
#' prediction, advanced optional backends, publication-ready outputs, and
#' agronomic teaching workflows.
#'
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check notes for ggplot2 NSE variables
utils::globalVariables(c(
  ".term", "HTMT", "R2", "RMSE", "conf_high", "conf_low",
  "construct", "construct1", "construct2", "difference",
  "estimate", "indicator", "label", "method", "name", "path",
  "setNames", "value", "x", "x0", "x1", "y", "y0", "y1"
))
