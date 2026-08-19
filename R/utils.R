# Internal utilities -------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

.pls_abort <- function(..., call. = FALSE) {
  stop(paste0(...), call. = call.)
}

.pls_warn <- function(..., call. = FALSE) {
  warning(paste0(...), call. = call.)
}

.pls_require <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Optional package '", pkg, "' is required")
    if (!is.null(reason)) msg <- paste0(msg, " ", reason)
    .pls_abort(msg, ". Install it explicitly to use this backend or feature.")
  }
  invisible(TRUE)
}

.pls_check_numeric <- function(data, vars) {
  bad <- vars[!vapply(data[vars], is.numeric, logical(1))]
  if (length(bad)) {
    .pls_abort("The native engine requires numeric indicators. Non-numeric: ",
               paste(bad, collapse = ", "), ".")
  }
  invisible(TRUE)
}

.pls_standardize <- function(x) {
  x <- as.matrix(x)
  mu <- colMeans(x, na.rm = TRUE)
  s <- apply(x, 2, stats::sd, na.rm = TRUE)
  if (any(!is.finite(s) | s <= .Machine$double.eps)) {
    bad <- colnames(x)[!is.finite(s) | s <= .Machine$double.eps]
    .pls_abort("Zero or undefined variance in indicator(s): ", paste(bad, collapse = ", "), ".")
  }
  z <- sweep(sweep(x, 2, mu, "-"), 2, s, "/")
  list(z = z, center = mu, scale = s)
}

.pls_std_vec <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s <= .Machine$double.eps) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

.pls_safe_solve <- function(A, b = NULL) {
  if (is.null(b)) {
    tryCatch(solve(A), error = function(e) qr.solve(A))
  } else {
    tryCatch(solve(A, b), error = function(e) qr.solve(A, b))
  }
}

.pls_quantile <- function(x, probs) {
  stats::quantile(x[is.finite(x)], probs = probs, names = FALSE, type = 7, na.rm = TRUE)
}

.pls_construct_names <- function(model) {
  vapply(model$measurement$constructs, `[[`, character(1), "name")
}

.pls_manifest_names <- function(model) {
  cs <- model$measurement$constructs
  cs <- cs[!vapply(cs, function(x) identical(x$type, "higher_order"), logical(1))]
  unique(unlist(lapply(cs, `[[`, "indicators"), use.names = FALSE))
}

.pls_endogenous <- function(model) {
  p <- .pls_paths_df(model)
  unique(p$to)
}

.pls_exogenous <- function(model) {
  setdiff(.pls_construct_names(model), .pls_endogenous(model))
}

.pls_paths_df <- function(model) {
  rel <- model$structural$relations
  rows <- list()
  for (r in rel) {
    if (identical(r$type, "path")) {
      rows[[length(rows) + 1L]] <- data.frame(from = r$from, to = r$to, relation = "direct", stringsAsFactors = FALSE)
    } else if (identical(r$type, "mediation")) {
      rows[[length(rows) + 1L]] <- data.frame(from = c(r$from, r$via), to = c(r$via, r$to), relation = "mediation_component", stringsAsFactors = FALSE)
      if (isTRUE(r$direct)) rows[[length(rows) + 1L]] <- data.frame(from = r$from, to = r$to, relation = "direct", stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(data.frame(from = character(), to = character(), relation = character()))
  unique(do.call(rbind, rows))
}

.pls_validate_model <- function(model, data = NULL) {
  if (!inherits(model, "plssem_model")) .pls_abort("'model' must be created with pls_model().")
  cn <- .pls_construct_names(model)
  if (anyDuplicated(cn)) .pls_abort("Construct names must be unique.")
  p <- .pls_paths_df(model)
  unknown <- setdiff(unique(c(p$from, p$to)), cn)
  if (length(unknown)) .pls_abort("Unknown constructs in structural model: ", paste(unknown, collapse = ", "), ".")
  if (!is.null(data)) {
    missing <- setdiff(.pls_manifest_names(model), names(data))
    if (length(missing)) .pls_abort("Missing indicators in data: ", paste(missing, collapse = ", "), ".")
  }
  invisible(TRUE)
}

.pls_is_advanced <- function(model) {
  reltypes <- vapply(model$structural$relations, `[[`, character(1), "type")
  any(reltypes %in% c("moderation", "quadratic", "nonlinear", "cluster")) ||
    any(vapply(model$measurement$constructs, function(x) identical(x$type, "higher_order"), logical(1)))
}

.pls_check_fit <- function(x) {
  if (!inherits(x, "plssem_fit")) .pls_abort("Expected a 'plssem_fit' object.")
  invisible(TRUE)
}

.pls_ci_label <- function(est, low, high, digits = 2) {
  paste0(formatC(est, digits = digits, format = "f"), " [",
         formatC(low, digits = digits, format = "f"), ", ",
         formatC(high, digits = digits, format = "f"), "]")
}
