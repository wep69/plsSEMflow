# Optional Python and Julia bridges -----------------------------------------

#' Inspect optional non-R interoperability targets
#'
#' @return A data frame describing Python and Julia availability and scope.
#' @export
pls_external_capabilities <- function() {
  has_reticulate <- requireNamespace("reticulate", quietly = TRUE)
  py_seminr <- FALSE
  py_plspm <- FALSE
  if (has_reticulate) {
    py_seminr <- tryCatch(reticulate::py_module_available("seminr"), error = function(e) FALSE)
    py_plspm <- tryCatch(reticulate::py_module_available("plspm"), error = function(e) FALSE)
  }
  julia_path <- Sys.which("julia")
  data.frame(
    target = c("seminr-py", "plspm-python", "Julia/StructuralEquationModels.jl", "SmartPLS", "WarpPLS", "XLSTAT PLS-PM", "ADANCO"),
    available = c(py_seminr, py_plspm, nzchar(julia_path), NA, NA, NA, NA),
    role = c(
      "Optional Python PLS-SEM parity/benchmark backend",
      "Optional Python classical PLS-PM benchmark",
      "SEM research/benchmark bridge; not treated as native PLS-SEM",
      "External PLS-SEM validation and comparison",
      "External nonlinear PLS-SEM validation and comparison",
      "External spreadsheet PLS-PM validation",
      "External commercial validation when a licensed local installation is available"
    ),
    stringsAsFactors = FALSE
  )
}

#' Fit a PLS-SEM model using the optional seminr Python port
#'
#' @description
#' This function is an optional interoperability layer. It never installs Python
#' or Python packages. Indicator names must follow a common-prefix numeric-suffix
#' convention within each construct (for example `soil1`, `soil2`, `soil3`).
#'
#' @param model A `plssem_model`.
#' @param data Data frame.
#' @param ... Additional arguments passed to Python `estimate_pls()`.
#' @return A wrapped Python backend object.
#' @export
pls_python_seminr <- function(model, data, ...) {
  .pls_require("reticulate", "for optional Python interoperability.")
  if (!reticulate::py_module_available("seminr")) {
    .pls_abort("Python module 'seminr' is not available in the active Python environment. Python is optional; the R core remains fully usable.")
  }
  py <- reticulate::import("seminr", delay_load = FALSE)
  cs <- model$measurement$constructs
  if (any(vapply(cs, function(x) identical(x$type, "higher_order"), logical(1)))) {
    .pls_abort("The current Python bridge does not translate higher-order constructs automatically. Use the native Python API or an R backend for this model.")
  }
  mm_terms <- vector("list", length(cs))
  for (i in seq_along(cs)) {
    inds <- cs[[i]]$indicators
    prefix <- sub("[0-9]+$", "", inds[1])
    suffix <- suppressWarnings(as.integer(sub(paste0("^", prefix), "", inds)))
    if (anyNA(suffix) || !identical(inds, paste0(prefix, suffix))) {
      .pls_abort("The seminr-py bridge currently requires common-prefix numeric-suffix indicator names within each construct.")
    }
    items <- py$multi_items(prefix, as.list(suffix))
    if (cs[[i]]$type == "reflective") {
      mm_terms[[i]] <- py$reflective(cs[[i]]$name, items)
    } else {
      mm_terms[[i]] <- py$composite(cs[[i]]$name, items)
    }
  }
  mm <- do.call(py$constructs, mm_terms)
  p <- .pls_paths_df(model)
  by_to <- split(p$from, p$to)
  rel_terms <- lapply(names(by_to), function(to) py$paths(as.list(unique(by_to[[to]])), to))
  sm <- do.call(py$relationships, rel_terms)
  obj <- do.call(py$estimate_pls, c(list(reticulate::r_to_py(data), mm, sm), list(...)))
  structure(list(engine = "seminr-py", backend = obj, model = model), class = "plssem_advanced_fit")
}

#' Create a Julia companion-analysis bundle
#'
#' @description
#' Writes data and a lavaan-like reflective-model syntax that can be translated
#' to `StructuralEquationModels.jl`. This is a SEM comparison bridge and is not
#' represented as a PLS-SEM estimator.
#'
#' @param model A `plssem_model`.
#' @param data Data frame.
#' @param dir Output directory.
#' @return Output directory invisibly.
#' @export
pls_julia_bundle <- function(model, data, dir = "plssem_julia_bundle") {
  cs <- model$measurement$constructs
  if (any(vapply(cs, function(x) x$type != "reflective", logical(1)))) {
    .pls_warn("Julia StructuralEquationModels.jl is a covariance-based SEM framework. Composite/formative semantics are not translated automatically.")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, file.path(dir, "data.csv"), row.names = FALSE)
  writeLines(pls_syntax(model, dialect = "lavaan"), file.path(dir, "model_lavaan_like.txt"))
  readme <- c(
    "# Julia companion bundle",
    "",
    "This bundle is for optional SEM benchmarking with StructuralEquationModels.jl.",
    "It is not a PLS-SEM backend and should not be reported as such.",
    "Translate the supplied lavaan-like syntax to StenoGraph/ParameterTable syntax before fitting."
  )
  writeLines(readme, file.path(dir, "README.md"))
  invisible(normalizePath(dir, mustWork = FALSE))
}
