#' Define a reflective construct
#'
#' @param name Construct name.
#' @param indicators Character vector of indicator names.
#' @return A measurement-construct specification.
#' @export
pls_reflective <- function(name, indicators) {
  structure(list(name = as.character(name), indicators = as.character(indicators),
                 type = "reflective", mode = "A"), class = "plssem_construct")
}

#' Define a composite construct
#'
#' @param name Construct name.
#' @param indicators Character vector of indicator names.
#' @param mode Outer-weight mode. Mode B is the default for composites.
#' @return A measurement-construct specification.
#' @export
pls_composite <- function(name, indicators, mode = c("B", "A")) {
  mode <- match.arg(mode)
  structure(list(name = as.character(name), indicators = as.character(indicators),
                 type = "composite", mode = mode), class = "plssem_construct")
}

#' Define a formative composite
#'
#' @inheritParams pls_composite
#' @return A formative-composite specification.
#' @export
pls_formative <- function(name, indicators, mode = c("B", "A")) {
  mode <- match.arg(mode)
  x <- pls_composite(name, indicators, mode)
  x$type <- "formative"
  x
}

#' Define a higher-order construct
#'
#' @param name Higher-order construct name.
#' @param dimensions Names of lower-order constructs.
#' @param type Either `reflective` or `composite`.
#' @param approach Higher-order estimation strategy.
#' @return A higher-order construct specification.
#' @export
pls_higher_order <- function(name, dimensions,
                             type = c("reflective", "composite"),
                             approach = c("two_stage", "mixed", "repeated")) {
  type <- match.arg(type); approach <- match.arg(approach)
  structure(list(name = name, indicators = as.character(dimensions), type = "higher_order",
                 higher_type = type, mode = if (type == "reflective") "A" else "B",
                 approach = approach), class = "plssem_construct")
}

#' Combine measurement specifications
#'
#' @param ... Objects created by `pls_reflective()`, `pls_composite()`,
#'   `pls_formative()`, or `pls_higher_order()`.
#' @return A measurement model.
#' @export
pls_measurement <- function(...) {
  x <- list(...)
  if (!length(x) || !all(vapply(x, inherits, logical(1), "plssem_construct"))) {
    .pls_abort("All elements of pls_measurement() must be construct specifications.")
  }
  structure(list(constructs = x), class = "plssem_measurement")
}

#' Define a direct structural path
#' @param from Predictor construct.
#' @param to Outcome construct.
#' @return A structural relation specification.
#' @export
pls_path <- function(from, to) structure(list(type = "path", from = from, to = to), class = "plssem_relation")

#' Define mediation
#' @param from Antecedent construct.
#' @param via Mediator construct.
#' @param to Outcome construct.
#' @param direct Include the direct path from antecedent to outcome.
#' @return A mediation specification.
#' @export
pls_mediation <- function(from, via, to, direct = TRUE) {
  structure(list(type = "mediation", from = from, via = via, to = to, direct = isTRUE(direct)), class = "plssem_relation")
}

#' Define moderation
#' @param predictor Predictor construct.
#' @param moderator Moderator construct.
#' @param outcome Outcome construct.
#' @param method Interaction construction method.
#' @return A moderation specification.
#' @export
pls_moderation <- function(predictor, moderator, outcome,
                           method = c("two_stage", "product_indicator", "orthogonal")) {
  method <- match.arg(method)
  structure(list(type = "moderation", predictor = predictor, moderator = moderator,
                 outcome = outcome, method = method), class = "plssem_relation")
}

#' Define a quadratic structural effect
#' @param predictor Predictor construct.
#' @param outcome Outcome construct.
#' @return A quadratic-effect specification.
#' @export
pls_quadratic <- function(predictor, outcome) {
  structure(list(type = "quadratic", predictor = predictor, outcome = outcome), class = "plssem_relation")
}

#' Define a nonlinear structural relation
#' @param predictor Predictor construct.
#' @param outcome Outcome construct.
#' @param degree Polynomial degree for native score-based sensitivity analysis.
#' @return A nonlinear-effect specification.
#' @export
pls_nonlinear <- function(predictor, outcome, degree = 2L) {
  structure(list(type = "nonlinear", predictor = predictor, outcome = outcome,
                 degree = as.integer(degree)), class = "plssem_relation")
}

#' Define a cluster variable for multilevel extensions
#' @param cluster Name of the cluster column in the data.
#' @return A cluster relation specification.
#' @export
pls_cluster <- function(cluster) {
  structure(list(type = "cluster", cluster = as.character(cluster)), class = "plssem_relation")
}

#' Combine structural relations
#' @param ... Structural relation specifications.
#' @return A structural model.
#' @export
pls_structural <- function(...) {
  x <- list(...)
  if (!all(vapply(x, inherits, logical(1), "plssem_relation"))) {
    .pls_abort("All elements of pls_structural() must be structural relation specifications.")
  }
  structure(list(relations = x), class = "plssem_structural")
}

#' Build a complete PLS-SEM specification
#'
#' @param measurement Measurement model.
#' @param structural Structural model.
#' @param name Optional model name.
#' @return A `plssem_model` object.
#' @export
pls_model <- function(measurement, structural, name = NULL) {
  if (!inherits(measurement, "plssem_measurement")) .pls_abort("Invalid measurement model.")
  if (!inherits(structural, "plssem_structural")) .pls_abort("Invalid structural model.")
  out <- structure(list(measurement = measurement, structural = structural,
                        name = name %||% "PLS-SEM model"), class = "plssem_model")
  .pls_validate_model(out)
  out
}

#' Convert a specification to backend syntax
#' @param model A `plssem_model`.
#' @param higher_order Include higher-order definitions.
#' @param dialect Syntax dialect: `cSEM`, `plssem`, or `lavaan`.
#'
#'   The argument only changes the **higher-order** construct line. For a model
#'   with no higher-order construct the three dialects return identical text,
#'   which you can confirm with `identical()`. When a higher-order construct is
#'   present, `cSEM` writes the two-step operator `<~` and `plssem` and `lavaan`
#'   write `=~`; structural and measurement lines are the same in all three.
#' @return Character string containing model syntax.
#' @export
pls_syntax <- function(model, higher_order = TRUE,
                       dialect = c("cSEM", "plssem", "lavaan")) {
  .pls_validate_model(model)
  dialect <- match.arg(dialect)
  p <- .pls_paths_df(model)
  rhs <- list()
  if (nrow(p)) {
    for (i in seq_len(nrow(p))) rhs[[p$to[i]]] <- unique(c(rhs[[p$to[i]]], p$from[i]))
  }
  # Add explicitly modeled nonlinear relations.
  for (r in model$structural$relations) {
    if (identical(r$type, "moderation")) {
      term <- if (dialect == "cSEM") paste(r$predictor, r$moderator, sep = ".") else paste(r$predictor, r$moderator, sep = ":")
      rhs[[r$outcome]] <- unique(c(rhs[[r$outcome]], r$predictor, r$moderator, term))
    }
    if (identical(r$type, "quadratic")) {
      term <- if (dialect == "cSEM") paste(r$predictor, r$predictor, sep = ".") else paste(r$predictor, r$predictor, sep = ":")
      rhs[[r$outcome]] <- unique(c(rhs[[r$outcome]], r$predictor, term))
    }
    if (identical(r$type, "nonlinear")) {
      terms <- r$predictor
      if (r$degree >= 2L) {
        for (d in 2:r$degree) {
          terms <- c(terms, if (dialect == "cSEM") paste(rep(r$predictor, d), collapse = ".") else paste(rep(r$predictor, d), collapse = ":"))
        }
      }
      rhs[[r$outcome]] <- unique(c(rhs[[r$outcome]], terms))
    }
  }
  lines <- character()
  if (length(rhs)) for (to in names(rhs)) lines <- c(lines, paste0(to, " ~ ", paste(rhs[[to]], collapse = " + ")))
  for (cst in model$measurement$constructs) {
    if (identical(cst$type, "higher_order")) {
      if (!higher_order) next
      op <- if (identical(cst$higher_type, "reflective")) "=~" else if (dialect == "cSEM") "<~" else "=~"
    } else {
      op <- if (identical(cst$type, "reflective")) "=~" else if (dialect == "cSEM") "<~" else "=~"
    }
    lines <- c(lines, paste(cst$name, op, paste(cst$indicators, collapse = " + ")))
  }
  paste(lines, collapse = "\n")
}

#' Print a PLS-SEM model specification
#' @param x A `plssem_model`.
#' @param ... Unused.
#' @export
print.plssem_model <- function(x, ...) {
  cat("<plsSEMflow model>
")
  cat("Name:", x$name, "
")
  cat("Constructs:", paste(.pls_construct_names(x), collapse = ", "), "
")
  cat("
Syntax:
", pls_syntax(x), "
", sep = "")
  invisible(x)
}
