# Optional backend adapters ------------------------------------------------

.pls_fit_seminr <- function(model, data, estimator = "PLS", ...) {
  .pls_require("seminr")
  cs <- model$measurement$constructs
  if (any(vapply(cs, function(x) x$type == "higher_order", logical(1)))) {
    .pls_abort("Use the cSEM or plssem backend for higher-order models in this development version.")
  }
  mm_calls <- vector("list", length(cs))
  for (i in seq_along(cs)) {
    inds <- cs[[i]]$indicators
    prefix <- sub("[0-9]+$", "", inds[1])
    suffix <- suppressWarnings(as.integer(sub(paste0("^", prefix), "", inds)))
    if (anyNA(suffix) || !identical(inds, paste0(prefix, suffix))) {
      .pls_abort("SEMinR adapter currently requires indicator names with a shared prefix and numeric suffix, e.g. soil1, soil2, soil3. Use engine='native' or 'cSEM' for arbitrary names.")
    }
    item_spec <- seminr::multi_items(prefix, suffix)
    mm_calls[[i]] <- if (cs[[i]]$type == "reflective") {
      seminr::reflective(cs[[i]]$name, item_spec)
    } else {
      seminr::composite(cs[[i]]$name, item_spec)
    }
  }
  mm <- do.call(seminr::constructs, mm_calls)
  p <- .pls_paths_df(model)
  rel <- list()
  if (nrow(p)) {
    by_to <- split(p$from, p$to)
    rel <- lapply(names(by_to), function(to) seminr::paths(from = unique(by_to[[to]]), to = to))
  }
  sm <- do.call(seminr::relationships, rel)
  seminr::estimate_pls(data = data, measurement_model = mm, structural_model = sm, ...)
}

.pls_fit_plspm <- function(model, data, ...) {
  .pls_require("plspm")
  cn <- .pls_construct_names(model)
  p <- .pls_paths_df(model)
  inner <- matrix(0, length(cn), length(cn), dimnames = list(cn, cn))
  if (nrow(p)) for (i in seq_len(nrow(p))) inner[p$to[i], p$from[i]] <- 1
  blocks <- lapply(model$measurement$constructs, `[[`, "indicators")
  modes <- vapply(model$measurement$constructs, function(x) if (x$mode == "A") "A" else "B", character(1))
  plspm::plspm(data, inner, blocks, modes = modes, ...)
}

#' Run cSEM post-estimation assessment
#' @param fit A `plssem_fit` estimated with cSEM.
#' @param ... Passed to `cSEM::assess()`.
#' @return cSEM assessment object.
#' @export
pls_csem_assess <- function(fit, ...) {
  .pls_check_fit(fit); if (fit$engine != "cSEM") .pls_abort("fit must use engine='cSEM'.")
  cSEM::assess(fit$backend, ...)
}

#' Run cSEM CVPAT
#'
#' CVPAT is a *predictive* validity test and therefore needs two models: one
#' estimated on an earlier stage of the data and one on a later stage.
#' `cSEM::testCVPAT()` takes both as `.object1` and `.object2`.
#'
#' @param fit A cSEM-backed fit, or a list of two cSEM-backed fits.
#' @param fit2 Optional second cSEM-backed fit, when `fit` is a single object.
#' @param ... Passed to `cSEM::testCVPAT()`.
#' @return Backend test result.
#' @export
pls_cvpat <- function(fit, fit2 = NULL, ...) {
  if (is.list(fit) && !inherits(fit, "plssem_fit")) {
    fit2 <- fit[[2]]
    fit  <- fit[[1]]
  }
  .pls_check_fit(fit)
  if (fit$engine != "cSEM") .pls_abort("CVPAT currently requires a cSEM-backed fit.")
  if (is.null(fit2)) {
    .pls_abort(paste(
      "CVPAT compares predictive validity across two data stages and needs two fits.",
      "Call pls_cvpat(fit_early, fit_late) or pls_cvpat(list(fit_early, fit_late))."))
  }
  .pls_check_fit(fit2)
  if (fit2$engine != "cSEM") .pls_abort("Both CVPAT fits must be cSEM-backed.")
  cSEM::testCVPAT(fit$backend, fit2$backend, ...)
}

#' Run MICOM using cSEM
#' @param fit A cSEM multigroup fit.
#' @param R Number of permutation runs selected by the user.
#' @param seed Optional seed.
#' @param ... Additional backend arguments.
#' @return MICOM result.
#' @export
pls_micom <- function(fit, R = 4999L, seed = NULL, ...) {
  .pls_check_fit(fit); if (fit$engine != "cSEM") .pls_abort("MICOM currently requires engine='cSEM'.")
  cSEM::testMICOM(fit$backend, .R = as.integer(R), .seed = seed, ...)
}

#' Test multigroup differences using cSEM
#'
#' MGD compares structural parameters **between groups**, so it needs a model
#' estimated per group. cSEM produces the required `cSEMResults_multi` object
#' when it receives a *list* of data frames, which is what this function does;
#' passing a single fitted model makes cSEM answer "At least two groups
#' required". The signature mirrors [pls_mga()], which does the same split with
#' the native engine.
#'
#' @param model A `plssem_model` estimated once per group.
#' @param data Data frame containing the grouping column.
#' @param group Name of the column holding the group labels. At least two
#'   distinct levels are required.
#' @param ... Additional arguments passed to `cSEM::csem()` and
#'   `cSEM::testMGD()`.
#' @return Multigroup-difference result.
#' @export
pls_mgd <- function(model, data, group, ...) {
  if (inherits(model, "plssem_fit")) {
    .pls_abort(paste(
      "pls_mgd() takes the model, the data and the grouping column, not a fitted",
      "object: MGD re-estimates one model per group. Call",
      "pls_mgd(model, data, group = 'region'). For a native two-group comparison",
      "see pls_mga()."))
  }
  .pls_require("cSEM", "for multigroup difference testing.")
  .pls_validate_model(model)
  if (!is.character(group) || length(group) != 1L) {
    .pls_abort("group must be the name of a single column, for example group = 'region'.")
  }
  if (!group %in% names(data)) {
    .pls_abort(paste0("Column '", group, "' is not in the data. Available: ",
                      paste(names(data), collapse = ", "), "."))
  }
  levs <- unique(data[[group]])
  if (length(levs) < 2L) {
    .pls_abort(paste0("MGD needs at least two groups. Column '", group,
                      "' has a single level: ", as.character(levs[1]), "."))
  }
  por_grupo <- stats::setNames(
    lapply(levs, function(g) data[data[[group]] == g, , drop = FALSE]),
    as.character(levs)
  )
  res <- cSEM::csem(por_grupo, pls_syntax(model, dialect = "cSEM"), ...)
  cSEM::testMGD(res, ...)
}

#' Test endogeneity using the cSEM Hausman workflow
#'
#' The Hausman test needs an **instrumented** model, and cSEM takes the
#' instrument information at estimation time, not at test time. There is
#' therefore no argument to `pls_endogeneity()` that can supply instruments
#' after the fact; the fit must already contain them.
#'
#' @param fit A cSEM-backed fit estimated with an instrumented specification.
#' @param ... Passed to `cSEM::testHausman()`.
#' @return Backend test result.
#' @export
pls_endogeneity <- function(fit, ...) {
  .pls_check_fit(fit); if (fit$engine != "cSEM") .pls_abort("Endogeneity testing currently requires engine='cSEM'.")
  res <- try(cSEM::testHausman(fit$backend, ...), silent = TRUE)
  if (inherits(res, "try-error") &&
      grepl("Instruments required", conditionMessage(attr(res, "condition")), fixed = TRUE)) {
    .pls_abort(paste(
      "The Hausman test needs an instrumented model, and cSEM records the",
      "instruments at estimation time, so no argument can add them here.",
      "Re-estimate with pls_fit(..., engine = 'cSEM') using an instrumented",
      "specification, then call pls_endogeneity() on that fit."))
  }
  res
}

#' Importance-performance map analysis using cSEM
#' @param fit A cSEM-backed fit.
#' @param ... Passed to `cSEM::doIPMA()`.
#' @return IPMA result.
#' @export
pls_ipma <- function(fit, ...) {
  .pls_check_fit(fit); if (fit$engine != "cSEM") .pls_abort("IPMA currently requires engine='cSEM'.")
  cSEM::doIPMA(fit$backend, ...)
}

#' Nonlinear effects analysis using cSEM
#' @param fit A cSEM-backed fit.
#' @param ... Passed to `cSEM::doNonlinearEffectsAnalysis()`.
#' @return Backend nonlinear-effects result.
#' @export
pls_nonlinear_effects <- function(fit, ...) {
  .pls_check_fit(fit); if (fit$engine != "cSEM") .pls_abort("This nonlinear-effects analysis requires engine='cSEM'.")
  cSEM::doNonlinearEffectsAnalysis(fit$backend, ...)
}
