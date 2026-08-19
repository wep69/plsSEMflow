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
#' @param fit A cSEM-backed fit.
#' @param ... Passed to `cSEM::testCVPAT()`.
#' @return Backend test result.
#' @export
pls_cvpat <- function(fit, ...) {
  .pls_check_fit(fit); if (fit$engine != "cSEM") .pls_abort("CVPAT currently requires a cSEM-backed fit.")
  cSEM::testCVPAT(fit$backend, ...)
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
#' @param fit A cSEM multigroup fit.
#' @param ... Passed to `cSEM::testMGD()`.
#' @return Multigroup-difference result.
#' @export
pls_mgd <- function(fit, ...) {
  .pls_check_fit(fit); if (fit$engine != "cSEM") .pls_abort("MGD currently requires engine='cSEM'.")
  cSEM::testMGD(fit$backend, ...)
}

#' Test endogeneity using the cSEM Hausman workflow
#' @param fit A cSEM-backed fit.
#' @param ... Passed to `cSEM::testHausman()`.
#' @return Backend test result.
#' @export
pls_endogeneity <- function(fit, ...) {
  .pls_check_fit(fit); if (fit$engine != "cSEM") .pls_abort("Endogeneity testing currently requires engine='cSEM'.")
  cSEM::testHausman(fit$backend, ...)
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
