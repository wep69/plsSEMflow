# Advanced workflows -------------------------------------------------------

#' Fit a multilevel PLS-SEM model using plssem
#'
#' @param syntax Model syntax accepted by `plssem::pls()`, including random effects.
#' @param data Data frame.
#' @param bootstrap Bootstrap in the backend.
#' @param R User-selected number of bootstrap replications.
#' @param ordered Optional ordered indicators.
#' @param ... Additional arguments.
#' @return A backend object wrapped in `plssem_advanced_fit`.
#' @export
pls_multilevel <- function(syntax, data, bootstrap=TRUE, R=999L, ordered=NULL, ...) {
  .pls_require("plssem", "for multilevel PLS-SEM.")
  obj<-plssem::pls(syntax,data=data,bootstrap=bootstrap,boot.R=as.integer(R),ordered=ordered,...)
  structure(list(engine="plssem",type="multilevel",backend=obj,syntax=syntax,R=R),class="plssem_advanced_fit")
}

#' Fit an ordinal PLS/PLSc model using plssem
#' @inheritParams pls_multilevel
#' @return A wrapped advanced fit.
#' @export
pls_ordinal <- function(syntax, data, ordered = character(0), bootstrap=TRUE, R=999L, ...) {
  .pls_require("plssem", "for ordinal PLS-SEM.")
  if (!is.character(ordered)) {
    .pls_abort("ordered must be a character vector of indicator names, for example ordered = c('soil1','soil2').")
  }
  obj<-plssem::pls(syntax,data=data,bootstrap=bootstrap,boot.R=as.integer(R),ordered=ordered,...)
  structure(list(engine="plssem",type="ordinal",backend=obj,syntax=syntax,R=R),class="plssem_advanced_fit")
}

#' Bayesian covariance-based SEM companion analysis
#'
#' @param model A `plssem_model` containing only reflective constructs.
#' @param data Data frame.
#' @param ... Passed to `blavaan::bsem()`.
#' @return A Bayesian companion fit. This is explicitly not labeled Bayesian PLS-SEM.
#' @export
pls_bayes_compare <- function(model, data, ...) {
  .pls_require("blavaan", "for Bayesian covariance-based SEM companion analysis.")
  cs<-model$measurement$constructs
  if(any(vapply(cs,function(x)x$type!="reflective",logical(1)))) .pls_abort("Bayesian companion conversion currently requires reflective/common-factor constructs only.")
  syntax<-pls_syntax(model)
  # cSEM composite operator is absent because all constructs are reflective here.
  obj<-blavaan::bsem(model=syntax,data=data,...)
  structure(list(engine="blavaan",type="Bayesian CB-SEM companion",backend=obj,syntax=syntax),class="plssem_advanced_fit")
}

#' Compare engines on a common model
#'
#' @param model Model specification.
#' @param data Data frame.
#' @param engines Engines to attempt.
#' @param estimator Estimator.
#' @return List of fits and status table.
#' @export
pls_compare_engines <- function(model, data, engines=c("native","cSEM"), estimator="PLS") {
  fits<-list(); status<-list()
  for(e in engines){res<-try(pls_fit(model,data,engine=e,estimator=estimator),silent=TRUE); ok<-!inherits(res,"try-error"); if(ok) fits[[e]]<-res; status[[e]]<-data.frame(engine=e,ok=ok,message=if(ok)"estimated" else as.character(res))}
  # The comparison is what the function name promises, so compute it here and
  # return it. The native engine is the reference when present; otherwise the
  # first successful engine is used.
  ref_name <- if ("native" %in% names(fits)) "native" else names(fits)[1]
  comparison <- data.frame()
  if (length(fits) >= 2L && !is.na(ref_name)) {
    refp <- .pls_extract_backend_paths(fits[[ref_name]])
    if (!is.null(refp) && nrow(refp)) {
      refp$key <- paste0(refp$from, " -> ", refp$to)
      rows <- list()
      for (e in setdiff(names(fits), ref_name)) {
        ext <- .pls_extract_backend_paths(fits[[e]])
        if (is.null(ext) || !nrow(ext)) next
        ext$key <- paste0(ext$from, " -> ", ext$to)
        keys <- intersect(refp$key, ext$key)
        if (!length(keys)) next
        rr <- refp[match(keys, refp$key), ]
        ee <- ext[match(keys, ext$key), ]
        rows[[e]] <- data.frame(
          reference=ref_name, engine=e, path=keys,
          reference_estimate=rr$estimate, engine_estimate=ee$estimate,
          abs_diff=abs(rr$estimate-ee$estimate),
          stringsAsFactors=FALSE
        )
      }
      if (length(rows)) comparison <- do.call(rbind, rows)
    }
  }
  structure(list(fits=fits, status=do.call(rbind,status),
                 comparison=comparison, reference=ref_name),
            class="plssem_engine_comparison")
}
