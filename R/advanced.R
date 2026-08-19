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
pls_ordinal <- function(syntax, data, ordered, bootstrap=TRUE, R=999L, ...) {
  .pls_require("plssem", "for ordinal PLS-SEM.")
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
  structure(list(fits=fits,status=do.call(rbind,status)),class="plssem_engine_comparison")
}
