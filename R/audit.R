# Auditing -----------------------------------------------------------------

#' Audit a PLS-SEM specification or fitted model
#'
#' @param x A `plssem_model` or `plssem_fit`.
#' @param data Optional data when auditing a specification.
#' @return A structured audit with issues, warnings and contextual notes.
#' @export
pls_audit <- function(x, data=NULL) {
  model<-if(inherits(x,"plssem_fit")) x$model else x
  if(!inherits(model,"plssem_model")) .pls_abort("x must be a plssem_model or plssem_fit.")
  if(is.null(data) && inherits(x,"plssem_fit")) data<-x$data
  issues<-character(); warnings<-character(); notes<-character()
  .pls_validate_model(model, data)
  cs<-model$measurement$constructs
  for(cst in cs){
    if(cst$type=="reflective" && length(cst$indicators)<2) warnings<-c(warnings,paste0(cst$name,": reflective construct has fewer than two indicators."))
    if(cst$type%in%c("composite","formative") && length(cst$indicators)<2) notes<-c(notes,paste0(cst$name,": single-indicator composite; justify its interpretation."))
  }
  if(!is.null(data)){
    man<-.pls_manifest_names(model); n<-nrow(data)
    if(n<100) notes<-c(notes,"Sample size is below 100. Do not use a universal sample-size rule; evaluate power, model complexity, effect sizes, indicator properties and design context.")
    miss<-colMeans(is.na(data[,man,drop=FALSE])); if(any(miss>0)) warnings<-c(warnings,paste0("Missing data present in ",sum(miss>0)," manifest variables; define the missing-data strategy before final inference."))
  }
  if(inherits(x,"plssem_fit") && x$engine=="native"){
    ma<-pls_measurement_assess(x); sa<-pls_structural_assess(x)
    low<-ma$loadings[abs(ma$loadings$loading)<0.70,,drop=FALSE]
    if(nrow(low)) notes<-c(notes,paste0(nrow(low)," loading(s) below |0.70|. Do not delete indicators automatically; inspect content validity, reliability, AVE and theoretical coverage."))
    if(nrow(ma$reliability) && any(ma$reliability$AVE<0.50,na.rm=TRUE)) warnings<-c(warnings,"At least one reflective construct has AVE below 0.50.")
    if(!is.null(ma$htmt) && any(ma$htmt[upper.tri(ma$htmt)]>0.90,na.rm=TRUE)) warnings<-c(warnings,"At least one HTMT value exceeds 0.90; inspect discriminant validity and bootstrap HTMT when available.")
    if(any(sa$vif$VIF>5,na.rm=TRUE)) warnings<-c(warnings,"Structural predictor VIF above 5 detected.")
    if(is.null(x$bootstrap)) notes<-c(notes,"No bootstrap object is attached. Final coefficient reporting should include user-selected resampling uncertainty when inferential claims are made.")
  }
  structure(list(issues=unique(issues),warnings=unique(warnings),notes=unique(notes),model=model$name),class="plssem_audit")
}

#' Print an audit
#' @param x Audit object.
#' @param ... Unused.
#' @export
print.plssem_audit <- function(x, ...) {
  cat("<plsSEMflow audit>",x$model,"
")
  for(nm in c("issues","warnings","notes")){cat("
",toupper(nm),"
",sep="");v<-x[[nm]];if(!length(v))cat("  None detected by automated checks.
") else for(z in v)cat(" - ",z,"
",sep="")}
  invisible(x)
}
