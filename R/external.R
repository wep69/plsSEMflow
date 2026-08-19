# External interoperability ------------------------------------------------

#' Export a model/data bundle for external comparison
#'
#' @param model Model specification.
#' @param data Data frame.
#' @param dir Destination directory.
#' @return Paths to exported files.
#' @export
pls_export_external <- function(model,data,dir="plssem_external_bundle"){
  dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(data,file.path(dir,"data.csv"),row.names=FALSE)
  writeLines(pls_syntax(model),file.path(dir,"model_csem_syntax.txt"))
  spec<-list(name=model$name,constructs=lapply(model$measurement$constructs,unclass),relations=lapply(model$structural$relations,unclass))
  if(requireNamespace("jsonlite",quietly=TRUE)) jsonlite::write_json(spec,file.path(dir,"model.json"),pretty=TRUE,auto_unbox=TRUE)
  invisible(normalizePath(dir,mustWork=FALSE))
}

#' Import a standardized external-results CSV
#' @param file CSV containing at least parameter, estimate, and optionally conf_low/conf_high.
#' @return Data frame.
#' @export
pls_import_external <- function(file){x<-utils::read.csv(file,check.names=FALSE);if(!all(c("parameter","estimate")%in%names(x))) .pls_abort("External result CSV must contain 'parameter' and 'estimate'.");x}

#' Compare native parameters with external results
#' @param fit Native fitted model.
#' @param external Data frame from `pls_import_external()`.
#' @return Parameter comparison table.
#' @export
pls_compare_external <- function(fit,external){.pls_check_fit(fit);if(fit$engine!="native") .pls_abort("Comparison currently uses native paths as the internal reference.");p<-fit$native$paths;p$parameter<-paste(p$from,p$to,sep=" -> ");m<-merge(p[,c("parameter","estimate")],external[,c("parameter","estimate")],by="parameter",suffixes=c("_internal","_external"));m$difference<-m$estimate_internal-m$estimate_external;m$abs_difference<-abs(m$difference);m}
