# Publication tables -------------------------------------------------------

#' Create publication-oriented tables
#'
#' @param fit Fitted model.
#' @param component paths, loadings, weights, reliability, htmt, r2, f2, vif, or audit.
#' @param boot `FALSE`, `TRUE`, or bootstrap object. Paths/loadings/weights can include bootstrap CIs.
#' @param R User-selected bootstrap replications when `boot=TRUE`.
#' @param seed Optional seed.
#' @param level Confidence level.
#' @param interval Bootstrap interval method.
#' @param format data.frame, gt, flextable, or markdown.
#' @param digits Display digits. Raw numeric values remain unchanged in the returned data when `format='data.frame'`.
#' @return Formatted table or data frame.
#' @export
pls_table <- function(fit,component=c("paths","loadings","weights","reliability","htmt","r2","f2","vif","audit"),
                      boot=FALSE,R=999L,seed=NULL,level=.95,interval="percentile",
                      format=c("data.frame","gt","flextable","markdown"),digits=3L){
  .pls_check_fit(fit);component<-match.arg(component);format<-match.arg(format)
  b<-.pls_get_boot(fit,boot,R,seed,level,interval)
  if(fit$engine!="native") .pls_abort("Unified table extraction is currently guaranteed for native fits. Use backend-specific functions for other engines.")
  ma<-pls_measurement_assess(fit);sa<-pls_structural_assess(fit)
  dat<-switch(component,paths=sa$paths,loadings=ma$loadings,weights=ma$weights,reliability=ma$reliability,
    htmt={H<-ma$htmt;if(is.null(H))data.frame() else {x<-as.data.frame(as.table(H));names(x)<-c("construct1","construct2","HTMT");x}},
    r2=sa$r2,f2=sa$f2,vif=sa$vif,audit={a<-pls_audit(fit);data.frame(level=c(rep("issue",length(a$issues)),rep("warning",length(a$warnings)),rep("note",length(a$notes))),message=c(a$issues,a$warnings,a$notes))})
  if(!is.null(b)&&b$engine=="native"&&component%in%c("paths","loadings","weights")){
    typ<-sub("s$","",component);ci<-b$table[b$table$component==typ,]
    if(component=="paths"){dat$label<-paste(dat$from,dat$to,sep=" -> ");m<-match(dat$label,ci$label)}
    else {sep<-if(component=="loadings")" =~ " else " <~ ";dat$label<-paste(dat$construct,dat$indicator,sep=sep);m<-match(dat$label,ci$label)}
    dat$boot_se<-ci$boot_se[m];dat$conf_low<-ci$conf_low[m];dat$conf_high<-ci$conf_high[m];dat$R_boot<-b$R;dat$CI_method<-b$interval
  }
  if(format=="data.frame") return(dat)
  disp<-dat; num<-vapply(disp,is.numeric,logical(1));disp[num]<-lapply(disp[num],round,digits=digits)
  if(format=="markdown") return(knitr::kable(disp,format="pipe"))
  if(format=="gt"){.pls_require("gt");return(gt::gt(disp))}
  .pls_require("flextable");flextable::flextable(disp)
}

#' Export a table to XLSX
#' @param table Data frame.
#' @param file Output file.
#' @export
pls_export_xlsx <- function(table,file){.pls_require("openxlsx");openxlsx::write.xlsx(table,file,overwrite=TRUE);invisible(file)}
