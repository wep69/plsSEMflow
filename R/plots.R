# Publication-ready plotting ------------------------------------------------

.pls_get_boot <- function(fit, boot, R, seed, level, interval) {
  if (inherits(boot,"plssem_bootstrap")) return(boot)
  if (isTRUE(boot)) return(pls_bootstrap(fit,R=R,seed=seed,level=level,interval=interval))
  if (is.null(boot) && inherits(fit$bootstrap,"plssem_bootstrap")) return(fit$bootstrap)
  NULL
}

#' Publication-oriented PLS-SEM plots
#'
#' @param fit Fitted model.
#' @param type Plot type: paths, loadings, weights, bootstrap, htmt, prediction, or r2. Use `pls_plot_coefficients()` for moderation/nonlinear coefficient results and `pls_plot_mga()` for multigroup differences.
#' @param boot `FALSE`, `TRUE`, or a `plssem_bootstrap` object. When `TRUE`, the plot computes user-controlled bootstrap intervals.
#' @param R Number of bootstrap replications when `boot=TRUE`.
#' @param seed Optional seed.
#' @param level Confidence level.
#' @param interval Bootstrap interval type.
#' @param file Optional output file. Vector PDF/SVG is preferred for diagrams.
#' @param width,height Figure size in inches.
#' @param dpi Raster resolution.
#' @param ... Additional plot-specific arguments.
#' @return A ggplot object when ggplot2 is available, otherwise a base plotting result.
#' @export
pls_plot <- function(fit, type=c("paths","loadings","weights","bootstrap","htmt","prediction","r2"),
                     boot=FALSE,R=999L,seed=NULL,level=.95,interval="percentile",
                     file=NULL,width=8,height=6,dpi=600,...) {
  .pls_check_fit(fit); type<-match.arg(type); .pls_require("ggplot2","for publication-oriented plotting.")
  b<-.pls_get_boot(fit,boot,R,seed,level,interval)
  if(fit$engine!="native" && type!="bootstrap") .pls_abort("Unified publication plotting is currently guaranteed for native fits. Use backend plotting for other engines.")
  gg<-NULL
  if(type=="loadings" || type=="weights"){
    dat<-if(type=="loadings") fit$native$loadings else fit$native$weights
    names(dat)[3]<-"value"
    if(!is.null(b) && b$engine=="native"){
      ci<-b$table[b$table$component==sub("s$","",type),c("label","conf_low","conf_high")]
      dat$label<-paste(dat$construct,dat$indicator,sep=if(type=="loadings")" =~ " else " <~ ")
      dat<-merge(dat,ci,by="label",all.x=TRUE)
    }
    gg<-ggplot2::ggplot(dat,ggplot2::aes(x=stats::reorder(indicator,value),y=value))+
      ggplot2::geom_point(size=2)+ggplot2::facet_wrap(~construct,scales="free_x")+
      ggplot2::coord_flip()+ggplot2::labs(x=NULL,y=if(type=="loadings")"Outer loading" else "Outer weight")+
      ggplot2::theme_minimal(base_size=11)
    if(all(c("conf_low","conf_high")%in%names(dat))) gg<-gg+ggplot2::geom_errorbar(ggplot2::aes(ymin=conf_low,ymax=conf_high),width=.15)
  } else if(type=="r2"){
    dat<-data.frame(construct=names(fit$native$r2),R2=as.numeric(fit$native$r2));dat<-dat[is.finite(dat$R2),]
    gg<-ggplot2::ggplot(dat,ggplot2::aes(x=stats::reorder(construct,R2),y=R2))+ggplot2::geom_col()+ggplot2::coord_flip()+ggplot2::labs(x=NULL,y=expression(R^2))+ggplot2::theme_minimal(base_size=11)
  } else if(type=="htmt"){
    H<-pls_measurement_assess(fit)$htmt; if(is.null(H)) .pls_abort("HTMT requires at least two reflective constructs.")
    dat<-as.data.frame(as.table(H)); names(dat)<-c("construct1","construct2","HTMT")
    gg<-ggplot2::ggplot(dat,ggplot2::aes(construct1,construct2,fill=HTMT))+ggplot2::geom_tile()+ggplot2::geom_text(ggplot2::aes(label=sprintf("%.2f",HTMT)),size=3)+ggplot2::labs(x=NULL,y=NULL)+ggplot2::theme_minimal(base_size=11)
  } else if(type=="bootstrap"){
    if(is.null(b)) b<-pls_bootstrap(fit,R=R,seed=seed,level=level,interval=interval)
    dat<-b$table[b$table$component=="path",];gg<-ggplot2::ggplot(dat,ggplot2::aes(x=stats::reorder(label,estimate),y=estimate))+ggplot2::geom_point(size=2)+ggplot2::geom_errorbar(ggplot2::aes(ymin=conf_low,ymax=conf_high),width=.15)+ggplot2::geom_hline(yintercept=0,linetype=2)+ggplot2::coord_flip()+ggplot2::labs(x=NULL,y=paste0("Path coefficient with ",round(level*100),"% bootstrap CI"))+ggplot2::theme_minimal(base_size=11)
  } else if(type=="paths"){
    p<-fit$native$paths; cn<-.pls_construct_names(fit$model); n<-length(cn); angle<-seq(0,2*pi,length.out=n+1)[-(n+1)]; nodes<-data.frame(name=cn,x=cos(angle),y=sin(angle))
    p<-merge(p,nodes,by.x="from",by.y="name");names(p)[names(p)%in%c("x","y")]<-c("x0","y0");p<-merge(p,nodes,by.x="to",by.y="name");names(p)[names(p)%in%c("x","y")]<-c("x1","y1")
    p$label<-sprintf("%.2f",p$estimate)
    if(!is.null(b)&&b$engine=="native"){ci<-b$table[b$table$component=="path",]; ci$key<-sub(" -> ","__",ci$label,fixed=TRUE);p$key<-paste(p$from,p$to,sep="__");m<-match(p$key,ci$key);p$label<-.pls_ci_label(p$estimate,ci$conf_low[m],ci$conf_high[m],2)}
    gg<-ggplot2::ggplot()+ggplot2::geom_segment(data=p,ggplot2::aes(x=x0,y=y0,xend=x1,yend=y1),arrow=grid::arrow(length=grid::unit(0.15,"cm")),lineend="round")+ggplot2::geom_label(data=nodes,ggplot2::aes(x=x,y=y,label=name),size=3.5,label.size=.3)+ggplot2::geom_label(data=p,ggplot2::aes(x=(x0+x1)/2,y=(y0+y1)/2,label=label),size=2.8,label.size=0,fill="white")+ggplot2::coord_equal()+ggplot2::theme_void()+ggplot2::labs(caption=if(is.null(b))"Path coefficients" else paste0("Path coefficients with ",round(level*100),"% ",b$interval," bootstrap CI; R = ",b$R))
  } else if(type=="prediction"){
    pr<-pls_predict(fit,...);dat<-pr$summary;dat2<-rbind(data.frame(construct=dat$construct,method="PLS-SEM",RMSE=dat$RMSE),data.frame(construct=dat$construct,method="Linear benchmark",RMSE=dat$LM_RMSE));gg<-ggplot2::ggplot(dat2,ggplot2::aes(construct,RMSE,shape=method,group=method))+ggplot2::geom_point(position=ggplot2::position_dodge(width=.4),size=2)+ggplot2::labs(x=NULL,y="Cross-validated RMSE",shape=NULL)+ggplot2::theme_minimal(base_size=11)
  }
  if(!is.null(file)){ggplot2::ggsave(file,gg,width=width,height=height,dpi=dpi,units="in")}
  gg
}

#' Save a publication figure
#' @param plot ggplot object.
#' @param file Output filename.
#' @param width,height Size in inches.
#' @param dpi Raster resolution.
#' @export
pls_save_plot <- function(plot,file,width=8,height=6,dpi=600){.pls_require("ggplot2");ggplot2::ggsave(file,plot,width=width,height=height,dpi=dpi,units="in");invisible(file)}
