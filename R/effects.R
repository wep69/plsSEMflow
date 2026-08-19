# Effects, mediation, moderation, multigroup ------------------------------

#' Estimate direct, indirect, and total effects
#'
#' @param fit Native fit.
#' @param boot Optional native bootstrap object. When supplied, direct effects
#'   reuse their configured bootstrap intervals and derived indirect/total
#'   effects receive percentile intervals computed from the stored bootstrap
#'   path draws.
#' @param level Confidence level for derived-effect intervals when `boot` is
#'   supplied. Defaults to the level stored in the bootstrap object.
#' @return Data frame of direct, indirect, and total effects.
#' @export
pls_effects <- function(fit, boot = NULL, level = NULL) {
  .pls_check_fit(fit)
  if (fit$engine != "native") .pls_abort("Unified effect decomposition currently requires the native engine.")
  p <- fit$native$paths
  direct <- if (nrow(p)) data.frame(from=p$from,to=p$to,estimate=p$estimate,effect="direct",stringsAsFactors=FALSE) else data.frame()
  rows <- if (nrow(direct)) list(direct) else list()
  medrels <- Filter(function(r) r$type == "mediation", fit$model$structural$relations)
  for (r in medrels) {
    a <- p$estimate[p$from==r$from & p$to==r$via]
    b <- p$estimate[p$from==r$via & p$to==r$to]
    if (length(a) && length(b)) {
      ind <- a[1]*b[1]
      rows[[length(rows)+1L]] <- data.frame(from=r$from,to=r$to,estimate=ind,
        effect=paste0("indirect via ",r$via),stringsAsFactors=FALSE)
      d <- p$estimate[p$from==r$from & p$to==r$to]
      if (length(d)) rows[[length(rows)+1L]] <- data.frame(from=r$from,to=r$to,
        estimate=d[1]+ind,effect=paste0("total via ",r$via),stringsAsFactors=FALSE)
    }
  }
  out <- if(length(rows)) unique(do.call(rbind,rows)) else data.frame(from=character(),to=character(),estimate=numeric(),effect=character())
  rownames(out) <- NULL

  if (!is.null(boot)) {
    if (!inherits(boot,"plssem_bootstrap") || boot$engine!="native") .pls_abort("boot must be a native plssem_bootstrap object.")
    if (is.null(level)) level <- boot$level
    alpha <- (1-level)/2
    out$boot_se <- NA_real_; out$conf_low <- NA_real_; out$conf_high <- NA_real_; out$CI_method <- NA_character_; out$R_boot <- boot$R
    for (i in seq_len(nrow(out))) {
      if (out$effect[i] == "direct") {
        lab <- paste(out$from[i],out$to[i],sep=" -> ")
        z <- boot$table[boot$table$component=="path" & boot$table$label==lab,,drop=FALSE]
        if (nrow(z)) { out$boot_se[i]<-z$boot_se[1];out$conf_low[i]<-z$conf_low[1];out$conf_high[i]<-z$conf_high[1];out$CI_method[i]<-boot$interval }
      } else {
        via <- sub("^(indirect|total) via ","",out$effect[i])
        ka <- paste0("path::",out$from[i]," -> ",via)
        kb <- paste0("path::",via," -> ",out$to[i])
        if (all(c(ka,kb)%in%colnames(boot$draws))) {
          draws <- boot$draws[,ka]*boot$draws[,kb]
          if (grepl("^total",out$effect[i])) {
            kd <- paste0("path::",out$from[i]," -> ",out$to[i])
            if (kd %in% colnames(boot$draws)) draws <- draws + boot$draws[,kd]
          }
          out$boot_se[i] <- stats::sd(draws,na.rm=TRUE)
          ci <- .pls_quantile(draws,c(alpha,1-alpha));out$conf_low[i]<-ci[1];out$conf_high[i]<-ci[2]
          out$CI_method[i] <- "percentile-derived"
        }
      }
    }
  }
  out
}

#' Two-stage score-based moderation sensitivity analysis
#'
#' @param fit Native fit containing the involved constructs.
#' @param predictor Predictor construct.
#' @param moderator Moderator construct.
#' @param outcome Outcome construct.
#' @param boot Logical; bootstrap moderation coefficient.
#' @param R User-selected bootstrap replications.
#' @param seed Optional seed.
#' @param level Confidence level.
#' @return Moderation estimates and optional bootstrap interval.
#' @export
pls_moderation_test <- function(fit, predictor, moderator, outcome, boot = TRUE,
                                R = 1999L, seed = NULL, level = 0.95) {
  .pls_check_fit(fit); if(fit$engine!="native") .pls_abort("Native moderation sensitivity analysis requires engine='native'.")
  S <- fit$native$scores
  dat <- data.frame(y=S[[outcome]], x=S[[predictor]], z=S[[moderator]])
  dat$xz <- dat$x*dat$z
  lm0 <- stats::lm(y ~ x + z + xz, data=dat)
  est <- stats::coef(lm0)
  out <- data.frame(term=names(est), estimate=as.numeric(est), stringsAsFactors=FALSE)
  if (isTRUE(boot)) {
    if(!is.null(seed)) set.seed(seed); n<-nrow(dat); b<-matrix(NA_real_,R,4)
    for(i in seq_len(R)){ii<-sample.int(n,n,TRUE); b[i,]<-stats::coef(stats::lm(y~x+z+xz,data=dat[ii,]))}
    a<-(1-level)/2; ci<-t(apply(b,2,.pls_quantile,probs=c(a,1-a)))
    out$conf_low<-ci[,1]; out$conf_high<-ci[,2]
  }
  attr(out,"note") <- "Two-stage construct-score moderation; use a dedicated PLS interaction backend for primary inference when required."
  out
}

#' Score-based polynomial nonlinear sensitivity analysis
#' @param fit Native fit.
#' @param predictor Predictor construct.
#' @param outcome Outcome construct.
#' @param degree Polynomial degree.
#' @param boot Bootstrap coefficients.
#' @param R User-selected bootstrap replications.
#' @param seed Optional seed.
#' @return Coefficient table.
#' @export
pls_nonlinear_test <- function(fit, predictor, outcome, degree=2L, boot=TRUE, R=1999L, seed=NULL) {
  .pls_check_fit(fit); if(fit$engine!="native") .pls_abort("Native nonlinear sensitivity analysis requires engine='native'.")
  S<-fit$native$scores; dat<-data.frame(y=S[[outcome]],x=S[[predictor]])
  f<-stats::as.formula(paste0("y ~ poly(x, ",as.integer(degree),", raw=TRUE)")); lm0<-stats::lm(f,dat)
  est<-stats::coef(lm0); out<-data.frame(term=names(est),estimate=as.numeric(est))
  if(boot){if(!is.null(seed)) set.seed(seed); n<-nrow(dat); b<-matrix(NA_real_,R,length(est)); for(i in seq_len(R)){ii<-sample.int(n,n,TRUE); b[i,]<-stats::coef(stats::lm(f,dat[ii,]))}; ci<-t(apply(b,2,.pls_quantile,probs=c(.025,.975)));out$conf_low<-ci[,1];out$conf_high<-ci[,2]}
  attr(out,"note")<-"Score-based polynomial sensitivity analysis, not a replacement for a dedicated nonlinear PLS-SEM estimator."
  out
}

#' Native bootstrap multigroup comparison
#'
#' @param model PLS-SEM model.
#' @param data Data frame containing a group column.
#' @param group Grouping column.
#' @param R User-selected bootstrap replications per group.
#' @param seed Optional seed.
#' @return Group-specific paths and bootstrap differences.
#' @export
pls_mga <- function(model, data, group, R=1999L, seed=NULL) {
  if(!group %in% names(data)) .pls_abort("Unknown group column.")
  lev<-unique(data[[group]]); if(length(lev)!=2) .pls_abort("Native MGA currently compares exactly two groups; use cSEM for general MGD/MICOM workflows.")
  fits<-lapply(lev,function(g) pls_fit(model,data[data[[group]]==g,,drop=FALSE],engine="native")); names(fits)<-as.character(lev)
  boots<-lapply(seq_along(fits),function(i) pls_bootstrap(fits[[i]],R=R,seed=if(is.null(seed)) NULL else seed+i)); names(boots)<-names(fits)
  p1<-fits[[1]]$native$paths; p2<-fits[[2]]$native$paths; p1$key<-paste(p1$from,p1$to);p2$key<-paste(p2$from,p2$to)
  keys<-intersect(p1$key,p2$key); rows<-list()
  for(k in keys){e1<-p1$estimate[p1$key==k];e2<-p2$estimate[p2$key==k]; c1<-boots[[1]]$table; c2<-boots[[2]]$table; lab<-gsub(" "," -> ",k,fixed=TRUE); d1<-boots[[1]]$draws[,paste0("path::",lab)];d2<-boots[[2]]$draws[,paste0("path::",lab)]; d<-d1-d2; rows[[k]]<-data.frame(path=lab,group1=lev[1],group2=lev[2],estimate1=e1,estimate2=e2,difference=e1-e2,conf_low=.pls_quantile(d,.025),conf_high=.pls_quantile(d,.975),p_two_sided=2*min(mean(d<=0,na.rm=TRUE),mean(d>=0,na.rm=TRUE)))}
  structure(list(groups=lev,fits=fits,bootstraps=boots,comparison=do.call(rbind,rows)),class="plssem_mga")
}
