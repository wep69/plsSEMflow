# Teaching tours and reports ----------------------------------------------

#' Guided PLS-SEM teaching tour
#'
#' @param topic Topic name.
#' @return Ordered teaching steps and suggested functions.
#' @export
pls_tour <- function(topic=c("foundations","measurement","structural","bootstrap","mediation","moderation","prediction","multigroup","nonlinear","multilevel","bayesian","reviewer")){
  topic<-match.arg(topic)
  map<-list(
    foundations=c("Define the agronomic question and unit of observation","Classify constructs as common factors or composites","Specify measurement blocks","Specify directional structural relations","Fit a simple model","Audit assumptions and design"),
    measurement=c("Inspect loadings/weights","Assess reliability and AVE where appropriate","Assess formative collinearity","Assess discriminant validity","Use bootstrap uncertainty before indicator decisions"),
    structural=c("Inspect predictor collinearity","Estimate paths with uncertainty","Assess R2 and f2","Interpret mediation/moderation conditionally","Check predictive performance"),
    bootstrap=c("Choose R explicitly","Set a fixed seed for reproducibility","Inspect bootstrap convergence","Report CI method and level","Show bootstrap CIs in tables and figures"),
    mediation=c("State causal ordering from domain knowledge","Estimate component paths","Bootstrap indirect effect","Distinguish direct, indirect and total effects","Avoid causal language unsupported by design"),
    moderation=c("State the agronomic moderator","Prefer dedicated interaction estimators for primary inference","Plot conditional effects","Bootstrap interaction coefficients","Check scale dependence"),
    prediction=c("Define prediction target","Use repeated k-fold CV","Compare against a benchmark","Inspect RMSE and MAE","Separate explanation from prediction"),
    multigroup=c("Define groups a priori","Assess measurement invariance before path comparison","Use permutation/MGA methods","Report group-specific CIs","Interpret practical differences"),
    nonlinear=c("Plot observed patterns","State functional rationale","Use cSEM/plssem nonlinear engines when primary","Use score-polynomial analysis only as sensitivity","Plot uncertainty across predictor range"),
    multilevel=c("Identify cluster level","Separate within/between processes","Use plssem multilevel estimator","Bootstrap at appropriate unit","Report random structure and cluster counts"),
    bayesian=c("Do not relabel CB-SEM as Bayesian PLS-SEM","Translate only compatible reflective models","Specify priors","Check convergence and posterior prediction","Treat as companion/sensitivity analysis"),
    reviewer=c("Check construct conceptualization","Check indicator deletion logic","Check bootstrap settings","Check discriminant validity","Check predictive assessment","Check MICOM before MGA","Check endogeneity rationale","Check reproducibility"))
  data.frame(step=seq_along(map[[topic]]),instruction=map[[topic]])
}

#' Write a reproducible Markdown report skeleton
#'
#' @param fit Fitted model.
#' @param file Output Markdown filename.
#' @param title Report title.
#' @param boot Optional bootstrap object.
#' @return Output path invisibly.
#' @export
pls_report <- function(fit,file="plssemflow_report.md",title="PLS-SEM analysis",boot=NULL){
  .pls_check_fit(fit); if(is.null(boot))boot<-fit$bootstrap
  lines<-c(paste0("# ",title),"","## Scientific question","Describe the agronomic mechanism and why each path was specified.","","## Data and unit of observation",paste0("Observations: ",nrow(fit$data)),"","## Model specification","```",pls_syntax(fit$model),"```","","## Estimation",paste0("Engine: `",fit$engine,"`; estimator: `",fit$estimator,"`."),"","## Measurement model","Report construct conceptualization, loadings/weights, reliability/AVE where appropriate, formative VIF and discriminant validity.","","## Structural model","Report path estimates, R2, f2 and collinearity.","","## Bootstrap uncertainty",if(is.null(boot))"Bootstrap not attached. Choose and justify R before final inferential reporting." else paste0("Bootstrap R = ",boot$R,", method = ",boot$interval,", level = ",boot$level,"."),"","## Prediction","Report repeated cross-validation and benchmark comparison when prediction is a scientific goal.","","## Diagnostics and sensitivity","Document missing-data handling, influential observations, multigroup invariance, nonlinear sensitivity, endogeneity checks or alternative engines as relevant.","","## Figures and tables","Prefer observed information plus estimates and uncertainty; state CI method in captions.","","## Reproducibility",paste0("Generated: ",Sys.time()),paste0("R version: ",R.version.string))
  writeLines(lines,file);invisible(normalizePath(file,mustWork=FALSE))
}
