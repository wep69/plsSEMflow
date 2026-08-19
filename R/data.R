# Agronomic teaching data --------------------------------------------------

#' Load a frozen agronomic teaching dataset
#'
#' @param name Dataset key: soil_crop, precision_adoption, irrigation_stress,
#'   bioinput_adoption, multilevel_farms, or ordinal_soil_health.
#' @return A data frame. All bundled datasets are simulated teaching data and are not field evidence.
#' @export
pls_data <- function(name=c("soil_crop","precision_adoption","irrigation_stress","bioinput_adoption","multilevel_farms","ordinal_soil_health")) {
  name<-match.arg(name)
  f<-system.file("extdata",paste0(name,".csv"),package="plsSEMflow")
  if(!nzchar(f)){
    # Development-tree fallback
    f<-file.path("inst","extdata",paste0(name,".csv"))
  }
  if(!file.exists(f)) .pls_abort("Teaching dataset not found: ",name)
  utils::read.csv(f,check.names=FALSE)
}

#' Describe bundled agronomic teaching datasets
#' @export
pls_datasets <- function(){
  data.frame(name=c("soil_crop","precision_adoption","irrigation_stress","bioinput_adoption","multilevel_farms","ordinal_soil_health"),
             primary_use=c("reflective PLS-SEM, mediation, prediction","ordinal adoption model, MGA","moderation and nonlinear response","formative/composite and mediation","multilevel PLS-SEM","ordinal indicators and PLSc"),
             context=c("soil quality, plant nutrition, crop vigor and yield","precision-agriculture adoption by farmers","irrigation management, water stress and yield stability","bioinput knowledge, perceived efficacy, adoption and crop response","farms nested within municipalities","ordinal soil-health assessment and crop resilience"),stringsAsFactors=FALSE)
}
