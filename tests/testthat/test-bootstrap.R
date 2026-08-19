test_that("bootstrap returns user-controlled coefficient intervals", {
  set.seed(2); n<-120; a<-rnorm(n); b<-.5*a+rnorm(n)
  d<-data.frame(a1=a+rnorm(n,0,.2),a2=a+rnorm(n,0,.2),b1=b+rnorm(n,0,.2),b2=b+rnorm(n,0,.2))
  m<-pls_model(pls_measurement(pls_reflective("A",c("a1","a2")),pls_reflective("B",c("b1","b2"))),pls_structural(pls_path("A","B")))
  f<-pls_fit(m,d,engine="native")
  bb<-suppressWarnings(pls_bootstrap(f,R=30,seed=10))
  expect_equal(bb$R,30)
  expect_true(all(c("conf_low","conf_high")%in%names(bb$table)))
})

test_that("mediation effects can carry bootstrap intervals", {
  d <- pls_data("soil_crop")
  m <- pls_model(
    pls_measurement(
      pls_reflective("SOIL",c("soil_om","soil_cec","soil_whc")),
      pls_reflective("NUTR",c("leaf_n","leaf_p","leaf_k")),
      pls_reflective("YIELD",c("grain_yield","biomass","harvest_index"))
    ),
    pls_structural(pls_mediation("SOIL","NUTR","YIELD",direct=TRUE))
  )
  f <- pls_fit(m,d,engine="native")
  b <- pls_bootstrap(f,R=25,seed=7)
  e <- pls_effects(f,b)
  expect_true(any(grepl("indirect",e$effect)))
  expect_true(all(c("conf_low","conf_high","R_boot") %in% names(e)))
  expect_true(all(e$R_boot==25))
})
