test_that("bootstrap paths table records R and CI metadata", {
  d <- pls_data("soil_crop")
  m <- pls_model(
    pls_measurement(
      pls_reflective("SOIL",c("soil_om","soil_cec","soil_whc")),
      pls_reflective("NUTR",c("leaf_n","leaf_p","leaf_k"))
    ),
    pls_structural(pls_path("SOIL","NUTR"))
  )
  f <- pls_fit(m,d,engine="native")
  tab <- pls_table(f,"paths",boot=TRUE,R=25,seed=9)
  expect_true(all(tab$R_boot==25))
  expect_true(all(c("conf_low","conf_high","CI_method") %in% names(tab)))
})

test_that("coefficient plotting supports bootstrap intervals", {
  skip_if_not_installed("ggplot2")
  x <- data.frame(term=c("management","interaction"),estimate=c(.3,.2),conf_low=c(.1,.02),conf_high=c(.5,.38))
  p <- pls_plot_coefficients(x)
  expect_s3_class(p,"ggplot")
})
