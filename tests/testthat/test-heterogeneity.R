test_that("specialist external heterogeneity workflow is explicit", {
  d <- pls_data("soil_crop")
  m <- pls_model(
    pls_measurement(
      pls_reflective("SOIL",c("soil_om","soil_cec","soil_whc")),
      pls_reflective("NUTR",c("leaf_n","leaf_p","leaf_k"))
    ),
    pls_structural(pls_path("SOIL","NUTR"))
  )
  f <- pls_fit(m,d,engine="native")
  x <- pls_heterogeneity(f,"FIMIX")
  expect_s3_class(x,"plssem_external_workflow")
  expect_false(x$executable)
})
