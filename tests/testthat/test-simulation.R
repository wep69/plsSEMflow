test_that("agronomic simulation recovers positive structural directions", {
  model <- pls_model(
    pls_measurement(
      pls_reflective("SOIL", c("s1","s2","s3")),
      pls_reflective("NUTR", c("n1","n2","n3")),
      pls_reflective("YIELD", c("y1","y2","y3"))
    ),
    pls_structural(
      pls_path("SOIL","NUTR"),
      pls_path("NUTR","YIELD")
    )
  )
  truth <- c("SOIL -> NUTR"=0.55, "NUTR -> YIELD"=0.45)
  d <- pls_simulate(model, n=500, path_values=truth, seed=260819)
  f <- pls_fit(model,d,engine="native")
  expect_true(all(f$native$paths$estimate > 0))
  expect_equal(nrow(d),500)
})

test_that("validation case reports user-selected bootstrap count", {
  model <- pls_model(
    pls_measurement(
      pls_reflective("A", c("a1","a2","a3")),
      pls_reflective("B", c("b1","b2","b3"))
    ),
    pls_structural(pls_path("A","B"))
  )
  v <- pls_validation_case(model,n=160,path_values=c("A -> B"=.45),R=25,seed=11)
  expect_equal(v$design$R,25)
  expect_true(all(c("truth","estimate","bias","covered") %in% names(v$table)))
})
