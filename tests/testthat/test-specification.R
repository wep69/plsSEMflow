test_that("model DSL builds a coherent model", {
  m <- pls_model(
    pls_measurement(pls_reflective("A", c("a1","a2")), pls_reflective("B", c("b1","b2"))),
    pls_structural(pls_path("A","B"))
  )
  expect_s3_class(m, "plssem_model")
  expect_match(pls_syntax(m), "B ~ A")
  expect_match(pls_syntax(m), "A =~ a1 \\+ a2")
})
