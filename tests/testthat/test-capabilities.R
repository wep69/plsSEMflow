test_that("capability registry includes native engine", {
  x<-pls_capabilities(); expect_true("native"%in%x$engine); expect_true(x$installed[x$engine=="native"])
})
