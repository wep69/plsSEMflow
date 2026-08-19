test_that("dataset registry is agronomic", {
  x<-pls_datasets()
  expect_gte(nrow(x),6)
  expect_true(all(grepl("soil|crop|agri|farm|irrig|bioinput|yield|health",paste(x$context,x$primary_use),ignore.case=TRUE)))
})
