context("DataSet setters (class unions)")

testthat::test_that("addStockEvent", {
  testthat::expect_error(addStockEvent(.Object = dataSet()))
  testthat::expect_error(addStockEvent(.Object = dataSet(), stockEvent = 1))
})