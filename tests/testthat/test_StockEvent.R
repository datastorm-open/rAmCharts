context("StockEvent setters (class unions)")

testthat::test_that("setStockGraph", {
  testthat::expect_error(setStockGraph(.Object = stockEvent()))
  testthat::expect_error(setStockGraph(.Object = stockEvent(), stockGraph = stockGraph_obj))
})