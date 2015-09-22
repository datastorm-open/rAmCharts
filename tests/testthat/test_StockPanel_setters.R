context("StockPanel setters (class unions)")

testthat::test_that("setDrawOnAxis", {
  testthat::expect_error(setDrawOnAxis(.Object = stockPanel()))
  testthat::expect_error(setDrawOnAxis(.Object = stockPanel(), valueAxis = 1))
  testthat::expect_error(setDrawOnAxis(.Object = stockPanel(), valueAxis = rep("a", 2)))
})

testthat::test_that("addStockGraph", {
  testthat::expect_error(addStockGraph(.Object = stockPanel()))
  testthat::expect_error(addStockGraph(.Object = stockPanel(), stockGraph = 1))
})


testthat::test_that("setStockLegend", {
  testthat::expect_error(setStockLegend(.Object = stockPanel()))
  testthat::expect_error(setStockLegend(.Object = stockPanel(), stockLegend = 1))
})
