context("AmStockChart setters (class unions)")

testthat::test_that("setChartScrollbarSettings", {
  testthat::expect_error(setChartScrollbarSettings(.Object = amStockChart()))
  testthat::expect_error(setChartScrollbarSettings(.Object = amStockChart(),
                                                   chartScrollbarSettings = "another class"))
})

testthat::test_that("addComparedDataSet", {
  testthat::expect_error(addComparedDataSet(.Object = amStockChart()))
  testthat::expect_error(addComparedDataSet(.Object = amStockChart(), dataSet = "another class"))
})

testthat::test_that("addDataSet", {
  testthat::expect_error(addDataSet(.Object = amStockChart()))
  testthat::expect_error(addDataSet(.Object = amStockChart(), dataSet = "another class"))
})

testthat::test_that("setMainDataSet", {
  testthat::expect_error(setMainDataSet(.Object = amStockChart()))
  testthat::expect_error(setMainDataSet(.Object = amStockChart(), dataSet = "another class"))
})

testthat::test_that("addPanel", {
  testthat::expect_error(addPanel(.Object = amStockChart()))
  testthat::expect_error(addPanel(.Object = amStockChart(), panel = "another class"))
})

testthat::test_that("setPeriodSelector", {
  testthat::expect_error(setPeriodSelector(.Object = amStockChart()))
  testthat::expect_error(setPeriodSelector(.Object = amStockChart(),
                                           periodSelector = "another class"))
})