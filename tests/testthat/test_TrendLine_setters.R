context("TrendLine setters (class unions)")

testthat::test_that("setValueAxis", {
  testthat::expect_error(setValueAxis(.Object = trendLine()))
  testthat::expect_error(setValueAxis(.Object = trendLine(), valueAxis = 1))
  testthat::expect_error(setValueAxis(.Object = trendLine(), valueAxis = rep("a", 2)))
})

testthat::test_that("setValueAxisX", {
  testthat::expect_error(setValueAxisX(.Object = trendLine()))
  testthat::expect_error(setValueAxisX(.Object = trendLine(), valueAxisX = 1))
  testthat::expect_error(setValueAxisX(.Object = trendLine(), valueAxisX = rep("a", 2)))
})
