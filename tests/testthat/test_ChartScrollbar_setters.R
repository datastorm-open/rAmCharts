context("ChartScrollbar setters (class unions)")

testthat::test_that("setGraph method", {
  testthat::expect_message(setGraph(.Object = chartScrollbar()))
  testthat::expect_error(setGraph(.Object = chartScrollbar(), graph = rep("a", 3)))
})