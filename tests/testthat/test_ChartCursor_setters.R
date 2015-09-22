context("ChartCursor setters (class unions)")

testthat::test_that("setValueLineAxis method", {
  testthat::expect_error(setValueLineAxis(.Object = chartCursor()))
  testthat::expect_error(setValueLineAxis(.Object = chartCursor(), valueLineAxis = rep("a", 3)))
})