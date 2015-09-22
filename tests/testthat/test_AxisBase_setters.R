context("AxisBase setters (class unions)")

testthat::test_that("addGuide method", {
  testthat::expect_error(addGuide(.Object = valueAxis()))
})