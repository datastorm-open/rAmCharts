context("GaugeArrow setters (class unions)")

testthat::test_that("setBands", {
  testthat::expect_error(setAxis(.Object = gaugeArrow()))
  testthat::expect_error(setAxis(.Object = gaugeArrow(), axis = 1))
})