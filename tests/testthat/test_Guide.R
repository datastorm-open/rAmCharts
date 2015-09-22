context("Guide (class union)")

testthat::test_that("setValueAxis", {
  testthat::expect_error(setValueAxis(.Object = guide()))
  testthat::expect_error(setValueAxis(.Object = guide(), valueAxis = 1))
  testthat::expect_error(setValueAxis(.Object = guide(), valueAxis = rep("a", 5)))
})