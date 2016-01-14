context("amTests functions")

testthat::test_that(".testNumeric", {
  # type
  testthat::expect_error(.testNumeric("a"))
  testthat::expect_error(.testNumeric(TRUE))
  testthat::expect_true(.testNumeric(5L))
  testthat::expect_true(.testNumeric(integer(1)))
  testthat::expect_true(.testNumeric(1))
  testthat::expect_true(.testNumeric(1:10))
  # message
  testthat::expect_error(.testNumeric(num = "a", arg = "toto"), regexp = "*.toto*.")
})

testthat::test_that(".testLogical", {
  # type
  testthat::expect_true(.testLogical(TRUE))
  var <- c(TRUE, FALSE, TRUE)
  testthat::expect_true(.testLogical(var))
  testthat::expect_error(.testLogical(5L))
  testthat::expect_error(.testLogical(integer(1)))
  testthat::expect_error(.testLogical(1))
  testthat::expect_error(.testLogical(1:10))
  # message
  testthat::expect_error(.testLogical("bad foo", arg = "toto"), regexp = "*.toto*.")
})