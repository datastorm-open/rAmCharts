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


testthat::test_that(".testCharacter", {
  # type
  testthat::expect_true(.testCharacter("TRUE"))
  testthat::expect_error(.testCharacter(5L))
  testthat::expect_error(.testCharacter(integer(1)))
  testthat::expect_error(.testCharacter(1))
  # with vector
  var <- c("TRUE", "FALSE", "TRUE")
  testthat::expect_true(.testCharacter(var))
  testthat::expect_error(.testCharacter(1:10))
  # message
  testthat::expect_error(.testCharacter(TRUE, arg = "toto"), regexp = "*.toto*.")
})

testthat::test_that(".testInterval", {
  # type
  testthat::expect_true(.testInterval(num = 10, binf = 9, bsup = 10))
  testthat::expect_true(.testInterval(num = 10, binf = 10, bsup = 10))
  testthat::expect_error(.testInterval(num = "10", binf = 10, bsup = 10))
  testthat::expect_error(.testInterval(num = TRUE, binf = 10, bsup = 10))
  #with vector
  var <- runif(10)
  testthat::expect_true(.testInterval(num = var, binf = 0, bsup = 1))
  testthat::expect_error(.testInterval(num = var, binf = -1, bsup = 0))
  # message
  testthat::expect_error(.testInterval(num = TRUE, binf = 10, bsup = 10, arg = "toto"), regexp = "*.toto*.")
})


testthat::test_that(".testLength", {
  # length
  vectNum <- 10:13
  vectLogical <- rep(TRUE, 3)
  testthat::expect_true(.testLength(param = 10, len = 1))
  testthat::expect_true(.testLength(param = TRUE, len = 1))
  testthat::expect_error(.testLength(param = "e", len = 2))
  testthat::expect_true(.testLength(param = vectNum, len = 4))
  testthat::expect_true(.testLength(param = vectLogical, len = 3))
  testthat::expect_error(.testLength(param = vectNum, len = 1:3))
  
  # message
  testthat::expect_error(.testLength(param = "e", len = 2, arg = "toto"), regexp = "*.toto*.")
})

testthat::test_that(".testIn", {
  # numeric
  vectNum <- 10:13
  controlNum1 <- 1:20
  controlNum2 <- 20
  testthat::expect_true(.testIn(vect = vectNum, control = controlNum1))
  testthat::expect_error(.testIn(vect = vectNum, control = controlNum2))
  
  # numeric
  vectChar <- c("a", "b", "c")
  controlChar1 <- c("a", "b", "c", "d")
  controlChar2 <- c("a", "b")
  testthat::expect_true(.testIn(vect = vectChar, control = controlChar1))
  testthat::expect_error(.testIn(vect = vectChar, control = controlChar2))
})