testthat::context("amMekko")
library(pipeR)
data(data_mekko)

testthat::test_that("Simple example", {
  testthat::expect_silent({
    amMekko(x = "var1", y = "var2", data = data_mekko)
  })
})

testthat::test_that("Simple example", {
  testthat::expect_silent({
    amMekko(x = "var1", y = "var2", data = data_mekko, horiz = TRUE)
  })
})

testthat::test_that("Simple example", {
  testthat::expect_silent({
    amMekko(x = "var1", y = "var2", data = data_mekko, show_values = TRUE)
  })
})
