testthat::context("amRadar")
require(pipeR)
data("data_candleStick2")


testthat::test_that("Basic example", {
  testthat::expect_silent({
    amCandlestick(data = data_candleStick2)
  })
})  

testthat::test_that("Change colors", {
  testthat::expect_silent({
    amCandlestick(data = data_candleStick2, positiveColor = "black", negativeColor = "green")
  })
})

testthat::test_that("Naming the axes", {
  testthat::expect_silent({
    amCandlestick(data = data_candleStick2, xlab = "categories", ylab = "values")
  })
})

testthat::test_that("Rotate the labels for x axis", {
  testthat::expect_silent({
    amCandlestick(data = data_candleStick2, labelRotation = 90)
  })
})

testthat::test_that("Change names", {
  testthat::expect_silent({
    amCandlestick(data = data_candleStick2, names = c("min", "begin", "end", "max"))
  })
})

testthat::test_that("Horizontal chart:", {
  testthat::expect_silent({
    amCandlestick(data = data_candleStick2, horiz = TRUE)
  })
})

testthat::test_that("Parse date ", {
  testthat::expect_silent({
    amCandlestick(data = data_candleStick2, dataDateFormat = "YYYY-MM-DD")
  })
})

testthat::test_that("Datas over months", {
  testthat::expect_silent({
    data_candleStick2$category <- c("2015-01-01", "2015-02-01", "2015-03-01",
                                    "2015-04-01", "2015-05-01", "2015-06-01",
                                    "2015-07-01", "2015-08-01", "2015-09-01",
                                    "2015-10-01", "2015-11-01", "2015-12-01")
    
    amCandlestick(data = data_candleStick2, dataDateFormat = "YYYY-MM-DD", minPeriod = "MM")
  })
})

testthat::test_that("Decimal precision", {
  testthat::expect_silent({
    amCandlestick(data = data_candleStick2, horiz = TRUE) %>>%
      setProperties(precision = 2)
  })
})