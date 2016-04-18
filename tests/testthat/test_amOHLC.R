testthat::context("amOHLC")
require(pipeR)
data("data_candleStick2")

testthat::test_that("Basic example", {
  testthat::expect_silent({
    amOHLC(data = data_candleStick2)
  })
})  

testthat::test_that("Change colors", {
  testthat::expect_silent({
    amOHLC(data = data_candleStick2, positiveColor = "green", negativeColor = "red")
  })
})    

testthat::test_that("Naming the axes", {
  testthat::expect_silent({
    amOHLC(data = data_candleStick2, xlab = "categories", ylab = "values") %>>% setChartCursor()
  })
})    

testthat::test_that("Rotate the labels for x axis", {
  testthat::expect_silent({
    amOHLC(data = data_candleStick2, labelRotation = 90)
  })
})    

testthat::test_that("Change names", {
  testthat::expect_silent({
    amOHLC(data = data_candleStick2, names = c("min", "begin", "end", "max")) %>>% setChartCursor()
  })
})    

testthat::test_that("Use amOptions", {
  testthat::expect_silent({
    amOHLC(data = data_candleStick2, zoom = FALSE)
  })
})