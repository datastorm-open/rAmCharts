testthat::context("amWind")
# Load data
data("data_wind")

testthat::test_that("amWind : reference example", {
  testthat::expect_silent({
    amWind(data_wind)
  })
})

testthat::test_that("amWind : Change color", {
  testthat::expect_silent({
    amWind(data = data_wind, col = "#0404B4")
    amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"))
  })
})

testthat::test_that("amWind : Change backTransparency", {
  testthat::expect_silent({
    amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = 0.1)
    amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = 1)
    amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = c(0.1, 0.1, 1))
  })
})