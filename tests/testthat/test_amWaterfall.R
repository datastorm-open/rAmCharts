testthat::context("amWaterfall")

# Load data
data("data_waterfall")

testthat::test_that("amWaterfall : reference example", {
  testthat::expect_silent({
    amWaterfall(data = data_waterfall, show_values = TRUE)
  })
})

testthat::test_that("amWaterfall : change orientation", {
  testthat::expect_silent({
    amWaterfall(data = data_waterfall, horiz = TRUE)
  })
})

    