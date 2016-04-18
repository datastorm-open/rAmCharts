testthat::context("amMekko")
library(pipeR)
data(data_funnel)

testthat::test_that("Pyramid", {
  testthat::expect_silent({
    amFunnel(data = data_funnel, inverse = TRUE)
  })
})

testthat::test_that("Orientation and legend", {
  testthat::expect_silent({
    amFunnel(data = data_funnel, inverse = FALSE, label_side = "left",
             margin_right = 15, margin_left = 160)
  })
})

testthat::test_that("Funnel", {
  testthat::expect_silent({
    amFunnel(data = data_funnel, neck_height = 30, neck_width = 40)
  })
})

testthat::test_that("3D pyramid", {
  testthat::expect_silent({
    amFunnel(data = data_funnel, depth = 50, inverse = TRUE)
  })
})


