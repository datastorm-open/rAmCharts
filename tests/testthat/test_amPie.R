testthat::context("amPie")

# Load data
data("data_pie")

testthat::test_that("Reference example", {
  testthat::expect_silent({
    amPie(data = data_pie)
  })
})

testthat::test_that("Hide values", {
  testthat::expect_silent({
    amPie(data = data_pie, show_values = FALSE)
  })
})

testthat::test_that("3D pie", {
  testthat::expect_silent({
    amPie(data = data_pie, depth = 10)
  })
})

testthat::test_that("Donut", {
  testthat::expect_silent({
    amPie(data = data_pie, inner_radius = 50)
  })
})

testthat::test_that("Other parameters", {
  testthat::expect_silent({
    amPie(data = data_pie, inner_radius = 50, depth = 10, show_values = FALSE)
  })
})
