testthat::context("amRadar")
data("data_radar")

testthat::test_that("Basic example", {
  testthat::expect_silent({
    amRadar(data = data_radar)
  })
})

testthat::test_that("Change color", {
  testthat::expect_silent({
    amRadar(data_radar, col = "#FF0000")
    amRadar(data_radar, col = c("#0000FF", "#00FF00", "#FF0000"))
  })
})

testthat::test_that("Change backTransparency", {
  testthat::expect_silent({
    amRadar(data_radar, backTransparency = 0.6)
    amRadar(data_radar, backTransparency = c(0, 0.4, 0.6))
  })
})

testthat::test_that("Change type", {
  testthat::expect_silent({
    amRadar(data_radar, type = "circles")
  })
})

testthat::test_that("Change pch", {
  testthat::expect_silent({
    amRadar(data_radar,  pch = "triangleRight")
    amRadar(data_radar,  pch = "triangleLeft")
  })
})


