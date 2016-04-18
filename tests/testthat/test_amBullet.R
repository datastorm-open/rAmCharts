testthat::context("amBullet")


testthat::test_that("Basic example", {
  testthat::expect_silent({
    amBullet(value = 65)
  })
})

testthat::test_that("Remove steps for background", {
  testthat::expect_silent({
    amBullet(value = 65, steps = FALSE)
  })
})

testthat::test_that("Tune the colors with name or HTML code", {
  testthat::expect_silent({
    amBullet(value = 65, val_color = "purple", limit_color = "#3c8dbc")
  })
})

testthat::test_that("Change the orientation", {
  testthat::expect_silent({
    amBullet(value = 65, steps = FALSE, horiz = FALSE)
  })
})

testthat::test_that("Add text", {
  testthat::expect_silent({
    amBullet(value = 65, label = "Evaluation")
  })
})

testthat::test_that("Change min and max values", {
  testthat::expect_silent({
    amBullet(value = 65, min = 20, max = 90)
  })
})
