testthat::context("amSolidGauge")

testthat::test_that("Simple example", {
  testthat::expect_silent({
    amSolidGauge(x = 65)
  })
})

testthat::test_that("Change min and max values", {
  testthat::expect_silent({
    amSolidGauge(x = 65, min = 0, max = 200)
  })
})

testthat::test_that("Semi gauge", {
  testthat::expect_silent({
    amSolidGauge(x = 70, min = 0, max = 100, type = "semi", width = 20, text = "%", 
                 color = c("#00ff00", "#ffd700", "#ff0000"), textSize = 25)
    amSolidGauge(x = 65, type = "semi")
  })
})

testthat::test_that("Change width", {
  testthat::expect_silent({
    amSolidGauge(x = 65, width = 50)
  })
})

testthat::test_that("Change color", {
  testthat::expect_silent({
    amSolidGauge(x = 65, color = "#2F4F4F")
  })
})

testthat::test_that("Put a color scale", {
  testthat::expect_silent({
    amSolidGauge(x = 10, color = c("#00ff00", "#ffd700", "#ff0000"))
    amSolidGauge(x = 35, color = c("#00ff00", "#ffd700", "#ff0000"))
    amSolidGauge(x = 70, color = c("#00ff00", "#ffd700", "#ff0000"))
    amSolidGauge(x = 90, color = c("#00ff00", "#ffd700", "#ff0000"))
  })
})

testthat::test_that("Add some text to the printed value", {
  testthat::expect_silent({
    amSolidGauge(x = 65, text = "%")
  })
})

testthat::test_that("Modify textSize value", {
  testthat::expect_silent({
    amSolidGauge(x = 65, text = "%", textSize = 50)
  })
})


testthat::test_that("amSolidGauge: Full example", {
  testthat::expect_silent({
    amSolidGauge(x = 75, min = 0, max = 100, type = "full", width = 20, 
                 color = "#1e90ff", text = "%", textSize = 50)
  })
})

