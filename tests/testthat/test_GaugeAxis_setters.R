context("GaugeAxis setters (class unions)")

testthat::test_that("setBands", {
  bands <- list(startValue = 0, endValue = 100)
  testthat::expect_error(gaugeAxis(bands = bands))
  testthat::expect_error(setBands(.Object = gaugeAxis(), bands = bands))
})

testthat::test_that("addBand", {
  testthat::expect_error(addBand(.Object = gaugeAxis()))
  testthat::expect_error(addBand(.Object = gaugeAxis(), band = "test"))
  band_obj <- list(startValue = 0, endValue = 100)
  testthat::expect_error(addBand(.Object = gaugeAxis(), band = c(band_obj, band_obj)))
})