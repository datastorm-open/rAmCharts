context("AmOptions")


.testAmOptions

testthat::test_that(".testAmOptions", {
  data(data_radar)
  testthat::expect_true(.testAmOptions(amRadar(data_radar)))

  
})