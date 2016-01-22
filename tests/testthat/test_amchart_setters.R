context("AmChart setters (class unions)")

testthat::test_that("addLabel method", {
  testthat::expect_error(addLabel(.Object = amSerialChart()))
  testthat::expect_error(addLabel(.Object = amChart(), label = "another class"))
})

testthat::test_that("addArrow method", {
  testthat::expect_error(addArrow(.Object = amAngularGaugeChart()))
  testthat::expect_error(addArrow(.Object = amAngularGaugeChart(), arrow = "error"))
})

testthat::test_that("addAxe method", {
  testthat::expect_error(addAxe(.Object = amAngularGaugeChart()))
  testthat::expect_error(addAxe(.Object = amAngularGaugeChart(), axe = "error"))
})

testthat::test_that("setBalloon method", {
  testthat::expect_error(setBalloon(.Object = amSerialChart()))
  testthat::expect_error(setBalloon(.Object = amSerialChart(), amBalloon = "error"))
})

testthat::test_that("setCategoryAxis method", {
  testthat::expect_error(setCategoryAxis(.Object = amSerialChart(), categoryAxis = "error"))
})

testthat::test_that("setChartCursor method", {
  testthat::expect_error(setChartCursor(.Object = amSerialChart(), chartCursor = "error"))
  testthat::expect_silent(setChartCursor(.Object = amAngularGaugeChart(), chartCursor = chartCursor()))
})

testthat::test_that("setChartScrollbar method", {
  testthat::expect_error(setChartScrollbar(.Object = amSerialChart(), chartScrollbar = "error"))
})

testthat::test_that("setDataProvider method", {
  dataProvider_obj <- c("FR", "US", "GER", "ENG", "IT" )
  testthat::expect_error(setDataProvider(.Object = amPieChart(), dataProvider = dataProvider_obj))
})

testthat::test_that("setDataProvider method", {
  dataProvider_obj <- data.frame(key = c("FR", "US", "GER", "ENG", "IT" ),
                                 value = round(runif(5, max = 100)))
  testthat::expect_error(setDataProvider(.Object = amPieChart(),
                                         dataProvider = dataProvider_obj,
                                         keepNA = "yes"))
})

testthat::test_that("setGraphs method", {
  graphs_ls <- list(list(balloonText = "balloonText"), list(type = "column"))
  testthat::expect_error(setGraphs(.Object = amChart(), graphs = graphs_ls))
})

testthat::test_that("addGraph method", {
  testthat::expect_error(addGraph(.Object = amSerialChart(), amGraph = "error"))
})

testthat::test_that("setGraph method", {
  testthat::expect_error(setGraph(.Object = amGanttChart(), graph = "error"))
})

testthat::test_that("addGuide method", {
  testthat::expect_error(addGuide(.Object = amSerialChart()))
})

testthat::test_that("setLegend method", {
  testthat::expect_error(setLegend(.Object = amSerialChart()))
})

testthat::test_that("setTitles method", {
  titles_ls <- list(title(text = "balloonText"), text = "column")
  testthat::expect_error(amPieChart(titles = titles_ls))
  testthat::expect_error(setTitles(.Object = amPieChart, titles = titles_ls))
})

testthat::test_that("addTitle method", {
  testthat::expect_error(addTitle(.Object = amPieChart()))
  testthat::expect_error(addTitle(.Object = amPieChart(), title = "error"))
})

testthat::test_that("addTrendLine method", {
  testthat::expect_error(addTrendLine(.Object = amSerialChart()))
  testthat::expect_error(addTrendLine(.Object = amSerialChart(), trendLine = "error"))
})

testthat::test_that("setValueAxes method", {
  valueAxes <- list(wrong = 1, element = 2)
  testthat::expect_error(setValueAxes(.Object = amSerialChart(), valueAxes = valueAxes))
})

testthat::test_that("addLabel method", {
  testthat::expect_error(addValueAxes(.Object = amSerialChart()))
  testthat::expect_error(addValueAxes(.Object = amSerialChart(), valueAxis = "error"))
})