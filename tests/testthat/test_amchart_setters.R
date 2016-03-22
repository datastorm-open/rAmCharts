context("AmChart setters (class unions)")

testthat::test_that("setAllLabels method", {
  allLabels_v1 <- list(label(text = "balloonText"), label(text = "column"))
  testthat::expect_silent(setAllLabels(.Object = amChart(), allLabels = allLabels_v1))
  
  allLabels_v2 <- list(label(text = "balloonText"), "column")
  testthat::expect_error(setAllLabels(.Object = amChart(), allLabels = allLabels_v2))
})

testthat::test_that("addLabel method", {
  testthat::expect_silent(addLabel(.Object = amChart(), text = "balloonText"))

  testthat::expect_error(addLabel(.Object = amChart()))
  testthat::expect_error(addLabel(.Object = amChart(), label = "another class"))
})

testthat::test_that("setArrows", {
  arrows_ls <- list(gaugeArrow(value = 130), gaugeArrow(value = 150))
  testthat::expect_silent(amAngularGaugeChart(arrows = arrows_ls))
  
  arrows_ls <- list(gaugeArrow(value = 130), "gaugeArrow(value = 150)")
  testthat::expect_error(amAngularGaugeChart(arrows = arrows_ls))
})


testthat::test_that("addArrow method", {
  testthat::expect_silent(addArrow(.Object = amAngularGaugeChart(), value = 10))
  gaugeArrow_obj <- gaugeArrow(value = 10)
  testthat::expect_silent(addArrow(.Object = amAngularGaugeChart(), arrow = gaugeArrow_obj))
  
  testthat::expect_error(addArrow(.Object = amAngularGaugeChart()))
  testthat::expect_error(addArrow(.Object = amAngularGaugeChart(), arrow = "error"))
})


testthat::test_that("setAxes", {
  axes_ls <- list(gaugeAxis(value = 130), gaugeAxis(value = 150))
  testthat::expect_silent(setAxes(.Object = amAngularGaugeChart(), axes = axes_ls))
  testthat::expect_silent(amChart(axes = axes_ls))
})



testthat::test_that("addAxis method", {
  testthat::expect_silent(addAxis(.Object = amAngularGaugeChart(), startValue = 0, endValue = 100, valueInterval = 10))
  gaugeAxis_obj <- gaugeAxis(startValue = 0, enValue = 100, valueInterval = 10)
  testthat::expect_silent(addAxis(.Object = amAngularGaugeChart(), axis = gaugeAxis_obj))

  testthat::expect_error(addAxis(.Object = amAngularGaugeChart()))
  testthat::expect_error(addAxis(.Object = amAngularGaugeChart(), axis = "error"))
})




testthat::test_that("setBalloon method", {
  testthat::expect_silent(setBalloon(.Object = amSerialChart(), adjustBorderColor = TRUE,
                                     fillColor = "#FFFFFF", color = "#000000", cornerRadius = 5))
  amBalloon_obj <- amBalloon(adjustBorderColor = TRUE, fillColor = "#FFFFFF",
                             color = "#000000", cornerRadius = 5)
  testthat::expect_silent(setBalloon(.Object = amSerialChart(), amBalloon = amBalloon_obj))
  
  
  testthat::expect_error(setBalloon(.Object = amSerialChart()))
  testthat::expect_error(setBalloon(.Object = amSerialChart(), amBalloon = "error"))
})

testthat::test_that("setCategoryAxis method", {
  testthat::expect_silent(setCategoryAxis(.Object = amSerialChart(), gridPosition = "start"))
  categoryAxis_obj <- categoryAxis(gridPosition = "start")
  testthat::expect_silent(setCategoryAxis(.Object = amSerialChart(), categoryAxis = categoryAxis_obj))
  
  testthat::expect_error(setCategoryAxis(.Object = amSerialChart(), categoryAxis = "error"))
})

testthat::test_that("setChartCursor method", {
  testthat::expect_silent(setChartCursor(.Object = amSerialChart()))
  testthat::expect_silent(setChartCursor(.Object = amSerialChart(), oneBallOnly = TRUE))
  chartCursor_obj <- chartCursor(oneBallOnly = TRUE)
  testthat::expect_silent(setChartCursor(.Object = amSerialChart(), chartCursor = chartCursor_obj))
  
  testthat::expect_error(setChartCursor(.Object = amSerialChart(), chartCursor = "error"))
  testthat::expect_silent(setChartCursor(.Object = amSerialChart(), chartCursor = chartCursor()))
})

testthat::test_that("setChartScrollbar method", {
  testthat::expect_silent(setChartScrollbar(.Object = amSerialChart()))
  chartScrollbar_obj <- chartScrollbar(updateOnReleaseOnly = FALSE)
  testthat::expect_silent(setChartScrollbar(.Object = amSerialChart(), chartScrollbar = chartScrollbar_obj))
  
  testthat::expect_error(setChartScrollbar(.Object = amSerialChart(), chartScrollbar = "error"))
})

testthat::test_that("setDataProvider method with NA", {
  dataProvider_obj <- data.frame(key = c("FR", "US", "GER", "ENG", "IT" ),
                                 value = round(runif(5, max = 100)))
  testthat::expect_silent(setDataProvider(.Object = amPieChart(), dataProvider = dataProvider_obj))
  
  dataProvider_obj <- c("FR", "US", "GER", "ENG", "IT" )
  testthat::expect_error(setDataProvider(.Object = amPieChart(), dataProvider = dataProvider_obj))
})


testthat::test_that("setGraphs method", {
  graphs_ls <- list(graph(balloonText = "balloonText"), graph(type = "column"))
  testthat::expect_silent(setGraphs(.Object = amSerialChart(), graphs = graphs_ls))
  
  graphs_ls <- list(list(balloonText = "balloonText"), list(type = "column"))
  testthat::expect_error(setGraphs(.Object = amChart(), graphs = graphs_ls))
})


testthat::test_that("addGraph method", {
  addGraph(.Object = amSerialChart(), balloonText = "balloonText", "type" = "column")
  amGraph_obj <- amGraph(balloonText = "balloonText", "type" = "column")
  testthat::expect_silent(addGraph(.Object = amSerialChart(), amGraph = amGraph_obj))
  
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

testthat::test_that("setValueAxis", {
  testthat::expect_silent(setValueAxis(.Object = amGanttChart()))
  testthat::expect_silent(setValueAxis(.Object = amGanttChart(), type = "date"))
  
  testthat::expect_error(setValueAxis(.Object = amSerialChart(), valueAxis = list(wrong = 1, element = 2)))
})

testthat::test_that("addValueAxis", {
  testthat::expect_silent(addValueAxis(.Object = amSerialChart(), axisTitleOffset = 12, tickLength = 10))
  valueAxis_obj <- valueAxis(axisTitleOffset = 12, tickLength = 10)
  testthat::expect_silent(addValueAxis(.Object = amSerialChart(), valueAxis = valueAxis_obj))
  
  testthat::expect_error(addValueAxes(.Object = amSerialChart()))
  testthat::expect_error(addValueAxes(.Object = amSerialChart(), valueAxis = "error"))
})

