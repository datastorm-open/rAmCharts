output$radar1 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amRadarChart(startDuration = 1, categoryField = category()),
    setDataProvider(data.frame(attribute = c("data", "brand", "singleness"),
                               p1 = c(.3, -1, 0), p2 = c(.7, 1, 2))),
    addGraph( balloonText = "Utility : [[value]]", valueField = "p1", title = "p1"),
    addGraph( balloonText = "Utility : [[value]]", valueField = "p2", title = "p2"),
    setLegend( useGraphSettings = TRUE),
    plot()
  )
})

output$code_radar1 <- renderText({
 "
  pipeR::pipeline(
    amRadarChart(startDuration = 1, categoryField = category()),
    setDataProvider(data.frame(attribute = c('data', 'brand', 'singleness'),
                               p1 = c(.3, -1, 0), p2 = c(.7, 1, 2))),
    addGraph(balloonText = 'Utility : [[value]]', valueField = 'p1', title = 'p1'),
    addGraph(balloonText = 'Utility : [[value]]', valueField = 'p2', title = 'p2'),
    setLegend(useGraphSettings = TRUE),
    plot()
  )
  "
})

output$radar2 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amRadarChart(theme  =  'light',  startDuration  =  1, categoryField  =  'direction'),
    setDataProvider(data.frame(direction  =  c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'),
                               value  =  c(10, 7, 8, 9, 5, 4, 6, 9))),
    setValueAxes(list(valueAxis(gridType = 'circles', minimum = 0, autoGridCount = FALSE,
                                axisAlpha = 0.2, fillAlpha = 0.05, fillColor = '#FFFFFF',
                                gridAlpha = 0.08, position = 'left'))),
    setGuides(list(guide(angle = 225, fillAlpha = 0.3, fillColor = '#0066CC',
                         tickLength = 0, toAngle = 315, toValue = 14, value = 0,
                         lineAlpha = 0), 
                   guide(angle = 45, fillAlpha = 0.3, fillColor = '#CC3333',
                         tickLength = 0, toAngle = 135, toValue = 14, value = 0,
                         lineAlpha = 0))),
    addGraph(balloonText = '[[category]]: [[value]] m/s', bullet = 'round',
             fillAlphas = 0.3, valueField = 'value'),
    plot()
  )
})

output$code_radar2 <- renderText({
  "
  pipeR::pipeline(
    amRadarChart(theme  =  'light',  startDuration  =  1, categoryField  =  'direction'),
    setDataProvider(data.frame(direction  =  c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'),
                               value  =  c(10, 7, 8, 9, 5, 4, 6, 9))),
    setValueAxes(list(valueAxis(gridType = 'circles', minimum = 0, autoGridCount = FALSE,
                                axisAlpha = 0.2, fillAlpha = 0.05, fillColor = '#FFFFFF',
                                gridAlpha = 0.08, position = 'left'))),
    setGuides(list(guide(angle = 225, fillAlpha = 0.3, fillColor = '#0066CC',
                         tickLength = 0, toAngle = 315, toValue = 14, value = 0,
                         lineAlpha = 0), 
                   guide(angle = 45, fillAlpha = 0.3, fillColor = '#CC3333',
                         tickLength = 0, toAngle = 135, toValue = 14, value = 0,
                         lineAlpha = 0))),
    addGraph(balloonText = '[[category]]: [[value]] m/s', bullet = 'round',
             fillAlphas = 0.3, valueField = 'value'),
    plot()
  )
  "
})