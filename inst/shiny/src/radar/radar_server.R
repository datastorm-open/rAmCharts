output$radar1 <- rAmCharts::renderAmCharts({
  # prepare data
  HairEyeColor_DT <- as.data.table(get('HairEyeColor', 'package:datasets'))
  (dp <- HairEyeColor_DT[, .(freq = sum(N)), by = list(Eye, Sex)])
  (dp <- split(dp, f = dp$Sex))
  setkey(dp$Male, Eye)
  setkey(dp$Female, Eye)
  (dp <- dp$Male[dp$Female])
  
  # build the chart
  pipeR::pipeline(
    amRadarChart(startDuration = 0, categoryField = 'Eye', dataProvider = dp),
    addGraph(balloonText = '(male) [[category]]: [[value]]', valueField = 'freq',
             title = 'Male', bullet = 'round'),
    addGraph(balloonText = '(female) [[category]]: [[value]]', valueField = 'i.freq',
             title = 'Female', bullet = 'round'),
    addValueAxis(gridType = 'circles'),
    setProperties(theme = input$theme_radar1)
  )
})

output$code_radar1 <- renderText({
  "
  # prepare data
  HairEyeColor_DT <- as.data.table(get('Titanic', 'package:datasets'))
  (dp <- HairEyeColor_DT[, .(freq = sum(N)), by = list(Eye, Sex)])
  (dp <- split(dp, f = dp$Sex))
  setkey(dp$Male, Eye)
  setkey(dp$Female, Eye)
  (dp <- dp$Male[dp$Female])
  
  # build the chart
  pipeR::pipeline(
    amRadarChart(startDuration = 0, categoryField = 'Eye', dataProvider = dp),
    addGraph(balloonText = '(male) [[category]]: [[value]]', valueField = 'freq',
             title = 'Male', bullet = 'round'),
    addGraph(balloonText = '(female) [[category]]: [[value]]', valueField = 'i.freq',
             title = 'Female', bullet = 'round'),
    addValueAxis(gridType = 'circles'),
    setProperties(theme = input$theme_radar1)
  )
  "
})

output$radar2 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amRadarChart(theme = 'light', startDuration = 0, categoryField  =  'direction'),
    setDataProvider(data.frame(direction  =  c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'),
                               value  =  c(10, 7, 8, 9, 5, 4, 6, 9))),
    addValueAxis(gridType = 'circles', minimum = 0, autoGridCount = FALSE,
                 axisAlpha = 0.2, fillAlpha = 0.05, fillColor = '#FFFFFF',
                 gridAlpha = 0.08, position = 'left'),
    addGuide(angle = 225, fillAlpha = 0.3, fillColor = '#0066CC',
             tickLength = 0, toAngle = 315, toValue = 14, value = 0,
             lineAlpha = 0), 
    addGuide(angle = 45, fillAlpha = 0.3, fillColor = '#CC3333',
             tickLength = 0, toAngle = 135, toValue = 14, value = 0,
             lineAlpha = 0),
    addGraph(balloonText = '[[category]]: [[value]] m/s', bullet = 'round',
             fillAlphas = 0.3, valueField = 'value')
  )
})

output$code_radar2 <- renderText({
  "
  pipeR::pipeline(
    amRadarChart(theme = 'light', startDuration = 0, categoryField  =  'direction'),
    setDataProvider(data.frame(direction  =  c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'),
                               value  =  c(10, 7, 8, 9, 5, 4, 6, 9))),
    addValueAxis(gridType = 'circles', minimum = 0, autoGridCount = FALSE,
                 axisAlpha = 0.2, fillAlpha = 0.05, fillColor = '#FFFFFF',
                 gridAlpha = 0.08, position = 'left'),
    addGuide(angle = 225, fillAlpha = 0.3, fillColor = '#0066CC',
             tickLength = 0, toAngle = 315, toValue = 14, value = 0,
             lineAlpha = 0), 
    addGuide(angle = 45, fillAlpha = 0.3, fillColor = '#CC3333',
             tickLength = 0, toAngle = 135, toValue = 14, value = 0,
             lineAlpha = 0),
    addGraph(balloonText = '[[category]]: [[value]] m/s', bullet = 'round',
             fillAlphas = 0.3, valueField = 'value')
  )
  "
})