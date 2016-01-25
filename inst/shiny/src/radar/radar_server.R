output$radar1 <- rAmCharts::renderAmCharts({
  # prepare data
  dp <- data.frame(Eye=c("Blue","Brown","Green","Hazel"),Male=c(101,98,33,47),Female = c(114, 122, 31, 46))
  # build the chart
  pipeR::pipeline(
    amRadarChart(startDuration = 0, categoryField = 'Eye', dataProvider = dp),
    addGraph(balloonText = '(male) [[category]]: [[value]]', valueField = 'Male',
             title = 'Male', bullet = 'round'),
    addGraph(balloonText = '(female) [[category]]: [[value]]', valueField = 'Female',
             title = 'Female', bullet = 'round'),
    addValueAxis(gridType = 'circles')
  )
})

output$code_radar1 <- renderText({
  "
  ##Data
  dp <- data.frame(Eye=c('Blue','Brown','Green','Hazel'),Male=c(101,98,33,47),Female = c(114, 122, 31, 46))
  
  ##Plot
  pipeR::pipeline(
  amRadarChart(startDuration = 0, categoryField = 'Eye', dataProvider = dp),
  addGraph(balloonText = '(male) [[category]]: [[value]]', valueField = 'Male',
  title = 'Male', bullet = 'round'),
  addGraph(balloonText = '(female) [[category]]: [[value]]', valueField = 'Female',
  title = 'Female', bullet = 'round'),
  addValueAxis(gridType = 'circles')
)
  "
})



output$radar3 <- rAmCharts::renderAmCharts({
  # Data
  dp <- data.frame(Eye = c('Blue' ,'Brown' , 'Green', 'Hazel'),
                   Male = c(101, 98, 33, 47),
                   Female = c(114, 122, 31, 46))
  # Plot
  pipeR::pipeline(
    amRadarChart(startDuration = 0, categoryField = 'Eye', dataProvider = dp),
    addGraph(balloonText = '(male) [[category]]: [[value]]', valueField = 'Male',
             title = 'Male',fillAlphas=0.5,bullet='xError'),
    addGraph(balloonText = '(female) [[category]]: [[value]]', valueField = 'Female',
             title = 'Female', bullet = 'round'),
    setLegend(position = 'right'),
    addTitle(text='Title exemple')
  )
})

output$code_radar3 <- renderText({
  "
  # Data
  dp <- data.frame(Eye = c('Blue' ,'Brown' , 'Green', 'Hazel'),
                   Male = c(101, 98, 33, 47),
                   Female = c(114, 122, 31, 46))
  # Plot
  pipeR::pipeline(
    amRadarChart(startDuration = 0, categoryField = 'Eye', dataProvider = dp),
    addGraph(balloonText = '(male) [[category]]: [[value]]', valueField = 'Male',
             title = 'Male',fillAlphas=0.5,bullet='xError'),
    addGraph(balloonText = '(female) [[category]]: [[value]]', valueField = 'Female',
             title = 'Female', bullet = 'round'),
    setLegend(position = 'right'),
    addTitle(text='Title exemple')
  )
  "
})

output$radar2 <- rAmCharts::renderAmCharts({
  ## Data
  data('data_wind')
  data_wind$category <- c("N", "NE", "E", "SE", "S", "SW", "W", "NO")
  
  ## Plot
  pipeR::pipeline(
    amRadarChart(theme = 'light', startDuration = 0, categoryField = 'category',
                 dataProvider = data_wind),
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
             fillAlphas = 0.3, valueField = 'weak')
  )
})

output$code_radar2 <- renderText({
  "
##Data
  data('data_wind')
  ##Plot
  pipeR::pipeline(
    amRadarChart(theme = 'light', startDuration = 0, categoryField  =  'direction'),
    setDataProvider(data_wind),
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