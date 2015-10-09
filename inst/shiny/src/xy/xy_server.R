output$xy1 <- rAmCharts::renderAmCharts({
  # prepare data
  dp <- data.table(y = c(10,5,-10,-6,15,13,1),
                   x = c(14,3,8,5,-4,1,6),
                   value = c(59,50,19,65,92,8,16),
                   y2 = c(-5,-15,-4,-5,-10,-2,0),
                   x2 = c(-3,-8,6,-6,-8,0,-3),
                   value2 = c(44,12,35,168,102,41,16))
  
  # build the chart
  pipeR::pipeline(
    amXYChart(startDuration = 0, dataProvider = dp,
              theme = input$theme_xy,
              marginLeft = 46, marginBottom = 35),
    addValueAxis(position = 'bottom', axisAlpha = 0),
    addValueAxis(minMaxMultiplier = 1.2, position = 'left', axisAlpha = 0),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>',
             bullet = 'circle', bulletBorderAlpha = 0.2,
             bulletAlpha = 0.8,lineAlpha=0, fillAlphas = 0,
             valueField = 'value', xField = 'x',yField = 'y', maxBulletSize = 100),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>',
             bullet = 'diamond',bulletBorderAlpha=0.2,
             bulletAlpha = 0.8, lineAlpha = 0, fillAlphas = 0, valueField = 'value2',
             xField = 'x2', yField = 'y2', maxBulletSize = 100)
  )
})

output$code_xy1 <- renderText({
  "
  # prepare data
  dp <- data.table(y = c(10,5,-10,-6,15,13,1),
                   x = c(14,3,8,5,-4,1,6),
                   value = c(59,50,19,65,92,8,16),
                   y2 = c(-5,-15,-4,-5,-10,-2,0),
                   x2 = c(-3,-8,6,-6,-8,0,-3),
                   value2 = c(44,12,35,168,102,41,16))
  
  # build the chart
  pipeR::pipeline(
    amXYChart(startDuration = 0, dataProvider = dp,
              theme = input$theme_xy,
              marginLeft = 46, marginBottom = 35),
    addValueAxis(position = 'bottom', axisAlpha = 0),
    addValueAxis(minMaxMultiplier = 1.2, position = 'left', axisAlpha = 0),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>',
             bullet = 'circle', bulletBorderAlpha = 0.2,
             bulletAlpha = 0.8,lineAlpha=0, fillAlphas = 0,
             valueField = 'value', xField = 'x',yField = 'y', maxBulletSize = 100),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>',
             bullet = 'diamond',bulletBorderAlpha=0.2,
             bulletAlpha = 0.8, lineAlpha = 0, fillAlphas = 0, valueField = 'value2',
             xField = 'x2', yField = 'y2', maxBulletSize = 100)
  )
  "
})