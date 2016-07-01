

output$xy0 <- rAmCharts::renderAmCharts({
  ##Data
  dp <- data.frame(x = round(runif(10), 2), y = round(runif(10), 2))
  ##Plot
  pipeR::pipeline(
    amXYChart(dataProvider = dp),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b>',
             bullet = 'circle', lineAlpha=0, xField = 'x',yField = 'y', maxBulletSize = 100)
  )
})



output$code_xy0 <- renderText({
  "
  ##Data
  dp <- data.frame(x = round(runif(10), 2), y = round(runif(10), 2))
  ##Plot
  pipeR::pipeline(
    amXYChart(dataProvider = dp),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b>',
             bullet = 'circle', lineAlpha=0, xField = 'x',yField = 'y', maxBulletSize = 100)
  )
"
})




output$xy1 <- rAmCharts::renderAmCharts({
  ##Data
  dp <- data.frame(y = c(10, 5, -10, -6, 15, 13, 1),
                   x = c(14, 3, 8, 5, -4, 1, 6),
                   value = c(59, 50, 19, 65, 92, 8, 16),
                   y2 = c(-5, -15, -4, -5, -10, -2, 0),
                   x2 = c(-3, -8, 6, -6, -8, 0, -3),
                   value2 = c(44, 12, 35, 168, 102, 41, 16))
  ##Plot
  pipeR::pipeline(
    amXYChart(startDuration = 0, dataProvider = dp,
              marginLeft = 46, marginBottom = 35),
    addValueAxis(position = 'bottom', axisAlpha = 0),
    addValueAxis(minMaxMultiplier = 1.2, position = 'left', axisAlpha = 0),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>',
             bullet = 'circle', bulletBorderAlpha = 0.2,
             bulletAlpha = 0.8,lineAlpha=0, fillAlphas = 0,
             valueField = 'value', xField = 'x',yField = 'y', maxBulletSize = 100, title = 'A'),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>',
             bullet = 'diamond',bulletBorderAlpha=0.2,
             bulletAlpha = 0.8, lineAlpha = 0, fillAlphas = 0, valueField = 'value2',
             xField = 'x2', yField = 'y2', maxBulletSize = 100, title = 'Z'),
    setLegend(useGrahSetting = TRUE)
  )
})

output$code_xy1 <- renderText({
  "
  ##Data
  dp <- data.frame(y = c(10, 5, -10, -6, 15, 13, 1),
                   x = c(14, 3, 8, 5, -4, 1, 6),
                   value = c(59, 50, 19, 65, 92, 8, 16),
                   y2 = c(-5, -15, -4, -5, -10, -2, 0),
                   x2 = c(-3, -8, 6, -6, -8, 0, -3),
                   value2 = c(44, 12, 35, 168, 102, 41, 16))
  ##Plot
  pipeR::pipeline(
    amXYChart(startDuration = 0, dataProvider = dp,
              marginLeft = 46, marginBottom = 35),
    addValueAxis(position = 'bottom', axisAlpha = 0),
    addValueAxis(minMaxMultiplier = 1.2, position = 'left', axisAlpha = 0),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>',
             bullet = 'circle', bulletBorderAlpha = 0.2,
             bulletAlpha = 0.8,lineAlpha=0, fillAlphas = 0,
             valueField = 'value', xField = 'x',yField = 'y', maxBulletSize = 100, title = 'A'),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>',
             bullet = 'diamond',bulletBorderAlpha=0.2,
             bulletAlpha = 0.8, lineAlpha = 0, fillAlphas = 0, valueField = 'value2',
             xField = 'x2', yField = 'y2', maxBulletSize = 100, title = 'Z'),
    setLegend(useGrahSetting = TRUE)
  )
  "
  })







output$xy2 <- rAmCharts::renderAmCharts({
  ##Data
  dp <- data.frame(y = c(1, -4, 5, 9),
                   x = c(-10, 5, 14, 8),
                   errorY = c(1, 2, 3, 1),
                   errorX = c( 2, 3, 1, 10))
  ##Plot
  pipeR::pipeline(
    amXYChart(dataProvider = dp),
    addValueAxis(position = 'bottom', id = 'X1'),
    addValueAxis(position = 'left', id = 'Y1'),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>x error:<b>[[errorX]]</b><br>y error:<b>[[errorY]]</b',
             bullet = 'xError', errorField = 'errorX', lineAlpha = 0, xField = 'x', yField = 'y', fillAlphas = 0,
             bulletAxis = 'X1'),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>x error:<b>[[errorX]]</b><br>y error:<b>[[errorY]]</b>',
             bullet = 'yError', errorField = 'errorY', lineAlpha = 0, xField = 'x', yField = 'y', fillAlphas = 0,
             bulletAxis = 'Y1')
  
  )
})


output$code_xy2 <- renderText({
  "
  ##Data
  dp <- data.frame(y = c(1, -4, 5, 9),
                   x = c(-10, 5, 14, 8),
                   errorY = c(1, 2, 3, 1),
                   errorX = c( 2, 3, 1, 10))
  ##Plot
  pipeR::pipeline(
    amXYChart(dataProvider = dp),
    addValueAxis(position = 'bottom', id = 'X1'),
    addValueAxis(position = 'left', id = 'Y1'),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>x error:<b>[[errorX]]</b><br>y error:<b>[[errorY]]</b',
             bullet = 'xError', errorField = 'errorX', lineAlpha = 0, xField = 'x', yField = 'y', fillAlphas = 0,
             bulletAxis = 'X1'),
    addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b><br>x error:<b>[[errorX]]</b><br>y error:<b>[[errorY]]</b>',
             bullet = 'yError', errorField = 'errorY', lineAlpha = 0, xField = 'x', yField = 'y', fillAlphas = 0,
             bulletAxis = 'Y1')
    
  )
  "
})



