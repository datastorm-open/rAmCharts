output$serial1 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amSerialChart(theme = 'dark', categoryField = 'country', creditsPosition = 'top-right'),
    setDataProvider(data.frame(country = c('FR', 'US'), visits = 1:2)),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'visits', fillAlphas = .8, lineAlpha = .2),
    addListener('clickGraphItem' , 'function(event) {alert(\"ok !\");}'), 
    setExport(position = 'bottom-right'),
    setChartCursor(),
    setChartScrollbar(),
    plot()
  )
})

output$code_serial1 <- renderText({
  "
  pipeR::pipeline(
    amSerialChart(theme = 'dark', categoryField = 'country', creditsPosition = 'top-right'),
    setDataProvider(data.frame(country = c('FR', 'US'), visits = 1:2)),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'visits', fillAlphas = .8, lineAlpha = .2),
    addListener('clickGraphItem' , 'function(event) {alert(\"ok !\");}'), 
    setExport(position = 'bottom-right'),
    setChartCursor(),
    setChartScrollbar(),
    plot()
  )
  "
})

output$serial2 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amSerialChart(addClassNames = TRUE, theme = 'chalk', autoMargins = FALSE,
                  marginLeft = 30, marginRight = 8, marginTop = 10,
                  marginBottom = 26, startDuration = 1,
                  categoryField = 'year'),
    setDataProvider(data.frame(year  =  c(2010, 2011, 2012, 2013, 2014), income  =  c(20.4, 20.6, 24.3, 21.5, 22.3), 
                               expenses  =  c(31.5, 32.4, 25.6, 22.6, 24.9),
                               dashLengthColumn = c(NA, NA, NA, NA, 5), 
                               alpha = c(NA, NA, NA, NA, 0.2),
                               additional = c('', '', '', '', '(projection)'))),
    setBalloon(adjustBorderColor = FALSE, horizontalPadding = 10,
               verticalPadding = 8, color = '#ffffff'),
    addValueAxes(axisAlpha = 0, position = 'left'),
    addGraph(alphaField = 'alpha', fillAlphas = 1, title = 'Income',
             balloonText = '<span style = "font-size:12px;">[[title]] in [[category]]:<br><span style = "font-size:20px;">[[value]]</span> [[additional]]</span>', 
             type = 'column', valueField = 'income',
             dashLengthField = 'dashLengthColumn'),
    addGraph(id = 'graph2', bullet = 'round', lineThickness = 3,
             bulletSize = 7, bulletBorderAlpha = 1, bulletColor = '#FFFFFF',
             useLineColorForBulletBorder = TRUE, bulletBorderThickness = 3, 
             fillAlphas = 0, lineAlpha = 1, title = 'Expenses', valueField = 'expenses',
             balloonText = '<span style = "font-size:12px;">[[title]] in [[category]]:<br><span style = "font-size:20px;">[[value]]</span> [[additional]]</span>'),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, tickLength = 0),
    plot()
  )
})

output$code_serial2 <- renderText({
  "
  pipeR::pipeline(
    amSerialChart(addClassNames = TRUE, theme = 'chalk', autoMargins = FALSE,
                  marginLeft = 30, marginRight = 8, marginTop = 10,
                  marginBottom = 26, startDuration = 1,
                  categoryField = 'year'),
    setDataProvider(data.frame(year  =  c(2010, 2011, 2012, 2013, 2014), income  =  c(20.4, 20.6, 24.3, 21.5, 22.3), 
                               expenses  =  c(31.5, 32.4, 25.6, 22.6, 24.9),
                               dashLengthColumn = c(NA, NA, NA, NA, 5), 
                               alpha = c(NA, NA, NA, NA, 0.2),
                               additional = c('', '', '', '', '(projection)'))),
    setBalloon(adjustBorderColor = FALSE, horizontalPadding = 10,
               verticalPadding = 8, color = '#ffffff'),
    addValueAxes(axisAlpha = 0, position = 'left'),
    addGraph(alphaField = 'alpha', fillAlphas = 1, title = 'Income',
             balloonText = '<span style = \"font-size:12px;\">[[title]] in [[category]]:<br><span style = \"font-size:20px;\">[[value]]</span> [[additional]]</span>', 
             type = 'column', valueField = 'income',
             dashLengthField = 'dashLengthColumn'),
    addGraph(id = 'graph2', bullet = 'round', lineThickness = 3,
             bulletSize = 7, bulletBorderAlpha = 1, bulletColor = '#FFFFFF',
             useLineColorForBulletBorder = TRUE, bulletBorderThickness = 3, 
             fillAlphas = 0, lineAlpha = 1, title = 'Expenses', valueField = 'expenses',
             balloonText = '<span style = \"font-size:12px;\">[[title]] in [[category]]:<br><span style = \"font-size:20px;\">[[value]]</span> [[additional]]</span>'),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, tickLength = 0),
    plot()
  )
  "
})

output$serial3 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amSerialChart(theme = 'chalk', startDuration = 2,
                  categoryField = 'country', depth3D = 40,
                  angle = 30),
    setDataProvider(data.table(
      country = c('USA', 'China', 'Japan', 'Germany', 'UK', 'France', 'India', 'Spain', 'Netherlands', 'Russia'), 
      visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580), 
      color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01', '#B0DE09', '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0'))),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', fillColorsField = 'color', 
             fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = 'visits'),
    setChartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0, zoomable = FALSE),
    setCategoryAxis(gridPosition = 'start', labelRotation = 45, axisAlpha = 0, gridAlpha = 0),
    plot()
  )
})

output$code_serial3 <- renderText({
  "
  pipeR::pipeline(
    amSerialChart(theme = 'chalk', startDuration = 2,
                  categoryField = 'country', depth3D = 40,
                  angle = 30),
    setDataProvider(data.table(
      country = c('USA', 'China', 'Japan', 'Germany', 'UK', 'France', 'India', 'Spain', 'Netherlands', 'Russia'), 
      visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580), 
      color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01', '#B0DE09', '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0'))),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', fillColorsField = 'color', 
             fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = 'visits'),
    setChartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0, zoomable = FALSE),
    setCategoryAxis(gridPosition = 'start', labelRotation = 45, axisAlpha = 0, gridAlpha = 0),
    plot()
  )
  "
})

output$serial4 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amSerialChart(theme = 'dark', marginRight = 30, plotAreaBorderAlpha = 0,
                  categoryField = 'year'),
    setDataProvider(data.frame(
      year = 1994:2012, 
      cars = c(15, 13, 14, 15, 16, 18, 19, 22, 24, 20, 24, 25, 26, 35, 36, 37, 38, 39, 40), 
      motorcycles = c(15, 13, 14, 15, 16, 18, 19, 22, 24, 20, 24, 25, 26, 35, 36, 37, 38, 39, 40), 
      bicycles = c(15, 13, 14, 15, 16, 18, 19, 22, 24, 20, 24, 25, 26, 35, 36, 37, 38, 39, 40))),
    setLegend(equalWidths = FALSE, periodValueText = 'total: [[value.sum]]', position = 'top', valueAlign = 'left', valueWidth = 100),
    addValueAxes(stackType = 'regular', gridAlpha = 0.07, position = 'left', title = 'Traffic incidents'),
    addGraph(balloonText = '<img src = "http://www.amcharts.com/lib/3/images/car.png" style = "vertical-align:bottom; margin-right: 10px; width:28px; height:21px;"><span style = "font-size:14px; color:#000000;"><b>[[value]]</b></span>', 
                    fillAlphas = 0.6, hidden = TRUE, lineAlpha = 0.4, title = 'Cars', valueField = 'cars'),
    addGraph(balloonText = '<img src = "http://www.amcharts.com/lib/3/images/motorcycle.png" style = "vertical-align:bottom; margin-right: 10px; width:28px; height:21px;"><span style = "font-size:14px; color:#000000;"><b>[[value]]</b></span>', 
                    fillAlphas = 0.6, lineAlpha = 0.4, title = 'Motorcycles', valueField = 'motorcycles'),
    addGraph(balloonText = '<img src = "http://www.amcharts.com/lib/3/images/bicycle.png" style = "vertical-align:bottom; margin-right: 10px; width:28px; height:21px;"><span style = "font-size:14px; color:#000000;"><b>[[value]]</b></span>', 
                    fillAlphas = 0.6, lineAlpha = 0.4, title = 'Bicycles', valueField = 'bicycles'),
    setChartCursor(cursorAlpha = 0),
    setCategoryAxis(startOnAxis = TRUE, axisColor = '#DADADA', gridAlpha = 0.07),
    setGuides(list(guide(category = '2001', toCategory = '2003',
                                lineColor = '#CC0000', lineAlpha = 1, fillAlpha = 0.2,
                                fillColor = '#CC0000', dashLength = 2, inside = TRUE,
                                labelRotation = 90, label = 'fines for speeding increased'),
                          guide(category = '2007', lineColor = '#CC0000', lineAlpha = 1,
                                dashLength = 2, inside = TRUE, labelRotation = 90,
                                label = 'motorcycle fee introduced'))),
    setChartScrollbar(),
    plot()
  )
})

output$code_serial4 <- renderText({
  "
  pipeR::pipeline(
    amSerialChart(theme = 'dark', marginRight = 30, plotAreaBorderAlpha = 0,
                  categoryField = 'year'),
    setDataProvider(data.frame(
      year = 1994:2012, 
      cars = c(15, 13, 14, 15, 16, 18, 19, 22, 24, 20, 24, 25, 26, 35, 36, 37, 38, 39, 40), 
      motorcycles = c(15, 13, 14, 15, 16, 18, 19, 22, 24, 20, 24, 25, 26, 35, 36, 37, 38, 39, 40), 
      bicycles = c(15, 13, 14, 15, 16, 18, 19, 22, 24, 20, 24, 25, 26, 35, 36, 37, 38, 39, 40))),
    setLegend(equalWidths = FALSE, periodValueText = 'total: [[value.sum]]', position = 'top', valueAlign = 'left', valueWidth = 100),
    addValueAxes(stackType = 'regular', gridAlpha = 0.07, position = 'left', title = 'Traffic incidents'),
    addGraph(balloonText = '<img src = \"http://www.amcharts.com/lib/3/images/car.png\" style = \"vertical-align:bottom; margin-right: 10px; width:28px; height:21px;\"><span style = \"font-size:14px; color:#000000;\"><b>[[value]]</b></span>', 
             fillAlphas = 0.6, hidden = TRUE, lineAlpha = 0.4, title = 'Cars', valueField = 'cars'),
    addGraph(balloonText = '<img src = \"http://www.amcharts.com/lib/3/images/motorcycle.png\" style = \"vertical-align:bottom; margin-right: 10px; width:28px; height:21px;\"><span style = \"font-size:14px; color:#000000;\"><b>[[value]]</b></span>', 
             fillAlphas = 0.6, lineAlpha = 0.4, title = 'Motorcycles', valueField = 'motorcycles'),
    addGraph(balloonText = '<img src = \"http://www.amcharts.com/lib/3/images/bicycle.png\" style = \"vertical-align:bottom; margin-right: 10px; width:28px; height:21px;\"><span style = \"font-size:14px; color:#000000;\"><b>[[value]]</b></span>', 
             fillAlphas = 0.6, lineAlpha = 0.4, title = 'Bicycles', valueField = 'bicycles'),
    setChartCursor(cursorAlpha = 0),
    setCategoryAxis(startOnAxis = TRUE, axisColor = '#DADADA', gridAlpha = 0.07),
    setGuides(list(guide(category = '2001', toCategory = '2003',
                         lineColor = '#CC0000', lineAlpha = 1, fillAlpha = 0.2,
                         fillColor = '#CC0000', dashLength = 2, inside = TRUE,
                         labelRotation = 90, label = 'fines for speeding increased'),
                   guide(category = '2007', lineColor = '#CC0000', lineAlpha = 1,
                         dashLength = 2, inside = TRUE, labelRotation = 90,
                         label = 'motorcycle fee introduced'))),
    setChartScrollbar(),
    plot()
  )
  "
})