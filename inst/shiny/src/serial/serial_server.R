output$serial1 <- renderAmCharts({
  # prepare data
  data <- as.data.table(get('Titanic', 'package:datasets'))
  data <- data[, .(freq = sum(N)), by = list(Sex, Survived)]
  (data <- split(data, f = data$Survived))
  setkey(data$Yes, Sex)
  setkey(data$No, Sex)
  (dp <- data$Yes[data$No])
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'Sex', startDuration = 0,
                  dataProvider = dp),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'freq', fillAlphas = .8, lineAlpha = .2, title = 'Yes'),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'i.freq', fillAlphas = .8, lineAlpha = .2, title = 'No'),
    addValueAxis(stackType = '100%'),
    addTitle(text = 'Survivors to Titanic'),
    setLegend(useGraphSettings = TRUE),
    setProperties(theme = input$theme_serial)
  )
})

output$code_serial1 <- renderText({
  "
  # prepare data
  data <- as.data.table(get('Titanic', 'package:datasets'))
  data <- data[, .(freq = sum(N)), by = list(Survived, Sex)]
  (data <- split(data, f = data$Sex))
  setkey(data$Male, Survived)
  setkey(data$Female, Survived)
  (dp <- data$Male[data$Female])
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'Survived', startDuration = 0,
                  dataProvider = dp),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'freq', fillAlphas = .8, lineAlpha = .2, title = 'Male'),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'i.freq', fillAlphas = .8, lineAlpha = .2, title = 'Female'),
    addValueAxis(stackType = '100%'),
    setTitle(text = 'Survivors to Titanic'),
    setLegend(useGraphSettings = TRUE)
  )
  "
})

output$serial2 <- renderAmCharts({
  # prepare data
  dp <- data.table(year  =  c(2010, 2011, 2012, 2013, 2014),
                   income  =  c(20.4, 20.6, 24.3, 21.5, 22.3), 
                   expenses  =  c(31.5, 32.4, 25.6, 22.6, 24.9),
                   dashLengthColumn = c(NA, NA, NA, NA, 5), 
                   alpha = c(NA, NA, NA, NA, 0.2),
                   additional = c('', '', '', '', '(projection)'))
  
  balloonText <- paste('<span style = \'font-size:12px;\'>[[title]] in [[category]]:',
                       '<br><span style = \'font-size:20px;\'>[[value]]</span> [[additional]]</span>')
  # build the chart
  pipeR::pipeline(
    amSerialChart(addClassNames = TRUE, autoMargins = FALSE,
                  dataProvider = dp, marginLeft = 30, marginRight = 8,
                  marginTop = 10, marginBottom = 26, startDuration = 1,
                  categoryField = 'year', startDuration = 0),
    setBalloon(adjustBorderColor = FALSE, horizontalPadding = 10,
               verticalPadding = 8, color = '#ffffff'),
    addValueAxis(axisAlpha = 0, position = 'left'),
    addGraph(alphaField = 'alpha', fillAlphas = 1, title = 'Income',
             balloonText = balloonText, 
             type = 'column', valueField = 'income',
             dashLengthField = 'dashLengthColumn'),
    addGraph(id = 'graph2', bullet = 'round', lineThickness = 3,
             bulletSize = 7, bulletBorderAlpha = 1, bulletColor = '#FFFFFF',
             useLineColorForBulletBorder = TRUE, bulletBorderThickness = 3, 
             fillAlphas = 0, lineAlpha = 1, title = 'Expenses', valueField = 'expenses',
             balloonText = balloonText),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, tickLength = 0)
  )
})

output$code_serial2 <- renderText({
  "
  # prepare data
  dp <- data.table(year  =  c(2010, 2011, 2012, 2013, 2014),
                   income  =  c(20.4, 20.6, 24.3, 21.5, 22.3), 
                   expenses  =  c(31.5, 32.4, 25.6, 22.6, 24.9),
                   dashLengthColumn = c(NA, NA, NA, NA, 5), 
                   alpha = c(NA, NA, NA, NA, 0.2),
                   additional = c('', '', '', '', '(projection)'))
  
  balloonText <- paste('<span style = \'font-size:12px;\'>[[title]] in [[category]]:',
                       '<br><span style = \'font-size:20px;\'>[[value]]</span> [[additional]]</span>')
  # build the chart
  pipeR::pipeline(
    amSerialChart(addClassNames = TRUE, autoMargins = FALSE,
                  dataProvider = dp, marginLeft = 30, marginRight = 8,
                  marginTop = 10, marginBottom = 26, startDuration = 1,
                  categoryField = 'year', startDuration = 0),
    setBalloon(adjustBorderColor = FALSE, horizontalPadding = 10,
               verticalPadding = 8, color = '#ffffff'),
    addValueAxis(axisAlpha = 0, position = 'left'),
    addGraph(alphaField = 'alpha', fillAlphas = 1, title = 'Income',
             balloonText = balloonText, 
             type = 'column', valueField = 'income',
             dashLengthField = 'dashLengthColumn'),
    addGraph(id = 'graph2', bullet = 'round', lineThickness = 3,
             bulletSize = 7, bulletBorderAlpha = 1, bulletColor = '#FFFFFF',
             useLineColorForBulletBorder = TRUE, bulletBorderThickness = 3, 
             fillAlphas = 0, lineAlpha = 1, title = 'Expenses', valueField = 'expenses',
             balloonText = balloonText),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, tickLength = 0)
  )
  "
})

output$serial3 <- renderAmCharts({
  # prepare data
  dp <- data.table(
    country = c('USA', 'China', 'Japan', 'Germany', 'UK', 'France',
                'India', 'Spain', 'Netherlands', 'Russia'), 
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580), 
    color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01',
              '#B0DE09', '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0')
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'country', dataProvider = dp,
                  depth3D = 40, angle = 30),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', fillColorsField = 'color', 
             fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = 'visits'),
    setChartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0, zoomable = FALSE),
    setCategoryAxis(gridPosition = 'start', labelRotation = 45, axisAlpha = 0, gridAlpha = 0)
  )
})

output$code_serial3 <- renderText({
  "
  # prepare data
  dp <- data.table(
    country = c('USA', 'China', 'Japan', 'Germany', 'UK', 'France',
                'India', 'Spain', 'Netherlands', 'Russia'), 
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580), 
    color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01',
              '#B0DE09', '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0')
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'country', dataProvider = dp,
                  depth3D = 40, angle = 30),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', fillColorsField = 'color', 
             fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = 'visits'),
    setChartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0, zoomable = FALSE),
    setCategoryAxis(gridPosition = 'start', labelRotation = 45, axisAlpha = 0, gridAlpha = 0)
  )
  "
})

output$serial4 <- rAmCharts::renderAmCharts({
  # prepare data
  dp <- data.table(year = 1994:2012, 
                   cars = rnorm(length(1994:2012), mean = 10), 
                   motorcycles = rnorm(length(1994:2012), mean = 15), 
                   bicycles = rnorm(length(1994:2012), mean = 20))
  dp <- round(dp)
  
  url_car <- 'http://www.amcharts.com/lib/3/images/car.png'
  url_motorcycle <- 'http://www.amcharts.com/lib/3/images/motorcycle.png'
  url_bicycle <- 'http://www.amcharts.com/lib/3/images/bicycle.png'
  pref <- '<img src = '
  suf <- paste('style = \'vertical-align:bottom;',
               'margin-right: 10px; width:28px; height:21px;\'>',
               '<span style = \'font-size:14px; color:#000000;\'>',
               '<b>[[value]]</b></span>')
  
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(marginRight = 30, plotAreaBorderAlpha = 0, categoryField = 'year',
                  startDuration = 0, dataProvider = dp, theme = 'light'),
    setLegend(equalWidths = FALSE, periodValueText = 'total: [[value.sum]]',
              position = 'top', valueAlign = 'left', valueWidth = 100),
    addValueAxis(stackType = 'regular', gridAlpha = 0.07, position = 'left',
                 title = 'Traffic incidents'),
    addGraph(balloonText = paste(pref, url_car, suf), fillAlphas = 0.6,
             hidden = TRUE, lineAlpha = 0.4, title = 'Cars', valueField = 'cars'),
    addGraph(balloonText = paste(pref, url_motorcycle, suf), fillAlphas = 0.6,
             lineAlpha = 0.4, title = 'Motorcycles', valueField = 'motorcycles'),
    addGraph(balloonText = paste(pref, url_bicycle, suf), fillAlphas = 0.6,
             lineAlpha = 0.4, title = 'Bicycles', valueField = 'bicycles'),
    setCategoryAxis(startOnAxis = TRUE, axisColor = '#DADADA', gridAlpha = 0.07),
    addGuide(category = '2001', toCategory = '2003',
             lineColor = '#CC0000', lineAlpha = 1, fillAlpha = 0.2,
             fillColor = '#CC0000', dashLength = 2, inside = TRUE,
             labelRotation = 90, label = 'fines for speeding increased'),
    addGuide(category = '2007', lineColor = '#CC0000', lineAlpha = 1,
             dashLength = 2, inside = TRUE, labelRotation = 90,
             label = 'motorcycle fee introduced'),
    setChartScrollbar(oppositeAxis = FALSE, dragIcon = 'dragIconRectBigBlack'),
    setChartCursor(cursorAlpha = 0)
  )
})

output$code_serial4 <- renderText({
  "
  # prepare data
  dp <- data.table(year = 1994:2012, 
                   cars = rnorm(length(1994:2012), mean = 10), 
                   motorcycles = rnorm(length(1994:2012), mean = 15), 
                   bicycles = rnorm(length(1994:2012), mean = 20))
  dp <- round(dp)
  
  url_car <- 'http://www.amcharts.com/lib/3/images/car.png'
  url_motorcycle <- 'http://www.amcharts.com/lib/3/images/motorcycle.png'
  url_bicycle <- 'http://www.amcharts.com/lib/3/images/bicycle.png'
  pref <- '<img src = '
  suf <- paste('style = \'vertical-align:bottom;',
               'margin-right: 10px; width:28px; height:21px;\'>',
               '<span style = \'font-size:14px; color:#000000;\'>',
               '<b>[[value]]</b></span>')
  
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(marginRight = 30, plotAreaBorderAlpha = 0, categoryField = 'year',
                  startDuration = 0, dataProvider = dp, theme = 'light'),
    setLegend(equalWidths = FALSE, periodValueText = 'total: [[value.sum]]',
              position = 'top', valueAlign = 'left', valueWidth = 100),
    addValueAxis(stackType = 'regular', gridAlpha = 0.07, position = 'left',
                 title = 'Traffic incidents'),
    addGraph(balloonText = paste(pref, url_car, suf), fillAlphas = 0.6,
             hidden = TRUE, lineAlpha = 0.4, title = 'Cars', valueField = 'cars'),
    addGraph(balloonText = paste(pref, url_motorcycle, suf), fillAlphas = 0.6,
             lineAlpha = 0.4, title = 'Motorcycles', valueField = 'motorcycles'),
    addGraph(balloonText = paste(pref, url_bicycle, suf), fillAlphas = 0.6,
             lineAlpha = 0.4, title = 'Bicycles', valueField = 'bicycles'),
    setCategoryAxis(startOnAxis = TRUE, axisColor = '#DADADA', gridAlpha = 0.07),
    addGuide(category = '2001', toCategory = '2003',
             lineColor = '#CC0000', lineAlpha = 1, fillAlpha = 0.2,
             fillColor = '#CC0000', dashLength = 2, inside = TRUE,
             labelRotation = 90, label = 'fines for speeding increased'),
    addGuide(category = '2007', lineColor = '#CC0000', lineAlpha = 1,
             dashLength = 2, inside = TRUE, labelRotation = 90,
             label = 'motorcycle fee introduced'),
    setChartScrollbar(oppositeAxis = FALSE, dragIcon = 'dragIconRectBigBlack'),
    setChartCursor(cursorAlpha = 0)
  )
  "
})

output$serial5 <- renderAmCharts({
  pipeR::pipeline(
    amSerialChart(marginTop = 0, marginRight = 80, dataDateFormat = 'YYYY',
                  categoryField = 'year', startDuration = 0),
    setDataProvider(data.frame(year = 1950:2015, value = runif(length(1950:2015), -1, 1))),
    addValueAxes(axisAlpha = 0, position = 'left'),
    addGraph(id = 'g1', balloonText =  '[[category]]<br><b><span style="font-size = 14px;">[[value]]</span></b>',
             bullet = 'round', bulletSize =  8, lineColor =  '#d1655d', lineThickness =  2,
             negativeLineColor =  '#637bb6', type =  'smoothedLine', valueField =  'value'),
    setChartScrollbar(graph = 'g1', gridAlpha = 0, color = '#888888', scrollbarHeight = 55, backgroundAlpha = 0,
                      selectedBackgroundAlpha = 0.1, selectedBackgroundColor = '#888888', graphFillAlpha = 0,
                      autoGridCount = TRUE, selectedGraphFillAlpha = 0, graphLineAlpha = 0.2,
                      graphLineColor = '#c2c2c2', selectedGraphLineColor = '#888888', selectedGraphLineAlpha = 1),
    setChartCursor(categoryBalloonDateFormat = 'YYYY', cursorAlpha = 0, valueLineEnabled =TRUE,
                   valueLineBalloonEnabled =TRUE, valueLineAlpha = 0.5, fullWidth = TRUE),
    setCategoryAxis(minPeriod = 'YYYY', parseDates = TRUE, minorGridAlpha = 0.1, minorGridEnabled = TRUE),
    addListener('rendered', paste('function(event) {',
                                  'event.chart.zoomToIndexes(Math.round(event.chart.dataProvider.length * 0.4),',
                                  'Math.round(event.chart.dataProvider.length * 0.55))',
                                  '}'))
  )
})

output$code_serial5 <- renderText({
  "
  pipeR::pipeline(
    amSerialChart(marginTop = 0, marginRight = 80, dataDateFormat = 'YYYY',
                  categoryField = 'year'),
    setDataProvider(data.frame(year = 1950:2015, value = runif(length(1950:2015), -1, 1))),
    addValueAxes(axisAlpha = 0, position = 'left'),
    addGraph(id = 'g1', balloonText =  '[[category]]<br><b><span style=\"font-size = 14px;\">[[value]]</span></b>',
             bullet = 'round', bulletSize =  8, lineColor =  '#d1655d', lineThickness =  2,
             negativeLineColor =  '#637bb6', type =  'smoothedLine', valueField =  'value'),
    setChartScrollbar(graph = 'g1', gridAlpha = 0, color = '#888888', scrollbarHeight = 55, backgroundAlpha = 0,
                      selectedBackgroundAlpha = 0.1, selectedBackgroundColor = '#888888', graphFillAlpha = 0,
                      autoGridCount = TRUE, selectedGraphFillAlpha = 0, graphLineAlpha = 0.2,
                      graphLineColor = '#c2c2c2', selectedGraphLineColor = '#888888', selectedGraphLineAlpha = 1),
    setChartCursor(categoryBalloonDateFormat = 'YYYY', cursorAlpha = 0, valueLineEnabled =TRUE,
                   valueLineBalloonEnabled =TRUE, valueLineAlpha = 0.5, fullWidth = TRUE),
    setCategoryAxis(minPeriod = 'YYYY', parseDates = TRUE, minorGridAlpha = 0.1, minorGridEnabled = TRUE),
    addListener('rendered', paste('function(event) {',
                                  'event.chart.zoomToIndexes(Math.round(event.chart.dataProvider.length * 0.4),',
                                  'Math.round(event.chart.dataProvider.length * 0.55))',
                                  '}'))
  )
  "
})

output$serial6 <- renderAmCharts({
  # prepare data
  dp <- data.table(binf = c(1,2,3,4,5),
                   bsup = c(3,4,5,6,7),
                   value = c(2,3,4,5,6),
                   heure = c('9h','10h','11h','12h','13h'))
  
  # prepare legend
  legend <- pipeR::pipeline(
    amLegend(equalWidths = FALSE, position = 'bottom', valueAlign = 'left', valueWidth = 100), 
    addListener('hideItem' , paste('function(event){',
                                   'var id = event.dataItem.id;',
                                   'event.chart.hideGraph(event.chart.getGraphById(id + \'_from\'));',
                                   'event.chart.hideGraph(event.chart.getGraphById(id + \'_to\'));',
                                   '}')),
    addListener('showItem' , paste('function(event){',
                                   'var id = event.dataItem.id;',
                                   'event.chart.showGraph(event.chart.getGraphById(id + \'_from\'));',
                                   'event.chart.showGraph(event.chart.getGraphById(id + \'_to\'));',
                                   '}'))
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(legend = legend, dataProvider = dp, addClassNames=TRUE,
                  autoMargins = TRUE, startDuration = 0.5, categoryField = 'heure'),
    setBalloon(adjustBorderColor = FALSE, horizontalPadding = 10,
               verticalPadding = 8,color = '#ffffff'),
    addValueAxis(axisAlpha = 0, position = 'left'),
    addGraph(id = '2014_from', lineAlpha = 0, valueField = 'binf',
             showBalloon = FALSE, visibleInLegend = FALSE),
    addGraph(id = '2014_to', lineAlpha = 0, fillAlphas = 0.2,
             fillToGraph = '2014_from', valueField = 'bsup', 
             showBalloon = FALSE, visibleInLegend = FALSE),
    addGraph(id = '2014', valueField = 'value', title = 'value'),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, tickLength = 0),
    addTitle(text = 'listeners on legend')
  )
})

output$code_serial6 <- renderText({
  "
  # prepare data
  dp <- data.table(binf = c(1,2,3,4,5),
                   bsup = c(3,4,5,6,7),
                   value = c(2,3,4,5,6),
                   heure = c('9h','10h','11h','12h','13h'))
  
  # prepare legend
  legend <- pipeR::pipeline(
    amLegend(equalWidths = FALSE, position = 'bottom', valueAlign = 'left', valueWidth = 100), 
    addListener('hideItem' , paste('function(event){',
                                   'var id = event.dataItem.id;',
                                   'event.chart.hideGraph(event.chart.getGraphById(id + \'_from\'));',
                                   'event.chart.hideGraph(event.chart.getGraphById(id + \'_to\'));',
                                   '}')),
    addListener('showItem' , paste('function(event){',
                                   'var id = event.dataItem.id;',
                                   'event.chart.showGraph(event.chart.getGraphById(id + \'_from\'));',
                                   'event.chart.showGraph(event.chart.getGraphById(id + \'_to\'));',
                                   '}'))
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(legend = legend, dataProvider = dp, addClassNames=TRUE,
                  autoMargins = TRUE, startDuration = 0.5, categoryField = 'heure'),
    setBalloon(adjustBorderColor = FALSE, horizontalPadding = 10,
               verticalPadding = 8,color = '#ffffff'),
    addValueAxis(axisAlpha = 0, position = 'left'),
    addGraph(id = '2014_from', lineAlpha = 0, valueField = 'binf',
             showBalloon = FALSE, visibleInLegend = FALSE),
    addGraph(id = '2014_to', lineAlpha = 0, fillAlphas = 0.2,
             fillToGraph = '2014_from', valueField = 'bsup', 
             showBalloon = FALSE, visibleInLegend = FALSE),
    addGraph(id = '2014', valueField = 'value', title = 'value'),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, tickLength = 0),
    addTitle(text = 'listeners on legend')
  )
  "
})

output$serial7 <- renderAmCharts({
  # prepare data
  data <- as.data.table(get('Titanic', 'package:datasets'))
  data <- data[, .(freq = sum(N)), by = list(Sex, Survived)]
  (data <- split(data, f = data$Survived))
  setkey(data$Yes, Sex)
  setkey(data$No, Sex)
  (dp <- data$Yes[data$No])
  catAxis <- pipeR::pipeline(
    categoryAxis(gridPosition = 'start'),
    addListener('clickItem', 'function(event) {alert(\'Click on category axis\')}')
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'Sex', startDuration = 0,
                  dataProvider = dp, categoryAxis = catAxis),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'freq', fillAlphas = .8, lineAlpha = .2, title = 'Yes'),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'i.freq', fillAlphas = .8, lineAlpha = .2, title = 'No'),
    addValueAxis(stackType = '100%'),
    addTitle(text = 'Survivors to Titanic'),
    setLegend(useGraphSettings = TRUE)
  )
})

output$code_serial7 <- renderText({
  "
  # prepare data
  data <- as.data.table(get('Titanic', 'package:datasets'))
  data <- data[, .(freq = sum(N)), by = list(Sex, Survived)]
  (data <- split(data, f = data$Survived))
  setkey(data$Yes, Sex)
  setkey(data$No, Sex)
  (dp <- data$Yes[data$No])
  catAxis <- pipeR::pipeline(
    categoryAxis(gridPosition = 'start'),
    addListener('clickItem', 'function(event) {alert(\'Click on category axis\')}')
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'Sex', startDuration = 0,
                  dataProvider = dp, categoryAxis = catAxis),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'freq', fillAlphas = .8, lineAlpha = .2, title = 'Yes'),
    addGraph(balloonText = '[[category]]: <b>[[value]]</b>', type = 'column',
             valueField = 'i.freq', fillAlphas = .8, lineAlpha = .2, title = 'No'),
    addValueAxis(stackType = '100%'),
    addTitle(text = 'Survivors to Titanic'),
    setLegend(useGraphSettings = TRUE)
  )
  "
})

output$serial8 <- renderAmCharts({
  # prepare data
  dp <- data.table(
    country = c('USA', 'China', 'Japan', 'Germany', 'UK', 'France',
                'India', 'Spain', 'Netherlands', 'Russia'), 
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580), 
    color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01',
              '#B0DE09', '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0')
  )
  chartCursor_obj <- pipeR::pipeline(
    chartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0),
    addListener('zoomed', 'function(event) {alert(\'Zoom to some period\')}')
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'country', dataProvider = dp,
                  chartCursor = chartCursor_obj),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', fillColorsField = 'color', 
             fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = 'visits'),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, gridAlpha = 0)
  )
})

output$code_serial8 <- renderText({
  "
  # prepare data
  dp <- data.table(
    country = c('USA', 'China', 'Japan', 'Germany', 'UK', 'France',
                'India', 'Spain', 'Netherlands', 'Russia'), 
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580), 
    color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01',
              '#B0DE09', '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0')
  )
  chartCursor_obj <- pipeR::pipeline(
    chartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0),
    addListener('zoomed', 'function(event) {alert(\'Zoom to some period\')}')
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'country', dataProvider = dp,
                  chartCursor = chartCursor_obj),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', fillColorsField = 'color', 
             fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = 'visits'),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, gridAlpha = 0)
  )
  "
})

output$serial9 <- renderAmCharts({
  # prepare data
  dp <- data.table(
    country = c('USA', 'China', 'Japan', 'Germany', 'UK', 'France',
                'India', 'Spain', 'Netherlands', 'Russia'), 
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580),
    random = rnorm(10, mean = 2000, sd = 1000),
    color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01',
              '#B0DE09', '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0')
  )
  
  # prepare value axes
  valueAxis_obj1 <- pipeR::pipeline(
    valueAxis(id = 'v1', title = 'valueAxis 1', position = 'left'),
    addListener('clickItem', 'function(event) {alert(\'Click on valueAxis 1\')}')
  )
  
  valueAxis_obj2 <- pipeR::pipeline(
    valueAxis(id = 'v2', title = 'valueAxis 2', position = 'right'),
    addListener('clickItem', 'function(event) {alert(\'Click on valueAxis 2\')}')
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'country', dataProvider = dp),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', fillColorsField = 'color', valueAxis = 'v1',
             fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = 'visits'),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', valueAxis = 'v2', fillAlphas = 0,
             fillAlphas = 0.85, lineAlpha = 0.1, bullet = 'round', valueField = 'random',
             lineColor = '#FF6600', lineThickness = 5),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, gridAlpha = 0),
    addValueAxis(valueAxis_obj1),
    addValueAxis(valueAxis_obj2)
  )
})

output$code_serial9 <- renderText({
  "
  # prepare data
  dp <- data.table(
    country = c('USA', 'China', 'Japan', 'Germany', 'UK', 'France',
                'India', 'Spain', 'Netherlands', 'Russia'), 
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580),
    random = rnorm(10, mean = 2000, sd = 1000),
    color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01',
              '#B0DE09', '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0')
  )

  # prepare value axes
  valueAxis_obj1 <- pipeR::pipeline(
    valueAxis(id = 'v1', title = 'valueAxis 1', position = 'left'),
    addListener('clickItem', 'function(event) {alert(\'Click on valueAxis 1\')}')
  )
  
  valueAxis_obj2 <- pipeR::pipeline(
    valueAxis(id = 'v2', title = 'valueAxis 2', position = 'right'),
    addListener('clickItem', 'function(event) {alert(\'Click on valueAxis 2\')}')
  )
  
  # build the chart
  pipeR::pipeline(
    amSerialChart(categoryField = 'country', dataProvider = dp),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', fillColorsField = 'color', valueAxis = 'v1',
             fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = 'visits'),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>', valueAxis = 'v2', fillAlphas = 0,
             fillAlphas = 0.85, lineAlpha = 0.1, bullet = 'round', valueField = 'random',
             lineColor = '#FF6600', lineThickness = 5),
    setCategoryAxis(gridPosition = 'start', axisAlpha = 0, gridAlpha = 0),
    addValueAxis(valueAxis_obj1),
    addValueAxis(valueAxis_obj2)
  )
  "
})

