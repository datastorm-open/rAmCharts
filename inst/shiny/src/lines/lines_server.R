output$lines0 <- rAmCharts::renderAmCharts({
  # load data
  data('data_AirPassengers')
  
  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_AirPassengers, theme = 'light', categoryField = 'Period'),
    addGraph(valueField = 'AirPassengers')
  )
})

output$code_lines0 <- renderText({
  "
  # load data
  data('data_AirPassengers')

  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_AirPassengers, theme = 'light', categoryField = 'Period'),
    addGraph(valueField = 'AirPassengers')
  )
  "
}) 

# ---

output$lines1 <- rAmCharts::renderAmCharts({
  # load data
  data('data_AirPassengers')
  
  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_AirPassengers, theme='light', categoryField='Period') ,
    addGraph(bullet = 'round', valueField = 'AirPassengers'),
    setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE),
    setChartScrollbar()
  )
})

output$code_lines1 <- renderText({
  "
  # load data
  data('data_AirPassengers')
  
  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_AirPassengers, theme='light', categoryField='Period') ,
    addGraph(bullet = 'round', valueField = 'AirPassengers'),
    setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE),
    setChartScrollbar()
  )
  "
}) 

# ---

output$lines2 <- rAmCharts::renderAmCharts({
  # load data
  data <- data.frame(class = 1:20, value = rnorm(20))
  
  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data, theme = 'light', categoryField = 'class') ,
    addGraph(valueField = 'value', type = 'smoothedLine', negativeLineColor =  '#637bb6', lineColor =  '#d1655d'),
    setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE),
    addGuide(dashLength = 6, inside = TRUE , label = 'average', lineAlpha = 1, value = mean(data$value)),
    setChartScrollbar()
  )
})

output$code_lines2 <- renderText({
  "
  # load data
  data <- data.frame(class = 1:20, value = rnorm(20))
  
  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data, theme = 'light', categoryField = 'class') ,
    addGraph(valueField = 'value', type = 'smoothedLine', negativeLineColor =  '#637bb6', lineColor =  '#d1655d'),
    setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE),
    addGuide(dashLength = 6, inside = TRUE , label = 'average', lineAlpha = 1, value = mean(data$value)),
    setChartScrollbar()
  )
  "
}) 

# ---

output$lines3 <- rAmCharts::renderAmCharts({
  # load data
  data('data_AirPassengers')
  
  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_AirPassengers, theme = 'light', categoryField = 'Period') ,
    addGraph(id='curve',valueField='AirPassengers', 
             balloonText ='AirPassengers : <b>[[AirPassengers]]</b>, [[[Binf]], [[Bsup]]]',
             lineColor = '#FF0000'),
    addGraph(id='down',lineAlpha = 0, valueField = 'Binf',
             showBalloon = FALSE, hidden  = TRUE, visibleInLegend = FALSE),
    addGraph(id='up',lineAlpha = 0, valueField = 'Bsup',
             showBalloon = FALSE, visibleInLegend = FALSE, fillAlphas = 0.2,
             fillToGraph = 'down', fillColors = '#FF0000'),
    setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE),
    setChartScrollbar()
  )
})



output$code_lines3 <- renderText({
  "
  # load data
  data('data_AirPassengers')
  
  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_AirPassengers, theme = 'light', categoryField = 'Period') ,
    addGraph(id='curve',valueField='AirPassengers', 
             balloonText ='AirPassengers : <b>[[AirPassengers]]</b>, [[[Binf]], [[Bsup]]]',
             lineColor = '#FF0000'),
    addGraph(id='down',lineAlpha = 0, valueField = 'Binf',
             showBalloon = FALSE, hidden  = TRUE, visibleInLegend = FALSE),
    addGraph(id='up',lineAlpha = 0, valueField = 'Bsup',
             showBalloon = FALSE, visibleInLegend = FALSE, fillAlphas = 0.2,
             fillToGraph = 'down', fillColors = '#FF0000'),
    setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE),
    setChartScrollbar()
  )
  "
})

output$lines4 <- rAmCharts::renderAmCharts({
  # load data
  data('data_AirPassengers')
  
  # prepare the custom legend object
  pipeR::pipeline(
    amLegend(equalWidths=FALSE, position='bottom', valueAlign='left', valueWidth=100),
    addListener('hideItem' , paste0('function(event) {
                                        var id = event.dataItem.id;
                                        event.chart.hideGraph(event.chart.getGraphById(id + \'down\'));
                                        event.chart.hideGraph(event.chart.getGraphById(id + \'up\'));
                                      }')),
    addListener('showItem' , paste0('function(event){
                                        var id = event.dataItem.id;
                                        event.chart.showGraph(event.chart.getGraphById(id + \'down\'));
                                        event.chart.showGraph(event.chart.getGraphById(id + \'up\'));
                                      }')),
    (~legend),
    invisible()
  )
  
  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_AirPassengers, theme = 'light', categoryField = 'Period', legend = legend) ,
    addGraph(id='C1',valueField='AirPassengers',
             balloonText ='AirPassengers : <b>[[AirPassengers]]</b>, [[[Binf]], [[Bsup]]]',
             lineColor = '#FF0000', title='Air 1'),
    addGraph(id='C1down',lineAlpha = 0, valueField = 'Binf', 
             showBalloon = FALSE, hidden  = TRUE, visibleInLegend = FALSE),
    addGraph(id='C1up',lineAlpha = 0, valueField = 'Bsup',
             showBalloon = FALSE, visibleInLegend = FALSE, fillAlphas = 0.2,
             fillToGraph = 'C1down', fillColors = '#FF0000'),
    addGraph(id='C2',valueField='AirPassengers2', balloonText ='AirPassengers2 : <b>[[AirPassengers2]]</b>, [[[Binf2]], [[Bsup2]]]',
             lineColor = '#00FF00', title='Air 2'),
    addGraph(id='C2down',lineAlpha = 0, valueField = 'Binf2', 
             showBalloon = FALSE, hidden  = TRUE, visibleInLegend = FALSE),
    addGraph(id='C2up',lineAlpha = 0, valueField = 'Bsup2', 
             showBalloon = FALSE, visibleInLegend = FALSE, fillAlphas = 0.2,
             fillToGraph = 'C2down', fillColors = '#00FF00'),
    setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE),
    setChartScrollbar()
  )
})


output$code_lines4 <- renderText({
  "
  # load data
  data('data_AirPassengers')
  
  # prepare the custom legend object
  pipeR::pipeline(
    amLegend(equalWidths=FALSE, position='bottom', valueAlign='left', valueWidth=100),
    addListener('hideItem' , paste0('function(event) {
                                    var id = event.dataItem.id;
                                    event.chart.hideGraph(event.chart.getGraphById(id + \'down\'));
                                    event.chart.hideGraph(event.chart.getGraphById(id + \'up\'));
}')),
    addListener('showItem' , paste0('function(event){
                                    var id = event.dataItem.id;
                                    event.chart.showGraph(event.chart.getGraphById(id + \'down\'));
                                    event.chart.showGraph(event.chart.getGraphById(id + \'up\'));
    }')),
    (~legend),
    invisible()
    )
  
  # plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_AirPassengers, theme = 'light', categoryField = 'Period', legend = legend) ,
    addGraph(id='C1',valueField='AirPassengers',
             balloonText ='AirPassengers : <b>[[AirPassengers]]</b>, [[[Binf]], [[Bsup]]]',
             lineColor = '#FF0000', title='Air 1'),
    addGraph(id='C1down',lineAlpha = 0, valueField = 'Binf', 
             showBalloon = FALSE, hidden  = TRUE, visibleInLegend = FALSE),
    addGraph(id='C1up',lineAlpha = 0, valueField = 'Bsup',
             showBalloon = FALSE, visibleInLegend = FALSE, fillAlphas = 0.2,
             fillToGraph = 'C1down', fillColors = '#FF0000'),
    addGraph(id='C2',valueField='AirPassengers2', balloonText ='AirPassengers2 : <b>[[AirPassengers2]]</b>, [[[Binf2]], [[Bsup2]]]',
             lineColor = '#00FF00', title='Air 2'),
    addGraph(id='C2down',lineAlpha = 0, valueField = 'Binf2', 
             showBalloon = FALSE, hidden  = TRUE, visibleInLegend = FALSE),
    addGraph(id='C2up',lineAlpha = 0, valueField = 'Bsup2', 
             showBalloon = FALSE, visibleInLegend = FALSE, fillAlphas = 0.2,
             fillToGraph = 'C2down', fillColors = '#00FF00'),
    setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE),
    setChartScrollbar()
  )
  "
})

# ---

output$lines5 <- rAmCharts::renderAmCharts({
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

output$code_lines5 <- renderText({
  "
  ##Data
  dp <- data.table(year = 1994:2012, 
    cars = rnorm(length(1994:2012), mean = 10), 
    motorcycles = rnorm(length(1994:2012), mean = 15), 
    bicycles = rnorm(length(1994:2012), mean = 20)
  )
  dp <- round(dp)
  
  url_car <- 'http://www.amcharts.com/lib/3/images/car.png'
  url_motorcycle <- 'http://www.amcharts.com/lib/3/images/motorcycle.png'
  url_bicycle <- 'http://www.amcharts.com/lib/3/images/bicycle.png'
  pref <- '<img src = '
  suf <- paste('style = \'vertical-align:bottom;',
  'margin-right: 10px; width:28px; height:21px;\'>',
  '<span style = \'font-size:14px; color:#000000;\'>',
  '<b>[[value]]</b></span>')
  
  ##Plot
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
