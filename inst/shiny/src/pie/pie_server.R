output$pie0 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country',
               dataProvider = data_gdp, startDuration = 0),
    setProperties(theme = input$theme_pie)
  )
})

output$code_pie0 <- renderText({
  "
  amPieChart(valueField = 'gdp', titleField = 'country', dataProvider = data_gdp)
  "
})

output$pie01 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country',
               dataProvider = data_gdp, startDuration = 0),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue')
  )
})

output$code_pie01 <- renderText({
  "
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', dataProvider = data_gdp),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue')
  )
  "
})

output$pie03 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', startDuration = 0,
               dataProvider = data_gdp, theme = 'dark'),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE)
  )
})

output$code_pie03 <- renderText({
  "
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', dataProvider = data_gdp, theme = 'dark'),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE)
  )
  "
})

output$pie04 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', startDuration = 0,
               dataProvider = data_gdp, theme = 'light'),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(amLegend(markerType = 'circle', position = 'right',
                       marginRight = 80, autoMargins = FALSE) %>>%
                addListener('hideItem' , 'function(event){alert(\"hide\");}'))
  )
})

output$code_pie04 <- renderText({
  "
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', startDuration = 0,
               dataProvider = data_gdp, theme = 'light'),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(amLegend(markerType = 'circle', position = 'right',
                       marginRight = 80, autoMargins = FALSE) %>>%
                addListener('hideItem' , 'function(event){alert(\"hide\");}'))
  )
  "
})

output$pie05 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', startDuration = 0,
               dataProvider = data_gdp, theme = 'patterns'),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE),
    addListener('clickSlice' , 'function(event){alert(\'Click slice !\');}')
  )
})

output$code_pie05 <- renderText({
  "
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', dataProvider = data_gdp, theme = 'patterns'),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE),
    addListener('clickSlice' , 'function(event){alert(\'Click slice !\');}')
  )
  "
})

output$pie06 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country',
               dataProvider = data_gdp, startDuration = 0,
               theme = 'default', outlineAlpha = .4, depth3D = 15, angle = 30),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE)
  )
})

output$code_pie06 <- renderText({
  "
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', dataProvider = data_gdp,
               theme = 'default', outlineAlpha = .4, depth3D = 15, angle = 30),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE)
  )
  "
})

output$pie07 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country',
               dataProvider = data_gdp, startDuration = 0,
               labelRadius = 5, radius = '30%', innerRadius = '50%'),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE)
  )
})

output$code_pie07 <- renderText({
  "
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', dataProvider = data_gdp,
               labelRadius = 5, radius = '30%', innerRadius = '50%'),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE)
  )
  "
})

