output$pie0 <- rAmCharts::renderAmCharts({
  ## Data
  data('data_gdp')
  ## Plot
  amPieChart(valueField = 'gdp', titleField = 'country',
             dataProvider = data_gdp, startDuration = 0)
})

output$code_pie0 <- renderText({
  "
  ## Data
  data('data_gdp')
  ## Plot
  amPieChart(valueField = 'gdp', titleField = 'country',
             dataProvider = data_gdp, startDuration = 0)
"
})




output$pie01 <- rAmCharts::renderAmCharts({
  ## Data
  data('data_gdp')
  ## Plot
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country',
               dataProvider = data_gdp, startDuration = 0),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue')
  )
})

output$code_pie01 <- renderText({
  "
  ## Data
  data('data_gdp')
  ## Plot
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country',
               dataProvider = data_gdp, startDuration = 0),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue')
  )
  "
})

output$pie03 <- rAmCharts::renderAmCharts({
  ## Data
  data('data_gdp')
  ## Plot
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', startDuration = 0,
               dataProvider = data_gdp, theme = 'dark', export=list(enabled = TRUE)),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE)
  )
})

output$code_pie03 <- renderText({
  "
  ## Data
  data('data_gdp')
  ## Plot
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', startDuration = 0,
               dataProvider = data_gdp, theme = 'dark', export=list(enabled = TRUE)),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE)
  )
  "
})

output$pie04 <- rAmCharts::renderAmCharts({
  ## Data
  data('data_gdp')
  ## Plot
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
  ## Data
  data('data_gdp')
  ## Plot
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
  ## Data
  data('data_gdp')
  ## Plot
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', startDuration = 0,
               dataProvider = data_gdp),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE),
    addListener('clickSlice' , 'function(event){alert(\'Click slice !\');}')
  )
})

output$code_pie05 <- renderText({
  "
  ## Data
  data('data_gdp')
  ## Plot
  pipeR::pipeline(
    amPieChart(valueField = 'gdp', titleField = 'country', startDuration = 0,
               dataProvider = data_gdp),
    addTitle(text = '10 Richest Countries in the World by 2015 GDP'),
    addTitle(text = 'data from http://www.insidermonkey.com', size = 10, color = 'blue'),
    setLegend(markerType = 'circle', position = 'right', marginRight = 80, autoMargins = FALSE),
    addListener('clickSlice' , 'function(event){alert(\'Click slice !\');}')
  )
  "
})


