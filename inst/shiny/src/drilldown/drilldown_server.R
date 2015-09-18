output$drillColumnChart1 <-rAmCharts::renderAmCharts({
  pipeR::pipeline(
    rAmCharts::amSerialChart(categoryField = 'name'),
    setDataProvider(data.frame(name = c('data', 'Brand', 'singleness'), start = c(8,10,6),
                               end = c(11,13,10), color = c('#007FFF', '#007FFF', '#003FFF'),
                               description = c('click to drill-down','',''))),
    rAmCharts::addGraph(valueField = 'end', type = 'column', openField = 'start', lineAlpha = 0,
                        fillColorsField = 'color', fillAlphas = 0.9,
                        balloonText = '<b>[[category]]</b><br>from [[start]] to [[end]]<br>[[description]]'),
    rAmCharts::addSubData( 1, data.frame(modality = c('3G', '4G'), utility = c(-1,2), 
                                         color = c('#007FFF', '#007FFF'))),
    rAmCharts::setSubChartProperties(.subObject = pipeR::pipeline(
      rAmCharts::amSerialChart(creditsPosition = 'bottom-right', categoryField = 'modality'),
      rAmCharts::addGraph(valueField = 'utility', type = 'column', categoryField = 'modality',
                          lineAlpha = 0, fillColorsField = 'color', fillAlphas = 0.9,
                          balloonText = '[[modality]]: <b>[[utility]]</b>'))),
    rAmCharts::plot()
  )
})

output$drillColumnChart1_code <- renderText({
  "
  pipeR::pipeline(
    rAmCharts::amSerialChart(categoryField = 'name'),
    setDataProvider(data.frame(name = c('data', 'Brand', 'singleness'), start = c(8,10,6),
                               end = c(11,13,10), color = c('#007FFF', '#007FFF', '#003FFF'),
                               description = c('click to drill-down','',''))),
    rAmCharts::addGraph(valueField = 'end', type = 'column', openField = 'start', lineAlpha = 0,
                        fillColorsField = 'color', fillAlphas = 0.9,
                        balloonText = '<b>[[category]]</b><br>from [[start]] to [[end]]<br>[[description]]'),
    rAmCharts::addSubData( 1, data.frame(modality = c('3G', '4G'), utility = c(-1,2), 
                                         color = c('#007FFF', '#007FFF'))),
    rAmCharts::setSubChartProperties(.subObject = pipeR::pipeline(
      rAmCharts::amSerialChart(creditsPosition = 'bottom-right', categoryField = 'modality'),
      rAmCharts::addGraph(valueField = 'utility', type = 'column', categoryField = 'modality',
                          lineAlpha = 0, fillColorsField = 'color', fillAlphas = 0.9,
                          balloonText = '[[modality]]: <b>[[utility]]</b>'))),
    rAmCharts::plot()
  )
  "
})