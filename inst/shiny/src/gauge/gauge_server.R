output$gauge1 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amAngularGaugeChart(startDuration = 0),
    addArrow(value = 100),
    addAxe(gaugeAxis(bottomText = '100 km/h', endValue = 220, valueInterval = 10) %>>%
             addBand(color = '#00CC00', endValue = 90, startValue = 0) %>>%
             addBand(color = '#ffac29', endValue = 130, startValue = 90) %>>%
             addBand(color = '#ea3838', endValue = 220, startValue = 130, innerRadius = '95%')),
    setExport(),
    plot()
  )
})

output$code_gauge1 <- renderText({
  "
  pipeR::pipeline(
    amAngularGaugeChart(),
    addArrow(value = 100),
    addAxe(gaugeAxis(bottomText = '100 km/h', endValue = 220, valueInterval = 10) %>>%
             addBand(color = '#00CC00', endValue = 90, startValue = 0) %>>%
             addBand(color = '#ffac29', endValue = 130, startValue = 90) %>>%
             addBand(color = '#ea3838', endValue = 220, startValue = 130, innerRadius = '95%')),
    setExport(),
    plot()
  )
  "
})
